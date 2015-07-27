{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- This module selects the email_queue table every N milliseconds and
-- sends all the emails that still need sending.

-- TODO
-- * use graceful package

import           Control.Concurrent (threadDelay)
import           Control.Exception (try, catch, SomeException)
import           Control.Monad (forever)
import           Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as S
import           Network.HTTP.Base (urlEncodeVars)
import qualified Network.Mail.Mime as M
import           Network.Mail.SMTP (sendMail)
import           Safe (readMay)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet (shamletFile)

serverBase :: T.Text
serverBase = "https://passopolis.com"

data EmailType = INVITE
               | VERIFY_ADDRESS
               | NEW_DEVICE
               | ISSUE_REPORTED
               | ONBOARD_FIRST_SECRET
               | ERROR String
               deriving Show

data Message = Invite { messageTo :: M.Address, messageFrom :: String, messageUser :: String, token :: String }
             | VerifyAddress { messageTo :: M.Address, messageToken :: String, messageUser :: String }
             | NewDevice { messageTo :: M.Address, messageExtra :: String, messageToken :: String, messageUser :: String}
             | IssueReported { messageTo :: M.Address }
             | OnboardFirstSecret { messageTo :: M.Address }
             deriving Show

instance Read EmailType where
    readsPrec _ "new_user_invitation" = [(INVITE, "")]
    readsPrec _ "address_verification" = [(VERIFY_ADDRESS, "")]
    readsPrec _ "new_device_login" = [(NEW_DEVICE, "")]
    readsPrec _ "issue_reported" = [(ISSUE_REPORTED, "")]
    readsPrec _ "onboard-first-secret" = [(ONBOARD_FIRST_SECRET, "")]
    readsPrec _ r = [(ERROR r, r)]

renderMail :: Message -> IO M.Mail
renderMail (VerifyAddress {..})  =
    M.simpleMail messageTo (M.Address (Just "Passopolis") "team@passopolis.com")
    "Please verify your address for Passopolis"
    (renderHtml $(shamletFile "./templates/verify-address.txt"))
    (renderHtml $(shamletFile "./templates/verify-address.html"))
    []

renderMail (OnboardFirstSecret {..})  =
    M.simpleMail messageTo (M.Address (Just "Passopolis") "team@passopolis.com")
    "Passopolis recorded your first secret"
    (renderHtml $(shamletFile "./templates/onboard-first-secret.txt"))
    (renderHtml $(shamletFile "./templates/onboard-first-secret.html"))
    []

renderMail (NewDevice {..})  =
    M.simpleMail messageTo (M.Address (Just "Passopolis") "team@passopolis.com")
    "Please confirm a new device for Passopolis"
    (renderHtml $(shamletFile "./templates/new-device.txt"))
    (renderHtml $(shamletFile "./templates/new-device.html"))
    []

renderMail (Invite {..})  =
    M.simpleMail messageTo (M.Address (Just "Passopolis") "team@passopolis.com")
    "You've been invited to a secret on Passopolis"
    (renderHtml $(shamletFile "./templates/invite.txt"))
    (renderHtml $(shamletFile "./templates/invite.html"))
    []

renderMail (IssueReported {..})  =
    M.simpleMail messageTo (M.Address (Just "Passopolis") "team@passopolis.com")
    "Thanks for reporting an issue."
    "TODO"
    "TODO"
    []

parseMsg :: (Int, String, Maybe String) -> Either String (EmailType, [Maybe String])
parseMsg (_, type_, args) =
    case readMay type_ of
        Just ep -> case decodeArgs args of
          Just argsOK -> Right (ep, argsOK)
          Nothing -> Left "Could not decode json args"
        Nothing -> Left ("Unknown message type " ++ type_)
  where
    decodeArgs :: Maybe String -> Maybe [Maybe String]
    decodeArgs Nothing = Just []
    decodeArgs (Just s) = decode (BS.pack s) :: Maybe [Maybe String]

decodeMessage :: (EmailType, [Maybe String]) -> Either String Message
decodeMessage (VERIFY_ADDRESS, [Just address, Just token]) =
    Right (VerifyAddress (M.Address Nothing (T.pack address)) token address)
decodeMessage (NEW_DEVICE, [Just address, Just args, Just token]) =
    Right (NewDevice (M.Address Nothing (T.pack address)) args token address)
decodeMessage (ONBOARD_FIRST_SECRET, [Nothing, Nothing, Nothing, Nothing, Just address]) =
    Right (OnboardFirstSecret (M.Address Nothing (T.pack address)))
decodeMessage (INVITE, [Just fromAdress, Just toAddress, Just token]) =
    Right (Invite (M.Address Nothing (T.pack fromAdress)) toAddress fromAdress token)
decodeMessage (ISSUE_REPORTED, [Just address]) =
    Right (IssueReported (M.Address Nothing (T.pack address)))
decodeMessage (ERROR _, _) = undefined
decodeMessage (tp, args) =
    Left ("wrong args for" ++ (show tp) ++ " args: " ++  (show args))

renderOne :: (Int, String, Maybe String) -> Either String (IO M.Mail)
renderOne row = do
    msg <- parseMsg row
    dec <- decodeMessage msg
    return $ renderMail dec

sendOne :: IO M.Mail -> IO Bool
sendOne m = do
    mail <- m
    r <- try (sendMail "127.0.0.1" mail)

    case r of
     Left (e :: SomeException) -> do
         print ("Could not send mail: " ++ (show e))
         return False
     Right _ -> return True

waitForEmail :: IO ()
waitForEmail = do
    conn <- S.connectPostgreSQL "host=127.0.0.1 dbname='mitro'"
    rows <- S.query_ conn "select id, type_string, arg_string from email_queue" :: IO [(Int, String, Maybe String)]

    -- Run this query to make sure we can access email_queue_sent -
    -- has caused permission bugs in the past.
    _ <- S.query_ conn "select id from email_queue_sent" :: IO [S.Only Int]

    let mails = map renderOne rows

    -- renderMail returns (IO Message) because it needs randomness for
    -- generating message boundaries.
    mapM_ (\((rowId, _, _), d) -> case d of
                 Left err -> print err
                 Right m -> do
                            ok <- sendOne m
                            print ("Email attempt for id: " ++ (show rowId) ++ " status: " ++ (show ok))
                            case ok of
                             True -> S.withTransaction conn $ do
                                 _ <- S.execute conn "insert into email_queue_sent select * from email_queue where id = ?" (S.Only rowId)
                                 _ <- S.execute conn "delete from email_queue where id = ?" (S.Only rowId)
                                 return ()
                             False -> return ()

         ) (zip rows mails)
           -- TODO fmap?

    threadDelay (1 * 1000 * 1000)

main :: IO ()
main = do
    forever waitForEmail
