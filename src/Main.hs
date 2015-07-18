{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

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
import qualified Network.Mail.Mime as M
import           Network.Mail.SMTP (sendMail)
import           Safe (readMay)
import           System.Random (getStdGen)

data EmailType = INVITE
               | VERIFY_ADDRESS
               | NEW_DEVICE
               | ISSUE_REPORTED
               | ONBOARD_FIRST_SECRET
               | ERROR String
               deriving Show

data Message = Invite { messageTo :: M.Address }
             | VerifyAddress { messageTo :: M.Address, messageToken :: String }
             | NewDevice { messageTo :: M.Address, messageExtra :: String, messageToken :: String}
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
renderMail (VerifyAddress to token)  =
    M.simpleMail to (M.Address (Just "Passopolis") "team@passopolis.com")
    "verify" "" "" []
renderMail (OnboardFirstSecret to)  =
    M.simpleMail to (M.Address (Just "Passopolis") "team@passopolis.com")
    "onboard" "" "" []
renderMail (NewDevice to args token)  =
    M.simpleMail to (M.Address (Just "Passopolis") "team@passopolis.com")
    "new-device" "" "" []
renderMail (Invite to)  =
    M.simpleMail to (M.Address (Just "Passopolis") "team@passopolis.com")
    "invite" "" "" []
renderMail (IssueReported to)  =
    M.simpleMail to (M.Address (Just "Passopolis") "team@passopolis.com")
    "invite" "" "" []

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
    Right (VerifyAddress (M.Address Nothing (T.pack address)) token)
decodeMessage (NEW_DEVICE, [Just address, Just args, Just token]) =
    Right (NewDevice (M.Address Nothing (T.pack address)) args token)
decodeMessage (ONBOARD_FIRST_SECRET, [Nothing, Nothing, Nothing, Nothing, Just address]) =
    Right (OnboardFirstSecret (M.Address Nothing (T.pack address)))
decodeMessage (INVITE, [Just address]) =
    Right (Invite (M.Address Nothing (T.pack address)))
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

    -- only need to create this table once but doing it lazily anyway:
    _ <- catch (S.execute_ conn "create table email_queue_sent as select * from  email_queue limit 0")
         (\(e :: S.SqlError) -> return 0)

    let mails = map renderOne rows

    -- renderMail returns (IO Message) because it needs randomness for
    -- generating message boundaries.
    mapM (\((id, _, _), d) -> case d of
                 Left err -> print err
                 Right m -> do
                            ok <- sendOne m
                            print ("Email attempt for id: " ++ (show id) ++ " status: " ++ (show ok))
                            case ok of
                             True -> S.withTransaction conn $ do
                                 _ <- S.execute conn "insert into email_queue_sent select * from email_queue where id = ?" (S.Only id)
                                 _ <- S.execute conn "delete from email_queue where id = ?" (S.Only id)
                                 return ()
                             False -> return ()

         ) (zip rows mails)
           -- TODO fmap?

    threadDelay (1 * 1000 * 1000)

main :: IO ()
main = forever waitForEmail
