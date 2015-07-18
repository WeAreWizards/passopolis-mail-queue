{-# LANGUAGE OverloadedStrings #-}
-- This module selects the email_queue table every 500ms and sends all
-- the emails that still need sending.

-- TODO
-- * use graceful package

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, mapM)
import           Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import qualified Database.PostgreSQL.Simple as S
import qualified Network.Mail.Mime as M
import           Network.Mail.SMTP (sendMail)
import           Safe (readMay)

data EmailType = INVITE
               | VERIFY_ADDRESS
               | NEW_DEVICE
               | ISSUE_REPORTED
               | ONBOARD_FIRST_SECRET
               | ERROR String
               deriving Show

data Message = Invite { messageAddress :: M.Address }
             | VerifyAddress { messageAddress :: M.Address, messageToken :: String }
             | NewDevice { messageAddress :: M.Address, messageExtra :: String, messageToken :: String}
             | IssueReported
             | OnboardFirstSecret { messageAddress :: M.Address }
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
decodeMessage (VERIFY_ADDRESS, _) =
    Left "wrong args"
decodeMessage (NEW_DEVICE, [Just address, Just args, Just token]) =
    Right (NewDevice (M.Address Nothing (T.pack address)) args token)
decodeMessage (ONBOARD_FIRST_SECRET, [Nothing, Nothing, Nothing, Nothing, Just address]) =
    Right (OnboardFirstSecret (M.Address Nothing (T.pack address)))

renderOne :: (Int, String, Maybe String) -> Either String (IO M.Mail)
renderOne row = do
    msg <- parseMsg row
    dec <- decodeMessage msg
    return $ renderMail dec

waitForEmail :: IO ()
waitForEmail = forever $ do
    conn <- S.connectPostgreSQL "host=127.0.0.1 dbname='mitro'"
    rows <- S.query_ conn "select id, type_string, arg_string from email_queue" :: IO [(Int, String, Maybe String)]

    let mails = map renderOne rows

    mapM (\d -> case d of
                 Left err -> print err
                 Right m -> m >>= M.renderMail' >>= print
         ) mails
    -- TODO how to make sure emails are sent at most once with high
    -- confidence?

    threadDelay (1 * 1000 * 1000)

main = do
    waitForEmail
    -- sendMail "127.0.0.1" m
