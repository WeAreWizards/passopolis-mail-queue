{-# LANGUAGE OverloadedStrings #-}
-- This module selects the email_queue table every 500ms and sends all
-- the emails that still need sending.

-- TODO
-- * use graceful package

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BS
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
               | ERROR String deriving Show

instance Read EmailType where
    readsPrec _ "new_user_invitation" = [(INVITE, "")]
    readsPrec _ "address_verification" = [(VERIFY_ADDRESS, "")]
    readsPrec _ "new_device_login" = [(NEW_DEVICE, "")]
    readsPrec _ "issue_reported" = [(ISSUE_REPORTED, "")]
    readsPrec _ "onboard-first-secret" = [(ONBOARD_FIRST_SECRET, "")]
    readsPrec _ r = [(ERROR r, r)]

renderMail :: M.Address -> EmailType -> IO M.Mail
renderMail to INVITE =
    M.simpleMail
    to
    (M.Address (Just "Passopolis") "team@passopolis.com")
    "invite"
    "invite"
    "invite"
    []

parseMsg :: (String, Maybe String) -> Either String (EmailType, [Maybe String])
parseMsg (type_, args) =
    case readMay type_ of
        Just ep -> case decodeArgs args of
          Just argsOK -> Right (ep, argsOK)
          Nothing -> Left "Could not decode json args"
        Nothing -> Left ("Unknown message type " ++ type_)
  where
    decodeArgs :: Maybe String -> Maybe [Maybe String]
    decodeArgs Nothing = Just []
    decodeArgs (Just s) = decode (BS.pack s) :: Maybe [Maybe String]

waitForEmail :: IO ()
waitForEmail = forever $ do
    conn <- S.connectPostgreSQL "host=127.0.0.1 dbname='mitro'"
    r <- S.query_ conn "select type_string, arg_string from email_queue" :: IO [(String, Maybe String)]

    print $ map parseMsg r
    threadDelay (1 * 1000 * 1000)

main = do
    waitForEmail
    -- sendMail "127.0.0.1" m
