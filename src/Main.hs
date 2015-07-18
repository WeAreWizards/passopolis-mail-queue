{-# LANGUAGE OverloadedStrings #-}

import Network.Mail.SMTP (sendMail)
import qualified Network.Mail.Mime as M

main = do
    m <- M.simpleMail
       (M.Address (Just "Tom") "tom@wearewizards.io")
       (M.Address (Just "Passopolis") "team@passopolis.com")
       "hello"
       "body"
       ""
       []

    sendMail "127.0.0.1" m
