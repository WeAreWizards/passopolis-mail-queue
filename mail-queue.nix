{ mkDerivation, aeson, base, blaze-html, bytestring, graceful, HTTP
, mime-mail, postgresql-simple, random, safe, shakespeare
, smtp-mail, stdenv, text, time
}:
mkDerivation {
  pname = "mail-queue";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base blaze-html bytestring graceful HTTP mime-mail
    postgresql-simple random safe shakespeare smtp-mail text time
  ];
  license = stdenv.lib.licenses.gpl3;
}
