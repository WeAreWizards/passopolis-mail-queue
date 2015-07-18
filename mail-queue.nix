{ mkDerivation, aeson, base, bytestring, graceful, mime-mail
, postgresql-simple, safe, smtp-mail, stdenv, time
}:
mkDerivation {
  pname = "mail-queue";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring graceful mime-mail postgresql-simple safe
    smtp-mail time
  ];
  license = stdenv.lib.licenses.gpl3;
}
