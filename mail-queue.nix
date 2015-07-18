{ mkDerivation, aeson, atomic-write, base, bytestring, graceful
, mime-mail, postgresql-simple, random, safe, smtp-mail, stdenv
, text, time
}:
mkDerivation {
  pname = "mail-queue";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson atomic-write base bytestring graceful mime-mail
    postgresql-simple random safe smtp-mail text time
  ];
  license = stdenv.lib.licenses.gpl3;
}
