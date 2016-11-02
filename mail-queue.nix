{ mkDerivation, aeson, base, blaze-html, bytestring, graceful, HTTP
, mime-mail, postgresql-simple, random, safe, shakespeare
, smtp-mail, stdenv, text, time, haskell
}:
let graceful_jb = haskell.lib.overrideCabal graceful (drv: { jailbreak = true; });
in mkDerivation {
  pname = "mail-queue";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base blaze-html bytestring graceful_jb HTTP mime-mail
    postgresql-simple random safe shakespeare smtp-mail text time
  ];
  license = stdenv.lib.licenses.gpl3;
  enableSharedExecutables = false;
}
