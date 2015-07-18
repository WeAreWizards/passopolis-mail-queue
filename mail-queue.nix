{ mkDerivation, base, bytestring, mime-mail, smtp-mail, stdenv }:
mkDerivation {
  pname = "mail-queue";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base bytestring mime-mail smtp-mail ];
  license = stdenv.lib.licenses.gpl3;
}
