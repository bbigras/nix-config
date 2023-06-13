{ stdenv, lib, fetchzip, patchelf }:

stdenv.mkDerivation rec {
  pname = "zrok";
  version = "0.3.6";

  src = fetchzip {
    url = "https://github.com/openziti/zrok/releases/download/v${version}/zrok_${version}_linux_arm64.tar.gz";
    stripRoot = false;
    sha256 = "sha256-cZDekIW2Z7hfIY6Y4xhsmgvMLnKYo6H9BAMg9/I5a10=";
  };

  installPhase =
    let
      interpreter = "$(< \"$NIX_CC/nix-support/dynamic-linker\")";
    in
    ''
      mkdir -p $out/bin
      cp zrok $out/bin/
      chmod +x $out/bin/zrok
      patchelf --set-interpreter "${interpreter}" "$out/bin/zrok"
    '';

  meta = {
    description = "Geo-scale, next-generation sharing platform built on top of OpenZiti";
    homepage = "https://zrok.io";
    maintainers = [ lib.maintainers.bandresen ];
    platforms = [ "aarch64-linux" ];
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    license = lib.licenses.apsl20;
  };

}
