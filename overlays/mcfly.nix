self: super: {
  mcfly = super.mcfly.overrideAttrs (
    old: rec {
      name = "mcfly-${version}";
      version = "0.5.0";

      src = super.fetchFromGitHub {
        owner = "cantino";
        repo = "mcfly";
        rev = "v${version}";
        sha256 = "155x745jakfcpr6kmp24cy8xwdhv81jdfjjhd149bnw5ilg0z037";
      };

      postInstall = ''
        substituteInPlace mcfly.bash --replace '$(which mcfly)' $out/bin/mcfly
        substituteInPlace mcfly.zsh  --replace '$(which mcfly)' $out/bin/mcfly
        substituteInPlace mcfly.fish --replace '(which mcfly)' $out/bin/mcfly
        install -Dm644 -t $out/share/mcfly mcfly.bash
        install -Dm644 -t $out/share/mcfly mcfly.zsh
        install -Dm644 -t $out/share/mcfly mcfly.fish
      '';

      cargoDeps = old.cargoDeps.overrideAttrs (_: {
        name = "${name}-vendor.tar.gz";
        inherit src;
        outputHash = "0y6sjbzg5qqqip9sc9ajyd5ra3n2wwvarj6nhpzjhh05kqz3qja4";
      });
    }
  );
}
