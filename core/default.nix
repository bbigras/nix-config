{ pkgs, base16-schemes, hostType, impermanence, nix-index-database, stylix, ... }: {
  imports = [
    (
      if hostType == "nixos" then ./nixos.nix
      else if hostType == "darwin" then ./darwin.nix
      else throw "Unknown hostType '${hostType}' for core"
    )
    ./aspell.nix
    ./nix.nix
  ];

  documentation = {
    enable = true;
    doc.enable = true;
    man.enable = true;
    info.enable = true;
  };

  environment = {
    pathsToLink = [
      "/share/zsh"
    ];
    systemPackages = with pkgs; [
      man-pages
      rsync
      molly-guard
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit
        base16-schemes
        hostType
        impermanence
        nix-index-database
        stylix;
    };
  };

  programs = {
    nix-index.enable = true;
    fish.enable = true;
    zsh.enable = true;
  };

  stylix = {
    base16Scheme = "${base16-schemes}/tomorrow-night.yaml";
    # We need this otherwise the autoimport clashes with our manual import.
    homeManagerIntegration.autoImport = false;
    image = pkgs.nixos-artwork.wallpapers.simple-dark-gray.gnomeFilePath;
    cursor = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };
  };
}
