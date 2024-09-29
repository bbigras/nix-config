{ pkgs, hostType, impermanence, nix-index-database, catppuccin, ... }: {
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
    systemPackages = with pkgs; [
      man-pages
      rsync
      molly-guard
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "hm-backup";
    extraSpecialArgs = {
      inherit
        hostType
        impermanence
        nix-index-database
        catppuccin;
    };
  };

  programs = {
    nix-index.enable = true;
    fish.enable = true;
  };
}
