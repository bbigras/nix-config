{
  pkgs,
  hostType,
  impermanence,
  nix-index-database,
  catppuccin,
  minimal-emacs-d,
  ...
}:
{
  imports = [
    (
      if hostType == "nixos" then
        ./nixos.nix
      else if hostType == "darwin" then
        ./darwin.nix
      else
        throw "Unknown hostType '${hostType}' for core"
    )
    ./aspell.nix
    ./nix.nix
    ./nix-ld.nix
  ];

  documentation = {
    enable = true;
    doc.enable = true;
    man.enable = true;
    info.enable = true;
  };

  environment = {
    pathsToLink = [
      "/share/fish"
      "/share/zsh"
    ];
    systemPackages =
      with pkgs;
      [
        rsync
        molly-guard
      ]
      ++ (lib.optional (hostType != "darwin") man-pages);
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit
        hostType
        impermanence
        nix-index-database
        catppuccin
        minimal-emacs-d
        ;
    };
  };

  programs = {
    nix-index.enable = true;
    fish.enable = true;
  };
}
