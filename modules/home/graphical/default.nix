{
  flake,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake) self;
  inherit (pkgs.stdenv) isDarwin isLinux;
in
{
  imports = [
    self.homeModules.graphical-linux
    flake.inputs.hermes-agent.homeManagerModules.default
  ];
  # ++ lib.optionals isLinux [ self.homeModules.graphical-linux ];

  home.packages =
    with pkgs;
    lib.filter (lib.meta.availableOn stdenv.hostPlatform) [
      libnotify
      qbittorrent
    ]
    ++ lib.optionals (stdenv.hostPlatform.system == "x86_64-linux") [
      # spotify
    ]
    ++ lib.optionals isLinux [
      xdg-utils
    ];

  programs = {
    hermes-agent = lib.mkIf (flake.inputs.hermes-agent.packages.${pkgs.stdenv.hostPlatform.system} ? desktop) {
      enable = true;
      desktop.enable = true;
    };

    ghostty = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
      installBatSyntax = isLinux;
      package = if isDarwin then null else pkgs.ghostty;
      settings = {
        clipboard-read = "ask";
        quit-after-last-window-closed = true;
      };
    };
  };

  services = {
    remmina.enable = true;
  };
}
