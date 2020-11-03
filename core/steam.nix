{ pkgs, ... }: {
  programs.steam.enable = true;

  # https://github.com/NixOS/nixpkgs/issues/45492
  # Set limits for esync.
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";

  # improve wine performance
  environment.sessionVariables = { WINEDEBUG = "-all"; };

  environment.systemPackages = with pkgs; [
    pkgs.steam-run
  ];

  security.pam.loginLimits = [
    {
      domain = "*";
      type = "hard";
      item = "nofile";
      value = "1048576";
    }
  ];
}
