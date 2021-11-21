{ pkgs, ... }: {
  imports = [
    ./ssh.nix
    ./gpg.nix
  ];

  home.packages = with pkgs; [ ];

  # programs.git.signing = {
  #   # key = "";
  #   # signByDefault = true;
  # };

  programs.gpg.settings = {
    # Default/trusted key ID to use (helpful with throw-keyids)
    # default-key = "";
    # trusted-key = "";
  };

  services.gpg-agent = {
    enable = true;
    enableExtraSocket = true;
    enableScDaemon = true;
    enableSshSupport = true;
    defaultCacheTtl = 34560000;
    maxCacheTtl = 34560000;
    extraConfig = ''
      extra-socket /run/user/8888/gnupg/S.gpg-agent.extra
    '';
  };
}
