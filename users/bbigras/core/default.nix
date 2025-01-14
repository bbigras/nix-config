{
  config,
  hostType,
  impermanence,
  nix-index-database,
  pkgs,
  lib,
  catppuccin,
  ...
}:

# let
#perfect_eq_repo = pkgs.fetchgit {
#  url = "https://github.com/JackHack96/EasyEffects-Presets";
#  rev = "041588fa7871b53282221741b3d11593f96ce468";
#  sha256 = "sha256-gpdW2k9E+3DtaztD89wp1TA1kyrAdQNXNFT3hk36WA0=";
#};

# easyeffects-presets_repo = pkgs.fetchgit {
#   url = "https://github.com/Digitalone1/EasyEffects-Presets";
#   rev = "1148788e2768170d704fd2de4f7f5053d32f71d4";
#   sparseCheckout = ''
#     LoudnessEqualizer.json
#   '';
#   sha256 = "sha256-WzfVg7PAfrreKC1ckzVtCfOJ90JaUdl/h5mcXt4SFUw=";
# };

# mic_gist_repo = pkgs.fetchgit {
#   url = "https://gist.github.com/a10225eb132cdcb97d7c458526f93085.git";
#   rev = "5219f20faeaab9ac069cfe93b1d6fbdd82301dfe";
#   sha256 = "sha256-pSjtpKs2nA5fZ85k2N18nzzK5JttUj0ZqxpMEXd+OEs=";
# };

#pipewire_repo = pkgs.fetchgit {
#  url = "https://gitlab.freedesktop.org/pipewire/pipewire.git";
#  rev = "c6ffeeeb342311f9d8b3916447f2001e959f99e6";
#  sha256 = "sha256-frkxyR63frjdOHSF8obOA3NWGyhSKg+yVjlZ1mLlsMY=";
#};
# in
{
  imports = [
    impermanence.nixosModules.home-manager.impermanence
    nix-index-database.hmModules.nix-index
    catppuccin.homeManagerModules.catppuccin

    ./atuin.nix
    ./btop.nix
    ./git.nix
    ./jujutsu.nix
    ./emacs
    ./tmux.nix
    ./xdg.nix
    ./fish.nix
    ./yazi.nix
  ];

  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "blue";
  };

  # XXX: Manually enabled in the graphic module
  dconf.enable = false;

  home = {
    username = "bbigras";
    stateVersion = "24.05";
    packages = with pkgs; [
      nix-closure-size
      mosh

      # dev
      colordiff

      # net
      dumbpipe
      croc
      rclone
      sendme
      tailscale
      tcpdump
      webwormhole
      xh

      # nix
      cachix
      comma
      manix
      nix-update
      nixpkgs-fmt

      # cool cli tools
      fd
      hexyl
      pwgen
      sd # find & replace
      doggo

      # Android
      # android-studio
      # scrcpy

      # backup
      restic

      # utils
      file
      strace

      # rust
      cargo

      pv

      docker-compose
      prettyping # a nicer ping

      killall

      unar

      # kubernetes
      kubectl
      kubectx
      kubelogin-oidc
      # istioctl
      kubernetes-helm
      kind

      docker-credential-helpers
      viddy
      natscli
      just

      # usb imagers
      usbimager

      # media
      mediainfo
      spotube
      kooha

      zrok

      devpod
      altair
      broot
      distrobox
      incus.client
      srgn
      attic-client
      aider-chat
      git-annex
      git-remote-gcrypt
      spacer
      faketty
      television
    ];
    shellAliases = {
      cls = "clear";
      j = "${pkgs.just}/bin/just";
      ".j" = "${pkgs.just}/bin/just --justfile ~/.user.justfile";
      less = "${pkgs.bat}/bin/bat";
    };
  };

  programs = {
    atuin = {
      enable = true;
      settings.auto_sync = true;
    };
    bat = {
      enable = true;
    };
    carapace.enable = true;
    eza.enable = true;
    jq.enable = true;
    kubecolor = {
      enable = true;
      enableAlias = true;
      settings = {
        preset = "dark";
        paging = "auto";
      };
    };
    fzf = {
      enable = true;
      tmux.enableShellIntegration = true;
      defaultCommand = "fd --type f";
      changeDirWidgetCommand = "fd --type d"; # alt+c
      fileWidgetCommand = "fd --type f";
    };
    gpg.enable = true;
    nix-index.enable = true;
    nix-your-shell.enable = true;
    pay-respects.enable = true;
    ripgrep.enable = true;
    sqls.enable = true;
    ssh = {
      enable = true;
      controlMaster = "auto";
      controlPersist = "10m";
      hashKnownHosts = true;
      includes = [
        "~/.ssh/devpod"
      ];
      extraOptionOverrides = {
        AddKeysToAgent = "confirm";
        VerifyHostKeyDNS = "ask";
      };
    };
    tealdeer.enable = true;
    zoxide.enable = true;
    zellij.enable = true;
  };

  services = {
    # spotifyd.enable = true;
    syncthing.enable = true;
    systembus-notify.enable = true;
  };

  # https://github.com/Digitalone1/EasyEffects-Presets
  # xdg.configFile."easyeffects/output/LoudnessEqualizer.json".source = "${easyeffects-presets_repo}/LoudnessEqualizer.json";

  # https://github.com/JackHack96/EasyEffects-Presets
  #xdg.configFile."easyeffects/output/Perfect EQ.json".source = "${perfect_eq_repo}/Perfect EQ.json";
  #xdg.configFile."easyeffects/output/Bass Enhancing + Perfect EQ.json".source = "${perfect_eq_repo}/Bass Enhancing + Perfect EQ.json";
  #xdg.configFile."easyeffects/output/Boosted.json".source = "${perfect_eq_repo}/Boosted.json";
  #xdg.configFile."easyeffects/output/Advanced Auto Gain.json".source = "${perfect_eq_repo}/Advanced Auto Gain.json";

  # xdg.configFile."easyeffects/input/Improved Microphone (Male voices, with Noise Reduction).json".source = "${mic_gist_repo}/Improved Microphone (Male voices, with Noise Reduction).json";

  #xdg.configFile."pipewire/filter-chain.conf.d/sink-virtual-surround-5.1-kemar2.conf".source = "${pipewire_repo}/src/daemon/filter-chain/sink-virtual-surround-5.1-kemar.conf";

  # /home/bbigras/src/virtual-surround/resources/hrir_kemar/hrir-kemar.wav

  # /nix/store/dr1vhsvi39i3nzi3ak4ncrcabqf59m87-pipewire-c6ffeee/src/daemon/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/nb3v25xs49b1aasjqlpywvv58z1iaqyy-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/jh21g751nxpzsyrrw92hryz8p6a0dwaw-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/vs9fqz47mgy9pi4d5znz7c8gg962vrs3-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf

  systemd.user.services = {
    syncthing = {
      Service = {
        StandardOutput = "null";
      };
    };
  };

  home.language.base = "fr_CA.UTF-8";

  systemd.user.startServices = "sd-switch";

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
