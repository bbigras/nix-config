{ config, lib, pkgs, ... }:
with lib;
rec {
  users.users.bbigras = {
    createHome = true;
    extraGroups = [ "wheel" ]
      ++ optionals config.virtualisation.docker.enable [ "docker" ]
      ++ optionals config.programs.wireshark.enable [ "wireshark" ]
      ++ optionals config.programs.sway.enable [ "input" "video" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.virtualisation.libvirtd.enable [ "libvirtd" ]
      ++ optionals config.virtualisation.kvmgt.enable [ "kvm" ];
    isNormalUser = true;
    shell = mkIf config.programs.fish.enable pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2Eo0xWZ1VPs5iHlDd3j+O+3I1qx4VqDaXpkL6phB6Z bbigras@desktop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE6LFIPxHhM18nw6Sp8xPVG2GGPNcTSrNwTQAoNIyA0r bbigras@laptop"
    ];
  };

  home-manager.users.bbigras = mkMerge [
    {
      imports = [
        ./core
        ./dev
      ];
    }
    (mkIf config.programs.sway.enable {
      imports = [
        ./sway
      ];
    }
    )
    (mkIf config.services.xserver.desktopManager.gnome3.enable {
      imports = [
        ./gnome.nix
      ];
    }
    )
  ];
}
