{ config, lib, pkgs, ... }:
with lib;
{
  users.users.bbigras = {
    createHome = true;
    extraGroups = [ "wheel" "audio" ]
      ++ optionals config.virtualisation.docker.enable [ "docker" ]
      ++ optionals config.programs.wireshark.enable [ "wireshark" ]
      ++ optionals config.programs.sway.enable [ "input" "video" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.virtualisation.libvirtd.enable [ "libvirtd" ]
      ++ optionals config.services.flatpak.enable [ "flatpak" ]
      ++ optionals config.services.ipfs.enable [ "ipfs" ]
      ++ optionals config.virtualisation.kvmgt.enable [ "kvm" ];
    isNormalUser = true;
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
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
        ./graphical
        ./graphical/sway
      ];
    }
    )
    (mkIf config.services.xserver.desktopManager.gnome.enable {
      imports = [
        ./graphical
        ./graphical/gnome.nix
      ];
    }
    )
  ];
}
