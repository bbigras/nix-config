{ config, lib, inputs, pkgs, ... }:
with lib;
{
  users.users.bbigras = {
    createHome = true;
    extraGroups = [ "wheel" "dialout" ]
      ++ optionals config.hardware.i2c.enable [ "i2c" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.programs.sway.enable [ "input" "video" ]
      ++ optionals config.services.unbound.enable [ "unbound" ]
      ++ optionals config.sound.enable [ "audio" ]
      ++ optionals config.virtualisation.docker.enable [ "docker" ]
      ++ optionals config.virtualisation.libvirtd.enable [ "libvirtd" ]
      ++ optionals config.virtualisation.kvmgt.enable [ "kvm" ]
      ++ optionals config.virtualisation.podman.enable [ "podman" ]
      ++ optionals config.programs.wireshark.enable [ "wireshark" ]
      ++ optionals config.services.flatpak.enable [ "flatpak" ]
      ++ optionals config.services.ipfs.enable [ "ipfs" ];
    isNormalUser = true;
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2Eo0xWZ1VPs5iHlDd3j+O+3I1qx4VqDaXpkL6phB6Z bbigras@desktop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE6LFIPxHhM18nw6Sp8xPVG2GGPNcTSrNwTQAoNIyA0r bbigras@laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAoSe4gCERcN4Fucwd7dFWUm9LLO/T0wBC+NzXcgrhwt bbigras@pixel6"
    ];
  };

  home-manager.users.bbigras = {
    imports = [
      "${inputs.impermanence}/home-manager.nix"
      ./core
      ./dev
    ] ++ optionals config.programs.sway.enable [
      ./graphical
      ./graphical/sway
    ] ++ optionals config.services.xserver.desktopManager.gnome.enable [
      ./graphical
      ./graphical/gnome.nix
    ];
  };
}
