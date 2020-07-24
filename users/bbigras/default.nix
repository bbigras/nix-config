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
  };

  home-manager.users.bbigras = mkMerge [
    {
      imports = [
        ./core
        ./dev
      ] ++ (if builtins.pathExists ../secrets/bbigras/default.nix then [ ../secrets/bbigras ] else [ ]);
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
