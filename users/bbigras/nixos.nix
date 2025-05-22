{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
{
  users.users.bbigras = {
    createHome = true;
    # group = "bbigras";
    extraGroups =
      [
        "wheel"
        "dialout"
      ]
      ++ optionals config.hardware.uinput.enable [ "uinput" ]
      ++ optionals config.hardware.i2c.enable [ "i2c" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.programs.wireshark.enable [ "wireshark" ]
      ++ optionals config.services.flatpak.enable [ "flatpak" ]
      ++ optionals config.services.kubo.enable [ "ipfs" ]
      ++ optionals config.programs.sway.enable [
        "input"
        "video"
      ]
      ++ optionals config.services.unbound.enable [ "unbound" ]
      ++ optionals config.services.transmission.enable [ "transmission" ]
      ++ optionals config.virtualisation.docker.enable [ "docker" ]
      ++ optionals config.virtualisation.libvirtd.enable [ "libvirtd" ]
      ++ optionals config.virtualisation.kvmgt.enable [ "kvm" ]
      ++ optionals config.virtualisation.incus.enable [ "incus-admin" ]
      ++ optionals config.virtualisation.podman.enable [ "podman" ];
    isNormalUser = true;
    shell = mkIf config.programs.fish.enable pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGPpDAsQDRslxy69ylheWAtg2synerGqkCeCw6F4ISXp TKey"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2Eo0xWZ1VPs5iHlDd3j+O+3I1qx4VqDaXpkL6phB6Z bbigras@desktop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICaqQycIPmT5lvdqdIQwcy+pitleXZtK0j8RsphADcfa bbigras@laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZqHDSyyXc5Zs5XzXveQaOzNoSMPLtY686W5/eVISuQ bbigras@pixel6"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCQErMfuhLr14DAHaUSgTLArydzPoLzeOzkYAzf/ye3qvP/vVXeGe4ruWWxvro0yS3DUlfUpmWUU3KRyv3ZN9z6Q9FnDsKKv+GXTrZq4owymd6NWKOLl2F6LGwUGgnkwvit5TDUVolCvIQTT7F/qLgYvmr0q1nTunrl+uPVNXFiAyalhIPMVU+atw/pNmp3JfIqBRefVlrxeoCQ81/nRhQJcNNXjRxSzIeKu80wwCxODYOBtHdIP/NJEzhAMOq/HLabC7ehZtNohweEAlK71HycqwWSNNonEBU0g9R0r/VfXiENa4x+IY5fvMjsdOj53dZuXCDV0AjOmd8sJoepjF7l pubkeygenerator@mobiledevice"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL+uP/uqqMvdcLF2j4Zz09NNDFO7DOHkpIqxI1XpYo2c bbigras@nixos"
    ];
  };

  # programs._1password-gui.polkitPolicyOwners = [ "bbigras" ];

  home-manager.users.bbigras = {
    imports = optionals config.services.desktopManager.cosmic.enable [
      ./graphical
    ];
  };
}
