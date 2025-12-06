# NixOS system user configuration for bbigras
{
  config,
  lib,
  pkgs,
  ...
}:
{
  # Link home-manager user to NixOS user
  home-manager.users.bbigras.home = {
    username = config.users.users.bbigras.name;
    # inherit (config.users.users.bbigras) uid;
  };

  age.secrets.bbigrasPassword.rekeyFile = ../../../secrets/bbigras-password.age;

  # users.groups.bbigras.gid = config.users.users.bbigras.uid;

  users.groups.bbigras = { };
  users.users.bbigras = {
    createHome = true;
    description = "Bruno Bigras";
    group = "bbigras";
    extraGroups = [
      "wheel"
      "dialout"
      "audio"
    ]
    ++ lib.optionals config.hardware.i2c.enable [ "i2c" ]
    ++ lib.optionals config.networking.networkmanager.enable [ "networkmanager" ]
    ++ lib.optionals config.programs.sway.enable [
      "input"
      "video"
    ]
    ++ lib.optionals config.services.unbound.enable [ "unbound" ]
    ++ lib.optionals config.services.transmission.enable [ "transmission" ]
    ++ lib.optionals config.virtualisation.docker.enable [ "docker" ]
    ++ lib.optionals config.virtualisation.libvirtd.enable [ "libvirtd" ]
    ++ lib.optionals config.virtualisation.kvmgt.enable [ "kvm" ]
    ++ lib.optionals config.virtualisation.podman.enable [ "podman" ];
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGPpDAsQDRslxy69ylheWAtg2synerGqkCeCw6F4ISXp TKey"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2Eo0xWZ1VPs5iHlDd3j+O+3I1qx4VqDaXpkL6phB6Z bbigras@desktop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICaqQycIPmT5lvdqdIQwcy+pitleXZtK0j8RsphADcfa bbigras@laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZqHDSyyXc5Zs5XzXveQaOzNoSMPLtY686W5/eVISuQ bbigras@pixel6"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCQErMfuhLr14DAHaUSgTLArydzPoLzeOzkYAzf/ye3qvP/vVXeGe4ruWWxvro0yS3DUlfUpmWUU3KRyv3ZN9z6Q9FnDsKKv+GXTrZq4owymd6NWKOLl2F6LGwUGgnkwvit5TDUVolCvIQTT7F/qLgYvmr0q1nTunrl+uPVNXFiAyalhIPMVU+atw/pNmp3JfIqBRefVlrxeoCQ81/nRhQJcNNXjRxSzIeKu80wwCxODYOBtHdIP/NJEzhAMOq/HLabC7ehZtNohweEAlK71HycqwWSNNonEBU0g9R0r/VfXiENa4x+IY5fvMjsdOj53dZuXCDV0AjOmd8sJoepjF7l pubkeygenerator@mobiledevice"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL+uP/uqqMvdcLF2j4Zz09NNDFO7DOHkpIqxI1XpYo2c bbigras@nixos"
    ];
    shell = pkgs.fish;
    # uid = 8888;

    hashedPasswordFile = config.age.secrets.bbigrasPassword.path;
  };
}
