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
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2Eo0xWZ1VPs5iHlDd3j+O+3I1qx4VqDaXpkL6phB6Z bbigras@desktop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICaqQycIPmT5lvdqdIQwcy+pitleXZtK0j8RsphADcfa bbigras@laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZqHDSyyXc5Zs5XzXveQaOzNoSMPLtY686W5/eVISuQ bbigras@pixel6"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOTrwYIPtKqyGjVAsUMBEroO5+FGOzmYk6VlSE0yaE+w nix-on-droid@tablet"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCQErMfuhLr14DAHaUSgTLArydzPoLzeOzkYAzf/ye3qvP/vVXeGe4ruWWxvro0yS3DUlfUpmWUU3KRyv3ZN9z6Q9FnDsKKv+GXTrZq4owymd6NWKOLl2F6LGwUGgnkwvit5TDUVolCvIQTT7F/qLgYvmr0q1nTunrl+uPVNXFiAyalhIPMVU+atw/pNmp3JfIqBRefVlrxeoCQ81/nRhQJcNNXjRxSzIeKu80wwCxODYOBtHdIP/NJEzhAMOq/HLabC7ehZtNohweEAlK71HycqwWSNNonEBU0g9R0r/VfXiENa4x+IY5fvMjsdOj53dZuXCDV0AjOmd8sJoepjF7l pubkeygenerator@mobiledevice"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCvDHgK4U+nWmYdcnENPvn9RuLZGkZj8FRfjv4IgGo7yelkWXXFWochjfuIrIZQUntL6C4kZpcO7MGl1yWELxop+hmm/zAyngy2h1q5RQSI4qkndrEvV8Mdntw2KZIym1XDAakAoI5yKnTCmKanJb7l0ros3l2hteqhPAnv6VA1gHuDAwgETR6IkFgrVWu0u6UxfKRQrQ6wnpZuAI54hhut4Yna37LRJ8ihT81IcMMuAr8kdl1CsrEEygfs2GzysZ1ElcalFqCckuxqjsv+YQum/cBo2sWDENt+UbDzDPzADKNan9K4bRVTuvXa3YiX7unuqikIlaxsmx0y3ofhjARl pubkeygenerator@tablet"
    ];
  };

  # programs._1password-gui.polkitPolicyOwners = [ "bbigras" ];

  home-manager.users.bbigras = {
    imports =
      optionals config.services.xserver.desktopManager.gnome.enable [
        ./graphical
        ./graphical/gnome.nix
      ]
      ++ optionals config.services.desktopManager.cosmic.enable [
        ./graphical
      ];
  };
}
