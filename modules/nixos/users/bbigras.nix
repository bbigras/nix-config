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
      "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIHjRA/aNknvSU1HlK0soY62w19+7ADm32w8/K5v8zIWWAAAABHNzaDo= bbigras@desktop"
      "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAICzYRvLmweSSKAnSYVIUokV4AN/wClqEgWcxAV/HTClFAAAABHNzaDo= bbigras@laptop"
    ];
    shell = pkgs.fish;
    # uid = 8888;

    hashedPasswordFile = config.age.secrets.bbigrasPassword.path;
  };
}
