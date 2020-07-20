{ lib, ... }: {
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };
}
