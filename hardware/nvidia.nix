{ config, ... }:
let
  # nvidia_x11 = config.boot.kernelPackages.nvidia_x11;
  nvidia_x11 = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  # nvidia_gl = nvidia_x11.out;
  # nvidia_gl_32 = nvidia_x11.lib32;
in
{
  boot.blacklistedKernelModules = [ "nouveau" ];

  hardware = {
    nvidia = {
      package = nvidia_x11;
      modesetting.enable = true;
      nvidiaPersistenced = true;
    };
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  # virtualisation.docker.enableNvidia = true;
  # virtualisation.podman.enableNvidia = true;
}
