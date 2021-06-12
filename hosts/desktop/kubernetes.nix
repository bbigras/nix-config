{ pkgs, ... }:

{
  services.k3s = {
    enable = true;
    role = "server";
    extraFlags = toString [
      "--kubelet-arg=eviction-hard=imagefs.available<1%,nodefs.available<1%"
      "--kubelet-arg=eviction-minimum-reclaim=imagefs.available=1%,nodefs.available=1%"
      "-disable=traefik"
    ];
  };

  environment.systemPackages = with pkgs; [
    k3s
    k9s
    istioctl
  ];
}
