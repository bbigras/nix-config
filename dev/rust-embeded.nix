{ pkgs, ... }:

{
  # rust embeded
  # https://discourse.nixos.org/t/providing-environment-for-the-embedded-rust-book/14539
  users.extraGroups.plugdev = { };
  users.extraUsers.bbigras.extraGroups = [ "plugdev" "dialout" ];
  services.udev.packages = [ pkgs.openocd ];
}
