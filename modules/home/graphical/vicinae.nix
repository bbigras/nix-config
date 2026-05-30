{
  programs.vicinae = {
    enable = true;
    systemd.enable = true;
  };

  systemd.user.services.vicinae.Service.Environment = [
    "PATH=/etc/profiles/per-user/bbigras/bin:/run/current-system/sw/bin"
  ];
}
