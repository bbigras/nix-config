{
  programs.vicinae = {
    enable = true;
    systemd.enable = true;

    # https://docs.vicinae.com/quickstart/cosmic#layer-shell
    settings.launcher_window.layer_shell.enabled = false;
  };

  systemd.user.services.vicinae.Service.Environment = [
    "PATH=/etc/profiles/per-user/bbigras/bin:/run/current-system/sw/bin"
  ];
}
