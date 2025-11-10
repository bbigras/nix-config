{
  programs.vicinae = {
    enable = true;
    systemd.enable = true;

    # https://docs.vicinae.com/quickstart/cosmic#layer-shell
    useLayerShell = false;
  };

  systemd.user.services.vicinae.Service.Environment = [
    "PATH=/etc/profiles/per-user/bbigras/bin"
  ];
}
