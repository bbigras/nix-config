{
  imports = [
    ./fonts.nix
  ];

  boot = {
    consoleLogLevel = 3;
    kernelParams = [
      "quiet"
      "systemd.show_status=auto"
      "udev.log_level=3"
      "vt.global_cursor_default=0"
    ];
  };
}
