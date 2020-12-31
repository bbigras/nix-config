{
  hardware.pulseaudio.enable = false;
  systemd.user.services.pipewire.wantedBy = [ "default.target" ];
  systemd.user.services.pipewire-pulse.wantedBy = [ "default.target" ];
  services.pipewire = {
    enable = true;
    socketActivation = false;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
