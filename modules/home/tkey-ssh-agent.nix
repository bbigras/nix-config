{ pkgs, ... }:

{
  home.sessionVariablesExtra = ''
    if [ -z "$SSH_AUTH_SOCK" ]; then
      export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/tkey-ssh-agent
    fi
  '';

  systemd.user.services = {
    tkey-ssh-agent = {
      Unit = {
        Description = "tkey-ssh-agent";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${pkgs.tkey-ssh-agent}/bin/tkey-ssh-agent -a %t/tkey-ssh-agent --uss";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
