{ lib, ... }:
{
  home.file.".ssh/sockets/.keep".text = "";

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    settings = {
      "*" = {
        CanonicalizeHostname = true;
        CanonicalDomains = "tamarin-macaroni.ts.net";
        PermitLocalCommand = true;

        ForwardAgent = false;
        AddKeysToAgent = "confirm";
        IdentityFile = [ ];

        Compression = false;
        ServerAliveInterval = 60;
        ServerAliveCountMax = 5;
        HashKnownHosts = true;
        UserKnownHostsFile = "~/.ssh/known_hosts";

        ControlMaster = "auto";
        ControlPath = "~/.ssh/sockets/master-%r@%n:%p";
        ControlPersist = "30m";

        PreferredAuthentications = "publickey,password";
        # PreferredAuthentications = "publickey";
        # PasswordAuthentication = "no";
        KbdInteractiveAuthentication = false;
        ChallengeResponseAuthentication = false;

        IdentitiesOnly = true;

        StrictHostKeyChecking = "accept-new";
        VerifyHostKeyDNS = true;
        UpdateHostKeys = true;

        ForwardX11 = false;
        ForwardX11Trusted = false;

        RekeyLimit = "1G 1h";
      };

      "Match canonical host *.tamarin-macaroni.ts.net" = lib.hm.dag.entryBefore [ "*" ] {
        ForwardAgent = true;
      };
    };
  };
}
