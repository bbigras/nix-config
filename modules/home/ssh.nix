{ lib, ... }:
{
  home.file.".ssh/sockets/.keep".text = "";

  programs.ssh = {
    enable = true;

    extraConfig = ''
      CanonicalizeHostname yes
      CanonicalDomains tamarin-macaroni.ts.net
      PermitLocalCommand yes
    '';

    matchBlocks = {
      "tailnet" = lib.hm.dag.entryBefore [ "*" ] {
        match = "canonical host *.tamarin-macaroni.ts.net";
        forwardAgent = true;
      };

      "*" = {
        forwardAgent = false;
        addKeysToAgent = "confirm";
        # identityFile = [ ];

        compression = false;
        serverAliveInterval = 60;
        serverAliveCountMax = 5;
        hashKnownHosts = true;
        userKnownHostsFile = "~/.ssh/known_hosts";

        controlMaster = "auto";
        controlPath = "~/.ssh/sockets/master-%r@%n:%p";
        controlPersist = "30m";

        extraOptions = {
          PreferredAuthentications = "publickey";
          PasswordAuthentication = "no";
          KbdInteractiveAuthentication = "no";
          ChallengeResponseAuthentication = "no";

          IdentitiesOnly = "yes";

          StrictHostKeyChecking = "accept-new";
          VerifyHostKeyDNS = "yes";
          UpdateHostKeys = "yes";

          ForwardX11 = "no";
          ForwardX11Trusted = "no";

          RekeyLimit = "1G 1h";
        };
      };
    };
  };
}
