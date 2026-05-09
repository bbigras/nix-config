{ lib, ... }:
{
  home.file.".ssh/sockets/.keep".text = "";

  # tkey hardware key public key (private half lives on the TKey, not on disk).
  # Used as IdentityFile so IdentitiesOnly=yes will offer this agent identity.
  home.file.".ssh/tkey.pub".text = ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGPpDAsQDRslxy69ylheWAtg2synerGqkCeCw6F4ISXp TKey
  '';

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
        identityFile = [ "~/.ssh/tkey.pub" ];

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
