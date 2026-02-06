{
  home.file.".ssh/config".text = ''
    Include ~/.ssh/config.host

    Host *
      CanonicalizeHostname yes
      PermitLocalCommand yes
      CanonicalDomains tamarin-macaroni.ts.net

    Match canonical Host *.tamarin-macaroni.ts.net
      ForwardAgent yes

    Match canonical Host *
      ChallengeResponseAuthentication no
      ControlMaster auto
      ControlPath ~/.ssh/ssh-%r@%h:%p
      ControlPersist 30m
      ForwardAgent no
      ForwardX11 no
      ForwardX11Trusted no
      HashKnownHosts yes
      ServerAliveCountMax 5
      ServerAliveInterval 60
      StrictHostKeyChecking ask
      VerifyHostKeyDNS yes
  '';
}
