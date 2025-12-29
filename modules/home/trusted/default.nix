{ flake, ... }:
{
  imports = with flake.self.homeModules; [ trusted-gpg ];

  # programs.git.settings = {
  #   commit.gpgsign = true;
  #   gpg.format = "ssh";
  #   gpg.ssh.allowedSignersFile = "~/.ssh/allowed_signers";
  #   user.signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEKJvfEJv2UsKPnfP9fHIwTuGsMWXc4wHYMf7HZlq2vY";
  # };
}
