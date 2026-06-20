# Docker-compatible CLI configuration for Podman tooling
{
  programs.docker-cli = {
    enable = true;
    settings = {
      credsStore = "secretservice";
    };
  };
}
