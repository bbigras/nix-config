# Docker-compatible CLI configuration for Podman tooling
{
  programs.docker-cli = {
    enable = true;
    settings = {
      credsStore = "secretservice";
      credHelpers = {
        "ghcr.io" = "secretservice";
        "registry.fly.io" = "secretservice";
        "registry.gitlab.com" = "secretservice";
      };
    };
  };
}
