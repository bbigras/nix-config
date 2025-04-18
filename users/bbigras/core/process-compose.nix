{ pkgs, ... }:

let
  process-compose-theme = pkgs.fetchFromGitHub {
    owner = "catppuccin";
    repo = "process-compose";
    rev = "b0c48aa07244a8ed6a7d339a9b9265a3b561464d";
    hash = "sha256-uqJR9OPrlbFVnWvI3vR8iZZyPSD3heI3Eky4aFdT0Qo=";
  };
  yaml = pkgs.formats.yaml { };
in
{
  xdg.configFile."process-compose/theme.yaml".source =
    "${process-compose-theme}/themes/catppuccin-mocha.yaml";
  xdg.configFile."process-compose/settings.yaml".source = yaml.generate "settings.yaml" {
    theme = "Custom Style";
  };
}
