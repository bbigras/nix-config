{ nixpkgs_zed, lib, ... }:

{
  programs.zed-editor = {
    enable = true;
    # https://github.com/zed-industries/extensions/tree/main/extensions
    extensions = [
      "env"
      "gleam"
      "graphql"
      "helm"
      "just"
      "nix"
      "sql"
    ];
    userSettings = {
      "lsp" = {
        "nil" = {
          # or "nixd" =
          "initialization_options" = {
            "formatting" = {
              "command" = [
                "alejandra"
                "--quiet"
                "--"
              ]; # or ["nixfmt"]
            };
          };
        };
      };
    };
  };
}
