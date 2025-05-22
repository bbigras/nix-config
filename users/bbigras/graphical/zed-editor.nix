{
  nixpkgs_zed,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    alejandra
    nil
    nixd
  ];

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
      lsp = {
        rust-analyzer = {
          initialization_options = {
            check = {
              command = "clippy"; # rust-analyzer.check.command (default: "check")
            };
            inlayHints = {
              maxLength = null;
              lifetimeElisionHints = {
                enable = "skip_trivial";
                useParameterNames = true;
              };
              closureReturnTypeHints = {
                enable = "always";
              };
            };
          };
        };
      };
    };
  };
}
