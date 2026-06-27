{ pkgs, ... }:

let
  grammarsLibPath = pkgs.emacsPackages.treesit-grammars.with-grammars (
    p: with p; [
      tree-sitter-bash
      tree-sitter-css
      tree-sitter-dockerfile
      tree-sitter-dot
      tree-sitter-elisp
      tree-sitter-fish
      tree-sitter-gleam
      tree-sitter-graphql
      tree-sitter-hcl
      tree-sitter-hjson
      tree-sitter-html
      tree-sitter-javascript
      tree-sitter-jsdoc
      tree-sitter-json
      tree-sitter-just
      tree-sitter-kotlin
      tree-sitter-markdown
      tree-sitter-nix
      tree-sitter-prisma
      tree-sitter-python
      tree-sitter-rust
      tree-sitter-sql
      tree-sitter-toml
      tree-sitter-tsx
      tree-sitter-typescript
      tree-sitter-yaml
    ]
  );
in
{
  programs.emacs = {
    init = {
      usePackage = {
        treesit = {
          enable = true;
          config = ''
            (setq treesit-extra-load-path '("${pkgs.lib.makeLibraryPath [ grammarsLibPath ]}"))
            (setq treesit-enabled-modes t)
          '';
        };
      };
    };
  };
}
