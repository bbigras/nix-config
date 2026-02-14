{ pkgs, ... }:

let
  grammarsLibPath = pkgs.emacsPackages.treesit-grammars.with-grammars (
    p: with p; [
      tree-sitter-bash
      tree-sitter-css
      tree-sitter-gleam
      tree-sitter-hcl
      tree-sitter-html
      tree-sitter-javascript
      tree-sitter-jsdoc
      tree-sitter-json
      tree-sitter-nix
      tree-sitter-prisma
      tree-sitter-python
      tree-sitter-rust
      tree-sitter-toml
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

            ;; Optional, but recommended. Tree-sitter enabled major modes are
            ;; distinct from their ordinary counterparts.
            ;;
            ;; You can remap major modes with `major-mode-remap-alist'. Note
            ;; that this does *not* extend to hooks! Make sure you migrate them
            ;; also
            (dolist (mapping '((python-mode . python-ts-mode)
                               (css-mode . css-ts-mode)
                               (typescript-mode . tsx-ts-mode)
                               (js-mode . js-ts-mode)
                               (css-mode . css-ts-mode)
                               (rust-mode . rust-ts-mode)
                               (conf-toml-mode . toml-ts-mode)
                               (yaml-mode . yaml-ts-mode)))
              (add-to-list 'major-mode-remap-alist mapping))

            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
            (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
            (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
            (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

            (with-eval-after-load 'org
              (add-to-list 'org-src-lang-modes '("yml"  . yaml-ts))
              (add-to-list 'org-src-lang-modes '("yaml" . yaml-ts)))
          '';
        };
      };
    };
  };
}
