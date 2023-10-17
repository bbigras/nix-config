{ pkgs, ... }:


let
  withPlugins = with pkgs; grammarFn:
    let
      grammars = grammarFn tree-sitter.builtGrammars;
    in
    linkFarm "grammars"
      (map
        (drv:
          let
            name = lib.strings.getName drv;
          in
          {
            name =
              "lib" +
              (lib.strings.removeSuffix "-grammar" name)
              + ".so";
            path = "${drv}/parser";
          }
        )
        grammars);

  grammarsLibPath = withPlugins (_: pkgs.tree-sitter.allGrammars);
in
{
  programs.emacs = {
    init = {
      usePackage = {
        treesit = {
          enable = true;
          config = ''
            (setq treesit-extra-load-path '("${grammarsLibPath}"))

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
          '';
        };
      };
    };
  };
}
