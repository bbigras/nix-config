{ pkgs, ... }:

{
  programs.emacs = {
    init = {
      usePackage = {
        eglot = {
          hook = [
            "(typescript-ts-mode . eglot-ensure)"
          ];
        };

        flymake-eslint = {
          enable = true;
          config = ''
            (setq flymake-eslint-executable-name "${pkgs.nodePackages.eslint}/bin/eslint")

            (add-hook 'typescript-ts-mode-hook (lambda ()
              (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)
              (setq-local flymake-eslint-project-root (locate-dominating-file buffer-file-name ".eslintrc.js"))
              (flymake-eslint-enable)))
            (add-hook 'js-ts-mode-hook (lambda ()
              (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)
              (setq-local flymake-eslint-project-root (locate-dominating-file buffer-file-name ".eslintrc.js"))
              (flymake-eslint-enable)))
          '';
        };
      };
    };
  };
}
