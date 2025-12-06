{
  programs.emacs = {
    init = {
      usePackage = {
        eglot = {
          hook = [
            "(rust-ts-mode . eglot-ensure)"
          ];
        };

        rustic = {
          enable = true;
          config = ''
            (setq rustic-lsp-server 'rust-analyzer)
            (setq rustic-cargo-bin "cargo")
            (setq rustic-cargo-bin-remote "cargo")
            (setq rustic-lsp-client 'eglot)
            (add-hook 'rust-mode-hook 'eglot-ensure)

            ;; prevent rust-ts-mode from stealing .rs for now
            (setcdr (assoc "\\.rs\\'" auto-mode-alist) 'rustic-mode)
          '';
        };
      };
    };
  };
}
