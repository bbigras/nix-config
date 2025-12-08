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

            ;; Option 1: Specify explicitly to use Orderless for Eglot
            (setq completion-category-overrides '((eglot (styles orderless))
                                                  (eglot-capf (styles orderless))))

            ;; Enable cache busting, depending on if your server returns
            ;; sufficiently many candidates in the first place.
            (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

            (defun my/eglot-capf ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 #'eglot-completion-at-point
                                 #'tempel-expand))))

            (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

          '';
        };
      };
    };
  };
}
