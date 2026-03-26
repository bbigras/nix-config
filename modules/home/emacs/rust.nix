{
  programs.emacs = {
    init = {
      usePackage = {
        cargo-mode = {
          enable = true;
          hook = [
            "(rust-ts-mode . cargo-minor-mode)"
          ];
          config = ''
            (setq compilation-scroll-output t)
          '';
        };

        rust-ts-mode = {
          enable = true;
          hook = [
            "(rust-ts-mode . eglot-ensure)"
          ];
          # config = ''
          #   (add-hook 'after-save-hook 'eglot-format)
          # '';
        };
      };
    };
  };
}
