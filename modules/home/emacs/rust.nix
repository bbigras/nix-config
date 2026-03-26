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

        inheritenv = {
          enable = true;
          config = ''
            (inheritenv-add-advice 'cargo-mode--start-cmd)
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
