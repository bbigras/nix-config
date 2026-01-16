{
  programs.emacs = {
    init = {
      usePackage = {
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
