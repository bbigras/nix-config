{
  programs.emacs = {
    init = {
      usePackage = {
        go-ts-mode = {
          enable = true;
          hook = [
            "(go-ts-mode . eglot-ensure)"
          ];
        };
      };
    };
  };
}
