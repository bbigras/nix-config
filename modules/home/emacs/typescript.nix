{
  programs.emacs = {
    init = {
      usePackage = {
        eglot = {
          hook = [
            "(typescript-ts-mode . eglot-ensure)"
          ];
        };
      };
    };
  };
}
