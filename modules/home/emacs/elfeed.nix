{
  programs.emacs.init = {
    usePackage = {
      elfeed = {
        enable = true;
        bind = {
          "C-x w" = "elfeed";
        };
      };
      elfeed-score = {
        enable = true;
        config = ''
          (progn
            (elfeed-score-enable)
            (define-key elfeed-search-mode-map "=" elfeed-score-map))
        '';
      };
      universal-sidecar = {
        enable = true;
      };
    };
  };
}
