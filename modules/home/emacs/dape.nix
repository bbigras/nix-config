{
  programs.emacs.init = {
    usePackage = {
      dape = {
        enable = true;
      };

      # For a more ergonomic Emacs and `dape' experience
      repeat = {
        enable = true;
        custom = {
          repeat-mode = "+1";
        };
      };

      # Left and right side windows occupy full frame height
      emacs = {
        enable = true;
        custom = {
          repeat-mode = "+1";
        };
      };
    };
  };
}
