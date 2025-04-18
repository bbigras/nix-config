{
  programs.atuin = {
    enable = true;
    flags = [ "--disable-up-arrow" ];
    settings = {
      search_mode = "prefix";
      theme = {
        name = "catppuccin-mocha-blue";
      };
    };
  };
}
