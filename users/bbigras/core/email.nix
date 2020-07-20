{ pkgs, ... }: {
  accounts.email.accounts.bbigras = {
    primary = true;
    notmuch.enable = true;

    lieer.enable = true;
    lieer.sync.enable = true;
  };

  programs.notmuch = {
    enable = true;
    new.tags = [ "new" ];
    search.excludeTags = [ "trash" "spam" ];
  };
  programs.lieer.enable = true;
  services.lieer.enable = true;
  programs.afew = {
    enable = true;

    # https://afew.readthedocs.io/en/latest/configuration.html
    extraConfig = ''
      [KillThreadsFilter]
      [ArchiveSentMailsFilter]
    '';
    # [ListMailsFilter]
    # [SpamFilter]
    # [InboxFilter]
  };
}
