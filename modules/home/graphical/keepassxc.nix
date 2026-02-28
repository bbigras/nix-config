{
  programs.keepassxc = {
    enable = true;
    settings = {
      Browser.Enabled = true;
      # Browser.UpdateBinaryPath = false;
      General = {
        MinimizeAfterUnlock = true;
      };
      FdoSecrets.Enabled = true;
      GUI = {
        AdvancedSettings = true;
        ApplicationTheme = "dark";
        CompactMode = true;
        HidePasswords = true;
        LaunchAtStartup = true;
        MinimizeOnClose = true;
        MinimizeToTray = true;
        ShowTrayIcon = true;
      };
      SSHAgent.Enabled = false;
      Security = {
        LockDatabaseIdleSeconds = 18000;
      };
    };
  };
}
