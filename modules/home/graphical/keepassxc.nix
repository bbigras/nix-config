{
  programs.keepassxc = {
    enable = true;
    settings = {
      Browser.Enabled = false;
      # Browser.UpdateBinaryPath = false;
      # MinimizeAfterUnlock = true;
      FdoSecrets.Enabled = true;
      GUI = {
        AdvancedSettings = true;
        ApplicationTheme = "dark";
        CompactMode = true;
        HidePasswords = true;
        LaunchAtStartup = true;
      };
      SSHAgent.Enabled = false;
      Security = {
        LockDatabaseIdleSeconds = 18000;
      };
    };
  };
}
