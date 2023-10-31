# { system }:

self: super: {
  tmuxPlugins = super.tmuxPlugins // {
    t-smart-tmux-session-manager = (import self.__inputs.nixpkgs_tmux { inherit (self) system; }).tmuxPlugins.t-smart-tmux-session-manager;
  };
}
