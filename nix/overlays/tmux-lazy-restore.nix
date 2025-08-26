final: prev: {
  tmux-lazy-restore =
    (final.callPackage "${prev.__inputs.nixpkgs_tmux}/pkgs/misc/tmux-plugins" { }).lazy-restore;
}
