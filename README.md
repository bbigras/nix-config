# nix-config ![CI](https://github.com/bbigras/nix-config/workflows/CI/badge.svg)

WIP

# Features
- flakes
- deploy with deploy-rs
- use the zen Linux kernel
- use nixos-hardware for laptop
- BTRFS on all system
- impermanence
  - full disk "wipe" on laptop
  - all expect /home wipe on desktop
- Full disk encryption on laptop
- [emacs-init module](https://gitlab.com/rycee/nur-expressions/blob/master/hm-modules/emacs-init.nix) for home-manager. See `users/bbigras/dev/emacs.nix`
- overlays
  - emacs-overlay
  - nixpkgs-mozilla
  - emacs-pgtk-nativecomp-overlay
  - nixpkgs-cdda-mods
- sway for 2 machines, gnome + paperwm for 1 machine not supporting sway
- NTS (secure NTP) with chronyd
- split-dns with systemd-resolved (might be set in my privates files)
- tailscale and zerotier

This is heavily based on https://github.com/lovesegfault/nix-config. Many files were copied directly from that repo.

Note that the license should include lovesegfault's copyright on many files. Not sure how to sort that yet.

My emacs.nix is a copy of [rycee's](https://gitlab.com/rycee/configurations/-/commits/master/user/emacs.nix) with some of my changes. So he has some copyright claim on that file.
