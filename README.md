# nix-config [![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org) [![ci](https://github.com/bbigras/nix-config/actions/workflows/ci.yaml/badge.svg)](https://github.com/bbigras/nix-config/actions/workflows/ci.yaml)

WIP

# Features
- flakes
- deploy with deploy-rs
- use the zen Linux kernel
- use nixos-hardware for laptop
- BTRFS on all system
- impermanence
  - full disk "wipe" on laptop
  - all except /home wipe on desktop
- Full disk encryption on laptop
- [emacs-init module](https://gitlab.com/rycee/nur-expressions/blob/master/hm-modules/emacs-init.nix) for home-manager. See `users/bbigras/dev/emacs.nix`
- overlays
  - emacs-overlay
  - nixpkgs-mozilla
  - emacs-pgtk-nativecomp-overlay
- sway for 2 machines, gnome + paperwm for 1 machine not supporting sway
- split-dns with systemd-resolved (might be set in my privates files)
- tailscale

This is heavily based on https://github.com/lovesegfault/nix-config. Many files were copied directly from that repo.

Note that the license should include lovesegfault's copyright on many files. Not sure how to sort that yet.

My emacs.nix is a copy of [rycee's](https://gitlab.com/rycee/configurations/-/commits/master/user/emacs.nix) with some of my changes. So he has some copyright claim on that file.

## Build one host

```sh
nix build .#hosts.desktop --impure
```

## Deploy one host

```sh
deploy -s .#pixel6 -- --impure
```

## nix-on-droid (first time)

On the phone:

```sh
nix-shell -p openssh -p which

# get user and group id and put it in hosts/pixel6/default.nix
id

ssh-keygen -q -N "" -t ed25519 -f ~/ssh_host_ed25519_key

cat <<EOF > tmp-sshd
HostKey ~/ssh_host_ed25519_key
Port 8022
EOF

mkdir -p ~/.ssh
cat <<EOF > ~/.ssh/authorized_keys
**my key**
EOF

# start sshd server to be able to deploy from desktop with deploy-rs
`which sshd` -dD -f ~/tmp-sshd
```
