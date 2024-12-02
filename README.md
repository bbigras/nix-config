# nix-config [![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org) [![ci](https://github.com/bbigras/nix-config/actions/workflows/ci.yaml/badge.svg)](https://github.com/bbigras/nix-config/actions/workflows/ci.yaml)

WIP

# Features
- flakes
- deploy with deploy-rs
- use the zen Linux kernel
- use [nixos-hardware](https://github.com/NixOS/nixos-hardware)
- use [srvos](https://github.com/nix-community/srvos)
- Full disk encryption on laptop
- [emacs-init module](https://gitlab.com/rycee/nur-expressions/blob/master/hm-modules/emacs-init.nix) for home-manager. See `users/bbigras/dev/emacs.nix`
- overlays
  - emacs-overlay
  - nur
- split-dns with systemd-resolved (might be set in my privates files)
- tailscale

This is heavily based on https://github.com/lovesegfault/nix-config. Many files were copied directly from that repo.

Note that the license should include lovesegfault's copyright on many files. Not sure how to sort that yet.

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
