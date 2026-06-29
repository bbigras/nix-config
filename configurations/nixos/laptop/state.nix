let
  persistedSshHostKeyUnits = [
    "persist-nix-state-etc-ssh-ssh_host_ed25519_key.service"
    "persist-nix-state-etc-ssh-ssh_host_ed25519_key.pub.service"
    "persist-nix-state-etc-ssh-ssh_host_rsa_key.service"
    "persist-nix-state-etc-ssh-ssh_host_rsa_key.pub.service"
  ];
  persistedLogrotateStatusUnit = "persist-nix-state-var-lib-logrotate.status.service";
in
{
  age.identityPaths = [ "/nix/state/etc/ssh/ssh_host_ed25519_key" ];

  environment.persistence."/nix/state" = {
    # hideMounts = true;
    directories = [
      "/etc/NetworkManager/system-connections"
      "/var/db/veilid-server"
      "/var/lib/bluetooth"
      "/var/lib/containers"
      "/var/lib/flatpak"
      "/var/lib/fwupd"
      "/var/lib/incus"
      "/var/lib/iwd"
      "/var/lib/lastlog"
      "/var/lib/libvirt"
      "/var/lib/netbird"
      "/var/lib/NetworkManager"
      "/var/lib/nixos"
      "/var/lib/sbctl"
      "/root/.cache/nix"
      {
        directory = "/var/lib/private/ncro";
        mode = "0700";
      }
      "/var/lib/systemd/backlight"
      "/var/lib/systemd/coredump"
      "/var/lib/systemd/rfkill"
      "/var/lib/tailscale"
      "/var/lib/upower"
      "/var/log"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/var/lib/logrotate.status"
      "/var/lib/sddm/state.conf"
      "/var/lib/systemd/credential.secret"
      "/var/lib/systemd/random-seed"
      "/var/lib/systemd/tpm2-srk-public-key.pem"
      "/var/lib/systemd/tpm2-srk-public-key.tpm2b_public"
    ];

    users.bbigras = {
      directories = [
        # Caches (regenerable, but expensive to rebuild)
        ".cache/.bun"
        ".cache/bun"
        ".cache/cargo"
        ".cache/chromium"
        ".cache/direnv"
        ".cache/fontconfig"
        ".cache/mesa_shader_cache"
        ".cache/mesa_shader_cache_db"
        ".cache/mozilla"
        ".cache/nix"
        ".cache/nyxt"
        ".cache/restic"
        ".cache/tealdeer"
        ".cargo"
        ".emacs.d/eln-cache"
        ".npm"
        ".omp"
        ".pi"

        # App config / state
        ".config/autostart"
        ".config/chromium"
        ".config/dconf"
        ".config/discord"
        ".config/doctl"
        ".config/easyeffects"
        ".config/Element"
        ".config/noctalia"
        ".config/equibop"
        ".config/freenet"
        ".config/freerdp"
        ".config/gh"
        ".config/github-copilot"
        ".config/jj"
        ".config/jjui"
        ".config/k9s"
        ".config/kdeconnect"
        ".config/keepassxc"
        ".config/mozilla"
        ".config/mcp"
        ".config/nix"
        ".config/opentofu"
        ".config/pipewire"
        ".config/rclone"
        ".config/resticprofile"
        ".config/streamlink"
        ".config/unity3d"
        ".config/vicinae"
        ".config/winboat"
        ".config/wivrn"
        ".config/wluma"
        ".config/wlxoverlay"
        ".config/.wrangler"
        ".config/zed"
        ".local/share/applications"
        ".local/share/atuin"
        ".local/share/containers"
        ".local/share/DBeaverData"
        ".local/share/direnv"
        ".local/share/easyeffects"
        ".local/share/fish"
        ".local/share/flatpak"
        ".local/share/freenet"
        ".local/share/icons"
        ".local/share/iwctl"
        ".local/share/krita"
        ".local/share/nyxt"
        ".local/share/orca"
        ".local/share/remmina"
        ".local/share/Steam"
        ".local/share/Trash"
        ".local/share/vicinae"
        ".local/share/viddy"
        ".local/share/wluma"
        ".local/share/zed"
        ".local/share/zoxide"
        ".local/state/syncthing"
        ".steam"
        ".var/app/com.valvesoftware.Steam"

        # Tool data
        ".android"
        ".claude-squad"
        ".kube/cache"
        ".lxmd"
        ".pki"
        ".radicle"
        ".reticulum"
        ".winboat"

        # User data
        "Documents"
        "dev"
        "doc"
        "keepass"
        "opt"
        "resto"
        "src"
        "Sync"
        "Téléchargements"
        "tmp"
        "winboat"

        {
          directory = ".gnupg";
          mode = "0700";
        }
        {
          directory = ".local/share/keyrings";
          mode = "0700";
        }
        {
          directory = ".ssh";
          mode = "0700";
        }
      ];
      files = [
        ".cache/keepassxc/keepassxc.ini"
        ".cache/noctalia/shell-state.json"
        ".config/cachix/cachix.dhall"
        ".config/git-annex/autostart"
        ".config/remmina/remmina.pref"
        ".config/syncthingtray.ini"
        ".local/share/recently-used.xbel"
        ".config/sops/age/keys.txt"
        ".kube/config"
        ".pulse-cookie"
        ".vault-token"
      ];
    };
  };

  systemd.services = {
    logrotate = {
      after = [ persistedLogrotateStatusUnit ];
      requires = [ persistedLogrotateStatusUnit ];
    };

    systemd-machine-id-commit.enable = false;

    sshd-keygen = {
      after = persistedSshHostKeyUnits;
      requires = persistedSshHostKeyUnits;
    };
  };
}
