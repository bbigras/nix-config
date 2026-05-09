# fs-diff.sh — show files that would be wiped on next boot by the
# initrd rollback service (laptop host, btrfs on /dev/mapper/crypted).
#
# Adapted from https://mt-caret.github.io/blog/posts/2020-06-29-optin-state.html
#
# Usage: sudo ./fs-diff.sh [root|home|all]   (default: all)
set -euo pipefail

DEVICE="/dev/mapper/crypted"
MNT="$(mktemp -d)"
TARGET="${1:-all}"

if [ "$(id -u)" -ne 0 ]; then
  exec sudo "$0" "$@"
fi

cleanup() {
  if mountpoint -q "$MNT"; then
    umount "$MNT"
  fi
  rmdir "$MNT"
}
trap cleanup EXIT

mount -o subvol=/ "$DEVICE" "$MNT"

diff_subvol() {
  local sub="$1"
  local blank="$MNT/${sub}-blank"
  local live="$MNT/${sub}"

  if [ ! -d "$blank" ] || [ ! -d "$live" ]; then
    echo "skipping $sub: missing $blank or $live" >&2
    return
  fi

  echo "===== $sub ====="
  local old_transid
  old_transid=$(btrfs subvolume find-new "$blank" 9999999)
  old_transid=${old_transid#transid marker was }

  btrfs subvolume find-new "$live" "$old_transid" |
    sed '$d' |
    cut -f17- -d' ' |
    sort -u |
    while read -r path; do
      # Paths are relative to the subvolume root; map to absolute paths
      # on the running system: root -> /, home -> /home
      case "$sub" in
        root) abs="/$path" ;;
        home) abs="/home/$path" ;;
        *)    abs="/$path" ;;
      esac
      if [ -L "$abs" ]; then
        : # symlink, likely managed by NixOS
      elif [ -d "$abs" ]; then
        : # directory, ignore
      else
        echo "$abs"
      fi
    done
}

case "$TARGET" in
  root) diff_subvol root ;;
  home) diff_subvol home ;;
  all)  diff_subvol root; diff_subvol home ;;
  *) echo "usage: $0 [root|home|all]" >&2; exit 1 ;;
esac
