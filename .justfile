build HOST:
  nix build .#packages.x86_64-linux.{{HOST}} --log-format internal-json -v |& nom --json

deploy HOST:
  deploy -s ".#{{HOST}}" -- --impure

pixel6:
  # nix build .#packages.aarch64-linux.pixel6 --builders "ssh-ng://bbigras@192.168.2.29?ssh-key=/opt/rpi5-remote-build aarch64-linux" --impure --max-jobs 0
  nix build .#packages.aarch64-linux.pixel6 --impure --log-format internal-json -v |& nom --json
