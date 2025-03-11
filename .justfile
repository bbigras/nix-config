build HOST:
  nix build .#packages.x86_64-linux.{{HOST}} --log-format internal-json -v |& nom --json

deploy HOST:
  deploy -s ".#{{HOST}}" -- --impure

pixel6:
  # nix build .#packages.aarch64-linux.pixel6 --builders "ssh-ng://bbigras@192.168.2.29?ssh-key=/opt/rpi5-remote-build aarch64-linux" --impure --max-jobs 0
  nix build .#packages.aarch64-linux.pixel6 --impure --log-format internal-json -v |& nom --json

laptop:
  nom build .#laptop --impure --extra-substituters http://192.168.68.6:8501 --extra-trusted-public-keys "192.168.68.6:zSAiwQJTX02yGP2NSof1Pin339R5YP+91Y5xdaqFsnU="
