build HOST:
  nix build .#packages.x86_64-linux.{{HOST}}

deploy HOST:
  deploy -s ".#{{HOST}}" -- --impure
