build HOST:
  #nix build ".#{{HOST}}" --impure
  nix-build -A packages.x86_64-linux.all.{{HOST}}

deploy HOST:
  deploy -s ".#{{HOST}}" -- --impure
