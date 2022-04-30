build HOST:
  nix build ".#{{HOST}}" --impure

deploy HOST:
  deploy -s ".#{{HOST}}" -- --impure
