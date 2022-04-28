build HOST:
  nix build ".#hosts.{{HOST}}" --impure

deploy HOST:
  deploy -s ".#{{HOST}}" -- --impure
