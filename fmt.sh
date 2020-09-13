#!/usr/bin/env bash

fd -e nix -E "/nix/sources.nix" -E "hardware-configuration*" -x nixpkgs-fmt "{}" \;
