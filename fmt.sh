#!/usr/bin/env bash

fd -e nix -E "hardware-configuration*" -x nixpkgs-fmt "{}" \;
