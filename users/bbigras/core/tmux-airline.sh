#!/usr/bin/env bash

WIDTH=${1}
SMALL=80

if [ "$WIDTH" -le "$SMALL" ]; then
	echo ""
else
	echo "#{prefix_highlight}#[fg=cyan,bg=black,nobold,noitalics,nounderscore]#[fg=black,bg=cyan,bold] #H "
fi
