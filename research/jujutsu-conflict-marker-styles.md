# Jujutsu conflict marker styles

- Jujutsu reads only selected Git configuration; `merge.conflictStyle` is not among the settings listed in its [Git compatibility documentation](https://docs.jj-vcs.dev/latest/git-compatibility/).
- Jujutsu's [configuration documentation](https://docs.jj-vcs.dev/latest/config/#conflict-marker-style) identifies `diff` as the default: one snapshot plus diffs. `snapshot` displays each side and base; `git` uses Git diff3-style markers.
- The [conflicts documentation](https://docs.jj-vcs.dev/latest/conflicts/#alternative-conflict-marker-styles) notes that `git` falls back to `snapshot` for conflicts with more than two sides, and explains that `diff` avoids manually comparing sides.
- [Alex Carney's Jujutsu notes](https://www.alcarney.me/notes/20250529T201728/) recommend Git-style markers for compatibility with other tools. This is one individual's recommendation, not evidence of a community consensus.
- Jujutsu's [built-in merge tool configuration](https://github.com/jj-vcs/jj/blob/main/cli/src/config/merge_tools.toml) selects Git-style markers for VS Code; a per-tool setting is distinct from the working-copy UI setting.
- This configuration repository sets Git `merge.conflictstyle` to `diff3` in `modules/home/git.nix`, but has no explicit Jujutsu marker-style setting in `modules/home/jujutsu.nix`.
