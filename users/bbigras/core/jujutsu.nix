{ lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      jjui
      kdiff3
      meld
      watchman
    ];
  };

  # environment.systemPackages = with pkgs; [
  #   watchman
  # ];

  programs = {
    jujutsu = {
      enable = true;
      settings = {
        aliases = {
          l = [
            "log"
            "-r"
            "(trunk()..@):: | (trunk()..@)-"
          ];
          dlog = [
            "log"
            "-r"
          ];
          tug = [
            "bookmark"
            "move"
            "--from"
            "closest_bookmark(@)"
            "--to"
            "closest_pushable(@)"
          ];
        };
        fsmonitor = {
          backend = "watchman";
          watchman.register-snapshot-trigger = true;
        };
        user = {
          name = "Bruno Bigras";
          email = "bigras.bruno@gmail.com";
        };
        template-aliases = {
          "format_short_signature(signature)" = "signature.email().local()";
        };
        templates = {
          log_node = ''
            coalesce(
              if(!self, label("elided", "~")),
              label(
                separate(" ",
                  if(current_working_copy, "working_copy"),
                  if(immutable, "immutable"),
                  if(conflict, "conflict"),
                ),
                coalesce(
                  if(current_working_copy, "@"),
                  if(immutable, "◆"),
                  if(conflict, "×"),
                  if(self.contained_in("private()"), "◌"),
                  "○",
                )
              )
            )'';
        };
        ui.default-command = "log";
        git.private-commits = "private()";
        revset-aliases = {
          "HEAD" = ''coalesce(@ ~ description(exact:""), @-)'';
          "desc(x)" = "description(x)";
          "user()" = ''user("bigras.bruno@gmail.com")'';
          "user(x)" = "author(x) | committer(x)";
          "closest_bookmark(to)" = "heads(::to & bookmarks())";
          "closest_pushable(to)" =
            ''heads(::to & mutable() & ~description(exact:"") & (~empty() | merges()))'';

          # "immutable_heads" = "trunk() | tags()" # default
          # "immutable_heads" = "main@origin | (main@origin.. & ~mine())"; # prevent rewriting commits on main@origin and commits authored by other users
          # "immutable_heads()" = "tags() | remote_bookmarks(remote=origin)"

          "pending()" = ".. ~ ::tags() ~ ::remote_bookmarks() ~ @ ~ private()";
          "private()" = ''
            description(glob:'wip:*') | description(glob:'private:*') |
            description(glob:'WIP:*') | description(glob:'PRIVATE:*') |
                conflicts() | (empty() ~ merges()) | description('substring-i:"DO NOT MAIL"')
          '';
        };
      };
    };
  };

}
