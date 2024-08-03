{ lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
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
        aliases.l = [ "log" "-r" "(trunk()..@):: | (trunk()..@)-" ];
        core.fsmonitor = "watchman";
        colors."working_copy commit_id" = { underline = true; };
        ui.allow-filesets = true;
        user = {
          name = "Bruno Bigras";
          email = "bigras.bruno@gmail.com";
        };
        template-aliases = {
          "format_short_signature(signature)" = "signature.username()";
        };
        revset-aliases = {
          "HEAD" = "@-";
          "user()" = ''user("bigras.bruno@gmail.com")'';
          "user(x)" = "author(x) | committer(x)";
        };
        templates = {
          draft_commit_description = ''
            concat(
            description,
            surround(
                "\nJJ: This commit contains the following changes:\n", "",
                indent("JJ:     ", diff.stat(72)),
            ),
            )
          '';
          log_node = ''
            coalesce(
              if(!self, "🮀"),
              if(current_working_copy, "@"),
              if(root, "┴"),
              if(immutable, "●", "○"),
            )
          '';
          op_log_node = ''if(current_operation, "@", "○")'';
        };
      };
    };
  };

}
