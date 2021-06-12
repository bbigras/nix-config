let
  emojimenu =
    { fetchurl
    , jq
    , runCommand
    , writeSaneShellScriptBin

    , displayCmd
    , yankCmd
    , extraInputs ? [ ]
    }:
    let
      emoji_json = fetchurl {
        name = "emojis.json";
        url = "https://raw.githubusercontent.com/github/gemoji/b1c7878afeb260d2ff6dc6655bf3aa7dee498e9c/db/emoji.json";
        sha256 = "sha256-bL+4ft3yzVUnVxDg7cwoS2hvbeqEKvhx77QjEgR+Yhk=";
      };
      emojis = runCommand "emojis.txt"
        { nativeBuildInputs = [ jq ]; } ''
        cat ${emoji_json} | jq -r '.[] | "\(.emoji) \t   \(.description)"' | sed -e 's,\\t,\t,g' > $out
      '';
    in
    writeSaneShellScriptBin {
      name = "emojimenu";

      buildInputs = [ ] ++ extraInputs;

      src = ''
        emoji="$(${displayCmd} < ${emojis} | cut -f1 -d" ")"

        ${yankCmd} <<< "$emoji"
      '';
    };
in
self: _: {
  emojimenu-wayland = self.callPackage emojimenu {
    displayCmd = ''wofi --cache-file="$XDG_CACHE_HOME/wofi/emojimenu" -d "allow_markup=false" -p emoji--show dmenu'';
    yankCmd = "wl-copy --trim-newline";
    extraInputs = with self; [ wl-clipboard wofi ];
  };

  emojimenu-x11 = self.callPackage emojimenu {
    displayCmd = ''rofi -cache-dir "$XDG_CACHE_HOME/rofi/emojimenu" -p emoji -dmenu'';
    yankCmd = "xclip -in -rmlastnl -selection clipboard";
    extraInputs = with self; [ rofi xclip ];
  };
}
