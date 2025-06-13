{ pkgs, ... }:

# easyeffects-presets

let
  perfect_eq_repo = pkgs.fetchgit {
    url = "https://github.com/JackHack96/EasyEffects-Presets";
    rev = "069195c4e73d5ce94a87acb45903d18e05bffdcc";
    sha256 = "sha256-nXVtX0ju+Ckauo0o30Y+sfNZ/wrx3HXNCK05z7dLaFc=";
  };

  easyeffects-presets_repo = pkgs.fetchgit {
    url = "https://github.com/Digitalone1/EasyEffects-Presets";
    rev = "1148788e2768170d704fd2de4f7f5053d32f71d4";
    sparseCheckout = ''
      LoudnessEqualizer.json
    '';
    sha256 = "sha256-WzfVg7PAfrreKC1ckzVtCfOJ90JaUdl/h5mcXt4SFUw=";
  };

  mic_gist_repo = pkgs.fetchgit {
    url = "https://gist.github.com/a10225eb132cdcb97d7c458526f93085.git";
    rev = "5219f20faeaab9ac069cfe93b1d6fbdd82301dfe";
    sha256 = "sha256-pSjtpKs2nA5fZ85k2N18nzzK5JttUj0ZqxpMEXd+OEs=";
  };

  # pipewire_repo = pkgs.fetchgit {
  #   url = "https://gitlab.freedesktop.org/pipewire/pipewire.git";
  #   rev = "c6ffeeeb342311f9d8b3916447f2001e959f99e6";
  #   sha256 = "sha256-frkxyR63frjdOHSF8obOA3NWGyhSKg+yVjlZ1mLlsMY=";
  # };
in
{
  # https://github.com/Digitalone1/EasyEffects-Presets
  # xdg.configFile."easyeffects/output/LoudnessEqualizer.json".source =
  #   "${easyeffects-presets_repo}/LoudnessEqualizer.json";

  # # https://github.com/JackHack96/EasyEffects-Presets
  xdg.configFile."easyeffects/output/Perfect EQ.json".source = "${perfect_eq_repo}/Perfect EQ.json";
  xdg.configFile."easyeffects/output/Bass Enhancing + Perfect EQ.json".source =
    "${perfect_eq_repo}/Bass Enhancing + Perfect EQ.json";
  xdg.configFile."easyeffects/output/Boosted.json".source = "${perfect_eq_repo}/Boosted.json";
  xdg.configFile."easyeffects/output/Advanced Auto Gain.json".source =
    "${perfect_eq_repo}/Advanced Auto Gain.json";

  # xdg.configFile."easyeffects/input/Improved Microphone (Male voices, with Noise Reduction).json".source =
  #   "${mic_gist_repo}/Improved Microphone (Male voices, with Noise Reduction).json";

  #xdg.configFile."pipewire/filter-chain.conf.d/sink-virtual-surround-5.1-kemar2.conf".source = "${pipewire_repo}/src/daemon/filter-chain/sink-virtual-surround-5.1-kemar.conf";

  # /home/bbigras/src/virtual-surround/resources/hrir_kemar/hrir-kemar.wav

  # /nix/store/dr1vhsvi39i3nzi3ak4ncrcabqf59m87-pipewire-c6ffeee/src/daemon/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/nb3v25xs49b1aasjqlpywvv58z1iaqyy-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/jh21g751nxpzsyrrw92hryz8p6a0dwaw-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/vs9fqz47mgy9pi4d5znz7c8gg962vrs3-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf

  services.easyeffects = {
    enable = true;
    preset = "my-preset";
    # extraPresets = {
    #   my-preset = {
    #     input = {
    #       blocklist = [

    #       ];
    #       "plugins_order" = [
    #         "rnnoise#0"
    #       ];
    #       "rnnoise#0" = {
    #         bypass = false;
    #         "enable-vad" = false;
    #         "input-gain" = 0.0;
    #         "model-path" = "";
    #         "output-gain" = 0.0;
    #         release = 20.0;
    #         "vad-thres" = 50.0;
    #         wet = 0.0;
    #       };
    #     };
    #   };
    # };
  };
}
