{ pkgs, ... }:

let
  perfect_eq_repo = pkgs.fetchgit {
    url = "https://github.com/JackHack96/EasyEffects-Presets";
    rev = "069195c4e73d5ce94a87acb45903d18e05bffdcc";
    sha256 = "sha256-nXVtX0ju+Ckauo0o30Y+sfNZ/wrx3HXNCK05z7dLaFc=";
  };

  mic_gist_repo = pkgs.fetchgit {
    url = "https://gist.github.com/jtrv/47542c8be6345951802eebcf9dc7da31";
    rev = "c0a71a61e30856989a6dc109b59873e3f3ea697d";
    sha256 = "sha256-aaViZbmK0X5y4uYe1bLabEpPGIwhXcztxxW3euPHTvU=";
  };
in
{
  xdg.configFile = {
    "easyeffects/output/Perfect EQ.json".source = "${perfect_eq_repo}/Perfect EQ.json";
    "easyeffects/input/fifine_male_voice_noise_reduction.json".source =
      "${mic_gist_repo}/EasyEffects Microphone Preset: Masc NPR Voice + Noise Reduction.json";
  };

  services.easyeffects = {
    enable = true;
  };
}
