{
  hardware.uinput.enable = true;
  services.kanata = {
    enable = true;
    keyboards = {
      default = {
        extraDefCfg = "process-unmapped-keys yes";
        config = ''
          (defsrc
          caps a s d f j k l ;
          )
          (defvar
          tap-time 150
          hold-time 200
          )
          (defalias
          caps (tap-hold 100 100 esc lctl)
          a (multi f24 (tap-hold $tap-time $hold-time a lmet))
          s (multi f24 (tap-hold $tap-time $hold-time s lalt))
          d (multi f24 (tap-hold $tap-time $hold-time d lctl))
          f (multi f24 (tap-hold $tap-time $hold-time f lsft))
          j (multi f24 (tap-hold $tap-time $hold-time j rsft))
          k (multi f24 (tap-hold $tap-time $hold-time k rctl))
          l (multi f24 (tap-hold $tap-time $hold-time l lalt))
          ; (multi f24 (tap-hold $tap-time $hold-time ; rmet))
          )

          (deflayer base
          @caps @a  @s  @d  @f  @j  @k  @l  @;
          )
        '';
      };
    };
  };
}
