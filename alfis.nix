{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.alfis;
  settingsFormat = pkgs.formats.toml { };
  configFile = settingsFormat.generate "alfis.toml" cfg;
in
{
  options.services.alfis = {
    enable = mkEnableOption "Alfis DNS server";

    #    settings = mkOption {
    #      type = settingsFormat.type;
    #      default = { };
    #      description = ''
    #        Alfis configuration.
    #      '';

    origin = mkOption {
      default = "0000001D2A77D63477172678502E51DE7F346061FF7EB188A2445ECA3FC0780E";
      type = types.str; # Should this by types.strMatching?
      description = ''
        The hash of first block in a chain to know with which nodes to work.
      '';
    };

    key_files = mkOption {
      default = [ ];
      type = types.listOf types.path;
      description = ''
        Paths to your key files to load automatically
      '';
    };

    check_blocks = mkOption {
      default = 8;
      type = types.ints.unsigned;
      description = ''
        How many last blocks to check on start
      '';
    };

    port = mkOption {
      default = 53;
      type = types.port;
      description = ''
        Listen port for Alfis DNS
      '';
    };

    net = {
      peers = mkOption {
        default = [
          "test-ip4.alfis.name:4244"
          "test-ip6.alfis.name:4244"
        ];
        type = types.listOf types.str;
        description = ''
          All bootstrap nodes
        '';
      };

      listen = mkOption {
        default = "[::]:4244";
        type = types.str;
        description = ''
          Your node will listen on that address for other nodes to connect
        '';
      };

      public = mkOption {
        default = true;
        type = types.bool;
        description = ''
          Set true if you want your IP to participate in peer-exchange, or false otherwise
        '';
      };
      yggdrasil_only = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Allow connections to/from Yggdrasil only (https://yggdrasil-network.github.io)
        '';
      };
    };

    dns = {
      listen = mkOption {
        default = "127.0.0.1:53";
        type = types.str;
        description = ''
          Your DNS resolver will be listening on this address and port (Usual port is 53)
        '';
      };
      threads = mkOption {
        default = 20;
        type = types.ints.positive;
        description = ''
          How many threads to spawn by DNS server
        '';
      };
      forwarders = mkOption {
        default = [ ];
        type = types.listOf types.str;
        description = ''
          Where to forward non-alfis requests to
        '';
        example = ''
          [
            # Adguard servers
            "94.140.14.14:53"
            "94.140.15.15:53"
          ]
        '';
      };
      hosts = mkOption {
        default = [ ];
        type = types.listOf types.path;
        description = ''
          Hosts file support (resolve local names or block ads)
        '';
        example = ''
          [
            "/etc/hosts"
            "/home/user/.config/hosts/adblock.txt"
          ]
        '';
      };
    };
    mining = {
      threads = mkOption {
        default = 0;
        type = types.ints.unsigned;
        description = ''
          How many CPU threads to spawn for mining, zero = number of CPU cores
        '';
      };
      lower = mkOption {
        default = true;
        type = types.bool;
        description = ''
          Set lower priority for mining threads
        '';
      };
    };
    #    settings = mkOption {
    #      type = settingsFormat.type;
    #      default = { };
    #      description = ''
    #        Alfis configuration.
    #      '';
    #
    #      example = literalExample ''
    #        {
    #          origin = "0000001D2A77D63477172678502E51DE7F346061FF7EB188A2445ECA3FC0780E";
    #          key_files = [
    #            "/home/username/.config/alfis/key1.toml"
    #            "/home/username/.config/alfis/key2.toml"
    #          ];
    #          check_blocks = 8;
    #          net = {
    #            peers = [
    #              "test-ip4.alfis.name:4244"
    #              "test-ip6.alfis.name:4244"    
    #            ];
    #            listen = "[::]:4244";
    #            public = true;
    #            yggdrasil_only = false;
    #          };
    #          dns = {
    #            listen = "127.0.0.1:53";
    #            threads = 20;
    #            forwarders = [
    #              "94.140.14.14:53"
    #              "94.140.14.15:53"
    #            ];
    #          };
    #          mining = {
    #            threads = 0;
    #            lower = true;
    #          };
    #        }
    #      '';
    #    };
  };
  config = mkIf cfg.enable {
    systemd.services.alfis = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      description = "Start the Alfis daemon.";
      serviceConfig = {
        DynamicUser = true;
        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        #CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        ExecStart = ''${pkgs.alfis-nogui}/bin/alfis -n -c ${configFile}'';
        Restart = "always";
        StateDirectory = "alfis";
        WorkingDirectory = "/var/lib/alfis";
      };
    };
  };
}

