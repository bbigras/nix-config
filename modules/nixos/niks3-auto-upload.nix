{ config, ... }:

{
  age.secrets.niks3-auth-token.rekeyFile = ../../secrets/niks3-auth-token.age;

  services.niks3-auto-upload = {
    enable = true;
    serverUrl = "http://cade:5751";
    authTokenFile = config.age.secrets.niks3-auth-token.path;
  };
}
