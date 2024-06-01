self: super:
let
  copilot-node-server_pkgs = self.__inputs.copilot-node-server_pkgs.legacyPackages.${super.system};
in
{
  my_copilot-node-server = copilot-node-server_pkgs.nodePackages.copilot-node-server;
}
