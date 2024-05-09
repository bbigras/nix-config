# self: super: {
#   emacsPackages = super.emacsPackages // {
#     copilot_el = super.emacsPackages.trivialBuild {
#       pname = "copilot.el";
#       version = "git";
#       src = self.__inputs.copilot_el;
#       packageRequires = with super.emacsPackages; [
#         dash
#         editorconfig
#         nodejs
#         s
#       ];
#       postInstall = ''
#         cp -r $src/dist $LISPDIR
#       '';
#     };
#   };
# }

self: super:
let
  pkgs = self.__inputs.copilot_pkgs.legacyPackages.${super.system};
  copilot-node-server_pkgs = self.__inputs.copilot-node-server_pkgs.legacyPackages.${super.system};
in
{
  my_copilot = pkgs.emacsPackages.copilot;
  my_copilot-node-server = copilot-node-server_pkgs.nodePackages.copilot-node-server;
}
