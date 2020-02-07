with import ./pin.nix { };
mkShell {
  name = "chess";

  buildInputs = with elmPackages; [
    elm
    elm-live
    elm-format
    elm-analyse
    nodePackages_10_x.uglify-js
  ];
}
