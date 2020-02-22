let nixpkgs = import ./pin.nix { };
in
nixpkgs.mkShell {
  buildInputs = with nixpkgs.elmPackages; [
    elm
    elm-live
    elm-format
    elm-analyse
    elm-test
    nixpkgs.nodePackages_10_x.uglify-js
  ];
}
