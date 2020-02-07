{ overlays ? [], config ? {} }:
let
    nixPath = builtins.fetchGit {
        url = https://github.com/NixOs/nixpkgs;
        rev = "8da81465c19fca393a3b17004c743e4d82a98e4f";
    };
in
import nixPath { inherit overlays config; }
