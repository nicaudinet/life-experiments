{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "life-experiments" ./. {}
