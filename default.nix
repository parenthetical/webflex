# default.nix
{ system ? builtins.currentSystem }:
let sources = import ./nix/sources.nix;
in (import sources.reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    webflex-core = ./webflex-core;
    webflex-sim = ./webflex-sim;
    reflex-extras = ./reflex-extras;
    webflex-counter = ./webflex-counter;
    webflex-client = ./webflex-client;
    webflex-server = ./webflex-server;
    webflex-todomvc = ./webflex-todomvc;
  };
  overrides = self: super: {
  };
  shellToolOverrides = ghc: super: {
    haskell-language-server = pkgs.callPackage (import sources.easy-hls-nix) {ghcVersions = ["8.10.7"];};
  };

  useWarp = true;

  withHoogle = true;
  
  shells = {
    ghc8_10 = [ "webflex-core" "webflex-sim" "reflex-extras" "webflex-counter" "webflex-server" "webflex-client" "webflex-todomvc"];
#    ghcjs8_10 = [ "webflex-core" "webflex-sim" "webflex-client" ];
  };
})
