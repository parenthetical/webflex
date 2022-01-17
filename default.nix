# default.nix
{ system ? builtins.currentSystem }:
let sources = import ./nix/sources.nix ;
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
    data-tree-print = self.callHackage "data-tree-print" "0.1.0.2" {};
    ghc-exactprint = self.callHackage "ghc-exactprint" "0.6.3.4" {};
    stylish-haskell = self.callHackage "stylish-haskell" "0.12.2.0" {};
    # ghcide = self.callHackage "ghcide" "1.5.0.1" {};
    brittany = self.callHackage "brittany" "0.13.1.2" {};
    # implicit-hie = self.callHackage "implicit-hie" "0.1.2.5" {};
  };

  # shellToolOverrides = self: super: {
  #   inherit (pkgs.haskell.packages.ghc8104) haskell-language-server;
  #   inherit (pkgs.haskell.packages.ghc8104) retrie;
  # };
  shellToolOverrides = ghc: super: {
    inherit (ghc) haskell-language-server;
    # inherit (ghc) hie-bios;
    # inherit (ghc) implicit-hie;
  };

  useWarp = true;

  withHoogle = true;
  
  shells = {
    ghc8_10 = [ "webflex-core" "webflex-sim" "reflex-extras" "webflex-counter" "webflex-server" "webflex-client" "webflex-todomvc"];
#    ghcjs8_10 = [ "webflex-core" "webflex-sim" "webflex-client" ];
  };
})
