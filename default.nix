# default.nix
{ system ? builtins.currentSystem }:
let sources = import ./nix/sources.nix ;
in (import sources.reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    webflex-core = ./webflex-core;
    webflex-sim = ./webflex-sim;
    reflex-extras = ./reflex-extras;
  };
  overrides = self: super: {
    # See: https://github.com/reflex-frp/reflex-platform/issues/717
    # mmorph = self.callHackage "mmorph" "1.1.3" {};
  };

  shellToolOverrides = self: super: {
    inherit (pkgs.haskell.packages.ghc865) haskell-language-server;
    inherit (pkgs.haskell.packages.ghc865) retrie;
  };

  useWarp = true;

  withHoogle = true;
  
  shells = {
    ghc = [ "webflex-core" "webflex-sim" "reflex-extras" ];
    ghcjs = [ "webflex-core" "webflex-sim" ];
  };
})
