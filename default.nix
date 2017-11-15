with import <nixpkgs> {};

let ghc = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
  # These are for spacemacs haskell layer. To get spacemacs with the
  # correct PATH. run nix-shell, then launch Emacs inside this
  # nix-shell.
  apply-refact hlint stylish-haskell hasktags
  # FIXME: ghc-syb-utils check fails because of a missing file.
  # It comes from by ghc-mod. Remove it when check is fixed.
  # ghc-syb-utils #.override (self: super: { doCheck = false; }) # used by ghc-mod
  ghc-mod

  #nix-env -qaP -A nixos.haskellPackages|fgrep aeson
  cabal-install aeson extra HUnit
  simple-sql-parser
]);
in stdenv.mkDerivation {
  name = "osp-utils";
  buildInputs = [ ghc python27Full python27Packages.seqdiag ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}
