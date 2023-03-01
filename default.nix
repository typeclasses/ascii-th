let

sources = import ./nix/sources.nix;
nixos-22-05 = import sources."nixos-22.05" {};
nixos-22-11 = import sources."nixos-22.11" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    ascii-th = ./ascii-th;
};

ghc."8.10" = nixos-22-05.haskell.packages.ghc8107.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            ascii-case = new.callPackage ./nix/ascii-case-1.0.1.nix {};
            ascii-caseless = new.callPackage ./nix/ascii-caseless-0.0.0.nix {};
            ascii-superset = new.callPackage ./nix/ascii-superset-1.2.4.nix {};
        })
    ];
});

ghc."9.0" = nixos-22-11.haskell.packages.ghc90.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            ascii-case = new.callPackage ./nix/ascii-case-1.0.1.nix {};
            ascii-caseless = new.callPackage ./nix/ascii-caseless-0.0.0.nix {};
            ascii-superset = new.callPackage ./nix/ascii-superset-1.2.4.nix {};
        })
    ];
});

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            ascii-case = new.callPackage ./nix/ascii-case-1.0.1.nix {};
            ascii-caseless = new.callPackage ./nix/ascii-caseless-0.0.0.nix {};
            ascii-superset = new.callPackage ./nix/ascii-superset-1.2.4.nix {};
        })
    ];
});

ghc."9.4" = nixos-22-11.haskell.packages.ghc94.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            ascii-case = new.callPackage ./nix/ascii-case-1.0.1.nix {};
            ascii-caseless = new.callPackage ./nix/ascii-caseless-0.0.0.nix {};
            ascii-superset = new.callPackage ./nix/ascii-superset-1.3.0.nix {};
        })
    ];
});

in

symlinkJoin {
    name = "ascii-th";
    paths = concatMap (x: [x.ascii-th]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
