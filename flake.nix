
{
    description = "Applicative Do Does More";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-23.05";
    };

    outputs = { self, nixpkgs, ... }:
    let
        system = "x86_64-linux";

        pkgs = import nixpkgs { inherit system; };
        ghc  = pkgs.haskell.packages.ghc96;

        applicative-do-more = ghc.callCabal2nix "applicative-do-more" ./.
        { };

        applicative-do-more-env = ghc.shellFor
        {
            packages = _: [ applicative-do-more ];

            build-depends = [ ghc.ghc ];

            nativeBuildInputs = [
                ghc.cabal-install
                ghc.haskell-language-server
            ];
        };
    in
    {
        packages.${system}.default = applicative-do-more;
        devShells.${system}.default = applicative-do-more-env;
    };
}
