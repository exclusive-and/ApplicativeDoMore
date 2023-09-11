
{
    description = "Applicative Do Does More";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-23.05";
    };

    outputs = { self, nixpkgs, ... }:
    let
        system = "x86_64-linux";

        pkgs = import nixpkgs { inherit system; };

        hs = pkgs.haskell.packages.ghc96;

        applicative-do-more = hs.callCabal2nix "applicative-do-more" ./.
        { };

        applicative-do-more-env = hs.shellFor
        {
            packages = _: [ applicative-do-more ];

            build-depends = [ hs.ghc ];

            nativeBuildInputs = [
                hs.cabal-install
                hs.haskell-language-server
            ];
        };
    in
    {
        packages.${system}.default = applicative-do-more;
        devShells.${system}.default = applicative-do-more-env;
    };
}
