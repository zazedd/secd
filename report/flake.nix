{
  description = "Build LaTeX document with minted";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        latex-packages = with pkgs; [
          (texlive.combine {
            inherit (texlive)
              sttools
              scheme-medium
              framed
              titlesec
              cleveref
              multirow
              wrapfig
              tabu
              threeparttable
              threeparttablex
              makecell
              environ
              biblatex
              biber
              fvextra
              upquote
              catchfile
              xstring
              csquotes
              minted
              dejavu
              comment
              footmisc
              xltabular
              ltablex
              dashrule
              ifmtarg
              soul
              xpatch
              preprint
              ;
          })
        ];

        dev-packages = with pkgs; [
          zathura
        ];
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.texliveFull
            latex-packages
            dev-packages
          ];
        };
      }
    );
}
