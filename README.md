Ubuntu 12.04 LTS

Install as many packages from Ubuntu repository as possible:

    libghc-attoparsec-conduit-dev
    libghc-aeson-dev
    libghc-stm-dev
    libghc-warp-dev

Packages `binary` and `binary-bits` are unavailable to this version, so you
need to install them via Cabal. To install them and the `helovalo` package
itself, run:

    cabal update
    cabal install
