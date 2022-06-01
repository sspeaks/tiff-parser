let
  myNixPkgs = import <nixpkgs> {};
  hls = myNixPkgs.haskell-language-server.override { dynamic = true; };
  myGhc = myNixPkgs.ghc.withPackages (hp: with hp; [ zlib hspec hspec-contrib HUnit QuickCheck]);
in

  myNixPkgs.mkShell {
    nativeBuildInputs = (with myNixPkgs; [
      zlib.dev
      cabal-install
      stack
      stylish-haskell
    ]) ++ [hls myGhc];
    shellHook = ''
      if ! grep -q "system-ghc:" "$HOME/.stack/config.yaml"; then
          echo "system-ghc: true" >> $HOME/.stack/config.yaml
      fi
      echo "Installing Hoogle..."
      stack install hoogle
      echo "Done"
      export PATH="$PATH:$HOME/.local/bin"
      hoogle > /dev/null 2>&1
      if [ $? -ne 0 ]; then
          hoogle generate
      fi
      touch $HOME/.ghci
      if ! grep -q ":def hoogle" "$HOME/.ghci"; then
          echo >> ~/.ghci ':def hoogle \x -> return $ ":!hoogle \"" ++ x ++ "\""'
      fi
      if ! grep -q ":def doc" "$HOME/.ghci"; then
          echo >> ~/.ghci ':def doc \x -> return $ ":!hoogle --info \"" ++ x ++ "\""'
      fi
      clear
      '';
  }
