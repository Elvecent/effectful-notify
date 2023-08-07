{
  nixpkgs = {
    name = "nixpkgs-unstable-2023-08-29";
    url = "https://github.com/nixos/nixpkgs/archive/10b5a0a72d65f8d1e783de466e66fb4f38fee4dd.tar.gz";
    sha256 = "1f4bfm7wyyixxk7addci0aka9kycl03bdabx38wmjw7pf81zxmpv";
  };
  
  haskellCompiler = "ghc945";
  
  haskellOverrides = {
  };
}
