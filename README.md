# notify-effectful

A fork of [fdo-notify](https://hackage.haskell.org/package/fdo-notify).

Differencies:
- wrapped into an [effect](https://hackage.haskell.org/package/effectful)
- added support for Action handlers
- `Generic` instances sprinkled over types
- some deprecated/unimplemented stuff removed

## Usage

Refer to the [test](./Main.hs) executable.

## Hacking
Enable binary cache[^1]:

```bash
cachix use elvecent-presonal
```

Enter the shell:

```bash
nix-shell --arg devMode true
```

The `devMode` argument disables library profiling and optimizations to *quickly*[^2] build a shell with `hoogle` and `stylish-haskell`.

[^1]: not that's not a typo
[^2]: time is relative
