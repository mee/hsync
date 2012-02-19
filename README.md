# HSync

An patch algorithm for delta-based update that's better than pulling
the whole file every time, but still pretty dumb.

To use, run `make-test-files.sh` and then load `Hsync.hs` into ghci.

You can then patch one file to another with,

```haskell
test "test/abc" "test/abc2"
```

This will patch `abc2` to match `abc`
