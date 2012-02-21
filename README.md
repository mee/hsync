# HSync

An patch algorithm for delta-based update that's better than pulling
the whole file every time, but still pretty dumb. Inspired by zsync.

To use, run `make-test-files.sh` and then load `Hsync.hs` into ghci.

You can then patch one file to another with,

```haskell
patch "test/abc" "test/abc2"
```

This will show information about the files, their similarities (lcs)
and the generated patch (list).
