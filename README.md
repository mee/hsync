# HSync (Work in Progess)

The goal here is to build a file server over http that clients can
pull patches from to stay in sync with the servers' files.

Inspired by zsync.

To use, run `make-test-files.sh` and then load `Hsync.hs` into ghci.

You can then fake-o patch one file to another with,

```haskell
patch "test/abc" "test/abc2"
```

This will show information about the files, their similarities (lcs)
and the generated patch (list).
