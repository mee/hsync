#!/bin/bash

mkdir test
cd test

dd if=/dev/urandom bs=4K count=1 of=a
dd if=/dev/urandom bs=4K count=1 of=b
dd if=/dev/urandom bs=4K count=1 of=c

cat a b c > abc
cat a b b > abb
cat b b b > bbb
cat b b c > bbc

cd ..
