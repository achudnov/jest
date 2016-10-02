#!/bin/sed -nf
# Extract GHC-style notes from a Haskell source text

/{- Note/b cmt
b
:cmt
/-\}/!{
N
b cmt
}
p