`ghc -O2 -threaded -rtsopts -eventlog fibExample.hs`
`time ./fibExample +RTS -N3 -l -K100M -H300M -Sn2.txt`
