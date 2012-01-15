ARCH=$(shell uname -m)
OS=$(shell uname -s | tr '[A-Z]' '[a-z]')

xmonad-$(ARCH)-$(OS): xmonad.hs
	ghc -O2 --make xmonad.hs -o $@
	strip -s $@
