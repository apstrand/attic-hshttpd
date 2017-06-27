
all:	hbc

HBCFLAGS = -D__HBC__ -fccall -I../hsos -I../ffi
LIBS = ../hsos/c/libos.a

hbc:
	hmake -i../hsos -i../ffi -hbc httpd $(HBCFLAGS) $(LIBS)


GHCFLAGS =  -D__GHC__ -fglasgow-exts

ghc:
	hmake -i../hsos -i../ffi -ghc httpd $(GHCFLAGS) $(LIBS)

nhc:
	hmake -i. -i../hsos -i../ffi httpd $(LIBS)



clean:
	rm -f *.hi *.o


