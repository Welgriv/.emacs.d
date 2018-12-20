# .emacs.d
my .emacs.d/ repo, with my customizations

To properly enable spell-check :
1) Download aspell at http://aspell.net/ (tar file : http://mirror.ibcp.fr/pub/gnu/aspell/)
2) uncompress SOMEWHERE -> go in SOMEWHERE -> ./configure --disable-curses --prefix=PATH_TO_ASPELL_BIN -> make -> make install
3) Set PATH_TO_ASPELL_BIN between the "" in the init.el for ispell
4) Download any dictionary at http://mirror.ibcp.fr/pub/gnu/aspell/dict/ (exemple tar file for EN : http://mirror.ibcp.fr/pub/gnu/aspell/dict/en/aspell6-en-2018.04.16-0.tar.bz2)
5) uncompress SOMEWHERE_D -> go in SOMEWHERE_D -> ./configure --vars ASPELL=PATH_TO_ASPELL_BIN/aspell -> make -> mak install
6) enjoye
