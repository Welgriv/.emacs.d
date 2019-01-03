# .emacs.d
My .emacs.d/ repo, with my customizations

## To properly enable spell-check :
1) Download aspell at http://aspell.net/ (tar file : http://mirror.ibcp.fr/pub/gnu/aspell/)
2) uncompress SOMEWHERE -> go in SOMEWHERE -> ./configure --disable-curses --prefix=PATH_TO_ASPELL_BIN -> make -> make install
3) Set PATH_TO_ASPELL_BIN between the "" in the init.el for ispell
4) Download any dictionary at http://mirror.ibcp.fr/pub/gnu/aspell/dict/ (exemple tar file for EN : http://mirror.ibcp.fr/pub/gnu/aspell/dict/en/aspell6-en-2018.04.16-0.tar.bz2)
5) uncompress SOMEWHERE_D -> go in SOMEWHERE_D -> ./configure --vars ASPELL=PATH_TO_ASPELL_BIN/aspell DESTDIR=PATH_TO_ASPELL_BIN/lib/ -> make -> make install (to be adapted...)
6) enjoy

## To use minted package in latex :
1) Download minted package at : https://ctan.org/pkg/minted
2) Exctract and run make to generate minted.sty
3) Copy minted.sty to TEXLIVEINSTALLDIR/texmf-local/tex/latex/local/
4) Run texhash
5) Enjoye
