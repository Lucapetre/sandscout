
allow(file-readSTAR, [subpath("/readpri/")]).
allow(file-readSTAR, [literal("/abc/ds")]).
allow(file-readSTAR, [subpath("/mobile/"),extension("ally"),extension("vigilance")]).
allow(file-readSTAR, [subpath("/mobile/"),extension("guard"),extension("vigilance")]).
allow(file-readSTAR, [subpath("/mobile/"),literal("/myfile"),extension("flying"),extension("vigilance")]).
allow(file-readSTAR, [subpath("/mobile/"),literal("/myfile"),extension("trample"),extension("vigilance")]).
allow(file-readSTAR, [subpath("/mobile/"),extension("ally"),extension("hexproof")]).
allow(file-readSTAR, [subpath("/mobile/"),extension("guard"),extension("hexproof")]).
allow(file-readSTAR, [subpath("/mobile/"),literal("/myfile"),extension("flying"),extension("hexproof")]).
allow(file-readSTAR, [subpath("/mobile/"),literal("/myfile"),extension("trample"),extension("hexproof")]).
