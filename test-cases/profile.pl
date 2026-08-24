
allow(file-readSTAR, [subpath("/readpri/")]).
allow(file-readSTAR, [literal("/abc/ds")]).
allow(file-readSTAR, [subpath("/private/var/"),extension("librarian"),require-not(regex("^/reggie1$"/i)),require-not(regex("^/reggie2$"/i))]).
allow(file-readSTAR, [subpath("/mobile/"),extension("ally")]).
allow(file-readSTAR, [subpath("/mobile/"),extension("guard")]).
allow(file-readSTAR, [subpath("/Media/"),require-entitlement("signing",[entitlement-value("safari")])]).
allow(file-readSTAR, [subpath("/Media/"),require-entitlement("signing",[entitlement-value("webapp")])]).
allow(file-writeSTAR, [subpath("/writepri/")]).
allow(file-writeSTAR, [literal("/abc/ds")]).
allow(file-writeSTAR, [regex("^/Sys1$"/i)]).
allow(file-writeSTAR, [regex("^/Sys2$"/i)]).
allow(file-writeSTAR, [subpath("/private/var/"),extension("librarian")]).
allow(file-writeSTAR, [subpath("/mobile/"),extension("ally")]).
allow(file-writeSTAR, [subpath("/mobile/"),extension("guard")]).
allow(file-writeSTAR, [subpath("/Media/"),require-entitlement("signing",[entitlement-value("safari")])]).
allow(file-writeSTAR, [subpath("/Media/"),require-entitlement("signing",[entitlement-value("webapp")])]).
