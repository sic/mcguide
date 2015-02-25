$ set file/prot=(w:re) sys$scratch:FULLPOL.log
$ set def USER$DISK:[SIC01.OSIRIS.GUIDE]
$ run s_g:spectra_full_inter.exe
