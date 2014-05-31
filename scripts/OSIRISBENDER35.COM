$ set file/prot=(w:re) sys$scratch:OSIRISBENDER35.log
$ set def USER$DISK:[SIC01.OSIRIS.GUIDE]
$ run s_g:spectra_full_inter.exe
