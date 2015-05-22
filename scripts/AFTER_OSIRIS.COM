$ set file/prot=(w:re) sys$scratch:AFTER_OSIRIS.log
$ set def USER$DISK:[SIC01.OSIRIS.GUIDE]
$ run s_g:spectra_norm_inter.exe
����������
