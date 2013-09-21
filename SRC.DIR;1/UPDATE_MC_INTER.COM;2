$ wso:==write sys$output
$ wso "Skipping compilation of Original MCGUIDE"                                               
$! @spectra_orig
$! rename spectra.exe s_g:spectra_orig.exe
$ wso "Skipping compilation of Polarizing Interpolation"
$! @spectra_full_inter
$! rename spectra_pol.exe s_g:spectra_full_inter.exe
$ wso "Compiling Normal Interpolation"         
$ @spectra_norm_inter
$ rename spectra.exe s_g:spectra_norm_inter.exe
$ wso "Skipping compilation of Step Approx. to Reflectivity"
$! @spectra_step
$! rename spectra.exe s_g:spectra_step.exe
$ wso "Skipping compilation of Grid output"
$! @spectra_grid
$! rename spectra.exe s_g:spectra_grid.exe                                               
$ wso "Clearing object files..."
$ delete *.obj;1
