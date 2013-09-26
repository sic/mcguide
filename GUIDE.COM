$ ass/nolog sys$command sys$input
$ wso = "write sys$output "
$ on control_y then goto start
$ start:
$ wso "[2J[H" 			! CLEAR SCREEN
$ wso "#3  Monte Carlo Guide Simulations "
$ wso "#4  Monte Carlo Guide Simulations "
$ wso " "
$ options:
$ wso " "
$ wso " Choices are.."
$ wso " "
$ wso " 1 - Builder"
$ wso " 2 - Original MCGUIDE"
$ wso " 3 - Full Interpolation Treatment"
$ wso " 4 - Interpolation on Normal Supermirror surfaces only"
$ wso " 5 - Contour of Exit & Sample"
$ wso " 6 - Benders with zero substrate thickness"
$ wso " 7 - No Interpolation - Step Reflectivity Approximation"
$ wso " 8 - Plot Results"
$ wso " 9 - Utilities"
$ wso " "
$ wso " 0 - Leave"
$ wso " "
$ inquire menu " What do you want to do? "
$ if (f$length(menu).lt.1) then goto start
$ goto o'menu'
!
$ o1:
$ wso "[2J[H" 			! CLEAR SCREEN
$ wso "#3  Guide Builder"
$ wso "#4  Guide Builder"
$ wso " "
$ wso " 1 - Run Builder "
$ wso "     (only possible if a valid br.in file exists in working directory)"
$ wso " "
$ wso " 2 - Edit BR.IN file"
$ wso " 3 - Edit BR.DAT file - Do not do unless you are sure of what you are doing!"
$ wso " "
$ wso " 4 - Create new guide"
$ wso " "
$ wso " Press RETURN to go back to Main Menu"
$ wso " "
!
$ inquire subm " What do you want to do? "
$ if (f$length(subm).lt.1) then goto end
$ goto build'subm'
!
$ build1:
$ run s_g:br.exe
$ goto o1
!
$ build2:
$ edit br.in
$ goto o1
!                            
$ build3:
$ edit br.dat
$ goto o1                    
!
$ build4:
$ @s_g:makeguide
$ goto o1                    
!
$ goto end
!
$ o2:					! Original Program (ish!)
$ @s_g:orig
$ goto end
!
$ o3:					! Full interpolation
$ @s_g:full_inter
$ goto end
!
$ o4:					! interpolation for normal surfaces only
$ @s_g:inter
$ goto end
!
$ o5:					! grid output
$ @s_g:grid
$ goto end
!
$ o6:					! Bender - zero thickness substrate
$ @s_g:no_thick
$ goto end
!
$ o7:					! No interpolation - step function for reflectivity
$ wso "[2J[H" 			! CLEAR SCREEN
$ wso "#3  Sorry....not available yet! "
$ wso "#4  Sorry....not available yet! "
$ wso " "
$ inquire query "Type RETURN to continue"
$ goto end
!
$ o8:					! write pgplot program 
$ wso "[2J[H" 			! CLEAR SCREEN
$ wso "#3  Sorry....not available yet! "
$ wso "#4  Sorry....not available yet! "
$ wso " "
$ inquire query "Type RETURN to continue"
$ goto end
!
$ o9:                                   ! Number of utility routines
$ wso "[2J[H" 			! CLEAR SCREEN
$ wso "#3  General Utilities "
$ wso "#4  General Utilities "
$ wso " "
$ wso "1 - Display current working directory "
$ wso "2 - Change working directory "
$ wso " "
$ inquire menu " What do you want to do? "
$ wso " "
$ if (f$length(menu).lt.1) then goto end
$ goto util'menu'
!
$ UTIL1:
$ @s_g:shodir.com		! displays the current directory
$ goto end
!
$ UTIL2:
$ @s_g:setdir.com               ! changes the current directory
$ goto end
!
$ end:
$ wso "[2J[H" 			! CLEAR SCREEN
$ goto start
!
$ o0:
$ exit
