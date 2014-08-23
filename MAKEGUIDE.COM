! Creates the files BR.IN, OPTIONS.IN and BENDER.IN
$ wso:==write sys$output
$ wso "[2J[H" 			! CLEAR SCREEN
$ wso "#3  Create New Guide "
$ wso "#4  Create New Guide "
$ wso " "
$ open/write brfile br.in
$ open/write optfile options.in
!
$ inquire name " Name of Guide ? "
$ wso " "
$ inquire times " How many times to loop simulation ? (Useful minimum value is 3)"
!
!Writing options.in
$ write/symbol optfile " ' ''name' ' "
$ write/symbol optfile "guideopt (1 if a guide is used), ntimes"
$ write/symbol optfile "1, ''times'"
$ close optfile 
!
$ inquire nhist " How many neutrons for each loop ? "
!
$ write/symbol brfile "Name of the guide"
$ write/symbol brfile name
$ inquire reg " How many regions does the guide consist of ? "
$ N=0
$ write/symbol brfile "Total number of regions used"
$ write/symbol brfile reg
!
$ START:
$ N = N + 1
!                     
$ write/symbol brfile "******** REGION ''N' ********"
$ if(N.gt.1) then goto DIM
!
$ write/symbol brfile "Entrance of guide coordinates: x1, y1"
$ inquire x " x - coordinate of the guide entrance? "
$ inquire y " y - coordinate of the guide entrance? "
$ write/symbol brfile "''x', ''y' "
$ write/symbol brfile "Angle between guide axis and "Y" axis in radians "  
$ inquire thet " Angle between guide axis and "Y" axis in radians "  
$ write/symbol brfile thet
!
$ DIM:
$ wso " "
$ wso " Guide Dimensions for region ''N': "
$ wso " --------------------------------- "
$ inquire length " Length "
$ inquire width " Width  " 
$ inquire height " Height "
$ inquire rad " Radius of Curvature (Enter Zero for a non-curved section) "
$ write/symbol brfile "Length, Width, Height and Radius of Curvature"
$ write/symbol brfile "for ''N' section of the guide"
$ write/symbol brfile "''length', ''width', ''height', ''rad' "
$ wso " "
!
$ if(N.gt.1) then goto SEPQUE
!
$ write/symbol brfile "Number of neutrons"
$ write/symbol brfile nhist
$ wso " "
!
$ MODERATOR:
$ wso " MODERATOR Dimensions : "  
$ inquire xm " Width  "
$ inquire ym " Height "
!$ if (f$length(xm).lt.1 .or. f$length(ym).lt.1 ) then 
!$ wso " "
!$ wso " **** One or both moderator Dimensions are invalid **** Please Re-enter values **** "
!$ wso " "
!$ goto MODERATOR
!$ endif
!
$ wso " "
$ SAMPLE:
$ wso " SAMPLE Dimensions : "  
$ inquire xs " Width  "
$ inquire ys " Height "
!$ if (f$length(xs).lt.1 .or. f$length(ys).lt.1 ) then 
!$ wso " "
!$ wso " **** One or both sample dimensions are invalid **** Please Re-enter values **** "
!$ wso " "
!$ goto SAMPLE
!$ endif
$ write/symbol brfile "Moderator and Sample dimensions"
$ write/symbol brfile "''xm', ''ym', ''xs', ''ys' "
!
$ wso " "
$ inquire nw " How many wavelengths ? "
$ inquire wavi " Minimum wavelength   "
$ inquire step " Wavelength Increment "
$ write/symbol brfile "Number of wavelengths, first wavelength and wavelength increment"
$ write/symbol brfile "''nw', ''wavi', ''step' "
$ wso " "
$ inquire phi " Angle between moderator plane and guide entrance in degrees "
$ write/symbol brfile "Angle between moderator plane and guide entrance in degrees "
$ write/symbol brfile "''phi' "
$ wso " "  
$ inquire tp " Angular range over which neutrons are accepted (times the critical angle) "
$ write/symbol brfile "Angular range over which neutrons are accepted (times the critical angle)."
$ write/symbol brfile tp
$ wso " "
$ inquire sep " Separation between guide exit and sample plane "
!
$ SEPQUE:
$ write/symbol brfile "Separation between guide exit and sample plane "
$ write/symbol brfile sep
$ wso " "
$ wso " Guide Coating Parameters : "
$ wso " -------------------------- "
!
$ FLAG1:
!$ if (f$length(wav).gt.1) then
!$ oldwav = wav 
!$ endif
$ inquire wav " Surface waviness (Previous value = ''oldwav')"
!!$ if (f$length(wav).lt.1) then
!$ wav = oldwav
!!$ goto FLAG1
!!$ endif 
$ FLAG2:
$ inquire rc " Reflection Coefficient (''rc')"
!!$ if (f$length(rc).lt.1) then goto FLAG2
$ FLAG3:
$ inquire std " Std. Dev. of Abutment Error (''std')"
!!$ if (f$length(std).lt.1) then goto FLAG3
$ FLAG4:
$ inquire gl " Length of glass sections within region (''gl')"
!!$ if (f$length(gl).lt.1) then goto FLAG4
$ FLAG5:
$ inquire m " Critical angle per unit wavelength (''m')"
!!$ if (f$length(m).lt.1) then goto FLAG5
!
$ write/symbol brfile "Surface waviness, Reflection coefficient, S.D. of abutment error"
$ write/symbol brfile "length of glass sections within region and"
$ write/symbol brfile "critical angle per unit wavelength"
$ write/symbol brfile "''wav', ''rc', ''std', ''gl', ''m' "
$ wso " "
$ QU1:
$ inquire bdans " Is this region a supermirror bender ? (Y/N) "
$ if (f$length(bdans).lt.1) then goto QU1
$ if (f$length(bdans).gt.1) then goto QU1
$ write/symbol brfile "Is this region a supermirror bender ? "
$ write/symbol brfile " ''bdans' "
$ wso " "
$ QU2:
$ inquire refans " Is this a reflective region ? (Y/N) "
$ if (f$length(refans).lt.1) then goto QU2
$ if (f$length(refans).gt.1) then goto QU2
$ write/symbol brfile "Is this a reflective region? "
$ write/symbol brfile " ''refans' "
$ wso " "
$ if(bdans.eq."N") then goto CONT
$ QU3:
$ inquire subst " How many internal substrates are in the bender ? "
$ if (f$length(subst).lt.1) then goto QU3
$ write/symbol brfile "How many internal dividers in the bender ? "
$ write/symbol brfile "''subst'"
$ wso " "
$ wso " Bender Coating Parameters : "
$ wso " -------------------------- "
$ QU3A:
$ inquire rc " Reflection Coefficient (''rc')"
$ if (f$length(rc).lt.1) then goto QU3A
$ QU3B:
$ inquire m " Critical angle per unit wavelength (''m')"
$ if (f$length(m).lt.1) then goto QU3B
$ QU3C:
$ inquire pc " Polarization Coeeficient (1=polarizing, 0=non=polarizing) "
$ if (f$length(pc).lt.1) then goto QU3C
$ QU3D:
$ inquire oa " Orientation angle of bender w.r.t previous region (degrees) (''oa')"
$ if (f$length(oa).lt.1) then goto QU3D
!
$ write/symbol brfile "Surface waviness, Reflection coefficient, S.D. of abutment error"
$ write/symbol brfile "''m', ''rc', ''pc', ''oa' "
$ wso " "
!
$ CONT:
$ inquire defans " Is this region a deflector ? (Y/N) "
$ if (f$length(defans).lt.1) then goto CONT
$ if (f$length(defans).gt.1) then goto CONT
$ write/symbol brfile "Is this region a deflector? "
$ write/symbol brfile defans
$ wso " "
$ QU4:
$ inquire tapeans " Is this region curved or tapered ? (Y/N) "
$ if (f$length(tapeans).lt.1) then goto QU4
$ if (f$length(tapeans).gt.1) then goto QU4
$ write/symbol brfile "Is this region curved or tapered? "
$ write/symbol brfile tapeans
$ if (tapeans.eq."Y".and.rad.ne.0) then goto QU5
$ if (tapeans.eq."N") then goto QU5
$ TAPER:
$ inquire xex " Width of guide exit ? "
$ if (f$length(xex).lt.1) then goto TAPER
$ inquire yex " Height of guide exit ? "
$ if (f$length(yex).lt.1) then goto TAPER
$ write/symbol brfile "Width and Height of guide exit"
$ write/symbol brfile "''xex', ''yex' "
!
$ QU5:
$ if(N.eq.reg) then goto LAST
$ inquire anoans " Another Region (Y) or (R)epeat Region ? (Y/R) "
$ if (f$length(anoans).lt.1) then goto QU5
$ if (f$length(anoans).gt.1) then goto QU5
$ write/symbol brfile "Another region or repeat the last one? "
$ write/symbol brfile anoans
$ if (anoans.eq."Y") then goto START
!
$ N = N + 1
$ write/symbol brfile "******** REGION ''N' ********"
$ write/symbol brfile "Length, Width, Height and Radius of Curvature"
$ write/symbol brfile "for ''N' section of the guide"
$ write/symbol brfile "''length', ''width', ''height', ''rad' "
$ write/symbol brfile "Separation between guide exit and sample plane "
$ write/symbol brfile sep
$ write/symbol brfile "Surface waviness, Reflection coefficient, S.D. of abutment error"
$ write/symbol brfile "length of glass sections within region and"
$ write/symbol brfile "critical angle per unit wavelength"
$ write/symbol brfile "''wav', ''rc', ''std', ''gl', ''m' "
$ write/symbol brfile "Is this region a supermirror bender ? "
$ write/symbol brfile bdans
$ write/symbol brfile "Is this a reflective region? "
$ write/symbol brfile refans
$ if (bdans.eq."N") then goto REP1
$ write/symbol brfile "How many internal dividers in the bender ? "
$ write/symbol brfile subst
$ write/symbol brfile "Surface waviness, Reflection coefficient, S.D. of abutment error"
$ write/symbol brfile "''m', ''rc', ''pc', ''oa' "
$ REP1:
$ write/symbol brfile "Is this region a deflector? "
$ write/symbol brfile defans
$ write/symbol brfile "Is this region curved or tapered? "
$ write/symbol brfile tapeans
$ goto QU5
!
$ LAST:
$ write/symbol brfile "Another region or repeat the last one? "
$ write/symbol brfile "N"
$ close brfile
