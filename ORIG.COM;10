$ wso = "write sys$output "
$ wso "[2J[H" 			! CLEAR SCREEN
$ wso "#3  Original MCGUIDE "
$ wso "#4  Original MCGUIDE "
$ wso " "
$ ds=f$environment("default")
$ DISK:
$ inquire disk " Disk where simulation is to be run (eg user$disk) ? "
$ if (f$length(disk).lt.1) then goto DISK
$ DIRECT:
$ inquire area " Directory in which to run the simulation (eg [username.guide]) ? "
$ if (f$length(area).lt.1) then goto DIRECT
$ if (f$extract(0,1,area).eqs."[") then goto S1
$ area="["+area
$ S1:
$ if (f$extract(f$length(area)-1,1,area).eqs."]") then goto CONT
$ area=area+"]"
$ CONT:
!
$ ds=disk+":"+area
!
$ inquire name " Name of Simulation ? "
$ if (f$length(area).lt.1) then goto CONT
$ open/write commfile 'name'.com
$ write/symbol commfile "$ set file/prot=(w:re) sys$scratch:''name'.log"
$ write/symbol commfile "$ set def ''ds'"
$ write/symbol commfile "$ run s_g:spectra_orig.exe"
$ close commfile
$ wso " "
!
$ batque="sys$batch"
$ text=" Default Batch queue is "+f$string(batque)
$ wso text
$ inquire que  " Batch queue ? "
$ if (f$length(que).lt.1) then que=batque
$ submit/que='que'/notify/noprint/keep/log=sys$scratch:'name'.log/name='name' 'name'.com
$ sh que 'que'/bat
$ wso " "
$ text = " Job is run by file ''name'.COM"
$ wso text
$ text = " Log of job is in file sys$scratch:''name'.LOG"
$ wso text
!
$ inquire query " Type RETURN to continue "
$ exit
