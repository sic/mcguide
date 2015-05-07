$ ds=f$environment("default")
$ wso " "
$ wso " Current directory is ''ds' "
$ wso " "
$ inquire changedir " Directory to change to "
$ set def 'changedir'
$ wso " "
$ ds=f$environment("default")
$ wso " "
$ wso " Current directory is ''ds' "
$ wso " "
$ inquire query "Type RETURN to continue" 
