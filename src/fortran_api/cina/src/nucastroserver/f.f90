      Program printer
      use ifport

      character val*40
      integer length,status
      integer(4) rtn


      call GET_ENVIRONMENT_VARIABLE('FOR_PRINT',val,length,status)
      print *,val,length,status
      print '(A)','Hello, World!'
      rtn=system('./baddy')
      print '(A,I0,A,I0)','Status of the command is: ',isha(rtn,-8),' errno: ',ierrno()
      rtn=system('./goody')
      print '(A,I0,A,I0)','Status of the command is: ',rtn,' errno: ',ierrno()
      rtn=system('bigbadjohn &> /dev/null')
      status=ierrno()
      print '(A,I0,A,I0)','Status of the command is: ',isha(rtn,-8),' errno: ',status

      end program
