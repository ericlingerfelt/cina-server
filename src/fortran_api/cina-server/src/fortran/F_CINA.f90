MODULE CINA
  INTERFACE
     ! In F_logs.cpp
     SUBROUTINE log_error(sessionp,client_msg,debug_msg,type,val,str,cvsid)
       CHARACTER,INTENT(IN) :: client_msg,debug_msg,str,cvsid
       INTEGER(KIND=4),INTENT(IN) :: sessionp
       INTEGER(KIND=4),INTENT(IN):: type,val
     END SUBROUTINE log_error

     ! In F_accounts.cpp
     SUBROUTINE get_account(sessionp,alias,email,err_msg)
       INTEGER(KIND=4),INTENT(INOUT) :: sessionp
       CHARACTER,INTENT(OUT):: alias,email,err_msg
     END SUBROUTINE get_account

     ! In F_sessions.cpp
     SUBROUTINE new_session(user,pw,request,body,id,sessionp,err_msg)
       CHARACTER,INTENT(IN) :: user,pw,request,body
       INTEGER(KIND=4),INTENT(INOUT) :: sessionp
       CHARACTER,INTENT(OUT):: id,err_msg
     END SUBROUTINE new_session

     SUBROUTINE close_session(sessionp,err_msg)
       INTEGER(KIND=4),INTENT(INOUT) :: sessionp
       CHARACTER,INTENT(OUT):: err_msg
     END SUBROUTINE close_session

     SUBROUTINE valid_session(user,pw,request,body,id,sessionp,err_msg)
       CHARACTER,INTENT(IN) :: user,pw,id,request,body
       INTEGER(KIND=4),INTENT(INOUT) :: sessionp
       CHARACTER,INTENT(OUT):: err_msg
     END SUBROUTINE valid_session

  END INTERFACE
END MODULE CINA

