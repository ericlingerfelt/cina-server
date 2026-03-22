MODULE inv_parm
  USE IFLPORT
  USE bind
  USE reactionstrings
  USE nuc_data
  USE constants

  PRIVATE
  PUBLIC   :: get_inv_parm

CONTAINS
  SUBROUTINE get_inv_parm(a,a_num,a_inv,r,del_a,status,err_msg)
! status == 0 if successful
    REAL(KIND=8),INTENT(IN)       :: a(:)
    INTEGER(KIND=4),INTENT(IN)    :: a_num
    REAL(KIND=8),INTENT(OUT)      :: a_inv(:),del_a(:)
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(OUT)   :: status
    CHARACTER(LEN=*),INTENT(OUT)  :: err_msg

    IF (MOD(a_num,7) /= 0) THEN
       WRITE(err_msg,'(A,I0,A)') 'The number of parameters ',a_num,' must be a multiple of 7'
    END IF

!WRITE(*,'(A,I0)') 'Reac type is ',getreactype(r)

    SELECT CASE (getreactype(r))
    CASE (4)
       CALL get_inv_4(a,a_num,a_inv,r,del_a,status,err_msg)
    CASE (5)
       CALL get_inv_5(a,a_num,a_inv,r,del_a,status,err_msg)
    CASE (6)
       CALL get_inv_6(a,a_num,a_inv,r,del_a,status,err_msg)
    CASE DEFAULT
       a_inv = 0.0
       del_a = 0.0
       status = -1
       err_msg = 'Calculation of inverse parameters for this reaction '// &
            'is not currently supported.'
    END SELECT
  END SUBROUTINE get_inv_parm

!---------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE get_inv_4(a,num_para,a_inv,r,del_a,status,err_msg)
! status == 0 if successful
! a + b -> c
    REAL(KIND=8),INTENT(IN)       :: a(:)
    INTEGER(KIND=4),INTENT(IN)    :: num_para
    TYPE(reactionparticles),INTENT(IN) :: r
    REAL(KIND=8),INTENT(OUT)      :: a_inv(:),del_a(:)
    INTEGER(KIND=4),INTENT(OUT)   :: status
    CHARACTER(LEN=*),INTENT(OUT)  :: err_msg
    INTEGER(KIND=4)               :: np1,np2,np3,np4,np5
    INTEGER(KIND=4)               :: nn1,nn2,nn3,nn4,nn5,iden1,iden2
    REAL(KIND=8)                  :: g1,g2,g3,bind,q
    REAL(KIND=8)                  :: m1,m2,m3
    
    status = -1
    err_msg = 'Unknown error in get_inv_4'

    np1 = getreac_z(r,1)
    np2 = getreac_z(r,2)
    np3 = getprod_z(r,1)
    np4 = getprod_z(r,2)
    np5 = getprod_z(r,3)
    nn1 = getreac_n(r,1)
    nn2 = getreac_n(r,2)
    nn3 = getprod_n(r,1)
    nn4 = getprod_n(r,2)
    nn5 = getprod_n(r,3)
    
    iden1=0
    iden2=0
    
    if (np3.eq.np4) then
       if (nn3.eq.nn4) iden2=iden2+1
    end if

    if (np3.eq.np5) then
       if (nn3.eq.nn5) iden2=iden2+1
    end if
    
    if (np4.eq.np5) then
       if (nn4.eq.nn5) iden2=iden2+1
    end if
    
    g1=2.0d0*nuc_spin(np1, nn1+np1)+1.0d0
    g2=2.0d0*nuc_spin(np2, nn2+np2)+1.0d0
    g3=2.0d0*nuc_spin(np3, nn3+np3)+1.0d0
    
!
! Particle #1 - together with particle #2, this is the 'incident' particle
! 		
    
    bind=binden(np1, nn1)
    bind=bind*1.07364d-6        ! Unit conversion to amu
    m1=dfloat(np1)*(MASS_P+MASS_E)+dfloat(nn1)*MASS_N-bind	! In amu
    
!
! Particle #2
!
    
    bind=binden(np2, nn2)
    bind=bind*1.07364d-6        ! Unit conversion to amu
    m2=dfloat(np2)*(MASS_P+MASS_E)+dfloat(nn2)*MASS_N-bind	! In amu
    
!
! Particle #3
!
    
    bind=binden(np3, nn3)
    bind=bind*1.07364d-6        ! Unit conversion to amu
    m3=dfloat(np3)*(MASS_P+MASS_E)+dfloat(nn3)*MASS_N-bind	! In amu
    
    
    
!
! Finding a_inv
!
    
    del_a = 0d0       ! Initialize del_a to zero

    q=nuc_m_excess(np1,np1+nn1)+nuc_m_excess(np2,np2+nn2) &
         -nuc_m_excess(np3,np3+nn3)
    del_a(1)=Log(0.98685d10*((g1*g2)/g3)*((m1*m2)/m3)**(3.0d0/2.0d0))
    del_a(2)=-q/((10.0d0**9.0d0)*BOLTZ)
    del_a(7)=1.5d0
    
!print*,q
!print*, del_a(1), del_a(2), del_a(7)
    
    do i=1, num_para
       a_inv(i)=a(i)
    end do
    
    do j=1, num_para/7
       a_inv(7*(j-1)+1)=a(7*(j-1)+1)+del_a(1)
       a_inv(7*(j-1)+2)=a(7*(j-1)+2)+del_a(2)
       a_inv(7*(j-1)+7)=a(7*(j-1)+7)+del_a(7)
    end do
    
! Copy del_a(1:7) to del_a(8:num_para)
    do i=2, num_para/7
! Copy del_a(1:7) to del_a(8:14), del_a(15:21), etc.
       del_a((i-1)*7+1 : i*7) = del_a(1:7)
    end do

    status = 0
    err_msg = ''
  END SUBROUTINE get_inv_4

!---------------------------------------------------------------------
  SUBROUTINE get_inv_5(a,num_para,a_inv,r,del_a,status,err_msg)
! status == 0 if successful
! a + b -> c + d
    REAL(KIND=8),INTENT(IN)       :: a(:)
    INTEGER(KIND=4),INTENT(IN)    :: num_para
    TYPE(reactionparticles),INTENT(IN) :: r
    REAL(KIND=8),INTENT(OUT)      :: a_inv(:),del_a(:)
    INTEGER(KIND=4),INTENT(OUT)   :: status
    CHARACTER(LEN=*),INTENT(OUT)  :: err_msg
    INTEGER(KIND=4)               :: np1,np2,np3,np4,np5
    INTEGER(KIND=4)               :: nn1,nn2,nn3,nn4,nn5,iden1,iden2
    REAL(KIND=8)                  :: g1,g2,g3,g4,bind,q
    REAL(KIND=8)                  :: m1,m2,m3,m4

    status = -1
    err_msg = 'Unknown error in get_inv_5'

    np1 = getreac_z(r,1)
    np2 = getreac_z(r,2)
    np3 = getprod_z(r,1)
    np4 = getprod_z(r,2)
    np5 = getprod_z(r,3)
    nn1 = getreac_n(r,1)
    nn2 = getreac_n(r,2)
    nn3 = getprod_n(r,1)
    nn4 = getprod_n(r,2)
    nn5 = getprod_n(r,3)
    
    iden1=0
    iden2=0
    
    if (np3.eq.np4) then
       if (nn3.eq.nn4) iden2=iden2+1
    end if

    if (np3.eq.np5) then
       if (nn3.eq.nn5) iden2=iden2+1
    end if
    
    if (np4.eq.np5) then
       if (nn4.eq.nn5) iden2=iden2+1
    end if
    
    g1=2.0d0*nuc_spin(np1, nn1+np1)+1.0d0
    g2=2.0d0*nuc_spin(np2, nn2+np2)+1.0d0
    g3=2.0d0*nuc_spin(np3, nn3+np3)+1.0d0
    g4=2.0d0*nuc_spin(np4, nn4+np4)+1.0d0
    
!
! Particle #1 - together with particle #2, this is the 'incident' particle
! 

    bind=binden(np1, nn1)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m1=dfloat(np1)*(MASS_P+MASS_E)+dfloat(nn1)*MASS_N-bind	! atomic mass in amu
    
!
! Particle #2
!

    bind=binden(np2, nn2)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m2=dfloat(np2)*(MASS_P+MASS_E)+dfloat(nn2)*MASS_N-bind	! In amu
    
!
! Particle #3
!

    bind=binden(np3, nn3)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m3=dfloat(np3)*(MASS_P+MASS_E)+dfloat(nn3)*MASS_N-bind	! In amu
    
!
! Particle #4
!
    
    bind=binden(np4, nn4)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m4=dfloat(np4)*(MASS_P+MASS_E)+dfloat(nn4)*MASS_N-bind	! In amu
    
!
!  Finding a_inv
!
    
    del_a = 0d0       ! Initialize del_a to zero

    q=nuc_m_excess(np1,np1+nn1)+nuc_m_excess(np2,np2+nn2) &
         -nuc_m_excess(np3,np3+nn3)-nuc_m_excess(np4,np4+nn4)
    
    del_a(1)=Log(((g1*g2)/(g3*g4))*((m1*m2)/(m3*m4))**(3.0d0/2.0d0))
    del_a(2)=-q/((10.0d0**9.0d0)*BOLTZ)
    
!print*,q
!print*, del_a(1), del_a(2)
    
    do i=1, num_para
       a_inv(i)=a(i)
    end do
    
    do j=1, num_para/7
       a_inv(7*(j-1)+1)=a(7*(j-1)+1)+del_a(1)
       a_inv(7*(j-1)+2)=a(7*(j-1)+2)+del_a(2)
    end do
    
! Copy del_a(1:7) to del_a(8:num_para)
    do i=2, num_para/7
! Copy del_a(1:7) to del_a(8:14), del_a(15:21), etc.
       del_a((i-1)*7+1 : i*7) = del_a(1:7)
    end do

    status = 0
    err_msg = ''
  END SUBROUTINE get_inv_5

!---------------------------------------------------------------------
  SUBROUTINE get_inv_6(a,num_para,a_inv,r,del_a,status,err_msg)
! status == 0 if successful
! a + b -> c + d + e
    REAL(KIND=8),INTENT(IN)       :: a(:)
    INTEGER(KIND=4),INTENT(IN)    :: num_para
    TYPE(reactionparticles),INTENT(IN) :: r
    REAL(KIND=8),INTENT(OUT)      :: a_inv(:),del_a(:)
    INTEGER(KIND=4),INTENT(OUT)   :: status
    CHARACTER(LEN=*),INTENT(OUT)  :: err_msg
    INTEGER(KIND=4)               :: np1,np2,np3,np4,np5
    INTEGER(KIND=4)               :: nn1,nn2,nn3,nn4,nn5,iden1,iden2
    REAL(KIND=8)                  :: g1,g2,g3,g4,g5,bind,q
    REAL(KIND=8)                  :: m1,m2,m3,m4,m5

    status = -1
    err_msg = 'Unknown error in get_inv_6'

    np1 = getreac_z(r,1)
    np2 = getreac_z(r,2)
    np3 = getprod_z(r,1)
    np4 = getprod_z(r,2)
    np5 = getprod_z(r,3)
    nn1 = getreac_n(r,1)
    nn2 = getreac_n(r,2)
    nn3 = getprod_n(r,1)
    nn4 = getprod_n(r,2)
    nn5 = getprod_n(r,3)
    
    iden1=0
    iden2=0
    
    if (np3.eq.np4) then
       if (nn3.eq.nn4) iden2=iden2+1
    end if

    if (np3.eq.np5) then
       if (nn3.eq.nn5) iden2=iden2+1
    end if
    
    if (np4.eq.np5) then
       if (nn4.eq.nn5) iden2=iden2+1
    end if
    
    g1=2.0d0*nuc_spin(np1, nn1+np1)+1.0d0
    g2=2.0d0*nuc_spin(np2, nn2+np2)+1.0d0
    g3=2.0d0*nuc_spin(np3, nn3+np3)+1.0d0
    g4=2.0d0*nuc_spin(np4, nn4+np4)+1.0d0
    g5=2.0d0*nuc_spin(np5, nn5+np5)+1.0d0
    
!
! Particle #1 - together with particle #2, this is the 'incident' particle
! 
    
    bind=binden(np1, nn1)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m1=dfloat(np1)*(MASS_P+MASS_E)+dfloat(nn1)*MASS_N-bind	! atomic mass in amu
    
!
! Particle #2
!
    
    bind=binden(np2, nn2)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m2=dfloat(np2)*(MASS_P+MASS_E)+dfloat(nn2)*MASS_N-bind	! In amu
    
!
! Particle #3
!
    
    bind=binden(np3, nn3)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m3=dfloat(np3)*(MASS_P+MASS_E)+dfloat(nn3)*MASS_N-bind	! In amu
    
!
! Particle #4
!
    
    bind=binden(np4, nn4)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m4=dfloat(np4)*(MASS_P+MASS_E)+dfloat(nn4)*MASS_N-bind	! In amu
    
!
! Particle #5
!
    
    bind=binden(np5, nn5)
    bind=bind*1.07364d-6	! Unit conversion to amu
    m5=dfloat(np5)*(MASS_P+MASS_E)+dfloat(nn5)*MASS_N-bind	! In amu
    
!
! Finding a_inv
!
    
    del_a = 0d0       ! Initialize del_a to zero

    q=nuc_m_excess(np1,np1+nn1)+nuc_m_excess(np2,np2+nn2) &
         -nuc_m_excess(np3,np3+nn3)-nuc_m_excess(np4,np4+nn4) &
         -nuc_m_excess(np5,np5+nn5)
    
    del_a(1)=Log(1.0d0/0.98685d10*(2.0d0**dfloat(iden2))* &
         ((g1*g2)/(g3*g4*g5))* &
         ((m1*m2)/(m3*m4*m5))**(3.0d0/2.0d0))
    del_a(2)=-q/((10.0d0**9.0d0)*BOLTZ)
    del_a(7)=1.5d0
    
!print*,q
!print*, del_a(1), del_a(2), del_a(7)
    
    do i=1, num_para
       a_inv(i)=a(i)
    end do
    
    do j=1, num_para/7
       a_inv(7*(j-1)+1)=a(7*(j-1)+1)+del_a(1)
       a_inv(7*(j-1)+2)=a(7*(j-1)+2)+del_a(2)
       a_inv(7*(j-1)+7)=a(7*(j-1)+7)+del_a(7)
    end do
    
! Copy del_a(1:7) to del_a(8:num_para)
    do i=2, num_para/7
! Copy del_a(1:7) to del_a(8:14), del_a(15:21), etc.
       del_a((i-1)*7+1 : i*7) = del_a(1:7)
    end do

    status = 0
    err_msg = ''
  END SUBROUTINE get_inv_6

!---------------------------------------------------------------------
END MODULE inv_parm
