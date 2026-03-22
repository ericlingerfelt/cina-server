! PURPOSE
!  Create binary files of xnet simulation data for the Element Synthesis
!  Visualizer in the Computational Infrastructure for Nuclear Astrophysics.
!  Only the net.f file needs to be modified to use this module.
!  A "USE save_em_sim" statement should be added at the beginning of the net
!  program and ts_output subroutine.
!  
!  save_em_sim_isomap() should be called after nuclear data has been loaded
!                       and before the loop through each zone.  This saves
!                       a list of the isotopes used in the simulation.  It 
!                       only needs to be called once regardless of the 
!                       number of zones.
!  
!  save_em_sim_reacmap() should be called after flux_init has been called
!                       and before the loop through each zone.  It only 
!                       needs to be called once regardless of the number
!                       of zones.
!
!  save_em_sim_init()   should be called inside the loop through each zone
!                       before full_net is called.  This opens all files
!                       used by save_em_sim_tstep for the current zone.
!  
!  save_em_sim_tstep()  should be called in ts_output after fluxes have
!                       been calculated.  This saves data for the current
!                       time step to the binary files.
!  
!  save_em_sim_close()  should be called inside the loop through each zone
!                       after full_net is called.  This closes all files
!                       used by save_em_sim_tstep for the current zone.
!  
!  The read_em_sim module provides procedures for reading the binary files
!  created by this module.
! NOTES
!  This module does not modify any data in xnet.  All input variables are
!  declared as INTENT(IN) for safety.
!
!  xnet resets the step number to 1 when switching to weak reactions.
!  This causes problems with the infrastructure so this module saves the
!  step number as one more than the previous step number regardless if
!  only weak reactions are used.
! COPYRIGHT
!  Copyright 2004 Astrophysics Data Team, Physics Division,
!  Oak Ridge National Laboratory.  All rights reserved.
!  This program is for internal, private use only.
! SOURCE

MODULE save_em_sim
! Load all interfaces to all procedures in Intel's portability library.
! This library contains non-standard but common FORTRAN procedures,
! such as FTELL()
  USE IFLPORT

  PUBLIC

! Parameters global to this module
  INTEGER,PARAMETER :: EDOT_UNIT = 52
  INTEGER,PARAMETER :: ISOTOPE_MAPPING_UNIT = 63
  INTEGER,PARAMETER :: TIME_MAPPING_UNIT =    64
  INTEGER,PARAMETER :: THERMO_PROFILE_UNIT =  65
  INTEGER,PARAMETER :: ABUNDANCES_UNIT =      66
  INTEGER,PARAMETER :: ABUNDANCE_COUNT_UNIT = 67
  INTEGER,PARAMETER :: TIME_STEP_COUNT_UNIT = 68
  INTEGER,PARAMETER :: REAC_MAPPING_UNIT =    69
  INTEGER,PARAMETER :: FLUX_UNIT =            70
  INTEGER,PARAMETER :: FLUX_COUNT_UNIT =      71

CONTAINS
!***

!****is* save_em_sim/save_em_sim_isomap
! NAME
!  SUBROUTINE save_em_sim_isomap(bin_file_base)
! PURPOSE
!  Save a list of isotopes and their integer mapping to the 'isomap' file
! STATUS
!  Complete and tested
! USES
!  MODULES: nuclear_data
! USAGE
!  This procedure should be called after nuclear data has been loaded
!  and before the loop through each zone.  It only needs to be called
!  once regardless of the number of zones.
! INPUTS
!  bin_file_base: prefix or path to 'isomap' file
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

  SUBROUTINE save_em_sim_isomap(bin_file_base)
    USE nuclear_data
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: bin_file_base
    INTEGER(KIND=2)               :: z, a
    INTEGER                       :: i
    CHARACTER(LEN=5)              :: new_name

! Save isotope mapping
    OPEN(ISOTOPE_MAPPING_UNIT, FILE=TRIM(bin_file_base) // 'isomap', &
         ACTION='WRITE', FORM='UNFORMATTED', STATUS='REPLACE')

    DO i = 1, ny
       z = zz(i)
       a = aa(i)
       new_name = ADJUSTL(nname(i))
       IF (z > 1) THEN
! Make first letter of symbol uppercase
          new_name(1:1) = ACHAR(IACHAR(new_name(1:1)) - 32)
       END IF

       WRITE(ISOTOPE_MAPPING_UNIT) i-1, z, a, new_name
    END DO

    CLOSE(ISOTOPE_MAPPING_UNIT)
  END SUBROUTINE save_em_sim_isomap
!***

!****is* save_em_sim/save_em_sim_reacmap
! NAME
!  SUBROUTINE save_em_sim_reacmap(bin_file_base)
! PURPOSE
!  Save a list of reactions and their integer mapping to the 'reacmap' file
! STATUS
!  Complete and tested
! USES
!  MODULES: match_data, flux_data
! USAGE
!  This procedure should be called after flux_init has been called
!  and before the loop through each zone.  It only needs to be called
!  once regardless of the number of zones.
! INPUTS
!  bin_file_base: prefix or path to 'reacmap' file
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/02/2005 jpscott       Started subroutine
! SOURCE

  SUBROUTINE save_em_sim_reacmap(bin_file_base)
    USE match_data
    USE flux_data
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: bin_file_base
    INTEGER                       :: i

! Save reaction mapping
    OPEN(REAC_MAPPING_UNIT, FILE=TRIM(bin_file_base) // 'reacmap', &
         ACTION='WRITE', FORM='UNFORMATTED', STATUS='REPLACE')

    DO i = 1, mflx
       WRITE(REAC_MAPPING_UNIT) i-1, nflx(1:7,i)-1, descx(i), iwflx(i)
    END DO

    CLOSE(REAC_MAPPING_UNIT)
  END SUBROUTINE save_em_sim_reacmap
!***

!****is* save_em_sim/save_em_sim_init
! NAME
!  SUBROUTINE save_em_sim_init(bin_file_base, izone, nzone)
! PURPOSE
!  Open binary files used in save_em_sim_tstep()
! STATUS
!  Complete and tested
! USAGE
!  This procedure should be called inside the loop through each zone 
!  before full_net is called.
! INPUTS
!  bin_file_base: prefix or path to 'isomap' file
!  izone: number of current zone
!  nzone: total number of zones in this simulation
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
!  08/17/2005 jpscott       Now opens flux and flux_cnt files
! SOURCE

  SUBROUTINE save_em_sim_init(bin_file_base, ev_file_base, izone, nzone)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: bin_file_base
    CHARACTER(LEN=*),INTENT(IN)   :: ev_file_base
    INTEGER,INTENT(IN)            :: izone, nzone
    CHARACTER(LEN=20)             :: filename

    filename = 'ev'
    CALL name_ordered(filename,izone,nzone)
    OPEN(EDOT_UNIT, FILE=TRIM(filename), &
         ACTION='WRITE', STATUS='REPLACE')

    filename = 'timemap'
    CALL name_ordered(filename,izone,nzone)
    OPEN(TIME_MAPPING_UNIT, FILE=TRIM(bin_file_base) // TRIM(filename), &
         ACTION='WRITE', FORM='UNFORMATTED', STATUS='REPLACE')

    filename = 'thermo'
    CALL name_ordered(filename,izone,nzone)
    OPEN(THERMO_PROFILE_UNIT, FILE=TRIM(bin_file_base) // TRIM(filename), &
         ACTION='WRITE', FORM='UNFORMATTED', STATUS='REPLACE')

    filename = 'abund'
    CALL name_ordered(filename,izone,nzone)
    OPEN(ABUNDANCES_UNIT, FILE=TRIM(bin_file_base) // TRIM(filename), &
         ACTION='WRITE', FORM='UNFORMATTED', STATUS='REPLACE')

    filename = TRIM(filename) // '_cnt'
    OPEN(ABUNDANCE_COUNT_UNIT, FILE=TRIM(bin_file_base) // TRIM(filename), &
         ACTION='WRITE', FORM='UNFORMATTED', STATUS='REPLACE')

    filename = 'flux'
    CALL name_ordered(filename,izone,nzone)
    OPEN(FLUX_UNIT, FILE=TRIM(bin_file_base) // TRIM(filename), &
         ACTION='WRITE', FORM='UNFORMATTED', STATUS='REPLACE')

    filename = TRIM(filename) // '_cnt'
    OPEN(FLUX_COUNT_UNIT, FILE=TRIM(bin_file_base) // TRIM(filename), &
         ACTION='WRITE', FORM='UNFORMATTED', STATUS='REPLACE')

    filename = 'step_num'
    CALL name_ordered(filename,izone,nzone)
    OPEN(TIME_STEP_COUNT_UNIT, FILE=TRIM(bin_file_base) // TRIM(filename), &
         ACTION='READWRITE', FORM='UNFORMATTED', STATUS='REPLACE')

  END SUBROUTINE save_em_sim_init
!***

!****is* save_em_sim/save_em_sim_tstep
! NAME
!  SUBROUTINE save_em_sim_tstep(kstep)
! PURPOSE
!  Saves data for the current time step to binary files.
! STATUS
!  Complete and tested, fluxes are not saved in this version
! USES
!  MODULES: conditions, nuc_number, abundances
! USAGE
!  This procedure should be called in ts_output after fluxes have been
!  calculated.
! INPUTS
!  kstep: current time step number
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
!  08/17/2005 jpscott       Now saves flux and flux_cnt files
! SOURCE

  SUBROUTINE save_em_sim_tstep(kstep,kstep_used,edot)
    USE conditions
    USE controls
    USE nuc_number
    USE abundances
    USE match_data
    USE flux_data
    IMPLICIT NONE
    INTEGER,INTENT(IN)            :: kstep
    INTEGER,INTENT(OUT),OPTIONAL  :: kstep_used
    INTEGER                       :: i, old_kstep, kstep_out
    INTEGER                       :: nonzero_count, file_position
    REAL(KIND=8)                  :: flxmin,edot

! Save current step number by reading previous value and comparing
    REWIND(TIME_STEP_COUNT_UNIT)
    READ(TIME_STEP_COUNT_UNIT, IOSTAT=i) old_kstep
    IF (i /= 0) old_kstep = -1   ! Check if the first step

    IF (kstep <= old_kstep) THEN
       kstep_out = old_kstep + 1
    ELSE
       kstep_out = kstep
    END IF

    IF (PRESENT(kstep_used)) kstep_used = kstep_out

! Write step number
    REWIND(TIME_STEP_COUNT_UNIT)
    WRITE(TIME_STEP_COUNT_UNIT) kstep_out

! Write edot values
    WRITE(EDOT_UNIT, "(ES15.5)") edot

! Write time mapping
    WRITE(TIME_MAPPING_UNIT) kstep_out,t

! Write termo profile
    WRITE(THERMO_PROFILE_UNIT) kstep_out,t9t,rhot

! Write and count the number of nonzero abundances
! Also save the file_position so visualizer may jump to the abundances
! for any time step without having to read all of them in
    file_position = FTELL(ABUNDANCES_UNIT)
    WRITE(ABUNDANCES_UNIT) kstep_out
    nonzero_count = 0
    DO i = 1, ny
       IF (y(i) /= 0D0) THEN
          nonzero_count = nonzero_count + 1
          WRITE(ABUNDANCES_UNIT) i-1, y(i)
       END IF
    END DO

    WRITE(ABUNDANCE_COUNT_UNIT) kstep_out, nonzero_count, file_position

! Write and count the number of nonzero fluxes
! Also save the file_position so visualizer may jump to the fluxes
! for any time step without having to read all of them in
    file_position = FTELL(FLUX_UNIT)
    WRITE(FLUX_UNIT) kstep_out
    nonzero_count = 0
    If (tdel/=0.0) Then 
      flxmin = ymin / tdel
    Else
      flxmin = 1e-40
    Endif
    DO i = 1, mflx
       IF (ABS(flx(i)) > flxmin) THEN
          nonzero_count = nonzero_count + 1
          WRITE(FLUX_UNIT) i-1, flx(i)
       END IF
    END DO

    WRITE(FLUX_COUNT_UNIT) kstep_out, nonzero_count, file_position
  END SUBROUTINE save_em_sim_tstep
!***

!****is* save_em_sim/save_em_sim_close
! NAME
!  SUBROUTINE save_em_sim_close()
! PURPOSE
!  Close binary files used in save_em_sim_tstep()
! STATUS
!  Complete and tested
! USAGE
!  This procedure should be called inside the loop through each zone
!  after full_net is called.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
!  08/17/2005 jpscott       Now closes flux and flux_cnt files
! SOURCE

  SUBROUTINE save_em_sim_close()
    IMPLICIT NONE

    CLOSE(EDOT_UNIT)
    CLOSE(TIME_MAPPING_UNIT)
    CLOSE(THERMO_PROFILE_UNIT)
    CLOSE(ABUNDANCES_UNIT)
    CLOSE(ABUNDANCE_COUNT_UNIT)
    CLOSE(TIME_STEP_COUNT_UNIT)
    CLOSE(FLUX_UNIT)
    CLOSE(FLUX_COUNT_UNIT)
  END SUBROUTINE save_em_sim_close
!***

END MODULE save_em_sim
