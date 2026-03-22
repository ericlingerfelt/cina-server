MODULE fileio
!DESC = This module contains general procedures for reading and writing files in various formats
!DESC =
!DESC = Use the subroutine fprintdatatypes to see the available data types
!DESC = Use the subroutine fprintRdataformats to see the available data formats for reading
!DESC = Use the subroutine fprintWdataformats to see the available data formats for writing
!
! Procedures in this module are:
! 
! fileio_ver                         Version of this file
! fcheckoptions(rw)                  Check options and supply defaults if possible
! file2array(array,points,optin)     Extract table of numbers from file into array
! array2file(array,points,optin)     Save table of numbers from an array into a file
! fprintoptions(optin,u)             Print the file io options
! fprintdatatypes(u)                 Print the available data types
! fprintRdataformats(u)              Print the available data formats for reading
! fprintWdataformats(u)              Print the available data formats for writing

!CAUTION = WHEN ADDING NEW DATA TYPES OR FORMATS CHECK ALL PROCEDURES IN THIS FILE

! Allow procedures in the io module to be used
  USE io
  USE reactionstrings

! By default all procedures and global variables are private
  PRIVATE

! These procedures are public
  PUBLIC :: fileio_ver,file2array,fprintoptions,fsetoptions,fprintdatatypes
  PUBLIC :: fprintRdataformats,fprintWdataformats,array2file
  PUBLIC :: nres2file,file2nres,icresults2file,file2icresults
  PUBLIC :: get_e_units,get_s_units,get_cs_units,get_t_units,get_r_units
  PUBLIC :: fopen,fwrite,fclose,file_exists,have_lock,fread
  PUBLIC :: fopen_nolock,fclose_nolock,fdel_nolock,fmv_nolock

  TYPE,PUBLIC                     :: fileiooptions
     TYPE(reactionparticles)      :: r
     CHARACTER(LEN=100)           :: file_in = ''
     CHARACTER(LEN=100)           :: file_out = ''
     CHARACTER(LEN=15)            :: data_type = ''
     CHARACTER(LEN=20)            :: data_format = ''
     CHARACTER(LEN=20)            :: fmt_options = ''
     INTEGER(KIND=1)              :: col_index(9) = -1
     INTEGER(KIND=1)              :: plevel = 2
     REAL(KIND=8)                 :: e_units_in = 0d0
     REAL(KIND=8)                 :: e_units_out = 0d0
     REAL(KIND=8)                 :: s_units_in = 0d0
     REAL(KIND=8)                 :: s_units_out = 0d0
     REAL(KIND=8)                 :: cs_units_in = 0d0
     REAL(KIND=8)                 :: cs_units_out = 0d0
     REAL(KIND=8)                 :: t_units_in = 0d0
     REAL(KIND=8)                 :: t_units_out = 0d0
     REAL(KIND=8)                 :: r_units_in = 0d0
     REAL(KIND=8)                 :: r_units_out = 0d0
     CHARACTER(LEN=8174)          :: notes = ''    ! 8174 was the max number allowed before an internal compiler error
  END TYPE fileiooptions

! Type to be used for inputcheck program (and is placed in fileio to prevent dependency problems)
  TYPE,PUBLIC                     :: inputcheckresult
     CHARACTER(LEN=30)            :: test = ''
     CHARACTER(LEN=7)             :: result = ''
     CHARACTER(LEN=50)            :: reason = ''
  END TYPE inputcheckresult

! Variables global to this module
  TYPE(fileiooptions),PRIVATE     :: fopt

  INTEGER(KIND=4),PARAMETER       :: SLEEP_TIME = 10 ! milliseconds
  INTEGER(KIND=4),PARAMETER       :: DEF_MAX_TRIES = 25

CONTAINS
!---------------------------------------------------------------------
  FUNCTION fileio_ver
!PURPOSE = Return the cvs revision number for this file
!STATUS = Complete and tested
    IMPLICIT NONE
    CHARACTER(LEN=20),PARAMETER   :: FILEIO_VERSION = '$Revision: 1.1.1.1 $'
    CHARACTER(LEN=10)             :: fileio_ver

    fileio_ver = FILEIO_VERSION(12:LEN_TRIM(FILEIO_VERSION)-2)

  END FUNCTION fileio_ver

!---------------------------------------------------------------------
  SUBROUTINE fcheckoptions(rw)
!PURPOSE = Check global options and supply defaults if necessary
!STATUS = Complete and tested
!STATUS = Supports data types: S(E),CS(E),R(T)
!STATUS = Supports read data formats: LOOSE_COLUMN,INPUTREAD,INPUTCHECK,RATEGEN
!STATUS = Supports write data formats: INPUTREAD,NARROW_RESONANCE,INPUTCHECK,RATEGEN
    USE convert
    IMPLICIT NONE
    CHARACTER(LEN=2),INTENT(IN)   :: rw
    INTEGER(KIND=1)               :: j

! Check file_in and file_out
    SELECT CASE (rw)
! rf is to read a file and get most options from the file
    CASE ('r ','rf')
       IF (LEN_TRIM(fopt%file_in) < 1) CALL printerror('Input file must be specified.',1)
       IF (LEN_TRIM(fopt%file_out) < 1) fopt%file_out = TRIM(fopt%file_in) // '.inrd'
    CASE ('w ')
       IF (LEN_TRIM(fopt%file_out) < 1) CALL printerror('Output file must be specified.',1)
    CASE DEFAULT
       CALL printerror('Unknown rw in fcheckoptions',1)
    END SELECT

! Check data type and units as long as they shouldn't be obtained from a file
    IF (rw(2:2) /= 'f') THEN
       type: SELECT CASE (lowercase(fopt%data_type))
       CASE ('s(e) ') type
! Check energy units
          IF ((fopt%e_units_in == 0D0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming input energy units are KeV',0)
             fopt%e_units_in = 1D3
          END IF
          IF ((fopt%e_units_out == 0D0) .AND. (rw(1:1) == 'w')) THEN
             CALL printerror('Assuming output energy units are KeV',0)
             fopt%e_units_out = 1D3
          END IF
! Check S units
          IF ((fopt%s_units_in == 0D0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming input S-factor units are KeV-b',0)
             fopt%s_units_in = 1D3
          END IF
          IF ((fopt%s_units_out == 0D0) .AND. (rw(1:1) == 'w')) THEN
             CALL printerror('Assuming output S-factor units are KeV-b',0)
             fopt%s_units_out = 1D3
          END IF
! Check column index
          IF ((LEN_TRIM(fopt%fmt_options) == 0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming column index is 1234',0)
             WRITE(*,'(A)') 'This means that the energy is in the first column,'
             WRITE(*,'(A)') 'energy error is in the second column, S-factor is'
             WRITE(*,'(A)') 'in the third column, and the S-factor error is in'
             WRITE(*,'(A)') 'the forth column.'
             fopt%fmt_options = '1234'
          END IF
          IF ((LEN_TRIM(fopt%fmt_options) /= 4) .AND. (rw(1:1) == 'r')) &
               CALL printerror('Column index must have 4 digits for S(E)',1)
       CASE ('cs(e) ') type
! Check energy units
          IF ((fopt%e_units_in == 0D0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming input energy units are KeV',0)
             fopt%e_units_in = 1D3
          END IF
          IF ((fopt%e_units_out == 0D0) .AND. (rw(1:1) == 'w')) THEN
             CALL printerror('Assuming output energy units are KeV',0)
             fopt%e_units_out = 1D3
          END IF
! Check CS units
          IF ((fopt%cs_units_in == 0D0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming input cross section units are barns',0)
             fopt%cs_units_in = 1D0
          END IF
          IF ((fopt%cs_units_out == 0D0) .AND. (rw(1:1) == 'w')) THEN
             CALL printerror('Assuming output cross section units are barns',0)
             fopt%cs_units_out = 1D0
          END IF
! Check column index
          IF ((LEN_TRIM(fopt%fmt_options) == 0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming column index is 1234',0)
             WRITE(*,'(A)') 'This means that the energy is in the first column,'
             WRITE(*,'(A)') 'energy error is in the second column, cross section'
             WRITE(*,'(A)') 'is in the third column, and the cross section error'
             WRITE(*,'(A)') 'is in the forth column.'
             fopt%fmt_options = '1234'
          END IF
          IF ((LEN_TRIM(fopt%fmt_options) /= 4) .AND. (rw(1:1) == 'r')) &
               CALL printerror('Column index must have 4 digits for CS(E)',1)
       CASE ('r(t) ')
! Check temp units
          IF ((fopt%t_units_in == 0D0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming input temperature units (K) are T9',0)
             fopt%t_units_in = 1D9
          END IF
          IF ((fopt%t_units_out == 0D0) .AND. (rw(1:1) == 'w')) THEN
             CALL printerror('Assuming output temperature units (K) are T9',0)
             fopt%t_units_out = 1D9
          END IF
! Check rate units
          IF ((fopt%r_units_in == 0D0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming standard input rate units',0)
             fopt%r_units_in = 1D0
          END IF
          IF ((fopt%r_units_out == 0D0) .AND. (rw(1:1) == 'w')) THEN
             CALL printerror('Assuming standard output rate units',0)
             fopt%r_units_out = 1D0
          END IF
! Check column index
          IF ((LEN_TRIM(fopt%fmt_options) == 0) .AND. (rw(1:1) == 'r')) THEN
             CALL printerror('Assuming column index is 1234',0)
             WRITE(*,'(A)') 'This means that the temperature is in the first'
             WRITE(*,'(A)') 'column, temperature error is in the second column,'
             WRITE(*,'(A)') 'rate is in the third column, and the rate error'
             WRITE(*,'(A)') 'is in the forth column.'
             fopt%fmt_options = '1234'
          END IF
          IF ((LEN_TRIM(fopt%fmt_options) /= 4) .AND. (rw(1:1) == 'r')) &
               CALL printerror('Column index must have 4 digits for R(T)',1)
       CASE DEFAULT type
          CALL fprintdatatypes
          CALL printerror('Unknown data type "'//TRIM(fopt%data_type)//'"',1)
       END SELECT type

! Check data format, format options, and col_index
       IF (LEN_TRIM(fopt%data_format) == 0) THEN
          CALL printerror('Assuming data format is "LOOSE_COLUMN"',0)
          fopt%data_format = 'LOOSE_COLUMN'
       END IF
    ELSE
       IF (LEN_TRIM(fopt%data_format) == 0) THEN
          CALL printerror('Data format must be specified if options are to be inferred',1)
       END IF
    END IF

    fmt: SELECT CASE (lowercase(fopt%data_format))
    CASE ('loose_column ') fmt
!! Check fmt_options and create col_index
       IF (LEN_TRIM(fopt%fmt_options) == 0) THEN
          CALL printerror('A data format or default value must be specified.',1)
       ELSE
          IF (VERIFY(TRIM(fopt%fmt_options),'0123456789') /= 0)           &
               CALL printerror('Column index can only contain digits',1)
          IF (LEN_TRIM(fopt%fmt_options) > 9)                          &
               CALL printerror('Column index can not have more than 9 digits',1)
          DO j = 1,9
             fopt%col_index(j) = ICHAR(fopt%fmt_options(j:j)) - 48
             IF ((fopt%col_index(j) < 0) .OR. (fopt%col_index(j) > 9)) fopt%col_index(j) = -1
          END DO
       END IF
    CASE ('inputread ') fmt
       IF (rw(1:1) == 'r') THEN
       ELSE IF (rw(1:1) == 'w') THEN
! Check fmt_options to specify if files should be overwritten or not
          SELECT CASE (lowercase(fopt%fmt_options))
          CASE ('new ')         ! Do not overwrite
          CASE ('unknown ')     ! Overwrite
          CASE DEFAULT
             WRITE(*,'(A)') 'Allowable format options are NEW, UNKNOWN'
             CALL printerror('Unknown format option "'//TRIM(fopt%fmt_options)//'"',1)
          END SELECT
       ENDIF
    CASE ('narrow_resonance ') fmt
       IF (rw(1:1) == 'r') THEN
       ELSE IF (rw(1:1) == 'w') THEN
! Check fmt_options to specify if files should be overwritten or not
          SELECT CASE (lowercase(fopt%fmt_options))
          CASE ('new ')         ! Do not overwrite
          CASE ('unknown ')     ! Overwrite
          CASE DEFAULT
             WRITE(*,'(A)') 'Allowable format options are NEW, UNKNOWN'
             CALL printerror('Unknown format option "'//TRIM(fopt%fmt_options)//'"',1)
          END SELECT
       ENDIF
    CASE ('inputcheck ') fmt
       IF (rw(1:1) == 'r') THEN
       ELSE IF (rw(1:1) == 'w') THEN
! Check fmt_options to specify if files should be overwritten or not
          SELECT CASE (lowercase(fopt%fmt_options))
          CASE ('new ')         ! Do not overwrite
          CASE ('unknown ')     ! Overwrite
          CASE DEFAULT
             WRITE(*,'(A)') 'Allowable format options are NEW, UNKNOWN'
             CALL printerror('Unknown format option "'//TRIM(fopt%fmt_options)//'"',1)
          END SELECT
       ENDIF
    CASE ('rategen ') fmt
       IF (rw(1:1) == 'r') THEN
       ELSE IF (rw(1:1) == 'w') THEN
! Check fmt_options to specify if files should be overwritten or not
          SELECT CASE (lowercase(fopt%fmt_options))
          CASE ('new ')         ! Do not overwrite
          CASE ('unknown ')     ! Overwrite
          CASE DEFAULT
             WRITE(*,'(A)') 'Allowable format options are NEW, UNKNOWN'
             CALL printerror('Unknown format option "'//TRIM(fopt%fmt_options)//'"',1)
          END SELECT
       ENDIF
    CASE DEFAULT fmt
       CALL fprintRdataformats
       CALL fprintWdataformats
       CALL printerror('Unknown data format "'//TRIM(fopt%data_format)//'"',1)
    END SELECT fmt

  END SUBROUTINE fcheckoptions

!---------------------------------------------------------------------
  FUNCTION get_e_units(in)
!PURPOSE =  Return the string for the energy units of the value
!STATUS = Complete and tested
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)        :: in
    CHARACTER(LEN=13)              :: get_e_units

    IF (in == 1D0) THEN
       get_e_units = 'eV'
    ELSE IF (in == 1D3) THEN
       get_e_units = 'KeV'
    ELSE IF (in == 1D6) THEN
       get_e_units = 'MeV'
    ELSE IF (in == 1D9) THEN
       get_e_units = 'GeV'
    ELSE
       get_e_units = 'unknown units'
    END IF
  END FUNCTION get_e_units

!---------------------------------------------------------------------
  FUNCTION get_s_units(in)
!PURPOSE =  Return the string for the s-factor units of the value
!STATUS = Complete and tested
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)        :: in
    CHARACTER(LEN=13)              :: get_s_units

    IF (in == 1D0) THEN
       get_s_units = 'eV-b'
    ELSE IF (in == 1D3) THEN
       get_s_units = 'KeV-b'
    ELSE IF (in == 1D6) THEN
       get_s_units = 'MeV-b'
    ELSE IF (in == 1D9) THEN
       get_s_units = 'GeV-b'
    ELSE
       get_s_units = 'unknown units'
    END IF
  END FUNCTION get_s_units

!---------------------------------------------------------------------
  FUNCTION get_cs_units(in)
!PURPOSE =  Return the string for the cross section units of the value
!STATUS = Complete and tested
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)        :: in
    CHARACTER(LEN=13)              :: get_cs_units

    IF (in == 1D0) THEN
       get_cs_units = 'barns'
    ELSE IF (in == 1D3) THEN
       get_cs_units = 'Kbarns'
    ELSE IF (in == 1D6) THEN
       get_cs_units = 'Mbarns'
    ELSE IF (in == 1D9) THEN
       get_cs_units = 'Gbarns'
    ELSE
       get_cs_units = 'unknown units'
    END IF
  END FUNCTION get_cs_units

!---------------------------------------------------------------------
  FUNCTION get_t_units(in)
!PURPOSE =  Return the string for the temperature units of the value
!STATUS = Complete and tested
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)        :: in
    CHARACTER(LEN=13)              :: get_t_units

    IF (in == 1D0) THEN
       get_t_units = 'T0'
    ELSE IF (in == 1D3) THEN
       get_t_units = 'T3'
    ELSE IF (in == 1D6) THEN
       get_t_units = 'T6'
    ELSE IF (in == 1D9) THEN
       get_t_units = 'T9'
    ELSE
       get_t_units = 'unknown units'
    END IF
  END FUNCTION get_t_units

!---------------------------------------------------------------------
  FUNCTION get_r_units(in)
!PURPOSE =  Return the string for the rate units of the value
!STATUS = Complete and tested
    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN)        :: in
    CHARACTER(LEN=18)              :: get_r_units

    get_r_units = ''        ! Return no units instead of the wrong ones
    RETURN

    IF (in == 1D0) THEN
       get_r_units = 'reactions/sec'
    ELSE IF (in == 1D3) THEN
       get_r_units = 'reactions/sec*10^3'
    ELSE IF (in == 1D6) THEN
       get_r_units = 'reactions/sec*10^6'
    ELSE IF (in == 1D9) THEN
       get_r_units = 'reactions/sec*10^9'
    ELSE
       get_r_units = 'unknown units'
    END IF
  END FUNCTION get_r_units

!---------------------------------------------------------------------
  SUBROUTINE file2array(array,points,opt)
!PURPOSE = Call the routines needed to read a file into a 2D array
!STATUS = Complete and tested
!STATUS = Supports data types: S(E),CS(E),R(T)
!STATUS = Supports read data formats: LOOSE_COLUMN,INPUTREAD,RATEGEN
    USE convert
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(OUT)   :: points
    REAL(KIND=8),INTENT(OUT)      :: array(:,:)
    CHARACTER(LEN=2)              :: rw
    TYPE(fileiooptions),INTENT(INOUT):: opt

! Set global options
    fopt = opt

    SELECT CASE (lowercase(fopt%data_format))
    CASE ('loose_column ')
       rw = 'r '
    CASE ('inputread ')
       rw = 'rf'
    CASE ('rategen ')
       rw = 'r '
    CASE DEFAULT
       rw = 'r '
    END SELECT

! Check the options and supply defaults if necessary
    CALL fcheckoptions(rw)
! Print the options if they are not to be inferred by the file
    IF ((fopt%plevel >= 3) .AND. (rw(2:2) /= 'f')) CALL fprintoptions

    type: SELECT CASE (lowercase(fopt%data_format))
    CASE ('loose_column ') type
       CALL rfmt_LOOSE_COLUMN(array,points)
    CASE ('inputread ')
       CALL rfmt_INPUTREAD(array,points)
! Now that some of the options were read from the file,
! check them, and print warnings if any are missing
       CALL fcheckoptions('r ')
    CASE ('rategen ')
       CALL rfmt_RATEGEN(array,points)
    CASE DEFAULT type
       CALL printerror('data format "'//TRIM(fopt%data_format)//'" not supported in file2array',1)
    END SELECT type

! Print the options if they are not to be inferred by the file
    IF ((fopt%plevel >= 3) .AND. (rw(2:2) == 'f')) CALL fprintoptions

! Send the global options back to the calling routine
    opt = fopt

  END SUBROUTINE file2array

!---------------------------------------------------------------------
  SUBROUTINE array2file(array,points,optin)
!PURPOSE = Call the routines needed to save a 2D array into a file
!STATUS = Complete and tested
!STATUS = Supports data types: S(E),CS(E),R(T)
!STATUS = Supports write data formats: INPUTREAD
!CAUTION = The array might change on return
!CAUTION = For example a data format might sort the array
    USE convert
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: points
    REAL(KIND=8),INTENT(INOUT)    :: array(:,:)
    TYPE(fileiooptions),INTENT(INOUT):: optin

! Set global options
    fopt = optin

! Check the options and supply defaults if necessary
    CALL fcheckoptions('w ')

! IF (fopt%plevel >= 3) CALL fprintoptions

    frmt: SELECT CASE (lowercase(fopt%data_format))
    CASE ('inputread ') frmt
       type: SELECT CASE (lowercase(fopt%data_type))
       CASE ('s(e) ','cs(e) ','r(t) ') type
          array = sort2Darray(array,points,4,1)
       CASE DEFAULT type
          CALL printerror('Unknown case in array2file',1)
       END SELECT type
       CALL wfmt_INPUTREAD(array,points)
    CASE DEFAULT frmt
       CALL printerror('data format "'//TRIM(fopt%data_format)//'" not supported in arrayfile',1)
    END SELECT frmt

! Send the global options back to the calling routine
    optin = fopt

  END SUBROUTINE array2file

!---------------------------------------------------------------------
  SUBROUTINE nres2file(nres,points,optin)
!PURPOSE = Call the routines needed to save narrow resonances to a file
!STATUS = Complete and tested
!STATUS = Supports data types: any
!STATUS = Supports write data formats: NARROW_RESONANCE
    USE convert
    USE reaction
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: points
    TYPE(narrowresonance),INTENT(IN) :: nres(:)
    TYPE(fileiooptions),INTENT(INOUT):: optin

! Set global options
    fopt = optin

! Supply data_format if it isn't present
    IF (LEN_TRIM(fopt%data_format) == 0) fopt%data_format = 'NARROW_RESONANCE'

! Check the options and supply defaults if necessary
    CALL fcheckoptions('w ')

! IF (fopt%plevel >= 3) CALL fprintoptions

! Make sure points value is inside the array
    IF (points > UBOUND(nres,1)) CALL printerror('nres2file: points is out of range',1)

    CALL wfmt_NARROW_RESONANCE(nres,points)

! Send the global options back to the calling routine
    optin = fopt

  END SUBROUTINE nres2file

!---------------------------------------------------------------------
  SUBROUTINE file2nres(nres,points,optin)
!PURPOSE = Call the routines needed to read narrow resonances from a file
!STATUS = Complete and tested
!STATUS = Supports data types: any
!STATUS = Supports write data formats: NARROW_RESONANCE
    USE convert
    USE reaction
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(OUT)    :: points
    TYPE(narrowresonance),INTENT(OUT) :: nres(:)
    TYPE(fileiooptions),INTENT(INOUT):: optin

! Set global options
    fopt = optin

! Supply data_format if it isn't present
    IF (LEN_TRIM(fopt%data_format) == 0) fopt%data_format = 'NARROW_RESONANCE'

! Check the options and supply defaults if necessary
    CALL fcheckoptions('rf')

! IF (fopt%plevel >= 3) CALL fprintoptions

    CALL rfmt_NARROW_RESONANCE(nres,points)

! Send the global options back to the calling routine
    optin = fopt

  END SUBROUTINE file2nres

!---------------------------------------------------------------------
  SUBROUTINE icresults2file(results,points,optin)
!PURPOSE = Call the routines needed to save inputcheck results to a file
!STATUS = Complete and tested
!STATUS = Supports data types: any
!STATUS = Supports write data formats: INPUTCHECK
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: points
    TYPE(inputcheckresult),INTENT(IN) :: results(:)
    TYPE(fileiooptions),INTENT(INOUT):: optin

! Set global options
    fopt = optin

! Supply data_format if it isn't present
    IF (LEN_TRIM(fopt%data_format) == 0) fopt%data_format = 'INPUTCHECK'

! Check the options and supply defaults if necessary
    CALL fcheckoptions('w ')

! IF (fopt%plevel >= 3) CALL fprintoptions

! Make sure points value is inside the array
    IF (points > UBOUND(results,1)) CALL printerror('icresults2file: points is out of range',1)

    CALL wfmt_INPUTCHECK(results,points)

! Send the global options back to the calling routine
    optin = fopt

  END SUBROUTINE icresults2file

!---------------------------------------------------------------------
  SUBROUTINE file2icresults(results,points,optin)
!PURPOSE = Call the routines needed to read inputcheck results from a file
!STATUS = Complete and tested
!STATUS = Supports data types: any
!STATUS = Supports write data formats: INPUTCHECK
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(OUT)    :: points
    TYPE(inputcheckresult),INTENT(OUT) :: results(:)
    TYPE(fileiooptions),INTENT(INOUT):: optin

! Set global options
    fopt = optin

! Supply data_format if it isn't present
    IF (LEN_TRIM(fopt%data_format) == 0) fopt%data_format = 'INPUTCHECK'

! Check the options and supply defaults if necessary
    CALL fcheckoptions('rf')

! IF (fopt%plevel >= 3) CALL fprintoptions

    CALL rfmt_INPUTCHECK(results,points)

! Send the global options back to the calling routine
    optin = fopt

  END SUBROUTINE file2icresults

!---------------------------------------------------------------------
  SUBROUTINE fprintoptions(optin,u)
!PURPOSE = Print fileio options to unit u.  If u is not specified, print to the screen
!STATUS = Complete and tested
    USE convert
    USE reactionstrings
    IMPLICIT NONE
    TYPE(fileiooptions),INTENT(IN):: optin
    INTEGER(KIND=1),INTENT(IN)    :: u
    OPTIONAL                      :: optin,u
    CHARACTER(LEN=60)             :: s
    CHARACTER(LEN=9)              :: tmps
    INTEGER(KIND=1)               :: i,unit

    IF (PRESENT(optin)) fopt = optin

    IF (PRESENT(u)) THEN
       unit = u
    ELSE
       unit = 6
    END IF

    s = getreac_str(fopt%r)
    WRITE(unit,'(A,T30,A)') 'Reaction:',TRIM(s)
    WRITE(unit,'(A,T30,A)') 'Notes:',TRIM(line2str(TRIM(fopt%notes)))
    WRITE(unit,'(A,T30,A)') 'Input file:',TRIM(fopt%file_in)
    WRITE(unit,'(A,T30,A)') 'Output file:',TRIM(fopt%file_out)
    WRITE(unit,'(A,T30,A)') 'Data type:',TRIM(fopt%data_type)
    WRITE(unit,'(A,T30,A)') 'Data format:',TRIM(fopt%data_format)
    WRITE(unit,'(A,T30,A)') 'Data format options:',TRIM(fopt%fmt_options)
    WRITE(unit,'(A,T30,G8.2)') 'Energy units in:',get_e_units(fopt%e_units_in)
    WRITE(unit,'(A,T30,G8.2)') 'Energy units out:',get_e_units(fopt%e_units_out)
    WRITE(unit,'(A,T30,G8.2)') 'S-factor units in:',get_s_units(fopt%s_units_in)
    WRITE(unit,'(A,T30,G8.2)') 'S-factor units out:',get_s_units(fopt%s_units_out)
    WRITE(unit,'(A,T30,G8.2)') 'Cross section units in:',get_cs_units(fopt%cs_units_in)
    WRITE(unit,'(A,T30,G8.2)') 'Cross section units out:',get_cs_units(fopt%cs_units_out)
    WRITE(unit,'(A,T30,G8.2)') 'Temperature units in:',get_t_units(fopt%t_units_in)
    WRITE(unit,'(A,T30,G8.2)') 'Temperature units out:',get_t_units(fopt%t_units_out)
    WRITE(unit,'(A,T30,G8.2)') 'Rate units in:',get_r_units(fopt%r_units_in)
    WRITE(unit,'(A,T30,G8.2)') 'Rate units out:',get_r_units(fopt%r_units_out)
    DO i = 1,9
       IF (fopt%col_INDEX(i) >= 0) THEN
          tmps(i:i) = CHAR(fopt%col_index(i) + 48)
       ELSE
          tmps(i:i) = ' '
       END IF
    END DO
    WRITE(unit,'(A,T30,A)') 'Column indices:',tmps
    WRITE(unit,'(A,T30,I0)') 'Print level:',fopt%plevel

  END SUBROUTINE fprintoptions

!---------------------------------------------------------------------
  SUBROUTINE fprintdatatypes(u)
!PURPOSE = Print the available data types to unit u.  If u is not specified, print to the screen
!STATUS = Complete and tested
    IMPLICIT NONE
    INTEGER(KIND=1),INTENT(IN)    :: u
    OPTIONAL                      :: u
    INTEGER(KIND=1)               :: unit

    IF (PRESENT(u)) THEN
       unit = u
    ELSE
       unit = 6
    END IF

    write(UNIT,'(A)') 'Available data types: S(E),CS(E),R(T)'

  END SUBROUTINE fprintdatatypes

!---------------------------------------------------------------------
  SUBROUTINE fprintRdataformats(u)
!PURPOSE = Print the available READ data formats to unit u.  If u is not specified, print to the screen
!STATUS = Complete and tested
    IMPLICIT NONE
    INTEGER(KIND=1),INTENT(IN)    :: u
    OPTIONAL                      :: u
    INTEGER(KIND=1)               :: unit

    IF (PRESENT(u)) THEN
       unit = u
    ELSE
       unit = 6
    END IF

    write(UNIT,'(2A)') 'Available read data formats: LOOSE_COLUMN,INPUTREAD,', &
         'NARROW_RESONANCE,INPUTCHECK,RATEGEN'

  END SUBROUTINE fprintRdataformats

!---------------------------------------------------------------------
  SUBROUTINE fprintWdataformats(u)
!PURPOSE = Print the available WRITE data formats to unit u.  If u is not specified, print to the screen
!STATUS = Complete and tested
    IMPLICIT NONE
    INTEGER(KIND=1),INTENT(IN)    :: u
    OPTIONAL                      :: u
    INTEGER(KIND=1)               :: unit

    IF (PRESENT(u)) THEN
       unit = u
    ELSE
       unit = 6
    END IF

    write(UNIT,'(2A)') 'Available write data formats: INPUTREAD,NARROW_RESONANCE,', &
         'INPUTCHECK'

  END SUBROUTINE fprintWdataformats

!---------------------------------------------------------------------
  SUBROUTINE wfmt_INPUTCHECK(results,points)
!PURPOSE = Write inputcheck results a file
!STATUS = Complete and tested
!STATUS = Supports data types: any
    USE convert
    USE reactionstrings
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: points
    TYPE(inputcheckresult),INTENT(IN):: results(:)
    INTEGER(KIND=4)               :: n,i,j
    CHARACTER(LEN=80)             :: tmps

! The output file will be overwritten if fmt_options is UNKNOWN
    OPEN(UNIT=3,FILE=fopt%file_out,IOSTAT=n,STATUS=fopt%fmt_options,ACTION='WRITE')

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open output file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Get reaction string
    tmps = getreac_str(fopt%r)

! Write the file header line
    WRITE(3,'(3A,I0,2A)',IOSTAT=n) 'INPUTCHECK,',TRIM(fopt%data_type),',',points,',',TRIM(tmps)

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write header to output file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Write the notes to next line (and it should take up only one line)
    WRITE(3,'(A)',IOSTAT=n) TRIM(str2line(TRIM(fopt%notes)))

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write notes to output file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

    DO i = 1,points
       WRITE(3,'(2A)') 'TEST: ',TRIM(results(i)%test)
       WRITE(3,'(2A)') 'RESULT: ',TRIM(results(i)%result)
       WRITE(3,'(2A)') 'REASON: ',TRIM(results(i)%reason)
       WRITE(3,'(A)') ' '
       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not write results to output file. IOSTAT=',n
          CALL printerror(tmps,1)
       END IF
    END DO

    CLOSE(3)

  END SUBROUTINE wfmt_INPUTCHECK

!---------------------------------------------------------------------
  SUBROUTINE rfmt_INPUTCHECK(results,points)
!PURPOSE = Read inputcheck results from a file in the INPUTCHECK format
!STATUS = Complete but NOT tested
!STATUS = Supports data types: any
    USE io
    USE convert
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(OUT)   :: points
    TYPE(inputcheckresult),INTENT(OUT):: results(:)
    CHARACTER(LEN=1000)           :: buffer
    CHARACTER(LEN=80)             :: tmps
    INTEGER(KIND=4)               :: n,start,last

! Open the file and generate an error if it does not exist
    OPEN(UNIT=3,FILE=fopt%file_in,IOSTAT=n,STATUS='OLD',ACTION='READ')

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Read in the header
    READ(3,'(A)',IOSTAT=n) buffer

! Make sure the read was successful
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read buffer in input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Parse buffer into data_type and # of points
! The buffer is a comma separated list
    last = INDEX(buffer,',')
    tmps = buffer(1:last-1)                       ! Read in data_format
    IF (lowercase(tmps) /= 'inputcheck ')   &
         CALL printerror('File '//fopt%file_in//' is not in the INPUTCHECK format.',1)

    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma
    fopt%data_type = buffer(start:last-1)             ! Read in data_type

    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma

    READ(buffer(start:last-1),'(I)',IOSTAT=n) points ! Read in # of points
    IF (n /= 0) CALL printerror('Error reading points in input file header.',1)

! Read in reaction type
    start = last + 1
    CALL read_reac_str(fopt%r,buffer(start:),tmps)
    IF (LEN_TRIM(tmps) > 0) THEN
       WRITE(*,'(A)') TRIM(tmps)
       CALL printerror('Could not read in reaction string',1)
    END IF

! Read in notes
    READ(3,'(A)',IOSTAT=n) fopt%notes
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read notes from input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Check that file will fit into nres
    IF (points > UBOUND(results,1)) THEN
       points = UBOUND(results,1)
       CALL printerror('Not enough storage for all inputcheck results',0)
    END IF

! Read in results from file
! start and last are reused as counter variables
    DO start = 1,points
       READ(3,'(A6,A)') tmps,results(start)%test
       IF (tmps /= 'TEST: ') THEN
          WRITE(buffer,'(I0)') (start-1)*4 + 2
          CALL printerror('File is not in INPUTCHECK format. Error in line '//TRIM(buffer),1)
       END IF
       READ(3,'(A8,A)') tmps,results(start)%result
       IF (tmps /= 'RESULT: ') THEN
          WRITE(buffer,'(I0)') (start-1)*4 + 3
          CALL printerror('File is not in INPUTCHECK format. Error in line '//TRIM(buffer),1)
       END IF
       READ(3,'(A8,A)') tmps,results(start)%reason
       IF (tmps /= 'REASON: ') THEN
          WRITE(buffer,'(I0)') (start-1)*4 + 4
          CALL printerror('File is not in INPUTCHECK format. Error in line '//TRIM(buffer),1)
       END IF
       READ(3,'(A)') tmps
       IF (tmps /= ' ') THEN
          WRITE(buffer,'(I0)') (start-1)*4 + 5
          CALL printerror('File is not in INPUTCHECK format. Error in line '//TRIM(buffer),1)
       END IF

       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read results from input file. IOSTAT=',n
          CALL printerror(tmps,1)
       END IF
    END DO

    CLOSE(3)

! Set fmt_options
    fopt%fmt_options = 'OLD'

  END SUBROUTINE rfmt_INPUTCHECK

!---------------------------------------------------------------------
  SUBROUTINE wfmt_NARROW_RESONANCE(nres,points)
!PURPOSE = Write nres to a file
!STATUS = Complete and tested
!STATUS = Supports data types: S(E),CS(E),R(T)
    USE convert
    USE reaction
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: points
    TYPE(narrowresonance),INTENT(IN):: nres(:)
    TYPE(narrowresonance)         :: units
    INTEGER(KIND=4)               :: n,i,j
    CHARACTER(LEN=80)             :: tmps

! The output file will be overwritten if fmt_options is UNKNOWN
    OPEN(UNIT=3,FILE=fopt%file_out,IOSTAT=n,STATUS=fopt%fmt_options,ACTION='WRITE')

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open output file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Set col_num and tmps (to hold a string containing the units)
    SELECT CASE (lowercase(fopt%data_type))
    CASE ('s(e) ')
       units%centroid = fopt%e_units_out
       units%fwhm = fopt%e_units_out
       units%ymax = fopt%s_units_out
       WRITE(tmps,'(G,A,G)',IOSTAT=n) fopt%e_units_out,',',fopt%s_units_out
    CASE ('cs(e) ')
       units%centroid = fopt%e_units_out
       units%fwhm = fopt%e_units_out
       units%ymax = fopt%cs_units_out
       WRITE(tmps,'(G,A,G)',IOSTAT=n) fopt%e_units_out,',',fopt%cs_units_out
    CASE ('r(t) ')
       units%centroid = fopt%t_units_out
       units%fwhm = fopt%t_units_out
       units%ymax = fopt%r_units_out
       WRITE(tmps,'(G,A,G)',IOSTAT=n) fopt%t_units_out,',',fopt%r_units_out
    CASE DEFAULT
       CALL printerror('Unknown data_type in wfmt_INPUTREAD',1)
    END SELECT

! Write the file header line
    WRITE(3,'(3A,I0,2A)',IOSTAT=n) 'NARROW_RESONANCE,',TRIM(fopt%data_type),',',points,',',TRIM(tmps)

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write header to output file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

    DO i = 1,points
       WRITE(3,'(3D25.17)',IOSTAT=n) nres(i)%centroid / units%centroid,   &
            nres(i)%ymax / units%ymax, nres(i)%fwhm / units%fwhm
       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not write resonances to output file. IOSTAT=',n
          CALL printerror(tmps,1)
       END IF
    END DO

    CLOSE(3)

  END SUBROUTINE wfmt_NARROW_RESONANCE

!---------------------------------------------------------------------
  SUBROUTINE rfmt_NARROW_RESONANCE(nres,points)
!PURPOSE = Read narrow resonances and options from a file in the NARROW_RESONANCE format
!STATUS = Complete and tested
!STATUS = Supports data types: S(E),CS(E),R(T)
    USE io
    USE convert
    USE reaction
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(OUT)   :: points
    TYPE(narrowresonance),INTENT(OUT):: nres(:)
    CHARACTER(LEN=1000)           :: buffer
    CHARACTER(LEN=80)             :: tmps
    INTEGER(KIND=4)               :: n,start,last,col_num

! Open the file and generate an error if it does not exist
    OPEN(UNIT=3,FILE=fopt%file_in,IOSTAT=n,STATUS='OLD',ACTION='READ')

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Read in the header
    READ(3,'(A)',IOSTAT=n) buffer

! Make sure the read was successful
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read buffer in input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Parse buffer into data_type, # of points, and units
! The buffer is a comma separated list
    last = INDEX(buffer,',')
    tmps = buffer(1:last-1)                       ! Read in data_format
    IF (lowercase(tmps) /= 'narrow_resonance ')   &
         CALL printerror('File '//fopt%file_in//' is not in the NARROW_RESONANCE format.',1)

    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma
    fopt%data_type = buffer(start:last-1)             ! Read in data_type

    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma

    READ(buffer(start:last-1),'(I)',IOSTAT=n) points ! Read in # of points
    IF (n /= 0) CALL printerror('Error reading points in input file header.',1)

    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma

! Read in units according to the data type
    SELECT CASE (lowercase(fopt%data_type))
    CASE ('s(e) ')
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%e_units_in
       IF (n /= 0) CALL printerror('Error reading energy units in input file header.',1)
       start = last + 1
       READ(buffer(start:),'(G)',IOSTAT=n) fopt%s_units_in
       IF (n /= 0) CALL printerror('Error reading s-factor units in input file header.',1)
       col_num = 4
    CASE ('cs(e) ')
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%e_units_in
       IF (n /= 0) CALL printerror('Error reading energy units in input file header.',1)
       start = last + 1
       READ(buffer(start:),'(G)',IOSTAT=n) fopt%cs_units_in
       IF (n /= 0) CALL printerror('Error reading cross section units in input file header.',1)
       col_num = 4
    CASE ('r(t) ')
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%t_units_in
       IF (n /= 0) CALL printerror('Error reading temperature units in input file header.',1)
       start = last + 1
       READ(buffer(start:),'(G)',IOSTAT=n) fopt%r_units_in
       IF (n /= 0) CALL printerror('Error reading rate units in input file header.',1)
       col_num = 4
    CASE DEFAULT
       CALL printerror('Unknown data type in input file',1)
    END SELECT

! Check that file will fit into nres
    IF (points > UBOUND(nres,1)) THEN
       points = UBOUND(nres,1)
       CALL printerror('Not enough storage for all narrow resonances',0)
    END IF

! Read in array from file
! start and last are reused as counter variables
    DO start = 1,points
       READ(3,'(3D25.17)',IOSTAT=n) nres(start)%centroid, nres(start)%ymax, nres(start)%fwhm
       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read narrow resonances from input file. IOSTAT=',n
          CALL printerror(tmps,1)
       END IF
    END DO

    CLOSE(3)

! Set fmt_options
    fopt%fmt_options = 'OLD'

  END SUBROUTINE rfmt_NARROW_RESONANCE

!---------------------------------------------------------------------
  SUBROUTINE rfmt_INPUTREAD(array,points)
!PURPOSE = Read an array and options from a file in the INPUTREAD format
!STATUS = Complete and tested
!STATUS = Supports data types: S(E),CS(E),R(T)
    USE io
    USE convert
    USE reactionstrings
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(OUT)   :: points
    REAL(KIND=8),INTENT(OUT)      :: array(:,:)
    CHARACTER(LEN=1000)           :: buffer
    CHARACTER(LEN=80)             :: tmps,tmps2
    INTEGER(KIND=4)               :: n,start,last,col_num

! Open the file and generate an error if it does not exist
    OPEN(UNIT=3,FILE=fopt%file_in,IOSTAT=n,STATUS='OLD',ACTION='READ')

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Read in the header
    READ(3,'(A)',IOSTAT=n) buffer

! Make sure the read was successful
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read buffer in input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Parse buffer into data_type, # of points, and units
! The buffer is a comma separated list
    last = INDEX(buffer,',')
    tmps = buffer(1:last-1)                       ! Read in data_format
    IF (lowercase(tmps) /= 'inputread ')   &
         CALL printerror('File '//fopt%file_in//' is not in the INPUTREAD format.',1)

    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma
    fopt%data_type = buffer(start:last-1)             ! Read in data_type

    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma

    READ(buffer(start:last-1),'(I)',IOSTAT=n) points ! Read in # of points
    IF (n /= 0) CALL printerror('Error reading points in input file header.',1)

    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma

! Read in units according to the data type
    SELECT CASE (lowercase(fopt%data_type))
    CASE ('s(e) ')
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%e_units_in
       IF (n /= 0) CALL printerror('Error reading energy units in input file header.',1)
       start = last + 1
       last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%s_units_in
       IF (n /= 0) CALL printerror('Error reading s-factor units in input file header.',1)
       col_num = 4
    CASE ('cs(e) ')
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%e_units_in
       IF (n /= 0) CALL printerror('Error reading energy units in input file header.',1)
       start = last + 1
       last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%cs_units_in
       IF (n /= 0) CALL printerror('Error reading cross section units in input file header.',1)
       col_num = 4
    CASE ('r(t) ')
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%t_units_in
       IF (n /= 0) CALL printerror('Error reading temperature units in input file header.',1)
       start = last + 1
       last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma
       READ(buffer(start:last-1),'(G)',IOSTAT=n) fopt%r_units_in
       IF (n /= 0) CALL printerror('Error reading rate units in input file header.',1)
       col_num = 4
    CASE DEFAULT
       CALL printerror('Unknown data type in input file',1)
    END SELECT

! Read in reaction
    start = last + 1
    last = INDEX(buffer(start:),',') + start - 1  ! Get the location of the next comma
    IF (last > start) THEN
! reaction type included in file
       tmps2 = buffer(start:last-1)

! Read in reaction type
       start = last + 1

       CALL read_reac_str(fopt%r,tmps2,tmps,buffer(start:))
    ELSE
       CALL read_reac_str(fopt%r,buffer(start:),tmps)
    END IF
    IF (LEN_TRIM(tmps) > 0) THEN
       WRITE(*,'(A)') TRIM(tmps)
       CALL printerror('Could not read in reaction string',1)
    END IF

! Read in notes
    READ(3,'(A)',IOSTAT=n) fopt%notes
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read notes from input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Read in array from file
! start and last are reused as counter variables
    DO start = 1,points
       READ(3,'(4D25.17)',IOSTAT=n) (array(start,last),last=1,col_num)
       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read array from input file. IOSTAT=',n
          CALL printerror(tmps,1)
       END IF
    END DO

    CLOSE(3)

! Set fmt_options
    fopt%fmt_options = '123456789'(1:col_num)

  END SUBROUTINE rfmt_INPUTREAD

!---------------------------------------------------------------------
  SUBROUTINE wfmt_INPUTREAD(array,points)
!PURPOSE = Write an array to a file in the INPUTREAD format
!STATUS = Complete and tested
!STATUS = Supports data types: S(E),CS(E),R(T)
    USE convert
    USE reactionstrings
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: points
    REAL(KIND=8),INTENT(IN)       :: array(:,:)
    REAL(KIND=8)                  :: tmpra(8)
    INTEGER(KIND=4)               :: n,i,j,col_num
    CHARACTER(LEN=80)             :: tmps,s,t

! The output file will be overwritten if fmt_options is UNKNOWN
    OPEN(UNIT=3,FILE=fopt%file_out,IOSTAT=n,STATUS=fopt%fmt_options,ACTION='WRITE')

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open output file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Set col_num and tmps (to hold a string containing the units)
    SELECT CASE (lowercase(fopt%data_type))
    CASE ('s(e) ')
       col_num = 4
       tmpra(1:2) = fopt%e_units_in / fopt%e_units_out
       tmpra(3:4) = fopt%s_units_in / fopt%s_units_out
       WRITE(tmps,'(G,A,G)',IOSTAT=n) fopt%e_units_out,',',fopt%s_units_out
    CASE ('cs(e) ')
       col_num = 4
       tmpra(1:2) = fopt%e_units_in / fopt%e_units_out
       tmpra(3:4) = fopt%cs_units_in / fopt%cs_units_out
       WRITE(tmps,'(G,A,G)',IOSTAT=n) fopt%e_units_out,',',fopt%cs_units_out
    CASE ('r(t) ')
       col_num = 4
       tmpra(1:2) = fopt%t_units_in / fopt%t_units_out
       tmpra(3:4) = fopt%r_units_in / fopt%r_units_out
       WRITE(tmps,'(G,A,G)',IOSTAT=n) fopt%t_units_out,',',fopt%r_units_out
    CASE DEFAULT
       CALL printerror('Unknown data_type in wfmt_INPUTREAD',1)
    END SELECT

! Get reaction string
    s = getreac_str(fopt%r)
    n = getreactype(fopt%r,t)
    IF (t /= '') s = TRIM(s) // ',' // t

! Write the file header line
    WRITE(3,'(3A,I0,4A)',IOSTAT=n) 'INPUTREAD,',TRIM(fopt%data_type),',',  &
         points,',',TRIM(tmps),',',TRIM(s)

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write header to output file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Write the notes to next line (and it should take up only one line)
    WRITE(3,'(A)',IOSTAT=n) TRIM(str2line(fopt%notes))

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not write notes to output file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

    DO i = 1,points
       WRITE(3,'(4D25.17)',IOSTAT=n) array(i,1:col_num) * tmpra(1:col_num)
       IF (n /= 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not write array to output file. IOSTAT=',n
          CALL printerror(tmps,1)
       END IF
    END DO

    CLOSE(3)

  END SUBROUTINE wfmt_INPUTREAD

!---------------------------------------------------------------------
  SUBROUTINE rfmt_LOOSE_COLUMN(array,points)
!PURPOSE = Read an array from a file in the LOOSE_COLUMN format
!STATUS = Complete and tested
!DESC = The data type does not matter in this subroutine
    USE convert
! Read an array of numbers in a loose column format from a file into a 2D array
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(OUT)   :: points
    REAL(KIND=8),INTENT(OUT)      :: array(:,:)
    INTEGER(KIND=1)               :: total
    INTEGER(KIND=4)               :: i,j,k,l,m,n,n2,w,ovfl,trun
    CHARACTER(LEN=80)             :: tmps
    CHARACTER(LEN=1000)           :: buffer
    CHARACTER(LEN=4)              :: AD,LF
    REAL(KIND=8)                  :: tmpr

    points = 0
    l = 0                                        ! l is a counter of the lines in the file
    m = MAXVAL(fopt%col_index)
    w = 0                                        ! w is a counter of the number of lines too long
    ovfl = 0                                     ! ovfl is a counter of the number of overflows
    trun = 0                                     ! trun is a counter of how many numbers were truncated
    total = 0
    DO i = 1,4
       IF (fopt%col_index(i) > 0) total = total + 1     ! Count the number of columns to get
    END DO
    IF (fopt%plevel >= 4) THEN
       AD = 'YES'
       LF = ''
    ELSE
       AD = 'NO'
       LF = ACHAR(10)
    END IF

    OPEN(UNIT=13,FILE=fopt%file_in,IOSTAT=n,STATUS='OLD',ACTION='READ')

    DO WHILE(n >= 0)                             ! Loop until the end of the file is reached
       l = l + 1
       IF (n /= 0) THEN
          WRITE(tmps,'(I0)') n
          CALL printerror('File read error '//TRIM(tmps),1)
       END IF

! This next 2 lines shows up if an error occurs
       IF (fopt%plevel == 3) WRITE(*,'(T1,2A)',ADVANCE=AD) REPEAT(' ',LEN(tmps)),LF
       IF (fopt%plevel >= 3) WRITE(*,'(T1,A,I0,A)',ADVANCE=AD) 'Reading line ',l,LF

       READ(13,'(A)',IOSTAT=n) buffer
       IF (n == 0) THEN
          IF (LEN(buffer) == LEN_TRIM(buffer)) w = w + 1

! Erase the current line in the array
          array(points+1,1:4) = 0d0

          n2 = 1
          k = 0
          DO i = 1,m
             IF (n2 > 0) THEN                    ! n2 is 0 when the line has no number
                IF (fopt%plevel >= 3) THEN
                   WRITE(tmps,'(T1,3(A,I0),4A)',IOSTAT=j) '1 i=',i,' l=', &
                        l,' n2=',n2,' "',TRIM(buffer(n2:)),'"',LF
                   WRITE(*,'(T1,A)',ADVANCE=AD) TRIM(tmps)
                END IF
                CALL nextnum(buffer,tmpr,n2,j,fopt%plevel)
                IF (fopt%plevel >= 3) THEN
                   IF (fopt%plevel == 3)                               &
                        WRITE(*,'(T1,2A)',ADVANCE=AD) REPEAT(' ',LEN(tmps)),LF
                   WRITE(*,'(T1,3(A,I0),4A)',ADVANCE=AD) '2 i=',i,' l=', &
                        l,' n2=',n2,' "',TRIM(buffer(n2:)),'"',LF
                END IF
! Check for overflow
                IF (j == 1) ovfl = ovfl + 1
! Check for truncation
                IF (j == 2) trun = trun + 1
             END IF
             IF (n2 > 0) THEN
                DO j = 1,4
                   IF (fopt%col_INDEX(j) == i) THEN
                      array(points+1,j) = tmpr
                      k = k + 1
                   END IF
                END DO
             END IF
          END DO
          IF (k == total) points = points + 1
       END IF
    END DO
    
    CLOSE(13)

! This is also used to stop non-advancing output from above
    IF (fopt%plevel >= 3) WRITE(*,'(T1,A,I0,A)') 'Found ',l,' lines in file.' 

    IF (w > 0) THEN
       WRITE(buffer,'(I0)') w
       CALL printerror('The ends of '//TRIM(buffer)//' lines with more than 1000 characters were ignored',0)
    END IF
    IF (ovfl > 0) THEN
       WRITE(buffer,'(I0)') ovfl
       CALL printerror(TRIM(buffer)//' number(s) was too large or small'// &
            ' and replaced with the nearest number allowable',0)
    END IF
    IF (trun > 0) THEN
       WRITE(buffer,'(I0)') trun
       CALL printerror(TRIM(buffer)//' number(s) had too many digits and was truncated',0)
    END IF
  END SUBROUTINE rfmt_LOOSE_COLUMN

!---------------------------------------------------------------------
  SUBROUTINE rfmt_RATEGEN(array,points)
!PUPOSE = 
!STATUS = 
    IMPLICIT NONE
    REAL(KIND=8),INTENT(OUT)      :: array(:,:)
    INTEGER(KIND=4),INTENT(OUT)   :: points
    CHARACTER(LEN=100)            :: tmps
    INTEGER(KIND=4)               :: n,i

! Open the file and generate an error if it does not exist
    OPEN(UNIT=3,FILE=fopt%file_in,IOSTAT=n,STATUS='OLD',ACTION='READ')

    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not open input file. IOSTAT=',n
       CALL printerror(tmps,1)
    END IF

! Read in number of interpolated points
    READ(3,'(I)',IOSTAT=n) i
    array(1,4) = i
!READ(3,'(A)') tmps
!WRITE(*,'(A)') TRIM(tmps)
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read in number of interpolated points.  IOSTAT=',n
       CALL printerror(tmps,1)
    END IF
    
! Read in header
    READ(3,'(A)',IOSTAT=n) tmps
    IF (n /= 0) THEN
       WRITE(tmps,'(A,I0)') 'Could not read in array header.  IOSTAT=',n
       CALL printerror(tmps,1)
    END IF
    IF (tmps /= '  T9            my            E_gam')     & 
         CALL printerror('Incorrect array header ',1)

! Read in data points
    points = 1
    n = 0
    DO WHILE (n == 0)   ! Loop until an error occurs
       READ(3,'(2(1pe13.4E3,1X),1pe13.4E3)',IOSTAT=n) array(points,1:3)
       points = points + 1
       IF (n > 0) THEN
          WRITE(tmps,'(A,I0)') 'Could not read in rate generation output.  IOSTAT=',n
          CALL printerror(tmps,1)
       END IF
    END DO
    CLOSE(3)
    points = points - 2
  END SUBROUTINE rfmt_RATEGEN

!---------------------------------------------------------------------
  FUNCTION file_exists(file,err_msg)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: file
    CHARACTER(LEN=*),INTENT(OUT)  :: err_msg
    INTEGER(KIND=4)               :: i,unit
    LOGICAL(KIND=4)               :: file_exists
    LOGICAL(KIND=4)               :: file_status
    OPTIONAL                      :: err_msg

!    unit = 99
    IF (PRESENT(err_msg)) err_msg = ''
!    OPEN(unit,FILE=file,IOSTAT=i,STATUS='OLD')
    INQUIRE(FILE=file,IOSTAT=i,EXIST=file_status)

!    SELECT CASE (i)
!    CASE (0)
!       file_exists = .TRUE.
!    CASE (152)
!       file_exists = .FALSE.
!    CASE DEFAULT
     file_exists=file_status
     IF( i /= 0) THEN
       IF (PRESENT(err_msg)) WRITE(err_msg,'(A,I0)') 'file_exists iostat=',i
     ENDIF
!    END SELECT

    CLOSE(unit,IOSTAT=i)
  END FUNCTION file_exists

!---------------------------------------------------------------------
  SUBROUTINE fopen(unit,file,action,status,iostat,position,iostat_str,max_tries,err_msg)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: unit,max_tries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(IN)   :: file,action,status,position
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str,err_msg
    INTEGER(KIND=4)               :: counter,ios,maxtries
    CHARACTER(LEN=20)             :: act,stat,pos,ios_str,pid
    OPTIONAL                      :: action,status,iostat,position,iostat_str,max_tries,err_msg
    
! Set defaults
    IF (PRESENT(action)) THEN
       act = action
    ELSE
       act = 'WRITE'
    END IF

    IF (PRESENT(status)) THEN
       stat = status
    ELSE
       stat = 'UNKNOWN'
    END IF

    IF (PRESENT(position)) THEN
       pos = position
    ELSE
       pos = 'ASIS'
    END IF

    IF (PRESENT(max_tries)) THEN
       maxtries = max_tries
    ELSE
       maxtries = DEF_MAX_TRIES
    END IF

! Try to get lock on file
    counter = 1
    DO WHILE (counter <= maxtries)
! Check for .lock file
       IF (file_exists(TRIM(file)//'.lock')) THEN
! Lock file exists, just wait and try again
!WRITE(*,'(A,I0)') 'CAUTION=.lock exists ',counter

! Wait a short period of time (SLEEP_TIME milliseconds)
          CALL SLEEPQQ(SLEEP_TIME)
       ELSE
! No lock file, so create one
!print *,'Opening lock file '//TRIM(file)//'.lock'
          CALL fopen_nolock(unit,TRIM(file)//'.lock','WRITE','NEW',ios,'ASIS',ios_str,maxtries)
          IF (ios /= 0) THEN
             IF (PRESENT(err_msg)) err_msg = 'Error '//TRIM(ios_str)//' while opening lock file for '//TRIM(file)
             IF (PRESENT(iostat)) iostat = ios
             IF (PRESENT(iostat_str)) iostat_str = ios_str
             RETURN
          END IF

!print *,'writing lock file'
! Put process id in the lock file
          WRITE(pid,'(I0)') GETPID()
          CALL fwrite_nolock(unit,TRIM(pid),ios,'YES',ios_str,maxtries)
          IF (ios /= 0) THEN
             IF (PRESENT(err_msg)) err_msg = 'Error '//TRIM(ios_str)//' while writing lock file for '//TRIM(file)
             IF (PRESENT(iostat)) iostat = ios
             IF (PRESENT(iostat_str)) iostat_str = ios_str
             RETURN
          END IF

!print *,'closing lock file'
! Close lock file
          CALL fclose_nolock(unit,ios,ios_str,maxtries)
          IF (ios /= 0) THEN
             IF (PRESENT(err_msg)) err_msg = 'Error '//TRIM(ios_str)//' while closing lock file for '//TRIM(file)
             IF (PRESENT(iostat)) iostat = ios
             IF (PRESENT(iostat_str)) iostat_str = ios_str
             RETURN
          END IF

!print *,'opening file'
! Now open original file
          CALL fopen_nolock(unit,file,act,stat,ios,pos,ios_str,maxtries)
          IF (ios /= 0) THEN
! Remove lock file
             CALL fdel_nolock(TRIM(file)//'.lock',ios,ios_str,maxtries)

             IF (PRESENT(err_msg)) err_msg = 'Error '//TRIM(ios_str)//' while opening locked file '//TRIM(file)
             IF (PRESENT(iostat)) iostat = ios
             IF (PRESENT(iostat_str)) iostat_str = ios_str
             RETURN
          END IF

          IF (have_lock(file) == -10) THEN
! Success
             IF (PRESENT(iostat)) iostat = 0
             IF (PRESENT(iostat_str)) iostat_str = '0'
             IF (PRESENT(err_msg)) err_msg = ''
             RETURN
          END IF
       END IF
       counter = counter + 1
       IF (counter > maxtries) THEN
          IF (PRESENT(iostat)) iostat = -333
          IF (PRESENT(iostat_str)) iostat_str = '-333'
          IF (PRESENT(err_msg)) err_msg = 'Failed to get lock on file '//TRIM(file)
          RETURN
       END IF
    END DO

    IF (PRESENT(err_msg)) err_msg = ''
    IF (PRESENT(iostat)) iostat = 0
    IF (PRESENT(iostat_str)) iostat_str = '0'
  END SUBROUTINE fopen

!---------------------------------------------------------------------
  SUBROUTINE fopen_nolock(unit,file,action,status,iostat,position,iostat_str,max_tries)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: unit,max_tries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(IN)   :: file,action,status,position
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str
    INTEGER(KIND=4)               :: counter

    counter = 1
    DO WHILE (counter <= max_tries)
       OPEN(unit,FILE=file,ACTION=action,STATUS=status,IOSTAT=iostat,POSITION=position)
!WRITE(*,*) 'open: ',TRIM(file),iostat,counter
       WRITE(iostat_str,'(I0)') iostat
       IF (iostat == 0) RETURN          ! Open successful

! Wait a short period of time (SLEEP_TIME milliseconds)
       CALL SLEEPQQ(SLEEP_TIME)
       counter = counter + 1
    END DO
  END SUBROUTINE fopen_nolock

!---------------------------------------------------------------------
  SUBROUTINE fclose_nolock(unit,iostat,iostat_str,max_tries)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: unit,max_tries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str
    INTEGER(KIND=4)               :: counter

    counter = 1
    DO WHILE (counter <= max_tries)
       CLOSE(unit,IOSTAT=iostat)
!WRITE(*,*) 'close: ',unit,iostat,counter
       WRITE(iostat_str,'(I0)') iostat
       IF (iostat == 0) RETURN          ! Close successful

! Wait a short period of time (SLEEP_TIME milliseconds)
       CALL SLEEPQQ(SLEEP_TIME)
       counter = counter + 1
    END DO
  END SUBROUTINE fclose_nolock

!---------------------------------------------------------------------
  SUBROUTINE fdel_nolock(file,iostat,iostat_str,max_tries)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: file
    INTEGER(KIND=4),INTENT(IN)    :: max_tries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str
    INTEGER(KIND=4)               :: counter

    counter = 1
    DO WHILE (counter <= max_tries)
       iostat = DELFILESQQ(file)
!WRITE(*,*) 'del: ',TRIM(file),iostat,counter
       WRITE(iostat_str,'(I0)') iostat
       IF (iostat == 1) RETURN          ! Deleted one file successful

! Wait a short period of time (SLEEP_TIME milliseconds)
       CALL SLEEPQQ(SLEEP_TIME)
       counter = counter + 1
    END DO
  END SUBROUTINE fdel_nolock

!---------------------------------------------------------------------
  SUBROUTINE fmv_nolock(file,new_file,iostat,iostat_str,max_tries,err_msg)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: file,new_file
    INTEGER(KIND=4),INTENT(IN)    :: max_tries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str,err_msg
    INTEGER(KIND=4)               :: counter
    LOGICAL(KIND=4)               :: good
    OPTIONAL                      :: err_msg

    counter = 1
    DO WHILE (counter <= max_tries)
       good = RENAMEFILEQQ(file,new_file)
!WRITE(*,*) 'mv: ',TRIM(file),good,counter
       IF (good) THEN
          iostat = 0
          iostat_str = '0'
          err_msg = ''
          RETURN
       END IF

! Wait a short period of time (SLEEP_TIME milliseconds)
       CALL SLEEPQQ(SLEEP_TIME)
       counter = counter + 1
    END DO

    iostat = 1
    iostat_str = '1'
    CALL GERROR(err_msg)
    err_msg = 'Error renaming '//TRIM(file)//' to '//TRIM(new_file)//': '//err_msg
  END SUBROUTINE fmv_nolock

!---------------------------------------------------------------------
  SUBROUTINE fclose(unit,file,iostat,iostat_str,max_tries,err_msg)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: file
    INTEGER(KIND=4),INTENT(IN)    :: unit,max_tries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str,err_msg
    INTEGER(KIND=4)               :: ios,maxtries
    CHARACTER(LEN=15)             :: ios_str
    OPTIONAL                      :: iostat,iostat_str,max_tries,err_msg

    ios = have_lock(file) + 10
    IF (ios /= 0) THEN
       IF (PRESENT(iostat_str)) WRITE(iostat_str,'(I0)') ios
       IF (PRESENT(err_msg)) err_msg = 'Error '//TRIM(ios_str)//' while checking if owner of lock file'
       RETURN
    END IF

    IF (PRESENT(max_tries)) THEN
       maxtries = max_tries
    ELSE
       maxtries = DEF_MAX_TRIES
    END IF
    
! Delete lock file
    CALL fdel_nolock(TRIM(file)//'.lock',ios,ios_str,maxtries)
    IF (ios /= 1) THEN
       IF (PRESENT(iostat)) iostat = -ios  ! Make negative to differentate between del and close errors
       IF (PRESENT(iostat_str)) WRITE(iostat_str,'(I0)') -ios
       IF (PRESENT(err_msg)) err_msg = 'Error '//TRIM(ios_str)//' while deleting lock file'
       RETURN
    END IF

! Close unit
    CALL fclose_nolock(unit,ios,ios_str,maxtries)
    IF (PRESENT(iostat)) iostat = ios
    IF (PRESENT(iostat_str)) iostat_str = ios_str
    IF (PRESENT(err_msg)) err_msg = ''
  END SUBROUTINE fclose

!---------------------------------------------------------------------
  SUBROUTINE fwrite(unit,file,string,iostat,advance,iostat_str,max_tries)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: unit,max_tries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(IN)   :: advance,string,file
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str
    INTEGER(KIND=4)               :: counter,ios,maxtries
    CHARACTER(LEN=20)             :: adv
    OPTIONAL                      :: iostat,advance,iostat_str,max_tries

    iostat = have_lock(file) + 10
    IF (iostat /= 0) THEN
!print *,'fwrite have_lock value ',iostat
       IF (PRESENT(iostat_str)) WRITE(iostat_str,'(I0)') ios
       RETURN
    END IF

! Set defaults
    IF (PRESENT(advance)) THEN
       adv = advance
    ELSE
       adv = 'YES'
    END IF

    IF (PRESENT(max_tries)) THEN
       maxtries = max_tries
    ELSE
       maxtries = DEF_MAX_TRIES
    END IF

    counter = 1
    DO WHILE (counter <= maxtries)
       WRITE(unit,'(A)',ADVANCE=adv,IOSTAT=ios) string
       IF (ios <= 0) THEN
!print *,'fwrite write error ',ios
! Write successful
          IF (PRESENT(iostat)) iostat = ios
          IF (PRESENT(iostat_str)) WRITE(iostat_str,'(I0)') ios
          RETURN
       END IF

! Wait a short period of time (SLEEP_TIME milliseconds)
       CALL SLEEPQQ(SLEEP_TIME)
       counter = counter + 1
    END DO

! Report errors
    IF (PRESENT(iostat)) iostat = ios
    IF (PRESENT(iostat_str)) WRITE(iostat_str,'(I0)') ios
!print *,'fwrite end ',ios
  END SUBROUTINE fwrite

!---------------------------------------------------------------------
  SUBROUTINE fwrite_nolock(unit,string,iostat,advance,iostat_str,maxtries)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: unit,maxtries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(IN)   :: advance,string
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str
    INTEGER(KIND=4)               :: counter

    counter = 1
    DO WHILE (counter <= maxtries)
       WRITE(unit,'(A)',ADVANCE=advance,IOSTAT=iostat) string
       IF (iostat <= 0) THEN
! Write successful
          WRITE(iostat_str,'(I0)') iostat
          RETURN
       END IF

! Wait a short period of time (SLEEP_TIME milliseconds)
       CALL SLEEPQQ(SLEEP_TIME)
       counter = counter + 1
    END DO

! Report errors
    WRITE(iostat_str,'(I0)') iostat
  END SUBROUTINE fwrite_nolock

!---------------------------------------------------------------------
  SUBROUTINE fread(unit,file,string,iostat,advance,iostat_str,max_tries)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: unit,max_tries
    INTEGER(KIND=4),INTENT(OUT)   :: iostat
    CHARACTER(LEN=*),INTENT(IN)   :: advance,file
    CHARACTER(LEN=*),INTENT(OUT)  :: iostat_str,string
    INTEGER(KIND=4)               :: counter,ios,maxtries
    CHARACTER(LEN=20)             :: adv
    OPTIONAL                      :: iostat,advance,iostat_str,max_tries

    iostat = have_lock(file) + 10
    IF (iostat /= 0) THEN
       IF (PRESENT(iostat_str)) WRITE(iostat_str,'(I0)') ios
       RETURN
    END IF

! Set defaults
    IF (PRESENT(advance)) THEN
       adv = advance
    ELSE
       adv = 'YES'
    END IF

    IF (PRESENT(max_tries)) THEN
       maxtries = max_tries
    ELSE
       maxtries = DEF_MAX_TRIES
    END IF

    counter = 1
    DO WHILE (counter <= maxtries)
       READ(unit,'(A)',ADVANCE=adv,IOSTAT=ios) string
       IF (ios <= 0) THEN
! Write successful
          IF (PRESENT(iostat)) iostat = ios
          IF (PRESENT(iostat_str)) WRITE(iostat_str,'(I0)') ios
          RETURN
       END IF

! Wait a short period of time (SLEEP_TIME milliseconds)
       CALL SLEEPQQ(SLEEP_TIME)
       counter = counter + 1
    END DO

! Report errors
    IF (PRESENT(iostat)) iostat = ios
    IF (PRESENT(iostat_str)) WRITE(iostat_str,'(I0)') ios
  END SUBROUTINE fread

!---------------------------------------------------------------------
  FUNCTION have_lock(file)
! Return -10 if owner of file lock, 0 if not owner, otherwise an error number
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: file
    INTEGER(KIND=4)               :: have_lock
    INTEGER(KIND=4)               :: iostat,pid
    
    OPEN(UNIT=77,FILE=TRIM(file)//'.lock',IOSTAT=iostat,ACTION='READ')
    IF (iostat /= 0) THEN
       have_lock = iostat
!print *,'error in have_lock open: return ',iostat
       RETURN
    END IF

    READ(77,'(I)',IOSTAT=iostat) pid
    CLOSE(77)
    IF (iostat /= 0) THEN
       have_lock = iostat
!print *,'error in have_lock read: return ',iostat
       RETURN
    END IF

    IF (GETPID() == pid) THEN
       have_lock = -10
    ELSE
       have_lock = 0
    END IF
!print *,'have_lock return ',have_lock
  END FUNCTION have_lock

!---------------------------------------------------------------------
END MODULE fileio
