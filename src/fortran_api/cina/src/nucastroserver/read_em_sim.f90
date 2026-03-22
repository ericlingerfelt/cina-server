! PURPOSE
!  Read binary files of xnet simulation data for the Element Synthesis
!  Visualizer in the Computational Infrastructure for Nuclear Astrophysics.
!
!  The save_em_sim module provides procedures for saving the binary files
!  read by this module.  It creates "isomap" and "reacmap" files for each
!  simulation and seven files (step_num, timemap, thermo, abund, abund_cnt)
!  for each zone in a simulation.  Isomap files may be read one isotope at
!  a time with the open_em_isomap() and read_em_isomap() subroutines.  The
!  whole "isomap" file may be read into memory with the load_em_isomap() 
!  subroutine and individual isotopes may be found with the 
!  get_index_to_isotope() function.  The same is true for "reacmap" files
!  which contain integer mappings for reactions for using flux data.
!  
!  - The "step_num" file contains the number of time steps of a zone in a 
!    simulation.  The get_em_step_num() subroutine returns this number.
!  - The "timemap" file contains the time vales associated with the time
!    step values.  Use the open_em_timemap() and read_em_timemap ()
!    subroutines for obtaining this information.
!  - The "thermo" file contains the temperature and density values 
!    associated with the time step values.  Use the open_em_thermo() and
!    read_em_thermo() subroutines for obtaining this information.
!  - The "abund" file contains non-zero abundances values and isotope 
!    mappings associated with the time step values.  Use the open_em_abund()
!    and read_em_abund() subroutines for obtaining this information.
!  - The "abund_cnt" file contains overhead info about the "abund" file that
!    allows one to jump to abundances for any time step without having to 
!    read the whole, large "abund" file.  It contains the time step number,
!    number of non-zero abundances, and the position in the "abund" file to
!    jump to for reading these abundances.  This file is used with the 
!    "abund" file with the open_em_abund() and read_em_abund() subroutines.
!  - The "flux" file contains non-zero fluxes values and reaction mappings
!    mapping associated with the time step values.  Use the open_em_flux()
!    and read_em_flux() subroutines for obtaining this information.
!  - The "flux_cnt" file contains overhead info about the "flux" file that
!    allows one to jump to fluxes for any time step without having to 
!    read the whole, large "flux" file.  It contains the time step number,
!    number of non-zero fluxes, and the position in the "flux" file to
!    jump to for reading these fluxes.  This file is used with the 
!    "flux" file with the open_em_flux() and read_em_flux() subroutines.
! COPYRIGHT
!  Copyright 2004 Astrophysics Data Team, Physics Division,
!  Oak Ridge National Laboratory.  All rights reserved.
!  This program is for internal, private use only.

       MODULE read_em_sim
! Load all interfaces to all procedures in Intel's portability library.
! This library contains non-standard but common FORTRAN procedures,
! such as FSEEK()
         USE IFLPORT

         PUBLIC

! Parameters global to this module
         INTEGER,PARAMETER :: MAX_NUM_ISOTOPES = 31000
         INTEGER,PARAMETER :: MAX_NUM_REACTIONS = 65000

! User defined type that holds all isotope info for a simulation
         TYPE,PUBLIC                   :: em_isomap
            INTEGER                    :: last_index = -1
            INTEGER                    :: z(0:MAX_NUM_ISOTOPES)
            INTEGER                    :: a(0:MAX_NUM_ISOTOPES)
            CHARACTER(LEN=5)           :: label(0:MAX_NUM_ISOTOPES)
         END TYPE em_isomap
         
! User defined type that holds all reaction info for a simulation
         TYPE,PUBLIC                   :: em_reacmap
            INTEGER                    :: last_index = -1
            INTEGER                    :: reactants(0:MAX_NUM_REACTIONS,3)
            INTEGER                    :: products(0:MAX_NUM_REACTIONS,4)
            CHARACTER(LEN=4)           :: biblio(0:MAX_NUM_REACTIONS)
            INTEGER                    :: iwflx(0:MAX_NUM_REACTIONS)
         END TYPE em_reacmap

! User defined type that holds info for matching reactions
         TYPE,PUBLIC                   :: em_matchmap
            INTEGER                    :: last_index = -1
! reacmap index of reaction with duplicate heavyweights
            INTEGER                    :: duplicate(0:MAX_NUM_REACTIONS)
! reacmap index to add duplicate fluxes to
            INTEGER                    :: addto(0:MAX_NUM_REACTIONS)
         END TYPE em_matchmap
         
       CONTAINS
!***

!****is* read_em_sim/get_em_step_num
! NAME
!  SUBROUTINE get_em_step_num(unit, file, step_num, iostat)
! PURPOSE
!  Return the number of time steps in a simulation
! STATUS
!  Complete and tested
! INPUTS
!  unit: Unit number to use with the "step_num" file
!  file: Name and path of "step_num" file
! OUTPUS
!  step_num: Number of time steps in a simulation
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE get_em_step_num(unit, file, step_num, iostat)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat, step_num
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           OPEN(unit, FILE=file, IOSTAT=i, ACTION='READ', FORM='UNFORMATTED')
           IF (i == 0) THEN
              READ(unit, IOSTAT=i) step_num
              CLOSE(unit)
           ELSE
              i = i+100
              CLOSE(unit)
           END IF
           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE get_em_step_num
!***

!****is* read_em_sim/open_em_timemap
! NAME
!  SUBROUTINE open_em_timemap(unit, file, iostat)
! PURPOSE
!  Open the "timemap" file used in read_em_timemap()
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "timemap" file with open_em_timemap()
!  3. Loop through all time steps and call read_em_timemap() to get each
!     time value.  The value of kstep should be the same as the loop
!     index.
!  4. Close the "timemap" file with CLOSE(unit)
! INPUTS
!  unit: Unit number to use with the "timemap" file
!  file: Name and path of "timemap" file
! OUTPUTS
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE open_em_timemap(unit, file, iostat)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           OPEN(unit, FILE=file, IOSTAT=i, ACTION='READ', FORM='UNFORMATTED')

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE open_em_timemap
!***

!****is* read_em_sim/read_em_timemap
! NAME
!  SUBROUTINE read_em_timemap(unit, kstep, t, iostat)
! PURPOSE
!  Read the next time and time step values
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "timemap" file with open_em_timemap()
!  3. Loop through all time steps and call read_em_timemap() to get each
!     time value.  The value of kstep should be the same as the loop
!     index.
!  4. Close the "timemap" file with CLOSE(unit)
! INPUTS
!  unit: Unit number to use with the "timemap" file
! OUTPUTS
!  kstep: time step number associated with the value of time returned
!  t: value of time associated with the time step number for a simulation
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE read_em_timemap(unit, kstep, t, iostat)
! Read next timemap value
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat, kstep
           REAL(KIND=8),INTENT(OUT)    :: t
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           READ(unit, IOSTAT=i) kstep, t

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE read_em_timemap
!***

!****is* read_em_sim/open_em_isomap
! NAME
!  SUBROUTINE open_em_isomap(unit, file, iostat)
! PURPOSE
!  Open the "isomap" file used in read_em_isomap()
! STATUS
!  Complete and tested
! USAGE
!  See load_em_isomap() for an example of how to read isomap files.
! NOTES
!  The order of isotopes is crucial for interpreting the abundances and
!  fluxes correctly.  Isotopes are mapped to an integer, and this integer
!  is saved in the binary files.  The "isomap" file contains the info
!  needed to relate the mapping integer to a label or value of z or a.
!  The integer that an isotope is mapped to is simply its index in memory
!  when xnet was running.
!
!  The load_em_isomap() subroutine greatly simplifies this mapping.  It
!  loads all isotope info into a structure of TYPE(em_isomap).  Then, the
!  z, a, and label of the nth isotope can be accessed as isomap%z(n), 
!  isomap%a(n), and isomap%label(n), respectively.  The structure also 
!  holds the number of isotopes in isomap%last_index.  The
!  get_index_to_isotope() function returns the mapping integer of an
!  isotope with a particular value of z, a, or label.
! INPUTS
!  unit: Unit number to use with the "isomap" file
!  file: Name and path of "isomap" file
! OUTPUTS
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE open_em_isomap(unit, file, iostat)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           OPEN(unit, FILE=file, IOSTAT=i, ACTION='READ', FORM='UNFORMATTED')

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE open_em_isomap
!***

!****is* read_em_sim/read_em_isomap
! NAME
!  SUBROUTINE read_em_isomap(unit, index, z, a, label, iostat)
! PURPOSE
!  Read info about the next isotope in the "isomap" file
! STATUS
!  Complete and tested
! USAGE
!  See load_em_isomap() for an example of how to read isomap files.
! NOTES
!  The order of isotopes is crucial for interpreting the abundances and
!  fluxes correctly.  Isotopes are mapped to an integer, and this integer
!  is saved in the binary files.  The "isomap" file contains the info
!  needed to relate the mapping integer to a label or value of z or a.
!  The integer that an isotope is mapped to is simply its index in memory
!  when xnet was running.
!
!  The load_em_isomap() subroutine greatly simplifies this mapping.  It
!  loads all isotope info into a structure of TYPE(em_isomap).  Then, the
!  z, a, and label or the nth isotope can be accessed as isomap%z(n), 
!  isomap%a(n), and isomap%label(n), respectively.  The structure also 
!  holds the number of isotopes in isomap%last_index.  The
!  get_index_to_isotope() function returns the mapping integer of an
!  isotope with a particular value of z, a, or label.
! INPUTS
!  unit: Unit number to use with the "isomap" file
! OUTPUTS
!  index: index or mapping integer of isotope returned
!  z: number of protons of isotope returned
!  a: atomic mass of isotope returned
!  label: 5 character label (Cr54, He4, etc.) of isotope
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE read_em_isomap(unit, index, z, a, label, iostat)
! Read next isomap value
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat, index, z, a
           CHARACTER(LEN=5),INTENT(OUT):: label
           INTEGER                     :: i
           INTEGER(KIND=2)             :: z_in, a_in
           OPTIONAL                    :: iostat

           READ(unit, IOSTAT=i) index, z_in, a_in, label
           z = z_in
           a = a_in

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE read_em_isomap
!***

!****is* read_em_sim/open_em_reacmap
! NAME
!  SUBROUTINE open_em_reacmap(unit, file, iostat)
! PURPOSE
!  Open the "reacmap" file used in read_em_reacmap()
! STATUS
!  Complete and tested
! USAGE
!  See load_em_reacmap() for an example of how to read reacmap files.
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
! INPUTS
!  unit: Unit number to use with the "reacmap" file
!  file: Name and path of "reacmap" file
! OUTPUTS
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/17/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE open_em_reacmap(unit, file, iostat)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           OPEN(unit, FILE=file, IOSTAT=i, ACTION='READ', FORM='UNFORMATTED')

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE open_em_reacmap
!***

!****is* read_em_sim/read_em_reacmap
! NAME
!  SUBROUTINE read_em_reacmap(unit, index, z, a, label, iostat)
! PURPOSE
!  Read info about the next isotope in the "reacmap" file
! STATUS
!  Complete and tested
! USAGE
!  See load_em_reacmap() for an example of how to read reacmap files.
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
! INPUTS
!  unit: Unit number to use with the "reacmap" file
! OUTPUTS
!  index: index or mapping integer of isotope returned
!  reactants: 3 element array with reactant isotope mappings, 0 if no particle
!  products: 4 element array with product isotope mappings, 0 if no particle
!  biblio: biblio code of reaction
!  iwflx: value from xnet for reaction, index to weak reaction for flux?
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE read_em_reacmap(unit, index, reactants, products, biblio, iwflx, iostat)
! Read next reacmap value
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat, index, reactants(3), products(4), iwflx
           CHARACTER(LEN=4),INTENT(OUT):: biblio
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           READ(unit, IOSTAT=i) index, reactants, products, biblio, iwflx

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE read_em_reacmap
!***

!****is* read_em_sim/open_em_thermo
! NAME
!  SUBROUTINE open_em_thermo(unit, file, iostat)
! PURPOSE
!  Open the "thermo" file used in read_em_thermo()
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "thermo" file with open_em_thermo()
!  3. Loop through all time steps and call read_em_thermo() to get each
!     temperature and density value.  The value of kstep should be the 
!     same as the loop index.
!  4. Close the "thermo" file with CLOSE(unit)
! INPUTS
!  unit: Unit number to use with the "thermo" file
!  file: Name and path of "thermo" file
! OUTPUTS
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE open_em_thermo(unit, file, iostat)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           OPEN(unit, FILE=file, IOSTAT=i, ACTION='READ',               &
     &       FORM='UNFORMATTED')

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE open_em_thermo
!***

!****is* read_em_sim/read_em_thermo
! NAME
!  SUBROUTINE read_em_thermo(unit, kstep, t, iostat)
! PURPOSE
!  Read the next temperature, density, and time step values
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "thermo" file with open_em_thermo()
!  3. Loop through all time steps and call read_em_thermo() to get each
!     temperature and density value.  The value of kstep should be the
!     same as the loop index.
!  4. Close the "thermo" file with CLOSE(unit)
! INPUTS
!  unit: Unit number to use with the "thermo" file
! OUTPUTS
!  kstep: time step number associated with the values returned
!  t9: value of temperature associated with the time step number
!  rhot: value of density associated with the time step number
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE read_em_thermo(unit, kstep, t9, rhot, iostat)
! Read next temperature and density values
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           INTEGER,INTENT(OUT)         :: iostat, kstep
           REAL(KIND=8),INTENT(OUT)    :: t9, rhot
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           READ(unit, IOSTAT=i) kstep, t9, rhot

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE read_em_thermo
!***

!****is* read_em_sim/open_em_abund
! NAME
!  SUBROUTINE open_em_abund(unit, cnt_unit, file, iostat)
! PURPOSE
!  Open the "abund" and "abund_cnt" files used in read_em_abund()
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "abund" and "abund_cnt" files with open_em_abund()
!  3. Loop through all time steps and call read_em_abund() to get all the
!     non-zero abundances for each time step.  The value of kstep should
!     be the same as the loop index.
!  4. Close the "abund" and "abund_cnt" files with CLOSE(unit) and
!     CLOSE(cnt_unit)
!  
!  The abund array contains all the non-zero abundances for a time step.
!  The isotope mapping index that each abund value is associated with is
!  stored in iso_ind.  For example, the isotope mapping of the abund(5)
!  value is in iso_ind(5).  The label of this isotope is in 
!  isomap%label(iso_ind(5)), assuming isomap was read using load_em_isomap
! NOTES
!  See the load_em_isomap() subroutine for an explanation of how isotopes
!  are mapped.
! INPUTS
!  unit: Unit number to use with the "abund" file
!  cnt_unit: Unit number to use with the "abund_cnt" file
!  file: Name and path of "abund" file
! OUTPUTS
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE open_em_abund(unit, cnt_unit, file, iostat)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit, cnt_unit
           INTEGER,INTENT(OUT)         :: iostat
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           OPEN(unit, FILE=file, IOSTAT=i, ACTION='READ',               &
     &          FORM='UNFORMATTED')
           IF (i == 0) OPEN(cnt_unit, FILE=TRIM(file) // '_cnt',        &
     &          IOSTAT=i, ACTION='READ', FORM='UNFORMATTED')

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE open_em_abund
!***

!****is* read_em_sim/read_em_abund
! NAME
!  SUBROUTINE read_em_abund(unit, cnt_unit, kstep, num_nz, iso_ind, abund,
!                           iostat)
! PURPOSE
!  Read the abundances for the next time step value from the "abund" file
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "abund" and "abund_cnt" files with open_em_abund()
!  3. Loop through all time steps and call read_em_abund() to get all the
!     non-zero abundances for each time step.  The value of kstep should
!     be the same as the loop index.
!  4. Close the "abund" and "abund_cnt" files with CLOSE(unit) and
!     CLOSE(cnt_unit)
!  
!  The abund array contains all the non-zero abundances for a time step.
!  The isotope mapping index that each abund value is associated with is
!  stored in iso_ind.  For example, the isotope mapping of the abund(5)
!  value is in iso_ind(5).  The label of this isotope is in 
!  isomap%label(iso_ind(5)), assuming isomap was read using load_em_isomap
! NOTES
!  See the load_em_isomap() subroutine for an explanation of how isotopes
!  are mapped.
! INPUTS
!  unit: Unit number to use with the "abund" file
!  cnt_unit: Unit number to use with the "abund_cnt" file
! OUTPUTS
!  kstep: time step number associated with the values returned
!  num_nz: number of values in iso_ind and non-zero abundances in abund
!  iso_ind: array of isotope mappings associated with values in abund
!  abund: array of all non-zero abundances associated with the time step
!         number
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE read_em_abund(unit, cnt_unit, kstep, num_nz,        &
      &         iso_ind, abund, iostat)
! Read next abundance values
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit, cnt_unit
           INTEGER,INTENT(OUT)         :: iostat, kstep, num_nz
           INTEGER,INTENT(OUT)         :: iso_ind(:)
           REAL(KIND=8),INTENT(OUT)    :: abund(:)
           INTEGER                     :: i, j, file_pos
           OPTIONAL                    :: iostat

! Read overhead info
           READ(cnt_unit, IOSTAT=i) kstep, num_nz, file_pos

           IF (i == 0) THEN
! Put abund file position in correct place
              i = FSEEK(unit, file_pos, 0)
              IF (i == 0) THEN
! Read in kstep from abund file
                 READ(unit, IOSTAT=i) j
! Make sure ksteps from both files agree
                 IF ((j == kstep) .AND. (i == 0)) THEN
                    j = 1
                    DO WHILE ((i == 0) .AND. (j <= num_nz)) 
                       IF ((j <= UBOUND(iso_ind,1)) .AND. &
                            (j <= UBOUND(abund,1))) THEN
                          READ(unit, IOSTAT=i) iso_ind(j), abund(j)
                          j = j + 1
                       ELSE
! iso_ind or abund is too small
                          i = -334
                       END IF
                    END DO
                 ELSE IF (i == 0) THEN
! ksteps from files don't agree
                    i = -333
                 END IF
              END IF
           END IF
           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE read_em_abund
!***

!****is* read_em_sim/seek_em_abund
! NAME
!  SUBROUTINE seek_em_abund(unit, cnt_unit, kstep, iostat)
! PURPOSE
!  Seek "abund" file to later timestep
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "abund" and "abund_cnt" files with open_em_abund()
!  3. Call seek_em_abund() with desired timestep
!  4. Loop through all time steps and call read_em_abund() to get all the
!     non-zero abundances for each time step.  The value of kstep should
!     be the same as the loop index.
!  5. Close the "abund" and "abund_cnt" files with CLOSE(unit) and
!     CLOSE(cnt_unit)
! INPUTS
!  unit: Unit number to use with the "abund" file
!  cnt_unit: Unit number to use with the "abund_cnt" file
!  kstep: time step number to jump to
! OUTPUTS
!  iostat: Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  12/15/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE seek_em_abund(unit, cnt_unit, step, iostat)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit, cnt_unit, step
           INTEGER,INTENT(OUT)         :: iostat
           INTEGER                     :: kstep, num_nz, file_pos

           iostat = 0
           kstep = 0

! Jump to desired time step
           DO WHILE (kstep < step .AND. iostat == 0)
! Read overhead info
              READ(cnt_unit, IOSTAT=iostat) kstep, num_nz, file_pos
           END DO
         END SUBROUTINE seek_em_abund
!***

!****is* read_em_sim/open_em_flux
! NAME
!  SUBROUTINE open_em_flux(unit, cnt_unit, file, iostat)
! PURPOSE
!  Open the "flux" and "flux_cnt" files used in read_em_flux()
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "flux" and "flux_cnt" files with open_em_flux()
!  3. Loop through all time steps and call read_em_flux() to get all the
!     non-zero fluxes for each time step.  The value of kstep should
!     be the same as the loop index.
!  4. Close the "flux" and "flux_cnt" files with CLOSE(unit) and
!     CLOSE(cnt_unit)
!  
!  The flux array contains all the non-zero fluxes for a time step.
!  The reaction mapping index that each flux value is associated with is
!  stored in reac_ind.  For example, the reaction mapping of the flux(5)
!  value is in reac_ind(5).  The biblio code of this reaction is in 
!  reacmap%biblio(reac_ind(5)), assuming reacmap was read using load_em_reacmap
! NOTES
!  See the load_em_reacmap() subroutine for an explanation of how reactions
!  are mapped.
! INPUTS
!  unit: Unit number to use with the "flux" file
!  cnt_unit: Unit number to use with the "flux_cnt" file
!  file: Name and path of "flux" file
! OUTPUTS
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/19/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE open_em_flux(unit, cnt_unit, file, iostat)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit, cnt_unit
           INTEGER,INTENT(OUT)         :: iostat
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER                     :: i
           OPTIONAL                    :: iostat

           OPEN(unit, FILE=file, IOSTAT=i, ACTION='READ', FORM='UNFORMATTED')
           IF (i == 0) OPEN(cnt_unit, FILE=TRIM(file) // '_cnt', IOSTAT=i, &
                ACTION='READ', FORM='UNFORMATTED')

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE open_em_flux
!***

!****is* read_em_sim/read_em_flux
! NAME
!  SUBROUTINE read_em_flux(unit, cnt_unit, kstep, num_nz, reac_ind, flux,
!                           iostat)
! PURPOSE
!  Read the fluxes for the next time step value from the "flux" file
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Open the "flux" and "flux_cnt" files with open_em_flux()
!  3. Loop through all time steps and call read_em_flux() to get all the
!     non-zero fluxes for each time step.  The value of kstep should
!     be the same as the loop index.
!  4. Close the "flux" and "flux_cnt" files with CLOSE(unit) and
!     CLOSE(cnt_unit)
!  
!  The flux array contains all the non-zero fluxes for a time step.
!  The reaction mapping index that each flux value is associated with is
!  stored in reac_ind.  For example, the reaction mapping of the flux(5)
!  value is in reac_ind(5).  The biblio code of this reaction is in 
!  reacmap%biblio(reac_ind(5)), assuming reacmap was read using load_em_reacmap
! NOTES
!  See the load_em_reacmap() subroutine for an explanation of how reactions
!  are mapped.
! INPUTS
!  unit: Unit number to use with the "flux" file
!  cnt_unit: Unit number to use with the "flux_cnt" file
! OUTPUTS
!  kstep: time step number associated with the values returned
!  num_nz: number of values in reac_ind and non-zero fluxes in flux
!  reac_ind: array of reaction mappings associated with values in flux
!  flux: array of all non-zero fluxes associated with the time step
!         number
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/19/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE read_em_flux(unit, cnt_unit, kstep, num_nz, reac_ind, flux, &
              iostat)
! Read next flux values
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit, cnt_unit
           INTEGER,INTENT(OUT)         :: iostat, kstep, num_nz, reac_ind(:)
           REAL(KIND=8),INTENT(OUT)    :: flux(:)
           INTEGER                     :: i, j, file_pos
           OPTIONAL                    :: iostat

! Read overhead info
           READ(cnt_unit, IOSTAT=i) kstep, num_nz, file_pos

           IF (i == 0) THEN
! Put flux file position in correct place
              i = FSEEK(unit, file_pos, 0)

              IF (i == 0) THEN
! Read in kstep from flux file
                 READ(unit, IOSTAT=i) j
! Make sure ksteps from both files agree
                 IF ((j == kstep) .AND. (i == 0)) THEN
                    j = 1
                    DO WHILE ((i == 0) .AND. (j <= num_nz)) 
                       IF ((j <= UBOUND(reac_ind,1)) .AND. &
                            (j <= UBOUND(flux,1))) THEN

                          READ(unit, IOSTAT=i) reac_ind(j), flux(j)
                          j = j + 1
                       ELSE
! reac_ind or flux is too small
                          i = -334
                       END IF
                    END DO
                 ELSE IF (i == 0) THEN
! ksteps from files don't agree
                    i = -333
                 END IF
              END IF
           END IF

           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE read_em_flux
!***

!****is* read_em_sim/read_em_flux_summed
! NAME
!  SUBROUTINE read_em_flux_summed(unit, cnt_unit, kstep, num_nz, reac_ind, 
!                                 flux, matchmap, iostat)
! PURPOSE
!  Read the fluxes for the next time step value from the "flux" file
! STATUS
!  Complete and tested
! USAGE
!  1. Get the number of time steps with get_em_step_num()
!  2. Get isomap, reacmap, and matchmap
!  3. Open the "flux" and "flux_cnt" files with open_em_flux()
!  4. Loop through all time steps and call read_em_flux() to get all the
!     non-zero fluxes for each time step.  The value of kstep should
!     be the same as the loop index.
!  5. Close the "flux" and "flux_cnt" files with CLOSE(unit) and
!     CLOSE(cnt_unit)
!  
!  The flux array contains all the non-zero fluxes for a time step.
!  The reaction mapping index that each flux value is associated with is
!  stored in reac_ind.  For example, the reaction mapping of the flux(5)
!  value is in reac_ind(5).  The biblio code of this reaction is in 
!  reacmap%biblio(reac_ind(5)), assuming reacmap was read using load_em_reacmap
! NOTES
!  See the load_em_reacmap() subroutine for an explanation of how reactions
!  are mapped.  This subroutine is identical to read_em_flux except that
!  the flux of reactions going between the same isotopes is summed.
! INPUTS
!  unit: Unit number to use with the "flux" file
!  cnt_unit: Unit number to use with the "flux_cnt" file
!  matchmap: Variable with list of reactions to sum
! OUTPUTS
!  kstep: time step number associated with the values returned
!  num_nz: number of values in reac_ind and non-zero fluxes in flux
!  reac_ind: array of reaction mappings associated with values in flux
!  flux: array of all non-zero fluxes associated with the time step
!         number
!  iostat (OPTIONAL): Value of IOSTAT.  Zero is returned if successful.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/20/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE read_em_flux_summed(unit, cnt_unit, kstep, num_nz, reac_ind, flux, &
              matchmap, iostat)
! Read next flux values
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit, cnt_unit
           TYPE(em_matchmap)           :: matchmap
           INTEGER,INTENT(OUT)         :: iostat, kstep, num_nz, reac_ind(:)
           REAL(KIND=8),INTENT(OUT)    :: flux(:)
           INTEGER                     :: i, j, k, file_pos, reaction, array_size, read_cnt
           REAL(KIND=8)                :: f
           LOGICAL                     :: foundit
           OPTIONAL                    :: iostat

! Read overhead info
           READ(cnt_unit, IOSTAT=i) kstep, num_nz, file_pos
!!$    print '(A)',''
!!$    print *,'reading step ',kstep

           IF (i == 0) THEN
! Put flux file position in correct place
              i = FSEEK(unit, file_pos, 0)

              IF (i == 0) THEN
! Read in kstep from flux file
                 READ(unit, IOSTAT=i) j
! Make sure ksteps from both files agree
                 IF ((j == kstep) .AND. (i == 0)) THEN

                    array_size = 1
                    read_cnt = 1
                    DO WHILE ((i == 0) .AND. (read_cnt <= num_nz)) 
                       READ(unit, IOSTAT=i) reaction, f
                       IF (i == 0) THEN
                          read_cnt = read_cnt + 1
                          j = get_addto(matchmap, reaction)
!!$                   print '(4(A,I0),A,E)','read_cnt=',read_cnt,' Timestep=', &
!!$                        kstep,' reaction=',reaction,' addto=',j,' flux=',f

                          IF (j /= reaction) THEN
! Flux should be added
! Check if j is already in reac_ind, 
! if not set j == reaction so it will be added
                             k = 0
                             foundit = .FALSE.
                             DO WHILE (k < array_size)
                                IF (reac_ind(k) == j) THEN
                                   flux(k) = flux(k) + f
!!$                            print '(A,I0,A,E)',' SUMMED with ',k,' to get ',flux(k)
                                   k = array_size
                                   foundit = .TRUE.
                                ELSE
                                   k = k + 1
                                END IF
                             END DO

                             IF (.NOT. foundit) THEN
                                reaction = j
!!$                         print '(A)',' adding new entry'
                             END IF
                          END IF

                          IF (j == reaction) THEN
!print '(A)',' adding new entry'
! Should not be summed

                             IF ((array_size <= UBOUND(reac_ind,1)) .AND. &
                                  (array_size <= UBOUND(flux,1))) THEN

                                reac_ind(array_size) = reaction
                                flux(array_size) = f
                                array_size = array_size + 1


                             ELSE
! reac_ind or flux is too small


                                i = -334
                             END IF


                          END IF


                       END IF


                       j = j + 1
                    END DO


                 ELSE IF (i == 0) THEN
! ksteps from files don't agree
                    i = -333
                 END IF


              END IF
           END IF
           IF (PRESENT(iostat)) iostat = i
         END SUBROUTINE read_em_flux_summed
!***

!****is* read_em_sim/load_em_isomap
! NAME
!  SUBROUTINE load_em_isomap(unit, file, isomap, iostat, err_str)
! PURPOSE
!  Load the "isomap" file into memory as an em_isomap derived type
! STATUS
!  Complete and tested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the isomap
!     Example: TYPE(em_isomap),INTENT(OUT) :: isomap
!  3. Load the isomap into memory by giving load_em_isomap a filename and
!     unit number to use.  
!     Example: CALL load_em_isomap(22, 'isomap', isomap, iostat=status)
!  4. Use the get_index_to_isotope() function and the isomap variable to
!     retrieve information about isotopes and their integer mapping.
! NOTES
!  The order of isotopes is crucial for interpreting the abundances and
!  fluxes correctly.  Isotopes are mapped to an integer, and this integer
!  is saved in the binary files.  The "isomap" file contains the info
!  needed to relate the mapping integer to a label or value of z or a.
!  The integer that an isotope is mapped to is simply its index in memory
!  when xnet was running.
!
!  The load_em_isomap() subroutine greatly simplifies this mapping.  It
!  loads all isotope info into a structure of TYPE(em_isomap).  Then, the
!  z, a, and label or the nth isotope can be accessed as isomap%z(n), 
!  isomap%a(n), and isomap%label(n), respectively.  The structure also 
!  holds the number of isotopes in isomap%last_index.  The
!  get_index_to_isotope() function returns the mapping integer of an
!  isotope with a particular value of z, a, or label.
! INPUTS
!  unit: Unit number to use with the "isomap" file
!  file: Name and path of "isomap" file
! OUTPUTS
!  isomap: Variable holding all info from "isomap" file
!  iostat: Value of IOSTAT.  Zero is returned if successful.
!  err_str (OPTIONAL): text explanation of error number
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started subroutine
! SOURCE

         SUBROUTINE load_em_isomap(unit, file, isomap, iostat, err_str)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER,INTENT(OUT)         :: iostat
           TYPE(em_isomap),INTENT(OUT) :: isomap
           CHARACTER(LEN=*),INTENT(OUT):: err_str
           INTEGER                     :: counter, index
           OPTIONAL                    :: err_str

           CALL open_em_isomap(unit, file, iostat)
           IF (iostat /= 0) THEN
              IF (PRESENT(err_str)) WRITE(err_str,'(A,I0,A)') 'Error ', iostat, &
                   ' opening isomap.'
              RETURN
           END IF

           isomap%z = -333
           isomap%a = -333
           isomap%label = ''

           counter = 0
           DO WHILE (iostat == 0)
              IF (counter > UBOUND(isomap%z,1)) THEN
                 iostat = HUGE(iostat)  ! Signal that an error occurred
                 IF (PRESENT(err_str)) WRITE(err_str,'(A,I0,A)') &
                      'Size of isomap(', UBOUND(isomap%z, 1), &
                      ') is too small to hold all isotopes.'
                 RETURN
              END IF

              CALL read_em_isomap(unit, index, isomap%z(counter), &
                   isomap%a(counter), isomap%label(counter), iostat)
              
              IF (iostat == 0) THEN
! Success

! Make sure index from file matches counter
                 IF (index /= counter) THEN
                    iostat = HUGE(iostat)  ! Signal that an error occurred
                    IF (PRESENT(err_str)) WRITE(err_str,'(A,I0,A,I0)') &
                         'Invalid isotope mapping: counter=', counter, &
                         ' index=', index
                    RETURN
                 END IF

                 counter = counter + 1
              ELSE IF (iostat == -1) THEN
! Last isotope read in
                 isomap%last_index = counter - 1
              ELSE
! Problem
                 isomap%last_index = counter - 2
              END IF
           END DO
           CLOSE(unit)
           IF (iostat == -1) iostat = 0
         END SUBROUTINE load_em_isomap
!***

!****if* read_em_sim/get_index_to_isotope
! NAME
!  FUNCTION get_index_to_isotope(isomap, z, a, label, alli)
! PURPOSE
!  Return the mapping integer(s) of an isotope with a particular value of
!  z, a, or label.
! STATUS
!  Complete and tested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the isomap
!     Example: TYPE(em_isomap),INTENT(OUT) :: isomap
!  3. Load the isomap into memory by giving load_em_isomap a filename and
!     unit number to use.  
!     Example: CALL load_em_isomap(22, 'isomap', isomap, iostat=status)
!  4. Use the get_index_to_isotope() function and the isomap variable to
!     retrieve information about isotopes and their integer mapping.
! NOTES
!  The order of isotopes is crucial for interpreting the abundances and
!  fluxes correctly.  Isotopes are mapped to an integer, and this integer
!  is saved in the binary files.  The "isomap" file contains the info
!  needed to relate the mapping integer to a label or value of z or a.
!  The integer that an isotope is mapped to is simply its index in memory
!  when xnet was running.
!
!  The load_em_isomap() subroutine greatly simplifies this mapping.  It
!  loads all isotope info into a structure of TYPE(em_isomap).  Then, the
!  z, a, and label or the nth isotope can be accessed as isomap%z(n), 
!  isomap%a(n), and isomap%label(n), respectively.  The structure also 
!  holds the number of isotopes in isomap%last_index.  The
!  get_index_to_isotope() function returns the mapping integer of an
!  isotope with a particular value of z, a, or label.
!
!  By default, only the first index that matches is returned.  Sometimes,
!  simulations will contain two states of Al26 and if the alli array is not
!  present, only one index is returned.  If both states are needed,
!  include the alli array.
! INPUTS
!  isomap: Variable holding all info from "isomap" file
!  z (OPTIONAL): number of protons of isotope to get mapping index of
!  a (OPTIONAL): atomic mass of isotope to get mapping index of
!  label (OPTIONAL): 5 character label of isotope to get mapping index of
!  
!  Values of (z and a) or a label should be provided.
! OUTPUTS
!  alli (OPTIONAL): Array of all indicies that match z,a or label
!                   loop through alli until negative value found
! RETURN VALUE
!  Integer isotope mapping of an isotope with a particular value of z, a,
!  or label.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/29/2004 jpscott       Started function
!  11/18/2005 jpscott       Works for multiple states of same isotope
! SOURCE

         FUNCTION get_index_to_isotope(isomap, z, a, label, alli)
           IMPLICIT NONE
           TYPE(em_isomap),INTENT(IN)  :: isomap
           INTEGER,INTENT(IN)          :: z, a
           CHARACTER(LEN=*),INTENT(IN) :: label
           INTEGER,INTENT(OUT)         :: alli(:)
           INTEGER                     :: i, get_index_to_isotope, alli_cnt
           OPTIONAL                    :: z, a, label, alli
           
           get_index_to_isotope = -4321
           IF (PRESENT(alli)) alli = -33
           alli_cnt = 0

           IF (PRESENT(label)) THEN
! Search for label
              DO i = 0, isomap%last_index
                 IF (label == isomap%label(i)) THEN
                    get_index_to_isotope = i
                    IF (PRESENT(alli)) THEN
                       alli_cnt = alli_cnt + 1
                       IF (alli_cnt >= UBOUND(alli,1)) RETURN
                       alli(alli_cnt) = i
                    ELSE
                       RETURN
                    END IF
                 END IF
              END DO
           ELSE IF (PRESENT(z) .AND. PRESENT(a)) THEN
! Search for z and a
              DO i = 0, isomap%last_index
                 IF ((z == isomap%z(i)) .AND. (a == isomap%a(i))) THEN
                    get_index_to_isotope = i
                    IF (PRESENT(alli)) THEN
                       alli_cnt = alli_cnt + 1
                       IF (alli_cnt >= UBOUND(alli,1)) RETURN
                       alli(alli_cnt) = i
                    ELSE
                       RETURN
                    END IF
                 END IF
              END DO
           END IF
         END FUNCTION get_index_to_isotope
!***

!****is* read_em_sim/load_em_reacmap
! NAME
!  SUBROUTINE load_em_reacmap(unit, file, reacmap, iostat, err_str)
! PURPOSE
!  Load the "reacmap" file into memory as an em_reacmap derived type
! STATUS
!  Untested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the reacmap
!     Example: TYPE(em_reacmap),INTENT(OUT) :: reacmap
!  3. Load the reacmap into memory by giving load_em_reacmap a filename and
!     unit number to use.  
!     Example: CALL load_em_reacmap(22, 'reacmap', reacmap, iostat=status)
!  4. Use the get_index_to_reaction() function and the reacmap variable to
!     retrieve information about reactions and their integer mappings.
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
! INPUTS
!  unit: Unit number to use with the "reacmap" file
!  file: Name and path of "reacmap" file
! OUTPUTS
!  reacmap: Variable holding all info from "reacmap" file
!  iostat: Value of IOSTAT.  Zero is returned if successful.
!  err_str (OPTIONAL): text explanation of error number
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/19/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE load_em_reacmap(unit, file, reacmap, iostat, err_str)
           IMPLICIT NONE
           INTEGER,INTENT(IN)          :: unit
           CHARACTER(LEN=*),INTENT(IN) :: file
           INTEGER,INTENT(OUT)         :: iostat
           TYPE(em_reacmap),INTENT(OUT):: reacmap
           CHARACTER(LEN=*),INTENT(OUT):: err_str
           INTEGER                     :: counter, index
           OPTIONAL                    :: err_str

           CALL open_em_reacmap(unit, file, iostat)
           IF (iostat /= 0) THEN
              IF (PRESENT(err_str)) WRITE(err_str,'(A,I0,A)') 'Error ', iostat, &
                   ' opening reacmap.'
              RETURN
           END IF

           reacmap%reactants = -333
           reacmap%products = -333
           reacmap%biblio = ''
           reacmap%iwflx = -333

           counter = 0
           DO WHILE (iostat == 0)
              IF (counter > UBOUND(reacmap%biblio,1)) THEN
                 iostat = HUGE(iostat)  ! Signal that an error occurred
                 IF (PRESENT(err_str)) WRITE(err_str,'(A,I0,A)') &
                      'Size of reacmap(', UBOUND(reacmap%biblio, 1), &
                      ') is too small to hold all reactions.'
                 RETURN
              END IF

              CALL read_em_reacmap(unit, index, reacmap%reactants(counter,:), &
                   reacmap%products(counter,:), reacmap%biblio(counter), &
                   reacmap%iwflx(counter), iostat)
              
              IF (iostat == 0) THEN
! Success
!!$          print *,reacmap%reactants(counter,:)
!!$          print *,reacmap%products(counter,:)
!!$          print *,index,reacmap%biblio(counter),reacmap%iwflx(counter)

! Make sure index from file matches counter
                 IF (index /= counter) THEN
                    iostat = HUGE(iostat)  ! Signal that an error occurred
                    IF (PRESENT(err_str)) WRITE(err_str,'(A,I0,A,I0)') &
                         'Invalid reaction mapping: counter=', counter, &
                         ' index=', index
                    RETURN
                 END IF
                 counter = counter + 1
              ELSE IF (iostat == -1) THEN
! Last reaction read in
                 reacmap%last_index = counter - 1
              ELSE
! Problem
                 reacmap%last_index = counter - 2
              END IF
           END DO
           CLOSE(unit)

           IF (iostat == -1) iostat = 0
         END SUBROUTINE load_em_reacmap
!***

!****if* read_em_sim/get_index_to_reaction
! NAME
!  FUNCTION get_index_to_reaction(reacmap, z, a, label)
! PURPOSE
!  Return the mapping integer of a reaction with a particular set of 
!  isotope mappings.
! STATUS
!  Untested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the reacmap
!     Example: TYPE(em_reacmap),INTENT(OUT) :: reacmap
!  3. Load the reacmap into memory by giving load_em_reacmap a filename and
!     unit number to use.  
!     Example: CALL load_em_reacmap(22, 'reacmap', reacmap, iostat=status)
!  4. Use the get_index_to_reaction() function and the reacmap variable to
!     retrieve information about reactions and their integer mapping.
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
! INPUTS
!  reacmap: Variable holding all info from "reacmap" file
!  reactants: 3 element array of reactant isotope mappings
!  products: 4 element array of product isotope mappings
! RETURN VALUE
!  Integer mapping of an reaction with a particular set of isotope 
!  mappings.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/19/2005 jpscott       Started function
! SOURCE

         FUNCTION get_index_to_reaction(reacmap, reactants, products)
           IMPLICIT NONE
           TYPE(em_reacmap),INTENT(IN)  :: reacmap
           INTEGER,INTENT(IN)          :: reactants(3),products(4)
           INTEGER                     :: i, get_index_to_reaction
           
           get_index_to_reaction = -4321

           DO i = 0, reacmap%last_index
              IF (ALL(reactants == reacmap%reactants(i,:)) .AND.  &
                   ALL(products == reacmap%products(i,:))) THEN
                 get_index_to_reaction = i
                 RETURN
              END IF
           END DO
         END FUNCTION get_index_to_reaction
!***

!****is* read_em_sim/get_channel_heavyweights
! NAME
!  SUBROUTINE get_channel_heavyweights(reacmap, index, isomap, in, out)
! PURPOSE
!  Return heaviest isotope for input and output channel for reaction
! STATUS
!  Complete and tested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the reacmap and isomap
!     Example: TYPE(em_reacmap),INTENT(OUT) :: reacmap
!  3. Load the reacmap and isomap into memory by giving load_em_reacmap and 
!     load_em_isomap a filename and unit number to use.  
!     Example: CALL load_em_reacmap(22, 'reacmap', reacmap, iostat=status)
!  4. Use the get_channel_heavyweights() function and the reacmap variable to
!     retrieve the heaviest input and output isotopes.
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
!
!  The isotope with the most protons is considered the heaviest.  If two
!  isotopes meet this criteria, the one with the most neutrons is used.
! INPUTS
!  reacmap: Variable holding all info from "reacmap" file
!  index: index to a specific reaction in reacmap
!  isomap: Variable holding all info from "isomap" file
! OUTPUTS
!  in: isotope index to heaviest reactant
!  out: isotope index to heaviest product
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  08/23/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE get_channel_heavyweights(reacmap, index, isomap, in, out)
           IMPLICIT NONE
           TYPE(em_reacmap),INTENT(IN) :: reacmap
           TYPE(em_isomap),INTENT(IN)  :: isomap
           INTEGER,INTENT(IN)          :: index
           INTEGER,INTENT(OUT)         :: in, out
           INTEGER                     :: i

           in = -1
           out = -1
           IF (index > reacmap%last_index) RETURN

! Set defaults
           in = reacmap%reactants(index,1)
           out = reacmap%products(index,1)
           
! Find heaviest reactant
           DO i = 2, 3
              IF (reacmap%reactants(index,i) > -1) THEN
                 IF (isomap%z(reacmap%reactants(index,i)) > isomap%z(in) .OR. &
                      (isomap%z(reacmap%reactants(index,i)) == isomap%z(in) .AND. &
                      isomap%a(reacmap%reactants(index,i)) > isomap%a(in))) THEN
                    in = reacmap%reactants(index,i)
                 END IF
              END IF
           END DO

! Find heaviest product
           DO i = 2, 4
              IF (reacmap%products(index,i) > -1) THEN
                 IF (isomap%z(reacmap%products(index,i)) > isomap%z(out) .OR. &
                      (isomap%z(reacmap%products(index,i)) == isomap%z(out) .AND. &
                      isomap%a(reacmap%products(index,i)) > isomap%a(out))) THEN
                    out = reacmap%products(index,i)
                 END IF
              END IF
           END DO

         END SUBROUTINE get_channel_heavyweights
!***

!****is* read_em_sim/match_heavyweights
! NAME
!  SUBROUTINE match_heavyweights(reacmap, isomap, matchmap)
! PURPOSE
!  Put matching reactions into matchmap
! STATUS
!  Complete and tested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the reacmap, isomap, and matchmap
!     Example: TYPE(em_reacmap),INTENT(OUT) :: reacmap
!  3. Load the reacmap and isomap into memory by giving load_em_reacmap and 
!     load_em_isomap a filename and unit number to use.  
!     Example: CALL load_em_reacmap(22, 'reacmap', reacmap, iostat=status)
!  4. Load the matchmap with match_heavyweights
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
!
!  The isotope with the most protons is considered the heaviest.  If two
!  isotopes meet this criteria, the one with the most neutrons is used.
! INPUTS
!  reacmap: Variable holding all info from "reacmap" file
!  isomap: Variable holding all info from "isomap" file
! OUTPUTS
!  matchmap: Variable with list of reactions to sum
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/20/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE match_heavyweights(reacmap, isomap, matchmap)
           IMPLICIT NONE
           TYPE(em_reacmap),INTENT(IN) :: reacmap
           TYPE(em_isomap),INTENT(IN)  :: isomap
           TYPE(em_matchmap),INTENT(OUT):: matchmap
           INTEGER                     :: in(0:MAX_NUM_REACTIONS)
           INTEGER                     :: out(0:MAX_NUM_REACTIONS)
           INTEGER                     :: chk,cmp

           matchmap%last_index = -1

! Get heavyweights for all reactions
           DO chk = 0, reacmap%last_index
              CALL get_channel_heavyweights(reacmap, chk, isomap, in(chk), out(chk))
!print *,chk,isomap%label(in(chk)),in(chk),isomap%label(out(chk)),out(chk)
           END DO

! Compare reaction with all previous reactions
           DO chk = 1, reacmap%last_index
              cmp = 0
              DO WHILE (cmp < chk)
                 IF (in(cmp) == in(chk) .AND. out(cmp) == out(chk)) THEN
                    matchmap%last_index = matchmap%last_index + 1
                    matchmap%duplicate(matchmap%last_index) = chk
                    matchmap%addto(matchmap%last_index) = cmp
                    cmp = chk ! exit loop
                 END IF
                 cmp = cmp + 1
              END DO
           END DO

! Print out matchmap
!!$    DO cmp = 0, matchmap%last_index
!!$       print '(I0,A,I0)',matchmap%duplicate(cmp),' should be added to ',matchmap%addto(cmp)
!!$       print '(3A)',TRIM(get_reac_str(reacmap,isomap,matchmap%duplicate(cmp))), &
!!$            ' should be added to ', &
!!$            TRIM(get_reac_str(reacmap,isomap,matchmap%addto(cmp)))
!!$    END DO
         END SUBROUTINE match_heavyweights
!***

!****is* read_em_sim/print_matchmap
! NAME
!  SUBROUTINE print_matchmap(reacmap, isomap, matchmap)
! PURPOSE
!  Print out which reactions in reacmap will be summed
! STATUS
!  Complete and tested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the reacmap, isomap, and matchmap
!     Example: TYPE(em_reacmap),INTENT(OUT) :: reacmap
!  3. Load the reacmap and isomap into memory by giving load_em_reacmap and 
!     load_em_isomap a filename and unit number to use.  
!     Example: CALL load_em_reacmap(22, 'reacmap', reacmap, iostat=status)
!  4. Load the matchmap with match_heavyweights
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
!
!  The isotope with the most protons is considered the heaviest.  If two
!  isotopes meet this criteria, the one with the most neutrons is used.
! INPUTS
!  reacmap: Variable holding all info from "reacmap" file
!  isomap: Variable holding all info from "isomap" file
!  matchmap: Variable with list of reactions to sum
! RETURN VALUE
!  INT of reaction flux should be added to, if i is not in matchmap, i is
!  returned.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/20/2005 jpscott       Started subroutine
! SOURCE

         SUBROUTINE print_matchmap(reacmap, isomap, matchmap)
           IMPLICIT NONE
           TYPE(em_reacmap),INTENT(IN) :: reacmap
           TYPE(em_isomap),INTENT(IN)  :: isomap
           TYPE(em_matchmap),INTENT(IN):: matchmap
           INTEGER                     :: i,j,m,c
           LOGICAL                     :: found1

           m = MAXVAL(matchmap%duplicate(0:matchmap%last_index))
           c = 1
           DO i = 0, m
              found1 = .FALSE.
! Look through matchmap for one that adds to i
              DO j = 0, matchmap%last_index
                 IF (matchmap%addto(j) == i) THEN
                    IF (.NOT. found1) THEN
                       WRITE(*,'(A,I0,3A)') 'Group ',c,': reactions added to ', &
                            TRIM(get_reac_str(reacmap,isomap,i)),' {'
                       found1 = .TRUE.
                       c = c + 1
                    END IF
                    
                    WRITE(*,'(T5,A)') TRIM(get_reac_str(reacmap,isomap,matchmap%duplicate(j)))
                 END IF
              END DO
              IF (found1) WRITE(*,'(T5,A)') '}'
           END DO
         END SUBROUTINE print_matchmap
!***

!****if* read_em_sim/get_addto
! NAME
!  FUNCTION get_addto(matchmap, i)
! PURPOSE
!  Return index of reaction flux should be added to
! STATUS
!  Complete add tested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the reacmap, isomap, and matchmap
!     Example: TYPE(em_reacmap),INTENT(OUT) :: reacmap
!  3. Load the reacmap and isomap into memory by giving load_em_reacmap and 
!     load_em_isomap a filename and unit number to use.  
!     Example: CALL load_em_reacmap(22, 'reacmap', reacmap, iostat=status)
!  4. Load the matchmap with match_heavyweights
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
!
!  The isotope with the most protons is considered the heaviest.  If two
!  isotopes meet this criteria, the one with the most neutrons is used.
! INPUTS
!  matchmap: Variable with list of reactions to sum
!  i: index to a specific reaction in matchmap
! RETURN VALUE
!  INT of reaction flux should be added to, if i is not in matchmap, i is
!  returned.
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/20/2005 jpscott       Started function
! SOURCE

         FUNCTION get_addto(matchmap, i)
           IMPLICIT NONE
           TYPE(em_matchmap),INTENT(IN):: matchmap
           INTEGER,INTENT(IN)          :: i
           INTEGER                     :: get_addto
           INTEGER                     :: n

           DO n = 0, matchmap%last_index
              IF (matchmap%duplicate(n) == i) THEN
                 get_addto = matchmap%addto(n)
                 RETURN
              END IF
           END DO
           get_addto = i
         END FUNCTION get_addto
!***

!****if* read_em_sim/get_reac_str
! NAME
!  FUNCTION get_reac_str(reacmap, isomap, i)
! PURPOSE
!  Return string representing reaction i in reacmap
! STATUS
!  Complete and tested
! USAGE
!  1. Use this module.  Example: USE read_em_sim
!  2. Allocate memory for the reacmap and isomap
!     Example: TYPE(em_reacmap),INTENT(OUT) :: reacmap
!  3. Load the reacmap and isomap into memory by giving load_em_reacmap and 
!     load_em_isomap a filename and unit number to use.  
!     Example: CALL load_em_reacmap(22, 'reacmap', reacmap, iostat=status)
!  4. Pass this function the index in reacmap to the reaction desired
! NOTES
!  The order of isotopes and reactions is crucial for interpreting the 
!  fluxes correctly.  Reactions are mapped to an integer, and this integer
!  is saved in the binary files.  The "reacmap" file contains the info
!  needed to relate the mapping integer, hence reaction, to an array of
!  isotope mappings.  The integer that a reaction is mapped to is 
!  simply its index in memory when xnet was running.
!
!  The load_em_reacmap() subroutine greatly simplifies this mapping.  It
!  loads all reaction info into a structure of TYPE(em_reacmap).  Then, the
!  reactant/product particle mappings, biblio code, and iwflx of the nth
!  reaction can be accessed as reacmap%reactants(n,1:3), 
!  reacmap%products(n,1:4), reacmap%biblio(n), and reacmap%iwflx(n),
!  respectively.  To get the z, a, or label of the ith reactant of the nth
!  reaction, call load_em_isomap, and use isomap%z(reacmap%reactants(n,i))
!  The get_index_to_reaction() function returns the mapping integer of a
!  reaction with particular reactant/product mappings.
!
!  The isotope with the most protons is considered the heaviest.  If two
!  isotopes meet this criteria, the one with the most neutrons is used.
! INPUTS
!  reacmap: Variable holding all info from "reacmap" file
!  isomap: Variable holding all info from "isomap" file
!  i: index to a specific reaction in reacmap
! RETURN VALUE
!  CHARACTER(LEN=100) with string for reaction i in reacmap
! AUTHORS
!  Jason Scott (jpscott@mail.phy.ornl.gov)
! MODIFICATION HISTORY
!  09/20/2005 jpscott       Started function
! SOURCE
         
         FUNCTION get_reac_str(reacmap, isomap, i)
           IMPLICIT NONE
           TYPE(em_reacmap),INTENT(IN) :: reacmap
           TYPE(em_isomap),INTENT(IN)  :: isomap
           INTEGER,INTENT(IN)          :: i
           CHARACTER(LEN=100)          :: get_reac_str
           INTEGER                     :: n,j

           get_reac_str = isomap%label(reacmap%reactants(i,1))

! Add reactants
           DO n = 2, UBOUND(reacmap%reactants,2)
              j = reacmap%reactants(i,n)
              IF (j > 0) get_reac_str = TRIM(get_reac_str) // ' + ' // isomap%label(j)
           END DO

           get_reac_str = TRIM(get_reac_str) // ' -->'

! Add products
           DO n = 1, UBOUND(reacmap%products,2)
              j = reacmap%products(i,n)
              IF (j >= 0) THEN
                 IF (n > 1) THEN
                    get_reac_str = TRIM(get_reac_str) // ' + ' // isomap%label(j)
                 ELSE
                    get_reac_str = TRIM(get_reac_str) // ' ' // isomap%label(j)
                 END IF
              END IF
           END DO

           get_reac_str = TRIM(get_reac_str) // ' [' // TRIM(ADJUSTL(reacmap%biblio(i))) // ']'
         END FUNCTION get_reac_str
!***

       END MODULE read_em_sim
