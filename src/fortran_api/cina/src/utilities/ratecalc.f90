!==============================================================================!
!     Thermonuclear Reaction Rate Calculator (rate)                            !
!     By: Kyle Thomsen & Michael Smith                                         !
!     Description: This is the March 2014 version                              !
!     Last modified: 17:00 01 Jul 2011                                         !
!==============================================================================!

!==============================================================================!
!     {TOC}                                                                    !
!     Table of Contents:                                                       !
!                                                                              !
!     To quickly jump to a section, just search for the shortened name in the  !
!     right-hand column.                                                       !
!                                                                              !
!     Section Title                                         | Shortened title  !
!  --------------------------------------------------------------------------  !
!                                                           |                  !
!     Table of Contents                                     |  {TOC}           !
!     Changelog                                             |  {CHLOG}         !
!     Guide to Documentation                                |  {G2DOC}         !
!     The Matrix                                            |  {MATRIX}        !
!     How to run the code                                   |  {RUN}           !
!     Sample Input File                                     |  {SAMPLE}        !
!                                                           |                  !
!  --------------------------------------------------------------------------  !
!  1. Module: constants                                     |  {CONST}         !
!                                                           |                  !
!  2. Module: variables                                     |  {VDEC}          !
!                                                           |                  !
!  3. Program: rate                                         |  {PRGM1}         !
!     > Local Variable Declarations                         |  {VDEC1}         !
!     > Variable Initialization                             |  {VINIT1}        !
!     > Runtime Options                                     |  {OPTIONS}       !
!     > Checking the Reaction for Validity                  |  {RXNCHK}        !
!     > Execute Calculation Subroutines                     |  {CALC}          !
!     > Total Reaction Rate Calculation                     |  {TOTALRATE}     !
!                                                           |                  !
!  4. Subroutine: input                                     |  {SBRT2}         !
!     > Local Variable Declarations                         |  {VDEC2}         !
!     > Open Input Files                                    |  {OPEN}          !
!     > Read Input File                                     |                  !
!           - Reactants                                     |  {READ1}         !
!           - Temperature Grid Options                      |  {READ2}         !
!           - S-Factors                                     |  {READ3}         !
!           - Resonance Data                                |  {READ4}         !
!                                                                              !
!  5. Subroutine: nr_rate                                   |  {SBRT3}         !
!     > Local Variable Declarations                         |  {VDEC3}         !
!     > Nonresonant Rate Calculation                        |  {NRES}          !
!                                                           |                  !
!  6. Subroutine: narrow_res_rate                           |  {SBRT4}         !
!     > Local Variable Declarations                         |  {VDEC4}         !
!     > Reaction Rate Calculation, Narrow Resonances        |  {RES_N}         !
!                                                           |                  !
!  7. Subroutine: broad_res_rate                            |  {SBRT5}         !
!     > Local Variable Declarations                         |  {VDEC5}         !
!     > Reaction Rate Calculation, Broad Resonances         |  {RES_B}         !
!                                                           |                  !
!  8. Subroutine: output                                    |  {SBRT6}         !
!     > Local Variable Declarations                         |  {VDEC6}         !
!     > Write the Results to reactionrates.dat              |  {WRITE}         !
!                                                           |                  !
!  9. Subroutine: strength_calc                             |  {SBRT7}         !
!     > Local Variable Declarations                         |  {VDEC7}         !
!                                                           |                  !
! 10. Subroutine: test                                      |  {SBRT8}         !
!     > Local Variable Declarations                         |  {VDEC8}         !
!     > Input Tests                                         |  {IN_TEST}       !
!     > Temperature Tests                                   |  {TEMP_TEST}     !
!     > Reactants Tests                                     |  {REAC_TEST}     !
!     > Nonresonant Tests                                   |  {NR_TEST}       !
!     > Resonant Tests                                      |  {RES_TEST}      !
!                                                           |                  !
! 11. Subroutine: data_table                                |  {SBRT9}         !
!     > Local Variable Declarations                         |  {VDEC9}         !
!     > Reading of the table from file                      |  {TABLE1}        !
!     > Retrieving the necessary values from the table      |  {TABLE2}        !
!                                                           |                  !
!==============================================================================!

!==============================================================================!
!     {CHLOG}                                                                  !
!     Changelog:                                                               !
!                                                                              !
!     *******************************************************************      !
!     *     Please read the notes at the bottom of this box before      *      !
!     *     adding to the changelog!                                    *      !
!     *******************************************************************      !
!                                                                              !
!     Programmers: Kyle Thomsen (kt)                                           !
!                  Chris Smith  (cs)                                           !
!                                                                              !
!     Version No. | Date            | Summary of changes                       !
!  --------------------------------------------------------------------------  !
!    0.1.01-cs    |                 | > Moved constants and common block       !
!                 |                 |   out of the program and subroutines and !
!                 |                 |   into the constants and variables       !
!                 |                 |   modules                                !
!                 |                 | > changed the mass table to read from a  !
!                 |                 |   file rather than use the large if      !
!                 |                 | > Moved input into a separate subroutine !
!                 |                 | > Added a new temperature method to give !
!                 |                 |   an even distribution in log(T9)        !
!
!
!  --------------------------------------------------------------------------  !
!    0.0.99-kt    | 01 Jul 2011     | > started working on DS using TReRaC     !
!                 |                 |   version 0.0.99 as a starting point     !
!                 |                 | > began detailing differences between    !
!                 |                 |   TReRaC and TReRaC DS in {DIFF} for     !
!                 |                 |   clarity                                !
!                 |                 | > renamed and reorganized common blocks  !
!                 |                 | > fixed previously-incorrect inclusion   !
!                 |                 |   of Kronecker delta for situations in   !
!                 |                 |   which z(1) = z(2) and a(1) = a(2)      !
!                 |                 |   by only applying the correction factor !
!                 |                 |   to the nonresonant and individual      !
!                 |                 |   resonant rates                         !
!                 |                 | > added --verbose-output (-vo) option,   !
!                 |                 |   replacing complete_output.dat          !
!                 |                 | > increased the size of elements in      !
!                 |                 |   argument from 16 to 64 to allow for    !
!                 |                 |   longer file names                      !
!                 |                 | > updated input section of verbose       !
!                 |                 |   output to be consistent with current   !
!                 |                 |   input file                             !
!                 |                 | > tweaked output such that regular,      !
!                 |                 |   verbose, or both types may be used     !
!                                                                              !
!     If you are going to edit this program, please add another section to the !
!     changelog.  Be sure to include your name, the current version number,    !
!     the dates of the last edits for each version number, and a summary of    !
!     the changes made in each version.  If you will keep this changelog       !
!     accurate, it will be of great benefit to future programmers and those    !
!     who will inevitably modify this code for their own calculations.         !
!                                                                              !
!     (Note 1: Please continue your own edits at the top, keeping the oldest   !
!            changes at the bottom of the log.)                                !
!                                                                              !
!     (Note 2: Please keep the format of the version continuous.  It should    !
!           end with the character(s) in parenthesis beside your name in the   !
!           'Programmers' list.  This will identify you as being the one       !
!           responsible for the edits listed.  If you happen to collaborate    !
!           with another programmer, append all the identifying characters for !
!           each programmer to the appropriate version number.)                !
!                                                                              !
!     (Note 3: Version number increments should be decided upon by the         !
!           programmers currently working on this code.  It should be neither  !
!           too rapid nor too sluggish in its ascension towards version 1.0.0  !
!           and above.  Significant progress should be made before proceeding  !
!           to the next version.  The definition of the term 'significant      !
!           progress' is left up to the current programmers.  Also, please     !
!           add the version number to the source code file name.)              !
!                                                                              !
!     Please keep a log of the various versions of this code.  This will make  !
!     it easier for others to view older editions of the code and be able to   !
!     use them as springboards for a variety of other uses (and it also makes  !
!     for a very organized bookkeeping system).  Thank you for your efforts on !
!     the behalf of all those who will benefit from the use of this code.      !
!                                                                              !
!     If you have any questions about this code, or you would like to send     !
!     comments, concerns, or thanks, please contact:                           !
!                                                                              !
!     >> Dr. Michael Smith, ORNL Physics Division                              !
!                                                                              !
!     >> Kyle Thomsen, Tennessee Technological University                      !
!                                                                              !
!==============================================================================!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     {G2DOC}                                                                  !
!    Guide to Documentation:                                                   !
!                                                                              !
!           One of my goals in this program is to make it as readable as       !
!     possible.  To do this, I am going to employ a systematic approach to the !
!     documentation.  As you can see, there are a few different types of       !
!     comment boxes, with the only limiting factor being that there must be    !
!     either an exclamation point or an asterisk in the first column.  Other   !
!     than that, my comments will take whatever form I see fit.  Below is a    !
!     listing of the pattern of each of the boxes and a brief description of   !
!     what you can expect to find in each one.                                 !
!                                                                              !
!     Box Pattern       | Brief Description of Contents     | Name of type     !
!  --------------------------------------------------------------------------  !
!     !!!!!!!!!!!!!     | This type of box contains info    |                  !
!     !           !     | that is absolutely essential to   | Important Box    !
!     !!!!!!!!!!!!!     | anyone working on this code.      |                  !
!  --------------------------------------------------------------------------  !
!     *************     | These are temporary notes for the |                  !
!     *           *     | current programmer.  They should  | Note-to-Self Box !
!     *************     | be deleted upon completion.       |                  !
!  --------------------------------------------------------------------------  !
!     !===========!     | This box contains identifying     |                  !
!     !           !     | information about the program.    | ID Box           !
!     !===========!     |                                   |                  !
!  --------------------------------------------------------------------------  !
!     !-----------!     | These are just typical comments   |                  !
!     !           !     | about parts of the code that      | Comment Box      !
!     !-----------!     | possibly deserve a closer look.   |                  !
!  --------------------------------------------------------------------------  !
!                       | This is a simple divider.  It     |                  !
!     *************     | helps keep the program nice and   | Divider          !
!                       | orderly.                          |                  !
!  --------------------------------------------------------------------------  !
!                       | This is a title for the various   |                  !
!     !!         !!     | sections of the code listed in    | Title            !
!                       | Table of Contents.                |                  !
!                                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     {MATRIX}                                                                 !
!     The Matrix                                                               !
!                                                                              !
!     To further simplify data-handling within the program, it was decided     !
!     that the best way to do this is to combine the arrays previously known   !
!     as t9, non_res, narrow_res, broad_res, and total_rate into a single      !
!     matrix, adding a column for total resonant rate.                         !
!                                                                              !
!     For simplicity, this matrix of data is known as 'matrix.'  Its           !
!     dimensions are max_temps (when temperature parametrization is            !
!     established) rows by max_resonances+4, with the four extra columns being !
!     T9, nonresonant rate, resonant sum, and total rate.  Visually,           !
!                                                                              !
!     T9  |  nonres  |  res 1 - - - res n  |  res total  |  total rate         !
!   ------|----------|---------------------|-------------|--------------       !
!     1   |  ~~~     |  ~~~         ~~~    |  ~~~        |  ~~~                !
!     2   |  ~~~     |  ~~~         ~~~    |  ~~~        |  ~~~                !
!     3   |  ~~~     |  ~~~         ~~~    |  ~~~        |  ~~~                !
!    etc. |  etc.    |  etc.        etc.   |  etc.       |  etc.               !
!                                                                              !
!     In this way, all the rates can be stored in a single array, without      !
!     the need to worry about rates being properly paired with temperatures.   !
!                                                                              !
!       Thus, for reference, the following can be used to convert old array    !
!       values into new ones:                                                  !
!                                                                              !
!       t9(counter) = matrix(counter, 1)                                       !
!       non_res(counter) = matrix(counter, 2)                                  !
!       narrow_res(counter, counter2) = matrix(counter, counter2 + 2)          !
!       broad_res(counter, counter2) = matrix(counter, counter2 + 2)           !
!       total_rate(counter) = matrix(counter, max_resonances + 4)              !
!                                                                              !
!     kt, 16 Jun 2010                                                          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     {RUN}                                                                    !
!     How to Run the Code                                                      !
!                                                                              !
!     compile it with                                                          !
!                                                                              !
!     >gfortran rate.f                                                         !
!                                                                              !
!     if the executable is rate.exe, then run it as follows in a command line  !
!     where ">" is the prompt:                                                 !
!                                                                              !
!     >/.rate.exe --input in_file_name.dat --verbose-output out_file_name.dat  !
!                                                                              !
!        or                                                                    !
!                                                                              !
!     >/.rate.exe -i in_file_name.dat -vo out_file_name.dat                    !
!                                                                              !
!     the format for the input file is described in detail within the file     !
!                                                                              !
!     a sample input file is appended to the end of this document, see {SAMPLE}!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Module constants
      implicit none
!------------------------------------------------------------------------------!
!     {CONST}                                                                  !
!     Constant declarations                                                    !
!------------------------------------------------------------------------------!

      integer*4               max_resonances          ! indicates the maximum
      parameter               (max_resonances = 100)  ! number of resonances
                                                      ! allowed

      integer*4               max_temps               ! the maximum number of
      parameter               (max_temps = 10000)     ! temperatures in the
                                                      ! temperature grid

      integer*4               max_energies            ! the maximum number of
      parameter               (max_energies = 100000) ! energies for the cross
                                                      ! section & s-factor

      real*8                  dummy                   ! the dummy value for
      parameter               (dummy = -1.0d0)        ! resonance data

      real*8                  k                       ! the Boltzmann constant,
      parameter               (k = 8.617343d-02)      ! in (MeV / GK)

      real*8                  pi                      ! the ratio of a circle's
      parameter               (pi = dacos(-1.0d0))    ! circumference to its
                                                      ! diameter

      real*8                  h_bar                   ! the Planck constant
      parameter               (h_bar = 6.582119d-22)  ! divided by 2*pi, in
                                                      ! (MeV * s)

      real*8                  N_a                     ! Avogadro's number
      parameter               (N_a = 6.022142d23)

      real*8                  e                       ! unit of electrical
      parameter               (e = 1.60219d-19)       ! charge, in coulombs

      real*8                  c                       ! speed of light in a
      parameter               (c = 2.997925d10)       ! vacuum, in cm/s

      real*8                  amu                     ! amu * c^2 in MeV
      parameter               (amu = 931.494)

      integer*4               num_energy              ! actual num. of energies 
      parameter               (num_energy = 100000)   ! for numerical integration

      real*8                  dE                  ! grid size for numerical integration
      parameter               (dE = 5.0d-05)      !   over energy in MeV [0.01 keV]     
                             
      real*8                  E_min               ! initial energy for integration 
      parameter               (E_min = 1.0d-04)   ! in MeV [0.1 keV]

end Module

Module variables
      use constants
      implicit none
!------------------------------------------------------------------------------!
!     {VDEC}                                                                  !
!     Variable declarations                                                    !
!------------------------------------------------------------------------------!

!=====/ reaction /==============================================================

      integer*4               z(4)                    ! z-values (atomic number)
                                                      ! for the four particles
                                                      ! of the reaction

      integer*4               a(4)                    ! a-values (atomic mass)
                                                      ! for the four particles
                                                      ! of the reaction

      real*8                  me(4)                   ! the binding energies of
                                                      ! the four particles of
                                                      ! the reaction

      real*8                  j(4)                    ! the momenta of the three
                                                      ! particles of the
                                                      ! reaction

      real*8                  p(4)                    ! parities of the three
                                                      ! particles of the
                                                      ! reaction

      real*8                  mass(4)                 ! mass of each particle
                                                      ! after accounting for
                                                      ! the mass defect

      real*8                  delta                   ! accounts for the
                                                      ! inability to determine
                                                      ! which of two identical
                                                      ! particles is the target
                                                      ! and which is the
                                                      ! projectile (1 if equal,
                                                      ! 0 otherwise)

      real*8                  red_mass                ! reduced mass for the
                                                      ! entrance channels

      real*8                  red_mass2               ! reduced mass for the
                                                      ! exit channels

      integer*4               gamma_flag              ! indicates photons in
                                                      ! the reaction

!=====/ tempgrid /==============================================================

      real*8                  Tmin                    ! minimum temperature

      real*8                  Tmax                    ! maximum temperature

      real*8                  Tstep                   ! temperature increment

      integer*4               number_temps            ! the total number of
                                                      ! temperatures in the
                                                      ! temperature grid

      integer*1               temp_flag               ! flag indicating which
                                                      ! style of temperature
                                                      ! grid is to be used

!=====/ nonres_param /==========================================================

      integer*1               nr_flag                 ! tells whether or not to
                                                      ! evaluate the nonresonant
                                                      ! rate

      real*8                  s                       ! S(0), the s-factor
                                                      ! at zero energy

      real*8                  ds                      ! S'(0), the first
                                                      ! derivative of the
                                                      ! s-factor at zero energy

      real*8                  dds                     ! S''(0), the second
                                                      ! derivative of the
                                                      ! s-factor at zero energy

      integer*8               cutoff_flag             ! tells whether or not a
                                                      ! a nonresonant cutoff
                                                      ! term is to be included

      real*8                  t9_cutoff               ! the parameter of the
                                                      ! cutoff term which
                                                      ! determines the range
                                                      ! over which the cutoff is
                                                      ! effective

!=====/ res_param /=============================================================

      integer*1               res_flag(max_resonances)! tells whether or not to
                                                      ! evaluate the rate for
                                                      ! each resonance

      integer*1               broad_flag(max_resonances)
                                                      ! allows for user-
                                                      ! reporting of broad
                                                      ! resonances

      real*8                  broad_cutoff            ! minimum ratio of total
                                                      ! width to resonance
                                                      ! energy for a resonance
                                                      ! to be considered broad

      integer*4               status(max_resonances)  ! tells the status of each
                                                      ! resonance (0 if narrow,
                                                      ! 1 if broad)

      real*8                  energy(max_resonances)  ! energies of resonances

      real*8                  strength(max_resonances)! strengths of resonances

      real*8                  omega(max_resonances)   ! the omega statistical
                                                      ! factor

      real*8                  gamma(max_resonances)   ! the width ratios of the
                                                      ! resonances

      real*8                  width_A(max_resonances) ! the partial width of
                                                      ! the entrance channel for
                                                      ! each resonance

      real*8                  width_B(max_resonances) ! the partial width of
                                                      ! the exit channel for
                                                      ! each resonance

      real*8                  width_C(max_resonances) ! the partial width of
                                                      ! any other channels for
                                                      ! each resonance

      real*8                  width_tot(max_resonances)     ! total widths of
                                                            ! resonances

      real*8                  j_res(max_resonances)   ! the relative angular
                                                      ! momenta of the
                                                      ! resonances

      integer*4               total_resonances        ! indicates the total
                                                      ! number of resonances

!====/ matrix /=================================================================

      real*8                  matrix(max_temps, max_resonances+4)
                                                      ! matrix containing
                                                      ! temperatures and rates

!====/ current /================================================================

      real*8                  current_energy          ! energies of current
                                                      ! resonance

      real*8                  current_strength        ! strength of current
                                                      ! resonance

      real*8                  current_omega           ! the omega statistical
                                                      ! factor for current
                                                      ! resonance

      real*8                  current_gamma           ! the width ratio of the
                                                      ! current resonance

      real*8                  current_width_A         ! the partial width of
                                                      ! the entrance channel for
                                                      ! current resonance

      real*8                  current_width_B         ! the partial width of
                                                      ! the exit channel for
                                                      ! current resonance

      real*8                  current_width_C         ! the partial width of
                                                      ! any other channels for
                                                      ! current resonance

      real*8                  current_width_tot       ! total width of
                                                      ! current resonance

      real*8                  current_j_res           ! the relative angular
                                                      ! momentum of current
                                                      ! resonance

      integer*4               res_number              ! the resonance currently
                                                      ! being evaluated

!=====/ location /==============================================================

      logical                 loc_flag                ! flag for indicating
                                                      ! input location's
                                                      ! existence

      character*64            loc_in                  ! the location of the
                                                      ! input file

      character*64            loc_out                 ! the location of the
                                                      ! output file

      character*64            loc_out2                ! the location of the
                                                      ! verbose output file

      character*72            loc_temp                ! location of input file
                                                      ! for temperatures

      logical                 err_flag                ! tells of errors in input
                                                      ! file read

      integer*4               err_line                ! gives line of read error

!=====/ misc /==================================================================

      character*64            argument(10)            ! contains the arguments
                                                      ! obtained by getarg

      integer*4               test_count              ! indicates which set of
                                                      ! tests should be executed

      real*8                  eqv_threshold           ! the maximum difference
                                                      ! between two numbers
                                                      ! whereby they are
                                                      ! considered equal

      integer*1               online_flag             ! tells whether or not the
                                                      ! program is being run
                                                      ! online

      character*72            title                   ! the title of the run

      character*72            summary1                ! line one of the summary
                                                      ! of the run

      character*72            summary2                ! line two of the summary
                                                      ! of the run

end Module
 
 
!!    {PRGM1}                                                                !! 
 
      program rate 
 
      use constants 
      use variables 
 
      implicit none 
 
 
!=====( Local variables )======================================================= 
 
      integer*4               counter                 ! counter variable 
      integer*4               counter2                ! counter variable 
 
      character*1             dummy_string            ! allows for skipping of 
                                                      ! lines in input file 
 
!=============================================================================== 
 
!******************************************************************************* 
 
!------------------------------------------------------------------------------! 
!     {VINIT1}                                                                 ! 
!     Variable initializations                                                 ! 
!     Here, variables are initialized and sorted by common block, if           ! 
!     applicable.  Also, on the right is an indicator of whether the           ! 
!     variable in question obtains its eventual value(s) from the input        ! 
!     file [input], a calculation from which it is derived [derived],          ! 
!     either input or derivation [either], both input and derived values       ! 
!     [both], from the table in the subroutine data_table [table], or          ! 
!     from TReRaC's runtime arguments [arg].                                   ! 
!------------------------------------------------------------------------------! 
 
!-----VARIABLES---------------------------------------------!-----SOURCE-------! 
                                                            !                  ! 
!     / reaction / initializations                          !                  ! 
      do 2, counter = 1, 4                                  !                  ! 
            z(counter) = 0.0d0                              !     [input]      ! 
            a(counter) = 0.0d0                              !     [input]      ! 
            me(counter) = 0.0d0                             !     [table]      ! 
            j(counter) = 0.0d0                              !     [table]      ! 
            p(counter) = 0.0d0                              !     [table]      ! 
            mass(counter) = 0.0d0                           !     [derived]    ! 
2     continue                                              !                  ! 
      delta = 0.0d0                                         !     [derived]    ! 
      red_mass = 0.0d0                                      !     [derived]    ! 
      red_mass2 = 0.0d0                                     !     [derived]    ! 
      gamma_flag = 0                                        !     [derived]    ! 
                                                            !                  ! 
!     / temp_grid / initializations                         !                  ! 
      Tmin = 0.0d0                                          !     [input]      ! 
      Tmax = 0.0d0                                          !     [input]      ! 
      Tstep = 0.0d0                                         !     [input]      ! 
      number_temps = 0                                      !     [derived]    ! 
      temp_flag = 0                                         !     [input]      ! 
                                                            !                  ! 
!     / nonres_param / initializations                      !                  ! 
      nr_flag = 0                                           !     [input]      ! 
      s = 0.0d0                                             !     [input]      ! 
      ds = 0.0d0                                            !     [input]      ! 
      dds = 0.0d0                                           !     [input]      ! 
      cutoff_flag = 0                                       !     [input]      ! 
      t9_cutoff = 0                                         !     [input]      ! 
                                                            !                  ! 
!     / res_param / initializations                         !                  ! 
      do 3, counter = 1, max_resonances                     !                  ! 
            res_flag(counter) = 0                           !     [input]      ! 
            broad_flag(counter) = 0                         !     [input]      ! 
            status(counter) = 0                             !     [derived]    ! 
            energy(counter) = 0.0d0                         !     [input]      ! 
            strength(counter) = 0.0d0                       !     [either]     ! 
            omega(counter) = 0.0d0                          !     [either]     ! 
            gamma(counter) = 0.0d0                          !     [either]     ! 
            width_A(counter) = 0.0d0                        !     [either]*    ! 
            width_B(counter) = 0.0d0                        !     [either]*    ! 
            width_C(counter) = 0.0d0                        !     [input]*     ! 
            width_tot(counter) = 0.0d0                      !     [either]*    ! 
            j_res(counter) = 0.0d0                          !     [input]      ! 
3     continue                                              !                  ! 
            broad_cutoff = 0.0d0                            !     [input]      ! 
            total_resonances = 0                            !     [derived]    ! 
                                                            !                  ! 
!     / matrix / initializations                            !                  ! 
      do 5, counter = 1, max_temps                          !                  ! 
            do 4, counter2 = 1, max_resonances              !                  ! 
                  matrix(counter, counter2) = 0.0d0         !     [both]       ! 
4           continue                                        !                  ! 
5     continue                                              !                  ! 
                                                            !                  ! 
!     / current / initializations                           !                  ! 
      current_energy = 0.0d0                                !     [input]      ! 
      current_strength = 0.0d0                              !     [either]     ! 
      current_omega = 0.0d0                                 !     [either]     ! 
      current_gamma = 0.0d0                                 !     [either]     ! 
      current_width_A = 0.0d0                               !     [either]*    ! 
      current_width_B = 0.0d0                               !     [either]*    ! 
      current_width_C = 0.0d0                               !     [input]*     ! 
      current_width_tot = 0.0d0                             !     [either]*    ! 
      current_j_res = 0.0d0                                 !     [input]      ! 
      res_number = 0                                        !     [derived]    ! 
                                                            !                  ! 
!     / location / initializations                          !                  ! 
      loc_flag = .false.                                    !     [derived]    ! 
      loc_in = ''                                           !     [arg]        ! 
      loc_out = ''                                          !     [arg]        ! 
      loc_out2 = ''                                         !     [arg]        ! 
      loc_temp = ''                                         !     [input]      ! 
      err_flag = .false.                                    !     [derived]    ! 
      err_line = 0                                          !     [derived]    ! 
                                                            !                  ! 
!     / misc / initializations                              !                  ! 
      do 6 counter = 1, 10                                  !                  ! 
            argument(counter) = ''                          !     [arg]        ! 
6     continue                                              !                  ! 
      test_count = 0                                        !     [derived]    ! 
      online_flag = 0                                       !     [arg]        ! 
      title = ''                                            !     [    ]       ! 
      summary1 = ''                                         !     [    ]       ! 
      summary2 = ''                                         !     [    ]       ! 
      eqv_threshold = 0.0d0                                 !     [input]      ! 
 
!******************************************************************************* 
 
!------------------------------------------------------------------------------! 
!     {OPTIONS}                                                                ! 
!     Runtime Options                                                          ! 
!------------------------------------------------------------------------------! 
 
!!    Below, the subroutine getarg is a native function of FORTRAN.  It takes 
!!    as input that which follows the program's execution.  For example, if 
!!    TReRaC were being run in a bash terminal, the following would be the 
!!    input for getarg: 
!! 
!!    >> compiling using gfortran and outputting TReRaC.out 
!!    user@desktop:~$ gfortran TReRaC.f -o TReRaC.out 
!! 
!!    >> executing TReRaC.out, but adding input for getarg 
!!    user@desktop:~$ ./TReRaC.out getarg_input 
!! 
!!    Here, the string 'getarg_input' can be used within TReRaC itself.  For 
!!    example, if the line 
!! 
!!          call getarg(1,string) 
!! 
!!    were included in TReRaC (provided that string is a variable of the 
!!    appropriate type and size), then the variable string will be given the 
!!    value 'getarg_input' from the program's execution. 
!! 
!!    In a similar fashion, getarg(2,variable), getarg(3,variable), etc. can 
!!    be used to obtain further command-line input. 
 
!!------------------------------------------------------------------------------ 
!!    BELOW IS THE OLD GETARG SECTION 
!!------------------------------------------------------------------------------ 
!       call getarg(1,identifier) 
!  
!       if(identifier .NE. '') then 
!             online_flag = 1 
!       do counter = 1, 24 
!             if(identifier(counter:counter) .NE. ' ') then 
!                   if(counter .EQ. 1) then 
!                   id = identifier(counter:counter) 
!                   else 
!                   id = id(:counter-1) // identifier(counter:counter) 
!                   end if 
!             else 
!                   continue 
!             end if 
! !       print *, id 
!       end do 
!       else 
!             online_flag = 0 
!       end if 
!  
!       if(online_flag .EQ. 1) then 
!             print *, "online" 
!       else 
!             print *, "offline" 
!       end if 
!!------------------------------------------------------------------------------ 
 
!!------------------------------------------------------------------------------ 
!!    BELOW IS THE NEW GETARG SECTION 
!!------------------------------------------------------------------------------ 
 
      do counter = 1, 10      ! begin loop over arguments 
            call getarg(counter, argument(counter)) 
      end do                  ! end loop over arguments 
 
      do counter = 1, 10      ! begin loop over arguments 
 
 
            if ((argument(counter) .EQ. '-h') .OR.        &  ! begin test for 
                (argument(counter) .EQ. '--help') .OR.    &  ! help option 
                (argument(counter) .EQ. '-?') .OR.        & 
                (argument(1) .EQ. '')) then 
                  print *, "" 
                  print *, "---------------------------------------",& 
                           "---------------\\" 
                  print *, "Thermonuclear Reaction Rate Calculator ",& 
                           "(TReRaC) Help  |" 
                  print *, "---------------------------------------",& 
                           "---------------/" 
                  print *, "" 
                  print *, "TReRaC supports the ability to modify ", & 
                           "the way in which the" 
                  print *, "program runs through the use of ",       & 
                           "runtime options which are listed " 
                  print *, "after the executable.  Listed below ",   & 
                           "are the available options " 
                  print *, "and their associated syntaxes.  Note ",  & 
                           "multiple options may be used" 
                  print *, "simultaneously, provided they are ",     & 
                           "separated by spaces." 
                  print *, "" 
                  print *, "" 
                  print *, "---------------------------\\" 
                  print *, "       Help                |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------\\" 
                  print *, "Options: -h, --help, -?                ",& 
                           "                                      |" 
                  print *, "Displays this help text.               ",& 
                           "                                      |" 
                  print *, "This is the default output for TReRaC w",& 
                           "hen no options are specified.         |" 
                  print *, "Use: ./trerac.out -h                   ",& 
                           "                                      |" 
                  print *, "     ./trerac.out --help               ",& 
                           "                                      |" 
                  print *, "     ./trerac.out -?                   ",& 
                           "                                      |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------/" 
                  print *, "" 
                  print *, "---------------------------\\" 
                  print *, "       Input               |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------\\" 
                  print *, "Options: -i, --input                   ",& 
                           "                                      |" 
                  print *, "Indicates what input file to use.  Requ",& 
                           "ired for normal operation.            |" 
                  print *, "Use: ./trerac.out -i <input file name> ",& 
                           "                                      |" 
                  print *, "     ./trerac.out --input <input file n",& 
                           "ame>                                  |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------/" 
                  print *, "" 
                  print *, "---------------------------\\" 
                  print *, "       Output              |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------\\" 
                  print *, "Options: -o, --output                  ",& 
                           "                                      |" 
                  print *, "Indicates what output file to write.  R",& 
                           "equired for normal operation.         |" 
                  print *, "Use: ./trerac.out -o <output file name>",& 
                           "                                      |" 
                  print *, "     ./trerac.out --output <output file",& 
                           " name>                                |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------/" 
                  print *, "" 
                  print *, "---------------------------\\" 
                  print *, "       Verbose Output      |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------\\" 
                  print *, "Options: -vo, --verbose-output         ",& 
                           "                                      |" 
                  print *, "Indicates that TReRaC should output an ",& 
                           "additional file which includes a custom" 
                  print *, " run title and summary, a copy of the i",& 
                           "nput file contents, and a labelled    |" 
                  print *, "table of temperatures and rates.       ",& 
                           "                                      |" 
                  print *, "Use: ./trerac.out -vo <verbose output f",& 
                           "ile name>                             |" 
                  print *, "     ./trerac.out --verbose-output <ver",& 
                           "bose output file name>                |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------/" 
                  print *, "" 
                  print *, "---------------------------\\" 
                  print *, "       Network             |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------\\" 
                  print *, "Options: -n, --network                 ",& 
                           "                                      |" 
                  print *, "Sets TReRaC to function in an online mo",& 
                           "de, which is intended for use within  |" 
                  print *, "the Computational Infrastructure for Nu",& 
                           "clear Astrophysics (CINA) at Oak      |" 
                  print *, "Ridge National Laboratory.             ",& 
                           "                                      |" 
                  print *, "Use: ./trerac.out -n                   ",& 
                           "                                      |" 
                  print *, "     ./trerac.out --network            ",& 
                           "                                      |" 
                  print *, "---------------------------------------",& 
                           "--------------------------------------/" 
                  print *, "" 
                  print *, "" 
                  stop 
            end if                                          ! end test for 
                                                            ! help option 
 
 
            if ((argument(counter) .EQ. '-i') .OR.         &! begin test for 
                (argument(counter) .EQ. '--input')) then    ! input option 
                  loc_in = argument(counter + 1) 
                        if (loc_in .EQ. '') then      ! check for omitted input 
                              print *, "Did you forget to include ", & 
                                       "an input file?" 
                              print *, "The -i or --input option ",  & 
                                       "must be followed by a file ",& 
                                       "name." 
                              stop 
                        end if                        ! end check for omitted input 
                  print *, "Selected input file: ", loc_in 
            end if                                          ! end test for 
                                                            ! input option 
 
 
            if ((argument(counter) .EQ. '-o') .OR.        &  ! begin test for 
                (argument(counter) .EQ. '--output')) then   ! output option 
                  loc_out = argument(counter + 1) 
                        if (loc_out .EQ. '') then      ! check for omitted output 
                              print *, "Did you forget to include ",  & 
                                       "an output file?" 
                              print *, "The -o or --output option ",  & 
                                       "must be followed by a file ", & 
                                       "name." 
                              stop 
                        end if                        ! end check for omitted output 
                  print *, "Selected output file: ", loc_out 
            end if                                          ! end test for 
                                                            ! output option 
 
 
                                                            ! begin test for 
            if ((argument(counter) .EQ. '-vo') .OR.&        ! verbose output 
                (argument(counter) .EQ. '--verbose-output')) then   ! option 
                  loc_out2 = argument(counter + 1) 
                        if (loc_out2 .EQ. '') then      ! check for omitted verbose output 
                              print *, "Did you forget to include ",   & 
                                       "a verbose output file?" 
                              print *, "The -vo or --verbose-output ", & 
                                       "option must be followed by ",  & 
                                       "a file name." 
                              stop 
                        end if                        ! end check for omitted verbose output 
                  print *, "Selected verbose output file: ", loc_out2 
            end if                                          ! end test for 
                                                            ! verbose output 
                                                                    ! option 
 
 
            if ((argument(counter) .EQ. '-n') .OR.    &     ! begin test for 
                (argument(counter) .EQ. '--network')) then  ! network option 
                  print *, "Online flag set to true." 
                  online_flag = 1 
            end if                                          ! end test for 
                                                            ! network option 
 
 
            if ((argument(counter) .NE. '') .AND.          &! begin test for 
                (argument(counter) .NE. '-h') .AND.        &! incorrect options 
                (argument(counter) .NE. '--help') .AND.    & 
                (argument(counter) .NE. '-?') .AND.        & 
                (argument(counter) .NE. '-i') .AND.        & 
                (argument(counter) .NE. '--input') .AND.   & 
                (argument(counter) .NE. '-o') .AND.        & 
                (argument(counter) .NE. '--output') .AND.  & 
                (argument(counter) .NE. '-vo') .AND.       & 
                (argument(counter) .NE. '--verbose-output') .AND. & 
                (argument(counter) .NE. '-n') .AND.        & 
                (argument(counter) .NE. '--network') .AND. & 
                ! the statements below make sure that if an option is supposed 
                ! to be followed by a file name, the file name isn't 
                ! accidentally marked as an invalid option 
                (argument(counter - 1) .NE. '-i') .AND.    & 
                (argument(counter - 1) .NE. '--input') .AND.      & 
                (argument(counter - 1) .NE. '-o') .AND.    & 
                (argument(counter - 1) .NE. '--output') .AND.     & 
                (argument(counter - 1) .NE. '-vo') .AND.   & 
                (argument(counter - 1) .NE. '--verbose-output')) then 
                  print *, "" 
                  print *, "" 
                  print *, "You seem to have entered an ",        & 
                           "invlaid option." 
                  print *, "Please check that everything is ",    & 
                           "in order and try again." 
                  print *, "" 
                  print *, "" 
                  print *, "If you require assistance, try viewing ",& 
                           "the help contents using the -h option." 
                  print *, "" 
                  print *, "" 
                  stop 
            end if                                          ! end test for 
                                                            ! incorrect options 
 
 
      end do                  ! end loop over arguments 
 
!------------------------------------------------------------------------------! 
!     Below is a test to ensure that there is an input file and at least one   ! 
!     output file.  It goes through and checks that of all the arguments,      ! 
!     one indicates an input and at least one indicates an output.             ! 
!------------------------------------------------------------------------------! 
 
            if ((argument(1) .NE. '-i') .AND.&   ! begin test for input file 
                (argument(2) .NE. '-i') .AND.& 
                (argument(3) .NE. '-i') .AND.& 
                (argument(4) .NE. '-i') .AND.& 
                (argument(5) .NE. '-i') .AND.& 
                (argument(6) .NE. '-i') .AND.& 
                (argument(7) .NE. '-i') .AND.& 
                (argument(8) .NE. '-i') .AND.& 
                (argument(9) .NE. '-i') .AND.& 
                (argument(10) .NE. '-i') .AND.& 
                (argument(1) .NE. '--input') .AND.& 
                (argument(2) .NE. '--input') .AND.& 
                (argument(3) .NE. '--input') .AND.& 
                (argument(4) .NE. '--input') .AND.& 
                (argument(5) .NE. '--input') .AND.& 
                (argument(6) .NE. '--input') .AND.& 
                (argument(7) .NE. '--input') .AND.& 
                (argument(8) .NE. '--input') .AND.& 
                (argument(9) .NE. '--input') .AND.& 
                (argument(10) .NE. '--input')) then 
                  print *, "You must specify an input file." 
                  stop 
            end if                              ! end test for output file 
 
            if ((argument(1) .NE. '-o') .AND.&   ! begin test for output files 
                (argument(2) .NE. '-o') .AND.& 
                (argument(3) .NE. '-o') .AND.& 
                (argument(4) .NE. '-o') .AND.& 
                (argument(5) .NE. '-o') .AND.& 
                (argument(6) .NE. '-o') .AND.& 
                (argument(7) .NE. '-o') .AND.& 
                (argument(8) .NE. '-o') .AND.& 
                (argument(9) .NE. '-o') .AND.& 
                (argument(10) .NE. '-o') .AND.& 
                (argument(1) .NE. '--output') .AND.& 
                (argument(2) .NE. '--output') .AND.& 
                (argument(3) .NE. '--output') .AND.& 
                (argument(4) .NE. '--output') .AND.& 
                (argument(5) .NE. '--output') .AND.& 
                (argument(6) .NE. '--output') .AND.& 
                (argument(7) .NE. '--output') .AND.& 
                (argument(8) .NE. '--output') .AND.& 
                (argument(9) .NE. '--output') .AND.& 
                (argument(10) .NE. '--output') .AND.& 
                (argument(1) .NE. '-vo') .AND.& 
                (argument(2) .NE. '-vo') .AND.& 
                (argument(3) .NE. '-vo') .AND.& 
                (argument(4) .NE. '-vo') .AND.& 
                (argument(5) .NE. '-vo') .AND.& 
                (argument(6) .NE. '-vo') .AND.& 
                (argument(7) .NE. '-vo') .AND.& 
                (argument(8) .NE. '-vo') .AND.& 
                (argument(9) .NE. '-vo') .AND.& 
                (argument(10) .NE. '-vo') .AND.& 
                (argument(1) .NE. '--verbose-output') .AND.& 
                (argument(2) .NE. '--verbose-output') .AND.& 
                (argument(3) .NE. '--verbose-output') .AND.& 
                (argument(4) .NE. '--verbose-output') .AND.& 
                (argument(5) .NE. '--verbose-output') .AND.& 
                (argument(6) .NE. '--verbose-output') .AND.& 
                (argument(7) .NE. '--verbose-output') .AND.& 
                (argument(8) .NE. '--verbose-output') .AND.& 
                (argument(9) .NE. '--verbose-output') .AND.& 
                (argument(10) .NE. '--verbose-output')) then 
                  print *, "You must specify an output file." 
                  stop 
            end if                              ! end test for output files 
 
!!------------------------------------------------------------------------------ 
 
      call input 
 
!------------------------------------------------------------------------------! 
!     {RXNCHK}                                                                 ! 
!     Check for the validity of the reaction.                                  ! 
!------------------------------------------------------------------------------! 
 
!     testing for typos in the reactants 
      test_count = 3 
      call test 
 
!------------------------------------------------------------------------------! 
!     {CALC}                                                                   ! 
!     Calling subroutines to do the mathematics needed to obtain rates.        ! 
!------------------------------------------------------------------------------! 
 
!     testing for problems with the nonresonant rate parameters 
      test_count = 4 
      call test 
      call nr_rate 
 
!     testing for problems with the resonant rate parameters 
      test_count = 5 
      call test 
 
      call strength_calc 
 
      do 199 counter = 1, total_resonances 
 
      res_number = counter 
 
      current_energy = energy(counter) 
      current_strength = strength(counter) 
      current_gamma = gamma(counter) 
      current_omega = omega(counter) 
      current_j_res = j_res(counter) 
      current_width_A = width_A(counter) 
      current_width_B = width_B(counter) 
      current_width_C = width_C(counter) 
      current_width_tot = width_tot(counter) 
 
      if ( res_flag(res_number) .EQ. 1 ) then 
 
            if ( status(counter) .EQ. 0 ) then 
                  print*, res_number, "narrow" 
                  call narrow_res_rate 
 
            else if ( status(counter) .EQ. 1 ) then 
                  print*, res_number, "broad" 
                  call broad_res_rate 
 
            end if 
 
      else 
 
      print*,  res_number, "uncalculated" 
 
      end if 
 
199   continue 
 
!------------------------------------------------------------------------------! 
!     {TOTALRATE}                                                              ! 
!     Total reaction rates calculated                                          ! 
!------------------------------------------------------------------------------! 
 
      do 201, counter = 1, number_temps 
            matrix(counter, max_resonances + 4) =                 & 
                   matrix(counter, 2) 
 
            do 200, counter2 = 1, total_resonances 
 
                  if ( res_flag(counter2) .EQ. 1 ) then 
 
                        matrix(counter, max_resonances + 4) =     & 
                        matrix(counter, max_resonances + 4) +     & 
                        matrix(counter, counter2 + 2) 
 
                        matrix(counter, max_resonances + 3) =     & 
                        matrix(counter, max_resonances + 3) +     & 
                        matrix(counter, counter2 + 2) 
 
                  else 
                        continue 
                  end if 
 
200         continue 
 
!                         matrix(counter, max_resonances + 4) = 
!      &                  matrix(counter, max_resonances + 4) / 
!      &                            ( 1.0d0 + delta ) 
 
201   continue 
 
      call output 
 
!******************************************************************************* 
 
!       open(11, file="matrix.dat", status="unknown") 
!  
!       do counter=1, number_temps 
!             do counter2=1, max_resonances+4 
!                   if ( counter2 .NE. max_resonances+4 ) then 
!                   write(11,222) matrix(counter,counter2), "||" 
! 222               format(2x, 1PE12.6, 2x, a2$) 
!                   else 
!                   write(11,223) matrix(counter,counter2) 
! 223               format(2x, 1PE12.6) 
!                   end if 
!             end do 
!       end do 
 
999   continue 
 
      stop 
 
      end program 

!!      {SBRT2}                                                               !!

      subroutine input
        use constants
        use variables
        
        implicit none

!!      {VDEC2}                                                               !!

!=====( Local variables )=======================================================

      integer*4               counter                 ! counter variable

      character*1             dummy_string            ! allows for skipping of
                                                      ! lines in input file

!===============================================================================
        
!*******************************************************************************

!------------------------------------------------------------------------------!
!     {OPEN}                                                                   !
!     >> Checking to make sure input file exists to avoid unexpected crashes   !
!     >> Opening input file                                                    !
!------------------------------------------------------------------------------!

!       if(online_flag .EQ. 0) then
!             loc_in = 'input.dat'
!             loc_out = 'output.dat'
!       else
!             loc_in = id(:index(id, ' ')-1) // '_in.dat'
!             loc_out = id(:index(id, ' ')-1) // '_out.dat'
!       end if

!     Checks for the necessary input files
      test_count = 1
      call test

      open(1, file = loc_in, status = 'old')

!*******************************************************************************

!------------------------------------------------------------------------------!
!     {READ1}                                                                  !
!     Reading the reactants into the corresponding arrays                      !
!------------------------------------------------------------------------------!

      read(1, 18) dummy_string
18    format(a1)

      do 30 counter = 1, 4
            read(1, 19) dummy_string
19          format(a1)
            read(1, 20) z(counter), a(counter)
20          format( 2(1x, i3))
30    continue
      read (1,18) dummy_string

!!    Below, the delta correction factor for identical particles is set.      !!

      if ( ( z(1) .EQ. z(2) ) .AND.                  &
           ( a(1) .EQ. a(2) ) ) then
            delta = 1.0d0
      else
            delta = 0.0d0
      end if

!!    gamma_flag is used to avoid issues with zero-mass particles later on,   !!
!!    particularly in calculating an exit reduced mass                        !!

      if (((a(3) .EQ. 0) .AND.                       &
           (z(3) .EQ. 0)) .OR.                       &
          ((a(4) .EQ. 0) .AND.                       &
           (z(4) .EQ. 0))) then
            gamma_flag = 1
      else
            gamma_flag = 0
      end if

      call data_table

      red_mass = (mass(1) * mass(2)) /               &
                 (mass(1) + mass(2))


!*******************************************************************************

!------------------------------------------------------------------------------!
!     {READ2}                                                                  !
!     Reading the temperature parameters                                       !
!------------------------------------------------------------------------------!

      do counter = 1, 7
      read(1, 31) dummy_string
31    format(a1)
      end do

      read(1,32) temp_flag
32    format(1x, i1)

!     Test the temperature grid for validity
      test_count = 2

      if (temp_flag .EQ. 1) then
!!    Logarithmic scale 1 - .01,.02,...,.09,.1,.2,...,.9,1,2,...,9,10

      do counter = 1, 9
      read(1, 31) dummy_string
      end do

            number_temps = 28
            do counter = 1, number_temps
                  if (counter .LE. 10) then
                        matrix(counter,1) = .01d0 * counter
                  else if (counter .LE. 19) then
                        matrix(counter,1) = .1d0 * (counter - 9)
                  else if (counter .LE. 28) then
                        matrix(counter,1) = 1d0 * (counter - 18)
                  end if
            end do

      call test

      else if (temp_flag .EQ. 2) then
!!    Logarithmic scale 2 - .01,.011,...,.099,.1,.11,...,.99,1,1.1,...,9.9,10

      do counter = 1, 9
      read(1, 31) dummy_string
      end do

            number_temps = 271
            do counter = 1, number_temps
                  if (counter .LE. 90) then
                        matrix(counter,1) = .01d0 +             &
                        .001d0 * (counter - 1)
                  else if (counter .LE. 180) then               
                        matrix(counter,1) = .1d0 +              &
                        .01d0 * (counter - 91)
                  else if (counter .LE. 271) then
                        matrix(counter,1) = 1d0 +               &
                        .1d0 * (counter - 181)
                  end if
            end do

      call test

      else if (temp_flag .EQ. 3) then
!!    Linear scale with user-defined parameters

      do counter = 1, 3
      read(1, 31) dummy_string
      end do
      read (1,33) Tmin, Tmax, Tstep
33    format(3(1x, 1pe12.6))
      read (1,18) dummy_string
      do counter = 1, 4
      read(1, 31) dummy_string
      end do

            number_temps = (Tmax - Tmin) / Tstep + 1d0
            if (number_temps .GT. max_temps) then
                  print *, "The entered temperature grid parameters"
                  print *, "result in a grid too large for TReRaC to"
                  print *, "manipulate.  Please change the parameters"
                  print *, "and try again."
                  stop
            end if
            do counter = 1, number_temps
                  matrix(counter,1) = Tmin + Tstep * (counter - 1d0)
            end do

      call test

      else if (temp_flag .EQ. 4) then
!!    Custom temperature grid read from input file

      do counter = 1, 7
      read(1, 31) dummy_string
      end do
      read(1,34) loc_temp
34    format(1x, a72)
      read(1, 31) dummy_string

            open(2, file=loc_temp, status="unknown")
            read(2,31) dummy_string
            read(2,31) dummy_string
            read(2,31) dummy_string
            read(2,31) dummy_string
            do counter = 1, max_temps + 1
                  read(2,35,err=36,end=37) matrix(counter,1)
                  number_temps = counter
35                format (1pe12.6)
                  if (counter .EQ. max_temps + 1) then
                        print *, "WARNING:"
                        print *, "TReRaC will only use the first 10000"
                        print *, "temperatures from the temperature"
                        print *, "input file."
                        number_temps = number_temps - 1
                        goto 37
                  end if
            end do

36          err_flag = .true.
            err_line = counter + 4

37          continue

      else if (temp_flag .EQ. 5) then
!!    New temperature grid that is evenly spaced in log(T9)
!!    log(T9)=-2,-1.99,...,-1.01,-1.0,-.99,...,-0.01,0.0,0.01,...,0.99,1.0
         do counter = 1, 9
            read(1, 31) dummy_string
         end do
         number_temps=300
         Tstep = 3.0/number_temps
         do counter = 1, number_temps+1
            matrix(counter,1) = 10**(-2+Tstep*(counter-1))
!            print *,counter, matrix(counter,1)
         end do


      call test

      else

      call test

      end if


!*******************************************************************************

!------------------------------------------------------------------------------!
!     {READ3}                                                                  !
!     Reading the nonresonant flag, s-factors, and cutoff terms into the       !
!     corresponding variables.                                                 !
!                                                                              !
!     UNITS: The units associated with these values are as follows:            !
!            s - MeV / barns                                                   !
!            ds - barns                                                        !
!            dds - barns / MeV                                                 !
!            cutoff_flag - unitless                                            !
!            t9_cutoff - GK
!------------------------------------------------------------------------------!

      read (1,18) dummy_string
      read(1,49) nr_flag
49    format(1x,i1)
      read (1,18) dummy_string
      read (1,18) dummy_string
      read (1,18) dummy_string

      read (1, 50) s, ds, dds
50    format (3(1x, 1pe13.6))
      read (1,18) dummy_string

      read (1,18) dummy_string
      read (1,18) dummy_string
      read (1,18) dummy_string
      read (1,18) dummy_string
      read (1,51) cutoff_flag, t9_cutoff
51    format(1x, i1, 1x, 1pe12.6)
      read (1,18) dummy_string

!------------------------------------------------------------------------------!
!     {READ4}                                                                  !
!     Reading the broad resonance cutoff, eqv_threshold, resonance flags, and  !
!     resonance data into the corresponding arrays                             !
!                                                                              !
!     UNITS: The units associated with the other information contained in      !
!            input file are as follows:                                        !
!            energy--------MeV                                                 !
!            strength------MeV                                                 !
!            omega---------[unitless]                                          !
!            gamma---------MeV                                                 !
!            width_A-------MeV                                                 !
!            width_B-------MeV                                                 !
!            width_tot-----MeV                                                 !
!            j_res---------[unitless]                                          !
!------------------------------------------------------------------------------!

      read(1, 56) dummy_string
56    format(a1)
      read(1, 57) broad_cutoff
57    format(1x, 1pd12.6)
      read (1,18) dummy_string

      read(1, 56) dummy_string
      read(1, 57) eqv_threshold

      do 59, counter = 1, 17
            read(1, 58) dummy_string
58          format(a1)
59    continue

      do 70 counter = 1, max_resonances
            read (1, 60, end = 80) res_flag(counter),                 &
                               broad_flag(counter),                   &
                               energy(counter), strength(counter),    &
                               omega(counter), gamma(counter),        &
                               width_A(counter), width_B(counter),    &
                               width_C(counter), width_tot(counter),  &
                               j_res(counter)
60          format (1x, i1, 3x, i1, 9(3x, 1pe13.6))

      if ( res_flag(counter) .EQ. 1 ) then

!!    calculation of missing widths


!!    three widths available

            if ((width_A(counter) .NE. dummy) .AND.                   &
                (width_B(counter) .NE. dummy) .AND.                   &
                (width_C(counter) .NE. dummy) .AND.                   &
                (width_tot(counter) .EQ. dummy)) then
                  width_tot(counter) = width_A(counter) +             &
                  width_B(counter) + width_C(counter)
      end if

            if ((width_A(counter) .NE. dummy) .AND.                   &
                (width_B(counter) .EQ. dummy) .AND.                   &
                (width_C(counter) .NE. dummy) .AND.                   &
                (width_tot(counter) .NE. dummy)) then
                  width_B(counter) = width_tot(counter) -             &
                  width_A(counter) - width_C(counter)
      end if

            if ((width_A(counter) .EQ. dummy) .AND.                   &
                (width_B(counter) .NE. dummy) .AND.                   &
                (width_C(counter) .NE. dummy) .AND.                   &
                (width_tot(counter) .NE. dummy)) then
                  width_A(counter) = width_tot(counter) -             &
                  width_B(counter) - width_C(counter)
      end if

!!    two widths available (specifically, combinations not including width_C)

            if ((width_A(counter) .NE. dummy) .AND.                   &
                (width_B(counter) .NE. dummy) .AND.                   &
                (width_C(counter) .EQ. dummy) .AND.                   &
                (width_tot(counter) .EQ. dummy)) then
                  width_tot(counter) = width_A(counter) +             &
                  width_B(counter)
      end if

            if ((width_A(counter) .NE. dummy) .AND.                   &
                (width_B(counter) .EQ. dummy) .AND.                   &
                (width_C(counter) .EQ. dummy) .AND.                   &
                (width_tot(counter) .NE. dummy)) then
                  width_B(counter) = width_tot(counter) -             &
                  width_A(counter)
      end if

            if ((width_A(counter) .EQ. dummy) .AND.                   &
                (width_B(counter) .NE. dummy) .AND.                   &
                (width_C(counter) .EQ. dummy) .AND.                   &
                (width_tot(counter) .NE. dummy)) then
                  width_A(counter) = width_tot(counter) -             &
                  width_B(counter)
      end if


      end if

            total_resonances = counter

70    continue
80    continue

!*******************************************************************************

!------------------------------------------------------------------------------!
!     {TITLE}                                                                  !
!     Entry of the run title and summary for offline users.                    !
!------------------------------------------------------------------------------!

      if (online_flag .EQ. 0) then

            print *, "What would you like the title of the run to be?"
            read '(a72)', title

            print *, "Summary of the run, line 1 (72-character limit):"
            read '(a72)', summary1
            print *, "Summary of the run, line 2 (72-character limit):"
            read '(a72)', summary2

      end if
      
      end subroutine

!!    {SBRT3}                                                                 !!

      subroutine nr_rate

!------------------------------------------------------------------------------!
!     This subroutine calculates the nonresonant reaction rate for the given   !
!     reaction.                                                                !
!------------------------------------------------------------------------------!

      use constants
      use variables

      implicit none

!!     {VDEC3}

!=====( Local variables )=======================================================

      integer*4               counter                 ! counter variables
      integer*4               counter2                !

      real*8                  c1                      ! coefficients from
      real*8                  c2                      ! the Iliadis text
      real*8                  c3
      real*8                  c4
      real*8                  c5
      real*8                  c6
      real*8                  c7


!------------------------------------------------------------------------------!
!     {NRES}                                                                   !
!     Nonresonant reaction rate calculated                                     !
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
!     The following code is based on equations 4.21, 4.23, 4.27, and 4.34      !
!     CAULDRONS IN THE COSMOS (1988) by Claus E. Rolfs and William S. Rodney,  !
!     chapter 4.2, pages 160-164.                                              !
!------------------------------------------------------------------------------!
!!$
!!$*     CONVERSION FROM MeV TO keV
!!$*      s=s*1d3
!!$*      dds=dds*1d-3
!!$*
!!$*      do 987 counter = 1, number_temps
!!$*
!!$*           tau(counter) =
!!$*     &  ( 42.48d0
!!$*     &    * ( ( z(1) ** 2.0d0 ) * ( z(2) ** 2.0d0 )
!!$*     &        * red_mass / ( matrix(counter,1) * 1.0d3 ) )
!!$*     &      ** ( 1.0d0 / 3.0d0 ) )
!!$*
!!$*            e0(counter) =
!!$*     &  ( 1.22d0
!!$*     &    * ( ( z(1) ** 2.0d0 )
!!$*     &      * ( z(2) ** 2.0d0 )
!!$*     &      * red_mass
!!$*     &      * ( matrix(counter,1) * 1.0d3 ) ** 2.0d0 )
!!$*     &        ** ( 1.0d0 / 3.0d0 ) )
!!$*
!!$*            s_eff(counter) =
!!$*     &  ( s *
!!$*     &    ( 1.0d0
!!$*     &      + 5.0d0 / ( 12.0d0 * tau(counter) )
!!$*     &      + ( ds / s )
!!$*     &            * ( e0(counter)
!!$*     &                  + ( 35.0d0 / 36.0d0)
!!$*     &                    * (k*1d3) * matrix(counter,1) )
!!$*     &      + .5d0 * ( dds / s )
!!$*     &            * ( e0(counter) ** 2d0
!!$*     &                  + ( 89d0 / 36d0 )
!!$*     &                    * e0(counter)
!!$*     &                    * (k*1d3) * matrix(counter,1) ) ) )
!!$*
!!$*            matrix(counter,2) =
!!$*     &  ( (N_a * 7.20d-19 )
!!$*     &    / ( red_mass * z(1) * z(2) )
!!$*     &        * tau(counter) ** 2d0
!!$*     &        * dexp( -tau(counter) )
!!$*     &        * s_eff(counter) )

!------------------------------------------------------------------------------!
!     The following code is based on equation 3.94 from NUCLEAR PHYSICS OF     !
!     STARS (2006) by Christian Iliadis, chapter 3.2, page 182.                !
!------------------------------------------------------------------------------!

            c1 =                                                       &
            7.8324d9 *                                                 &
            (z(1)**(2.0d0)*z(2)**(2.0d0)*red_mass)**(1.0d0/6.0d0) *    &
            s /                                                        &
            (red_mass)**(0.5d0)

            c2 =                                                       &
            4.2475d0 *                                                 &
            (z(1)**(2.0d0)*z(2)**(2.0d0)*red_mass)**(1.0d0/3.0d0)

            c3 =                                                       &
            9.810d-2 /                                                 &
            (z(1)**(2.0d0)*z(2)**(2.0d0)*red_mass)**(1.0d0/3.0d0)

            c4 =                                                       &
            .1220d0 *                                                  &
            ds / s *                                                   &
            (z(1)**(2.0d0)*z(2)**(2.0d0)*red_mass)**(1.0d0/3.0d0)

            c5 =                                                       &
            8.377d-2 *                                                 &
            ds / s

            c6 =                                                       &
            7.442d-3 *                                                 &
            dds / s *                                                  &
            (z(1)**(2.0d0)*z(2)**(2.0d0)*red_mass)**(2.0d0/3.0d0)

            c7 =                                                       &
            1.299d-2 *                                                 &
            dds / s *                                                  &
            (z(1)**(2.0d0)*z(2)**(2.0d0)*red_mass)**(1.0d0/3.0d0)


      do 987, counter = 1, number_temps

            matrix(counter, 2) =                                       &
            c1 / matrix(counter,1)**(2.0d0/3.0d0) *                    &
            exp(-c2 * (matrix(counter,1))**(-1d0/3d0)) *               &
            (                                                          &
            1.0d0 +                                                    &
            c3 * matrix(counter,1)**(1.0d0/3.0d0) +                    &
            c4 * matrix(counter,1)**(2.0d0/3.0d0) +                    &
            c5 * matrix(counter,1) +                                   &
            c6 * matrix(counter,1)**(4.0d0/3.0d0) +                    &
            c7 * matrix(counter,1)**(5.0d0/3.0d0)                      &
            )

!     if cutoff_flag .eq. 1, include the nonresonant cutoff
      if(cutoff_flag .EQ. 1) then
            print*, "T9 = ", matrix(counter, 1)
            print *, "nonresonant rate = ", matrix(counter, 2)
            matrix(counter, 2) = matrix(counter, 2) *                  &
            exp(-1.0d0 * (matrix(counter,1)/t9_cutoff)**2)
            print *, "nonresonant cutoff = ", matrix(counter, 2)
      end if      ! end of nonresonant cutoff

!     taking into account the possibility of identical particles
      if (delta .EQ. 1) then
            matrix(counter, 2) =                                       &
            matrix(counter, 2) / (1 + delta)
      end if

987   continue

!      print *, "s = ", s
!      print *, "ds = ", ds
!      print *, "dds = ", dds

      return
      end


!!    {SBRT4}                                                                 !!

      subroutine narrow_res_rate


!------------------------------------------------------------------------------!
!     This subroutine calculates the resonant reaction rate for the given      !
!     narrow resonances.                                                       !
!------------------------------------------------------------------------------!

      use constants
      use variables

      implicit none

!!    {VDEC4}

!=====( Local variables )=======================================================

      integer*4               counter                 ! counter variables
      integer*4               counter2

!------------------------------------------------------------------------------!
!     {RES_N}                                                                  !
!     Reaction rates calculated for narrow resonances                          !
!------------------------------------------------------------------------------!

      do 101, counter = 1, number_temps

!------------------------------------------------------------------------------!
!     The following code is based on equation 3.114 from NUCLEAR PHYSICS OF    !
!     STARS (2006) by Christian Iliadis, chapter 3.2, pages 193.               !
!------------------------------------------------------------------------------!

                  matrix(counter, res_number + 2) =                    &
                  1.5399d11 / (red_mass *                              &
                  matrix(counter, 1)) ** (1.5)                         &
                  * current_strength *                                 &
                  exp(-11.605 * current_energy /                       &
                  matrix(counter, 1))

!     taking into account the possibility of identical particles
      if (delta .EQ. 1) then
            matrix(counter, res_number + 2) =                          &
            matrix(counter, res_number + 2) / (1 + delta)
      end if

101   continue

      return
      end


!!    {SBRT5}                                                                 !!

      subroutine broad_res_rate

!------------------------------------------------------------------------------!
!     This subroutine calculates the resonant reaction rate for the given      !
!     broad resonances (where the ratio of the total width to the resonance    !
!     energy is greater than or equal to broad_cutoff.)                        !
!------------------------------------------------------------------------------!
      use constants
      use variables
      implicit none

!!     {VDEC5}

!=====( Local variables )=======================================================

      integer*4               counter                 ! counter variable
      integer*4               counter2                ! counter variable
      integer*4               counter3                ! counter variable
      
!=====( variables from Dr. Smith's original subroutine )========================

      real*8                  tt9                     ! local temperature 
                                                      ! variable

      real*8                  sum                     ! running sum of numerical
                                                      ! integration

      real*8                  integrand               ! value of integrand for
                                                      ! each step of numerical
                                                      ! integration

      real*8                  broad_energy(max_energies)  ! energy of the cross
                                                          ! section in MeV

      real*8                  sigma_BW(2,max_energies) ! Breit-Wigner Cross
                                                       ! section for broad
                                                       ! resonance as function
                                                       ! of energy in barns:
                                                       ! sigma_BW(1,i)=energy
                                                       ! sigma_BW(2,i)=cross
                                                       ! section

      real*8                  sfac_BW(2,max_energies) ! Breit-Wigner S-factor
                                                      ! for broad resonance
                                                      !   as function of energy
                                                      ! in MeV * barns:
                                                      ! sfac_BW(1,i)=energy
                                                      ! sfac_BW(2,i)=s-factor

!===============================================================================

!*******************************************************************************


!------------------------------------------------------------------------------!
!     {RES_B}                                                                  !
!     Reaction rates calculated for broad resonances                           !
!------------------------------------------------------------------------------!

!     calculate rate for a BROAD RESONANCE 
!     calls a subroutine to calculate breit-wigner cross section & s-factor
!     over broad resonance
!     then does a numerical integration of this cross section 
!     [folded with Maxwell-Boltzmann term] to get the reaction rate

!     initialize variables

      do counter = 1,max_energies
         broad_energy(counter)=0.0d0
      enddo

      do counter2=1,2
         do counter=1,max_energies
            sigma_BW(counter2,counter)=0.0d0
            sfac_BW(counter2,counter)=0.0d0
         enddo
      enddo



!     set up energy grid
!     store in common
!     make sure that num_energy, dE, and E_min are all appropriate for this reaction
!     this will depend on the reaction Q value, the energy and width of the resonance, 
!     and other factors
!     ideally we will have a TEST here to determine the appropriate grid
!
!     to test for "convergence" of our numerical integration,
!     we usually will tweak the grid [add more points] and see if the integration changes 
!     more than a threshold value. if NOT, then we are done. if it does change, repeat 
!     until no change above the threshold.
!
!     set quantities for energy grid and variations
!
!      num_energy = 100000    ! actual number of energies for numerical integration
!      if(num_energy.gt.max_energies) then print*,'too many energies'
!      
!      dE = 5.0d-05           ! grid size for numerical integration 
!                             !   over energy in MeV [0.01 keV]
!
!      E_min = 1.0d-04        ! initial energy for integration in MeV [0.1 keV]
!
      do counter = 1, num_energy   ! loop over energy to set up the grid

           broad_energy(counter) = E_min + (counter-1)*dE ! simple energy grid in MeV

      enddo         ! loop over energy to set up the grid


!       print*, 'before calling subroutine breit'

      call breit         ! calculate the breit-wigner cross section at all energies in the grid
                         ! store sigma_BW  and sfac_BW in common array

!       print*, 'after calling subroutine breit'

!     main temperature loop

      do counter2 = 1, number_temps      ! main loop over temperature to define the rate

      tt9 = matrix(counter2,1)             ! local temperature

      sum = 0.0d0                          ! initialize the running sum at start of loop
      integrand = 0.0d0                    ! initialize the integrand at start of loop

          do counter = 1, num_energy       ! loop in energy for the numerical integration

             integrand = 0.0d0             ! initialize the integrand at start of loop

!            units for reaction rate are cm3 s-1 mole-1

             if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'*****************'
             end if

             integrand = N_a*1.0d-30*c*(dsqrt(8.0d0/(pi*red_mass*amu)))  
                                        ! units are                                             
                                        ! Mole-1 * cm/sec * MeV**-0.5
                                        ! take out a factor of 1d30 to keep integrand small
             if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'1',counter,integrand
             end if

             integrand = integrand/((k*tt9)**1.5d0)    
                                         ! units   Mole-1  *  cm/sec * MeV**-2

             if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'2',counter,integrand
             end if

             integrand = integrand * 1.0d30 *                                    &
                        (sigma_BW(2,counter) * 1.0d-24) *                        &
                         broad_energy(counter)
                                          ! units Mole-1  *  cm/sec * MeV**-2 * cm2 * MeV
                                          ! = Mole-1 * cm3 * sec-1 * MeV-1
                                          ! note: I have put back in factor of 1d30
                                          ! note: conversion of sigma_BW to cm2 from barns

              if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'3',counter,integrand
              end if

              integrand = integrand*                                             &
                         (exp(-1.0d0*broad_energy(counter)/(k*tt9)))
                                           ! units Mole-1 * cm3 * sec-1 MeV-1 

              if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'4',counter,integrand
              end if

              if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'5  to add ',counter,dE*integrand
              end if

!        update the running integral
!        this uses a rectangular approximation - rather crude
!        can replace this with something more sophisticated in the future

              if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'sum before',counter,sum
              end if

              if(counter.eq.1.or.counter.eq.num_energy)then   
                                 ! only add in half of the energy interval at the ends
                 sum = sum + integrand*0.50d0*dE
              else
                 sum = sum + integrand*dE
              endif
                                 ! units of sum = Mole-1 * cm3 * sec-1, a rate unit

              if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'sum after',counter,sum
              end if

              if((counter2.eq.1).and.(counter.lt.4)) then
                  print*,'*****************'
              end if

          enddo ! energy loop over counter

        matrix(counter2, res_number+2) = sum              
                ! this pairs the rate with the correct temperature

!     taking into account the possibility of identical particles
      if (delta .EQ. 1) then
            matrix(counter2, res_number + 2) =                             &
            matrix(counter2, res_number + 2) / (1 + delta)
      end if

         print*,matrix(counter2, 1), matrix(counter2, res_number+2)

         print*,"end temp loop", counter2

      enddo ! temperature loop over counter2

      return
      end



      subroutine breit

!     calculate breit wigner cross section for a broad resonance
!     return into sigma_BW and sfac_BW and put into common
!     must make sure the energy grid is exactly the same as that used in the subroutine broad_resonance
!     so need to pass the energy grid ? determine this beforehand and put into common

      real*8 broad_energy(100000)             ! energy of the cross section in MeV
      real*8 sigma_BW(2,100000)         ! Breit-Wigner Cross section for broad resonance 
                                        !   as function of energy in barns
                                        ! sigma_BW(1,i)=energy
                                        ! sigma_BW(2,i)=cross section
      real*8 sfac_BW(2,100000)          ! Breit-Wigner S-factor for broad resonance 
                                        !   as function of energy in MeV * barns
                                        ! sfac_BW(1,i)=energy
                                        ! sfac_BW(2,i)=s-factor
      integer*4 i,j,n
      
      common / breit_wigner / broad_energy,sigma_BW,sfac_BW

      print*, 'start of subroutine breit'

!        for now, I will just put the cross section as a constant, which is ridiculous
!        for now, I will just put the s factor as a constant, which is not so bad actually

         do i=1,100000

            sigma_BW(1,i)=broad_energy(i)     ! in MeV
            sigma_BW(2,i)=1.0d-06       ! in barns   [1 microbarn]

            sfac_BW(1,i)=broad_energy(i)      ! in MeV
            sfac_BW(2,i)=1.0d-05        ! in MeV barns  [10 keV barns]

         enddo

      print*, 'end of subroutine breit'

      return
      end



!!    {SBRT6}                                                                 !!

      subroutine output

!------------------------------------------------------------------------------!
!     This subroutine calculates the total reaction rate at each temperature   !
!     in the temperature grid and writes the temperatures to the output file   !
!------------------------------------------------------------------------------!

      use constants
      use variables

      implicit none

!!    {VDEC6}

!=====( Local variables )=======================================================


      integer*4               counter                 ! counter variable
      integer*4               counter2                ! counter variable

      character*1             dummy_string            ! allows for skipping of
                                                      ! lines in input file

!===============================================================================

!------------------------------------------------------------------------------!
!     {WRITE}                                                                  !
!     Writing the temperatures and reaction rates to output                    !
!------------------------------------------------------------------------------!
                        ! begin test for missing output files
            if ((loc_out .NE. '') .AND.                                    &
                (loc_out2 .NE. '')) then
                  open(3, file = loc_out, status = 'unknown')
                  open(4, file = loc_out2, status = 'unknown')
            else if (loc_out .NE. '') then
                  open(3, file = loc_out, status = 'unknown')
            else if (loc_out2 .NE. '') then
                  open(4, file = loc_out2, status = 'unknown')
            else
                  print *, "Please specify an output file ",               &
                           "using -o or -vo."
                  stop
            end if      ! end test for missing output files

!!----writing output (online)---------------------------------------------------

      if (online_flag .EQ. 1) then

      write(3,*) "#"
!, identifier
      do counter = 1, number_temps
            write(3, 111) matrix(counter, 1), matrix(counter, 2)
111         format(2(1x,1pe13.6e3))
      end do
      do counter = 1, total_resonances
            write(3,*) "#"
            do counter2 = 1, number_temps
                  write(3,111) matrix(counter2, 1),                        &
                               matrix(counter2, counter+2)
            end do
      end do
      write(3,*) "#"
      do counter = 1, number_temps
            write(3,111) matrix(counter,1),                                &
                         matrix(counter, max_resonances+3)
      end do
      write(3,*) "#"
      do counter = 1, number_temps
            write(3,111) matrix(counter,1),                                &
                         matrix(counter, max_resonances+4)
      end do

      end if

!!----writing output (offline)--------------------------------------------------

      if (online_flag .EQ. 0) then

!!----output--------------------------------------------------------------------

      do 567, counter = 1, number_temps
            write (3, 345) matrix(counter,1), matrix(counter,2)
345         format (2(2X, 1pe13.6e3)$)

            do 456, counter2 = 1, total_resonances

!                   if ( ( matrix(counter, counter2 + 2) +
!      &                   matrix(counter, counter2 + 2) ) .EQ. 0d0 ) then
!                   continue
! 
!                   end if
                  write(3, 790) matrix(counter, counter2+2)
790               format (2X,1pe13.6e3$)


456         continue

            write (3, 457) matrix(counter, max_resonances+3),                &
                           matrix(counter, max_resonances+4)
457         format (2X, 1pe13.6e3, 2X, 1pe13.6e3)

567   continue

!!----verbose-output------------------------------------------------------------

      write(4,*) "Run title:"
      write(4,*) title
      write(4,*) ""
      write(4,*) "Run description:"
      write(4,*) summary1
      write(4,*) summary2
      write(4,*) ""
      write(4,*) "-------------------------------------------",              &
                 "-----------------------------"

      write(4,*) ""
      write(4,*) "Below is a record of the input used in this run:"
      write(4,*) ""

      write(4,*) ">> Reaction: Gives information about the ",                &
                 "reaction being evaluated."
      write(4,*) "--Particle 1: charge, mass [format: 2(1x, i3)]"
      write(4,98) z(1), a(1)
98    format (2(1x, i3))
      write(4,*) "--Particle 2: charge, mass [format: 2(1x, 1pe12.6)]]"
      write(4,98) z(2), a(2)
      write(4,*) "--Particle 3: charge, mass [format: 2(1x, 1pe12.6)]"
      write(4,98) z(3), a(3)
      write(4,*) "--Particle 4: charge, mass [format: 2(1x, 1pe12.6)]"
      write(4,98) z(4), a(4)

      write(4,*) "========================================",               &
                 "========================================"

      write(4,*) ">> Temperature flag: allows you to choose ",             &
                 "what type of temperature grid you'd like to use."
      write(4,*) "1. Logarithmic scale, 28 values: .01, .02, ..., ",       &
                 ".09, .10, .20, ..., .90, 1.0, 2.0, ..., 9.0, 10.0"
      write(4,*) "2. Logarithmic scale, 271 values: .01, .011, ..., ",     &
                 ".099, .10, .11, ..., .99, 1.0, 1.1, ..., 9.9, 10.0"
      write(4,*) "3. Custom linear scale: set the minimum, maximum, ",     &
                 "and step size according to the directions below."
      write(4,*) "4. Imported scale: enter the name of the file ",         &
                 "containing your desired temperatures according ",        &
                 "to the directions below."
      write(4,*) "--[format: (1x, i1), eg, "" 1""]"
      write(4,99) temp_flag
99    format (1x, i1)

      write(4,*) "========================================",               &
                 "========================================"

      write(4,*) ">> Temperature range values: minimum temperature,",      &
                 " maximum temperature, and temperature increment, ",      &
                 "respectively."
      write(4,*) "--temperature units: GK [format: 3(1x, 1pe12.6)]"
      write(4,100) Tmin, Tmax, Tstep
100   format (3(1x, 1pe12.6))

      write(4,*) "========================================",               &
                 "========================================"

      write(4,*) ">> Temperature grid input file name (maximum ",          &
                 "of 72 characters, including extension).  [format: ",     &
                 "(1x, a72), eg, "" this_is_the_temp_grid.dat""]"
      write(4,*) "--Be sure to arrange the temperatures such that ",       &
                 "there is only one temperature per line and no blank ",   &
                 "line at the end of the file."
      write(4,101) loc_temp
101   format (1x, a72)

      write(4,*) "========================================",               &
                 "========================================"

      write(4,*) ">> Nonresonant inclusion flag: Tells whether ",          &
                 "or not to calculate the nonresonant rate (no = ",        &
                 "0, yes = 1). [format: 1x, i1]"
      write(4,102) nr_flag
102   format (1x, i1)

      write(4,*) "========================================",               &
                 "========================================"

      write(4,*) ">> S-factors: The values of the astrophysical ",       &
                 "s-factor and its first two derivatives at zero."
      write(4,*) "--s-factor units: MeV*b, b, b/MeV ",                   &
                 "[format: 1x, i1, 3(1x, 1pe13.6)]"
      write(4,103) s, ds, dds
103   format (3(1x, 1pe13.6))

      write(4,*) "========================================",             &
                 "========================================"

      write(4,*) ">> Nonresonant Cutoff: An additional term ",           &
                 "added to alter the nonresonant rate at high ",         &
                 "temperatures." 
      write(4,*) "--The leading integer is the cutoff flag, which ",     &
                 "tells TReRaC whether or not it should reduce the ",    &
                 "nonresonant rate at high temperatures."
      write(4,*) "--The second number is the temperature cutoff for ",   &
                 "such an operation."
      write(4,*) "--Temperature Cutoff units: GK [format: 1x, i1, ",     &
                 "1x, 1pe12.6)]"
      write(4,104) cutoff_flag, t9_cutoff
104   format (1x, i1, 1x, 1pe12.6)

      write(4,*) "========================================",             &
                 "========================================"

      write(4,*) ">> Broad resonance cutoff: minimum ratio of total ",   &
                 "width to resonance energy for a resonance to be ",     &
                 "considered broad [format: 1x, 1pe12.6]"
      write(4,105) broad_cutoff
105   format (1x, 1pe12.6)

      write(4,*) "========================================",             &
                 "========================================"

      write(4,*) ">> Floating-Point Comparison Threshold: maximum ",     &
                 "tolerance of floating point comparisons as a ",        &
                 "percentage [format: 1x, 1pe12.6]"
      write(4,106) eqv_threshold
106   format (1x, 1pe12.6)

      write(4,*) "========================================",             &
                 "========================================"

      write(4,*) ">> Resonance Data: The known information ",            &
                 "about each resonance or the dummy value ",             &
                 "-1.000000E+00."
      write(4,*) "1. resonance flag: tells whether or not the ",         &
                 "resonance should be included in the calculation ",     &
                 "(no = 0, yes = 1)"
      write(4,*) "2. broad flag: allows user to specify whether ",       &
                 "or not a resonance is broad (narrow = 0, broad = ",    &
                 "1, unknown = 2)"
      write(4,*) "3. energy of the resonance, units: MeV"
      write(4,*) "4. strength of the resonance, units: MeV"
      write(4,*) "5. omega, the statistical factor, units: none"
      write(4,*) "6. gamma, the width ratio, units: MeV"
      write(4,*) "7. the width of the entrance channel, units: MeV"
      write(4,*) "8. the width of the exit channel, units: MeV"
      write(4,*) "9. the width of other channels, units: MeV"
      write(4,*) "10. the total width of the resonance, units: MeV"
      write(4,*) "11. the angular momentum of the resonant state, ",     &
                 "units: none."

      write(4,*) "[format: 1x, i1, 3x, i1, 9(3x, 1pe13.6)]"
      write(4,*) ""
      write(4,*) "1 | 2 |        3      |        4      |        ",      &
                 "5      |        6      |        7      |        ",     &
                 "8      |        9      |       10      |       11"
      write(4,*) "--|---|---------------|---------------|--------",      &
                 "-------|---------------|---------------|--------",     &
                 "-------|---------------|---------------|-------------"

      do counter = 1, total_resonances
      write(4,107) res_flag(counter), "|",                               &
                   broad_flag(counter), "|",                             &
                   energy(counter), "|",                                 &
                   strength(counter), "|",                               &
                   omega(counter), "|",                                  &
                   gamma(counter), "|",                                  &
                   width_A(counter), "|",                                &
                   width_B(counter), "|",                                &
                   width_C(counter), "|",                                &
                   width_tot(counter), "|",                              &
                   j_res(counter)
107   format (1x, i1, 1x, a1, 1x, i1, 9(1x, a1, 1x, 1pe13.6))
      end do

      write(4,*) ""
      write(4,*) "-------------------------------------------",          &
                 "-----------------------------"
      write(4,*) ""
      write(4,*) "Below is a record of the various calculated rates"
      write(4,*) "paired with their respective temperatures:"
      write(4,*) ""

      write(4,108) "  T9 [GK]   |   nonres    |"
108   format(1x, a27$)
      do counter = 1, total_resonances
            write(4,109) "rate", counter, " |"
109         format(1x, a5, 1x, i3, 2x, a2$)
      end do
      write(4,*) " res total  | total rate"

      do counter = 1, number_temps
            write(4, 346) matrix(counter,1), "|",                        &
                          matrix(counter,2),"|"
346         format(1X, 1PE11.4e3, 1X,a1,1x, 1PE11.4e3,1x,a1$)

            do counter2 = 1, total_resonances
                  write(4, 791) matrix(counter, counter2+2), "|"
791               format(1X,1PE11.4e3,1x,a1$)

            end do

            write(4, 458) matrix(counter, max_resonances+3), "|",        &
                          matrix(counter, max_resonances+4)
458         format(1X, 1PE11.4e3, 1X,a1,1x, 1PE11.4e3)

      end do

!!-----writing output to be used with xmgr--------------------------------------

!       open (5, file = 'xmgr_total_rate.dat', status = 'unknown')
!       do counter = 1, number_temps
!             write(5,378) matrix(counter,1),
!      &                   matrix(counter,max_resonances+4)
! 378         format (1x, 1pe13.6e3, 1x, 1pe13.6e3)
!       end do
! 
!       open (6, file = 'xmgr_total_res_rate.dat', status = 'unknown')
!       do counter = 1, number_temps
!             write(6,378) matrix(counter,1),
!      &                   matrix(counter,max_resonances+3)
!       end do
! 
!       open (7, file = 'xmgr_total_nonres_rate.dat', status = 'unknown')
!       do counter = 1, number_temps
!             write(7,378) matrix(counter,1),
!      &                   matrix(counter,2)
!       end do

      end if


      return
      end



!!    {SBRT7}                                                                !!

      subroutine strength_calc

!------------------------------------------------------------------------------!
!     This subroutine calculates the strengths for resonances if they are not  !
!     already given.                                                           !
!------------------------------------------------------------------------------!

      use constants
      use variables

      implicit none

!!    {VDEC7}

!=====( Local variables )=======================================================

      integer*4               counter                 ! counter variable

!===============================================================================


!------------------------------------------------------------------------------!
!     {STRCALC}                                                                !
!     Strengths for resonances calculated                                      !
!------------------------------------------------------------------------------!

      do counter = 1, total_resonances

!------------------------------------------------------------------------------!
!     calculate omega and gamma, if necessary                                  !
!------------------------------------------------------------------------------!

!       if ( res_flag(counter) .NE. 0 ) then
! 
!       if ( strength(counter) .EQ. dummy ) then
!             if ( omega(counter) .EQ. dummy ) then
!                   if ( j_res(counter) .EQ. dummy ) then
!             print *, "You seem to be missing the strength, omega, and "
!             print *, "j_res for resonance ", counter, "."
!             print *, "Please try entering them again."
!             print *, "The program will now exit."
!             stop
!                   end if
!             end if
! 
!             if ( gamma(counter) .EQ. dummy ) then
!                   if (  width_A(counter) .EQ. dummy  .OR.
!      &                  width_B(counter) .EQ. dummy  .OR.
!      &                  width_tot(counter) .EQ. dummy  ) then
!             print *, "You seem to be missing the strength, gamma, and "
!             print *, "width_A/width_B/width_tot for ",
!      &                "resonance ", counter, "."
!             print *, "Please try entering them again."
!             print *, "The program will now exit."
!             stop
!                   end if
!             end if
!       end if
! 
!       end if

      if (strength(counter) .EQ. dummy) then

      if (res_flag(counter) .EQ. 1) then

            if ( omega(counter) .EQ. dummy ) then
                  omega(counter) = ( 2.0d0 * j_res(counter) + 1.0d0 ) /     &
                                          (                                 &
                                          ( 2.0d0 * j(1) + 1.0d0 ) *        &
                                          ( 2.0d0 * j(2) + 1.0d0 )          &
                                          )
            end if

            if ( gamma(counter) .EQ. dummy ) then
                  gamma(counter) = ( width_A(counter) *                     &
                                     width_B(counter) ) /                   &
                                   ( width_tot(counter) )
            end if

      end if

      end if

!------------------------------------------------------------------------------!
!     the strength need only be calculated if the resonance is narrow          !
!------------------------------------------------------------------------------!

!             if ( status(counter) .EQ. 0 ) then
                  if ( strength(counter) .EQ. dummy ) then
                        strength(counter) = omega(counter) *           &
                                            gamma(counter)
                  end if
!             end if

      end do

      return
      end

!!    {SBRT8}

      subroutine test

!------------------------------------------------------------------------------!
!     This subroutine tests various combinations of inputs to determine        !
!     whether or not they are acceptable.                                      !
!------------------------------------------------------------------------------!
      use constants
      use variables
      implicit none

!!    {VDEC8}

!=====( Local variables )=======================================================

      integer*4               counter                 ! counter variable

      integer*1               error_flag              ! 0 if error-free,
                                                      ! 1 is error present

!===============================================================================


!------------------------------------------------------------------------------!
!     {IN_TEST}                                                                !
!     Input Tests                                                              !
!------------------------------------------------------------------------------!

!!----------------------------------------------------------------------------!!
!!    Opening output file in case there are any errors                        !!
!!----------------------------------------------------------------------------!!

      if (online_flag .EQ. 0) then
            open(12, file="errors.dat", status="unknown")
      else
            open(12, file=loc_out, status="unknown")
      end if

      if (test_count .EQ. 1) then

!!----Test for the existence of file in location--------------------------------

      inquire(file = loc_in, exist = loc_flag)
      if (.NOT. loc_flag) then
            print *, "I'm sorry, but you appear to be missing ",        &
                     "the input file."
            print *, "Please try again."
            write(12,300) 1
 300         format(i2,1x$)
            write(12,*)  "input file not found"
            stop
      end if

      end if

!------------------------------------------------------------------------------!
!     {TEMP_TEST}
!     Temperature Grid Tests                                                   !
!------------------------------------------------------------------------------!

      if (test_count .EQ. 2) then

!!----Testing for values of temp_flag-------------------------------------------

      if ((temp_flag .NE. 1) .AND.                                      &
          (temp_flag .NE. 2) .AND.                                      &
          (temp_flag .NE. 3) .AND.                                      &
          (temp_flag .NE. 4) .AND.                                      &
          (temp_flag .NE. 5)) then
      print *, "Invalid value for temp_flag."
      print *, "Please try entering it again."
            write(12,300) 2
            write(12,*)  "invalid temp_flag"
            stop
      end if

!!----Testing for read errors---------------------------------------------------

      if (err_flag .EQV. .true.) then
            print *, "Error while reading line number", err_line
            print *, "of temperature input file."
            print *, "Please check the file for errors and try again."
            write(12,300) 3
            write(12,302)  "temp input file error on line ", err_line
            stop
      end if

!!----Testing for too many temperatures-----------------------------------------

      if ((number_temps .LE. 0) .OR.                                     &
          (number_temps .GT. max_temps)) then
      print *, "Invalid number of temperatures."
      print *, "Please adjust your temperature grid accordingly."
            write(12,300) 4
            write(12,*)  "invalid number of temperatures"
            stop
      end if

!!----Testing for invalid temperatures------------------------------------------

      do counter = 1, number_temps
            if ((matrix(counter,1) .LT. 1.0d-2) .OR.                     &
                (matrix(counter,1) .GT. 1.0d1)) then
            print *, "Temperature ", counter, "is invalid."
            print *, "Please try entering temperatures "
            print *, "between .01GK and 10GK."
            write(12,300) 5
            write(12,*)  "invalid temperature"
            stop
            end if      ! end invalid temperature test
      end do      ! end loop over number_temps

!!----Testing for existence of temperature input file---------------------------

      if (temp_flag .EQ. 4) then
            inquire(file = loc_temp, exist = loc_flag)
            if (.NOT. loc_flag) then
                  print *, "Temperature input file does not exist."
                  print *, "Please chack that the file name was"
                  print *, "entered correctly and try again."
            write(12,300) 6
            write(12,*)  "missing temperature input file"
            stop
            end if
      end if

      end if

!------------------------------------------------------------------------------!
!     {REAC_TEST}                                                              !
!     Reactants Tests                                                          !
!------------------------------------------------------------------------------!

      if (test_count .EQ. 3) then

!!----Test for Reaction Validity------------------------------------------------

      if ( ( z(1) .LE. 0 ) .OR.                                           &
           ( z(2) .LE. 0 ) .OR.                                           &
           ( z(3) .LT. 0 ) .OR.                                           &
           ( z(4) .LT. 0 ) .OR.                                           &
           ( z(1) + z(2) .NE. z(3) + z(4) ) .OR.                          &
           ( a(1) .LE. 0 ) .OR.                                           &
           ( a(2) .LE. 0 ) .OR.                                           &
           ( a(3) .LT. 0 ) .OR.                                           &
           ( a(4) .LT. 0 ) .OR.                                           &
           ( a(1) + a(2) .NE. a(3) + a(4) ) ) then
            print *, 'Invalid reaction in input file.'
            print *, 'Please verify that you have the correct ',          &
                     'values and try running the program again.'
            write(12,300) 7
            write(12,*)  "invalid reaction"
            stop
      end if

      end if

!------------------------------------------------------------------------------!
!     {NR_TEST}                                                                !
!     Nonresonant Rate Calculation Tests                                       !
!------------------------------------------------------------------------------!

      if (test_count .EQ. 4) then

!!----Test for acceptable values of nr_flag-------------------------------------

      if ((nr_flag .NE. 1) .AND. (nr_flag .NE. 0)) then
            print *, "Unacceptable value for nr_flag."
            print *, "Please enter either 0 or 1."
            write(12,300) 8
            write(12,*) "unacceptable nr_flag value"
            stop
      end if

      if ((cutoff_flag .NE. 1) .AND. (cutoff_flag .NE. 0)) then
            print *, "Unacceptable value for cutoff_flag."
            print *, "Please enter either 0 or 1."
            write(12,300) 9
            write(12,*) "unacceptable cutoff_flag value"
            stop
      end if

!     note that we don't care what t9_cutoff is if cutoff_flag is 0
      if ((t9_cutoff .LE. 0) .AND. (cutoff_flag .EQ. 1)) then
            print *, "Unacceptable value for t9_cutoff."
            print *, "Please enter a positive temperature."
            write(12,300) 10
            write(12,*) "unacceptable t9_cutoff value"
            stop
      end if

      end if

!------------------------------------------------------------------------------!
!     {RES_TEST}                                                               !
!     Resonant Rate Calculation Tests                                          !
!------------------------------------------------------------------------------!

      if (test_count .EQ. 5) then

      do counter = 1, total_resonances

!!----Test for unacceptable values of broad_flag--------------------------------

            if ((broad_flag(counter) .NE. 0) .AND.                         &
                (broad_flag(counter) .NE. 1) .AND.                         &
                (broad_flag(counter) .NE. 2)) then
                  print *, "Unacceptable value for broad_flag",            &
                           counter, "."
                  print *, "Please enter 0, 1, or 2."
            write(12,300) 11
            write(12,*)  "unacceptable broad_flag value"
                  stop
            end if


!!----Test for broad_cutoff being too low---------------------------------------------

            if (broad_cutoff .LT. 1.0d-3) then
                  print *, "Your entry for broad_cutoff is too low."
                  print *, "Defaulting to .1% ."
                  broad_cutoff = 1.0d-3
            end if


!!----Test for width_tot/energy ratio being too low while-----------------------
!!----status indicates that the resonance should be broad-----------------------

                  if ((status(counter) .EQ. 1) .AND.                      &
                      (width_tot(counter) .NE. dummy) .AND.               &
                      (width_tot(counter) / energy(counter) .LT.          &
                       broad_cutoff)) then
                        print *, "Your entries for the width and ",       &
                        "energy of resonance ", counter, "are in a "      
                        print *, "ratio that is too low to be ",          &
                        "calculated as broad.  Please change "
                        print *, "its label and try again."
            write(12,300) 12
            write(12,301)  "resonance ",counter," not broad"
301         format(1x,a,i2,a)
                        stop
                  end if





      if (res_flag(counter) .EQ. 1) then

!!----Test to set status by broad_cutoff----------------------------------------------

            if (width_tot(counter) .NE. dummy) then
                  if (width_tot(counter) / energy(counter) .LT.           &
                       broad_cutoff) then
                        status(counter) = 0
                  else if (width_tot(counter) / energy(counter) .GE.      &
                            broad_cutoff) then
                        status(counter) = 1
                  end if
            else
                  print *, "Resonance number ", counter, " calculated",   &
                           " as narrow due to lack of total width."
                  status(counter) = 0
            end if

!!----Test for inconsistency between broad_cutoff-derived status and------------------
!!----user-entered value for broad_flag-----------------------------------------

            if ((broad_flag(counter) .NE. 2) .AND.                        &
                (status(counter) .NE. broad_flag(counter))) then
                  print *, "broad_flag", counter, " does not ",           &
                           "agree with broad_cutoff-evaluated value ",    &
                           "for status."
                  print *, "The program will continue ",                  &
                           "using broad_flag."
                  status(counter) = broad_flag(counter)
            end if

!!----Test for non-inclusion of width_tot for broad resonances------------------

            if ((status(counter) .EQ. 1) .AND.                            &
                (width_tot(counter) .EQ. dummy)) then
                  print *, "Resonance", counter, "is broad, but ",        &
                           "the total width is not included."
                  print *, "Please enter the total width in input file."
            write(12,300) 13
            write(12,301)  "resonance ",counter," missing total width"
                  stop
            end if

!!----Test for acceptable values of energy--------------------------------------

            if (energy(counter) .LE. 0d0) then
                  if (energy(counter) .NE. -1.0d0) then
                  print *, "Energy of resonance ", counter, "is ",        &
                           "negative."
                  print *, "Please change this value and try again."
            write(12,300) 14
            write(12,302)  "unacceptable value for energy ",counter
302         format(1x,a,i4)
                  stop
                  else
                  print *, "Energy is required for resonance ", counter
                  print *, "and should be included in input file."
            write(12,300) 15
            write(12,301)  "resonance ",counter," missing energy"
                  stop
                  end if
            end if

!!----Test for acceptable values of strength------------------------------------

            if (((omega(counter) .EQ. dummy) .AND.                      &
                 (j_res(counter) .EQ. dummy)) .AND.                     &
                ((gamma(counter) .EQ. dummy) .AND.                      &
                 (width_A(counter) .EQ. dummy) .AND.                    &
                 (width_B(counter) .EQ. dummy) .AND.                    &
                 (width_C(counter) .EQ. dummy) .AND.                    &
                 (width_tot(counter) .EQ. dummy))) then
                  if (strength(counter) .LT. 0) then
                        if (strength(counter) .NE. dummy) then
                              print *, "Strength ", counter,            &
                              "needs to be a positive value."
            write(12,300) 16
            write(12,302)  "unacceptable value for strength ",counter
                              stop
                        else
                              print *, "Please enter strength ",        &
                              "or values enough to calculate it for "
                              print *, "resonance ", counter
            write(12,300) 17
            write(12,301)  "resonance ",counter," missing strength"
                              stop
                        end if
                  end if
            end if

      !!----only giving errors if the strength is not given---------------------
      !!----and there are errors in omega/j_res or widths/gamma-----------------

      if (strength(counter) .EQ. dummy) then

!!----Test for acceptable values of omega and j_res-----------------------------

            if (omega(counter) .EQ. dummy) then
                  if (j_res(counter) .EQ. dummy) then
            print *, "Resonance ", counter, "is missing ",                &
                     "both omega and j_res."
            print *, "Please try entering them again."
            write(12,300) 18
            write(12,301) "resonance ",counter," missing omega/j_res"
            stop
                  else if (j_res(counter) .LT. 0) then
            print *, "J_res ", counter, "cannot have a negative value."
            print *, "Please try entering it again."
            write(12,300) 19
            write(12,302) "unacceptable value for j_res ",counter
                  stop
                  end if
            else if (omega(counter) .LT. 0) then
            print *, "Omega ", counter, "cannot have a negative value."
            print *, "Please try entering it again."
            write(12,300) 20
            write(12,302) "unacceptable value for omega ",counter
            stop
            end if

!!----Test for consistency of omega/j_res---------------------------------------

      if (omega(counter) .NE. dummy) then

      if ( abs((omega(counter) - (2.0d0 * j_res(counter)                  &
              + 1.0d0) / ((2.0d0 * j(1) + 1.0d0))) /                      &
            omega(counter)) .GT. eqv_threshold ) then
            print *, "Omega and j_res are inconsistent."
            print *, "Please check that the entered values are"
            print *, "correct or change the threshold for"
            print *, "floating-point equivalence."
            write(12,300) 21
            write(12,302) "inconsistency in omega/j_res ",counter
            stop
      else if ( abs((omega(counter) - (2.0d0 * j_res(counter)             &
              + 1.0d0) / ((2.0d0 * j(1) + 1.0d0))) /                      &
            (2.0d0 * j_res(counter)                                       &
              + 1.0d0) / ((2.0d0 * j(1) + 1.0d0))) .GT.                   &
            eqv_threshold ) then
            print *, "Omega and j_res are inconsistent."
            print *, "Please check that the entered values are"
            print *, "correct or change the threshold for"
            print *, "floating-point equivalence."
            write(12,300) 21
            write(12,302) "inconsistency in omega/j_res ",counter
            stop
      end if

      end if



!!----Test for acceptable values of gamma and widths----------------------------

            if ( gamma(counter) .EQ. dummy ) then
                  if ((width_A(counter) .EQ. dummy) .OR.                   &
                      (width_B(counter) .EQ. dummy) .OR.                   &
                      (width_tot(counter) .EQ. dummy)  ) then
            print *, "Resonance ", counter, "is missing the following:"
            print *, " > gamma"
            if (width_A(counter) .EQ. dummy) print *, " > width_A"
            if (width_B(counter) .EQ. dummy) print *, " > width_B"
            if (width_tot(counter) .EQ. dummy) print *, " > width_tot"
            print *, "Please try entering them again."
            write(12,300) 22
            write(12,301) "resonance ",counter," missing gamma/width(s)"
                  stop
                  else if (width_A(counter) .LT. 0) then
            print *, "width_A ", counter, "cannot have a ",                &
                     "negative value."
            print *, "Please try entering it again."
            write(12,300) 23
            write(12,302) "unacceptable value for width_A ",counter
                  stop
                  else if (width_B(counter) .LT. 0) then
            print *, "width_B ", counter, "cannot have a ",                &
                     "negative value."
            print *, "Please try entering it again."
            write(12,300) 24
            write(12,302) "unacceptable value for width_B ",counter
                  stop
                  else if ((width_C(counter) .LT. 0) .AND.                 &
                           (width_C(counter) .NE. dummy)) then
            print *, "width_C ", counter, "cannot have a ",                &
                     "negative value."
            print *, "Please try entering it again."
            write(12,300) 25
            write(12,302) "unacceptable value for width_C ",counter
                  stop
                  else if ((width_tot(counter) .LT. 0) .AND.               &
                           (width_tot(counter) .NE. dummy)) then
            print *, "width_tot ", counter, "cannot have a ",              &
                     "negative value."
            print *, "Please try entering it again."
            write(12,300) 26
            write(12,302) "unacceptable value for width_tot ",counter
                  stop
                  end if
            else if (gamma(counter) .LT. 0) then
            print *, "Gamma ", counter, "cannot have a negative value."
            print *, "Please try entering it again."
            write(12,300) 27
            write(12,302) "unacceptable value for gamma ",counter
            stop
            end if

!!----Test for width consistency------------------------------------------------

            if (gamma(counter) .NE. dummy) then

      if ( abs((gamma(counter) - width_A(counter) *                       &
            width_B(counter) / width_tot(counter)) /                      &
            gamma(counter)) .GT. eqv_threshold ) then
            print *, "Gamma and the entered widths are inconsistent."
            print *, "Please check that the entered values are"
            print *, "correct or change the threshold for"
            print *, "floating-point equivalence."
            write(12,300) 28
            write(12,302) "inconsistency in gamma/widths ",counter
            stop
      else if ( abs((gamma(counter) - width_A(counter) *                  &
            width_B(counter) / width_tot(counter)) /                      &
            (width_A(counter) *                                           &
            width_B(counter) / width_tot(counter))) .GT.                  &
            eqv_threshold ) then
            print *, "Gamma and the entered widths are inconsistent."
            print *, "Please check that the entered values are"
            print *, "correct or change the threshold for"
            print *, "floating-point equivalence."
            write(12,300) 29
            write(12,302) "inconsistency in gamma/widths ",counter
            stop
      end if

            end if

            end if

      end if

!!------------------------------------------------------------------------------

      end do      ! end of resonances loop

      if(online_flag .EQ. 0) then
      write(12,*) "No errors detected"
      end if

      end if      ! end of resonant rate tests



      return
      end


!!    {SBRT9}                                                                 !!

      subroutine data_table

!------------------------------------------------------------------------------!
!     This subroutine sets the binding energies and momenta for each of the    !
!     particles involved in the reaction.                                      !
!------------------------------------------------------------------------------!

      use constants
      use variables

      implicit none

!!    {VDEC9}

!=====( Local Variables )=======================================================

      integer*4               counter                 ! counter variable

      REAL, DIMENSION(0:120,500,2) :: mass_table      !new array that will hold
                                                      !the mass table after
                                                      !being read in

      integer ::      IOstatus                        !variable to track if at
                                                      !EOF

      real :: z_temp, a_temp, ME_temp, J_temp         !temporary variables
                                                      !that are read and stored
                                                      !in mass_table

!===============================================================================

!------------------------------------------------------------------------------!
!     {TABLE1}                                                                 !
!                                                                              !
!------------------------------------------------------------------------------!

      open(42, file = 'mass_table.dat', status = 'old')
      
      do
         read(42, *, IOSTAT=IOstatus) z_temp, a_temp,         &
                                      ME_temp, J_temp
         if (IOstatus .eq. 0) then
            mass_table(z_temp,a_temp,1)=ME_temp
            mass_table(z_temp,a_temp,2)=J_temp
         else if (IOstatus .gt. 0) then
            print *, 'There is a problem with the mass table'
         else
            exit
         end if
      end do

!------------------------------------------------------------------------------!
!     {TABLE2}                                                                 !
!                                                                              !
!------------------------------------------------------------------------------!


      do counter = 1,4
         me(counter)=mass_table(z(counter),a(counter),1)
         j(counter)=mass_table(z(counter),a(counter),2)

!Convert from Mass Excess to mass
         mass(counter) = (real(a(counter))*amu+me(counter))/amu
!         print *,"z",z(counter),"a",a(counter),"mass",mass(counter)
      end do
!         print *, "z  ", counter, " = ", z(counter)
!         print *, "a  ", counter, " = ", a(counter)
!         print *, "me ", counter, " = ", me(counter)
!         print *, "j  ", counter, " = ", j(counter)
!         print *, "p  ", counter, " = ", p(counter)

      close(42)

      if(gamma_flag .EQ. 0) then
            red_mass2 = mass(3) * mass(4) /                     &
                       (mass(3) + mass(4))
      end if

      return
      end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     {SAMPLE}                                                                 !
!     Sample input file                                                        !
!                                                                              !
!     Note: I have added "! " to each line below                               !
!                                                                              !
! >> Reaction: Gives information about the reaction being evaluated.
! --Particle 1: charge, mass [format: 2(1x, i3), eg, " 123 456"]
!    9  17
! --Particle 2: charge, mass [format: 2(1x, i3), eg, " 123 456"]
!    1   1
! --Particle 3: charge, mass [format: 2(1x, i3), eg, " 123 456"]
!    0   0
! --Particle 4: charge, mass [format: 2(1x, i3), eg, " 123 456"]
!   10  18
! ================================================================================
! >> Temperature flag: allows you to choose what type of temperature grid you'd like to use.
! 1. Logarithmic scale, 28 values: .01, .02, ..., .09, .10, .20, ..., .90, 1.0, 2.0, ..., 9.0, 10.0
! 2. Logarithmic scale, 271 values: .01, .011, ..., .099, .10, .11, ..., .99, 1.0, 1.1, ..., 9.9, 10.0
! 3. Custom linear scale: set the minimum, maximum, and step size according to the directions below.
! 4. Imported scale: enter the name of the file containing your desired temperatures according to the directions below.
! 5. Linear scale in the logarithm of T9: -2, -1.99, ..., -1, -.99, ..., -.01, 0.0, 0.1, ..., .99, 1.0
! --[format: (1x, i1), eg, " 1"]
!  2
! ================================================================================
! >> Temperature range values: minimum temperature, maximum temperature, and temperature increment, respectively.
! --temperature units: GK [format: 3(1x, 1pe12.6), eg, " 1.000000E-02 1.000000E-01 1.000000+00"]
!  1.000000E-01 1.000000E+01 1.000000E-01
! ================================================================================
! >> Temperature grid input file name (maximum of 72 characters, including extension).  [format: (1x, a72), eg, " this_is_the_temp_grid.dat"]
! --Be sure to arrange the temperatures such that there is only one temperature per line and no blank line at the end of the file.
!  input_temp.dat
! ================================================================================
! >> Nonresonant inclusion flag: Tells whether or not to calculate the nonresonant rate (no = 0, yes = 1). [format: 1x, i1]
!  1
! ================================================================================
! >> S-factors: The values of the astrophysical s-factor and its first two derivatives at zero.
! --s-factor units: MeV*b, b, b/MeV [format: 3(1x, 1pe13.6)]
! +2.900000E-03 -1.300000E-03 +4.700000E-04
! ================================================================================
! >> Nonresonant Cutoff: An additional term added to alter the nonresonant rate at high temperatures.
! --The leading integer is the cutoff flag, which tells TReRaC whether or not it should reduce the nonresonant rate at high temperatures.
! --The second number is the temperature cutoff for such an operation.
! --Temperature Cutoff units: GK [format: 1x, i1, 1x, 1pe12.6)]
!  0 0.000000E+00
! ================================================================================
! >> Broad Resonance Cutoff: minimum ratio of total width to resonance energy for a resonance to be considered broad [format: 1x, 1pe12.6]
!  3.000000E-02
! ================================================================================
! >> Floating-Point Comparison Threshold: maximum tolerance of floating point comparisons as a percentage [format: 1x, 1pe12.6]
!  1.000000E-02
! ================================================================================
! >> Resonance Data: The known information about each resonance or the dummy value -1.000000E+00.
! 1. resonance flag: tells whether or not the resonance should be included in the calculation (no = 0, yes = 1)
! 2. broad flag: allows user to specify whether or not a resonance is broad (narrow = 0, broad = 1, unknown = 2)
! 3. energy of the resonance: units: MeV
! 4. strength of the resonance: units: MeV
! 5. omega: the statistical factor: units: none
! 6. gamma: the width ratio: units: MeV
! 7. the width of the entrance channel: units: MeV
! 8. the width of the exit channel: units: MeV
! 9. the width of other channels: units: MeV
! 10. the total width of the resonance: units: MeV
! 11. the angular momentum of the resonant state: units: none.
! [format: 1x, i1, 3x, i1, 9(3x, 1pe13.6), eg, " 1 | 1 | +1.000000E-01 | -1.000000E+00 | ...etc."]
! 
!  1 | 2 |        3      |        4      |        5      |        6      |        7      |        8      |        9      |       10      |       11
! ---|---|---------------|---------------|---------------|---------------|---------------|---------------|---------------|---------------|-------------
!  1 | 0 | +0.597000E+00 | +0.032609E-07 | -1.000000E+00 | -1.000000E+00 | +0.100000E-03 | +1.500000E-08 | -1.000000E+00 | +1.149901E-04 | +1.000000E+00
!  1 | 1 | +0.599800E+00 | +0.325635E-07 | -1.000000E+00 | -1.000000E+00 | +1.800000E-02 | +5.600000E-08 | +0.000000E+00 | +1.800000E-02 | +3.000000E+00
!  1 | 0 | +0.666000E+00 | +0.000833E-07 | -1.000000E+00 | -1.000000E+00 | +1.000000E-03 | +1.000000E-09 | -1.000000E+00 | +1.000797E-03 | +0.000000E+00
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
