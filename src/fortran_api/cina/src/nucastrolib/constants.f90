MODULE constants

  ! PROGRAM CONSTANTS
  ! Maximum # of parameters
  INTEGER(KIND=4),PARAMETER       :: MAX_A = 105

  ! Max # of allowed Zs
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_Z = 127
  ! Max # of isotopes for a given Z
  INTEGER(KIND=2),PARAMETER,PUBLIC   :: MAX_ISO = 300

  ! Max length of path
  INTEGER(KIND=2),PARAMETER       :: MAX_PATH_LEN = 250


  ! PHYSICS CONSTANTS
  ! Mass of proton in amu
  REAL(KIND=8),PARAMETER          :: MASS_P = 1.007276d0
  ! Mass of neutron in amu
  REAL(KIND=8),PARAMETER          :: MASS_N = 1.008665d0
  ! Mass of electron in amu
  REAL(KIND=8),PARAMETER          :: MASS_E = 0.000549d0
  ! Boltzmann constant in MeV/K
  REAL(KIND=8),PARAMETER          :: BOLTZ = 8.617385d-11

END MODULE constants
