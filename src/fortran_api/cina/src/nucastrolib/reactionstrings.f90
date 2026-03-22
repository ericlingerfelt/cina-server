MODULE reactionstrings
!DESC = This module contains procedures for processing nuclear reaction strings
!DESC = This module does not depend on another module  

! To use this module, FIRST CALL setreactype, then call setreac and setprod
! then you can use the get functions

! Procedures in this module are:
! FUNCTION reactionstrings_ver                 Return CVS revision number for this file
! SUBROUTINE setreactype(r,t)                  Initialize the reaction type
! PRIVATE SUBROUTINE setpart(r,n,z,a)          Initialize the nth particle 
! SUBROUTINE setreac(r,n,z,a)                  Initialize the nth reactant 
! SUBROUTINE setprod(r,n,z,a)                  Initialize the nth product

! SUBROUTINE read_reac_str(r,s,m)              Convert a string into a reactionparticles variable

! FUNCTION getreactype(r)                      Return integer representating reaction type
! FUNCTION getreactype_s(r)                    Return string representating reaction type
! FUNCTION getreac_num(r)                      Return number of reactants
! FUNCTION getprod_num(r)                      Return number of products
! FUNCTION getreac_z(r,n)                      Return Z of nth reactant
! FUNCTION getreac_n(r,n)                      Return N of nth reactant
! FUNCTION getreac_a(r,n)                      Return A of nth reactant
! FUNCTION getprod_z(r,n)                      Return Z of nth product
! FUNCTION getprod_n(r,n)                      Return N of nth product
! FUNCTION getprod_a(r,n)                      Return A of nth product
! PRIVATE FUNCTION getpart_s(r,n,o,a)          Return string like Be7 or 7Be for the nth particle
! FUNCTION getreac_s(r,n,o,a)                  Return string like Be7 or 7Be for the nth reactant
! FUNCTION getprod_s(r,n,o,a)                  Return string like Be7 or 7Be for the nth product
! PRIVATE SUBROUTINE sort_reac(r,sortr,sortp)  Sort reaction by increasing Z
! FUNCTION getreac_str(r,f)                    Return string representating whole reaction using format f
! SUBROUTINE validreaction(r,o,m)              o is zero if valid reaction, m is reason for invalid reaction
! PRIVATE FUNCTION findsymbol(s)               Return atomic number of symbol in string
! PRIVATE SUBROUTINE read_int(s,n,cur)         Return the first integer in a string
! SUBROUTINE str2za(s,z,a,cur)                 Return Z and A for first particle in a string
! FUNCTION getinverse(r)                       Return an r for the inverse reaction
! FUNCTION same_reaction(r1,r2)                Return .TRUE. if r1 == r2
! FUNCTION decay(r)                            Return .TRUE. if r is a decay reaction
! FUNCTION get_iso_str(z,a)                    Return string like 7Be for a z and a

! By default all procedures and global variables are private
  PRIVATE
  PUBLIC   :: reactionstrings_ver,setreactype,setreac,setprod,getinverse
  PUBLIC   :: getreactype,getreactype_s,getreac_num,getprod_num,str2za
  PUBLIC   :: getreac_z,getreac_n,getreac_a,getprod_z,getprod_n,getprod_a
  PUBLIC   :: getreac_s,getprod_s,getreac_str,validreaction,read_reac_str
  PUBLIC   :: same_reaction,decay,get_iso_str,get_qvalue
  PUBLIC   :: sort_r,cmp_r,cmp_part,sort_reac,sort_prod

  TYPE,PUBLIC                     :: reactionparticles
! Functions outside this module CAN NOT access the variables below.
! This is to prevent anything but this module from modifing the values
! That way this module can ensure these variables remain in a valid state
! To access these values outside this module, use functions in this module
     PRIVATE
! 1 is Decay, 2 is a-->b+c, 3 is a-->b+c+d, 4 is a+b-->c, 5 is a+b-->c+d, 
! 6 is a+b-->c+d+e, 7 is a+b-->c+d+e+f, 8 is a+b+c-->d(+e), 9 is other
! Use the getreactype or getreactype_s functions to obtain the reac_type. 
! You CAN NOT use variable%reac_type
     INTEGER(KIND=4)              :: reac_type = -1     ! Initialize to invalid values
! Use the getreac_num function to obtain this value
     INTEGER(KIND=4)              :: reac_num = -1
! Use the getprod_num function to obtain this value
     INTEGER(KIND=4)              :: prod_num = -1
! Use the getreac_z or getprod_z function to obtain these values
     INTEGER(KIND=4)              :: part_z(8) = -1
! Use the getreac_n or getprod_n function to obtain these values
     INTEGER(KIND=4)              :: part_n(8) = -1
! Used for identifing decays
     CHARACTER(LEN=4)             :: type_str = ''
! Used for Al*6 and Al-6
     CHARACTER(LEN=10)            :: special_num(8) = ''
  END TYPE reactionparticles

  CHARACTER(LEN=13),PARAMETER,PUBLIC :: elements(120) = (/     &
       'Hydrogen     ','Helium       ','Lithium      ','Beryllium    ','Boron        ', &
       'Carbon       ','Nitrogen     ','Oxygen       ','Fluorine     ','Neon         ', &
       'Sodium       ','Magnesium    ','Aluminum     ','Silicon      ','Phosphorus   ', &
       'Sulfur       ','Chlorine     ','Argon        ','Potassium    ','Calcium      ', &
       'Scandium     ','Titanium     ','Vanadium     ','Chromium     ','Manganese    ', &
       'Iron         ','Cobalt       ','Nickel       ','Copper       ','Zinc         ', &
       'Gallium      ','Germanium    ','Arsenic      ','Selenium     ','Bromine      ', &
       'Krypton      ','Rubidium     ','Strontium    ','Yttrium      ','Zirconium    ', &
       'Niobium      ','Molybdenum   ','Technetium   ','Ruthenium    ','Rhodium      ', &
       'Palladium    ','Silver       ','Cadmium      ','Indium       ','Tin          ', &
       'Antimony     ','Tellurium    ','Iodine       ','Xenon        ','Cesium       ', &
       'Barium       ','Lanthanum    ','Cerium       ','Praseodymium ','Neodymium    ', &
       'Promethium   ','Samarium     ','Europium     ','Gadolinium   ','Terbium      ', &
       'Dysprosium   ','Holmium      ','Erbium       ','Thulium      ','Ytterbium    ', &
       'Lutetium     ','Hafnium      ','Tantalum     ','Wolfram      ','Rhenium      ', &
       'Osmium       ','Iridium      ','Platinum     ','Gold         ','Mercury      ', &
       'Thallium     ','Lead         ','Bismuth      ','Polonium     ','Astatine     ', &
       'Radon        ','Francium     ','Radium       ','Actinium     ','Thorium      ', &
       'Protactinium ','Uranium      ','Neptunium    ','Plutonium    ','Americium    ', &
       'Curium       ','Berkelium    ','Californium  ','Einsteinium  ','Fermium      ', &
       'Mendelevium  ','Nobelium     ','Lawrencium   ','Rutherfordium','Dubnium      ', &
       'Seaborgium   ','Bohrium      ','Hassium      ','Meitnerium   ','Darmstadtium ', &
       'Roentgenium  ','Copernicium  ','Nihonium     ','Flerovium    ','Moscovium    ', &
       'Livermorium  ','Tennesine    ','Oganesson    ','             ','             '  /)

  CHARACTER(LEN=3),PARAMETER,PUBLIC :: symbols(120) = (/     &
       'H  ','He ','Li ','Be ','B  ','C  ','N  ','O  ','F  ','Ne ', &  !   1- 10
       'Na ','Mg ','Al ','Si ','P  ','S  ','Cl ','Ar ','K  ','Ca ', &  !  11- 20
       'Sc ','Ti ','V  ','Cr ','Mn ','Fe ','Co ','Ni ','Cu ','Zn ', &  !  21- 30
       'Ga ','Ge ','As ','Se ','Br ','Kr ','Rb ','Sr ','Y  ','Zr ', &  !  31- 40
       'Nb ','Mo ','Tc ','Ru ','Rh ','Pd ','Ag ','Cd ','In ','Sn ', &  !  41- 50
       'Sb ','Te ','I  ','Xe ','Cs ','Ba ','La ','Ce ','Pr ','Nd ', &  !  51- 60
       'Pm ','Sm ','Eu ','Gd ','Tb ','Dy ','Ho ','Er ','Tm ','Yb ', &  !  61- 70
       'Lu ','Hf ','Ta ','W  ','Re ','Os ','Ir ','Pt ','Au ','Hg ', &  !  71- 80
       'Tl ','Pb ','Bi ','Po ','At ','Rn ','Fr ','Ra ','Ac ','Th ', &  !  81- 90
       'Pa ','U  ','Np ','Pu ','Am ','Cm ','Bk ','Cf ','Es ','Fm ', &  !  91-100
       'Md ','No ','Lr ','Rf ','Db ','Sg ','Bh ','Hs ','Mt ','Ds ', &  ! 101-110
       'Rg ','Cn ','Nh ','Fl ','Mc ','Lv ','Ts ','Og ','Uue','Ubn'  /) ! 111-120

CONTAINS
!---------------------------------------------------------------------
  FUNCTION reactionstrings_ver
!PURPOSE = Return the cvs revision number for this file
!STATUS = 
    IMPLICIT NONE
    CHARACTER(LEN=10)             :: reactionstrings_ver
    CHARACTER(LEN=20),PARAMETER   :: REACTIONSTR_VERSION = '$Revision: 1.1.1.1 $'

    reactionstrings_ver = REACTIONSTR_VERSION(12:LEN_TRIM(REACTIONSTR_VERSION)-2)

  END FUNCTION reactionstrings_ver

!---------------------------------------------------------------------
  SUBROUTINE setreactype(r,t,type_str)
!PURPOSE = Set the reaction type in r to the value in t
!STATUS = Complete and tested except for decay reactions
!DESC = r is a reactionparticles structure
!DESC = t is an INT(KIND=4)
!DESC = Custom reactions (t == 9) are not supported by this function
!DESC = Use a read function (read_reac_str) to use custom reactions
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(OUT) :: r
    INTEGER(KIND=4),INTENT(IN)    :: t
    CHARACTER(LEN=*),INTENT(IN)   :: type_str
    OPTIONAL                      :: type_str

    If (PRESENT(type_str)) THEN
       r%type_str = type_str
    ELSE
       r%type_str = ''
    END If

    r%reac_type = t
    IF ((r%reac_type < 1) .OR. (r%reac_type > 8)) r%reac_type = -1

    SELECT CASE (r%reac_type)
    CASE (1)                                     ! Decay
       r%reac_num = 1    !! Replace with correct values
       r%prod_num = 1    !! Replace with correct values
    CASE (2)
       r%reac_num = 1
       r%prod_num = 2
    CASE (3)
       r%reac_num = 1
       r%prod_num = 3
    CASE (4)
       r%reac_num = 2
       r%prod_num = 1
    CASE (5)
       r%reac_num = 2
       r%prod_num = 2
    CASE (6)
       r%reac_num = 2
       r%prod_num = 3
    CASE (7)
       r%reac_num = 2
       r%prod_num = 4
    CASE (8)
       r%reac_num = 3
       r%prod_num = 1     !! Reac type 8 may have 1 or 2 products
    CASE (9)
       r%reac_num = -1    !! Set to invalid values for now, fix later
       r%prod_num = -1    !! Set to invalid values for now, fix later
    END SELECT

! Now that reac_type has changed, initialize all particles to invalid
    r%part_z = -1
    r%part_n = -1
    
  END SUBROUTINE setreactype

!---------------------------------------------------------------------
  SUBROUTINE setpart(r,n,z,a,special_num)
!PURPOSE = Set the nth particle using values z and a
!STATUS = 
!CAUTION = The reaction type must be specified before using this function
!DESC = r is a reactionparticles structure
!DESC = n, z, and a are INTEGER(KIND=4)
!DESC = Represent a gamma particle with z=-2, a=anything
!DESC = Represent a neutrino particle with z=-3, a=anything
!DESC = Represent an electron with z=-4, a=anything
!DESC = Any other negative values are invalid
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(INOUT) :: r
    INTEGER(KIND=4),INTENT(IN)    :: n,z,a
    CHARACTER(LEN=10)             :: special_num
    OPTIONAL                      :: special_num

! Return if reaction type is not specified
    IF (r%reac_type == -1) RETURN

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%reac_num+r%prod_num)) RETURN

    r%part_z(n) = z
    IF (r%part_z(n) < -4) r%part_z(n) = -1

    r%part_n(n) = -1
    IF ((z >= 0) .AND. (a >= z)) r%part_n(n) = a - z

! If other particle (gamma, electron, neutrino) make n same as z
    IF (z < 0) r%part_n(n) = r%part_z(n)

! If the neutrons are invalid, make protons invalid
    IF ((z >= 0) .AND. (r%part_n(n) == -1)) r%part_z(n) = -1

! If no protons or neutrons make particle invalid
    IF ((z == 0) .AND. (a == 0)) r%part_z(n) = -1

    r%special_num(n) = special_num

  END SUBROUTINE setpart

!---------------------------------------------------------------------
  SUBROUTINE setreac(r,n,z,a,special_num)
!PURPOSE = Set the nth reactant using values z and a
!STATUS = 
!CAUTION = The reaction type must be specified before using this function
!DESC = r is a reactionparticles structure
!DESC = n, z, and a are INTEGER(KIND=4)
!DESC = Represent a gamma particle with z=-2, a=anything
!DESC = Represent a neutrino particle with z=-3, a=anything
!DESC = Represent an electron with z=-4, a=anything
!DESC = Any other negative values are invalid
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(INOUT) :: r
    INTEGER(KIND=4),INTENT(IN)    :: n,z,a
    CHARACTER(LEN=10)             :: special_num
    OPTIONAL                      :: special_num

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%reac_num)) RETURN

    IF (PRESENT(special_num)) THEN
       CALL setpart(r,n,z,a,special_num)
    ELSE
       CALL setpart(r,n,z,a)
    END IF

  END SUBROUTINE setreac

!---------------------------------------------------------------------
  SUBROUTINE setprod(r,n,z,a,special_num)
!PURPOSE = Set the nth product using values z and a
!STATUS = 
!CAUTION = The reaction type must be specified before using this function
!DESC = r is a reactionparticles structure
!DESC = n, z, and a are INTEGER(KIND=4)
!DESC = Represent a gamma particle with z=-2, a=anything
!DESC = Represent a neutrino particle with z=-3, a=anything
!DESC = Represent an electron with z=-4, a=anything
!DESC = Any other negative values are invalid
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(INOUT) :: r
    INTEGER(KIND=4),INTENT(IN)    :: n,z,a
    CHARACTER(LEN=10)             :: special_num
    OPTIONAL                      :: special_num

! Reaction type 8 may have 1 or 2 products
    IF ((r%reac_type == 8) .AND. (n == 2) .AND. (r%prod_num == 1)) r%prod_num = 2

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%prod_num)) RETURN

    IF (PRESENT(special_num)) THEN
       CALL setpart(r,n + r%reac_num,z,a,special_num)
    ELSE
       CALL setpart(r,n + r%reac_num,z,a)
    END IF

  END SUBROUTINE setprod

!---------------------------------------------------------------------
  SUBROUTINE read_reac_str(r,s,m,type_str)
!PURPOSE = Convert a string into a reactionparticles variable
!STATUS = 
!DESC = r is a reactionparticles structure
!DESC = s is the string containing the reaction
!DESC = m is a string of length 100 containing any warning or error messages
    IMPLICIT NONE
    CHARACTER(LEN=10),PARAMETER   :: digit = '0123456789'
    TYPE(reactionparticles),INTENT(OUT) :: r
    CHARACTER(LEN=*),INTENT(IN)   :: s,type_str
    CHARACTER(LEN=*),INTENT(OUT)  :: m
    CHARACTER(LEN=2)              :: sym
    CHARACTER(LEN=1)              :: c
    CHARACTER(LEN=10)             :: special_num
    INTEGER(KIND=4)               :: len_s,i,cur,z,a,j,fmt,g
    LOGICAL(KIND=1)               :: loop
    OPTIONAL                      :: type_str

    len_s = LEN_TRIM(s)
    m = ''
    cur = 1
    c = s(cur:cur)

    loop = .TRUE.
    i = 0   ! i is index into r%part_z
    fmt = 0 ! format of string, 0 is unknown, 1 is A->C+D, 2 is A(B,C D)E
    DO WHILE (loop)
! Ignore any spaces
       DO WHILE (s(cur:cur) == ' ')
          cur = cur + 1
          IF (cur > len_s) THEN
             m = 'End reached before "->" or "," was found'
             RETURN
          END IF
       END DO

       j = cur   ! Remember cursor before looking for a particle in case of an error

! Try to read in particle
       CALL str2za(s,z,a,cur,special_num)

!print '(2A,3(A,I0))','s="',TRIM(s),'", z=',z,' a=',a,' cur=',cur

! If a particle was not found, write error, and return
       IF (z == -1) THEN
          m = 'Looking for reactant, found "' // TRIM(s(j:)) // '"'
! If unable to read in at least one reactant, exit with error
          IF (i == 0) THEN
             m = 'Could not find 1st reactant'
          END IF
          RETURN
       END IF

! Ignore any spaces
       DO WHILE (s(cur:cur) == ' ')
          cur = cur + 1
          IF (cur > len_s) THEN
             m = 'End reached before "->" or "," was found'
             RETURN
          END IF
       END DO

! Found particle, record it
       i = i + 1
       r%part_z(i) = z
       IF (z >= 0) THEN
          r%part_n(i) = a - z
       ELSE
          r%part_n(i) = z
       END IF
       r%special_num(i) = special_num

       c = s(cur:cur)

! Look for plus sign
       IF ((c == '+') .AND. (fmt <= 1)) THEN
          fmt = 1
! Advance cursor past plus sign
          cur = cur + 1
          IF (cur > len_s) THEN
             m = 'End reached just after "+" was found'
             RETURN
          END IF
! Loop and look for another particle

! Look for minus sign
       ELSE IF ((c == '-') .AND. (fmt <= 1)) THEN
          fmt = 1
! Ignore any remaining minus signs
          DO WHILE (s(cur:cur) == '-')
             cur = cur + 1
             IF (cur > len_s) THEN
                m = 'End reached before ">" was found'
                RETURN
             END IF
          END DO

! Look for greater than symbol
          IF (s(cur:cur) == '>') THEN
             cur = cur + 1
             r%reac_num = i
             IF (cur > len_s) THEN
                m = 'End reached just after ">" was found'
                RETURN
             END IF
             loop = .FALSE.
          ELSE
             m = 'Did not find ">" after "-"'
             RETURN
          END IF

       ELSE IF ((c == '(') .AND. (fmt /= 1)) THEN
          fmt = 2
! Advance cursor past "("
          cur = cur + 1
          IF (cur > len_s) THEN
             m = 'End reached just after "(" was found'
             RETURN
          END IF
! Loop and look for another particle

       ELSE IF ((c == ',') .AND. (fmt == 2)) THEN
! Advance cursor past ","
          cur = cur + 1
          IF (cur > len_s) THEN
             m = 'End reached just after "," was found'
             RETURN
          END IF
          r%reac_num = i
          loop = .FALSE.
       END IF
    END DO
!print *,r%reac_num,r%part_z(1),r%part_n(1),r%part_z(2),r%part_n(2)

! Ignore any spaces
    DO WHILE (s(cur:cur) == ' ')
       cur = cur + 1
       IF (cur > len_s) THEN
          m = 'End reached before product was found'
          RETURN
       END IF
    END DO

    g = r%reac_num

! Read in products
    loop = .TRUE.
    i = 0   ! i is index into r%part_z

    DO WHILE (loop)
       j = cur   ! Remember cursor before looking for a particle in case of an error

! Try to read in particle
       CALL str2za(s,z,a,cur,special_num)

!print '(2A,3(A,I0))','P s="',TRIM(s),'", z=',z,' a=',a,' cur=',cur

! If a particle was not found, write error, and return
       IF (z == -1) THEN
          m = 'Looking for product, found "' // TRIM(s(j:)) // '"'
! If unable to read in at least one product, exit with error
          IF (i == 0) THEN
             m = 'Could not find 1st product'
          END IF
          RETURN
       END IF

! Ignore any spaces
       DO WHILE (s(cur:cur) == ' ')
          cur = cur + 1
          IF (cur > len_s) THEN
             loop = .FALSE.
          END IF
       END DO

! Found particle, record it
       i = i + 1
       r%part_z(i+g) = z
       IF (z >= 0) THEN
          r%part_n(i+g) = a - z
       ELSE
          r%part_n(i+g) = z
       END IF
       r%special_num(i+g) = special_num

       c = s(cur:cur)

! Look for plus sign
       IF ((c == '+') .AND. (fmt == 1)) THEN
          fmt = 1
! Advance cursor past plus sign
          cur = cur + 1
          IF (cur > len_s) THEN
             loop = .FALSE.
          END IF
! Loop and look for another particle

       ELSE IF ((c == ')') .AND. (fmt == 2)) THEN
          fmt = 2
! Advance cursor past ")"
          cur = cur + 1
          IF (cur > len_s) THEN
             loop = .FALSE.
          END IF
! Loop and look for another particle

       END IF

! Ignore any spaces
       DO WHILE (s(cur:cur) == ' ')
          cur = cur + 1
          IF (cur > len_s) THEN
             loop = .FALSE.
          END IF
       END DO

! If at end, stop loop
       IF (cur > len_s) THEN
          loop = .FALSE.
       END IF
    END DO

! Make sure a product was found
    IF (i == 0) THEN
       m = 'Did not find a product'
       RETURN
    END IF

    r%prod_num = i
    r%reac_type = -1

! Find reaction type (won't work for decay reactions)
    IF ((g == 1) .AND. (i == 1)) THEN
       r%reac_type = 1
    ELSE IF ((g == 1) .AND. (i == 2)) THEN
       r%reac_type = 2
    ELSE IF ((g == 1) .AND. (i == 3)) THEN
       r%reac_type = 3
    ELSE IF ((g == 2) .AND. (i == 1)) THEN
       r%reac_type = 4
    ELSE IF ((g == 2) .AND. (i == 2)) THEN
       r%reac_type = 5
    ELSE IF ((g == 2) .AND. (i == 3)) THEN
       r%reac_type = 6
    ELSE IF ((g == 2) .AND. (i == 4)) THEN
       r%reac_type = 7
    ELSE IF ((g == 3) .AND. ((i == 2) .OR. (i == 1))) THEN
       r%reac_type = 8
    ELSE
       r%reac_type = 9
    END IF

! May try to determine decays later
    If (PRESENT(type_str)) THEN
       r%type_str = type_str
    ELSE
       r%type_str = ''
    END If

!print *,r%prod_num,r%part_z(1+g),r%part_n(1+g),r%part_z(2+g),r%part_n(2+g)
!print *,'rt ',r%reac_type

  END SUBROUTINE read_reac_str

!---------------------------------------------------------------------
  FUNCTION getreactype(r,type_str)
!PURPOSE = Return the reaction type of a reactionparticles structure
!STATUS = Complete and tested
!RETURNS = An INTEGER(KIND=4) indicating the reaction type if valid, -1 otherwise
!DESC = r is a reactionparticles structure
!DESC = Use the getreactype_s function to obtain a string representation of the type
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    CHARACTER(LEN=*),INTENT(OUT)       :: type_str
    INTEGER(KIND=4)                    :: getreactype
    OPTIONAL                           :: type_str

    getreactype = r%reac_type
    IF (PRESENT(type_str)) type_str = r%type_str

  END FUNCTION getreactype

!---------------------------------------------------------------------
  FUNCTION getreactype_s(r,type_str)
!PURPOSE = Return the reaction type of a reactionparticles structure
!STATUS = Complete and tested
!RETURNS = A string of length 25 indicating the reaction type if valid, '' otherwise
!DESC = r is a reactionparticles structure
!DESC = Use the getreactype function to obtain an integer representation of the type
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    CHARACTER(LEN=*),INTENT(OUT)       :: type_str
    CHARACTER(LEN=25)             :: getreactype_s
    OPTIONAL                           :: type_str

    SELECT CASE (r%reac_type)
    CASE (1)
       getreactype_s = 'a->b'
    CASE (2)
       getreactype_s = 'a->b+c'
    CASE (3)
       getreactype_s = 'a->b+c+d'
    CASE (4)
       getreactype_s = 'a+b->c'
    CASE (5)
       getreactype_s = 'a+b->c+d'
    CASE (6)
       getreactype_s = 'a+b->c+d+e'
    CASE (7)
       getreactype_s = 'a+b->c+d+e+f'
    CASE (8)
       getreactype_s = 'a+b+c->d(+e)'
    CASE (9)
       getreactype_s = 'Custom'  !! Could make this more informative
    CASE DEFAULT
       getreactype_s = 'Invalid'
    END SELECT

    IF (PRESENT(type_str)) type_str = r%type_str

  END FUNCTION getreactype_s

!---------------------------------------------------------------------
  FUNCTION getreac_num(r)
!PURPOSE = Return the number of reactants using the reaction type of a reactionparticles structure
!STATUS = Complete and tested
!RETURNS = INTEGER(KIND=4) of the number of reactants if valid, 0 otherwise
!DESC = r is a reactionparticles structure
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4)               :: getreac_num

    getreac_num = r%reac_num

  END FUNCTION getreac_num

!---------------------------------------------------------------------
  FUNCTION getprod_num(r)
!PURPOSE = Return the number of products using the reaction type of a reactionparticles structure
!STATUS = Complete and tested
!RETURNS = INTEGER(KIND=4) of the number of products if valid, 0 otherwise
!DESC = r is a reactionparticles structure
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4)               :: getprod_num

    getprod_num = r%prod_num

  END FUNCTION getprod_num

!---------------------------------------------------------------------
  FUNCTION getreac_z(r,n)
!PURPOSE = Return the number of protons of the nth reactant from a reactionparticles structure
!STATUS = 
!RETURNS = An INTEGER(KIND=4) indicating Z if valid, -1 otherwise
!DESC = r is a reactionparticles structure
!DESC = n is an INTEGER(KIND=4) variable.
!DESC = In invalid particle returns -1
!DESC = A gamma particle is represented with -2
!DESC = A neutrino is represented with -3
!DESC = A electron is represented with -4
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)         :: n
    INTEGER(KIND=4)                    :: getreac_z

    getreac_z = -1

! Return if reaction type is not specified
    IF (r%reac_type == -1) RETURN

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%reac_num)) RETURN

    getreac_z = r%part_z(n)

  END FUNCTION getreac_z

!---------------------------------------------------------------------
  FUNCTION getreac_n(r,n)
!PURPOSE = Return the number of neutrons of the nth reactant from a reactionparticles structure
!STATUS = 
!RETURNS = An INTEGER(KIND=4) indicating N if valid, -1 otherwise
!DESC = r is a reactionparticles structure
!DESC = n is an INTEGER(KIND=4) variable.
!DESC = In invalid particle returns -1
!DESC = A gamma particle is represented with -2
!DESC = A neutrino is represented with -3
!DESC = A electron is represented with -4
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)         :: n
    INTEGER(KIND=4)                    :: getreac_n

    getreac_n = -1

! Return if reaction type is not specified
    IF (r%reac_type == -1) RETURN

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%reac_num)) RETURN

    getreac_n = r%part_n(n)

  END FUNCTION getreac_n

!---------------------------------------------------------------------
  FUNCTION getreac_a(r,n)
!PURPOSE = Return the mass of the nth reactant from a reactionparticles structure
!STATUS = 
!RETURNS = An INTEGER(KIND=4) indicating N if valid, -1 otherwise
!DESC = r is a reactionparticles structure
!DESC = n is an INTEGER(KIND=4) variable.
!DESC = In invalid particle returns -1
!DESC = A gamma particle is represented with -2
!DESC = A neutrino is represented with -3
!DESC = A electron is represented with -4
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)         :: n
    INTEGER(KIND=4)                    :: getreac_a

    getreac_a = -1

! Return if reaction type is not specified
    IF (r%reac_type == -1) RETURN

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%reac_num)) RETURN

    IF (r%part_z(n) >= 0) THEN
       getreac_a = r%part_z(n) + r%part_n(n)
    ELSE
! If a special particle, return it only
       getreac_a = r%part_z(n)
    END IF

  END FUNCTION getreac_a

!---------------------------------------------------------------------
  FUNCTION getprod_z(r,n)
!PURPOSE = Return the number of protons of the nth product from a reactionparticles structure
!STATUS = 
!RETURNS = An INTEGER(KIND=4) indicating Z if valid, -1 otherwise
!DESC = r is a reactionparticles structure
!DESC = n is an INTEGER(KIND=4) variable.
!DESC = In invalid particle returns -1
!DESC = A gamma particle is represented with -2
!DESC = A neutrino is represented with -3
!DESC = A electron is represented with -4
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)         :: n
    INTEGER(KIND=4)                    :: getprod_z

    getprod_z = -1

! Return if reaction type is not specified
    IF (r%reac_type == -1) RETURN

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%prod_num)) RETURN

    getprod_z = r%part_z(n + r%reac_num)

  END FUNCTION getprod_z

!---------------------------------------------------------------------
  FUNCTION getprod_n(r,n)
!PURPOSE = Return the number of neutrons of the nth product from a reactionparticles structure
!STATUS = 
!RETURNS = An INTEGER(KIND=4) indicating N if valid, -1 otherwise
!DESC = r is a reactionparticles structure
!DESC = n is an INTEGER(KIND=4) variable.
!DESC = In invalid particle returns -1
!DESC = A gamma particle is represented with -2
!DESC = A neutrino is represented with -3
!DESC = A electron is represented with -4
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)         :: n
    INTEGER(KIND=4)                    :: getprod_n

    getprod_n = -1

! Return if reaction type is not specified
    IF (r%reac_type == -1) RETURN

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%prod_num)) RETURN

    getprod_n = r%part_n(n + r%reac_num)

  END FUNCTION getprod_n

!---------------------------------------------------------------------
  FUNCTION getprod_a(r,n)
!PURPOSE = Return the mass of the nth product from a reactionparticles structure
!STATUS = 
!RETURNS = An INTEGER(KIND=4) indicating N if valid, -1 otherwise
!DESC = r is a reactionparticles structure
!DESC = n is an INTEGER(KIND=4) variable.
!DESC = In invalid particle returns -1
!DESC = A gamma particle is represented with -2
!DESC = A neutrino is represented with -3
!DESC = A electron is represented with -4
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)         :: n
    INTEGER(KIND=4)                    :: getprod_a,g

    getprod_a = -1

! Return if reaction type is not specified
    IF (r%reac_type == -1) RETURN

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%prod_num)) RETURN

    g = r%reac_num

    IF (r%part_z(n+g) >= 0) THEN
       getprod_a = r%part_z(n+g) + r%part_n(n+g)
    ELSE
! If a special particle, return it only
       getprod_a = r%part_z(n+g)
    END IF

  END FUNCTION getprod_a

!---------------------------------------------------------------------
  FUNCTION getpart_s(r,n,o,a,in)
!PURPOSE = Get a string (like Be7, 7Be, or 4,7) representating the nth particle
!STATUS = 
!RETURNS = String of 10 Characters.  Use the TRIM function to shorten it as much as possible
!DESC = r is a reactionparticles structure
!DESC = If o (INT(KIND=4)) is zero or omitted the mass precedes the symbol
!DESC = If a (INT(KIND=4)) is zero or omitted alpha particles are represented as 4He otherwise by 'a'
!DESC = If in (INT(KIND=4)) is nonzero, integer format returned (4,7)
!DESC = protons are represented by 'p', neutrons by 'n', and gamma particles by 'g'
!DESC = deuterons are represented by 'd', tritium is represented by 't'
!DESC = neutrinos are represented by 'nu', electrons are represented by 'e-'
!DESC = Unknown elements return a string with Z followed by a comma followed by its mass
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)    :: n,o,a,in
    CHARACTER(LEN=10)             :: getpart_s
    OPTIONAL                      :: o,a,in
    INTEGER(KIND=4)               :: i,order = 0,alpha = 0,Z,Neu
    CHARACTER(LEN=10)              :: s

    getpart_s = 'Error'

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%reac_num+r%prod_num)) RETURN

    IF (PRESENT(o)) order = o
    IF (PRESENT(a)) alpha = a

! Get N and string for symbol
    Z = r%part_z(n)    ! Atomic number
    Neu = r%part_n(n)  ! Neutrons
    s = ''

    IF (PRESENT(in)) THEN
       IF (in /= 0) THEN
          WRITE(getpart_s,'(I0,A,I0)') Z,',',Z+Neu
          RETURN
       END IF
    END IF

! Check to make sure Z is in symbols
    IF ((Z >= 0) .AND. (Z <= UBOUND(symbols,1))) THEN
! Check for proton
       IF ((Z == 1) .AND. (Neu == 0)) s = 'p'
! Check for neutron
       IF ((Z == 0) .AND. (Neu == 1)) s = 'n'
! Check for deuteron
       IF ((Z == 1) .AND. (Neu == 1)) s = 'd'
! Check for tritium
       IF ((Z == 1) .AND. (Neu == 2)) s = 't'
! Check for alpha particle
       IF ((alpha /= 0) .AND. (Z == 2) .AND. (Neu == 2)) s = 'a'
       IF (LEN_TRIM(s) > 0) THEN
          getpart_s = s
          RETURN
       END IF
       s = TRIM(symbols(Z))
    END IF
    IF (LEN_TRIM(s) == 0) THEN
! Check for gamma particle
       IF (Z == -2) s = 'g'
! Check for neutrino
       IF (Z == -3) s = 'nu'
! Check for electron
       IF (Z == -4) s = 'e-'
! If no match so far, make string Z,A
       IF (LEN_TRIM(s) == 0) THEN
          IF (Z >= 0) THEN
             WRITE(s,'(I0,A,I0)') Z,',',Z+Neu
          ELSE
             s = '?'
          END IF
       END IF
       getpart_s = s
       RETURN
    END IF

    IF (r%special_num(n) == '') THEN
       IF (order == 0) THEN                         ! Mass first
          WRITE(getpart_s,'(I0,A)') Neu+Z,TRIM(s)
       ELSE                                         ! Symbol first
          WRITE(getpart_s,'(A,I0)') TRIM(s),Z+Neu
       END IF
    ELSE
       IF (order == 0) THEN                         ! Mass first
          getpart_s = TRIM(r%special_num(n)) // TRIM(s)
       ELSE                                         ! Symbol first
          getpart_s = TRIM(s) // TRIM(r%special_num(n))
       END IF
    END IF
  END FUNCTION getpart_s

!---------------------------------------------------------------------
  FUNCTION getreac_s(r,n,o,a,i)
!PURPOSE = Get a string (like Be7, 7Be, or 4,7) representating the nth reactant
!STATUS = 
!RETURNS = String of 10 Characters.  Use the TRIM function to shorten it as much as possible
!DESC = r is a reactionparticles structure
!DESC = If o (INT(KIND=4)) is zero or omitted the mass precedes the symbol
!DESC = If a (INT(KIND=4)) is zero or omitted alpha particles are represented as 4He otherwise by 'a'
!DESC = If i (INT(KIND=4)) is nonzero, integer format returned (4,7)
!DESC = protons are represented by 'p', neutrons by 'n', and gamma particles by 'g'
!DESC = deuterons are represented by 'd', tritium is represented by 't'
!DESC = neutrinos are represented by 'nu', electrons are represented by 'e-'
!DESC = Unknown elements return a string with Z followed by a comma followed by its mass
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)    :: n,o,a,i
    CHARACTER(LEN=10)             :: getreac_s
    INTEGER(KIND=4)               :: o2,a2,i2
    OPTIONAL                      :: o,a,i

    getreac_s = 'Error'
    o2=0
    a2=0
    i2=0
    
    IF(PRESENT(o)) o2=o
    IF(PRESENT(a)) a2=a
    IF(PRESENT(i)) i2=i

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%reac_num)) RETURN
       
    getreac_s = getpart_s(r,n,o2,a2,i2)

  END FUNCTION getreac_s

!---------------------------------------------------------------------
  FUNCTION getprod_s(r,n,o,a,i)
!PURPOSE = Get a string (like Be7, 7Be, or 4,7) representating the nth product
!STATUS = 
!RETURNS = String of 10 Characters.  Use the TRIM function to shorten it as much as possible
!DESC = r is a reactionparticles structure
!DESC = If o (INT(KIND=4)) is zero or omitted the number of neutrons precedes the symbol
!DESC = If a (INT(KIND=4)) is zero or omitted alpha particles are represented as 4He otherwise by 'a'
!DESC = If i (INT(KIND=4)) is nonzero, integer format returned (4,7)
!DESC = protons are represented by 'p', neutrons by 'n', and gamma particles by 'g'
!DESC = deuterons are represented by 'd', tritium is represented by 't'
!DESC = neutrinos are represented by 'nu', electrons are represented by 'e-'
!DESC = Unknown elements return a string with Z followed by a comma followed by its mass
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)    :: n,o,a,i
    CHARACTER(LEN=10)             :: getprod_s
    INTEGER(KIND=4)               :: o2,a2,i2
    OPTIONAL                      :: o,a,i

    getprod_s = 'Error'
    o2=0
    a2=0
    i2=0

    IF(PRESENT(o)) o2=o
    IF(PRESENT(a)) a2=a
    IF(PRESENT(i)) i2=i

! Return if n is out of range
    IF ((n < 1) .OR. (n > r%prod_num)) RETURN
       
    getprod_s = getpart_s(r,n + r%reac_num, o2,a2,i2)

  END FUNCTION getprod_s

!---------------------------------------------------------------------
  SUBROUTINE sort_part(r,sortr,sortp)
!PURPOSE = Sort reactants and products by increasing Z, then by N
!STATUS = 
!DESC = r is a reactionparticles structure
!DESC = sortr and sortp are INT(KIND=4) arrays of length rp_num
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(OUT)   :: sortr(10),sortp(10)
    INTEGER(KIND=4)               :: i,j,m,mi,g
    LOGICAL(KIND=1)               :: mask(10)

! Sort reactants
    mask = .TRUE.
    DO i = 1, r%reac_num
       m = HUGE(m)  
       mi = -1
       DO j = 1, r%reac_num
!print *,'j: ',j,mi,m,mask(1:r%reac_num)
          IF ((r%part_z(j) < m) .AND. mask(j)) THEN
             m = r%part_z(j)                     ! m stores the minimum z found
             mi = j                              ! mi stores index to minimum z found
          ELSE IF ((r%part_z(j) == m) .AND. mask(j)) THEN
! Check which one has lower neutrons
             IF (r%part_n(j) < r%part_n(mi)) THEN
                m = r%part_z(j)                  ! m stores the minimum z found
                mi = j                           ! mi stores index to minimum z found
             END IF
          END IF
       END DO
       mask(mi) = .FALSE.                        ! Flag this reactant as sorted
       sortr(i) = mi
!print *,'i: ',i,mi,m,mask(1:r%reac_num)
    END DO

    g = r%reac_num

! Sort products
    mask = .TRUE.
    DO i = 1, r%prod_num
       m = HUGE(m)  
       mi = -1
       DO j = 1, r%prod_num
          IF ((r%part_z(j+g) < m) .AND. mask(j)) THEN
             m = r%part_z(j+g)                   ! m stores the minimum z found
             mi = j                              ! mi stores index to minimum z found
          ELSE IF ((r%part_z(j+g) == m) .AND. mask(j)) THEN
! Check which one has lower neutrons
             IF (r%part_n(j+g) < r%part_n(mi+g)) THEN
                m = r%part_z(j+g)                ! m stores the minimum z found
                mi = j                           ! mi stores index to minimum z found
             END IF
          END IF
       END DO
       mask(mi) = .FALSE.                        ! Flag this reactant as sorted
       sortp(i) = mi
    END DO

  END SUBROUTINE sort_part

!---------------------------------------------------------------------
  FUNCTION getreac_str(r,f)
!PURPOSE = Return a string representating whole reaction using format f
!STATUS = 
!RETURNS = String of length 60.  Use TRIM to remove extra blanks at end of string
!DESC = r is a reactionparticles structure
!DESC = f is an INT(KIND=4) for a certain format.  If omitted, REACLIB format is used.
!DESC = If f == 0, REACLIB format is used
!DESC = If f == 1, REACLIB format is used with numbers preceeding symbol names
!DESC = If f == 2, REACLIB format is used with -> instead of -->
!DESC = If f == 3, Numerical format is used with -->
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(IN)    :: f
    CHARACTER(LEN=60)             :: getreac_str
    OPTIONAL                      :: f
    INTEGER(KIND=4)               :: i,g,fmt = 0
    INTEGER(KIND=4)               :: sortr(10),sortp(10)

    IF (PRESENT(f)) fmt = f

! Sort reactants by Z.  If protons are same, sort by neutrons
    CALL sort_part(r,sortr,sortp)

    SELECT CASE (fmt)
    CASE (0)                                  ! REACLIB format
       getreac_str = TRIM(getreac_s(r,sortr(1),2))
       DO i = 2, r%reac_num
          getreac_str = TRIM(getreac_str) // ' + ' // TRIM(getreac_s(r,sortr(i),2))
       END DO
       getreac_str = TRIM(getreac_str) // ' --> ' // TRIM(getprod_s(r,sortp(1),2))
       DO i = 2, r%prod_num
          getreac_str = TRIM(getreac_str) // ' + ' // TRIM(getprod_s(r,sortp(i),2))
       END DO
    CASE (1)                                  ! REACLIB format with numbers preceeding symbols
       getreac_str = TRIM(getreac_s(r,sortr(1),0))
       DO i = 2, r%reac_num
          getreac_str = TRIM(getreac_str) // ' + ' // TRIM(getreac_s(r,sortr(i),0))
       END DO
       getreac_str = TRIM(getreac_str) // ' --> ' // TRIM(getprod_s(r,sortp(1),0))
       DO i = 2, r%prod_num
          getreac_str = TRIM(getreac_str) // ' + ' // TRIM(getprod_s(r,sortp(i),0))
       END DO
    CASE (2)                                  ! REACLIB format with -> instead of -->
       getreac_str = TRIM(getreac_s(r,sortr(1),2))
       DO i = 2, r%reac_num
          getreac_str = TRIM(getreac_str) // ' + ' // TRIM(getreac_s(r,sortr(i),2))
       END DO
       getreac_str = TRIM(getreac_str) // ' -> ' // TRIM(getprod_s(r,sortp(1),2))
       DO i = 2, r%prod_num
          getreac_str = TRIM(getreac_str) // ' + ' // TRIM(getprod_s(r,sortp(i),2))
       END DO
    CASE (3)                                  ! Numerical format is used with -->
       getreac_str = TRIM(getreac_s(r,sortr(1),i=2))
       DO i = 2, r%reac_num
          getreac_str = TRIM(getreac_str) // ' + ' // TRIM(getreac_s(r,sortr(i),i=2))
       END DO
       getreac_str = TRIM(getreac_str) // ' --> ' // TRIM(getprod_s(r,sortp(1),i=2))
       DO i = 2, r%prod_num
          getreac_str = TRIM(getreac_str) // ' + ' // TRIM(getprod_s(r,sortp(i),i=2))
       END DO
    END SELECT

  END FUNCTION getreac_str

!---------------------------------------------------------------------
  SUBROUTINE validreaction(r,o,m)
!PURPOSE = Check if the reaction in r is valid
!STATUS = 
!DESC = r is a reactionparticles structure
!DESC = o is an INT(KIND=4) of the result.  0 means valid, -1 is invalid, -2 is don't know how to check
!DESC = m is a string of why the reaction is invalid, otherwise ''
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(OUT)   :: o
    CHARACTER(LEN=*),INTENT(OUT)  :: m
    INTEGER(KIND=4)               :: i,j,s,t,g

    o = 0
    m = ''
    g = r%reac_num

! Check for valid reaction type
    IF (r%reac_type == -1) THEN
       o = -1
       m = 'Invalid reaction type'
       RETURN
    END IF

    SELECT CASE (r%type_str)
    CASE ('')
! Ordinary reaction
! Check for e- or nu
       DO i = 1, r%reac_num + r%prod_num
          IF (r%part_z(i) == -3) THEN
             o = -2
             m = 'Do not know how to check reactions with neutrinos'
             RETURN
          ELSE IF (r%part_z(i) == -4) THEN
             o = -2
             m = 'Do not know how to check reactions with electrons'
             RETURN
          ELSE IF (r%part_z(i) == -1 .OR. r%part_n(i) == -1) THEN
             o = -1
             IF (i <= r%reac_num) THEN
                WRITE(m,'(A,I0,A)') 'Reactant # ',i,' is not valid or defined'
             ELSE
                WRITE(m,'(A,I0,A)') 'Product # ',i-r%reac_num,' is not valid or defined'
             END IF
             RETURN
          END IF
       END DO
       
! Get sum of reactant protons
       s = 0
       DO i = 1, g
          t = r%part_z(i)
          IF (t >= 0) s = s + t
       END DO

! Get sum of product protons
       j = 0
       DO i = 1, r%prod_num
          t = r%part_z(i+g)
          IF (t >= 0) j = j + t
       END DO

! If protons aren't balanced
       IF (s /= j) THEN
          o = -1
          m = 'Protons are not conserved'
          RETURN
       END IF

! Get sum of reactant neutrons
       s = 0
       DO i = 1, g
          t = r%part_n(i)
          IF (t >= 0) s = s + t
       END DO

! Get sum of product neutrons
       j = 0
       DO i = 1, r%prod_num
          t = r%part_n(i+g)
          IF (t >= 0) j = j + t
       END DO

! If neutrons aren't balanced
       IF (s /= j) THEN
          o = -1
          m = 'Neutrons are not conserved'
          RETURN
       END IF
    CASE DEFAULT
! Probably a decay
       o = -2
       m = 'Do not know how to check '//TRIM(r%type_str)//' reactions.'
       RETURN
    END SELECT
  END SUBROUTINE validreaction

!---------------------------------------------------------------------
  FUNCTION findsymbol(s)
!PURPOSE = Return atomic number of symbol in string s
!STATUS = 
!RETURNS = INT(KIND=4). If symbol is valid, atomic number is returned, otherwise 0
!DESC = s is a string
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)   :: s
    INTEGER(KIND=4)               :: i,l,findsymbol

    findsymbol = 0

! l holds the number of character to compare with a symbol
    l = LEN_TRIM(s)
    IF (l == 0) RETURN
    IF (l > LEN(symbols(1))) l = LEN(symbols(1))
    i = 1
    DO WHILE (i <= UBOUND(symbols,1))
       IF (s(1:l) == symbols(i)) THEN
          findsymbol = i
          RETURN
       END IF
       i = i + 1
    END DO
  END FUNCTION findsymbol

!---------------------------------------------------------------------
  SUBROUTINE read_int(s,n,cur)
!PURPOSE = Return the first positive integer from a string
!STATUS = 
!DESC = s is a string, n is the integer returned
!DESC = cur is the starting index into the string to look for an integer
!DESC = If the character pointed to by cur is not a digit, then n = -1
!DESC = on return, cur points to the character after the integer
!DESC = The maximum number allowed is 999
    IMPLICIT NONE
    CHARACTER(LEN=10),PARAMETER   :: digit = '0123456789'
    CHARACTER(LEN=*),INTENT(IN)   :: s
    INTEGER(KIND=4),INTENT(OUT)   :: n
    INTEGER(KIND=4),INTENT(INOUT) :: cur

    CHARACTER(LEN=1)              :: c
    INTEGER(KIND=4)               :: i

! Make sure cur is valid
    IF ((cur < 1) .OR. (cur > LEN(s))) THEN
       n = -1
       RETURN
    END IF

! Check that the first number is a digit
    c = s(cur:cur)
    IF (VERIFY(c,digit) /= 0) THEN
       n = -1
       RETURN
    END IF

! Read in number
    n = 0
    DO i = 1, 3
       IF (VERIFY(c,digit) == 0) THEN
! If a digit
          n = n * 10 + IACHAR(c) - 48
       ELSE
          RETURN
       END IF
       cur = cur + 1
       IF (cur > LEN(s)) RETURN
       c = s(cur:cur)
    END DO

! Advance cursor to end of number
    DO WHILE (VERIFY(c,digit) == 0)
       cur = cur + 1
       IF (cur > LEN(s)) RETURN
       c = s(cur:cur)
    END DO
  END SUBROUTINE read_int

!---------------------------------------------------------------------
  SUBROUTINE str2za(s,z,a,cur,special_num)
!PURPOSE = Return Z and A for first particle in a string
!STATUS = 
!DESC = s is a string, z and a are INT(KIND=4) values returned
!DESC = a particle (C14) can be written as '6,8' or 'C14' or '14C'
!DESC = other particles recognized are 'p','n','d','t','g','gamma','a','alpha','e-','nu'
!DESC = Z,A values for gamma particle are -2,-2
!DESC = Z,A values for nu are -3,-3
!DESC = Z,A values for e- are -4,-4
!DESC = cur is the starting index into the string to look for particle
!DESC = If the character pointed to by cur is not a particle, then z and a = -1
!DESC = on return, cur points to the character after the particle
    IMPLICIT NONE
    CHARACTER(LEN=10),PARAMETER   :: digit = '0123456789'
    CHARACTER(LEN=26),PARAMETER   :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(LEN=26),PARAMETER   :: lcase = 'abcdefghijklmnopqrstuvwxyz'
    CHARACTER(LEN=*),INTENT(IN)   :: s
    CHARACTER(LEN=10),INTENT(OUT) :: special_num
    INTEGER(KIND=4),INTENT(OUT)   :: z,a
    INTEGER(KIND=4),INTENT(INOUT) :: cur
    INTEGER(KIND=4)               :: i,j
    CHARACTER(LEN=1)              :: c

    special_num = ''

! Make sure cur is valid
    IF ((cur < 1) .OR. (cur > LEN(s))) THEN
       z = -1
       a = -1
       RETURN
    END IF

    c = s(cur:cur)

! Look for 'p'    Note that phosphorus should be a capital letter
    IF (c == 'p') THEN
       z = 1
       a = 1
       cur = cur + 1
       RETURN
    ELSE IF (c == 'n') THEN
! Check if 'n' or 'nu'
       cur = cur + 1
       IF (s(cur:cur) == 'u') THEN
          z = -3
          a = -3
          cur = cur + 1
       ELSE
          z = 0
          a = 1
       END IF
       RETURN
    ELSE IF (c == 'd') THEN
       z = 1
       a = 2
       cur = cur + 1
       RETURN
    ELSE IF (c == 't') THEN
       z = 1
       a = 3
       cur = cur + 1
       RETURN
    ELSE IF (c == 'g') THEN
       z = -2
       a = -2
! If 'gamma' then advance cur
       IF (s(cur:cur+4) == 'gamma') THEN
          cur = cur + 5
       ELSE
          cur = cur + 1
       END IF
       RETURN
    ELSE IF (c == 'a') THEN
       z = 2
       a = 4
! If 'alpha' then advance cur
       IF (s(cur:cur+4) == 'alpha') THEN
          cur = cur + 5
       ELSE
          cur = cur + 1
       END IF
       RETURN
    ELSE IF (s(cur:cur+1) == 'e-') THEN
       z = -4
       a = -4
       cur = cur + 2
       RETURN
    ELSE IF ((s(cur:cur+3) == 'Al-6') .OR. (s(cur:cur+3) == '-6Al')) THEN
       z = 13
       a = 26
       cur = cur + 4
       special_num = '-6'
       RETURN
    ELSE IF ((s(cur:cur+3) == 'Al*6') .OR. (s(cur:cur+3) == '*6Al')) THEN
       z = 13
       a = 26
       cur = cur + 4
       special_num = '*6'
       RETURN
    END IF

! Try to read in a number
    CALL read_int(s,i,cur)

    c = s(cur:cur)

    IF (i >= 0) THEN
! If a number, should find a comma or element symbol
       IF (c == ',') THEN
          z = i
! Try to read in second number
! Advance cur past comma
          cur = cur + 1
          CALL read_int(s,i,cur)
          IF (i > 0) THEN
! If a number was found
             a = i
          ELSE
! Signal an error
             z = -1
             a = -1
          END IF
       ELSE
! Read in symbol
! Check for uppercase character
          IF (VERIFY(c,cap) == 0) THEN
! If uppercase chr look for lowercase chr
             j = cur   ! Store index to start of symbol
             cur = cur + 1
             IF (VERIFY(s(cur:cur),lcase) == 0) THEN
! If lowercase chr, look for 2nd lowercase chr
                cur = cur + 1
                IF (VERIFY(s(cur:cur),lcase) == 0) cur = cur + 1   ! Symbol is three letters long
             END IF
! Find atomic number from symbol
             z = findsymbol(s(j:cur-1))
             IF (z > 0) THEN
                a = i
             ELSE
                z = -1
                a = -1
             END IF
          ELSE
! Signal an error
             z = -1
             a = -1
          END IF
       END IF
    ELSE
! Read in symbol
! Check for uppercase character
       IF (VERIFY(c,cap) == 0) THEN
! If uppercase chr look for lowercase chr
          j = cur   ! Store index to start of symbol
          cur = cur + 1
          IF (VERIFY(s(cur:cur),lcase) == 0) THEN
! If lowercase chr, look for 2nd lowercase chr
             cur = cur + 1
             IF (VERIFY(s(cur:cur),lcase) == 0) cur = cur + 1   ! Symbol is three letters long
          END IF
! Find atomic number from symbol
          z = findsymbol(s(j:cur-1))
! Try to read in a number
          CALL read_int(s,i,cur)
          IF ((i > 0) .AND. (z > 0)) THEN
             a = i
          ELSE
             z = -1
             a = -1
          END IF
       ELSE
! Signal an error
          z = -1
          a = -1
       END IF
    END IF

  END SUBROUTINE str2za

!---------------------------------------------------------------------
  FUNCTION getinverse(r)
!PURPOSE = Return an r for the inverse reaction
!STATUS = Complete but not fully tested
!DESC = Return a reactionparticles structure for the inverse reaction
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    TYPE(reactionparticles)            :: getinverse
    INTEGER(KIND=4)                    :: i

! Find reaction type (won't work for decay reactions)
    IF ((r%prod_num == 1) .AND. (r%reac_num == 1)) THEN
       getinverse%reac_type = 1
    ELSE IF ((r%prod_num == 1) .AND. (r%reac_num == 2)) THEN
       getinverse%reac_type = 2
    ELSE IF ((r%prod_num == 1) .AND. (r%reac_num == 3)) THEN
       getinverse%reac_type = 3
    ELSE IF ((r%prod_num == 2) .AND. (r%reac_num == 1)) THEN
       getinverse%reac_type = 4
    ELSE IF ((r%prod_num == 2) .AND. (r%reac_num == 2)) THEN
       getinverse%reac_type = 5
    ELSE IF ((r%prod_num == 2) .AND. (r%reac_num == 3)) THEN
       getinverse%reac_type = 6
    ELSE IF ((r%prod_num == 2) .AND. (r%reac_num == 4)) THEN
       getinverse%reac_type = 7
    ELSE IF ((r%prod_num == 3) .AND. (r%reac_num == 1)) THEN
       getinverse%reac_type = 8
    ELSE IF ((r%prod_num == 3) .AND. (r%reac_num == 2)) THEN
       getinverse%reac_type = 8
    ELSE
       getinverse%reac_type = 9
    END IF

    getinverse%reac_num = r%prod_num
    getinverse%prod_num = r%reac_num

    DO i = 1, r%prod_num
       getinverse%part_z(i) = r%part_z(i+r%reac_num)
       getinverse%part_n(i) = r%part_n(i+r%reac_num)
       getinverse%special_num(i) = r%special_num(i+r%reac_num)
    END DO

    DO i = 1, r%reac_num
       getinverse%part_z(i+r%prod_num) = r%part_z(i)
       getinverse%part_n(i+r%prod_num) = r%part_n(i)
       getinverse%special_num(i+r%prod_num) = r%special_num(i)
    END DO

! The inverse of a decay is not a decay
    IF (decay(r)) THEN
       getinverse%type_str = ''
    ELSE
! Check if this should be a decay
       getinverse%type_str = ''
    END IF
  END FUNCTION getinverse

!---------------------------------------------------------------------
  FUNCTION same_reaction(r1,r2)
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r1,r2
    LOGICAL(KIND=4)                    :: same_reaction
    INTEGER(KIND=4)                    :: i,t

    same_reaction = .FALSE.

    IF (r1%reac_type /= r2%reac_type) RETURN
    IF (r1%reac_num /= r2%reac_num) RETURN
    IF (r1%prod_num /= r2%prod_num) RETURN
    IF (r1%type_str /= r2%type_str) RETURN

    t = r1%reac_num + r1%prod_num

    DO i = 1, t
       IF (r1%part_z(i) /= r2%part_z(i)) RETURN
       IF (r1%part_n(i) /= r2%part_n(i)) RETURN
       IF (r1%special_num(i) /= r2%special_num(i)) RETURN
    END DO

    same_reaction = .TRUE.
  END FUNCTION same_reaction

!---------------------------------------------------------------------
  FUNCTION decay(r)
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    LOGICAL(KIND=4)                    :: decay

    SELECT CASE (r%type_str)
    CASE ('ec', 'bet+', 'bet-', 'bec')
       decay = .TRUE.
    CASE DEFAULT
       decay = .FALSE.
    END SELECT
  END FUNCTION decay

!---------------------------------------------------------------------
  FUNCTION get_iso_str(z,a,fmt)
    IMPLICIT NONE
    INTEGER(KIND=4),INTENT(IN)    :: z,a,fmt
    CHARACTER(LEN=7)              :: get_iso_str

    IF (z == 0 .AND. a == 1) THEN
       get_iso_str = 'n'
    ELSE IF (z == 1 .AND. a == 1) THEN
       get_iso_str = 'p'
    ELSE IF (z == 1 .AND. a == 2) THEN
       get_iso_str = 'd'
    ELSE IF (z == 1 .AND. a == 3) THEN
       get_iso_str = 't'
    ELSE IF (z > 0 .AND. z <= UBOUND(symbols,1)) THEN
       IF (fmt == 0) THEN
          WRITE(get_iso_str,'(I0,A)') a,symbols(z)
       ELSE
          WRITE(get_iso_str,'(A,I0)') TRIM(symbols(z)),a
       END IF
    ELSE
       WRITE(get_iso_str,'(I0,A,I0)') z,',',a
    END IF
  END FUNCTION get_iso_str

!---------------------------------------------------------------------
  FUNCTION cmp_part(r1,index1,r2,index2)
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r1,r2
    INTEGER(KIND=4),INTENT(IN)    :: index1,index2
    INTEGER(KIND=4)               :: cmp_part
    INTEGER(KIND=4)               :: a,b

    cmp_part = 0
    
    IF (r1%part_z(index1) < r2%part_z(index2)) THEN
       cmp_part = -1
    ELSE IF (r1%part_z(index1) > r2%part_z(index2)) THEN
       cmp_part = 1
    ELSE
! z is same
       IF (r1%part_n(index1) < r2%part_n(index2)) THEN
          cmp_part = -1
       ELSE IF (r1%part_n(index1) > r2%part_n(index2)) THEN
          cmp_part = 1
       ELSE
! z and n are same
          IF (r1%special_num(index1) == '') THEN
             a = 0
          ELSE IF (INDEX(r1%special_num(index1),'-') > 0) THEN
             a = 2
          ELSE IF (INDEX(r1%special_num(index1),'*') > 0) THEN
             a = 3
          ELSE
             a = 1
          END IF
          IF (r2%special_num(index2) == '') THEN
             b = 0
          ELSE IF (INDEX(r2%special_num(index2),'-') > 0) THEN
             b = 2
          ELSE IF (INDEX(r2%special_num(index2),'*') > 0) THEN
             b = 3
          ELSE
             b = 1
          END IF

          IF (a < b) THEN
             cmp_part = -1
          ELSE IF (a > b) THEN
             cmp_part = 1
          END IF
       END IF
    END IF
  END FUNCTION cmp_part
    
!---------------------------------------------------------------------
  SUBROUTINE sort_reac(r,sorti)
! sort(1) is index to lightest reactant
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(OUT)   :: sorti(:)
    INTEGER(KIND=4)               :: i,j,next,a,b
    LOGICAL(KIND=4)               :: done(UBOUND(sorti,1))

    sorti = 0
    done = .FALSE.

    DO i = 1, r%reac_num
       next = 0

       DO j = 1, r%reac_num
          IF (.NOT. done(j)) THEN
             IF (next == 0) THEN
                next = j
             ELSE
                IF (r%part_z(j) < r%part_z(next)) THEN
                   next = j
                ELSE IF (r%part_z(j) == r%part_z(next)) THEN
! z are same, check n
                   IF (r%part_n(j) < r%part_n(next)) THEN
                      next = j
                   ELSE IF (r%part_n(j) == r%part_n(next)) THEN
! z and n are same, check special_num
                      IF (r%special_num(j) == '') THEN
                         a = 0
                      ELSE IF (INDEX(r%special_num(j),'-') > 0) THEN
                         a = 2
                      ELSE IF (INDEX(r%special_num(j),'*') > 0) THEN
                         a = 3
                      ELSE
                         a = 1
                      END IF
                      IF (r%special_num(j) == '') THEN
                         b = 0
                      ELSE IF (INDEX(r%special_num(j),'-') > 0) THEN
                         b = 2
                      ELSE IF (INDEX(r%special_num(j),'*') > 0) THEN
                         b = 3
                      ELSE
                         b = 1
                      END IF

                      IF (a < b) next = j
! If z, n, and special_num are same, use first identical reactant found
                   END IF
                END IF
             END IF
          END IF
       END DO

       sorti(i) = next
       done(i) = .TRUE.
    END DO
  END SUBROUTINE sort_reac

!---------------------------------------------------------------------
  SUBROUTINE sort_prod(r,sorti)
! sort(1) is index to lightest product
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    INTEGER(KIND=4),INTENT(OUT)   :: sorti(:)
    INTEGER(KIND=4)               :: i,j,next,a,b
    LOGICAL(KIND=4)               :: done(UBOUND(sorti,1))

    sorti = 0
    done = .FALSE.

    DO i = 1, r%prod_num
       next = 0

       DO j = 1, r%prod_num
          IF (.NOT. done(j)) THEN
             IF (next == 0) THEN
                next = j
             ELSE
                IF (r%part_z(j + r%reac_num) < r%part_z(next + r%reac_num)) THEN
                   next = j
                ELSE IF (r%part_z(j + r%reac_num) == r%part_z(next + r%reac_num)) THEN
! z are same, check n
                   IF (r%part_n(j + r%reac_num) < r%part_n(next + r%reac_num)) THEN
                      next = j
                   ELSE IF (r%part_n(j + r%reac_num) == r%part_n(next + r%reac_num)) THEN
! z and n are same, check special_num
                      IF (r%special_num(j + r%reac_num) == '') THEN
                         a = 0
                      ELSE IF (INDEX(r%special_num(j + r%reac_num),'-') > 0) THEN
                         a = 2
                      ELSE IF (INDEX(r%special_num(j + r%reac_num),'*') > 0) THEN
                         a = 3
                      ELSE
                         a = 1
                      END IF
                      IF (r%special_num(j + r%reac_num) == '') THEN
                         b = 0
                      ELSE IF (INDEX(r%special_num(j + r%reac_num),'-') > 0) THEN
                         b = 2
                      ELSE IF (INDEX(r%special_num(j + r%reac_num),'*') > 0) THEN
                         b = 3
                      ELSE
                         b = 1
                      END IF

                      IF (a < b) next = j
! If z, n, and special_num are same, use first identical product found
                   END IF
                END IF
             END IF
          END IF
       END DO

       sorti(i) = next
       done(i) = .TRUE.
    END DO
  END SUBROUTINE sort_prod

!---------------------------------------------------------------------
  FUNCTION cmp_r(r1,r2)
! Return -1 if r1 < r2, 0 if r1 == r2, 1 if r1 > r2
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r1,r2
    INTEGER(KIND=4)               :: cmp_r
    INTEGER(KIND=4)               :: n,sorti1(8),sorti2(8)
    LOGICAL(KIND=4)               :: loop

    IF (r1%reac_type < r2%reac_type) THEN
       cmp_r = -1
       RETURN
    ELSE IF (r1%reac_type > r2%reac_type) THEN
       cmp_r = 1
       RETURN
    END IF

! Sort reactants
    CALL sort_reac(r1,sorti1)
    CALL sort_reac(r2,sorti2)

    loop = .TRUE.
! r1 and r2 have same # of reactants and products
    n = r1%reac_num
    DO WHILE (loop)
       cmp_r = cmp_part(r1,sorti1(n),r2,sorti2(n))
       IF (cmp_r /= 0) RETURN
       n = n - 1
       IF (n < 1) loop = .FALSE.
    END DO
       
! Sort products
    CALL sort_prod(r1,sorti1)
    CALL sort_prod(r2,sorti2)
    sorti1 = sorti1 + r1%reac_num
    sorti2 = sorti2 + r2%reac_num

    loop = .TRUE.
! r1 and r2 have same # of reactants and products
    n = r1%prod_num
    DO WHILE (loop)
       cmp_r = cmp_part(r1,sorti1(n),r2,sorti2(n))
       IF (cmp_r /= 0) RETURN
       n = n - 1
       IF (n < 1) loop = .FALSE.
    END DO
       
! Check type_str
    IF (r1%type_str < r2%type_str) THEN
       cmp_r = -1
    ELSE IF (r1%type_str > r2%type_str) THEN
       cmp_r = 1
    ELSE
       cmp_r = 0
    END IF
  END FUNCTION cmp_r

!---------------------------------------------------------------------
  SUBROUTINE sort_r(r,num_rids,sorti)
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r(:)
    INTEGER(KIND=4),INTENT(IN)    :: num_rids
    INTEGER(KIND=4),INTENT(OUT)   :: sorti(:)
    TYPE(reactionparticles)       :: next_r
    INTEGER(KIND=4)               :: i,next,rindex,c
    LOGICAL                       :: done(UBOUND(sorti,1))

    done = .FALSE.
    sorti = 0

    DO rindex = 1, num_rids
       next = 0

       DO i = 1, num_rids
          IF (.NOT. done(i)) THEN
! Get first unused value
             IF (next == 0) THEN
                next = i
             ELSE
                IF (cmp_r(r(i),r(next)) < 0) next = i
             END IF
          END IF
       END DO

       sorti(rindex) = next
       done(next) = .TRUE.
    END DO
  END SUBROUTINE sort_r

!---------------------------------------------------------------------
! Returns 0.0 on error
  FUNCTION get_qvalue(r)
    USE nuc_data
    IMPLICIT NONE
    TYPE(reactionparticles),INTENT(IN) :: r
    REAL(KIND=8)       :: get_qvalue
    INTEGER(KIND=4)    :: i

    get_qvalue = 0.0

    DO i = 1, r%reac_num
       get_qvalue = get_qvalue + &
            nuc_m_excess(r%part_z(i), r%part_z(i)+r%part_n(i))
    END DO
    IF (get_qvalue >= HUGE(get_qvalue)) THEN
       get_qvalue = 0.0
       RETURN
    END IF

    DO i = 1, r%prod_num
       get_qvalue = get_qvalue - nuc_m_excess(r%part_z(i+r%reac_num), &
           r%part_z(i+r%reac_num) + r%part_n(i+r%reac_num))
    END DO
    IF (-get_qvalue >= HUGE(get_qvalue)) get_qvalue = 0.0
  END FUNCTION get_qvalue

!---------------------------------------------------------------------
!---------------------------------------------------------------------
END MODULE reactionstrings
