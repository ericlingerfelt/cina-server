MODULE nuc_data
CONTAINS

  !---------------------------------------------------------------------
  FUNCTION nuc_spin(z,a)
    IMPLICIT NONE
    REAL(KIND=8),PARAMETER        :: bad = HUGE(1D0)
    INTEGER(KIND=4),INTENT(IN)    :: z,a
    REAL(KIND=8)                  :: nuc_spin
    
    nuc_spin = bad    !no data 
    
    SELECT CASE (z)
    CASE (0)
       SELECT CASE (a)
       CASE (1)
          nuc_spin =    0.5    ! n
       END SELECT
    CASE (1)
       SELECT CASE (a)
       CASE (1)
          nuc_spin =    0.5    ! p
       CASE (2)
          nuc_spin =    1.0    ! d
       CASE (3)
          nuc_spin =    0.5    ! t
       END SELECT
    CASE (2)
       SELECT CASE (a)
       CASE (3)
          nuc_spin =    0.5    ! he3
       CASE (4)
          nuc_spin =    0.0    ! he4
       CASE (6)
          nuc_spin =    0.0    ! he6
       END SELECT
    CASE (3)
       SELECT CASE (a)
       CASE (6)
          nuc_spin =    1.0    ! li6
       CASE (7)
          nuc_spin =    1.5    ! li7
       CASE (8)
          nuc_spin =    2.0    ! li8
       CASE (9)
          nuc_spin =    1.5    ! li9
       END SELECT
    CASE (4)
       SELECT CASE (a)
       CASE (7)
          nuc_spin =    1.5    ! be7
       CASE (8)
          nuc_spin =    0.0    ! be8
       CASE (9)
          nuc_spin =    1.5    ! be9
       CASE (10)
          nuc_spin =    0.0    ! be10
       CASE (11)
          nuc_spin =    0.5    ! be11
       CASE (12)
          nuc_spin =    0.0    ! be12
       END SELECT
    CASE (5)
       SELECT CASE (a)
       CASE (8)
          nuc_spin =    2.0    ! b8
       CASE (10)
          nuc_spin =    3.0    ! b10
       CASE (11)
          nuc_spin =    1.5    ! b11
       CASE (12)
          nuc_spin =    1.0    ! b12
       CASE (13)
          nuc_spin =    1.5    ! b13
       CASE (14)
          nuc_spin =    2.0    ! b14
       END SELECT
    CASE (6)
       SELECT CASE (a)
       CASE (9)
          nuc_spin =    1.5    ! c9
       CASE (10)
          nuc_spin =    0.0    ! c10
       CASE (11)
          nuc_spin =    1.5    ! c11
       CASE (12)
          nuc_spin =    0.0    ! c12
       CASE (13)
          nuc_spin =    0.5    ! c13
       CASE (14)
          nuc_spin =    0.0    ! c14
       CASE (15)
          nuc_spin =    0.5    ! c15
       CASE (16)
          nuc_spin =    0.0    ! c16
       CASE (17)
          nuc_spin =    0.0    ! c17
       CASE (18)
          nuc_spin =    0.0    ! c18
       END SELECT
    CASE (7)
       SELECT CASE (a)
       CASE (11)
          nuc_spin =    0.0    ! n11
       CASE (12)
          nuc_spin =    1.0    ! n12
       CASE (13)
          nuc_spin =    0.5    ! n13
       CASE (14)
          nuc_spin =    1.0    ! n14
       CASE (15)
          nuc_spin =    0.5    ! n15
       CASE (16)
          nuc_spin =    2.0    ! n16
       CASE (17)
          nuc_spin =    0.5    ! n17
       CASE (18)
          nuc_spin =    1.0    ! n18
       CASE (19)
          nuc_spin =    0.0    ! n19
       CASE (20)
          nuc_spin =    0.0    ! n20
       CASE (21)
          nuc_spin =    0.0    ! n21
       END SELECT
    CASE (8)
       SELECT CASE (a)
       CASE (13)
          nuc_spin =    1.5    ! o13
       CASE (14)
          nuc_spin =    0.0    ! o14
       CASE (15)
          nuc_spin =    0.5    ! o15
       CASE (16)
          nuc_spin =    0.0    ! o16
       CASE (17)
          nuc_spin =    2.5    ! o17
       CASE (18)
          nuc_spin =    0.0    ! o18
       CASE (19)
          nuc_spin =    2.5    ! o19
       CASE (20)
          nuc_spin =    0.0    ! o20
       CASE (21)
          nuc_spin =    0.5    ! o21
       CASE (22)
          nuc_spin =    0.0    ! o22
       END SELECT
    CASE (9)
       SELECT CASE (a)
       CASE (14)
          nuc_spin =    0.0    ! f14
       CASE (16)
          nuc_spin =    0.0    ! f16
       CASE (17)
          nuc_spin =    2.5    ! f17
       CASE (18)
          nuc_spin =    1.0    ! f18
       CASE (19)
          nuc_spin =    0.5    ! f19
       CASE (20)
          nuc_spin =    2.0    ! f20
       CASE (21)
          nuc_spin =    2.5    ! f21
       CASE (22)
          nuc_spin =    4.0    ! f22
       CASE (23)
          nuc_spin =    1.5    ! f23
       CASE (24)
          nuc_spin =    0.0    ! f24
       CASE (25)
          nuc_spin =    0.5    ! f25
       CASE (26)
          nuc_spin =    2.0    ! f26
       END SELECT
    CASE (10)
       SELECT CASE (a)
       CASE (15)
          nuc_spin =    0.0    ! ne15
       CASE (16)
          nuc_spin =    0.0    ! ne16
       CASE (17)
          nuc_spin =    0.5    ! ne17
       CASE (18)
          nuc_spin =    0.0    ! ne18
       CASE (19)
          nuc_spin =    0.5    ! ne19
       CASE (20)
          nuc_spin =    0.0    ! ne20
       CASE (21)
          nuc_spin =    1.5    ! ne21
       CASE (22)
          nuc_spin =    0.0    ! ne22
       CASE (23)
          nuc_spin =    2.5    ! ne23
       CASE (24)
          nuc_spin =    0.0    ! ne24
       CASE (25)
          nuc_spin =    0.5    ! ne25
       CASE (26)
          nuc_spin =    0.0    ! ne26
       CASE (27)
          nuc_spin =    1.5    ! ne27
       CASE (28)
          nuc_spin =    0.0    ! ne28
       CASE (29)
          nuc_spin =    0.5    ! ne29
       CASE (30)
          nuc_spin =    0.0    ! ne30
       CASE (31)
          nuc_spin =    1.5    ! ne31
       CASE (32)
          nuc_spin =    0.0    ! ne32
       CASE (33)
          nuc_spin =    1.5    ! ne33
       CASE (34)
          nuc_spin =    0.0    ! ne34
       CASE (35)
          nuc_spin =    2.5    ! ne35
       CASE (36)
          nuc_spin =    0.0    ! ne36
       CASE (37)
          nuc_spin =    0.5    ! ne37
       CASE (38)
          nuc_spin =    0.0    ! ne38
       CASE (39)
          nuc_spin =    0.5    ! ne39
       CASE (40)
          nuc_spin =    0.0    ! ne40
       CASE (41)
          nuc_spin =    3.5    ! ne41
       END SELECT
    CASE (11)
       SELECT CASE (a)
       CASE (17)
          nuc_spin =    0.0    ! na17
       CASE (18)
          nuc_spin =    0.0    ! na18
       CASE (19)
          nuc_spin =    1.5    ! na19
       CASE (20)
          nuc_spin =    2.0    ! na20
       CASE (21)
          nuc_spin =    1.5    ! na21
       CASE (22)
          nuc_spin =    3.0    ! na22
       CASE (23)
          nuc_spin =    1.5    ! na23
       CASE (24)
          nuc_spin =    4.0    ! na24
       CASE (25)
          nuc_spin =    2.5    ! na25
       CASE (26)
          nuc_spin =    3.0    ! na26
       CASE (27)
          nuc_spin =    2.5    ! na27
       CASE (28)
          nuc_spin =    1.0    ! na28
       CASE (29)
          nuc_spin =    1.5    ! na29
       CASE (30)
          nuc_spin =    2.0    ! na30
       CASE (31)
          nuc_spin =    2.5    ! na31
       CASE (32)
          nuc_spin =    0.0    ! na32
       CASE (33)
          nuc_spin =    1.5    ! na33
       CASE (34)
          nuc_spin =    0.0    ! na34
       CASE (35)
          nuc_spin =    1.5    ! na35
       CASE (36)
          nuc_spin =    3.0    ! na36
       CASE (37)
          nuc_spin =    1.5    ! na37
       CASE (38)
          nuc_spin =    2.0    ! na38
       CASE (39)
          nuc_spin =    1.5    ! na39
       CASE (40)
          nuc_spin =    2.0    ! na40
       CASE (41)
          nuc_spin =    1.5    ! na41
       CASE (42)
          nuc_spin =    2.0    ! na42
       CASE (43)
          nuc_spin =    1.5    ! na43
       CASE (44)
          nuc_spin =    2.0    ! na44
       END SELECT
    CASE (12)
       SELECT CASE (a)
       CASE (19)
          nuc_spin =    0.0    ! mg19
       CASE (20)
          nuc_spin =    0.0    ! mg20
       CASE (21)
          nuc_spin =    1.5    ! mg21
       CASE (22)
          nuc_spin =    0.0    ! mg22
       CASE (23)
          nuc_spin =    1.5    ! mg23
       CASE (24)
          nuc_spin =    0.0    ! mg24
       CASE (25)
          nuc_spin =    2.5    ! mg25
       CASE (26)
          nuc_spin =    0.0    ! mg26
       CASE (27)
          nuc_spin =    0.5    ! mg27
       CASE (28)
          nuc_spin =    0.0    ! mg28
       CASE (29)
          nuc_spin =    1.5    ! mg29
       CASE (30)
          nuc_spin =    0.0    ! mg30
       CASE (31)
          nuc_spin =    1.5    ! mg31
       CASE (32)
          nuc_spin =    0.0    ! mg32
       CASE (33)
          nuc_spin =    2.5    ! mg33
       CASE (34)
          nuc_spin =    0.0    ! mg34
       CASE (35)
          nuc_spin =    1.5    ! mg35
       CASE (36)
          nuc_spin =    0.0    ! mg36
       CASE (37)
          nuc_spin =    2.5    ! mg37
       CASE (38)
          nuc_spin =    0.0    ! mg38
       CASE (39)
          nuc_spin =    0.5    ! mg39
       CASE (40)
          nuc_spin =    0.0    ! mg40
       CASE (41)
          nuc_spin =    3.5    ! mg41
       CASE (42)
          nuc_spin =    0.0    ! mg42
       CASE (43)
          nuc_spin =    0.5    ! mg43
       CASE (44)
          nuc_spin =    0.0    ! mg44
       CASE (45)
          nuc_spin =    0.5    ! mg45
       CASE (46)
          nuc_spin =    0.0    ! mg46
       CASE (47)
          nuc_spin =    1.5    ! mg47
       END SELECT
    CASE (13)
       SELECT CASE (a)
       CASE (21)
          nuc_spin =    2.5    ! al21
       CASE (22)
          nuc_spin =    3.0    ! al22
       CASE (23)
          nuc_spin =    2.5    ! al23
       CASE (24)
          nuc_spin =    4.0    ! al24
       CASE (25)
          nuc_spin =    2.5    ! al25
       CASE (26)
          nuc_spin =    5.0    ! al26
       CASE (27)
          nuc_spin =    2.5    ! al27
       CASE (28)
          nuc_spin =    3.0    ! al28
       CASE (29)
          nuc_spin =    2.5    ! al29
       CASE (30)
          nuc_spin =    3.0    ! al30
       CASE (31)
          nuc_spin =    1.5    ! al31
       CASE (32)
          nuc_spin =    1.0    ! al32
       CASE (33)
          nuc_spin =    1.5    ! al33
       CASE (34)
          nuc_spin =    2.0    ! al34
       CASE (35)
          nuc_spin =    2.5    ! al35
       CASE (36)
          nuc_spin =    3.0    ! al36
       CASE (37)
          nuc_spin =    2.5    ! al37
       CASE (38)
          nuc_spin =    0.0    ! al38
       CASE (39)
          nuc_spin =    2.5    ! al39
       CASE (40)
          nuc_spin =    4.0    ! al40
       CASE (41)
          nuc_spin =    1.5    ! al41
       CASE (42)
          nuc_spin =    2.0    ! al42
       CASE (43)
          nuc_spin =    2.5    ! al43
       CASE (44)
          nuc_spin =    2.0    ! al44
       CASE (45)
          nuc_spin =    2.5    ! al45
       CASE (46)
          nuc_spin =    2.0    ! al46
       CASE (47)
          nuc_spin =    0.5    ! al47
       CASE (48)
          nuc_spin =    5.0    ! al48
       CASE (49)
          nuc_spin =    0.5    ! al49
       CASE (50)
          nuc_spin =    1.0    ! al50
       CASE (51)
          nuc_spin =    0.5    ! al51
       END SELECT
    CASE (14)
       SELECT CASE (a)
       CASE (22)
          nuc_spin =    0.0    ! si22
       CASE (23)
          nuc_spin =    2.5    ! si23
       CASE (24)
          nuc_spin =    0.0    ! si24
       CASE (25)
          nuc_spin =    2.5    ! si25
       CASE (26)
          nuc_spin =    0.0    ! si26
       CASE (27)
          nuc_spin =    2.5    ! si27
       CASE (28)
          nuc_spin =    0.0    ! si28
       CASE (29)
          nuc_spin =    0.5    ! si29
       CASE (30)
          nuc_spin =    0.0    ! si30
       CASE (31)
          nuc_spin =    1.5    ! si31
       CASE (32)
          nuc_spin =    0.0    ! si32
       CASE (33)
          nuc_spin =    1.5    ! si33
       CASE (34)
          nuc_spin =    0.0    ! si34
       CASE (35)
          nuc_spin =    2.5    ! si35
       CASE (36)
          nuc_spin =    0.0    ! si36
       CASE (37)
          nuc_spin =    1.5    ! si37
       CASE (38)
          nuc_spin =    0.0    ! si38
       CASE (39)
          nuc_spin =    2.5    ! si39
       CASE (40)
          nuc_spin =    0.0    ! si40
       CASE (41)
          nuc_spin =    0.5    ! si41
       CASE (42)
          nuc_spin =    0.0    ! si42
       CASE (43)
          nuc_spin =    1.5    ! si43
       CASE (44)
          nuc_spin =    0.0    ! si44
       CASE (45)
          nuc_spin =    0.5    ! si45
       CASE (46)
          nuc_spin =    0.0    ! si46
       CASE (47)
          nuc_spin =    2.5    ! si47
       CASE (48)
          nuc_spin =    0.0    ! si48
       CASE (49)
          nuc_spin =    4.5    ! si49
       CASE (50)
          nuc_spin =    0.0    ! si50
       CASE (51)
          nuc_spin =    2.5    ! si51
       CASE (52)
          nuc_spin =    0.0    ! si52
       CASE (53)
          nuc_spin =    2.5    ! si53
       CASE (54)
          nuc_spin =    0.0    ! si54
       END SELECT
    CASE (15)
       SELECT CASE (a)
       CASE (23)
          nuc_spin =    0.5    ! p23
       CASE (24)
          nuc_spin =    3.0    ! p24
       CASE (25)
          nuc_spin =    0.5    ! p25
       CASE (26)
          nuc_spin =    3.0    ! p26
       CASE (27)
          nuc_spin =    0.5    ! p27
       CASE (28)
          nuc_spin =    3.0    ! p28
       CASE (29)
          nuc_spin =    0.5    ! p29
       CASE (30)
          nuc_spin =    1.0    ! p30
       CASE (31)
          nuc_spin =    0.5    ! p31
       CASE (32)
          nuc_spin =    1.0    ! p32
       CASE (33)
          nuc_spin =    0.5    ! p33
       CASE (34)
          nuc_spin =    1.0    ! p34
       CASE (35)
          nuc_spin =    0.5    ! p35
       CASE (36)
          nuc_spin =    2.0    ! p36
       CASE (37)
          nuc_spin =    0.5    ! p37
       CASE (38)
          nuc_spin =    2.0    ! p38
       CASE (39)
          nuc_spin =    0.5    ! p39
       CASE (40)
          nuc_spin =    2.0    ! p40
       CASE (41)
          nuc_spin =    0.5    ! p41
       CASE (42)
          nuc_spin =    4.0    ! p42
       CASE (43)
          nuc_spin =    1.5    ! p43
       CASE (44)
          nuc_spin =    0.0    ! p44
       CASE (45)
          nuc_spin =    0.5    ! p45
       CASE (46)
          nuc_spin =    0.0    ! p46
       CASE (47)
          nuc_spin =    0.5    ! p47
       CASE (48)
          nuc_spin =    3.0    ! p48
       CASE (49)
          nuc_spin =    1.5    ! p49
       CASE (50)
          nuc_spin =    3.0    ! p50
       CASE (51)
          nuc_spin =    1.5    ! p51
       CASE (52)
          nuc_spin =    1.0    ! p52
       CASE (53)
          nuc_spin =    2.5    ! p53
       CASE (54)
          nuc_spin =    3.0    ! p54
       CASE (55)
          nuc_spin =    2.5    ! p55
       CASE (56)
          nuc_spin =    3.0    ! p56
       CASE (57)
          nuc_spin =    0.5    ! p57
       END SELECT
    CASE (16)
       SELECT CASE (a)
       CASE (24)
          nuc_spin =    0.0    ! s24
       CASE (25)
          nuc_spin =    0.5    ! s25
       CASE (26)
          nuc_spin =    0.0    ! s26
       CASE (27)
          nuc_spin =    1.5    ! s27
       CASE (28)
          nuc_spin =    0.0    ! s28
       CASE (29)
          nuc_spin =    2.5    ! s29
       CASE (30)
          nuc_spin =    0.0    ! s30
       CASE (31)
          nuc_spin =    0.5    ! s31
       CASE (32)
          nuc_spin =    0.0    ! s32
       CASE (33)
          nuc_spin =    1.5    ! s33
       CASE (34)
          nuc_spin =    0.0    ! s34
       CASE (35)
          nuc_spin =    1.5    ! s35
       CASE (36)
          nuc_spin =    0.0    ! s36
       CASE (37)
          nuc_spin =    3.5    ! s37
       CASE (38)
          nuc_spin =    0.0    ! s38
       CASE (39)
          nuc_spin =    1.5    ! s39
       CASE (40)
          nuc_spin =    0.0    ! s40
       CASE (41)
          nuc_spin =    2.5    ! s41
       CASE (42)
          nuc_spin =    0.0    ! s42
       CASE (43)
          nuc_spin =    3.5    ! s43
       CASE (44)
          nuc_spin =    0.0    ! s44
       CASE (45)
          nuc_spin =    0.5    ! s45
       CASE (46)
          nuc_spin =    0.0    ! s46
       CASE (47)
          nuc_spin =    0.5    ! s47
       CASE (48)
          nuc_spin =    0.0    ! s48
       CASE (49)
          nuc_spin =    1.5    ! s49
       CASE (50)
          nuc_spin =    0.0    ! s50
       CASE (51)
          nuc_spin =    4.5    ! s51
       CASE (52)
          nuc_spin =    0.0    ! s52
       CASE (53)
          nuc_spin =    1.5    ! s53
       CASE (54)
          nuc_spin =    0.0    ! s54
       CASE (55)
          nuc_spin =    2.5    ! s55
       CASE (56)
          nuc_spin =    0.0    ! s56
       CASE (57)
          nuc_spin =    2.5    ! s57
       CASE (58)
          nuc_spin =    0.0    ! s58
       CASE (59)
          nuc_spin =    0.5    ! s59
       CASE (60)
          nuc_spin =    0.0    ! s60
       END SELECT
    CASE (17)
       SELECT CASE (a)
       CASE (26)
          nuc_spin =    1.0    ! cl26
       CASE (27)
          nuc_spin =    0.5    ! cl27
       CASE (28)
          nuc_spin =    1.0    ! cl28
       CASE (29)
          nuc_spin =    1.5    ! cl29
       CASE (30)
          nuc_spin =    1.0    ! cl30
       CASE (31)
          nuc_spin =    1.5    ! cl31
       CASE (32)
          nuc_spin =    1.0    ! cl32
       CASE (33)
          nuc_spin =    1.5    ! cl33
       CASE (34)
          nuc_spin =    0.0    ! cl34
       CASE (35)
          nuc_spin =    1.5    ! cl35
       CASE (36)
          nuc_spin =    2.0    ! cl36
       CASE (37)
          nuc_spin =    1.5    ! cl37
       CASE (38)
          nuc_spin =    2.0    ! cl38
       CASE (39)
          nuc_spin =    1.5    ! cl39
       CASE (40)
          nuc_spin =    2.0    ! cl40
       CASE (41)
          nuc_spin =    0.5    ! cl41
       CASE (42)
          nuc_spin =    2.0    ! cl42
       CASE (43)
          nuc_spin =    0.5    ! cl43
       CASE (44)
          nuc_spin =    4.0    ! cl44
       CASE (45)
          nuc_spin =    1.5    ! cl45
       CASE (46)
          nuc_spin =    0.0    ! cl46
       CASE (47)
          nuc_spin =    0.5    ! cl47
       CASE (48)
          nuc_spin =    0.0    ! cl48
       CASE (49)
          nuc_spin =    0.5    ! cl49
       CASE (50)
          nuc_spin =    2.0    ! cl50
       CASE (51)
          nuc_spin =    0.5    ! cl51
       CASE (52)
          nuc_spin =    5.0    ! cl52
       CASE (53)
          nuc_spin =    0.5    ! cl53
       CASE (54)
          nuc_spin =    0.0    ! cl54
       CASE (55)
          nuc_spin =    0.5    ! cl55
       CASE (56)
          nuc_spin =    2.0    ! cl56
       CASE (57)
          nuc_spin =    0.5    ! cl57
       CASE (58)
          nuc_spin =    3.0    ! cl58
       CASE (59)
          nuc_spin =    0.5    ! cl59
       CASE (60)
          nuc_spin =    3.0    ! cl60
       CASE (61)
          nuc_spin =    0.5    ! cl61
       CASE (62)
          nuc_spin =    1.0    ! cl62
       CASE (63)
          nuc_spin =    0.5    ! cl63
       END SELECT
    CASE (18)
       SELECT CASE (a)
       CASE (27)
          nuc_spin =    2.5    ! ar27
       CASE (28)
          nuc_spin =    0.0    ! ar28
       CASE (29)
          nuc_spin =    1.5    ! ar29
       CASE (30)
          nuc_spin =    0.0    ! ar30
       CASE (31)
          nuc_spin =    0.5    ! ar31
       CASE (32)
          nuc_spin =    0.0    ! ar32
       CASE (33)
          nuc_spin =    0.5    ! ar33
       CASE (34)
          nuc_spin =    0.0    ! ar34
       CASE (35)
          nuc_spin =    1.5    ! ar35
       CASE (36)
          nuc_spin =    0.0    ! ar36
       CASE (37)
          nuc_spin =    1.5    ! ar37
       CASE (38)
          nuc_spin =    0.0    ! ar38
       CASE (39)
          nuc_spin =    3.5    ! ar39
       CASE (40)
          nuc_spin =    0.0    ! ar40
       CASE (41)
          nuc_spin =    3.5    ! ar41
       CASE (42)
          nuc_spin =    0.0    ! ar42
       CASE (43)
          nuc_spin =    1.5    ! ar43
       CASE (44)
          nuc_spin =    0.0    ! ar44
       CASE (45)
          nuc_spin =    0.5    ! ar45
       CASE (46)
          nuc_spin =    0.0    ! ar46
       CASE (47)
          nuc_spin =    1.5    ! ar47
       CASE (48)
          nuc_spin =    0.0    ! ar48
       CASE (49)
          nuc_spin =    0.5    ! ar49
       CASE (50)
          nuc_spin =    0.0    ! ar50
       CASE (51)
          nuc_spin =    0.5    ! ar51
       CASE (52)
          nuc_spin =    0.0    ! ar52
       CASE (53)
          nuc_spin =    4.5    ! ar53
       CASE (54)
          nuc_spin =    0.0    ! ar54
       CASE (55)
          nuc_spin =    4.5    ! ar55
       CASE (56)
          nuc_spin =    0.0    ! ar56
       CASE (57)
          nuc_spin =    4.5    ! ar57
       CASE (58)
          nuc_spin =    0.0    ! ar58
       CASE (59)
          nuc_spin =    3.5    ! ar59
       CASE (60)
          nuc_spin =    0.0    ! ar60
       CASE (61)
          nuc_spin =    2.5    ! ar61
       CASE (62)
          nuc_spin =    0.0    ! ar62
       CASE (63)
          nuc_spin =    1.5    ! ar63
       CASE (64)
          nuc_spin =    0.0    ! ar64
       CASE (65)
          nuc_spin =    0.5    ! ar65
       CASE (66)
          nuc_spin =    0.0    ! ar66
       CASE (67)
          nuc_spin =    4.5    ! ar67
       END SELECT
    CASE (19)
       SELECT CASE (a)
       CASE (29)
          nuc_spin =    1.5    ! k29
       CASE (30)
          nuc_spin =    1.0    ! k30
       CASE (31)
          nuc_spin =    1.5    ! k31
       CASE (32)
          nuc_spin =    1.0    ! k32
       CASE (33)
          nuc_spin =    0.5    ! k33
       CASE (34)
          nuc_spin =    1.0    ! k34
       CASE (35)
          nuc_spin =    1.5    ! k35
       CASE (36)
          nuc_spin =    2.0    ! k36
       CASE (37)
          nuc_spin =    1.5    ! k37
       CASE (38)
          nuc_spin =    3.0    ! k38
       CASE (39)
          nuc_spin =    1.5    ! k39
       CASE (40)
          nuc_spin =    4.0    ! k40
       CASE (41)
          nuc_spin =    1.5    ! k41
       CASE (42)
          nuc_spin =    2.0    ! k42
       CASE (43)
          nuc_spin =    1.5    ! k43
       CASE (44)
          nuc_spin =    2.0    ! k44
       CASE (45)
          nuc_spin =    1.5    ! k45
       CASE (46)
          nuc_spin =    2.0    ! k46
       CASE (47)
          nuc_spin =    0.5    ! k47
       CASE (48)
          nuc_spin =    2.0    ! k48
       CASE (49)
          nuc_spin =    1.5    ! k49
       CASE (50)
          nuc_spin =    0.0    ! k50
       CASE (51)
          nuc_spin =    0.5    ! k51
       CASE (52)
          nuc_spin =    2.0    ! k52
       CASE (53)
          nuc_spin =    1.5    ! k53
       CASE (54)
          nuc_spin =    5.0    ! k54
       CASE (55)
          nuc_spin =    3.5    ! k55
       CASE (56)
          nuc_spin =    2.0    ! k56
       CASE (57)
          nuc_spin =    0.5    ! k57
       CASE (58)
          nuc_spin =    5.0    ! k58
       CASE (59)
          nuc_spin =    0.5    ! k59
       CASE (60)
          nuc_spin =    0.0    ! k60
       CASE (61)
          nuc_spin =    3.5    ! k61
       CASE (62)
          nuc_spin =    4.0    ! k62
       CASE (63)
          nuc_spin =    3.5    ! k63
       CASE (64)
          nuc_spin =    3.0    ! k64
       CASE (65)
          nuc_spin =    0.5    ! k65
       CASE (66)
          nuc_spin =    3.0    ! k66
       CASE (67)
          nuc_spin =    0.5    ! k67
       CASE (68)
          nuc_spin =    5.0    ! k68
       CASE (69)
          nuc_spin =    0.5    ! k69
       CASE (70)
          nuc_spin =    3.0    ! k70
       END SELECT
    CASE (20)
       SELECT CASE (a)
       CASE (30)
          nuc_spin =    0.0    ! ca30
       CASE (31)
          nuc_spin =    2.5    ! ca31
       CASE (32)
          nuc_spin =    0.0    ! ca32
       CASE (33)
          nuc_spin =    1.5    ! ca33
       CASE (34)
          nuc_spin =    0.0    ! ca34
       CASE (35)
          nuc_spin =    0.5    ! ca35
       CASE (36)
          nuc_spin =    0.0    ! ca36
       CASE (37)
          nuc_spin =    1.5    ! ca37
       CASE (38)
          nuc_spin =    0.0    ! ca38
       CASE (39)
          nuc_spin =    1.5    ! ca39
       CASE (40)
          nuc_spin =    0.0    ! ca40
       CASE (41)
          nuc_spin =    3.5    ! ca41
       CASE (42)
          nuc_spin =    0.0    ! ca42
       CASE (43)
          nuc_spin =    3.5    ! ca43
       CASE (44)
          nuc_spin =    0.0    ! ca44
       CASE (45)
          nuc_spin =    3.5    ! ca45
       CASE (46)
          nuc_spin =    0.0    ! ca46
       CASE (47)
          nuc_spin =    3.5    ! ca47
       CASE (48)
          nuc_spin =    0.0    ! ca48
       CASE (49)
          nuc_spin =    1.5    ! ca49
       CASE (50)
          nuc_spin =    0.0    ! ca50
       CASE (51)
          nuc_spin =    1.5    ! ca51
       CASE (52)
          nuc_spin =    0.0    ! ca52
       CASE (53)
          nuc_spin =    1.5    ! ca53
       CASE (54)
          nuc_spin =    0.0    ! ca54
       CASE (55)
          nuc_spin =    2.5    ! ca55
       CASE (56)
          nuc_spin =    0.0    ! ca56
       CASE (57)
          nuc_spin =    1.5    ! ca57
       CASE (58)
          nuc_spin =    0.0    ! ca58
       CASE (59)
          nuc_spin =    0.5    ! ca59
       CASE (60)
          nuc_spin =    0.0    ! ca60
       CASE (61)
          nuc_spin =    4.5    ! ca61
       CASE (62)
          nuc_spin =    0.0    ! ca62
       CASE (63)
          nuc_spin =    1.5    ! ca63
       CASE (64)
          nuc_spin =    0.0    ! ca64
       CASE (65)
          nuc_spin =    2.5    ! ca65
       CASE (66)
          nuc_spin =    0.0    ! ca66
       CASE (67)
          nuc_spin =    3.5    ! ca67
       CASE (68)
          nuc_spin =    0.0    ! ca68
       CASE (69)
          nuc_spin =    4.5    ! ca69
       CASE (70)
          nuc_spin =    0.0    ! ca70
       CASE (71)
          nuc_spin =    0.5    ! ca71
       CASE (72)
          nuc_spin =    0.0    ! ca72
       CASE (73)
          nuc_spin =    1.5    ! ca73
       END SELECT
    CASE (21)
       SELECT CASE (a)
       CASE (32)
          nuc_spin =    0.0    ! sc32
       CASE (33)
          nuc_spin =    3.5    ! sc33
       CASE (34)
          nuc_spin =    2.0    ! sc34
       CASE (35)
          nuc_spin =    3.5    ! sc35
       CASE (36)
          nuc_spin =    4.0    ! sc36
       CASE (37)
          nuc_spin =    3.5    ! sc37
       CASE (38)
          nuc_spin =    2.0    ! sc38
       CASE (39)
          nuc_spin =    1.5    ! sc39
       CASE (40)
          nuc_spin =    4.0    ! sc40
       CASE (41)
          nuc_spin =    3.5    ! sc41
       CASE (42)
          nuc_spin =    0.0    ! sc42
       CASE (43)
          nuc_spin =    3.5    ! sc43
       CASE (44)
          nuc_spin =    2.0    ! sc44
       CASE (45)
          nuc_spin =    3.5    ! sc45
       CASE (46)
          nuc_spin =    4.0    ! sc46
       CASE (47)
          nuc_spin =    3.5    ! sc47
       CASE (48)
          nuc_spin =    6.0    ! sc48
       CASE (49)
          nuc_spin =    3.5    ! sc49
       CASE (50)
          nuc_spin =    5.0    ! sc50
       CASE (51)
          nuc_spin =    3.5    ! sc51
       CASE (52)
          nuc_spin =    3.0    ! sc52
       CASE (53)
          nuc_spin =    3.5    ! sc53
       CASE (54)
          nuc_spin =    1.0    ! sc54
       CASE (55)
          nuc_spin =    3.5    ! sc55
       CASE (56)
          nuc_spin =    3.0    ! sc56
       CASE (57)
          nuc_spin =    3.5    ! sc57
       CASE (58)
          nuc_spin =    4.0    ! sc58
       CASE (59)
          nuc_spin =    3.5    ! sc59
       CASE (60)
          nuc_spin =    3.0    ! sc60
       CASE (61)
          nuc_spin =    3.5    ! sc61
       CASE (62)
          nuc_spin =    1.0    ! sc62
       CASE (63)
          nuc_spin =    0.5    ! sc63
       CASE (64)
          nuc_spin =    2.0    ! sc64
       CASE (65)
          nuc_spin =    0.5    ! sc65
       CASE (66)
          nuc_spin =    2.0    ! sc66
       CASE (67)
          nuc_spin =    0.5    ! sc67
       CASE (68)
          nuc_spin =    4.0    ! sc68
       CASE (69)
          nuc_spin =    0.5    ! sc69
       CASE (70)
          nuc_spin =    4.0    ! sc70
       CASE (71)
          nuc_spin =    0.5    ! sc71
       CASE (72)
          nuc_spin =    0.0    ! sc72
       CASE (73)
          nuc_spin =    0.5    ! sc73
       CASE (74)
          nuc_spin =    2.0    ! sc74
       CASE (75)
          nuc_spin =    0.5    ! sc75
       CASE (76)
          nuc_spin =    0.0    ! sc76
       END SELECT
    CASE (22)
       SELECT CASE (a)
       CASE (34)
          nuc_spin =    0.0    ! ti34
       CASE (35)
          nuc_spin =    2.5    ! ti35
       CASE (36)
          nuc_spin =    0.0    ! ti36
       CASE (37)
          nuc_spin =    0.5    ! ti37
       CASE (38)
          nuc_spin =    0.0    ! ti38
       CASE (39)
          nuc_spin =    0.5    ! ti39
       CASE (40)
          nuc_spin =    0.0    ! ti40
       CASE (41)
          nuc_spin =    1.5    ! ti41
       CASE (42)
          nuc_spin =    0.0    ! ti42
       CASE (43)
          nuc_spin =    3.5    ! ti43
       CASE (44)
          nuc_spin =    0.0    ! ti44
       CASE (45)
          nuc_spin =    3.5    ! ti45
       CASE (46)
          nuc_spin =    0.0    ! ti46
       CASE (47)
          nuc_spin =    2.5    ! ti47
       CASE (48)
          nuc_spin =    0.0    ! ti48
       CASE (49)
          nuc_spin =    3.5    ! ti49
       CASE (50)
          nuc_spin =    0.0    ! ti50
       CASE (51)
          nuc_spin =    1.5    ! ti51
       CASE (52)
          nuc_spin =    0.0    ! ti52
       CASE (53)
          nuc_spin =    1.5    ! ti53
       CASE (54)
          nuc_spin =    0.0    ! ti54
       CASE (55)
          nuc_spin =    0.5    ! ti55
       CASE (56)
          nuc_spin =    0.0    ! ti56
       CASE (57)
          nuc_spin =    1.5    ! ti57
       CASE (58)
          nuc_spin =    0.0    ! ti58
       CASE (59)
          nuc_spin =    1.5    ! ti59
       CASE (60)
          nuc_spin =    0.0    ! ti60
       CASE (61)
          nuc_spin =    0.5    ! ti61
       CASE (62)
          nuc_spin =    0.0    ! ti62
       CASE (63)
          nuc_spin =    4.5    ! ti63
       CASE (64)
          nuc_spin =    0.0    ! ti64
       CASE (65)
          nuc_spin =    1.5    ! ti65
       CASE (66)
          nuc_spin =    0.0    ! ti66
       CASE (67)
          nuc_spin =    2.5    ! ti67
       CASE (68)
          nuc_spin =    0.0    ! ti68
       CASE (69)
          nuc_spin =    3.5    ! ti69
       CASE (70)
          nuc_spin =    0.0    ! ti70
       CASE (71)
          nuc_spin =    4.5    ! ti71
       CASE (72)
          nuc_spin =    0.0    ! ti72
       CASE (73)
          nuc_spin =    0.5    ! ti73
       CASE (74)
          nuc_spin =    0.0    ! ti74
       CASE (75)
          nuc_spin =    1.5    ! ti75
       CASE (76)
          nuc_spin =    0.0    ! ti76
       CASE (77)
          nuc_spin =    0.5    ! ti77
       CASE (78)
          nuc_spin =    0.0    ! ti78
       CASE (79)
          nuc_spin =    1.5    ! ti79
       CASE (80)
          nuc_spin =    0.0    ! ti80
       END SELECT
    CASE (23)
       SELECT CASE (a)
       CASE (36)
          nuc_spin =    3.0    ! v36
       CASE (37)
          nuc_spin =    1.5    ! v37
       CASE (38)
          nuc_spin =    2.0    ! v38
       CASE (39)
          nuc_spin =    1.5    ! v39
       CASE (40)
          nuc_spin =    2.0    ! v40
       CASE (41)
          nuc_spin =    0.5    ! v41
       CASE (42)
          nuc_spin =    2.0    ! v42
       CASE (43)
          nuc_spin =    0.5    ! v43
       CASE (44)
          nuc_spin =    3.0    ! v44
       CASE (45)
          nuc_spin =    3.5    ! v45
       CASE (46)
          nuc_spin =    0.0    ! v46
       CASE (47)
          nuc_spin =    1.5    ! v47
       CASE (48)
          nuc_spin =    4.0    ! v48
       CASE (49)
          nuc_spin =    3.5    ! v49
       CASE (50)
          nuc_spin =    6.0    ! v50
       CASE (51)
          nuc_spin =    3.5    ! v51
       CASE (52)
          nuc_spin =    3.0    ! v52
       CASE (53)
          nuc_spin =    3.5    ! v53
       CASE (54)
          nuc_spin =    3.0    ! v54
       CASE (55)
          nuc_spin =    3.5    ! v55
       CASE (56)
          nuc_spin =    2.0    ! v56
       CASE (57)
          nuc_spin =    1.5    ! v57
       CASE (58)
          nuc_spin =    2.0    ! v58
       CASE (59)
          nuc_spin =    1.5    ! v59
       CASE (60)
          nuc_spin =    1.0    ! v60
       CASE (61)
          nuc_spin =    2.5    ! v61
       CASE (62)
          nuc_spin =    3.0    ! v62
       CASE (63)
          nuc_spin =    1.5    ! v63
       CASE (64)
          nuc_spin =    2.0    ! v64
       CASE (65)
          nuc_spin =    1.5    ! v65
       CASE (66)
          nuc_spin =    0.0    ! v66
       CASE (67)
          nuc_spin =    1.5    ! v67
       CASE (68)
          nuc_spin =    3.0    ! v68
       CASE (69)
          nuc_spin =    1.5    ! v69
       CASE (70)
          nuc_spin =    2.0    ! v70
       CASE (71)
          nuc_spin =    1.5    ! v71
       CASE (72)
          nuc_spin =    5.0    ! v72
       CASE (73)
          nuc_spin =    1.5    ! v73
       CASE (74)
          nuc_spin =    2.0    ! v74
       CASE (75)
          nuc_spin =    1.5    ! v75
       CASE (76)
          nuc_spin =    0.0    ! v76
       CASE (77)
          nuc_spin =    1.5    ! v77
       CASE (78)
          nuc_spin =    0.0    ! v78
       CASE (79)
          nuc_spin =    1.5    ! v79
       CASE (80)
          nuc_spin =    0.0    ! v80
       CASE (81)
          nuc_spin =    1.5    ! v81
       CASE (82)
          nuc_spin =    2.0    ! v82
       CASE (83)
          nuc_spin =    1.5    ! v83
       END SELECT
    CASE (24)
       SELECT CASE (a)
       CASE (38)
          nuc_spin =    0.0    ! cr38
       CASE (39)
          nuc_spin =    0.5    ! cr39
       CASE (40)
          nuc_spin =    0.0    ! cr40
       CASE (41)
          nuc_spin =    0.5    ! cr41
       CASE (42)
          nuc_spin =    0.0    ! cr42
       CASE (43)
          nuc_spin =    1.5    ! cr43
       CASE (44)
          nuc_spin =    0.0    ! cr44
       CASE (45)
          nuc_spin =    2.5    ! cr45
       CASE (46)
          nuc_spin =    0.0    ! cr46
       CASE (47)
          nuc_spin =    1.5    ! cr47
       CASE (48)
          nuc_spin =    0.0    ! cr48
       CASE (49)
          nuc_spin =    2.5    ! cr49
       CASE (50)
          nuc_spin =    0.0    ! cr50
       CASE (51)
          nuc_spin =    3.5    ! cr51
       CASE (52)
          nuc_spin =    0.0    ! cr52
       CASE (53)
          nuc_spin =    1.5    ! cr53
       CASE (54)
          nuc_spin =    0.0    ! cr54
       CASE (55)
          nuc_spin =    1.5    ! cr55
       CASE (56)
          nuc_spin =    0.0    ! cr56
       CASE (57)
          nuc_spin =    1.5    ! cr57
       CASE (58)
          nuc_spin =    0.0    ! cr58
       CASE (59)
          nuc_spin =    1.5    ! cr59
       CASE (60)
          nuc_spin =    0.0    ! cr60
       CASE (61)
          nuc_spin =    1.5    ! cr61
       CASE (62)
          nuc_spin =    0.0    ! cr62
       CASE (63)
          nuc_spin =    1.5    ! cr63
       CASE (64)
          nuc_spin =    0.0    ! cr64
       CASE (65)
          nuc_spin =    0.5    ! cr65
       CASE (66)
          nuc_spin =    0.0    ! cr66
       CASE (67)
          nuc_spin =    1.5    ! cr67
       CASE (68)
          nuc_spin =    0.0    ! cr68
       CASE (69)
          nuc_spin =    2.5    ! cr69
       CASE (70)
          nuc_spin =    0.0    ! cr70
       CASE (71)
          nuc_spin =    3.5    ! cr71
       CASE (72)
          nuc_spin =    0.0    ! cr72
       CASE (73)
          nuc_spin =    4.5    ! cr73
       CASE (74)
          nuc_spin =    0.0    ! cr74
       CASE (75)
          nuc_spin =    0.5    ! cr75
       CASE (76)
          nuc_spin =    0.0    ! cr76
       CASE (77)
          nuc_spin =    1.5    ! cr77
       CASE (78)
          nuc_spin =    0.0    ! cr78
       CASE (79)
          nuc_spin =    1.5    ! cr79
       CASE (80)
          nuc_spin =    0.0    ! cr80
       CASE (81)
          nuc_spin =    1.5    ! cr81
       CASE (82)
          nuc_spin =    0.0    ! cr82
       CASE (83)
          nuc_spin =    1.5    ! cr83
       CASE (84)
          nuc_spin =    0.0    ! cr84
       CASE (85)
          nuc_spin =    1.5    ! cr85
       CASE (86)
          nuc_spin =    0.0    ! cr86
       END SELECT
    CASE (25)
       SELECT CASE (a)
       CASE (40)
          nuc_spin =    2.0    ! mn40
       CASE (41)
          nuc_spin =    2.5    ! mn41
       CASE (42)
          nuc_spin =    2.0    ! mn42
       CASE (43)
          nuc_spin =    2.5    ! mn43
       CASE (44)
          nuc_spin =    2.0    ! mn44
       CASE (45)
          nuc_spin =    2.5    ! mn45
       CASE (46)
          nuc_spin =    4.0    ! mn46
       CASE (47)
          nuc_spin =    2.5    ! mn47
       CASE (48)
          nuc_spin =    4.0    ! mn48
       CASE (49)
          nuc_spin =    2.5    ! mn49
       CASE (50)
          nuc_spin =    0.0    ! mn50
       CASE (51)
          nuc_spin =    2.5    ! mn51
       CASE (52)
          nuc_spin =    6.0    ! mn52
       CASE (53)
          nuc_spin =    3.5    ! mn53
       CASE (54)
          nuc_spin =    3.0    ! mn54
       CASE (55)
          nuc_spin =    2.5    ! mn55
       CASE (56)
          nuc_spin =    3.0    ! mn56
       CASE (57)
          nuc_spin =    2.5    ! mn57
       CASE (58)
          nuc_spin =    3.0    ! mn58
       CASE (59)
          nuc_spin =    1.5    ! mn59
       CASE (60)
          nuc_spin =    3.0    ! mn60
       CASE (61)
          nuc_spin =    2.5    ! mn61
       CASE (62)
          nuc_spin =    3.0    ! mn62
       CASE (63)
          nuc_spin =    2.5    ! mn63
       CASE (64)
          nuc_spin =    1.0    ! mn64
       CASE (65)
          nuc_spin =    2.5    ! mn65
       CASE (66)
          nuc_spin =    0.0    ! mn66
       CASE (67)
          nuc_spin =    2.5    ! mn67
       CASE (68)
          nuc_spin =    3.0    ! mn68
       CASE (69)
          nuc_spin =    2.5    ! mn69
       CASE (70)
          nuc_spin =    0.0    ! mn70
       CASE (71)
          nuc_spin =    2.5    ! mn71
       CASE (72)
          nuc_spin =    4.0    ! mn72
       CASE (73)
          nuc_spin =    2.5    ! mn73
       CASE (74)
          nuc_spin =    2.0    ! mn74
       CASE (75)
          nuc_spin =    2.5    ! mn75
       CASE (76)
          nuc_spin =    2.0    ! mn76
       CASE (77)
          nuc_spin =    2.5    ! mn77
       CASE (78)
          nuc_spin =    3.0    ! mn78
       CASE (79)
          nuc_spin =    2.5    ! mn79
       CASE (80)
          nuc_spin =    3.0    ! mn80
       CASE (81)
          nuc_spin =    2.5    ! mn81
       CASE (82)
          nuc_spin =    3.0    ! mn82
       CASE (83)
          nuc_spin =    2.5    ! mn83
       CASE (84)
          nuc_spin =    1.0    ! mn84
       CASE (85)
          nuc_spin =    2.5    ! mn85
       CASE (86)
          nuc_spin =    3.0    ! mn86
       CASE (87)
          nuc_spin =    2.5    ! mn87
       CASE (88)
          nuc_spin =    3.0    ! mn88
       CASE (89)
          nuc_spin =    0.5    ! mn89
       END SELECT
    CASE (26)
       SELECT CASE (a)
       CASE (42)
          nuc_spin =    0.0    ! fe42
       CASE (43)
          nuc_spin =    0.5    ! fe43
       CASE (44)
          nuc_spin =    0.0    ! fe44
       CASE (45)
          nuc_spin =    1.5    ! fe45
       CASE (46)
          nuc_spin =    0.0    ! fe46
       CASE (47)
          nuc_spin =    2.5    ! fe47
       CASE (48)
          nuc_spin =    0.0    ! fe48
       CASE (49)
          nuc_spin =    3.5    ! fe49
       CASE (50)
          nuc_spin =    0.0    ! fe50
       CASE (51)
          nuc_spin =    2.5    ! fe51
       CASE (52)
          nuc_spin =    0.0    ! fe52
       CASE (53)
          nuc_spin =    3.5    ! fe53
       CASE (54)
          nuc_spin =    0.0    ! fe54
       CASE (55)
          nuc_spin =    1.5    ! fe55
       CASE (56)
          nuc_spin =    0.0    ! fe56
       CASE (57)
          nuc_spin =    0.5    ! fe57
       CASE (58)
          nuc_spin =    0.0    ! fe58
       CASE (59)
          nuc_spin =    1.5    ! fe59
       CASE (60)
          nuc_spin =    0.0    ! fe60
       CASE (61)
          nuc_spin =    1.5    ! fe61
       CASE (62)
          nuc_spin =    0.0    ! fe62
       CASE (63)
          nuc_spin =    2.5    ! fe63
       CASE (64)
          nuc_spin =    0.0    ! fe64
       CASE (65)
          nuc_spin =    0.5    ! fe65
       CASE (66)
          nuc_spin =    0.0    ! fe66
       CASE (67)
          nuc_spin =    0.5    ! fe67
       CASE (68)
          nuc_spin =    0.0    ! fe68
       CASE (69)
          nuc_spin =    1.5    ! fe69
       CASE (70)
          nuc_spin =    0.0    ! fe70
       CASE (71)
          nuc_spin =    2.5    ! fe71
       CASE (72)
          nuc_spin =    0.0    ! fe72
       CASE (73)
          nuc_spin =    3.5    ! fe73
       CASE (74)
          nuc_spin =    0.0    ! fe74
       CASE (75)
          nuc_spin =    4.5    ! fe75
       CASE (76)
          nuc_spin =    0.0    ! fe76
       CASE (77)
          nuc_spin =    0.5    ! fe77
       CASE (78)
          nuc_spin =    0.0    ! fe78
       CASE (79)
          nuc_spin =    1.5    ! fe79
       CASE (80)
          nuc_spin =    0.0    ! fe80
       CASE (81)
          nuc_spin =    0.5    ! fe81
       CASE (82)
          nuc_spin =    0.0    ! fe82
       CASE (83)
          nuc_spin =    1.5    ! fe83
       CASE (84)
          nuc_spin =    0.0    ! fe84
       CASE (85)
          nuc_spin =    1.5    ! fe85
       CASE (86)
          nuc_spin =    0.0    ! fe86
       CASE (87)
          nuc_spin =    1.5    ! fe87
       CASE (88)
          nuc_spin =    0.0    ! fe88
       CASE (89)
          nuc_spin =    2.5    ! fe89
       CASE (90)
          nuc_spin =    0.0    ! fe90
       CASE (91)
          nuc_spin =    0.5    ! fe91
       CASE (92)
          nuc_spin =    0.0    ! fe92
       END SELECT
    CASE (27)
       SELECT CASE (a)
       CASE (44)
          nuc_spin =    4.0    ! co44
       CASE (45)
          nuc_spin =    0.5    ! co45
       CASE (46)
          nuc_spin =    0.0    ! co46
       CASE (47)
          nuc_spin =    0.5    ! co47
       CASE (48)
          nuc_spin =    3.0    ! co48
       CASE (49)
          nuc_spin =    1.5    ! co49
       CASE (50)
          nuc_spin =    4.0    ! co50
       CASE (51)
          nuc_spin =    3.5    ! co51
       CASE (52)
          nuc_spin =    1.0    ! co52
       CASE (53)
          nuc_spin =    3.5    ! co53
       CASE (54)
          nuc_spin =    0.0    ! co54
       CASE (55)
          nuc_spin =    3.5    ! co55
       CASE (56)
          nuc_spin =    4.0    ! co56
       CASE (57)
          nuc_spin =    3.5    ! co57
       CASE (58)
          nuc_spin =    2.0    ! co58
       CASE (59)
          nuc_spin =    3.5    ! co59
       CASE (60)
          nuc_spin =    5.0    ! co60
       CASE (61)
          nuc_spin =    3.5    ! co61
       CASE (62)
          nuc_spin =    2.0    ! co62
       CASE (63)
          nuc_spin =    3.5    ! co63
       CASE (64)
          nuc_spin =    1.0    ! co64
       CASE (65)
          nuc_spin =    3.5    ! co65
       CASE (66)
          nuc_spin =    3.0    ! co66
       CASE (67)
          nuc_spin =    3.5    ! co67
       CASE (68)
          nuc_spin =    4.0    ! co68
       CASE (69)
          nuc_spin =    3.5    ! co69
       CASE (70)
          nuc_spin =    2.0    ! co70
       CASE (71)
          nuc_spin =    3.5    ! co71
       CASE (72)
          nuc_spin =    4.0    ! co72
       CASE (73)
          nuc_spin =    3.5    ! co73
       CASE (74)
          nuc_spin =    0.0    ! co74
       CASE (75)
          nuc_spin =    3.5    ! co75
       CASE (76)
          nuc_spin =    5.0    ! co76
       CASE (77)
          nuc_spin =    3.5    ! co77
       CASE (78)
          nuc_spin =    4.0    ! co78
       CASE (79)
          nuc_spin =    3.5    ! co79
       CASE (80)
          nuc_spin =    2.0    ! co80
       CASE (81)
          nuc_spin =    3.5    ! co81
       CASE (82)
          nuc_spin =    4.0    ! co82
       CASE (83)
          nuc_spin =    3.5    ! co83
       CASE (84)
          nuc_spin =    2.0    ! co84
       CASE (85)
          nuc_spin =    0.5    ! co85
       CASE (86)
          nuc_spin =    1.0    ! co86
       CASE (87)
          nuc_spin =    0.5    ! co87
       CASE (88)
          nuc_spin =    2.0    ! co88
       CASE (89)
          nuc_spin =    0.5    ! co89
       CASE (90)
          nuc_spin =    3.0    ! co90
       CASE (91)
          nuc_spin =    2.5    ! co91
       CASE (92)
          nuc_spin =    0.0    ! co92
       CASE (93)
          nuc_spin =    0.5    ! co93
       CASE (94)
          nuc_spin =    3.0    ! co94
       CASE (95)
          nuc_spin =    0.5    ! co95
       CASE (96)
          nuc_spin =    3.0    ! co96
       END SELECT
    CASE (28)
       SELECT CASE (a)
       CASE (46)
          nuc_spin =    0.0    ! ni46
       CASE (47)
          nuc_spin =    0.5    ! ni47
       CASE (48)
          nuc_spin =    0.0    ! ni48
       CASE (49)
          nuc_spin =    3.5    ! ni49
       CASE (50)
          nuc_spin =    0.0    ! ni50
       CASE (51)
          nuc_spin =    1.5    ! ni51
       CASE (52)
          nuc_spin =    0.0    ! ni52
       CASE (53)
          nuc_spin =    3.5    ! ni53
       CASE (54)
          nuc_spin =    0.0    ! ni54
       CASE (55)
          nuc_spin =    3.5    ! ni55
       CASE (56)
          nuc_spin =    0.0    ! ni56
       CASE (57)
          nuc_spin =    1.5    ! ni57
       CASE (58)
          nuc_spin =    0.0    ! ni58
       CASE (59)
          nuc_spin =    1.5    ! ni59
       CASE (60)
          nuc_spin =    0.0    ! ni60
       CASE (61)
          nuc_spin =    1.5    ! ni61
       CASE (62)
          nuc_spin =    0.0    ! ni62
       CASE (63)
          nuc_spin =    0.5    ! ni63
       CASE (64)
          nuc_spin =    0.0    ! ni64
       CASE (65)
          nuc_spin =    2.5    ! ni65
       CASE (66)
          nuc_spin =    0.0    ! ni66
       CASE (67)
          nuc_spin =    0.5    ! ni67
       CASE (68)
          nuc_spin =    0.0    ! ni68
       CASE (69)
          nuc_spin =    4.5    ! ni69
       CASE (70)
          nuc_spin =    0.0    ! ni70
       CASE (71)
          nuc_spin =    1.5    ! ni71
       CASE (72)
          nuc_spin =    0.0    ! ni72
       CASE (73)
          nuc_spin =    2.5    ! ni73
       CASE (74)
          nuc_spin =    0.0    ! ni74
       CASE (75)
          nuc_spin =    3.5    ! ni75
       CASE (76)
          nuc_spin =    0.0    ! ni76
       CASE (77)
          nuc_spin =    4.5    ! ni77
       CASE (78)
          nuc_spin =    0.0    ! ni78
       CASE (79)
          nuc_spin =    0.5    ! ni79
       CASE (80)
          nuc_spin =    0.0    ! ni80
       CASE (81)
          nuc_spin =    1.5    ! ni81
       CASE (82)
          nuc_spin =    0.0    ! ni82
       CASE (83)
          nuc_spin =    0.5    ! ni83
       CASE (84)
          nuc_spin =    0.0    ! ni84
       CASE (85)
          nuc_spin =    1.5    ! ni85
       CASE (86)
          nuc_spin =    0.0    ! ni86
       CASE (87)
          nuc_spin =    1.5    ! ni87
       CASE (88)
          nuc_spin =    0.0    ! ni88
       CASE (89)
          nuc_spin =    1.5    ! ni89
       CASE (90)
          nuc_spin =    0.0    ! ni90
       CASE (91)
          nuc_spin =    2.5    ! ni91
       CASE (92)
          nuc_spin =    0.0    ! ni92
       CASE (93)
          nuc_spin =    0.5    ! ni93
       CASE (94)
          nuc_spin =    0.0    ! ni94
       CASE (95)
          nuc_spin =    0.5    ! ni95
       CASE (96)
          nuc_spin =    0.0    ! ni96
       CASE (97)
          nuc_spin =    3.5    ! ni97
       CASE (98)
          nuc_spin =    0.0    ! ni98
       CASE (99)
          nuc_spin =    0.5    ! ni99
       END SELECT
    CASE (29)
       SELECT CASE (a)
       CASE (48)
          nuc_spin =    2.0    ! cu48
       CASE (49)
          nuc_spin =    1.5    ! cu49
       CASE (50)
          nuc_spin =    4.0    ! cu50
       CASE (51)
          nuc_spin =    1.5    ! cu51
       CASE (52)
          nuc_spin =    1.0    ! cu52
       CASE (53)
          nuc_spin =    0.5    ! cu53
       CASE (54)
          nuc_spin =    3.0    ! cu54
       CASE (55)
          nuc_spin =    0.5    ! cu55
       CASE (56)
          nuc_spin =    3.0    ! cu56
       CASE (57)
          nuc_spin =    1.5    ! cu57
       CASE (58)
          nuc_spin =    1.0    ! cu58
       CASE (59)
          nuc_spin =    1.5    ! cu59
       CASE (60)
          nuc_spin =    2.0    ! cu60
       CASE (61)
          nuc_spin =    1.5    ! cu61
       CASE (62)
          nuc_spin =    1.0    ! cu62
       CASE (63)
          nuc_spin =    1.5    ! cu63
       CASE (64)
          nuc_spin =    1.0    ! cu64
       CASE (65)
          nuc_spin =    1.5    ! cu65
       CASE (66)
          nuc_spin =    1.0    ! cu66
       CASE (67)
          nuc_spin =    1.5    ! cu67
       CASE (68)
          nuc_spin =    1.0    ! cu68
       CASE (69)
          nuc_spin =    1.5    ! cu69
       CASE (70)
          nuc_spin =    1.0    ! cu70
       CASE (71)
          nuc_spin =    1.5    ! cu71
       CASE (72)
          nuc_spin =    1.0    ! cu72
       CASE (73)
          nuc_spin =    0.5    ! cu73
       CASE (74)
          nuc_spin =    1.0    ! cu74
       CASE (75)
          nuc_spin =    0.5    ! cu75
       CASE (76)
          nuc_spin =    4.0    ! cu76
       CASE (77)
          nuc_spin =    0.5    ! cu77
       CASE (78)
          nuc_spin =    4.0    ! cu78
       CASE (79)
          nuc_spin =    0.5    ! cu79
       CASE (80)
          nuc_spin =    0.0    ! cu80
       CASE (81)
          nuc_spin =    0.5    ! cu81
       CASE (82)
          nuc_spin =    2.0    ! cu82
       CASE (83)
          nuc_spin =    0.5    ! cu83
       CASE (84)
          nuc_spin =    2.0    ! cu84
       CASE (85)
          nuc_spin =    0.5    ! cu85
       CASE (86)
          nuc_spin =    3.0    ! cu86
       CASE (87)
          nuc_spin =    3.5    ! cu87
       CASE (88)
          nuc_spin =    4.0    ! cu88
       CASE (89)
          nuc_spin =    3.5    ! cu89
       CASE (90)
          nuc_spin =    2.0    ! cu90
       CASE (91)
          nuc_spin =    3.5    ! cu91
       CASE (92)
          nuc_spin =    1.0    ! cu92
       CASE (93)
          nuc_spin =    3.5    ! cu93
       CASE (94)
          nuc_spin =    4.0    ! cu94
       CASE (95)
          nuc_spin =    3.5    ! cu95
       CASE (96)
          nuc_spin =    4.0    ! cu96
       CASE (97)
          nuc_spin =    3.5    ! cu97
       CASE (98)
          nuc_spin =    4.0    ! cu98
       CASE (99)
          nuc_spin =    3.5    ! cu99
       CASE (100)
          nuc_spin =    4.0    ! cu100
       CASE (101)
          nuc_spin =    3.5    ! cu101
       CASE (102)
          nuc_spin =    4.0    ! cu102
       END SELECT
    CASE (30)
       SELECT CASE (a)
       CASE (51)
          nuc_spin =    3.5    ! zn51
       CASE (52)
          nuc_spin =    0.0    ! zn52
       CASE (53)
          nuc_spin =    1.5    ! zn53
       CASE (54)
          nuc_spin =    0.0    ! zn54
       CASE (55)
          nuc_spin =    2.5    ! zn55
       CASE (56)
          nuc_spin =    0.0    ! zn56
       CASE (57)
          nuc_spin =    3.5    ! zn57
       CASE (58)
          nuc_spin =    0.0    ! zn58
       CASE (59)
          nuc_spin =    1.5    ! zn59
       CASE (60)
          nuc_spin =    0.0    ! zn60
       CASE (61)
          nuc_spin =    1.5    ! zn61
       CASE (62)
          nuc_spin =    0.0    ! zn62
       CASE (63)
          nuc_spin =    1.5    ! zn63
       CASE (64)
          nuc_spin =    0.0    ! zn64
       CASE (65)
          nuc_spin =    2.5    ! zn65
       CASE (66)
          nuc_spin =    0.0    ! zn66
       CASE (67)
          nuc_spin =    2.5    ! zn67
       CASE (68)
          nuc_spin =    0.0    ! zn68
       CASE (69)
          nuc_spin =    0.5    ! zn69
       CASE (70)
          nuc_spin =    0.0    ! zn70
       CASE (71)
          nuc_spin =    0.5    ! zn71
       CASE (72)
          nuc_spin =    0.0    ! zn72
       CASE (73)
          nuc_spin =    0.5    ! zn73
       CASE (74)
          nuc_spin =    0.0    ! zn74
       CASE (75)
          nuc_spin =    3.5    ! zn75
       CASE (76)
          nuc_spin =    0.0    ! zn76
       CASE (77)
          nuc_spin =    3.5    ! zn77
       CASE (78)
          nuc_spin =    0.0    ! zn78
       CASE (79)
          nuc_spin =    4.5    ! zn79
       CASE (80)
          nuc_spin =    0.0    ! zn80
       CASE (81)
          nuc_spin =    0.5    ! zn81
       CASE (82)
          nuc_spin =    0.0    ! zn82
       CASE (83)
          nuc_spin =    0.5    ! zn83
       CASE (84)
          nuc_spin =    0.0    ! zn84
       CASE (85)
          nuc_spin =    1.5    ! zn85
       CASE (86)
          nuc_spin =    0.0    ! zn86
       CASE (87)
          nuc_spin =    0.5    ! zn87
       CASE (88)
          nuc_spin =    0.0    ! zn88
       CASE (89)
          nuc_spin =    1.5    ! zn89
       CASE (90)
          nuc_spin =    0.0    ! zn90
       CASE (91)
          nuc_spin =    1.5    ! zn91
       CASE (92)
          nuc_spin =    0.0    ! zn92
       CASE (93)
          nuc_spin =    2.5    ! zn93
       CASE (94)
          nuc_spin =    0.0    ! zn94
       CASE (95)
          nuc_spin =    2.5    ! zn95
       CASE (96)
          nuc_spin =    0.0    ! zn96
       CASE (97)
          nuc_spin =    0.5    ! zn97
       CASE (98)
          nuc_spin =    0.0    ! zn98
       CASE (99)
          nuc_spin =    3.5    ! zn99
       CASE (100)
          nuc_spin =    0.0    ! zn100
       CASE (101)
          nuc_spin =    3.5    ! zn101
       CASE (102)
          nuc_spin =    0.0    ! zn102
       CASE (103)
          nuc_spin =    0.5    ! zn103
       CASE (104)
          nuc_spin =    0.0    ! zn104
       CASE (105)
          nuc_spin =    0.5    ! zn105
       END SELECT
    CASE (31)
       SELECT CASE (a)
       CASE (53)
          nuc_spin =    0.5    ! ga53
       CASE (54)
          nuc_spin =    1.0    ! ga54
       CASE (55)
          nuc_spin =    0.5    ! ga55
       CASE (56)
          nuc_spin =    3.0    ! ga56
       CASE (57)
          nuc_spin =    0.5    ! ga57
       CASE (58)
          nuc_spin =    3.0    ! ga58
       CASE (59)
          nuc_spin =    0.5    ! ga59
       CASE (60)
          nuc_spin =    1.0    ! ga60
       CASE (61)
          nuc_spin =    1.5    ! ga61
       CASE (62)
          nuc_spin =    0.0    ! ga62
       CASE (63)
          nuc_spin =    1.5    ! ga63
       CASE (64)
          nuc_spin =    0.0    ! ga64
       CASE (65)
          nuc_spin =    1.5    ! ga65
       CASE (66)
          nuc_spin =    0.0    ! ga66
       CASE (67)
          nuc_spin =    1.5    ! ga67
       CASE (68)
          nuc_spin =    1.0    ! ga68
       CASE (69)
          nuc_spin =    1.5    ! ga69
       CASE (70)
          nuc_spin =    1.0    ! ga70
       CASE (71)
          nuc_spin =    1.5    ! ga71
       CASE (72)
          nuc_spin =    3.0    ! ga72
       CASE (73)
          nuc_spin =    1.5    ! ga73
       CASE (74)
          nuc_spin =    3.0    ! ga74
       CASE (75)
          nuc_spin =    1.5    ! ga75
       CASE (76)
          nuc_spin =    2.0    ! ga76
       CASE (77)
          nuc_spin =    1.5    ! ga77
       CASE (78)
          nuc_spin =    3.0    ! ga78
       CASE (79)
          nuc_spin =    1.5    ! ga79
       CASE (80)
          nuc_spin =    0.0    ! ga80
       CASE (81)
          nuc_spin =    2.5    ! ga81
       CASE (82)
          nuc_spin =    0.0    ! ga82
       CASE (83)
          nuc_spin =    0.5    ! ga83
       CASE (84)
          nuc_spin =    0.0    ! ga84
       CASE (85)
          nuc_spin =    0.5    ! ga85
       CASE (86)
          nuc_spin =    2.0    ! ga86
       CASE (87)
          nuc_spin =    1.5    ! ga87
       CASE (88)
          nuc_spin =    0.0    ! ga88
       CASE (89)
          nuc_spin =    1.5    ! ga89
       CASE (90)
          nuc_spin =    2.0    ! ga90
       CASE (91)
          nuc_spin =    0.5    ! ga91
       CASE (92)
          nuc_spin =    2.0    ! ga92
       CASE (93)
          nuc_spin =    0.5    ! ga93
       CASE (94)
          nuc_spin =    2.0    ! ga94
       CASE (95)
          nuc_spin =    0.5    ! ga95
       CASE (96)
          nuc_spin =    3.0    ! ga96
       CASE (97)
          nuc_spin =    1.5    ! ga97
       CASE (98)
          nuc_spin =    1.0    ! ga98
       CASE (99)
          nuc_spin =    1.5    ! ga99
       CASE (100)
          nuc_spin =    4.0    ! ga100
       CASE (101)
          nuc_spin =    1.5    ! ga101
       CASE (102)
          nuc_spin =    3.0    ! ga102
       CASE (103)
          nuc_spin =    1.5    ! ga103
       CASE (104)
          nuc_spin =    2.0    ! ga104
       CASE (105)
          nuc_spin =    1.5    ! ga105
       CASE (106)
          nuc_spin =    2.0    ! ga106
       CASE (107)
          nuc_spin =    1.5    ! ga107
       CASE (108)
          nuc_spin =    2.0    ! ga108
       END SELECT
    CASE (32)
       SELECT CASE (a)
       CASE (55)
          nuc_spin =    1.5    ! ge55
       CASE (56)
          nuc_spin =    0.0    ! ge56
       CASE (57)
          nuc_spin =    2.5    ! ge57
       CASE (58)
          nuc_spin =    0.0    ! ge58
       CASE (59)
          nuc_spin =    3.5    ! ge59
       CASE (60)
          nuc_spin =    0.0    ! ge60
       CASE (61)
          nuc_spin =    1.5    ! ge61
       CASE (62)
          nuc_spin =    0.0    ! ge62
       CASE (63)
          nuc_spin =    0.5    ! ge63
       CASE (64)
          nuc_spin =    0.0    ! ge64
       CASE (65)
          nuc_spin =    1.5    ! ge65
       CASE (66)
          nuc_spin =    0.0    ! ge66
       CASE (67)
          nuc_spin =    0.5    ! ge67
       CASE (68)
          nuc_spin =    0.0    ! ge68
       CASE (69)
          nuc_spin =    2.5    ! ge69
       CASE (70)
          nuc_spin =    0.0    ! ge70
       CASE (71)
          nuc_spin =    0.5    ! ge71
       CASE (72)
          nuc_spin =    0.0    ! ge72
       CASE (73)
          nuc_spin =    4.5    ! ge73
       CASE (74)
          nuc_spin =    0.0    ! ge74
       CASE (75)
          nuc_spin =    0.5    ! ge75
       CASE (76)
          nuc_spin =    0.0    ! ge76
       CASE (77)
          nuc_spin =    3.5    ! ge77
       CASE (78)
          nuc_spin =    0.0    ! ge78
       CASE (79)
          nuc_spin =    0.5    ! ge79
       CASE (80)
          nuc_spin =    0.0    ! ge80
       CASE (81)
          nuc_spin =    4.5    ! ge81
       CASE (82)
          nuc_spin =    0.0    ! ge82
       CASE (83)
          nuc_spin =    2.5    ! ge83
       CASE (84)
          nuc_spin =    0.0    ! ge84
       CASE (85)
          nuc_spin =    0.5    ! ge85
       CASE (86)
          nuc_spin =    0.0    ! ge86
       CASE (87)
          nuc_spin =    1.5    ! ge87
       CASE (88)
          nuc_spin =    0.0    ! ge88
       CASE (89)
          nuc_spin =    1.5    ! ge89
       CASE (90)
          nuc_spin =    0.0    ! ge90
       CASE (91)
          nuc_spin =    1.5    ! ge91
       CASE (92)
          nuc_spin =    0.0    ! ge92
       CASE (93)
          nuc_spin =    1.5    ! ge93
       CASE (94)
          nuc_spin =    0.0    ! ge94
       CASE (95)
          nuc_spin =    2.5    ! ge95
       CASE (96)
          nuc_spin =    0.0    ! ge96
       CASE (97)
          nuc_spin =    2.5    ! ge97
       CASE (98)
          nuc_spin =    0.0    ! ge98
       CASE (99)
          nuc_spin =    2.5    ! ge99
       CASE (100)
          nuc_spin =    0.0    ! ge100
       CASE (101)
          nuc_spin =    3.5    ! ge101
       CASE (102)
          nuc_spin =    0.0    ! ge102
       CASE (103)
          nuc_spin =    2.5    ! ge103
       CASE (104)
          nuc_spin =    0.0    ! ge104
       CASE (105)
          nuc_spin =    4.5    ! ge105
       CASE (106)
          nuc_spin =    0.0    ! ge106
       CASE (107)
          nuc_spin =    0.5    ! ge107
       CASE (108)
          nuc_spin =    0.0    ! ge108
       CASE (109)
          nuc_spin =    1.5    ! ge109
       CASE (110)
          nuc_spin =    0.0    ! ge110
       CASE (111)
          nuc_spin =    0.5    ! ge111
       CASE (112)
          nuc_spin =    0.0    ! ge112
       END SELECT
    CASE (33)
       SELECT CASE (a)
       CASE (57)
          nuc_spin =    1.5    ! as57
       CASE (58)
          nuc_spin =    1.0    ! as58
       CASE (59)
          nuc_spin =    1.5    ! as59
       CASE (60)
          nuc_spin =    4.0    ! as60
       CASE (61)
          nuc_spin =    0.5    ! as61
       CASE (62)
          nuc_spin =    1.0    ! as62
       CASE (63)
          nuc_spin =    1.5    ! as63
       CASE (64)
          nuc_spin =    1.0    ! as64
       CASE (65)
          nuc_spin =    1.5    ! as65
       CASE (66)
          nuc_spin =    2.0    ! as66
       CASE (67)
          nuc_spin =    2.5    ! as67
       CASE (68)
          nuc_spin =    3.0    ! as68
       CASE (69)
          nuc_spin =    2.5    ! as69
       CASE (70)
          nuc_spin =    0.0    ! as70
       CASE (71)
          nuc_spin =    2.5    ! as71
       CASE (72)
          nuc_spin =    2.0    ! as72
       CASE (73)
          nuc_spin =    1.5    ! as73
       CASE (74)
          nuc_spin =    2.0    ! as74
       CASE (75)
          nuc_spin =    1.5    ! as75
       CASE (76)
          nuc_spin =    2.0    ! as76
       CASE (77)
          nuc_spin =    1.5    ! as77
       CASE (78)
          nuc_spin =    2.0    ! as78
       CASE (79)
          nuc_spin =    1.5    ! as79
       CASE (80)
          nuc_spin =    1.0    ! as80
       CASE (81)
          nuc_spin =    1.5    ! as81
       CASE (82)
          nuc_spin =    1.0    ! as82
       CASE (83)
          nuc_spin =    2.5    ! as83
       CASE (84)
          nuc_spin =    0.0    ! as84
       CASE (85)
          nuc_spin =    1.5    ! as85
       CASE (86)
          nuc_spin =    2.0    ! as86
       CASE (87)
          nuc_spin =    1.5    ! as87
       CASE (88)
          nuc_spin =    2.0    ! as88
       CASE (89)
          nuc_spin =    0.5    ! as89
       CASE (90)
          nuc_spin =    2.0    ! as90
       CASE (91)
          nuc_spin =    0.5    ! as91
       CASE (92)
          nuc_spin =    1.0    ! as92
       CASE (93)
          nuc_spin =    0.5    ! as93
       CASE (94)
          nuc_spin =    2.0    ! as94
       CASE (95)
          nuc_spin =    1.5    ! as95
       CASE (96)
          nuc_spin =    1.0    ! as96
       CASE (97)
          nuc_spin =    1.5    ! as97
       CASE (98)
          nuc_spin =    3.0    ! as98
       CASE (99)
          nuc_spin =    0.5    ! as99
       CASE (100)
          nuc_spin =    3.0    ! as100
       CASE (101)
          nuc_spin =    0.5    ! as101
       CASE (102)
          nuc_spin =    3.0    ! as102
       CASE (103)
          nuc_spin =    0.5    ! as103
       CASE (104)
          nuc_spin =    2.0    ! as104
       CASE (105)
          nuc_spin =    0.5    ! as105
       CASE (106)
          nuc_spin =    5.0    ! as106
       CASE (107)
          nuc_spin =    0.5    ! as107
       CASE (108)
          nuc_spin =    4.0    ! as108
       CASE (109)
          nuc_spin =    0.5    ! as109
       CASE (110)
          nuc_spin =    5.0    ! as110
       CASE (111)
          nuc_spin =    0.5    ! as111
       CASE (112)
          nuc_spin =    1.0    ! as112
       CASE (113)
          nuc_spin =    0.5    ! as113
       CASE (114)
          nuc_spin =    5.0    ! as114
       CASE (115)
          nuc_spin =    2.5    ! as115
       END SELECT
    CASE (34)
       SELECT CASE (a)
       CASE (59)
          nuc_spin =    2.5    ! se59
       CASE (60)
          nuc_spin =    0.0    ! se60
       CASE (61)
          nuc_spin =    3.5    ! se61
       CASE (62)
          nuc_spin =    0.0    ! se62
       CASE (63)
          nuc_spin =    0.5    ! se63
       CASE (64)
          nuc_spin =    0.0    ! se64
       CASE (65)
          nuc_spin =    0.5    ! se65
       CASE (66)
          nuc_spin =    0.0    ! se66
       CASE (67)
          nuc_spin =    1.5    ! se67
       CASE (68)
          nuc_spin =    0.0    ! se68
       CASE (69)
          nuc_spin =    1.5    ! se69
       CASE (70)
          nuc_spin =    0.0    ! se70
       CASE (71)
          nuc_spin =    1.5    ! se71
       CASE (72)
          nuc_spin =    0.0    ! se72
       CASE (73)
          nuc_spin =    4.5    ! se73
       CASE (74)
          nuc_spin =    0.0    ! se74
       CASE (75)
          nuc_spin =    2.5    ! se75
       CASE (76)
          nuc_spin =    0.0    ! se76
       CASE (77)
          nuc_spin =    0.5    ! se77
       CASE (78)
          nuc_spin =    0.0    ! se78
       CASE (79)
          nuc_spin =    3.5    ! se79
       CASE (80)
          nuc_spin =    0.0    ! se80
       CASE (81)
          nuc_spin =    0.5    ! se81
       CASE (82)
          nuc_spin =    0.0    ! se82
       CASE (83)
          nuc_spin =    4.5    ! se83
       CASE (84)
          nuc_spin =    0.0    ! se84
       CASE (85)
          nuc_spin =    2.5    ! se85
       CASE (86)
          nuc_spin =    0.0    ! se86
       CASE (87)
          nuc_spin =    2.5    ! se87
       CASE (88)
          nuc_spin =    0.0    ! se88
       CASE (89)
          nuc_spin =    2.5    ! se89
       CASE (90)
          nuc_spin =    0.0    ! se90
       CASE (91)
          nuc_spin =    1.5    ! se91
       CASE (92)
          nuc_spin =    0.0    ! se92
       CASE (93)
          nuc_spin =    1.5    ! se93
       CASE (94)
          nuc_spin =    0.0    ! se94
       CASE (95)
          nuc_spin =    1.5    ! se95
       CASE (96)
          nuc_spin =    0.0    ! se96
       CASE (97)
          nuc_spin =    2.5    ! se97
       CASE (98)
          nuc_spin =    0.0    ! se98
       CASE (99)
          nuc_spin =    2.5    ! se99
       CASE (100)
          nuc_spin =    0.0    ! se100
       CASE (101)
          nuc_spin =    2.5    ! se101
       CASE (102)
          nuc_spin =    0.0    ! se102
       CASE (103)
          nuc_spin =    3.5    ! se103
       CASE (104)
          nuc_spin =    0.0    ! se104
       CASE (105)
          nuc_spin =    2.5    ! se105
       CASE (106)
          nuc_spin =    0.0    ! se106
       CASE (107)
          nuc_spin =    4.5    ! se107
       CASE (108)
          nuc_spin =    0.0    ! se108
       CASE (109)
          nuc_spin =    3.5    ! se109
       CASE (110)
          nuc_spin =    0.0    ! se110
       CASE (111)
          nuc_spin =    4.5    ! se111
       CASE (112)
          nuc_spin =    0.0    ! se112
       CASE (113)
          nuc_spin =    0.5    ! se113
       CASE (114)
          nuc_spin =    0.0    ! se114
       CASE (115)
          nuc_spin =    5.5    ! se115
       CASE (116)
          nuc_spin =    0.0    ! se116
       CASE (117)
          nuc_spin =    3.5    ! se117
       CASE (118)
          nuc_spin =    0.0    ! se118
       END SELECT
    CASE (35)
       SELECT CASE (a)
       CASE (61)
          nuc_spin =    0.5    ! br61
       CASE (62)
          nuc_spin =    0.0    ! br62
       CASE (63)
          nuc_spin =    4.5    ! br63
       CASE (64)
          nuc_spin =    5.0    ! br64
       CASE (65)
          nuc_spin =    4.5    ! br65
       CASE (66)
          nuc_spin =    2.0    ! br66
       CASE (67)
          nuc_spin =    4.5    ! br67
       CASE (68)
          nuc_spin =    4.0    ! br68
       CASE (69)
          nuc_spin =    4.5    ! br69
       CASE (70)
          nuc_spin =    5.0    ! br70
       CASE (71)
          nuc_spin =    2.5    ! br71
       CASE (72)
          nuc_spin =    3.0    ! br72
       CASE (73)
          nuc_spin =    0.5    ! br73
       CASE (74)
          nuc_spin =    0.0    ! br74
       CASE (75)
          nuc_spin =    1.5    ! br75
       CASE (76)
          nuc_spin =    1.0    ! br76
       CASE (77)
          nuc_spin =    1.5    ! br77
       CASE (78)
          nuc_spin =    1.0    ! br78
       CASE (79)
          nuc_spin =    1.5    ! br79
       CASE (80)
          nuc_spin =    1.0    ! br80
       CASE (81)
          nuc_spin =    1.5    ! br81
       CASE (82)
          nuc_spin =    5.0    ! br82
       CASE (83)
          nuc_spin =    1.5    ! br83
       CASE (84)
          nuc_spin =    2.0    ! br84
       CASE (85)
          nuc_spin =    1.5    ! br85
       CASE (86)
          nuc_spin =    2.0    ! br86
       CASE (87)
          nuc_spin =    1.5    ! br87
       CASE (88)
          nuc_spin =    0.0    ! br88
       CASE (89)
          nuc_spin =    1.5    ! br89
       CASE (90)
          nuc_spin =    1.0    ! br90
       CASE (91)
          nuc_spin =    0.5    ! br91
       CASE (92)
          nuc_spin =    2.0    ! br92
       CASE (93)
          nuc_spin =    2.5    ! br93
       CASE (94)
          nuc_spin =    1.0    ! br94
       CASE (95)
          nuc_spin =    0.5    ! br95
       CASE (96)
          nuc_spin =    2.0    ! br96
       CASE (97)
          nuc_spin =    0.5    ! br97
       CASE (98)
          nuc_spin =    3.0    ! br98
       CASE (99)
          nuc_spin =    0.5    ! br99
       CASE (100)
          nuc_spin =    3.0    ! br100
       CASE (101)
          nuc_spin =    0.5    ! br101
       CASE (102)
          nuc_spin =    2.0    ! br102
       CASE (103)
          nuc_spin =    0.5    ! br103
       CASE (104)
          nuc_spin =    4.0    ! br104
       CASE (105)
          nuc_spin =    0.5    ! br105
       CASE (106)
          nuc_spin =    3.0    ! br106
       CASE (107)
          nuc_spin =    0.5    ! br107
       CASE (108)
          nuc_spin =    4.0    ! br108
       CASE (109)
          nuc_spin =    0.5    ! br109
       CASE (110)
          nuc_spin =    4.0    ! br110
       CASE (111)
          nuc_spin =    4.5    ! br111
       CASE (112)
          nuc_spin =    2.0    ! br112
       CASE (113)
          nuc_spin =    1.5    ! br113
       CASE (114)
          nuc_spin =    1.0    ! br114
       CASE (115)
          nuc_spin =    1.5    ! br115
       CASE (116)
          nuc_spin =    5.0    ! br116
       CASE (117)
          nuc_spin =    0.5    ! br117
       CASE (118)
          nuc_spin =    4.0    ! br118
       CASE (119)
          nuc_spin =    0.5    ! br119
       CASE (120)
          nuc_spin =    1.0    ! br120
       CASE (121)
          nuc_spin =    2.5    ! br121
       END SELECT
    CASE (36)
       SELECT CASE (a)
       CASE (63)
          nuc_spin =    0.5    ! kr63
       CASE (64)
          nuc_spin =    0.0    ! kr64
       CASE (65)
          nuc_spin =    1.5    ! kr65
       CASE (66)
          nuc_spin =    0.0    ! kr66
       CASE (67)
          nuc_spin =    2.5    ! kr67
       CASE (68)
          nuc_spin =    0.0    ! kr68
       CASE (69)
          nuc_spin =    0.5    ! kr69
       CASE (70)
          nuc_spin =    0.0    ! kr70
       CASE (71)
          nuc_spin =    4.5    ! kr71
       CASE (72)
          nuc_spin =    0.0    ! kr72
       CASE (73)
          nuc_spin =    2.5    ! kr73
       CASE (74)
          nuc_spin =    0.0    ! kr74
       CASE (75)
          nuc_spin =    2.5    ! kr75
       CASE (76)
          nuc_spin =    0.0    ! kr76
       CASE (77)
          nuc_spin =    2.5    ! kr77
       CASE (78)
          nuc_spin =    0.0    ! kr78
       CASE (79)
          nuc_spin =    0.5    ! kr79
       CASE (80)
          nuc_spin =    0.0    ! kr80
       CASE (81)
          nuc_spin =    3.5    ! kr81
       CASE (82)
          nuc_spin =    0.0    ! kr82
       CASE (83)
          nuc_spin =    4.5    ! kr83
       CASE (84)
          nuc_spin =    0.0    ! kr84
       CASE (85)
          nuc_spin =    4.5    ! kr85
       CASE (86)
          nuc_spin =    0.0    ! kr86
       CASE (87)
          nuc_spin =    2.5    ! kr87
       CASE (88)
          nuc_spin =    0.0    ! kr88
       CASE (89)
          nuc_spin =    1.5    ! kr89
       CASE (90)
          nuc_spin =    0.0    ! kr90
       CASE (91)
          nuc_spin =    2.5    ! kr91
       CASE (92)
          nuc_spin =    0.0    ! kr92
       CASE (93)
          nuc_spin =    0.5    ! kr93
       CASE (94)
          nuc_spin =    0.0    ! kr94
       CASE (95)
          nuc_spin =    1.5    ! kr95
       CASE (96)
          nuc_spin =    0.0    ! kr96
       CASE (97)
          nuc_spin =    1.5    ! kr97
       CASE (98)
          nuc_spin =    0.0    ! kr98
       CASE (99)
          nuc_spin =    2.5    ! kr99
       CASE (100)
          nuc_spin =    0.0    ! kr100
       CASE (101)
          nuc_spin =    2.5    ! kr101
       CASE (102)
          nuc_spin =    0.0    ! kr102
       CASE (103)
          nuc_spin =    0.5    ! kr103
       CASE (104)
          nuc_spin =    0.0    ! kr104
       CASE (105)
          nuc_spin =    3.5    ! kr105
       CASE (106)
          nuc_spin =    0.0    ! kr106
       CASE (107)
          nuc_spin =    2.5    ! kr107
       CASE (108)
          nuc_spin =    0.0    ! kr108
       CASE (109)
          nuc_spin =    4.5    ! kr109
       CASE (110)
          nuc_spin =    0.0    ! kr110
       CASE (111)
          nuc_spin =    0.5    ! kr111
       CASE (112)
          nuc_spin =    0.0    ! kr112
       CASE (113)
          nuc_spin =    1.5    ! kr113
       CASE (114)
          nuc_spin =    0.0    ! kr114
       CASE (115)
          nuc_spin =    0.5    ! kr115
       CASE (116)
          nuc_spin =    0.0    ! kr116
       CASE (117)
          nuc_spin =    5.5    ! kr117
       CASE (118)
          nuc_spin =    0.0    ! kr118
       CASE (119)
          nuc_spin =    3.5    ! kr119
       CASE (120)
          nuc_spin =    0.0    ! kr120
       CASE (121)
          nuc_spin =    1.5    ! kr121
       CASE (122)
          nuc_spin =    0.0    ! kr122
       CASE (123)
          nuc_spin =    0.5    ! kr123
       CASE (124)
          nuc_spin =    0.0    ! kr124
       END SELECT
    CASE (37)
       SELECT CASE (a)
       CASE (66)
          nuc_spin =    2.0    ! rb66
       CASE (67)
          nuc_spin =    1.5    ! rb67
       CASE (68)
          nuc_spin =    2.0    ! rb68
       CASE (69)
          nuc_spin =    1.5    ! rb69
       CASE (70)
          nuc_spin =    4.0    ! rb70
       CASE (71)
          nuc_spin =    3.5    ! rb71
       CASE (72)
          nuc_spin =    1.0    ! rb72
       CASE (73)
          nuc_spin =    1.5    ! rb73
       CASE (74)
          nuc_spin =    0.0    ! rb74
       CASE (75)
          nuc_spin =    1.5    ! rb75
       CASE (76)
          nuc_spin =    0.0    ! rb76
       CASE (77)
          nuc_spin =    1.5    ! rb77
       CASE (78)
          nuc_spin =    0.0    ! rb78
       CASE (79)
          nuc_spin =    2.5    ! rb79
       CASE (80)
          nuc_spin =    1.0    ! rb80
       CASE (81)
          nuc_spin =    1.5    ! rb81
       CASE (82)
          nuc_spin =    1.0    ! rb82
       CASE (83)
          nuc_spin =    2.5    ! rb83
       CASE (84)
          nuc_spin =    2.0    ! rb84
       CASE (85)
          nuc_spin =    2.5    ! rb85
       CASE (86)
          nuc_spin =    2.0    ! rb86
       CASE (87)
          nuc_spin =    1.5    ! rb87
       CASE (88)
          nuc_spin =    2.0    ! rb88
       CASE (89)
          nuc_spin =    1.5    ! rb89
       CASE (90)
          nuc_spin =    0.0    ! rb90
       CASE (91)
          nuc_spin =    1.5    ! rb91
       CASE (92)
          nuc_spin =    0.0    ! rb92
       CASE (93)
          nuc_spin =    2.5    ! rb93
       CASE (94)
          nuc_spin =    0.0    ! rb94
       CASE (95)
          nuc_spin =    2.5    ! rb95
       CASE (96)
          nuc_spin =    2.0    ! rb96
       CASE (97)
          nuc_spin =    1.5    ! rb97
       CASE (98)
          nuc_spin =    2.0    ! rb98
       CASE (99)
          nuc_spin =    2.5    ! rb99
       CASE (100)
          nuc_spin =    3.0    ! rb100
       CASE (101)
          nuc_spin =    1.5    ! rb101
       CASE (102)
          nuc_spin =    1.0    ! rb102
       CASE (103)
          nuc_spin =    1.5    ! rb103
       CASE (104)
          nuc_spin =    1.0    ! rb104
       CASE (105)
          nuc_spin =    1.5    ! rb105
       CASE (106)
          nuc_spin =    2.0    ! rb106
       CASE (107)
          nuc_spin =    1.5    ! rb107
       CASE (108)
          nuc_spin =    1.0    ! rb108
       CASE (109)
          nuc_spin =    1.5    ! rb109
       CASE (110)
          nuc_spin =    5.0    ! rb110
       CASE (111)
          nuc_spin =    3.5    ! rb111
       CASE (112)
          nuc_spin =    4.0    ! rb112
       CASE (113)
          nuc_spin =    0.5    ! rb113
       CASE (114)
          nuc_spin =    1.0    ! rb114
       CASE (115)
          nuc_spin =    0.5    ! rb115
       CASE (116)
          nuc_spin =    1.0    ! rb116
       CASE (117)
          nuc_spin =    0.5    ! rb117
       CASE (118)
          nuc_spin =    6.0    ! rb118
       CASE (119)
          nuc_spin =    1.5    ! rb119
       CASE (120)
          nuc_spin =    3.0    ! rb120
       CASE (121)
          nuc_spin =    0.5    ! rb121
       CASE (122)
          nuc_spin =    2.0    ! rb122
       CASE (123)
          nuc_spin =    1.5    ! rb123
       CASE (124)
          nuc_spin =    1.0    ! rb124
       CASE (125)
          nuc_spin =    1.5    ! rb125
       CASE (126)
          nuc_spin =    2.0    ! rb126
       CASE (127)
          nuc_spin =    1.5    ! rb127
       CASE (128)
          nuc_spin =    2.0    ! rb128
       END SELECT
    CASE (38)
       SELECT CASE (a)
       CASE (68)
          nuc_spin =    0.0    ! sr68
       CASE (69)
          nuc_spin =    0.5    ! sr69
       CASE (70)
          nuc_spin =    0.0    ! sr70
       CASE (71)
          nuc_spin =    1.5    ! sr71
       CASE (72)
          nuc_spin =    0.0    ! sr72
       CASE (73)
          nuc_spin =    1.5    ! sr73
       CASE (74)
          nuc_spin =    0.0    ! sr74
       CASE (75)
          nuc_spin =    1.5    ! sr75
       CASE (76)
          nuc_spin =    0.0    ! sr76
       CASE (77)
          nuc_spin =    2.5    ! sr77
       CASE (78)
          nuc_spin =    0.0    ! sr78
       CASE (79)
          nuc_spin =    1.5    ! sr79
       CASE (80)
          nuc_spin =    0.0    ! sr80
       CASE (81)
          nuc_spin =    0.5    ! sr81
       CASE (82)
          nuc_spin =    0.0    ! sr82
       CASE (83)
          nuc_spin =    3.5    ! sr83
       CASE (84)
          nuc_spin =    0.0    ! sr84
       CASE (85)
          nuc_spin =    4.5    ! sr85
       CASE (86)
          nuc_spin =    0.0    ! sr86
       CASE (87)
          nuc_spin =    4.5    ! sr87
       CASE (88)
          nuc_spin =    0.0    ! sr88
       CASE (89)
          nuc_spin =    2.5    ! sr89
       CASE (90)
          nuc_spin =    0.0    ! sr90
       CASE (91)
          nuc_spin =    2.5    ! sr91
       CASE (92)
          nuc_spin =    0.0    ! sr92
       CASE (93)
          nuc_spin =    2.5    ! sr93
       CASE (94)
          nuc_spin =    0.0    ! sr94
       CASE (95)
          nuc_spin =    0.5    ! sr95
       CASE (96)
          nuc_spin =    0.0    ! sr96
       CASE (97)
          nuc_spin =    0.5    ! sr97
       CASE (98)
          nuc_spin =    0.0    ! sr98
       CASE (99)
          nuc_spin =    1.5    ! sr99
       CASE (100)
          nuc_spin =    0.0    ! sr100
       CASE (101)
          nuc_spin =    2.5    ! sr101
       CASE (102)
          nuc_spin =    0.0    ! sr102
       CASE (103)
          nuc_spin =    2.5    ! sr103
       CASE (104)
          nuc_spin =    0.0    ! sr104
       CASE (105)
          nuc_spin =    0.5    ! sr105
       CASE (106)
          nuc_spin =    0.0    ! sr106
       CASE (107)
          nuc_spin =    3.5    ! sr107
       CASE (108)
          nuc_spin =    0.0    ! sr108
       CASE (109)
          nuc_spin =    0.5    ! sr109
       CASE (110)
          nuc_spin =    0.0    ! sr110
       CASE (111)
          nuc_spin =    2.5    ! sr111
       CASE (112)
          nuc_spin =    0.0    ! sr112
       CASE (113)
          nuc_spin =    2.5    ! sr113
       CASE (114)
          nuc_spin =    0.0    ! sr114
       CASE (115)
          nuc_spin =    1.5    ! sr115
       CASE (116)
          nuc_spin =    0.0    ! sr116
       CASE (117)
          nuc_spin =    0.5    ! sr117
       CASE (118)
          nuc_spin =    0.0    ! sr118
       CASE (119)
          nuc_spin =    0.5    ! sr119
       CASE (120)
          nuc_spin =    0.0    ! sr120
       CASE (121)
          nuc_spin =    3.5    ! sr121
       CASE (122)
          nuc_spin =    0.0    ! sr122
       CASE (123)
          nuc_spin =    2.5    ! sr123
       CASE (124)
          nuc_spin =    0.0    ! sr124
       CASE (125)
          nuc_spin =    2.5    ! sr125
       CASE (126)
          nuc_spin =    0.0    ! sr126
       CASE (127)
          nuc_spin =    0.5    ! sr127
       CASE (128)
          nuc_spin =    0.0    ! sr128
       CASE (129)
          nuc_spin =    1.5    ! sr129
       CASE (130)
          nuc_spin =    0.0    ! sr130
       CASE (131)
          nuc_spin =    1.5    ! sr131
       END SELECT
    CASE (39)
       SELECT CASE (a)
       CASE (70)
          nuc_spin =    2.0    ! y70
       CASE (71)
          nuc_spin =    2.5    ! y71
       CASE (72)
          nuc_spin =    3.0    ! y72
       CASE (73)
          nuc_spin =    2.5    ! y73
       CASE (74)
          nuc_spin =    3.0    ! y74
       CASE (75)
          nuc_spin =    2.5    ! y75
       CASE (76)
          nuc_spin =    1.0    ! y76
       CASE (77)
          nuc_spin =    2.5    ! y77
       CASE (78)
          nuc_spin =    3.0    ! y78
       CASE (79)
          nuc_spin =    2.5    ! y79
       CASE (80)
          nuc_spin =    3.0    ! y80
       CASE (81)
          nuc_spin =    2.5    ! y81
       CASE (82)
          nuc_spin =    1.0    ! y82
       CASE (83)
          nuc_spin =    4.5    ! y83
       CASE (84)
          nuc_spin =    1.0    ! y84
       CASE (85)
          nuc_spin =    0.5    ! y85
       CASE (86)
          nuc_spin =    4.0    ! y86
       CASE (87)
          nuc_spin =    0.5    ! y87
       CASE (88)
          nuc_spin =    4.0    ! y88
       CASE (89)
          nuc_spin =    0.5    ! y89
       CASE (90)
          nuc_spin =    2.0    ! y90
       CASE (91)
          nuc_spin =    0.5    ! y91
       CASE (92)
          nuc_spin =    2.0    ! y92
       CASE (93)
          nuc_spin =    0.5    ! y93
       CASE (94)
          nuc_spin =    2.0    ! y94
       CASE (95)
          nuc_spin =    0.5    ! y95
       CASE (96)
          nuc_spin =    0.0    ! y96
       CASE (97)
          nuc_spin =    0.5    ! y97
       CASE (98)
          nuc_spin =    0.0    ! y98
       CASE (99)
          nuc_spin =    2.5    ! y99
       CASE (100)
          nuc_spin =    1.0    ! y100
       CASE (101)
          nuc_spin =    2.5    ! y101
       CASE (102)
          nuc_spin =    1.0    ! y102
       CASE (103)
          nuc_spin =    2.5    ! y103
       CASE (104)
          nuc_spin =    3.0    ! y104
       CASE (105)
          nuc_spin =    2.5    ! y105
       CASE (106)
          nuc_spin =    3.0    ! y106
       CASE (107)
          nuc_spin =    2.5    ! y107
       CASE (108)
          nuc_spin =    4.0    ! y108
       CASE (109)
          nuc_spin =    2.5    ! y109
       CASE (110)
          nuc_spin =    2.0    ! y110
       CASE (111)
          nuc_spin =    2.5    ! y111
       CASE (112)
          nuc_spin =    3.0    ! y112
       CASE (113)
          nuc_spin =    2.5    ! y113
       CASE (114)
          nuc_spin =    2.0    ! y114
       CASE (115)
          nuc_spin =    4.5    ! y115
       CASE (116)
          nuc_spin =    5.0    ! y116
       CASE (117)
          nuc_spin =    4.5    ! y117
       CASE (118)
          nuc_spin =    4.0    ! y118
       CASE (119)
          nuc_spin =    4.5    ! y119
       CASE (120)
          nuc_spin =    3.0    ! y120
       CASE (121)
          nuc_spin =    0.5    ! y121
       CASE (122)
          nuc_spin =    3.0    ! y122
       CASE (123)
          nuc_spin =    0.5    ! y123
       CASE (124)
          nuc_spin =    3.0    ! y124
       CASE (125)
          nuc_spin =    0.5    ! y125
       CASE (126)
          nuc_spin =    3.0    ! y126
       CASE (127)
          nuc_spin =    0.5    ! y127
       CASE (128)
          nuc_spin =    1.0    ! y128
       CASE (129)
          nuc_spin =    1.5    ! y129
       CASE (130)
          nuc_spin =    3.0    ! y130
       CASE (131)
          nuc_spin =    2.5    ! y131
       CASE (132)
          nuc_spin =    1.0    ! y132
       CASE (133)
          nuc_spin =    2.5    ! y133
       CASE (134)
          nuc_spin =    0.0    ! y134
       END SELECT
    CASE (40)
       SELECT CASE (a)
       CASE (72)
          nuc_spin =    0.0    ! zr72
       CASE (73)
          nuc_spin =    1.5    ! zr73
       CASE (74)
          nuc_spin =    0.0    ! zr74
       CASE (75)
          nuc_spin =    1.5    ! zr75
       CASE (76)
          nuc_spin =    0.0    ! zr76
       CASE (77)
          nuc_spin =    1.5    ! zr77
       CASE (78)
          nuc_spin =    0.0    ! zr78
       CASE (79)
          nuc_spin =    2.5    ! zr79
       CASE (80)
          nuc_spin =    0.0    ! zr80
       CASE (81)
          nuc_spin =    0.5    ! zr81
       CASE (82)
          nuc_spin =    0.0    ! zr82
       CASE (83)
          nuc_spin =    0.5    ! zr83
       CASE (84)
          nuc_spin =    0.0    ! zr84
       CASE (85)
          nuc_spin =    3.5    ! zr85
       CASE (86)
          nuc_spin =    0.0    ! zr86
       CASE (87)
          nuc_spin =    4.5    ! zr87
       CASE (88)
          nuc_spin =    0.0    ! zr88
       CASE (89)
          nuc_spin =    4.5    ! zr89
       CASE (90)
          nuc_spin =    0.0    ! zr90
       CASE (91)
          nuc_spin =    2.5    ! zr91
       CASE (92)
          nuc_spin =    0.0    ! zr92
       CASE (93)
          nuc_spin =    2.5    ! zr93
       CASE (94)
          nuc_spin =    0.0    ! zr94
       CASE (95)
          nuc_spin =    2.5    ! zr95
       CASE (96)
          nuc_spin =    0.0    ! zr96
       CASE (97)
          nuc_spin =    0.5    ! zr97
       CASE (98)
          nuc_spin =    0.0    ! zr98
       CASE (99)
          nuc_spin =    0.5    ! zr99
       CASE (100)
          nuc_spin =    0.0    ! zr100
       CASE (101)
          nuc_spin =    1.5    ! zr101
       CASE (102)
          nuc_spin =    0.0    ! zr102
       CASE (103)
          nuc_spin =    2.5    ! zr103
       CASE (104)
          nuc_spin =    0.0    ! zr104
       CASE (105)
          nuc_spin =    2.5    ! zr105
       CASE (106)
          nuc_spin =    0.0    ! zr106
       CASE (107)
          nuc_spin =    0.5    ! zr107
       CASE (108)
          nuc_spin =    0.0    ! zr108
       CASE (109)
          nuc_spin =    3.5    ! zr109
       CASE (110)
          nuc_spin =    0.0    ! zr110
       CASE (111)
          nuc_spin =    0.5    ! zr111
       CASE (112)
          nuc_spin =    0.0    ! zr112
       CASE (113)
          nuc_spin =    2.5    ! zr113
       CASE (114)
          nuc_spin =    0.0    ! zr114
       CASE (115)
          nuc_spin =    2.5    ! zr115
       CASE (116)
          nuc_spin =    0.0    ! zr116
       CASE (117)
          nuc_spin =    1.5    ! zr117
       CASE (118)
          nuc_spin =    0.0    ! zr118
       CASE (119)
          nuc_spin =    0.5    ! zr119
       CASE (120)
          nuc_spin =    0.0    ! zr120
       CASE (121)
          nuc_spin =    3.5    ! zr121
       CASE (122)
          nuc_spin =    0.0    ! zr122
       CASE (123)
          nuc_spin =    2.5    ! zr123
       CASE (124)
          nuc_spin =    0.0    ! zr124
       CASE (125)
          nuc_spin =    1.5    ! zr125
       CASE (126)
          nuc_spin =    0.0    ! zr126
       CASE (127)
          nuc_spin =    0.5    ! zr127
       CASE (128)
          nuc_spin =    0.0    ! zr128
       CASE (129)
          nuc_spin =    0.5    ! zr129
       CASE (130)
          nuc_spin =    0.0    ! zr130
       CASE (131)
          nuc_spin =    1.5    ! zr131
       CASE (132)
          nuc_spin =    0.0    ! zr132
       CASE (133)
          nuc_spin =    1.5    ! zr133
       CASE (134)
          nuc_spin =    0.0    ! zr134
       CASE (135)
          nuc_spin =    2.5    ! zr135
       CASE (136)
          nuc_spin =    0.0    ! zr136
       CASE (137)
          nuc_spin =    2.5    ! zr137
       END SELECT
    CASE (41)
       SELECT CASE (a)
       CASE (74)
          nuc_spin =    4.0    ! nb74
       CASE (75)
          nuc_spin =    0.5    ! nb75
       CASE (76)
          nuc_spin =    2.0    ! nb76
       CASE (77)
          nuc_spin =    0.5    ! nb77
       CASE (78)
          nuc_spin =    1.0    ! nb78
       CASE (79)
          nuc_spin =    0.5    ! nb79
       CASE (80)
          nuc_spin =    3.0    ! nb80
       CASE (81)
          nuc_spin =    0.5    ! nb81
       CASE (82)
          nuc_spin =    1.0    ! nb82
       CASE (83)
          nuc_spin =    2.5    ! nb83
       CASE (84)
          nuc_spin =    3.0    ! nb84
       CASE (85)
          nuc_spin =    4.5    ! nb85
       CASE (86)
          nuc_spin =    5.0    ! nb86
       CASE (87)
          nuc_spin =    4.5    ! nb87
       CASE (88)
          nuc_spin =    8.0    ! nb88
       CASE (89)
          nuc_spin =    4.5    ! nb89
       CASE (90)
          nuc_spin =    8.0    ! nb90
       CASE (91)
          nuc_spin =    4.5    ! nb91
       CASE (92)
          nuc_spin =    7.0    ! nb92
       CASE (93)
          nuc_spin =    4.5    ! nb93
       CASE (94)
          nuc_spin =    6.0    ! nb94
       CASE (95)
          nuc_spin =    4.5    ! nb95
       CASE (96)
          nuc_spin =    6.0    ! nb96
       CASE (97)
          nuc_spin =    4.5    ! nb97
       CASE (98)
          nuc_spin =    1.0    ! nb98
       CASE (99)
          nuc_spin =    4.5    ! nb99
       CASE (100)
          nuc_spin =    1.0    ! nb100
       CASE (101)
          nuc_spin =    0.5    ! nb101
       CASE (102)
          nuc_spin =    1.0    ! nb102
       CASE (103)
          nuc_spin =    2.5    ! nb103
       CASE (104)
          nuc_spin =    1.0    ! nb104
       CASE (105)
          nuc_spin =    2.5    ! nb105
       CASE (106)
          nuc_spin =    3.0    ! nb106
       CASE (107)
          nuc_spin =    0.5    ! nb107
       CASE (108)
          nuc_spin =    2.0    ! nb108
       CASE (109)
          nuc_spin =    2.5    ! nb109
       CASE (110)
          nuc_spin =    1.0    ! nb110
       CASE (111)
          nuc_spin =    2.5    ! nb111
       CASE (112)
          nuc_spin =    0.0    ! nb112
       CASE (113)
          nuc_spin =    0.5    ! nb113
       CASE (114)
          nuc_spin =    4.0    ! nb114
       CASE (115)
          nuc_spin =    3.5    ! nb115
       CASE (116)
          nuc_spin =    4.0    ! nb116
       CASE (117)
          nuc_spin =    3.5    ! nb117
       CASE (118)
          nuc_spin =    2.0    ! nb118
       CASE (119)
          nuc_spin =    3.5    ! nb119
       CASE (120)
          nuc_spin =    4.0    ! nb120
       CASE (121)
          nuc_spin =    3.5    ! nb121
       CASE (122)
          nuc_spin =    4.0    ! nb122
       CASE (123)
          nuc_spin =    0.5    ! nb123
       CASE (124)
          nuc_spin =    5.0    ! nb124
       CASE (125)
          nuc_spin =    4.5    ! nb125
       CASE (126)
          nuc_spin =    2.0    ! nb126
       CASE (127)
          nuc_spin =    0.5    ! nb127
       CASE (128)
          nuc_spin =    1.0    ! nb128
       CASE (129)
          nuc_spin =    1.5    ! nb129
       CASE (130)
          nuc_spin =    1.0    ! nb130
       CASE (131)
          nuc_spin =    1.5    ! nb131
       CASE (132)
          nuc_spin =    0.0    ! nb132
       CASE (133)
          nuc_spin =    1.5    ! nb133
       CASE (134)
          nuc_spin =    2.0    ! nb134
       CASE (135)
          nuc_spin =    1.5    ! nb135
       CASE (136)
          nuc_spin =    3.0    ! nb136
       CASE (137)
          nuc_spin =    2.5    ! nb137
       CASE (138)
          nuc_spin =    0.0    ! nb138
       CASE (139)
          nuc_spin =    2.5    ! nb139
       CASE (140)
          nuc_spin =    3.0    ! nb140
       END SELECT
    CASE (42)
       SELECT CASE (a)
       CASE (77)
          nuc_spin =    4.5    ! mo77
       CASE (78)
          nuc_spin =    0.0    ! mo78
       CASE (79)
          nuc_spin =    1.5    ! mo79
       CASE (80)
          nuc_spin =    0.0    ! mo80
       CASE (81)
          nuc_spin =    2.5    ! mo81
       CASE (82)
          nuc_spin =    0.0    ! mo82
       CASE (83)
          nuc_spin =    3.5    ! mo83
       CASE (84)
          nuc_spin =    0.0    ! mo84
       CASE (85)
          nuc_spin =    1.5    ! mo85
       CASE (86)
          nuc_spin =    0.0    ! mo86
       CASE (87)
          nuc_spin =    3.5    ! mo87
       CASE (88)
          nuc_spin =    0.0    ! mo88
       CASE (89)
          nuc_spin =    4.5    ! mo89
       CASE (90)
          nuc_spin =    0.0    ! mo90
       CASE (91)
          nuc_spin =    4.5    ! mo91
       CASE (92)
          nuc_spin =    0.0    ! mo92
       CASE (93)
          nuc_spin =    2.5    ! mo93
       CASE (94)
          nuc_spin =    0.0    ! mo94
       CASE (95)
          nuc_spin =    2.5    ! mo95
       CASE (96)
          nuc_spin =    0.0    ! mo96
       CASE (97)
          nuc_spin =    2.5    ! mo97
       CASE (98)
          nuc_spin =    0.0    ! mo98
       CASE (99)
          nuc_spin =    0.5    ! mo99
       CASE (100)
          nuc_spin =    0.0    ! mo100
       CASE (101)
          nuc_spin =    0.5    ! mo101
       CASE (102)
          nuc_spin =    0.0    ! mo102
       CASE (103)
          nuc_spin =    1.5    ! mo103
       CASE (104)
          nuc_spin =    0.0    ! mo104
       CASE (105)
          nuc_spin =    2.5    ! mo105
       CASE (106)
          nuc_spin =    0.0    ! mo106
       CASE (107)
          nuc_spin =    2.5    ! mo107
       CASE (108)
          nuc_spin =    0.0    ! mo108
       CASE (109)
          nuc_spin =    0.5    ! mo109
       CASE (110)
          nuc_spin =    0.0    ! mo110
       CASE (111)
          nuc_spin =    3.5    ! mo111
       CASE (112)
          nuc_spin =    0.0    ! mo112
       CASE (113)
          nuc_spin =    2.5    ! mo113
       CASE (114)
          nuc_spin =    0.0    ! mo114
       CASE (115)
          nuc_spin =    0.5    ! mo115
       CASE (116)
          nuc_spin =    0.0    ! mo116
       CASE (117)
          nuc_spin =    2.5    ! mo117
       CASE (118)
          nuc_spin =    0.0    ! mo118
       CASE (119)
          nuc_spin =    1.5    ! mo119
       CASE (120)
          nuc_spin =    0.0    ! mo120
       CASE (121)
          nuc_spin =    0.5    ! mo121
       CASE (122)
          nuc_spin =    0.0    ! mo122
       CASE (123)
          nuc_spin =    5.5    ! mo123
       CASE (124)
          nuc_spin =    0.0    ! mo124
       CASE (125)
          nuc_spin =    3.5    ! mo125
       CASE (126)
          nuc_spin =    0.0    ! mo126
       CASE (127)
          nuc_spin =    1.5    ! mo127
       CASE (128)
          nuc_spin =    0.0    ! mo128
       CASE (129)
          nuc_spin =    1.5    ! mo129
       CASE (130)
          nuc_spin =    0.0    ! mo130
       CASE (131)
          nuc_spin =    0.5    ! mo131
       CASE (132)
          nuc_spin =    0.0    ! mo132
       CASE (133)
          nuc_spin =    1.5    ! mo133
       CASE (134)
          nuc_spin =    0.0    ! mo134
       CASE (135)
          nuc_spin =    1.5    ! mo135
       CASE (136)
          nuc_spin =    0.0    ! mo136
       CASE (137)
          nuc_spin =    2.5    ! mo137
       CASE (138)
          nuc_spin =    0.0    ! mo138
       CASE (139)
          nuc_spin =    2.5    ! mo139
       CASE (140)
          nuc_spin =    0.0    ! mo140
       CASE (141)
          nuc_spin =    0.5    ! mo141
       CASE (142)
          nuc_spin =    0.0    ! mo142
       CASE (143)
          nuc_spin =    3.5    ! mo143
       CASE (144)
          nuc_spin =    0.0    ! mo144
       END SELECT
    CASE (43)
       SELECT CASE (a)
       CASE (79)
          nuc_spin =    2.5    ! tc79
       CASE (80)
          nuc_spin =    3.0    ! tc80
       CASE (81)
          nuc_spin =    2.5    ! tc81
       CASE (82)
          nuc_spin =    5.0    ! tc82
       CASE (83)
          nuc_spin =    2.5    ! tc83
       CASE (84)
          nuc_spin =    1.0    ! tc84
       CASE (85)
          nuc_spin =    1.5    ! tc85
       CASE (86)
          nuc_spin =    2.0    ! tc86
       CASE (87)
          nuc_spin =    1.5    ! tc87
       CASE (88)
          nuc_spin =    1.0    ! tc88
       CASE (89)
          nuc_spin =    1.5    ! tc89
       CASE (90)
          nuc_spin =    1.0    ! tc90
       CASE (91)
          nuc_spin =    4.5    ! tc91
       CASE (92)
          nuc_spin =    8.0    ! tc92
       CASE (93)
          nuc_spin =    4.5    ! tc93
       CASE (94)
          nuc_spin =    7.0    ! tc94
       CASE (95)
          nuc_spin =    4.5    ! tc95
       CASE (96)
          nuc_spin =    7.0    ! tc96
       CASE (97)
          nuc_spin =    4.5    ! tc97
       CASE (98)
          nuc_spin =    6.0    ! tc98
       CASE (99)
          nuc_spin =    4.5    ! tc99
       CASE (100)
          nuc_spin =    1.0    ! tc100
       CASE (101)
          nuc_spin =    4.5    ! tc101
       CASE (102)
          nuc_spin =    1.0    ! tc102
       CASE (103)
          nuc_spin =    2.5    ! tc103
       CASE (104)
          nuc_spin =    3.0    ! tc104
       CASE (105)
          nuc_spin =    1.5    ! tc105
       CASE (106)
          nuc_spin =    1.0    ! tc106
       CASE (107)
          nuc_spin =    1.5    ! tc107
       CASE (108)
          nuc_spin =    2.0    ! tc108
       CASE (109)
          nuc_spin =    2.5    ! tc109
       CASE (110)
          nuc_spin =    2.0    ! tc110
       CASE (111)
          nuc_spin =    2.5    ! tc111
       CASE (112)
          nuc_spin =    4.0    ! tc112
       CASE (113)
          nuc_spin =    2.5    ! tc113
       CASE (114)
          nuc_spin =    0.0    ! tc114
       CASE (115)
          nuc_spin =    2.5    ! tc115
       CASE (116)
          nuc_spin =    3.0    ! tc116
       CASE (117)
          nuc_spin =    2.5    ! tc117
       CASE (118)
          nuc_spin =    0.0    ! tc118
       CASE (119)
          nuc_spin =    2.5    ! tc119
       CASE (120)
          nuc_spin =    3.0    ! tc120
       CASE (121)
          nuc_spin =    2.5    ! tc121
       CASE (122)
          nuc_spin =    2.0    ! tc122
       CASE (123)
          nuc_spin =    2.5    ! tc123
       CASE (124)
          nuc_spin =    4.0    ! tc124
       CASE (125)
          nuc_spin =    3.5    ! tc125
       CASE (126)
          nuc_spin =    0.0    ! tc126
       CASE (127)
          nuc_spin =    4.5    ! tc127
       CASE (128)
          nuc_spin =    0.0    ! tc128
       CASE (129)
          nuc_spin =    1.5    ! tc129
       CASE (130)
          nuc_spin =    1.0    ! tc130
       CASE (131)
          nuc_spin =    0.5    ! tc131
       CASE (132)
          nuc_spin =    0.0    ! tc132
       CASE (133)
          nuc_spin =    2.5    ! tc133
       CASE (134)
          nuc_spin =    1.0    ! tc134
       CASE (135)
          nuc_spin =    2.5    ! tc135
       CASE (136)
          nuc_spin =    3.0    ! tc136
       CASE (137)
          nuc_spin =    2.5    ! tc137
       CASE (138)
          nuc_spin =    3.0    ! tc138
       CASE (139)
          nuc_spin =    1.5    ! tc139
       CASE (140)
          nuc_spin =    1.0    ! tc140
       CASE (141)
          nuc_spin =    1.5    ! tc141
       CASE (142)
          nuc_spin =    1.0    ! tc142
       CASE (143)
          nuc_spin =    1.5    ! tc143
       CASE (144)
          nuc_spin =    2.0    ! tc144
       CASE (145)
          nuc_spin =    1.5    ! tc145
       CASE (146)
          nuc_spin =    1.0    ! tc146
       CASE (147)
          nuc_spin =    1.5    ! tc147
       END SELECT
    CASE (44)
       SELECT CASE (a)
       CASE (81)
          nuc_spin =    0.5    ! ru81
       CASE (82)
          nuc_spin =    0.0    ! ru82
       CASE (83)
          nuc_spin =    0.5    ! ru83
       CASE (84)
          nuc_spin =    0.0    ! ru84
       CASE (85)
          nuc_spin =    0.5    ! ru85
       CASE (86)
          nuc_spin =    0.0    ! ru86
       CASE (87)
          nuc_spin =    1.5    ! ru87
       CASE (88)
          nuc_spin =    0.0    ! ru88
       CASE (89)
          nuc_spin =    2.5    ! ru89
       CASE (90)
          nuc_spin =    0.0    ! ru90
       CASE (91)
          nuc_spin =    4.5    ! ru91
       CASE (92)
          nuc_spin =    0.0    ! ru92
       CASE (93)
          nuc_spin =    4.5    ! ru93
       CASE (94)
          nuc_spin =    0.0    ! ru94
       CASE (95)
          nuc_spin =    2.5    ! ru95
       CASE (96)
          nuc_spin =    0.0    ! ru96
       CASE (97)
          nuc_spin =    2.5    ! ru97
       CASE (98)
          nuc_spin =    0.0    ! ru98
       CASE (99)
          nuc_spin =    2.5    ! ru99
       CASE (100)
          nuc_spin =    0.0    ! ru100
       CASE (101)
          nuc_spin =    2.5    ! ru101
       CASE (102)
          nuc_spin =    0.0    ! ru102
       CASE (103)
          nuc_spin =    1.5    ! ru103
       CASE (104)
          nuc_spin =    0.0    ! ru104
       CASE (105)
          nuc_spin =    1.5    ! ru105
       CASE (106)
          nuc_spin =    0.0    ! ru106
       CASE (107)
          nuc_spin =    2.5    ! ru107
       CASE (108)
          nuc_spin =    0.0    ! ru108
       CASE (109)
          nuc_spin =    2.5    ! ru109
       CASE (110)
          nuc_spin =    0.0    ! ru110
       CASE (111)
          nuc_spin =    4.5    ! ru111
       CASE (112)
          nuc_spin =    0.0    ! ru112
       CASE (113)
          nuc_spin =    3.5    ! ru113
       CASE (114)
          nuc_spin =    0.0    ! ru114
       CASE (115)
          nuc_spin =    2.5    ! ru115
       CASE (116)
          nuc_spin =    0.0    ! ru116
       CASE (117)
          nuc_spin =    0.5    ! ru117
       CASE (118)
          nuc_spin =    0.0    ! ru118
       CASE (119)
          nuc_spin =    2.5    ! ru119
       CASE (120)
          nuc_spin =    0.0    ! ru120
       CASE (121)
          nuc_spin =    1.5    ! ru121
       CASE (122)
          nuc_spin =    0.0    ! ru122
       CASE (123)
          nuc_spin =    0.5    ! ru123
       CASE (124)
          nuc_spin =    0.0    ! ru124
       CASE (125)
          nuc_spin =    5.5    ! ru125
       CASE (126)
          nuc_spin =    0.0    ! ru126
       CASE (127)
          nuc_spin =    3.5    ! ru127
       CASE (128)
          nuc_spin =    0.0    ! ru128
       CASE (129)
          nuc_spin =    1.5    ! ru129
       CASE (130)
          nuc_spin =    0.0    ! ru130
       CASE (131)
          nuc_spin =    0.5    ! ru131
       CASE (132)
          nuc_spin =    0.0    ! ru132
       CASE (133)
          nuc_spin =    0.5    ! ru133
       CASE (134)
          nuc_spin =    0.0    ! ru134
       CASE (135)
          nuc_spin =    1.5    ! ru135
       CASE (136)
          nuc_spin =    0.0    ! ru136
       CASE (137)
          nuc_spin =    1.5    ! ru137
       CASE (138)
          nuc_spin =    0.0    ! ru138
       CASE (139)
          nuc_spin =    2.5    ! ru139
       CASE (140)
          nuc_spin =    0.0    ! ru140
       CASE (141)
          nuc_spin =    2.5    ! ru141
       CASE (142)
          nuc_spin =    0.0    ! ru142
       CASE (143)
          nuc_spin =    0.5    ! ru143
       CASE (144)
          nuc_spin =    0.0    ! ru144
       CASE (145)
          nuc_spin =    3.5    ! ru145
       CASE (146)
          nuc_spin =    0.0    ! ru146
       CASE (147)
          nuc_spin =    2.5    ! ru147
       CASE (148)
          nuc_spin =    0.0    ! ru148
       CASE (149)
          nuc_spin =    3.5    ! ru149
       CASE (150)
          nuc_spin =    0.0    ! ru150
       END SELECT
    CASE (45)
       SELECT CASE (a)
       CASE (83)
          nuc_spin =    1.5    ! rh83
       CASE (84)
          nuc_spin =    2.0    ! rh84
       CASE (85)
          nuc_spin =    2.5    ! rh85
       CASE (86)
          nuc_spin =    3.0    ! rh86
       CASE (87)
          nuc_spin =    2.5    ! rh87
       CASE (88)
          nuc_spin =    1.0    ! rh88
       CASE (89)
          nuc_spin =    2.5    ! rh89
       CASE (90)
          nuc_spin =    3.0    ! rh90
       CASE (91)
          nuc_spin =    2.5    ! rh91
       CASE (92)
          nuc_spin =    1.0    ! rh92
       CASE (93)
          nuc_spin =    2.5    ! rh93
       CASE (94)
          nuc_spin =    3.0    ! rh94
       CASE (95)
          nuc_spin =    4.5    ! rh95
       CASE (96)
          nuc_spin =    6.0    ! rh96
       CASE (97)
          nuc_spin =    4.5    ! rh97
       CASE (98)
          nuc_spin =    2.0    ! rh98
       CASE (99)
          nuc_spin =    0.5    ! rh99
       CASE (100)
          nuc_spin =    1.0    ! rh100
       CASE (101)
          nuc_spin =    0.5    ! rh101
       CASE (102)
          nuc_spin =    1.0    ! rh102
       CASE (103)
          nuc_spin =    0.5    ! rh103
       CASE (104)
          nuc_spin =    1.0    ! rh104
       CASE (105)
          nuc_spin =    3.5    ! rh105
       CASE (106)
          nuc_spin =    1.0    ! rh106
       CASE (107)
          nuc_spin =    3.5    ! rh107
       CASE (108)
          nuc_spin =    5.0    ! rh108
       CASE (109)
          nuc_spin =    3.5    ! rh109
       CASE (110)
          nuc_spin =    1.0    ! rh110
       CASE (111)
          nuc_spin =    3.5    ! rh111
       CASE (112)
          nuc_spin =    1.0    ! rh112
       CASE (113)
          nuc_spin =    1.5    ! rh113
       CASE (114)
          nuc_spin =    1.0    ! rh114
       CASE (115)
          nuc_spin =    3.5    ! rh115
       CASE (116)
          nuc_spin =    1.0    ! rh116
       CASE (117)
          nuc_spin =    3.5    ! rh117
       CASE (118)
          nuc_spin =    1.0    ! rh118
       CASE (119)
          nuc_spin =    1.5    ! rh119
       CASE (120)
          nuc_spin =    3.0    ! rh120
       CASE (121)
          nuc_spin =    1.5    ! rh121
       CASE (122)
          nuc_spin =    0.0    ! rh122
       CASE (123)
          nuc_spin =    1.5    ! rh123
       CASE (124)
          nuc_spin =    2.0    ! rh124
       CASE (125)
          nuc_spin =    2.5    ! rh125
       CASE (126)
          nuc_spin =    6.0    ! rh126
       CASE (127)
          nuc_spin =    2.5    ! rh127
       CASE (128)
          nuc_spin =    4.0    ! rh128
       CASE (129)
          nuc_spin =    4.5    ! rh129
       CASE (130)
          nuc_spin =    3.0    ! rh130
       CASE (131)
          nuc_spin =    2.5    ! rh131
       CASE (132)
          nuc_spin =    2.0    ! rh132
       CASE (133)
          nuc_spin =    2.5    ! rh133
       CASE (134)
          nuc_spin =    3.0    ! rh134
       CASE (135)
          nuc_spin =    2.5    ! rh135
       CASE (136)
          nuc_spin =    2.0    ! rh136
       CASE (137)
          nuc_spin =    0.5    ! rh137
       CASE (138)
          nuc_spin =    1.0    ! rh138
       CASE (139)
          nuc_spin =    0.5    ! rh139
       CASE (140)
          nuc_spin =    3.0    ! rh140
       CASE (141)
          nuc_spin =    0.5    ! rh141
       CASE (142)
          nuc_spin =    2.0    ! rh142
       CASE (143)
          nuc_spin =    0.5    ! rh143
       CASE (144)
          nuc_spin =    0.0    ! rh144
       CASE (145)
          nuc_spin =    0.5    ! rh145
       CASE (146)
          nuc_spin =    3.0    ! rh146
       CASE (147)
          nuc_spin =    0.5    ! rh147
       CASE (148)
          nuc_spin =    2.0    ! rh148
       CASE (149)
          nuc_spin =    3.5    ! rh149
       CASE (150)
          nuc_spin =    0.0    ! rh150
       CASE (151)
          nuc_spin =    3.5    ! rh151
       CASE (152)
          nuc_spin =    4.0    ! rh152
       CASE (153)
          nuc_spin =    3.5    ! rh153
       END SELECT
    CASE (46)
       SELECT CASE (a)
       CASE (86)
          nuc_spin =    0.0    ! pd86
       CASE (87)
          nuc_spin =    0.5    ! pd87
       CASE (88)
          nuc_spin =    0.0    ! pd88
       CASE (89)
          nuc_spin =    1.5    ! pd89
       CASE (90)
          nuc_spin =    0.0    ! pd90
       CASE (91)
          nuc_spin =    2.5    ! pd91
       CASE (92)
          nuc_spin =    0.0    ! pd92
       CASE (93)
          nuc_spin =    3.5    ! pd93
       CASE (94)
          nuc_spin =    0.0    ! pd94
       CASE (95)
          nuc_spin =    4.5    ! pd95
       CASE (96)
          nuc_spin =    0.0    ! pd96
       CASE (97)
          nuc_spin =    2.5    ! pd97
       CASE (98)
          nuc_spin =    0.0    ! pd98
       CASE (99)
          nuc_spin =    2.5    ! pd99
       CASE (100)
          nuc_spin =    0.0    ! pd100
       CASE (101)
          nuc_spin =    2.5    ! pd101
       CASE (102)
          nuc_spin =    0.0    ! pd102
       CASE (103)
          nuc_spin =    2.5    ! pd103
       CASE (104)
          nuc_spin =    0.0    ! pd104
       CASE (105)
          nuc_spin =    2.5    ! pd105
       CASE (106)
          nuc_spin =    0.0    ! pd106
       CASE (107)
          nuc_spin =    2.5    ! pd107
       CASE (108)
          nuc_spin =    0.0    ! pd108
       CASE (109)
          nuc_spin =    2.5    ! pd109
       CASE (110)
          nuc_spin =    0.0    ! pd110
       CASE (111)
          nuc_spin =    2.5    ! pd111
       CASE (112)
          nuc_spin =    0.0    ! pd112
       CASE (113)
          nuc_spin =    2.5    ! pd113
       CASE (114)
          nuc_spin =    0.0    ! pd114
       CASE (115)
          nuc_spin =    2.5    ! pd115
       CASE (116)
          nuc_spin =    0.0    ! pd116
       CASE (117)
          nuc_spin =    2.5    ! pd117
       CASE (118)
          nuc_spin =    0.0    ! pd118
       CASE (119)
          nuc_spin =    0.5    ! pd119
       CASE (120)
          nuc_spin =    0.0    ! pd120
       CASE (121)
          nuc_spin =    2.5    ! pd121
       CASE (122)
          nuc_spin =    0.0    ! pd122
       CASE (123)
          nuc_spin =    1.5    ! pd123
       CASE (124)
          nuc_spin =    0.0    ! pd124
       CASE (125)
          nuc_spin =    4.5    ! pd125
       CASE (126)
          nuc_spin =    0.0    ! pd126
       CASE (127)
          nuc_spin =    5.5    ! pd127
       CASE (128)
          nuc_spin =    0.0    ! pd128
       CASE (129)
          nuc_spin =    3.5    ! pd129
       CASE (130)
          nuc_spin =    0.0    ! pd130
       CASE (131)
          nuc_spin =    1.5    ! pd131
       CASE (132)
          nuc_spin =    0.0    ! pd132
       CASE (133)
          nuc_spin =    0.5    ! pd133
       CASE (134)
          nuc_spin =    0.0    ! pd134
       CASE (135)
          nuc_spin =    1.5    ! pd135
       CASE (136)
          nuc_spin =    0.0    ! pd136
       CASE (137)
          nuc_spin =    1.5    ! pd137
       CASE (138)
          nuc_spin =    0.0    ! pd138
       CASE (139)
          nuc_spin =    1.5    ! pd139
       CASE (140)
          nuc_spin =    0.0    ! pd140
       CASE (141)
          nuc_spin =    2.5    ! pd141
       CASE (142)
          nuc_spin =    0.0    ! pd142
       CASE (143)
          nuc_spin =    2.5    ! pd143
       CASE (144)
          nuc_spin =    0.0    ! pd144
       CASE (145)
          nuc_spin =    0.5    ! pd145
       CASE (146)
          nuc_spin =    0.0    ! pd146
       CASE (147)
          nuc_spin =    3.5    ! pd147
       CASE (148)
          nuc_spin =    0.0    ! pd148
       CASE (149)
          nuc_spin =    2.5    ! pd149
       CASE (150)
          nuc_spin =    0.0    ! pd150
       CASE (151)
          nuc_spin =    3.5    ! pd151
       CASE (152)
          nuc_spin =    0.0    ! pd152
       CASE (153)
          nuc_spin =    0.5    ! pd153
       CASE (154)
          nuc_spin =    0.0    ! pd154
       CASE (155)
          nuc_spin =    0.5    ! pd155
       CASE (156)
          nuc_spin =    0.0    ! pd156
       END SELECT
    CASE (47)
       SELECT CASE (a)
       CASE (88)
          nuc_spin =    3.0    ! ag88
       CASE (89)
          nuc_spin =    3.5    ! ag89
       CASE (90)
          nuc_spin =    4.0    ! ag90
       CASE (91)
          nuc_spin =    3.5    ! ag91
       CASE (92)
          nuc_spin =    1.0    ! ag92
       CASE (93)
          nuc_spin =    3.5    ! ag93
       CASE (94)
          nuc_spin =    4.0    ! ag94
       CASE (95)
          nuc_spin =    3.5    ! ag95
       CASE (96)
          nuc_spin =    1.0    ! ag96
       CASE (97)
          nuc_spin =    4.5    ! ag97
       CASE (98)
          nuc_spin =    5.0    ! ag98
       CASE (99)
          nuc_spin =    4.5    ! ag99
       CASE (100)
          nuc_spin =    5.0    ! ag100
       CASE (101)
          nuc_spin =    4.5    ! ag101
       CASE (102)
          nuc_spin =    5.0    ! ag102
       CASE (103)
          nuc_spin =    3.5    ! ag103
       CASE (104)
          nuc_spin =    5.0    ! ag104
       CASE (105)
          nuc_spin =    0.5    ! ag105
       CASE (106)
          nuc_spin =    1.0    ! ag106
       CASE (107)
          nuc_spin =    0.5    ! ag107
       CASE (108)
          nuc_spin =    1.0    ! ag108
       CASE (109)
          nuc_spin =    0.5    ! ag109
       CASE (110)
          nuc_spin =    1.0    ! ag110
       CASE (111)
          nuc_spin =    0.5    ! ag111
       CASE (112)
          nuc_spin =    0.0    ! ag112
       CASE (113)
          nuc_spin =    0.5    ! ag113
       CASE (114)
          nuc_spin =    1.0    ! ag114
       CASE (115)
          nuc_spin =    0.5    ! ag115
       CASE (116)
          nuc_spin =    2.0    ! ag116
       CASE (117)
          nuc_spin =    0.5    ! ag117
       CASE (118)
          nuc_spin =    0.0    ! ag118
       CASE (119)
          nuc_spin =    3.5    ! ag119
       CASE (120)
          nuc_spin =    0.0    ! ag120
       CASE (121)
          nuc_spin =    3.5    ! ag121
       CASE (122)
          nuc_spin =    3.0    ! ag122
       CASE (123)
          nuc_spin =    3.5    ! ag123
       CASE (124)
          nuc_spin =    0.0    ! ag124
       CASE (125)
          nuc_spin =    4.5    ! ag125
       CASE (126)
          nuc_spin =    6.0    ! ag126
       CASE (127)
          nuc_spin =    1.5    ! ag127
       CASE (128)
          nuc_spin =    2.0    ! ag128
       CASE (129)
          nuc_spin =    4.5    ! ag129
       CASE (130)
          nuc_spin =    2.0    ! ag130
       CASE (131)
          nuc_spin =    2.5    ! ag131
       CASE (132)
          nuc_spin =    5.0    ! ag132
       CASE (133)
          nuc_spin =    3.5    ! ag133
       CASE (134)
          nuc_spin =    4.0    ! ag134
       CASE (135)
          nuc_spin =    3.5    ! ag135
       CASE (136)
          nuc_spin =    2.0    ! ag136
       CASE (137)
          nuc_spin =    3.5    ! ag137
       CASE (138)
          nuc_spin =    4.0    ! ag138
       CASE (139)
          nuc_spin =    0.5    ! ag139
       CASE (140)
          nuc_spin =    2.0    ! ag140
       CASE (141)
          nuc_spin =    0.5    ! ag141
       CASE (142)
          nuc_spin =    3.0    ! ag142
       CASE (143)
          nuc_spin =    0.5    ! ag143
       CASE (144)
          nuc_spin =    3.0    ! ag144
       CASE (145)
          nuc_spin =    0.5    ! ag145
       CASE (146)
          nuc_spin =    1.0    ! ag146
       CASE (147)
          nuc_spin =    0.5    ! ag147
       CASE (148)
          nuc_spin =    4.0    ! ag148
       CASE (149)
          nuc_spin =    3.5    ! ag149
       CASE (150)
          nuc_spin =    2.0    ! ag150
       CASE (151)
          nuc_spin =    0.5    ! ag151
       CASE (152)
          nuc_spin =    3.0    ! ag152
       CASE (153)
          nuc_spin =    0.5    ! ag153
       CASE (154)
          nuc_spin =    1.0    ! ag154
       CASE (155)
          nuc_spin =    0.5    ! ag155
       CASE (156)
          nuc_spin =    1.0    ! ag156
       CASE (157)
          nuc_spin =    0.5    ! ag157
       CASE (158)
          nuc_spin =    1.0    ! ag158
       CASE (159)
          nuc_spin =    0.5    ! ag159
       CASE (160)
          nuc_spin =    3.0    ! ag160
       END SELECT
    CASE (48)
       SELECT CASE (a)
       CASE (90)
          nuc_spin =    0.0    ! cd90
       CASE (91)
          nuc_spin =    1.5    ! cd91
       CASE (92)
          nuc_spin =    0.0    ! cd92
       CASE (93)
          nuc_spin =    2.5    ! cd93
       CASE (94)
          nuc_spin =    0.0    ! cd94
       CASE (95)
          nuc_spin =    3.5    ! cd95
       CASE (96)
          nuc_spin =    0.0    ! cd96
       CASE (97)
          nuc_spin =    4.5    ! cd97
       CASE (98)
          nuc_spin =    0.0    ! cd98
       CASE (99)
          nuc_spin =    2.5    ! cd99
       CASE (100)
          nuc_spin =    0.0    ! cd100
       CASE (101)
          nuc_spin =    2.5    ! cd101
       CASE (102)
          nuc_spin =    0.0    ! cd102
       CASE (103)
          nuc_spin =    2.5    ! cd103
       CASE (104)
          nuc_spin =    0.0    ! cd104
       CASE (105)
          nuc_spin =    2.5    ! cd105
       CASE (106)
          nuc_spin =    0.0    ! cd106
       CASE (107)
          nuc_spin =    2.5    ! cd107
       CASE (108)
          nuc_spin =    0.0    ! cd108
       CASE (109)
          nuc_spin =    2.5    ! cd109
       CASE (110)
          nuc_spin =    0.0    ! cd110
       CASE (111)
          nuc_spin =    0.5    ! cd111
       CASE (112)
          nuc_spin =    0.0    ! cd112
       CASE (113)
          nuc_spin =    0.5    ! cd113
       CASE (114)
          nuc_spin =    0.0    ! cd114
       CASE (115)
          nuc_spin =    0.5    ! cd115
       CASE (116)
          nuc_spin =    0.0    ! cd116
       CASE (117)
          nuc_spin =    0.5    ! cd117
       CASE (118)
          nuc_spin =    0.0    ! cd118
       CASE (119)
          nuc_spin =    1.5    ! cd119
       CASE (120)
          nuc_spin =    0.0    ! cd120
       CASE (121)
          nuc_spin =    1.5    ! cd121
       CASE (122)
          nuc_spin =    0.0    ! cd122
       CASE (123)
          nuc_spin =    1.5    ! cd123
       CASE (124)
          nuc_spin =    0.0    ! cd124
       CASE (125)
          nuc_spin =    1.5    ! cd125
       CASE (126)
          nuc_spin =    0.0    ! cd126
       CASE (127)
          nuc_spin =    1.5    ! cd127
       CASE (128)
          nuc_spin =    0.0    ! cd128
       CASE (129)
          nuc_spin =    5.5    ! cd129
       CASE (130)
          nuc_spin =    0.0    ! cd130
       CASE (131)
          nuc_spin =    3.5    ! cd131
       CASE (132)
          nuc_spin =    0.0    ! cd132
       CASE (133)
          nuc_spin =    1.5    ! cd133
       CASE (134)
          nuc_spin =    0.0    ! cd134
       CASE (135)
          nuc_spin =    1.5    ! cd135
       CASE (136)
          nuc_spin =    0.0    ! cd136
       CASE (137)
          nuc_spin =    1.5    ! cd137
       CASE (138)
          nuc_spin =    0.0    ! cd138
       CASE (139)
          nuc_spin =    1.5    ! cd139
       CASE (140)
          nuc_spin =    0.0    ! cd140
       CASE (141)
          nuc_spin =    1.5    ! cd141
       CASE (142)
          nuc_spin =    0.0    ! cd142
       CASE (143)
          nuc_spin =    2.5    ! cd143
       CASE (144)
          nuc_spin =    0.0    ! cd144
       CASE (145)
          nuc_spin =    2.5    ! cd145
       CASE (146)
          nuc_spin =    0.0    ! cd146
       CASE (147)
          nuc_spin =    0.5    ! cd147
       CASE (148)
          nuc_spin =    0.0    ! cd148
       CASE (149)
          nuc_spin =    2.5    ! cd149
       CASE (150)
          nuc_spin =    0.0    ! cd150
       CASE (151)
          nuc_spin =    2.5    ! cd151
       CASE (152)
          nuc_spin =    0.0    ! cd152
       CASE (153)
          nuc_spin =    3.5    ! cd153
       CASE (154)
          nuc_spin =    0.0    ! cd154
       CASE (155)
          nuc_spin =    4.5    ! cd155
       CASE (156)
          nuc_spin =    0.0    ! cd156
       CASE (157)
          nuc_spin =    0.5    ! cd157
       CASE (158)
          nuc_spin =    0.0    ! cd158
       CASE (159)
          nuc_spin =    1.5    ! cd159
       CASE (160)
          nuc_spin =    0.0    ! cd160
       CASE (161)
          nuc_spin =    3.5    ! cd161
       CASE (162)
          nuc_spin =    0.0    ! cd162
       CASE (163)
          nuc_spin =    5.5    ! cd163
       END SELECT
    CASE (49)
       SELECT CASE (a)
       CASE (92)
          nuc_spin =    3.0    ! in92
       CASE (93)
          nuc_spin =    4.5    ! in93
       CASE (94)
          nuc_spin =    5.0    ! in94
       CASE (95)
          nuc_spin =    4.5    ! in95
       CASE (96)
          nuc_spin =    1.0    ! in96
       CASE (97)
          nuc_spin =    4.5    ! in97
       CASE (98)
          nuc_spin =    5.0    ! in98
       CASE (99)
          nuc_spin =    4.5    ! in99
       CASE (100)
          nuc_spin =    5.0    ! in100
       CASE (101)
          nuc_spin =    4.5    ! in101
       CASE (102)
          nuc_spin =    3.0    ! in102
       CASE (103)
          nuc_spin =    4.5    ! in103
       CASE (104)
          nuc_spin =    6.0    ! in104
       CASE (105)
          nuc_spin =    4.5    ! in105
       CASE (106)
          nuc_spin =    7.0    ! in106
       CASE (107)
          nuc_spin =    4.5    ! in107
       CASE (108)
          nuc_spin =    7.0    ! in108
       CASE (109)
          nuc_spin =    4.5    ! in109
       CASE (110)
          nuc_spin =    7.0    ! in110
       CASE (111)
          nuc_spin =    4.5    ! in111
       CASE (112)
          nuc_spin =    1.0    ! in112
       CASE (113)
          nuc_spin =    4.5    ! in113
       CASE (114)
          nuc_spin =    1.0    ! in114
       CASE (115)
          nuc_spin =    4.5    ! in115
       CASE (116)
          nuc_spin =    1.0    ! in116
       CASE (117)
          nuc_spin =    4.5    ! in117
       CASE (118)
          nuc_spin =    1.0    ! in118
       CASE (119)
          nuc_spin =    4.5    ! in119
       CASE (120)
          nuc_spin =    1.0    ! in120
       CASE (121)
          nuc_spin =    4.5    ! in121
       CASE (122)
          nuc_spin =    1.0    ! in122
       CASE (123)
          nuc_spin =    4.5    ! in123
       CASE (124)
          nuc_spin =    3.0    ! in124
       CASE (125)
          nuc_spin =    4.5    ! in125
       CASE (126)
          nuc_spin =    0.0    ! in126
       CASE (127)
          nuc_spin =    4.5    ! in127
       CASE (128)
          nuc_spin =    3.0    ! in128
       CASE (129)
          nuc_spin =    4.5    ! in129
       CASE (130)
          nuc_spin =    0.0    ! in130
       CASE (131)
          nuc_spin =    4.5    ! in131
       CASE (132)
          nuc_spin =    7.0    ! in132
       CASE (133)
          nuc_spin =    4.5    ! in133
       CASE (134)
          nuc_spin =    5.0    ! in134
       CASE (135)
          nuc_spin =    4.5    ! in135
       CASE (136)
          nuc_spin =    2.0    ! in136
       CASE (137)
          nuc_spin =    4.5    ! in137
       CASE (138)
          nuc_spin =    5.0    ! in138
       CASE (139)
          nuc_spin =    4.5    ! in139
       CASE (140)
          nuc_spin =    2.0    ! in140
       CASE (141)
          nuc_spin =    0.5    ! in141
       CASE (142)
          nuc_spin =    2.0    ! in142
       CASE (143)
          nuc_spin =    3.5    ! in143
       CASE (144)
          nuc_spin =    1.0    ! in144
       CASE (145)
          nuc_spin =    3.5    ! in145
       CASE (146)
          nuc_spin =    4.0    ! in146
       CASE (147)
          nuc_spin =    3.5    ! in147
       CASE (148)
          nuc_spin =    4.0    ! in148
       CASE (149)
          nuc_spin =    3.5    ! in149
       CASE (150)
          nuc_spin =    4.0    ! in150
       CASE (151)
          nuc_spin =    0.5    ! in151
       CASE (152)
          nuc_spin =    3.0    ! in152
       CASE (153)
          nuc_spin =    0.5    ! in153
       CASE (154)
          nuc_spin =    4.0    ! in154
       CASE (155)
          nuc_spin =    0.5    ! in155
       CASE (156)
          nuc_spin =    5.0    ! in156
       CASE (157)
          nuc_spin =    0.5    ! in157
       CASE (158)
          nuc_spin =    0.0    ! in158
       CASE (159)
          nuc_spin =    0.5    ! in159
       CASE (160)
          nuc_spin =    5.0    ! in160
       CASE (161)
          nuc_spin =    4.5    ! in161
       CASE (162)
          nuc_spin =    5.0    ! in162
       CASE (163)
          nuc_spin =    4.5    ! in163
       CASE (164)
          nuc_spin =    1.0    ! in164
       CASE (165)
          nuc_spin =    4.5    ! in165
       CASE (166)
          nuc_spin =    1.0    ! in166
       END SELECT
    CASE (50)
       SELECT CASE (a)
       CASE (94)
          nuc_spin =    0.0    ! sn94
       CASE (95)
          nuc_spin =    0.5    ! sn95
       CASE (96)
          nuc_spin =    0.0    ! sn96
       CASE (97)
          nuc_spin =    4.5    ! sn97
       CASE (98)
          nuc_spin =    0.0    ! sn98
       CASE (99)
          nuc_spin =    4.5    ! sn99
       CASE (100)
          nuc_spin =    0.0    ! sn100
       CASE (101)
          nuc_spin =    0.5    ! sn101
       CASE (102)
          nuc_spin =    0.0    ! sn102
       CASE (103)
          nuc_spin =    1.5    ! sn103
       CASE (104)
          nuc_spin =    0.0    ! sn104
       CASE (105)
          nuc_spin =    2.5    ! sn105
       CASE (106)
          nuc_spin =    0.0    ! sn106
       CASE (107)
          nuc_spin =    2.5    ! sn107
       CASE (108)
          nuc_spin =    0.0    ! sn108
       CASE (109)
          nuc_spin =    2.5    ! sn109
       CASE (110)
          nuc_spin =    0.0    ! sn110
       CASE (111)
          nuc_spin =    3.5    ! sn111
       CASE (112)
          nuc_spin =    0.0    ! sn112
       CASE (113)
          nuc_spin =    0.5    ! sn113
       CASE (114)
          nuc_spin =    0.0    ! sn114
       CASE (115)
          nuc_spin =    0.5    ! sn115
       CASE (116)
          nuc_spin =    0.0    ! sn116
       CASE (117)
          nuc_spin =    0.5    ! sn117
       CASE (118)
          nuc_spin =    0.0    ! sn118
       CASE (119)
          nuc_spin =    0.5    ! sn119
       CASE (120)
          nuc_spin =    0.0    ! sn120
       CASE (121)
          nuc_spin =    1.5    ! sn121
       CASE (122)
          nuc_spin =    0.0    ! sn122
       CASE (123)
          nuc_spin =    5.5    ! sn123
       CASE (124)
          nuc_spin =    0.0    ! sn124
       CASE (125)
          nuc_spin =    5.5    ! sn125
       CASE (126)
          nuc_spin =    0.0    ! sn126
       CASE (127)
          nuc_spin =    5.5    ! sn127
       CASE (128)
          nuc_spin =    0.0    ! sn128
       CASE (129)
          nuc_spin =    1.5    ! sn129
       CASE (130)
          nuc_spin =    0.0    ! sn130
       CASE (131)
          nuc_spin =    1.5    ! sn131
       CASE (132)
          nuc_spin =    0.0    ! sn132
       CASE (133)
          nuc_spin =    3.5    ! sn133
       CASE (134)
          nuc_spin =    0.0    ! sn134
       CASE (135)
          nuc_spin =    0.5    ! sn135
       CASE (136)
          nuc_spin =    0.0    ! sn136
       CASE (137)
          nuc_spin =    1.5    ! sn137
       CASE (138)
          nuc_spin =    0.0    ! sn138
       CASE (139)
          nuc_spin =    2.5    ! sn139
       CASE (140)
          nuc_spin =    0.0    ! sn140
       CASE (141)
          nuc_spin =    1.5    ! sn141
       CASE (142)
          nuc_spin =    0.0    ! sn142
       CASE (143)
          nuc_spin =    1.5    ! sn143
       CASE (144)
          nuc_spin =    0.0    ! sn144
       CASE (145)
          nuc_spin =    2.5    ! sn145
       CASE (146)
          nuc_spin =    0.0    ! sn146
       CASE (147)
          nuc_spin =    2.5    ! sn147
       CASE (148)
          nuc_spin =    0.0    ! sn148
       CASE (149)
          nuc_spin =    0.5    ! sn149
       CASE (150)
          nuc_spin =    0.0    ! sn150
       CASE (151)
          nuc_spin =    3.5    ! sn151
       CASE (152)
          nuc_spin =    0.0    ! sn152
       CASE (153)
          nuc_spin =    2.5    ! sn153
       CASE (154)
          nuc_spin =    0.0    ! sn154
       CASE (155)
          nuc_spin =    3.5    ! sn155
       CASE (156)
          nuc_spin =    0.0    ! sn156
       CASE (157)
          nuc_spin =    4.5    ! sn157
       CASE (158)
          nuc_spin =    0.0    ! sn158
       CASE (159)
          nuc_spin =    0.5    ! sn159
       CASE (160)
          nuc_spin =    0.0    ! sn160
       CASE (161)
          nuc_spin =    3.5    ! sn161
       CASE (162)
          nuc_spin =    0.0    ! sn162
       CASE (163)
          nuc_spin =    1.5    ! sn163
       CASE (164)
          nuc_spin =    0.0    ! sn164
       CASE (165)
          nuc_spin =    5.5    ! sn165
       CASE (166)
          nuc_spin =    0.0    ! sn166
       CASE (167)
          nuc_spin =    5.5    ! sn167
       CASE (168)
          nuc_spin =    0.0    ! sn168
       CASE (169)
          nuc_spin =    1.5    ! sn169
       END SELECT
    CASE (51)
       SELECT CASE (a)
       CASE (97)
          nuc_spin =    0.5    ! sb97
       CASE (98)
          nuc_spin =    3.0    ! sb98
       CASE (99)
          nuc_spin =    0.5    ! sb99
       CASE (100)
          nuc_spin =    5.0    ! sb100
       CASE (101)
          nuc_spin =    0.5    ! sb101
       CASE (102)
          nuc_spin =    1.0    ! sb102
       CASE (103)
          nuc_spin =    0.5    ! sb103
       CASE (104)
          nuc_spin =    1.0    ! sb104
       CASE (105)
          nuc_spin =    2.5    ! sb105
       CASE (106)
          nuc_spin =    1.0    ! sb106
       CASE (107)
          nuc_spin =    2.5    ! sb107
       CASE (108)
          nuc_spin =    4.0    ! sb108
       CASE (109)
          nuc_spin =    2.5    ! sb109
       CASE (110)
          nuc_spin =    3.0    ! sb110
       CASE (111)
          nuc_spin =    2.5    ! sb111
       CASE (112)
          nuc_spin =    3.0    ! sb112
       CASE (113)
          nuc_spin =    2.5    ! sb113
       CASE (114)
          nuc_spin =    3.0    ! sb114
       CASE (115)
          nuc_spin =    2.5    ! sb115
       CASE (116)
          nuc_spin =    3.0    ! sb116
       CASE (117)
          nuc_spin =    2.5    ! sb117
       CASE (118)
          nuc_spin =    1.0    ! sb118
       CASE (119)
          nuc_spin =    2.5    ! sb119
       CASE (120)
          nuc_spin =    1.0    ! sb120
       CASE (121)
          nuc_spin =    2.5    ! sb121
       CASE (122)
          nuc_spin =    2.0    ! sb122
       CASE (123)
          nuc_spin =    3.5    ! sb123
       CASE (124)
          nuc_spin =    3.0    ! sb124
       CASE (125)
          nuc_spin =    3.5    ! sb125
       CASE (126)
          nuc_spin =    8.0    ! sb126
       CASE (127)
          nuc_spin =    3.5    ! sb127
       CASE (128)
          nuc_spin =    8.0    ! sb128
       CASE (129)
          nuc_spin =    3.5    ! sb129
       CASE (130)
          nuc_spin =    8.0    ! sb130
       CASE (131)
          nuc_spin =    3.5    ! sb131
       CASE (132)
          nuc_spin =    4.0    ! sb132
       CASE (133)
          nuc_spin =    3.5    ! sb133
       CASE (134)
          nuc_spin =    0.0    ! sb134
       CASE (135)
          nuc_spin =    3.5    ! sb135
       CASE (136)
          nuc_spin =    2.0    ! sb136
       CASE (137)
          nuc_spin =    0.5    ! sb137
       CASE (138)
          nuc_spin =    0.0    ! sb138
       CASE (139)
          nuc_spin =    0.5    ! sb139
       CASE (140)
          nuc_spin =    2.0    ! sb140
       CASE (141)
          nuc_spin =    0.5    ! sb141
       CASE (142)
          nuc_spin =    1.0    ! sb142
       CASE (143)
          nuc_spin =    4.5    ! sb143
       CASE (144)
          nuc_spin =    5.0    ! sb144
       CASE (145)
          nuc_spin =    4.5    ! sb145
       CASE (146)
          nuc_spin =    2.0    ! sb146
       CASE (147)
          nuc_spin =    4.5    ! sb147
       CASE (148)
          nuc_spin =    5.0    ! sb148
       CASE (149)
          nuc_spin =    4.5    ! sb149
       CASE (150)
          nuc_spin =    4.0    ! sb150
       CASE (151)
          nuc_spin =    4.5    ! sb151
       CASE (152)
          nuc_spin =    2.0    ! sb152
       CASE (153)
          nuc_spin =    4.5    ! sb153
       CASE (154)
          nuc_spin =    3.0    ! sb154
       CASE (155)
          nuc_spin =    1.5    ! sb155
       CASE (156)
          nuc_spin =    2.0    ! sb156
       CASE (157)
          nuc_spin =    1.5    ! sb157
       CASE (158)
          nuc_spin =    3.0    ! sb158
       CASE (159)
          nuc_spin =    1.5    ! sb159
       CASE (160)
          nuc_spin =    2.0    ! sb160
       CASE (161)
          nuc_spin =    1.5    ! sb161
       CASE (162)
          nuc_spin =    0.0    ! sb162
       CASE (163)
          nuc_spin =    0.5    ! sb163
       CASE (164)
          nuc_spin =    2.0    ! sb164
       CASE (165)
          nuc_spin =    0.5    ! sb165
       CASE (166)
          nuc_spin =    5.0    ! sb166
       CASE (167)
          nuc_spin =    0.5    ! sb167
       CASE (168)
          nuc_spin =    5.0    ! sb168
       CASE (169)
          nuc_spin =    0.5    ! sb169
       CASE (170)
          nuc_spin =    3.0    ! sb170
       CASE (171)
          nuc_spin =    3.5    ! sb171
       CASE (172)
          nuc_spin =    4.0    ! sb172
       END SELECT
    CASE (52)
       SELECT CASE (a)
       CASE (99)
          nuc_spin =    3.5    ! te99
       CASE (100)
          nuc_spin =    0.0    ! te100
       CASE (101)
          nuc_spin =    4.5    ! te101
       CASE (102)
          nuc_spin =    0.0    ! te102
       CASE (103)
          nuc_spin =    0.5    ! te103
       CASE (104)
          nuc_spin =    0.0    ! te104
       CASE (105)
          nuc_spin =    1.5    ! te105
       CASE (106)
          nuc_spin =    0.0    ! te106
       CASE (107)
          nuc_spin =    1.5    ! te107
       CASE (108)
          nuc_spin =    0.0    ! te108
       CASE (109)
          nuc_spin =    1.5    ! te109
       CASE (110)
          nuc_spin =    0.0    ! te110
       CASE (111)
          nuc_spin =    0.5    ! te111
       CASE (112)
          nuc_spin =    0.0    ! te112
       CASE (113)
          nuc_spin =    3.5    ! te113
       CASE (114)
          nuc_spin =    0.0    ! te114
       CASE (115)
          nuc_spin =    3.5    ! te115
       CASE (116)
          nuc_spin =    0.0    ! te116
       CASE (117)
          nuc_spin =    0.5    ! te117
       CASE (118)
          nuc_spin =    0.0    ! te118
       CASE (119)
          nuc_spin =    0.5    ! te119
       CASE (120)
          nuc_spin =    0.0    ! te120
       CASE (121)
          nuc_spin =    0.5    ! te121
       CASE (122)
          nuc_spin =    0.0    ! te122
       CASE (123)
          nuc_spin =    0.5    ! te123
       CASE (124)
          nuc_spin =    0.0    ! te124
       CASE (125)
          nuc_spin =    0.5    ! te125
       CASE (126)
          nuc_spin =    0.0    ! te126
       CASE (127)
          nuc_spin =    1.5    ! te127
       CASE (128)
          nuc_spin =    0.0    ! te128
       CASE (129)
          nuc_spin =    1.5    ! te129
       CASE (130)
          nuc_spin =    0.0    ! te130
       CASE (131)
          nuc_spin =    1.5    ! te131
       CASE (132)
          nuc_spin =    0.0    ! te132
       CASE (133)
          nuc_spin =    1.5    ! te133
       CASE (134)
          nuc_spin =    0.0    ! te134
       CASE (135)
          nuc_spin =    3.5    ! te135
       CASE (136)
          nuc_spin =    0.0    ! te136
       CASE (137)
          nuc_spin =    3.5    ! te137
       CASE (138)
          nuc_spin =    0.0    ! te138
       CASE (139)
          nuc_spin =    1.5    ! te139
       CASE (140)
          nuc_spin =    0.0    ! te140
       CASE (141)
          nuc_spin =    0.5    ! te141
       CASE (142)
          nuc_spin =    0.0    ! te142
       CASE (143)
          nuc_spin =    1.5    ! te143
       CASE (144)
          nuc_spin =    0.0    ! te144
       CASE (145)
          nuc_spin =    1.5    ! te145
       CASE (146)
          nuc_spin =    0.0    ! te146
       CASE (147)
          nuc_spin =    2.5    ! te147
       CASE (148)
          nuc_spin =    0.0    ! te148
       CASE (149)
          nuc_spin =    2.5    ! te149
       CASE (150)
          nuc_spin =    0.0    ! te150
       CASE (151)
          nuc_spin =    0.5    ! te151
       CASE (152)
          nuc_spin =    0.0    ! te152
       CASE (153)
          nuc_spin =    2.5    ! te153
       CASE (154)
          nuc_spin =    0.0    ! te154
       CASE (155)
          nuc_spin =    2.5    ! te155
       CASE (156)
          nuc_spin =    0.0    ! te156
       CASE (157)
          nuc_spin =    3.5    ! te157
       CASE (158)
          nuc_spin =    0.0    ! te158
       CASE (159)
          nuc_spin =    4.5    ! te159
       CASE (160)
          nuc_spin =    0.0    ! te160
       CASE (161)
          nuc_spin =    0.5    ! te161
       CASE (162)
          nuc_spin =    0.0    ! te162
       CASE (163)
          nuc_spin =    1.5    ! te163
       CASE (164)
          nuc_spin =    0.0    ! te164
       CASE (165)
          nuc_spin =    1.5    ! te165
       CASE (166)
          nuc_spin =    0.0    ! te166
       CASE (167)
          nuc_spin =    5.5    ! te167
       CASE (168)
          nuc_spin =    0.0    ! te168
       CASE (169)
          nuc_spin =    5.5    ! te169
       CASE (170)
          nuc_spin =    0.0    ! te170
       CASE (171)
          nuc_spin =    0.5    ! te171
       CASE (172)
          nuc_spin =    0.0    ! te172
       CASE (173)
          nuc_spin =    0.5    ! te173
       CASE (174)
          nuc_spin =    0.0    ! te174
       CASE (175)
          nuc_spin =    1.5    ! te175
       CASE (176)
          nuc_spin =    0.0    ! te176
       END SELECT
    CASE (53)
       SELECT CASE (a)
       CASE (101)
          nuc_spin =    0.5    ! i101
       CASE (102)
          nuc_spin =    3.0    ! i102
       CASE (103)
          nuc_spin =    1.5    ! i103
       CASE (104)
          nuc_spin =    1.0    ! i104
       CASE (105)
          nuc_spin =    0.5    ! i105
       CASE (106)
          nuc_spin =    1.0    ! i106
       CASE (107)
          nuc_spin =    0.5    ! i107
       CASE (108)
          nuc_spin =    1.0    ! i108
       CASE (109)
          nuc_spin =    1.5    ! i109
       CASE (110)
          nuc_spin =    1.0    ! i110
       CASE (111)
          nuc_spin =    2.5    ! i111
       CASE (112)
          nuc_spin =    1.0    ! i112
       CASE (113)
          nuc_spin =    0.5    ! i113
       CASE (114)
          nuc_spin =    1.0    ! i114
       CASE (115)
          nuc_spin =    2.5    ! i115
       CASE (116)
          nuc_spin =    1.0    ! i116
       CASE (117)
          nuc_spin =    2.5    ! i117
       CASE (118)
          nuc_spin =    2.0    ! i118
       CASE (119)
          nuc_spin =    2.5    ! i119
       CASE (120)
          nuc_spin =    2.0    ! i120
       CASE (121)
          nuc_spin =    2.5    ! i121
       CASE (122)
          nuc_spin =    1.0    ! i122
       CASE (123)
          nuc_spin =    2.5    ! i123
       CASE (124)
          nuc_spin =    2.0    ! i124
       CASE (125)
          nuc_spin =    2.5    ! i125
       CASE (126)
          nuc_spin =    2.0    ! i126
       CASE (127)
          nuc_spin =    2.5    ! i127
       CASE (128)
          nuc_spin =    1.0    ! i128
       CASE (129)
          nuc_spin =    3.5    ! i129
       CASE (130)
          nuc_spin =    5.0    ! i130
       CASE (131)
          nuc_spin =    3.5    ! i131
       CASE (132)
          nuc_spin =    4.0    ! i132
       CASE (133)
          nuc_spin =    3.5    ! i133
       CASE (134)
          nuc_spin =    4.0    ! i134
       CASE (135)
          nuc_spin =    3.5    ! i135
       CASE (136)
          nuc_spin =    1.0    ! i136
       CASE (137)
          nuc_spin =    3.5    ! i137
       CASE (138)
          nuc_spin =    2.0    ! i138
       CASE (139)
          nuc_spin =    3.5    ! i139
       CASE (140)
          nuc_spin =    0.0    ! i140
       CASE (141)
          nuc_spin =    0.5    ! i141
       CASE (142)
          nuc_spin =    1.0    ! i142
       CASE (143)
          nuc_spin =    1.5    ! i143
       CASE (144)
          nuc_spin =    1.0    ! i144
       CASE (145)
          nuc_spin =    0.5    ! i145
       CASE (146)
          nuc_spin =    1.0    ! i146
       CASE (147)
          nuc_spin =    0.5    ! i147
       CASE (148)
          nuc_spin =    3.0    ! i148
       CASE (149)
          nuc_spin =    1.5    ! i149
       CASE (150)
          nuc_spin =    3.0    ! i150
       CASE (151)
          nuc_spin =    1.5    ! i151
       CASE (152)
          nuc_spin =    2.0    ! i152
       CASE (153)
          nuc_spin =    1.5    ! i153
       CASE (154)
          nuc_spin =    3.0    ! i154
       CASE (155)
          nuc_spin =    1.5    ! i155
       CASE (156)
          nuc_spin =    2.0    ! i156
       CASE (157)
          nuc_spin =    4.5    ! i157
       CASE (158)
          nuc_spin =    5.0    ! i158
       CASE (159)
          nuc_spin =    4.5    ! i159
       CASE (160)
          nuc_spin =    5.0    ! i160
       CASE (161)
          nuc_spin =    4.5    ! i161
       CASE (162)
          nuc_spin =    4.0    ! i162
       CASE (163)
          nuc_spin =    4.5    ! i163
       CASE (164)
          nuc_spin =    5.0    ! i164
       CASE (165)
          nuc_spin =    1.5    ! i165
       CASE (166)
          nuc_spin =    0.0    ! i166
       CASE (167)
          nuc_spin =    1.5    ! i167
       CASE (168)
          nuc_spin =    6.0    ! i168
       CASE (169)
          nuc_spin =    1.5    ! i169
       CASE (170)
          nuc_spin =    6.0    ! i170
       CASE (171)
          nuc_spin =    1.5    ! i171
       CASE (172)
          nuc_spin =    3.0    ! i172
       CASE (173)
          nuc_spin =    2.5    ! i173
       CASE (174)
          nuc_spin =    2.0    ! i174
       CASE (175)
          nuc_spin =    2.5    ! i175
       CASE (176)
          nuc_spin =    3.0    ! i176
       CASE (177)
          nuc_spin =    2.5    ! i177
       CASE (178)
          nuc_spin =    2.0    ! i178
       CASE (179)
          nuc_spin =    1.5    ! i179
       END SELECT
    CASE (54)
       SELECT CASE (a)
       CASE (103)
          nuc_spin =    4.5    ! xe103
       CASE (104)
          nuc_spin =    0.0    ! xe104
       CASE (105)
          nuc_spin =    0.5    ! xe105
       CASE (106)
          nuc_spin =    0.0    ! xe106
       CASE (107)
          nuc_spin =    0.5    ! xe107
       CASE (108)
          nuc_spin =    0.0    ! xe108
       CASE (109)
          nuc_spin =    1.5    ! xe109
       CASE (110)
          nuc_spin =    0.0    ! xe110
       CASE (111)
          nuc_spin =    0.5    ! xe111
       CASE (112)
          nuc_spin =    0.0    ! xe112
       CASE (113)
          nuc_spin =    1.5    ! xe113
       CASE (114)
          nuc_spin =    0.0    ! xe114
       CASE (115)
          nuc_spin =    2.5    ! xe115
       CASE (116)
          nuc_spin =    0.0    ! xe116
       CASE (117)
          nuc_spin =    2.5    ! xe117
       CASE (118)
          nuc_spin =    0.0    ! xe118
       CASE (119)
          nuc_spin =    2.5    ! xe119
       CASE (120)
          nuc_spin =    0.0    ! xe120
       CASE (121)
          nuc_spin =    2.5    ! xe121
       CASE (122)
          nuc_spin =    0.0    ! xe122
       CASE (123)
          nuc_spin =    0.5    ! xe123
       CASE (124)
          nuc_spin =    0.0    ! xe124
       CASE (125)
          nuc_spin =    0.5    ! xe125
       CASE (126)
          nuc_spin =    0.0    ! xe126
       CASE (127)
          nuc_spin =    0.5    ! xe127
       CASE (128)
          nuc_spin =    0.0    ! xe128
       CASE (129)
          nuc_spin =    0.5    ! xe129
       CASE (130)
          nuc_spin =    0.0    ! xe130
       CASE (131)
          nuc_spin =    1.5    ! xe131
       CASE (132)
          nuc_spin =    0.0    ! xe132
       CASE (133)
          nuc_spin =    1.5    ! xe133
       CASE (134)
          nuc_spin =    0.0    ! xe134
       CASE (135)
          nuc_spin =    1.5    ! xe135
       CASE (136)
          nuc_spin =    0.0    ! xe136
       CASE (137)
          nuc_spin =    3.5    ! xe137
       CASE (138)
          nuc_spin =    0.0    ! xe138
       CASE (139)
          nuc_spin =    1.5    ! xe139
       CASE (140)
          nuc_spin =    0.0    ! xe140
       CASE (141)
          nuc_spin =    2.5    ! xe141
       CASE (142)
          nuc_spin =    0.0    ! xe142
       CASE (143)
          nuc_spin =    2.5    ! xe143
       CASE (144)
          nuc_spin =    0.0    ! xe144
       CASE (145)
          nuc_spin =    1.5    ! xe145
       CASE (146)
          nuc_spin =    0.0    ! xe146
       CASE (147)
          nuc_spin =    1.5    ! xe147
       CASE (148)
          nuc_spin =    0.0    ! xe148
       CASE (149)
          nuc_spin =    2.5    ! xe149
       CASE (150)
          nuc_spin =    0.0    ! xe150
       CASE (151)
          nuc_spin =    2.5    ! xe151
       CASE (152)
          nuc_spin =    0.0    ! xe152
       CASE (153)
          nuc_spin =    0.5    ! xe153
       CASE (154)
          nuc_spin =    0.0    ! xe154
       CASE (155)
          nuc_spin =    3.5    ! xe155
       CASE (156)
          nuc_spin =    0.0    ! xe156
       CASE (157)
          nuc_spin =    2.5    ! xe157
       CASE (158)
          nuc_spin =    0.0    ! xe158
       CASE (159)
          nuc_spin =    3.5    ! xe159
       CASE (160)
          nuc_spin =    0.0    ! xe160
       CASE (161)
          nuc_spin =    4.5    ! xe161
       CASE (162)
          nuc_spin =    0.0    ! xe162
       CASE (163)
          nuc_spin =    0.5    ! xe163
       CASE (164)
          nuc_spin =    0.0    ! xe164
       CASE (165)
          nuc_spin =    1.5    ! xe165
       CASE (166)
          nuc_spin =    0.0    ! xe166
       CASE (167)
          nuc_spin =    3.5    ! xe167
       CASE (168)
          nuc_spin =    0.0    ! xe168
       CASE (169)
          nuc_spin =    5.5    ! xe169
       CASE (170)
          nuc_spin =    0.0    ! xe170
       CASE (171)
          nuc_spin =    5.5    ! xe171
       CASE (172)
          nuc_spin =    0.0    ! xe172
       CASE (173)
          nuc_spin =    0.5    ! xe173
       CASE (174)
          nuc_spin =    0.0    ! xe174
       CASE (175)
          nuc_spin =    0.5    ! xe175
       CASE (176)
          nuc_spin =    0.0    ! xe176
       CASE (177)
          nuc_spin =    1.5    ! xe177
       CASE (178)
          nuc_spin =    0.0    ! xe178
       CASE (179)
          nuc_spin =    0.5    ! xe179
       CASE (180)
          nuc_spin =    0.0    ! xe180
       CASE (181)
          nuc_spin =    0.5    ! xe181
       CASE (182)
          nuc_spin =    0.0    ! xe182
       END SELECT
    CASE (55)
       SELECT CASE (a)
       CASE (106)
          nuc_spin =    1.0    ! cs106
       CASE (107)
          nuc_spin =    1.5    ! cs107
       CASE (108)
          nuc_spin =    1.0    ! cs108
       CASE (109)
          nuc_spin =    1.5    ! cs109
       CASE (110)
          nuc_spin =    2.0    ! cs110
       CASE (111)
          nuc_spin =    1.5    ! cs111
       CASE (112)
          nuc_spin =    1.0    ! cs112
       CASE (113)
          nuc_spin =    1.5    ! cs113
       CASE (114)
          nuc_spin =    1.0    ! cs114
       CASE (115)
          nuc_spin =    0.5    ! cs115
       CASE (116)
          nuc_spin =    1.0    ! cs116
       CASE (117)
          nuc_spin =    4.5    ! cs117
       CASE (118)
          nuc_spin =    2.0    ! cs118
       CASE (119)
          nuc_spin =    4.5    ! cs119
       CASE (120)
          nuc_spin =    2.0    ! cs120
       CASE (121)
          nuc_spin =    1.5    ! cs121
       CASE (122)
          nuc_spin =    1.0    ! cs122
       CASE (123)
          nuc_spin =    0.5    ! cs123
       CASE (124)
          nuc_spin =    1.0    ! cs124
       CASE (125)
          nuc_spin =    0.5    ! cs125
       CASE (126)
          nuc_spin =    1.0    ! cs126
       CASE (127)
          nuc_spin =    0.5    ! cs127
       CASE (128)
          nuc_spin =    1.0    ! cs128
       CASE (129)
          nuc_spin =    0.5    ! cs129
       CASE (130)
          nuc_spin =    1.0    ! cs130
       CASE (131)
          nuc_spin =    2.5    ! cs131
       CASE (132)
          nuc_spin =    2.0    ! cs132
       CASE (133)
          nuc_spin =    3.5    ! cs133
       CASE (134)
          nuc_spin =    4.0    ! cs134
       CASE (135)
          nuc_spin =    3.5    ! cs135
       CASE (136)
          nuc_spin =    6.0    ! cs136
       CASE (137)
          nuc_spin =    3.5    ! cs137
       CASE (138)
          nuc_spin =    3.0    ! cs138
       CASE (139)
          nuc_spin =    3.5    ! cs139
       CASE (140)
          nuc_spin =    1.0    ! cs140
       CASE (141)
          nuc_spin =    3.5    ! cs141
       CASE (142)
          nuc_spin =    0.0    ! cs142
       CASE (143)
          nuc_spin =    1.5    ! cs143
       CASE (144)
          nuc_spin =    0.0    ! cs144
       CASE (145)
          nuc_spin =    1.5    ! cs145
       CASE (146)
          nuc_spin =    1.0    ! cs146
       CASE (147)
          nuc_spin =    1.5    ! cs147
       CASE (148)
          nuc_spin =    0.0    ! cs148
       CASE (149)
          nuc_spin =    1.5    ! cs149
       CASE (150)
          nuc_spin =    2.0    ! cs150
       CASE (151)
          nuc_spin =    1.5    ! cs151
       CASE (152)
          nuc_spin =    3.0    ! cs152
       CASE (153)
          nuc_spin =    0.5    ! cs153
       CASE (154)
          nuc_spin =    0.0    ! cs154
       CASE (155)
          nuc_spin =    0.5    ! cs155
       CASE (156)
          nuc_spin =    3.0    ! cs156
       CASE (157)
          nuc_spin =    0.5    ! cs157
       CASE (158)
          nuc_spin =    2.0    ! cs158
       CASE (159)
          nuc_spin =    0.5    ! cs159
       CASE (160)
          nuc_spin =    4.0    ! cs160
       CASE (161)
          nuc_spin =    0.5    ! cs161
       CASE (162)
          nuc_spin =    5.0    ! cs162
       CASE (163)
          nuc_spin =    0.5    ! cs163
       CASE (164)
          nuc_spin =    0.0    ! cs164
       CASE (165)
          nuc_spin =    0.5    ! cs165
       CASE (166)
          nuc_spin =    2.0    ! cs166
       CASE (167)
          nuc_spin =    0.5    ! cs167
       CASE (168)
          nuc_spin =    4.0    ! cs168
       CASE (169)
          nuc_spin =    0.5    ! cs169
       CASE (170)
          nuc_spin =    5.0    ! cs170
       CASE (171)
          nuc_spin =    0.5    ! cs171
       CASE (172)
          nuc_spin =    4.0    ! cs172
       CASE (173)
          nuc_spin =    2.5    ! cs173
       CASE (174)
          nuc_spin =    3.0    ! cs174
       CASE (175)
          nuc_spin =    1.5    ! cs175
       CASE (176)
          nuc_spin =    2.0    ! cs176
       CASE (177)
          nuc_spin =    1.5    ! cs177
       CASE (178)
          nuc_spin =    0.0    ! cs178
       CASE (179)
          nuc_spin =    1.5    ! cs179
       CASE (180)
          nuc_spin =    2.0    ! cs180
       CASE (181)
          nuc_spin =    2.5    ! cs181
       CASE (182)
          nuc_spin =    3.0    ! cs182
       CASE (183)
          nuc_spin =    2.5    ! cs183
       CASE (184)
          nuc_spin =    1.0    ! cs184
       CASE (185)
          nuc_spin =    0.5    ! cs185
       END SELECT
    CASE (56)
       SELECT CASE (a)
       CASE (108)
          nuc_spin =    0.0    ! ba108
       CASE (109)
          nuc_spin =    0.5    ! ba109
       CASE (110)
          nuc_spin =    0.0    ! ba110
       CASE (111)
          nuc_spin =    1.5    ! ba111
       CASE (112)
          nuc_spin =    0.0    ! ba112
       CASE (113)
          nuc_spin =    0.5    ! ba113
       CASE (114)
          nuc_spin =    0.0    ! ba114
       CASE (115)
          nuc_spin =    1.5    ! ba115
       CASE (116)
          nuc_spin =    0.0    ! ba116
       CASE (117)
          nuc_spin =    1.5    ! ba117
       CASE (118)
          nuc_spin =    0.0    ! ba118
       CASE (119)
          nuc_spin =    2.5    ! ba119
       CASE (120)
          nuc_spin =    0.0    ! ba120
       CASE (121)
          nuc_spin =    2.5    ! ba121
       CASE (122)
          nuc_spin =    0.0    ! ba122
       CASE (123)
          nuc_spin =    2.5    ! ba123
       CASE (124)
          nuc_spin =    0.0    ! ba124
       CASE (125)
          nuc_spin =    0.5    ! ba125
       CASE (126)
          nuc_spin =    0.0    ! ba126
       CASE (127)
          nuc_spin =    0.5    ! ba127
       CASE (128)
          nuc_spin =    0.0    ! ba128
       CASE (129)
          nuc_spin =    0.5    ! ba129
       CASE (130)
          nuc_spin =    0.0    ! ba130
       CASE (131)
          nuc_spin =    0.5    ! ba131
       CASE (132)
          nuc_spin =    0.0    ! ba132
       CASE (133)
          nuc_spin =    0.5    ! ba133
       CASE (134)
          nuc_spin =    0.0    ! ba134
       CASE (135)
          nuc_spin =    1.5    ! ba135
       CASE (136)
          nuc_spin =    0.0    ! ba136
       CASE (137)
          nuc_spin =    1.5    ! ba137
       CASE (138)
          nuc_spin =    0.0    ! ba138
       CASE (139)
          nuc_spin =    3.5    ! ba139
       CASE (140)
          nuc_spin =    0.0    ! ba140
       CASE (141)
          nuc_spin =    1.5    ! ba141
       CASE (142)
          nuc_spin =    0.0    ! ba142
       CASE (143)
          nuc_spin =    2.5    ! ba143
       CASE (144)
          nuc_spin =    0.0    ! ba144
       CASE (145)
          nuc_spin =    2.5    ! ba145
       CASE (146)
          nuc_spin =    0.0    ! ba146
       CASE (147)
          nuc_spin =    1.5    ! ba147
       CASE (148)
          nuc_spin =    0.0    ! ba148
       CASE (149)
          nuc_spin =    1.5    ! ba149
       CASE (150)
          nuc_spin =    0.0    ! ba150
       CASE (151)
          nuc_spin =    2.5    ! ba151
       CASE (152)
          nuc_spin =    0.0    ! ba152
       CASE (153)
          nuc_spin =    2.5    ! ba153
       CASE (154)
          nuc_spin =    0.0    ! ba154
       CASE (155)
          nuc_spin =    0.5    ! ba155
       CASE (156)
          nuc_spin =    0.0    ! ba156
       CASE (157)
          nuc_spin =    3.5    ! ba157
       CASE (158)
          nuc_spin =    0.0    ! ba158
       CASE (159)
          nuc_spin =    2.5    ! ba159
       CASE (160)
          nuc_spin =    0.0    ! ba160
       CASE (161)
          nuc_spin =    3.5    ! ba161
       CASE (162)
          nuc_spin =    0.0    ! ba162
       CASE (163)
          nuc_spin =    4.5    ! ba163
       CASE (164)
          nuc_spin =    0.0    ! ba164
       CASE (165)
          nuc_spin =    0.5    ! ba165
       CASE (166)
          nuc_spin =    0.0    ! ba166
       CASE (167)
          nuc_spin =    1.5    ! ba167
       CASE (168)
          nuc_spin =    0.0    ! ba168
       CASE (169)
          nuc_spin =    3.5    ! ba169
       CASE (170)
          nuc_spin =    0.0    ! ba170
       CASE (171)
          nuc_spin =    5.5    ! ba171
       CASE (172)
          nuc_spin =    0.0    ! ba172
       CASE (173)
          nuc_spin =    4.5    ! ba173
       CASE (174)
          nuc_spin =    0.0    ! ba174
       CASE (175)
          nuc_spin =    1.5    ! ba175
       CASE (176)
          nuc_spin =    0.0    ! ba176
       CASE (177)
          nuc_spin =    0.5    ! ba177
       CASE (178)
          nuc_spin =    0.0    ! ba178
       CASE (179)
          nuc_spin =    1.5    ! ba179
       CASE (180)
          nuc_spin =    0.0    ! ba180
       CASE (181)
          nuc_spin =    0.5    ! ba181
       CASE (182)
          nuc_spin =    0.0    ! ba182
       CASE (183)
          nuc_spin =    0.5    ! ba183
       CASE (184)
          nuc_spin =    0.0    ! ba184
       CASE (185)
          nuc_spin =    1.5    ! ba185
       CASE (186)
          nuc_spin =    0.0    ! ba186
       CASE (187)
          nuc_spin =    0.5    ! ba187
       CASE (188)
          nuc_spin =    0.0    ! ba188
       CASE (189)
          nuc_spin =    2.5    ! ba189
       END SELECT
    CASE (57)
       SELECT CASE (a)
       CASE (110)
          nuc_spin =    1.0    ! la110
       CASE (111)
          nuc_spin =    0.5    ! la111
       CASE (112)
          nuc_spin =    2.0    ! la112
       CASE (113)
          nuc_spin =    0.5    ! la113
       CASE (114)
          nuc_spin =    1.0    ! la114
       CASE (115)
          nuc_spin =    1.5    ! la115
       CASE (116)
          nuc_spin =    0.0    ! la116
       CASE (117)
          nuc_spin =    1.5    ! la117
       CASE (118)
          nuc_spin =    3.0    ! la118
       CASE (119)
          nuc_spin =    1.5    ! la119
       CASE (120)
          nuc_spin =    2.0    ! la120
       CASE (121)
          nuc_spin =    1.5    ! la121
       CASE (122)
          nuc_spin =    2.0    ! la122
       CASE (123)
          nuc_spin =    0.5    ! la123
       CASE (124)
          nuc_spin =    0.0    ! la124
       CASE (125)
          nuc_spin =    5.5    ! la125
       CASE (126)
          nuc_spin =    3.0    ! la126
       CASE (127)
          nuc_spin =    1.5    ! la127
       CASE (128)
          nuc_spin =    4.0    ! la128
       CASE (129)
          nuc_spin =    1.5    ! la129
       CASE (130)
          nuc_spin =    0.0    ! la130
       CASE (131)
          nuc_spin =    1.5    ! la131
       CASE (132)
          nuc_spin =    2.0    ! la132
       CASE (133)
          nuc_spin =    2.5    ! la133
       CASE (134)
          nuc_spin =    1.0    ! la134
       CASE (135)
          nuc_spin =    2.5    ! la135
       CASE (136)
          nuc_spin =    1.0    ! la136
       CASE (137)
          nuc_spin =    3.5    ! la137
       CASE (138)
          nuc_spin =    5.0    ! la138
       CASE (139)
          nuc_spin =    3.5    ! la139
       CASE (140)
          nuc_spin =    3.0    ! la140
       CASE (141)
          nuc_spin =    3.5    ! la141
       CASE (142)
          nuc_spin =    2.0    ! la142
       CASE (143)
          nuc_spin =    3.5    ! la143
       CASE (144)
          nuc_spin =    3.0    ! la144
       CASE (145)
          nuc_spin =    1.5    ! la145
       CASE (146)
          nuc_spin =    2.0    ! la146
       CASE (147)
          nuc_spin =    1.5    ! la147
       CASE (148)
          nuc_spin =    2.0    ! la148
       CASE (149)
          nuc_spin =    0.5    ! la149
       CASE (150)
          nuc_spin =    2.0    ! la150
       CASE (151)
          nuc_spin =    0.5    ! la151
       CASE (152)
          nuc_spin =    3.0    ! la152
       CASE (153)
          nuc_spin =    0.5    ! la153
       CASE (154)
          nuc_spin =    2.0    ! la154
       CASE (155)
          nuc_spin =    0.5    ! la155
       CASE (156)
          nuc_spin =    0.0    ! la156
       CASE (157)
          nuc_spin =    0.5    ! la157
       CASE (158)
          nuc_spin =    4.0    ! la158
       CASE (159)
          nuc_spin =    0.5    ! la159
       CASE (160)
          nuc_spin =    3.0    ! la160
       CASE (161)
          nuc_spin =    0.5    ! la161
       CASE (162)
          nuc_spin =    3.0    ! la162
       CASE (163)
          nuc_spin =    0.5    ! la163
       CASE (164)
          nuc_spin =    4.0    ! la164
       CASE (165)
          nuc_spin =    0.5    ! la165
       CASE (166)
          nuc_spin =    1.0    ! la166
       CASE (167)
          nuc_spin =    2.5    ! la167
       CASE (168)
          nuc_spin =    3.0    ! la168
       CASE (169)
          nuc_spin =    2.5    ! la169
       CASE (170)
          nuc_spin =    4.0    ! la170
       CASE (171)
          nuc_spin =    2.5    ! la171
       CASE (172)
          nuc_spin =    3.0    ! la172
       CASE (173)
          nuc_spin =    2.5    ! la173
       CASE (174)
          nuc_spin =    4.0    ! la174
       CASE (175)
          nuc_spin =    0.5    ! la175
       CASE (176)
          nuc_spin =    2.0    ! la176
       CASE (177)
          nuc_spin =    2.5    ! la177
       CASE (178)
          nuc_spin =    2.0    ! la178
       CASE (179)
          nuc_spin =    0.5    ! la179
       CASE (180)
          nuc_spin =    2.0    ! la180
       CASE (181)
          nuc_spin =    0.5    ! la181
       CASE (182)
          nuc_spin =    0.0    ! la182
       CASE (183)
          nuc_spin =    3.5    ! la183
       CASE (184)
          nuc_spin =    3.0    ! la184
       CASE (185)
          nuc_spin =    3.5    ! la185
       CASE (186)
          nuc_spin =    4.0    ! la186
       CASE (187)
          nuc_spin =    2.5    ! la187
       CASE (188)
          nuc_spin =    3.0    ! la188
       CASE (189)
          nuc_spin =    2.5    ! la189
       CASE (190)
          nuc_spin =    1.0    ! la190
       CASE (191)
          nuc_spin =    1.5    ! la191
       CASE (192)
          nuc_spin =    2.0    ! la192
       END SELECT
    CASE (58)
       SELECT CASE (a)
       CASE (113)
          nuc_spin =    1.5    ! ce113
       CASE (114)
          nuc_spin =    0.0    ! ce114
       CASE (115)
          nuc_spin =    1.5    ! ce115
       CASE (116)
          nuc_spin =    0.0    ! ce116
       CASE (117)
          nuc_spin =    1.5    ! ce117
       CASE (118)
          nuc_spin =    0.0    ! ce118
       CASE (119)
          nuc_spin =    2.5    ! ce119
       CASE (120)
          nuc_spin =    0.0    ! ce120
       CASE (121)
          nuc_spin =    1.5    ! ce121
       CASE (122)
          nuc_spin =    0.0    ! ce122
       CASE (123)
          nuc_spin =    2.5    ! ce123
       CASE (124)
          nuc_spin =    0.0    ! ce124
       CASE (125)
          nuc_spin =    2.5    ! ce125
       CASE (126)
          nuc_spin =    0.0    ! ce126
       CASE (127)
          nuc_spin =    3.5    ! ce127
       CASE (128)
          nuc_spin =    0.0    ! ce128
       CASE (129)
          nuc_spin =    2.5    ! ce129
       CASE (130)
          nuc_spin =    0.0    ! ce130
       CASE (131)
          nuc_spin =    3.5    ! ce131
       CASE (132)
          nuc_spin =    0.0    ! ce132
       CASE (133)
          nuc_spin =    0.5    ! ce133
       CASE (134)
          nuc_spin =    0.0    ! ce134
       CASE (135)
          nuc_spin =    0.5    ! ce135
       CASE (136)
          nuc_spin =    0.0    ! ce136
       CASE (137)
          nuc_spin =    1.5    ! ce137
       CASE (138)
          nuc_spin =    0.0    ! ce138
       CASE (139)
          nuc_spin =    1.5    ! ce139
       CASE (140)
          nuc_spin =    0.0    ! ce140
       CASE (141)
          nuc_spin =    3.5    ! ce141
       CASE (142)
          nuc_spin =    0.0    ! ce142
       CASE (143)
          nuc_spin =    1.5    ! ce143
       CASE (144)
          nuc_spin =    0.0    ! ce144
       CASE (145)
          nuc_spin =    1.5    ! ce145
       CASE (146)
          nuc_spin =    0.0    ! ce146
       CASE (147)
          nuc_spin =    2.5    ! ce147
       CASE (148)
          nuc_spin =    0.0    ! ce148
       CASE (149)
          nuc_spin =    1.5    ! ce149
       CASE (150)
          nuc_spin =    0.0    ! ce150
       CASE (151)
          nuc_spin =    1.5    ! ce151
       CASE (152)
          nuc_spin =    0.0    ! ce152
       CASE (153)
          nuc_spin =    2.5    ! ce153
       CASE (154)
          nuc_spin =    0.0    ! ce154
       CASE (155)
          nuc_spin =    2.5    ! ce155
       CASE (156)
          nuc_spin =    0.0    ! ce156
       CASE (157)
          nuc_spin =    0.5    ! ce157
       CASE (158)
          nuc_spin =    0.0    ! ce158
       CASE (159)
          nuc_spin =    3.5    ! ce159
       CASE (160)
          nuc_spin =    0.0    ! ce160
       CASE (161)
          nuc_spin =    2.5    ! ce161
       CASE (162)
          nuc_spin =    0.0    ! ce162
       CASE (163)
          nuc_spin =    3.5    ! ce163
       CASE (164)
          nuc_spin =    0.0    ! ce164
       CASE (165)
          nuc_spin =    4.5    ! ce165
       CASE (166)
          nuc_spin =    0.0    ! ce166
       CASE (167)
          nuc_spin =    0.5    ! ce167
       CASE (168)
          nuc_spin =    0.0    ! ce168
       CASE (169)
          nuc_spin =    1.5    ! ce169
       CASE (170)
          nuc_spin =    0.0    ! ce170
       CASE (171)
          nuc_spin =    3.5    ! ce171
       CASE (172)
          nuc_spin =    0.0    ! ce172
       CASE (173)
          nuc_spin =    5.5    ! ce173
       CASE (174)
          nuc_spin =    0.0    ! ce174
       CASE (175)
          nuc_spin =    4.5    ! ce175
       CASE (176)
          nuc_spin =    0.0    ! ce176
       CASE (177)
          nuc_spin =    1.5    ! ce177
       CASE (178)
          nuc_spin =    0.0    ! ce178
       CASE (179)
          nuc_spin =    0.5    ! ce179
       CASE (180)
          nuc_spin =    0.0    ! ce180
       CASE (181)
          nuc_spin =    1.5    ! ce181
       CASE (182)
          nuc_spin =    0.0    ! ce182
       CASE (183)
          nuc_spin =    0.5    ! ce183
       CASE (184)
          nuc_spin =    0.0    ! ce184
       CASE (185)
          nuc_spin =    4.5    ! ce185
       CASE (186)
          nuc_spin =    0.0    ! ce186
       CASE (187)
          nuc_spin =    1.5    ! ce187
       CASE (188)
          nuc_spin =    0.0    ! ce188
       CASE (189)
          nuc_spin =    0.5    ! ce189
       CASE (190)
          nuc_spin =    0.0    ! ce190
       CASE (191)
          nuc_spin =    2.5    ! ce191
       CASE (192)
          nuc_spin =    0.0    ! ce192
       CASE (193)
          nuc_spin =    1.5    ! ce193
       CASE (194)
          nuc_spin =    0.0    ! ce194
       CASE (195)
          nuc_spin =    1.5    ! ce195
       END SELECT
    CASE (59)
       SELECT CASE (a)
       CASE (115)
          nuc_spin =    1.5    ! pr115
       CASE (116)
          nuc_spin =    0.0    ! pr116
       CASE (117)
          nuc_spin =    1.5    ! pr117
       CASE (118)
          nuc_spin =    2.0    ! pr118
       CASE (119)
          nuc_spin =    1.5    ! pr119
       CASE (120)
          nuc_spin =    1.0    ! pr120
       CASE (121)
          nuc_spin =    1.5    ! pr121
       CASE (122)
          nuc_spin =    0.0    ! pr122
       CASE (123)
          nuc_spin =    1.5    ! pr123
       CASE (124)
          nuc_spin =    3.0    ! pr124
       CASE (125)
          nuc_spin =    1.5    ! pr125
       CASE (126)
          nuc_spin =    2.0    ! pr126
       CASE (127)
          nuc_spin =    1.5    ! pr127
       CASE (128)
          nuc_spin =    4.0    ! pr128
       CASE (129)
          nuc_spin =    1.5    ! pr129
       CASE (130)
          nuc_spin =    3.0    ! pr130
       CASE (131)
          nuc_spin =    1.5    ! pr131
       CASE (132)
          nuc_spin =    2.0    ! pr132
       CASE (133)
          nuc_spin =    1.5    ! pr133
       CASE (134)
          nuc_spin =    2.0    ! pr134
       CASE (135)
          nuc_spin =    1.5    ! pr135
       CASE (136)
          nuc_spin =    2.0    ! pr136
       CASE (137)
          nuc_spin =    2.5    ! pr137
       CASE (138)
          nuc_spin =    1.0    ! pr138
       CASE (139)
          nuc_spin =    2.5    ! pr139
       CASE (140)
          nuc_spin =    1.0    ! pr140
       CASE (141)
          nuc_spin =    2.5    ! pr141
       CASE (142)
          nuc_spin =    2.0    ! pr142
       CASE (143)
          nuc_spin =    3.5    ! pr143
       CASE (144)
          nuc_spin =    0.0    ! pr144
       CASE (145)
          nuc_spin =    3.5    ! pr145
       CASE (146)
          nuc_spin =    2.0    ! pr146
       CASE (147)
          nuc_spin =    1.5    ! pr147
       CASE (148)
          nuc_spin =    1.0    ! pr148
       CASE (149)
          nuc_spin =    2.5    ! pr149
       CASE (150)
          nuc_spin =    1.0    ! pr150
       CASE (151)
          nuc_spin =    1.5    ! pr151
       CASE (152)
          nuc_spin =    4.0    ! pr152
       CASE (153)
          nuc_spin =    1.5    ! pr153
       CASE (154)
          nuc_spin =    3.0    ! pr154
       CASE (155)
          nuc_spin =    1.5    ! pr155
       CASE (156)
          nuc_spin =    1.0    ! pr156
       CASE (157)
          nuc_spin =    1.5    ! pr157
       CASE (158)
          nuc_spin =    1.0    ! pr158
       CASE (159)
          nuc_spin =    1.5    ! pr159
       CASE (160)
          nuc_spin =    2.0    ! pr160
       CASE (161)
          nuc_spin =    1.5    ! pr161
       CASE (162)
          nuc_spin =    1.0    ! pr162
       CASE (163)
          nuc_spin =    1.5    ! pr163
       CASE (164)
          nuc_spin =    4.0    ! pr164
       CASE (165)
          nuc_spin =    1.5    ! pr165
       CASE (166)
          nuc_spin =    5.0    ! pr166
       CASE (167)
          nuc_spin =    1.5    ! pr167
       CASE (168)
          nuc_spin =    1.0    ! pr168
       CASE (169)
          nuc_spin =    2.5    ! pr169
       CASE (170)
          nuc_spin =    1.0    ! pr170
       CASE (171)
          nuc_spin =    0.5    ! pr171
       CASE (172)
          nuc_spin =    2.0    ! pr172
       CASE (173)
          nuc_spin =    1.5    ! pr173
       CASE (174)
          nuc_spin =    6.0    ! pr174
       CASE (175)
          nuc_spin =    1.5    ! pr175
       CASE (176)
          nuc_spin =    5.0    ! pr176
       CASE (177)
          nuc_spin =    1.5    ! pr177
       CASE (178)
          nuc_spin =    0.0    ! pr178
       CASE (179)
          nuc_spin =    0.5    ! pr179
       CASE (180)
          nuc_spin =    0.0    ! pr180
       CASE (181)
          nuc_spin =    0.5    ! pr181
       CASE (182)
          nuc_spin =    3.0    ! pr182
       CASE (183)
          nuc_spin =    2.5    ! pr183
       CASE (184)
          nuc_spin =    2.0    ! pr184
       CASE (185)
          nuc_spin =    0.5    ! pr185
       CASE (186)
          nuc_spin =    5.0    ! pr186
       CASE (187)
          nuc_spin =    2.5    ! pr187
       CASE (188)
          nuc_spin =    4.0    ! pr188
       CASE (189)
          nuc_spin =    2.5    ! pr189
       CASE (190)
          nuc_spin =    1.0    ! pr190
       CASE (191)
          nuc_spin =    1.5    ! pr191
       CASE (192)
          nuc_spin =    3.0    ! pr192
       CASE (193)
          nuc_spin =    1.5    ! pr193
       CASE (194)
          nuc_spin =    2.0    ! pr194
       CASE (195)
          nuc_spin =    1.5    ! pr195
       CASE (196)
          nuc_spin =    0.0    ! pr196
       CASE (197)
          nuc_spin =    1.5    ! pr197
       CASE (198)
          nuc_spin =    1.0    ! pr198
       END SELECT
    CASE (60)
       SELECT CASE (a)
       CASE (118)
          nuc_spin =    0.0    ! nd118
       CASE (119)
          nuc_spin =    1.5    ! nd119
       CASE (120)
          nuc_spin =    0.0    ! nd120
       CASE (121)
          nuc_spin =    2.5    ! nd121
       CASE (122)
          nuc_spin =    0.0    ! nd122
       CASE (123)
          nuc_spin =    1.5    ! nd123
       CASE (124)
          nuc_spin =    0.0    ! nd124
       CASE (125)
          nuc_spin =    2.5    ! nd125
       CASE (126)
          nuc_spin =    0.0    ! nd126
       CASE (127)
          nuc_spin =    0.5    ! nd127
       CASE (128)
          nuc_spin =    0.0    ! nd128
       CASE (129)
          nuc_spin =    2.5    ! nd129
       CASE (130)
          nuc_spin =    0.0    ! nd130
       CASE (131)
          nuc_spin =    2.5    ! nd131
       CASE (132)
          nuc_spin =    0.0    ! nd132
       CASE (133)
          nuc_spin =    3.5    ! nd133
       CASE (134)
          nuc_spin =    0.0    ! nd134
       CASE (135)
          nuc_spin =    4.5    ! nd135
       CASE (136)
          nuc_spin =    0.0    ! nd136
       CASE (137)
          nuc_spin =    0.5    ! nd137
       CASE (138)
          nuc_spin =    0.0    ! nd138
       CASE (139)
          nuc_spin =    1.5    ! nd139
       CASE (140)
          nuc_spin =    0.0    ! nd140
       CASE (141)
          nuc_spin =    1.5    ! nd141
       CASE (142)
          nuc_spin =    0.0    ! nd142
       CASE (143)
          nuc_spin =    3.5    ! nd143
       CASE (144)
          nuc_spin =    0.0    ! nd144
       CASE (145)
          nuc_spin =    3.5    ! nd145
       CASE (146)
          nuc_spin =    0.0    ! nd146
       CASE (147)
          nuc_spin =    2.5    ! nd147
       CASE (148)
          nuc_spin =    0.0    ! nd148
       CASE (149)
          nuc_spin =    2.5    ! nd149
       CASE (150)
          nuc_spin =    0.0    ! nd150
       CASE (151)
          nuc_spin =    1.5    ! nd151
       CASE (152)
          nuc_spin =    0.0    ! nd152
       CASE (153)
          nuc_spin =    0.5    ! nd153
       CASE (154)
          nuc_spin =    0.0    ! nd154
       CASE (155)
          nuc_spin =    2.5    ! nd155
       CASE (156)
          nuc_spin =    0.0    ! nd156
       CASE (157)
          nuc_spin =    2.5    ! nd157
       CASE (158)
          nuc_spin =    0.0    ! nd158
       CASE (159)
          nuc_spin =    0.5    ! nd159
       CASE (160)
          nuc_spin =    0.0    ! nd160
       CASE (161)
          nuc_spin =    3.5    ! nd161
       CASE (162)
          nuc_spin =    0.0    ! nd162
       CASE (163)
          nuc_spin =    2.5    ! nd163
       CASE (164)
          nuc_spin =    0.0    ! nd164
       CASE (165)
          nuc_spin =    3.5    ! nd165
       CASE (166)
          nuc_spin =    0.0    ! nd166
       CASE (167)
          nuc_spin =    4.5    ! nd167
       CASE (168)
          nuc_spin =    0.0    ! nd168
       CASE (169)
          nuc_spin =    0.5    ! nd169
       CASE (170)
          nuc_spin =    0.0    ! nd170
       CASE (171)
          nuc_spin =    1.5    ! nd171
       CASE (172)
          nuc_spin =    0.0    ! nd172
       CASE (173)
          nuc_spin =    5.5    ! nd173
       CASE (174)
          nuc_spin =    0.0    ! nd174
       CASE (175)
          nuc_spin =    5.5    ! nd175
       CASE (176)
          nuc_spin =    0.0    ! nd176
       CASE (177)
          nuc_spin =    4.5    ! nd177
       CASE (178)
          nuc_spin =    0.0    ! nd178
       CASE (179)
          nuc_spin =    0.5    ! nd179
       CASE (180)
          nuc_spin =    0.0    ! nd180
       CASE (181)
          nuc_spin =    0.5    ! nd181
       CASE (182)
          nuc_spin =    0.0    ! nd182
       CASE (183)
          nuc_spin =    1.5    ! nd183
       CASE (184)
          nuc_spin =    0.0    ! nd184
       CASE (185)
          nuc_spin =    0.5    ! nd185
       CASE (186)
          nuc_spin =    0.0    ! nd186
       CASE (187)
          nuc_spin =    4.5    ! nd187
       CASE (188)
          nuc_spin =    0.0    ! nd188
       CASE (189)
          nuc_spin =    1.5    ! nd189
       CASE (190)
          nuc_spin =    0.0    ! nd190
       CASE (191)
          nuc_spin =    2.5    ! nd191
       CASE (192)
          nuc_spin =    0.0    ! nd192
       CASE (193)
          nuc_spin =    2.5    ! nd193
       CASE (194)
          nuc_spin =    0.0    ! nd194
       CASE (195)
          nuc_spin =    1.5    ! nd195
       CASE (196)
          nuc_spin =    0.0    ! nd196
       CASE (197)
          nuc_spin =    1.5    ! nd197
       CASE (198)
          nuc_spin =    0.0    ! nd198
       CASE (199)
          nuc_spin =    2.5    ! nd199
       CASE (200)
          nuc_spin =    0.0    ! nd200
       CASE (201)
          nuc_spin =    2.5    ! nd201
       END SELECT
    CASE (61)
       SELECT CASE (a)
       CASE (120)
          nuc_spin =    1.0    ! pm120
       CASE (121)
          nuc_spin =    2.5    ! pm121
       CASE (122)
          nuc_spin =    3.0    ! pm122
       CASE (123)
          nuc_spin =    2.5    ! pm123
       CASE (124)
          nuc_spin =    3.0    ! pm124
       CASE (125)
          nuc_spin =    2.5    ! pm125
       CASE (126)
          nuc_spin =    0.0    ! pm126
       CASE (127)
          nuc_spin =    2.5    ! pm127
       CASE (128)
          nuc_spin =    2.0    ! pm128
       CASE (129)
          nuc_spin =    2.5    ! pm129
       CASE (130)
          nuc_spin =    1.0    ! pm130
       CASE (131)
          nuc_spin =    2.5    ! pm131
       CASE (132)
          nuc_spin =    3.0    ! pm132
       CASE (133)
          nuc_spin =    5.5    ! pm133
       CASE (134)
          nuc_spin =    5.0    ! pm134
       CASE (135)
          nuc_spin =    5.5    ! pm135
       CASE (136)
          nuc_spin =    2.0    ! pm136
       CASE (137)
          nuc_spin =    5.5    ! pm137
       CASE (138)
          nuc_spin =    1.0    ! pm138
       CASE (139)
          nuc_spin =    2.5    ! pm139
       CASE (140)
          nuc_spin =    1.0    ! pm140
       CASE (141)
          nuc_spin =    2.5    ! pm141
       CASE (142)
          nuc_spin =    1.0    ! pm142
       CASE (143)
          nuc_spin =    2.5    ! pm143
       CASE (144)
          nuc_spin =    5.0    ! pm144
       CASE (145)
          nuc_spin =    2.5    ! pm145
       CASE (146)
          nuc_spin =    3.0    ! pm146
       CASE (147)
          nuc_spin =    3.5    ! pm147
       CASE (148)
          nuc_spin =    1.0    ! pm148
       CASE (149)
          nuc_spin =    3.5    ! pm149
       CASE (150)
          nuc_spin =    1.0    ! pm150
       CASE (151)
          nuc_spin =    2.5    ! pm151
       CASE (152)
          nuc_spin =    1.0    ! pm152
       CASE (153)
          nuc_spin =    2.5    ! pm153
       CASE (154)
          nuc_spin =    1.0    ! pm154
       CASE (155)
          nuc_spin =    2.5    ! pm155
       CASE (156)
          nuc_spin =    0.0    ! pm156
       CASE (157)
          nuc_spin =    2.5    ! pm157
       CASE (158)
          nuc_spin =    3.0    ! pm158
       CASE (159)
          nuc_spin =    2.5    ! pm159
       CASE (160)
          nuc_spin =    3.0    ! pm160
       CASE (161)
          nuc_spin =    2.5    ! pm161
       CASE (162)
          nuc_spin =    4.0    ! pm162
       CASE (163)
          nuc_spin =    2.5    ! pm163
       CASE (164)
          nuc_spin =    3.0    ! pm164
       CASE (165)
          nuc_spin =    2.5    ! pm165
       CASE (166)
          nuc_spin =    1.0    ! pm166
       CASE (167)
          nuc_spin =    2.5    ! pm167
       CASE (168)
          nuc_spin =    5.0    ! pm168
       CASE (169)
          nuc_spin =    2.5    ! pm169
       CASE (170)
          nuc_spin =    2.0    ! pm170
       CASE (171)
          nuc_spin =    2.5    ! pm171
       CASE (172)
          nuc_spin =    2.0    ! pm172
       CASE (173)
          nuc_spin =    1.5    ! pm173
       CASE (174)
          nuc_spin =    6.0    ! pm174
       CASE (175)
          nuc_spin =    0.5    ! pm175
       CASE (176)
          nuc_spin =    6.0    ! pm176
       CASE (177)
          nuc_spin =    0.5    ! pm177
       CASE (178)
          nuc_spin =    5.0    ! pm178
       CASE (179)
          nuc_spin =    0.5    ! pm179
       CASE (180)
          nuc_spin =    3.0    ! pm180
       CASE (181)
          nuc_spin =    2.5    ! pm181
       CASE (182)
          nuc_spin =    5.0    ! pm182
       CASE (183)
          nuc_spin =    5.5    ! pm183
       CASE (184)
          nuc_spin =    0.0    ! pm184
       CASE (185)
          nuc_spin =    1.5    ! pm185
       CASE (186)
          nuc_spin =    2.0    ! pm186
       CASE (187)
          nuc_spin =    1.5    ! pm187
       CASE (188)
          nuc_spin =    3.0    ! pm188
       CASE (189)
          nuc_spin =    1.5    ! pm189
       CASE (190)
          nuc_spin =    2.0    ! pm190
       CASE (191)
          nuc_spin =    1.5    ! pm191
       CASE (192)
          nuc_spin =    3.0    ! pm192
       CASE (193)
          nuc_spin =    3.5    ! pm193
       CASE (194)
          nuc_spin =    3.0    ! pm194
       CASE (195)
          nuc_spin =    2.5    ! pm195
       CASE (196)
          nuc_spin =    3.0    ! pm196
       CASE (197)
          nuc_spin =    2.5    ! pm197
       CASE (198)
          nuc_spin =    1.0    ! pm198
       CASE (199)
          nuc_spin =    2.5    ! pm199
       CASE (200)
          nuc_spin =    0.0    ! pm200
       CASE (201)
          nuc_spin =    2.5    ! pm201
       CASE (202)
          nuc_spin =    3.0    ! pm202
       CASE (203)
          nuc_spin =    2.5    ! pm203
       CASE (204)
          nuc_spin =    3.0    ! pm204
       CASE (205)
          nuc_spin =    2.5    ! pm205
       END SELECT
    CASE (62)
       SELECT CASE (a)
       CASE (123)
          nuc_spin =    2.5    ! sm123
       CASE (124)
          nuc_spin =    0.0    ! sm124
       CASE (125)
          nuc_spin =    1.5    ! sm125
       CASE (126)
          nuc_spin =    0.0    ! sm126
       CASE (127)
          nuc_spin =    2.5    ! sm127
       CASE (128)
          nuc_spin =    0.0    ! sm128
       CASE (129)
          nuc_spin =    0.5    ! sm129
       CASE (130)
          nuc_spin =    0.0    ! sm130
       CASE (131)
          nuc_spin =    3.5    ! sm131
       CASE (132)
          nuc_spin =    0.0    ! sm132
       CASE (133)
          nuc_spin =    0.5    ! sm133
       CASE (134)
          nuc_spin =    0.0    ! sm134
       CASE (135)
          nuc_spin =    3.5    ! sm135
       CASE (136)
          nuc_spin =    0.0    ! sm136
       CASE (137)
          nuc_spin =    4.5    ! sm137
       CASE (138)
          nuc_spin =    0.0    ! sm138
       CASE (139)
          nuc_spin =    0.5    ! sm139
       CASE (140)
          nuc_spin =    0.0    ! sm140
       CASE (141)
          nuc_spin =    0.5    ! sm141
       CASE (142)
          nuc_spin =    0.0    ! sm142
       CASE (143)
          nuc_spin =    1.5    ! sm143
       CASE (144)
          nuc_spin =    0.0    ! sm144
       CASE (145)
          nuc_spin =    3.5    ! sm145
       CASE (146)
          nuc_spin =    0.0    ! sm146
       CASE (147)
          nuc_spin =    3.5    ! sm147
       CASE (148)
          nuc_spin =    0.0    ! sm148
       CASE (149)
          nuc_spin =    3.5    ! sm149
       CASE (150)
          nuc_spin =    0.0    ! sm150
       CASE (151)
          nuc_spin =    2.5    ! sm151
       CASE (152)
          nuc_spin =    0.0    ! sm152
       CASE (153)
          nuc_spin =    1.5    ! sm153
       CASE (154)
          nuc_spin =    0.0    ! sm154
       CASE (155)
          nuc_spin =    1.5    ! sm155
       CASE (156)
          nuc_spin =    0.0    ! sm156
       CASE (157)
          nuc_spin =    1.5    ! sm157
       CASE (158)
          nuc_spin =    0.0    ! sm158
       CASE (159)
          nuc_spin =    2.5    ! sm159
       CASE (160)
          nuc_spin =    0.0    ! sm160
       CASE (161)
          nuc_spin =    0.5    ! sm161
       CASE (162)
          nuc_spin =    0.0    ! sm162
       CASE (163)
          nuc_spin =    3.5    ! sm163
       CASE (164)
          nuc_spin =    0.0    ! sm164
       CASE (165)
          nuc_spin =    2.5    ! sm165
       CASE (166)
          nuc_spin =    0.0    ! sm166
       CASE (167)
          nuc_spin =    3.5    ! sm167
       CASE (168)
          nuc_spin =    0.0    ! sm168
       CASE (169)
          nuc_spin =    4.5    ! sm169
       CASE (170)
          nuc_spin =    0.0    ! sm170
       CASE (171)
          nuc_spin =    0.5    ! sm171
       CASE (172)
          nuc_spin =    0.0    ! sm172
       CASE (173)
          nuc_spin =    1.5    ! sm173
       CASE (174)
          nuc_spin =    0.0    ! sm174
       CASE (175)
          nuc_spin =    5.5    ! sm175
       CASE (176)
          nuc_spin =    0.0    ! sm176
       CASE (177)
          nuc_spin =    3.5    ! sm177
       CASE (178)
          nuc_spin =    0.0    ! sm178
       CASE (179)
          nuc_spin =    4.5    ! sm179
       CASE (180)
          nuc_spin =    0.0    ! sm180
       CASE (181)
          nuc_spin =    0.5    ! sm181
       CASE (182)
          nuc_spin =    0.0    ! sm182
       CASE (183)
          nuc_spin =    0.5    ! sm183
       CASE (184)
          nuc_spin =    0.0    ! sm184
       CASE (185)
          nuc_spin =    1.5    ! sm185
       CASE (186)
          nuc_spin =    0.0    ! sm186
       CASE (187)
          nuc_spin =    0.5    ! sm187
       CASE (188)
          nuc_spin =    0.0    ! sm188
       CASE (189)
          nuc_spin =    4.5    ! sm189
       CASE (190)
          nuc_spin =    0.0    ! sm190
       CASE (191)
          nuc_spin =    1.5    ! sm191
       CASE (192)
          nuc_spin =    0.0    ! sm192
       CASE (193)
          nuc_spin =    0.5    ! sm193
       CASE (194)
          nuc_spin =    0.0    ! sm194
       CASE (195)
          nuc_spin =    2.5    ! sm195
       CASE (196)
          nuc_spin =    0.0    ! sm196
       CASE (197)
          nuc_spin =    1.5    ! sm197
       CASE (198)
          nuc_spin =    0.0    ! sm198
       CASE (199)
          nuc_spin =    1.5    ! sm199
       CASE (200)
          nuc_spin =    0.0    ! sm200
       CASE (201)
          nuc_spin =    2.5    ! sm201
       CASE (202)
          nuc_spin =    0.0    ! sm202
       CASE (203)
          nuc_spin =    2.5    ! sm203
       CASE (204)
          nuc_spin =    0.0    ! sm204
       CASE (205)
          nuc_spin =    0.5    ! sm205
       CASE (206)
          nuc_spin =    0.0    ! sm206
       CASE (207)
          nuc_spin =    3.5    ! sm207
       CASE (208)
          nuc_spin =    0.0    ! sm208
       END SELECT
    CASE (63)
       SELECT CASE (a)
       CASE (125)
          nuc_spin =    1.5    ! eu125
       CASE (126)
          nuc_spin =    2.0    ! eu126
       CASE (127)
          nuc_spin =    1.5    ! eu127
       CASE (128)
          nuc_spin =    1.0    ! eu128
       CASE (129)
          nuc_spin =    1.5    ! eu129
       CASE (130)
          nuc_spin =    1.0    ! eu130
       CASE (131)
          nuc_spin =    1.5    ! eu131
       CASE (132)
          nuc_spin =    2.0    ! eu132
       CASE (133)
          nuc_spin =    2.5    ! eu133
       CASE (134)
          nuc_spin =    2.0    ! eu134
       CASE (135)
          nuc_spin =    2.5    ! eu135
       CASE (136)
          nuc_spin =    7.0    ! eu136
       CASE (137)
          nuc_spin =    5.5    ! eu137
       CASE (138)
          nuc_spin =    6.0    ! eu138
       CASE (139)
          nuc_spin =    5.5    ! eu139
       CASE (140)
          nuc_spin =    1.0    ! eu140
       CASE (141)
          nuc_spin =    2.5    ! eu141
       CASE (142)
          nuc_spin =    1.0    ! eu142
       CASE (143)
          nuc_spin =    2.5    ! eu143
       CASE (144)
          nuc_spin =    1.0    ! eu144
       CASE (145)
          nuc_spin =    2.5    ! eu145
       CASE (146)
          nuc_spin =    4.0    ! eu146
       CASE (147)
          nuc_spin =    2.5    ! eu147
       CASE (148)
          nuc_spin =    5.0    ! eu148
       CASE (149)
          nuc_spin =    2.5    ! eu149
       CASE (150)
          nuc_spin =    0.0    ! eu150
       CASE (151)
          nuc_spin =    2.5    ! eu151
       CASE (152)
          nuc_spin =    3.0    ! eu152
       CASE (153)
          nuc_spin =    2.5    ! eu153
       CASE (154)
          nuc_spin =    3.0    ! eu154
       CASE (155)
          nuc_spin =    2.5    ! eu155
       CASE (156)
          nuc_spin =    0.0    ! eu156
       CASE (157)
          nuc_spin =    2.5    ! eu157
       CASE (158)
          nuc_spin =    1.0    ! eu158
       CASE (159)
          nuc_spin =    2.5    ! eu159
       CASE (160)
          nuc_spin =    0.0    ! eu160
       CASE (161)
          nuc_spin =    2.5    ! eu161
       CASE (162)
          nuc_spin =    2.0    ! eu162
       CASE (163)
          nuc_spin =    2.5    ! eu163
       CASE (164)
          nuc_spin =    1.0    ! eu164
       CASE (165)
          nuc_spin =    2.5    ! eu165
       CASE (166)
          nuc_spin =    0.0    ! eu166
       CASE (167)
          nuc_spin =    2.5    ! eu167
       CASE (168)
          nuc_spin =    4.0    ! eu168
       CASE (169)
          nuc_spin =    2.5    ! eu169
       CASE (170)
          nuc_spin =    2.0    ! eu170
       CASE (171)
          nuc_spin =    2.5    ! eu171
       CASE (172)
          nuc_spin =    3.0    ! eu172
       CASE (173)
          nuc_spin =    2.5    ! eu173
       CASE (174)
          nuc_spin =    1.0    ! eu174
       CASE (175)
          nuc_spin =    1.5    ! eu175
       CASE (176)
          nuc_spin =    4.0    ! eu176
       CASE (177)
          nuc_spin =    1.5    ! eu177
       CASE (178)
          nuc_spin =    4.0    ! eu178
       CASE (179)
          nuc_spin =    1.5    ! eu179
       CASE (180)
          nuc_spin =    3.0    ! eu180
       CASE (181)
          nuc_spin =    1.5    ! eu181
       CASE (182)
          nuc_spin =    4.0    ! eu182
       CASE (183)
          nuc_spin =    1.5    ! eu183
       CASE (184)
          nuc_spin =    2.0    ! eu184
       CASE (185)
          nuc_spin =    1.5    ! eu185
       CASE (186)
          nuc_spin =    6.0    ! eu186
       CASE (187)
          nuc_spin =    5.5    ! eu187
       CASE (188)
          nuc_spin =    0.0    ! eu188
       CASE (189)
          nuc_spin =    2.5    ! eu189
       CASE (190)
          nuc_spin =    5.0    ! eu190
       CASE (191)
          nuc_spin =    0.5    ! eu191
       CASE (192)
          nuc_spin =    1.0    ! eu192
       CASE (193)
          nuc_spin =    2.5    ! eu193
       CASE (194)
          nuc_spin =    3.0    ! eu194
       CASE (195)
          nuc_spin =    0.5    ! eu195
       CASE (196)
          nuc_spin =    3.0    ! eu196
       CASE (197)
          nuc_spin =    3.5    ! eu197
       CASE (198)
          nuc_spin =    2.0    ! eu198
       CASE (199)
          nuc_spin =    2.5    ! eu199
       CASE (200)
          nuc_spin =    3.0    ! eu200
       CASE (201)
          nuc_spin =    2.5    ! eu201
       CASE (202)
          nuc_spin =    3.0    ! eu202
       CASE (203)
          nuc_spin =    2.5    ! eu203
       CASE (204)
          nuc_spin =    3.0    ! eu204
       CASE (205)
          nuc_spin =    2.5    ! eu205
       CASE (206)
          nuc_spin =    2.0    ! eu206
       CASE (207)
          nuc_spin =    2.5    ! eu207
       CASE (208)
          nuc_spin =    4.0    ! eu208
       CASE (209)
          nuc_spin =    2.5    ! eu209
       CASE (210)
          nuc_spin =    3.0    ! eu210
       CASE (211)
          nuc_spin =    2.5    ! eu211
       END SELECT
    CASE (64)
       SELECT CASE (a)
       CASE (128)
          nuc_spin =    0.0    ! gd128
       CASE (129)
          nuc_spin =    2.5    ! gd129
       CASE (130)
          nuc_spin =    0.0    ! gd130
       CASE (131)
          nuc_spin =    0.5    ! gd131
       CASE (132)
          nuc_spin =    0.0    ! gd132
       CASE (133)
          nuc_spin =    3.5    ! gd133
       CASE (134)
          nuc_spin =    0.0    ! gd134
       CASE (135)
          nuc_spin =    2.5    ! gd135
       CASE (136)
          nuc_spin =    0.0    ! gd136
       CASE (137)
          nuc_spin =    4.5    ! gd137
       CASE (138)
          nuc_spin =    0.0    ! gd138
       CASE (139)
          nuc_spin =    4.5    ! gd139
       CASE (140)
          nuc_spin =    0.0    ! gd140
       CASE (141)
          nuc_spin =    0.5    ! gd141
       CASE (142)
          nuc_spin =    0.0    ! gd142
       CASE (143)
          nuc_spin =    0.5    ! gd143
       CASE (144)
          nuc_spin =    0.0    ! gd144
       CASE (145)
          nuc_spin =    0.5    ! gd145
       CASE (146)
          nuc_spin =    0.0    ! gd146
       CASE (147)
          nuc_spin =    3.5    ! gd147
       CASE (148)
          nuc_spin =    0.0    ! gd148
       CASE (149)
          nuc_spin =    3.5    ! gd149
       CASE (150)
          nuc_spin =    0.0    ! gd150
       CASE (151)
          nuc_spin =    3.5    ! gd151
       CASE (152)
          nuc_spin =    0.0    ! gd152
       CASE (153)
          nuc_spin =    1.5    ! gd153
       CASE (154)
          nuc_spin =    0.0    ! gd154
       CASE (155)
          nuc_spin =    1.5    ! gd155
       CASE (156)
          nuc_spin =    0.0    ! gd156
       CASE (157)
          nuc_spin =    1.5    ! gd157
       CASE (158)
          nuc_spin =    0.0    ! gd158
       CASE (159)
          nuc_spin =    1.5    ! gd159
       CASE (160)
          nuc_spin =    0.0    ! gd160
       CASE (161)
          nuc_spin =    2.5    ! gd161
       CASE (162)
          nuc_spin =    0.0    ! gd162
       CASE (163)
          nuc_spin =    2.5    ! gd163
       CASE (164)
          nuc_spin =    0.0    ! gd164
       CASE (165)
          nuc_spin =    3.5    ! gd165
       CASE (166)
          nuc_spin =    0.0    ! gd166
       CASE (167)
          nuc_spin =    2.5    ! gd167
       CASE (168)
          nuc_spin =    0.0    ! gd168
       CASE (169)
          nuc_spin =    3.5    ! gd169
       CASE (170)
          nuc_spin =    0.0    ! gd170
       CASE (171)
          nuc_spin =    4.5    ! gd171
       CASE (172)
          nuc_spin =    0.0    ! gd172
       CASE (173)
          nuc_spin =    0.5    ! gd173
       CASE (174)
          nuc_spin =    0.0    ! gd174
       CASE (175)
          nuc_spin =    1.5    ! gd175
       CASE (176)
          nuc_spin =    0.0    ! gd176
       CASE (177)
          nuc_spin =    5.5    ! gd177
       CASE (178)
          nuc_spin =    0.0    ! gd178
       CASE (179)
          nuc_spin =    3.5    ! gd179
       CASE (180)
          nuc_spin =    0.0    ! gd180
       CASE (181)
          nuc_spin =    4.5    ! gd181
       CASE (182)
          nuc_spin =    0.0    ! gd182
       CASE (183)
          nuc_spin =    0.5    ! gd183
       CASE (184)
          nuc_spin =    0.0    ! gd184
       CASE (185)
          nuc_spin =    0.5    ! gd185
       CASE (186)
          nuc_spin =    0.0    ! gd186
       CASE (187)
          nuc_spin =    1.5    ! gd187
       CASE (188)
          nuc_spin =    0.0    ! gd188
       CASE (189)
          nuc_spin =    0.5    ! gd189
       CASE (190)
          nuc_spin =    0.0    ! gd190
       CASE (191)
          nuc_spin =    4.5    ! gd191
       CASE (192)
          nuc_spin =    0.0    ! gd192
       CASE (193)
          nuc_spin =    1.5    ! gd193
       CASE (194)
          nuc_spin =    0.0    ! gd194
       CASE (195)
          nuc_spin =    0.5    ! gd195
       CASE (196)
          nuc_spin =    0.0    ! gd196
       CASE (197)
          nuc_spin =    2.5    ! gd197
       CASE (198)
          nuc_spin =    0.0    ! gd198
       CASE (199)
          nuc_spin =    1.5    ! gd199
       CASE (200)
          nuc_spin =    0.0    ! gd200
       CASE (201)
          nuc_spin =    1.5    ! gd201
       CASE (202)
          nuc_spin =    0.0    ! gd202
       CASE (203)
          nuc_spin =    2.5    ! gd203
       CASE (204)
          nuc_spin =    0.0    ! gd204
       CASE (205)
          nuc_spin =    2.5    ! gd205
       CASE (206)
          nuc_spin =    0.0    ! gd206
       CASE (207)
          nuc_spin =    0.5    ! gd207
       CASE (208)
          nuc_spin =    0.0    ! gd208
       CASE (209)
          nuc_spin =    3.5    ! gd209
       CASE (210)
          nuc_spin =    0.0    ! gd210
       CASE (211)
          nuc_spin =    2.5    ! gd211
       CASE (212)
          nuc_spin =    0.0    ! gd212
       CASE (213)
          nuc_spin =    3.5    ! gd213
       CASE (214)
          nuc_spin =    0.0    ! gd214
       END SELECT
    CASE (65)
       SELECT CASE (a)
       CASE (130)
          nuc_spin =    3.0    ! tb130
       CASE (131)
          nuc_spin =    2.5    ! tb131
       CASE (132)
          nuc_spin =    3.0    ! tb132
       CASE (133)
          nuc_spin =    2.5    ! tb133
       CASE (134)
          nuc_spin =    2.0    ! tb134
       CASE (135)
          nuc_spin =    1.5    ! tb135
       CASE (136)
          nuc_spin =    1.0    ! tb136
       CASE (137)
          nuc_spin =    1.5    ! tb137
       CASE (138)
          nuc_spin =    5.0    ! tb138
       CASE (139)
          nuc_spin =    1.5    ! tb139
       CASE (140)
          nuc_spin =    5.0    ! tb140
       CASE (141)
          nuc_spin =    2.5    ! tb141
       CASE (142)
          nuc_spin =    1.0    ! tb142
       CASE (143)
          nuc_spin =    5.5    ! tb143
       CASE (144)
          nuc_spin =    1.0    ! tb144
       CASE (145)
          nuc_spin =    0.5    ! tb145
       CASE (146)
          nuc_spin =    1.0    ! tb146
       CASE (147)
          nuc_spin =    0.5    ! tb147
       CASE (148)
          nuc_spin =    2.0    ! tb148
       CASE (149)
          nuc_spin =    0.5    ! tb149
       CASE (150)
          nuc_spin =    2.0    ! tb150
       CASE (151)
          nuc_spin =    0.5    ! tb151
       CASE (152)
          nuc_spin =    2.0    ! tb152
       CASE (153)
          nuc_spin =    2.5    ! tb153
       CASE (154)
          nuc_spin =    1.0    ! tb154
       CASE (155)
          nuc_spin =    1.5    ! tb155
       CASE (156)
          nuc_spin =    3.0    ! tb156
       CASE (157)
          nuc_spin =    1.5    ! tb157
       CASE (158)
          nuc_spin =    3.0    ! tb158
       CASE (159)
          nuc_spin =    1.5    ! tb159
       CASE (160)
          nuc_spin =    3.0    ! tb160
       CASE (161)
          nuc_spin =    1.5    ! tb161
       CASE (162)
          nuc_spin =    1.0    ! tb162
       CASE (163)
          nuc_spin =    1.5    ! tb163
       CASE (164)
          nuc_spin =    5.0    ! tb164
       CASE (165)
          nuc_spin =    1.5    ! tb165
       CASE (166)
          nuc_spin =    4.0    ! tb166
       CASE (167)
          nuc_spin =    1.5    ! tb167
       CASE (168)
          nuc_spin =    3.0    ! tb168
       CASE (169)
          nuc_spin =    1.5    ! tb169
       CASE (170)
          nuc_spin =    2.0    ! tb170
       CASE (171)
          nuc_spin =    1.5    ! tb171
       CASE (172)
          nuc_spin =    3.0    ! tb172
       CASE (173)
          nuc_spin =    1.5    ! tb173
       CASE (174)
          nuc_spin =    2.0    ! tb174
       CASE (175)
          nuc_spin =    1.5    ! tb175
       CASE (176)
          nuc_spin =    0.0    ! tb176
       CASE (177)
          nuc_spin =    2.5    ! tb177
       CASE (178)
          nuc_spin =    6.0    ! tb178
       CASE (179)
          nuc_spin =    2.5    ! tb179
       CASE (180)
          nuc_spin =    1.0    ! tb180
       CASE (181)
          nuc_spin =    2.5    ! tb181
       CASE (182)
          nuc_spin =    5.0    ! tb182
       CASE (183)
          nuc_spin =    2.5    ! tb183
       CASE (184)
          nuc_spin =    1.0    ! tb184
       CASE (185)
          nuc_spin =    1.5    ! tb185
       CASE (186)
          nuc_spin =    5.0    ! tb186
       CASE (187)
          nuc_spin =    4.5    ! tb187
       CASE (188)
          nuc_spin =    3.0    ! tb188
       CASE (189)
          nuc_spin =    0.5    ! tb189
       CASE (190)
          nuc_spin =    5.0    ! tb190
       CASE (191)
          nuc_spin =    1.5    ! tb191
       CASE (192)
          nuc_spin =    6.0    ! tb192
       CASE (193)
          nuc_spin =    5.5    ! tb193
       CASE (194)
          nuc_spin =    1.0    ! tb194
       CASE (195)
          nuc_spin =    0.5    ! tb195
       CASE (196)
          nuc_spin =    1.0    ! tb196
       CASE (197)
          nuc_spin =    2.5    ! tb197
       CASE (198)
          nuc_spin =    3.0    ! tb198
       CASE (199)
          nuc_spin =    1.5    ! tb199
       CASE (200)
          nuc_spin =    1.0    ! tb200
       CASE (201)
          nuc_spin =    2.5    ! tb201
       CASE (202)
          nuc_spin =    4.0    ! tb202
       CASE (203)
          nuc_spin =    1.5    ! tb203
       CASE (204)
          nuc_spin =    3.0    ! tb204
       CASE (205)
          nuc_spin =    1.5    ! tb205
       CASE (206)
          nuc_spin =    3.0    ! tb206
       CASE (207)
          nuc_spin =    1.5    ! tb207
       CASE (208)
          nuc_spin =    1.0    ! tb208
       CASE (209)
          nuc_spin =    1.5    ! tb209
       CASE (210)
          nuc_spin =    2.0    ! tb210
       CASE (211)
          nuc_spin =    1.5    ! tb211
       CASE (212)
          nuc_spin =    1.0    ! tb212
       CASE (213)
          nuc_spin =    1.5    ! tb213
       CASE (214)
          nuc_spin =    4.0    ! tb214
       CASE (215)
          nuc_spin =    1.5    ! tb215
       CASE (216)
          nuc_spin =    5.0    ! tb216
       CASE (217)
          nuc_spin =    1.5    ! tb217
       CASE (218)
          nuc_spin =    1.0    ! tb218
       END SELECT
    CASE (66)
       SELECT CASE (a)
       CASE (133)
          nuc_spin =    0.5    ! dy133
       CASE (134)
          nuc_spin =    0.0    ! dy134
       CASE (135)
          nuc_spin =    3.5    ! dy135
       CASE (136)
          nuc_spin =    0.0    ! dy136
       CASE (137)
          nuc_spin =    2.5    ! dy137
       CASE (138)
          nuc_spin =    0.0    ! dy138
       CASE (139)
          nuc_spin =    4.5    ! dy139
       CASE (140)
          nuc_spin =    0.0    ! dy140
       CASE (141)
          nuc_spin =    4.5    ! dy141
       CASE (142)
          nuc_spin =    0.0    ! dy142
       CASE (143)
          nuc_spin =    1.5    ! dy143
       CASE (144)
          nuc_spin =    0.0    ! dy144
       CASE (145)
          nuc_spin =    0.5    ! dy145
       CASE (146)
          nuc_spin =    0.0    ! dy146
       CASE (147)
          nuc_spin =    0.5    ! dy147
       CASE (148)
          nuc_spin =    0.0    ! dy148
       CASE (149)
          nuc_spin =    3.5    ! dy149
       CASE (150)
          nuc_spin =    0.0    ! dy150
       CASE (151)
          nuc_spin =    3.5    ! dy151
       CASE (152)
          nuc_spin =    0.0    ! dy152
       CASE (153)
          nuc_spin =    3.5    ! dy153
       CASE (154)
          nuc_spin =    0.0    ! dy154
       CASE (155)
          nuc_spin =    1.5    ! dy155
       CASE (156)
          nuc_spin =    0.0    ! dy156
       CASE (157)
          nuc_spin =    1.5    ! dy157
       CASE (158)
          nuc_spin =    0.0    ! dy158
       CASE (159)
          nuc_spin =    1.5    ! dy159
       CASE (160)
          nuc_spin =    0.0    ! dy160
       CASE (161)
          nuc_spin =    2.5    ! dy161
       CASE (162)
          nuc_spin =    0.0    ! dy162
       CASE (163)
          nuc_spin =    2.5    ! dy163
       CASE (164)
          nuc_spin =    0.0    ! dy164
       CASE (165)
          nuc_spin =    3.5    ! dy165
       CASE (166)
          nuc_spin =    0.0    ! dy166
       CASE (167)
          nuc_spin =    0.5    ! dy167
       CASE (168)
          nuc_spin =    0.0    ! dy168
       CASE (169)
          nuc_spin =    2.5    ! dy169
       CASE (170)
          nuc_spin =    0.0    ! dy170
       CASE (171)
          nuc_spin =    3.5    ! dy171
       CASE (172)
          nuc_spin =    0.0    ! dy172
       CASE (173)
          nuc_spin =    4.5    ! dy173
       CASE (174)
          nuc_spin =    0.0    ! dy174
       CASE (175)
          nuc_spin =    0.5    ! dy175
       CASE (176)
          nuc_spin =    0.0    ! dy176
       CASE (177)
          nuc_spin =    1.5    ! dy177
       CASE (178)
          nuc_spin =    0.0    ! dy178
       CASE (179)
          nuc_spin =    5.5    ! dy179
       CASE (180)
          nuc_spin =    0.0    ! dy180
       CASE (181)
          nuc_spin =    3.5    ! dy181
       CASE (182)
          nuc_spin =    0.0    ! dy182
       CASE (183)
          nuc_spin =    4.5    ! dy183
       CASE (184)
          nuc_spin =    0.0    ! dy184
       CASE (185)
          nuc_spin =    0.5    ! dy185
       CASE (186)
          nuc_spin =    0.0    ! dy186
       CASE (187)
          nuc_spin =    0.5    ! dy187
       CASE (188)
          nuc_spin =    0.0    ! dy188
       CASE (189)
          nuc_spin =    1.5    ! dy189
       CASE (190)
          nuc_spin =    0.0    ! dy190
       CASE (191)
          nuc_spin =    0.5    ! dy191
       CASE (192)
          nuc_spin =    0.0    ! dy192
       CASE (193)
          nuc_spin =    4.5    ! dy193
       CASE (194)
          nuc_spin =    0.0    ! dy194
       CASE (195)
          nuc_spin =    1.5    ! dy195
       CASE (196)
          nuc_spin =    0.0    ! dy196
       CASE (197)
          nuc_spin =    0.5    ! dy197
       CASE (198)
          nuc_spin =    0.0    ! dy198
       CASE (199)
          nuc_spin =    2.5    ! dy199
       CASE (200)
          nuc_spin =    0.0    ! dy200
       CASE (201)
          nuc_spin =    1.5    ! dy201
       CASE (202)
          nuc_spin =    0.0    ! dy202
       CASE (203)
          nuc_spin =    1.5    ! dy203
       CASE (204)
          nuc_spin =    0.0    ! dy204
       CASE (205)
          nuc_spin =    2.5    ! dy205
       CASE (206)
          nuc_spin =    0.0    ! dy206
       CASE (207)
          nuc_spin =    2.5    ! dy207
       CASE (208)
          nuc_spin =    0.0    ! dy208
       CASE (209)
          nuc_spin =    0.5    ! dy209
       CASE (210)
          nuc_spin =    0.0    ! dy210
       CASE (211)
          nuc_spin =    3.5    ! dy211
       CASE (212)
          nuc_spin =    0.0    ! dy212
       CASE (213)
          nuc_spin =    2.5    ! dy213
       CASE (214)
          nuc_spin =    0.0    ! dy214
       CASE (215)
          nuc_spin =    3.5    ! dy215
       CASE (216)
          nuc_spin =    0.0    ! dy216
       CASE (217)
          nuc_spin =    4.5    ! dy217
       CASE (218)
          nuc_spin =    0.0    ! dy218
       CASE (219)
          nuc_spin =    0.5    ! dy219
       CASE (220)
          nuc_spin =    0.0    ! dy220
       CASE (221)
          nuc_spin =    3.5    ! dy221
       END SELECT
    CASE (67)
       SELECT CASE (a)
       CASE (136)
          nuc_spin =    4.0    ! ho136
       CASE (137)
          nuc_spin =    3.5    ! ho137
       CASE (138)
          nuc_spin =    1.0    ! ho138
       CASE (139)
          nuc_spin =    3.5    ! ho139
       CASE (140)
          nuc_spin =    1.0    ! ho140
       CASE (141)
          nuc_spin =    3.5    ! ho141
       CASE (142)
          nuc_spin =    1.0    ! ho142
       CASE (143)
          nuc_spin =    3.5    ! ho143
       CASE (144)
          nuc_spin =    2.0    ! ho144
       CASE (145)
          nuc_spin =    5.5    ! ho145
       CASE (146)
          nuc_spin =    0.0    ! ho146
       CASE (147)
          nuc_spin =    5.5    ! ho147
       CASE (148)
          nuc_spin =    1.0    ! ho148
       CASE (149)
          nuc_spin =    5.5    ! ho149
       CASE (150)
          nuc_spin =    2.0    ! ho150
       CASE (151)
          nuc_spin =    5.5    ! ho151
       CASE (152)
          nuc_spin =    2.0    ! ho152
       CASE (153)
          nuc_spin =    5.5    ! ho153
       CASE (154)
          nuc_spin =    2.0    ! ho154
       CASE (155)
          nuc_spin =    2.5    ! ho155
       CASE (156)
          nuc_spin =    4.0    ! ho156
       CASE (157)
          nuc_spin =    3.5    ! ho157
       CASE (158)
          nuc_spin =    5.0    ! ho158
       CASE (159)
          nuc_spin =    3.5    ! ho159
       CASE (160)
          nuc_spin =    5.0    ! ho160
       CASE (161)
          nuc_spin =    3.5    ! ho161
       CASE (162)
          nuc_spin =    1.0    ! ho162
       CASE (163)
          nuc_spin =    3.5    ! ho163
       CASE (164)
          nuc_spin =    1.0    ! ho164
       CASE (165)
          nuc_spin =    3.5    ! ho165
       CASE (166)
          nuc_spin =    0.0    ! ho166
       CASE (167)
          nuc_spin =    3.5    ! ho167
       CASE (168)
          nuc_spin =    3.0    ! ho168
       CASE (169)
          nuc_spin =    3.5    ! ho169
       CASE (170)
          nuc_spin =    6.0    ! ho170
       CASE (171)
          nuc_spin =    3.5    ! ho171
       CASE (172)
          nuc_spin =    4.0    ! ho172
       CASE (173)
          nuc_spin =    3.5    ! ho173
       CASE (174)
          nuc_spin =    5.0    ! ho174
       CASE (175)
          nuc_spin =    3.5    ! ho175
       CASE (176)
          nuc_spin =    3.0    ! ho176
       CASE (177)
          nuc_spin =    3.5    ! ho177
       CASE (178)
          nuc_spin =    4.0    ! ho178
       CASE (179)
          nuc_spin =    3.5    ! ho179
       CASE (180)
          nuc_spin =    2.0    ! ho180
       CASE (181)
          nuc_spin =    3.5    ! ho181
       CASE (182)
          nuc_spin =    4.0    ! ho182
       CASE (183)
          nuc_spin =    3.5    ! ho183
       CASE (184)
          nuc_spin =    1.0    ! ho184
       CASE (185)
          nuc_spin =    3.5    ! ho185
       CASE (186)
          nuc_spin =    1.0    ! ho186
       CASE (187)
          nuc_spin =    0.5    ! ho187
       CASE (188)
          nuc_spin =    0.0    ! ho188
       CASE (189)
          nuc_spin =    0.5    ! ho189
       CASE (190)
          nuc_spin =    2.0    ! ho190
       CASE (191)
          nuc_spin =    4.5    ! ho191
       CASE (192)
          nuc_spin =    5.0    ! ho192
       CASE (193)
          nuc_spin =    1.5    ! ho193
       CASE (194)
          nuc_spin =    0.0    ! ho194
       CASE (195)
          nuc_spin =    4.5    ! ho195
       CASE (196)
          nuc_spin =    2.0    ! ho196
       CASE (197)
          nuc_spin =    1.5    ! ho197
       CASE (198)
          nuc_spin =    1.0    ! ho198
       CASE (199)
          nuc_spin =    1.5    ! ho199
       CASE (200)
          nuc_spin =    3.0    ! ho200
       CASE (201)
          nuc_spin =    2.5    ! ho201
       CASE (202)
          nuc_spin =    1.0    ! ho202
       CASE (203)
          nuc_spin =    1.5    ! ho203
       CASE (204)
          nuc_spin =    2.0    ! ho204
       CASE (205)
          nuc_spin =    1.5    ! ho205
       CASE (206)
          nuc_spin =    1.0    ! ho206
       CASE (207)
          nuc_spin =    3.5    ! ho207
       CASE (208)
          nuc_spin =    4.0    ! ho208
       CASE (209)
          nuc_spin =    3.5    ! ho209
       CASE (210)
          nuc_spin =    3.0    ! ho210
       CASE (211)
          nuc_spin =    3.5    ! ho211
       CASE (212)
          nuc_spin =    4.0    ! ho212
       CASE (213)
          nuc_spin =    3.5    ! ho213
       CASE (214)
          nuc_spin =    4.0    ! ho214
       CASE (215)
          nuc_spin =    3.5    ! ho215
       CASE (216)
          nuc_spin =    0.0    ! ho216
       CASE (217)
          nuc_spin =    3.5    ! ho217
       CASE (218)
          nuc_spin =    1.0    ! ho218
       CASE (219)
          nuc_spin =    3.5    ! ho219
       CASE (220)
          nuc_spin =    4.0    ! ho220
       CASE (221)
          nuc_spin =    3.5    ! ho221
       CASE (222)
          nuc_spin =    0.0    ! ho222
       CASE (223)
          nuc_spin =    3.5    ! ho223
       CASE (224)
          nuc_spin =    0.0    ! ho224
       END SELECT
    CASE (68)
       SELECT CASE (a)
       CASE (138)
          nuc_spin =    0.0    ! er138
       CASE (139)
          nuc_spin =    4.5    ! er139
       CASE (140)
          nuc_spin =    0.0    ! er140
       CASE (141)
          nuc_spin =    4.5    ! er141
       CASE (142)
          nuc_spin =    0.0    ! er142
       CASE (143)
          nuc_spin =    4.5    ! er143
       CASE (144)
          nuc_spin =    0.0    ! er144
       CASE (145)
          nuc_spin =    1.5    ! er145
       CASE (146)
          nuc_spin =    0.0    ! er146
       CASE (147)
          nuc_spin =    5.5    ! er147
       CASE (148)
          nuc_spin =    0.0    ! er148
       CASE (149)
          nuc_spin =    0.5    ! er149
       CASE (150)
          nuc_spin =    0.0    ! er150
       CASE (151)
          nuc_spin =    3.5    ! er151
       CASE (152)
          nuc_spin =    0.0    ! er152
       CASE (153)
          nuc_spin =    3.5    ! er153
       CASE (154)
          nuc_spin =    0.0    ! er154
       CASE (155)
          nuc_spin =    3.5    ! er155
       CASE (156)
          nuc_spin =    0.0    ! er156
       CASE (157)
          nuc_spin =    1.5    ! er157
       CASE (158)
          nuc_spin =    0.0    ! er158
       CASE (159)
          nuc_spin =    1.5    ! er159
       CASE (160)
          nuc_spin =    0.0    ! er160
       CASE (161)
          nuc_spin =    1.5    ! er161
       CASE (162)
          nuc_spin =    0.0    ! er162
       CASE (163)
          nuc_spin =    2.5    ! er163
       CASE (164)
          nuc_spin =    0.0    ! er164
       CASE (165)
          nuc_spin =    2.5    ! er165
       CASE (166)
          nuc_spin =    0.0    ! er166
       CASE (167)
          nuc_spin =    3.5    ! er167
       CASE (168)
          nuc_spin =    0.0    ! er168
       CASE (169)
          nuc_spin =    0.5    ! er169
       CASE (170)
          nuc_spin =    0.0    ! er170
       CASE (171)
          nuc_spin =    2.5    ! er171
       CASE (172)
          nuc_spin =    0.0    ! er172
       CASE (173)
          nuc_spin =    3.5    ! er173
       CASE (174)
          nuc_spin =    0.0    ! er174
       CASE (175)
          nuc_spin =    4.5    ! er175
       CASE (176)
          nuc_spin =    0.0    ! er176
       CASE (177)
          nuc_spin =    0.5    ! er177
       CASE (178)
          nuc_spin =    0.0    ! er178
       CASE (179)
          nuc_spin =    1.5    ! er179
       CASE (180)
          nuc_spin =    0.0    ! er180
       CASE (181)
          nuc_spin =    5.5    ! er181
       CASE (182)
          nuc_spin =    0.0    ! er182
       CASE (183)
          nuc_spin =    3.5    ! er183
       CASE (184)
          nuc_spin =    0.0    ! er184
       CASE (185)
          nuc_spin =    4.5    ! er185
       CASE (186)
          nuc_spin =    0.0    ! er186
       CASE (187)
          nuc_spin =    0.5    ! er187
       CASE (188)
          nuc_spin =    0.0    ! er188
       CASE (189)
          nuc_spin =    0.5    ! er189
       CASE (190)
          nuc_spin =    0.0    ! er190
       CASE (191)
          nuc_spin =    1.5    ! er191
       CASE (192)
          nuc_spin =    0.0    ! er192
       CASE (193)
          nuc_spin =    0.5    ! er193
       CASE (194)
          nuc_spin =    0.0    ! er194
       CASE (195)
          nuc_spin =    4.5    ! er195
       CASE (196)
          nuc_spin =    0.0    ! er196
       CASE (197)
          nuc_spin =    1.5    ! er197
       CASE (198)
          nuc_spin =    0.0    ! er198
       CASE (199)
          nuc_spin =    0.5    ! er199
       CASE (200)
          nuc_spin =    0.0    ! er200
       CASE (201)
          nuc_spin =    0.5    ! er201
       CASE (202)
          nuc_spin =    0.0    ! er202
       CASE (203)
          nuc_spin =    1.5    ! er203
       CASE (204)
          nuc_spin =    0.0    ! er204
       CASE (205)
          nuc_spin =    1.5    ! er205
       CASE (206)
          nuc_spin =    0.0    ! er206
       CASE (207)
          nuc_spin =    2.5    ! er207
       CASE (208)
          nuc_spin =    0.0    ! er208
       CASE (209)
          nuc_spin =    2.5    ! er209
       CASE (210)
          nuc_spin =    0.0    ! er210
       CASE (211)
          nuc_spin =    0.5    ! er211
       CASE (212)
          nuc_spin =    0.0    ! er212
       CASE (213)
          nuc_spin =    3.5    ! er213
       CASE (214)
          nuc_spin =    0.0    ! er214
       CASE (215)
          nuc_spin =    2.5    ! er215
       CASE (216)
          nuc_spin =    0.0    ! er216
       CASE (217)
          nuc_spin =    3.5    ! er217
       CASE (218)
          nuc_spin =    0.0    ! er218
       CASE (219)
          nuc_spin =    4.5    ! er219
       CASE (220)
          nuc_spin =    0.0    ! er220
       CASE (221)
          nuc_spin =    0.5    ! er221
       CASE (222)
          nuc_spin =    0.0    ! er222
       CASE (223)
          nuc_spin =    1.5    ! er223
       CASE (224)
          nuc_spin =    0.0    ! er224
       CASE (225)
          nuc_spin =    3.5    ! er225
       CASE (226)
          nuc_spin =    0.0    ! er226
       CASE (227)
          nuc_spin =    5.5    ! er227
       END SELECT
    CASE (69)
       SELECT CASE (a)
       CASE (141)
          nuc_spin =    0.5    ! tm141
       CASE (142)
          nuc_spin =    3.0    ! tm142
       CASE (143)
          nuc_spin =    0.5    ! tm143
       CASE (144)
          nuc_spin =    3.0    ! tm144
       CASE (145)
          nuc_spin =    0.5    ! tm145
       CASE (146)
          nuc_spin =    4.0    ! tm146
       CASE (147)
          nuc_spin =    5.5    ! tm147
       CASE (148)
          nuc_spin =    3.0    ! tm148
       CASE (149)
          nuc_spin =    5.5    ! tm149
       CASE (150)
          nuc_spin =    6.0    ! tm150
       CASE (151)
          nuc_spin =    5.5    ! tm151
       CASE (152)
          nuc_spin =    2.0    ! tm152
       CASE (153)
          nuc_spin =    5.5    ! tm153
       CASE (154)
          nuc_spin =    2.0    ! tm154
       CASE (155)
          nuc_spin =    5.5    ! tm155
       CASE (156)
          nuc_spin =    2.0    ! tm156
       CASE (157)
          nuc_spin =    0.5    ! tm157
       CASE (158)
          nuc_spin =    2.0    ! tm158
       CASE (159)
          nuc_spin =    2.5    ! tm159
       CASE (160)
          nuc_spin =    1.0    ! tm160
       CASE (161)
          nuc_spin =    3.5    ! tm161
       CASE (162)
          nuc_spin =    1.0    ! tm162
       CASE (163)
          nuc_spin =    0.5    ! tm163
       CASE (164)
          nuc_spin =    1.0    ! tm164
       CASE (165)
          nuc_spin =    0.5    ! tm165
       CASE (166)
          nuc_spin =    2.0    ! tm166
       CASE (167)
          nuc_spin =    0.5    ! tm167
       CASE (168)
          nuc_spin =    3.0    ! tm168
       CASE (169)
          nuc_spin =    0.5    ! tm169
       CASE (170)
          nuc_spin =    1.0    ! tm170
       CASE (171)
          nuc_spin =    0.5    ! tm171
       CASE (172)
          nuc_spin =    2.0    ! tm172
       CASE (173)
          nuc_spin =    0.5    ! tm173
       CASE (174)
          nuc_spin =    4.0    ! tm174
       CASE (175)
          nuc_spin =    0.5    ! tm175
       CASE (176)
          nuc_spin =    4.0    ! tm176
       CASE (177)
          nuc_spin =    0.5    ! tm177
       CASE (178)
          nuc_spin =    0.0    ! tm178
       CASE (179)
          nuc_spin =    0.5    ! tm179
       CASE (180)
          nuc_spin =    2.0    ! tm180
       CASE (181)
          nuc_spin =    0.5    ! tm181
       CASE (182)
          nuc_spin =    5.0    ! tm182
       CASE (183)
          nuc_spin =    0.5    ! tm183
       CASE (184)
          nuc_spin =    4.0    ! tm184
       CASE (185)
          nuc_spin =    0.5    ! tm185
       CASE (186)
          nuc_spin =    4.0    ! tm186
       CASE (187)
          nuc_spin =    0.5    ! tm187
       CASE (188)
          nuc_spin =    4.0    ! tm188
       CASE (189)
          nuc_spin =    3.5    ! tm189
       CASE (190)
          nuc_spin =    3.0    ! tm190
       CASE (191)
          nuc_spin =    3.5    ! tm191
       CASE (192)
          nuc_spin =    4.0    ! tm192
       CASE (193)
          nuc_spin =    3.5    ! tm193
       CASE (194)
          nuc_spin =    3.0    ! tm194
       CASE (195)
          nuc_spin =    2.5    ! tm195
       CASE (196)
          nuc_spin =    5.0    ! tm196
       CASE (197)
          nuc_spin =    3.5    ! tm197
       CASE (198)
          nuc_spin =    1.0    ! tm198
       CASE (199)
          nuc_spin =    2.5    ! tm199
       CASE (200)
          nuc_spin =    1.0    ! tm200
       CASE (201)
          nuc_spin =    2.5    ! tm201
       CASE (202)
          nuc_spin =    2.0    ! tm202
       CASE (203)
          nuc_spin =    2.5    ! tm203
       CASE (204)
          nuc_spin =    3.0    ! tm204
       CASE (205)
          nuc_spin =    2.5    ! tm205
       CASE (206)
          nuc_spin =    2.0    ! tm206
       CASE (207)
          nuc_spin =    3.5    ! tm207
       CASE (208)
          nuc_spin =    4.0    ! tm208
       CASE (209)
          nuc_spin =    3.5    ! tm209
       CASE (210)
          nuc_spin =    1.0    ! tm210
       CASE (211)
          nuc_spin =    3.5    ! tm211
       CASE (212)
          nuc_spin =    4.0    ! tm212
       CASE (213)
          nuc_spin =    3.5    ! tm213
       CASE (214)
          nuc_spin =    4.0    ! tm214
       CASE (215)
          nuc_spin =    3.5    ! tm215
       CASE (216)
          nuc_spin =    1.0    ! tm216
       CASE (217)
          nuc_spin =    0.5    ! tm217
       CASE (218)
          nuc_spin =    3.0    ! tm218
       CASE (219)
          nuc_spin =    0.5    ! tm219
       CASE (220)
          nuc_spin =    4.0    ! tm220
       CASE (221)
          nuc_spin =    0.5    ! tm221
       CASE (222)
          nuc_spin =    1.0    ! tm222
       CASE (223)
          nuc_spin =    0.5    ! tm223
       CASE (224)
          nuc_spin =    3.0    ! tm224
       CASE (225)
          nuc_spin =    0.5    ! tm225
       CASE (226)
          nuc_spin =    3.0    ! tm226
       CASE (227)
          nuc_spin =    0.5    ! tm227
       CASE (228)
          nuc_spin =    6.0    ! tm228
       CASE (229)
          nuc_spin =    0.5    ! tm229
       CASE (230)
          nuc_spin =    5.0    ! tm230
       END SELECT
    CASE (70)
       SELECT CASE (a)
       CASE (143)
          nuc_spin =    2.5    ! yb143
       CASE (144)
          nuc_spin =    0.0    ! yb144
       CASE (145)
          nuc_spin =    3.5    ! yb145
       CASE (146)
          nuc_spin =    0.0    ! yb146
       CASE (147)
          nuc_spin =    1.5    ! yb147
       CASE (148)
          nuc_spin =    0.0    ! yb148
       CASE (149)
          nuc_spin =    0.5    ! yb149
       CASE (150)
          nuc_spin =    0.0    ! yb150
       CASE (151)
          nuc_spin =    0.5    ! yb151
       CASE (152)
          nuc_spin =    0.0    ! yb152
       CASE (153)
          nuc_spin =    3.5    ! yb153
       CASE (154)
          nuc_spin =    0.0    ! yb154
       CASE (155)
          nuc_spin =    3.5    ! yb155
       CASE (156)
          nuc_spin =    0.0    ! yb156
       CASE (157)
          nuc_spin =    3.5    ! yb157
       CASE (158)
          nuc_spin =    0.0    ! yb158
       CASE (159)
          nuc_spin =    2.5    ! yb159
       CASE (160)
          nuc_spin =    0.0    ! yb160
       CASE (161)
          nuc_spin =    1.5    ! yb161
       CASE (162)
          nuc_spin =    0.0    ! yb162
       CASE (163)
          nuc_spin =    1.5    ! yb163
       CASE (164)
          nuc_spin =    0.0    ! yb164
       CASE (165)
          nuc_spin =    2.5    ! yb165
       CASE (166)
          nuc_spin =    0.0    ! yb166
       CASE (167)
          nuc_spin =    2.5    ! yb167
       CASE (168)
          nuc_spin =    0.0    ! yb168
       CASE (169)
          nuc_spin =    3.5    ! yb169
       CASE (170)
          nuc_spin =    0.0    ! yb170
       CASE (171)
          nuc_spin =    0.5    ! yb171
       CASE (172)
          nuc_spin =    0.0    ! yb172
       CASE (173)
          nuc_spin =    2.5    ! yb173
       CASE (174)
          nuc_spin =    0.0    ! yb174
       CASE (175)
          nuc_spin =    3.5    ! yb175
       CASE (176)
          nuc_spin =    0.0    ! yb176
       CASE (177)
          nuc_spin =    4.5    ! yb177
       CASE (178)
          nuc_spin =    0.0    ! yb178
       CASE (179)
          nuc_spin =    0.5    ! yb179
       CASE (180)
          nuc_spin =    0.0    ! yb180
       CASE (181)
          nuc_spin =    1.5    ! yb181
       CASE (182)
          nuc_spin =    0.0    ! yb182
       CASE (183)
          nuc_spin =    5.5    ! yb183
       CASE (184)
          nuc_spin =    0.0    ! yb184
       CASE (185)
          nuc_spin =    3.5    ! yb185
       CASE (186)
          nuc_spin =    0.0    ! yb186
       CASE (187)
          nuc_spin =    4.5    ! yb187
       CASE (188)
          nuc_spin =    0.0    ! yb188
       CASE (189)
          nuc_spin =    0.5    ! yb189
       CASE (190)
          nuc_spin =    0.0    ! yb190
       CASE (191)
          nuc_spin =    0.5    ! yb191
       CASE (192)
          nuc_spin =    0.0    ! yb192
       CASE (193)
          nuc_spin =    1.5    ! yb193
       CASE (194)
          nuc_spin =    0.0    ! yb194
       CASE (195)
          nuc_spin =    0.5    ! yb195
       CASE (196)
          nuc_spin =    0.0    ! yb196
       CASE (197)
          nuc_spin =    4.5    ! yb197
       CASE (198)
          nuc_spin =    0.0    ! yb198
       CASE (199)
          nuc_spin =    1.5    ! yb199
       CASE (200)
          nuc_spin =    0.0    ! yb200
       CASE (201)
          nuc_spin =    1.5    ! yb201
       CASE (202)
          nuc_spin =    0.0    ! yb202
       CASE (203)
          nuc_spin =    0.5    ! yb203
       CASE (204)
          nuc_spin =    0.0    ! yb204
       CASE (205)
          nuc_spin =    1.5    ! yb205
       CASE (206)
          nuc_spin =    0.0    ! yb206
       CASE (207)
          nuc_spin =    1.5    ! yb207
       CASE (208)
          nuc_spin =    0.0    ! yb208
       CASE (209)
          nuc_spin =    2.5    ! yb209
       CASE (210)
          nuc_spin =    0.0    ! yb210
       CASE (211)
          nuc_spin =    2.5    ! yb211
       CASE (212)
          nuc_spin =    0.0    ! yb212
       CASE (213)
          nuc_spin =    0.5    ! yb213
       CASE (214)
          nuc_spin =    0.0    ! yb214
       CASE (215)
          nuc_spin =    2.5    ! yb215
       CASE (216)
          nuc_spin =    0.0    ! yb216
       CASE (217)
          nuc_spin =    2.5    ! yb217
       CASE (218)
          nuc_spin =    0.0    ! yb218
       CASE (219)
          nuc_spin =    3.5    ! yb219
       CASE (220)
          nuc_spin =    0.0    ! yb220
       CASE (221)
          nuc_spin =    4.5    ! yb221
       CASE (222)
          nuc_spin =    0.0    ! yb222
       CASE (223)
          nuc_spin =    0.5    ! yb223
       CASE (224)
          nuc_spin =    0.0    ! yb224
       CASE (225)
          nuc_spin =    3.5    ! yb225
       CASE (226)
          nuc_spin =    0.0    ! yb226
       CASE (227)
          nuc_spin =    3.5    ! yb227
       CASE (228)
          nuc_spin =    0.0    ! yb228
       CASE (229)
          nuc_spin =    5.5    ! yb229
       CASE (230)
          nuc_spin =    0.0    ! yb230
       CASE (231)
          nuc_spin =    4.5    ! yb231
       CASE (232)
          nuc_spin =    0.0    ! yb232
       CASE (233)
          nuc_spin =    1.5    ! yb233
       CASE (234)
          nuc_spin =    0.0    ! yb234
       END SELECT
    CASE (71)
       SELECT CASE (a)
       CASE (146)
          nuc_spin =    5.0    ! lu146
       CASE (147)
          nuc_spin =    2.5    ! lu147
       CASE (148)
          nuc_spin =    1.0    ! lu148
       CASE (149)
          nuc_spin =    2.5    ! lu149
       CASE (150)
          nuc_spin =    3.0    ! lu150
       CASE (151)
          nuc_spin =    5.5    ! lu151
       CASE (152)
          nuc_spin =    5.0    ! lu152
       CASE (153)
          nuc_spin =    3.5    ! lu153
       CASE (154)
          nuc_spin =    7.0    ! lu154
       CASE (155)
          nuc_spin =    0.5    ! lu155
       CASE (156)
          nuc_spin =    5.0    ! lu156
       CASE (157)
          nuc_spin =    0.5    ! lu157
       CASE (158)
          nuc_spin =    3.0    ! lu158
       CASE (159)
          nuc_spin =    2.5    ! lu159
       CASE (160)
          nuc_spin =    2.0    ! lu160
       CASE (161)
          nuc_spin =    2.5    ! lu161
       CASE (162)
          nuc_spin =    1.0    ! lu162
       CASE (163)
          nuc_spin =    0.5    ! lu163
       CASE (164)
          nuc_spin =    4.0    ! lu164
       CASE (165)
          nuc_spin =    0.5    ! lu165
       CASE (166)
          nuc_spin =    6.0    ! lu166
       CASE (167)
          nuc_spin =    3.5    ! lu167
       CASE (168)
          nuc_spin =    6.0    ! lu168
       CASE (169)
          nuc_spin =    3.5    ! lu169
       CASE (170)
          nuc_spin =    0.0    ! lu170
       CASE (171)
          nuc_spin =    3.5    ! lu171
       CASE (172)
          nuc_spin =    4.0    ! lu172
       CASE (173)
          nuc_spin =    3.5    ! lu173
       CASE (174)
          nuc_spin =    1.0    ! lu174
       CASE (175)
          nuc_spin =    3.5    ! lu175
       CASE (176)
          nuc_spin =    7.0    ! lu176
       CASE (177)
          nuc_spin =    3.5    ! lu177
       CASE (178)
          nuc_spin =    0.0    ! lu178
       CASE (179)
          nuc_spin =    3.5    ! lu179
       CASE (180)
          nuc_spin =    3.0    ! lu180
       CASE (181)
          nuc_spin =    3.5    ! lu181
       CASE (182)
          nuc_spin =    6.5    ! lu182
       CASE (183)
          nuc_spin =    3.5    ! lu183
       CASE (184)
          nuc_spin =    6.0    ! lu184
       CASE (185)
          nuc_spin =    4.5    ! lu185
       CASE (186)
          nuc_spin =    1.0    ! lu186
       CASE (187)
          nuc_spin =    4.5    ! lu187
       CASE (188)
          nuc_spin =    5.0    ! lu188
       CASE (189)
          nuc_spin =    3.5    ! lu189
       CASE (190)
          nuc_spin =    2.0    ! lu190
       CASE (191)
          nuc_spin =    2.5    ! lu191
       CASE (192)
          nuc_spin =    3.0    ! lu192
       CASE (193)
          nuc_spin =    2.5    ! lu193
       CASE (194)
          nuc_spin =    1.0    ! lu194
       CASE (195)
          nuc_spin =    2.5    ! lu195
       CASE (196)
          nuc_spin =    3.0    ! lu196
       CASE (197)
          nuc_spin =    3.5    ! lu197
       CASE (198)
          nuc_spin =    2.0    ! lu198
       CASE (199)
          nuc_spin =    2.5    ! lu199
       CASE (200)
          nuc_spin =    2.0    ! lu200
       CASE (201)
          nuc_spin =    3.5    ! lu201
       CASE (202)
          nuc_spin =    2.0    ! lu202
       CASE (203)
          nuc_spin =    3.5    ! lu203
       CASE (204)
          nuc_spin =    3.0    ! lu204
       CASE (205)
          nuc_spin =    3.5    ! lu205
       CASE (206)
          nuc_spin =    4.0    ! lu206
       CASE (207)
          nuc_spin =    2.5    ! lu207
       CASE (208)
          nuc_spin =    2.0    ! lu208
       CASE (209)
          nuc_spin =    2.5    ! lu209
       CASE (210)
          nuc_spin =    3.0    ! lu210
       CASE (211)
          nuc_spin =    0.5    ! lu211
       CASE (212)
          nuc_spin =    2.0    ! lu212
       CASE (213)
          nuc_spin =    0.5    ! lu213
       CASE (214)
          nuc_spin =    1.0    ! lu214
       CASE (215)
          nuc_spin =    0.5    ! lu215
       CASE (216)
          nuc_spin =    3.0    ! lu216
       CASE (217)
          nuc_spin =    0.5    ! lu217
       CASE (218)
          nuc_spin =    4.0    ! lu218
       CASE (219)
          nuc_spin =    0.5    ! lu219
       CASE (220)
          nuc_spin =    3.0    ! lu220
       CASE (221)
          nuc_spin =    0.5    ! lu221
       CASE (222)
          nuc_spin =    5.0    ! lu222
       CASE (223)
          nuc_spin =    3.5    ! lu223
       CASE (224)
          nuc_spin =    3.0    ! lu224
       CASE (225)
          nuc_spin =    3.5    ! lu225
       CASE (226)
          nuc_spin =    4.0    ! lu226
       CASE (227)
          nuc_spin =    3.5    ! lu227
       CASE (228)
          nuc_spin =    4.0    ! lu228
       CASE (229)
          nuc_spin =    3.5    ! lu229
       CASE (230)
          nuc_spin =    1.0    ! lu230
       CASE (231)
          nuc_spin =    4.5    ! lu231
       CASE (232)
          nuc_spin =    0.0    ! lu232
       CASE (233)
          nuc_spin =    4.5    ! lu233
       CASE (234)
          nuc_spin =    5.0    ! lu234
       CASE (235)
          nuc_spin =    4.5    ! lu235
       CASE (236)
          nuc_spin =    5.0    ! lu236
       CASE (237)
          nuc_spin =    4.5    ! lu237
       END SELECT
    CASE (72)
       SELECT CASE (a)
       CASE (149)
          nuc_spin =    1.5    ! hf149
       CASE (150)
          nuc_spin =    0.0    ! hf150
       CASE (151)
          nuc_spin =    0.5    ! hf151
       CASE (152)
          nuc_spin =    0.0    ! hf152
       CASE (153)
          nuc_spin =    0.5    ! hf153
       CASE (154)
          nuc_spin =    0.0    ! hf154
       CASE (155)
          nuc_spin =    3.5    ! hf155
       CASE (156)
          nuc_spin =    0.0    ! hf156
       CASE (157)
          nuc_spin =    0.5    ! hf157
       CASE (158)
          nuc_spin =    0.0    ! hf158
       CASE (159)
          nuc_spin =    1.5    ! hf159
       CASE (160)
          nuc_spin =    0.0    ! hf160
       CASE (161)
          nuc_spin =    1.5    ! hf161
       CASE (162)
          nuc_spin =    0.0    ! hf162
       CASE (163)
          nuc_spin =    0.5    ! hf163
       CASE (164)
          nuc_spin =    0.0    ! hf164
       CASE (165)
          nuc_spin =    2.5    ! hf165
       CASE (166)
          nuc_spin =    0.0    ! hf166
       CASE (167)
          nuc_spin =    2.5    ! hf167
       CASE (168)
          nuc_spin =    0.0    ! hf168
       CASE (169)
          nuc_spin =    2.5    ! hf169
       CASE (170)
          nuc_spin =    0.0    ! hf170
       CASE (171)
          nuc_spin =    3.5    ! hf171
       CASE (172)
          nuc_spin =    0.0    ! hf172
       CASE (173)
          nuc_spin =    0.5    ! hf173
       CASE (174)
          nuc_spin =    0.0    ! hf174
       CASE (175)
          nuc_spin =    2.5    ! hf175
       CASE (176)
          nuc_spin =    0.0    ! hf176
       CASE (177)
          nuc_spin =    3.5    ! hf177
       CASE (178)
          nuc_spin =    0.0    ! hf178
       CASE (179)
          nuc_spin =    4.5    ! hf179
       CASE (180)
          nuc_spin =    0.0    ! hf180
       CASE (181)
          nuc_spin =    0.5    ! hf181
       CASE (182)
          nuc_spin =    0.0    ! hf182
       CASE (183)
          nuc_spin =    1.5    ! hf183
       CASE (184)
          nuc_spin =    0.0    ! hf184
       CASE (185)
          nuc_spin =    5.5    ! hf185
       CASE (186)
          nuc_spin =    0.0    ! hf186
       CASE (187)
          nuc_spin =    3.5    ! hf187
       CASE (188)
          nuc_spin =    0.0    ! hf188
       CASE (189)
          nuc_spin =    4.5    ! hf189
       CASE (190)
          nuc_spin =    0.0    ! hf190
       CASE (191)
          nuc_spin =    0.5    ! hf191
       CASE (192)
          nuc_spin =    0.0    ! hf192
       CASE (193)
          nuc_spin =    0.5    ! hf193
       CASE (194)
          nuc_spin =    0.0    ! hf194
       CASE (195)
          nuc_spin =    1.5    ! hf195
       CASE (196)
          nuc_spin =    0.0    ! hf196
       CASE (197)
          nuc_spin =    0.5    ! hf197
       CASE (198)
          nuc_spin =    0.0    ! hf198
       CASE (199)
          nuc_spin =    4.5    ! hf199
       CASE (200)
          nuc_spin =    0.0    ! hf200
       CASE (201)
          nuc_spin =    1.5    ! hf201
       CASE (202)
          nuc_spin =    0.0    ! hf202
       CASE (203)
          nuc_spin =    0.5    ! hf203
       CASE (204)
          nuc_spin =    0.0    ! hf204
       CASE (205)
          nuc_spin =    2.5    ! hf205
       CASE (206)
          nuc_spin =    0.0    ! hf206
       CASE (207)
          nuc_spin =    1.5    ! hf207
       CASE (208)
          nuc_spin =    0.0    ! hf208
       CASE (209)
          nuc_spin =    1.5    ! hf209
       CASE (210)
          nuc_spin =    0.0    ! hf210
       CASE (211)
          nuc_spin =    2.5    ! hf211
       CASE (212)
          nuc_spin =    0.0    ! hf212
       CASE (213)
          nuc_spin =    2.5    ! hf213
       CASE (214)
          nuc_spin =    0.0    ! hf214
       CASE (215)
          nuc_spin =    0.5    ! hf215
       CASE (216)
          nuc_spin =    0.0    ! hf216
       CASE (217)
          nuc_spin =    2.5    ! hf217
       CASE (218)
          nuc_spin =    0.0    ! hf218
       CASE (219)
          nuc_spin =    3.5    ! hf219
       CASE (220)
          nuc_spin =    0.0    ! hf220
       CASE (221)
          nuc_spin =    3.5    ! hf221
       CASE (222)
          nuc_spin =    0.0    ! hf222
       CASE (223)
          nuc_spin =    4.5    ! hf223
       CASE (224)
          nuc_spin =    0.0    ! hf224
       CASE (225)
          nuc_spin =    0.5    ! hf225
       CASE (226)
          nuc_spin =    0.0    ! hf226
       CASE (227)
          nuc_spin =    3.5    ! hf227
       CASE (228)
          nuc_spin =    0.0    ! hf228
       CASE (229)
          nuc_spin =    1.5    ! hf229
       CASE (230)
          nuc_spin =    0.0    ! hf230
       CASE (231)
          nuc_spin =    5.5    ! hf231
       CASE (232)
          nuc_spin =    0.0    ! hf232
       CASE (233)
          nuc_spin =    4.5    ! hf233
       CASE (234)
          nuc_spin =    0.0    ! hf234
       CASE (235)
          nuc_spin =    6.5    ! hf235
       CASE (236)
          nuc_spin =    0.0    ! hf236
       CASE (237)
          nuc_spin =    1.5    ! hf237
       CASE (238)
          nuc_spin =    0.0    ! hf238
       CASE (239)
          nuc_spin =    2.5    ! hf239
       CASE (240)
          nuc_spin =    0.0    ! hf240
       END SELECT
    CASE (73)
       SELECT CASE (a)
       CASE (151)
          nuc_spin =    1.5    ! ta151
       CASE (152)
          nuc_spin =    1.0    ! ta152
       CASE (153)
          nuc_spin =    1.5    ! ta153
       CASE (154)
          nuc_spin =    2.0    ! ta154
       CASE (155)
          nuc_spin =    4.5    ! ta155
       CASE (156)
          nuc_spin =    4.0    ! ta156
       CASE (157)
          nuc_spin =    4.5    ! ta157
       CASE (158)
          nuc_spin =    3.0    ! ta158
       CASE (159)
          nuc_spin =    0.5    ! ta159
       CASE (160)
          nuc_spin =    2.0    ! ta160
       CASE (161)
          nuc_spin =    0.5    ! ta161
       CASE (162)
          nuc_spin =    3.0    ! ta162
       CASE (163)
          nuc_spin =    2.5    ! ta163
       CASE (164)
          nuc_spin =    3.0    ! ta164
       CASE (165)
          nuc_spin =    2.5    ! ta165
       CASE (166)
          nuc_spin =    2.0    ! ta166
       CASE (167)
          nuc_spin =    4.5    ! ta167
       CASE (168)
          nuc_spin =    2.0    ! ta168
       CASE (169)
          nuc_spin =    2.5    ! ta169
       CASE (170)
          nuc_spin =    3.0    ! ta170
       CASE (171)
          nuc_spin =    2.5    ! ta171
       CASE (172)
          nuc_spin =    3.0    ! ta172
       CASE (173)
          nuc_spin =    2.5    ! ta173
       CASE (174)
          nuc_spin =    0.0    ! ta174
       CASE (175)
          nuc_spin =    3.5    ! ta175
       CASE (176)
          nuc_spin =    1.0    ! ta176
       CASE (177)
          nuc_spin =    3.5    ! ta177
       CASE (178)
          nuc_spin =    0.0    ! ta178
       CASE (179)
          nuc_spin =    3.5    ! ta179
       CASE (180)
          nuc_spin =    1.0    ! ta180
       CASE (181)
          nuc_spin =    3.5    ! ta181
       CASE (182)
          nuc_spin =    3.0    ! ta182
       CASE (183)
          nuc_spin =    3.5    ! ta183
       CASE (184)
          nuc_spin =    5.0    ! ta184
       CASE (185)
          nuc_spin =    3.5    ! ta185
       CASE (186)
          nuc_spin =    4.5    ! ta186
       CASE (187)
          nuc_spin =    3.5    ! ta187
       CASE (188)
          nuc_spin =    0.0    ! ta188
       CASE (189)
          nuc_spin =    3.5    ! ta189
       CASE (190)
          nuc_spin =    5.0    ! ta190
       CASE (191)
          nuc_spin =    4.5    ! ta191
       CASE (192)
          nuc_spin =    2.0    ! ta192
       CASE (193)
          nuc_spin =    1.5    ! ta193
       CASE (194)
          nuc_spin =    1.0    ! ta194
       CASE (195)
          nuc_spin =    1.5    ! ta195
       CASE (196)
          nuc_spin =    2.0    ! ta196
       CASE (197)
          nuc_spin =    1.5    ! ta197
       CASE (198)
          nuc_spin =    1.0    ! ta198
       CASE (199)
          nuc_spin =    4.5    ! ta199
       CASE (200)
          nuc_spin =    5.0    ! ta200
       CASE (201)
          nuc_spin =    4.5    ! ta201
       CASE (202)
          nuc_spin =    5.0    ! ta202
       CASE (203)
          nuc_spin =    4.5    ! ta203
       CASE (204)
          nuc_spin =    4.0    ! ta204
       CASE (205)
          nuc_spin =    4.5    ! ta205
       CASE (206)
          nuc_spin =    5.0    ! ta206
       CASE (207)
          nuc_spin =    0.5    ! ta207
       CASE (208)
          nuc_spin =    1.0    ! ta208
       CASE (209)
          nuc_spin =    0.5    ! ta209
       CASE (210)
          nuc_spin =    1.0    ! ta210
       CASE (211)
          nuc_spin =    0.5    ! ta211
       CASE (212)
          nuc_spin =    3.0    ! ta212
       CASE (213)
          nuc_spin =    2.5    ! ta213
       CASE (214)
          nuc_spin =    0.0    ! ta214
       CASE (215)
          nuc_spin =    2.5    ! ta215
       CASE (216)
          nuc_spin =    3.0    ! ta216
       CASE (217)
          nuc_spin =    2.5    ! ta217
       CASE (218)
          nuc_spin =    3.0    ! ta218
       CASE (219)
          nuc_spin =    2.5    ! ta219
       CASE (220)
          nuc_spin =    1.0    ! ta220
       CASE (221)
          nuc_spin =    4.5    ! ta221
       CASE (222)
          nuc_spin =    5.0    ! ta222
       CASE (223)
          nuc_spin =    4.5    ! ta223
       CASE (224)
          nuc_spin =    5.0    ! ta224
       CASE (225)
          nuc_spin =    4.5    ! ta225
       CASE (226)
          nuc_spin =    5.0    ! ta226
       CASE (227)
          nuc_spin =    4.5    ! ta227
       CASE (228)
          nuc_spin =    4.0    ! ta228
       CASE (229)
          nuc_spin =    4.5    ! ta229
       CASE (230)
          nuc_spin =    5.0    ! ta230
       CASE (231)
          nuc_spin =    4.5    ! ta231
       CASE (232)
          nuc_spin =    1.0    ! ta232
       CASE (233)
          nuc_spin =    4.5    ! ta233
       CASE (234)
          nuc_spin =    0.0    ! ta234
       CASE (235)
          nuc_spin =    3.5    ! ta235
       CASE (236)
          nuc_spin =    7.0    ! ta236
       CASE (237)
          nuc_spin =    3.5    ! ta237
       CASE (238)
          nuc_spin =    4.0    ! ta238
       CASE (239)
          nuc_spin =    3.5    ! ta239
       CASE (240)
          nuc_spin =    2.0    ! ta240
       CASE (241)
          nuc_spin =    4.5    ! ta241
       CASE (242)
          nuc_spin =    2.0    ! ta242
       CASE (243)
          nuc_spin =    4.5    ! ta243
       END SELECT
    CASE (74)
       SELECT CASE (a)
       CASE (154)
          nuc_spin =    0.0    ! w154
       CASE (155)
          nuc_spin =    0.5    ! w155
       CASE (156)
          nuc_spin =    0.0    ! w156
       CASE (157)
          nuc_spin =    3.5    ! w157
       CASE (158)
          nuc_spin =    0.0    ! w158
       CASE (159)
          nuc_spin =    1.5    ! w159
       CASE (160)
          nuc_spin =    0.0    ! w160
       CASE (161)
          nuc_spin =    1.5    ! w161
       CASE (162)
          nuc_spin =    0.0    ! w162
       CASE (163)
          nuc_spin =    1.5    ! w163
       CASE (164)
          nuc_spin =    0.0    ! w164
       CASE (165)
          nuc_spin =    2.5    ! w165
       CASE (166)
          nuc_spin =    0.0    ! w166
       CASE (167)
          nuc_spin =    0.5    ! w167
       CASE (168)
          nuc_spin =    0.0    ! w168
       CASE (169)
          nuc_spin =    2.5    ! w169
       CASE (170)
          nuc_spin =    0.0    ! w170
       CASE (171)
          nuc_spin =    2.5    ! w171
       CASE (172)
          nuc_spin =    0.0    ! w172
       CASE (173)
          nuc_spin =    2.5    ! w173
       CASE (174)
          nuc_spin =    0.0    ! w174
       CASE (175)
          nuc_spin =    0.5    ! w175
       CASE (176)
          nuc_spin =    0.0    ! w176
       CASE (177)
          nuc_spin =    0.5    ! w177
       CASE (178)
          nuc_spin =    0.0    ! w178
       CASE (179)
          nuc_spin =    3.5    ! w179
       CASE (180)
          nuc_spin =    0.0    ! w180
       CASE (181)
          nuc_spin =    4.5    ! w181
       CASE (182)
          nuc_spin =    0.0    ! w182
       CASE (183)
          nuc_spin =    0.5    ! w183
       CASE (184)
          nuc_spin =    0.0    ! w184
       CASE (185)
          nuc_spin =    1.5    ! w185
       CASE (186)
          nuc_spin =    0.0    ! w186
       CASE (187)
          nuc_spin =    1.5    ! w187
       CASE (188)
          nuc_spin =    0.0    ! w188
       CASE (189)
          nuc_spin =    1.5    ! w189
       CASE (190)
          nuc_spin =    0.0    ! w190
       CASE (191)
          nuc_spin =    4.5    ! w191
       CASE (192)
          nuc_spin =    0.0    ! w192
       CASE (193)
          nuc_spin =    0.5    ! w193
       CASE (194)
          nuc_spin =    0.0    ! w194
       CASE (195)
          nuc_spin =    0.5    ! w195
       CASE (196)
          nuc_spin =    0.0    ! w196
       CASE (197)
          nuc_spin =    1.5    ! w197
       CASE (198)
          nuc_spin =    0.0    ! w198
       CASE (199)
          nuc_spin =    0.5    ! w199
       CASE (200)
          nuc_spin =    0.0    ! w200
       CASE (201)
          nuc_spin =    4.5    ! w201
       CASE (202)
          nuc_spin =    0.0    ! w202
       CASE (203)
          nuc_spin =    1.5    ! w203
       CASE (204)
          nuc_spin =    0.0    ! w204
       CASE (205)
          nuc_spin =    0.5    ! w205
       CASE (206)
          nuc_spin =    0.0    ! w206
       CASE (207)
          nuc_spin =    2.5    ! w207
       CASE (208)
          nuc_spin =    0.0    ! w208
       CASE (209)
          nuc_spin =    1.5    ! w209
       CASE (210)
          nuc_spin =    0.0    ! w210
       CASE (211)
          nuc_spin =    1.5    ! w211
       CASE (212)
          nuc_spin =    0.0    ! w212
       CASE (213)
          nuc_spin =    2.5    ! w213
       CASE (214)
          nuc_spin =    0.0    ! w214
       CASE (215)
          nuc_spin =    2.5    ! w215
       CASE (216)
          nuc_spin =    0.0    ! w216
       CASE (217)
          nuc_spin =    0.5    ! w217
       CASE (218)
          nuc_spin =    0.0    ! w218
       CASE (219)
          nuc_spin =    2.5    ! w219
       CASE (220)
          nuc_spin =    0.0    ! w220
       CASE (221)
          nuc_spin =    3.5    ! w221
       CASE (222)
          nuc_spin =    0.0    ! w222
       CASE (223)
          nuc_spin =    3.5    ! w223
       CASE (224)
          nuc_spin =    0.0    ! w224
       CASE (225)
          nuc_spin =    4.5    ! w225
       CASE (226)
          nuc_spin =    0.0    ! w226
       CASE (227)
          nuc_spin =    3.5    ! w227
       CASE (228)
          nuc_spin =    0.0    ! w228
       CASE (229)
          nuc_spin =    0.5    ! w229
       CASE (230)
          nuc_spin =    0.0    ! w230
       CASE (231)
          nuc_spin =    1.5    ! w231
       CASE (232)
          nuc_spin =    0.0    ! w232
       CASE (233)
          nuc_spin =    4.5    ! w233
       CASE (234)
          nuc_spin =    0.0    ! w234
       CASE (235)
          nuc_spin =    5.5    ! w235
       CASE (236)
          nuc_spin =    0.0    ! w236
       CASE (237)
          nuc_spin =    6.5    ! w237
       CASE (238)
          nuc_spin =    0.0    ! w238
       CASE (239)
          nuc_spin =    1.5    ! w239
       CASE (240)
          nuc_spin =    0.0    ! w240
       CASE (241)
          nuc_spin =    2.5    ! w241
       CASE (242)
          nuc_spin =    0.0    ! w242
       CASE (243)
          nuc_spin =    4.5    ! w243
       CASE (244)
          nuc_spin =    0.0    ! w244
       CASE (245)
          nuc_spin =    1.5    ! w245
       CASE (246)
          nuc_spin =    0.0    ! w246
       CASE (247)
          nuc_spin =    0.5    ! w247
       END SELECT
    CASE (75)
       SELECT CASE (a)
       CASE (156)
          nuc_spin =    0.0    ! re156
       CASE (157)
          nuc_spin =    5.5    ! re157
       CASE (158)
          nuc_spin =    3.0    ! re158
       CASE (159)
          nuc_spin =    5.5    ! re159
       CASE (160)
          nuc_spin =    2.0    ! re160
       CASE (161)
          nuc_spin =    0.5    ! re161
       CASE (162)
          nuc_spin =    3.0    ! re162
       CASE (163)
          nuc_spin =    4.5    ! re163
       CASE (164)
          nuc_spin =    3.0    ! re164
       CASE (165)
          nuc_spin =    4.5    ! re165
       CASE (166)
          nuc_spin =    5.0    ! re166
       CASE (167)
          nuc_spin =    4.5    ! re167
       CASE (168)
          nuc_spin =    5.0    ! re168
       CASE (169)
          nuc_spin =    2.5    ! re169
       CASE (170)
          nuc_spin =    5.0    ! re170
       CASE (171)
          nuc_spin =    4.5    ! re171
       CASE (172)
          nuc_spin =    3.0    ! re172
       CASE (173)
          nuc_spin =    2.5    ! re173
       CASE (174)
          nuc_spin =    2.0    ! re174
       CASE (175)
          nuc_spin =    2.5    ! re175
       CASE (176)
          nuc_spin =    0.0    ! re176
       CASE (177)
          nuc_spin =    2.5    ! re177
       CASE (178)
          nuc_spin =    3.0    ! re178
       CASE (179)
          nuc_spin =    2.5    ! re179
       CASE (180)
          nuc_spin =    1.0    ! re180
       CASE (181)
          nuc_spin =    2.5    ! re181
       CASE (182)
          nuc_spin =    7.0    ! re182
       CASE (183)
          nuc_spin =    2.5    ! re183
       CASE (184)
          nuc_spin =    0.0    ! re184
       CASE (185)
          nuc_spin =    2.5    ! re185
       CASE (186)
          nuc_spin =    1.0    ! re186
       CASE (187)
          nuc_spin =    2.5    ! re187
       CASE (188)
          nuc_spin =    1.0    ! re188
       CASE (189)
          nuc_spin =    2.5    ! re189
       CASE (190)
          nuc_spin =    2.0    ! re190
       CASE (191)
          nuc_spin =    1.5    ! re191
       CASE (192)
          nuc_spin =    2.0    ! re192
       CASE (193)
          nuc_spin =    2.5    ! re193
       CASE (194)
          nuc_spin =    7.0    ! re194
       CASE (195)
          nuc_spin =    0.5    ! re195
       CASE (196)
          nuc_spin =    1.0    ! re196
       CASE (197)
          nuc_spin =    0.5    ! re197
       CASE (198)
          nuc_spin =    1.0    ! re198
       CASE (199)
          nuc_spin =    0.5    ! re199
       CASE (200)
          nuc_spin =    1.0    ! re200
       CASE (201)
          nuc_spin =    5.5    ! re201
       CASE (202)
          nuc_spin =    6.0    ! re202
       CASE (203)
          nuc_spin =    5.5    ! re203
       CASE (204)
          nuc_spin =    4.0    ! re204
       CASE (205)
          nuc_spin =    5.5    ! re205
       CASE (206)
          nuc_spin =    6.0    ! re206
       CASE (207)
          nuc_spin =    5.5    ! re207
       CASE (208)
          nuc_spin =    1.0    ! re208
       CASE (209)
          nuc_spin =    0.5    ! re209
       CASE (210)
          nuc_spin =    3.0    ! re210
       CASE (211)
          nuc_spin =    4.5    ! re211
       CASE (212)
          nuc_spin =    3.0    ! re212
       CASE (213)
          nuc_spin =    4.5    ! re213
       CASE (214)
          nuc_spin =    2.0    ! re214
       CASE (215)
          nuc_spin =    4.5    ! re215
       CASE (216)
          nuc_spin =    5.0    ! re216
       CASE (217)
          nuc_spin =    4.5    ! re217
       CASE (218)
          nuc_spin =    4.0    ! re218
       CASE (219)
          nuc_spin =    4.5    ! re219
       CASE (220)
          nuc_spin =    4.0    ! re220
       CASE (221)
          nuc_spin =    2.5    ! re221
       CASE (222)
          nuc_spin =    4.0    ! re222
       CASE (223)
          nuc_spin =    2.5    ! re223
       CASE (224)
          nuc_spin =    1.0    ! re224
       CASE (225)
          nuc_spin =    2.5    ! re225
       CASE (226)
          nuc_spin =    2.0    ! re226
       CASE (227)
          nuc_spin =    2.5    ! re227
       CASE (228)
          nuc_spin =    1.0    ! re228
       CASE (229)
          nuc_spin =    2.5    ! re229
       CASE (230)
          nuc_spin =    3.0    ! re230
       CASE (231)
          nuc_spin =    2.5    ! re231
       CASE (232)
          nuc_spin =    1.0    ! re232
       CASE (233)
          nuc_spin =    2.5    ! re233
       CASE (234)
          nuc_spin =    5.0    ! re234
       CASE (235)
          nuc_spin =    2.5    ! re235
       CASE (236)
          nuc_spin =    6.0    ! re236
       CASE (237)
          nuc_spin =    2.5    ! re237
       CASE (238)
          nuc_spin =    4.0    ! re238
       CASE (239)
          nuc_spin =    2.5    ! re239
       CASE (240)
          nuc_spin =    1.0    ! re240
       CASE (241)
          nuc_spin =    2.5    ! re241
       CASE (242)
          nuc_spin =    5.0    ! re242
       CASE (243)
          nuc_spin =    2.5    ! re243
       CASE (244)
          nuc_spin =    5.0    ! re244
       CASE (245)
          nuc_spin =    2.5    ! re245
       CASE (246)
          nuc_spin =    2.0    ! re246
       CASE (247)
          nuc_spin =    0.5    ! re247
       CASE (248)
          nuc_spin =    3.0    ! re248
       CASE (249)
          nuc_spin =    2.5    ! re249
       CASE (250)
          nuc_spin =    8.0    ! re250
       END SELECT
    CASE (76)
       SELECT CASE (a)
       CASE (159)
          nuc_spin =    3.5    ! os159
       CASE (160)
          nuc_spin =    0.0    ! os160
       CASE (161)
          nuc_spin =    1.5    ! os161
       CASE (162)
          nuc_spin =    0.0    ! os162
       CASE (163)
          nuc_spin =    1.5    ! os163
       CASE (164)
          nuc_spin =    0.0    ! os164
       CASE (165)
          nuc_spin =    1.5    ! os165
       CASE (166)
          nuc_spin =    0.0    ! os166
       CASE (167)
          nuc_spin =    2.5    ! os167
       CASE (168)
          nuc_spin =    0.0    ! os168
       CASE (169)
          nuc_spin =    0.5    ! os169
       CASE (170)
          nuc_spin =    0.0    ! os170
       CASE (171)
          nuc_spin =    2.5    ! os171
       CASE (172)
          nuc_spin =    0.0    ! os172
       CASE (173)
          nuc_spin =    2.5    ! os173
       CASE (174)
          nuc_spin =    0.0    ! os174
       CASE (175)
          nuc_spin =    2.5    ! os175
       CASE (176)
          nuc_spin =    0.0    ! os176
       CASE (177)
          nuc_spin =    0.5    ! os177
       CASE (178)
          nuc_spin =    0.0    ! os178
       CASE (179)
          nuc_spin =    0.5    ! os179
       CASE (180)
          nuc_spin =    0.0    ! os180
       CASE (181)
          nuc_spin =    0.5    ! os181
       CASE (182)
          nuc_spin =    0.0    ! os182
       CASE (183)
          nuc_spin =    4.5    ! os183
       CASE (184)
          nuc_spin =    0.0    ! os184
       CASE (185)
          nuc_spin =    0.5    ! os185
       CASE (186)
          nuc_spin =    0.0    ! os186
       CASE (187)
          nuc_spin =    0.5    ! os187
       CASE (188)
          nuc_spin =    0.0    ! os188
       CASE (189)
          nuc_spin =    1.5    ! os189
       CASE (190)
          nuc_spin =    0.0    ! os190
       CASE (191)
          nuc_spin =    4.5    ! os191
       CASE (192)
          nuc_spin =    0.0    ! os192
       CASE (193)
          nuc_spin =    1.5    ! os193
       CASE (194)
          nuc_spin =    0.0    ! os194
       CASE (195)
          nuc_spin =    6.5    ! os195
       CASE (196)
          nuc_spin =    0.0    ! os196
       CASE (197)
          nuc_spin =    0.5    ! os197
       CASE (198)
          nuc_spin =    0.0    ! os198
       CASE (199)
          nuc_spin =    1.5    ! os199
       CASE (200)
          nuc_spin =    0.0    ! os200
       CASE (201)
          nuc_spin =    0.5    ! os201
       CASE (202)
          nuc_spin =    0.0    ! os202
       CASE (203)
          nuc_spin =    4.5    ! os203
       CASE (204)
          nuc_spin =    0.0    ! os204
       CASE (205)
          nuc_spin =    1.5    ! os205
       CASE (206)
          nuc_spin =    0.0    ! os206
       CASE (207)
          nuc_spin =    2.5    ! os207
       CASE (208)
          nuc_spin =    0.0    ! os208
       CASE (209)
          nuc_spin =    2.5    ! os209
       CASE (210)
          nuc_spin =    0.0    ! os210
       CASE (211)
          nuc_spin =    1.5    ! os211
       CASE (212)
          nuc_spin =    0.0    ! os212
       CASE (213)
          nuc_spin =    1.5    ! os213
       CASE (214)
          nuc_spin =    0.0    ! os214
       CASE (215)
          nuc_spin =    2.5    ! os215
       CASE (216)
          nuc_spin =    0.0    ! os216
       CASE (217)
          nuc_spin =    2.5    ! os217
       CASE (218)
          nuc_spin =    0.0    ! os218
       CASE (219)
          nuc_spin =    2.5    ! os219
       CASE (220)
          nuc_spin =    0.0    ! os220
       CASE (221)
          nuc_spin =    0.5    ! os221
       CASE (222)
          nuc_spin =    0.0    ! os222
       CASE (223)
          nuc_spin =    3.5    ! os223
       CASE (224)
          nuc_spin =    0.0    ! os224
       CASE (225)
          nuc_spin =    3.5    ! os225
       CASE (226)
          nuc_spin =    0.0    ! os226
       CASE (227)
          nuc_spin =    4.5    ! os227
       CASE (228)
          nuc_spin =    0.0    ! os228
       CASE (229)
          nuc_spin =    3.5    ! os229
       CASE (230)
          nuc_spin =    0.0    ! os230
       CASE (231)
          nuc_spin =    0.5    ! os231
       CASE (232)
          nuc_spin =    0.0    ! os232
       CASE (233)
          nuc_spin =    4.5    ! os233
       CASE (234)
          nuc_spin =    0.0    ! os234
       CASE (235)
          nuc_spin =    1.5    ! os235
       CASE (236)
          nuc_spin =    0.0    ! os236
       CASE (237)
          nuc_spin =    5.5    ! os237
       CASE (238)
          nuc_spin =    0.0    ! os238
       CASE (239)
          nuc_spin =    6.5    ! os239
       CASE (240)
          nuc_spin =    0.0    ! os240
       CASE (241)
          nuc_spin =    1.5    ! os241
       CASE (242)
          nuc_spin =    0.0    ! os242
       CASE (243)
          nuc_spin =    4.5    ! os243
       CASE (244)
          nuc_spin =    0.0    ! os244
       CASE (245)
          nuc_spin =    1.5    ! os245
       CASE (246)
          nuc_spin =    0.0    ! os246
       CASE (247)
          nuc_spin =    1.5    ! os247
       CASE (248)
          nuc_spin =    0.0    ! os248
       CASE (249)
          nuc_spin =    0.5    ! os249
       CASE (250)
          nuc_spin =    0.0    ! os250
       CASE (251)
          nuc_spin =    7.5    ! os251
       CASE (252)
          nuc_spin =    0.0    ! os252
       CASE (253)
          nuc_spin =    0.5    ! os253
       END SELECT
    CASE (77)
       SELECT CASE (a)
       CASE (162)
          nuc_spin =    3.0    ! ir162
       CASE (163)
          nuc_spin =    1.5    ! ir163
       CASE (164)
          nuc_spin =    5.0    ! ir164
       CASE (165)
          nuc_spin =    5.5    ! ir165
       CASE (166)
          nuc_spin =    6.0    ! ir166
       CASE (167)
          nuc_spin =    5.5    ! ir167
       CASE (168)
          nuc_spin =    3.0    ! ir168
       CASE (169)
          nuc_spin =    5.5    ! ir169
       CASE (170)
          nuc_spin =    6.0    ! ir170
       CASE (171)
          nuc_spin =    5.5    ! ir171
       CASE (172)
          nuc_spin =    3.0    ! ir172
       CASE (173)
          nuc_spin =    1.5    ! ir173
       CASE (174)
          nuc_spin =    3.0    ! ir174
       CASE (175)
          nuc_spin =    2.5    ! ir175
       CASE (176)
          nuc_spin =    1.0    ! ir176
       CASE (177)
          nuc_spin =    2.5    ! ir177
       CASE (178)
          nuc_spin =    4.0    ! ir178
       CASE (179)
          nuc_spin =    2.5    ! ir179
       CASE (180)
          nuc_spin =    3.0    ! ir180
       CASE (181)
          nuc_spin =    2.5    ! ir181
       CASE (182)
          nuc_spin =    5.0    ! ir182
       CASE (183)
          nuc_spin =    2.5    ! ir183
       CASE (184)
          nuc_spin =    5.0    ! ir184
       CASE (185)
          nuc_spin =    2.5    ! ir185
       CASE (186)
          nuc_spin =    5.0    ! ir186
       CASE (187)
          nuc_spin =    1.5    ! ir187
       CASE (188)
          nuc_spin =    1.0    ! ir188
       CASE (189)
          nuc_spin =    1.5    ! ir189
       CASE (190)
          nuc_spin =    4.0    ! ir190
       CASE (191)
          nuc_spin =    1.5    ! ir191
       CASE (192)
          nuc_spin =    0.0    ! ir192
       CASE (193)
          nuc_spin =    1.5    ! ir193
       CASE (194)
          nuc_spin =    1.0    ! ir194
       CASE (195)
          nuc_spin =    1.5    ! ir195
       CASE (196)
          nuc_spin =    0.0    ! ir196
       CASE (197)
          nuc_spin =    1.5    ! ir197
       CASE (198)
          nuc_spin =    2.0    ! ir198
       CASE (199)
          nuc_spin =    1.5    ! ir199
       CASE (200)
          nuc_spin =    0.0    ! ir200
       CASE (201)
          nuc_spin =    1.5    ! ir201
       CASE (202)
          nuc_spin =    2.0    ! ir202
       CASE (203)
          nuc_spin =    0.5    ! ir203
       CASE (204)
          nuc_spin =    3.0    ! ir204
       CASE (205)
          nuc_spin =    1.5    ! ir205
       CASE (206)
          nuc_spin =    1.0    ! ir206
       CASE (207)
          nuc_spin =    1.5    ! ir207
       CASE (208)
          nuc_spin =    3.0    ! ir208
       CASE (209)
          nuc_spin =    0.5    ! ir209
       CASE (210)
          nuc_spin =    6.0    ! ir210
       CASE (211)
          nuc_spin =    5.5    ! ir211
       CASE (212)
          nuc_spin =    6.0    ! ir212
       CASE (213)
          nuc_spin =    5.5    ! ir213
       CASE (214)
          nuc_spin =    5.0    ! ir214
       CASE (215)
          nuc_spin =    5.5    ! ir215
       CASE (216)
          nuc_spin =    6.0    ! ir216
       CASE (217)
          nuc_spin =    5.5    ! ir217
       CASE (218)
          nuc_spin =    3.0    ! ir218
       CASE (219)
          nuc_spin =    0.5    ! ir219
       CASE (220)
          nuc_spin =    2.0    ! ir220
       CASE (221)
          nuc_spin =    0.5    ! ir221
       CASE (222)
          nuc_spin =    0.0    ! ir222
       CASE (223)
          nuc_spin =    0.5    ! ir223
       CASE (224)
          nuc_spin =    3.0    ! ir224
       CASE (225)
          nuc_spin =    0.5    ! ir225
       CASE (226)
          nuc_spin =    4.0    ! ir226
       CASE (227)
          nuc_spin =    0.5    ! ir227
       CASE (228)
          nuc_spin =    5.0    ! ir228
       CASE (229)
          nuc_spin =    0.5    ! ir229
       CASE (230)
          nuc_spin =    4.0    ! ir230
       CASE (231)
          nuc_spin =    0.5    ! ir231
       CASE (232)
          nuc_spin =    6.0    ! ir232
       CASE (233)
          nuc_spin =    5.5    ! ir233
       CASE (234)
          nuc_spin =    6.0    ! ir234
       CASE (235)
          nuc_spin =    5.5    ! ir235
       CASE (236)
          nuc_spin =    6.0    ! ir236
       CASE (237)
          nuc_spin =    5.5    ! ir237
       CASE (238)
          nuc_spin =    4.0    ! ir238
       CASE (239)
          nuc_spin =    5.5    ! ir239
       CASE (240)
          nuc_spin =    1.0    ! ir240
       CASE (241)
          nuc_spin =    5.5    ! ir241
       CASE (242)
          nuc_spin =    4.0    ! ir242
       CASE (243)
          nuc_spin =    5.5    ! ir243
       CASE (244)
          nuc_spin =    6.0    ! ir244
       CASE (245)
          nuc_spin =    5.5    ! ir245
       CASE (246)
          nuc_spin =    4.0    ! ir246
       CASE (247)
          nuc_spin =    5.5    ! ir247
       CASE (248)
          nuc_spin =    2.0    ! ir248
       CASE (249)
          nuc_spin =    1.5    ! ir249
       CASE (250)
          nuc_spin =    8.0    ! ir250
       CASE (251)
          nuc_spin =    5.5    ! ir251
       CASE (252)
          nuc_spin =    6.0    ! ir252
       CASE (253)
          nuc_spin =    5.5    ! ir253
       CASE (254)
          nuc_spin =    1.0    ! ir254
       CASE (255)
          nuc_spin =    1.5    ! ir255
       CASE (256)
          nuc_spin =    1.0    ! ir256
       END SELECT
    CASE (78)
       SELECT CASE (a)
       CASE (165)
          nuc_spin =    2.5    ! pt165
       CASE (166)
          nuc_spin =    0.0    ! pt166
       CASE (167)
          nuc_spin =    3.5    ! pt167
       CASE (168)
          nuc_spin =    0.0    ! pt168
       CASE (169)
          nuc_spin =    2.5    ! pt169
       CASE (170)
          nuc_spin =    0.0    ! pt170
       CASE (171)
          nuc_spin =    0.5    ! pt171
       CASE (172)
          nuc_spin =    0.0    ! pt172
       CASE (173)
          nuc_spin =    1.5    ! pt173
       CASE (174)
          nuc_spin =    0.0    ! pt174
       CASE (175)
          nuc_spin =    2.5    ! pt175
       CASE (176)
          nuc_spin =    0.0    ! pt176
       CASE (177)
          nuc_spin =    2.5    ! pt177
       CASE (178)
          nuc_spin =    0.0    ! pt178
       CASE (179)
          nuc_spin =    0.5    ! pt179
       CASE (180)
          nuc_spin =    0.0    ! pt180
       CASE (181)
          nuc_spin =    0.5    ! pt181
       CASE (182)
          nuc_spin =    0.0    ! pt182
       CASE (183)
          nuc_spin =    0.5    ! pt183
       CASE (184)
          nuc_spin =    0.0    ! pt184
       CASE (185)
          nuc_spin =    4.5    ! pt185
       CASE (186)
          nuc_spin =    0.0    ! pt186
       CASE (187)
          nuc_spin =    1.5    ! pt187
       CASE (188)
          nuc_spin =    0.0    ! pt188
       CASE (189)
          nuc_spin =    1.5    ! pt189
       CASE (190)
          nuc_spin =    0.0    ! pt190
       CASE (191)
          nuc_spin =    1.5    ! pt191
       CASE (192)
          nuc_spin =    0.0    ! pt192
       CASE (193)
          nuc_spin =    0.5    ! pt193
       CASE (194)
          nuc_spin =    0.0    ! pt194
       CASE (195)
          nuc_spin =    0.5    ! pt195
       CASE (196)
          nuc_spin =    0.0    ! pt196
       CASE (197)
          nuc_spin =    0.5    ! pt197
       CASE (198)
          nuc_spin =    0.0    ! pt198
       CASE (199)
          nuc_spin =    2.5    ! pt199
       CASE (200)
          nuc_spin =    0.0    ! pt200
       CASE (201)
          nuc_spin =    2.5    ! pt201
       CASE (202)
          nuc_spin =    0.0    ! pt202
       CASE (203)
          nuc_spin =    0.5    ! pt203
       CASE (204)
          nuc_spin =    0.0    ! pt204
       CASE (205)
          nuc_spin =    4.5    ! pt205
       CASE (206)
          nuc_spin =    0.0    ! pt206
       CASE (207)
          nuc_spin =    1.5    ! pt207
       CASE (208)
          nuc_spin =    0.0    ! pt208
       CASE (209)
          nuc_spin =    2.5    ! pt209
       CASE (210)
          nuc_spin =    0.0    ! pt210
       CASE (211)
          nuc_spin =    0.5    ! pt211
       CASE (212)
          nuc_spin =    0.0    ! pt212
       CASE (213)
          nuc_spin =    1.5    ! pt213
       CASE (214)
          nuc_spin =    0.0    ! pt214
       CASE (215)
          nuc_spin =    0.5    ! pt215
       CASE (216)
          nuc_spin =    0.0    ! pt216
       CASE (217)
          nuc_spin =    2.5    ! pt217
       CASE (218)
          nuc_spin =    0.0    ! pt218
       CASE (219)
          nuc_spin =    2.5    ! pt219
       CASE (220)
          nuc_spin =    0.0    ! pt220
       CASE (221)
          nuc_spin =    2.5    ! pt221
       CASE (222)
          nuc_spin =    0.0    ! pt222
       CASE (223)
          nuc_spin =    0.5    ! pt223
       CASE (224)
          nuc_spin =    0.0    ! pt224
       CASE (225)
          nuc_spin =    3.5    ! pt225
       CASE (226)
          nuc_spin =    0.0    ! pt226
       CASE (227)
          nuc_spin =    3.5    ! pt227
       CASE (228)
          nuc_spin =    0.0    ! pt228
       CASE (229)
          nuc_spin =    4.5    ! pt229
       CASE (230)
          nuc_spin =    0.0    ! pt230
       CASE (231)
          nuc_spin =    3.5    ! pt231
       CASE (232)
          nuc_spin =    0.0    ! pt232
       CASE (233)
          nuc_spin =    0.5    ! pt233
       CASE (234)
          nuc_spin =    0.0    ! pt234
       CASE (235)
          nuc_spin =    0.5    ! pt235
       CASE (236)
          nuc_spin =    0.0    ! pt236
       CASE (237)
          nuc_spin =    5.5    ! pt237
       CASE (238)
          nuc_spin =    0.0    ! pt238
       CASE (239)
          nuc_spin =    1.5    ! pt239
       CASE (240)
          nuc_spin =    0.0    ! pt240
       CASE (241)
          nuc_spin =    6.5    ! pt241
       CASE (242)
          nuc_spin =    0.0    ! pt242
       CASE (243)
          nuc_spin =    1.5    ! pt243
       CASE (244)
          nuc_spin =    0.0    ! pt244
       CASE (245)
          nuc_spin =    0.5    ! pt245
       CASE (246)
          nuc_spin =    0.0    ! pt246
       CASE (247)
          nuc_spin =    1.5    ! pt247
       CASE (248)
          nuc_spin =    0.0    ! pt248
       CASE (249)
          nuc_spin =    1.5    ! pt249
       CASE (250)
          nuc_spin =    0.0    ! pt250
       CASE (251)
          nuc_spin =    7.5    ! pt251
       CASE (252)
          nuc_spin =    0.0    ! pt252
       CASE (253)
          nuc_spin =    0.5    ! pt253
       CASE (254)
          nuc_spin =    0.0    ! pt254
       CASE (255)
          nuc_spin =    0.5    ! pt255
       CASE (256)
          nuc_spin =    0.0    ! pt256
       CASE (257)
          nuc_spin =    0.5    ! pt257
       CASE (258)
          nuc_spin =    0.0    ! pt258
       CASE (259)
          nuc_spin =    1.5    ! pt259
       CASE (260)
          nuc_spin =    0.0    ! pt260
       END SELECT
    CASE (79)
       SELECT CASE (a)
       CASE (167)
          nuc_spin =    0.5    ! au167
       CASE (168)
          nuc_spin =    4.0    ! au168
       CASE (169)
          nuc_spin =    0.5    ! au169
       CASE (170)
          nuc_spin =    2.0    ! au170
       CASE (171)
          nuc_spin =    0.5    ! au171
       CASE (172)
          nuc_spin =    2.0    ! au172
       CASE (173)
          nuc_spin =    0.5    ! au173
       CASE (174)
          nuc_spin =    2.0    ! au174
       CASE (175)
          nuc_spin =    0.5    ! au175
       CASE (176)
          nuc_spin =    0.0    ! au176
       CASE (177)
          nuc_spin =    0.5    ! au177
       CASE (178)
          nuc_spin =    5.0    ! au178
       CASE (179)
          nuc_spin =    0.5    ! au179
       CASE (180)
          nuc_spin =    2.0    ! au180
       CASE (181)
          nuc_spin =    2.5    ! au181
       CASE (182)
          nuc_spin =    0.0    ! au182
       CASE (183)
          nuc_spin =    2.5    ! au183
       CASE (184)
          nuc_spin =    3.0    ! au184
       CASE (185)
          nuc_spin =    2.5    ! au185
       CASE (186)
          nuc_spin =    3.0    ! au186
       CASE (187)
          nuc_spin =    0.5    ! au187
       CASE (188)
          nuc_spin =    0.0    ! au188
       CASE (189)
          nuc_spin =    0.5    ! au189
       CASE (190)
          nuc_spin =    1.0    ! au190
       CASE (191)
          nuc_spin =    1.5    ! au191
       CASE (192)
          nuc_spin =    1.0    ! au192
       CASE (193)
          nuc_spin =    1.5    ! au193
       CASE (194)
          nuc_spin =    1.0    ! au194
       CASE (195)
          nuc_spin =    1.5    ! au195
       CASE (196)
          nuc_spin =    2.0    ! au196
       CASE (197)
          nuc_spin =    1.5    ! au197
       CASE (198)
          nuc_spin =    2.0    ! au198
       CASE (199)
          nuc_spin =    1.5    ! au199
       CASE (200)
          nuc_spin =    0.0    ! au200
       CASE (201)
          nuc_spin =    1.5    ! au201
       CASE (202)
          nuc_spin =    1.0    ! au202
       CASE (203)
          nuc_spin =    1.5    ! au203
       CASE (204)
          nuc_spin =    2.0    ! au204
       CASE (205)
          nuc_spin =    1.5    ! au205
       CASE (206)
          nuc_spin =    5.0    ! au206
       CASE (207)
          nuc_spin =    0.5    ! au207
       CASE (208)
          nuc_spin =    3.0    ! au208
       CASE (209)
          nuc_spin =    0.5    ! au209
       CASE (210)
          nuc_spin =    5.0    ! au210
       CASE (211)
          nuc_spin =    0.5    ! au211
       CASE (212)
          nuc_spin =    3.0    ! au212
       CASE (213)
          nuc_spin =    0.5    ! au213
       CASE (214)
          nuc_spin =    2.0    ! au214
       CASE (215)
          nuc_spin =    1.5    ! au215
       CASE (216)
          nuc_spin =    1.0    ! au216
       CASE (217)
          nuc_spin =    1.5    ! au217
       CASE (218)
          nuc_spin =    1.0    ! au218
       CASE (219)
          nuc_spin =    0.5    ! au219
       CASE (220)
          nuc_spin =    3.0    ! au220
       CASE (221)
          nuc_spin =    5.5    ! au221
       CASE (222)
          nuc_spin =    6.0    ! au222
       CASE (223)
          nuc_spin =    5.5    ! au223
       CASE (224)
          nuc_spin =    6.0    ! au224
       CASE (225)
          nuc_spin =    5.5    ! au225
       CASE (226)
          nuc_spin =    6.0    ! au226
       CASE (227)
          nuc_spin =    5.5    ! au227
       CASE (228)
          nuc_spin =    2.0    ! au228
       CASE (229)
          nuc_spin =    5.5    ! au229
       CASE (230)
          nuc_spin =    1.0    ! au230
       CASE (231)
          nuc_spin =    1.5    ! au231
       CASE (232)
          nuc_spin =    2.0    ! au232
       CASE (233)
          nuc_spin =    5.5    ! au233
       CASE (234)
          nuc_spin =    6.0    ! au234
       CASE (235)
          nuc_spin =    0.5    ! au235
       CASE (236)
          nuc_spin =    0.0    ! au236
       CASE (237)
          nuc_spin =    0.5    ! au237
       CASE (238)
          nuc_spin =    1.0    ! au238
       CASE (239)
          nuc_spin =    0.5    ! au239
       CASE (240)
          nuc_spin =    2.0    ! au240
       CASE (241)
          nuc_spin =    0.5    ! au241
       CASE (242)
          nuc_spin =    5.0    ! au242
       CASE (243)
          nuc_spin =    1.5    ! au243
       CASE (244)
          nuc_spin =    6.0    ! au244
       CASE (245)
          nuc_spin =    1.5    ! au245
       CASE (246)
          nuc_spin =    2.0    ! au246
       CASE (247)
          nuc_spin =    1.5    ! au247
       CASE (248)
          nuc_spin =    6.0    ! au248
       CASE (249)
          nuc_spin =    1.5    ! au249
       CASE (250)
          nuc_spin =    1.0    ! au250
       CASE (251)
          nuc_spin =    0.5    ! au251
       CASE (252)
          nuc_spin =    1.0    ! au252
       CASE (253)
          nuc_spin =    1.5    ! au253
       CASE (254)
          nuc_spin =    1.0    ! au254
       CASE (255)
          nuc_spin =    0.5    ! au255
       CASE (256)
          nuc_spin =    1.0    ! au256
       CASE (257)
          nuc_spin =    0.5    ! au257
       CASE (258)
          nuc_spin =    1.0    ! au258
       CASE (259)
          nuc_spin =    0.5    ! au259
       CASE (260)
          nuc_spin =    1.0    ! au260
       CASE (261)
          nuc_spin =    0.5    ! au261
       CASE (262)
          nuc_spin =    1.0    ! au262
       CASE (263)
          nuc_spin =    0.5    ! au263
       END SELECT
    CASE (80)
       SELECT CASE (a)
       CASE (170)
          nuc_spin =    0.0    ! hg170
       CASE (171)
          nuc_spin =    1.5    ! hg171
       CASE (172)
          nuc_spin =    0.0    ! hg172
       CASE (173)
          nuc_spin =    2.5    ! hg173
       CASE (174)
          nuc_spin =    0.0    ! hg174
       CASE (175)
          nuc_spin =    6.5    ! hg175
       CASE (176)
          nuc_spin =    0.0    ! hg176
       CASE (177)
          nuc_spin =    0.5    ! hg177
       CASE (178)
          nuc_spin =    0.0    ! hg178
       CASE (179)
          nuc_spin =    1.5    ! hg179
       CASE (180)
          nuc_spin =    0.0    ! hg180
       CASE (181)
          nuc_spin =    0.5    ! hg181
       CASE (182)
          nuc_spin =    0.0    ! hg182
       CASE (183)
          nuc_spin =    0.5    ! hg183
       CASE (184)
          nuc_spin =    0.0    ! hg184
       CASE (185)
          nuc_spin =    0.5    ! hg185
       CASE (186)
          nuc_spin =    0.0    ! hg186
       CASE (187)
          nuc_spin =    6.5    ! hg187
       CASE (188)
          nuc_spin =    0.0    ! hg188
       CASE (189)
          nuc_spin =    1.5    ! hg189
       CASE (190)
          nuc_spin =    0.0    ! hg190
       CASE (191)
          nuc_spin =    1.5    ! hg191
       CASE (192)
          nuc_spin =    0.0    ! hg192
       CASE (193)
          nuc_spin =    1.5    ! hg193
       CASE (194)
          nuc_spin =    0.0    ! hg194
       CASE (195)
          nuc_spin =    0.5    ! hg195
       CASE (196)
          nuc_spin =    0.0    ! hg196
       CASE (197)
          nuc_spin =    0.5    ! hg197
       CASE (198)
          nuc_spin =    0.0    ! hg198
       CASE (199)
          nuc_spin =    0.5    ! hg199
       CASE (200)
          nuc_spin =    0.0    ! hg200
       CASE (201)
          nuc_spin =    1.5    ! hg201
       CASE (202)
          nuc_spin =    0.0    ! hg202
       CASE (203)
          nuc_spin =    2.5    ! hg203
       CASE (204)
          nuc_spin =    0.0    ! hg204
       CASE (205)
          nuc_spin =    0.5    ! hg205
       CASE (206)
          nuc_spin =    0.0    ! hg206
       CASE (207)
          nuc_spin =    4.5    ! hg207
       CASE (208)
          nuc_spin =    0.0    ! hg208
       CASE (209)
          nuc_spin =    4.5    ! hg209
       CASE (210)
          nuc_spin =    0.0    ! hg210
       CASE (211)
          nuc_spin =    2.5    ! hg211
       CASE (212)
          nuc_spin =    0.0    ! hg212
       CASE (213)
          nuc_spin =    2.5    ! hg213
       CASE (214)
          nuc_spin =    0.0    ! hg214
       CASE (215)
          nuc_spin =    4.5    ! hg215
       CASE (216)
          nuc_spin =    0.0    ! hg216
       CASE (217)
          nuc_spin =    7.5    ! hg217
       CASE (218)
          nuc_spin =    0.0    ! hg218
       CASE (219)
          nuc_spin =    2.5    ! hg219
       CASE (220)
          nuc_spin =    0.0    ! hg220
       CASE (221)
          nuc_spin =    2.5    ! hg221
       CASE (222)
          nuc_spin =    0.0    ! hg222
       CASE (223)
          nuc_spin =    2.5    ! hg223
       CASE (224)
          nuc_spin =    0.0    ! hg224
       CASE (225)
          nuc_spin =    3.5    ! hg225
       CASE (226)
          nuc_spin =    0.0    ! hg226
       CASE (227)
          nuc_spin =    2.5    ! hg227
       CASE (228)
          nuc_spin =    0.0    ! hg228
       CASE (229)
          nuc_spin =    3.5    ! hg229
       CASE (230)
          nuc_spin =    0.0    ! hg230
       CASE (231)
          nuc_spin =    4.5    ! hg231
       CASE (232)
          nuc_spin =    0.0    ! hg232
       CASE (233)
          nuc_spin =    3.5    ! hg233
       CASE (234)
          nuc_spin =    0.0    ! hg234
       CASE (235)
          nuc_spin =    0.5    ! hg235
       CASE (236)
          nuc_spin =    0.0    ! hg236
       CASE (237)
          nuc_spin =    0.5    ! hg237
       CASE (238)
          nuc_spin =    0.0    ! hg238
       CASE (239)
          nuc_spin =    5.5    ! hg239
       CASE (240)
          nuc_spin =    0.0    ! hg240
       CASE (241)
          nuc_spin =    1.5    ! hg241
       CASE (242)
          nuc_spin =    0.0    ! hg242
       CASE (243)
          nuc_spin =    6.5    ! hg243
       CASE (244)
          nuc_spin =    0.0    ! hg244
       CASE (245)
          nuc_spin =    2.5    ! hg245
       CASE (246)
          nuc_spin =    0.0    ! hg246
       CASE (247)
          nuc_spin =    3.5    ! hg247
       CASE (248)
          nuc_spin =    0.0    ! hg248
       CASE (249)
          nuc_spin =    0.5    ! hg249
       CASE (250)
          nuc_spin =    0.0    ! hg250
       CASE (251)
          nuc_spin =    1.5    ! hg251
       CASE (252)
          nuc_spin =    0.0    ! hg252
       CASE (253)
          nuc_spin =    1.5    ! hg253
       CASE (254)
          nuc_spin =    0.0    ! hg254
       CASE (255)
          nuc_spin =    2.5    ! hg255
       CASE (256)
          nuc_spin =    0.0    ! hg256
       CASE (257)
          nuc_spin =    0.5    ! hg257
       CASE (258)
          nuc_spin =    0.0    ! hg258
       CASE (259)
          nuc_spin =    0.5    ! hg259
       CASE (260)
          nuc_spin =    0.0    ! hg260
       CASE (261)
          nuc_spin =    1.5    ! hg261
       CASE (262)
          nuc_spin =    0.0    ! hg262
       CASE (263)
          nuc_spin =    0.5    ! hg263
       CASE (264)
          nuc_spin =    0.0    ! hg264
       CASE (265)
          nuc_spin =    3.5    ! hg265
       CASE (266)
          nuc_spin =    0.0    ! hg266
       END SELECT
    CASE (81)
       SELECT CASE (a)
       CASE (173)
          nuc_spin =    0.5    ! tl173
       CASE (174)
          nuc_spin =    2.0    ! tl174
       CASE (175)
          nuc_spin =    0.5    ! tl175
       CASE (176)
          nuc_spin =    0.0    ! tl176
       CASE (177)
          nuc_spin =    0.5    ! tl177
       CASE (178)
          nuc_spin =    2.0    ! tl178
       CASE (179)
          nuc_spin =    0.5    ! tl179
       CASE (180)
          nuc_spin =    0.0    ! tl180
       CASE (181)
          nuc_spin =    0.5    ! tl181
       CASE (182)
          nuc_spin =    7.0    ! tl182
       CASE (183)
          nuc_spin =    0.5    ! tl183
       CASE (184)
          nuc_spin =    5.0    ! tl184
       CASE (185)
          nuc_spin =    0.5    ! tl185
       CASE (186)
          nuc_spin =    7.0    ! tl186
       CASE (187)
          nuc_spin =    0.5    ! tl187
       CASE (188)
          nuc_spin =    2.0    ! tl188
       CASE (189)
          nuc_spin =    0.5    ! tl189
       CASE (190)
          nuc_spin =    2.0    ! tl190
       CASE (191)
          nuc_spin =    0.5    ! tl191
       CASE (192)
          nuc_spin =    2.0    ! tl192
       CASE (193)
          nuc_spin =    0.5    ! tl193
       CASE (194)
          nuc_spin =    2.0    ! tl194
       CASE (195)
          nuc_spin =    0.5    ! tl195
       CASE (196)
          nuc_spin =    2.0    ! tl196
       CASE (197)
          nuc_spin =    0.5    ! tl197
       CASE (198)
          nuc_spin =    2.0    ! tl198
       CASE (199)
          nuc_spin =    0.5    ! tl199
       CASE (200)
          nuc_spin =    2.0    ! tl200
       CASE (201)
          nuc_spin =    0.5    ! tl201
       CASE (202)
          nuc_spin =    2.0    ! tl202
       CASE (203)
          nuc_spin =    0.5    ! tl203
       CASE (204)
          nuc_spin =    2.0    ! tl204
       CASE (205)
          nuc_spin =    0.5    ! tl205
       CASE (206)
          nuc_spin =    0.0    ! tl206
       CASE (207)
          nuc_spin =    0.5    ! tl207
       CASE (208)
          nuc_spin =    0.0    ! tl208
       CASE (209)
          nuc_spin =    0.5    ! tl209
       CASE (210)
          nuc_spin =    5.0    ! tl210
       CASE (211)
          nuc_spin =    0.5    ! tl211
       CASE (212)
          nuc_spin =    3.0    ! tl212
       CASE (213)
          nuc_spin =    0.5    ! tl213
       CASE (214)
          nuc_spin =    1.0    ! tl214
       CASE (215)
          nuc_spin =    0.5    ! tl215
       CASE (216)
          nuc_spin =    1.0    ! tl216
       CASE (217)
          nuc_spin =    0.5    ! tl217
       CASE (218)
          nuc_spin =    1.0    ! tl218
       CASE (219)
          nuc_spin =    0.5    ! tl219
       CASE (220)
          nuc_spin =    3.0    ! tl220
       CASE (221)
          nuc_spin =    1.5    ! tl221
       CASE (222)
          nuc_spin =    3.0    ! tl222
       CASE (223)
          nuc_spin =    1.5    ! tl223
       CASE (224)
          nuc_spin =    1.0    ! tl224
       CASE (225)
          nuc_spin =    1.5    ! tl225
       CASE (226)
          nuc_spin =    2.0    ! tl226
       CASE (227)
          nuc_spin =    1.5    ! tl227
       CASE (228)
          nuc_spin =    3.0    ! tl228
       CASE (229)
          nuc_spin =    1.5    ! tl229
       CASE (230)
          nuc_spin =    2.0    ! tl230
       CASE (231)
          nuc_spin =    1.5    ! tl231
       CASE (232)
          nuc_spin =    3.0    ! tl232
       CASE (233)
          nuc_spin =    5.5    ! tl233
       CASE (234)
          nuc_spin =    2.0    ! tl234
       CASE (235)
          nuc_spin =    1.5    ! tl235
       CASE (236)
          nuc_spin =    2.0    ! tl236
       CASE (237)
          nuc_spin =    1.5    ! tl237
       CASE (238)
          nuc_spin =    2.0    ! tl238
       CASE (239)
          nuc_spin =    1.5    ! tl239
       CASE (240)
          nuc_spin =    1.0    ! tl240
       CASE (241)
          nuc_spin =    0.5    ! tl241
       CASE (242)
          nuc_spin =    6.0    ! tl242
       CASE (243)
          nuc_spin =    0.5    ! tl243
       CASE (244)
          nuc_spin =    1.0    ! tl244
       CASE (245)
          nuc_spin =    0.5    ! tl245
       CASE (246)
          nuc_spin =    8.0    ! tl246
       CASE (247)
          nuc_spin =    0.5    ! tl247
       CASE (248)
          nuc_spin =    1.0    ! tl248
       CASE (249)
          nuc_spin =    0.5    ! tl249
       CASE (250)
          nuc_spin =    1.0    ! tl250
       CASE (251)
          nuc_spin =    0.5    ! tl251
       CASE (252)
          nuc_spin =    1.0    ! tl252
       CASE (253)
          nuc_spin =    0.5    ! tl253
       CASE (254)
          nuc_spin =    3.0    ! tl254
       CASE (255)
          nuc_spin =    0.5    ! tl255
       CASE (256)
          nuc_spin =    1.0    ! tl256
       CASE (257)
          nuc_spin =    0.5    ! tl257
       CASE (258)
          nuc_spin =    1.0    ! tl258
       CASE (259)
          nuc_spin =    0.5    ! tl259
       CASE (260)
          nuc_spin =    1.0    ! tl260
       CASE (261)
          nuc_spin =    0.5    ! tl261
       CASE (262)
          nuc_spin =    1.0    ! tl262
       CASE (263)
          nuc_spin =    0.5    ! tl263
       CASE (264)
          nuc_spin =    1.0    ! tl264
       CASE (265)
          nuc_spin =    0.5    ! tl265
       CASE (266)
          nuc_spin =    4.0    ! tl266
       CASE (267)
          nuc_spin =    0.5    ! tl267
       CASE (268)
          nuc_spin =    1.0    ! tl268
       CASE (269)
          nuc_spin =    0.5    ! tl269
       END SELECT
    CASE (82)
       SELECT CASE (a)
       CASE (175)
          nuc_spin =    1.5    ! pb175
       CASE (176)
          nuc_spin =    0.0    ! pb176
       CASE (177)
          nuc_spin =    0.5    ! pb177
       CASE (178)
          nuc_spin =    0.0    ! pb178
       CASE (179)
          nuc_spin =    3.5    ! pb179
       CASE (180)
          nuc_spin =    0.0    ! pb180
       CASE (181)
          nuc_spin =    6.5    ! pb181
       CASE (182)
          nuc_spin =    0.0    ! pb182
       CASE (183)
          nuc_spin =    0.5    ! pb183
       CASE (184)
          nuc_spin =    0.0    ! pb184
       CASE (185)
          nuc_spin =    1.5    ! pb185
       CASE (186)
          nuc_spin =    0.0    ! pb186
       CASE (187)
          nuc_spin =    6.5    ! pb187
       CASE (188)
          nuc_spin =    0.0    ! pb188
       CASE (189)
          nuc_spin =    2.5    ! pb189
       CASE (190)
          nuc_spin =    0.0    ! pb190
       CASE (191)
          nuc_spin =    1.5    ! pb191
       CASE (192)
          nuc_spin =    0.0    ! pb192
       CASE (193)
          nuc_spin =    1.5    ! pb193
       CASE (194)
          nuc_spin =    0.0    ! pb194
       CASE (195)
          nuc_spin =    1.5    ! pb195
       CASE (196)
          nuc_spin =    0.0    ! pb196
       CASE (197)
          nuc_spin =    1.5    ! pb197
       CASE (198)
          nuc_spin =    0.0    ! pb198
       CASE (199)
          nuc_spin =    1.5    ! pb199
       CASE (200)
          nuc_spin =    0.0    ! pb200
       CASE (201)
          nuc_spin =    2.5    ! pb201
       CASE (202)
          nuc_spin =    0.0    ! pb202
       CASE (203)
          nuc_spin =    2.5    ! pb203
       CASE (204)
          nuc_spin =    0.0    ! pb204
       CASE (205)
          nuc_spin =    2.5    ! pb205
       CASE (206)
          nuc_spin =    0.0    ! pb206
       CASE (207)
          nuc_spin =    0.5    ! pb207
       CASE (208)
          nuc_spin =    0.0    ! pb208
       CASE (209)
          nuc_spin =    4.5    ! pb209
       CASE (210)
          nuc_spin =    0.0    ! pb210
       CASE (211)
          nuc_spin =    4.5    ! pb211
       CASE (212)
          nuc_spin =    0.0    ! pb212
       CASE (213)
          nuc_spin =    4.5    ! pb213
       CASE (214)
          nuc_spin =    0.0    ! pb214
       CASE (215)
          nuc_spin =    3.5    ! pb215
       CASE (216)
          nuc_spin =    0.0    ! pb216
       CASE (217)
          nuc_spin =    4.5    ! pb217
       CASE (218)
          nuc_spin =    0.0    ! pb218
       CASE (219)
          nuc_spin =    0.5    ! pb219
       CASE (220)
          nuc_spin =    0.0    ! pb220
       CASE (221)
          nuc_spin =    2.5    ! pb221
       CASE (222)
          nuc_spin =    0.0    ! pb222
       CASE (223)
          nuc_spin =    2.5    ! pb223
       CASE (224)
          nuc_spin =    0.0    ! pb224
       CASE (225)
          nuc_spin =    3.5    ! pb225
       CASE (226)
          nuc_spin =    0.0    ! pb226
       CASE (227)
          nuc_spin =    0.5    ! pb227
       CASE (228)
          nuc_spin =    0.0    ! pb228
       CASE (229)
          nuc_spin =    2.5    ! pb229
       CASE (230)
          nuc_spin =    0.0    ! pb230
       CASE (231)
          nuc_spin =    3.5    ! pb231
       CASE (232)
          nuc_spin =    0.0    ! pb232
       CASE (233)
          nuc_spin =    4.5    ! pb233
       CASE (234)
          nuc_spin =    0.0    ! pb234
       CASE (235)
          nuc_spin =    3.5    ! pb235
       CASE (236)
          nuc_spin =    0.0    ! pb236
       CASE (237)
          nuc_spin =    0.5    ! pb237
       CASE (238)
          nuc_spin =    0.0    ! pb238
       CASE (239)
          nuc_spin =    0.5    ! pb239
       CASE (240)
          nuc_spin =    0.0    ! pb240
       CASE (241)
          nuc_spin =    5.5    ! pb241
       CASE (242)
          nuc_spin =    0.0    ! pb242
       CASE (243)
          nuc_spin =    6.5    ! pb243
       CASE (244)
          nuc_spin =    0.0    ! pb244
       CASE (245)
          nuc_spin =    0.5    ! pb245
       CASE (246)
          nuc_spin =    0.0    ! pb246
       CASE (247)
          nuc_spin =    0.5    ! pb247
       CASE (248)
          nuc_spin =    0.0    ! pb248
       CASE (249)
          nuc_spin =    1.5    ! pb249
       CASE (250)
          nuc_spin =    0.0    ! pb250
       CASE (251)
          nuc_spin =    0.5    ! pb251
       CASE (252)
          nuc_spin =    0.0    ! pb252
       CASE (253)
          nuc_spin =    2.5    ! pb253
       CASE (254)
          nuc_spin =    0.0    ! pb254
       CASE (255)
          nuc_spin =    2.5    ! pb255
       CASE (256)
          nuc_spin =    0.0    ! pb256
       CASE (257)
          nuc_spin =    2.5    ! pb257
       CASE (258)
          nuc_spin =    0.0    ! pb258
       CASE (259)
          nuc_spin =    0.5    ! pb259
       CASE (260)
          nuc_spin =    0.0    ! pb260
       CASE (261)
          nuc_spin =    0.5    ! pb261
       CASE (262)
          nuc_spin =    0.0    ! pb262
       CASE (263)
          nuc_spin =    1.5    ! pb263
       CASE (264)
          nuc_spin =    0.0    ! pb264
       CASE (265)
          nuc_spin =    0.5    ! pb265
       CASE (266)
          nuc_spin =    0.0    ! pb266
       CASE (267)
          nuc_spin =    3.5    ! pb267
       CASE (268)
          nuc_spin =    0.0    ! pb268
       CASE (269)
          nuc_spin =    0.5    ! pb269
       CASE (270)
          nuc_spin =    0.0    ! pb270
       CASE (271)
          nuc_spin =    2.5    ! pb271
       CASE (272)
          nuc_spin =    0.0    ! pb272
       CASE (273)
          nuc_spin =    3.5    ! pb273
       END SELECT
    CASE (83)
       SELECT CASE (a)
       CASE (178)
          nuc_spin =    3.0    ! bi178
       CASE (179)
          nuc_spin =    0.5    ! bi179
       CASE (180)
          nuc_spin =    3.0    ! bi180
       CASE (181)
          nuc_spin =    4.5    ! bi181
       CASE (182)
          nuc_spin =    5.0    ! bi182
       CASE (183)
          nuc_spin =    4.5    ! bi183
       CASE (184)
          nuc_spin =    2.0    ! bi184
       CASE (185)
          nuc_spin =    4.5    ! bi185
       CASE (186)
          nuc_spin =    6.0    ! bi186
       CASE (187)
          nuc_spin =    4.5    ! bi187
       CASE (188)
          nuc_spin =    0.0    ! bi188
       CASE (189)
          nuc_spin =    4.5    ! bi189
       CASE (190)
          nuc_spin =    5.0    ! bi190
       CASE (191)
          nuc_spin =    4.5    ! bi191
       CASE (192)
          nuc_spin =    2.0    ! bi192
       CASE (193)
          nuc_spin =    4.5    ! bi193
       CASE (194)
          nuc_spin =    2.0    ! bi194
       CASE (195)
          nuc_spin =    4.5    ! bi195
       CASE (196)
          nuc_spin =    3.0    ! bi196
       CASE (197)
          nuc_spin =    4.5    ! bi197
       CASE (198)
          nuc_spin =    2.0    ! bi198
       CASE (199)
          nuc_spin =    4.5    ! bi199
       CASE (200)
          nuc_spin =    7.0    ! bi200
       CASE (201)
          nuc_spin =    4.5    ! bi201
       CASE (202)
          nuc_spin =    5.0    ! bi202
       CASE (203)
          nuc_spin =    4.5    ! bi203
       CASE (204)
          nuc_spin =    6.0    ! bi204
       CASE (205)
          nuc_spin =    4.5    ! bi205
       CASE (206)
          nuc_spin =    0.0    ! bi206
       CASE (207)
          nuc_spin =    4.5    ! bi207
       CASE (208)
          nuc_spin =    5.0    ! bi208
       CASE (209)
          nuc_spin =    4.5    ! bi209
       CASE (210)
          nuc_spin =    1.0    ! bi210
       CASE (211)
          nuc_spin =    4.5    ! bi211
       CASE (212)
          nuc_spin =    0.0    ! bi212
       CASE (213)
          nuc_spin =    4.5    ! bi213
       CASE (214)
          nuc_spin =    1.0    ! bi214
       CASE (215)
          nuc_spin =    4.5    ! bi215
       CASE (216)
          nuc_spin =    3.0    ! bi216
       CASE (217)
          nuc_spin =    0.5    ! bi217
       CASE (218)
          nuc_spin =    1.0    ! bi218
       CASE (219)
          nuc_spin =    0.5    ! bi219
       CASE (220)
          nuc_spin =    3.0    ! bi220
       CASE (221)
          nuc_spin =    0.5    ! bi221
       CASE (222)
          nuc_spin =    3.0    ! bi222
       CASE (223)
          nuc_spin =    0.5    ! bi223
       CASE (224)
          nuc_spin =    2.0    ! bi224
       CASE (225)
          nuc_spin =    0.5    ! bi225
       CASE (226)
          nuc_spin =    3.0    ! bi226
       CASE (227)
          nuc_spin =    1.5    ! bi227
       CASE (228)
          nuc_spin =    3.0    ! bi228
       CASE (229)
          nuc_spin =    1.5    ! bi229
       CASE (230)
          nuc_spin =    1.0    ! bi230
       CASE (231)
          nuc_spin =    1.5    ! bi231
       CASE (232)
          nuc_spin =    4.0    ! bi232
       CASE (233)
          nuc_spin =    1.5    ! bi233
       CASE (234)
          nuc_spin =    5.0    ! bi234
       CASE (235)
          nuc_spin =    0.5    ! bi235
       CASE (236)
          nuc_spin =    4.0    ! bi236
       CASE (237)
          nuc_spin =    1.5    ! bi237
       CASE (238)
          nuc_spin =    1.0    ! bi238
       CASE (239)
          nuc_spin =    1.5    ! bi239
       CASE (240)
          nuc_spin =    1.0    ! bi240
       CASE (241)
          nuc_spin =    1.5    ! bi241
       CASE (242)
          nuc_spin =    1.0    ! bi242
       CASE (243)
          nuc_spin =    0.5    ! bi243
       CASE (244)
          nuc_spin =    7.0    ! bi244
       CASE (245)
          nuc_spin =    0.5    ! bi245
       CASE (246)
          nuc_spin =    0.0    ! bi246
       CASE (247)
          nuc_spin =    4.5    ! bi247
       CASE (248)
          nuc_spin =    0.0    ! bi248
       CASE (249)
          nuc_spin =    0.5    ! bi249
       CASE (250)
          nuc_spin =    2.0    ! bi250
       CASE (251)
          nuc_spin =    0.5    ! bi251
       CASE (252)
          nuc_spin =    0.0    ! bi252
       CASE (253)
          nuc_spin =    0.5    ! bi253
       CASE (254)
          nuc_spin =    2.0    ! bi254
       CASE (255)
          nuc_spin =    0.5    ! bi255
       CASE (256)
          nuc_spin =    2.0    ! bi256
       CASE (257)
          nuc_spin =    0.5    ! bi257
       CASE (258)
          nuc_spin =    2.0    ! bi258
       CASE (259)
          nuc_spin =    4.5    ! bi259
       CASE (260)
          nuc_spin =    2.0    ! bi260
       CASE (261)
          nuc_spin =    4.5    ! bi261
       CASE (262)
          nuc_spin =    4.0    ! bi262
       CASE (263)
          nuc_spin =    4.5    ! bi263
       CASE (264)
          nuc_spin =    2.0    ! bi264
       CASE (265)
          nuc_spin =    3.5    ! bi265
       CASE (266)
          nuc_spin =    4.0    ! bi266
       CASE (267)
          nuc_spin =    3.5    ! bi267
       CASE (268)
          nuc_spin =    4.0    ! bi268
       CASE (269)
          nuc_spin =    3.5    ! bi269
       CASE (270)
          nuc_spin =    4.0    ! bi270
       CASE (271)
          nuc_spin =    4.5    ! bi271
       CASE (272)
          nuc_spin =    5.0    ! bi272
       CASE (273)
          nuc_spin =    4.5    ! bi273
       CASE (274)
          nuc_spin =    1.0    ! bi274
       CASE (275)
          nuc_spin =    4.5    ! bi275
       CASE (276)
          nuc_spin =    3.0    ! bi276
       END SELECT
    CASE (84)
       SELECT CASE (a)
       CASE (182)
          nuc_spin =    0.0    ! po182
       CASE (183)
          nuc_spin =    3.5    ! po183
       CASE (184)
          nuc_spin =    0.0    ! po184
       CASE (185)
          nuc_spin =    0.5    ! po185
       CASE (186)
          nuc_spin =    0.0    ! po186
       CASE (187)
          nuc_spin =    2.5    ! po187
       CASE (188)
          nuc_spin =    0.0    ! po188
       CASE (189)
          nuc_spin =    3.5    ! po189
       CASE (190)
          nuc_spin =    0.0    ! po190
       CASE (191)
          nuc_spin =    4.5    ! po191
       CASE (192)
          nuc_spin =    0.0    ! po192
       CASE (193)
          nuc_spin =    2.5    ! po193
       CASE (194)
          nuc_spin =    0.0    ! po194
       CASE (195)
          nuc_spin =    1.5    ! po195
       CASE (196)
          nuc_spin =    0.0    ! po196
       CASE (197)
          nuc_spin =    1.5    ! po197
       CASE (198)
          nuc_spin =    0.0    ! po198
       CASE (199)
          nuc_spin =    1.5    ! po199
       CASE (200)
          nuc_spin =    0.0    ! po200
       CASE (201)
          nuc_spin =    1.5    ! po201
       CASE (202)
          nuc_spin =    0.0    ! po202
       CASE (203)
          nuc_spin =    2.5    ! po203
       CASE (204)
          nuc_spin =    0.0    ! po204
       CASE (205)
          nuc_spin =    2.5    ! po205
       CASE (206)
          nuc_spin =    0.0    ! po206
       CASE (207)
          nuc_spin =    2.5    ! po207
       CASE (208)
          nuc_spin =    0.0    ! po208
       CASE (209)
          nuc_spin =    0.5    ! po209
       CASE (210)
          nuc_spin =    0.0    ! po210
       CASE (211)
          nuc_spin =    4.5    ! po211
       CASE (212)
          nuc_spin =    0.0    ! po212
       CASE (213)
          nuc_spin =    4.5    ! po213
       CASE (214)
          nuc_spin =    0.0    ! po214
       CASE (215)
          nuc_spin =    4.5    ! po215
       CASE (216)
          nuc_spin =    0.0    ! po216
       CASE (217)
          nuc_spin =    3.5    ! po217
       CASE (218)
          nuc_spin =    0.0    ! po218
       CASE (219)
          nuc_spin =    1.5    ! po219
       CASE (220)
          nuc_spin =    0.0    ! po220
       CASE (221)
          nuc_spin =    1.5    ! po221
       CASE (222)
          nuc_spin =    0.0    ! po222
       CASE (223)
          nuc_spin =    2.5    ! po223
       CASE (224)
          nuc_spin =    0.0    ! po224
       CASE (225)
          nuc_spin =    2.5    ! po225
       CASE (226)
          nuc_spin =    0.0    ! po226
       CASE (227)
          nuc_spin =    3.5    ! po227
       CASE (228)
          nuc_spin =    0.0    ! po228
       CASE (229)
          nuc_spin =    0.5    ! po229
       CASE (230)
          nuc_spin =    0.0    ! po230
       CASE (231)
          nuc_spin =    2.5    ! po231
       CASE (232)
          nuc_spin =    0.0    ! po232
       CASE (233)
          nuc_spin =    3.5    ! po233
       CASE (234)
          nuc_spin =    0.0    ! po234
       CASE (235)
          nuc_spin =    4.5    ! po235
       CASE (236)
          nuc_spin =    0.0    ! po236
       CASE (237)
          nuc_spin =    3.5    ! po237
       CASE (238)
          nuc_spin =    0.0    ! po238
       CASE (239)
          nuc_spin =    0.5    ! po239
       CASE (240)
          nuc_spin =    0.0    ! po240
       CASE (241)
          nuc_spin =    0.5    ! po241
       CASE (242)
          nuc_spin =    0.0    ! po242
       CASE (243)
          nuc_spin =    0.5    ! po243
       CASE (244)
          nuc_spin =    0.0    ! po244
       CASE (245)
          nuc_spin =    1.5    ! po245
       CASE (246)
          nuc_spin =    0.0    ! po246
       CASE (247)
          nuc_spin =    7.5    ! po247
       CASE (248)
          nuc_spin =    0.0    ! po248
       CASE (249)
          nuc_spin =    0.5    ! po249
       CASE (250)
          nuc_spin =    0.0    ! po250
       CASE (251)
          nuc_spin =    1.5    ! po251
       CASE (252)
          nuc_spin =    0.0    ! po252
       CASE (253)
          nuc_spin =    1.5    ! po253
       CASE (254)
          nuc_spin =    0.0    ! po254
       CASE (255)
          nuc_spin =    1.5    ! po255
       CASE (256)
          nuc_spin =    0.0    ! po256
       CASE (257)
          nuc_spin =    2.5    ! po257
       CASE (258)
          nuc_spin =    0.0    ! po258
       CASE (259)
          nuc_spin =    2.5    ! po259
       CASE (260)
          nuc_spin =    0.0    ! po260
       CASE (261)
          nuc_spin =    2.5    ! po261
       CASE (262)
          nuc_spin =    0.0    ! po262
       CASE (263)
          nuc_spin =    0.5    ! po263
       CASE (264)
          nuc_spin =    0.0    ! po264
       CASE (265)
          nuc_spin =    1.5    ! po265
       CASE (266)
          nuc_spin =    0.0    ! po266
       CASE (267)
          nuc_spin =    0.5    ! po267
       CASE (268)
          nuc_spin =    0.0    ! po268
       CASE (269)
          nuc_spin =    3.5    ! po269
       CASE (270)
          nuc_spin =    0.0    ! po270
       CASE (271)
          nuc_spin =    0.5    ! po271
       CASE (272)
          nuc_spin =    0.0    ! po272
       CASE (273)
          nuc_spin =    2.5    ! po273
       CASE (274)
          nuc_spin =    0.0    ! po274
       CASE (275)
          nuc_spin =    3.5    ! po275
       CASE (276)
          nuc_spin =    0.0    ! po276
       END SELECT
    CASE (85)
       SELECT CASE (a)
       CASE (186)
          nuc_spin =    2.0    ! at186
       CASE (187)
          nuc_spin =    1.5    ! at187
       CASE (188)
          nuc_spin =    0.0    ! at188
       CASE (189)
          nuc_spin =    2.5    ! at189
       CASE (190)
          nuc_spin =    2.0    ! at190
       CASE (191)
          nuc_spin =    3.5    ! at191
       CASE (192)
          nuc_spin =    0.0    ! at192
       CASE (193)
          nuc_spin =    3.5    ! at193
       CASE (194)
          nuc_spin =    4.0    ! at194
       CASE (195)
          nuc_spin =    3.5    ! at195
       CASE (196)
          nuc_spin =    2.0    ! at196
       CASE (197)
          nuc_spin =    4.5    ! at197
       CASE (198)
          nuc_spin =    3.0    ! at198
       CASE (199)
          nuc_spin =    4.5    ! at199
       CASE (200)
          nuc_spin =    3.0    ! at200
       CASE (201)
          nuc_spin =    4.5    ! at201
       CASE (202)
          nuc_spin =    5.0    ! at202
       CASE (203)
          nuc_spin =    4.5    ! at203
       CASE (204)
          nuc_spin =    7.0    ! at204
       CASE (205)
          nuc_spin =    4.5    ! at205
       CASE (206)
          nuc_spin =    5.0    ! at206
       CASE (207)
          nuc_spin =    4.5    ! at207
       CASE (208)
          nuc_spin =    6.0    ! at208
       CASE (209)
          nuc_spin =    4.5    ! at209
       CASE (210)
          nuc_spin =    5.0    ! at210
       CASE (211)
          nuc_spin =    4.5    ! at211
       CASE (212)
          nuc_spin =    1.0    ! at212
       CASE (213)
          nuc_spin =    4.5    ! at213
       CASE (214)
          nuc_spin =    1.0    ! at214
       CASE (215)
          nuc_spin =    4.5    ! at215
       CASE (216)
          nuc_spin =    0.0    ! at216
       CASE (217)
          nuc_spin =    4.5    ! at217
       CASE (218)
          nuc_spin =    2.0    ! at218
       CASE (219)
          nuc_spin =    1.5    ! at219
       CASE (220)
          nuc_spin =    2.0    ! at220
       CASE (221)
          nuc_spin =    1.5    ! at221
       CASE (222)
          nuc_spin =    1.0    ! at222
       CASE (223)
          nuc_spin =    0.5    ! at223
       CASE (224)
          nuc_spin =    3.0    ! at224
       CASE (225)
          nuc_spin =    0.5    ! at225
       CASE (226)
          nuc_spin =    2.0    ! at226
       CASE (227)
          nuc_spin =    1.5    ! at227
       CASE (228)
          nuc_spin =    4.0    ! at228
       CASE (229)
          nuc_spin =    0.5    ! at229
       CASE (230)
          nuc_spin =    1.0    ! at230
       CASE (231)
          nuc_spin =    0.5    ! at231
       CASE (232)
          nuc_spin =    3.0    ! at232
       CASE (233)
          nuc_spin =    0.5    ! at233
       CASE (234)
          nuc_spin =    3.0    ! at234
       CASE (235)
          nuc_spin =    0.5    ! at235
       CASE (236)
          nuc_spin =    5.0    ! at236
       CASE (237)
          nuc_spin =    0.5    ! at237
       CASE (238)
          nuc_spin =    4.0    ! at238
       CASE (239)
          nuc_spin =    0.5    ! at239
       CASE (240)
          nuc_spin =    1.0    ! at240
       CASE (241)
          nuc_spin =    0.5    ! at241
       CASE (242)
          nuc_spin =    6.0    ! at242
       CASE (243)
          nuc_spin =    0.5    ! at243
       CASE (244)
          nuc_spin =    1.0    ! at244
       CASE (245)
          nuc_spin =    0.5    ! at245
       CASE (246)
          nuc_spin =    1.0    ! at246
       CASE (247)
          nuc_spin =    2.5    ! at247
       CASE (248)
          nuc_spin =    5.0    ! at248
       CASE (249)
          nuc_spin =    1.5    ! at249
       CASE (250)
          nuc_spin =    8.0    ! at250
       CASE (251)
          nuc_spin =    1.5    ! at251
       CASE (252)
          nuc_spin =    2.0    ! at252
       CASE (253)
          nuc_spin =    1.5    ! at253
       CASE (254)
          nuc_spin =    0.0    ! at254
       CASE (255)
          nuc_spin =    1.5    ! at255
       CASE (256)
          nuc_spin =    0.0    ! at256
       CASE (257)
          nuc_spin =    3.5    ! at257
       CASE (258)
          nuc_spin =    3.0    ! at258
       CASE (259)
          nuc_spin =    1.5    ! at259
       CASE (260)
          nuc_spin =    2.0    ! at260
       CASE (261)
          nuc_spin =    3.5    ! at261
       CASE (262)
          nuc_spin =    4.0    ! at262
       CASE (263)
          nuc_spin =    3.5    ! at263
       CASE (264)
          nuc_spin =    4.0    ! at264
       CASE (265)
          nuc_spin =    3.5    ! at265
       CASE (266)
          nuc_spin =    2.0    ! at266
       CASE (267)
          nuc_spin =    4.5    ! at267
       CASE (268)
          nuc_spin =    2.0    ! at268
       CASE (269)
          nuc_spin =    2.5    ! at269
       CASE (270)
          nuc_spin =    1.0    ! at270
       CASE (271)
          nuc_spin =    2.5    ! at271
       CASE (272)
          nuc_spin =    1.0    ! at272
       CASE (273)
          nuc_spin =    4.5    ! at273
       CASE (274)
          nuc_spin =    3.0    ! at274
       CASE (275)
          nuc_spin =    2.5    ! at275
       CASE (276)
          nuc_spin =    3.0    ! at276
       CASE (277)
          nuc_spin =    2.5    ! at277
       CASE (278)
          nuc_spin =    0.0    ! at278
       CASE (279)
          nuc_spin =    2.5    ! at279
       END SELECT
    END SELECT
  END FUNCTION nuc_spin

  !---------------------------------------------------------------------
  FUNCTION nuc_m_excess(z,a)
    IMPLICIT NONE
    REAL(KIND=8),PARAMETER        :: bad = HUGE(1D0)
    INTEGER(KIND=4),INTENT(IN)    :: z,a
    REAL(KIND=8)                  :: nuc_m_excess
    
    nuc_m_excess = bad    !no data
    
    SELECT CASE (z)
    CASE (0)
       SELECT CASE (a)
       CASE(1)
          nuc_m_excess = 8.0713171      ! n
       END SELECT
    CASE (1)
       SELECT CASE (a)
       CASE(1)
          nuc_m_excess = 7.2889705      ! H1
       CASE(2)
          nuc_m_excess = 13.13572158      ! H2
       CASE(3)
          nuc_m_excess = 14.949806      ! H3
       CASE(4)
          nuc_m_excess = 25.901518      ! H4
       CASE(5)
          nuc_m_excess = 32.89244      ! H5
       CASE(6)
          nuc_m_excess = 41.863757      ! H6
       CASE(7)
          nuc_m_excess = 49.135      ! H7
       END SELECT
    CASE (2)
       SELECT CASE (a)
       CASE(3)
          nuc_m_excess = 14.93121475      ! He3
       CASE(4)
          nuc_m_excess = 2.42491565      ! He4
       CASE(5)
          nuc_m_excess = 11.386233      ! He5
       CASE(6)
          nuc_m_excess = 17.595106      ! He6
       CASE(7)
          nuc_m_excess = 26.101038      ! He7
       CASE(8)
          nuc_m_excess = 31.598044      ! He8
       CASE(9)
          nuc_m_excess = 40.939429      ! He9
       CASE(10)
          nuc_m_excess = 48.809203      ! He10
       END SELECT
    CASE (3)
       SELECT CASE (a)
       CASE(3)
          nuc_m_excess = 28.667      ! Li3
       CASE(4)
          nuc_m_excess = 25.323185      ! Li4
       CASE(5)
          nuc_m_excess = 11.678886      ! Li5
       CASE(6)
          nuc_m_excess = 14.086793      ! Li6
       CASE(7)
          nuc_m_excess = 14.908141      ! Li7
       CASE(8)
          nuc_m_excess = 20.946844      ! Li8
       CASE(9)
          nuc_m_excess = 24.954264      ! Li9
       CASE(10)
          nuc_m_excess = 33.050581      ! Li10
       CASE(11)
          nuc_m_excess = 40.79731      ! Li11
       CASE(12)
          nuc_m_excess = 50.096      ! Li12
       END SELECT
    CASE (4)
       SELECT CASE (a)
       CASE(5)
          nuc_m_excess = 37.996      ! Be5
       CASE(6)
          nuc_m_excess = 18.374947      ! Be6
       CASE(7)
          nuc_m_excess = 15.770034      ! Be7
       CASE(8)
          nuc_m_excess = 4.941672      ! Be8
       CASE(9)
          nuc_m_excess = 11.347648      ! Be9
       CASE(10)
          nuc_m_excess = 12.60667      ! Be10
       CASE(11)
          nuc_m_excess = 20.174064      ! Be11
       CASE(12)
          nuc_m_excess = 25.076506      ! Be12
       CASE(13)
          nuc_m_excess = 33.247823      ! Be13
       CASE(14)
          nuc_m_excess = 39.954498      ! Be14
       CASE(15)
          nuc_m_excess = 49.798      ! Be15
       CASE(16)
          nuc_m_excess = 57.678      ! Be16
       END SELECT
    CASE (5)
       SELECT CASE (a)
       CASE(6)
          nuc_m_excess = 43.603      ! B6
       CASE(7)
          nuc_m_excess = 27.868346      ! B7
       CASE(8)
          nuc_m_excess = 22.92149      ! B8
       CASE(9)
          nuc_m_excess = 12.415681      ! B9
       CASE(10)
          nuc_m_excess = 12.050731      ! B10
       CASE(11)
          nuc_m_excess = 8.667931      ! B11
       CASE(12)
          nuc_m_excess = 13.368899      ! B12
       CASE(13)
          nuc_m_excess = 16.562166      ! B13
       CASE(14)
          nuc_m_excess = 23.663683      ! B14
       CASE(15)
          nuc_m_excess = 28.972278      ! B15
       CASE(16)
          nuc_m_excess = 37.081686      ! B16
       CASE(17)
          nuc_m_excess = 43.770816      ! B17
       CASE(18)
          nuc_m_excess = 52.322      ! B18
       CASE(19)
          nuc_m_excess = 59.364      ! B19
       END SELECT
    CASE (6)
       SELECT CASE (a)
       CASE(8)
          nuc_m_excess = 35.09406      ! C8
       CASE(9)
          nuc_m_excess = 28.910491      ! C9
       CASE(10)
          nuc_m_excess = 15.698682      ! C10
       CASE(11)
          nuc_m_excess = 10.650342      ! C11
       CASE(12)
          nuc_m_excess = 0      ! C12
       CASE(13)
          nuc_m_excess = 3.12501129      ! C13
       CASE(14)
          nuc_m_excess = 3.01989305      ! C14
       CASE(15)
          nuc_m_excess = 9.873144      ! C15
       CASE(16)
          nuc_m_excess = 13.694129      ! C16
       CASE(17)
          nuc_m_excess = 21.038832      ! C17
       CASE(18)
          nuc_m_excess = 24.926178      ! C18
       CASE(19)
          nuc_m_excess = 32.420666      ! C19
       CASE(20)
          nuc_m_excess = 37.55761      ! C20
       CASE(21)
          nuc_m_excess = 45.96      ! C21
       CASE(22)
          nuc_m_excess = 53.281      ! C22
       END SELECT
    CASE (7)
       SELECT CASE (a)
       CASE(10)
          nuc_m_excess = 38.800148      ! N10
       CASE(11)
          nuc_m_excess = 24.303569      ! N11
       CASE(12)
          nuc_m_excess = 17.338082      ! N12
       CASE(13)
          nuc_m_excess = 5.345481      ! N13
       CASE(14)
          nuc_m_excess = 2.86341704      ! N14
       CASE(15)
          nuc_m_excess = 0.10143805      ! N15
       CASE(16)
          nuc_m_excess = 5.683658      ! N16
       CASE(17)
          nuc_m_excess = 7.871368      ! N17
       CASE(18)
          nuc_m_excess = 13.114466      ! N18
       CASE(19)
          nuc_m_excess = 15.862129      ! N19
       CASE(20)
          nuc_m_excess = 21.76511      ! N20
       CASE(21)
          nuc_m_excess = 25.251164      ! N21
       CASE(22)
          nuc_m_excess = 32.038675      ! N22
       CASE(23)
          nuc_m_excess = 38.396      ! N23
       CASE(24)
          nuc_m_excess = 47.543      ! N24
       CASE(25)
          nuc_m_excess = 56.504      ! N25
       END SELECT
    CASE (8)
       SELECT CASE (a)
       CASE(12)
          nuc_m_excess = 32.047954      ! O12
       CASE(13)
          nuc_m_excess = 23.112428      ! O13
       CASE(14)
          nuc_m_excess = 8.007356      ! O14
       CASE(15)
          nuc_m_excess = 2.855605      ! O15
       CASE(16)
          nuc_m_excess = -4.73700141      ! O16
       CASE(17)
          nuc_m_excess = -0.808813      ! O17
       CASE(18)
          nuc_m_excess = -0.781522      ! O18
       CASE(19)
          nuc_m_excess = 3.33487      ! O19
       CASE(20)
          nuc_m_excess = 3.797462      ! O20
       CASE(21)
          nuc_m_excess = 8.062906      ! O21
       CASE(22)
          nuc_m_excess = 9.284152      ! O22
       CASE(23)
          nuc_m_excess = 14.61296      ! O23
       CASE(24)
          nuc_m_excess = 19.0704      ! O24
       CASE(25)
          nuc_m_excess = 27.442      ! O25
       CASE(26)
          nuc_m_excess = 35.713      ! O26
       CASE(27)
          nuc_m_excess = 44.954      ! O27
       CASE(28)
          nuc_m_excess = 53.85      ! O28
       END SELECT
    CASE (9)
       SELECT CASE (a)
       CASE(14)
          nuc_m_excess = 32.658      ! F14
       CASE(15)
          nuc_m_excess = 16.775372      ! F15
       CASE(16)
          nuc_m_excess = 10.680254      ! F16
       CASE(17)
          nuc_m_excess = 1.951701      ! F17
       CASE(18)
          nuc_m_excess = 0.873701      ! F18
       CASE(19)
          nuc_m_excess = -1.487386      ! F19
       CASE(20)
          nuc_m_excess = -0.017404      ! F20
       CASE(21)
          nuc_m_excess = -0.047551      ! F21
       CASE(22)
          nuc_m_excess = 2.793378      ! F22
       CASE(23)
          nuc_m_excess = 3.329747      ! F23
       CASE(24)
          nuc_m_excess = 7.559527      ! F24
       CASE(25)
          nuc_m_excess = 11.272706      ! F25
       CASE(26)
          nuc_m_excess = 18.271772      ! F26
       CASE(27)
          nuc_m_excess = 24.92686      ! F27
       CASE(28)
          nuc_m_excess = 33.226      ! F28
       CASE(29)
          nuc_m_excess = 40.296      ! F29
       CASE(30)
          nuc_m_excess = 48.903      ! F30
       CASE(31)
          nuc_m_excess = 56.289      ! F31
       END SELECT
    CASE (10)
       SELECT CASE (a)
       CASE(16)
          nuc_m_excess = 23.996462      ! Ne16
       CASE(17)
          nuc_m_excess = 16.460901      ! Ne17
       CASE(18)
          nuc_m_excess = 5.317166      ! Ne18
       CASE(19)
          nuc_m_excess = 1.75144      ! Ne19
       CASE(20)
          nuc_m_excess = -7.04193131      ! Ne20
       CASE(21)
          nuc_m_excess = -5.731776      ! Ne21
       CASE(22)
          nuc_m_excess = -8.024715      ! Ne22
       CASE(23)
          nuc_m_excess = -5.154045      ! Ne23
       CASE(24)
          nuc_m_excess = -5.951521      ! Ne24
       CASE(25)
          nuc_m_excess = -2.108075      ! Ne25
       CASE(26)
          nuc_m_excess = 0.429611      ! Ne26
       CASE(27)
          nuc_m_excess = 7.069949      ! Ne27
       CASE(28)
          nuc_m_excess = 11.244601      ! Ne28
       CASE(29)
          nuc_m_excess = 18.057881      ! Ne29
       CASE(30)
          nuc_m_excess = 23.102025      ! Ne30
       CASE(31)
          nuc_m_excess = 30.842      ! Ne31
       CASE(32)
          nuc_m_excess = 37.278      ! Ne32
       CASE(33)
          nuc_m_excess = 45.997      ! Ne33
       CASE(34)
          nuc_m_excess = 53.121      ! Ne34
       END SELECT
    CASE (11)
       SELECT CASE (a)
       CASE(18)
          nuc_m_excess = 24.189968      ! Na18
       CASE(19)
          nuc_m_excess = 12.926808      ! Na19
       CASE(20)
          nuc_m_excess = 6.847719      ! Na20
       CASE(21)
          nuc_m_excess = -2.184161      ! Na21
       CASE(22)
          nuc_m_excess = -5.182436      ! Na22
       CASE(23)
          nuc_m_excess = -9.52985358      ! Na23
       CASE(24)
          nuc_m_excess = -8.418114      ! Na24
       CASE(25)
          nuc_m_excess = -9.357818      ! Na25
       CASE(26)
          nuc_m_excess = -6.862316      ! Na26
       CASE(27)
          nuc_m_excess = -5.517436      ! Na27
       CASE(28)
          nuc_m_excess = -0.989247      ! Na28
       CASE(29)
          nuc_m_excess = 2.665004      ! Na29
       CASE(30)
          nuc_m_excess = 8.36109      ! Na30
       CASE(31)
          nuc_m_excess = 12.654768      ! Na31
       CASE(32)
          nuc_m_excess = 19.064478      ! Na32
       CASE(33)
          nuc_m_excess = 24.889293      ! Na33
       CASE(34)
          nuc_m_excess = 32.761      ! Na34
       CASE(35)
          nuc_m_excess = 39.582      ! Na35
       CASE(36)
          nuc_m_excess = 47.953      ! Na36
       CASE(37)
          nuc_m_excess = 55.275      ! Na37
       END SELECT
    CASE (12)
       SELECT CASE (a)
       CASE(19)
          nuc_m_excess = 33.040092      ! Mg19
       CASE(20)
          nuc_m_excess = 17.570348      ! Mg20
       CASE(21)
          nuc_m_excess = 10.910506      ! Mg21
       CASE(22)
          nuc_m_excess = -0.396963      ! Mg22
       CASE(23)
          nuc_m_excess = -5.473766      ! Mg23
       CASE(24)
          nuc_m_excess = -13.933567      ! Mg24
       CASE(25)
          nuc_m_excess = -13.192826      ! Mg25
       CASE(26)
          nuc_m_excess = -16.214582      ! Mg26
       CASE(27)
          nuc_m_excess = -14.586651      ! Mg27
       CASE(28)
          nuc_m_excess = -15.018641      ! Mg28
       CASE(29)
          nuc_m_excess = -10.619032      ! Mg29
       CASE(30)
          nuc_m_excess = -8.910672      ! Mg30
       CASE(31)
          nuc_m_excess = -3.21738      ! Mg31
       CASE(32)
          nuc_m_excess = -0.954781      ! Mg32
       CASE(33)
          nuc_m_excess = 4.89407      ! Mg33
       CASE(34)
          nuc_m_excess = 8.808603      ! Mg34
       CASE(35)
          nuc_m_excess = 16.152      ! Mg35
       CASE(36)
          nuc_m_excess = 21.424      ! Mg36
       CASE(37)
          nuc_m_excess = 29.249      ! Mg37
       CASE(38)
          nuc_m_excess = 34.996      ! Mg38
       CASE(39)
          nuc_m_excess = 43.568      ! Mg39
       CASE(40)
          nuc_m_excess = 50.235      ! Mg40
       END SELECT
    CASE (13)
       SELECT CASE (a)
       CASE(21)
          nuc_m_excess = 26.119      ! Al21
       CASE(22)
          nuc_m_excess = 18.183      ! Al22
       CASE(23)
          nuc_m_excess = 6.76957      ! Al23
       CASE(24)
          nuc_m_excess = -0.056946      ! Al24
       CASE(25)
          nuc_m_excess = -8.916172      ! Al25
       CASE(26)
          nuc_m_excess = -12.210309      ! Al26
       CASE(27)
          nuc_m_excess = -17.196658      ! Al27
       CASE(28)
          nuc_m_excess = -16.850441      ! Al28
       CASE(29)
          nuc_m_excess = -18.215322      ! Al29
       CASE(30)
          nuc_m_excess = -15.872419      ! Al30
       CASE(31)
          nuc_m_excess = -14.953628      ! Al31
       CASE(32)
          nuc_m_excess = -11.061966      ! Al32
       CASE(33)
          nuc_m_excess = -8.529377      ! Al33
       CASE(34)
          nuc_m_excess = -2.932495      ! Al34
       CASE(35)
          nuc_m_excess = -0.13019      ! Al35
       CASE(36)
          nuc_m_excess = 5.781974      ! Al36
       CASE(37)
          nuc_m_excess = 9.946326      ! Al37
       CASE(38)
          nuc_m_excess = 16.050594      ! Al38
       CASE(39)
          nuc_m_excess = 21.396417      ! Al39
       CASE(40)
          nuc_m_excess = 29.295      ! Al40
       CASE(41)
          nuc_m_excess = 35.704      ! Al41
       CASE(42)
          nuc_m_excess = 43.678      ! Al42
       END SELECT
    CASE (14)
       SELECT CASE (a)
       CASE(22)
          nuc_m_excess = 32.164      ! Si22
       CASE(23)
          nuc_m_excess = 23.772      ! Si23
       CASE(24)
          nuc_m_excess = 10.754673      ! Si24
       CASE(25)
          nuc_m_excess = 3.824318      ! Si25
       CASE(26)
          nuc_m_excess = -7.144632      ! Si26
       CASE(27)
          nuc_m_excess = -12.384301      ! Si27
       CASE(28)
          nuc_m_excess = -21.49279678      ! Si28
       CASE(29)
          nuc_m_excess = -21.895046      ! Si29
       CASE(30)
          nuc_m_excess = -24.432928      ! Si30
       CASE(31)
          nuc_m_excess = -22.949006      ! Si31
       CASE(32)
          nuc_m_excess = -24.080907      ! Si32
       CASE(33)
          nuc_m_excess = -20.492662      ! Si33
       CASE(34)
          nuc_m_excess = -19.95677      ! Si34
       CASE(35)
          nuc_m_excess = -14.360307      ! Si35
       CASE(36)
          nuc_m_excess = -12.482507      ! Si36
       CASE(37)
          nuc_m_excess = -6.579998      ! Si37
       CASE(38)
          nuc_m_excess = -4.067274      ! Si38
       CASE(39)
          nuc_m_excess = 1.928205      ! Si39
       CASE(40)
          nuc_m_excess = 5.467052      ! Si40
       CASE(41)
          nuc_m_excess = 13.562553      ! Si41
       CASE(42)
          nuc_m_excess = 18.434      ! Si42
       CASE(43)
          nuc_m_excess = 26.697      ! Si43
       CASE(44)
          nuc_m_excess = 32.844      ! Si44
       END SELECT
    CASE (15)
       SELECT CASE (a)
       CASE(24)
          nuc_m_excess = 31.997      ! P24
       CASE(25)
          nuc_m_excess = 18.872      ! P25
       CASE(26)
          nuc_m_excess = 10.973      ! P26
       CASE(27)
          nuc_m_excess = -0.71703      ! P27
       CASE(28)
          nuc_m_excess = -7.158753      ! P28
       CASE(29)
          nuc_m_excess = -16.952626      ! P29
       CASE(30)
          nuc_m_excess = -20.200575      ! P30
       CASE(31)
          nuc_m_excess = -24.440885      ! P31
       CASE(32)
          nuc_m_excess = -24.305218      ! P32
       CASE(33)
          nuc_m_excess = -26.337486      ! P33
       CASE(34)
          nuc_m_excess = -24.557669      ! P34
       CASE(35)
          nuc_m_excess = -24.85774      ! P35
       CASE(36)
          nuc_m_excess = -20.250977      ! P36
       CASE(37)
          nuc_m_excess = -18.994145      ! P37
       CASE(38)
          nuc_m_excess = -14.75782      ! P38
       CASE(39)
          nuc_m_excess = -12.873735      ! P39
       CASE(40)
          nuc_m_excess = -8.106838      ! P40
       CASE(41)
          nuc_m_excess = -5.276508      ! P41
       CASE(42)
          nuc_m_excess = 0.938865      ! P42
       CASE(43)
          nuc_m_excess = 5.765948      ! P43
       CASE(44)
          nuc_m_excess = 12.1      ! P44
       CASE(45)
          nuc_m_excess = 17.903      ! P45
       CASE(46)
          nuc_m_excess = 25.504      ! P46
       END SELECT
    CASE (16)
       SELECT CASE (a)
       CASE(26)
          nuc_m_excess = 25.97      ! S26
       CASE(27)
          nuc_m_excess = 17.543      ! S27
       CASE(28)
          nuc_m_excess = 4.073203      ! S28
       CASE(29)
          nuc_m_excess = -3.159582      ! S29
       CASE(30)
          nuc_m_excess = -14.062532      ! S30
       CASE(31)
          nuc_m_excess = -19.044648      ! S31
       CASE(32)
          nuc_m_excess = -26.015697      ! S32
       CASE(33)
          nuc_m_excess = -26.585994      ! S33
       CASE(34)
          nuc_m_excess = -29.931788      ! S34
       CASE(35)
          nuc_m_excess = -28.846356      ! S35
       CASE(36)
          nuc_m_excess = -30.664075      ! S36
       CASE(37)
          nuc_m_excess = -26.89636      ! S37
       CASE(38)
          nuc_m_excess = -26.861197      ! S38
       CASE(39)
          nuc_m_excess = -23.162245      ! S39
       CASE(40)
          nuc_m_excess = -22.866568      ! S40
       CASE(41)
          nuc_m_excess = -19.019105      ! S41
       CASE(42)
          nuc_m_excess = -17.677503      ! S42
       CASE(43)
          nuc_m_excess = -11.965235      ! S43
       CASE(44)
          nuc_m_excess = -9.116168      ! S44
       CASE(45)
          nuc_m_excess = -3.252672      ! S45
       CASE(46)
          nuc_m_excess = 0.699      ! S46
       CASE(47)
          nuc_m_excess = 8.002      ! S47
       CASE(48)
          nuc_m_excess = 13.199      ! S48
       CASE(49)
          nuc_m_excess = 22.001      ! S49
       END SELECT
    CASE (17)
       SELECT CASE (a)
       CASE(28)
          nuc_m_excess = 26.557      ! Cl28
       CASE(29)
          nuc_m_excess = 13.143      ! Cl29
       CASE(30)
          nuc_m_excess = 4.443      ! Cl30
       CASE(31)
          nuc_m_excess = -7.067165      ! Cl31
       CASE(32)
          nuc_m_excess = -13.329771      ! Cl32
       CASE(33)
          nuc_m_excess = -21.003432      ! Cl33
       CASE(34)
          nuc_m_excess = -24.439776      ! Cl34
       CASE(35)
          nuc_m_excess = -29.01354      ! Cl35
       CASE(36)
          nuc_m_excess = -29.521857      ! Cl36
       CASE(37)
          nuc_m_excess = -31.761532      ! Cl37
       CASE(38)
          nuc_m_excess = -29.798097      ! Cl38
       CASE(39)
          nuc_m_excess = -29.800203      ! Cl39
       CASE(40)
          nuc_m_excess = -27.55781      ! Cl40
       CASE(41)
          nuc_m_excess = -27.307189      ! Cl41
       CASE(42)
          nuc_m_excess = -24.91299      ! Cl42
       CASE(43)
          nuc_m_excess = -24.168168      ! Cl43
       CASE(44)
          nuc_m_excess = -20.231052      ! Cl44
       CASE(45)
          nuc_m_excess = -18.362647      ! Cl45
       CASE(46)
          nuc_m_excess = -14.708253      ! Cl46
       CASE(47)
          nuc_m_excess = -10.517      ! Cl47
       CASE(48)
          nuc_m_excess = -4.704      ! Cl48
       CASE(49)
          nuc_m_excess = 0.298      ! Cl49
       CASE(50)
          nuc_m_excess = 7.303      ! Cl50
       CASE(51)
          nuc_m_excess = 13.497      ! Cl51
       END SELECT
    CASE (18)
       SELECT CASE (a)
       CASE(30)
          nuc_m_excess = 20.083      ! Ar30
       CASE(31)
          nuc_m_excess = 11.293      ! Ar31
       CASE(32)
          nuc_m_excess = -2.200204      ! Ar32
       CASE(33)
          nuc_m_excess = -9.384141      ! Ar33
       CASE(34)
          nuc_m_excess = -18.377217      ! Ar34
       CASE(35)
          nuc_m_excess = -23.047411      ! Ar35
       CASE(36)
          nuc_m_excess = -30.23154      ! Ar36
       CASE(37)
          nuc_m_excess = -30.947659      ! Ar37
       CASE(38)
          nuc_m_excess = -34.714551      ! Ar38
       CASE(39)
          nuc_m_excess = -33.242011      ! Ar39
       CASE(40)
          nuc_m_excess = -35.03989602      ! Ar40
       CASE(41)
          nuc_m_excess = -33.067467      ! Ar41
       CASE(42)
          nuc_m_excess = -34.422675      ! Ar42
       CASE(43)
          nuc_m_excess = -32.009808      ! Ar43
       CASE(44)
          nuc_m_excess = -32.673053      ! Ar44
       CASE(45)
          nuc_m_excess = -29.770589      ! Ar45
       CASE(46)
          nuc_m_excess = -29.720127      ! Ar46
       CASE(47)
          nuc_m_excess = -25.907836      ! Ar47
       CASE(48)
          nuc_m_excess = -23.716      ! Ar48
       CASE(49)
          nuc_m_excess = -18.146      ! Ar49
       CASE(50)
          nuc_m_excess = -14.503      ! Ar50
       CASE(51)
          nuc_m_excess = -7.797      ! Ar51
       CASE(52)
          nuc_m_excess = -2.999      ! Ar52
       CASE(53)
          nuc_m_excess = 4.602      ! Ar53
       END SELECT
    CASE (19)
       SELECT CASE (a)
       CASE(32)
          nuc_m_excess = 20.418      ! K32
       CASE(33)
          nuc_m_excess = 6.763      ! K33
       CASE(34)
          nuc_m_excess = -1.481      ! K34
       CASE(35)
          nuc_m_excess = -11.1689      ! K35
       CASE(36)
          nuc_m_excess = -17.426171      ! K36
       CASE(37)
          nuc_m_excess = -24.800199      ! K37
       CASE(38)
          nuc_m_excess = -28.800691      ! K38
       CASE(39)
          nuc_m_excess = -33.807011      ! K39
       CASE(40)
          nuc_m_excess = -33.535205      ! K40
       CASE(41)
          nuc_m_excess = -35.559074      ! K41
       CASE(42)
          nuc_m_excess = -35.021556      ! K42
       CASE(43)
          nuc_m_excess = -36.593239      ! K43
       CASE(44)
          nuc_m_excess = -35.809606      ! K44
       CASE(45)
          nuc_m_excess = -36.608186      ! K45
       CASE(46)
          nuc_m_excess = -35.418323      ! K46
       CASE(47)
          nuc_m_excess = -35.696272      ! K47
       CASE(48)
          nuc_m_excess = -32.123935      ! K48
       CASE(49)
          nuc_m_excess = -30.319265      ! K49
       CASE(50)
          nuc_m_excess = -25.352141      ! K50
       CASE(51)
          nuc_m_excess = -22.002      ! K51
       CASE(52)
          nuc_m_excess = -16.199      ! K52
       CASE(53)
          nuc_m_excess = -11.998      ! K53
       CASE(54)
          nuc_m_excess = -5.403      ! K54
       CASE(55)
          nuc_m_excess = -0.27      ! K55
       END SELECT
    CASE (20)
       SELECT CASE (a)
       CASE(34)
          nuc_m_excess = 13.153      ! Ca34
       CASE(35)
          nuc_m_excess = 4.602      ! Ca35
       CASE(36)
          nuc_m_excess = -6.439359      ! Ca36
       CASE(37)
          nuc_m_excess = -13.16176      ! Ca37
       CASE(38)
          nuc_m_excess = -22.05922      ! Ca38
       CASE(39)
          nuc_m_excess = -27.2744      ! Ca39
       CASE(40)
          nuc_m_excess = -34.846275      ! Ca40
       CASE(41)
          nuc_m_excess = -35.137759      ! Ca41
       CASE(42)
          nuc_m_excess = -38.547072      ! Ca42
       CASE(43)
          nuc_m_excess = -38.408639      ! Ca43
       CASE(44)
          nuc_m_excess = -41.468479      ! Ca44
       CASE(45)
          nuc_m_excess = -40.81195      ! Ca45
       CASE(46)
          nuc_m_excess = -43.135077      ! Ca46
       CASE(47)
          nuc_m_excess = -42.340123      ! Ca47
       CASE(48)
          nuc_m_excess = -44.214129      ! Ca48
       CASE(49)
          nuc_m_excess = -41.289265      ! Ca49
       CASE(50)
          nuc_m_excess = -39.570832      ! Ca50
       CASE(51)
          nuc_m_excess = -35.863251      ! Ca51
       CASE(52)
          nuc_m_excess = -32.509141      ! Ca52
       CASE(53)
          nuc_m_excess = -27.898      ! Ca53
       CASE(54)
          nuc_m_excess = -23.893      ! Ca54
       CASE(55)
          nuc_m_excess = -18.118      ! Ca55
       CASE(56)
          nuc_m_excess = -13.441      ! Ca56
       CASE(57)
          nuc_m_excess = -7.12      ! Ca57
       END SELECT
    CASE (21)
       SELECT CASE (a)
       CASE(36)
          nuc_m_excess = 13.898      ! Sc36
       CASE(37)
          nuc_m_excess = 2.841      ! Sc37
       CASE(38)
          nuc_m_excess = -4.937      ! Sc38
       CASE(39)
          nuc_m_excess = -14.168021      ! Sc39
       CASE(40)
          nuc_m_excess = -20.523228      ! Sc40
       CASE(41)
          nuc_m_excess = -28.642392      ! Sc41
       CASE(42)
          nuc_m_excess = -32.121239      ! Sc42
       CASE(43)
          nuc_m_excess = -36.187929      ! Sc43
       CASE(44)
          nuc_m_excess = -37.816093      ! Sc44
       CASE(45)
          nuc_m_excess = -41.067792      ! Sc45
       CASE(46)
          nuc_m_excess = -41.757115      ! Sc46
       CASE(47)
          nuc_m_excess = -44.332121      ! Sc47
       CASE(48)
          nuc_m_excess = -44.496101      ! Sc48
       CASE(49)
          nuc_m_excess = -46.552368      ! Sc49
       CASE(50)
          nuc_m_excess = -44.536885      ! Sc50
       CASE(51)
          nuc_m_excess = -43.218184      ! Sc51
       CASE(52)
          nuc_m_excess = -40.356541      ! Sc52
       CASE(53)
          nuc_m_excess = -37.623      ! Sc53
       CASE(54)
          nuc_m_excess = -34.218841      ! Sc54
       CASE(55)
          nuc_m_excess = -29.580571      ! Sc55
       CASE(56)
          nuc_m_excess = -25.271      ! Sc56
       CASE(57)
          nuc_m_excess = -20.688      ! Sc57
       CASE(58)
          nuc_m_excess = -15.174      ! Sc58
       CASE(59)
          nuc_m_excess = -10.042      ! Sc59
       CASE(60)
          nuc_m_excess = -3.996      ! Sc60
       END SELECT
    CASE (22)
       SELECT CASE (a)
       CASE(38)
          nuc_m_excess = 9.101      ! Ti38
       CASE(39)
          nuc_m_excess = 1.5      ! Ti39
       CASE(40)
          nuc_m_excess = -8.850275      ! Ti40
       CASE(41)
          nuc_m_excess = -15.7      ! Ti41
       CASE(42)
          nuc_m_excess = -25.121552      ! Ti42
       CASE(43)
          nuc_m_excess = -29.321103      ! Ti43
       CASE(44)
          nuc_m_excess = -37.548459      ! Ti44
       CASE(45)
          nuc_m_excess = -39.005737      ! Ti45
       CASE(46)
          nuc_m_excess = -44.123422      ! Ti46
       CASE(47)
          nuc_m_excess = -44.932394      ! Ti47
       CASE(48)
          nuc_m_excess = -48.487727      ! Ti48
       CASE(49)
          nuc_m_excess = -48.558799      ! Ti49
       CASE(50)
          nuc_m_excess = -51.426672      ! Ti50
       CASE(51)
          nuc_m_excess = -49.727849      ! Ti51
       CASE(52)
          nuc_m_excess = -49.464837      ! Ti52
       CASE(53)
          nuc_m_excess = -46.828839      ! Ti53
       CASE(54)
          nuc_m_excess = -45.594395      ! Ti54
       CASE(55)
          nuc_m_excess = -41.670332      ! Ti55
       CASE(56)
          nuc_m_excess = -38.936785      ! Ti56
       CASE(57)
          nuc_m_excess = -33.543903      ! Ti57
       CASE(58)
          nuc_m_excess = -30.767      ! Ti58
       CASE(59)
          nuc_m_excess = -25.216      ! Ti59
       CASE(60)
          nuc_m_excess = -21.648      ! Ti60
       CASE(61)
          nuc_m_excess = -15.649      ! Ti61
       CASE(62)
          nuc_m_excess = -11.653      ! Ti62
       CASE(63)
          nuc_m_excess = -5.198      ! Ti63
       END SELECT
    CASE (23)
       SELECT CASE (a)
       CASE(40)
          nuc_m_excess = 10.33      ! V40
       CASE(41)
          nuc_m_excess = -0.205      ! V41
       CASE(42)
          nuc_m_excess = -8.169      ! V42
       CASE(43)
          nuc_m_excess = -18.024      ! V43
       CASE(44)
          nuc_m_excess = -24.11638      ! V44
       CASE(45)
          nuc_m_excess = -31.879629      ! V45
       CASE(46)
          nuc_m_excess = -37.073013      ! V46
       CASE(47)
          nuc_m_excess = -42.002051      ! V47
       CASE(48)
          nuc_m_excess = -44.475385      ! V48
       CASE(49)
          nuc_m_excess = -47.956943      ! V49
       CASE(50)
          nuc_m_excess = -49.221554      ! V50
       CASE(51)
          nuc_m_excess = -52.201383      ! V51
       CASE(52)
          nuc_m_excess = -51.44131      ! V52
       CASE(53)
          nuc_m_excess = -51.848839      ! V53
       CASE(54)
          nuc_m_excess = -49.890954      ! V54
       CASE(55)
          nuc_m_excess = -49.151491      ! V55
       CASE(56)
          nuc_m_excess = -46.080109      ! V56
       CASE(57)
          nuc_m_excess = -44.188742      ! V57
       CASE(58)
          nuc_m_excess = -40.208743      ! V58
       CASE(59)
          nuc_m_excess = -37.066562      ! V59
       CASE(60)
          nuc_m_excess = -32.577268      ! V60
       CASE(61)
          nuc_m_excess = -29.361      ! V61
       CASE(62)
          nuc_m_excess = -24.424      ! V62
       CASE(63)
          nuc_m_excess = -20.912      ! V63
       CASE(64)
          nuc_m_excess = -15.398      ! V64
       CASE(65)
          nuc_m_excess = -11.252      ! V65
       END SELECT
    CASE (24)
       SELECT CASE (a)
       CASE(42)
          nuc_m_excess = 5.99      ! Cr42
       CASE(43)
          nuc_m_excess = -2.133      ! Cr43
       CASE(44)
          nuc_m_excess = -13.461      ! Cr44
       CASE(45)
          nuc_m_excess = -18.965218      ! Cr45
       CASE(46)
          nuc_m_excess = -29.473742      ! Cr46
       CASE(47)
          nuc_m_excess = -34.558385      ! Cr47
       CASE(48)
          nuc_m_excess = -42.81918      ! Cr48
       CASE(49)
          nuc_m_excess = -45.330484      ! Cr49
       CASE(50)
          nuc_m_excess = -50.259499      ! Cr50
       CASE(51)
          nuc_m_excess = -51.448807      ! Cr51
       CASE(52)
          nuc_m_excess = -55.416933      ! Cr52
       CASE(53)
          nuc_m_excess = -55.284741      ! Cr53
       CASE(54)
          nuc_m_excess = -56.932545      ! Cr54
       CASE(55)
          nuc_m_excess = -55.107491      ! Cr55
       CASE(56)
          nuc_m_excess = -55.281245      ! Cr56
       CASE(57)
          nuc_m_excess = -52.52414      ! Cr57
       CASE(58)
          nuc_m_excess = -51.834726      ! Cr58
       CASE(59)
          nuc_m_excess = -47.89149      ! Cr59
       CASE(60)
          nuc_m_excess = -46.503876      ! Cr60
       CASE(61)
          nuc_m_excess = -42.180653      ! Cr61
       CASE(62)
          nuc_m_excess = -40.414553      ! Cr62
       CASE(63)
          nuc_m_excess = -35.527      ! Cr63
       CASE(64)
          nuc_m_excess = -33.152      ! Cr64
       CASE(65)
          nuc_m_excess = -27.796      ! Cr65
       CASE(66)
          nuc_m_excess = -24.796      ! Cr66
       CASE(67)
          nuc_m_excess = -19.049      ! Cr67
       END SELECT
    CASE (25)
       SELECT CASE (a)
       CASE(44)
          nuc_m_excess = 6.399      ! Mn44
       CASE(45)
          nuc_m_excess = -5.114      ! Mn45
       CASE(46)
          nuc_m_excess = -12.37      ! Mn46
       CASE(47)
          nuc_m_excess = -22.263      ! Mn47
       CASE(48)
          nuc_m_excess = -29.323431      ! Mn48
       CASE(49)
          nuc_m_excess = -37.615586      ! Mn49
       CASE(50)
          nuc_m_excess = -42.626814      ! Mn50
       CASE(51)
          nuc_m_excess = -48.241341      ! Mn51
       CASE(52)
          nuc_m_excess = -50.705444      ! Mn52
       CASE(53)
          nuc_m_excess = -54.687904      ! Mn53
       CASE(54)
          nuc_m_excess = -55.55537      ! Mn54
       CASE(55)
          nuc_m_excess = -57.71058      ! Mn55
       CASE(56)
          nuc_m_excess = -56.90971      ! Mn56
       CASE(57)
          nuc_m_excess = -57.4868      ! Mn57
       CASE(58)
          nuc_m_excess = -55.906827      ! Mn58
       CASE(59)
          nuc_m_excess = -55.479562      ! Mn59
       CASE(60)
          nuc_m_excess = -53.177832      ! Mn60
       CASE(61)
          nuc_m_excess = -51.555736      ! Mn61
       CASE(62)
          nuc_m_excess = -48.038804      ! Mn62
       CASE(63)
          nuc_m_excess = -46.351151      ! Mn63
       CASE(64)
          nuc_m_excess = -42.616698      ! Mn64
       CASE(65)
          nuc_m_excess = -40.672693      ! Mn65
       CASE(66)
          nuc_m_excess = -36.254      ! Mn66
       CASE(67)
          nuc_m_excess = -33.403      ! Mn67
       CASE(68)
          nuc_m_excess = -28.597      ! Mn68
       CASE(69)
          nuc_m_excess = -25.299      ! Mn69
       END SELECT
    CASE (26)
       SELECT CASE (a)
       CASE(45)
          nuc_m_excess = 13.579      ! Fe45
       CASE(46)
          nuc_m_excess = 0.755      ! Fe46
       CASE(47)
          nuc_m_excess = -6.623      ! Fe47
       CASE(48)
          nuc_m_excess = -18.16      ! Fe48
       CASE(49)
          nuc_m_excess = -24.582      ! Fe49
       CASE(50)
          nuc_m_excess = -34.475541      ! Fe50
       CASE(51)
          nuc_m_excess = -40.222341      ! Fe51
       CASE(52)
          nuc_m_excess = -48.331615      ! Fe52
       CASE(53)
          nuc_m_excess = -50.945323      ! Fe53
       CASE(54)
          nuc_m_excess = -56.252456      ! Fe54
       CASE(55)
          nuc_m_excess = -57.479368      ! Fe55
       CASE(56)
          nuc_m_excess = -60.605352      ! Fe56
       CASE(57)
          nuc_m_excess = -60.18013      ! Fe57
       CASE(58)
          nuc_m_excess = -62.153418      ! Fe58
       CASE(59)
          nuc_m_excess = -60.663114      ! Fe59
       CASE(60)
          nuc_m_excess = -61.411832      ! Fe60
       CASE(61)
          nuc_m_excess = -58.921391      ! Fe61
       CASE(62)
          nuc_m_excess = -58.900749      ! Fe62
       CASE(63)
          nuc_m_excess = -55.545834      ! Fe63
       CASE(64)
          nuc_m_excess = -54.770668      ! Fe64
       CASE(65)
          nuc_m_excess = -50.877951      ! Fe65
       CASE(66)
          nuc_m_excess = -49.573517      ! Fe66
       CASE(67)
          nuc_m_excess = -45.692348      ! Fe67
       CASE(68)
          nuc_m_excess = -43.128173      ! Fe68
       CASE(69)
          nuc_m_excess = -38.396      ! Fe69
       CASE(70)
          nuc_m_excess = -35.9      ! Fe70
       CASE(71)
          nuc_m_excess = -31      ! Fe71
       CASE(72)
          nuc_m_excess = -28.299      ! Fe72
       END SELECT
    CASE (27)
       SELECT CASE (a)
       CASE(47)
          nuc_m_excess = 10.703      ! Co47
       CASE(48)
          nuc_m_excess = 1.639      ! Co48
       CASE(49)
          nuc_m_excess = -9.576      ! Co49
       CASE(50)
          nuc_m_excess = -17.195      ! Co50
       CASE(51)
          nuc_m_excess = -27.274      ! Co51
       CASE(52)
          nuc_m_excess = -33.916      ! Co52
       CASE(53)
          nuc_m_excess = -42.644824      ! Co53
       CASE(54)
          nuc_m_excess = -48.009541      ! Co54
       CASE(55)
          nuc_m_excess = -54.027557      ! Co55
       CASE(56)
          nuc_m_excess = -56.039352      ! Co56
       CASE(57)
          nuc_m_excess = -59.344204      ! Co57
       CASE(58)
          nuc_m_excess = -59.845868      ! Co58
       CASE(59)
          nuc_m_excess = -62.228412      ! Co59
       CASE(60)
          nuc_m_excess = -61.649012      ! Co60
       CASE(61)
          nuc_m_excess = -62.898422      ! Co61
       CASE(62)
          nuc_m_excess = -61.431505      ! Co62
       CASE(63)
          nuc_m_excess = -61.840387      ! Co63
       CASE(64)
          nuc_m_excess = -59.792686      ! Co64
       CASE(65)
          nuc_m_excess = -59.169934      ! Co65
       CASE(66)
          nuc_m_excess = -56.111332      ! Co66
       CASE(67)
          nuc_m_excess = -55.061049      ! Co67
       CASE(68)
          nuc_m_excess = -51.350415      ! Co68
       CASE(69)
          nuc_m_excess = -50.002598      ! Co69
       CASE(70)
          nuc_m_excess = -45.643206      ! Co70
       CASE(71)
          nuc_m_excess = -43.873368      ! Co71
       CASE(72)
          nuc_m_excess = -39.3      ! Co72
       CASE(73)
          nuc_m_excess = -37.036      ! Co73
       CASE(74)
          nuc_m_excess = -32.248      ! Co74
       CASE(75)
          nuc_m_excess = -29.5      ! Co75
       END SELECT
    CASE (28)
       SELECT CASE (a)
       CASE(48)
          nuc_m_excess = 18.397      ! Ni48
       CASE(49)
          nuc_m_excess = 8.998      ! Ni49
       CASE(50)
          nuc_m_excess = -3.791      ! Ni50
       CASE(51)
          nuc_m_excess = -11.439      ! Ni51
       CASE(52)
          nuc_m_excess = -22.654      ! Ni52
       CASE(53)
          nuc_m_excess = -29.37      ! Ni53
       CASE(54)
          nuc_m_excess = -39.210779      ! Ni54
       CASE(55)
          nuc_m_excess = -45.335579      ! Ni55
       CASE(56)
          nuc_m_excess = -53.903674      ! Ni56
       CASE(57)
          nuc_m_excess = -56.081969      ! Ni57
       CASE(58)
          nuc_m_excess = -60.227694      ! Ni58
       CASE(59)
          nuc_m_excess = -61.15565      ! Ni59
       CASE(60)
          nuc_m_excess = -64.472079      ! Ni60
       CASE(61)
          nuc_m_excess = -64.220892      ! Ni61
       CASE(62)
          nuc_m_excess = -66.746096      ! Ni62
       CASE(63)
          nuc_m_excess = -65.512556      ! Ni63
       CASE(64)
          nuc_m_excess = -67.099277      ! Ni64
       CASE(65)
          nuc_m_excess = -65.126052      ! Ni65
       CASE(66)
          nuc_m_excess = -66.006285      ! Ni66
       CASE(67)
          nuc_m_excess = -63.74268      ! Ni67
       CASE(68)
          nuc_m_excess = -63.463815      ! Ni68
       CASE(69)
          nuc_m_excess = -59.978648      ! Ni69
       CASE(70)
          nuc_m_excess = -59.14987      ! Ni70
       CASE(71)
          nuc_m_excess = -55.203797      ! Ni71
       CASE(72)
          nuc_m_excess = -53.940319      ! Ni72
       CASE(73)
          nuc_m_excess = -49.863      ! Ni73
       CASE(74)
          nuc_m_excess = -48.372      ! Ni74
       CASE(75)
          nuc_m_excess = -43.901      ! Ni75
       CASE(76)
          nuc_m_excess = -41.61      ! Ni76
       CASE(77)
          nuc_m_excess = -36.747      ! Ni77
       CASE(78)
          nuc_m_excess = -34.298      ! Ni78
       END SELECT
    CASE (29)
       SELECT CASE (a)
       CASE(52)
          nuc_m_excess = -2.627      ! Cu52
       CASE(53)
          nuc_m_excess = -13.46      ! Cu53
       CASE(54)
          nuc_m_excess = -21.694      ! Cu54
       CASE(55)
          nuc_m_excess = -31.624      ! Cu55
       CASE(56)
          nuc_m_excess = -38.601      ! Cu56
       CASE(57)
          nuc_m_excess = -47.309576      ! Cu57
       CASE(58)
          nuc_m_excess = -51.662055      ! Cu58
       CASE(59)
          nuc_m_excess = -56.357224      ! Cu59
       CASE(60)
          nuc_m_excess = -58.344099      ! Cu60
       CASE(61)
          nuc_m_excess = -61.98364      ! Cu61
       CASE(62)
          nuc_m_excess = -62.797837      ! Cu62
       CASE(63)
          nuc_m_excess = -65.579531      ! Cu63
       CASE(64)
          nuc_m_excess = -65.424243      ! Cu64
       CASE(65)
          nuc_m_excess = -67.263661      ! Cu65
       CASE(66)
          nuc_m_excess = -66.258274      ! Cu66
       CASE(67)
          nuc_m_excess = -67.318779      ! Cu67
       CASE(68)
          nuc_m_excess = -65.567035      ! Cu68
       CASE(69)
          nuc_m_excess = -65.736213      ! Cu69
       CASE(70)
          nuc_m_excess = -62.976127      ! Cu70
       CASE(71)
          nuc_m_excess = -62.711127      ! Cu71
       CASE(72)
          nuc_m_excess = -59.782999      ! Cu72
       CASE(73)
          nuc_m_excess = -58.986595      ! Cu73
       CASE(74)
          nuc_m_excess = -56.006205      ! Cu74
       CASE(75)
          nuc_m_excess = -54.119802      ! Cu75
       CASE(76)
          nuc_m_excess = -50.975985      ! Cu76
       CASE(77)
          nuc_m_excess = -48.577      ! Cu77
       CASE(78)
          nuc_m_excess = -44.749      ! Cu78
       CASE(79)
          nuc_m_excess = -42.327      ! Cu79
       CASE(80)
          nuc_m_excess = -36.449      ! Cu80
       END SELECT
    CASE (30)
       SELECT CASE (a)
       CASE(54)
          nuc_m_excess = -6.567      ! Zn54
       CASE(55)
          nuc_m_excess = -14.923      ! Zn55
       CASE(56)
          nuc_m_excess = -25.728      ! Zn56
       CASE(57)
          nuc_m_excess = -32.8      ! Zn57
       CASE(58)
          nuc_m_excess = -42.297694      ! Zn58
       CASE(59)
          nuc_m_excess = -47.260499      ! Zn59
       CASE(60)
          nuc_m_excess = -54.187768      ! Zn60
       CASE(61)
          nuc_m_excess = -56.34548      ! Zn61
       CASE(62)
          nuc_m_excess = -61.171431      ! Zn62
       CASE(63)
          nuc_m_excess = -62.213025      ! Zn63
       CASE(64)
          nuc_m_excess = -66.003595      ! Zn64
       CASE(65)
          nuc_m_excess = -65.911599      ! Zn65
       CASE(66)
          nuc_m_excess = -68.899427      ! Zn66
       CASE(67)
          nuc_m_excess = -67.880441      ! Zn67
       CASE(68)
          nuc_m_excess = -70.00722      ! Zn68
       CASE(69)
          nuc_m_excess = -68.417973      ! Zn69
       CASE(70)
          nuc_m_excess = -69.564648      ! Zn70
       CASE(71)
          nuc_m_excess = -67.326897      ! Zn71
       CASE(72)
          nuc_m_excess = -68.13138      ! Zn72
       CASE(73)
          nuc_m_excess = -65.410343      ! Zn73
       CASE(74)
          nuc_m_excess = -65.708883      ! Zn74
       CASE(75)
          nuc_m_excess = -62.469023      ! Zn75
       CASE(76)
          nuc_m_excess = -62.13664      ! Zn76
       CASE(77)
          nuc_m_excess = -58.722344      ! Zn77
       CASE(78)
          nuc_m_excess = -57.34257      ! Zn78
       CASE(79)
          nuc_m_excess = -53.42      ! Zn79
       CASE(80)
          nuc_m_excess = -51.844769      ! Zn80
       CASE(81)
          nuc_m_excess = -46.128      ! Zn81
       CASE(82)
          nuc_m_excess = -42.457      ! Zn82
       CASE(83)
          nuc_m_excess = -36.3      ! Zn83
       END SELECT
    CASE (31)
       SELECT CASE (a)
       CASE(56)
          nuc_m_excess = -4.741      ! Ga56
       CASE(57)
          nuc_m_excess = -15.901      ! Ga57
       CASE(58)
          nuc_m_excess = -23.986      ! Ga58
       CASE(59)
          nuc_m_excess = -34.121      ! Ga59
       CASE(60)
          nuc_m_excess = -39.998      ! Ga60
       CASE(61)
          nuc_m_excess = -47.09048      ! Ga61
       CASE(62)
          nuc_m_excess = -52.000431      ! Ga62
       CASE(63)
          nuc_m_excess = -56.547093      ! Ga63
       CASE(64)
          nuc_m_excess = -58.834328      ! Ga64
       CASE(65)
          nuc_m_excess = -62.657173      ! Ga65
       CASE(66)
          nuc_m_excess = -63.724427      ! Ga66
       CASE(67)
          nuc_m_excess = -66.879683      ! Ga67
       CASE(68)
          nuc_m_excess = -67.08612      ! Ga68
       CASE(69)
          nuc_m_excess = -69.327758      ! Ga69
       CASE(70)
          nuc_m_excess = -68.910089      ! Ga70
       CASE(71)
          nuc_m_excess = -70.140242      ! Ga71
       CASE(72)
          nuc_m_excess = -68.58938      ! Ga72
       CASE(73)
          nuc_m_excess = -69.699335      ! Ga73
       CASE(74)
          nuc_m_excess = -68.049585      ! Ga74
       CASE(75)
          nuc_m_excess = -68.46458      ! Ga75
       CASE(76)
          nuc_m_excess = -66.29664      ! Ga76
       CASE(77)
          nuc_m_excess = -65.992344      ! Ga77
       CASE(78)
          nuc_m_excess = -63.70657      ! Ga78
       CASE(79)
          nuc_m_excess = -62.509526      ! Ga79
       CASE(80)
          nuc_m_excess = -59.135169      ! Ga80
       CASE(81)
          nuc_m_excess = -57.983308      ! Ga81
       CASE(82)
          nuc_m_excess = -53.104      ! Ga82
       CASE(83)
          nuc_m_excess = -49.388      ! Ga83
       CASE(84)
          nuc_m_excess = -44.106      ! Ga84
       CASE(85)
          nuc_m_excess = -40.054      ! Ga85
       CASE(86)
          nuc_m_excess = -34.353      ! Ga86
       END SELECT
    CASE (32)
       SELECT CASE (a)
       CASE(58)
          nuc_m_excess = -8.374      ! Ge58
       CASE(59)
          nuc_m_excess = -17      ! Ge59
       CASE(60)
          nuc_m_excess = -27.768      ! Ge60
       CASE(61)
          nuc_m_excess = -33.729      ! Ge61
       CASE(62)
          nuc_m_excess = -42.243      ! Ge62
       CASE(63)
          nuc_m_excess = -46.91      ! Ge63
       CASE(64)
          nuc_m_excess = -54.349881      ! Ge64
       CASE(65)
          nuc_m_excess = -56.414625      ! Ge65
       CASE(66)
          nuc_m_excess = -61.624427      ! Ge66
       CASE(67)
          nuc_m_excess = -62.65781      ! Ge67
       CASE(68)
          nuc_m_excess = -66.979785      ! Ge68
       CASE(69)
          nuc_m_excess = -67.100605      ! Ge69
       CASE(70)
          nuc_m_excess = -70.563111      ! Ge70
       CASE(71)
          nuc_m_excess = -69.907736      ! Ge71
       CASE(72)
          nuc_m_excess = -72.585911      ! Ge72
       CASE(73)
          nuc_m_excess = -71.297534      ! Ge73
       CASE(74)
          nuc_m_excess = -73.422437      ! Ge74
       CASE(75)
          nuc_m_excess = -71.856427      ! Ge75
       CASE(76)
          nuc_m_excess = -73.213046      ! Ge76
       CASE(77)
          nuc_m_excess = -71.214029      ! Ge77
       CASE(78)
          nuc_m_excess = -71.862211      ! Ge78
       CASE(79)
          nuc_m_excess = -69.488526      ! Ge79
       CASE(80)
          nuc_m_excess = -69.515169      ! Ge80
       CASE(81)
          nuc_m_excess = -66.303308      ! Ge81
       CASE(82)
          nuc_m_excess = -65.624008      ! Ge82
       CASE(83)
          nuc_m_excess = -60.901      ! Ge83
       CASE(84)
          nuc_m_excess = -58.246      ! Ge84
       CASE(85)
          nuc_m_excess = -53.067      ! Ge85
       CASE(86)
          nuc_m_excess = -49.844      ! Ge86
       CASE(87)
          nuc_m_excess = -44.237      ! Ge87
       CASE(88)
          nuc_m_excess = -40.138      ! Ge88
       CASE(89)
          nuc_m_excess = -33.692      ! Ge89
       END SELECT
    CASE (33)
       SELECT CASE (a)
       CASE(60)
          nuc_m_excess = -6.399      ! As60
       CASE(61)
          nuc_m_excess = -18.052      ! As61
       CASE(62)
          nuc_m_excess = -24.964      ! As62
       CASE(63)
          nuc_m_excess = -33.823      ! As63
       CASE(64)
          nuc_m_excess = -39.521      ! As64
       CASE(65)
          nuc_m_excess = -46.981      ! As65
       CASE(66)
          nuc_m_excess = -51.502304      ! As66
       CASE(67)
          nuc_m_excess = -56.64781      ! As67
       CASE(68)
          nuc_m_excess = -58.899233      ! As68
       CASE(69)
          nuc_m_excess = -63.086666      ! As69
       CASE(70)
          nuc_m_excess = -64.343111      ! As70
       CASE(71)
          nuc_m_excess = -67.894336      ! As71
       CASE(72)
          nuc_m_excess = -68.229809      ! As72
       CASE(73)
          nuc_m_excess = -70.956701      ! As73
       CASE(74)
          nuc_m_excess = -70.859967      ! As74
       CASE(75)
          nuc_m_excess = -73.03241      ! As75
       CASE(76)
          nuc_m_excess = -72.289504      ! As76
       CASE(77)
          nuc_m_excess = -73.916577      ! As77
       CASE(78)
          nuc_m_excess = -72.817419      ! As78
       CASE(79)
          nuc_m_excess = -73.636526      ! As79
       CASE(80)
          nuc_m_excess = -72.159286      ! As80
       CASE(81)
          nuc_m_excess = -72.533308      ! As81
       CASE(82)
          nuc_m_excess = -70.324008      ! As82
       CASE(83)
          nuc_m_excess = -69.880657      ! As83
       CASE(84)
          nuc_m_excess = -66.082      ! As84
       CASE(85)
          nuc_m_excess = -63.323      ! As85
       CASE(86)
          nuc_m_excess = -59.15      ! As86
       CASE(87)
          nuc_m_excess = -55.983      ! As87
       CASE(88)
          nuc_m_excess = -51.288      ! As88
       CASE(89)
          nuc_m_excess = -47.143      ! As89
       CASE(90)
          nuc_m_excess = -41.451      ! As90
       CASE(91)
          nuc_m_excess = -36.859      ! As91
       CASE(92)
          nuc_m_excess = -30.926      ! As92
       END SELECT
    CASE (34)
       SELECT CASE (a)
       CASE(65)
          nuc_m_excess = -32.919      ! Se65
       CASE(66)
          nuc_m_excess = -41.722      ! Se66
       CASE(67)
          nuc_m_excess = -46.491      ! Se67
       CASE(68)
          nuc_m_excess = -54.214814      ! Se68
       CASE(69)
          nuc_m_excess = -56.301531      ! Se69
       CASE(70)
          nuc_m_excess = -62.046216      ! Se70
       CASE(71)
          nuc_m_excess = -63.116336      ! Se71
       CASE(72)
          nuc_m_excess = -67.894407      ! Se72
       CASE(73)
          nuc_m_excess = -68.217642      ! Se73
       CASE(74)
          nuc_m_excess = -72.212735      ! Se74
       CASE(75)
          nuc_m_excess = -72.169018      ! Se75
       CASE(76)
          nuc_m_excess = -75.25205      ! Se76
       CASE(77)
          nuc_m_excess = -74.599594      ! Se77
       CASE(78)
          nuc_m_excess = -77.026086      ! Se78
       CASE(79)
          nuc_m_excess = -75.917602      ! Se79
       CASE(80)
          nuc_m_excess = -77.759936      ! Se80
       CASE(81)
          nuc_m_excess = -76.389519      ! Se81
       CASE(82)
          nuc_m_excess = -77.594008      ! Se82
       CASE(83)
          nuc_m_excess = -75.340657      ! Se83
       CASE(84)
          nuc_m_excess = -75.951829      ! Se84
       CASE(85)
          nuc_m_excess = -72.428267      ! Se85
       CASE(86)
          nuc_m_excess = -70.54057      ! Se86
       CASE(87)
          nuc_m_excess = -66.581926      ! Se87
       CASE(88)
          nuc_m_excess = -63.878135      ! Se88
       CASE(89)
          nuc_m_excess = -59.196      ! Se89
       CASE(90)
          nuc_m_excess = -55.927      ! Se90
       CASE(91)
          nuc_m_excess = -50.338      ! Se91
       CASE(92)
          nuc_m_excess = -46.649      ! Se92
       CASE(93)
          nuc_m_excess = -40.716      ! Se93
       CASE(94)
          nuc_m_excess = -36.803      ! Se94
       END SELECT
    CASE (35)
       SELECT CASE (a)
       CASE(67)
          nuc_m_excess = -32.798      ! Br67
       CASE(68)
          nuc_m_excess = -38.642      ! Br68
       CASE(69)
          nuc_m_excess = -46.476      ! Br69
       CASE(70)
          nuc_m_excess = -51.426      ! Br70
       CASE(71)
          nuc_m_excess = -57.063323      ! Br71
       CASE(72)
          nuc_m_excess = -59.015201      ! Br72
       CASE(73)
          nuc_m_excess = -63.628936      ! Br73
       CASE(74)
          nuc_m_excess = -65.306081      ! Br74
       CASE(75)
          nuc_m_excess = -69.139018      ! Br75
       CASE(76)
          nuc_m_excess = -70.289169      ! Br76
       CASE(77)
          nuc_m_excess = -73.234914      ! Br77
       CASE(78)
          nuc_m_excess = -73.452302      ! Br78
       CASE(79)
          nuc_m_excess = -76.068514      ! Br79
       CASE(80)
          nuc_m_excess = -75.889472      ! Br80
       CASE(81)
          nuc_m_excess = -77.974839      ! Br81
       CASE(82)
          nuc_m_excess = -77.496465      ! Br82
       CASE(83)
          nuc_m_excess = -79.00893      ! Br83
       CASE(84)
          nuc_m_excess = -77.799335      ! Br84
       CASE(85)
          nuc_m_excess = -78.610267      ! Br85
       CASE(86)
          nuc_m_excess = -75.63957      ! Br86
       CASE(87)
          nuc_m_excess = -73.856926      ! Br87
       CASE(88)
          nuc_m_excess = -70.732135      ! Br88
       CASE(89)
          nuc_m_excess = -68.57162      ! Br89
       CASE(90)
          nuc_m_excess = -64.619846      ! Br90
       CASE(91)
          nuc_m_excess = -61.508323      ! Br91
       CASE(92)
          nuc_m_excess = -56.580144      ! Br92
       CASE(93)
          nuc_m_excess = -53.049      ! Br93
       CASE(94)
          nuc_m_excess = -47.804      ! Br94
       CASE(95)
          nuc_m_excess = -43.901      ! Br95
       CASE(96)
          nuc_m_excess = -38.629      ! Br96
       CASE(97)
          nuc_m_excess = -34.652      ! Br97
       END SELECT
    CASE (36)
       SELECT CASE (a)
       CASE(69)
          nuc_m_excess = -32.435      ! Kr69
       CASE(70)
          nuc_m_excess = -41.676      ! Kr70
       CASE(71)
          nuc_m_excess = -46.923323      ! Kr71
       CASE(72)
          nuc_m_excess = -53.940919      ! Kr72
       CASE(73)
          nuc_m_excess = -56.551751      ! Kr73
       CASE(74)
          nuc_m_excess = -62.331509      ! Kr74
       CASE(75)
          nuc_m_excess = -64.323624      ! Kr75
       CASE(76)
          nuc_m_excess = -69.014318      ! Kr76
       CASE(77)
          nuc_m_excess = -70.169443      ! Kr77
       CASE(78)
          nuc_m_excess = -74.179727      ! Kr78
       CASE(79)
          nuc_m_excess = -74.442736      ! Kr79
       CASE(80)
          nuc_m_excess = -77.892492      ! Kr80
       CASE(81)
          nuc_m_excess = -77.694038      ! Kr81
       CASE(82)
          nuc_m_excess = -80.589508      ! Kr82
       CASE(83)
          nuc_m_excess = -79.981709      ! Kr83
       CASE(84)
          nuc_m_excess = -82.430991      ! Kr84
       CASE(85)
          nuc_m_excess = -81.480267      ! Kr85
       CASE(86)
          nuc_m_excess = -83.26557      ! Kr86
       CASE(87)
          nuc_m_excess = -80.709426      ! Kr87
       CASE(88)
          nuc_m_excess = -79.692135      ! Kr88
       CASE(89)
          nuc_m_excess = -76.72662      ! Kr89
       CASE(90)
          nuc_m_excess = -74.969846      ! Kr90
       CASE(91)
          nuc_m_excess = -71.310323      ! Kr91
       CASE(92)
          nuc_m_excess = -68.785048      ! Kr92
       CASE(93)
          nuc_m_excess = -64.017525      ! Kr93
       CASE(94)
          nuc_m_excess = -61.143      ! Kr94
       CASE(95)
          nuc_m_excess = -56.039      ! Kr95
       CASE(96)
          nuc_m_excess = -53.03      ! Kr96
       CASE(97)
          nuc_m_excess = -47.916      ! Kr97
       CASE(98)
          nuc_m_excess = -44.796      ! Kr98
       CASE(99)
          nuc_m_excess = -39.495      ! Kr99
       CASE(100)
          nuc_m_excess = -36.198      ! Kr100
       END SELECT
    CASE (37)
       SELECT CASE (a)
       CASE(71)
          nuc_m_excess = -32.304      ! Rb71
       CASE(72)
          nuc_m_excess = -38.117      ! Rb72
       CASE(73)
          nuc_m_excess = -46.052      ! Rb73
       CASE(74)
          nuc_m_excess = -51.91705      ! Rb74
       CASE(75)
          nuc_m_excess = -57.221677      ! Rb75
       CASE(76)
          nuc_m_excess = -60.479832      ! Rb76
       CASE(77)
          nuc_m_excess = -64.824531      ! Rb77
       CASE(78)
          nuc_m_excess = -66.936228      ! Rb78
       CASE(79)
          nuc_m_excess = -70.803362      ! Rb79
       CASE(80)
          nuc_m_excess = -72.172854      ! Rb80
       CASE(81)
          nuc_m_excess = -75.454821      ! Rb81
       CASE(82)
          nuc_m_excess = -76.188201      ! Rb82
       CASE(83)
          nuc_m_excess = -79.074805      ! Rb83
       CASE(84)
          nuc_m_excess = -79.750025      ! Rb84
       CASE(85)
          nuc_m_excess = -82.167331      ! Rb85
       CASE(86)
          nuc_m_excess = -82.747017      ! Rb86
       CASE(87)
          nuc_m_excess = -84.597795      ! Rb87
       CASE(88)
          nuc_m_excess = -82.608998      ! Rb88
       CASE(89)
          nuc_m_excess = -81.712502      ! Rb89
       CASE(90)
          nuc_m_excess = -79.361712      ! Rb90
       CASE(91)
          nuc_m_excess = -77.745323      ! Rb91
       CASE(92)
          nuc_m_excess = -74.772048      ! Rb92
       CASE(93)
          nuc_m_excess = -72.617525      ! Rb93
       CASE(94)
          nuc_m_excess = -68.553352      ! Rb94
       CASE(95)
          nuc_m_excess = -65.853935      ! Rb95
       CASE(96)
          nuc_m_excess = -61.224644      ! Rb96
       CASE(97)
          nuc_m_excess = -58.356314      ! Rb97
       CASE(98)
          nuc_m_excess = -54.221644      ! Rb98
       CASE(99)
          nuc_m_excess = -50.87887      ! Rb99
       CASE(100)
          nuc_m_excess = -46.696      ! Rb100
       CASE(101)
          nuc_m_excess = -43.597231      ! Rb101
       CASE(102)
          nuc_m_excess = -38.312      ! Rb102
       END SELECT
    CASE (38)
       SELECT CASE (a)
       CASE(73)
          nuc_m_excess = -31.699      ! Sr73
       CASE(74)
          nuc_m_excess = -40.697      ! Sr74
       CASE(75)
          nuc_m_excess = -46.621677      ! Sr75
       CASE(76)
          nuc_m_excess = -54.243893      ! Sr76
       CASE(77)
          nuc_m_excess = -57.804063      ! Sr77
       CASE(78)
          nuc_m_excess = -63.173924      ! Sr78
       CASE(79)
          nuc_m_excess = -65.476577      ! Sr79
       CASE(80)
          nuc_m_excess = -70.308223      ! Sr80
       CASE(81)
          nuc_m_excess = -71.527705      ! Sr81
       CASE(82)
          nuc_m_excess = -76.008384      ! Sr82
       CASE(83)
          nuc_m_excess = -76.795439      ! Sr83
       CASE(84)
          nuc_m_excess = -80.643837      ! Sr84
       CASE(85)
          nuc_m_excess = -81.102572      ! Sr85
       CASE(86)
          nuc_m_excess = -84.523576      ! Sr86
       CASE(87)
          nuc_m_excess = -84.880413      ! Sr87
       CASE(88)
          nuc_m_excess = -87.92174      ! Sr88
       CASE(89)
          nuc_m_excess = -86.209141      ! Sr89
       CASE(90)
          nuc_m_excess = -85.941604      ! Sr90
       CASE(91)
          nuc_m_excess = -83.645279      ! Sr91
       CASE(92)
          nuc_m_excess = -82.867702      ! Sr92
       CASE(93)
          nuc_m_excess = -80.084606      ! Sr93
       CASE(94)
          nuc_m_excess = -78.84043      ! Sr94
       CASE(95)
          nuc_m_excess = -75.116826      ! Sr95
       CASE(96)
          nuc_m_excess = -72.938959      ! Sr96
       CASE(97)
          nuc_m_excess = -68.788109      ! Sr97
       CASE(98)
          nuc_m_excess = -66.645662      ! Sr98
       CASE(99)
          nuc_m_excess = -62.185677      ! Sr99
       CASE(100)
          nuc_m_excess = -60.219307      ! Sr100
       CASE(101)
          nuc_m_excess = -55.407231      ! Sr101
       CASE(102)
          nuc_m_excess = -53.077471      ! Sr102
       CASE(103)
          nuc_m_excess = -47.553      ! Sr103
       CASE(104)
          nuc_m_excess = -44.404      ! Sr104
       CASE(105)
          nuc_m_excess = -38.582      ! Sr105
       END SELECT
    CASE (39)
       SELECT CASE (a)
       CASE(76)
          nuc_m_excess = -38.704      ! Y76
       CASE(77)
          nuc_m_excess = -46.905      ! Y77
       CASE(78)
          nuc_m_excess = -52.527      ! Y78
       CASE(79)
          nuc_m_excess = -58.356577      ! Y79
       CASE(80)
          nuc_m_excess = -61.217786      ! Y80
       CASE(81)
          nuc_m_excess = -66.017338      ! Y81
       CASE(82)
          nuc_m_excess = -68.192393      ! Y82
       CASE(83)
          nuc_m_excess = -72.326557      ! Y83
       CASE(84)
          nuc_m_excess = -74.157855      ! Y84
       CASE(85)
          nuc_m_excess = -77.842123      ! Y85
       CASE(86)
          nuc_m_excess = -79.283576      ! Y86
       CASE(87)
          nuc_m_excess = -83.018723      ! Y87
       CASE(88)
          nuc_m_excess = -84.29914      ! Y88
       CASE(89)
          nuc_m_excess = -87.701749      ! Y89
       CASE(90)
          nuc_m_excess = -86.487462      ! Y90
       CASE(91)
          nuc_m_excess = -86.345031      ! Y91
       CASE(92)
          nuc_m_excess = -84.813327      ! Y92
       CASE(93)
          nuc_m_excess = -84.22316      ! Y93
       CASE(94)
          nuc_m_excess = -82.348499      ! Y94
       CASE(95)
          nuc_m_excess = -81.207068      ! Y95
       CASE(96)
          nuc_m_excess = -78.346709      ! Y96
       CASE(97)
          nuc_m_excess = -76.257693      ! Y97
       CASE(98)
          nuc_m_excess = -72.46742      ! Y98
       CASE(99)
          nuc_m_excess = -70.200924      ! Y99
       CASE(100)
          nuc_m_excess = -67.294307      ! Y100
       CASE(101)
          nuc_m_excess = -64.912231      ! Y101
       CASE(102)
          nuc_m_excess = -61.892471      ! Y102
       CASE(103)
          nuc_m_excess = -58.936      ! Y103
       CASE(104)
          nuc_m_excess = -54.912      ! Y104
       CASE(105)
          nuc_m_excess = -51.353      ! Y105
       CASE(106)
          nuc_m_excess = -46.77      ! Y106
       CASE(107)
          nuc_m_excess = -42.718      ! Y107
       CASE(108)
          nuc_m_excess = -37.744      ! Y108
       END SELECT
    CASE (40)
       SELECT CASE (a)
       CASE(78)
          nuc_m_excess = -41.703      ! Zr78
       CASE(79)
          nuc_m_excess = -47.357      ! Zr79
       CASE(80)
          nuc_m_excess = -55.517043      ! Zr80
       CASE(81)
          nuc_m_excess = -58.488484      ! Zr81
       CASE(82)
          nuc_m_excess = -64.192      ! Zr82
       CASE(83)
          nuc_m_excess = -66.458557      ! Zr83
       CASE(84)
          nuc_m_excess = -71.492      ! Zr84
       CASE(85)
          nuc_m_excess = -73.149123      ! Zr85
       CASE(86)
          nuc_m_excess = -77.80435      ! Zr86
       CASE(87)
          nuc_m_excess = -79.34815      ! Zr87
       CASE(88)
          nuc_m_excess = -83.623101      ! Zr88
       CASE(89)
          nuc_m_excess = -84.868884      ! Zr89
       CASE(90)
          nuc_m_excess = -88.767265      ! Zr90
       CASE(91)
          nuc_m_excess = -87.890402      ! Zr91
       CASE(92)
          nuc_m_excess = -88.453882      ! Zr92
       CASE(93)
          nuc_m_excess = -87.11704      ! Zr93
       CASE(94)
          nuc_m_excess = -87.266837      ! Zr94
       CASE(95)
          nuc_m_excess = -85.657766      ! Zr95
       CASE(96)
          nuc_m_excess = -85.442791      ! Zr96
       CASE(97)
          nuc_m_excess = -82.946645      ! Zr97
       CASE(98)
          nuc_m_excess = -81.286925      ! Zr98
       CASE(99)
          nuc_m_excess = -77.768473      ! Zr99
       CASE(100)
          nuc_m_excess = -76.604307      ! Zr100
       CASE(101)
          nuc_m_excess = -73.457231      ! Zr101
       CASE(102)
          nuc_m_excess = -71.742471      ! Zr102
       CASE(103)
          nuc_m_excess = -68.372027      ! Zr103
       CASE(104)
          nuc_m_excess = -66.341      ! Zr104
       CASE(105)
          nuc_m_excess = -62.364      ! Zr105
       CASE(106)
          nuc_m_excess = -59.699      ! Zr106
       CASE(107)
          nuc_m_excess = -55.191      ! Zr107
       CASE(108)
          nuc_m_excess = -52.201      ! Zr108
       CASE(109)
          nuc_m_excess = -47.283      ! Zr109
       CASE(110)
          nuc_m_excess = -43.901      ! Zr110
       END SELECT
    CASE (41)
       SELECT CASE (a)
       CASE(81)
          nuc_m_excess = -47.478      ! Nb81
       CASE(82)
          nuc_m_excess = -52.974      ! Nb82
       CASE(83)
          nuc_m_excess = -58.958557      ! Nb83
       CASE(84)
          nuc_m_excess = -61.879      ! Nb84
       CASE(85)
          nuc_m_excess = -67.149123      ! Nb85
       CASE(86)
          nuc_m_excess = -69.82635      ! Nb86
       CASE(87)
          nuc_m_excess = -74.18315      ! Nb87
       CASE(88)
          nuc_m_excess = -76.073101      ! Nb88
       CASE(89)
          nuc_m_excess = -80.650386      ! Nb89
       CASE(90)
          nuc_m_excess = -82.656265      ! Nb90
       CASE(91)
          nuc_m_excess = -86.632442      ! Nb91
       CASE(92)
          nuc_m_excess = -86.448337      ! Nb92
       CASE(93)
          nuc_m_excess = -87.208278      ! Nb93
       CASE(94)
          nuc_m_excess = -86.364502      ! Nb94
       CASE(95)
          nuc_m_excess = -86.781901      ! Nb95
       CASE(96)
          nuc_m_excess = -85.603696      ! Nb96
       CASE(97)
          nuc_m_excess = -85.605644      ! Nb97
       CASE(98)
          nuc_m_excess = -83.528547      ! Nb98
       CASE(99)
          nuc_m_excess = -82.326954      ! Nb99
       CASE(100)
          nuc_m_excess = -79.939307      ! Nb100
       CASE(101)
          nuc_m_excess = -78.942231      ! Nb101
       CASE(102)
          nuc_m_excess = -76.347471      ! Nb102
       CASE(103)
          nuc_m_excess = -75.317027      ! Nb103
       CASE(104)
          nuc_m_excess = -72.223666      ! Nb104
       CASE(105)
          nuc_m_excess = -70.852653      ! Nb105
       CASE(106)
          nuc_m_excess = -67.096      ! Nb106
       CASE(107)
          nuc_m_excess = -64.916      ! Nb107
       CASE(108)
          nuc_m_excess = -60.696      ! Nb108
       CASE(109)
          nuc_m_excess = -58.097      ! Nb109
       CASE(110)
          nuc_m_excess = -53.617      ! Nb110
       CASE(111)
          nuc_m_excess = -50.627      ! Nb111
       CASE(112)
          nuc_m_excess = -45.802      ! Nb112
       CASE(113)
          nuc_m_excess = -42.197      ! Nb113
       END SELECT
    CASE (42)
       SELECT CASE (a)
       CASE(83)
          nuc_m_excess = -47.748      ! Mo83
       CASE(84)
          nuc_m_excess = -55.806      ! Mo84
       CASE(85)
          nuc_m_excess = -59.103      ! Mo85
       CASE(86)
          nuc_m_excess = -64.55635      ! Mo86
       CASE(87)
          nuc_m_excess = -67.694927      ! Mo87
       CASE(88)
          nuc_m_excess = -72.700088      ! Mo88
       CASE(89)
          nuc_m_excess = -75.003889      ! Mo89
       CASE(90)
          nuc_m_excess = -80.167265      ! Mo90
       CASE(91)
          nuc_m_excess = -82.204165      ! Mo91
       CASE(92)
          nuc_m_excess = -86.805003      ! Mo92
       CASE(93)
          nuc_m_excess = -86.803495      ! Mo93
       CASE(94)
          nuc_m_excess = -88.409708      ! Mo94
       CASE(95)
          nuc_m_excess = -87.707492      ! Mo95
       CASE(96)
          nuc_m_excess = -88.790496      ! Mo96
       CASE(97)
          nuc_m_excess = -87.540442      ! Mo97
       CASE(98)
          nuc_m_excess = -88.111723      ! Mo98
       CASE(99)
          nuc_m_excess = -85.96584      ! Mo99
       CASE(100)
          nuc_m_excess = -86.184307      ! Mo100
       CASE(101)
          nuc_m_excess = -83.511231      ! Mo101
       CASE(102)
          nuc_m_excess = -83.557471      ! Mo102
       CASE(103)
          nuc_m_excess = -80.847027      ! Mo103
       CASE(104)
          nuc_m_excess = -80.328666      ! Mo104
       CASE(105)
          nuc_m_excess = -77.337653      ! Mo105
       CASE(106)
          nuc_m_excess = -76.255078      ! Mo106
       CASE(107)
          nuc_m_excess = -72.942869      ! Mo107
       CASE(108)
          nuc_m_excess = -71.303      ! Mo108
       CASE(109)
          nuc_m_excess = -67.245      ! Mo109
       CASE(110)
          nuc_m_excess = -65.456      ! Mo110
       CASE(111)
          nuc_m_excess = -61.097      ! Mo111
       CASE(112)
          nuc_m_excess = -58.833      ! Mo112
       CASE(113)
          nuc_m_excess = -54.138      ! Mo113
       CASE(114)
          nuc_m_excess = -51.307      ! Mo114
       CASE(115)
          nuc_m_excess = -46.305      ! Mo115
       END SELECT
    CASE (43)
       SELECT CASE (a)
       CASE(85)
          nuc_m_excess = -47.665      ! Tc85
       CASE(86)
          nuc_m_excess = -53.207      ! Tc86
       CASE(87)
          nuc_m_excess = -59.122      ! Tc87
       CASE(88)
          nuc_m_excess = -62.71      ! Tc88
       CASE(89)
          nuc_m_excess = -67.844      ! Tc89
       CASE(90)
          nuc_m_excess = -71.206603      ! Tc90
       CASE(91)
          nuc_m_excess = -75.984165      ! Tc91
       CASE(92)
          nuc_m_excess = -78.934648      ! Tc92
       CASE(93)
          nuc_m_excess = -83.602533      ! Tc93
       CASE(94)
          nuc_m_excess = -84.153961      ! Tc94
       CASE(95)
          nuc_m_excess = -86.016873      ! Tc95
       CASE(96)
          nuc_m_excess = -85.817254      ! Tc96
       CASE(97)
          nuc_m_excess = -87.220108      ! Tc97
       CASE(98)
          nuc_m_excess = -86.427771      ! Tc98
       CASE(99)
          nuc_m_excess = -87.323141      ! Tc99
       CASE(100)
          nuc_m_excess = -86.016224      ! Tc100
       CASE(101)
          nuc_m_excess = -86.33584      ! Tc101
       CASE(102)
          nuc_m_excess = -84.565665      ! Tc102
       CASE(103)
          nuc_m_excess = -84.597027      ! Tc103
       CASE(104)
          nuc_m_excess = -82.486166      ! Tc104
       CASE(105)
          nuc_m_excess = -82.287653      ! Tc105
       CASE(106)
          nuc_m_excess = -79.775078      ! Tc106
       CASE(107)
          nuc_m_excess = -79.102869      ! Tc107
       CASE(108)
          nuc_m_excess = -75.952879      ! Tc108
       CASE(109)
          nuc_m_excess = -74.535668      ! Tc109
       CASE(110)
          nuc_m_excess = -70.960763      ! Tc110
       CASE(111)
          nuc_m_excess = -69.216683      ! Tc111
       CASE(112)
          nuc_m_excess = -65.999617      ! Tc112
       CASE(113)
          nuc_m_excess = -63.724      ! Tc113
       CASE(114)
          nuc_m_excess = -59.727      ! Tc114
       CASE(115)
          nuc_m_excess = -57.11      ! Tc115
       CASE(116)
          nuc_m_excess = -52.751      ! Tc116
       CASE(117)
          nuc_m_excess = -49.854      ! Tc117
       CASE(118)
          nuc_m_excess = -45.196      ! Tc118
       END SELECT
    CASE (44)
       SELECT CASE (a)
       CASE(87)
          nuc_m_excess = -47.339      ! Ru87
       CASE(88)
          nuc_m_excess = -55.647      ! Ru88
       CASE(89)
          nuc_m_excess = -59.513      ! Ru89
       CASE(90)
          nuc_m_excess = -65.307      ! Ru90
       CASE(91)
          nuc_m_excess = -68.658      ! Ru91
       CASE(92)
          nuc_m_excess = -74.408      ! Ru92
       CASE(93)
          nuc_m_excess = -77.265533      ! Ru93
       CASE(94)
          nuc_m_excess = -82.567898      ! Ru94
       CASE(95)
          nuc_m_excess = -83.449819      ! Ru95
       CASE(96)
          nuc_m_excess = -86.072062      ! Ru96
       CASE(97)
          nuc_m_excess = -86.112242      ! Ru97
       CASE(98)
          nuc_m_excess = -88.224469      ! Ru98
       CASE(99)
          nuc_m_excess = -87.616977      ! Ru99
       CASE(100)
          nuc_m_excess = -89.218984      ! Ru100
       CASE(101)
          nuc_m_excess = -87.94972      ! Ru101
       CASE(102)
          nuc_m_excess = -89.098043      ! Ru102
       CASE(103)
          nuc_m_excess = -87.258775      ! Ru103
       CASE(104)
          nuc_m_excess = -88.088872      ! Ru104
       CASE(105)
          nuc_m_excess = -85.927653      ! Ru105
       CASE(106)
          nuc_m_excess = -86.322078      ! Ru106
       CASE(107)
          nuc_m_excess = -83.922869      ! Ru107
       CASE(108)
          nuc_m_excess = -83.672879      ! Ru108
       CASE(109)
          nuc_m_excess = -80.850668      ! Ru109
       CASE(110)
          nuc_m_excess = -79.981763      ! Ru110
       CASE(111)
          nuc_m_excess = -76.665683      ! Ru111
       CASE(112)
          nuc_m_excess = -75.483617      ! Ru112
       CASE(113)
          nuc_m_excess = -72.202714      ! Ru113
       CASE(114)
          nuc_m_excess = -70.532      ! Ru114
       CASE(115)
          nuc_m_excess = -66.428402      ! Ru115
       CASE(116)
          nuc_m_excess = -64.45      ! Ru116
       CASE(117)
          nuc_m_excess = -60.007      ! Ru117
       CASE(118)
          nuc_m_excess = -57.92      ! Ru118
       CASE(119)
          nuc_m_excess = -53.244      ! Ru119
       CASE(120)
          nuc_m_excess = -50.943      ! Ru120
       END SELECT
    CASE (45)
       SELECT CASE (a)
       CASE(89)
          nuc_m_excess = -47.658      ! Rh89
       CASE(90)
          nuc_m_excess = -53.216      ! Rh90
       CASE(91)
          nuc_m_excess = -59.103      ! Rh91
       CASE(92)
          nuc_m_excess = -63.36      ! Rh92
       CASE(93)
          nuc_m_excess = -69.173      ! Rh93
       CASE(94)
          nuc_m_excess = -72.938      ! Rh94
       CASE(95)
          nuc_m_excess = -78.339819      ! Rh95
       CASE(96)
          nuc_m_excess = -79.679409      ! Rh96
       CASE(97)
          nuc_m_excess = -82.589242      ! Rh97
       CASE(98)
          nuc_m_excess = -83.174815      ! Rh98
       CASE(99)
          nuc_m_excess = -85.574394      ! Rh99
       CASE(100)
          nuc_m_excess = -85.584225      ! Rh100
       CASE(101)
          nuc_m_excess = -87.408021      ! Rh101
       CASE(102)
          nuc_m_excess = -86.775004      ! Rh102
       CASE(103)
          nuc_m_excess = -88.022185      ! Rh103
       CASE(104)
          nuc_m_excess = -86.949825      ! Rh104
       CASE(105)
          nuc_m_excess = -87.845641      ! Rh105
       CASE(106)
          nuc_m_excess = -86.361478      ! Rh106
       CASE(107)
          nuc_m_excess = -86.863285      ! Rh107
       CASE(108)
          nuc_m_excess = -85.019304      ! Rh108
       CASE(109)
          nuc_m_excess = -85.010668      ! Rh109
       CASE(110)
          nuc_m_excess = -82.775901      ! Rh110
       CASE(111)
          nuc_m_excess = -82.357192      ! Rh111
       CASE(112)
          nuc_m_excess = -79.741327      ! Rh112
       CASE(113)
          nuc_m_excess = -78.682714      ! Rh113
       CASE(114)
          nuc_m_excess = -75.631725      ! Rh114
       CASE(115)
          nuc_m_excess = -74.208402      ! Rh115
       CASE(116)
          nuc_m_excess = -70.735792      ! Rh116
       CASE(117)
          nuc_m_excess = -68.949      ! Rh117
       CASE(118)
          nuc_m_excess = -65.139      ! Rh118
       CASE(119)
          nuc_m_excess = -63.239      ! Rh119
       CASE(120)
          nuc_m_excess = -59.234      ! Rh120
       CASE(121)
          nuc_m_excess = -57.082      ! Rh121
       CASE(122)
          nuc_m_excess = -52.9      ! Rh122
       END SELECT
    CASE (46)
       SELECT CASE (a)
       CASE(91)
          nuc_m_excess = -47.403      ! Pd91
       CASE(92)
          nuc_m_excess = -55.498      ! Pd92
       CASE(93)
          nuc_m_excess = -59.699      ! Pd93
       CASE(94)
          nuc_m_excess = -66.35      ! Pd94
       CASE(95)
          nuc_m_excess = -70.151      ! Pd95
       CASE(96)
          nuc_m_excess = -76.229409      ! Pd96
       CASE(97)
          nuc_m_excess = -77.799242      ! Pd97
       CASE(98)
          nuc_m_excess = -81.299957      ! Pd98
       CASE(99)
          nuc_m_excess = -82.187735      ! Pd99
       CASE(100)
          nuc_m_excess = -85.226218      ! Pd100
       CASE(101)
          nuc_m_excess = -85.428021      ! Pd101
       CASE(102)
          nuc_m_excess = -87.925075      ! Pd102
       CASE(103)
          nuc_m_excess = -87.47911      ! Pd103
       CASE(104)
          nuc_m_excess = -89.390045      ! Pd104
       CASE(105)
          nuc_m_excess = -88.412828      ! Pd105
       CASE(106)
          nuc_m_excess = -89.902478      ! Pd106
       CASE(107)
          nuc_m_excess = -88.367594      ! Pd107
       CASE(108)
          nuc_m_excess = -89.524304      ! Pd108
       CASE(109)
          nuc_m_excess = -87.606591      ! Pd109
       CASE(110)
          nuc_m_excess = -88.349175      ! Pd110
       CASE(111)
          nuc_m_excess = -86.004158      ! Pd111
       CASE(112)
          nuc_m_excess = -86.336399      ! Pd112
       CASE(113)
          nuc_m_excess = -83.692028      ! Pd113
       CASE(114)
          nuc_m_excess = -83.496665      ! Pd114
       CASE(115)
          nuc_m_excess = -80.403      ! Pd115
       CASE(116)
          nuc_m_excess = -79.960691      ! Pd116
       CASE(117)
          nuc_m_excess = -76.530301      ! Pd117
       CASE(118)
          nuc_m_excess = -75.465639      ! Pd118
       CASE(119)
          nuc_m_excess = -71.623      ! Pd119
       CASE(120)
          nuc_m_excess = -70.149064      ! Pd120
       CASE(121)
          nuc_m_excess = -66.257      ! Pd121
       CASE(122)
          nuc_m_excess = -64.692      ! Pd122
       CASE(123)
          nuc_m_excess = -60.612      ! Pd123
       CASE(124)
          nuc_m_excess = -58.796      ! Pd124
       END SELECT
    CASE (47)
       SELECT CASE (a)
       CASE(93)
          nuc_m_excess = -46.78      ! Ag93
       CASE(94)
          nuc_m_excess = -53.3      ! Ag94
       CASE(95)
          nuc_m_excess = -60.1      ! Ag95
       CASE(96)
          nuc_m_excess = -64.571      ! Ag96
       CASE(97)
          nuc_m_excess = -70.819242      ! Ag97
       CASE(98)
          nuc_m_excess = -73.060614      ! Ag98
       CASE(99)
          nuc_m_excess = -76.757735      ! Ag99
       CASE(100)
          nuc_m_excess = -78.148384      ! Ag100
       CASE(101)
          nuc_m_excess = -81.224197      ! Ag101
       CASE(102)
          nuc_m_excess = -82.264893      ! Ag102
       CASE(103)
          nuc_m_excess = -84.791366      ! Ag103
       CASE(104)
          nuc_m_excess = -85.111392      ! Ag104
       CASE(105)
          nuc_m_excess = -87.067992      ! Ag105
       CASE(106)
          nuc_m_excess = -86.93734      ! Ag106
       CASE(107)
          nuc_m_excess = -88.401743      ! Ag107
       CASE(108)
          nuc_m_excess = -87.601836      ! Ag108
       CASE(109)
          nuc_m_excess = -88.722669      ! Ag109
       CASE(110)
          nuc_m_excess = -87.460552      ! Ag110
       CASE(111)
          nuc_m_excess = -88.220719      ! Ag111
       CASE(112)
          nuc_m_excess = -86.624458      ! Ag112
       CASE(113)
          nuc_m_excess = -87.032672      ! Ag113
       CASE(114)
          nuc_m_excess = -84.948803      ! Ag114
       CASE(115)
          nuc_m_excess = -84.987      ! Ag115
       CASE(116)
          nuc_m_excess = -82.567691      ! Ag116
       CASE(117)
          nuc_m_excess = -82.265301      ! Ag117
       CASE(118)
          nuc_m_excess = -79.565639      ! Ag118
       CASE(119)
          nuc_m_excess = -78.557492      ! Ag119
       CASE(120)
          nuc_m_excess = -75.649064      ! Ag120
       CASE(121)
          nuc_m_excess = -74.661064      ! Ag121
       CASE(122)
          nuc_m_excess = -71.231      ! Ag122
       CASE(123)
          nuc_m_excess = -69.955      ! Ag123
       CASE(124)
          nuc_m_excess = -66.471      ! Ag124
       CASE(125)
          nuc_m_excess = -64.804      ! Ag125
       CASE(126)
          nuc_m_excess = -61.013      ! Ag126
       CASE(127)
          nuc_m_excess = -58.898      ! Ag127
       CASE(128)
          nuc_m_excess = -54.8      ! Ag128
       CASE(129)
          nuc_m_excess = -52.452      ! Ag129
       CASE(130)
          nuc_m_excess = -46.157      ! Ag130
       END SELECT
    CASE (48)
       SELECT CASE (a)
       CASE(95)
          nuc_m_excess = -46.696      ! Cd95
       CASE(96)
          nuc_m_excess = -56.104      ! Cd96
       CASE(97)
          nuc_m_excess = -60.603      ! Cd97
       CASE(98)
          nuc_m_excess = -67.630614      ! Cd98
       CASE(99)
          nuc_m_excess = -69.853      ! Cd99
       CASE(100)
          nuc_m_excess = -74.249829      ! Cd100
       CASE(101)
          nuc_m_excess = -75.74766      ! Cd101
       CASE(102)
          nuc_m_excess = -79.677893      ! Cd102
       CASE(103)
          nuc_m_excess = -80.649453      ! Cd103
       CASE(104)
          nuc_m_excess = -83.974674      ! Cd104
       CASE(105)
          nuc_m_excess = -84.330103      ! Cd105
       CASE(106)
          nuc_m_excess = -87.132499      ! Cd106
       CASE(107)
          nuc_m_excess = -86.984841      ! Cd107
       CASE(108)
          nuc_m_excess = -89.252325      ! Cd108
       CASE(109)
          nuc_m_excess = -88.508424      ! Cd109
       CASE(110)
          nuc_m_excess = -90.35299      ! Cd110
       CASE(111)
          nuc_m_excess = -89.257519      ! Cd111
       CASE(112)
          nuc_m_excess = -90.580518      ! Cd112
       CASE(113)
          nuc_m_excess = -89.049279      ! Cd113
       CASE(114)
          nuc_m_excess = -90.020941      ! Cd114
       CASE(115)
          nuc_m_excess = -88.090485      ! Cd115
       CASE(116)
          nuc_m_excess = -88.719392      ! Cd116
       CASE(117)
          nuc_m_excess = -86.425301      ! Cd117
       CASE(118)
          nuc_m_excess = -86.708557      ! Cd118
       CASE(119)
          nuc_m_excess = -83.907492      ! Cd119
       CASE(120)
          nuc_m_excess = -83.974064      ! Cd120
       CASE(121)
          nuc_m_excess = -81.061064      ! Cd121
       CASE(122)
          nuc_m_excess = -80.73032      ! Cd122
       CASE(123)
          nuc_m_excess = -77.311209      ! Cd123
       CASE(124)
          nuc_m_excess = -76.710752      ! Cd124
       CASE(125)
          nuc_m_excess = -73.358534      ! Cd125
       CASE(126)
          nuc_m_excess = -72.327416      ! Cd126
       CASE(127)
          nuc_m_excess = -68.5171      ! Cd127
       CASE(128)
          nuc_m_excess = -67.288998      ! Cd128
       CASE(129)
          nuc_m_excess = -63.202      ! Cd129
       CASE(130)
          nuc_m_excess = -61.569949      ! Cd130
       CASE(131)
          nuc_m_excess = -55.266      ! Cd131
       CASE(132)
          nuc_m_excess = -50.72      ! Cd132
       END SELECT
    CASE (49)
       SELECT CASE (a)
       CASE(97)
          nuc_m_excess = -47.003      ! In97
       CASE(98)
          nuc_m_excess = -53.896      ! In98
       CASE(99)
          nuc_m_excess = -61.274      ! In99
       CASE(100)
          nuc_m_excess = -64.169829      ! In100
       CASE(101)
          nuc_m_excess = -68.614      ! In101
       CASE(102)
          nuc_m_excess = -70.709488      ! In102
       CASE(103)
          nuc_m_excess = -74.599453      ! In103
       CASE(104)
          nuc_m_excess = -76.106627      ! In104
       CASE(105)
          nuc_m_excess = -79.481085      ! In105
       CASE(106)
          nuc_m_excess = -80.606451      ! In106
       CASE(107)
          nuc_m_excess = -83.559577      ! In107
       CASE(108)
          nuc_m_excess = -84.115603      ! In108
       CASE(109)
          nuc_m_excess = -86.488746      ! In109
       CASE(110)
          nuc_m_excess = -86.47499      ! In110
       CASE(111)
          nuc_m_excess = -88.395728      ! In111
       CASE(112)
          nuc_m_excess = -87.996067      ! In112
       CASE(113)
          nuc_m_excess = -89.36962      ! In113
       CASE(114)
          nuc_m_excess = -88.572155      ! In114
       CASE(115)
          nuc_m_excess = -89.536616      ! In115
       CASE(116)
          nuc_m_excess = -88.250019      ! In116
       CASE(117)
          nuc_m_excess = -88.945042      ! In117
       CASE(118)
          nuc_m_excess = -87.230346      ! In118
       CASE(119)
          nuc_m_excess = -87.704492      ! In119
       CASE(120)
          nuc_m_excess = -85.735073      ! In120
       CASE(121)
          nuc_m_excess = -85.841064      ! In121
       CASE(122)
          nuc_m_excess = -83.577359      ! In122
       CASE(123)
          nuc_m_excess = -83.426209      ! In123
       CASE(124)
          nuc_m_excess = -80.876752      ! In124
       CASE(125)
          nuc_m_excess = -80.480534      ! In125
       CASE(126)
          nuc_m_excess = -77.813416      ! In126
       CASE(127)
          nuc_m_excess = -76.9851      ! In127
       CASE(128)
          nuc_m_excess = -74.358998      ! In128
       CASE(129)
          nuc_m_excess = -72.938793      ! In129
       CASE(130)
          nuc_m_excess = -69.889949      ! In130
       CASE(131)
          nuc_m_excess = -68.137141      ! In131
       CASE(132)
          nuc_m_excess = -62.419171      ! In132
       CASE(133)
          nuc_m_excess = -57.93      ! In133
       CASE(134)
          nuc_m_excess = -52.024      ! In134
       CASE(135)
          nuc_m_excess = -47.199      ! In135
       END SELECT
    CASE (50)
       SELECT CASE (a)
       CASE(99)
          nuc_m_excess = -47.199      ! Sn99
       CASE(100)
          nuc_m_excess = -56.779829      ! Sn100
       CASE(101)
          nuc_m_excess = -59.56      ! Sn101
       CASE(102)
          nuc_m_excess = -64.929488      ! Sn102
       CASE(103)
          nuc_m_excess = -66.974      ! Sn103
       CASE(104)
          nuc_m_excess = -71.591627      ! Sn104
       CASE(105)
          nuc_m_excess = -73.262528      ! Sn105
       CASE(106)
          nuc_m_excess = -77.425204      ! Sn106
       CASE(107)
          nuc_m_excess = -78.576801      ! Sn107
       CASE(108)
          nuc_m_excess = -82.040983      ! Sn108
       CASE(109)
          nuc_m_excess = -82.639154      ! Sn109
       CASE(110)
          nuc_m_excess = -85.843888      ! Sn110
       CASE(111)
          nuc_m_excess = -85.944797      ! Sn111
       CASE(112)
          nuc_m_excess = -88.661269      ! Sn112
       CASE(113)
          nuc_m_excess = -88.333039      ! Sn113
       CASE(114)
          nuc_m_excess = -90.560901      ! Sn114
       CASE(115)
          nuc_m_excess = -90.035978      ! Sn115
       CASE(116)
          nuc_m_excess = -91.528107      ! Sn116
       CASE(117)
          nuc_m_excess = -90.39995      ! Sn117
       CASE(118)
          nuc_m_excess = -91.65606      ! Sn118
       CASE(119)
          nuc_m_excess = -90.068363      ! Sn119
       CASE(120)
          nuc_m_excess = -91.105073      ! Sn120
       CASE(121)
          nuc_m_excess = -89.204076      ! Sn121
       CASE(122)
          nuc_m_excess = -89.94595      ! Sn122
       CASE(123)
          nuc_m_excess = -87.820474      ! Sn123
       CASE(124)
          nuc_m_excess = -88.236752      ! Sn124
       CASE(125)
          nuc_m_excess = -85.898534      ! Sn125
       CASE(126)
          nuc_m_excess = -86.020416      ! Sn126
       CASE(127)
          nuc_m_excess = -83.4991      ! Sn127
       CASE(128)
          nuc_m_excess = -83.334598      ! Sn128
       CASE(129)
          nuc_m_excess = -80.593793      ! Sn129
       CASE(130)
          nuc_m_excess = -80.138949      ! Sn130
       CASE(131)
          nuc_m_excess = -77.314218      ! Sn131
       CASE(132)
          nuc_m_excess = -76.554171      ! Sn132
       CASE(133)
          nuc_m_excess = -70.952598      ! Sn133
       CASE(134)
          nuc_m_excess = -66.795791      ! Sn134
       CASE(135)
          nuc_m_excess = -60.799      ! Sn135
       CASE(136)
          nuc_m_excess = -56.504      ! Sn136
       CASE(137)
          nuc_m_excess = -50.31      ! Sn137
       END SELECT
    CASE (51)
       SELECT CASE (a)
       CASE(103)
          nuc_m_excess = -56.178      ! Sb103
       CASE(104)
          nuc_m_excess = -59.176      ! Sb104
       CASE(105)
          nuc_m_excess = -63.820056      ! Sb105
       CASE(106)
          nuc_m_excess = -66.33      ! Sb106
       CASE(107)
          nuc_m_excess = -70.654      ! Sb107
       CASE(108)
          nuc_m_excess = -72.507      ! Sb108
       CASE(109)
          nuc_m_excess = -76.259154      ! Sb109
       CASE(110)
          nuc_m_excess = -77.544      ! Sb110
       CASE(111)
          nuc_m_excess = -80.888145      ! Sb111
       CASE(112)
          nuc_m_excess = -81.600729      ! Sb112
       CASE(113)
          nuc_m_excess = -84.419744      ! Sb113
       CASE(114)
          nuc_m_excess = -84.515383      ! Sb114
       CASE(115)
          nuc_m_excess = -87.003403      ! Sb115
       CASE(116)
          nuc_m_excess = -86.821176      ! Sb116
       CASE(117)
          nuc_m_excess = -88.64475      ! Sb117
       CASE(118)
          nuc_m_excess = -87.99942      ! Sb118
       CASE(119)
          nuc_m_excess = -89.477443      ! Sb119
       CASE(120)
          nuc_m_excess = -88.424465      ! Sb120
       CASE(121)
          nuc_m_excess = -89.595111      ! Sb121
       CASE(122)
          nuc_m_excess = -88.330175      ! Sb122
       CASE(123)
          nuc_m_excess = -89.224113      ! Sb123
       CASE(124)
          nuc_m_excess = -87.620291      ! Sb124
       CASE(125)
          nuc_m_excess = -88.255501      ! Sb125
       CASE(126)
          nuc_m_excess = -86.398416      ! Sb126
       CASE(127)
          nuc_m_excess = -86.7001      ! Sb127
       CASE(128)
          nuc_m_excess = -84.608531      ! Sb128
       CASE(129)
          nuc_m_excess = -84.627681      ! Sb129
       CASE(130)
          nuc_m_excess = -82.291605      ! Sb130
       CASE(131)
          nuc_m_excess = -81.987983      ! Sb131
       CASE(132)
          nuc_m_excess = -79.673573      ! Sb132
       CASE(133)
          nuc_m_excess = -78.942598      ! Sb133
       CASE(134)
          nuc_m_excess = -74.165791      ! Sb134
       CASE(135)
          nuc_m_excess = -69.707636      ! Sb135
       CASE(136)
          nuc_m_excess = -64.879      ! Sb136
       CASE(137)
          nuc_m_excess = -60.258      ! Sb137
       CASE(138)
          nuc_m_excess = -55.154      ! Sb138
       CASE(139)
          nuc_m_excess = -50.319      ! Sb139
       END SELECT
    CASE (52)
       SELECT CASE (a)
       CASE(105)
          nuc_m_excess = -52.499      ! Te105
       CASE(106)
          nuc_m_excess = -58.214429      ! Te106
       CASE(107)
          nuc_m_excess = -60.541      ! Te107
       CASE(108)
          nuc_m_excess = -65.721935      ! Te108
       CASE(109)
          nuc_m_excess = -67.612012      ! Te109
       CASE(110)
          nuc_m_excess = -72.27712      ! Te110
       CASE(111)
          nuc_m_excess = -73.484917      ! Te111
       CASE(112)
          nuc_m_excess = -77.301267      ! Te112
       CASE(113)
          nuc_m_excess = -78.34703      ! Te113
       CASE(114)
          nuc_m_excess = -81.88857      ! Te114
       CASE(115)
          nuc_m_excess = -82.062759      ! Te115
       CASE(116)
          nuc_m_excess = -85.268962      ! Te116
       CASE(117)
          nuc_m_excess = -85.096897      ! Te117
       CASE(118)
          nuc_m_excess = -87.721044      ! Te118
       CASE(119)
          nuc_m_excess = -87.184443      ! Te119
       CASE(120)
          nuc_m_excess = -89.404587      ! Te120
       CASE(121)
          nuc_m_excess = -88.551151      ! Te121
       CASE(122)
          nuc_m_excess = -90.314028      ! Te122
       CASE(123)
          nuc_m_excess = -89.171894      ! Te123
       CASE(124)
          nuc_m_excess = -90.524548      ! Te124
       CASE(125)
          nuc_m_excess = -89.022201      ! Te125
       CASE(126)
          nuc_m_excess = -90.064575      ! Te126
       CASE(127)
          nuc_m_excess = -88.2811      ! Te127
       CASE(128)
          nuc_m_excess = -88.992091      ! Te128
       CASE(129)
          nuc_m_excess = -87.003181      ! Te129
       CASE(130)
          nuc_m_excess = -87.35141      ! Te130
       CASE(131)
          nuc_m_excess = -85.209473      ! Te131
       CASE(132)
          nuc_m_excess = -85.182183      ! Te132
       CASE(133)
          nuc_m_excess = -82.944598      ! Te133
       CASE(134)
          nuc_m_excess = -82.55949      ! Te134
       CASE(135)
          nuc_m_excess = -77.827636      ! Te135
       CASE(136)
          nuc_m_excess = -74.42521      ! Te136
       CASE(137)
          nuc_m_excess = -69.561221      ! Te137
       CASE(138)
          nuc_m_excess = -65.931      ! Te138
       CASE(139)
          nuc_m_excess = -60.799      ! Te139
       CASE(140)
          nuc_m_excess = -56.961      ! Te140
       CASE(141)
          nuc_m_excess = -51.558      ! Te141
       CASE(142)
          nuc_m_excess = -47.432      ! Te142
       END SELECT
    CASE (53)
       SELECT CASE (a)
       CASE(108)
          nuc_m_excess = -52.652      ! I108
       CASE(109)
          nuc_m_excess = -57.613447      ! I109
       CASE(110)
          nuc_m_excess = -60.321      ! I110
       CASE(111)
          nuc_m_excess = -64.947      ! I111
       CASE(112)
          nuc_m_excess = -67.096      ! I112
       CASE(113)
          nuc_m_excess = -71.128339      ! I113
       CASE(114)
          nuc_m_excess = -72.796      ! I114
       CASE(115)
          nuc_m_excess = -76.337797      ! I115
       CASE(116)
          nuc_m_excess = -77.49226      ! I116
       CASE(117)
          nuc_m_excess = -80.434508      ! I117
       CASE(118)
          nuc_m_excess = -80.971048      ! I118
       CASE(119)
          nuc_m_excess = -83.76553      ! I119
       CASE(120)
          nuc_m_excess = -83.789587      ! I120
       CASE(121)
          nuc_m_excess = -86.28726      ! I121
       CASE(122)
          nuc_m_excess = -86.080028      ! I122
       CASE(123)
          nuc_m_excess = -87.943313      ! I123
       CASE(124)
          nuc_m_excess = -87.364961      ! I124
       CASE(125)
          nuc_m_excess = -88.836431      ! I125
       CASE(126)
          nuc_m_excess = -87.910536      ! I126
       CASE(127)
          nuc_m_excess = -88.983125      ! I127
       CASE(128)
          nuc_m_excess = -87.737939      ! I128
       CASE(129)
          nuc_m_excess = -88.503367      ! I129
       CASE(130)
          nuc_m_excess = -86.932379      ! I130
       CASE(131)
          nuc_m_excess = -87.444363      ! I131
       CASE(132)
          nuc_m_excess = -85.699888      ! I132
       CASE(133)
          nuc_m_excess = -85.886598      ! I133
       CASE(134)
          nuc_m_excess = -84.07249      ! I134
       CASE(135)
          nuc_m_excess = -83.789636      ! I135
       CASE(136)
          nuc_m_excess = -79.499294      ! I136
       CASE(137)
          nuc_m_excess = -76.50282      ! I137
       CASE(138)
          nuc_m_excess = -72.33089      ! I138
       CASE(139)
          nuc_m_excess = -68.837893      ! I139
       CASE(140)
          nuc_m_excess = -64.273      ! I140
       CASE(141)
          nuc_m_excess = -60.519      ! I141
       CASE(142)
          nuc_m_excess = -55.722      ! I142
       CASE(143)
          nuc_m_excess = -51.642      ! I143
       CASE(144)
          nuc_m_excess = -46.584      ! I144
       END SELECT
    CASE (54)
       SELECT CASE (a)
       CASE(110)
          nuc_m_excess = -51.904646      ! Xe110
       CASE(111)
          nuc_m_excess = -54.397      ! Xe111
       CASE(112)
          nuc_m_excess = -59.966685      ! Xe112
       CASE(113)
          nuc_m_excess = -62.092297      ! Xe113
       CASE(114)
          nuc_m_excess = -67.085913      ! Xe114
       CASE(115)
          nuc_m_excess = -68.656771      ! Xe115
       CASE(116)
          nuc_m_excess = -73.046747      ! Xe116
       CASE(117)
          nuc_m_excess = -74.185361      ! Xe117
       CASE(118)
          nuc_m_excess = -78.079081      ! Xe118
       CASE(119)
          nuc_m_excess = -78.794437      ! Xe119
       CASE(120)
          nuc_m_excess = -82.172448      ! Xe120
       CASE(121)
          nuc_m_excess = -82.472775      ! Xe121
       CASE(122)
          nuc_m_excess = -85.355002      ! Xe122
       CASE(123)
          nuc_m_excess = -85.248552      ! Xe123
       CASE(124)
          nuc_m_excess = -87.660103      ! Xe124
       CASE(125)
          nuc_m_excess = -87.192064      ! Xe125
       CASE(126)
          nuc_m_excess = -89.168536      ! Xe126
       CASE(127)
          nuc_m_excess = -88.320793      ! Xe127
       CASE(128)
          nuc_m_excess = -89.860039      ! Xe128
       CASE(129)
          nuc_m_excess = -88.697386      ! Xe129
       CASE(130)
          nuc_m_excess = -89.881713      ! Xe130
       CASE(131)
          nuc_m_excess = -88.415211      ! Xe131
       CASE(132)
          nuc_m_excess = -89.28048      ! Xe132
       CASE(133)
          nuc_m_excess = -87.643598      ! Xe133
       CASE(134)
          nuc_m_excess = -88.12449      ! Xe134
       CASE(135)
          nuc_m_excess = -86.417033      ! Xe135
       CASE(136)
          nuc_m_excess = -86.425137      ! Xe136
       CASE(137)
          nuc_m_excess = -82.37935      ! Xe137
       CASE(138)
          nuc_m_excess = -80.15089      ! Xe138
       CASE(139)
          nuc_m_excess = -75.643893      ! Xe139
       CASE(140)
          nuc_m_excess = -72.990992      ! Xe140
       CASE(141)
          nuc_m_excess = -68.326902      ! Xe141
       CASE(142)
          nuc_m_excess = -65.475096      ! Xe142
       CASE(143)
          nuc_m_excess = -60.445      ! Xe143
       CASE(144)
          nuc_m_excess = -57.278      ! Xe144
       CASE(145)
          nuc_m_excess = -52.098      ! Xe145
       CASE(146)
          nuc_m_excess = -48.671      ! Xe146
       CASE(147)
          nuc_m_excess = -43.259      ! Xe147
       END SELECT
    CASE (55)
       SELECT CASE (a)
       CASE(112)
          nuc_m_excess = -46.294      ! Cs112
       CASE(113)
          nuc_m_excess = -51.704182      ! Cs113
       CASE(114)
          nuc_m_excess = -54.539      ! Cs114
       CASE(115)
          nuc_m_excess = -59.699      ! Cs115
       CASE(116)
          nuc_m_excess = -62.068      ! Cs116
       CASE(117)
          nuc_m_excess = -66.442814      ! Cs117
       CASE(118)
          nuc_m_excess = -68.409391      ! Cs118
       CASE(119)
          nuc_m_excess = -72.305075      ! Cs119
       CASE(120)
          nuc_m_excess = -73.888663      ! Cs120
       CASE(121)
          nuc_m_excess = -77.100495      ! Cs121
       CASE(122)
          nuc_m_excess = -78.139833      ! Cs122
       CASE(123)
          nuc_m_excess = -81.043671      ! Cs123
       CASE(124)
          nuc_m_excess = -81.731335      ! Cs124
       CASE(125)
          nuc_m_excess = -84.087575      ! Cs125
       CASE(126)
          nuc_m_excess = -84.34494      ! Cs126
       CASE(127)
          nuc_m_excess = -86.24002      ! Cs127
       CASE(128)
          nuc_m_excess = -85.931378      ! Cs128
       CASE(129)
          nuc_m_excess = -87.500424      ! Cs129
       CASE(130)
          nuc_m_excess = -86.900424      ! Cs130
       CASE(131)
          nuc_m_excess = -88.059787      ! Cs131
       CASE(132)
          nuc_m_excess = -87.155926      ! Cs132
       CASE(133)
          nuc_m_excess = -88.070958      ! Cs133
       CASE(134)
          nuc_m_excess = -86.891181      ! Cs134
       CASE(135)
          nuc_m_excess = -87.581853      ! Cs135
       CASE(136)
          nuc_m_excess = -86.338711      ! Cs136
       CASE(137)
          nuc_m_excess = -86.545599      ! Cs137
       CASE(138)
          nuc_m_excess = -82.887407      ! Cs138
       CASE(139)
          nuc_m_excess = -80.700915      ! Cs139
       CASE(140)
          nuc_m_excess = -77.050992      ! Cs140
       CASE(141)
          nuc_m_excess = -74.476902      ! Cs141
       CASE(142)
          nuc_m_excess = -70.515096      ! Cs142
       CASE(143)
          nuc_m_excess = -67.67141      ! Cs143
       CASE(144)
          nuc_m_excess = -63.269947      ! Cs144
       CASE(145)
          nuc_m_excess = -60.056986      ! Cs145
       CASE(146)
          nuc_m_excess = -55.620044      ! Cs146
       CASE(147)
          nuc_m_excess = -52.019275      ! Cs147
       CASE(148)
          nuc_m_excess = -47.302986      ! Cs148
       CASE(149)
          nuc_m_excess = -43.845      ! Cs149
       CASE(150)
          nuc_m_excess = -38.964      ! Cs150
       CASE(151)
          nuc_m_excess = -35.22      ! Cs151
       END SELECT
    CASE (56)
       SELECT CASE (a)
       CASE(114)
          nuc_m_excess = -45.945564      ! Ba114
       CASE(115)
          nuc_m_excess = -49.025      ! Ba115
       CASE(116)
          nuc_m_excess = -54.604      ! Ba116
       CASE(117)
          nuc_m_excess = -57.288      ! Ba117
       CASE(118)
          nuc_m_excess = -62.373      ! Ba118
       CASE(119)
          nuc_m_excess = -64.59011      ! Ba119
       CASE(120)
          nuc_m_excess = -68.888663      ! Ba120
       CASE(121)
          nuc_m_excess = -70.742779      ! Ba121
       CASE(122)
          nuc_m_excess = -74.608944      ! Ba122
       CASE(123)
          nuc_m_excess = -75.654978      ! Ba123
       CASE(124)
          nuc_m_excess = -79.0898      ! Ba124
       CASE(125)
          nuc_m_excess = -79.66797      ! Ba125
       CASE(126)
          nuc_m_excess = -82.669928      ! Ba126
       CASE(127)
          nuc_m_excess = -82.815595      ! Ba127
       CASE(128)
          nuc_m_excess = -85.401514      ! Ba128
       CASE(129)
          nuc_m_excess = -85.064555      ! Ba129
       CASE(130)
          nuc_m_excess = -87.261603      ! Ba130
       CASE(131)
          nuc_m_excess = -86.68379      ! Ba131
       CASE(132)
          nuc_m_excess = -88.434841      ! Ba132
       CASE(133)
          nuc_m_excess = -87.553459      ! Ba133
       CASE(134)
          nuc_m_excess = -88.949868      ! Ba134
       CASE(135)
          nuc_m_excess = -87.850512      ! Ba135
       CASE(136)
          nuc_m_excess = -88.886935      ! Ba136
       CASE(137)
          nuc_m_excess = -87.721227      ! Ba137
       CASE(138)
          nuc_m_excess = -88.261631      ! Ba138
       CASE(139)
          nuc_m_excess = -84.913745      ! Ba139
       CASE(140)
          nuc_m_excess = -83.271368      ! Ba140
       CASE(141)
          nuc_m_excess = -79.725632      ! Ba141
       CASE(142)
          nuc_m_excess = -77.823147      ! Ba142
       CASE(143)
          nuc_m_excess = -73.935736      ! Ba143
       CASE(144)
          nuc_m_excess = -71.768956      ! Ba144
       CASE(145)
          nuc_m_excess = -67.414986      ! Ba145
       CASE(146)
          nuc_m_excess = -65.00005      ! Ba146
       CASE(147)
          nuc_m_excess = -60.598      ! Ba147
       CASE(148)
          nuc_m_excess = -58.013403      ! Ba148
       CASE(149)
          nuc_m_excess = -53.486      ! Ba149
       CASE(150)
          nuc_m_excess = -50.599      ! Ba150
       CASE(151)
          nuc_m_excess = -45.82      ! Ba151
       CASE(152)
          nuc_m_excess = -42.597      ! Ba152
       CASE(153)
          nuc_m_excess = -37.623      ! Ba153
       END SELECT
    CASE (57)
       SELECT CASE (a)
       CASE(117)
          nuc_m_excess = -46.512      ! La117
       CASE(118)
          nuc_m_excess = -49.621      ! La118
       CASE(119)
          nuc_m_excess = -54.967      ! La119
       CASE(120)
          nuc_m_excess = -57.687      ! La120
       CASE(121)
          nuc_m_excess = -62.401      ! La121
       CASE(122)
          nuc_m_excess = -64.543      ! La122
       CASE(123)
          nuc_m_excess = -68.707      ! La123
       CASE(124)
          nuc_m_excess = -70.25861      ! La124
       CASE(125)
          nuc_m_excess = -73.759389      ! La125
       CASE(126)
          nuc_m_excess = -74.973468      ! La126
       CASE(127)
          nuc_m_excess = -77.895769      ! La127
       CASE(128)
          nuc_m_excess = -78.631901      ! La128
       CASE(129)
          nuc_m_excess = -81.32612      ! La129
       CASE(130)
          nuc_m_excess = -81.628008      ! La130
       CASE(131)
          nuc_m_excess = -83.769256      ! La131
       CASE(132)
          nuc_m_excess = -83.740245      ! La132
       CASE(133)
          nuc_m_excess = -85.494383      ! La133
       CASE(134)
          nuc_m_excess = -85.21865      ! La134
       CASE(135)
          nuc_m_excess = -86.650512      ! La135
       CASE(136)
          nuc_m_excess = -86.036945      ! La136
       CASE(137)
          nuc_m_excess = -87.100653      ! La137
       CASE(138)
          nuc_m_excess = -86.524681      ! La138
       CASE(139)
          nuc_m_excess = -87.231371      ! La139
       CASE(140)
          nuc_m_excess = -84.321031      ! La140
       CASE(141)
          nuc_m_excess = -82.938221      ! La141
       CASE(142)
          nuc_m_excess = -80.034775      ! La142
       CASE(143)
          nuc_m_excess = -78.187073      ! La143
       CASE(144)
          nuc_m_excess = -74.892447      ! La144
       CASE(145)
          nuc_m_excess = -72.986839      ! La145
       CASE(146)
          nuc_m_excess = -69.122947      ! La146
       CASE(147)
          nuc_m_excess = -66.848403      ! La147
       CASE(148)
          nuc_m_excess = -63.128403      ! La148
       CASE(149)
          nuc_m_excess = -60.795      ! La149
       CASE(150)
          nuc_m_excess = -57.035      ! La150
       CASE(151)
          nuc_m_excess = -54.287      ! La151
       CASE(152)
          nuc_m_excess = -50.068      ! La152
       CASE(153)
          nuc_m_excess = -46.929      ! La153
       CASE(154)
          nuc_m_excess = -42.383      ! La154
       CASE(155)
          nuc_m_excess = -38.797      ! La155
       END SELECT
    CASE (58)
       SELECT CASE (a)
       CASE(119)
          nuc_m_excess = -44.004      ! Ce119
       CASE(120)
          nuc_m_excess = -49.705      ! Ce120
       CASE(121)
          nuc_m_excess = -52.704      ! Ce121
       CASE(122)
          nuc_m_excess = -57.836      ! Ce122
       CASE(123)
          nuc_m_excess = -60.175      ! Ce123
       CASE(124)
          nuc_m_excess = -64.823      ! Ce124
       CASE(125)
          nuc_m_excess = -66.658      ! Ce125
       CASE(126)
          nuc_m_excess = -70.820558      ! Ce126
       CASE(127)
          nuc_m_excess = -71.975611      ! Ce127
       CASE(128)
          nuc_m_excess = -75.533918      ! Ce128
       CASE(129)
          nuc_m_excess = -76.287496      ! Ce129
       CASE(130)
          nuc_m_excess = -79.422905      ! Ce130
       CASE(131)
          nuc_m_excess = -79.715394      ! Ce131
       CASE(132)
          nuc_m_excess = -82.474025      ! Ce132
       CASE(133)
          nuc_m_excess = -82.423228      ! Ce133
       CASE(134)
          nuc_m_excess = -84.835983      ! Ce134
       CASE(135)
          nuc_m_excess = -84.62493      ! Ce135
       CASE(136)
          nuc_m_excess = -86.468332      ! Ce136
       CASE(137)
          nuc_m_excess = -85.878553      ! Ce137
       CASE(138)
          nuc_m_excess = -87.568521      ! Ce138
       CASE(139)
          nuc_m_excess = -86.952496      ! Ce139
       CASE(140)
          nuc_m_excess = -88.083278      ! Ce140
       CASE(141)
          nuc_m_excess = -85.440105      ! Ce141
       CASE(142)
          nuc_m_excess = -84.538479      ! Ce142
       CASE(143)
          nuc_m_excess = -81.611999      ! Ce143
       CASE(144)
          nuc_m_excess = -80.436989      ! Ce144
       CASE(145)
          nuc_m_excess = -77.096839      ! Ce145
       CASE(146)
          nuc_m_excess = -75.675496      ! Ce146
       CASE(147)
          nuc_m_excess = -72.028748      ! Ce147
       CASE(148)
          nuc_m_excess = -70.390756      ! Ce148
       CASE(149)
          nuc_m_excess = -66.69508      ! Ce149
       CASE(150)
          nuc_m_excess = -64.823663      ! Ce150
       CASE(151)
          nuc_m_excess = -61.500777      ! Ce151
       CASE(152)
          nuc_m_excess = -59.113      ! Ce152
       CASE(153)
          nuc_m_excess = -55.349      ! Ce153
       CASE(154)
          nuc_m_excess = -52.704      ! Ce154
       CASE(155)
          nuc_m_excess = -48.4      ! Ce155
       CASE(156)
          nuc_m_excess = -45.401      ! Ce156
       CASE(157)
          nuc_m_excess = -40.669      ! Ce157
       END SELECT
    CASE (59)
       SELECT CASE (a)
       CASE(121)
          nuc_m_excess = -41.579      ! Pr121
       CASE(122)
          nuc_m_excess = -44.889      ! Pr122
       CASE(123)
          nuc_m_excess = -50.338      ! Pr123
       CASE(124)
          nuc_m_excess = -53.132      ! Pr124
       CASE(125)
          nuc_m_excess = -57.911      ! Pr125
       CASE(126)
          nuc_m_excess = -60.258      ! Pr126
       CASE(127)
          nuc_m_excess = -64.431      ! Pr127
       CASE(128)
          nuc_m_excess = -66.330757      ! Pr128
       CASE(129)
          nuc_m_excess = -69.773559      ! Pr129
       CASE(130)
          nuc_m_excess = -71.175457      ! Pr130
       CASE(131)
          nuc_m_excess = -74.278264      ! Pr131
       CASE(132)
          nuc_m_excess = -75.213484      ! Pr132
       CASE(133)
          nuc_m_excess = -77.937607      ! Pr133
       CASE(134)
          nuc_m_excess = -78.514011      ! Pr134
       CASE(135)
          nuc_m_excess = -80.935888      ! Pr135
       CASE(136)
          nuc_m_excess = -81.327241      ! Pr136
       CASE(137)
          nuc_m_excess = -83.177333      ! Pr137
       CASE(138)
          nuc_m_excess = -83.131521      ! Pr138
       CASE(139)
          nuc_m_excess = -84.823335      ! Pr139
       CASE(140)
          nuc_m_excess = -84.695278      ! Pr140
       CASE(141)
          nuc_m_excess = -86.020892      ! Pr141
       CASE(142)
          nuc_m_excess = -83.792724      ! Pr142
       CASE(143)
          nuc_m_excess = -83.073499      ! Pr143
       CASE(144)
          nuc_m_excess = -80.755645      ! Pr144
       CASE(145)
          nuc_m_excess = -79.631839      ! Pr145
       CASE(146)
          nuc_m_excess = -76.713808      ! Pr146
       CASE(147)
          nuc_m_excess = -75.454748      ! Pr147
       CASE(148)
          nuc_m_excess = -72.530756      ! Pr148
       CASE(149)
          nuc_m_excess = -71.05655      ! Pr149
       CASE(150)
          nuc_m_excess = -68.303663      ! Pr150
       CASE(151)
          nuc_m_excess = -66.770777      ! Pr151
       CASE(152)
          nuc_m_excess = -63.808061      ! Pr152
       CASE(153)
          nuc_m_excess = -61.628663      ! Pr153
       CASE(154)
          nuc_m_excess = -58.201466      ! Pr154
       CASE(155)
          nuc_m_excess = -55.778      ! Pr155
       CASE(156)
          nuc_m_excess = -51.912      ! Pr156
       CASE(157)
          nuc_m_excess = -48.969      ! Pr157
       CASE(158)
          nuc_m_excess = -44.73      ! Pr158
       CASE(159)
          nuc_m_excess = -41.451      ! Pr159
       END SELECT
    CASE (60)
       SELECT CASE (a)
       CASE(124)
          nuc_m_excess = -44.497      ! Nd124
       CASE(125)
          nuc_m_excess = -47.618      ! Nd125
       CASE(126)
          nuc_m_excess = -52.89      ! Nd126
       CASE(127)
          nuc_m_excess = -55.424      ! Nd127
       CASE(128)
          nuc_m_excess = -60.184      ! Nd128
       CASE(129)
          nuc_m_excess = -62.235      ! Nd129
       CASE(130)
          nuc_m_excess = -66.596233      ! Nd130
       CASE(131)
          nuc_m_excess = -67.768984      ! Nd131
       CASE(132)
          nuc_m_excess = -71.425808      ! Nd132
       CASE(133)
          nuc_m_excess = -72.332373      ! Nd133
       CASE(134)
          nuc_m_excess = -75.646459      ! Nd134
       CASE(135)
          nuc_m_excess = -76.213758      ! Nd135
       CASE(136)
          nuc_m_excess = -79.199314      ! Nd136
       CASE(137)
          nuc_m_excess = -79.580199      ! Nd137
       CASE(138)
          nuc_m_excess = -82.018083      ! Nd138
       CASE(139)
          nuc_m_excess = -81.991697      ! Nd139
       CASE(140)
          nuc_m_excess = -84.25177      ! Nd140
       CASE(141)
          nuc_m_excess = -84.197879      ! Nd141
       CASE(142)
          nuc_m_excess = -85.955195      ! Nd142
       CASE(143)
          nuc_m_excess = -84.007448      ! Nd143
       CASE(144)
          nuc_m_excess = -83.753165      ! Nd144
       CASE(145)
          nuc_m_excess = -81.437134      ! Nd145
       CASE(146)
          nuc_m_excess = -80.93105      ! Nd146
       CASE(147)
          nuc_m_excess = -78.151936      ! Nd147
       CASE(148)
          nuc_m_excess = -77.413404      ! Nd148
       CASE(149)
          nuc_m_excess = -74.380875      ! Nd149
       CASE(150)
          nuc_m_excess = -73.689663      ! Nd150
       CASE(151)
          nuc_m_excess = -70.952896      ! Nd151
       CASE(152)
          nuc_m_excess = -70.158061      ! Nd152
       CASE(153)
          nuc_m_excess = -67.348663      ! Nd153
       CASE(154)
          nuc_m_excess = -65.691466      ! Nd154
       CASE(155)
          nuc_m_excess = -62.473      ! Nd155
       CASE(156)
          nuc_m_excess = -60.530237      ! Nd156
       CASE(157)
          nuc_m_excess = -56.793      ! Nd157
       CASE(158)
          nuc_m_excess = -54.399      ! Nd158
       CASE(159)
          nuc_m_excess = -50.217      ! Nd159
       CASE(160)
          nuc_m_excess = -47.422      ! Nd160
       CASE(161)
          nuc_m_excess = -42.961      ! Nd161
       END SELECT
    CASE (61)
       SELECT CASE (a)
       CASE(126)
          nuc_m_excess = -39.57      ! Pm126
       CASE(127)
          nuc_m_excess = -45.056      ! Pm127
       CASE(128)
          nuc_m_excess = -48.046      ! Pm128
       CASE(129)
          nuc_m_excess = -52.946      ! Pm129
       CASE(130)
          nuc_m_excess = -55.47      ! Pm130
       CASE(131)
          nuc_m_excess = -59.737      ! Pm131
       CASE(132)
          nuc_m_excess = -61.711      ! Pm132
       CASE(133)
          nuc_m_excess = -65.407646      ! Pm133
       CASE(134)
          nuc_m_excess = -66.738751      ! Pm134
       CASE(135)
          nuc_m_excess = -69.977556      ! Pm135
       CASE(136)
          nuc_m_excess = -71.197972      ! Pm136
       CASE(137)
          nuc_m_excess = -74.072875      ! Pm137
       CASE(138)
          nuc_m_excess = -74.940294      ! Pm138
       CASE(139)
          nuc_m_excess = -77.496499      ! Pm139
       CASE(140)
          nuc_m_excess = -78.20657      ! Pm140
       CASE(141)
          nuc_m_excess = -80.522949      ! Pm141
       CASE(142)
          nuc_m_excess = -81.156907      ! Pm142
       CASE(143)
          nuc_m_excess = -82.965734      ! Pm143
       CASE(144)
          nuc_m_excess = -81.421106      ! Pm144
       CASE(145)
          nuc_m_excess = -81.273762      ! Pm145
       CASE(146)
          nuc_m_excess = -79.45988      ! Pm146
       CASE(147)
          nuc_m_excess = -79.047936      ! Pm147
       CASE(148)
          nuc_m_excess = -76.871898      ! Pm148
       CASE(149)
          nuc_m_excess = -76.071245      ! Pm149
       CASE(150)
          nuc_m_excess = -73.603339      ! Pm150
       CASE(151)
          nuc_m_excess = -73.395232      ! Pm151
       CASE(152)
          nuc_m_excess = -71.262276      ! Pm152
       CASE(153)
          nuc_m_excess = -70.684663      ! Pm153
       CASE(154)
          nuc_m_excess = -68.498396      ! Pm154
       CASE(155)
          nuc_m_excess = -66.973239      ! Pm155
       CASE(156)
          nuc_m_excess = -64.220237      ! Pm156
       CASE(157)
          nuc_m_excess = -62.373426      ! Pm157
       CASE(158)
          nuc_m_excess = -59.092669      ! Pm158
       CASE(159)
          nuc_m_excess = -56.849      ! Pm159
       CASE(160)
          nuc_m_excess = -53.104      ! Pm160
       CASE(161)
          nuc_m_excess = -50.431      ! Pm161
       CASE(162)
          nuc_m_excess = -46.305      ! Pm162
       CASE(163)
          nuc_m_excess = -43.147      ! Pm163
       END SELECT
    CASE (62)
       SELECT CASE (a)
       CASE(128)
          nuc_m_excess = -39.048      ! Sm128
       CASE(129)
          nuc_m_excess = -42.253      ! Sm129
       CASE(130)
          nuc_m_excess = -47.581      ! Sm130
       CASE(131)
          nuc_m_excess = -50.198      ! Sm131
       CASE(132)
          nuc_m_excess = -55.247      ! Sm132
       CASE(133)
          nuc_m_excess = -57.129      ! Sm133
       CASE(134)
          nuc_m_excess = -61.507      ! Sm134
       CASE(135)
          nuc_m_excess = -62.857216      ! Sm135
       CASE(136)
          nuc_m_excess = -66.810917      ! Sm136
       CASE(137)
          nuc_m_excess = -68.025381      ! Sm137
       CASE(138)
          nuc_m_excess = -71.49779      ! Sm138
       CASE(139)
          nuc_m_excess = -72.380247      ! Sm139
       CASE(140)
          nuc_m_excess = -75.455963      ! Sm140
       CASE(141)
          nuc_m_excess = -75.938662      ! Sm141
       CASE(142)
          nuc_m_excess = -78.992889      ! Sm142
       CASE(143)
          nuc_m_excess = -79.523191      ! Sm143
       CASE(144)
          nuc_m_excess = -81.971958      ! Sm144
       CASE(145)
          nuc_m_excess = -80.657737      ! Sm145
       CASE(146)
          nuc_m_excess = -81.00188      ! Sm146
       CASE(147)
          nuc_m_excess = -79.272075      ! Sm147
       CASE(148)
          nuc_m_excess = -79.342169      ! Sm148
       CASE(149)
          nuc_m_excess = -77.141922      ! Sm149
       CASE(150)
          nuc_m_excess = -77.057339      ! Sm150
       CASE(151)
          nuc_m_excess = -74.582481      ! Sm151
       CASE(152)
          nuc_m_excess = -74.768765      ! Sm152
       CASE(153)
          nuc_m_excess = -72.565846      ! Sm153
       CASE(154)
          nuc_m_excess = -72.461596      ! Sm154
       CASE(155)
          nuc_m_excess = -70.197239      ! Sm155
       CASE(156)
          nuc_m_excess = -69.370326      ! Sm156
       CASE(157)
          nuc_m_excess = -66.733426      ! Sm157
       CASE(158)
          nuc_m_excess = -65.212669      ! Sm158
       CASE(159)
          nuc_m_excess = -62.213301      ! Sm159
       CASE(160)
          nuc_m_excess = -60.417      ! Sm160
       CASE(161)
          nuc_m_excess = -56.979      ! Sm161
       CASE(162)
          nuc_m_excess = -54.753      ! Sm162
       CASE(163)
          nuc_m_excess = -50.897      ! Sm163
       CASE(164)
          nuc_m_excess = -48.177      ! Sm164
       CASE(165)
          nuc_m_excess = -43.799      ! Sm165
       END SELECT
    CASE (63)
       SELECT CASE (a)
       CASE(130)
          nuc_m_excess = -33.936      ! Eu130
       CASE(131)
          nuc_m_excess = -39.353      ! Eu131
       CASE(132)
          nuc_m_excess = -42.504      ! Eu132
       CASE(133)
          nuc_m_excess = -47.283      ! Eu133
       CASE(134)
          nuc_m_excess = -49.826      ! Eu134
       CASE(135)
          nuc_m_excess = -54.194      ! Eu135
       CASE(136)
          nuc_m_excess = -56.262      ! Eu136
       CASE(137)
          nuc_m_excess = -60.016      ! Eu137
       CASE(138)
          nuc_m_excess = -61.749669      ! Eu138
       CASE(139)
          nuc_m_excess = -65.39807      ! Eu139
       CASE(140)
          nuc_m_excess = -66.985963      ! Eu140
       CASE(141)
          nuc_m_excess = -69.926584      ! Eu141
       CASE(142)
          nuc_m_excess = -71.319889      ! Eu142
       CASE(143)
          nuc_m_excess = -74.242392      ! Eu143
       CASE(144)
          nuc_m_excess = -75.621643      ! Eu144
       CASE(145)
          nuc_m_excess = -77.998429      ! Eu145
       CASE(146)
          nuc_m_excess = -77.122285      ! Eu146
       CASE(147)
          nuc_m_excess = -77.550499      ! Eu147
       CASE(148)
          nuc_m_excess = -76.302498      ! Eu148
       CASE(149)
          nuc_m_excess = -76.44656      ! Eu149
       CASE(150)
          nuc_m_excess = -74.797274      ! Eu150
       CASE(151)
          nuc_m_excess = -74.659094      ! Eu151
       CASE(152)
          nuc_m_excess = -72.894497      ! Eu152
       CASE(153)
          nuc_m_excess = -73.373467      ! Eu153
       CASE(154)
          nuc_m_excess = -71.744378      ! Eu154
       CASE(155)
          nuc_m_excess = -71.824466      ! Eu155
       CASE(156)
          nuc_m_excess = -70.092829      ! Eu156
       CASE(157)
          nuc_m_excess = -69.467426      ! Eu157
       CASE(158)
          nuc_m_excess = -67.211669      ! Eu158
       CASE(159)
          nuc_m_excess = -66.053301      ! Eu159
       CASE(160)
          nuc_m_excess = -63.369      ! Eu160
       CASE(161)
          nuc_m_excess = -61.777      ! Eu161
       CASE(162)
          nuc_m_excess = -58.647      ! Eu162
       CASE(163)
          nuc_m_excess = -56.626      ! Eu163
       CASE(164)
          nuc_m_excess = -53.104      ! Eu164
       CASE(165)
          nuc_m_excess = -50.561      ! Eu165
       CASE(166)
          nuc_m_excess = -46.603      ! Eu166
       CASE(167)
          nuc_m_excess = -43.585      ! Eu167
       END SELECT
    CASE (64)
       SELECT CASE (a)
       CASE(134)
          nuc_m_excess = -41.573      ! Gd134
       CASE(135)
          nuc_m_excess = -44.181      ! Gd135
       CASE(136)
          nuc_m_excess = -49.052      ! Gd136
       CASE(137)
          nuc_m_excess = -51.214      ! Gd137
       CASE(138)
          nuc_m_excess = -55.778      ! Gd138
       CASE(139)
          nuc_m_excess = -57.529      ! Gd139
       CASE(140)
          nuc_m_excess = -61.782272      ! Gd140
       CASE(141)
          nuc_m_excess = -63.224224      ! Gd141
       CASE(142)
          nuc_m_excess = -66.959515      ! Gd142
       CASE(143)
          nuc_m_excess = -68.232392      ! Gd143
       CASE(144)
          nuc_m_excess = -71.759504      ! Gd144
       CASE(145)
          nuc_m_excess = -72.927362      ! Gd145
       CASE(146)
          nuc_m_excess = -76.093179      ! Gd146
       CASE(147)
          nuc_m_excess = -75.363063      ! Gd147
       CASE(148)
          nuc_m_excess = -76.27583      ! Gd148
       CASE(149)
          nuc_m_excess = -75.133454      ! Gd149
       CASE(150)
          nuc_m_excess = -75.768769      ! Gd150
       CASE(151)
          nuc_m_excess = -74.194911      ! Gd151
       CASE(152)
          nuc_m_excess = -74.714206      ! Gd152
       CASE(153)
          nuc_m_excess = -72.889831      ! Gd153
       CASE(154)
          nuc_m_excess = -73.713221      ! Gd154
       CASE(155)
          nuc_m_excess = -72.077123      ! Gd155
       CASE(156)
          nuc_m_excess = -72.542197      ! Gd156
       CASE(157)
          nuc_m_excess = -70.830678      ! Gd157
       CASE(158)
          nuc_m_excess = -70.696751      ! Gd158
       CASE(159)
          nuc_m_excess = -68.568524      ! Gd159
       CASE(160)
          nuc_m_excess = -67.948626      ! Gd160
       CASE(161)
          nuc_m_excess = -65.512709      ! Gd161
       CASE(162)
          nuc_m_excess = -64.28729      ! Gd162
       CASE(163)
          nuc_m_excess = -61.488      ! Gd163
       CASE(164)
          nuc_m_excess = -59.746      ! Gd164
       CASE(165)
          nuc_m_excess = -56.467      ! Gd165
       CASE(166)
          nuc_m_excess = -54.399      ! Gd166
       CASE(167)
          nuc_m_excess = -50.701      ! Gd167
       CASE(168)
          nuc_m_excess = -48.102      ! Gd168
       CASE(169)
          nuc_m_excess = -43.901      ! Gd169
       END SELECT
    CASE (65)
       SELECT CASE (a)
       CASE(136)
          nuc_m_excess = -35.974      ! Tb136
       CASE(137)
          nuc_m_excess = -41.004      ! Tb137
       CASE(138)
          nuc_m_excess = -43.631      ! Tb138
       CASE(139)
          nuc_m_excess = -48.168      ! Tb139
       CASE(140)
          nuc_m_excess = -50.482272      ! Tb140
       CASE(141)
          nuc_m_excess = -54.540837      ! Tb141
       CASE(142)
          nuc_m_excess = -57.06      ! Tb142
       CASE(143)
          nuc_m_excess = -60.4344      ! Tb143
       CASE(144)
          nuc_m_excess = -62.368181      ! Tb144
       CASE(145)
          nuc_m_excess = -65.880845      ! Tb145
       CASE(146)
          nuc_m_excess = -67.76937      ! Tb146
       CASE(147)
          nuc_m_excess = -70.752013      ! Tb147
       CASE(148)
          nuc_m_excess = -70.540457      ! Tb148
       CASE(149)
          nuc_m_excess = -71.495975      ! Tb149
       CASE(150)
          nuc_m_excess = -71.110545      ! Tb150
       CASE(151)
          nuc_m_excess = -71.62952      ! Tb151
       CASE(152)
          nuc_m_excess = -70.724206      ! Tb152
       CASE(153)
          nuc_m_excess = -71.320222      ! Tb153
       CASE(154)
          nuc_m_excess = -70.161974      ! Tb154
       CASE(155)
          nuc_m_excess = -71.254414      ! Tb155
       CASE(156)
          nuc_m_excess = -70.09752      ! Tb156
       CASE(157)
          nuc_m_excess = -70.770626      ! Tb157
       CASE(158)
          nuc_m_excess = -69.477216      ! Tb158
       CASE(159)
          nuc_m_excess = -69.539048      ! Tb159
       CASE(160)
          nuc_m_excess = -67.842938      ! Tb160
       CASE(161)
          nuc_m_excess = -67.468186      ! Tb161
       CASE(162)
          nuc_m_excess = -65.681287      ! Tb162
       CASE(163)
          nuc_m_excess = -64.601404      ! Tb163
       CASE(164)
          nuc_m_excess = -62.083294      ! Tb164
       CASE(165)
          nuc_m_excess = -60.659      ! Tb165
       CASE(166)
          nuc_m_excess = -57.760118      ! Tb166
       CASE(167)
          nuc_m_excess = -55.843      ! Tb167
       CASE(168)
          nuc_m_excess = -52.499      ! Tb168
       CASE(169)
          nuc_m_excess = -50.096      ! Tb169
       CASE(170)
          nuc_m_excess = -46.342      ! Tb170
       CASE(171)
          nuc_m_excess = -43.501      ! Tb171
       END SELECT
    CASE (66)
       SELECT CASE (a)
       CASE(138)
          nuc_m_excess = -34.94      ! Dy138
       CASE(139)
          nuc_m_excess = -37.688      ! Dy139
       CASE(140)
          nuc_m_excess = -42.839      ! Dy140
       CASE(141)
          nuc_m_excess = -45.317      ! Dy141
       CASE(142)
          nuc_m_excess = -49.96      ! Dy142
       CASE(143)
          nuc_m_excess = -52.322      ! Dy143
       CASE(144)
          nuc_m_excess = -56.584535      ! Dy144
       CASE(145)
          nuc_m_excess = -58.288238      ! Dy145
       CASE(146)
          nuc_m_excess = -62.554136      ! Dy146
       CASE(147)
          nuc_m_excess = -64.187855      ! Dy147
       CASE(148)
          nuc_m_excess = -67.859496      ! Dy148
       CASE(149)
          nuc_m_excess = -67.715154      ! Dy149
       CASE(150)
          nuc_m_excess = -69.316955      ! Dy150
       CASE(151)
          nuc_m_excess = -68.758601      ! Dy151
       CASE(152)
          nuc_m_excess = -70.124452      ! Dy152
       CASE(153)
          nuc_m_excess = -69.149764      ! Dy153
       CASE(154)
          nuc_m_excess = -70.398165      ! Dy154
       CASE(155)
          nuc_m_excess = -69.159914      ! Dy155
       CASE(156)
          nuc_m_excess = -70.529829      ! Dy156
       CASE(157)
          nuc_m_excess = -69.427886      ! Dy157
       CASE(158)
          nuc_m_excess = -70.412109      ! Dy158
       CASE(159)
          nuc_m_excess = -69.173476      ! Dy159
       CASE(160)
          nuc_m_excess = -69.678064      ! Dy160
       CASE(161)
          nuc_m_excess = -68.061133      ! Dy161
       CASE(162)
          nuc_m_excess = -68.186808      ! Dy162
       CASE(163)
          nuc_m_excess = -66.386498      ! Dy163
       CASE(164)
          nuc_m_excess = -65.973294      ! Dy164
       CASE(165)
          nuc_m_excess = -63.617935      ! Dy165
       CASE(166)
          nuc_m_excess = -62.590118      ! Dy166
       CASE(167)
          nuc_m_excess = -59.936551      ! Dy167
       CASE(168)
          nuc_m_excess = -58.564175      ! Dy168
       CASE(169)
          nuc_m_excess = -55.603099      ! Dy169
       CASE(170)
          nuc_m_excess = -53.663      ! Dy170
       CASE(171)
          nuc_m_excess = -50.114      ! Dy171
       CASE(172)
          nuc_m_excess = -47.73      ! Dy172
       CASE(173)
          nuc_m_excess = -43.78      ! Dy173
       END SELECT
    CASE (67)
       SELECT CASE (a)
       CASE(140)
          nuc_m_excess = -29.305      ! Ho140
       CASE(141)
          nuc_m_excess = -34.374      ! Ho141
       CASE(142)
          nuc_m_excess = -37.474      ! Ho142
       CASE(143)
          nuc_m_excess = -42.281      ! Ho143
       CASE(144)
          nuc_m_excess = -45.196      ! Ho144
       CASE(145)
          nuc_m_excess = -49.183      ! Ho145
       CASE(146)
          nuc_m_excess = -51.568      ! Ho146
       CASE(147)
          nuc_m_excess = -55.837477      ! Ho147
       CASE(148)
          nuc_m_excess = -58.01531      ! Ho148
       CASE(149)
          nuc_m_excess = -61.688403      ! Ho149
       CASE(150)
          nuc_m_excess = -61.947908      ! Ho150
       CASE(151)
          nuc_m_excess = -63.632086      ! Ho151
       CASE(152)
          nuc_m_excess = -63.608266      ! Ho152
       CASE(153)
          nuc_m_excess = -65.01941      ! Ho153
       CASE(154)
          nuc_m_excess = -64.644213      ! Ho154
       CASE(155)
          nuc_m_excess = -66.039673      ! Ho155
       CASE(156)
          nuc_m_excess = -65.354551      ! Ho156
       CASE(157)
          nuc_m_excess = -66.82893      ! Ho157
       CASE(158)
          nuc_m_excess = -66.191026      ! Ho158
       CASE(159)
          nuc_m_excess = -67.335876      ! Ho159
       CASE(160)
          nuc_m_excess = -66.388064      ! Ho160
       CASE(161)
          nuc_m_excess = -67.202843      ! Ho161
       CASE(162)
          nuc_m_excess = -66.047112      ! Ho162
       CASE(163)
          nuc_m_excess = -66.383942      ! Ho163
       CASE(164)
          nuc_m_excess = -64.987069      ! Ho164
       CASE(165)
          nuc_m_excess = -64.904574      ! Ho165
       CASE(166)
          nuc_m_excess = -63.076897      ! Ho166
       CASE(167)
          nuc_m_excess = -62.286551      ! Ho167
       CASE(168)
          nuc_m_excess = -60.066731      ! Ho168
       CASE(169)
          nuc_m_excess = -58.803099      ! Ho169
       CASE(170)
          nuc_m_excess = -56.244606      ! Ho170
       CASE(171)
          nuc_m_excess = -54.524862      ! Ho171
       CASE(172)
          nuc_m_excess = -51.4      ! Ho172
       CASE(173)
          nuc_m_excess = -49.099      ! Ho173
       CASE(174)
          nuc_m_excess = -45.503      ! Ho174
       CASE(175)
          nuc_m_excess = -42.802      ! Ho175
       END SELECT
    CASE (68)
       SELECT CASE (a)
       CASE(143)
          nuc_m_excess = -31.354      ! Er143
       CASE(144)
          nuc_m_excess = -36.906      ! Er144
       CASE(145)
          nuc_m_excess = -39.691      ! Er145
       CASE(146)
          nuc_m_excess = -44.712      ! Er146
       CASE(147)
          nuc_m_excess = -47.05      ! Er147
       CASE(148)
          nuc_m_excess = -51.651      ! Er148
       CASE(149)
          nuc_m_excess = -53.741615      ! Er149
       CASE(150)
          nuc_m_excess = -57.832887      ! Er150
       CASE(151)
          nuc_m_excess = -58.265971      ! Er151
       CASE(152)
          nuc_m_excess = -60.500173      ! Er152
       CASE(153)
          nuc_m_excess = -60.487968      ! Er153
       CASE(154)
          nuc_m_excess = -62.612157      ! Er154
       CASE(155)
          nuc_m_excess = -62.215463      ! Er155
       CASE(156)
          nuc_m_excess = -64.212821      ! Er156
       CASE(157)
          nuc_m_excess = -63.419838      ! Er157
       CASE(158)
          nuc_m_excess = -65.303809      ! Er158
       CASE(159)
          nuc_m_excess = -64.567376      ! Er159
       CASE(160)
          nuc_m_excess = -66.058488      ! Er160
       CASE(161)
          nuc_m_excess = -65.20895      ! Er161
       CASE(162)
          nuc_m_excess = -66.34262      ! Er162
       CASE(163)
          nuc_m_excess = -65.174074      ! Er163
       CASE(164)
          nuc_m_excess = -65.949562      ! Er164
       CASE(165)
          nuc_m_excess = -64.528312      ! Er165
       CASE(166)
          nuc_m_excess = -64.931595      ! Er166
       CASE(167)
          nuc_m_excess = -63.296733      ! Er167
       CASE(168)
          nuc_m_excess = -62.996731      ! Er168
       CASE(169)
          nuc_m_excess = -60.928684      ! Er169
       CASE(170)
          nuc_m_excess = -60.114606      ! Er170
       CASE(171)
          nuc_m_excess = -57.724862      ! Er171
       CASE(172)
          nuc_m_excess = -56.489417      ! Er172
       CASE(173)
          nuc_m_excess = -53.654      ! Er173
       CASE(174)
          nuc_m_excess = -51.949      ! Er174
       CASE(175)
          nuc_m_excess = -48.652      ! Er175
       CASE(176)
          nuc_m_excess = -46.5      ! Er176
       CASE(177)
          nuc_m_excess = -42.802      ! Er177
       END SELECT
    CASE (69)
       SELECT CASE (a)
       CASE(145)
          nuc_m_excess = -27.877      ! Tm145
       CASE(146)
          nuc_m_excess = -31.275      ! Tm146
       CASE(147)
          nuc_m_excess = -36.365      ! Tm147
       CASE(148)
          nuc_m_excess = -39.272      ! Tm148
       CASE(149)
          nuc_m_excess = -44.041      ! Tm149
       CASE(150)
          nuc_m_excess = -46.612      ! Tm150
       CASE(151)
          nuc_m_excess = -50.781802      ! Tm151
       CASE(152)
          nuc_m_excess = -51.770574      ! Tm152
       CASE(153)
          nuc_m_excess = -54.01537      ! Tm153
       CASE(154)
          nuc_m_excess = -54.429236      ! Tm154
       CASE(155)
          nuc_m_excess = -56.635339      ! Tm155
       CASE(156)
          nuc_m_excess = -56.839827      ! Tm156
       CASE(157)
          nuc_m_excess = -58.709273      ! Tm157
       CASE(158)
          nuc_m_excess = -58.703194      ! Tm158
       CASE(159)
          nuc_m_excess = -60.570398      ! Tm159
       CASE(160)
          nuc_m_excess = -60.302313      ! Tm160
       CASE(161)
          nuc_m_excess = -61.898708      ! Tm161
       CASE(162)
          nuc_m_excess = -61.483558      ! Tm162
       CASE(163)
          nuc_m_excess = -62.735074      ! Tm163
       CASE(164)
          nuc_m_excess = -61.888462      ! Tm164
       CASE(165)
          nuc_m_excess = -62.935934      ! Tm165
       CASE(166)
          nuc_m_excess = -61.893929      ! Tm166
       CASE(167)
          nuc_m_excess = -62.548311      ! Tm167
       CASE(168)
          nuc_m_excess = -61.317664      ! Tm168
       CASE(169)
          nuc_m_excess = -61.279963      ! Tm169
       CASE(170)
          nuc_m_excess = -59.800614      ! Tm170
       CASE(171)
          nuc_m_excess = -59.215595      ! Tm171
       CASE(172)
          nuc_m_excess = -57.37999      ! Tm172
       CASE(173)
          nuc_m_excess = -56.258878      ! Tm173
       CASE(174)
          nuc_m_excess = -53.869598      ! Tm174
       CASE(175)
          nuc_m_excess = -52.315634      ! Tm175
       CASE(176)
          nuc_m_excess = -49.374133      ! Tm176
       CASE(177)
          nuc_m_excess = -47.469      ! Tm177
       CASE(178)
          nuc_m_excess = -44.116      ! Tm178
       CASE(179)
          nuc_m_excess = -41.601      ! Tm179
       END SELECT
    CASE (70)
       SELECT CASE (a)
       CASE(148)
          nuc_m_excess = -30.348      ! Yb148
       CASE(149)
          nuc_m_excess = -33.497      ! Yb149
       CASE(150)
          nuc_m_excess = -38.732      ! Yb150
       CASE(151)
          nuc_m_excess = -41.543916      ! Yb151
       CASE(152)
          nuc_m_excess = -46.305574      ! Yb152
       CASE(153)
          nuc_m_excess = -47.059      ! Yb153
       CASE(154)
          nuc_m_excess = -49.933735      ! Yb154
       CASE(155)
          nuc_m_excess = -50.503433      ! Yb155
       CASE(156)
          nuc_m_excess = -53.26449      ! Yb156
       CASE(157)
          nuc_m_excess = -53.441815      ! Yb157
       CASE(158)
          nuc_m_excess = -56.014817      ! Yb158
       CASE(159)
          nuc_m_excess = -55.842973      ! Yb159
       CASE(160)
          nuc_m_excess = -58.169617      ! Yb160
       CASE(161)
          nuc_m_excess = -57.844214      ! Yb161
       CASE(162)
          nuc_m_excess = -59.831527      ! Yb162
       CASE(163)
          nuc_m_excess = -59.304213      ! Yb163
       CASE(164)
          nuc_m_excess = -61.022716      ! Yb164
       CASE(165)
          nuc_m_excess = -60.287224      ! Yb165
       CASE(166)
          nuc_m_excess = -61.588481      ! Yb166
       CASE(167)
          nuc_m_excess = -60.594053      ! Yb167
       CASE(168)
          nuc_m_excess = -61.574646      ! Yb168
       CASE(169)
          nuc_m_excess = -60.370311      ! Yb169
       CASE(170)
          nuc_m_excess = -60.768957      ! Yb170
       CASE(171)
          nuc_m_excess = -59.312137      ! Yb171
       CASE(172)
          nuc_m_excess = -59.26028      ! Yb172
       CASE(173)
          nuc_m_excess = -57.556282      ! Yb173
       CASE(174)
          nuc_m_excess = -56.949598      ! Yb174
       CASE(175)
          nuc_m_excess = -54.700634      ! Yb175
       CASE(176)
          nuc_m_excess = -53.494133      ! Yb176
       CASE(177)
          nuc_m_excess = -50.989216      ! Yb177
       CASE(178)
          nuc_m_excess = -49.698297      ! Yb178
       CASE(179)
          nuc_m_excess = -46.416      ! Yb179
       CASE(180)
          nuc_m_excess = -44.404      ! Yb180
       CASE(181)
          nuc_m_excess = -40.846      ! Yb181
       END SELECT
    CASE (71)
       SELECT CASE (a)
       CASE(150)
          nuc_m_excess = -24.938      ! Lu150
       CASE(151)
          nuc_m_excess = -30.202      ! Lu151
       CASE(152)
          nuc_m_excess = -33.422      ! Lu152
       CASE(153)
          nuc_m_excess = -38.407984      ! Lu153
       CASE(154)
          nuc_m_excess = -39.568      ! Lu154
       CASE(155)
          nuc_m_excess = -42.554171      ! Lu155
       CASE(156)
          nuc_m_excess = -43.749923      ! Lu156
       CASE(157)
          nuc_m_excess = -46.483134      ! Lu157
       CASE(158)
          nuc_m_excess = -47.214373      ! Lu158
       CASE(159)
          nuc_m_excess = -49.714975      ! Lu159
       CASE(160)
          nuc_m_excess = -50.269937      ! Lu160
       CASE(161)
          nuc_m_excess = -52.562344      ! Lu161
       CASE(162)
          nuc_m_excess = -52.836866      ! Lu162
       CASE(163)
          nuc_m_excess = -54.791409      ! Lu163
       CASE(164)
          nuc_m_excess = -54.64237      ! Lu164
       CASE(165)
          nuc_m_excess = -56.442273      ! Lu165
       CASE(166)
          nuc_m_excess = -56.020981      ! Lu166
       CASE(167)
          nuc_m_excess = -57.501125      ! Lu167
       CASE(168)
          nuc_m_excess = -57.064151      ! Lu168
       CASE(169)
          nuc_m_excess = -58.077311      ! Lu169
       CASE(170)
          nuc_m_excess = -57.310199      ! Lu170
       CASE(171)
          nuc_m_excess = -57.833542      ! Lu171
       CASE(172)
          nuc_m_excess = -56.741334      ! Lu172
       CASE(173)
          nuc_m_excess = -56.885778      ! Lu173
       CASE(174)
          nuc_m_excess = -55.575279      ! Lu174
       CASE(175)
          nuc_m_excess = -55.170695      ! Lu175
       CASE(176)
          nuc_m_excess = -53.387359      ! Lu176
       CASE(177)
          nuc_m_excess = -52.389034      ! Lu177
       CASE(178)
          nuc_m_excess = -50.343004      ! Lu178
       CASE(179)
          nuc_m_excess = -49.06417      ! Lu179
       CASE(180)
          nuc_m_excess = -46.685398      ! Lu180
       CASE(181)
          nuc_m_excess = -44.74      ! Lu181
       CASE(182)
          nuc_m_excess = -41.88      ! Lu182
       CASE(183)
          nuc_m_excess = -39.523      ! Lu183
       CASE(184)
          nuc_m_excess = -36.412      ! Lu184
       END SELECT
    CASE (72)
       SELECT CASE (a)
       CASE(153)
          nuc_m_excess = -27.302      ! Hf153
       CASE(154)
          nuc_m_excess = -32.733      ! Hf154
       CASE(155)
          nuc_m_excess = -34.102      ! Hf155
       CASE(156)
          nuc_m_excess = -37.852167      ! Hf156
       CASE(157)
          nuc_m_excess = -38.754      ! Hf157
       CASE(158)
          nuc_m_excess = -42.104119      ! Hf158
       CASE(159)
          nuc_m_excess = -42.853503      ! Hf159
       CASE(160)
          nuc_m_excess = -45.937205      ! Hf160
       CASE(161)
          nuc_m_excess = -46.318685      ! Hf161
       CASE(162)
          nuc_m_excess = -49.173105      ! Hf162
       CASE(163)
          nuc_m_excess = -49.28628      ! Hf163
       CASE(164)
          nuc_m_excess = -51.821541      ! Hf164
       CASE(165)
          nuc_m_excess = -51.635507      ! Hf165
       CASE(166)
          nuc_m_excess = -53.858984      ! Hf166
       CASE(167)
          nuc_m_excess = -53.467756      ! Hf167
       CASE(168)
          nuc_m_excess = -55.360552      ! Hf168
       CASE(169)
          nuc_m_excess = -54.71689      ! Hf169
       CASE(170)
          nuc_m_excess = -56.253855      ! Hf170
       CASE(171)
          nuc_m_excess = -55.431345      ! Hf171
       CASE(172)
          nuc_m_excess = -56.403544      ! Hf172
       CASE(173)
          nuc_m_excess = -55.411784      ! Hf173
       CASE(174)
          nuc_m_excess = -55.846626      ! Hf174
       CASE(175)
          nuc_m_excess = -54.483847      ! Hf175
       CASE(176)
          nuc_m_excess = -54.577509      ! Hf176
       CASE(177)
          nuc_m_excess = -52.889623      ! Hf177
       CASE(178)
          nuc_m_excess = -52.444262      ! Hf178
       CASE(179)
          nuc_m_excess = -50.471936      ! Hf179
       CASE(180)
          nuc_m_excess = -49.788398      ! Hf180
       CASE(181)
          nuc_m_excess = -47.411884      ! Hf181
       CASE(182)
          nuc_m_excess = -46.058563      ! Hf182
       CASE(183)
          nuc_m_excess = -43.286117      ! Hf183
       CASE(184)
          nuc_m_excess = -41.501304      ! Hf184
       CASE(185)
          nuc_m_excess = -38.359      ! Hf185
       CASE(186)
          nuc_m_excess = -36.431      ! Hf186
       CASE(187)
          nuc_m_excess = -32.984      ! Hf187
       CASE(188)
          nuc_m_excess = -30.879      ! Hf188
       END SELECT
    CASE (73)
       SELECT CASE (a)
       CASE(155)
          nuc_m_excess = -23.668      ! Ta155
       CASE(156)
          nuc_m_excess = -25.799      ! Ta156
       CASE(157)
          nuc_m_excess = -29.628547      ! Ta157
       CASE(158)
          nuc_m_excess = -31.019      ! Ta158
       CASE(159)
          nuc_m_excess = -34.44835      ! Ta159
       CASE(160)
          nuc_m_excess = -35.875507      ! Ta160
       CASE(161)
          nuc_m_excess = -38.734      ! Ta161
       CASE(162)
          nuc_m_excess = -39.782377      ! Ta162
       CASE(163)
          nuc_m_excess = -42.541078      ! Ta163
       CASE(164)
          nuc_m_excess = -43.282801      ! Ta164
       CASE(165)
          nuc_m_excess = -45.855107      ! Ta165
       CASE(166)
          nuc_m_excess = -46.097776      ! Ta166
       CASE(167)
          nuc_m_excess = -48.35106      ! Ta167
       CASE(168)
          nuc_m_excess = -48.393908      ! Ta168
       CASE(169)
          nuc_m_excess = -50.29043      ! Ta169
       CASE(170)
          nuc_m_excess = -50.137665      ! Ta170
       CASE(171)
          nuc_m_excess = -51.720273      ! Ta171
       CASE(172)
          nuc_m_excess = -51.329977      ! Ta172
       CASE(173)
          nuc_m_excess = -52.396538      ! Ta173
       CASE(174)
          nuc_m_excess = -51.740766      ! Ta174
       CASE(175)
          nuc_m_excess = -52.408647      ! Ta175
       CASE(176)
          nuc_m_excess = -51.365374      ! Ta176
       CASE(177)
          nuc_m_excess = -51.723623      ! Ta177
       CASE(178)
          nuc_m_excess = -50.507262      ! Ta178
       CASE(179)
          nuc_m_excess = -50.366314      ! Ta179
       CASE(180)
          nuc_m_excess = -48.936195      ! Ta180
       CASE(181)
          nuc_m_excess = -48.441634      ! Ta181
       CASE(182)
          nuc_m_excess = -46.433254      ! Ta182
       CASE(183)
          nuc_m_excess = -45.296117      ! Ta183
       CASE(184)
          nuc_m_excess = -42.841304      ! Ta184
       CASE(185)
          nuc_m_excess = -41.396176      ! Ta185
       CASE(186)
          nuc_m_excess = -38.608542      ! Ta186
       CASE(187)
          nuc_m_excess = -36.766      ! Ta187
       CASE(188)
          nuc_m_excess = -33.813      ! Ta188
       CASE(189)
          nuc_m_excess = -31.829      ! Ta189
       CASE(190)
          nuc_m_excess = -28.662      ! Ta190
       END SELECT
    CASE (74)
       SELECT CASE (a)
       CASE(158)
          nuc_m_excess = -23.695      ! W158
       CASE(159)
          nuc_m_excess = -25.227      ! W159
       CASE(160)
          nuc_m_excess = -29.361804      ! W160
       CASE(161)
          nuc_m_excess = -30.407      ! W161
       CASE(162)
          nuc_m_excess = -34.001937      ! W162
       CASE(163)
          nuc_m_excess = -34.909095      ! W163
       CASE(164)
          nuc_m_excess = -38.233747      ! W164
       CASE(165)
          nuc_m_excess = -38.861977      ! W165
       CASE(166)
          nuc_m_excess = -41.891844      ! W166
       CASE(167)
          nuc_m_excess = -42.088612      ! W167
       CASE(168)
          nuc_m_excess = -44.890192      ! W168
       CASE(169)
          nuc_m_excess = -44.917768      ! W169
       CASE(170)
          nuc_m_excess = -47.293364      ! W170
       CASE(171)
          nuc_m_excess = -47.086091      ! W171
       CASE(172)
          nuc_m_excess = -49.097186      ! W172
       CASE(173)
          nuc_m_excess = -48.727383      ! W173
       CASE(174)
          nuc_m_excess = -50.227088      ! W174
       CASE(175)
          nuc_m_excess = -49.632795      ! W175
       CASE(176)
          nuc_m_excess = -50.641603      ! W176
       CASE(177)
          nuc_m_excess = -49.701726      ! W177
       CASE(178)
          nuc_m_excess = -50.415962      ! W178
       CASE(179)
          nuc_m_excess = -49.303561      ! W179
       CASE(180)
          nuc_m_excess = -49.644477      ! W180
       CASE(181)
          nuc_m_excess = -48.253952      ! W181
       CASE(182)
          nuc_m_excess = -48.247518      ! W182
       CASE(183)
          nuc_m_excess = -46.367023      ! W183
       CASE(184)
          nuc_m_excess = -45.707304      ! W184
       CASE(185)
          nuc_m_excess = -43.389676      ! W185
       CASE(186)
          nuc_m_excess = -42.509542      ! W186
       CASE(187)
          nuc_m_excess = -39.904768      ! W187
       CASE(188)
          nuc_m_excess = -38.66715      ! W188
       CASE(189)
          nuc_m_excess = -35.477935      ! W189
       CASE(190)
          nuc_m_excess = -34.296326      ! W190
       CASE(191)
          nuc_m_excess = -31.112      ! W191
       CASE(192)
          nuc_m_excess = -29.649      ! W192
       END SELECT
    CASE (75)
       SELECT CASE (a)
       CASE(160)
          nuc_m_excess = -16.66      ! Re160
       CASE(161)
          nuc_m_excess = -20.875601      ! Re161
       CASE(162)
          nuc_m_excess = -22.354      ! Re162
       CASE(163)
          nuc_m_excess = -26.006814      ! Re163
       CASE(164)
          nuc_m_excess = -27.644      ! Re164
       CASE(165)
          nuc_m_excess = -30.656812      ! Re165
       CASE(166)
          nuc_m_excess = -31.85      ! Re166
       CASE(167)
          nuc_m_excess = -34.837      ! Re167
       CASE(168)
          nuc_m_excess = -35.794885      ! Re168
       CASE(169)
          nuc_m_excess = -38.385847      ! Re169
       CASE(170)
          nuc_m_excess = -38.917754      ! Re170
       CASE(171)
          nuc_m_excess = -41.250281      ! Re171
       CASE(172)
          nuc_m_excess = -41.523244      ! Re172
       CASE(173)
          nuc_m_excess = -43.553865      ! Re173
       CASE(174)
          nuc_m_excess = -43.673097      ! Re174
       CASE(175)
          nuc_m_excess = -45.288307      ! Re175
       CASE(176)
          nuc_m_excess = -45.062886      ! Re176
       CASE(177)
          nuc_m_excess = -46.26917      ! Re177
       CASE(178)
          nuc_m_excess = -45.653453      ! Re178
       CASE(179)
          nuc_m_excess = -46.586212      ! Re179
       CASE(180)
          nuc_m_excess = -45.839673      ! Re180
       CASE(181)
          nuc_m_excess = -46.511436      ! Re181
       CASE(182)
          nuc_m_excess = -45.447518      ! Re182
       CASE(183)
          nuc_m_excess = -45.811023      ! Re183
       CASE(184)
          nuc_m_excess = -44.226631      ! Re184
       CASE(185)
          nuc_m_excess = -43.822152      ! Re185
       CASE(186)
          nuc_m_excess = -41.930192      ! Re186
       CASE(187)
          nuc_m_excess = -41.215714      ! Re187
       CASE(188)
          nuc_m_excess = -39.01615      ! Re188
       CASE(189)
          nuc_m_excess = -37.977935      ! Re189
       CASE(190)
          nuc_m_excess = -35.566326      ! Re190
       CASE(191)
          nuc_m_excess = -34.348616      ! Re191
       CASE(192)
          nuc_m_excess = -31.708      ! Re192
       CASE(193)
          nuc_m_excess = -30.302      ! Re193
       CASE(194)
          nuc_m_excess = -27.554      ! Re194
       END SELECT
    CASE (76)
       SELECT CASE (a)
       CASE(162)
          nuc_m_excess = -14.503      ! Os162
       CASE(163)
          nuc_m_excess = -16.124      ! Os163
       CASE(164)
          nuc_m_excess = -20.459661      ! Os164
       CASE(165)
          nuc_m_excess = -21.646      ! Os165
       CASE(166)
          nuc_m_excess = -25.4384      ! Os166
       CASE(167)
          nuc_m_excess = -26.502896      ! Os167
       CASE(168)
          nuc_m_excess = -29.99068      ! Os168
       CASE(169)
          nuc_m_excess = -30.721352      ! Os169
       CASE(170)
          nuc_m_excess = -33.92778      ! Os170
       CASE(171)
          nuc_m_excess = -34.29312      ! Os171
       CASE(172)
          nuc_m_excess = -37.238053      ! Os172
       CASE(173)
          nuc_m_excess = -37.438225      ! Os173
       CASE(174)
          nuc_m_excess = -39.996301      ! Os174
       CASE(175)
          nuc_m_excess = -40.104697      ! Os175
       CASE(176)
          nuc_m_excess = -42.09794      ! Os176
       CASE(177)
          nuc_m_excess = -41.94953      ! Os177
       CASE(178)
          nuc_m_excess = -43.546189      ! Os178
       CASE(179)
          nuc_m_excess = -43.020103      ! Os179
       CASE(180)
          nuc_m_excess = -44.358859      ! Os180
       CASE(181)
          nuc_m_excess = -43.552934      ! Os181
       CASE(182)
          nuc_m_excess = -44.609074      ! Os182
       CASE(183)
          nuc_m_excess = -43.662754      ! Os183
       CASE(184)
          nuc_m_excess = -44.256145      ! Os184
       CASE(185)
          nuc_m_excess = -42.809355      ! Os185
       CASE(186)
          nuc_m_excess = -42.999479      ! Os186
       CASE(187)
          nuc_m_excess = -41.218183      ! Os187
       CASE(188)
          nuc_m_excess = -41.136426      ! Os188
       CASE(189)
          nuc_m_excess = -38.98538      ! Os189
       CASE(190)
          nuc_m_excess = -38.706326      ! Os190
       CASE(191)
          nuc_m_excess = -36.393733      ! Os191
       CASE(192)
          nuc_m_excess = -35.880506      ! Os192
       CASE(193)
          nuc_m_excess = -33.392604      ! Os193
       CASE(194)
          nuc_m_excess = -32.432681      ! Os194
       CASE(195)
          nuc_m_excess = -29.689824      ! Os195
       CASE(196)
          nuc_m_excess = -28.280779      ! Os196
       END SELECT
    CASE (77)
       SELECT CASE (a)
       CASE(164)
          nuc_m_excess = -7.265      ! Ir164
       CASE(165)
          nuc_m_excess = -11.625      ! Ir165
       CASE(166)
          nuc_m_excess = -13.205      ! Ir166
       CASE(167)
          nuc_m_excess = -17.078797      ! Ir167
       CASE(168)
          nuc_m_excess = -18.741      ! Ir168
       CASE(169)
          nuc_m_excess = -22.081119      ! Ir169
       CASE(170)
          nuc_m_excess = -23.32      ! Ir170
       CASE(171)
          nuc_m_excess = -26.430172      ! Ir171
       CASE(172)
          nuc_m_excess = -27.52      ! Ir172
       CASE(173)
          nuc_m_excess = -30.271935      ! Ir173
       CASE(174)
          nuc_m_excess = -30.868738      ! Ir174
       CASE(175)
          nuc_m_excess = -33.428622      ! Ir175
       CASE(176)
          nuc_m_excess = -33.861029      ! Ir176
       CASE(177)
          nuc_m_excess = -36.047421      ! Ir177
       CASE(178)
          nuc_m_excess = -36.251884      ! Ir178
       CASE(179)
          nuc_m_excess = -38.077364      ! Ir179
       CASE(180)
          nuc_m_excess = -37.977526      ! Ir180
       CASE(181)
          nuc_m_excess = -39.471782      ! Ir181
       CASE(182)
          nuc_m_excess = -39.051679      ! Ir182
       CASE(183)
          nuc_m_excess = -40.197266      ! Ir183
       CASE(184)
          nuc_m_excess = -39.610851      ! Ir184
       CASE(185)
          nuc_m_excess = -40.335554      ! Ir185
       CASE(186)
          nuc_m_excess = -39.172952      ! Ir186
       CASE(187)
          nuc_m_excess = -39.715774      ! Ir187
       CASE(188)
          nuc_m_excess = -38.328071      ! Ir188
       CASE(189)
          nuc_m_excess = -38.453064      ! Ir189
       CASE(190)
          nuc_m_excess = -36.751194      ! Ir190
       CASE(191)
          nuc_m_excess = -36.706409      ! Ir191
       CASE(192)
          nuc_m_excess = -34.833207      ! Ir192
       CASE(193)
          nuc_m_excess = -34.533808      ! Ir193
       CASE(194)
          nuc_m_excess = -32.529281      ! Ir194
       CASE(195)
          nuc_m_excess = -31.689824      ! Ir195
       CASE(196)
          nuc_m_excess = -29.438431      ! Ir196
       CASE(197)
          nuc_m_excess = -28.267783      ! Ir197
       CASE(198)
          nuc_m_excess = -25.821      ! Ir198
       CASE(199)
          nuc_m_excess = -24.400873      ! Ir199
       END SELECT
    CASE (78)
       SELECT CASE (a)
       CASE(166)
          nuc_m_excess = -4.792      ! Pt166
       CASE(167)
          nuc_m_excess = -6.54      ! Pt167
       CASE(168)
          nuc_m_excess = -11.037512      ! Pt168
       CASE(169)
          nuc_m_excess = -12.375      ! Pt169
       CASE(170)
          nuc_m_excess = -16.305533      ! Pt170
       CASE(171)
          nuc_m_excess = -17.470596      ! Pt171
       CASE(172)
          nuc_m_excess = -21.101014      ! Pt172
       CASE(173)
          nuc_m_excess = -21.94157      ! Pt173
       CASE(174)
          nuc_m_excess = -25.319155      ! Pt174
       CASE(175)
          nuc_m_excess = -25.69009      ! Pt175
       CASE(176)
          nuc_m_excess = -28.927898      ! Pt176
       CASE(177)
          nuc_m_excess = -29.370489      ! Pt177
       CASE(178)
          nuc_m_excess = -31.998007      ! Pt178
       CASE(179)
          nuc_m_excess = -32.263781      ! Pt179
       CASE(180)
          nuc_m_excess = -34.435958      ! Pt180
       CASE(181)
          nuc_m_excess = -34.374658      ! Pt181
       CASE(182)
          nuc_m_excess = -36.169301      ! Pt182
       CASE(183)
          nuc_m_excess = -35.772441      ! Pt183
       CASE(184)
          nuc_m_excess = -37.332183      ! Pt184
       CASE(185)
          nuc_m_excess = -36.683166      ! Pt185
       CASE(186)
          nuc_m_excess = -37.864474      ! Pt186
       CASE(187)
          nuc_m_excess = -36.712973      ! Pt187
       CASE(188)
          nuc_m_excess = -37.82295      ! Pt188
       CASE(189)
          nuc_m_excess = -36.483186      ! Pt189
       CASE(190)
          nuc_m_excess = -37.323422      ! Pt190
       CASE(191)
          nuc_m_excess = -35.69796      ! Pt191
       CASE(192)
          nuc_m_excess = -36.292864      ! Pt192
       CASE(193)
          nuc_m_excess = -34.477014      ! Pt193
       CASE(194)
          nuc_m_excess = -34.763121      ! Pt194
       CASE(195)
          nuc_m_excess = -32.796847      ! Pt195
       CASE(196)
          nuc_m_excess = -32.647448      ! Pt196
       CASE(197)
          nuc_m_excess = -30.422425      ! Pt197
       CASE(198)
          nuc_m_excess = -29.907673      ! Pt198
       CASE(199)
          nuc_m_excess = -27.392356      ! Pt199
       CASE(200)
          nuc_m_excess = -26.602838      ! Pt200
       CASE(201)
          nuc_m_excess = -23.741111      ! Pt201
       CASE(202)
          nuc_m_excess = -22.598      ! Pt202
       END SELECT
    CASE (79)
       SELECT CASE (a)
       CASE(169)
          nuc_m_excess = -1.788      ! Au169
       CASE(170)
          nuc_m_excess = -3.612      ! Au170
       CASE(171)
          nuc_m_excess = -7.564773      ! Au171
       CASE(172)
          nuc_m_excess = -9.282      ! Au172
       CASE(173)
          nuc_m_excess = -12.819798      ! Au173
       CASE(174)
          nuc_m_excess = -14.195      ! Au174
       CASE(175)
          nuc_m_excess = -17.443057      ! Au175
       CASE(176)
          nuc_m_excess = -18.537      ! Au176
       CASE(177)
          nuc_m_excess = -21.550199      ! Au177
       CASE(178)
          nuc_m_excess = -22.326122      ! Au178
       CASE(179)
          nuc_m_excess = -24.952104      ! Au179
       CASE(180)
          nuc_m_excess = -25.596408      ! Au180
       CASE(181)
          nuc_m_excess = -27.871187      ! Au181
       CASE(182)
          nuc_m_excess = -28.300768      ! Au182
       CASE(183)
          nuc_m_excess = -30.186894      ! Au183
       CASE(184)
          nuc_m_excess = -30.31871      ! Au184
       CASE(185)
          nuc_m_excess = -31.866958      ! Au185
       CASE(186)
          nuc_m_excess = -31.714853      ! Au186
       CASE(187)
          nuc_m_excess = -33.005122      ! Au187
       CASE(188)
          nuc_m_excess = -32.300802      ! Au188
       CASE(189)
          nuc_m_excess = -33.581955      ! Au189
       CASE(190)
          nuc_m_excess = -32.881422      ! Au190
       CASE(191)
          nuc_m_excess = -33.809297      ! Au191
       CASE(192)
          nuc_m_excess = -32.776523      ! Au192
       CASE(193)
          nuc_m_excess = -33.394325      ! Au193
       CASE(194)
          nuc_m_excess = -32.262062      ! Au194
       CASE(195)
          nuc_m_excess = -32.570023      ! Au195
       CASE(196)
          nuc_m_excess = -31.140018      ! Au196
       CASE(197)
          nuc_m_excess = -31.141091      ! Au197
       CASE(198)
          nuc_m_excess = -29.582104      ! Au198
       CASE(199)
          nuc_m_excess = -29.095035      ! Au199
       CASE(200)
          nuc_m_excess = -27.268884      ! Au200
       CASE(201)
          nuc_m_excess = -26.401111      ! Au201
       CASE(202)
          nuc_m_excess = -24.399705      ! Au202
       CASE(203)
          nuc_m_excess = -23.143394      ! Au203
       CASE(204)
          nuc_m_excess = -20.75      ! Au204
       CASE(205)
          nuc_m_excess = -18.751      ! Au205
       END SELECT
    CASE (80)
       SELECT CASE (a)
       CASE(171)
          nuc_m_excess = 3.502      ! Hg171
       CASE(172)
          nuc_m_excess = -1.087345      ! Hg172
       CASE(173)
          nuc_m_excess = -2.569      ! Hg173
       CASE(174)
          nuc_m_excess = -6.647425      ! Hg174
       CASE(175)
          nuc_m_excess = -7.989172      ! Hg175
       CASE(176)
          nuc_m_excess = -11.779132      ! Hg176
       CASE(177)
          nuc_m_excess = -12.780882      ! Hg177
       CASE(178)
          nuc_m_excess = -16.316846      ! Hg178
       CASE(179)
          nuc_m_excess = -16.921649      ! Hg179
       CASE(180)
          nuc_m_excess = -20.244723      ! Hg180
       CASE(181)
          nuc_m_excess = -20.661178      ! Hg181
       CASE(182)
          nuc_m_excess = -23.576146      ! Hg182
       CASE(183)
          nuc_m_excess = -23.799819      ! Hg183
       CASE(184)
          nuc_m_excess = -26.349123      ! Hg184
       CASE(185)
          nuc_m_excess = -26.175833      ! Hg185
       CASE(186)
          nuc_m_excess = -28.539309      ! Hg186
       CASE(187)
          nuc_m_excess = -28.117858      ! Hg187
       CASE(188)
          nuc_m_excess = -30.201784      ! Hg188
       CASE(189)
          nuc_m_excess = -29.630792      ! Hg189
       CASE(190)
          nuc_m_excess = -31.370436      ! Hg190
       CASE(191)
          nuc_m_excess = -30.59296      ! Hg191
       CASE(192)
          nuc_m_excess = -32.011418      ! Hg192
       CASE(193)
          nuc_m_excess = -31.050961      ! Hg193
       CASE(194)
          nuc_m_excess = -32.192983      ! Hg194
       CASE(195)
          nuc_m_excess = -31.000015      ! Hg195
       CASE(196)
          nuc_m_excess = -31.826683      ! Hg196
       CASE(197)
          nuc_m_excess = -30.540979      ! Hg197
       CASE(198)
          nuc_m_excess = -30.954447      ! Hg198
       CASE(199)
          nuc_m_excess = -29.547053      ! Hg199
       CASE(200)
          nuc_m_excess = -29.504137      ! Hg200
       CASE(201)
          nuc_m_excess = -27.663259      ! Hg201
       CASE(202)
          nuc_m_excess = -27.345859      ! Hg202
       CASE(203)
          nuc_m_excess = -25.269119      ! Hg203
       CASE(204)
          nuc_m_excess = -24.690242      ! Hg204
       CASE(205)
          nuc_m_excess = -22.287497      ! Hg205
       CASE(206)
          nuc_m_excess = -20.945512      ! Hg206
       CASE(207)
          nuc_m_excess = -16.218666      ! Hg207
       CASE(208)
          nuc_m_excess = -13.097      ! Hg208
       CASE(209)
          nuc_m_excess = -8.346      ! Hg209
       CASE(210)
          nuc_m_excess = -5.114      ! Hg210
       END SELECT
    CASE (81)
       SELECT CASE (a)
       CASE(176)
          nuc_m_excess = 0.55      ! Tl176
       CASE(177)
          nuc_m_excess = -3.327962      ! Tl177
       CASE(178)
          nuc_m_excess = -4.754      ! Tl178
       CASE(179)
          nuc_m_excess = -8.300467      ! Tl179
       CASE(180)
          nuc_m_excess = -9.403      ! Tl180
       CASE(181)
          nuc_m_excess = -12.801105      ! Tl181
       CASE(182)
          nuc_m_excess = -13.351007      ! Tl182
       CASE(183)
          nuc_m_excess = -16.587297      ! Tl183
       CASE(184)
          nuc_m_excess = -16.885078      ! Tl184
       CASE(185)
          nuc_m_excess = -19.755772      ! Tl185
       CASE(186)
          nuc_m_excess = -20.190133      ! Tl186
       CASE(187)
          nuc_m_excess = -22.443512      ! Tl187
       CASE(188)
          nuc_m_excess = -22.346744      ! Tl188
       CASE(189)
          nuc_m_excess = -24.602221      ! Tl189
       CASE(190)
          nuc_m_excess = -24.333279      ! Tl190
       CASE(191)
          nuc_m_excess = -26.281028      ! Tl191
       CASE(192)
          nuc_m_excess = -25.872246      ! Tl192
       CASE(193)
          nuc_m_excess = -27.318856      ! Tl193
       CASE(194)
          nuc_m_excess = -26.827027      ! Tl194
       CASE(195)
          nuc_m_excess = -28.155026      ! Tl195
       CASE(196)
          nuc_m_excess = -27.496631      ! Tl196
       CASE(197)
          nuc_m_excess = -28.34116      ! Tl197
       CASE(198)
          nuc_m_excess = -27.494447      ! Tl198
       CASE(199)
          nuc_m_excess = -28.059394      ! Tl199
       CASE(200)
          nuc_m_excess = -27.048096      ! Tl200
       CASE(201)
          nuc_m_excess = -27.182028      ! Tl201
       CASE(202)
          nuc_m_excess = -25.983272      ! Tl202
       CASE(203)
          nuc_m_excess = -25.761192      ! Tl203
       CASE(204)
          nuc_m_excess = -24.345972      ! Tl204
       CASE(205)
          nuc_m_excess = -23.820592      ! Tl205
       CASE(206)
          nuc_m_excess = -22.253094      ! Tl206
       CASE(207)
          nuc_m_excess = -21.033666      ! Tl207
       CASE(208)
          nuc_m_excess = -16.749473      ! Tl208
       CASE(209)
          nuc_m_excess = -13.638048      ! Tl209
       CASE(210)
          nuc_m_excess = -9.246298      ! Tl210
       CASE(211)
          nuc_m_excess = -6.076      ! Tl211
       CASE(212)
          nuc_m_excess = -1.651      ! Tl212
       END SELECT
    CASE (82)
       SELECT CASE (a)
       CASE(178)
          nuc_m_excess = 3.5678      ! Pb178
       CASE(179)
          nuc_m_excess = 2.003      ! Pb179
       CASE(180)
          nuc_m_excess = -1.939209      ! Pb180
       CASE(181)
          nuc_m_excess = -3.144762      ! Pb181
       CASE(182)
          nuc_m_excess = -6.826135      ! Pb182
       CASE(183)
          nuc_m_excess = -7.568734      ! Pb183
       CASE(184)
          nuc_m_excess = -11.045339      ! Pb184
       CASE(185)
          nuc_m_excess = -11.541263      ! Pb185
       CASE(186)
          nuc_m_excess = -14.681328      ! Pb186
       CASE(187)
          nuc_m_excess = -14.979941      ! Pb187
       CASE(188)
          nuc_m_excess = -17.815439      ! Pb188
       CASE(189)
          nuc_m_excess = -17.878165      ! Pb189
       CASE(190)
          nuc_m_excess = -20.416935      ! Pb190
       CASE(191)
          nuc_m_excess = -20.246022      ! Pb191
       CASE(192)
          nuc_m_excess = -22.555968      ! Pb192
       CASE(193)
          nuc_m_excess = -22.19449      ! Pb193
       CASE(194)
          nuc_m_excess = -24.2076      ! Pb194
       CASE(195)
          nuc_m_excess = -23.713927      ! Pb195
       CASE(196)
          nuc_m_excess = -25.360754      ! Pb196
       CASE(197)
          nuc_m_excess = -24.748749      ! Pb197
       CASE(198)
          nuc_m_excess = -26.050199      ! Pb198
       CASE(199)
          nuc_m_excess = -25.227978      ! Pb199
       CASE(200)
          nuc_m_excess = -26.243283      ! Pb200
       CASE(201)
          nuc_m_excess = -25.257915      ! Pb201
       CASE(202)
          nuc_m_excess = -25.9336      ! Pb202
       CASE(203)
          nuc_m_excess = -24.78657      ! Pb203
       CASE(204)
          nuc_m_excess = -25.109735      ! Pb204
       CASE(205)
          nuc_m_excess = -23.770091      ! Pb205
       CASE(206)
          nuc_m_excess = -23.78544      ! Pb206
       CASE(207)
          nuc_m_excess = -22.451905      ! Pb207
       CASE(208)
          nuc_m_excess = -21.748455      ! Pb208
       CASE(209)
          nuc_m_excess = -17.61444      ! Pb209
       CASE(210)
          nuc_m_excess = -14.728292      ! Pb210
       CASE(211)
          nuc_m_excess = -10.49145      ! Pb211
       CASE(212)
          nuc_m_excess = -7.547389      ! Pb212
       CASE(213)
          nuc_m_excess = -3.184313      ! Pb213
       CASE(214)
          nuc_m_excess = -0.181261      ! Pb214
       CASE(215)
          nuc_m_excess = 4.477      ! Pb215
       END SELECT
    CASE (83)
       SELECT CASE (a)
       CASE(184)
          nuc_m_excess = 1.047      ! Bi184
       CASE(185)
          nuc_m_excess = -2.213      ! Bi185
       CASE(186)
          nuc_m_excess = -3.169291      ! Bi186
       CASE(187)
          nuc_m_excess = -6.373435      ! Bi187
       CASE(188)
          nuc_m_excess = -7.204962      ! Bi188
       CASE(189)
          nuc_m_excess = -10.061055      ! Bi189
       CASE(190)
          nuc_m_excess = -10.903017      ! Bi190
       CASE(191)
          nuc_m_excess = -13.240145      ! Bi191
       CASE(192)
          nuc_m_excess = -13.545828      ! Bi192
       CASE(193)
          nuc_m_excess = -15.872871      ! Bi193
       CASE(194)
          nuc_m_excess = -15.990063      ! Bi194
       CASE(195)
          nuc_m_excess = -18.023722      ! Bi195
       CASE(196)
          nuc_m_excess = -18.009031      ! Bi196
       CASE(197)
          nuc_m_excess = -19.687634      ! Bi197
       CASE(198)
          nuc_m_excess = -19.369486      ! Bi198
       CASE(199)
          nuc_m_excess = -20.798434      ! Bi199
       CASE(200)
          nuc_m_excess = -20.37007      ! Bi200
       CASE(201)
          nuc_m_excess = -21.415944      ! Bi201
       CASE(202)
          nuc_m_excess = -20.732892      ! Bi202
       CASE(203)
          nuc_m_excess = -21.539866      ! Bi203
       CASE(204)
          nuc_m_excess = -20.667303      ! Bi204
       CASE(205)
          nuc_m_excess = -21.06167      ! Bi205
       CASE(206)
          nuc_m_excess = -20.027931      ! Bi206
       CASE(207)
          nuc_m_excess = -20.054433      ! Bi207
       CASE(208)
          nuc_m_excess = -18.870022      ! Bi208
       CASE(209)
          nuc_m_excess = -18.258461      ! Bi209
       CASE(210)
          nuc_m_excess = -14.791778      ! Bi210
       CASE(211)
          nuc_m_excess = -11.858421      ! Bi211
       CASE(212)
          nuc_m_excess = -8.117295      ! Bi212
       CASE(213)
          nuc_m_excess = -5.230649      ! Bi213
       CASE(214)
          nuc_m_excess = -1.200193      ! Bi214
       CASE(215)
          nuc_m_excess = 1.648537      ! Bi215
       CASE(216)
          nuc_m_excess = 5.873948      ! Bi216
       CASE(217)
          nuc_m_excess = 8.821      ! Bi217
       CASE(218)
          nuc_m_excess = 13.335      ! Bi218
       END SELECT
    CASE (84)
       SELECT CASE (a)
       CASE(188)
          nuc_m_excess = -0.538359      ! Po188
       CASE(189)
          nuc_m_excess = -1.415347      ! Po189
       CASE(190)
          nuc_m_excess = -4.563217      ! Po190
       CASE(191)
          nuc_m_excess = -5.053834      ! Po191
       CASE(192)
          nuc_m_excess = -8.071257      ! Po192
       CASE(193)
          nuc_m_excess = -8.359902      ! Po193
       CASE(194)
          nuc_m_excess = -11.005037      ! Po194
       CASE(195)
          nuc_m_excess = -11.074785      ! Po195
       CASE(196)
          nuc_m_excess = -13.474451      ! Po196
       CASE(197)
          nuc_m_excess = -13.357968      ! Po197
       CASE(198)
          nuc_m_excess = -15.473405      ! Po198
       CASE(199)
          nuc_m_excess = -15.214964      ! Po199
       CASE(200)
          nuc_m_excess = -16.954491      ! Po200
       CASE(201)
          nuc_m_excess = -16.524923      ! Po201
       CASE(202)
          nuc_m_excess = -17.924235      ! Po202
       CASE(203)
          nuc_m_excess = -17.307062      ! Po203
       CASE(204)
          nuc_m_excess = -18.333551      ! Po204
       CASE(205)
          nuc_m_excess = -17.508993      ! Po205
       CASE(206)
          nuc_m_excess = -18.181739      ! Po206
       CASE(207)
          nuc_m_excess = -17.145849      ! Po207
       CASE(208)
          nuc_m_excess = -17.469516      ! Po208
       CASE(209)
          nuc_m_excess = -16.365944      ! Po209
       CASE(210)
          nuc_m_excess = -15.953071      ! Po210
       CASE(211)
          nuc_m_excess = -12.432507      ! Po211
       CASE(212)
          nuc_m_excess = -10.36942      ! Po212
       CASE(213)
          nuc_m_excess = -6.6534      ! Po213
       CASE(214)
          nuc_m_excess = -4.469913      ! Po214
       CASE(215)
          nuc_m_excess = -0.540277      ! Po215
       CASE(216)
          nuc_m_excess = 1.783844      ! Po216
       CASE(217)
          nuc_m_excess = 5.900825      ! Po217
       CASE(218)
          nuc_m_excess = 8.358331      ! Po218
       CASE(219)
          nuc_m_excess = 12.802      ! Po219
       CASE(220)
          nuc_m_excess = 15.465      ! Po220
       END SELECT
    CASE (85)
       SELECT CASE (a)
       CASE(193)
          nuc_m_excess = -0.14614      ! At193
       CASE(194)
          nuc_m_excess = -1.187575      ! At194
       CASE(195)
          nuc_m_excess = -3.476244      ! At195
       CASE(196)
          nuc_m_excess = -3.92338      ! At196
       CASE(197)
          nuc_m_excess = -6.344205      ! At197
       CASE(198)
          nuc_m_excess = -6.672103      ! At198
       CASE(199)
          nuc_m_excess = -8.819148      ! At199
       CASE(200)
          nuc_m_excess = -8.987739      ! At200
       CASE(201)
          nuc_m_excess = -10.789496      ! At201
       CASE(202)
          nuc_m_excess = -10.590867      ! At202
       CASE(203)
          nuc_m_excess = -12.163464      ! At203
       CASE(204)
          nuc_m_excess = -11.875313      ! At204
       CASE(205)
          nuc_m_excess = -12.971536      ! At205
       CASE(206)
          nuc_m_excess = -12.419576      ! At206
       CASE(207)
          nuc_m_excess = -13.242582      ! At207
       CASE(208)
          nuc_m_excess = -12.491356      ! At208
       CASE(209)
          nuc_m_excess = -12.879634      ! At209
       CASE(210)
          nuc_m_excess = -11.97183      ! At210
       CASE(211)
          nuc_m_excess = -11.647148      ! At211
       CASE(212)
          nuc_m_excess = -8.62119      ! At212
       CASE(213)
          nuc_m_excess = -6.579472      ! At213
       CASE(214)
          nuc_m_excess = -3.379708      ! At214
       CASE(215)
          nuc_m_excess = -1.255123      ! At215
       CASE(216)
          nuc_m_excess = 2.25725      ! At216
       CASE(217)
          nuc_m_excess = 4.395555      ! At217
       CASE(218)
          nuc_m_excess = 8.098722      ! At218
       CASE(219)
          nuc_m_excess = 10.397048      ! At219
       CASE(220)
          nuc_m_excess = 14.352164      ! At220
       CASE(221)
          nuc_m_excess = 16.813      ! At221
       CASE(222)
          nuc_m_excess = 20.8      ! At222
       CASE(223)
          nuc_m_excess = 23.464      ! At223
       END SELECT
    CASE (86)
       SELECT CASE (a)
       CASE(195)
          nuc_m_excess = 5.065181      ! Rn195
       CASE(196)
          nuc_m_excess = 1.970318      ! Rn196
       CASE(197)
          nuc_m_excess = 1.475814      ! Rn197
       CASE(198)
          nuc_m_excess = -1.230817      ! Rn198
       CASE(199)
          nuc_m_excess = -1.518058      ! Rn199
       CASE(200)
          nuc_m_excess = -4.006076      ! Rn200
       CASE(201)
          nuc_m_excess = -4.072179      ! Rn201
       CASE(202)
          nuc_m_excess = -6.275017      ! Rn202
       CASE(203)
          nuc_m_excess = -6.160261      ! Rn203
       CASE(204)
          nuc_m_excess = -7.984077      ! Rn204
       CASE(205)
          nuc_m_excess = -7.713889      ! Rn205
       CASE(206)
          nuc_m_excess = -9.115503      ! Rn206
       CASE(207)
          nuc_m_excess = -8.631014      ! Rn207
       CASE(208)
          nuc_m_excess = -9.647976      ! Rn208
       CASE(209)
          nuc_m_excess = -8.92861      ! Rn209
       CASE(210)
          nuc_m_excess = -9.597913      ! Rn210
       CASE(211)
          nuc_m_excess = -8.755556      ! Rn211
       CASE(212)
          nuc_m_excess = -8.659606      ! Rn212
       CASE(213)
          nuc_m_excess = -5.698258      ! Rn213
       CASE(214)
          nuc_m_excess = -4.319753      ! Rn214
       CASE(215)
          nuc_m_excess = -1.168574      ! Rn215
       CASE(216)
          nuc_m_excess = 0.255574      ! Rn216
       CASE(217)
          nuc_m_excess = 3.658607      ! Rn217
       CASE(218)
          nuc_m_excess = 5.217537      ! Rn218
       CASE(219)
          nuc_m_excess = 8.830754      ! Rn219
       CASE(220)
          nuc_m_excess = 10.613426      ! Rn220
       CASE(221)
          nuc_m_excess = 14.47242      ! Rn221
       CASE(222)
          nuc_m_excess = 16.373558      ! Rn222
       CASE(223)
          nuc_m_excess = 20.297      ! Rn223
       CASE(224)
          nuc_m_excess = 22.44      ! Rn224
       CASE(225)
          nuc_m_excess = 26.492      ! Rn225
       CASE(226)
          nuc_m_excess = 28.774      ! Rn226
       CASE(227)
          nuc_m_excess = 32.981      ! Rn227
       CASE(228)
          nuc_m_excess = 35.384      ! Rn228
       END SELECT
    CASE (87)
       SELECT CASE (a)
       CASE(199)
          nuc_m_excess = 6.760921      ! Fr199
       CASE(200)
          nuc_m_excess = 6.122235      ! Fr200
       CASE(201)
          nuc_m_excess = 3.596375      ! Fr201
       CASE(202)
          nuc_m_excess = 3.141787      ! Fr202
       CASE(203)
          nuc_m_excess = 0.861303      ! Fr203
       CASE(204)
          nuc_m_excess = 0.608456      ! Fr204
       CASE(205)
          nuc_m_excess = -1.309717      ! Fr205
       CASE(206)
          nuc_m_excess = -1.242551      ! Fr206
       CASE(207)
          nuc_m_excess = -2.841602      ! Fr207
       CASE(208)
          nuc_m_excess = -2.665206      ! Fr208
       CASE(209)
          nuc_m_excess = -3.769239      ! Fr209
       CASE(210)
          nuc_m_excess = -3.34617      ! Fr210
       CASE(211)
          nuc_m_excess = -4.157682      ! Fr211
       CASE(212)
          nuc_m_excess = -3.537587      ! Fr212
       CASE(213)
          nuc_m_excess = -3.549848      ! Fr213
       CASE(214)
          nuc_m_excess = -0.958372      ! Fr214
       CASE(215)
          nuc_m_excess = 0.318103      ! Fr215
       CASE(216)
          nuc_m_excess = 2.978909      ! Fr216
       CASE(217)
          nuc_m_excess = 4.314635      ! Fr217
       CASE(218)
          nuc_m_excess = 7.059162      ! Fr218
       CASE(219)
          nuc_m_excess = 8.618322      ! Fr219
       CASE(220)
          nuc_m_excess = 11.482904      ! Fr220
       CASE(221)
          nuc_m_excess = 13.278226      ! Fr221
       CASE(222)
          nuc_m_excess = 16.349332      ! Fr222
       CASE(223)
          nuc_m_excess = 18.383833      ! Fr223
       CASE(224)
          nuc_m_excess = 21.65719      ! Fr224
       CASE(225)
          nuc_m_excess = 23.814031      ! Fr225
       CASE(226)
          nuc_m_excess = 27.373099      ! Fr226
       CASE(227)
          nuc_m_excess = 29.654986      ! Fr227
       CASE(228)
          nuc_m_excess = 33.282      ! Fr228
       CASE(229)
          nuc_m_excess = 35.816157      ! Fr229
       CASE(230)
          nuc_m_excess = 39.598      ! Fr230
       CASE(231)
          nuc_m_excess = 42.328      ! Fr231
       CASE(232)
          nuc_m_excess = 46.363      ! Fr232
       END SELECT
    CASE (88)
       SELECT CASE (a)
       CASE(202)
          nuc_m_excess = 9.213115      ! Ra202
       CASE(203)
          nuc_m_excess = 8.636458      ! Ra203
       CASE(204)
          nuc_m_excess = 6.054402      ! Ra204
       CASE(205)
          nuc_m_excess = 5.839136      ! Ra205
       CASE(206)
          nuc_m_excess = 3.565079      ! Ra206
       CASE(207)
          nuc_m_excess = 3.537912      ! Ra207
       CASE(208)
          nuc_m_excess = 1.713894      ! Ra208
       CASE(209)
          nuc_m_excess = 1.854952      ! Ra209
       CASE(210)
          nuc_m_excess = 0.461069      ! Ra210
       CASE(211)
          nuc_m_excess = 0.83647      ! Ra211
       CASE(212)
          nuc_m_excess = -0.191422      ! Ra212
       CASE(213)
          nuc_m_excess = 0.357656      ! Ra213
       CASE(214)
          nuc_m_excess = 0.100503      ! Ra214
       CASE(215)
          nuc_m_excess = 2.533509      ! Ra215
       CASE(216)
          nuc_m_excess = 3.291002      ! Ra216
       CASE(217)
          nuc_m_excess = 5.887347      ! Ra217
       CASE(218)
          nuc_m_excess = 6.651082      ! Ra218
       CASE(219)
          nuc_m_excess = 9.39419      ! Ra219
       CASE(220)
          nuc_m_excess = 10.272874      ! Ra220
       CASE(221)
          nuc_m_excess = 12.963917      ! Ra221
       CASE(222)
          nuc_m_excess = 14.321283      ! Ra222
       CASE(223)
          nuc_m_excess = 17.234662      ! Ra223
       CASE(224)
          nuc_m_excess = 18.82719      ! Ra224
       CASE(225)
          nuc_m_excess = 21.994031      ! Ra225
       CASE(226)
          nuc_m_excess = 23.669099      ! Ra226
       CASE(227)
          nuc_m_excess = 27.178986      ! Ra227
       CASE(228)
          nuc_m_excess = 28.941792      ! Ra228
       CASE(229)
          nuc_m_excess = 32.562774      ! Ra229
       CASE(230)
          nuc_m_excess = 34.517809      ! Ra230
       CASE(231)
          nuc_m_excess = 38.396      ! Ra231
       CASE(232)
          nuc_m_excess = 40.649      ! Ra232
       CASE(233)
          nuc_m_excess = 44.767      ! Ra233
       CASE(234)
          nuc_m_excess = 47.23      ! Ra234
       END SELECT
    CASE (89)
       SELECT CASE (a)
       CASE(206)
          nuc_m_excess = 13.511303      ! Ac206
       CASE(207)
          nuc_m_excess = 11.131119      ! Ac207
       CASE(208)
          nuc_m_excess = 10.760201      ! Ac208
       CASE(209)
          nuc_m_excess = 8.844409      ! Ac209
       CASE(210)
          nuc_m_excess = 8.789565      ! Ac210
       CASE(211)
          nuc_m_excess = 7.204953      ! Ac211
       CASE(212)
          nuc_m_excess = 7.278529      ! Ac212
       CASE(213)
          nuc_m_excess = 6.15498      ! Ac213
       CASE(214)
          nuc_m_excess = 6.428984      ! Ac214
       CASE(215)
          nuc_m_excess = 6.011514      ! Ac215
       CASE(216)
          nuc_m_excess = 8.122698      ! Ac216
       CASE(217)
          nuc_m_excess = 8.706595      ! Ac217
       CASE(218)
          nuc_m_excess = 10.843944      ! Ac218
       CASE(219)
          nuc_m_excess = 11.569518      ! Ac219
       CASE(220)
          nuc_m_excess = 13.751627      ! Ac220
       CASE(221)
          nuc_m_excess = 14.523155      ! Ac221
       CASE(222)
          nuc_m_excess = 16.621441      ! Ac222
       CASE(223)
          nuc_m_excess = 17.826437      ! Ac223
       CASE(224)
          nuc_m_excess = 20.23472      ! Ac224
       CASE(225)
          nuc_m_excess = 21.63822      ! Ac225
       CASE(226)
          nuc_m_excess = 24.310214      ! Ac226
       CASE(227)
          nuc_m_excess = 25.850941      ! Ac227
       CASE(228)
          nuc_m_excess = 28.895981      ! Ac228
       CASE(229)
          nuc_m_excess = 30.753502      ! Ac229
       CASE(230)
          nuc_m_excess = 33.807809      ! Ac230
       CASE(231)
          nuc_m_excess = 35.917278      ! Ac231
       CASE(232)
          nuc_m_excess = 39.148307      ! Ac232
       CASE(233)
          nuc_m_excess = 41.498      ! Ac233
       CASE(234)
          nuc_m_excess = 45.103      ! Ac234
       CASE(235)
          nuc_m_excess = 47.722      ! Ac235
       CASE(236)
          nuc_m_excess = 51.508      ! Ac236
       END SELECT
    CASE (90)
       SELECT CASE (a)
       CASE(209)
          nuc_m_excess = 16.502052      ! Th209
       CASE(210)
          nuc_m_excess = 14.042591      ! Th210
       CASE(211)
          nuc_m_excess = 13.905728      ! Th211
       CASE(212)
          nuc_m_excess = 12.091061      ! Th212
       CASE(213)
          nuc_m_excess = 12.118868      ! Th213
       CASE(214)
          nuc_m_excess = 10.711967      ! Th214
       CASE(215)
          nuc_m_excess = 10.926733      ! Th215
       CASE(216)
          nuc_m_excess = 10.304294      ! Th216
       CASE(217)
          nuc_m_excess = 12.215918      ! Th217
       CASE(218)
          nuc_m_excess = 12.374432      ! Th218
       CASE(219)
          nuc_m_excess = 14.472525      ! Th219
       CASE(220)
          nuc_m_excess = 14.668946      ! Th220
       CASE(221)
          nuc_m_excess = 16.937984      ! Th221
       CASE(222)
          nuc_m_excess = 17.202945      ! Th222
       CASE(223)
          nuc_m_excess = 19.385739      ! Th223
       CASE(224)
          nuc_m_excess = 19.996285      ! Th224
       CASE(225)
          nuc_m_excess = 22.310233      ! Th225
       CASE(226)
          nuc_m_excess = 23.19706      ! Th226
       CASE(227)
          nuc_m_excess = 25.806176      ! Th227
       CASE(228)
          nuc_m_excess = 26.772188      ! Th228
       CASE(229)
          nuc_m_excess = 29.586514      ! Th229
       CASE(230)
          nuc_m_excess = 30.863976      ! Th230
       CASE(231)
          nuc_m_excess = 33.817278      ! Th231
       CASE(232)
          nuc_m_excess = 35.448307      ! Th232
       CASE(233)
          nuc_m_excess = 38.733238      ! Th233
       CASE(234)
          nuc_m_excess = 40.614285      ! Th234
       CASE(235)
          nuc_m_excess = 44.25535      ! Th235
       CASE(236)
          nuc_m_excess = 46.454      ! Th236
       CASE(237)
          nuc_m_excess = 50.202      ! Th237
       CASE(238)
          nuc_m_excess = 52.625      ! Th238
       END SELECT
    CASE (91)
       SELECT CASE (a)
       CASE(212)
          nuc_m_excess = 21.614516      ! Pa212
       CASE(213)
          nuc_m_excess = 19.663224      ! Pa213
       CASE(214)
          nuc_m_excess = 19.48538      ! Pa214
       CASE(215)
          nuc_m_excess = 17.871519      ! Pa215
       CASE(216)
          nuc_m_excess = 17.800445      ! Pa216
       CASE(217)
          nuc_m_excess = 17.068683      ! Pa217
       CASE(218)
          nuc_m_excess = 18.6689      ! Pa218
       CASE(219)
          nuc_m_excess = 18.521029      ! Pa219
       CASE(220)
          nuc_m_excess = 20.376714      ! Pa220
       CASE(221)
          nuc_m_excess = 20.379211      ! Pa221
       CASE(222)
          nuc_m_excess = 22.116      ! Pa222
       CASE(223)
          nuc_m_excess = 22.320714      ! Pa223
       CASE(224)
          nuc_m_excess = 23.870222      ! Pa224
       CASE(225)
          nuc_m_excess = 24.34057      ! Pa225
       CASE(226)
          nuc_m_excess = 26.033165      ! Pa226
       CASE(227)
          nuc_m_excess = 26.831753      ! Pa227
       CASE(228)
          nuc_m_excess = 28.924171      ! Pa228
       CASE(229)
          nuc_m_excess = 29.897971      ! Pa229
       CASE(230)
          nuc_m_excess = 32.174506      ! Pa230
       CASE(231)
          nuc_m_excess = 33.425722      ! Pa231
       CASE(232)
          nuc_m_excess = 35.947837      ! Pa232
       CASE(233)
          nuc_m_excess = 37.490098      ! Pa233
       CASE(234)
          nuc_m_excess = 40.341197      ! Pa234
       CASE(235)
          nuc_m_excess = 42.330456      ! Pa235
       CASE(236)
          nuc_m_excess = 45.346325      ! Pa236
       CASE(237)
          nuc_m_excess = 47.641875      ! Pa237
       CASE(238)
          nuc_m_excess = 50.768948      ! Pa238
       CASE(239)
          nuc_m_excess = 53.337      ! Pa239
       CASE(240)
          nuc_m_excess = 56.803      ! Pa240
       END SELECT
    CASE (92)
       SELECT CASE (a)
       CASE(217)
          nuc_m_excess = 22.699383      ! U217
       CASE(218)
          nuc_m_excess = 21.923337      ! U218
       CASE(219)
          nuc_m_excess = 23.212048      ! U219
       CASE(220)
          nuc_m_excess = 23.029      ! U220
       CASE(221)
          nuc_m_excess = 24.591      ! U221
       CASE(222)
          nuc_m_excess = 24.299      ! U222
       CASE(223)
          nuc_m_excess = 25.83834      ! U223
       CASE(224)
          nuc_m_excess = 25.713685      ! U224
       CASE(225)
          nuc_m_excess = 27.377277      ! U225
       CASE(226)
          nuc_m_excess = 27.328826      ! U226
       CASE(227)
          nuc_m_excess = 29.02197      ! U227
       CASE(228)
          nuc_m_excess = 29.224699      ! U228
       CASE(229)
          nuc_m_excess = 31.210582      ! U229
       CASE(230)
          nuc_m_excess = 31.614706      ! U230
       CASE(231)
          nuc_m_excess = 33.807368      ! U231
       CASE(232)
          nuc_m_excess = 34.610734      ! U232
       CASE(233)
          nuc_m_excess = 36.919958      ! U233
       CASE(234)
          nuc_m_excess = 38.146625      ! U234
       CASE(235)
          nuc_m_excess = 40.920456      ! U235
       CASE(236)
          nuc_m_excess = 42.446325      ! U236
       CASE(237)
          nuc_m_excess = 45.391875      ! U237
       CASE(238)
          nuc_m_excess = 47.308948      ! U238
       CASE(239)
          nuc_m_excess = 50.573883      ! U239
       CASE(240)
          nuc_m_excess = 52.715098      ! U240
       CASE(241)
          nuc_m_excess = 56.197      ! U241
       CASE(242)
          nuc_m_excess = 58.62      ! U242
       END SELECT
    CASE (93)
       SELECT CASE (a)
       CASE(225)
          nuc_m_excess = 31.590626      ! Np225
       CASE(226)
          nuc_m_excess = 32.738      ! Np226
       CASE(227)
          nuc_m_excess = 32.56204      ! Np227
       CASE(228)
          nuc_m_excess = 33.701      ! Np228
       CASE(229)
          nuc_m_excess = 33.779521      ! Np229
       CASE(230)
          nuc_m_excess = 35.236181      ! Np230
       CASE(231)
          nuc_m_excess = 35.625068      ! Np231
       CASE(232)
          nuc_m_excess = 37.361      ! Np232
       CASE(233)
          nuc_m_excess = 37.949575      ! Np233
       CASE(234)
          nuc_m_excess = 39.956471      ! Np234
       CASE(235)
          nuc_m_excess = 41.044669      ! Np235
       CASE(236)
          nuc_m_excess = 43.379304      ! Np236
       CASE(237)
          nuc_m_excess = 44.873275      ! Np237
       CASE(238)
          nuc_m_excess = 47.456272      ! Np238
       CASE(239)
          nuc_m_excess = 49.312385      ! Np239
       CASE(240)
          nuc_m_excess = 52.314736      ! Np240
       CASE(241)
          nuc_m_excess = 54.261791      ! Np241
       CASE(242)
          nuc_m_excess = 57.41839      ! Np242
       CASE(243)
          nuc_m_excess = 59.875      ! Np243
       CASE(244)
          nuc_m_excess = 63.202      ! Np244
       END SELECT
    CASE (94)
       SELECT CASE (a)
       CASE(228)
          nuc_m_excess = 36.088247      ! Pu228
       CASE(229)
          nuc_m_excess = 37.399683      ! Pu229
       CASE(230)
          nuc_m_excess = 36.933632      ! Pu230
       CASE(231)
          nuc_m_excess = 38.285435      ! Pu231
       CASE(232)
          nuc_m_excess = 38.365534      ! Pu232
       CASE(233)
          nuc_m_excess = 40.051797      ! Pu233
       CASE(234)
          nuc_m_excess = 40.349597      ! Pu234
       CASE(235)
          nuc_m_excess = 42.183684      ! Pu235
       CASE(236)
          nuc_m_excess = 42.902718      ! Pu236
       CASE(237)
          nuc_m_excess = 45.093307      ! Pu237
       CASE(238)
          nuc_m_excess = 46.164745      ! Pu238
       CASE(239)
          nuc_m_excess = 48.589877      ! Pu239
       CASE(240)
          nuc_m_excess = 50.126995      ! Pu240
       CASE(241)
          nuc_m_excess = 52.956791      ! Pu241
       CASE(242)
          nuc_m_excess = 54.71839      ! Pu242
       CASE(243)
          nuc_m_excess = 57.755509      ! Pu243
       CASE(244)
          nuc_m_excess = 59.805555      ! Pu244
       CASE(245)
          nuc_m_excess = 63.106068      ! Pu245
       CASE(246)
          nuc_m_excess = 65.39519      ! Pu246
       CASE(247)
          nuc_m_excess = 68.996      ! Pu247
       END SELECT
    CASE (95)
       SELECT CASE (a)
       CASE(231)
          nuc_m_excess = 42.439      ! Am231
       CASE(232)
          nuc_m_excess = 43.398      ! Am232
       CASE(233)
          nuc_m_excess = 43.173      ! Am233
       CASE(234)
          nuc_m_excess = 44.534      ! Am234
       CASE(235)
          nuc_m_excess = 44.662      ! Am235
       CASE(236)
          nuc_m_excess = 46.183      ! Am236
       CASE(237)
          nuc_m_excess = 46.571      ! Am237
       CASE(238)
          nuc_m_excess = 48.423087      ! Am238
       CASE(239)
          nuc_m_excess = 49.391985      ! Am239
       CASE(240)
          nuc_m_excess = 51.511785      ! Am240
       CASE(241)
          nuc_m_excess = 52.936008      ! Am241
       CASE(242)
          nuc_m_excess = 55.469685      ! Am242
       CASE(243)
          nuc_m_excess = 57.176109      ! Am243
       CASE(244)
          nuc_m_excess = 59.880951      ! Am244
       CASE(245)
          nuc_m_excess = 61.899746      ! Am245
       CASE(246)
          nuc_m_excess = 64.99464      ! Am246
       CASE(247)
          nuc_m_excess = 67.154      ! Am247
       CASE(248)
          nuc_m_excess = 70.562      ! Am248
       CASE(249)
          nuc_m_excess = 73.104      ! Am249
       END SELECT
    CASE (96)
       SELECT CASE (a)
       CASE(233)
          nuc_m_excess = 47.293098      ! Cm233
       CASE(234)
          nuc_m_excess = 46.723592      ! Cm234
       CASE(235)
          nuc_m_excess = 47.91      ! Cm235
       CASE(236)
          nuc_m_excess = 47.89      ! Cm236
       CASE(237)
          nuc_m_excess = 49.277      ! Cm237
       CASE(238)
          nuc_m_excess = 49.395914      ! Cm238
       CASE(239)
          nuc_m_excess = 51.192      ! Cm239
       CASE(240)
          nuc_m_excess = 51.725434      ! Cm240
       CASE(241)
          nuc_m_excess = 53.703425      ! Cm241
       CASE(242)
          nuc_m_excess = 54.805218      ! Cm242
       CASE(243)
          nuc_m_excess = 57.183593      ! Cm243
       CASE(244)
          nuc_m_excess = 58.453651      ! Cm244
       CASE(245)
          nuc_m_excess = 61.004706      ! Cm245
       CASE(246)
          nuc_m_excess = 62.618439      ! Cm246
       CASE(247)
          nuc_m_excess = 65.533901      ! Cm247
       CASE(248)
          nuc_m_excess = 67.392202      ! Cm248
       CASE(249)
          nuc_m_excess = 70.75015      ! Cm249
       CASE(250)
          nuc_m_excess = 72.989038      ! Cm250
       CASE(251)
          nuc_m_excess = 76.647617      ! Cm251
       CASE(252)
          nuc_m_excess = 79.056      ! Cm252
       END SELECT
    CASE (97)
       SELECT CASE (a)
       CASE(235)
          nuc_m_excess = 52.704      ! Bk235
       CASE(236)
          nuc_m_excess = 53.403      ! Bk236
       CASE(237)
          nuc_m_excess = 53.098      ! Bk237
       CASE(238)
          nuc_m_excess = 54.288      ! Bk238
       CASE(239)
          nuc_m_excess = 54.287      ! Bk239
       CASE(240)
          nuc_m_excess = 55.665      ! Bk240
       CASE(241)
          nuc_m_excess = 56.103      ! Bk241
       CASE(242)
          nuc_m_excess = 57.735      ! Bk242
       CASE(243)
          nuc_m_excess = 58.691176      ! Bk243
       CASE(244)
          nuc_m_excess = 60.715501      ! Bk244
       CASE(245)
          nuc_m_excess = 61.815448      ! Bk245
       CASE(246)
          nuc_m_excess = 63.968439      ! Bk246
       CASE(247)
          nuc_m_excess = 65.490624      ! Bk247
       CASE(248)
          nuc_m_excess = 68.08      ! Bk248
       CASE(249)
          nuc_m_excess = 69.849622      ! Bk249
       CASE(250)
          nuc_m_excess = 72.95137      ! Bk250
       CASE(251)
          nuc_m_excess = 75.227617      ! Bk251
       CASE(252)
          nuc_m_excess = 78.534      ! Bk252
       CASE(253)
          nuc_m_excess = 80.929      ! Bk253
       CASE(254)
          nuc_m_excess = 84.393      ! Bk254
       END SELECT
    CASE (98)
       SELECT CASE (a)
       CASE(237)
          nuc_m_excess = 57.818      ! Cf237
       CASE(238)
          nuc_m_excess = 57.203      ! Cf238
       CASE(239)
          nuc_m_excess = 58.145      ! Cf239
       CASE(240)
          nuc_m_excess = 58.034      ! Cf240
       CASE(241)
          nuc_m_excess = 59.361      ! Cf241
       CASE(242)
          nuc_m_excess = 59.337614      ! Cf242
       CASE(243)
          nuc_m_excess = 60.945      ! Cf243
       CASE(244)
          nuc_m_excess = 61.479247      ! Cf244
       CASE(245)
          nuc_m_excess = 63.386875      ! Cf245
       CASE(246)
          nuc_m_excess = 64.091733      ! Cf246
       CASE(247)
          nuc_m_excess = 66.136624      ! Cf247
       CASE(248)
          nuc_m_excess = 67.239766      ! Cf248
       CASE(249)
          nuc_m_excess = 69.725622      ! Cf249
       CASE(250)
          nuc_m_excess = 71.171793      ! Cf250
       CASE(251)
          nuc_m_excess = 74.134617      ! Cf251
       CASE(252)
          nuc_m_excess = 76.033987      ! Cf252
       CASE(253)
          nuc_m_excess = 79.301015      ! Cf253
       CASE(254)
          nuc_m_excess = 81.340767      ! Cf254
       CASE(255)
          nuc_m_excess = 84.809      ! Cf255
       CASE(256)
          nuc_m_excess = 87.039      ! Cf256
       END SELECT
    CASE (99)
       SELECT CASE (a)
       CASE(240)
          nuc_m_excess = 64.199      ! Es240
       CASE(241)
          nuc_m_excess = 63.843      ! Es241
       CASE(242)
          nuc_m_excess = 64.967      ! Es242
       CASE(243)
          nuc_m_excess = 64.783      ! Es243
       CASE(244)
          nuc_m_excess = 66.027      ! Es244
       CASE(245)
          nuc_m_excess = 66.438      ! Es245
       CASE(246)
          nuc_m_excess = 67.902      ! Es246
       CASE(247)
          nuc_m_excess = 68.61      ! Es247
       CASE(248)
          nuc_m_excess = 70.301      ! Es248
       CASE(249)
          nuc_m_excess = 71.176      ! Es249
       CASE(250)
          nuc_m_excess = 73.227      ! Es250
       CASE(251)
          nuc_m_excess = 74.512202      ! Es251
       CASE(252)
          nuc_m_excess = 77.293987      ! Es252
       CASE(253)
          nuc_m_excess = 79.013697      ! Es253
       CASE(254)
          nuc_m_excess = 81.991986      ! Es254
       CASE(255)
          nuc_m_excess = 84.088873      ! Es255
       CASE(256)
          nuc_m_excess = 87.186      ! Es256
       CASE(257)
          nuc_m_excess = 89.403      ! Es257
       CASE(258)
          nuc_m_excess = 92.702      ! Es258
       END SELECT
    CASE (100)
       SELECT CASE (a)
       CASE(242)
          nuc_m_excess = 68.4      ! Fm242
       CASE(243)
          nuc_m_excess = 69.259      ! Fm243
       CASE(244)
          nuc_m_excess = 69.009      ! Fm244
       CASE(245)
          nuc_m_excess = 70.221      ! Fm245
       CASE(246)
          nuc_m_excess = 70.140589      ! Fm246
       CASE(247)
          nuc_m_excess = 71.583      ! Fm247
       CASE(248)
          nuc_m_excess = 71.906414      ! Fm248
       CASE(249)
          nuc_m_excess = 73.619      ! Fm249
       CASE(250)
          nuc_m_excess = 74.073581      ! Fm250
       CASE(251)
          nuc_m_excess = 75.98664      ! Fm251
       CASE(252)
          nuc_m_excess = 76.817382      ! Fm252
       CASE(253)
          nuc_m_excess = 79.349538      ! Fm253
       CASE(254)
          nuc_m_excess = 80.904186      ! Fm254
       CASE(255)
          nuc_m_excess = 83.799253      ! Fm255
       CASE(256)
          nuc_m_excess = 85.486109      ! Fm256
       CASE(257)
          nuc_m_excess = 88.589481      ! Fm257
       CASE(258)
          nuc_m_excess = 90.426      ! Fm258
       CASE(259)
          nuc_m_excess = 93.704      ! Fm259
       CASE(260)
          nuc_m_excess = 95.644      ! Fm260
       END SELECT
    CASE (101)
       SELECT CASE (a)
       CASE(245)
          nuc_m_excess = 75.292      ! Md245
       CASE(246)
          nuc_m_excess = 76.276      ! Md246
       CASE(247)
          nuc_m_excess = 76.043      ! Md247
       CASE(248)
          nuc_m_excess = 77.149      ! Md248
       CASE(249)
          nuc_m_excess = 77.326      ! Md249
       CASE(250)
          nuc_m_excess = 78.636      ! Md250
       CASE(251)
          nuc_m_excess = 79.027      ! Md251
       CASE(252)
          nuc_m_excess = 80.63      ! Md252
       CASE(253)
          nuc_m_excess = 81.301      ! Md253
       CASE(254)
          nuc_m_excess = 83.514      ! Md254
       CASE(255)
          nuc_m_excess = 84.842994      ! Md255
       CASE(256)
          nuc_m_excess = 87.615418      ! Md256
       CASE(257)
          nuc_m_excess = 88.996213      ! Md257
       CASE(258)
          nuc_m_excess = 91.688184      ! Md258
       CASE(259)
          nuc_m_excess = 93.624      ! Md259
       CASE(260)
          nuc_m_excess = 96.551      ! Md260
       CASE(261)
          nuc_m_excess = 98.478      ! Md261
       CASE(262)
          nuc_m_excess = 101.407      ! Md262
       END SELECT
    CASE (102)
       SELECT CASE (a)
       CASE(248)
          nuc_m_excess = 80.664      ! No248
       CASE(249)
          nuc_m_excess = 81.816      ! No249
       CASE(250)
          nuc_m_excess = 81.516      ! No250
       CASE(251)
          nuc_m_excess = 82.914      ! No251
       CASE(252)
          nuc_m_excess = 82.881097      ! No252
       CASE(253)
          nuc_m_excess = 84.466      ! No253
       CASE(254)
          nuc_m_excess = 84.724274      ! No254
       CASE(255)
          nuc_m_excess = 86.853555      ! No255
       CASE(256)
          nuc_m_excess = 87.823739      ! No256
       CASE(257)
          nuc_m_excess = 90.240522      ! No257
       CASE(258)
          nuc_m_excess = 91.479      ! No258
       CASE(259)
          nuc_m_excess = 94.109      ! No259
       CASE(260)
          nuc_m_excess = 95.611      ! No260
       CASE(261)
          nuc_m_excess = 98.504      ! No261
       CASE(262)
          nuc_m_excess = 99.951      ! No262
       CASE(263)
          nuc_m_excess = 102.979      ! No263
       CASE(264)
          nuc_m_excess = 104.649      ! No264
       END SELECT
    CASE (103)
       SELECT CASE (a)
       CASE(251)
          nuc_m_excess = 87.896      ! Lr251
       CASE(252)
          nuc_m_excess = 88.837      ! Lr252
       CASE(253)
          nuc_m_excess = 88.687      ! Lr253
       CASE(254)
          nuc_m_excess = 89.847      ! Lr254
       CASE(255)
          nuc_m_excess = 90.057      ! Lr255
       CASE(256)
          nuc_m_excess = 91.872      ! Lr256
       CASE(257)
          nuc_m_excess = 92.735      ! Lr257
       CASE(258)
          nuc_m_excess = 94.839      ! Lr258
       CASE(259)
          nuc_m_excess = 95.852      ! Lr259
       CASE(260)
          nuc_m_excess = 98.276      ! Lr260
       CASE(261)
          nuc_m_excess = 99.561      ! Lr261
       CASE(262)
          nuc_m_excess = 102.123      ! Lr262
       CASE(263)
          nuc_m_excess = 103.669      ! Lr263
       CASE(264)
          nuc_m_excess = 106.226      ! Lr264
       CASE(265)
          nuc_m_excess = 107.903      ! Lr265
       CASE(266)
          nuc_m_excess = 111.132      ! Lr266
       END SELECT
    CASE (104)
       SELECT CASE (a)
       CASE(253)
          nuc_m_excess = 93.791      ! Rf253
       CASE(254)
          nuc_m_excess = 93.32      ! Rf254
       CASE(255)
          nuc_m_excess = 94.397      ! Rf255
       CASE(256)
          nuc_m_excess = 94.235704      ! Rf256
       CASE(257)
          nuc_m_excess = 95.934      ! Rf257
       CASE(258)
          nuc_m_excess = 96.399      ! Rf258
       CASE(259)
          nuc_m_excess = 98.4      ! Rf259
       CASE(260)
          nuc_m_excess = 99.149      ! Rf260
       CASE(261)
          nuc_m_excess = 101.315395      ! Rf261
       CASE(262)
          nuc_m_excess = 102.394      ! Rf262
       CASE(263)
          nuc_m_excess = 104.837      ! Rf263
       CASE(264)
          nuc_m_excess = 106.176      ! Rf264
       CASE(265)
          nuc_m_excess = 108.709      ! Rf265
       CASE(266)
          nuc_m_excess = 109.876      ! Rf266
       CASE(267)
          nuc_m_excess = 113.204      ! Rf267
       CASE(268)
          nuc_m_excess = 115.174      ! Rf268
       END SELECT
    CASE (105)
       SELECT CASE (a)
       CASE(255)
          nuc_m_excess = 100.041      ! Db255
       CASE(256)
          nuc_m_excess = 100.72      ! Db256
       CASE(257)
          nuc_m_excess = 100.342      ! Db257
       CASE(258)
          nuc_m_excess = 101.748      ! Db258
       CASE(259)
          nuc_m_excess = 102.101      ! Db259
       CASE(260)
          nuc_m_excess = 103.676      ! Db260
       CASE(261)
          nuc_m_excess = 104.379      ! Db261
       CASE(262)
          nuc_m_excess = 106.269      ! Db262
       CASE(263)
          nuc_m_excess = 107.111      ! Db263
       CASE(264)
          nuc_m_excess = 109.361      ! Db264
       CASE(265)
          nuc_m_excess = 110.476      ! Db265
       CASE(266)
          nuc_m_excess = 112.738      ! Db266
       CASE(267)
          nuc_m_excess = 113.994      ! Db267
       CASE(268)
          nuc_m_excess = 116.851      ! Db268
       CASE(269)
          nuc_m_excess = 118.728      ! Db269
       CASE(270)
          nuc_m_excess = 121.757      ! Db270
       END SELECT
    CASE (106)
       SELECT CASE (a)
       CASE(258)
          nuc_m_excess = 105.415      ! Sg258
       CASE(259)
          nuc_m_excess = 106.656      ! Sg259
       CASE(260)
          nuc_m_excess = 106.583474      ! Sg260
       CASE(261)
          nuc_m_excess = 108.162      ! Sg261
       CASE(262)
          nuc_m_excess = 108.424      ! Sg262
       CASE(263)
          nuc_m_excess = 110.216      ! Sg263
       CASE(264)
          nuc_m_excess = 110.784      ! Sg264
       CASE(265)
          nuc_m_excess = 112.817611      ! Sg265
       CASE(266)
          nuc_m_excess = 113.703      ! Sg266
       CASE(267)
          nuc_m_excess = 115.901      ! Sg267
       CASE(268)
          nuc_m_excess = 117.001      ! Sg268
       CASE(269)
          nuc_m_excess = 119.934      ! Sg269
       CASE(270)
          nuc_m_excess = 121.4      ! Sg270
       CASE(271)
          nuc_m_excess = 124.329      ! Sg271
       CASE(272)
          nuc_m_excess = 125.898      ! Sg272
       CASE(273)
          nuc_m_excess = 128.751      ! Sg273
       END SELECT
    CASE (107)
       SELECT CASE (a)
       CASE(260)
          nuc_m_excess = 113.614      ! Bh260
       CASE(261)
          nuc_m_excess = 113.329      ! Bh261
       CASE(262)
          nuc_m_excess = 114.473      ! Bh262
       CASE(263)
          nuc_m_excess = 114.606      ! Bh263
       CASE(264)
          nuc_m_excess = 116.068      ! Bh264
       CASE(265)
          nuc_m_excess = 116.574      ! Bh265
       CASE(266)
          nuc_m_excess = 118.246      ! Bh266
       CASE(267)
          nuc_m_excess = 118.906      ! Bh267
       CASE(268)
          nuc_m_excess = 120.866      ! Bh268
       CASE(269)
          nuc_m_excess = 121.741      ! Bh269
       CASE(270)
          nuc_m_excess = 124.463      ! Bh270
       CASE(271)
          nuc_m_excess = 125.919      ! Bh271
       CASE(272)
          nuc_m_excess = 128.576      ! Bh272
       CASE(273)
          nuc_m_excess = 130.053      ! Bh273
       CASE(274)
          nuc_m_excess = 132.682      ! Bh274
       CASE(275)
          nuc_m_excess = 134.368      ! Bh275
       END SELECT
    CASE (108)
       SELECT CASE (a)
       CASE(263)
          nuc_m_excess = 119.751      ! Hs263
       CASE(264)
          nuc_m_excess = 119.599066      ! Hs264
       CASE(265)
          nuc_m_excess = 121.173      ! Hs265
       CASE(266)
          nuc_m_excess = 121.185      ! Hs266
       CASE(267)
          nuc_m_excess = 122.761      ! Hs267
       CASE(268)
          nuc_m_excess = 123.108      ! Hs268
       CASE(269)
          nuc_m_excess = 124.872      ! Hs269
       CASE(270)
          nuc_m_excess = 125.426      ! Hs270
       CASE(271)
          nuc_m_excess = 128.226      ! Hs271
       CASE(272)
          nuc_m_excess = 129.526      ! Hs272
       CASE(273)
          nuc_m_excess = 132.259      ! Hs273
       CASE(274)
          nuc_m_excess = 133.325      ! Hs274
       CASE(275)
          nuc_m_excess = 135.953      ! Hs275
       CASE(276)
          nuc_m_excess = 137.123      ! Hs276
       CASE(277)
          nuc_m_excess = 139.576      ! Hs277
       END SELECT
    CASE (109)
       SELECT CASE (a)
       CASE(265)
          nuc_m_excess = 126.824      ! Mt265
       CASE(266)
          nuc_m_excess = 127.893      ! Mt266
       CASE(267)
          nuc_m_excess = 127.901      ! Mt267
       CASE(268)
          nuc_m_excess = 129.224      ! Mt268
       CASE(269)
          nuc_m_excess = 129.529      ! Mt269
       CASE(270)
          nuc_m_excess = 131.021      ! Mt270
       CASE(271)
          nuc_m_excess = 131.47      ! Mt271
       CASE(272)
          nuc_m_excess = 133.891      ! Mt272
       CASE(273)
          nuc_m_excess = 134.986      ! Mt273
       CASE(274)
          nuc_m_excess = 137.388      ! Mt274
       CASE(275)
          nuc_m_excess = 138.463      ! Mt275
       CASE(276)
          nuc_m_excess = 140.801      ! Mt276
       CASE(277)
          nuc_m_excess = 141.978      ! Mt277
       CASE(278)
          nuc_m_excess = 144.207      ! Mt278
       CASE(279)
          nuc_m_excess = 145.493      ! Mt279
       END SELECT
    CASE (110)
       SELECT CASE (a)
       CASE(267)
          nuc_m_excess = 134.453      ! Ea267
       CASE(268)
          nuc_m_excess = 133.944      ! Ea268
       CASE(269)
          nuc_m_excess = 135.183      ! Ea269
       CASE(270)
          nuc_m_excess = 134.806      ! Ea270
       CASE(271)
          nuc_m_excess = 136.056      ! Ea271
       CASE(272)
          nuc_m_excess = 136.293      ! Ea272
       CASE(273)
          nuc_m_excess = 138.665      ! Ea273
       CASE(274)
          nuc_m_excess = 139.251      ! Ea274
       CASE(275)
          nuc_m_excess = 141.751      ! Ea275
       CASE(276)
          nuc_m_excess = 142.551      ! Ea276
       CASE(277)
          nuc_m_excess = 144.984      ! Ea277
       CASE(278)
          nuc_m_excess = 145.75      ! Ea278
       CASE(279)
          nuc_m_excess = 147.978      ! Ea279
       CASE(280)
          nuc_m_excess = 148.848      ! Ea280
       CASE(281)
          nuc_m_excess = 150.959      ! Ea281
       END SELECT
    CASE (111)
       SELECT CASE (a)
       CASE(272)
          nuc_m_excess = 143.091      ! Eb272
       CASE(273)
          nuc_m_excess = 143.154      ! Eb273
       CASE(274)
          nuc_m_excess = 145.046      ! Eb274
       CASE(275)
          nuc_m_excess = 145.445      ! Eb275
       CASE(276)
          nuc_m_excess = 147.636      ! Eb276
       CASE(277)
          nuc_m_excess = 148.591      ! Eb277
       CASE(278)
          nuc_m_excess = 150.533      ! Eb278
       CASE(279)
          nuc_m_excess = 151.338      ! Eb279
       CASE(280)
          nuc_m_excess = 153.206      ! Eb280
       CASE(281)
          nuc_m_excess = 154.043      ! Eb281
       CASE(282)
          nuc_m_excess = 156.012      ! Eb282
       CASE(283)
          nuc_m_excess = 156.878      ! Eb283
       END SELECT
    CASE (112)
       SELECT CASE (a)
       CASE(277)
          nuc_m_excess = 152.712      ! Ec277
       CASE(278)
          nuc_m_excess = 153.056      ! Ec278
       CASE(279)
          nuc_m_excess = 155.136      ! Ec279
       CASE(280)
          nuc_m_excess = 155.596      ! Ec280
       CASE(281)
          nuc_m_excess = 157.689      ! Ec281
       CASE(282)
          nuc_m_excess = 158.135      ! Ec282
       CASE(283)
          nuc_m_excess = 160.023      ! Ec283
       CASE(284)
          nuc_m_excess = 160.574      ! Ec284
       CASE(285)
          nuc_m_excess = 162.177      ! Ec285
       END SELECT
    CASE (113)
       SELECT CASE (a)
       CASE(283)
          nuc_m_excess = 164.363      ! Ed283
       CASE(284)
          nuc_m_excess = 165.881      ! Ed284
       CASE(285)
          nuc_m_excess = 166.488      ! Ed285
       CASE(286)
          nuc_m_excess = 168.117      ! Ed286
       CASE(287)
          nuc_m_excess = 168.643      ! Ed287
       END SELECT
    CASE (114)
       SELECT CASE (a)
       CASE(285)
          nuc_m_excess = 171.114      ! Ee285
       CASE(286)
          nuc_m_excess = 171.26      ! Ee286
       CASE(287)
          nuc_m_excess = 172.884      ! Ee287
       CASE(288)
          nuc_m_excess = 172.968      ! Ee288
       CASE(289)
          nuc_m_excess = 174.449      ! Ee289
       END SELECT
    CASE (115)
       SELECT CASE (a)
       CASE(287)
          nuc_m_excess = 178.088      ! Ef287
       CASE(288)
          nuc_m_excess = 179.305      ! Ef288
       CASE(289)
          nuc_m_excess = 179.513      ! Ef289
       CASE(290)
          nuc_m_excess = 180.842      ! Ef290
       CASE(291)
          nuc_m_excess = 181.068      ! Ef291
       END SELECT
    CASE (116)
       SELECT CASE (a)
       CASE(289)
          nuc_m_excess = 185.239      ! Eg289
       CASE(290)
          nuc_m_excess = 184.985      ! Eg290
       CASE(291)
          nuc_m_excess = 186.309      ! Eg291
       CASE(292)
          nuc_m_excess = 186.1      ! Eg292
       END SELECT
    CASE (117)
       SELECT CASE (a)
       CASE(291)
          nuc_m_excess = 192.413      ! Eh291
       CASE(292)
          nuc_m_excess = 193.33      ! Eh292
       END SELECT
    CASE (118)
       SELECT CASE (a)
       CASE(293)
          nuc_m_excess = 199.964      ! Ei293
       END SELECT
    END SELECT
  END FUNCTION nuc_m_excess

END MODULE nuc_data

