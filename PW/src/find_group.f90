!
! Copyright (C) 2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!---------------------------------------------------------------------------
SUBROUTINE find_group( nrot, smat, gname, code_group )
  !--------------------------------------------------------------------------
  !! Given a group of \(\text{nrot}\) rotation matrices \(\text{smat}\) (in 
  !! cartesian coordinates) this routine finds the name of the point group.
  !! It assumes but does not check that:
  !
  !! * the \(\text{nrot}\) matrices \(\text{smat}\) are actually a group;
  !! * the group is one of the 32 point groups.
  !
  USE kinds, ONLY : DP
  !
  IMPLICIT NONE
  !
  INTEGER :: nrot
  !! number of rotation matrices
  INTEGER :: code_group
  !! code that identifies the group
  REAL(DP) :: smat(3,3,nrot)
  !! rotation matrices in cartesian coordinates
  CHARACTER (LEN=11) :: gname
  !! name of the group
  !
  ! ... local variables
  !
  CHARACTER(LEN=11) :: group_name
  INTEGER :: noperation(6), irot, ts, tipo_sym
  !
  ! For each possible group operation the function tipo_sym gives a code
  !   1 identity, 
  !   2 inversion, 
  !   3 proper rotation <> 180, 
  !   4 proper rotation 180 degrees, 
  !   5 mirror, 
  !   6 improper rotation
  ! the variable noperation counts how many operations are present in the group.
  !
  noperation=0
  DO irot = 1, nrot
     ts = tipo_sym(smat(1,1,irot))
     noperation(ts) = noperation(ts)+1
  ENDDO
  !
  IF (noperation(1) /= 1) CALL errore( 'find_group', 'the group has not identity', 1 )
  !
  code_group=0
  !
  IF (noperation(2)==0) THEN
     !
     !  There is not inversion
     !
     SELECT CASE( nrot )
     CASE( 1 )
        code_group=1                                          ! C_1
     CASE( 2 )
        IF (noperation(4)==1) code_group=4                    ! C_2
        IF (noperation(5)==1) code_group=3                    ! C_s
     CASE( 3 )
        IF (noperation(3)==2) code_group=5                    ! C_3
     CASE( 4 )
        IF (noperation(6)>0)  code_group=26                   ! S_4
        IF (noperation(5)>0.AND.code_group==0) code_group=12  ! C_2v
        IF (noperation(3)>0.AND.code_group==0) code_group=6   ! C_4
        IF (noperation(4)>0.AND.code_group==0) code_group=8   ! D_2
     CASE( 6 )
        IF (noperation(5)==3) code_group=13                   ! C_3v
        IF (noperation(5)==1) code_group=17                   ! C_3h
        IF (noperation(4)==3.AND.code_group==0) code_group=9  ! D_3
        IF (noperation(3)> 0.AND.code_group==0) code_group=7  ! C_6
     CASE( 8 )
        IF (noperation(5)==4) code_group=14                   ! C_4v
        IF (noperation(5)==2) code_group=24                   ! D_2d
        IF (noperation(3)>0.AND.code_group==0) code_group=10  ! D_4
     CASE( 12 )
        IF (noperation(5)==6) code_group=15                   ! C_6v
        IF (noperation(5)==4) code_group=21                   ! D_3h
        IF (noperation(4)>6.AND.code_group==0) code_group=11  ! D_6
        IF (noperation(3)>0.AND.code_group==0) code_group=28  ! T
     CASE( 24 )
        IF (noperation(5)>0) code_group=30                    ! T_d
        IF (noperation(5)==0) code_group=31                   ! O
     CASE DEFAULT
        CALL errore( 'find_group','wrong number of elements', 1 )
     END SELECT
     !
  ELSEIF (noperation(2)==1) THEN
     !
     !  There is inversion
     !
     SELECT CASE( nrot )
     CASE( 2 )
        code_group=2                                          ! C_i
     CASE( 4 )
        code_group=16                                         ! C_2h
     CASE( 6 )
        code_group=27                                         ! S_6
     CASE( 8 )
        IF (noperation(5)==3) code_group=20                   ! D_2h
        IF (noperation(5)==1) code_group=18                   ! C_4h
     CASE( 12 )
        IF (noperation(5)==3) code_group=25                   ! D_3d
        IF (noperation(5)==1) code_group=19                   ! C_6h
     CASE( 16 )
        IF (noperation(5)==5) code_group=22                   ! D_4h
     CASE( 24 )
        IF (noperation(5)>6)  code_group=23                   ! D_6h
        IF (noperation(5)==3) code_group=29                   ! T_h
     CASE( 48 )
        code_group=32                                         ! O_h
     CASE DEFAULT
        CALL errore( 'find_group', 'wrong number of elements', 1 )
     END SELECT
  ELSE
     CALL errore( 'find_group', 'too many inversions', 1 )
  ENDIF
  !
  IF (code_group==0) CALL errore( 'find_group', 'incompatible operations', 1 )
  !
  gname = group_name( code_group )
  !
  RETURN
  !
END SUBROUTINE find_group
!
!
!--------------------------------------------------------------------------
FUNCTION group_name( code )
  !--------------------------------------------------------------------------
  !! This function receives a code of the group and provides the name of the
  !! group. The order is the following:
  !
  !! \begin{array}{|r|r|r|r|r|r|}
  !! \hline
  !! 1  & C_1 & 12 & C_2v & 23 & D_6h \\
  !! \hline
  !! 2  & C_i & 13 & C_3v & 24 & D_2d \\
  !! \hline
  !! 3  & C_s & 14 & C_4v & 25 & D_3d \\
  !! \hline
  !! 4  & C_2 & 15 & C_6v & 26 & S_4  \\
  !! \hline
  !! 5  & C_3 & 16 & C_2h & 27 & S_6  \\
  !! \hline
  !! 6  & C_4 & 17 & C_3h & 28 & T    \\
  !! \hline
  !! 7  & C_6 & 18 & C_4h & 29 & T_h  \\
  !! \hline
  !! 8  & D_2 & 19 & C_6h & 30 & T_d  \\
  !! \hline
  !! 9  & D_3 & 20 & D_2h & 31 & O    \\
  !! \hline
  !! 10 & D_4 & 21 & D_3h & 32 & O_h  \\
  !! \hline
  !! 11 & D_6 & 22 & D_4h &    &      \\
  !! \hline
  !! \end{array}
  !
  IMPLICIT NONE
  !
  INTEGER :: code
  !! input: code of the group
  CHARACTER(LEN=11) :: group_name
  !! output: name of the group
  !
  ! ... local variables
  !
  CHARACTER(LEN=11) :: gname(32)
  DATA gname  / "C_1 (1)    ", "C_i (-1)   ", "C_s (m)    ", "C_2  (2)   ", &
                "C_3 (3)    ", "C_4 (4)    ", "C_6 (6)    ", "D_2  (222) ", &
                "D_3 (32)   ", "D_4 (422)  ", "D_6 (622)  ", "C_2v (mm2) ", &
                "C_3v (3m)  ", "C_4v (4mm) ", "C_6v (6mm) ", "C_2h (2/m) ", &
                "C_3h (-6)  ", "C_4h (4/m) ", "C_6h (6/m) ", "D_2h (mmm) ", &
                "D_3h (-62m)", "D_4h(4/mmm)", "D_6h(6/mmm)", "D_2d (-42m)", &
                "D_3d (-3m) ", "S_4 (-4)   ", "S_6 (-3)   ", "T    (23)  ", & 
                "T_h (m-3)  ", "T_d (-43m) ", "O   (432)  ", "O_h (m-3m) "  /
  !
  IF (code < 1 .OR. code > 32 ) CALL errore( 'group_name', 'code is out of range', 1 )
  !
  group_name = gname(code)
  !
  RETURN
  !
END FUNCTION group_name
!
!
!--------------------------------------------------------------------------
FUNCTION tipo_sym( s )
  !--------------------------------------------------------------------------
  !! This function receives a 3x3 orthogonal matrix which is a symmetry 
  !! operation of the point group of the crystal written in cartesian 
  !! coordinates and gives as output a code according to the following:  
  !! 1 - identity;  
  !! 2 - inversion;  
  !! 3 - proper rotation of an angle <> 180 degrees;  
  !! 4 - proper rotation of 180 degrees;  
  !! 5 - mirror symmetry;  
  !! 6 - improper rotation.
  !
  USE kinds, ONLY : DP
  !
  IMPLICIT NONE
  !
  REAL(DP) :: s(3,3)
  !! input: see the function main comments
  INTEGER  :: tipo_sym
  !! output: see the function main comments
  !
  ! ... local variables
  !
  REAL(DP), PARAMETER :: eps=1.d-7
  REAL(DP) :: det, det1
  !
  ! Check for identity
  !
  IF ((ABS(s(1,1)-1.d0) < eps).AND. &
      (ABS(s(2,2)-1.d0) < eps).AND. &
      (ABS(s(3,3)-1.d0) < eps).AND. &
      (ABS(s(1,2)) < eps).AND.(ABS(s(2,1)) < eps).AND.(ABS(s(2,3)) < eps).AND. &
      (ABS(s(3,2)) < eps).AND.(ABS(s(1,3)) < eps).AND.(ABS(s(3,1)) < eps)) THEN
     tipo_sym=1
     RETURN
  ENDIF
  !
  ! Check for inversion
  !
  IF ((ABS(s(1,1)+1.d0) < eps).AND. &
      (ABS(s(2,2)+1.d0) < eps).AND. &
      (ABS(s(3,3)+1.d0) < eps).AND. &
      (ABS(s(1,2)) < eps).AND.(ABS(s(2,1)) < eps).AND.(ABS(s(2,3)) < eps).AND. &
      (ABS(s(3,2)) < eps).AND.(ABS(s(1,3)) < eps).AND.(ABS(s(3,1)) < eps)) THEN
     tipo_sym=2
     RETURN
  ENDIF
  !
  ! compute the determinant
  !
  det = s(1,1) * ( s(2,2) * s(3,3) - s(3,2) * s(2,3) ) -  &
        s(1,2) * ( s(2,1) * s(3,3) - s(3,1) * s(2,3) ) +  &
        s(1,3) * ( s(2,1) * s(3,2) - s(3,1) * s(2,2) ) 
  !
  ! Determinant equal to 1: proper rotation
  !
  IF (ABS(det-1.d0) < eps) THEN
     !
     !  check if an eigenvalue is equal to -1.d0 (180 rotation)
     !
     det1=(s(1,1)+1.d0)*((s(2,2)+1.d0)*(s(3,3)+1.d0)-s(3,2)*s(2,3)) -  &
           s(1,2)*       (s(2,1)*      (s(3,3)+1.d0)-s(3,1)*s(2,3)) +  &
           s(1,3)*       (s(2,1)*s(3,2)             -s(3,1)*(s(2,2)+1.d0)) 

     IF (ABS(det1) < eps) THEN
        tipo_sym = 4     ! 180 proper rotation
     ELSE
        tipo_sym = 3     ! proper rotation <> 180
     ENDIF
     RETURN
  ENDIF
  !
  ! Determinant equal to -1: mirror symmetry or improper rotation
  !
  IF (ABS(det+1.d0) < eps) THEN
     !
     !  check if an eigenvalue is equal to 1.d0 (mirror symmetry)
     !
     det1=(s(1,1)-1.d0)*((s(2,2)-1.d0)*(s(3,3)-1.d0)-s(3,2)*s(2,3)) -  &
           s(1,2)*       (s(2,1)*      (s(3,3)-1.d0)-s(3,1)*s(2,3)) +  &
           s(1,3)*       (s(2,1)*s(3,2)             -s(3,1)*(s(2,2)-1.d0)) 

     IF (ABS(det1) < eps) THEN
        tipo_sym=5   ! mirror symmetry
     ELSE
        tipo_sym=6   ! improper rotation
     ENDIF
     !
     RETURN
     !
  ELSE
     CALL errore( 'tipo_sym', 'symmetry not recognized', 1 )
  ENDIF
  !
END FUNCTION tipo_sym
!
!--------------------------------------------------------------------------
FUNCTION laue_class( code )
!--------------------------------------------------------------------------
  !! This function receives a code of the point group and provides the
  !! code of the point group that defines the Laue class (that is the point 
  !! group obtained by multipling by inversion).  
  !! The order is the following:  
  !
  !! \begin{array}{|r|c|r|c|r|c|}
  !! \hline
  !! 1  & C_1\rightarrow 2  & 12 & C_2v\rightarrow 20 & 23 & D_6h\rightarrow 23 \\
  !! \hline
  !! 2  & C_i\rightarrow 2  & 13 & C_3v\rightarrow 25 & 24 & D_2d\rightarrow 22 \\
  !! \hline
  !! 3  & C_s\rightarrow 16 & 14 & C_4v\rightarrow 22 & 25 & D_3d\rightarrow 25 \\
  !! \hline
  !! 4  & C_2\rightarrow 16 & 15 & C_6v\rightarrow 23 & 26 & S_4\rightarrow  18 \\
  !! \hline
  !! 5  & C_3\rightarrow 27 & 16 & C_2h\rightarrow 16 & 27 & S_6\rightarrow  27 \\
  !! \hline
  !! 6  & C_4\rightarrow 18 & 17 & C_3h\rightarrow 19 & 28 & T\rightarrow    29 \\
  !! \hline
  !! 7  & C_6\rightarrow 19 & 18 & C_4h\rightarrow 18 & 29 & T_h\rightarrow  29 \\
  !! \hline
  !! 8  & D_2\rightarrow 20 & 19 & C_6h\rightarrow 19 & 30 & T_d\rightarrow  32 \\
  !! \hline
  !! 9  & D_3\rightarrow 25 & 20 & D_2h\rightarrow 20 & 31 & O\rightarrow    32 \\
  !! \hline
  !! 10 & D_4\rightarrow 22 & 21 & D_3h\rightarrow 23 & 32 & O_h\rightarrow  32 \\
  !! \hline
  !! 11 & D_6\rightarrow 23 & 22 & D_4h\rightarrow 22 &    &                    \\
  !! \hline
  !! \end{array}
  !
  IMPLICIT NONE
  !
  INTEGER :: code
  !! code of the point group
  INTEGER :: laue_class
  !! code of the point group that defines the Laue class
  !
  ! ... local variables
  !
  INTEGER :: laue(32)
  !
  DATA  laue  / 2,   2, 16, 16, 27, 18, 19, 20, 25, 22,  &
                23, 20, 25, 22, 23, 16, 19, 18, 19, 20,  &
                23, 22, 23, 22, 25, 18, 27, 29, 29, 32,  &
                32, 32 /
  !
  IF (code<1 .OR. code>32) CALL errore( 'laue_class', 'code is out of range', 1 )
  !
  laue_class = laue(code)
  !
  RETURN
  !
END FUNCTION laue_class

