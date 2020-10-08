!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!---------------------------------------------------------------
SUBROUTINE d_matrix( dy1, dy2, dy3 )
  !---------------------------------------------------------------
  !! Calculates the d-matrices.
  !
  USE kinds,            ONLY: DP
  USE symm_base,        ONLY: nsym, sr
  USE random_numbers,   ONLY: randy
  USE matrix_inversion
  !
  IMPLICIT NONE
  !
  REAL(DP) :: dy1(3,3,48)
  !! first d_matrix
  REAL(DP) :: dy2(5,5,48)
  !! second d_matrix
  REAL(DP) :: dy3(7,7,48)
  !! third d_matrix
  !
  ! ... local variables
  !
  INTEGER, PARAMETER :: maxl = 3, maxm = 2*maxl+1, &
                        maxlm = (maxl+1)*(maxl+1)
  ! maxl = max value of l allowed
  ! maxm = number of m components for l=maxl
  ! maxlm= number of l,m spherical harmonics for l <= maxl
  INTEGER :: m, n, isym
  REAL(DP) :: ylm(maxm,maxlm), yl1(3,3), yl2(5,5), yl3(7,7), &
              yl1_inv(3,3), yl2_inv(5,5), yl3_inv(7,7),      &
              ylms(maxm,maxlm), rl(3,maxm), rrl(maxm),       &
              srl(3,maxm), delta(7,7), capel
  REAL(DP), PARAMETER :: eps = 1.0d-9
  REAL(DP), EXTERNAL :: ddot
  !
  ! ... randomly distributed points on a sphere
  !
  DO m = 1, maxm
     rl(1,m) = randy() - 0.5d0
     rl(2,m) = randy() - 0.5d0
     rl(3,m) = randy() - 0.5d0
     rrl(m) = rl(1,m)**2 + rl(2,m)**2 + rl(3,m)**2
  ENDDO
  !
  CALL ylmr2( maxlm, 2*maxl+1, rl, rrl, ylm )
  !
  !  ... invert Yl for each block of definite l (note the transpose 
  !      operation)
  !
  !  l = 1 block
  !
  DO m = 1, 3
     DO n = 1, 3
        yl1(m,n) = ylm(n,1+m)
     ENDDO
  ENDDO
  CALL invmat( 3, yl1, yl1_inv )
  !
  !  l = 2 block
  !
  DO m = 1, 5
     DO n = 1, 5
        yl2(m,n) = ylm(n,4+m)
     ENDDO
  ENDDO
  CALL invmat( 5, yl2, yl2_inv)
  !
  !  l = 3 block
  !
  DO m = 1, 7
     DO n = 1, 7
        yl3(m,n) = ylm(n,9+m)
     ENDDO
  ENDDO
  CALL invmat( 7, yl3, yl3_inv )
  !
  ! ... now for each symmetry operation of the point-group ...
  !
  DO isym = 1, nsym
     !
     ! ... srl(:,m) = rotated rl(:,m) vectors
     !
     srl = MATMUL( sr(:,:,isym), rl )
     !
     CALL ylmr2( maxlm, maxm, srl, rrl, ylms )
     !
     !  ... find  D_S = Yl_S * Yl_inv (again, beware the transpose)
     !
     !  l = 1
     !
     DO m = 1, 3
        DO n = 1, 3
           yl1(m,n) = ylms(n,1+m)
        ENDDO
     ENDDO
     dy1 (:, :, isym) = MATMUL( yl1(:,:), yl1_inv(:,:) )
     !
     !  l = 2 block
     !
     DO m = 1, 5
        DO n = 1, 5
           yl2(m,n) = ylms(n,4+m)
        ENDDO
     ENDDO
     dy2(:,:,isym) = MATMUL( yl2(:,:), yl2_inv(:,:) )
     !
     !  l = 3 block
     !
     DO m = 1, 7
        DO n = 1, 7
           yl3 (m, n) = ylms (n, 9+m)
        ENDDO
     ENDDO
     dy3 (:,:,isym) = MATMUL( yl3(:,:), yl3_inv(:,:) )
     !
  ENDDO
  !
  ! ... check that D_S matrices are orthogonal as they should if Ylm are
  !     correctly defined.
  !
  delta(:,:) = 0.d0
  DO m = 1, 7
     delta(m,m) = 1.d0
  ENDDO
  !
  DO isym =1,nsym
     !
     !  l = 1 block
     !
     capel = 0.d0
     DO m = 1, 3
        DO n = 1, 3
           capel = capel +  &
                   ( ddot(3,dy1(1,m,isym),1,dy1(1,n,isym),1) - delta(m,n) )**2
        ENDDO
     ENDDO
     IF (capel > eps) CALL errore( 'd_matrix', &
        'D_S (l=1) for this symmetry operation is not orthogonal', isym )
     !
     !  l = 2 block
     !
     capel = 0.d0
     DO m = 1, 5
        DO n = 1, 5
           capel = capel +  &
                   ( ddot(5,dy2(1,m,isym),1,dy2(1,n,isym),1) - delta(m,n) )**2
        ENDDO
     ENDDO
     IF (capel > eps) CALL errore( 'd_matrix', &
        'D_S (l=2) for this symmetry operation is not orthogonal', isym )
     !
     !  l = 3 block
     !
     capel = 0.d0
     DO m = 1, 7
        DO n = 1, 7
           capel = capel +  &
                   ( ddot(7,dy3(1,m,isym),1,dy3(1,n,isym),1) - delta(m,n) )**2
        ENDDO
     ENDDO
     IF (capel > eps) CALL errore( 'd_matrix', &
        'D_S (l=3) for this symmetry operation is not orthogonal', isym )
     !
  ENDDO
  !
  !
  RETURN
  !
END SUBROUTINE d_matrix
