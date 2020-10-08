!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine set_kplusq_nc (xk, wk, xq, nks, npk)
  !-----------------------------------------------------------------------
  !     This routine sets the k and k+q points (with zero weight) used in
  !     the preparatory run for a linear response calculation.
  !
  !     on input: xk and wk contain k-points and corresponding weights
  !
  !     on output: the number of points is doubled and xk and wk in the
  !                odd  positions are the original ones; those in the
  !                even positions are the corresponding k+q values.
  !     the gamma point is treated in a special way. No change is done
  !     to the k-points
  !
  USE kinds, only : DP
  implicit none
  !
  !    First the dummy variables
  !

  integer :: npk, nks
  ! input-output: maximum allowed number of k
  ! input-output: starting and ending number of
  real(DP) :: xk (3, npk), wk (npk), eps, xq (3)
  ! input-output: coordinates of k points
  ! input-output: weights of k points
  ! the smallest xq
  ! input: coordinates of a q-point
  !
  !    And then the local variables
  !

  logical :: lgamma
  ! true if xq is the gamma point
  integer :: ik, j
  ! counter on k
  ! counter
  !
  eps = 1.d-12
  !
  ! shift the k points in the odd positions and fill the even ones with k+
  !

  lgamma = abs (xq (1) ) .lt.eps.and.abs (xq (2) ) .lt.eps.and.abs ( &
       xq (3) ) .lt.eps

  if (.not.lgamma) then

     if (4 * nks.gt.npk) call errore ('set_kplusq', 'too many k points', &
          & nks)
     do ik = nks, 1, - 1
        xk (:, 4 * ik - 3) = xk (:, ik)
        xk (:, 4 * ik - 2) = xk (:, ik) + xq (:)
        xk (:, 4 * ik - 1) = -xk (:, ik)
        xk (:, 4 * ik    ) = -xk (:, ik) - xq (:)
        wk (4 * ik - 3) = wk (ik) 
        wk (4 * ik - 2) = 0.0_DP
        wk (4 * ik - 1) = 0.0_DP
        wk (4 * ik    ) = 0.d0
     enddo
     nks = 4 * nks
  else
     if (2 * nks.gt.npk) call errore ('set_kplusq', 'too many k points', &
          & nks)
     do ik = nks, 1, - 1
        xk (:, 2 * ik - 1) = xk (:, ik)
        xk (:, 2 * ik    ) = -xk (:, ik) 
        wk (2 * ik - 1) = wk (ik) 
        wk (2 * ik    ) = 0.0_DP
     enddo
     nks = 2 * nks
  ENDIF
  return
end subroutine set_kplusq_nc
