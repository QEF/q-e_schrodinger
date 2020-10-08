!
! Copyright (C) 2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------------
MODULE start_k
  !--------------------------------------------------------------------------
  !! Basic variables for k-point generations, as read from input.
  !
  USE kinds,      ONLY : DP
  USE cell_base,  ONLY : bg
  !
  SAVE
  !
  ! ... uniform k-point grid parameters
  !
  INTEGER :: nk1
  !! the special-point grid, direction 1
  INTEGER :: nk2
  !! the special-point grid, direction 2
  INTEGER :: nk3
  !! the special-point grid, direction 3
  INTEGER :: k1
  !! the offset from the origin, direction 1
  INTEGER :: k2
  !! the offset from the origin, direction 2
  INTEGER :: k3
  !! the offset from the origin, direction 3
  !
  ! ... k points and weights, read from input, if any
  !
  INTEGER :: nks_start=0
  !! number of  k points
  REAL(DP), ALLOCATABLE :: wk_start(:)
  !! weights of k points
  REAL(DP), ALLOCATABLE :: xk_start(:,:)
  !! coordinates of k points
  !
  CONTAINS
  !
  !---------------------------------------------------------------------  
    SUBROUTINE init_start_k( nk1_, nk2_, nk3_, k1_, k2_, k3_, k_points, &
                             nk_, xk_, wk_ ) 
       !-------------------------------------------------------------------
       !! Initialize the grid of k points. See module \(\textrm{start_k}\)
       !! to know meaning of the variables.
       !
       INTEGER, INTENT(IN) :: nk1_, nk2_, nk3_, k1_, k2_, k3_, nk_
       CHARACTER(LEN=*), INTENT(IN) :: k_points
       REAL(DP), INTENT(INOUT) :: xk_(3,nk_)
       REAL(DP), INTENT(IN) :: wk_(nk_)
       !
       LOGICAL :: done
       !
       ! variables for automatic grid
       !
       nk1 = 0; nk2 = 0; nk3 = 0; k1 = 0; k2 = 0; k3 = 0
       done = reset_grid ( nk1_, nk2_, nk3_, k1_, k2_, k3_ )
       IF ( k_points == 'automatic' .AND. .NOT. done ) &
          CALL errore( 'init_start_k','automatic k-points and nk*=0?', 1 )
       !
       ! variables for manual grid
       !
       IF ( k_points == 'gamma' ) THEN
          nks_start = 1
       ELSE
          nks_start = nk_
       ENDIF
       !
       IF ( nks_start > 0) THEN
          !
          IF ( .NOT. ALLOCATED(xk_start) ) ALLOCATE( xk_start(3,nks_start) )
          IF ( .NOT. ALLOCATED(wk_start) ) ALLOCATE( wk_start(nks_start) )
          !
          ! k-points in crystal axis: transform to cartesian (in units 2pi/a)
          ! BEWARE: reciprocal axis bg NEEDED, must have been initialized
          !
          IF ( k_points == 'crystal' ) CALL cryst_to_cart( nk_, xk_, bg, 1 )
          !
          IF ( k_points == 'gamma' ) THEN
            xk_start(:,1) = 0.0_dp
            wk_start(1)   = 1.0_dp
          ELSE
            xk_start(:,:) = xk_(:,1:nk_)
            wk_start(:)   = wk_(1:nk_)
          ENDIF
          !
       ENDIF
       !
    END SUBROUTINE init_start_k 
    !
    !---------------------------------------------------------------
    LOGICAL FUNCTION reset_grid( nk1_, nk2_, nk3_, k1_, k2_, k3_ ) 
       !-----------------------------------------------------------
       !! Reset the automatic grid to new values if these are > 0.
       !
       INTEGER, INTENT(IN) :: nk1_, nk2_, nk3_, k1_, k2_, k3_
       !
       reset_grid = (nk1_*nk2_*nk3_ > 0)
       IF ( .NOT. reset_grid ) RETURN
       nk1 = nk1_
       nk2 = nk2_
       nk3 = nk3_
       k1 = k1_
       k2 = k2_
       k3 = k3_
       !
    END FUNCTION reset_grid
    !
    !
END MODULE start_k
