!
! Copyright (C) 2001-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE reset_k_points_and_reinit_nscf()
  USE kinds, ONLY : DP
  USE klist,       ONLY : &
       nkstot,            & ! total number of k-points
       nks,               & ! number of k-points for local pool
       xk,                & ! k-points coordinates
       wk                 ! k-points weight

  USE lsda_mod,    ONLY : nspin,lsda,isk
  USE basis,           ONLY : starting_wfc, starting_pot, startingconfig
  USE uspp_param,         ONLY : upf
  USE ions_base,          ONLY : ntyp => nsp
  USE ldaU,               ONLY : lda_plus_u, init_lda_plus_u, deallocate_ldaU, &
                                 lda_plus_u_kind
  USE noncollin_module,   ONLY : noncolin
  USE symm_base,          ONLY : d1, d2, d3, fft_fact
  USE parameters,         ONLY : npk
  USE lsda_mod,           ONLY : lsda, nspin, current_spin, isk, nspin
  USE constants,          ONLY : degspin

  IMPLICIT NONE 

  CALL clean_pw( .FALSE. )
  !
  CALL close_files(.true.)
  
  CALL read_k_points()
  
  nkstot=nks
  
  CALL divide_et_impera( nkstot, xk, wk, isk, nks )

  ! ... Setting the values for the nscf run
  !
  startingconfig    = 'input'
  starting_pot      = 'file'
  starting_wfc      = 'atomic'
  !
  ! DFT+U(+V) case
  !
  IF (lda_plus_u) THEN
     !
     ! ... Deallocate Hubbard-related quantities
     !
     CALL deallocate_ldaU ( .TRUE. )
     !
     ! ... Reallocate and set-up Hubbard-related quantities
     !
     IF (lda_plus_u_kind == 2) CALL read_V  
     CALL init_lda_plus_u ( upf(1:ntyp)%psd, nspin, noncolin )
     !
     ! ... Initialize d1, d2, and d3 to rotate the spherical harmonics
     !
     CALL d_matrix( d1, d2, d3 )
     !
  ENDIF
  !
  ! ... needed in FFT re-initialization
  !
  fft_fact(:) = 1
  ! 
  call init_run()
  !
end SUBROUTINE reset_k_points_and_reinit_nscf
