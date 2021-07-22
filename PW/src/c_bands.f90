!
! Copyright (C) 2001-2020 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE c_bands( iter )
  !----------------------------------------------------------------------------
  !! Driver routine for the Hamiltonian diagonalization ones.
  !! It reads the Hamiltonian and an initial guess of the wavefunctions
  !! from a file and computes initialization quantities for the
  !! diagonalization routines.
  !
  USE kinds,                ONLY : DP
  USE io_global,            ONLY : stdout
  USE io_files,             ONLY : iunhub, iunwfc, nwordwfc, nwordwfcU
  USE buffers,              ONLY : get_buffer, save_buffer, close_buffer
  USE klist,                ONLY : nkstot, nks, ngk, igk_k, igk_k_d, xk
  USE uspp,                 ONLY : vkb, vkb_d, nkb, using_vkb, using_vkb_d
  USE gvect,                ONLY : g
  USE wvfct,                ONLY : et, nbnd, npwx, current_k
  USE control_flags,        ONLY : ethr, isolve, restart, use_gpu, iverbosity
  USE ldaU,                 ONLY : lda_plus_u, lda_plus_u_kind, U_projection, wfcU
  USE lsda_mod,             ONLY : current_spin, lsda, isk
  USE wavefunctions,        ONLY : evc
  USE bp,                   ONLY : lelfield
  USE mp_pools,             ONLY : npool, kunit, inter_pool_comm
  USE mp,                   ONLY : mp_sum
  USE check_stop,           ONLY : check_stop_now
  USE gcscf_module,         ONLY : lgcscf

  USE wavefunctions_gpum,   ONLY : using_evc
  USE wvfct_gpum,           ONLY : using_et
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: iter
  !! iteration index
  !
  ! ... local variables
  !
  REAL(DP) :: avg_iter
  ! average number of H*psi products
  INTEGER :: ik_, ik, nkdum, ios
  ! ik : counter on k points
  ! ik_: k-point already done in a previous run
  LOGICAL :: exst
  !
  !
  CALL start_clock( 'c_bands' ); !write (*,*) 'start c_bands' ; FLUSH(6)
  CALL using_evc(0)
  !
  ik_ = 0
  avg_iter = 0.D0
  IF ( restart ) CALL using_et(1)
  IF ( restart ) CALL restart_in_cbands( ik_, ethr, avg_iter, et )
  !
  ! ... If restarting, calculated wavefunctions have to be read from file
  ! ... (not needed for a single k-point: this is done in wfcinit, 
  ! ...  directly from file, in order to avoid wasting memory)
  !
  DO ik = 1, ik_
     IF ( nks > 1 .OR. lelfield ) &
        CALL get_buffer ( evc, nwordwfc, iunwfc, ik )
     IF ( nks > 1 .OR. lelfield ) CALL using_evc(1)
  ENDDO
  !
  IF ( isolve == 0 ) THEN
     WRITE( stdout, '(5X,"Davidson diagonalization with overlap")' )
  ELSEIF ( isolve == 1 ) THEN
     WRITE( stdout, '(5X,"CG style diagonalization")')
  ELSEIF ( isolve == 2 ) THEN
     WRITE( stdout, '(5X,"PPCG style diagonalization")')
  ELSEIF ( isolve == 3 ) THEN
     WRITE( stdout, '(5X,"ParO style diagonalization")')
  ELSE
     CALL errore ( 'c_bands', 'invalid type of diagonalization', isolve)
  ENDIF
  !
  if (iverbosity > 0) CALL print_mem_usage(stdout, 'c_bands before calling an iterative solver')
  !
  ! ... For each k point diagonalizes the hamiltonian
  !
  k_loop: DO ik = ik_+1, nks
     !
     ! ... Set k-point, spin, kinetic energy, needed by Hpsi
     !
     current_k = ik
     !
     IF (lda_plus_u .AND. lda_plus_u_kind.EQ.2) CALL phase_factor(ik)
     !
     IF ( lsda ) current_spin = isk(ik)
     !
     IF ( use_gpu ) THEN
        CALL g2_kin_gpu( ik )
     ELSE
        CALL g2_kin( ik )
     END IF
     !
     ! ... More stuff needed by the hamiltonian: nonlocal projectors
     !
     IF ( use_gpu ) THEN
        IF ( nkb > 0 ) CALL using_vkb_d(2)
        IF ( nkb > 0 ) CALL init_us_2_gpu( ngk(ik), igk_k_d(1,ik), xk(1,ik), vkb_d )
     ELSE
        IF ( nkb > 0 ) CALL using_vkb(2)
        IF ( nkb > 0 ) CALL init_us_2( ngk(ik), igk_k(1,ik), xk(1,ik), vkb )
     END IF
     !
     ! ... read in wavefunctions from the previous iteration
     !
     IF ( nks > 1 .OR. lelfield ) &
          CALL get_buffer ( evc, nwordwfc, iunwfc, ik )
     IF ( nks > 1 .OR. lelfield ) CALL using_evc(2)
     !
     ! ... Needed for LDA+U
     !
     IF ( nks > 1 .AND. lda_plus_u .AND. (U_projection .NE. 'pseudo') ) &
          CALL get_buffer ( wfcU, nwordwfcU, iunhub, ik )
     !
     ! ... diagonalization of bands for k-point ik
     !
     call diag_bands ( iter, ik, avg_iter )
     !
     ! ... save wave-functions to be used as input for the
     ! ... iterative diagonalization of the next scf iteration
     ! ... and for rho calculation
     !
     CALL using_evc(0)
     IF ( nks > 1 .OR. lelfield ) &
          CALL save_buffer ( evc, nwordwfc, iunwfc, ik )
     !
     ! ... beware: with pools, if the number of k-points on different
     ! ... pools differs, make sure that all processors are still in
     ! ... the loop on k-points before checking for stop condition
     !
     nkdum  = kunit * ( nkstot / kunit / npool )
     !
     IF (ik <= nkdum) THEN
        IF (check_stop_now()) THEN
           CALL using_et(0)
           CALL save_in_cbands( ik, ethr, avg_iter, et )
           RETURN
        ENDIF
     ENDIF
     !
  ENDDO k_loop
  !
  CALL mp_sum( avg_iter, inter_pool_comm )
  avg_iter = avg_iter / nkstot
  !
  WRITE( stdout, &
       '( 5X,"ethr = ",1PE9.2,",  avg # of iterations =",0PF5.1 )' ) &
       ethr, avg_iter
  !
  CALL stop_clock( 'c_bands' ); !write (*,*) 'stop c_bands' ; FLUSH(6)
  !
  RETURN
  !
END SUBROUTINE c_bands
!
!----------------------------------------------------------------------------
SUBROUTINE diag_bands( iter, ik, avg_iter )
  !----------------------------------------------------------------------------
  !! Driver routine for diagonalization at each k-point. Types of iterative
  !! diagonalizations currently in use:
  !
  !! * Davidson algorithm (all-band);
  !! * Conjugate Gradient (band-by-band);
  !! * Projected Preconditioned Conjugate Gradient (block);
  !! * Parallel Orbital update (all-band).
  !
  !! Internal procedures:
  !
  !! * \(\textrm{diag_bands_gamma}\)(): optimized algorithms for gamma sampling
  !!                                    of the BZ (real Hamiltonian);
  !! * \(\textrm{diag_bands_k}\)(): general algorithm for arbitrary BZ sampling
  !!                                (complex Hamiltonian);
  !! * \(\textrm{test_exit_cond}\)(): the test on the iterative diagonalization.
  !
  USE kinds,                ONLY : DP
  USE buffers,              ONLY : get_buffer
  USE io_global,            ONLY : stdout
  USE io_files,             ONLY : nwordwfc, iunefieldp, iunefieldm
  USE uspp,                 ONLY : vkb, nkb, okvan, using_vkb
  USE gvect,                ONLY : gstart
  USE wvfct,                ONLY : g2kin, nbndx, et, nbnd, npwx, btype
  USE control_flags,        ONLY : ethr, lscf, max_cg_iter, max_ppcg_iter, isolve, &
                                   gamma_only, use_para_diag, use_gpu
  USE noncollin_module,     ONLY : npol
  USE wavefunctions,        ONLY : evc
  USE g_psi_mod,            ONLY : h_diag, s_diag
  USE g_psi_mod_gpum,       ONLY : h_diag_d, s_diag_d, using_h_diag, using_s_diag, using_h_diag_d, using_s_diag_d
  USE scf,                  ONLY : v_of_0
  USE bp,                   ONLY : lelfield, evcel, evcelp, evcelm, bec_evcel, &
                                   gdir, l3dstring, efield, efield_cry
  USE becmod,               ONLY : bec_type, becp, calbec, &
                                   allocate_bec_type, deallocate_bec_type
  USE klist,                ONLY : nks, ngk
  USE mp_bands,             ONLY : nproc_bgrp, intra_bgrp_comm, inter_bgrp_comm, &
                                   my_bgrp_id, nbgrp
  USE mp,                   ONLY : mp_sum, mp_bcast
  USE xc_lib,               ONLY : exx_is_active
  USE gcscf_module,         ONLY : lgcscf
  USE wavefunctions_gpum,   ONLY : evc_d, using_evc, using_evc_d
  USE wvfct_gpum,           ONLY : et_d, using_et, using_et_d, &
                                   g2kin_d, using_g2kin, using_g2kin_d
  USE becmod_subs_gpum,     ONLY : using_becp_auto
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: iter
  !! iteration index
  INTEGER, INTENT(IN) :: ik
  !! k-point index
  REAL(KIND=DP), INTENT(INOUT) :: avg_iter
  !! average number of H*psi products
  !
  ! ... local variables
  !
  REAL(KIND=DP) :: cg_iter, ppcg_iter
  ! (weighted) number of iterations in Conjugate-Gradient
  INTEGER :: npw, ig, dav_iter, ntry, notconv, nhpsi
  ! number of iterations in Davidson
  ! number or repeated call to diagonalization in case of non convergence
  ! number of notconverged elements
  INTEGER :: ierr, ipw, ibnd, ibnd_start, ibnd_end
  !
  LOGICAL :: lrot
  ! .TRUE. if the wfc have already be rotated
  !
  INTEGER, PARAMETER :: sbsize = 5, rrstep = 7
  ! block dimensions used in PPCG 
  !
  ! Davidson diagonalization uses these external routines on groups of nvec bands
  EXTERNAL h_psi, s_psi, g_psi
  EXTERNAL h_psi_gpu, s_psi_gpu, g_psi_gpu
  ! subroutine h_psi(npwx,npw,nvec,psi,hpsi)  computes H*psi
  ! subroutine s_psi(npwx,npw,nvec,psi,spsi)  computes S*psi (if needed)
  ! subroutine g_psi(npwx,npw,nvec,psi,eig)   computes G*psi -> psi
  !------------------------------------------------------------------------
  ! CG diagonalization uses these external routines on a single band
  EXTERNAL hs_1psi, s_1psi, hs_psi
  EXTERNAL hs_psi_gpu
  EXTERNAL hs_1psi_gpu, s_1psi_gpu
  ! subroutine hs_1psi(npwx,npw,psi,hpsi,spsi)  computes H*psi and S*psi
  ! subroutine s_1psi(npwx,npw,psi,spsi)        computes S*psi (if needed)
  ! In addition to the above the initial wfc rotation uses h_psi, and s_psi
  !------------------------------------------------------------------------
  ! PPCG diagonalization uses these external routines on groups of bands
  ! subroutine h_psi(npwx,npw,nvec,psi,hpsi)  computes H*psi
  ! subroutine s_psi(npwx,npw,nvec,psi,spsi)  computes S*psi (if needed)
  !------------------------------------------------------------------------
  ! ParO diagonalization uses these external routines on a single band
  ! subroutine hs_1psi(npwx,npw,psi,hpsi,spsi)  computes H*psi and S*psi
  ! subroutine g_1psi(npwx,npw,psi,eig)         computes G*psi -> psi
  ! In addition to the above the initial wfc rotation uses h_psi, and s_psi
  external g_1psi
  external g_1psi_gpu

  ALLOCATE( h_diag( npwx, npol ), STAT=ierr )
  IF( ierr /= 0 ) &
     CALL errore( ' diag_bands ', ' cannot allocate h_diag ', ABS(ierr) )
  !
  ALLOCATE( s_diag( npwx, npol ), STAT=ierr )
  IF( ierr /= 0 ) &
     CALL errore( ' diag_bands ', ' cannot allocate s_diag ', ABS(ierr) )
  !
  call using_h_diag(2); call using_s_diag(2)
  ipw=npwx
  CALL mp_sum(ipw, intra_bgrp_comm)
  IF ( nbndx > ipw ) &
     CALL errore ( 'diag_bands', 'too many bands, or too few plane waves',1)
  !
  ! ... allocate space for <beta_i|psi_j> - used in h_psi and s_psi
  !
  CALL allocate_bec_type( nkb, nbnd, becp, intra_bgrp_comm )
  CALL using_becp_auto(2)
  !
  npw = ngk(ik)
  IF ( gamma_only ) THEN
     !
     CALL diag_bands_gamma()
     !
  ELSE
     !
     CALL diag_bands_k()
     !
  ENDIF
  !
  ! ... deallocate work space
  !
  CALL deallocate_bec_type( becp )
  CALL using_becp_auto(2)
  DEALLOCATE( s_diag )
  DEALLOCATE( h_diag )
  call using_h_diag(2); call using_s_diag(2)
  !
  IF ( notconv > MAX( 5, nbnd / 4 ) ) THEN
     !
     CALL errore( 'c_bands', 'too many bands are not converged', 1 )
     !
  ELSEIF ( notconv > 0 ) THEN
     !
     WRITE( stdout, '(5X,"c_bands: ",I2, " eigenvalues not converged")' ) notconv
     !
  ENDIF
  !
  RETURN
  !
 CONTAINS
  !
  ! ... internal procedures
  !
  !-----------------------------------------------------------------------
  SUBROUTINE diag_bands_gamma()
    !-----------------------------------------------------------------------
    !
    ! ... Diagonalization of a real Hamiltonian
    !
    IMPLICIT NONE
    !
    INTEGER :: j
    !
    IF ( isolve == 1 .OR. isolve == 2 .OR. isolve == 3) THEN
       !
       ! ... (Projected Preconditioned) Conjugate-Gradient diagonalization
       !
       ! ... h_diag is the precondition matrix
       !
       CALL using_g2kin(0)
       CALL using_h_diag(2)
       IF ( isolve == 1 .OR. isolve == 2 ) THEN
          FORALL( ig = 1 : npw )
             h_diag(ig,1) = 1.D0 + g2kin(ig) + SQRT( 1.D0 + ( g2kin(ig) - 1.D0 )**2 )
          END FORALL
       ELSE
          FORALL( ig = 1 : npw )
             h_diag(ig, 1) = g2kin(ig) + v_of_0
          END FORALL
          CALL usnldiag( npw, h_diag, s_diag )
       END IF
       !
       ntry = 0
       !
       CG_loop : DO
          !
          IF ( isolve == 1 .OR. isolve == 2 ) THEN
             lrot = ( iter == 1 .AND. ntry == 0 )
             !
             IF ( .NOT. lrot ) THEN
                !
                IF (.not. use_gpu) THEN
                   CALL using_evc(1);  CALL using_et(1); ! et is used as intent(out), set intento=2?
                   CALL rotate_wfc( npwx, npw, nbnd, gstart, nbnd, evc, npol, okvan, evc, et(1,ik) )
                ELSE
                   CALL using_evc_d(1);  CALL using_et_d(1); ! et is used as intent(out), set intento=2?
                   CALL rotate_wfc_gpu( npwx, npw, nbnd, gstart, nbnd, evc_d, npol, okvan, evc_d, et_d(1,ik) )
                END IF
                !
                avg_iter = avg_iter + 1.D0
                !
             ENDIF
          ENDIF
          !
          IF ( isolve == 1 ) THEN
             IF (.not. use_gpu) THEN
                CALL using_evc(1);  CALL using_et(1); CALL using_h_diag(0) ! precontidtion has intent(in)
                CALL rcgdiagg( hs_1psi, s_1psi, h_diag, &
                         npwx, npw, nbnd, evc, et(1,ik), btype(1,ik), &
                         ethr, max_cg_iter, .NOT. lscf, notconv, cg_iter )
             ELSE
                CALL using_evc_d(1);  CALL using_et_d(1); CALL using_h_diag_d(0) ! precontidtion has intent(in)
                CALL rcgdiagg_gpu( hs_1psi_gpu, s_1psi_gpu, h_diag_d, &
                         npwx, npw, nbnd, evc_d, et_d(1,ik), btype(1,ik), &
                         ethr, max_cg_iter, .NOT. lscf, notconv, cg_iter )
             !
             END IF
             !
             avg_iter = avg_iter + cg_iter
             !
          ELSE IF ( isolve == 2 ) THEN
             IF (.not. use_gpu) THEN
               CALL using_evc(1);  CALL using_et(1); CALL using_h_diag(0) ! precontidtion has intent(in)
               CALL ppcg_gamma( h_psi, s_psi, okvan, h_diag, &
                           npwx, npw, nbnd, evc, et(1,ik), btype(1,ik), &
                           0.1d0*ethr, max_ppcg_iter, notconv, ppcg_iter, sbsize , rrstep, iter )
               !
               avg_iter = avg_iter + ppcg_iter
               !
             ELSE
               CALL using_evc_d(1);  CALL using_et_d(1); CALL using_h_diag_d(0) ! precontidtion has intent(in)
               CALL ppcg_gamma_gpu( h_psi_gpu, s_psi_gpu, okvan, h_diag_d, &
                           npwx, npw, nbnd, evc_d, et_d(1,ik), btype(1,ik), &
                           0.1d0*ethr, max_ppcg_iter, notconv, ppcg_iter, sbsize , rrstep, iter )
               !
               avg_iter = avg_iter + ppcg_iter
               !
             END IF 
          ELSE
             !
             IF (.not. use_gpu ) THEN
               CALL using_evc(1);  CALL using_et(1); CALL using_h_diag(0) ! precontidtion has intent(in)
               CALL paro_gamma_new( h_psi, s_psi, hs_psi, g_1psi, okvan, &
                          npwx, npw, nbnd, evc, et(1,ik), btype(1,ik), ethr, notconv, nhpsi )
               !
               avg_iter = avg_iter + nhpsi/float(nbnd) 
               ! write (6,*) ntry, avg_iter, nhpsi
               !
             ELSE
               CALL using_evc_d(1);  CALL using_et_d(1); CALL using_h_diag_d(0) ! precontidtion has intent(in)
               CALL paro_gamma_new_gpu( h_psi_gpu, s_psi_gpu, hs_psi_gpu, g_1psi_gpu, okvan, &
                          npwx, npw, nbnd, evc_d, et_d(1,ik), btype(1,ik), ethr, notconv, nhpsi )
               !
               avg_iter = avg_iter + nhpsi/float(nbnd) 
               ! write (6,*) ntry, avg_iter, nhpsi
               !
             ENDIF  
          ENDIF
          !
          !
          ntry = ntry + 1
          !
          ! ... exit condition
          !
          IF ( test_exit_cond() ) EXIT  CG_loop
          !
       ENDDO CG_loop
       !
    ELSE
       !
       ! ... Davidson diagonalization
       !
       ! ... h_diag are the diagonal matrix elements of the
       ! ... hamiltonian used in g_psi to evaluate the correction
       ! ... to the trial eigenvectors
       !
       IF ( .not. use_gpu ) THEN
          call using_h_diag(2); call using_s_diag(2);
          !
          CALL using_g2kin(0)
          DO j=1, npw
             h_diag(j, 1) = g2kin(j) + v_of_0
          END DO
          !
          CALL usnldiag( npw, h_diag, s_diag )
       ELSE
          call using_h_diag_d(2); call using_s_diag_d(2);
          !
          CALL using_g2kin_d(0)
          !$cuf kernel do(1)
          DO j=1, npw
             h_diag_d(j, 1) = g2kin_d(j) + v_of_0
          END DO
          !
          CALL usnldiag_gpu( npw, h_diag_d, s_diag_d )
       END IF
       !
       ntry = 0
       !
       david_loop: DO
          !
          lrot = ( iter == 1 )
          !
          IF (.not. use_gpu) THEN
             CALL using_evc(1); CALL using_et(1);
             IF ( use_para_diag ) THEN
!                ! make sure that all processors have the same wfc
                CALL pregterg( h_psi, s_psi, okvan, g_psi, &
                            npw, npwx, nbnd, nbndx, evc, ethr, &
                            et(1,ik), btype(1,ik), notconv, lrot, dav_iter, nhpsi ) !    BEWARE gstart has been removed from call
             ELSE
                CALL regterg (  h_psi, s_psi, okvan, g_psi, &
                         npw, npwx, nbnd, nbndx, evc, ethr, &
                         et(1,ik), btype(1,ik), notconv, lrot, dav_iter, nhpsi ) !    BEWARE gstart has been removed from call
             END IF
             ! CALL using_evc(1) done above
          ELSE
             CALL using_evc_d(1); CALL using_et_d(1);
             IF ( use_para_diag ) THEN
                CALL pregterg_gpu( h_psi_gpu, s_psi_gpu, okvan, g_psi_gpu, &
                            npw, npwx, nbnd, nbndx, evc_d, ethr, &
                            et_d(1, ik), btype(1,ik), notconv, lrot, dav_iter, nhpsi ) !    BEWARE gstart has been removed from call 
                !
             ELSE
                !
                CALL regterg_gpu (  h_psi_gpu, s_psi_gpu, okvan, g_psi_gpu, &
                         npw, npwx, nbnd, nbndx, evc_d, ethr, &
                         et_d(1, ik), btype(1,ik), notconv, lrot, dav_iter, nhpsi ) !    BEWARE gstart has been removed from call
             END IF
             ! CALL using_evc_d(1) ! done above
          END IF
          !
          avg_iter = avg_iter + dav_iter
          !
          ntry = ntry + 1
          !
          ! ... exit condition
          !
          IF ( test_exit_cond() ) EXIT  david_loop
          !
       ENDDO david_loop
       !
    ENDIF
    !
    !
    RETURN
    !
  END SUBROUTINE diag_bands_gamma
  !
  !-----------------------------------------------------------------------
  SUBROUTINE diag_bands_k()
    !-----------------------------------------------------------------------
    !! Complex Hamiltonian diagonalization.
    !
    IMPLICIT NONE
    !
    ! ... local variables
    !
    INTEGER :: ipol
    REAL(DP) :: eps=0.000001d0
    ! --- Define a small number ---
    INTEGER :: j
    !
    !write (*,*) ' enter diag_bands_k'; FLUSH(6)
    IF ( lelfield ) THEN
       !
       ! ... save wave functions from previous iteration for electric field
       !
       CALL using_evc(0)
       evcel = evc
       !
       !... read projectors from disk
       !
       IF (.NOT.l3dstring .AND. ABS(efield)>eps ) THEN
          CALL get_buffer (evcelm(:,:,gdir), nwordwfc, iunefieldm, ik+(gdir-1)*nks)
          CALL get_buffer (evcelp(:,:,gdir), nwordwfc, iunefieldp, ik+(gdir-1)*nks)
       ELSE
          DO ipol = 1, 3
             IF ( ABS(efield_cry(ipol))>eps ) THEN
                CALL get_buffer( evcelm(:,:,ipol), nwordwfc, iunefieldm, ik+(ipol-1)*nks )
                CALL get_buffer( evcelp(:,:,ipol), nwordwfc, iunefieldp, ik+(ipol-1)*nks )
             ENDIF
          ENDDO
       ENDIF
       !
       IF ( okvan ) THEN
          !
          CALL allocate_bec_type( nkb, nbnd, bec_evcel )
          CALL using_vkb(0)
          !
          CALL calbec( npw, vkb, evcel, bec_evcel )
          !
       ENDIF
       !
    ENDIF
    !
    !write (*,*) ' current isolve value ( 0 Davidson, 1 CG, 2 PPCG)', isolve; FLUSH(6)
    IF ( isolve == 1 .OR. isolve == 2 .OR. isolve == 3 ) THEN
       !
       ! ... (Projected Preconditioned) Conjugate-Gradient diagonalization
       !
       ! ... h_diag is the precondition matrix
       !
       !write (*,*) ' inside CG solver branch '
       !
       CALL using_g2kin(0)
       CALL using_h_diag(2);
       h_diag = 1.D0
       IF ( isolve == 1 .OR. isolve == 2) THEN
          FORALL( ig = 1 : npwx )
             h_diag(ig,:) = 1.D0 + g2kin(ig) + SQRT( 1.D0 + ( g2kin(ig) - 1.D0 )**2 )
          END FORALL
       ELSE
          FORALL( ig = 1 : npwx )
             h_diag(ig, :) = g2kin(ig) + v_of_0
          END FORALL
          CALL usnldiag( npw, h_diag, s_diag )
       ENDIF
       !
       ntry = 0
       !
       CG_loop : DO
          !
          IF ( isolve == 1 .OR. isolve == 2 ) THEN
             lrot = ( iter == 1 .AND. ntry == 0 )
             !
             IF ( .NOT. lrot ) THEN
                !
                IF ( .not. use_gpu ) THEN
                   CALL using_evc(1); CALL using_et(1);
                   CALL rotate_wfc( npwx, npw, nbnd, gstart, nbnd, evc, npol, okvan, evc, et(1,ik) )
                ELSE
                   CALL using_evc_d(1); CALL using_et_d(1);
                   CALL rotate_wfc_gpu( npwx, npw, nbnd, gstart, nbnd, evc_d, npol, okvan, evc_d, et_d(1,ik) )
                END IF
                !
                avg_iter = avg_iter + 1.D0
             ENDIF
          ENDIF
          !
          IF ( isolve == 1) then
             IF ( .not. use_gpu ) THEN
                CALL using_evc(1); CALL using_et(1); CALL using_h_diag(0)
                CALL ccgdiagg( hs_1psi, s_1psi, h_diag, &
                         npwx, npw, nbnd, npol, evc, et(1,ik), btype(1,ik), &
                         ethr, max_cg_iter, .NOT. lscf, notconv, cg_iter )
             ELSE
                CALL using_evc_d(1); CALL using_et_d(1); CALL using_h_diag_d(0)
                CALL ccgdiagg_gpu( hs_1psi_gpu, s_1psi_gpu, h_diag_d, &
                         npwx, npw, nbnd, npol, evc_d, et_d(1,ik), btype(1,ik), &
                         ethr, max_cg_iter, .NOT. lscf, notconv, cg_iter )
             END IF
             !
             avg_iter = avg_iter + cg_iter
             !
          ELSE IF ( isolve == 2) then
             IF ( .not. use_gpu ) THEN
               CALL using_evc(1); CALL using_et(1); CALL using_h_diag(0)
               ! BEWARE npol should be added to the arguments
               CALL ppcg_k( h_psi, s_psi, okvan, h_diag, &
                           npwx, npw, nbnd, npol, evc, et(1,ik), btype(1,ik), &
                           0.1d0*ethr, max_ppcg_iter, notconv, ppcg_iter, sbsize , rrstep, iter )
               !
               avg_iter = avg_iter + ppcg_iter
               !
             ELSE
               CALL using_evc_d(1); CALL using_et_d(1); CALL using_h_diag_d(0)
               ! BEWARE npol should be added to the arguments
               CALL ppcg_k_gpu( h_psi_gpu, s_psi_gpu, okvan, h_diag_d, &
                           npwx, npw, nbnd, npol, evc_d, et_d(1,ik), btype(1,ik), &
                           0.1d0*ethr, max_ppcg_iter, notconv, ppcg_iter, sbsize , rrstep, iter )
               !
               avg_iter = avg_iter + ppcg_iter
               !
             END IF
          ELSE 
             !
             IF ( .not. use_gpu ) THEN
               CALL using_evc(1); CALL using_et(1); CALL using_h_diag(0)
               CALL paro_k_new( h_psi, s_psi, hs_psi, g_1psi, okvan, &
                        npwx, npw, nbnd, npol, evc, et(1,ik), btype(1,ik), ethr, notconv, nhpsi )
               !
               avg_iter = avg_iter + nhpsi/float(nbnd) 
               ! write (6,*) ntry, avg_iter, nhpsi
             ELSE
               CALL using_evc_d(1); CALL using_et_d(1); CALL using_h_diag_d(0)
               CALL paro_k_new_gpu( h_psi_gpu, s_psi_gpu, hs_psi_gpu, g_1psi_gpu, okvan, &
                        npwx, npw, nbnd, npol, evc_d, et_d(1,ik), btype(1,ik), ethr, notconv, nhpsi )
               !
               avg_iter = avg_iter + nhpsi/float(nbnd) 
               ! write (6,*) ntry, avg_iter, nhpsi
               !
             END IF
          ENDIF
          ntry = ntry + 1
          !
          ! ... exit condition
          !
          IF ( test_exit_cond() ) EXIT  CG_loop
          !
       ENDDO CG_loop
       !
    ELSE
       !
       ! ... Davidson diagonalization
       !
       ! ... h_diag are the diagonal matrix elements of the
       ! ... hamiltonian used in g_psi to evaluate the correction
       ! ... to the trial eigenvectors
       !
       IF ( .not. use_gpu ) THEN
          !
          CALL using_g2kin(0); CALL using_h_diag(2);
          !
          DO ipol = 1, npol
             !
             h_diag(1:npw, ipol) = g2kin(1:npw) + v_of_0
             !
          END DO
          !
          call using_s_diag(2);
          CALL usnldiag( npw, h_diag, s_diag )
       ELSE
          !
          CALL using_g2kin_d(0) ;CALL using_h_diag_d(2)
          !
          DO ipol = 1, npol
             !
             !$cuf kernel do(1)
             DO j = 1, npw
                h_diag_d(j, ipol) = g2kin_d(j) + v_of_0
             END DO
             !
          END DO
          !
          CALL using_s_diag_d(2); CALL using_h_diag_d(1)
          CALL usnldiag_gpu( npw, h_diag_d, s_diag_d )
       END IF
       !
       ntry = 0
       !
       david_loop: DO
          !
          lrot = ( iter == 1 )
          !
          IF (.not. use_gpu ) THEN
             CALL using_evc(1) ; CALL using_et(1)
             IF ( use_para_diag ) then
                !
                CALL pcegterg( h_psi, s_psi, okvan, g_psi, &
                               npw, npwx, nbnd, nbndx, npol, evc, ethr, &
                               et(1,ik), btype(1,ik), notconv, lrot, dav_iter, nhpsi )
                !
             ELSE
                !
                CALL cegterg ( h_psi, s_psi, okvan, g_psi, &
                               npw, npwx, nbnd, nbndx, npol, evc, ethr, &
                               et(1,ik), btype(1,ik), notconv, lrot, dav_iter, nhpsi )
             END IF
          ELSE
             CALL using_evc_d(1) ; CALL using_et_d(1) 
             IF ( use_para_diag ) then
                !
                CALL pcegterg_gpu( h_psi_gpu, s_psi_gpu, okvan, g_psi_gpu, &
                               npw, npwx, nbnd, nbndx, npol, evc_d, ethr, &
                               et_d(1, ik), btype(1,ik), notconv, lrot, dav_iter, nhpsi )

                !
             ELSE
                !
                CALL cegterg_gpu ( h_psi_gpu, s_psi_gpu, okvan, g_psi_gpu, &
                               npw, npwx, nbnd, nbndx, npol, evc_d, ethr, &
                               et_d(1, ik), btype(1,ik), notconv, lrot, dav_iter, nhpsi )
             END IF
          END IF
          !
          avg_iter = avg_iter + dav_iter
          !
          ! ... save wave-functions to be used as input for the
          ! ... iterative diagonalization of the next scf iteration
          ! ... and for rho calculation
          !
          ntry = ntry + 1
          !
          ! ... exit condition
          !
          IF ( test_exit_cond() ) EXIT david_loop
          !
       ENDDO david_loop
       !
    ENDIF
    !
    IF ( lelfield .AND. okvan ) CALL deallocate_bec_type( bec_evcel )
    !
    RETURN
    !
  END SUBROUTINE diag_bands_k
  !
  !-----------------------------------------------------------------------
  FUNCTION test_exit_cond()
    !-----------------------------------------------------------------------
    !! This logical function is .TRUE. when iterative diagonalization
    !! is converged.
    !
    IMPLICIT NONE
    !
    LOGICAL :: test_exit_cond
    !
    IF ( lscf .AND. lgcscf ) THEN
       !
       ! ... tight condition for GC-SCF
       !
       test_exit_cond = .NOT. ( ( ntry <= 8 ) .AND. ( notconv > 0 ) )
       !
    ELSE
       !
       test_exit_cond = .NOT. ( ( ntry <= 5 ) .AND. &
            ( ( .NOT. lscf .AND. ( notconv > 0 ) ) .OR. &
            (       lscf .AND. ( notconv > 5 ) ) ) )
       !
    END IF
    !
  END FUNCTION test_exit_cond
  !
END SUBROUTINE diag_bands
!
!----------------------------------------------------------------------------
SUBROUTINE c_bands_efield( iter )
  !----------------------------------------------------------------------------
  !! Driver routine for Hamiltonian diagonalization under an electric field.
  !
  USE noncollin_module,     ONLY : npol
  USE kinds,                ONLY : DP
  USE bp,                   ONLY : nberrycyc, fact_hepsi, &
                                   evcel, evcelp, evcelm, gdir, l3dstring,&
                                   efield, efield_cry
  USE klist,                ONLY : nks
  USE wvfct,                ONLY : nbnd, npwx
  USE io_global,            ONLY : stdout
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: iter
  !! iteration index
  !
  ! ... local variables
  !
  INTEGER :: inberry, ipol, ierr
  !
  !
  ALLOCATE( evcel ( npol*npwx, nbnd ), STAT=ierr )
  IF( ierr /= 0 ) &
     CALL errore( ' c_bands_efield ', ' cannot allocate evcel ', ABS( ierr ) )
  ALLOCATE( evcelm( npol*npwx, nbnd, 3  ), STAT=ierr )
  IF( ierr /= 0 ) &
     CALL errore( ' c_bands_efield ', ' cannot allocate evcelm ', ABS( ierr ) )
  ALLOCATE( evcelp( npol*npwx, nbnd, 3 ), STAT=ierr )
  IF( ierr /= 0 ) &
     CALL errore( ' c_bands_efield ', ' cannot allocate evcelp ', ABS( ierr ) )
  ALLOCATE( fact_hepsi(nks, 3), STAT=ierr )
  IF( ierr /= 0 ) &
     CALL errore( ' c_bands_efield ', ' cannot allocate fact_hepsi ', ABS( ierr ) )
  !
  DO inberry = 1, nberrycyc
     !
     !...set up electric field hermitean operator
     !
     FLUSH(stdout)
     IF (.NOT.l3dstring) THEN
        CALL h_epsi_her_set (gdir, efield)
     ELSE
        DO ipol=1,3
           CALL h_epsi_her_set(ipol, efield_cry(ipol))
        ENDDO
     ENDIF
     FLUSH(stdout)
     !
     CALL c_bands( iter )
     !
  ENDDO
  !
  DEALLOCATE( fact_hepsi )
  DEALLOCATE( evcelp )
  DEALLOCATE( evcelm )
  DEALLOCATE( evcel  )
  !
  RETURN
  !
END SUBROUTINE c_bands_efield
!
!------------------------------------------------------------------------------
SUBROUTINE c_bands_nscf( )
  !----------------------------------------------------------------------------
  !! Driver routine for Hamiltonian diagonalization routines
  !! specialized to non-self-consistent calculations (no electric field).
  !
  USE kinds,                ONLY : DP
  USE io_global,            ONLY : stdout
  USE io_files,             ONLY : iunhub, iunwfc, nwordwfc, nwordwfcU
  USE buffers,              ONLY : get_buffer, save_buffer, close_buffer
  USE basis,                ONLY : starting_wfc
  USE klist,                ONLY : nkstot, nks, xk, ngk, igk_k, igk_k_d
  USE uspp,                 ONLY : vkb, vkb_d, nkb, using_vkb, using_vkb_d
  USE gvect,                ONLY : g
  USE wvfct,                ONLY : et, nbnd, npwx, current_k
  USE control_flags,        ONLY : ethr, restart, isolve, io_level, iverbosity, use_gpu
  USE ldaU,                 ONLY : lda_plus_u, lda_plus_u_kind, U_projection, wfcU
  USE lsda_mod,             ONLY : current_spin, lsda, isk
  USE wavefunctions,        ONLY : evc
  USE mp_pools,             ONLY : npool, kunit, inter_pool_comm
  USE mp,                   ONLY : mp_sum
  USE check_stop,           ONLY : check_stop_now
  USE wavefunctions_gpum,   ONLY : using_evc
  USE wvfct_gpum,           ONLY : using_et
  IMPLICIT NONE
  !
  ! ... local variables
  !
  REAL(DP) :: avg_iter, ethr_
  ! average number of H*psi products
  INTEGER :: ik_, ik, nkdum, ios
  ! ik_: k-point already done in a previous run
  ! ik : counter on k points
  LOGICAL :: exst
  !
  REAL(DP), EXTERNAL :: get_clock
  !
  !
  CALL start_clock( 'c_bands' )
  !
  ik_ = 0
  avg_iter = 0.D0
  IF ( restart ) CALL using_et(1)
  IF ( restart ) CALL restart_in_cbands( ik_, ethr, avg_iter, et )
  !
  ! ... If restarting, calculated wavefunctions have to be read from file
  !
  CALL using_evc(1)
  DO ik = 1, ik_
     CALL get_buffer( evc, nwordwfc, iunwfc, ik )
  ENDDO
  !
  IF ( isolve == 0 ) THEN
     WRITE( stdout, '(5X,"Davidson diagonalization with overlap")' )
  ELSEIF ( isolve == 1 ) THEN
     WRITE( stdout, '(5X,"CG style diagonalization")' )
  ELSEIF ( isolve == 2 ) THEN
     WRITE( stdout, '(5X,"PPCG style diagonalization")' )
  ELSEIF ( isolve == 3 ) THEN
     WRITE( stdout, '(5X,"ParO style diagonalization")')
  ELSE
     CALL errore ( 'c_bands', 'invalid type of diagonalization', isolve )
  ENDIF
  !
  ! ... For each k point (except those already calculated if restarting)
  ! ... diagonalizes the hamiltonian
  !
  k_loop: DO ik = ik_+1, nks
     !
     ! ... Set k-point, spin, kinetic energy, needed by Hpsi
     !
     current_k = ik
     !
     IF (lda_plus_u .AND. lda_plus_u_kind.EQ.2) CALL phase_factor(ik)
     !
     IF ( lsda ) current_spin = isk(ik)
     !
     IF (.not. use_gpu ) CALL g2_kin( ik )
     IF (      use_gpu ) CALL g2_kin_gpu( ik )
     ! 
     ! ... More stuff needed by the hamiltonian: nonlocal projectors
     !
     IF ( nkb > 0 ) THEN
        IF (.not. use_gpu ) CALL using_vkb(1)
        IF (.not. use_gpu ) CALL init_us_2( ngk(ik), igk_k(1,ik), xk(1,ik), vkb )
        IF (      use_gpu ) CALL using_vkb_d(1)
        IF (      use_gpu ) CALL init_us_2_gpu( ngk(ik), igk_k_d(1,ik), xk(1,ik), vkb_d )
     ENDIF
     !
     ! ... Needed for LDA+U
     !
     IF ( nks > 1 .AND. lda_plus_u .AND. (U_projection .NE. 'pseudo') ) &
          CALL get_buffer( wfcU, nwordwfcU, iunhub, ik )
     !
     ! ... calculate starting  wavefunctions
     !
     IF ( iverbosity > 0 .AND. npool == 1 ) THEN
        WRITE( stdout, 9001 ) ik, nks
     ELSEIF ( iverbosity > 0 .AND. npool > 1 ) THEN
        WRITE( stdout, 9002 ) ik, nks
     ENDIF
     !
     IF ( TRIM(starting_wfc) == 'file' ) THEN
        !
        CALL using_evc(1)
        CALL get_buffer( evc, nwordwfc, iunwfc, ik )
        !
     ELSE
        !
        IF (.not. use_gpu ) CALL init_wfc( ik )
        IF (      use_gpu ) CALL init_wfc_gpu( ik )
        !
     ENDIF
     !
     ! ... diagonalization of bands for k-point ik
     !
     CALL diag_bands( 1, ik, avg_iter )
     !
     ! ... save wave-functions (unless disabled in input)
     !
     IF ( io_level > -1 ) CALL using_evc(0)
     IF ( io_level > -1 ) CALL save_buffer( evc, nwordwfc, iunwfc, ik )
     !
     ! ... beware: with pools, if the number of k-points on different
     ! ... pools differs, make sure that all processors are still in
     ! ... the loop on k-points before checking for stop condition
     !
     nkdum  = kunit * ( nkstot / kunit / npool )
     IF (ik <= nkdum) THEN
        !
        ! ... stop requested by user: save restart information,
        ! ... save wavefunctions to file
        !
        IF ( check_stop_now() ) THEN
           CALL using_et(0)
           CALL save_in_cbands( ik, ethr, avg_iter, et )
           RETURN
        ENDIF
        !
     ENDIF
     !
     ! report about timing
     !
     IF ( iverbosity > 0 ) THEN
        WRITE( stdout, 9000 ) get_clock( 'PWSCF' )
        FLUSH( stdout )
     ENDIF
     !
  ENDDO k_loop
  !
  CALL mp_sum( avg_iter, inter_pool_comm )
  avg_iter = avg_iter / nkstot
  !
  WRITE( stdout, '(/,5X,"ethr = ",1PE9.2,",  avg # of iterations =",0PF5.1)' ) &
       ethr, avg_iter
  !
  CALL stop_clock( 'c_bands' )
  !
  RETURN
  !
  ! formats
  !
9002 FORMAT(/'     Computing kpt #: ',I5, '  of ',I5,' on this pool' )
9001 FORMAT(/'     Computing kpt #: ',I5, '  of ',I5 )
9000 FORMAT( '     total cpu time spent up to now is ',F10.1,' secs' )
  !
END SUBROUTINE c_bands_nscf
