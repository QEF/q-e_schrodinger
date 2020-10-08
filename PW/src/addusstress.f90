!
! Copyright (C) 2001-2015 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------
SUBROUTINE addusstress( sigmanlc )
  !----------------------------------------------------------------------
  !! Driver routine to compute the part of the crystal stress which is due
  !! to the dependence of the Q function on the atomic position.
  !
  USE kinds,          ONLY : dp
  USE control_flags,  ONLY : tqr
  USE realus,         ONLY : addusstress_r
  !
  IMPLICIT NONE
  !
  REAL(DP), INTENT(INOUT) :: sigmanlc(3, 3)
  !! the nonlocal stress
  !
  ! ... local variables
  !
  REAL(DP) :: sigma_r(3,3), sigma_g(3,3)
  INTEGER  :: na,ijh, ipol,jpol
  !
  IF ( tqr ) THEN
     sigma_r(:,:) = 0.d0
     CALL addusstress_r( sigma_r )
     !WRITE (6,'(A)') 'addusstress_r'
     !WRITE (6,'(3f13.8)') sigma_r
     sigmanlc = sigmanlc + sigma_r
     sigma_g(:,:) = 0.d0
     CALL addusstress_g( sigma_g )
!     sigmanlc = sigmanlc + sigma_g
     !WRITE (6,'(A)') 'addusstress_g'
     !WRITE (6,'(3f13.8)') sigma_g
  ELSE
     sigma_g(:,:) = 0.d0
     CALL addusstress_g( sigma_g )
     sigmanlc = sigmanlc + sigma_g
     !WRITE (6,'(A)') 'addusstress_g'
     !WRITE (6,'(3f13.8)') sigma_g
  END IF
  !
END SUBROUTINE addusstress
!
!----------------------------------------------------------------------
SUBROUTINE addusstress_g( sigmanlc )
  !----------------------------------------------------------------------
  !! This routine computes the part of the crystal stress which is due
  !! to the dependence of the Q function on the atomic position.  
  !! It adds contribution to input \(\text{sigmanlc}\), it does not sum 
  !! contributions from various processors (sum is performed by calling
  !! routine).
  !
  USE kinds,          ONLY : DP
  USE ions_base,      ONLY : nat, ntyp => nsp, ityp
  USE cell_base,      ONLY : omega, tpiba
  USE fft_base,       ONLY : dfftp
  USE gvect,          ONLY : ngm, gg, g, eigts1, eigts2, eigts3, mill
  USE lsda_mod,       ONLY : nspin
  USE scf,            ONLY : v, vltot
  USE uspp,           ONLY : becsum, okvan
  USE uspp_param,     ONLY : upf, lmaxq, nh, nhm
  USE control_flags,  ONLY : gamma_only
  USE fft_interfaces, ONLY : fwfft
  USE mp_pools,       ONLY : inter_pool_comm
  USE mp,             ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  REAL(DP), INTENT(INOUT) :: sigmanlc(3, 3)
  !! the nonlocal stress
  !
  ! ... local variables
  !
  INTEGER :: ngm_s, ngm_e, ngm_l
  ! starting/ending indices, local number of G-vectors
  INTEGER :: ig, nt, ih, jh, ijh, ipol, jpol, is, na, nij
  ! counters
  COMPLEX(DP), ALLOCATABLE :: aux(:), aux1(:,:), aux2(:,:), vg(:,:), qgm(:,:)
  ! work space (complex)
  COMPLEX(DP)              :: cfac
  REAL(DP)                 :: fac(3,nspin), sus(3,3)
  ! auxiliary variables
  REAL(DP) , ALLOCATABLE :: qmod(:), ylmk0(:,:), dylmk0(:,:), tbecsum(:,:)
  ! work space (real)
  !
  !
  sus(:,:) = 0.d0
  !
  ! fourier transform of the total effective potential
  !
  ALLOCATE( vg(ngm,nspin) )
  ALLOCATE( aux(dfftp%nnr) )
  DO is = 1, nspin
     IF ( nspin == 4 .and. is /= 1 ) THEN
        aux(:) = v%of_r(:,is)
     ELSE
        aux(:) = vltot(:) + v%of_r(:,is)
     ENDIF
     CALL fwfft( 'Rho', aux, dfftp )
     DO ig = 1, ngm
        vg(ig, is) = aux( dfftp%nl (ig) )
     ENDDO
  ENDDO
  DEALLOCATE( aux )
  !
  ! With k-point parallelization, distribute G-vectors across processors
  ! ngm_s = index of first G-vector for this processor
  ! ngm_e = index of last  G-vector for this processor
  ! ngm_l = local number of G-vectors 
  !
  CALL divide( inter_pool_comm, ngm, ngm_s, ngm_e )
  ngm_l = ngm_e-ngm_s+1
  ! for the extraordinary unlikely case of more processors than G-vectors
  IF ( ngm_l <= 0 ) GO TO 10
  !
  ALLOCATE( aux1(ngm_l,3), aux2(ngm_l,nspin), qmod(ngm_l) )
  ALLOCATE( ylmk0(ngm_l,lmaxq*lmaxq), dylmk0(ngm_l,lmaxq*lmaxq) )
  !
  CALL ylmr2( lmaxq * lmaxq, ngm_l, g(1,ngm_s), gg(ngm_s), ylmk0 )
  !
  DO ig = 1, ngm_l
     qmod(ig) = SQRT( gg(ngm_s+ig-1) ) * tpiba
  ENDDO
  !
  ! here we compute the integral Q*V for each atom,
  !       I = sum_G i G_a exp(-iR.G) Q_nm v^*
  ! (no contribution from G=0)
  !
  DO ipol = 1, 3
     CALL dylmr2( lmaxq * lmaxq, ngm_l, g(1,ngm_s), gg(ngm_s), dylmk0, ipol )
     DO nt = 1, ntyp
        IF ( upf(nt)%tvanp ) THEN
           nij = nh(nt)*(nh(nt)+1)/2
           ALLOCATE( qgm(ngm_l,nij), tbecsum(nij,nspin) )
           ijh = 0
           DO ih = 1, nh(nt)
              DO jh = ih, nh(nt)
                 ijh = ijh + 1
                 CALL dqvan2( ih, jh, nt, ipol, ngm_l, g(1,ngm_s), tpiba, &
                      qmod, ylmk0, dylmk0, qgm(1,ijh) )
              ENDDO
           ENDDO
           !
           DO na = 1, nat
              IF (ityp(na) == nt) THEN
                 !
                 tbecsum(:,:) = becsum( 1:nij, na, 1:nspin )
                 !
                 CALL dgemm( 'N', 'N', 2*ngm_l, nspin, nij, 1.0_dp, &
                      qgm, 2*ngm_l, tbecsum, nij, 0.0_dp, aux2, 2*ngm_l )
                 !
!$omp parallel do default(shared) private(is, ig)
                 DO is = 1, nspin
                    DO ig = 1, ngm_l
                       aux2(ig,is) = aux2(ig,is) * CONJG(vg (ngm_s+ig-1, is))
                    END DO
                 END DO
!$omp end parallel do
!$omp parallel do default(shared) private(ig, cfac)
                 DO ig = 1, ngm_l
                    cfac = CONJG( eigts1(mill (1,ngm_s+ig-1), na) * &
                                  eigts2(mill (2,ngm_s+ig-1), na) * &
                                  eigts3(mill (3,ngm_s+ig-1), na) ) * tpiba
                     aux1(ig,1) = cfac * g(1,ngm_s+ig-1)
                     aux1(ig,2) = cfac * g(2,ngm_s+ig-1)
                     aux1(ig,3) = cfac * g(3,ngm_s+ig-1)
                ENDDO
!$omp end parallel do
                CALL DGEMM('T','N', 3, nspin, 2*ngm_l, 1.0_dp, aux1, 2*ngm_l, &
                           aux2, 2*ngm_l, 0.0_dp, fac, 3 )    
                DO is = 1, nspin
                   DO jpol = 1, 3
                      sus(ipol, jpol) = sus(ipol, jpol) - omega * fac(jpol, is)
                   ENDDO
                ENDDO
              ENDIF
           ENDDO
           DEALLOCATE( tbecsum, qgm )
        ENDIF
     ENDDO

  ENDDO
10 CONTINUE
  CALL mp_sum( sus, inter_pool_comm )
  IF (gamma_only) THEN
     sigmanlc(:,:) = sigmanlc(:,:) + 2.0_dp*sus(:,:)
  ELSE
     sigmanlc(:,:) = sigmanlc(:,:) + sus(:,:)
  ENDIF
  DEALLOCATE( ylmk0, dylmk0 )
  DEALLOCATE( aux1, aux2, vg, qmod )
  !
  RETURN
  !
END SUBROUTINE addusstress_g
