!
! Copyright (C) 2001-2015 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE force_us( forcenl )
  !----------------------------------------------------------------------------
  !! The nonlocal potential contribution to forces.
  !
  USE kinds,                ONLY : DP
  USE control_flags,        ONLY : gamma_only
  USE cell_base,            ONLY : tpiba
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE klist,                ONLY : nks, xk, ngk, igk_k
  USE gvect,                ONLY : g
  USE uspp,                 ONLY : nkb, vkb, qq_at, deeq, qq_so, deeq_nc, ofsbeta
  USE uspp_param,           ONLY : upf, nh, nhm
  USE wvfct,                ONLY : nbnd, npwx, wg, et
  USE lsda_mod,             ONLY : lsda, current_spin, isk, nspin
  USE symme,                ONLY : symvector
  USE wavefunctions,        ONLY : evc
  USE noncollin_module,     ONLY : npol, noncolin
  USE io_files,             ONLY : iunwfc, nwordwfc
  USE buffers,              ONLY : get_buffer
  USE becmod,               ONLY : calbec, becp, bec_type, allocate_bec_type, &
                                   deallocate_bec_type
  USE becmod_gpum,          ONLY : becp_d, bec_type_d
  USE becmod_subs_gpum,     ONLY : using_becp_d_auto, calbec_gpu
  USE mp_pools,             ONLY : inter_pool_comm
  USE mp_bands,             ONLY : intra_bgrp_comm
  USE mp,                   ONLY : mp_sum, mp_get_comm_null
  USE wavefunctions_gpum,   ONLY : using_evc
  USE wvfct_gpum,           ONLY : using_et
  USE becmod_subs_gpum,     ONLY : using_becp_auto, allocate_bec_type_gpu, &
                                   synchronize_bec_type_gpu
  USE uspp_init,            ONLY : init_us_2
  !
  IMPLICIT NONE
  !
  REAL(DP), INTENT(OUT) :: forcenl(3,nat)
  !! the nonlocal contribution
  !
  ! ... local variables
  !
  COMPLEX(DP), ALLOCATABLE :: vkb1(:,:)   ! contains g*|beta>
  COMPLEX(DP), ALLOCATABLE :: deff_nc(:,:,:,:)
  REAL(DP),    ALLOCATABLE :: deff(:,:,:)
  TYPE(bec_type) :: dbecp                 ! contains <dbeta|psi>
  INTEGER :: npw, ik, ipol, ig, jkb
  INTEGER :: itot, nt, na
  INTEGER, ALLOCATABLE :: nt_list(:), na_list(:)
  LOGICAL, ALLOCATABLE :: ismulti_np(:)
#if defined(__CUDA)
  TYPE(bec_type_d), TARGET :: dbecp_d
  COMPLEX(DP), POINTER, DEVICE :: becpnc(:,:,:),  becpk(:,:), &
                                  dbecpnc(:,:,:), dbecpk(:,:)
#else
  COMPLEX(DP), ALLOCATABLE     :: becpnc(:,:,:),  becpk(:,:), &
                                  dbecpnc(:,:,:), dbecpk(:,:)
#endif
  !
  forcenl(:,:) = 0.D0
  !
  CALL allocate_bec_type( nkb, nbnd, becp, intra_bgrp_comm )
  CALL using_becp_auto( 2 )
  CALL allocate_bec_type( nkb, nbnd, dbecp, intra_bgrp_comm )
  !
#if defined(__CUDA)
  CALL allocate_bec_type_gpu( nkb, nbnd, dbecp_d, intra_bgrp_comm )
  !
  IF (noncolin) THEN
    becpnc => becp_d%nc_d
    dbecpnc => dbecp_d%nc_d
  ELSEIF (.NOT. gamma_only ) THEN
    becpk => becp_d%k_d 
    dbecpk => dbecp_d%k_d
  ENDIF
#else
  IF (noncolin) THEN
    ALLOCATE( becpnc(nkb,npol,nbnd), dbecpnc(nkb,npol,nbnd) )
  ELSEIF (.NOT. gamma_only ) THEN
    ALLOCATE( becpk(nkb,nbnd), dbecpk(nkb,nbnd) )
  ENDIF
#endif
  !
  ALLOCATE( vkb1(npwx,nkb) )
  IF (noncolin) THEN
    ALLOCATE( deff_nc(nhm,nhm,nat,nspin) )
  ELSEIF (.NOT. gamma_only ) THEN
    ALLOCATE( deff(nhm,nhm,nat) )
  ENDIF
  CALL using_evc(0)
  !$acc data create(vkb1,deff,deff_nc) copyin(evc)
  !
  ALLOCATE( nt_list(nat), na_list(nat), ismulti_np(nat) )
  !
  itot = 0
  DO nt = 1, ntyp
     DO na = 1, nat
        IF ( ityp(na)==nt ) THEN
           itot = itot + 1
           nt_list(itot) = nt
           na_list(itot) = na
           ismulti_np(itot) = upf(nt)%tvanp .OR. upf(nt)%is_multiproj
        ENDIF
     ENDDO
  ENDDO
  IF (itot /= nat) CALL errore( 'force_us', 'Something wrong in atoms counting', 1 )
  !
  ! ... the forces are a sum over the K points and over the bands
  !
  DO ik = 1, nks
     !
     IF ( lsda ) current_spin = isk(ik)
     npw = ngk(ik)
     !
     IF ( nks > 1 ) THEN
        CALL get_buffer( evc, nwordwfc, iunwfc, ik )
        CALL using_evc(1)
        !$acc update device( evc )
        IF ( nkb > 0 ) CALL init_us_2( npw, igk_k(1,ik), xk(1,ik), vkb, .TRUE. )
     ENDIF
     !
#if defined(__CUDA)
     CALL using_becp_d_auto(2)
     !$acc update device(evc)
     !$acc host_data use_device(vkb,evc)
     CALL calbec_gpu( npw, vkb, evc, becp_d )
     !$acc end host_data
#else
     CALL using_becp_auto(2)
     !$acc update self(vkb,evc)
     CALL calbec( npw, vkb, evc, becp )
     IF (noncolin) THEN
       becpnc = becp%nc
     ELSEIF (.NOT. gamma_only ) THEN
       becpk = becp%k
     ENDIF
#endif
     !
     DO ipol = 1, 3
        !
#if defined(_OPENACC)
        !$acc parallel loop collapse(2)
#else
        !$omp parallel do collapse(2) private(ig)
#endif
        DO jkb = 1, nkb
           DO ig = 1, npw
              vkb1(ig,jkb) = vkb(ig,jkb) * (0.D0,-1.D0) * g(ipol,igk_k(ig,ik))
           ENDDO
        ENDDO
        !
#if defined(__CUDA)
        !$acc host_data use_device(vkb1,evc)
        CALL calbec_gpu( npw, vkb1, evc, dbecp_d )
        !$acc end host_data
        CALL synchronize_bec_type_gpu( dbecp_d, dbecp, 'h' )
#else
        CALL calbec( npw, vkb1, evc, dbecp )
        IF (noncolin) THEN
          dbecpnc = dbecp%nc
        ELSEIF (.NOT. gamma_only ) THEN
          dbecpk = dbecp%k
        ENDIF
#endif
        !
        !$acc data copyin(nt_list,na_list,ismulti_np)
        IF ( gamma_only ) THEN
           !
           CALL force_us_gamma( forcenl )
           !
        ELSE
           !
           CALL force_us_k( forcenl )
           !
        ENDIF
        !$acc end data
        !
     ENDDO
  ENDDO
  !
  ! ... if sums over bands are parallelized over the band group
  !
  CALL using_becp_auto( 0 )
  IF ( becp%comm /= mp_get_comm_null() ) CALL mp_sum( forcenl, becp%comm )
  !
  !$acc end data
  DEALLOCATE( vkb1 )
  IF ( noncolin ) THEN
     DEALLOCATE( deff_nc )
  ELSEIF ( .NOT. gamma_only ) THEN
     DEALLOCATE( deff )
  ENDIF
  !
  CALL deallocate_bec_type( dbecp )
  CALL deallocate_bec_type( becp )
  CALL using_becp_auto( 2 )
#if defined(__CUDA)
  CALL using_becp_d_auto( 2 )
#endif
  !
  ! ... collect contributions across pools from all k-points
  !
  CALL mp_sum( forcenl, inter_pool_comm )
  !
  ! ... The total D matrix depends on the ionic position via the
  ! ... augmentation part \int V_eff Q dr, the term deriving from the 
  ! ... derivative of Q is added in the routine addusforce
  !
  CALL addusforce( forcenl )
  !
  ! ... Since our summation over k points was only on the irreducible 
  ! ... BZ we have to symmetrize the forces.
  !
  CALL symvector( nat, forcenl )
  !
  DEALLOCATE( nt_list, na_list, ismulti_np )
  !   
#if !defined(__CUDA) || !defined(_OPENACC)
  IF ( noncolin ) THEN
    DEALLOCATE( becpnc, dbecpnc )
  ELSEIF (.NOT. gamma_only ) THEN
    DEALLOCATE( becpk, dbecpk )
  ENDIF
#endif
  !
  RETURN
  !
  CONTAINS
     !
     !-----------------------------------------------------------------------
     SUBROUTINE force_us_gamma( forcenl )
       !-----------------------------------------------------------------------
       !! Nonlocal contribution to the force. Calculation at Gamma.
       !
       ! Important notice about parallelization over the band group of processors:
       ! 1) internally, "calbec" parallelises on plane waves over the band group
       ! 2) the results of "calbec" are distributed across processors of the band
       !    group: the band index of becp, dbecp is distributed
       ! 3) the band group is subsequently used to parallelize over bands
       !
       USE uspp,     ONLY : qq_at, deeq
       !
       IMPLICIT NONE
       !
       REAL(DP) :: forcenl(3,nat)
       !! the nonlocal contribution
       !
       ! ... local variables
       !
       REAL(DP), ALLOCATABLE :: aux(:,:)
       REAL(DP) :: forcenl_ipol
       INTEGER :: nt, na, ibnd, ibnd_loc, ih, jh, ijkb0
       INTEGER :: nh_nt, becp_ibnd_begin, becp_nbnd_loc, nbnd_siz
#if defined(__CUDA)
       REAL(DP), POINTER, DEVICE :: dbecprd(:,:), becprd(:,:)
       !
       dbecprd => dbecp_d%r_d
       becprd  => becp_d%r_d
       becp_nbnd_loc = becp_d%nbnd_loc
       becp_ibnd_begin = becp_d%ibnd_begin
#else
       REAL(DP), ALLOCATABLE :: dbecprd(:,:), becprd(:,:)
       !
       nbnd_siz = nbnd / becp%nproc
       ALLOCATE( becprd(nkb,nbnd_siz), dbecprd(nkb,nbnd_siz) )
       !
       dbecprd = dbecp%r
       becprd  = becp%r
       becp_nbnd_loc = becp%nbnd_loc
       becp_ibnd_begin = becp%ibnd_begin
#endif
       !
       !$acc data copyin( et, wg )
       !
       DO nt = 1, ntyp
          !
          IF ( nh(nt) == 0 ) CYCLE
          !
          ALLOCATE( aux(nh(nt), becp%nbnd_loc) )
          !$acc data create(aux)
          !
          nh_nt = nh(nt)
          !
          DO na = 1, nat
             IF ( ityp(na) == nt ) THEN
                ijkb0 = ofsbeta(na)
                ! ... this is \sum_j q_{ij} <beta_j|psi>
                !
                !$acc host_data use_device(aux, qq_at)
                CALL MYDGEMM( 'N','N', nh(nt), becp_nbnd_loc, nh(nt),      &
                              1.0_DP, qq_at(1,1,na), nhm, becprd(ijkb0+1,1), &
                              nkb, 0.0_DP, aux, nh(nt) )
                !$acc end host_data
                !
                ! ... multiply by -\epsilon_n
                !
#if defined(_OPENACC)
!$acc parallel loop collapse(2)
#else
!$omp parallel do default(shared) private(ibnd_loc,ibnd,ih)
#endif
                DO ih = 1, nh_nt
                   DO ibnd_loc = 1, becp_nbnd_loc
                      ibnd = ibnd_loc + becp_ibnd_begin - 1
                      aux(ih,ibnd_loc) = - et(ibnd,ik) * aux(ih,ibnd_loc)
                   ENDDO
                ENDDO
#if !defined(_OPENACC)
!$omp end parallel do
#endif
                !
                ! ... add  \sum_j d_{ij} <beta_j|psi>
                !
                !$acc host_data use_device(aux, deeq)
                CALL MYDGEMM( 'N','N', nh(nt), becp_nbnd_loc, nh(nt), &
                              1.0_DP, deeq(1,1,na,current_spin), nhm, &
                              becprd(ijkb0+1,1), nkb, 1.0_DP, aux, nh(nt) )
                !$acc end host_data
                !
                ! ... Auxiliary variable to perform the reduction with gpu kernels
                forcenl_ipol = 0.0_DP
#if defined(_OPENACC)
!$acc parallel loop collapse(2) reduction(+:forcenl_ipol)
#else
!$omp parallel do default(shared) private(ibnd_loc,ibnd,ih) reduction(-:forcenl_ipol)
#endif
                DO ih = 1, nh_nt
                   DO ibnd_loc = 1, becp_nbnd_loc
                      ibnd = ibnd_loc + becp_ibnd_begin - 1
                      forcenl_ipol = forcenl_ipol - 2.0_DP*tpiba * aux(ih,ibnd_loc) *&
                      dbecprd(ijkb0+ih,ibnd_loc) * wg(ibnd,ik)
                   ENDDO
                ENDDO
#if !defined(_OPENACC)
!$omp end parallel do
#endif
                !
                forcenl(ipol,na) = forcenl(ipol,na) + forcenl_ipol
                !
             ENDIF
          ENDDO
          !
          !$acc end data
          DEALLOCATE( aux )
          !
       ENDDO
       !
       !$acc end data
       !
#if !defined(__CUDA)
       DEALLOCATE( becprd, dbecprd )
#endif
       !
     END SUBROUTINE force_us_gamma
     !
     !
     !-----------------------------------------------------------------------
     SUBROUTINE force_us_k( forcenl )
       !-----------------------------------------------------------------------
       !! Nonlocal contribution to the force. Calculation for k-points.
       !
       IMPLICIT NONE
       !
       REAL(DP) :: forcenl(3,nat)
       !! the nonlocal contribution
       !
       ! ... local variables
       !
       REAL(DP) :: fac
       REAL(DP) :: forcenl_p1, forcenl_p2
       INTEGER  :: ibnd, ih, jh, na, nt, ikb, jkb, ijkb0, is, js, ijs !counters
       INTEGER  :: nh_nt, it
       !
       CALL using_et(0)
       !CALL using_becp_auto(0);
       !
       !$acc data copy(forcenl)
       !
       DO ibnd = 1, nbnd
          !
          IF (noncolin) THEN
             CALL compute_deff_nc( deff_nc, et(ibnd,ik) )
          ELSE
             CALL compute_deff( deff, et(ibnd,ik) )
          ENDIF
          !
          fac = wg(ibnd,ik)*tpiba
          !
#if defined(_OPENACC)
          !$acc parallel loop gang reduction(+:forcenl_p2)
#else
          !$omp parallel do private(nt,na,ijkb0,nh_nt,forcenl_p1,forcenl_p2,ih,&
          !$omp                     ikb,is,js,ijs,jkb)
#endif
          DO it = 1, nat
             !
             nt = nt_list(it)
             na = na_list(it)
             ijkb0 = ofsbeta(na)
             nh_nt = nh(nt)
             !
             forcenl_p2 = 0.d0
             !$acc loop vector reduction(+:forcenl_p2)
             DO ih = 1, nh_nt
                !
                ikb = ijkb0 + ih
                IF (noncolin) THEN
                   forcenl_p1 = 0.d0
                   !$acc loop seq collapse(2) reduction(+:forcenl_p1)
                   DO is = 1, npol
                     DO js = 1, npol
                       ijs = (is-1)*npol+js
                       forcenl_p1 = forcenl_p1 - &
                                    deff_nc(ih,ih,na,ijs)*fac*(  &
                                    CONJG(dbecpnc(ikb,is,ibnd))* &
                                    becpnc(ikb,js,ibnd)+         &
                                    CONJG(becpnc(ikb,is,ibnd))*  &
                                    dbecpnc(ikb,js,ibnd) )
                     ENDDO
                   ENDDO
                ELSE
                   forcenl_p1 = -2.D0 * fac * deff(ih,ih,na) *    &
                                DBLE( CONJG( dbecpk(ikb,ibnd) ) * &
                                becpk(ikb,ibnd) )
                ENDIF
                !
                forcenl_p2 = forcenl_p2 + forcenl_p1
                !
             ENDDO
             !
             forcenl(ipol,na) = forcenl(ipol,na) + forcenl_p2
             !
             IF ( ismulti_np(it) ) THEN
                !
                forcenl_p2 = 0.d0
                !$acc loop vector reduction(+:forcenl_p2)
                DO ih = 1, nh_nt
                   ikb = ijkb0 + ih
                   !
                   ! ... in US case there is a contribution for jh<>ih. 
                   ! ... We use here the symmetry in the interchange 
                   ! ... of ih and jh
                   !
                   forcenl_p1 = 0.d0
                   !$acc loop seq
                   DO jh = ih+1, nh_nt
                      jkb = ijkb0 + jh
                      IF (noncolin) THEN
                        !$acc loop seq collapse(2) reduction(+:forcenl_p1)
                        DO is = 1, npol
                          DO js = 1, npol
                             ijs = (is-1)*npol+js
                             forcenl_p1 = forcenl_p1 - &
                                          deff_nc(ih,jh,na,ijs)*fac*(  &
                                          CONJG(dbecpnc(ikb,is,ibnd))* &
                                          becpnc(jkb,js,ibnd) +        &
                                          CONJG(becpnc(ikb,is,ibnd))*  &
                                          dbecpnc(jkb,js,ibnd)) -      &
                                          deff_nc(jh,ih,na,ijs)*fac*(  &
                                          CONJG(dbecpnc(jkb,is,ibnd))* &
                                          becpnc(ikb,js,ibnd) +        &
                                          CONJG(becpnc(jkb,is,ibnd))*  &
                                          dbecpnc(ikb,js,ibnd) )
                          ENDDO
                        ENDDO
                      ELSE
                        forcenl_p1 = forcenl_p1 - &
                                     2.D0 * fac * deff(ih,jh,na) *      &
                                     DBLE( CONJG( dbecpk(ikb,ibnd) ) *  &
                                     becpk(jkb,ibnd) + dbecpk(jkb,ibnd) &
                                     * CONJG( becpk(ikb,ibnd) ) )
                      ENDIF
                   ENDDO !jh
                   !
                   forcenl_p2 = forcenl_p2 + forcenl_p1
                   !
                ENDDO !ih
                !
                forcenl(ipol,na) = forcenl(ipol,na) + forcenl_p2
                !
             ENDIF ! tvanp
             !
          ENDDO ! it=nt|na
#if !defined(_OPENACC)
          !$omp end parallel do
#endif
          !
       ENDDO ! nbnd
       !
       !$acc end data
       !
     END SUBROUTINE force_us_k
     !
     !
END SUBROUTINE force_us
