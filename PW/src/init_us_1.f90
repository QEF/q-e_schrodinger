
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
subroutine init_us_1
  !----------------------------------------------------------------------
  !
  !   This routine performs the following tasks:
  !   a) For each non vanderbilt pseudopotential it computes the D and
  !      the betar in the same form of the Vanderbilt pseudopotential.
  !   b) It computes the indices indv which establish the correspondence
  !      nh <-> beta in the atom
  !   c) It computes the indices nhtol which establish the correspondence
  !      nh <-> angular momentum of the beta function
  !   d) It computes the indices nhtolm which establish the correspondence
  !      nh <-> combined (l,m) index for the beta function.
  !   e) It computes the coefficients c_{LM}^{nm} which relates the
  !      spherical harmonics in the Q expansion
  !   f) It computes the interpolation table "qrad" for Q(G)
  !   g) It computes the qq terms which define the S matrix.
  !   h) It fills the interpolation table "tab" for the beta functions
  !
  USE kinds,        ONLY : DP
  USE constants,    ONLY : fpi, sqrt2
  USE atom,         ONLY : rgrid
  USE ions_base,    ONLY : ntyp => nsp, ityp, nat
  USE cell_base,    ONLY : omega
  USE gvect,        ONLY : g, gg
  USE lsda_mod,     ONLY : nspin
  USE us,           ONLY : nqxq, dq, nqx, tab, tab_d2y, qrad, spline_ps
  USE splinelib
  USE uspp,         ONLY : nhtol, nhtoj, nhtolm, ijtoh, dvan, qq_at, qq_nt, indv,&
                           ap, aainit, qq_so, dvan_so, okvan, indv_ijkb0
  USE uspp_param,   ONLY : upf, lmaxq, nh, nhm, lmaxkb
  USE spin_orb,     ONLY : lspinorb, rot_ylm, fcoef, lmaxx
  USE paw_variables,ONLY : okpaw
  USE mp_bands,     ONLY : intra_bgrp_comm
  USE mp,           ONLY : mp_sum
  !
  implicit none
  !
  !     here a few local variables
  !
  integer :: nt, ih, jh, nb, mb, ijv, l, m, ir, iq, is, startq, &
       lastq, ilast, ndm, ia
  ! various counters
  real(DP), allocatable :: aux (:), besr (:)
  ! various work space
  real(DP) :: pref, qi
  ! the prefactor of the beta functions
  ! q-point grid for interpolation
  real(DP), allocatable :: ylmk0 (:)
  ! the spherical harmonics
  real(DP) ::  vqint, j
  ! interpolated value
  ! J=L+S (noninteger!)
  integer :: n1, m0, m1, n, li, mi, vi, vj, ijs, is1, is2, &
             lk, mk, vk, kh, lh, ijkb0, na
  integer, external :: sph_ind
  complex(DP) :: coeff, qgm(1)
  real(DP) :: ji, jk, d1
  real(DP), allocatable :: xdata(:)
  real(DP), EXTERNAL :: spinor
  !
  call start_clock ('init_us_1')
  !
  !    Initialization of the variables
  !
  ap (:,:,:)   = 0.d0
  !
  ! the following prevents an out-of-bound error: upf(nt)%nqlc=2*lmax+1
  ! but in some versions of the PP files lmax is not set to the maximum
  ! l of the beta functions but includes the l of the local potential
  !
  do nt=1,ntyp
     upf(nt)%nqlc = MIN ( upf(nt)%nqlc, lmaxq )
     IF ( upf(nt)%nqlc < 0 )  upf(nt)%nqlc = 0
  end do

  if (lspinorb) then
!
!  In the spin-orbit case we need the unitary matrix u which rotates the
!  real spherical harmonics and yields the complex ones.
!
     rot_ylm=(0.d0,0.d0)
     l=lmaxx
     rot_ylm(l+1,1)=(1.d0,0.d0)
     do n1=2,2*l+1,2
       m=n1/2
       n=l+1-m
       rot_ylm(n,n1)=CMPLX((-1.d0)**m/sqrt2,0.0_dp,kind=DP)
       rot_ylm(n,n1+1)=CMPLX(0.d0,-(-1.d0)**m/sqrt2,kind=DP)
       n=l+1+m
       rot_ylm(n,n1)=CMPLX(1.0_dp/sqrt2,0.d0,kind=DP)
       rot_ylm(n,n1+1)=CMPLX(0.d0, 1.0_dp/sqrt2,kind=DP)
     enddo
     fcoef=(0.d0,0.d0)
     dvan_so = (0.d0,0.d0)
     qq_so=(0.d0,0.d0)
     qq_at  = 0.d0
     qq_nt=0.d0
  else
     qq_nt=0.d0
     qq_at  = 0.d0
     dvan = 0.d0
  endif
  !
  !   For each pseudopotential we initialize the indices nhtol, nhtolm,
  !   nhtoj, indv, and if the pseudopotential is of KB type we initialize the
  !   atomic D terms
  !
  ijkb0 = 0
  do nt = 1, ntyp
     ih = 1
     do nb = 1, upf(nt)%nbeta
        l = upf(nt)%lll (nb)
        do m = 1, 2 * l + 1
           nhtol (ih, nt) = l
           nhtolm(ih, nt) = l*l+m
           indv  (ih, nt) = nb
           ih = ih + 1
        enddo
     enddo
     if ( upf(nt)%has_so ) then
        ih = 1
        do nb = 1, upf(nt)%nbeta
           l = upf(nt)%lll (nb)
           j = upf(nt)%jjj (nb)
           do m = 1, 2 * l + 1
              nhtoj (ih, nt) = j
              ih = ih + 1
           enddo
        enddo
     endif
     !
     ! ijtoh map augmentation channel indexes ih and jh to composite
     ! "triangular" index ijh
     ijtoh(:,:,nt) = -1
     ijv = 0
     do ih = 1,nh(nt)
         do jh = ih,nh(nt)
             ijv = ijv+1
             ijtoh(ih,jh,nt) = ijv
             ijtoh(jh,ih,nt) = ijv
         enddo
     enddo
     !
     ! ijkb0 points to the last beta "in the solid" for atom ia-1
     ! i.e. ijkb0+1,.. ijkb0+nh(ityp(ia)) are the nh beta functions of
     !      atom ia in the global list of beta functions (ijkb0=0 for ia=1)
     do ia = 1,nat
       IF ( ityp(ia) == nt ) THEN
          indv_ijkb0(ia) = ijkb0
          ijkb0 = ijkb0 + nh(nt)
        END IF
     enddo
     !
     !    From now on the only difference between KB and US pseudopotentials
     !    is in the presence of the qq and Q functions.
     !
     !    Here we initialize the D of the solid
     !
     if (upf(nt)%has_so) then
     !
     !  first calculate the fcoef coefficients
     !
       do ih = 1, nh (nt)
          li = nhtol(ih, nt)
          ji = nhtoj(ih, nt)
          mi = nhtolm(ih, nt)-li*li
          vi = indv (ih, nt)
          do kh=1,nh(nt)
            lk = nhtol(kh, nt)
            jk = nhtoj(kh, nt)
            mk = nhtolm(kh, nt)-lk*lk
            vk = indv (kh, nt)
            if (li == lk .and. abs(ji-jk) < 1.d-7) then
              do is1=1,2
                do is2=1,2
                  coeff = (0.d0, 0.d0)
                  do m=-li-1, li
                    m0= sph_ind(li,ji,m,is1) + lmaxx + 1
                    m1= sph_ind(lk,jk,m,is2) + lmaxx + 1
                    coeff=coeff + rot_ylm(m0,mi)*spinor(li,ji,m,is1)* &
                            CONJG(rot_ylm(m1,mk))*spinor(lk,jk,m,is2)
                  enddo
                  fcoef(ih,kh,is1,is2,nt)=coeff
                enddo
              enddo
            endif
          enddo
        enddo
!
!   and calculate the bare coefficients
!
        do ih = 1, nh (nt)
           vi = indv (ih, nt)
           do jh = 1, nh (nt)
              vj = indv (jh, nt)
              ijs=0
              do is1=1,2
                 do is2=1,2
                    ijs=ijs+1
                    dvan_so(ih,jh,ijs,nt) = upf(nt)%dion(vi,vj) * &
                                            fcoef(ih,jh,is1,is2,nt)
                    if (vi.ne.vj) fcoef(ih,jh,is1,is2,nt)=(0.d0,0.d0)
                 enddo
              enddo
           enddo
        enddo
     else
        do ih = 1, nh (nt)
          do jh = 1, nh (nt)
            if (nhtol (ih, nt) == nhtol (jh, nt) .and. &
              nhtolm(ih, nt) == nhtolm(jh, nt) ) then
              ir = indv (ih, nt)
              is = indv (jh, nt)
              if (lspinorb) then
                 dvan_so (ih, jh, 1, nt) = upf(nt)%dion (ir, is)
                 dvan_so (ih, jh, 4, nt) = upf(nt)%dion (ir, is)
              else
                 dvan (ih, jh, nt) = upf(nt)%dion (ir, is)
              endif
            endif
          enddo
        enddo
     endif
  enddo
  !
  !  compute Clebsch-Gordan coefficients
  !
  if (okvan .or. okpaw) call aainit (lmaxkb + 1)
  !
  !   here for the US types we compute the Fourier transform of the
  !   Q functions.
  !
  IF ( lmaxq > 0 ) CALL compute_qrad( )
  !
  !   and finally we compute the qq coefficients by integrating the Q.
  !   The qq are the g=0 components of Q
  !
#if defined(__MPI)
  if (gg (1) > 1.0d-8) goto 100
#endif
  allocate (ylmk0( lmaxq * lmaxq))
  call ylmr2 (lmaxq * lmaxq, 1, g, gg, ylmk0)
  do nt = 1, ntyp
    if ( upf(nt)%tvanp ) then
      if (upf(nt)%has_so) then
        do ih=1,nh(nt)
          do jh=1,nh(nt)
            call qvan2 (1, ih, jh, nt, gg, qgm, ylmk0)
            qq_nt(ih,jh,nt) = omega * DBLE(qgm (1) )
            do kh=1,nh(nt)
              do lh=1,nh(nt)
                ijs=0
                do is1=1,2
                  do is2=1,2
                    ijs=ijs+1
                    do is=1,2
                      qq_so(kh,lh,ijs,nt) = qq_so(kh,lh,ijs,nt)       &
                          + omega* DBLE(qgm(1))*fcoef(kh,ih,is1,is,nt)&
                                               *fcoef(jh,lh,is,is2,nt)
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      else
        do ih = 1, nh (nt)
          do jh = ih, nh (nt)
             call qvan2 (1, ih, jh, nt, gg, qgm, ylmk0)
             if (lspinorb) then
                 qq_so (ih, jh, 1, nt) = omega *  DBLE (qgm (1) )
                 qq_so (jh, ih, 1, nt) = qq_so (ih, jh, 1, nt)
                 qq_so (ih, jh, 4, nt) = qq_so (ih, jh, 1, nt)
                 qq_so (jh, ih, 4, nt) = qq_so (ih, jh, 4, nt)
             endif
             qq_nt(ih,jh,nt) = omega * DBLE(qgm (1) )
             qq_nt(jh,ih,nt) = omega * DBLE(qgm (1) )
          enddo
        enddo
      endif
    endif
  enddo
  deallocate (ylmk0)
#if defined(__MPI)
100 continue
  if (lspinorb) then
    call mp_sum(  qq_so , intra_bgrp_comm )
    call mp_sum(  qq_nt, intra_bgrp_comm )
  else
    call mp_sum(  qq_nt, intra_bgrp_comm )
  endif
#endif
  ! finally we set the atomic specific qq_at matrices
  do na=1, nat
     qq_at(:,:, na) = qq_nt(:,:,ityp(na))
  end do
  !
  !     fill the interpolation table tab
  !
  ndm = MAXVAL ( upf(:)%kkbeta )
  allocate( aux (ndm) )
  allocate (besr( ndm))
  pref = fpi / sqrt (omega)
  call divide (intra_bgrp_comm, nqx, startq, lastq)
  tab (:,:,:) = 0.d0
  do nt = 1, ntyp
     if ( upf(nt)%is_gth ) cycle
     do nb = 1, upf(nt)%nbeta
        l = upf(nt)%lll (nb)
        do iq = startq, lastq
           qi = (iq - 1) * dq
           call sph_bes (upf(nt)%kkbeta, rgrid(nt)%r, qi, l, besr)
           do ir = 1, upf(nt)%kkbeta
              aux (ir) = upf(nt)%beta (ir, nb) * besr (ir) * rgrid(nt)%r(ir)
           enddo
           call simpson (upf(nt)%kkbeta, aux, rgrid(nt)%rab, vqint)
           tab (iq, nb, nt) = vqint * pref
        enddo
     enddo
  enddo
  deallocate (besr)
  deallocate (aux)

  call mp_sum(  tab, intra_bgrp_comm )

  ! initialize spline interpolation
  if (spline_ps) then
     allocate( xdata(nqx) )
     do iq = 1, nqx
        xdata(iq) = (iq - 1) * dq
     enddo
     do nt = 1, ntyp
        do nb = 1, upf(nt)%nbeta 
           d1 = (tab(2,nb,nt) - tab(1,nb,nt)) / dq
           call spline(xdata, tab(:,nb,nt), 0.d0, d1, tab_d2y(:,nb,nt))
        enddo
     enddo
     deallocate(xdata)
  endif

  call stop_clock ('init_us_1')
  return
end subroutine init_us_1

!----------------------------------------------------------------------
SUBROUTINE compute_qrad ( )
  !----------------------------------------------------------------------
  !
  ! Compute interpolation table qrad(i,nm,l+1,nt) = Q^{(L)}_{nm,nt}(q_i)
  ! of angular momentum L, for atom of type nt, on grid q_i, where
  ! nm = combined index for n,m=1,nh(nt)
  !
  USE kinds,        ONLY : dp
  USE constants,    ONLY : fpi
  USE ions_base,    ONLY : ntyp => nsp
  USE cell_base,    ONLY : omega
  USE atom,         ONLY : rgrid
  USE uspp_param,   ONLY : upf, lmaxq, nbetam, nh, nhm, lmaxkb
  USE us,           ONLY : nqxq, dq, qrad
  USE mp_bands,     ONLY : intra_bgrp_comm
  USE mp,           ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  INTEGER :: ndm, startq, lastq, nt, l, nb, mb, ijv, iq, ir
  ! various indices
  REAL(dp) :: prefr
  ! the prefactor of the Q functions
  REAL(dp) :: q
  REAL(dp), ALLOCATABLE :: aux (:), besr (:)
  ! various work space
  !
  prefr = fpi / omega
  ndm = MAXVAL ( upf(:)%kkbeta )
  ALLOCATE (aux ( ndm))
  ALLOCATE (besr( ndm))

  CALL divide (intra_bgrp_comm, nqxq, startq, lastq)
  !
  qrad(:,:,:,:)= 0.d0
  DO nt = 1, ntyp
     if ( upf(nt)%tvanp ) then
        DO l = 0, upf(nt)%nqlc -1
           !
           !     note that l is the true (combined) angular momentum
           !     and that the arrays have dimensions 0..l (no more 1..l+1)
           !
           DO iq = startq, lastq
              !
              q = (iq - 1) * dq
              !
              !     here we compute the spherical bessel function for each q_i
              !
              CALL sph_bes ( upf(nt)%kkbeta, rgrid(nt)%r, q, l, besr)
              !
              DO nb = 1, upf(nt)%nbeta
                 !
                 !    the Q are symmetric with respect to indices
                 !
                 DO mb = nb, upf(nt)%nbeta
                    ijv = mb * (mb - 1) / 2 + nb
                    IF ( ( l >= abs(upf(nt)%lll(nb) - upf(nt)%lll(mb)) ) .AND. &
                         ( l <=     upf(nt)%lll(nb) + upf(nt)%lll(mb)  ) .AND. &
                         (mod(l+upf(nt)%lll(nb)+upf(nt)%lll(mb),2)==0) ) THEN
                       DO ir = 1, upf(nt)%kkbeta
                          aux  (ir) = besr (ir) * upf(nt)%qfuncl(ir,ijv,l)
                       ENDDO
                       !
                       !   and then we integrate with all the Q functions
                       !
                       CALL simpson ( upf(nt)%kkbeta, aux, rgrid(nt)%rab, &
                                     qrad(iq,ijv,l+1, nt) )
                    ENDIF
                 ENDDO
              ENDDO
              ! igl
           ENDDO
           ! l
        ENDDO
        qrad (:, :, :, nt) = qrad (:, :, :, nt)*prefr
        CALL mp_sum ( qrad (:, :, :, nt), intra_bgrp_comm )
     ENDIF
     ! ntyp
  ENDDO
  !
  DEALLOCATE (besr)
  DEALLOCATE (aux)
  !
END SUBROUTINE compute_qrad
