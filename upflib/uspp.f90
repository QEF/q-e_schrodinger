!
! Copyright (C) 2004-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
MODULE uspp_param
  !
  ! ... Ultrasoft and Norm-Conserving pseudopotential parameters
  !  
  USE upf_kinds,    ONLY : DP
  USE upf_params,   ONLY : npsx
  USE pseudo_types, ONLY : pseudo_upf
  !
  SAVE
  PRIVATE :: randy
  !
  TYPE (pseudo_upf),  ALLOCATABLE, TARGET :: upf(:)

  INTEGER :: &
       nh(npsx),             &! number of beta functions per atomic type
       nhm,                  &! max number of different beta functions per atom
       nbetam,               &! max number of beta functions
       iver(3,npsx)           ! version of the atomic code
  INTEGER :: &
       lmaxkb,               &! max angular momentum
       lmaxq                  ! max angular momentum + 1 for Q functions
  INTEGER :: &
       nvb,                  &! number of species with Vanderbilt PPs (CPV)
       ish(npsx)              ! for each specie the index of the first beta 
                              ! function: ish(1)=1, ish(i)=1+SUM(nh(1:i-1))
CONTAINS
  !
  !----------------------------------------------------------------------------
  FUNCTION n_atom_wfc( nat, ityp, noncolin )
  !----------------------------------------------------------------------------
  !
  ! ... Find number of starting atomic orbitals
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN)  :: nat, ityp(nat)
  LOGICAL, INTENT(IN), OPTIONAL  :: noncolin
  INTEGER  :: n_atom_wfc
  !
  INTEGER  :: na, nt, n
  LOGICAL  :: non_col
  !
  !
  non_col = .FALSE.
  IF ( PRESENT (noncolin) ) non_col=noncolin
  n_atom_wfc = 0
  !
  DO na = 1, nat
     !
     nt = ityp(na)
     !
     DO n = 1, upf(nt)%nwfc
        !
        IF ( upf(nt)%oc(n) >= 0.D0 ) THEN
           !
           IF ( non_col ) THEN
              !
              IF ( upf(nt)%has_so ) THEN
                 !
                 n_atom_wfc = n_atom_wfc + 2 * upf(nt)%lchi(n)
                 !
                 IF ( ABS( upf(nt)%jchi(n)-upf(nt)%lchi(n) - 0.5D0 ) < 1.D-6 ) &
                    n_atom_wfc = n_atom_wfc + 2
                 !
              ELSE
                 !
                 n_atom_wfc = n_atom_wfc + 2 * ( 2 * upf(nt)%lchi(n) + 1 )
                 !
              END IF
              !
           ELSE
              !
              n_atom_wfc = n_atom_wfc + 2 * upf(nt)%lchi(n) + 1
              !
           END IF
        END IF
     END DO
  END DO
  !
  RETURN
  !
  END FUNCTION n_atom_wfc
END MODULE uspp_param

! <<<<<<<<<<<<<<<~~~~<<<<<<<<<<<<<<<<-----------------

MODULE uspp
  !
  ! Ultrasoft PPs:
  ! - Clebsch-Gordan coefficients "ap", auxiliary variables "lpx", "lpl"
  ! - beta and q functions of the solid
  !
  USE upf_kinds,  ONLY: DP
  USE upf_params, ONLY: lmaxx, lqmax
  IMPLICIT NONE
  PRIVATE
  SAVE
  PUBLIC :: nlx, lpx, lpl, ap, aainit, indv, nhtol, nhtolm, indv_ijkb0, &
            nkb, nkbus, vkb, dvan, deeq, qq_at, qq_nt, nhtoj, ijtoh, beta, &
            becsum, ebecsum, deallocate_uspp
       
  PUBLIC :: okvan, nlcc_any
  PUBLIC :: qq_so, dvan_so, deeq_nc 
  PUBLIC :: dbeta
  INTEGER, PARAMETER :: &
       nlx  = (lmaxx+1)**2, &! maximum number of combined angular momentum
       mx   = 2*lqmax-1      ! maximum magnetic angular momentum of Q
  INTEGER ::             &! for each pair of combined momenta lm(1),lm(2): 
       lpx(nlx,nlx),     &! maximum combined angular momentum LM
       lpl(nlx,nlx,mx)    ! list of combined angular momenta  LM
  REAL(DP) :: &
       ap(lqmax*lqmax,nlx,nlx)
  ! Clebsch-Gordan coefficients for spherical harmonics
  !
  INTEGER :: nkb,        &! total number of beta functions, with struct.fact.
             nkbus        ! as above, for US-PP only
  !
  INTEGER, ALLOCATABLE ::&
       indv(:,:),        &! indes linking  atomic beta's to beta's in the solid
       nhtol(:,:),       &! correspondence n <-> angular momentum l
       nhtolm(:,:),      &! correspondence n <-> combined lm index for (l,m)
       ijtoh(:,:,:),     &! correspondence beta indexes ih,jh -> composite index ijh
       indv_ijkb0(:)      ! first beta (index in the solid) for each atom 
  !
  LOGICAL :: &
       okvan = .FALSE.,&  ! if .TRUE. at least one pseudo is Vanderbilt
       nlcc_any=.FALSE.   ! if .TRUE. at least one pseudo has core corrections
  !
  COMPLEX(DP), ALLOCATABLE, TARGET :: &
       vkb(:,:)                ! all beta functions in reciprocal space
  REAL(DP), ALLOCATABLE :: &
       becsum(:,:,:)           ! \sum_i f(i) <psi(i)|beta_l><beta_m|psi(i)>
  REAL(DP), ALLOCATABLE :: &
       ebecsum(:,:,:)          ! \sum_i f(i) et(i) <psi(i)|beta_l><beta_m|psi(i)>
  REAL(DP), ALLOCATABLE :: &
       dvan(:,:,:),           &! the D functions of the solid
       deeq(:,:,:,:),         &! the integral of V_eff and Q_{nm} 
       qq_nt(:,:,:),          &! the integral of q functions in the solid (ONE PER NTYP) used to be the qq array
       qq_at(:,:,:),          &! the integral of q functions in the solid (ONE PER ATOM !!!!) 
       nhtoj(:,:)              ! correspondence n <-> total angular momentum
  !
  COMPLEX(DP), ALLOCATABLE :: & ! variables for spin-orbit/noncolinear case:
       qq_so(:,:,:,:),           &! Q_{nm}
       dvan_so(:,:,:,:),         &! D_{nm}
       deeq_nc(:,:,:,:)           ! \int V_{eff}(r) Q_{nm}(r) dr 
  !
  ! spin-orbit coupling: qq and dvan are complex, qq has additional spin index
  ! noncolinear magnetism: deeq is complex (even in absence of spin-orbit)
  !
  REAL(DP), ALLOCATABLE :: &
       beta(:,:,:)           ! beta functions for CP (without struct.factor)
  REAL(DP), ALLOCATABLE :: &
       dbeta(:,:,:,:,:)      ! derivative of beta functions w.r.t. cell for CP (without struct.factor)
  !
CONTAINS
  !
  !-----------------------------------------------------------------------
  subroutine aainit(lli)
    !-----------------------------------------------------------------------
    !
    ! this routine computes the coefficients of the expansion of the product
    ! of two real spherical harmonics into real spherical harmonics.
    !
    !     Y_limi(r) * Y_ljmj(r) = \sum_LM  ap(LM,limi,ljmj)  Y_LM(r)
    !
    ! On output:
    ! ap     the expansion coefficients
    ! lpx    for each input limi,ljmj is the number of LM in the sum
    ! lpl    for each input limi,ljmj points to the allowed LM
    !
    ! The indices limi,ljmj and LM assume the order for real spherical
    ! harmonics given in routine ylmr2
    !
    USE upf_invmat
    implicit none
    !
    ! input: the maximum li considered
    !  
    integer :: lli
    !
    ! local variables
    !
    integer :: llx, l, li, lj
    real(DP) , allocatable :: r(:,:), rr(:), ylm(:,:), mly(:,:)
    ! an array of random vectors: r(3,llx)
    ! the norm of r: rr(llx)
    ! the real spherical harmonics for array r: ylm(llx,llx)
    ! the inverse of ylm considered as a matrix: mly(llx,llx)
    !
    if (lli < 0) call upf_error('aainit','lli not allowed',lli)

    if (lli*lli > nlx) call upf_error('aainit','nlx is too small ',lli*lli)

    llx = (2*lli-1)**2
    if (2*lli-1 > lqmax) &
         call upf_error('aainit','ap leading dimension is too small',llx)

    allocate (r( 3, llx ))    
    allocate (rr( llx ))    
    allocate (ylm( llx, llx ))    
    allocate (mly( llx, llx ))    

    r(:,:)   = 0.0_DP
    ylm(:,:) = 0.0_DP
    mly(:,:) = 0.0_DP
    ap(:,:,:)= 0.0_DP

    ! - generate an array of random vectors (uniform deviate on unitary sphere)

    call gen_rndm_r(llx,r,rr)

    ! - generate the real spherical harmonics for the array: ylm(ir,lm)

    call ylmr2(llx,llx,r,rr,ylm)

    !-  store the inverse of ylm(ir,lm) in mly(lm,ir)

    call invmat(llx, ylm, mly)

    !-  for each li,lj compute ap(l,li,lj) and the indices, lpx and lpl
    do li = 1, lli*lli
       do lj = 1, lli*lli
          lpx(li,lj)=0
          do l = 1, llx
             ap(l,li,lj) = compute_ap(l,li,lj,llx,ylm,mly)
             if (abs(ap(l,li,lj)) > 1.d-3) then
                lpx(li,lj) = lpx(li,lj) + 1
                if (lpx(li,lj) > mx) &
                     call upf_error('aainit','mx dimension too small', lpx(li,lj))
                lpl(li,lj,lpx(li,lj)) = l
             end if
          end do
       end do
    end do
    
    deallocate(mly)
    deallocate(ylm)
    deallocate(rr)
    deallocate(r)
    
    return
  end subroutine aainit
  !
  !-----------------------------------------------------------------------
  subroutine gen_rndm_r(llx,r,rr)
    !-----------------------------------------------------------------------
    ! - generate an array of random vectors (uniform deviate on unitary sphere)
    !
    USE upf_const,  ONLY: tpi
    implicit none
    !
    ! first the I/O variables
    !
    integer :: llx   ! input: the dimension of r and rr
    real(DP) :: &
         r(3,llx),  &! output: an array of random vectors
         rr(llx)     ! output: the norm of r
    !
    ! here the local variables
    !
    integer  :: ir
    real(DP) :: costheta, sintheta, phi
    
    do ir = 1, llx
       costheta = 2.0_DP * randy() - 1.0_DP
       sintheta = SQRT ( 1.0_DP - costheta*costheta)
       phi = tpi * randy()
       r (1,ir) = sintheta * cos(phi)
       r (2,ir) = sintheta * sin(phi)
       r (3,ir) = costheta
       rr(ir)   = 1.0_DP
    end do
    
    return
  end subroutine gen_rndm_r
!
!------------------------------------------------------------------------
   FUNCTION randy ( irand )
      !------------------------------------------------------------------------
      !   
      ! x=randy(n): reseed with initial seed idum=n ( 0 <= n <= ic, see below)
      !             if randy is not explicitly initialized, it will be
      !             initialized with seed idum=0 the first time it is called
      ! x=randy() : generate uniform real(DP) numbers x in [0,1]
      !   
      use upf_kinds, only : DP
      implicit none
      !
      REAL(DP) :: randy
      INTEGER, optional    :: irand
      !   
      INTEGER , PARAMETER  :: m    = 714025, &
                              ia   = 1366, &
                              ic   = 150889, &
                              ntab = 97
      REAL(DP), PARAMETER  :: rm = 1.0_DP / m 
      INTEGER              :: j
      INTEGER, SAVE        :: ir(ntab), iy, idum=0
      LOGICAL, SAVE        :: first=.true.
      !   
      IF ( present(irand) ) THEN
         idum = MIN( ABS(irand), ic) 
         first=.true.
      END IF

      IF ( first ) THEN
         !   
         first = .false.
         idum = MOD( ic - idum, m ) 
         !   
         DO j=1,ntab
            idum=mod(ia*idum+ic,m)
            ir(j)=idum
         END DO
         idum=mod(ia*idum+ic,m)
         iy=idum
      END IF
      j=1+(ntab*iy)/m
      IF( j > ntab .OR. j <  1 ) call upf_error('randy','j out of range',ABS(j)+1)
      iy=ir(j)
      randy=iy*rm
      idum=mod(ia*idum+ic,m)
      ir(j)=idum
      !   
      RETURN
      !   
   END FUNCTION randy
  !
  !-----------------------------------------------------------------------
  function compute_ap(l,li,lj,llx,ylm,mly)
    !-----------------------------------------------------------------------
    !-  given an l and a li,lj pair compute ap(l,li,lj)
    implicit none
    !
    ! first the I/O variables
    !
    integer :: &
         llx,         &! the dimension of ylm and mly
         l,li,lj       ! the arguments of the array ap
    
    real(DP) :: &
         compute_ap,  &! this function
         ylm(llx,llx),&! the real spherical harmonics for array r
         mly(llx,llx)  ! the inverse of ylm considered as a matrix
    !
    ! here the local variables
    !
    integer :: ir
    
    compute_ap = 0.0_DP
    do ir = 1,llx
       compute_ap = compute_ap + mly(l,ir)*ylm(ir,li)*ylm(ir,lj)
    end do
    
    return
  end function compute_ap
  !
  !-----------------------------------------------------------------------
  SUBROUTINE deallocate_uspp()
    !-----------------------------------------------------------------------
    !
    IF( ALLOCATED( nhtol ) )      DEALLOCATE( nhtol )
    IF( ALLOCATED( indv ) )       DEALLOCATE( indv )
    IF( ALLOCATED( nhtolm ) )     DEALLOCATE( nhtolm )
    IF( ALLOCATED( nhtoj ) )      DEALLOCATE( nhtoj )
    IF( ALLOCATED( indv_ijkb0 ) ) DEALLOCATE( indv_ijkb0 )
    IF( ALLOCATED( ijtoh ) )      DEALLOCATE( ijtoh )
    IF( ALLOCATED( vkb ) )        DEALLOCATE( vkb )
    IF( ALLOCATED( becsum ) )     DEALLOCATE( becsum )
    IF( ALLOCATED( ebecsum ) )    DEALLOCATE( ebecsum )
    IF( ALLOCATED( qq_at ) )      DEALLOCATE( qq_at )
    IF( ALLOCATED( qq_nt ) )      DEALLOCATE( qq_nt )
    IF( ALLOCATED( dvan ) )       DEALLOCATE( dvan )
    IF( ALLOCATED( deeq ) )       DEALLOCATE( deeq )
    IF( ALLOCATED( qq_so ) )      DEALLOCATE( qq_so )
    IF( ALLOCATED( dvan_so ) )    DEALLOCATE( dvan_so )
    IF( ALLOCATED( deeq_nc ) )    DEALLOCATE( deeq_nc )
    IF( ALLOCATED( beta ) )       DEALLOCATE( beta )
    IF( ALLOCATED( dbeta ) )      DEALLOCATE( dbeta )
    !
  END SUBROUTINE deallocate_uspp
  !
END MODULE uspp

