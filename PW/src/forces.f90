!
! Copyright (C) 2001-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
! TB
! included gate related forces
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
SUBROUTINE forces()
  !----------------------------------------------------------------------------
  !! This routine is a driver routine which computes the forces
  !! acting on the atoms. The complete expression of the forces
  !! contains four parts which are computed by different routines:
  !
  !! a) force_lc: local contribution to the forces;  
  !! b) force_cc: contribution due to NLCC;  
  !! c) force_ew: contribution due to the electrostatic ewald term;  
  !! d) force_us: contribution due to the non-local potential;  
  !! e) force_corr: correction term for incomplete self-consistency;  
  !! f) force_hub: contribution due to the Hubbard term;  
  !! g) force_london: semi-empirical correction for dispersion forces;  
  !! h) force_d3: Grimme-D3 (DFT-D3) correction to dispersion forces.
  !
  USE kinds,             ONLY : DP
  USE io_global,         ONLY : stdout
  USE cell_base,         ONLY : at, bg, alat, omega  
  USE ions_base,         ONLY : nat, ntyp => nsp, ityp, tau, zv, amass, extfor, atm
  USE fft_base,          ONLY : dfftp
  USE gvect,             ONLY : ngm, gstart, ngl, igtongl, g, gg, gcutm
  USE lsda_mod,          ONLY : nspin
  USE symme,             ONLY : symvector
  USE vlocal,            ONLY : strf, vloc
  USE force_mod,         ONLY : force, lforce, sumfor
  USE scf,               ONLY : rho
  USE ions_base,         ONLY : if_pos
  USE ldaU,              ONLY : lda_plus_u, U_projection
  USE extfield,          ONLY : tefield, forcefield, gate, forcegate, relaxz
  USE control_flags,     ONLY : gamma_only, remove_rigid_rot, textfor,  &
                                iverbosity, llondon, ldftd3, lxdm, ts_vdw
  USE plugin_flags
  USE bp,                ONLY : lelfield, gdir, l3dstring, efield_cart, &
                                efield_cry,efield
  USE uspp,              ONLY : okvan
  USE martyna_tuckerman, ONLY : do_comp_mt, wg_corr_force
  USE london_module,     ONLY : force_london
  USE dftd3_api,         ONLY : get_atomic_number, dftd3_calc
  USE dftd3_qe,          ONLY : dftd3_pbc_gdisp, dftd3

  USE xdm_module,        ONLY : force_xdm
  USE tsvdw_module,      ONLY : FtsvdW
  USE esm,               ONLY : do_comp_esm, esm_bc, esm_force_ew
  USE qmmm,              ONLY : qmmm_mode
  !
  IMPLICIT NONE
  !
  REAL(DP), ALLOCATABLE :: forcenl(:,:),         &
                           forcelc(:,:),         &
                           forcecc(:,:),         &
                           forceion(:,:),        &
                           force_disp(:,:),      &
                           force_d3(:,:),        &
                           force_disp_xdm(:,:),  &
                           force_mt(:,:),        &
                           forcescc(:,:),        &
                           forces_bp_efield(:,:),&
                           forceh(:,:)
  ! nonlocal, local, core-correction, ewald, scf correction terms, and hubbard
  !
  ! aux is used to store a possible additional density
  ! now defined in real space
  !
  COMPLEX(DP), ALLOCATABLE :: auxg(:), auxr(:)
  !
  REAL(DP) :: sumscf, sum_mm
  REAL(DP), PARAMETER :: eps = 1.e-12_dp
  INTEGER  :: ipol, na
  ! counter on polarization
  ! counter on atoms
  !
  REAL(DP) :: latvecs(3,3)
  INTEGER :: atnum(1:nat)
  REAL(DP) :: stress_dftd3(3,3)
  !
  !
  CALL start_clock( 'forces' )
  !
  ALLOCATE( forcenl(3,nat), forcelc(3,nat), forcecc(3,nat), &
            forceh(3,nat), forceion(3,nat), forcescc(3,nat) )
  !    
  forcescc(:,:) = 0.D0
  forceh(:,:)   = 0.D0
  force(:,:)    = 0.D0
  !
  ! ... The nonlocal contribution is computed here
  !
  CALL force_us( forcenl )
  !
  ! ... The local contribution
  !
  CALL force_lc( nat, tau, ityp, alat, omega, ngm, ngl, igtongl,       &
                 g, rho%of_r(:,1), dfftp%nl, gstart, gamma_only, vloc, &
                 forcelc )
  !
  ! ... The NLCC contribution
  !
  CALL force_cc( forcecc )
  !
  ! ... The Hubbard contribution
  !     (included by force_us if using beta as local projectors)
  !
  IF ( lda_plus_u .AND. U_projection.NE.'pseudo' ) CALL force_hub( forceh )
  !
  ! ... The ionic contribution is computed here
  !
  IF( do_comp_esm ) THEN
     CALL esm_force_ew( forceion )
  ELSE
     CALL force_ew( alat, nat, ntyp, ityp, zv, at, bg, tau, omega, g, &
                    gg, ngm, gstart, gamma_only, gcutm, strf, forceion )
  ENDIF
  !
  ! ... the semi-empirical dispersion correction
  !
  IF ( llondon ) THEN
    !
    ALLOCATE( force_disp(3,nat) )
    force_disp(:,:) = 0.0_DP
    force_disp = force_london( alat , nat , ityp , at , bg , tau )
    !
  ENDIF
  !
  ! ... The Grimme-D3 dispersion correction
  !
  IF ( ldftd3 ) THEN
    !
    ALLOCATE( force_d3(3, nat) )
    force_d3(:,:) = 0.0_DP
    latvecs(:,:) = at(:,:)*alat
    tau(:,:) = tau(:,:)*alat
    atnum(:) = get_atomic_number(atm(ityp(:)))
    CALL dftd3_pbc_gdisp( dftd3, tau, atnum, latvecs, &
                          force_d3, stress_dftd3 )
    force_d3 = -2.d0*force_d3
    tau(:,:) = tau(:,:)/alat
  ENDIF
  !
  !
  IF (lxdm) THEN
     ALLOCATE( force_disp_xdm(3,nat) )
     force_disp_xdm = 0._dp
     force_disp_xdm = force_xdm(nat)
  ENDIF
  !
  ! ... The SCF contribution
  !
  CALL force_corr( forcescc )
  !
  IF (do_comp_mt) THEN
    !
    ALLOCATE( force_mt(3,nat) )
    CALL wg_corr_force( .TRUE., omega, nat, ntyp, ityp, ngm, g, tau, zv, strf, &
                        rho%of_g(:,1), force_mt )
  ENDIF
  !
  ! ... call void routine for user define/ plugin patches on internal forces
  !
  CALL plugin_int_forces()
  !
  ! ... Berry's phase electric field terms
  !
  IF (lelfield) THEN
     ALLOCATE( forces_bp_efield(3,nat) )
     forces_bp_efield(:,:) = 0.d0
     IF (.NOT.l3dstring) THEN
        IF (okvan) CALL forces_us_efield( forces_bp_efield, gdir, efield )
        CALL forces_ion_efield( forces_bp_efield, gdir, efield )
     ELSE
        IF (okvan) THEN
           DO ipol = 1, 3
              CALL forces_us_efield( forces_bp_efield, ipol, efield_cry(ipol) )
           ENDDO
        ENDIF
        DO ipol = 1, 3
           CALL forces_ion_efield( forces_bp_efield, ipol, efield_cart(ipol) )
        ENDDO
     ENDIF
  ENDIF
  !
  ! ... here we sum all the contributions and compute the total force acting
  ! ... on the crystal
  !
  DO ipol = 1, 3
     !
     sumfor = 0.D0
     !
     DO na = 1, nat
        !
        force(ipol,na) = force(ipol,na)    + &
                         forcenl(ipol,na)  + &
                         forceion(ipol,na) + &
                         forcelc(ipol,na)  + &
                         forcecc(ipol,na)  + &
                         forceh(ipol,na)   + &
                         forcescc(ipol,na)
        !
        IF ( llondon )  force(ipol,na) = force(ipol,na) + force_disp(ipol,na)
        IF ( ldftd3 )   force(ipol,na) = force(ipol,na) + force_d3(ipol,na)
        IF ( lxdm )     force(ipol,na) = force(ipol,na) + force_disp_xdm(ipol,na)
        ! factor 2 converts from Ha to Ry a.u.
        IF ( ts_vdw )   force(ipol,na) = force(ipol,na) + 2.0_dp*FtsvdW(ipol,na)
        IF ( tefield )  force(ipol,na) = force(ipol,na) + forcefield(ipol,na)
        IF ( gate )     force(ipol,na) = force(ipol,na) + forcegate(ipol,na) ! TB
        IF (lelfield)   force(ipol,na) = force(ipol,na) + forces_bp_efield(ipol,na)
        IF (do_comp_mt) force(ipol,na) = force(ipol,na) + force_mt(ipol,na) 
        !
        sumfor = sumfor + force(ipol,na)
        !
     ENDDO
     !
     !TB
     IF ((gate.AND.relaxz).AND.(ipol==3)) WRITE( stdout, '("Total force in z direction = 0 disabled")')
     !
     IF ( (do_comp_esm .AND. ( esm_bc /= 'pbc' )).OR.(gate.AND.relaxz) ) THEN
        !
        ! ... impose total force along xy = 0
        !
        DO na = 1, nat
           IF ( ipol /= 3) force(ipol,na) = force(ipol,na)  &
                                            - sumfor / DBLE( nat )
        ENDDO
        !
     ELSEIF ( qmmm_mode < 0 ) THEN
        !
        ! ... impose total force = 0 except in a QM-MM calculation
        !
        DO na = 1, nat
           force(ipol,na) = force(ipol,na) - sumfor / DBLE( nat ) 
        ENDDO
        !
     ENDIF
     !
  ENDDO
  !
  ! ... resymmetrize (should not be needed, but ...)
  !
  CALL symvector( nat, force )
  !
  IF ( remove_rigid_rot ) &
     CALL remove_tot_torque( nat, tau, amass(ityp(:)), force  )
  !
  IF( textfor ) force(:,:) = force(:,:) + extfor(:,:)
  !
  ! ... call void routine for user define/ plugin patches on external forces
  !
  CALL plugin_ext_forces()
  !
  ! ... write on output the forces
  !
  WRITE( stdout, '(/,5x,"Forces acting on atoms (cartesian axes, Ry/au):", / )')
  DO na = 1, nat
     WRITE( stdout, 9035) na, ityp(na), force(:,na)
  ENDDO
  !
  ! ... forces on fixed coordinates are set to zero ( C.S. 15/10/2003 )
  !
  force(:,:)    = force(:,:)    * DBLE( if_pos )
  forcescc(:,:) = forcescc(:,:) * DBLE( if_pos )
  !
  IF ( iverbosity > 0 ) THEN
     IF ( do_comp_mt ) THEN
        WRITE( stdout, '(5x,"The Martyna-Tuckerman correction term to forces")')
        DO na = 1, nat
           WRITE( stdout, 9035) na, ityp(na), ( force_mt(ipol,na), ipol = 1, 3 )
        ENDDO
     END IF
     !
     WRITE( stdout, '(5x,"The non-local contrib.  to forces")')
     DO na = 1, nat
        WRITE( stdout, 9035) na, ityp(na), ( forcenl(ipol,na), ipol = 1, 3 )
     ENDDO
     WRITE( stdout, '(5x,"The ionic contribution  to forces")')
     DO na = 1, nat
        WRITE( stdout, 9035) na, ityp(na), ( forceion(ipol,na), ipol = 1, 3 )
     ENDDO
     WRITE( stdout, '(5x,"The local contribution  to forces")')
     DO na = 1, nat
        WRITE( stdout, 9035) na, ityp(na), ( forcelc(ipol,na), ipol = 1, 3 )
     ENDDO
     WRITE( stdout, '(5x,"The core correction contribution to forces")')
     DO na = 1, nat
        WRITE( stdout, 9035) na, ityp(na), ( forcecc(ipol,na), ipol = 1, 3 )
     ENDDO
     WRITE( stdout, '(5x,"The Hubbard contrib.    to forces")')
     DO na = 1, nat
        WRITE( stdout, 9035) na, ityp(na), ( forceh(ipol,na), ipol = 1, 3 )
     ENDDO
     WRITE( stdout, '(5x,"The SCF correction term to forces")')
     DO na = 1, nat
        WRITE( stdout, 9035) na, ityp(na), ( forcescc(ipol,na), ipol = 1, 3 )
     ENDDO
     !
     IF ( llondon) THEN
        WRITE( stdout, '(/,5x,"Dispersion contribution to forces:")')
        DO na = 1, nat
           WRITE( stdout, 9035) na, ityp(na), (force_disp(ipol,na), ipol = 1, 3)
        ENDDO
     END IF
     !
     IF ( ldftd3 ) THEN
        WRITE( stdout, '(/,5x,"DFT-D3 dispersion contribution to forces:")')
        DO na = 1, nat
           WRITE( stdout, 9035) na, ityp(na), (force_d3(ipol,na), ipol = 1, 3)
        ENDDO
     END IF
     !
     IF (lxdm) THEN
        WRITE( stdout, '(/,5x,"XDM contribution to forces:")')
        DO na = 1, nat
           WRITE( stdout, 9035) na, ityp(na), (force_disp_xdm(ipol,na), ipol = 1, 3)
        ENDDO
     END IF
     !
     IF ( ts_vdw) THEN
        WRITE( stdout, '(/,5x,"TS-VDW contribution to forces:")')
        DO na = 1, nat
           WRITE( stdout, 9035) na, ityp(na), (2.0d0*FtsvdW(ipol,na), ipol=1,3)
        ENDDO
     END IF
     !
     ! TB gate forces
     IF ( gate ) THEN
        WRITE( stdout, '(/,5x,"Gate contribution to forces:")')
        DO na = 1, nat
           WRITE( stdout, 9035) na, ityp(na), (forcegate(ipol,na), ipol = 1, 3)
        ENDDO
     END IF
     !
  END IF
  !
  sumfor = 0.D0
  sumscf = 0.D0
  !
  DO na = 1, nat
     !
     sumfor = sumfor + force(1,na)**2 + force(2,na)**2 + force(3,na)**2
     sumscf = sumscf + forcescc(1,na)**2 + forcescc(2,na)**2+ forcescc(3,na)**2
     !
  ENDDO
  !
  sumfor = SQRT( sumfor )
  sumscf = SQRT( sumscf )
  !
  WRITE( stdout, '(/5x,"Total force = ",F12.6,5X, &
              &  "Total SCF correction = ",F12.6)') sumfor, sumscf
  !
  IF ( llondon .AND. iverbosity > 0 ) THEN
     !
     sum_mm = 0.D0
     DO na = 1, nat
        sum_mm = sum_mm + &
                 force_disp(1,na)**2 + force_disp(2,na)**2 + force_disp(3,na)**2
     ENDDO
     sum_mm = SQRT( sum_mm )
     WRITE ( stdout, '(/,5x, "Total Dispersion Force = ",F12.6)') sum_mm
     !
  END IF
  !
  IF ( ldftd3 .AND. iverbosity > 0 ) THEN
     !
     sum_mm = 0.D0
     DO na = 1, nat
        sum_mm = sum_mm + &
                 force_d3(1,na)**2 + force_d3(2,na)**2 + force_d3(3,na)**2
     ENDDO
     sum_mm = SQRT( sum_mm )
     WRITE ( stdout, '(/,5x, "DFT-D3 dispersion Force = ",F12.6)') sum_mm
     !
  END IF
  !
  IF ( lxdm .AND. iverbosity > 0 ) THEN
     !
     sum_mm = 0.D0
     DO na = 1, nat
        sum_mm = sum_mm + &
                 force_disp_xdm(1,na)**2 + force_disp_xdm(2,na)**2 + force_disp_xdm(3,na)**2
     ENDDO
     sum_mm = SQRT( sum_mm )
     WRITE ( stdout, '(/,5x, "Total XDM Force = ",F12.6)') sum_mm
     !
  END IF
  !
  DEALLOCATE( forcenl, forcelc, forcecc, forceh, forceion, forcescc )
  IF ( llondon  ) DEALLOCATE( force_disp       )
  IF ( ldftd3   ) DEALLOCATE( force_d3         )
  IF ( lxdm     ) DEALLOCATE( force_disp_xdm   ) 
  IF ( lelfield ) DEALLOCATE( forces_bp_efield )
  !
  lforce = .TRUE.
  !
  CALL stop_clock( 'forces' )
  !
  IF ( ( sumfor < 10.D0*sumscf ) .AND. ( sumfor > nat*eps ) ) &
  WRITE( stdout,'(5x,"SCF correction compared to forces is large: ", &
                   &  "reduce conv_thr to get better values")')
  !
  IF(ALLOCATED(force_mt))   DEALLOCATE( force_mt )

  RETURN
  !
9035 FORMAT(5X,'atom ',I4,' type ',I2,'   force = ',3F14.8)
  !
END SUBROUTINE forces
