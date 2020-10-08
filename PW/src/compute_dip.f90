!
! Copyright (C) 2003-2017 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!          25/06/2009 (Riccardo Sabatini)
!               reformulation using a unique saw(x) function (included in 
!               cell_base) in all e-field related routines and inclusion of 
!               a macroscopic electronic dipole contribution in the mixing 
!               scheme. 
!
!   the calculation of the dipole is split in the ionic (compute_ion_dip)
!   and electronic (compute_el_dip) contributions.
!
! TB
! included gate in the calculation of the dipole
! search for 'TB'
!
!------------------------------------------------------------------------------
SUBROUTINE compute_ion_dip( emaxpos, eopreg, edir, ion_dipole )
  !---------------------------------------------------------------------------
  !! Calculation of the dipole - ionic contribution.
  !
  USE io_global,  ONLY : stdout, ionode
  USE ions_base,  ONLY : nat, ityp, tau, zv
  USE constants,  ONLY : fpi
  USE kinds,      ONLY : DP
  USE cell_base,  ONLY : at, bg, omega, alat
  USE klist,      ONLY : nelec !TB
  USE extfield,   ONLY : gate, dipfield, zgate, saw
  !
  IMPLICIT NONE
  !
  REAL(DP), INTENT(IN) :: emaxpos
  !! position of the maximum of the field (0<emaxpos<1)
  REAL(DP), INTENT(IN) :: eopreg
  !! amplitude of the inverse region (0<eopreg<1)
  INTEGER, INTENT(IN)  :: edir
  !! direction of the field
  REAL(DP), INTENT(OUT) ::  ion_dipole
  !! ionic dipole
  !
  ! ... local variables
  !
  REAL(DP) :: bmod
  INTEGER  :: na
  REAL(DP) :: sawarg, tvectb, zvia, ionic_charge !TB added ionic_charge

  !--------------------------
  !  Fix some values for later calculations
  !--------------------------
  bmod=SQRT(bg(1,edir)**2+bg(2,edir)**2+bg(3,edir)**2)

  !--------------------------
  !  Calculate IONIC dipole
  !--------------------------
  
  !
  ! P_{ion} = \sum^{nat}_{s} z_{v} Saw\left( \vec{t_{s}}\cdot\vec{b_{edir}}} 
  !                            \right) \frac{alat}{bmod} \frac{4\pi}{\Omega}
  !

  ion_dipole=0.d0
  
  DO na = 1, nat
     !
     ! Ion charge
     zvia = zv(ityp(na))
     
     ! Position vector
     tvectb = tau(1,na)*bg(1,edir) + tau(2,na)*bg(2,edir) + tau(3,na)*bg(3,edir)

     ion_dipole = ion_dipole + zvia* saw(emaxpos,eopreg, tvectb ) &
                                                * (alat/bmod) * (fpi/omega)
 
  END DO

  !
  ! if the gate is used to represent the background charge we need to include
  ! the charged plate in the calculation of the dipole
  !
  IF (gate.AND.dipfield) THEN
     ionic_charge = SUM( zv(ityp(1:nat)) )
     ion_dipole = ion_dipole + (nelec-ionic_charge) * saw(emaxpos,eopreg, zgate ) &
                                                * (alat/bmod) * (fpi/omega)
  ENDIF
  
  RETURN
  
END SUBROUTINE compute_ion_dip
!
!------------------------------------------------------------------------------
SUBROUTINE compute_el_dip( emaxpos, eopreg, edir, charge, e_dipole )
  !---------------------------------------------------------------------------
  !! Calculation of the dipole - electronic contribution.
  !
  USE io_global,  ONLY : stdout, ionode
  USE lsda_mod,   ONLY : nspin
  USE constants,  ONLY : fpi
  USE kinds,      ONLY : DP
  USE cell_base,  ONLY : at, bg, omega, alat
  USE fft_base,   ONLY : dfftp
  USE fft_types,  ONLY : fft_index_to_3d
  USE extfield,   ONLY : saw
  USE mp_bands,   ONLY : me_bgrp, intra_bgrp_comm
  USE mp,         ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  REAL(DP), INTENT(IN) :: emaxpos
  !! position of the maximum of the field (0<emaxpos<1)
  REAL(DP), INTENT(IN) :: eopreg
  !! amplitude of the inverse region (0<eopreg<1)
  REAL(DP), INTENT(IN), DIMENSION(dfftp%nnr) :: charge
  !! charge density
  INTEGER, INTENT(IN)  :: edir
  !! direction of the field
  REAL(DP), INTENT(OUT) ::  e_dipole
  !! electronic dipole
  !
  ! ... local variables
  !
  REAL(DP), ALLOCATABLE :: rho_all(:), aux(:)
  REAL(DP) :: rhoir,bmod
  REAL(DP) :: sawarg, tvectb
  INTEGER  :: i, k, j, ip, ir, na
  LOGICAL  :: offrange
  !--------------------------
  !  Fix some values for later calculations
  !--------------------------
  bmod=SQRT(bg(1,edir)**2+bg(2,edir)**2+bg(3,edir)**2)

  !
  !--------------------------
  !  Calculate ELECTRONIC dipole
  !--------------------------

  !
  ! Case with edir = 3 (in the formula changes only the argument of saw, i for
  ! edir=1 and j for edir = 2)
  !
  ! P_{ele} = \sum_{ijk} \rho_{r_{ijk}} Saw\left( \frac{k}{nr3} \right)
  !                   \frac{alat}{bmod} \frac{\Omega}{nrxx} \frac{4\pi}{\Omega}
  !
  e_dipole  = 0.D0
  !
  ! Loop in the charge array
  !
  DO ir = 1, dfftp%nr1x*dfftp%my_nr2p*dfftp%my_nr3p
     !
     ! ... three dimensional indexes
     !
     CALL fft_index_to_3d (ir, dfftp, i,j,k, offrange)
     ! IF ( offrange ) CYCLE ! NB this was commented before
     !
     ! Define the argument for the saw function     
     !
     if (edir.eq.1) sawarg = DBLE(i)/DBLE(dfftp%nr1)
     if (edir.eq.2) sawarg = DBLE(j)/DBLE(dfftp%nr2)
     if (edir.eq.3) sawarg = DBLE(k)/DBLE(dfftp%nr3)

     rhoir = charge(ir)

     e_dipole = e_dipole + rhoir * saw(emaxpos,eopreg, sawarg) &
                      * (alat/bmod) * (fpi/(dfftp%nr1*dfftp%nr2*dfftp%nr3))

  END DO

  CALL mp_sum(  e_dipole , intra_bgrp_comm )

  RETURN

END SUBROUTINE compute_el_dip
