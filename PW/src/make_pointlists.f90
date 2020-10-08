!
! Copyright (C) 2001-2014 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!--------------------------------------------------------------------------
SUBROUTINE compute_distances_SoA( pos, N, RSoA, Distances )
  !--------------------------------------------------------------------------
  !! This routine computes the distance between pos and all the centers in RSoA
  !! All the positions are in crystal coordinates
  !
  USE kinds,        ONLY : DP
  USE cell_base,    ONLY : at
  !
  IMPLICIT NONE
  !
  REAL(DP), INTENT(IN) :: pos(3)
  !! reference position
  INTEGER, INTENT(IN) :: N 
  !! number of elements in RSoA
  REAL(DP), INTENT(IN) :: RSoA(N,3)
  !! positions in SoA layout
  REAL(DP), INTENT(OUT) :: Distances(N)
  !! minimal distances
  !
  ! ... local variables
  !
  REAL(DP) :: corners(3,8)
  REAL(DP) :: dx, dy, dz, dx_c, dy_c, dz_c
  REAL(DP) :: dist, dist_min
  INTEGER :: ix, iy, iz, ic, iat
  !
  !
  ic = 0
  DO ix = 0, 1
    dx = DBLE(-ix)
    DO iy = 0, 1
      dy = DBLE(-iy)
      DO iz = 0, 1
        dz = DBLE(-iz)
        ic = ic + 1
        corners(1,ic) = dx*at(1,1) + dy*at(1,2) + dz*at(1,3)
        corners(2,ic) = dx*at(2,1) + dy*at(2,2) + dz*at(2,3)
        corners(3,ic) = dx*at(3,1) + dy*at(3,2) + dz*at(3,3)
      ENDDO
    ENDDO
  ENDDO
  !
  DO iat = 1, N
    dx = RSoA(iat,1) - pos(1)
    dx = dx - FLOOR(dx)
    dy = RSoA(iat,2) - pos(2)
    dy = dy - FLOOR(dy)
    dz = RSoA(iat,3) - pos(3)
    dz = dz - FLOOR(dz)
    dx_c = dx*at(1,1) + dy*at(1,2) + dz*at(1,3)
    dy_c = dx*at(2,1) + dy*at(2,2) + dz*at(2,3)
    dz_c = dx*at(3,1) + dy*at(3,2) + dz*at(3,3)
    dist_min = dx_c*dx_c + dy_c*dy_c + dz_c*dz_c;
    DO ic = 2, 8
      dx = dx_c + corners(1,ic)
      dy = dy_c + corners(2,ic)
      dz = dz_c + corners(3,ic)
      dist = dx*dx + dy*dy + dz*dz
      IF (dist < dist_min) dist_min = dist
    ENDDO
    Distances(iat) = SQRT(dist_min)
  ENDDO
  !
END SUBROUTINE compute_distances_SoA
!
!--------------------------------------------------------------------------
SUBROUTINE make_pointlists
  !--------------------------------------------------------------------------
  !! This initialization is needed in order to integrate charge (or
  !! magnetic moment) in a sphere around the atomic positions.
  !! This can be used to simply monitor these quantities during the scf
  !! cycles or in order to calculate constrains on these quantities.
  !
  !! If the integration radius r_m is not provided in input, it is
  !! calculated here. The integration is a sum over all points in real
  !! space with the weight 1, if they are closer than r_m to an atom
  !! and \(1 - (\text{distance}-r_m)/(0.2*r_m) \) if \( r_m < 
  !! \text{distance}<1.2*r_m \).
  !
  USE kinds,               ONLY : DP
  USE io_global,           ONLY : stdout
  USE ions_base,           ONLY : nat, tau, ntyp => nsp, ityp
  USE cell_base,           ONLY : at, bg, alat
  USE mp_bands,            ONLY : me_bgrp
  USE fft_base,            ONLY : dfftp
  USE fft_types,           ONLY : fft_index_to_3d
  USE noncollin_module,    ONLY : factlist, pointlist, r_m
  !
  IMPLICIT NONE
  !
  INTEGER :: indproc, iat, ir, iat1
  INTEGER :: i, j, k, i0, j0, k0, ipol, nt, nt1
  LOGICAL :: offrange
  REAL(DP) :: posi(3), WS_radius, dist
  REAL(DP), ALLOCATABLE :: tau0(:,:), tau_SoA(:,:), distmin(:), distances(:)
  !
  WRITE( stdout,'(5x,"Generating pointlists ...")')
  !
  ALLOCATE( tau0(3,nat), tau_SoA(nat,3), distances(nat) )
  ALLOCATE( distmin(ntyp) )
  !
  ! ... Bring all the atomic positions on the first unit cell
  !
  tau0 = tau
  CALL cryst_to_cart( nat, tau0, bg, -1 )
  DO iat = 1, nat
     DO ipol = 1, 3
        tau_SoA(iat,ipol) = tau0(ipol,iat)
     ENDDO
  ENDDO
  !
  ! ... Check the minimum distance between two atoms in the system
  WS_radius = 1.d100
  DO i = -1,1
    DO j = -1,1
      DO k = -1,1
        IF ( i==0 .AND. j==0 .AND. k==0 ) CYCLE
        dist = ( DBLE(i)*at(1,1) + DBLE(j)*at(1,2) + DBLE(k)*at(1,3) )**2 &
             + ( DBLE(i)*at(2,1) + DBLE(j)*at(2,2) + DBLE(k)*at(2,3) )**2 &
             + ( DBLE(i)*at(3,1) + DBLE(j)*at(3,2) + DBLE(k)*at(3,3) )**2
        IF (dist<WS_radius) WS_radius = dist
      ENDDO
    ENDDO
  ENDDO
  WS_radius = SQRT(WS_radius)
  !
  distmin(:) = WS_radius
  !
  DO iat = 1,nat
     nt = ityp(iat)
     CALL compute_distances_SoA( tau0(1:3,iat), nat, tau_SoA, distances )
     DO iat1 = 1,nat
        IF ( iat == iat1 ) CYCLE
        nt1 = ityp(iat1)
        IF (distances(iat1) < distmin(nt)) distmin(nt) = distances(iat1)
        IF (distances(iat1) < distmin(nt1)) distmin(nt1) = distances(iat1)
     ENDDO                  ! iat1
  ENDDO                     ! iat
  !
  DO nt = 1, ntyp
     IF ((distmin(nt) < (2.d0*r_m(nt)*1.2d0)).OR.(r_m(nt) < 1.d-8)) THEN
     ! ... set the radius r_m to a value a little smaller than the minimum
     ! distance divided by 2*1.2 (so no point in space can belong to more
     ! than one atom)
        r_m(nt) = 0.5d0*distmin(nt)/1.2d0 * 0.99d0
        WRITE( stdout,'(5x,"new r_m : ",f8.4," (alat units)", f8.4, &
                          &" (a.u.) for type",i5)') &
                                        r_m(nt), r_m(nt) * alat, nt
     ENDIF
  ENDDO
  DEALLOCATE( distmin )
  !
  ! ... now, set for every point in the fft grid an index corresponding
  ! to the atom whose integration sphere the grid point belong to.
  ! if the point is outside of all spherical regions set the index to 0.
  ! Set as well the integration weight
  ! This also works in the parallel case.
  !
  pointlist(:) = 0
  factlist(:) = 0.d0
  !
  DO ir = 1, dfftp%nr1x*dfftp%my_nr2p*dfftp%my_nr3p
     ! ... check result vector boundary
     IF( ir > SIZE(factlist) .OR. ir > SIZE(pointlist) )  &
       CALL errore( ' make_pointlists ', ' inconsistent sizes ', 1 )
     !
     ! ... three dimensional indexes
     !
     CALL fft_index_to_3d (ir, dfftp, i0,j0,k0, offrange)
     IF ( offrange ) CYCLE
     !
     posi(1) = DBLE(i0)/DBLE(dfftp%nr1)
     posi(2) = DBLE(j0)/DBLE(dfftp%nr2)
     posi(3) = DBLE(k0)/DBLE(dfftp%nr3)
     CALL compute_distances_SoA( posi, nat, tau_SoA, distances )
     !
     DO iat = 1,nat
        nt=ityp(iat)
        IF (distances(iat) <= r_m(nt)) THEN
           factlist(ir) = 1.d0
           pointlist(ir) = iat
           EXIT
        ELSEIF (distances(iat) <= 1.2*r_m(nt)) THEN
           factlist(ir) = 1.d0 - (distances(iat) - r_m(nt))/(0.2d0*r_m(nt))
           pointlist(ir) = iat
           EXIT
        ENDIF
     ENDDO
  ENDDO       ! ir
  !
  DEALLOCATE( tau0, tau_SoA, distances )
  !
END SUBROUTINE make_pointlists
