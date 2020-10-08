!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!------------------------------------------------------------------------------------
SUBROUTINE rotate_wfc( npwx, npw, nstart, gstart, nbnd, psi, npol, overlap, evc, e )
  !--------------------------------------------------------------------------------
  !! Driver routine (maybe it should be an interface) for Hamiltonian 
  !! diagonalization in the subspace spanned by nstart states 
  !! psi (atomic or random wavefunctions). 
  !! Produces on output nbnd eigenvectors ( nbnd <= nstart ) in evc.
  !! Calls \(\texttt{h_psi, s_psi}\) to calculate \(H|psi\rangle\) and
  !! \(S|psi\rangle\). 
  !! It only uses an auxiliary array of the same size as psi.
  !
  USE kinds,           ONLY: DP
  USE control_flags,   ONLY: use_para_diag, gamma_only
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: npw
  !! dimension of the matrix to be diagonalized
  INTEGER, INTENT(IN) :: npwx
  !! leading dimension of matrix psi, as declared in the calling pgm unit
  INTEGER, INTENT(IN) :: nstart
  !! input number of states
  INTEGER, INTENT(IN) :: nbnd
  !! output number of states
  INTEGER, INTENT(IN) :: gstart
  !! first G with nonzero norm
  INTEGER, INTENT(IN) :: npol
  !! number of spin polarizations
  LOGICAL, INTENT(IN) :: overlap
  !! if .FALSE. \(S|psi\rangle\) not needed
  COMPLEX(DP), INTENT(INOUT) :: psi(npwx*npol,nstart)
  !! I/O eigenvectors (may overlap)
  COMPLEX(DP), INTENT(INOUT) :: evc(npwx*npol,nbnd)
  !! I/O eigenvectors (may overlap)
  REAL(DP), INTENT(OUT) :: e(nbnd)
  !! eigenvalues
  EXTERNAL  h_psi, s_psi
  ! h_psi(npwx,npw,nvec,psi,hpsi)
  !     calculates H|psi>
  ! s_psi(npwx,npw,nvec,spsi)
  !     calculates S|psi> (if needed)
  !     Vectors psi,hpsi,spsi are dimensioned (npwx,npol,nvec)
  !
  CALL start_clock( 'wfcrot' ); !write (*,*) 'start wfcrot' ; FLUSH(6)
  !write (*,*) 'gamma_only' , gamma_only; FLUSH(6)
  !
  IF( use_para_diag ) THEN
     !
     ! use data distributed subroutine
     !
     IF ( gamma_only ) THEN
        !write (*,*) 'inside para gamma'; FLUSH(6)
        !
        CALL protate_wfc_gamma ( h_psi, s_psi, overlap, &
                                 npwx, npw, nstart, nbnd, psi, evc, e )
        !
     ELSE
        !write (*,*) 'inside para k'; FLUSH(6)
        !
        CALL protate_wfc_k( h_psi, s_psi, overlap, &
                            npwx, npw, nstart, nbnd, npol, psi, evc, e )
        !
     ENDIF
     !
  ELSE
     !
     ! use serial subroutines
     !
     IF ( gamma_only ) THEN
        !write (*,*) 'inside serial gamma'; FLUSH(6)
        !
        CALL rotate_wfc_gamma( h_psi, s_psi, overlap, &
                               npwx, npw, nstart, nbnd, psi, evc, e )
        !
     ELSE
  !write (*,*) 'inside serial k'; FLUSH(6)
        !
        CALL rotate_wfc_k( h_psi, s_psi, overlap, &
                           npwx, npw, nstart, nbnd, npol, psi, evc, e )
        !
     ENDIF
     !
  ENDIF
  !
  CALL stop_clock( 'wfcrot' ); !write (*,*) 'stop wfcrot' ; FLUSH(6)
  !
END SUBROUTINE rotate_wfc
