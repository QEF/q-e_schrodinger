!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#define TEST_NEW_PRECONDITIONING
!
!-----------------------------------------------------------------------
SUBROUTINE g_psi( lda, n, m, npol, psi, e )
  !-----------------------------------------------------------------------
  !! This routine computes an estimate of the inverse Hamiltonian
  !! and applies it to m wavefunctions.
  !
  USE kinds
  USE g_psi_mod
  USE g_psi_mod_gpum, ONLY : using_h_diag, using_s_diag
  !
  IMPLICIT NONE
  !
  INTEGER :: lda
  !! input: the leading dimension of psi
  INTEGER :: n
  !! input: the real dimension of psi
  INTEGER :: m
  !! input: the number of coordinates of psi
  INTEGER :: npol
  !! input: the number of bands
  COMPLEX(DP) :: psi(lda, npol, m)
  !! inp/out: the psi vector
  REAL(DP) :: e(m)
  !! input: the eigenvectors
  !
  !  ... local variables
  !
  INTEGER :: ipol
  ! counter of coordinates of psi
  REAL(DP), PARAMETER :: eps = 1.0d-4
  ! a small number
  REAL(DP) :: x, scala, denm
  !
  INTEGER :: k, i
  ! counter on psi functions
  ! counter on G vectors
  INTEGER, PARAMETER :: blocksize = 256
  INTEGER :: iblock, numblock
  ! chunking parameters
  !
  CALL using_h_diag(0); call using_s_diag(0)
  CALL start_clock( 'g_psi' )
  !
  ! compute the number of chuncks
  numblock  = (n+blocksize-1)/blocksize
  !
#ifdef TEST_NEW_PRECONDITIONING
  scala = 1.d0
  !$omp parallel do collapse(3) private(x, denm)
  DO k = 1, m
     DO ipol=1, npol
        DO iblock = 1, numblock
           DO i = (iblock-1)*blocksize+1, MIN( iblock*blocksize, n )
              x = (h_diag(i,ipol) - e(k)*s_diag(i,ipol))*scala
              denm = 0.5_dp*(1.d0+x+SQRT(1.d0+(x-1)*(x-1.d0)))/scala
              psi (i, ipol, k) = psi (i, ipol, k) / denm
           ENDDO
        ENDDO
     ENDDO
  ENDDO
  !$omp end parallel do
#else
  !$omp parallel do collapse(3) private(denm)
  DO ipol=1,npol
     DO k = 1, m
        DO iblock = 1, numblock
           DO i = (iblock-1)*blocksize+1, MIN( iblock*blocksize, n )
              denm = h_diag(i,ipol) - e(k) * s_diag(i,ipol)
              !
              ! denm = g2+v(g=0) - e(k)
              !
                 IF (ABS(denm) < eps) denm = SIGN( eps, denm )
              !
              ! denm = sign( max( abs(denm),eps ), denm )
              !
              psi(i, ipol, k) = psi(i, ipol, k) / denm
           ENDDO
        ENDDO
     ENDDO
  ENDDO
  !$omp end parallel do
#endif
  !
  CALL stop_clock( 'g_psi' )
  !
  RETURN
  !
END SUBROUTINE g_psi

!-----------------------------------------------------------------------
subroutine g_1psi (lda, n, psi, e)
  !-----------------------------------------------------------------------
  !
  !    This routine computes an estimate of the inverse Hamiltonian
  !    and applies it to one wavefunction
  !
  USE kinds
  USE noncollin_module,     ONLY : npol
  USE iso_c_binding

  implicit none

  integer :: lda, & ! input: the leading dimension of psi
             n      ! input: the real dimension of psi
  complex(DP) :: psi (lda, npol) ! inp/out: the psi vector
  real(DP), target :: e     ! input: the eigenvectors
  real(DP), dimension(:), pointer :: e_vec
  !
  call start_clock ('g_1psi')

  ! cast scalar to size 1 vector to exactly match g_psi argument type
  call C_F_POINTER(C_LOC(e), e_vec, [1])
  CALL g_psi (lda, n, 1, npol, psi, e_vec)

  call stop_clock ('g_1psi')

  return

end subroutine g_1psi
