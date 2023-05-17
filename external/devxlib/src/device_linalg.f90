!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, AF
!
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
!
! Utility functions to perform memcpy and memset on the device with CUDA Fortran
! cuf_memXXX contains a CUF KERNEL to perform the selected operation
! cu_memsync are wrappers for cuda_memcpy functions
!
#if defined(_CUDA)
#  define __CUDA
#endif

module device_linalg_loc
#ifdef __CUDA
    use cublas
#endif
    implicit none
    integer, parameter :: DP = selected_real_kind(14,200)
    integer, parameter :: SP = selected_real_kind(6, 37)
end module
!
SUBROUTINE dp_dev_memcpy_r1d(array_out, array_in, &
                                            range1 )
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:)
    REAL(PRCSN), INTENT(IN) :: array_in(:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    ENDDO
    !
END SUBROUTINE dp_dev_memcpy_r1d
!
SUBROUTINE dp_dev_memcpy_r2d(array_out, array_in, &
                                            range1, &
                                            range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:)
    REAL(PRCSN), INTENT(IN) :: array_in(:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memcpy_r2d
!
SUBROUTINE dp_dev_memcpy_r3d(array_out, array_in, &
                                            range1, &
                                            range2, &
                                            range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    REAL(PRCSN), INTENT(IN) :: array_in(:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memcpy_r3d
!
SUBROUTINE dp_dev_memcpy_r4d(array_out, array_in, &
                                            range1, &
                                            range2, &
                                            range3, &
                                            range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    REAL(PRCSN), INTENT(IN) :: array_in(:,:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_out, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memcpy_r4d
!
SUBROUTINE dp_dev_memcpy_c1d(array_out, array_in, &
                                            range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:)
    COMPLEX(PRCSN), INTENT(IN) :: array_in(:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    ENDDO
    !
END SUBROUTINE dp_dev_memcpy_c1d
!
SUBROUTINE dp_dev_memcpy_c2d(array_out, array_in, &
                                            range1, &
                                            range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:)
    COMPLEX(PRCSN), INTENT(IN) :: array_in(:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memcpy_c2d
!
SUBROUTINE dp_dev_memcpy_c3d(array_out, array_in, &
                                            range1, &
                                            range2, &
                                            range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    COMPLEX(PRCSN), INTENT(IN) :: array_in(:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memcpy_c3d
!
SUBROUTINE dp_dev_memcpy_c4d(array_out, array_in, &
                                            range1, &
                                            range2, &
                                            range3, &
                                            range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    COMPLEX(PRCSN), INTENT(IN) :: array_in(:,:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_out, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memcpy_c4d
!
SUBROUTINE sp_dev_memcpy_r1d(array_out, array_in, &
                                            range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:)
    REAL(PRCSN), INTENT(IN) :: array_in(:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    ENDDO
    !
END SUBROUTINE sp_dev_memcpy_r1d
!
SUBROUTINE sp_dev_memcpy_r2d(array_out, array_in, &
                                            range1, &
                                            range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:)
    REAL(PRCSN), INTENT(IN) :: array_in(:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memcpy_r2d
!
SUBROUTINE sp_dev_memcpy_r3d(array_out, array_in, &
                                            range1, &
                                            range2, &
                                            range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    REAL(PRCSN), INTENT(IN) :: array_in(:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memcpy_r3d
!
SUBROUTINE sp_dev_memcpy_r4d(array_out, array_in, &
                                            range1, &
                                            range2, &
                                            range3, &
                                            range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    REAL(PRCSN), INTENT(IN) :: array_in(:,:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_out, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memcpy_r4d
!
SUBROUTINE sp_dev_memcpy_c1d(array_out, array_in, &
                                            range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:)
    COMPLEX(PRCSN), INTENT(IN) :: array_in(:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    ENDDO
    !
END SUBROUTINE sp_dev_memcpy_c1d
!
SUBROUTINE sp_dev_memcpy_c2d(array_out, array_in, &
                                            range1, &
                                            range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:)
    COMPLEX(PRCSN), INTENT(IN) :: array_in(:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memcpy_c2d
!
SUBROUTINE sp_dev_memcpy_c3d(array_out, array_in, &
                                            range1, &
                                            range2, &
                                            range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    COMPLEX(PRCSN), INTENT(IN) :: array_in(:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memcpy_c3d
!
SUBROUTINE sp_dev_memcpy_c4d(array_out, array_in, &
                                            range1, &
                                            range2, &
                                            range3, &
                                            range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    COMPLEX(PRCSN), INTENT(IN) :: array_in(:,:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out, array_in
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_out, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memcpy_c4d
!
!
SUBROUTINE dp_dev_memset_r1d(array_out, val, range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:)
    REAL(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_out(i1 ) = val
    ENDDO
    !
END SUBROUTINE dp_dev_memset_r1d
!
SUBROUTINE dp_dev_memset_r2d(array_out, val, range1, range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:)
    REAL(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2 ) = val
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memset_r2d
!
SUBROUTINE dp_dev_memset_r3d(array_out, val, range1, range2, range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    REAL(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memset_r3d
!
SUBROUTINE dp_dev_memset_r4d(array_out, val, range1, range2, range3, range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    REAL(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_out, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memset_r4d
!
SUBROUTINE dp_dev_memset_c1d(array_out, val, range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:)
    COMPLEX(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_out(i1 ) = val
    ENDDO
    !
END SUBROUTINE dp_dev_memset_c1d
!
SUBROUTINE dp_dev_memset_c2d(array_out, val, range1, range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:)
    COMPLEX(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2 ) = val
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memset_c2d
!
SUBROUTINE dp_dev_memset_c3d(array_out, val, range1, range2, range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    COMPLEX(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memset_c3d
!
SUBROUTINE dp_dev_memset_c4d(array_out, val, range1, range2, range3, range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    COMPLEX(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_out, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_memset_c4d
!
SUBROUTINE sp_dev_memset_r1d(array_out, val, range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:)
    REAL(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_out(i1 ) = val
    ENDDO
    !
END SUBROUTINE sp_dev_memset_r1d
!
SUBROUTINE sp_dev_memset_r2d(array_out, val, range1, range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:)
    REAL(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2 ) = val
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memset_r2d
!
SUBROUTINE sp_dev_memset_r3d(array_out, val, range1, range2, range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    REAL(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memset_r3d
!
SUBROUTINE sp_dev_memset_r4d(array_out, val, range1, range2, range3, range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    REAL(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_out, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memset_r4d
!
SUBROUTINE sp_dev_memset_c1d(array_out, val, range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:)
    COMPLEX(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_out(i1 ) = val
    ENDDO
    !
END SUBROUTINE sp_dev_memset_c1d
!
SUBROUTINE sp_dev_memset_c2d(array_out, val, range1, range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:)
    COMPLEX(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2 ) = val
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memset_c2d
!
SUBROUTINE sp_dev_memset_c3d(array_out, val, range1, range2, range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    COMPLEX(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memset_c3d
!
SUBROUTINE sp_dev_memset_c4d(array_out, val, range1, range2, range3, range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    COMPLEX(PRCSN), INTENT(IN) :: val
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_out, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_out, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_out, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_out, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_memset_c4d
!
!
SUBROUTINE dp_h2d_memsync_r1d(array_out, array_in, range1 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:)
    INTEGER, INTENT(IN) ::  range1(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
#else
    array_out(d1_start:d1_start+d1_size-1) = array_in(d1_start:d1_start+d1_size-1)
#endif
    !
END SUBROUTINE dp_h2d_memsync_r1d
!
SUBROUTINE dp_h2d_memsync_r2d(array_out, array_in, range1, range2 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d2_ld, d1_size, d2_size )
#else

    !pinned_buffer(1:nbase, n_start:n_end) = sc_d( 1:nbase, n_start:n_end )
    !ierr = cudaMemcpy2D( pinned_buffer(1, n_start) , nvecx, sc_d( 1, n_start ), nvecx, nbase, n_end-n_start+1 )

    array_out(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1) =  &
                array_in(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1)
#endif
    !
END SUBROUTINE dp_h2d_memsync_r2d
!
SUBROUTINE dp_h2d_memsync_r3d(array_out, array_in, range1, range2, range3 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE dp_h2d_memsync_r3d
!
SUBROUTINE dp_h2d_memsync_r4d(array_out, array_in, range1, range2, range3, range4 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3), range4(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: d4_start, d4_size, d4_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    d4_start = 1
    d4_size = range4(2) - range4(1) + 1
    d4_ld = range4(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE dp_h2d_memsync_r4d
!
SUBROUTINE dp_h2d_memsync_c1d(array_out, array_in, range1 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:)
    INTEGER, INTENT(IN) ::  range1(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
#else
    array_out(d1_start:d1_start+d1_size-1) = array_in(d1_start:d1_start+d1_size-1)
#endif
    !
END SUBROUTINE dp_h2d_memsync_c1d
!
SUBROUTINE dp_h2d_memsync_c2d(array_out, array_in, range1, range2 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d2_ld, d1_size, d2_size )
#else

    !pinned_buffer(1:nbase, n_start:n_end) = sc_d( 1:nbase, n_start:n_end )
    !ierr = cudaMemcpy2D( pinned_buffer(1, n_start) , nvecx, sc_d( 1, n_start ), nvecx, nbase, n_end-n_start+1 )

    array_out(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1) =  &
                array_in(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1)
#endif
    !
END SUBROUTINE dp_h2d_memsync_c2d
!
SUBROUTINE dp_h2d_memsync_c3d(array_out, array_in, range1, range2, range3 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE dp_h2d_memsync_c3d
!
SUBROUTINE dp_h2d_memsync_c4d(array_out, array_in, range1, range2, range3, range4 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3), range4(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: d4_start, d4_size, d4_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    d4_start = 1
    d4_size = range4(2) - range4(1) + 1
    d4_ld = range4(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE dp_h2d_memsync_c4d
!
SUBROUTINE sp_h2d_memsync_r1d(array_out, array_in, range1 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:)
    INTEGER, INTENT(IN) ::  range1(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
#else
    array_out(d1_start:d1_start+d1_size-1) = array_in(d1_start:d1_start+d1_size-1)
#endif
    !
END SUBROUTINE sp_h2d_memsync_r1d
!
SUBROUTINE sp_h2d_memsync_r2d(array_out, array_in, range1, range2 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d2_ld, d1_size, d2_size )
#else

    !pinned_buffer(1:nbase, n_start:n_end) = sc_d( 1:nbase, n_start:n_end )
    !ierr = cudaMemcpy2D( pinned_buffer(1, n_start) , nvecx, sc_d( 1, n_start ), nvecx, nbase, n_end-n_start+1 )

    array_out(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1) =  &
                array_in(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1)
#endif
    !
END SUBROUTINE sp_h2d_memsync_r2d
!
SUBROUTINE sp_h2d_memsync_r3d(array_out, array_in, range1, range2, range3 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE sp_h2d_memsync_r3d
!
SUBROUTINE sp_h2d_memsync_r4d(array_out, array_in, range1, range2, range3, range4 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3), range4(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: d4_start, d4_size, d4_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    d4_start = 1
    d4_size = range4(2) - range4(1) + 1
    d4_ld = range4(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE sp_h2d_memsync_r4d
!
SUBROUTINE sp_h2d_memsync_c1d(array_out, array_in, range1 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:)
    INTEGER, INTENT(IN) ::  range1(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
#else
    array_out(d1_start:d1_start+d1_size-1) = array_in(d1_start:d1_start+d1_size-1)
#endif
    !
END SUBROUTINE sp_h2d_memsync_c1d
!
SUBROUTINE sp_h2d_memsync_c2d(array_out, array_in, range1, range2 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d2_ld, d1_size, d2_size )
#else

    !pinned_buffer(1:nbase, n_start:n_end) = sc_d( 1:nbase, n_start:n_end )
    !ierr = cudaMemcpy2D( pinned_buffer(1, n_start) , nvecx, sc_d( 1, n_start ), nvecx, nbase, n_end-n_start+1 )

    array_out(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1) =  &
                array_in(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1)
#endif
    !
END SUBROUTINE sp_h2d_memsync_c2d
!
SUBROUTINE sp_h2d_memsync_c3d(array_out, array_in, range1, range2, range3 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE sp_h2d_memsync_c3d
!
SUBROUTINE sp_h2d_memsync_c4d(array_out, array_in, range1, range2, range3, range4 )
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3), range4(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_out
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: d4_start, d4_size, d4_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    d4_start = 1
    d4_size = range4(2) - range4(1) + 1
    d4_ld = range4(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE sp_h2d_memsync_c4d
!
!
SUBROUTINE dp_d2h_memsync_r1d(array_out, array_in, range1 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:)
    INTEGER, INTENT(IN) ::  range1(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
#else
    array_out(d1_start:d1_start+d1_size-1) = array_in(d1_start:d1_start+d1_size-1)
#endif
    !
END SUBROUTINE dp_d2h_memsync_r1d
!
SUBROUTINE dp_d2h_memsync_r2d(array_out, array_in, range1, range2 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d2_ld, d1_size, d2_size )
#else

    !pinned_buffer(1:nbase, n_start:n_end) = sc_d( 1:nbase, n_start:n_end )
    !ierr = cudaMemcpy2D( pinned_buffer(1, n_start) , nvecx, sc_d( 1, n_start ), nvecx, nbase, n_end-n_start+1 )

    array_out(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1) =  &
                array_in(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1)
#endif
    !
END SUBROUTINE dp_d2h_memsync_r2d
!
SUBROUTINE dp_d2h_memsync_r3d(array_out, array_in, range1, range2, range3 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE dp_d2h_memsync_r3d
!
SUBROUTINE dp_d2h_memsync_r4d(array_out, array_in, range1, range2, range3, range4 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3), range4(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: d4_start, d4_size, d4_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    d4_start = 1
    d4_size = range4(2) - range4(1) + 1
    d4_ld = range4(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE dp_d2h_memsync_r4d
!
SUBROUTINE dp_d2h_memsync_c1d(array_out, array_in, range1 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:)
    INTEGER, INTENT(IN) ::  range1(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
#else
    array_out(d1_start:d1_start+d1_size-1) = array_in(d1_start:d1_start+d1_size-1)
#endif
    !
END SUBROUTINE dp_d2h_memsync_c1d
!
SUBROUTINE dp_d2h_memsync_c2d(array_out, array_in, range1, range2 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d2_ld, d1_size, d2_size )
#else

    !pinned_buffer(1:nbase, n_start:n_end) = sc_d( 1:nbase, n_start:n_end )
    !ierr = cudaMemcpy2D( pinned_buffer(1, n_start) , nvecx, sc_d( 1, n_start ), nvecx, nbase, n_end-n_start+1 )

    array_out(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1) =  &
                array_in(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1)
#endif
    !
END SUBROUTINE dp_d2h_memsync_c2d
!
SUBROUTINE dp_d2h_memsync_c3d(array_out, array_in, range1, range2, range3 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE dp_d2h_memsync_c3d
!
SUBROUTINE dp_d2h_memsync_c4d(array_out, array_in, range1, range2, range3, range4 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3), range4(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: d4_start, d4_size, d4_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    d4_start = 1
    d4_size = range4(2) - range4(1) + 1
    d4_ld = range4(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE dp_d2h_memsync_c4d
!
SUBROUTINE sp_d2h_memsync_r1d(array_out, array_in, range1 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:)
    INTEGER, INTENT(IN) ::  range1(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
#else
    array_out(d1_start:d1_start+d1_size-1) = array_in(d1_start:d1_start+d1_size-1)
#endif
    !
END SUBROUTINE sp_d2h_memsync_r1d
!
SUBROUTINE sp_d2h_memsync_r2d(array_out, array_in, range1, range2 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d2_ld, d1_size, d2_size )
#else

    !pinned_buffer(1:nbase, n_start:n_end) = sc_d( 1:nbase, n_start:n_end )
    !ierr = cudaMemcpy2D( pinned_buffer(1, n_start) , nvecx, sc_d( 1, n_start ), nvecx, nbase, n_end-n_start+1 )

    array_out(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1) =  &
                array_in(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1)
#endif
    !
END SUBROUTINE sp_d2h_memsync_r2d
!
SUBROUTINE sp_d2h_memsync_r3d(array_out, array_in, range1, range2, range3 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE sp_d2h_memsync_r3d
!
SUBROUTINE sp_d2h_memsync_r4d(array_out, array_in, range1, range2, range3, range4 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    REAL(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    REAL(PRCSN), INTENT(IN)    :: array_in(:,:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3), range4(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: d4_start, d4_size, d4_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    d4_start = 1
    d4_size = range4(2) - range4(1) + 1
    d4_ld = range4(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE sp_d2h_memsync_r4d
!
SUBROUTINE sp_d2h_memsync_c1d(array_out, array_in, range1 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:)
    INTEGER, INTENT(IN) ::  range1(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
#else
    array_out(d1_start:d1_start+d1_size-1) = array_in(d1_start:d1_start+d1_size-1)
#endif
    !
END SUBROUTINE sp_d2h_memsync_c1d
!
SUBROUTINE sp_d2h_memsync_c2d(array_out, array_in, range1, range2 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d2_ld, d1_size, d2_size )
#else

    !pinned_buffer(1:nbase, n_start:n_end) = sc_d( 1:nbase, n_start:n_end )
    !ierr = cudaMemcpy2D( pinned_buffer(1, n_start) , nvecx, sc_d( 1, n_start ), nvecx, nbase, n_end-n_start+1 )

    array_out(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1) =  &
                array_in(d1_start:d1_start+d1_size-1, d2_start:d2_start+d2_size-1)
#endif
    !
END SUBROUTINE sp_d2h_memsync_c2d
!
SUBROUTINE sp_d2h_memsync_c3d(array_out, array_in, range1, range2, range3 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE sp_d2h_memsync_c3d
!
SUBROUTINE sp_d2h_memsync_c4d(array_out, array_in, range1, range2, range3, range4 )
    !
#if defined(__CUDA)
    USE cudafor
#endif
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_out(:,:,:,:)
    COMPLEX(PRCSN), INTENT(IN)    :: array_in(:,:,:,:)
    INTEGER, INTENT(IN) ::  range1(3), range2(3), range3(3), range4(3)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_in
#endif
    !
    INTEGER :: d1_start, d1_size, d1_ld
    INTEGER :: d2_start, d2_size, d2_ld
    INTEGER :: d3_start, d3_size, d3_ld
    INTEGER :: d4_start, d4_size, d4_ld
    INTEGER :: ierr
    !
    d1_start = 1
    d1_size = range1(2) - range1(1) + 1
    d1_ld = range1(3)
    d2_start = 1
    d2_size = range2(2) - range2(1) + 1
    d2_ld = range2(3)
    d3_start = 1
    d3_size = range3(2) - range3(1) + 1
    d3_ld = range3(3)
    d4_start = 1
    d4_size = range4(2) - range4(1) + 1
    d4_ld = range4(3)
    !
#if defined(__CUDA)
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#else
    CALL dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
#endif
    !
END SUBROUTINE sp_d2h_memsync_c4d
!


SUBROUTINE dp_dev_conjg_c1d(array_inout, range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_inout(:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_inout
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_inout, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_inout(i1 ) = &
            conjg (  array_inout (i1 ) )
    ENDDO
    !
END SUBROUTINE dp_dev_conjg_c1d
!
SUBROUTINE dp_dev_conjg_c2d(array_inout, range1, range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_inout(:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_inout
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_inout, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_inout, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_inout(i1,i2 ) = &
            conjg (  array_inout (i1,i2 ) )
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_conjg_c2d
!
SUBROUTINE dp_dev_conjg_c3d(array_inout, range1, range2, range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_inout(:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_inout
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_inout, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_inout, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_inout, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_inout(i1,i2,i3 ) = &
            conjg (  array_inout (i1,i2,i3 ) )
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_conjg_c3d
!
SUBROUTINE dp_dev_conjg_c4d(array_inout, range1, range2, range3, range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(14,200)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_inout(:,:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_inout
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_inout, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_inout, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_inout, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_inout, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_inout(i1,i2,i3,i4 ) = &
            conjg (  array_inout (i1,i2,i3,i4 ) )
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE dp_dev_conjg_c4d
!
SUBROUTINE sp_dev_conjg_c1d(array_inout, range1 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_inout(:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_inout
#endif
    !
    INTEGER :: i1, d1s, d1e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_inout, 1)
    END IF
    !
    !
    !$cuf kernel do(1)
    DO i1 = d1s, d1e
        array_inout(i1 ) = &
            conjg (  array_inout (i1 ) )
    ENDDO
    !
END SUBROUTINE sp_dev_conjg_c1d
!
SUBROUTINE sp_dev_conjg_c2d(array_inout, range1, range2 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_inout(:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_inout
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_inout, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_inout, 2)
    END IF
    !
    !
    !$cuf kernel do(2)
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_inout(i1,i2 ) = &
            conjg (  array_inout (i1,i2 ) )
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_conjg_c2d
!
SUBROUTINE sp_dev_conjg_c3d(array_inout, range1, range2, range3 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_inout(:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_inout
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_inout, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_inout, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_inout, 3)
    END IF
    !
    !
    !$cuf kernel do(3)
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_inout(i1,i2,i3 ) = &
            conjg (  array_inout (i1,i2,i3 ) )
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_conjg_c3d
!
SUBROUTINE sp_dev_conjg_c4d(array_inout, range1, range2, range3, range4 )
    !
    IMPLICIT NONE
    !
    INTEGER, PARAMETER :: PRCSN = selected_real_kind(6, 37)
    COMPLEX(PRCSN), INTENT(INOUT) :: array_inout(:,:,:,:)
    INTEGER, OPTIONAL, INTENT(IN) ::  range1(2), range2(2), range3(2), range4(2)
    !
#if defined(__CUDA)
    attributes(DEVICE) :: array_inout
#endif
    !
    INTEGER :: i1, d1s, d1e
    INTEGER :: i2, d2s, d2e
    INTEGER :: i3, d3s, d3e
    INTEGER :: i4, d4s, d4e
    !
    IF (PRESENT(range1)) THEN
        d1s = 1
        d1e = range1(2) - range1(1) + 1
    ELSE
        d1s = 1
        d1e = SIZE(array_inout, 1)
    END IF
    !
    IF (PRESENT(range2)) THEN
        d2s = 1
        d2e = range2(2) - range2(1) + 1
    ELSE
        d2s = 1
        d2e = SIZE(array_inout, 2)
    END IF
    !
    IF (PRESENT(range3)) THEN
        d3s = 1
        d3e = range3(2) - range3(1) + 1
    ELSE
        d3s = 1
        d3e = SIZE(array_inout, 3)
    END IF
    !
    IF (PRESENT(range4)) THEN
        d4s = 1
        d4e = range4(2) - range4(1) + 1
    ELSE
        d4s = 1
        d4e = SIZE(array_inout, 4)
    END IF
    !
    !
    !$cuf kernel do(4)
    DO i4 = d4s, d4e
    DO i3 = d3s, d3e
    DO i2 = d2s, d2e
    DO i1 = d1s, d1e
        array_inout(i1,i2,i3,i4 ) = &
            conjg (  array_inout (i1,i2,i3,i4 ) )
    ENDDO
    ENDDO
    ENDDO
    ENDDO
    !
END SUBROUTINE sp_dev_conjg_c4d
!


SUBROUTINE dxlib_errore( calling_routine, message, ierr )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: calling_routine, message
    ! the name of the calling calling_routine
    ! the output message
    INTEGER,          INTENT(IN) :: ierr
    !
    PRINT *, calling_routine, message
    !
END SUBROUTINE dxlib_errore
