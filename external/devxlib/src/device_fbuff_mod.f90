!
! Copyright (C) 2002-2018 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Utility functions to perform memcpy and memset on the device with CUDA Fortran
! cuf_memXXX contains a CUF KERNEL to perform the selected operation
! cu_memsync are wrappers for cuda_memcpy functions
!
#if defined(_CUDA)
#  define __CUDA
#endif
!
module device_fbuff_m
  use tb_dev, only : tb_dev_t
  use tb_pin, only : tb_pin_t

  implicit none

  TYPE(tb_dev_t) :: dev_buf ! A global variable hosting a global device buffer
  TYPE(tb_pin_t) :: pin_buf ! A global variable hosting a global pinned memory buffer

#include<device_fbuff_interf.f90>

end module device_fbuff_m

