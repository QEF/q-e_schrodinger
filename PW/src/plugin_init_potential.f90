! Copyright (C) 2013 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
subroutine plugin_init_potential(vltot)
!
! This routine is used for initializing potentials in plugins
! DO NOT REMOVE THE TAGS ! ***ADDSON_NAME KIND_OF_PATCH***
!
USE plugin_flags
USE fft_base,  ONLY : dfftp
!
! ***Environ MODULES BEGIN***
! ***Environ MODULES END***
!
implicit none
!
REAL(DP), INTENT(IN) :: vltot(dfftp%nnr)
! ***Environ VARIABLES BEGIN***
! ***Environ VARIABLES END***
!
! ***Environ CALLS BEGIN***
! ***Environ CALLS END***
!
end subroutine plugin_init_potential

