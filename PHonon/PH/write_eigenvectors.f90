!
! Copyright (C) 2012 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine write_eigenvectors (nat,ntyp,amass,ityp,q,w2,z,iout)
  !-----------------------------------------------------------------------
  !
  !   write modes on output file in a readable way
  !
  use kinds, only: dp
  use constants, only: amu_ry, ry_to_thz, ry_to_cmm1
  implicit none
  ! input
  integer, intent(in) :: nat, iout,ntyp
  integer ityp(nat)
  real(DP), intent(in) :: q(3), w2(3*nat),amass(ntyp)
  complex(DP), intent(in) :: z(3*nat,3*nat)
  ! local
  integer nat3, na, nta, ipol, i, j
  real(DP):: freq(3*nat)
  complex(DP) :: z_(3*nat,3*nat)
  !
  nat3=3*nat
  !
  !  write frequencies and phonon eigenvectors
  !
  write(iout,'(5x,''diagonalizing the dynamical matrix ...''/)')
  write(iout,'(1x,''q = '',3f12.4)') q
  write(iout,'(1x,74(''*''))')

 do i = 1,nat3
    do na = 1,nat
       nta = ityp(na)
       do ipol = 1,3
          z_((na-1)*3+ipol,i) = z((na-1)*3+ipol,i)* sqrt(amu_ry*amass(nta))
       end do
    end do
 end do

  do i = 1,nat3
     !
     freq(i)= sqrt(abs(w2(i)))
     if (w2(i) < 0.0) freq(i) = -freq(i)
     write (iout,9010) i, freq(i)*ry_to_thz, freq(i)*ry_to_cmm1
     do na = 1,nat
        write (iout,9020) (z_((na-1)*3+ipol,i),ipol=1,3)
     end do
     !
  end do
  write(iout,'(1x,74(''*''))')
  !
  return
  !
9010 format(5x,'freq (',i5,') =',f15.6,' [THz] =',f15.6,' [cm-1]')
9020 format (1x,'(',3 (f10.6,1x,f10.6,3x),')')
  !
end subroutine write_eigenvectors
!
!
!-----------------------------------------------------------------------
subroutine writemodes (nat,q,w2,z,iout)
  !-----------------------------------------------------------------------
  !
  !   write modes on output file in a readable way
  !
  use kinds, only: dp
  USE constants, ONLY : ry_to_thz, ry_to_cmm1
  implicit none
  ! input
  integer, intent(in) :: nat, iout
  real(DP), intent(in) :: q(3), w2(3*nat)
  complex(DP), intent(in) :: z(3*nat,3*nat)
  ! local
  integer nat3, na, ipol, i, j
  real(DP):: freq(3*nat)
  real(DP):: znorm
  !
  nat3=3*nat
  !
  !  write frequencies and normalised displacements
  !
  write(iout,'(5x,''diagonalizing the dynamical matrix ...''/)')
  write(iout,'(1x,''q = '',3f12.4)') q
  write(iout,'(1x,74(''*''))')
  do i = 1,nat3
     !
     freq(i)= sqrt(abs(w2(i)))
     if (w2(i).lt.0.0_DP) freq(i) = -freq(i)
     write (iout,9010) i, freq(i)*ry_to_thz, freq(i)*ry_to_cmm1
     znorm = 0.0d0
     do j=1,nat3
        znorm=znorm+abs(z(j,i))**2
     end do
     znorm = sqrt(znorm)
     do na = 1,nat
        write (iout,9020) (z((na-1)*3+ipol,i)/znorm,ipol=1,3)
     end do
     !
  end do
  write(iout,'(1x,74(''*''))')
  !
  return
  !
9010 format(5x,'freq (',i5,') =',f15.6,' [THz] =',f15.6,' [cm-1]')
9020 format (1x,'(',3 (f10.6,1x,f10.6,3x),')')
  !
end subroutine writemodes
!
!-----------------------------------------------------------------------
subroutine writemolden (flmol, gamma, nat, atm, a0, tau, ityp, w2, z)
  !-----------------------------------------------------------------------
  !
  !   write modes on output file in a molden-friendly way
  !
  use kinds, only: dp
  USE constants, ONLY : ry_to_cmm1
  implicit none
  ! input
  integer, intent(in) :: nat, ityp(nat)
  real(DP), intent(in) :: a0, tau(3,nat), w2(3*nat)
  complex(DP), intent(in) :: z(3*nat,3*nat)
  character(len=50), intent(in) :: flmol
  character(len=3), intent(in) :: atm(*)
  logical, intent(in) :: gamma
  ! local
  integer :: nat3, na, ipol, i, j, iout
  real(DP) :: freq(3*nat)
  real(DP) :: znorm
  !
  if (flmol.eq.' ') then
     return
  else
     iout=4
     open (unit=iout,file=flmol,status='unknown',form='formatted')
  end if
  nat3=3*nat
  !
  !  write frequencies and normalised displacements
  !
  write(iout,'(''[Molden Format]'')')
  !
  write(iout,'(''[FREQ]'')')
  do i = 1,nat3
     freq(i)= sqrt(abs(w2(i)))*ry_to_cmm1
     if (w2(i).lt.0.0d0) freq(i) = 0.0d0
     write (iout,'(f8.2)') freq(i)
  end do
  !
  write(iout,'(''[FR-COORD]'')')
  do na = 1,nat
     write (iout,'(a6,1x,3f15.5)') atm(ityp(na)),  &
          a0*tau(1,na), a0*tau(2,na), a0*tau(3,na)
  end do
  !
  write(iout,'(''[FR-NORM-COORD]'')')
  do i = 1,nat3
     write(iout,'('' vibration'',i6)') i
     znorm = 0.0d0
     do j=1,nat3
        znorm=znorm+abs(z(j,i))**2
     end do
     znorm = sqrt(znorm)
     do na = 1,nat
        if (gamma) then
           write (iout,'(3f10.5)') (DBLE(z((na-1)*3+ipol,i))/znorm,ipol=1,3)
        else
           write (iout,'(3f10.5)') ( abs(z((na-1)*3+ipol,i))/znorm,ipol=1,3)
        end if
     end do
  end do
  !
  close(unit=iout)
  !
  return
  !
end subroutine writemolden
!
!-----------------------------------------------------------------------
subroutine writexsf (xsffile, gamma, nat, atm, a0, at, tau, ityp, z)
  !-----------------------------------------------------------------------
  !
  !   write modes on output file in a xcrysden-friendly way
  !
  use kinds, only: dp
  USE constants, ONLY : BOHR_RADIUS_ANGS
  implicit none
  ! input
  integer :: nat, ityp(nat)
  real(DP) :: a0, tau(3,nat), at(3,3)
  complex(DP) :: z(3*nat,3*nat)
  character(len=50) :: xsffile
  character(len=3) :: atm(*)
  logical :: gamma
  ! local
  integer :: nat3, na, ipol, i, j, iout
  real(DP) :: znorm
  !
  if (xsffile == ' ') then
     return
  else
     iout=4
     open (unit=iout, file=xsffile, status='unknown', form='formatted')
  end if
  nat3=3*nat
  !
  !  write atomic positions and normalised displacements
  !
  write(iout,'("ANIMSTEPS",i4)') nat3
  !
  write(iout,'("CRYSTAL")')
  !
  write(iout,'("PRIMVEC")')
  write(iout,'(2(3F15.9/),3f15.9)') at(:,:)*a0*BOHR_RADIUS_ANGS
  !
  do i = 1,nat3
     write(iout,'("PRIMCOORD",i3)') i
     write(iout,'(3x,2i4)') nat, 1
     znorm = 0.0d0
     do j=1,nat3
       znorm=znorm+abs(z(j,i))**2
    end do
    ! empirical factor: displacement vector normalised to 0.1
    znorm = sqrt(znorm)*10.d0
    do na = 1,nat
       if (gamma) then
          write (iout,'(a6,1x,6f10.5)') atm(ityp(na)),  &
                      a0*BOHR_RADIUS_ANGS*tau(1,na), &
                      a0*BOHR_RADIUS_ANGS*tau(2,na), &
                      a0*BOHR_RADIUS_ANGS*tau(3,na), &
                      (DBLE(z((na-1)*3+ipol,i))/znorm,ipol=1,3)
       else
          write (iout,'(a6,1x,6f10.5)') atm(ityp(na)),  &
                      a0*BOHR_RADIUS_ANGS*tau(1,na), &
                      a0*BOHR_RADIUS_ANGS*tau(2,na), &
                      a0*BOHR_RADIUS_ANGS*tau(3,na), &
                      ( abs(z((na-1)*3+ipol,i))/znorm,ipol=1,3)
       end if
    end do
 end do
 !
 close(unit=iout)
 !
 return
 !
end subroutine writexsf
!
!-----------------------------------------------------------------------
subroutine writespm (nat, w2, z, zstar, iout)
  !-----------------------------------------------------------------------
  !
  !   write frequencies and intensities on output file in a spm-friendly way
  !
  use kinds, only: dp
  use constants, only: ry_to_cmm1, amu_ry
  implicit none
  ! input
  integer, intent(in) :: nat, iout
  real(DP), intent(in) :: w2(3*nat), zstar(3, 3, nat)
  complex(DP), intent(in) :: z(3*nat,3*nat)
  ! local
  integer nat3, na, ipol, jpol, nu
  real(DP):: freq(3*nat), infrared(3*nat)
  real(DP):: irfac, polar(3), znorm
  !
  nat3=3*nat
  !
  ! Compute IR intensities (stolen from dynmat_sub::RamanIR)
  !
  irfac = 4.80324d0**2/2.d0*amu_ry
  !
  do nu = 1, 3*nat
    do ipol = 1, 3
       polar(ipol)=0.0d0
    end do
    do na=1, nat
       do ipol=1, 3
          do jpol=1, 3
             polar(ipol) = polar(ipol) +  &
                  zstar(ipol,jpol,na)*z((na-1)*3+jpol, nu)
          end do
       end do
    end do
    !
    infrared(nu) = 2.d0*(polar(1)**2+polar(2)**2+polar(3)**2)*irfac
    !
    ! Check for nan
    if(infrared(nu) /= infrared(nu)) infrared(nu) = 0.0
  end do
  !
  !  write frequencies and intensities
  !
  write(iout, '("{ ")')
  write(iout, '(" s_m_m2io_version")')
  write(iout, '(" :::")')
  write(iout, '(" 2.0.0 ")')
  write(iout, '("} ")')
  write(iout, '("")')
  write(iout, '("f_m_table { ")')
  write(iout, '(" s_j_spectrum_type")')
  write(iout, '(" s_j_x_label")')
  write(iout, '(" s_j_y_label")')
  write(iout, '(" :::")')
  write(iout, '('' "Infrared Vibrational Frequencies" '')')
  write(iout, '("  r_j_Frequency_(cm-1) ")')
  write(iout, '("  r_matsci_Intensity_(D^2/A^2/amu) ")')
  write(iout, '(" m_column[3] { ")')
  write(iout, '("  s_m_data_name")')
  write(iout, '("  s_m_column_name")')
  write(iout, '("  i_m_width")')
  write(iout, '("  b_m_visible")')
  write(iout, '("  :::")')
  write(iout, '(''  1 r_j_Frequency_(cm-1)  "Frequency (cm-1)"  10 1'')')
  write(iout, '(''  2 r_matsci_Intensity_(D^2/A^2/amu)  "Intensity (D^2/A^2/amu)"  10 1'')')
  write(iout, '(''  3 s_j_Symmetry  "Symmetry"  10 1'')')
  write(iout, '("  :::")')
  write(iout, '(" } ")')
  !
  write(iout, '(" m_row[", i0, "] { ")') nat3
  write(iout, '("  r_j_Frequency_(cm-1)")')
  write(iout, '("  r_matsci_Intensity_(D^2/A^2/amu)")')
  write(iout, '("  s_j_Symmetry")')
  write(iout, '("  :::")')
  !
  do nu = 1, nat3
    freq(nu) = sqrt(abs(w2(nu)))
    if (w2(nu).lt.0.0_DP) freq(nu) = -freq(nu)
    !
    write(iout, '(2x, i0, 1x, f15.6, 1x, f15.6, 1x, ''"Ap    "'')') nu, &
      freq(nu)*ry_to_cmm1, infrared(nu)
  end do
  !
  write(iout, '("  :::")')
  write(iout, '(" } ")')
  !
  write(iout, '(" m_exists[", i0, "] { ")') nat3
  write(iout, '(" s_m_exists")')
  write(iout, '(" :::")')
  !
  do nu = 1, nat3
    write(iout, '("  ", i0, " 111")') nu
  end do
  !
  write(iout, '("  :::")')
  write(iout, '(" } ")')
  write(iout, '("} ")')
  !
  return
  !
end subroutine writespm
!
!-----------------------------------------------------------------------
subroutine writevib (nat,ntyp,amass,ityp,q,w2,z,zstar,iout)
  !-----------------------------------------------------------------------
  !
  !   write frequencies and vibrations on output file in a vib-friendly way
  !
  use kinds, only: dp
  use constants, only: amu_ry, ry_to_thz, ry_to_cmm1
  implicit none
  ! input
  integer, intent(in) :: nat, iout,ntyp
  integer ityp(nat)
  real(DP), intent(in) :: q(3), w2(3*nat), amass(ntyp), zstar(3, 3, nat)
  real(DP):: irfac, polar(3), znorm
  complex(DP), intent(in) :: z(3*nat,3*nat)
  ! local
  integer nat3, na, nta, ipol, jpol, i, j, nu
  integer :: exists(3*nat)
  real(DP):: freq(3*nat), infrared(3*nat)
  complex(DP) :: z_(3*nat,3*nat)
  !
  nat3=3*nat
  exists(:) = 1
  !
  !
  ! Compute IR intensities (stolen from dynmat_sub::RamanIR)
  !
  irfac = 4.80324d0**2/2.d0*amu_ry
  !
  do nu = 1, 3*nat
    do ipol = 1, 3
       polar(ipol)=0.0d0
    end do
    do na=1, nat
       do ipol=1, 3
          do jpol=1, 3
             polar(ipol) = polar(ipol) +  &
                  zstar(ipol,jpol,na)*z((na-1)*3+jpol, nu)
          end do
       end do
    end do
    !
    infrared(nu) = 2.d0*(polar(1)**2+polar(2)**2+polar(3)**2)*irfac
    !
    ! Check for nan
    if(infrared(nu) /= infrared(nu)) infrared(nu) = 0.0
  end do
  !
  !  write frequencies and phonon eigenvectors
  !
  do i = 1,nat3
    do na = 1,nat
       nta = ityp(na)
       do ipol = 1,3
          z_((na-1)*3+ipol,i) = z((na-1)*3+ipol,i)* sqrt(amu_ry*amass(nta))
       end do
    end do
  end do

  do i = 1,nat3
     !
     freq(i)= sqrt(abs(w2(i))) * ry_to_cmm1
     if (w2(i) < 0.0) freq(i) = -freq(i)
     !
  end do
  !
  write(iout, '("{ ")')
  write(iout, '(" s_m_m2io_version")')
  write(iout, '(" :::")')
  write(iout, '(" 2.0.0 ")')
  write(iout, '("} ")')
  write(iout, '("")')
  write(iout, '("f_m_table { ")')
  write(iout, '(" i_j_atom_total")')
  write(iout, '(" :::")')
  write(iout, '(" ", i0)') nat
  write(iout, '(" m_column[", i0, "] { ")') nat3 + 5
  write(iout, '("  s_m_data_name")')
  write(iout, '("  s_m_column_name")')
  write(iout, '("  i_m_width")')
  write(iout, '("  b_m_visible")')
  write(iout, '("  s_m_units")')
  write(iout, '("  :::")')
  write(iout, '(''  1 r_j_Frequency  "Frequency"  10 1 ""'')')
  write(iout, '(''  2 s_j_Symmetry  "Symmetry"  10 1 ""'')')
  write(iout, '(''  3 r_j_Intensity  "Intensity"  10 1 ""'')')
  write(iout, '(''  4 r_j_Raman_Act  "Raman Act"  10 0 ""'')')
  write(iout, '(''  5 r_j_Raman_Int  "Raman Int"  10 0 ""'')')
  !
  j = 6
  do i = 1, nat
    write(iout, '("  ", i0, " r_j_x", i0, ''  "x'', i0, ''"  10 0 ""'')') &
      j, i, i
    j = j + 1
    write(iout, '("  ", i0, " r_j_y", i0, ''  "y'', i0, ''"  10 0 ""'')') &
      j, i, i
    j = j + 1
    write(iout, '("  ", i0, " r_j_z", i0, ''  "z'', i0, ''"  10 0 ""'')') &
      j, i, i
    j = j + 1
  end do
  !
  write(iout, '("  :::")')
  write(iout, '(" } ")')
  !
  write(iout, '(" m_row[", i0, "] { ")') nat3
  write(iout, '("  r_j_Frequency")')
  write(iout, '("  s_j_Symmetry")')
  write(iout, '("  r_j_Intensity")')
  write(iout, '("  r_j_Raman_Act")')
  write(iout, '("  r_j_Raman_Int")')
  !
  do i = 1, nat
    write(iout, '("  r_j_x", i0)') i
    write(iout, '("  r_j_y", i0)') i
    write(iout, '("  r_j_z", i0)') i
  end do
  !
  write(iout, '("  :::")')
  !
  do i = 1, nat3
    !
    znorm = 0.0d0
    do j = 1, nat3
      znorm = znorm + abs(z(j,i))**2
    end do
    !
    znorm = sqrt(znorm)
    !
    write(iout, '("  ", i0, 1x, f20.10, '' "" '', f20.10, "0 0 0", 99999f15.9)') &
      i, freq(i), infrared(i), ((DBLE(z((na-1)*3+ipol,i))/znorm, ipol=1,3), na=1,nat)
  end do
  !
  write(iout, '("  :::")')
  write(iout, '(" } ")')
  write(iout, '(" m_exists[", i0, "] { ")') nat3
  write(iout, '("  s_m_exists")')
  write(iout, '("  :::")')
  !
  do i = 1, nat3
    write(iout, '("  ", i0, 1x, 99999i0)') i, exists
  end do
  !
  write(iout, '("  :::")')
  write(iout, '(" } ")')
  write(iout, '("}")')
  !
  return
  !
end subroutine writevib
!
