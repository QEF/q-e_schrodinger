!
! Copyright (C) 2002-2018 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! dev_util memcpy async test
!
#include<device_macros.h>
#if defined(__CUDA)
#  define __PINNED pinned,
#else
#  define __PINNED 
#endif
!
program test_memcpy_async
  !
  ! This program tests the routines and interfaces related to
  ! device_util.
  !
#ifdef __CUDA
  use cudafor
#endif
  use timer_m
  use iso_fortran_env
  implicit none

#include "device_memcpy_interf.f90"
#include "device_auxfunc_interf.f90"

  real(real64), parameter :: thr=1.0d-6
  integer,      parameter :: nranks=4
  integer :: ndim(nranks)
  integer :: vrange(2,nranks)
  integer :: vlbound(nranks)
  real(real64):: t0,t1
  integer :: npass,nfail
#if defined(__CUDA)
  integer(kind=cuda_Stream_Kind) :: stream
#else
  integer :: stream
#endif
  real(real32), __PINNED allocatable :: A_hst1__sp_r1d(:)
  real(real32), __PINNED allocatable :: A_hst2__sp_r1d(:)
  real(real32), allocatable :: A_dev1__sp_r1d(:)
  real(real32), allocatable :: A_dev2__sp_r1d(:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__sp_r1d, A_dev2__sp_r1d
#endif
  

  real(real32), __PINNED allocatable :: A_hst1__sp_r2d(:,:)
  real(real32), __PINNED allocatable :: A_hst2__sp_r2d(:,:)
  real(real32), allocatable :: A_dev1__sp_r2d(:,:)
  real(real32), allocatable :: A_dev2__sp_r2d(:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__sp_r2d, A_dev2__sp_r2d
#endif
  

  real(real32), __PINNED allocatable :: A_hst1__sp_r3d(:,:,:)
  real(real32), __PINNED allocatable :: A_hst2__sp_r3d(:,:,:)
  real(real32), allocatable :: A_dev1__sp_r3d(:,:,:)
  real(real32), allocatable :: A_dev2__sp_r3d(:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__sp_r3d, A_dev2__sp_r3d
#endif
  

  real(real32), __PINNED allocatable :: A_hst1__sp_r4d(:,:,:,:)
  real(real32), __PINNED allocatable :: A_hst2__sp_r4d(:,:,:,:)
  real(real32), allocatable :: A_dev1__sp_r4d(:,:,:,:)
  real(real32), allocatable :: A_dev2__sp_r4d(:,:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__sp_r4d, A_dev2__sp_r4d
#endif
  


  real(real64), __PINNED allocatable :: A_hst1__dp_r1d(:)
  real(real64), __PINNED allocatable :: A_hst2__dp_r1d(:)
  real(real64), allocatable :: A_dev1__dp_r1d(:)
  real(real64), allocatable :: A_dev2__dp_r1d(:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__dp_r1d, A_dev2__dp_r1d
#endif
  

  real(real64), __PINNED allocatable :: A_hst1__dp_r2d(:,:)
  real(real64), __PINNED allocatable :: A_hst2__dp_r2d(:,:)
  real(real64), allocatable :: A_dev1__dp_r2d(:,:)
  real(real64), allocatable :: A_dev2__dp_r2d(:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__dp_r2d, A_dev2__dp_r2d
#endif
  

  real(real64), __PINNED allocatable :: A_hst1__dp_r3d(:,:,:)
  real(real64), __PINNED allocatable :: A_hst2__dp_r3d(:,:,:)
  real(real64), allocatable :: A_dev1__dp_r3d(:,:,:)
  real(real64), allocatable :: A_dev2__dp_r3d(:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__dp_r3d, A_dev2__dp_r3d
#endif
  

  real(real64), __PINNED allocatable :: A_hst1__dp_r4d(:,:,:,:)
  real(real64), __PINNED allocatable :: A_hst2__dp_r4d(:,:,:,:)
  real(real64), allocatable :: A_dev1__dp_r4d(:,:,:,:)
  real(real64), allocatable :: A_dev2__dp_r4d(:,:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__dp_r4d, A_dev2__dp_r4d
#endif
  



  complex(real32), __PINNED allocatable :: A_hst1__sp_c1d(:)
  complex(real32), __PINNED allocatable :: A_hst2__sp_c1d(:)
  complex(real32), allocatable :: A_dev1__sp_c1d(:)
  complex(real32), allocatable :: A_dev2__sp_c1d(:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__sp_c1d, A_dev2__sp_c1d
#endif
  
  real(real32), __PINNED allocatable :: A_rtmp__sp_1d(:)
  

  complex(real32), __PINNED allocatable :: A_hst1__sp_c2d(:,:)
  complex(real32), __PINNED allocatable :: A_hst2__sp_c2d(:,:)
  complex(real32), allocatable :: A_dev1__sp_c2d(:,:)
  complex(real32), allocatable :: A_dev2__sp_c2d(:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__sp_c2d, A_dev2__sp_c2d
#endif
  
  real(real32), __PINNED allocatable :: A_rtmp__sp_2d(:,:)
  

  complex(real32), __PINNED allocatable :: A_hst1__sp_c3d(:,:,:)
  complex(real32), __PINNED allocatable :: A_hst2__sp_c3d(:,:,:)
  complex(real32), allocatable :: A_dev1__sp_c3d(:,:,:)
  complex(real32), allocatable :: A_dev2__sp_c3d(:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__sp_c3d, A_dev2__sp_c3d
#endif
  
  real(real32), __PINNED allocatable :: A_rtmp__sp_3d(:,:,:)
  

  complex(real32), __PINNED allocatable :: A_hst1__sp_c4d(:,:,:,:)
  complex(real32), __PINNED allocatable :: A_hst2__sp_c4d(:,:,:,:)
  complex(real32), allocatable :: A_dev1__sp_c4d(:,:,:,:)
  complex(real32), allocatable :: A_dev2__sp_c4d(:,:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__sp_c4d, A_dev2__sp_c4d
#endif
  
  real(real32), __PINNED allocatable :: A_rtmp__sp_4d(:,:,:,:)
  


  complex(real64), __PINNED allocatable :: A_hst1__dp_c1d(:)
  complex(real64), __PINNED allocatable :: A_hst2__dp_c1d(:)
  complex(real64), allocatable :: A_dev1__dp_c1d(:)
  complex(real64), allocatable :: A_dev2__dp_c1d(:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__dp_c1d, A_dev2__dp_c1d
#endif
  
  real(real64), __PINNED allocatable :: A_rtmp__dp_1d(:)
  

  complex(real64), __PINNED allocatable :: A_hst1__dp_c2d(:,:)
  complex(real64), __PINNED allocatable :: A_hst2__dp_c2d(:,:)
  complex(real64), allocatable :: A_dev1__dp_c2d(:,:)
  complex(real64), allocatable :: A_dev2__dp_c2d(:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__dp_c2d, A_dev2__dp_c2d
#endif
  
  real(real64), __PINNED allocatable :: A_rtmp__dp_2d(:,:)
  

  complex(real64), __PINNED allocatable :: A_hst1__dp_c3d(:,:,:)
  complex(real64), __PINNED allocatable :: A_hst2__dp_c3d(:,:,:)
  complex(real64), allocatable :: A_dev1__dp_c3d(:,:,:)
  complex(real64), allocatable :: A_dev2__dp_c3d(:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__dp_c3d, A_dev2__dp_c3d
#endif
  
  real(real64), __PINNED allocatable :: A_rtmp__dp_3d(:,:,:)
  

  complex(real64), __PINNED allocatable :: A_hst1__dp_c4d(:,:,:,:)
  complex(real64), __PINNED allocatable :: A_hst2__dp_c4d(:,:,:,:)
  complex(real64), allocatable :: A_dev1__dp_c4d(:,:,:,:)
  complex(real64), allocatable :: A_dev2__dp_c4d(:,:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__dp_c4d, A_dev2__dp_c4d
#endif
  
  real(real64), __PINNED allocatable :: A_rtmp__dp_4d(:,:,:,:)
  



  integer(int32), __PINNED allocatable :: A_hst1__i4_i1d(:)
  integer(int32), __PINNED allocatable :: A_hst2__i4_i1d(:)
  integer(int32), allocatable :: A_dev1__i4_i1d(:)
  integer(int32), allocatable :: A_dev2__i4_i1d(:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__i4_i1d, A_dev2__i4_i1d
#endif
  
  real(int32), __PINNED allocatable :: A_rtmp__i4_1d(:)
  

  integer(int32), __PINNED allocatable :: A_hst1__i4_i2d(:,:)
  integer(int32), __PINNED allocatable :: A_hst2__i4_i2d(:,:)
  integer(int32), allocatable :: A_dev1__i4_i2d(:,:)
  integer(int32), allocatable :: A_dev2__i4_i2d(:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__i4_i2d, A_dev2__i4_i2d
#endif
  
  real(int32), __PINNED allocatable :: A_rtmp__i4_2d(:,:)
  

  integer(int32), __PINNED allocatable :: A_hst1__i4_i3d(:,:,:)
  integer(int32), __PINNED allocatable :: A_hst2__i4_i3d(:,:,:)
  integer(int32), allocatable :: A_dev1__i4_i3d(:,:,:)
  integer(int32), allocatable :: A_dev2__i4_i3d(:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__i4_i3d, A_dev2__i4_i3d
#endif
  
  real(int32), __PINNED allocatable :: A_rtmp__i4_3d(:,:,:)
  

  integer(int32), __PINNED allocatable :: A_hst1__i4_i4d(:,:,:,:)
  integer(int32), __PINNED allocatable :: A_hst2__i4_i4d(:,:,:,:)
  integer(int32), allocatable :: A_dev1__i4_i4d(:,:,:,:)
  integer(int32), allocatable :: A_dev2__i4_i4d(:,:,:,:)
#if defined(__CUDA)
  attributes(device) :: A_dev1__i4_i4d, A_dev2__i4_i4d
#endif
  
  real(int32), __PINNED allocatable :: A_rtmp__i4_4d(:,:,:,:)
  



  
  integer :: i,ierr
  !integer :: i1, i2, i3, i4 
  integer :: ndim1, ndim2, ndim3, ndim4 
  integer :: lbound1, lbound2, lbound3, lbound4 
  integer :: range1(2), range2(2), range3(2), range4(2) 
  integer :: bound1(2), bound2(2), bound3(2), bound4(2) 
  logical :: is_pinned(2)
  character(256) :: arg, str

!
!============================
! get dims
!============================
!
  ! defaults
  ndim(:)=100
  vrange(1,:)=1
  vrange(2,:)=ndim
  vlbound(:)=1

  i=0
  do
    call get_command_argument(i, arg)
    if (len_trim(arg) == 0) exit
    !
    select case (trim(arg))
    case("-h","--help")
      write(0,"(a)") "Usage: "
      write(0,"(a)") "   ./test_memcpy_async.x [--dims <vals>] [--range <vals>] [--lbound <vals>]"
      stop
    end select
    !
    i = i+1
    call get_command_argument(i, str)
    if (len_trim(str) == 0) exit
    !
    select case (trim(arg))
    case("-dims","--dims")
      read(str,*,iostat=ierr) ndim(:)
      if (ierr/=0) STOP "reading cmd-line args: dims"
    case("-range","--range")
      read(str,*,iostat=ierr) vrange(:,:)
      if (ierr/=0) STOP "reading cmd-line args: range"
    case("-lbound","--lbound")
      read(str,*,iostat=ierr) vlbound(:)
      if (ierr/=0) STOP "reading cmd-line args: lbound"
    end select
  enddo
  !
  write(0,"(/,a,/)") "Running test_memcpy_async.x with params: "
  write(0,"(3x,a,10i5)") "  ndim: ", ndim(:)
  write(0,"(3x,a,10i5)") "lbound: ", vlbound(:)
  do i = 1, nranks
     write(0,"(3x,a,i2,3x,10i5)") " range: ", i, vrange(:,i)
  enddo
  write(0,"()")
  !
  npass=0
  nfail=0

  stream=0
#ifdef __CUDA
  ierr = cudaStreamCreate(stream)
  !ierr = cudaStreamCreateWithFLags(stream,cudaStreamNonBlocking)
#endif



  ndim1=ndim(1)
  lbound1=vlbound(1)
  range1=vrange(:,1)
  bound1(1)=lbound1
  bound1(2)=lbound1+ndim1-1
  ndim2=ndim(2)
  lbound2=vlbound(2)
  range2=vrange(:,2)
  bound2(1)=lbound2
  bound2(2)=lbound2+ndim2-1
  ndim3=ndim(3)
  lbound3=vlbound(3)
  range3=vrange(:,3)
  bound3(1)=lbound3
  bound3(2)=lbound3+ndim3-1
  ndim4=ndim(4)
  lbound4=vlbound(4)
  range4=vrange(:,4)
  bound4(1)=lbound4
  bound4(2)=lbound4+ndim4-1

!
!============================
! check memcpy
!============================
!
  !
  !=====================
  write(0,"(/,3x,a)") "checking sp_r1d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__sp_r1d(bound1(1):bound1(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__sp_r1d(bound1(1):bound1(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__sp_r1d(bound1(1):bound1(2)) )
  allocate( A_hst2__sp_r1d(bound1(1):bound1(2)) )
#endif
  allocate( A_dev1__sp_r1d(bound1(1):bound1(2)) )
  allocate( A_dev2__sp_r1d(bound1(1):bound1(2)) )
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_hst1__sp_r1d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__sp_r1d, A_hst1__sp_r1d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__sp_r1d, A_dev1__sp_r1d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__sp_r1d, A_dev2__sp_r1d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__sp_r1d -A_hst1__sp_r1d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__sp_r1d)
  deallocate(A_hst2__sp_r1d)
  deallocate(A_dev1__sp_r1d)
  deallocate(A_dev2__sp_r1d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking sp_r2d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__sp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__sp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__sp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_hst2__sp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
#endif
  allocate( A_dev1__sp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_dev2__sp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_hst1__sp_r2d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__sp_r2d, A_hst1__sp_r2d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__sp_r2d, A_dev1__sp_r2d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__sp_r2d, A_dev2__sp_r2d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__sp_r2d -A_hst1__sp_r2d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__sp_r2d)
  deallocate(A_hst2__sp_r2d)
  deallocate(A_dev1__sp_r2d)
  deallocate(A_dev2__sp_r2d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking sp_r3d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__sp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__sp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__sp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_hst2__sp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
#endif
  allocate( A_dev1__sp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_dev2__sp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_hst1__sp_r3d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__sp_r3d, A_hst1__sp_r3d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__sp_r3d, A_dev1__sp_r3d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__sp_r3d, A_dev2__sp_r3d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__sp_r3d -A_hst1__sp_r3d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__sp_r3d)
  deallocate(A_hst2__sp_r3d)
  deallocate(A_dev1__sp_r3d)
  deallocate(A_dev2__sp_r3d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking sp_r4d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__sp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__sp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__sp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_hst2__sp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
#endif
  allocate( A_dev1__sp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_dev2__sp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_hst1__sp_r4d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__sp_r4d, A_hst1__sp_r4d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__sp_r4d, A_dev1__sp_r4d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__sp_r4d, A_dev2__sp_r4d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__sp_r4d -A_hst1__sp_r4d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__sp_r4d)
  deallocate(A_hst2__sp_r4d)
  deallocate(A_dev1__sp_r4d)
  deallocate(A_dev2__sp_r4d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !


  !
  !=====================
  write(0,"(/,3x,a)") "checking dp_r1d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__dp_r1d(bound1(1):bound1(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__dp_r1d(bound1(1):bound1(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__dp_r1d(bound1(1):bound1(2)) )
  allocate( A_hst2__dp_r1d(bound1(1):bound1(2)) )
#endif
  allocate( A_dev1__dp_r1d(bound1(1):bound1(2)) )
  allocate( A_dev2__dp_r1d(bound1(1):bound1(2)) )
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_hst1__dp_r1d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__dp_r1d, A_hst1__dp_r1d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__dp_r1d, A_dev1__dp_r1d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__dp_r1d, A_dev2__dp_r1d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__dp_r1d -A_hst1__dp_r1d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__dp_r1d)
  deallocate(A_hst2__dp_r1d)
  deallocate(A_dev1__dp_r1d)
  deallocate(A_dev2__dp_r1d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking dp_r2d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__dp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__dp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__dp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_hst2__dp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
#endif
  allocate( A_dev1__dp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_dev2__dp_r2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_hst1__dp_r2d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__dp_r2d, A_hst1__dp_r2d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__dp_r2d, A_dev1__dp_r2d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__dp_r2d, A_dev2__dp_r2d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__dp_r2d -A_hst1__dp_r2d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__dp_r2d)
  deallocate(A_hst2__dp_r2d)
  deallocate(A_dev1__dp_r2d)
  deallocate(A_dev2__dp_r2d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking dp_r3d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__dp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__dp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__dp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_hst2__dp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
#endif
  allocate( A_dev1__dp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_dev2__dp_r3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_hst1__dp_r3d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__dp_r3d, A_hst1__dp_r3d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__dp_r3d, A_dev1__dp_r3d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__dp_r3d, A_dev2__dp_r3d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__dp_r3d -A_hst1__dp_r3d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__dp_r3d)
  deallocate(A_hst2__dp_r3d)
  deallocate(A_dev1__dp_r3d)
  deallocate(A_dev2__dp_r3d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking dp_r4d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__dp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__dp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__dp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_hst2__dp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
#endif
  allocate( A_dev1__dp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_dev2__dp_r4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_hst1__dp_r4d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__dp_r4d, A_hst1__dp_r4d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__dp_r4d, A_dev1__dp_r4d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__dp_r4d, A_dev2__dp_r4d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__dp_r4d -A_hst1__dp_r4d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__dp_r4d)
  deallocate(A_hst2__dp_r4d)
  deallocate(A_dev1__dp_r4d)
  deallocate(A_dev2__dp_r4d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !



  !
  !=====================
  write(0,"(/,3x,a)") "checking sp_c1d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__sp_c1d(bound1(1):bound1(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__sp_c1d(bound1(1):bound1(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__sp_c1d(bound1(1):bound1(2)) )
  allocate( A_hst2__sp_c1d(bound1(1):bound1(2)) )
#endif
  allocate( A_dev1__sp_c1d(bound1(1):bound1(2)) )
  allocate( A_dev2__sp_c1d(bound1(1):bound1(2)) )
  allocate( A_rtmp__sp_1d(bound1(1):bound1(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__sp_1d )
  A_hst1__sp_c1d=A_rtmp__sp_1d
  call random_number( A_rtmp__sp_1d )
  A_hst1__sp_c1d=A_hst1__sp_c1d+cmplx(0.0,1.0)*A_rtmp__sp_1d
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__sp_c1d, A_hst1__sp_c1d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__sp_c1d, A_dev1__sp_c1d )
  !
  ! make cmplx conjg
  call dev_conjg( A_dev2__sp_c1d )
  
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__sp_c1d, A_dev2__sp_c1d,stream)
  call dev_stream_sync( stream )
  !
  ! retrieve conjg data
  A_hst2__sp_c1d = conjg( A_hst2__sp_c1d )
  
  !
  ! check
  if ( any(abs(A_hst2__sp_c1d -A_hst1__sp_c1d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__sp_c1d)
  deallocate(A_hst2__sp_c1d)
  deallocate(A_dev1__sp_c1d)
  deallocate(A_dev2__sp_c1d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking sp_c2d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__sp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__sp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__sp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_hst2__sp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
#endif
  allocate( A_dev1__sp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_dev2__sp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_rtmp__sp_2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__sp_2d )
  A_hst1__sp_c2d=A_rtmp__sp_2d
  call random_number( A_rtmp__sp_2d )
  A_hst1__sp_c2d=A_hst1__sp_c2d+cmplx(0.0,1.0)*A_rtmp__sp_2d
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__sp_c2d, A_hst1__sp_c2d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__sp_c2d, A_dev1__sp_c2d )
  !
  ! make cmplx conjg
  call dev_conjg( A_dev2__sp_c2d )
  
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__sp_c2d, A_dev2__sp_c2d,stream)
  call dev_stream_sync( stream )
  !
  ! retrieve conjg data
  A_hst2__sp_c2d = conjg( A_hst2__sp_c2d )
  
  !
  ! check
  if ( any(abs(A_hst2__sp_c2d -A_hst1__sp_c2d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__sp_c2d)
  deallocate(A_hst2__sp_c2d)
  deallocate(A_dev1__sp_c2d)
  deallocate(A_dev2__sp_c2d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking sp_c3d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__sp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__sp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__sp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_hst2__sp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
#endif
  allocate( A_dev1__sp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_dev2__sp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_rtmp__sp_3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__sp_3d )
  A_hst1__sp_c3d=A_rtmp__sp_3d
  call random_number( A_rtmp__sp_3d )
  A_hst1__sp_c3d=A_hst1__sp_c3d+cmplx(0.0,1.0)*A_rtmp__sp_3d
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__sp_c3d, A_hst1__sp_c3d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__sp_c3d, A_dev1__sp_c3d )
  !
  ! make cmplx conjg
  call dev_conjg( A_dev2__sp_c3d )
  
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__sp_c3d, A_dev2__sp_c3d,stream)
  call dev_stream_sync( stream )
  !
  ! retrieve conjg data
  A_hst2__sp_c3d = conjg( A_hst2__sp_c3d )
  
  !
  ! check
  if ( any(abs(A_hst2__sp_c3d -A_hst1__sp_c3d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__sp_c3d)
  deallocate(A_hst2__sp_c3d)
  deallocate(A_dev1__sp_c3d)
  deallocate(A_dev2__sp_c3d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking sp_c4d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__sp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__sp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__sp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_hst2__sp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
#endif
  allocate( A_dev1__sp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_dev2__sp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_rtmp__sp_4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__sp_4d )
  A_hst1__sp_c4d=A_rtmp__sp_4d
  call random_number( A_rtmp__sp_4d )
  A_hst1__sp_c4d=A_hst1__sp_c4d+cmplx(0.0,1.0)*A_rtmp__sp_4d
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__sp_c4d, A_hst1__sp_c4d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__sp_c4d, A_dev1__sp_c4d )
  !
  ! make cmplx conjg
  call dev_conjg( A_dev2__sp_c4d )
  
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__sp_c4d, A_dev2__sp_c4d,stream)
  call dev_stream_sync( stream )
  !
  ! retrieve conjg data
  A_hst2__sp_c4d = conjg( A_hst2__sp_c4d )
  
  !
  ! check
  if ( any(abs(A_hst2__sp_c4d -A_hst1__sp_c4d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__sp_c4d)
  deallocate(A_hst2__sp_c4d)
  deallocate(A_dev1__sp_c4d)
  deallocate(A_dev2__sp_c4d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !


  !
  !=====================
  write(0,"(/,3x,a)") "checking dp_c1d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__dp_c1d(bound1(1):bound1(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__dp_c1d(bound1(1):bound1(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__dp_c1d(bound1(1):bound1(2)) )
  allocate( A_hst2__dp_c1d(bound1(1):bound1(2)) )
#endif
  allocate( A_dev1__dp_c1d(bound1(1):bound1(2)) )
  allocate( A_dev2__dp_c1d(bound1(1):bound1(2)) )
  allocate( A_rtmp__dp_1d(bound1(1):bound1(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__dp_1d )
  A_hst1__dp_c1d=A_rtmp__dp_1d
  call random_number( A_rtmp__dp_1d )
  A_hst1__dp_c1d=A_hst1__dp_c1d+cmplx(0.0,1.0)*A_rtmp__dp_1d
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__dp_c1d, A_hst1__dp_c1d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__dp_c1d, A_dev1__dp_c1d )
  !
  ! make cmplx conjg
  call dev_conjg( A_dev2__dp_c1d )
  
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__dp_c1d, A_dev2__dp_c1d,stream)
  call dev_stream_sync( stream )
  !
  ! retrieve conjg data
  A_hst2__dp_c1d = conjg( A_hst2__dp_c1d )
  
  !
  ! check
  if ( any(abs(A_hst2__dp_c1d -A_hst1__dp_c1d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__dp_c1d)
  deallocate(A_hst2__dp_c1d)
  deallocate(A_dev1__dp_c1d)
  deallocate(A_dev2__dp_c1d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking dp_c2d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__dp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__dp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__dp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_hst2__dp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
#endif
  allocate( A_dev1__dp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_dev2__dp_c2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_rtmp__dp_2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__dp_2d )
  A_hst1__dp_c2d=A_rtmp__dp_2d
  call random_number( A_rtmp__dp_2d )
  A_hst1__dp_c2d=A_hst1__dp_c2d+cmplx(0.0,1.0)*A_rtmp__dp_2d
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__dp_c2d, A_hst1__dp_c2d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__dp_c2d, A_dev1__dp_c2d )
  !
  ! make cmplx conjg
  call dev_conjg( A_dev2__dp_c2d )
  
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__dp_c2d, A_dev2__dp_c2d,stream)
  call dev_stream_sync( stream )
  !
  ! retrieve conjg data
  A_hst2__dp_c2d = conjg( A_hst2__dp_c2d )
  
  !
  ! check
  if ( any(abs(A_hst2__dp_c2d -A_hst1__dp_c2d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__dp_c2d)
  deallocate(A_hst2__dp_c2d)
  deallocate(A_dev1__dp_c2d)
  deallocate(A_dev2__dp_c2d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking dp_c3d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__dp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__dp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__dp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_hst2__dp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
#endif
  allocate( A_dev1__dp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_dev2__dp_c3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_rtmp__dp_3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__dp_3d )
  A_hst1__dp_c3d=A_rtmp__dp_3d
  call random_number( A_rtmp__dp_3d )
  A_hst1__dp_c3d=A_hst1__dp_c3d+cmplx(0.0,1.0)*A_rtmp__dp_3d
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__dp_c3d, A_hst1__dp_c3d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__dp_c3d, A_dev1__dp_c3d )
  !
  ! make cmplx conjg
  call dev_conjg( A_dev2__dp_c3d )
  
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__dp_c3d, A_dev2__dp_c3d,stream)
  call dev_stream_sync( stream )
  !
  ! retrieve conjg data
  A_hst2__dp_c3d = conjg( A_hst2__dp_c3d )
  
  !
  ! check
  if ( any(abs(A_hst2__dp_c3d -A_hst1__dp_c3d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__dp_c3d)
  deallocate(A_hst2__dp_c3d)
  deallocate(A_dev1__dp_c3d)
  deallocate(A_dev2__dp_c3d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking dp_c4d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__dp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__dp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__dp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_hst2__dp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
#endif
  allocate( A_dev1__dp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_dev2__dp_c4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_rtmp__dp_4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__dp_4d )
  A_hst1__dp_c4d=A_rtmp__dp_4d
  call random_number( A_rtmp__dp_4d )
  A_hst1__dp_c4d=A_hst1__dp_c4d+cmplx(0.0,1.0)*A_rtmp__dp_4d
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__dp_c4d, A_hst1__dp_c4d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__dp_c4d, A_dev1__dp_c4d )
  !
  ! make cmplx conjg
  call dev_conjg( A_dev2__dp_c4d )
  
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__dp_c4d, A_dev2__dp_c4d,stream)
  call dev_stream_sync( stream )
  !
  ! retrieve conjg data
  A_hst2__dp_c4d = conjg( A_hst2__dp_c4d )
  
  !
  ! check
  if ( any(abs(A_hst2__dp_c4d -A_hst1__dp_c4d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__dp_c4d)
  deallocate(A_hst2__dp_c4d)
  deallocate(A_dev1__dp_c4d)
  deallocate(A_dev2__dp_c4d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !



  !
  !=====================
  write(0,"(/,3x,a)") "checking i4_i1d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__i4_i1d(bound1(1):bound1(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__i4_i1d(bound1(1):bound1(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__i4_i1d(bound1(1):bound1(2)) )
  allocate( A_hst2__i4_i1d(bound1(1):bound1(2)) )
#endif
  allocate( A_dev1__i4_i1d(bound1(1):bound1(2)) )
  allocate( A_dev2__i4_i1d(bound1(1):bound1(2)) )
  allocate( A_rtmp__i4_1d(bound1(1):bound1(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__i4_1d )
  A_hst1__i4_i1d=int( 1000* A_rtmp__i4_1d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__i4_i1d, A_hst1__i4_i1d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__i4_i1d, A_dev1__i4_i1d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__i4_i1d, A_dev2__i4_i1d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__i4_i1d -A_hst1__i4_i1d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__i4_i1d)
  deallocate(A_hst2__i4_i1d)
  deallocate(A_dev1__i4_i1d)
  deallocate(A_dev2__i4_i1d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking i4_i2d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__i4_i2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__i4_i2d(bound1(1):bound1(2),bound2(1):bound2(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__i4_i2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_hst2__i4_i2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
#endif
  allocate( A_dev1__i4_i2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_dev2__i4_i2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  allocate( A_rtmp__i4_2d(bound1(1):bound1(2),bound2(1):bound2(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__i4_2d )
  A_hst1__i4_i2d=int( 1000* A_rtmp__i4_2d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__i4_i2d, A_hst1__i4_i2d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__i4_i2d, A_dev1__i4_i2d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__i4_i2d, A_dev2__i4_i2d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__i4_i2d -A_hst1__i4_i2d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__i4_i2d)
  deallocate(A_hst2__i4_i2d)
  deallocate(A_dev1__i4_i2d)
  deallocate(A_dev2__i4_i2d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking i4_i3d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__i4_i3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__i4_i3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__i4_i3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_hst2__i4_i3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
#endif
  allocate( A_dev1__i4_i3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_dev2__i4_i3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  allocate( A_rtmp__i4_3d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__i4_3d )
  A_hst1__i4_i3d=int( 1000* A_rtmp__i4_3d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__i4_i3d, A_hst1__i4_i3d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__i4_i3d, A_dev1__i4_i3d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__i4_i3d, A_dev2__i4_i3d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__i4_i3d -A_hst1__i4_i3d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__i4_i3d)
  deallocate(A_hst2__i4_i3d)
  deallocate(A_dev1__i4_i3d)
  deallocate(A_dev2__i4_i3d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !

  !
  !=====================
  write(0,"(/,3x,a)") "checking i4_i4d ..." 
  t0=wallclock()
  !
  ! allocations
#ifdef __CUDA
  allocate( A_hst1__i4_i4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(1) )
  allocate( A_hst2__i4_i4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)), PINNED=is_pinned(2) )
#else
  is_pinned=.false.
  allocate( A_hst1__i4_i4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_hst2__i4_i4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
#endif
  allocate( A_dev1__i4_i4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_dev2__i4_i4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  allocate( A_rtmp__i4_4d(bound1(1):bound1(2),bound2(1):bound2(2),bound3(1):bound3(2),bound4(1):bound4(2)) )
  
  write(0,"(3x,a,2l)") "host memory is pinned: ", is_pinned(1:2) 
  !
  ! init
  call random_number( A_rtmp__i4_4d )
  A_hst1__i4_i4d=int( 1000* A_rtmp__i4_4d )
  
  !
  ! mem copy h2d
  call dev_memcpy_async(A_dev1__i4_i4d, A_hst1__i4_i4d,stream)
  call dev_stream_sync( stream )
  !
  ! mem copy
  call dev_memcpy(A_dev2__i4_i4d, A_dev1__i4_i4d )
  !
  !
  ! mem copy d2h
  call dev_memcpy_async(A_hst2__i4_i4d, A_dev2__i4_i4d,stream)
  call dev_stream_sync( stream )
  !
  !
  ! check
  if ( any(abs(A_hst2__i4_i4d -A_hst1__i4_i4d )> thr) ) then
     write(0,"(3x,a)") "FAILED" 
     nfail=nfail+1
  else
     write(0,"(3x,a)") "passed" 
     npass=npass+1
  endif
  !
  deallocate(A_hst1__i4_i4d)
  deallocate(A_hst2__i4_i4d)
  deallocate(A_dev1__i4_i4d)
  deallocate(A_dev2__i4_i4d)
  !
  t1=wallclock()
  write(0,"(3x,a,f12.6,' sec')") "Timing: ", t1-t0
  !




  !
  ! summary
  !
  write(0,"(/,a)") "Test SUMMARY:"
  write(0,"(3x,a,i5)") "# passed: ", npass
  write(0,"(3x,a,i5)") "# failed: ", nfail
  write(0,"()")

end program test_memcpy_async
