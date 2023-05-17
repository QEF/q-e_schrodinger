module offloadable
use iso_c_binding
use iso_fortran_env, only: DP=> REAL64 
implicit none 

type data_flags 
   logical  :: use_gpu 
end type data_flags

#if !defined(__CUDA) 
type c_devptr 
   type(c_ptr)  :: cptr 
end type c_devptr 

type cudaArrayPtr 
   type (c_ptr)  :: aptr 
end type cudaArrayPtr 
#endif 

type offloadable_base 
  type(c_ptr)          :: gpu_void =  c_null_ptr 
  type (c_ptr)         :: cpu_void =  c_null_ptr 
  integer                    ::  rank 
  integer,allocatable        :: shape(:)
  integer                    :: size 
  integer,allocatable        :: stride(:)  
  logical                    :: dev_tready = .false. 
  logical                    :: host_tready =.false. 
  logical                    :: owns_data   = .false. 
end type offloadable_base 

type offloadable_complex
  type(offloadable_base)     :: base 
  complex(dp), pointer           ::  gpu_pt(:) => NULL()
  complex(dp), pointer    ::  cpu_pt(:) => NULL() 
#if defined(__CUDA) 
  attributes, device    :: gpu_pt 
#endif
end type offloadable_complex

type offloadable_real
  type(offloadable_base)        ::  base 
  real(dp), pointer           ::  gpu_pt(:) =>NULL()
  real(dp), pointer           ::  cpu_pt(:) =>NULL()
#if defined(__CUDA)  
  attributes, device          :: gpu_pt 
#endif
end type offloadable_real

type offloadable_integer 
  type(offloadable_base)         :: base 
  integer(dp), pointer           ::  gpu_pt(:) =>NULL()
  integer(dp), pointer           ::  cpu_pt(:) =>NULL()
#if defined(__CUDA)  
  attributes,device              :: gpu_pt 
#endif 
end type offloadable_integer 

interface 
  function castpointers (c_pointer) bind(C, name="c_cast_ptr") result (t_ptr)
      import  c_size_t, c_ptr
      implicit none 
      type(c_ptr), value :: c_pointer
      integer(c_size_t)  :: t_ptr 
  end function castpointers  
end interface 


interface initdata 
   module procedure init_data_complex, init_data_real, init_data_integer
end interface initdata 

interface associate_read 
!   module procedure read_complex, read_real, read_integer 
end interface associate_read 

interface associate_write 
   module procedure write_complex, write_real, write_integer
   module procedure write_complex2d, write_real2d, write_integer2d
   module procedure write_complex3d, write_real3d, write_integer3d 
end   interface associate_write

interface free
!module procedure   free_complex, free_real, free_integer
end interface free 

contains 

function system_descriptor(use_gpu) result (descsys) 
   implicit none 
   type(data_flags) :: descsys 
   logical, optional, intent(in) :: use_gpu 
   descsys%use_gpu  = .false.
#if defined(__CUDA) 
   if (present(use_gpu)) descsys%use_gpu = use_gpu 
#endif 
end function system_descriptor

subroutine init_base(obj, rank, shape, cpu_ptr, gpu_ptr, descsys) 
   implicit none 
   type(offloadable_base) :: obj
   integer                :: rank 
   integer                :: shape(:) 
   type(c_ptr)      :: cpu_ptr, gpu_ptr 
   type(data_flags)       :: descsys 
   integer                :: i 
   obj%rank = rank 
   obj%size =1 
   do i =1, rank 
      obj%size = obj%size * shape(i) 
   end do 
   allocate(obj%shape, source = shape) 
   obj%cpu_void = cpu_ptr
   obj%gpu_void = gpu_ptr 
end subroutine init_base 

subroutine init_data_complex(a_manager, a, a_d, rank, shape, descsys) 
   implicit none 
   complex(dp),target :: a(:)
   complex(dp),target :: a_d(:) 
   type(offloadable_complex),intent(out) :: a_manager 
#if defined(__CUDA) 
   attributes, device :: a_d 
#endif 
   integer :: rank 
   integer :: shape(rank)
   type(data_flags) :: descsys 
   ! 
   type (c_ptr)       :: cpu_ptr, gpu_ptr
   type (c_devptr)    :: aux_ptr 

   cpu_ptr = c_loc(a) 
#if defined (__CUDA) 
   if (descsys%use_gpu) then 
      aux_ptr = c_devloc ( a_d)
      gpu_ptr = aux_ptr%cptr 
   else 
      gpu_ptr = c_loc(a_d) 
   end if 
#else 
   gpu_ptr= c_loc(a_d) 
#endif
   call init_base(a_manager%base, rank, shape, cpu_ptr, gpu_ptr, descsys ) 
end subroutine init_data_complex

subroutine init_data_real(a_manager, a, a_d, rank, shape, descsys) 
   implicit none 
   real(dp),target :: a(:)
   real(dp),target :: a_d(:) 
   type(offloadable_real),intent(out) :: a_manager 
#if defined(__CUDA) 
   attributes, device :: a_d 
#endif 
   integer :: rank 
   integer :: shape(rank)
   type(data_flags) :: descsys 
   ! 
   type (c_ptr)       :: cpu_ptr, gpu_ptr
   type (c_devptr)    :: aux_ptr 

   cpu_ptr = c_loc(a) 
#if defined (__CUDA) 
   if (descsys%use_gpu) then 
      aux_ptr = c_devloc ( a_d)
      gpu_ptr = aux_ptr%cptr 
   else 
      gpu_ptr = c_loc(a_d) 
   end if 
#else 
   gpu_ptr= c_loc(a_d) 
#endif
   call init_base(a_manager%base, rank, shape, cpu_ptr, gpu_ptr, descsys ) 
end subroutine init_data_real


subroutine init_data_integer(a_manager, a, a_d, rank, shape, descsys) 
   implicit none 
   integer,target :: a(:)
   integer,target :: a_d(:) 
   type(offloadable_integer),intent(out) :: a_manager 
#if defined(__CUDA) 
   attributes, device :: a_d 
#endif 
   integer :: rank 
   integer :: shape(rank)
   type(data_flags) :: descsys 
   ! 
   type (c_ptr)       :: cpu_ptr, gpu_ptr
   type (c_devptr)    :: aux_ptr 

   cpu_ptr = c_loc(a) 
#if defined (__CUDA) 
   if (descsys%use_gpu) then 
      aux_ptr = c_devloc ( a_d)
      gpu_ptr = aux_ptr%cptr 
   else 
      gpu_ptr = c_loc(a_d) 
   end if 
#else 
   gpu_ptr= c_loc(a_d) 
#endif
   call init_base(a_manager%base, rank, shape, cpu_ptr, gpu_ptr, descsys ) 
end subroutine init_data_integer

subroutine write_complex(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_complex) :: a_manager 
   complex(dp),pointer       :: ptr(:)    
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   type(c_ptr)               :: aux_cptr 
#if defined(__CUDA)
   type(c_devptr)            :: aux_cdevptr
#endif 
   if ( mode == 'd' .or. mode == 'D') then 
      if (descsys%use_gpu) then 
         a_manager%base%dev_tready = .true. 
         a_manager%base%host_tready = .false. 
#if defined (__CUDA)
         aux_devcptr%cptr  = a_manager%base%gpu_void 
         call c_f_pointer(aux_devcptr, ptr(a_manager%size)) 
#else 
         aux_cptr = a_manager%base%gpu_void  
         call c_f_pointer(aux_cptr, ptr,shape=[a_manager%base%size])  
#endif 
      else
         a_manager%base%dev_tready =.false. 
         a_manager%base%host_tready =.true. 
         aux_cptr = a_manager%base%cpu_void 
         call c_f_pointer(aux_cptr, ptr,shape=[a_manager%base%size]) 
      end if 
   else if ( mode == 'h' .or. mode == 'H')  then 
      a_manager%base%dev_tready = .false. 
      a_manager%base%host_tready = .true. 
      aux_cptr = a_manager%base%cpu_void 
      call c_f_pointer (aux_cptr, ptr, shape=[a_manager%base%size]) 
   end if 
end subroutine write_complex 


subroutine write_complex2d(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_complex) :: a_manager 
   complex(dp),pointer       :: ptr(:,:)   
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   complex(dp), pointer       :: auxpt(:) 
   type(c_ptr)               :: tempcptr 
   call write_complex(a_manager, mode, auxpt, descsys) 
   tempcptr = c_loc(auxpt) 
   call  c_f_pointer(tempcptr, ptr, a_manager%base%shape) 
end subroutine write_complex2d 
         

subroutine write_complex3d(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_complex) :: a_manager 
   complex(dp),pointer       :: ptr(:,:,:)   
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   complex(dp), pointer       :: auxpt(:) 
   type(c_ptr)               :: tempcptr 
   call write_complex(a_manager, mode, auxpt, descsys) 
   tempcptr = c_loc(auxpt) 
   call  c_f_pointer(tempcptr, ptr, a_manager%base%shape) 
end subroutine write_complex3d 
 
subroutine write_real(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_real) :: a_manager 
   real(dp),pointer       :: ptr(:)    
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   type(c_ptr)               :: aux_cptr 
#if defined(__CUDA)
   type(c_devptr)            :: aux_cdevptr
#endif 
   if ( mode == 'd' .or. mode == 'D') then 
      if (descsys%use_gpu) then 
         a_manager%base%dev_tready = .true. 
         a_manager%base%host_tready = .false. 
#if defined (__CUDA)
         aux_devcptr%cptr  = a_manager%base%gpu_void 
         call c_f_pointer(aux_devcptr, ptr(a_manager%size)) 
#else 
         aux_cptr = a_manager%base%gpu_void  
         call c_f_pointer(aux_cptr, ptr,shape=[a_manager%base%size])  
#endif 
      else
         a_manager%base%dev_tready =.false. 
         a_manager%base%host_tready =.true. 
         aux_cptr = a_manager%base%cpu_void 
         call c_f_pointer(aux_cptr, ptr,shape=[a_manager%base%size]) 
      end if 
   else if ( mode == 'h' .or. mode == 'H')  then 
      a_manager%base%dev_tready = .false. 
      a_manager%base%host_tready = .true. 
      aux_cptr = a_manager%base%cpu_void 
      call c_f_pointer (aux_cptr, ptr, shape=[a_manager%base%size]) 
   end if 
end subroutine write_real  

subroutine write_real2d(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_real) :: a_manager 
   real(dp),pointer          :: ptr(:,:)   
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   real(dp), pointer       :: auxpt(:) 
   type(c_ptr)               :: tempcptr 
   call write_real(a_manager, mode, auxpt, descsys) 
   tempcptr = c_loc(auxpt) 
   call  c_f_pointer(tempcptr, ptr, a_manager%base%shape) 
end subroutine write_real2d 
         

subroutine write_real3d(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_real) :: a_manager 
   real(dp),pointer       :: ptr(:,:,:)   
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   real(dp), pointer       :: auxpt(:) 
   type(c_ptr)               :: tempcptr 
   call write_real(a_manager, mode, auxpt, descsys) 
   tempcptr = c_loc(auxpt) 
   call  c_f_pointer(tempcptr, ptr, a_manager%base%shape) 
end subroutine write_real3d 
 
subroutine write_integer(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_integer) :: a_manager 
   integer(dp),pointer       :: ptr(:)    
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   type(c_ptr)               :: aux_cptr 
#if defined(__CUDA)
   type(c_devptr)            :: aux_cdevptr
#endif 
   if ( mode == 'd' .or. mode == 'D') then 
      if (descsys%use_gpu) then 
         a_manager%base%dev_tready = .true. 
         a_manager%base%host_tready = .false. 
#if defined (__CUDA)
         aux_devcptr%cptr  = a_manager%base%gpu_void 
         call c_f_pointer(aux_devcptr, ptr(a_manager%size)) 
#else 
         aux_cptr = a_manager%base%gpu_void  
         call c_f_pointer(aux_cptr, ptr,shape=[a_manager%base%size])  
#endif 
      else
         a_manager%base%dev_tready =.false. 
         a_manager%base%host_tready =.true. 
         aux_cptr = a_manager%base%cpu_void 
         call c_f_pointer(aux_cptr, ptr,shape=[a_manager%base%size]) 
      end if 
   else if ( mode == 'h' .or. mode == 'H')  then 
      a_manager%base%dev_tready = .false. 
      a_manager%base%host_tready = .true. 
      aux_cptr = a_manager%base%cpu_void 
      call c_f_pointer (aux_cptr, ptr, shape=[a_manager%base%size]) 
   end if 
end subroutine write_integer  

subroutine write_integer2d(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_integer) :: a_manager 
   integer(dp),pointer          :: ptr(:,:)   
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   integer(dp), pointer       :: auxpt(:) 
   type(c_ptr)               :: tempcptr 
   call write_integer(a_manager, mode, auxpt, descsys) 
   tempcptr = c_loc(auxpt) 
   call  c_f_pointer(tempcptr, ptr, a_manager%base%shape) 
end subroutine write_integer2d 
        ! 

subroutine write_integer3d(a_manager, mode, ptr, descsys) 
   implicit none 
   type(offloadable_integer) :: a_manager 
   integer(dp),pointer       :: ptr(:,:,:)   
   character                 :: mode 
   type(data_flags)          :: descsys 
   ! 
   integer(dp), pointer       :: auxpt(:) 
   type(c_ptr)               :: tempcptr 
   call write_integer(a_manager, mode, auxpt, descsys) 
   tempcptr = c_loc(auxpt) 
   call  c_f_pointer(tempcptr, ptr, a_manager%base%shape) 
end subroutine write_integer3d 
 
subroutine read_complex(a_manager, mode, ptr, descsys)  
   implicit none 
   type(offloadable_complex) :: a_manager 
   complex(dp),pointer       :: ptr(:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   type(c_ptr)               :: aux_cptr 
   complex(dp),pointer       :: aux_d(:), aux(:)
#if defined(__CUDA)
   type(c_devptr)            :: aux_cdevptr
   attributes, device        :: aux(:) 
#endif 
   call c_f_pointer(a_manager%base%cpu_void, aux,   shape=[a_manager%base%size]) 
   if ( descsys%use_gpu) then 
      call c_f_pointer(a_manager%base%gpu_void, aux_d, shape=[a_manager%base%size])
   else 
      call c_f_pointer(a_manager%base%cpu_void, aux_d, shape=[a_manager%base%size]) 
   end if 
   select case (trim(mode))
      case ('rwd' , 'RWD', 'rwD', 'RWd', 'rod', 'ROD', 'ROd', 'roD') 
         if (.not. a_manager%base%dev_tready)  aux_d = aux 
         ptr => aux_d 
         !!  writeout some info msg about the copy really occurring ?  
         a_manager%base%dev_tready = .true. 
         if ( any(['rwd','RWD','RWd', 'rwD'] == trim(mode))) then 
            a_manager%base%host_tready=.false. 
         else 
            a_manager%base%host_tready = .true. 
         end if 
      case ('rwh', 'RWH', 'rwH', 'RWh', 'roh', 'ROH', 'ROh', 'roH') 
        if (.not. a_manager%base%host_tready) aux = aux_d 
        ptr => aux 
        a_manager%base%host_tready = .true. 
        if ( any(['rwh', 'RWH', 'rwH', 'RWH'] == trim(mode))) then 
           a_manager%base%dev_tready = .false. 
        else 
           a_manager%base%dev_tready = .true. 
        end if 
      case default 
         call dxlib_errore ('associate_read', 'unexpected error, wrong mode argument passed', 2)  
   end select  
end subroutine read_complex    

subroutine read_complex2d(a_manager, mode, ptr, descsys)
   implicit none 
   type(offloadable_complex) :: a_manager 
   complex(dp),pointer       :: ptr(:,:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   complex(dp),pointer       :: aux(:) 
   type(c_ptr)               :: aux_cptr
   call read_complex(a_manager, mode, aux, descsys) 
   aux_cptr = c_loc(aux) 
   call c_f_pointer (aux_cptr, ptr, a_manager%base%shape) 
end subroutine read_complex2d 

subroutine read_complex3d(a_manager, mode, ptr, descsys)
   implicit none 
   type(offloadable_complex) :: a_manager 
   complex(dp),pointer       :: ptr(:,:,:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   complex(dp),pointer       :: aux(:) 
   type(c_ptr)               :: aux_cptr
   call read_complex(a_manager, mode, aux, descsys) 
   aux_cptr = c_loc(aux) 
   call c_f_pointer (aux_cptr, ptr, a_manager%base%shape) 
end subroutine read_complex3d 

subroutine read_real(a_manager, mode, ptr, descsys)  
   implicit none 
   type(offloadable_real) :: a_manager 
   real(dp),pointer       :: ptr(:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   type(c_ptr)               :: aux_cptr 
   real(dp),pointer       :: aux_d(:), aux(:)
#if defined(__CUDA)
   type(c_devptr)            :: aux_cdevptr
   attributes, device        :: aux(:) 
#endif 
   call c_f_pointer(a_manager%base%cpu_void, aux,   shape=[a_manager%base%size]) 
   if ( descsys%use_gpu) then 
      call c_f_pointer(a_manager%base%gpu_void, aux_d, shape=[a_manager%base%size])
   else 
      call c_f_pointer(a_manager%base%cpu_void, aux_d, shape=[a_manager%base%size]) 
   end if 
   select case (trim(mode))
      case ('rwd' , 'RWD', 'rwD', 'RWd', 'rod', 'ROD', 'ROd', 'roD') 
         if (.not. a_manager%base%dev_tready)  aux_d = aux 
         ptr => aux_d 
         !!  writeout some info msg about the copy really occurring ?  
         a_manager%base%dev_tready = .true. 
         if ( any(['rwd','RWD','RWd', 'rwD'] == trim(mode))) then 
            a_manager%base%host_tready=.false. 
         else 
            a_manager%base%host_tready = .true. 
         end if 
      case ('rwh', 'RWH', 'rwH', 'RWh', 'roh', 'ROH', 'ROh', 'roH') 
        if (.not. a_manager%base%host_tready) aux = aux_d 
        ptr => aux
        a_manager%base%host_tready = .true. 
        if ( any(['rwh', 'RWH', 'rwH', 'RWH'] == trim(mode))) then 
           a_manager%base%dev_tready = .false. 
        else 
           a_manager%base%dev_tready = .true. 
        end if 
      case default 
         call dxlib_errore ('associate_read', 'unexpected error, wrong mode argument passed', 2)  
   end select  
end subroutine read_real    

subroutine read_real2d(a_manager, mode, ptr, descsys)
   implicit none 
   type(offloadable_real) :: a_manager 
   real(dp),pointer       :: ptr(:,:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   real(dp),pointer       :: aux(:) 
   type(c_ptr)            :: aux_cptr 
   call read_real(a_manager, mode, aux, descsys) 
   aux_cptr = c_loc(aux) 
   call c_f_pointer (aux_cptr, ptr, a_manager%base%shape) 
end subroutine read_real2d 

subroutine read_real3d(a_manager, mode, ptr, descsys)
   implicit none 
   type(offloadable_real) :: a_manager 
   real(dp),pointer       :: ptr(:,:,:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   real(dp),pointer       :: aux(:) 
   type(c_ptr)            :: aux_cptr
   call read_real(a_manager, mode, aux, descsys) 
   aux_cptr = c_loc(aux) 
   call c_f_pointer (aux_cptr, ptr, a_manager%base%shape) 
end subroutine read_real3d 

subroutine read_integer(a_manager, mode, ptr, descsys)  
   implicit none 
   type(offloadable_integer) :: a_manager 
   integer, pointer       :: ptr(:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   type(c_ptr)               :: aux_cptr 
   integer, pointer       :: aux_d(:), aux(:)
#if defined(__CUDA)
   type(c_devptr)            :: aux_cdevptr
   attributes, device        :: aux(:) 
#endif 
   call c_f_pointer(a_manager%base%cpu_void, aux,   shape=[a_manager%base%size]) 
   if ( descsys%use_gpu) then 
      call c_f_pointer(a_manager%base%gpu_void, aux_d, shape=[a_manager%base%size])
   else 
      call c_f_pointer(a_manager%base%cpu_void, aux_d, shape=[a_manager%base%size]) 
   end if 
   select case (trim(mode))
      case ('rwd' , 'RWD', 'rwD', 'RWd', 'rod', 'ROD', 'ROd', 'roD') 
         if (.not. a_manager%base%dev_tready)  aux_d = aux 
         ptr => aux_d 
         !!  writeout some info msg about the copy really occurring ?  
         a_manager%base%dev_tready = .true. 
         if ( any(['rwd','RWD','RWd', 'rwD'] == trim(mode))) then 
            a_manager%base%host_tready=.false. 
         else 
            a_manager%base%host_tready = .true. 
         end if 
      case ('rwh', 'RWH', 'rwH', 'RWh', 'roh', 'ROH', 'ROh', 'roH') 
        if (.not. a_manager%base%host_tready) aux = aux_d 
        ptr => aux 
        a_manager%base%host_tready = .true. 
        if ( any(['rwh', 'RWH', 'rwH', 'RWH'] == trim(mode))) then 
           a_manager%base%dev_tready = .false. 
        else 
           a_manager%base%dev_tready = .true. 
        end if 
      case default 
         call dxlib_errore ('associate_read', 'unexpected error, wrong mode argument passed', 2)  
   end select  
end subroutine read_integer    

subroutine read_integer2d(a_manager, mode, ptr, descsys)
   implicit none 
   type(offloadable_integer) :: a_manager 
   integer, pointer       :: ptr(:,:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   integer, pointer       :: aux(:) 
   type(c_ptr)            :: aux_cptr
   call read_integer(a_manager, mode, aux, descsys) 
   aux_cptr = c_loc(aux) 
   call c_f_pointer (aux_cptr, ptr, a_manager%base%shape) 
end subroutine read_integer2d 

subroutine read_integer3d(a_manager, mode, ptr, descsys)
   implicit none 
   type(offloadable_integer) :: a_manager 
   integer, pointer       :: ptr(:,:,:)    
   character(3)              :: mode 
   type(data_flags)          :: descsys 
   ! 
   integer, pointer       :: aux(:) 
   type(c_ptr)            :: aux_cptr
   call read_integer(a_manager, mode, aux, descsys) 
   aux_cptr = c_loc(aux) 
   call c_f_pointer (aux_cptr, ptr, a_manager%base%shape) 
end subroutine read_integer3d 




end module offloadable 
