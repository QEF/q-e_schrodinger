#define HANDLE_ERROR( return_err )  call cudaErrorCheck( return_err, errored )

subroutine cudaErrorCheck( err, b )
   use cuda_runtime_h
   implicit none
   
   integer (KIND(cudaSuccess)), intent(in) :: err
   
   character, pointer :: string_err(:)
   integer :: i
   logical :: b
   
   b = .false.
   if ( err /= cudaSuccess ) then
       
       call C_F_POINTER( cudaGetErrorString( err ), string_err, [1] )
       i = 1
       do while ( string_err( i ) /= C_NULL_CHAR )
           i = i + 1
       enddo
   
       !print *, 'CUDA Error Detected'
       call dxlib_errore("devxlib", "CUDA Error Detected " // string_err( 1:i ), i)
       b = .true.
   endif

end subroutine cudaErrorCheck

module c_allocation
   use ISO_C_BINDING
   implicit none
   private
   public c_malloc, c_free

   interface

      function c_malloc(size) bind(C,name='malloc')
         import
         implicit none
         type(C_PTR) c_malloc
         integer(C_SIZE_T), value :: size
      end function c_malloc

      subroutine c_free(ptr) bind(C,name='free')
         import
         implicit none
         type(C_PTR), value :: ptr
      end subroutine c_free
   end interface
   
end module c_allocation

module offloadable_allocation_cudac
  
  
  implicit none

contains

subroutine allocate_(n_bytes, cpu_ptr, gpu_ptr, pinned)
   use, intrinsic :: ISO_C_BINDING
   use c_allocation, only : c_malloc
   use cuda_runtime_h, only : cudaMalloc, cudaMallocHost

   implicit none
   integer(c_size_t),intent(in)  :: n_bytes
   type(c_ptr) :: cpu_ptr
   type(c_ptr) :: gpu_ptr
   logical, intent(in) :: pinned
   
   logical :: errored = .false.
   
   HANDLE_ERROR( cudaMalloc( gpu_ptr, n_bytes ) )
   if (pinned) then
      HANDLE_ERROR( cudaMallocHost( cpu_ptr, n_bytes ) )
   else
      cpu_ptr = c_malloc (n_bytes)
   endif
end subroutine allocate_

subroutine deallocate_(cpu_ptr, gpu_ptr, pinned)
   use, intrinsic :: ISO_C_BINDING
   use c_allocation, only : c_free
   use cuda_runtime_h, only : cudaFree, cudaFreeHost
   implicit none
   type(c_ptr) :: cpu_ptr
   type(c_ptr) :: gpu_ptr
   logical, intent(in) :: pinned
   
   logical :: errored = .false.
   
   HANDLE_ERROR( cudaFree( gpu_ptr ) )
   if (pinned) then
      HANDLE_ERROR( cudaFreeHost( cpu_ptr ) )
   else
      CALL c_free (cpu_ptr)
   endif
end subroutine deallocate_
end module offloadable_allocation_cudac
