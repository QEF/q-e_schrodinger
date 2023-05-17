
#if defined(_CUDA)
#  ifndef __CUDA
#     define __CUDA
#  endif
#endif
#if defined(_OPENMP)
#  ifndef __OPENMP
#     define __OPENMP
#  endif
#endif

#if defined(__CUDA) || defined(__OPENACC) || defined(__OPENMP5)
#  define __HAVE_DEVICE
#endif


#if defined __OPENACC
#  define DEV_ACC !$acc
#else
#  define DEV_ACC !!!!
#endif

#if defined __CUDA
#  define DEV_CUF !$cuf
#else
#  define DEV_CUF !!!!
#endif

#if defined __OPENMP && !defined (__HAVE_DEVICE)
#  define DEV_OMP !$omp
#else
#  define DEV_OMP !!!!
#endif

