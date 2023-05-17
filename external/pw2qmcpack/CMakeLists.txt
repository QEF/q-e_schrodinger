###########################################################
# pw2qmcpack.x
###########################################################

# MPI C
add_library(pw2qmcpack_mpi_c INTERFACE)
qe_install_targets(pw2qmcpack_mpi_c)

if(QE_ENABLE_MPI)
    find_package(MPI REQUIRED C)
    target_link_libraries(pw2qmcpack_mpi_c
        INTERFACE MPI::MPI_C)
endif()

# HDF5 C
add_library(pw2qmcpack_hdf5_c INTERFACE)

if(QE_ENABLE_MPI)
    option(HDF5_PREFER_PARALLEL "Prefer parallel HDF5" ON)
endif()

if(QE_ENABLE_STATIC_BUILD)
    set(HDF5_USE_STATIC_LIBRARIES TRUE)
endif()

find_package(HDF5 REQUIRED C HL)

if(NOT HDF5_FOUND)
    message(FATAL_ERROR "pw2qmcpack requires C and HL interfaces from the HDF5 library!")
endif()

if(NOT QE_ENABLE_MPI AND HDF5_IS_PARALLEL)
    message(FATAL_ERROR "pw2qmcpack without MPI requires serial HDF5!")
endif()

target_link_libraries(pw2qmcpack_hdf5_c
    INTERFACE
        ${HDF5_C_LIBRARIES}
        ${HDF5_C_HL_LIBRARIES})
target_include_directories(pw2qmcpack_hdf5_c
    INTERFACE
        ${HDF5_C_INCLUDE_DIRS})
target_compile_definitions(pw2qmcpack_hdf5_c
    INTERFACE
        ${HDF5_C_DEFINITIONS})

qe_add_library(pw2qmcpack_esh5 src/esh5_interfaces.c)
target_compile_definitions(pw2qmcpack_esh5 PUBLIC "H5_USE_16_API;__HDF5_C")
target_link_libraries(pw2qmcpack_esh5 PRIVATE pw2qmcpack_hdf5_c pw2qmcpack_mpi_c)

set(sources src/pw2qmcpack.f90)
qe_add_executable(pw2qmcpack_exe ${sources})
set_target_properties(pw2qmcpack_exe PROPERTIES OUTPUT_NAME pw2qmcpack.x)
target_link_libraries(pw2qmcpack_exe
    PRIVATE
        qe_mpi_fortran
        qe_pw
        qe_pp
        qe_modules
        qe_fftx
        qe_upflib
        qe_fox
        qe_xclib
        pw2qmcpack_esh5)

qe_install_targets(
    # Libraries
    pw2qmcpack_hdf5_c
    pw2qmcpack_esh5
    # Executables
    pw2qmcpack_exe)

add_custom_target(pw2qmcpack
    DEPENDS
        pw2qmcpack_exe
    COMMENT
        "pw2qmcpack program")
