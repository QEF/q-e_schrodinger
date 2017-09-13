!
! Copyright (C) 2004-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------------
!
MODULE dtr
#if defined (__DTR)
  !
  ! ... The variables needed to describe the atoms and related quantities
  !
  USE ISO_C_BINDING, ONLY: c_ptr, c_float
  USE kinds,         ONLY : DP
  USE ions_base,        ONLY: nat
  IMPLICIT NONE
  !
  PUBLIC dtr_init_writer, dtr_close_writer, dtr_add_step
  !
  PRIVATE
  !
  SAVE
  !
  TYPE(c_ptr) :: dtr_handle
  !
  PRIVATE fopen_file_write, fwrite_timestep_from_data
  !
  CONTAINS
    !
    !------------------------------------------------------------------------
    SUBROUTINE dtr_init_writer()
      USE ISO_C_BINDING, ONLY: C_ASSOCIATED
      USE io_global, ONLY : ionode, stdout
      USE io_files,   ONLY : prefix
      USE ions_base,       ONLY: nat
      IMPLICIT NONE
      !
      dtr_handle = fopen_file_write(TRIM(prefix)//'_trj', nat)
      IF (ionode) THEN
        WRITE(stdout, *) "  DTR module: initializing..."
        IF (.not. C_ASSOCIATED(dtr_handle)) THEN
          WRITE(stdout, *) "  DTR module: ERROR: dtr_handle is null."
        ENDIF
      ENDIF
    END SUBROUTINE dtr_init_writer

    SUBROUTINE dtr_add_step(istep)
      USE cell_base,       ONLY : at, alat
      USE ions_base,       ONLY: nat, tau
      USE io_global, ONLY : ionode, ionode_id, stdout
      IMPLICIT NONE
      !
      INTEGER, INTENT(in) :: istep
      INTEGER :: ret
      REAL :: pos(3 * nat), box(9)
      REAL(DP) :: time
      !
      pos = REAL(RESHAPE(tau, (/3 * nat/) ) * alat)
      box = REAL(RESHAPE(at, (/9/)) * alat)
      time = REAL(istep, kind=DP)

      IF (ionode) THEN
        ret = fwrite_timestep_from_data(dtr_handle, pos, box, time)
        WRITE(stdout, '("  DTR module: write status = ", i3)') ret
      ENDIF
    END SUBROUTINE dtr_add_step
    !
    FUNCTION fopen_file_write(fpath, fnatoms)
    USE ISO_C_BINDING, ONLY: c_ptr, c_char, c_int, C_NULL_CHAR
    IMPLICIT NONE
    TYPE(c_ptr) :: fopen_file_write
    CHARACTER(*), INTENT(in) :: fpath
    INTEGER, INTENT(in) :: fnatoms
    !
    INTERFACE
    FUNCTION copen_file_write(cpath, ctype, cnatoms) BIND(C, name="open_file_write")
    !  void *open_file_write(const char *path, const char *type, int natoms)
      USE ISO_C_BINDING, ONLY: c_char, c_int, c_ptr
      TYPE(c_ptr) :: copen_file_write
      CHARACTER(kind=c_char) :: cpath(*)
      CHARACTER(kind=c_char) :: ctype(*)
      INTEGER(kind=c_int), VALUE    :: cnatoms
    END FUNCTION copen_file_write
    END INTERFACE
    !
    CHARACTER(len=len_trim(fpath)+1,kind=c_char) :: cpath!(*)
    CHARACTER(len=len_trim("dtr")+1,kind=c_char) :: ctype!(*)
    INTEGER(kind=c_int)    :: cnatoms
    !
    cpath = TRIM(fpath)//C_NULL_CHAR
    ctype = TRIM("dtr")//C_NULL_CHAR
    cnatoms = INT(fnatoms, kind=c_int)
    fopen_file_write = copen_file_write(cpath, ctype, cnatoms)
  END FUNCTION fopen_file_write
  !
  SUBROUTINE dtr_close_writer()
    USE io_global, ONLY : ionode
    IMPLICIT NONE
    !
    INTERFACE
    SUBROUTINE cclose_file_write(chandle) BIND(C, name="close_file_write")
    !  void close_file_write(void* handle)
      USE ISO_C_BINDING, ONLY: c_ptr
      TYPE(c_ptr), VALUE :: chandle
    END SUBROUTINE cclose_file_write
    END INTERFACE
    !
    IF (ionode) CALL cclose_file_write(dtr_handle)
  END SUBROUTINE dtr_close_writer
  !
  FUNCTION fwrite_timestep_from_data(fhandle, fcoords, fbox, ftime)
    USE ISO_C_BINDING, ONLY: c_ptr, c_float, c_double, c_int, C_LOC
    USE mp,                  ONLY: mp_barrier
    USE mp_world,            ONLY: world_comm
    IMPLICIT NONE
    INTEGER :: fwrite_timestep_from_data
    TYPE(c_ptr) :: fhandle
    REAL, INTENT(in) :: fcoords(nat * 3), fbox(9)
    REAL(DP), INTENT(in) :: ftime
    !
    INTERFACE
    FUNCTION cwrite_timestep_from_data(chandle, ccoords, cbox, ctime) BIND(C, name="write_timestep_from_data")
    !  int write_timestep_from_data(void *v, float *coords, float *box, double time)
      USE ISO_C_BINDING, ONLY: c_int, c_ptr, c_double
      INTEGER(c_int) :: cwrite_timestep_from_data
      TYPE(c_ptr), VALUE :: chandle
      TYPE(c_ptr), VALUE :: ccoords, cbox
      REAL(c_double), VALUE :: ctime
    END FUNCTION cwrite_timestep_from_data
    END INTERFACE
    !
    REAL(kind=c_float), TARGET :: ccoords(nat * 3), cbox(9)
    INTEGER(kind=c_int) :: tmp
    REAL(kind=c_double) :: ctime
    !
    ccoords = REAL(fcoords, kind=c_float)
    cbox = REAL(fbox, kind=c_float)
    ctime = REAL(ftime, kind=c_double)
    ! without this mp_barrier code crashes, still trying to understand this
#if defined(__MPI)
    CALL mp_barrier(world_comm)
#endif
    tmp = cwrite_timestep_from_data(dtr_handle, C_LOC(ccoords), C_LOC(cbox), ctime)
    fwrite_timestep_from_data = INT(tmp)
  END FUNCTION fwrite_timestep_from_data
!
#endif
END MODULE dtr
