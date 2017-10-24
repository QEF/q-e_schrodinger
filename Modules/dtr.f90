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
  USE ISO_C_BINDING, ONLY: c_ptr, c_float, C_NULL_ptr
  USE kinds,         ONLY : DP
  USE ions_base,        ONLY: nat
  IMPLICIT NONE
  !
  PUBLIC dtr_init_writer, dtr_close_writer, dtr_add_step
  !
  PRIVATE
  !
  TYPE(c_ptr), SAVE :: dtr_handle = C_NULL_ptr
  !
  PRIVATE fopen_file_write, fwrite_timestep_from_data
  !
  CONTAINS
    !
    !------------------------------------------------------------------------
    SUBROUTINE dtr_init_writer()
      USE ISO_C_BINDING, ONLY: C_ASSOCIATED
      USE io_global, ONLY : meta_ionode, stdout
      USE io_files,   ONLY : prefix, tmp_dir
      USE ions_base,       ONLY: nat
      USE wrappers,  ONLY : f_mkdir_safe
      IMPLICIT NONE
      CHARACTER(256) :: dirname
      INTEGER        :: ierr
      !
      IF (meta_ionode) THEN
        dirname = TRIM(TRIM(tmp_dir)//TRIM(prefix)//'_trj')
        ierr = f_mkdir_safe(dirname)
        CALL errore( 'create_directory', &
           'unable to create directory ' // TRIM( dirname ), ierr )

        dtr_handle = fopen_file_write(dirname, nat)
        WRITE(stdout, *) "  DTR module: initializing..."
        WRITE(stdout, *) dirname
        IF (.not. C_ASSOCIATED(dtr_handle)) THEN
          WRITE(stdout, *) "  DTR module: ERROR: dtr_handle is null."
        ENDIF
      ENDIF
    END SUBROUTINE dtr_init_writer

    SUBROUTINE dtr_add_step(istep)
      USE cell_base,       ONLY : at, alat
      USE ions_base,       ONLY: nat, tau
      USE io_global, ONLY : meta_ionode, stdout
      USE constants, ONLY: BOHR_RADIUS_ANGS
      USE mp_world,            ONLY: world_comm
      IMPLICIT NONE
      !
      INTEGER, INTENT(in) :: istep
      INTEGER :: ret
      REAL :: pos(3 * nat), box(9)
      REAL(DP) :: time
      !
      call mp_synchronize(world_comm)
      IF (meta_ionode) THEN
        pos = REAL(RESHAPE(tau, (/3 * nat/) ) * alat * BOHR_RADIUS_ANGS)
        box = REAL(RESHAPE(at, (/9/)) * alat * BOHR_RADIUS_ANGS)
        time = REAL(istep, kind=DP)
        ret = fwrite_timestep_from_data(pos, box, time)
        WRITE(stdout, '("  DTR module: write status = ", i3)') ret
      ENDIF
      call mp_synchronize(world_comm)

    END SUBROUTINE dtr_add_step
    !
    FUNCTION fopen_file_write(fpath, fnatoms) RESULT(this)
    USE ISO_C_BINDING, ONLY: c_ptr, c_char, c_int, C_NULL_CHAR
    IMPLICIT NONE
    TYPE(c_ptr) :: this
    CHARACTER(*), INTENT(in) :: fpath
    INTEGER(c_int), INTENT(in) :: fnatoms
    !
    INTERFACE
    FUNCTION copen_file_write(cpath, ctype, cnatoms) RESULT(this) BIND(C, name="open_file_write")
    !  void *open_file_write(const char *path, const char *type, int natoms)
      USE ISO_C_BINDING, ONLY: c_char, c_int, c_ptr
      TYPE(c_ptr) :: this
      CHARACTER(kind=c_char) :: cpath(*)
      CHARACTER(kind=c_char) :: ctype(*)
      INTEGER(kind=c_int), VALUE :: cnatoms
    END FUNCTION copen_file_write
    END INTERFACE
    !
    CHARACTER(len=len_trim(fpath)+1,kind=c_char) :: cpath!(*)
    CHARACTER(len=len_trim("dtr")+1,kind=c_char) :: ctype!(*)
    !
    cpath = TRIM(fpath)//C_NULL_CHAR
    ctype = TRIM("dtr")//C_NULL_CHAR
    this = copen_file_write(cpath, ctype, fnatoms)
  END FUNCTION fopen_file_write
  !
  SUBROUTINE dtr_close_writer()
    USE io_global, ONLY : meta_ionode
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
    IF (meta_ionode) CALL cclose_file_write(dtr_handle)
  END SUBROUTINE dtr_close_writer
  !
  FUNCTION fwrite_timestep_from_data(fcoords, fbox, ftime)
    USE ISO_C_BINDING, ONLY: c_ptr, c_float, c_double, c_int, C_ASSOCIATED
    USE io_global, ONLY : stdout
    IMPLICIT NONE
    INTEGER :: fwrite_timestep_from_data
    REAL(c_float), INTENT(in) :: fcoords(nat * 3), fbox(9)
    REAL(c_double), INTENT(in) :: ftime
    !
    INTERFACE
    FUNCTION cwrite_timestep_from_data(chandle, ccoords, cbox, ctime) BIND(C, name="write_timestep_from_data")
    !  int write_timestep_from_data(void *v, float *coords, float *box, double time)
      USE ISO_C_BINDING, ONLY: c_int, c_ptr, c_double, c_float
      INTEGER(c_int) :: cwrite_timestep_from_data
      TYPE(c_ptr), VALUE :: chandle
      REAL(c_float) :: ccoords(*), cbox(*)
      REAL(c_double), VALUE :: ctime
    END FUNCTION cwrite_timestep_from_data
    END INTERFACE
    !
    IF (C_ASSOCIATED(dtr_handle)) THEN
          WRITE(stdout, *) "  DTR module: dtr_handle still associated."
    ENDIF
    !
    fwrite_timestep_from_data = cwrite_timestep_from_data(&
        dtr_handle, fcoords, fbox, ftime)
  END FUNCTION fwrite_timestep_from_data
!
#endif
END MODULE dtr
