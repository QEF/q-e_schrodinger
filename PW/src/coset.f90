!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
SUBROUTINE coset( nrot, table, sym, nsym, irg )
  !-----------------------------------------------------------------------
  !!  Divides the elements of a given group into left cosets of one
  !!  of its subgroups.
  !
  !!  The input is the array sym which is true only for the
  !!  operations of the subgroup, the output is nsym, and the array irg,
  !!  which contains as its first elements the indices of the subgroup,
  !!  and then its right cosets.
  !
  !!  Revised layout 1 may 1995 by A. Dal Corso
  !
  USE kinds
  !
  IMPLICIT NONE
  !
  INTEGER :: nrot
  !! input: order of the group
  INTEGER :: table(48, 48)
  !! input: multiplication table of the group
  INTEGER :: nsym
  !! output: order of the subgroup
  INTEGER :: irg(48)
  !! output: gives the correspondence of symme
  !! operations forming a n-th coset
  LOGICAL :: sym(48)
  !! input: flag indicating if an operation
  !! belongs to the subgroup
  !
  ! ... local variables
  !
  LOGICAL :: done(48)
  ! if true the operation has been already ch
  !
  INTEGER :: irot, ncos, isym, nc, nelm
  ! counter on rotations
  ! number of cosets (=nrot/nsym)
  ! counter on symmetries
  ! counter on cosets
  ! counter on the number of elements
  !
  !    here we count the elements of the subgroup and set the first part o
  !    irg which contain the subgroup
  !
  nsym = 0
  DO irot = 1, nrot
     done (irot) = sym (irot)
     IF ( sym (irot) ) THEN
        nsym = nsym + 1
        irg (nsym) = irot
     ENDIF
  ENDDO
  !
  ! ... we check that the order of the subgroup is a divisor of the order
  ! total group. ncos is the number of cosets
  !
  IF ( nsym == 0 ) CALL errore( 'coset', 'nsym == 0', 1 ) 
  !
  ncos = nrot / nsym
  IF ( ncos * nsym /= nrot ) CALL errore( 'coset', &
   'The order'//' of the group is not a multiple of that of the subgroup', 1 )
  !
  ! ... here we set the other elements of irg, by using the multiplication
  !
  nelm = nsym
  DO nc = 2, ncos
     DO irot = 1, nrot
        IF ( .NOT.done(irot) ) THEN
           DO isym = 1, nsym
              nelm = nelm + 1
              irg (nelm) = table (irot, irg (isym) )
              done (irg (nelm) ) = .TRUE.
           ENDDO
        ENDIF
     ENDDO
     !
  ENDDO
  !
  RETURN
  !
END SUBROUTINE coset
