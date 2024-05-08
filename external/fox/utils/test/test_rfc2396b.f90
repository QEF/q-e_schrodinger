program test_rfc2396

  use fox_m_utils_uri

  ! These URI examples come from appendix C of RFC 2396 and as such 
  ! are a derivative work subject to the following:
  !
  ! Copyright (C) The Internet Society (1998).  All Rights Reserved.
  !
  ! This document and translations of it may be copied and furnished to
  ! others, and derivative works that comment on or otherwise explain it
  ! or assist in its implementation may be prepared, copied, published
  ! and distributed, in whole or in part, without restriction of any
  ! kind, provided that the above copyright notice and this paragraph are
  ! included on all such copies and derivative works.  However, this
  ! document itself may not be modified in any way, such as by removing
  ! the copyright notice or references to the Internet Society or other
  ! Internet organizations, except as needed for the purpose of
  ! developing Internet standards in which case the procedures for
  ! copyrights defined in the Internet Standards process must be
  ! followed, or as required to translate it into languages other than
  ! English.
  ! 

  type(URI), pointer :: u, base, u3

  base => parseURI("http://a/b/c/d;p?q")

  u3 => parseURI("")
  u => rebaseURI(base, u3)
  call check

  ! RFC 2396 and 3986 differ in the next
  ! four cases. We follow 3986 and should
  ! return http://a/g in all four cases.
  u3 => parseURI("../../../g")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("../../../../g")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("/./g")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("/../g")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g.")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI(".g")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g..")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("..g")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("./../g")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("./g/.")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g/./h")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g/../h")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g;x=1/./y")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g;x=1/../y")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g?y/./x")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g?y/../x")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g#s/./x")
  u => rebaseURI(base, u3)
  call check

  u3 => parseURI("g#s/../x")
  u => rebaseURI(base, u3)
  call check

  ! We are 'strictly conforming' not 'backwards
  ! compatable' for the following:
  u3 => parseURI("http:g")
  u => rebaseURI(base, u3)
  call check

  call destroyURI(base)

  contains
    subroutine check
      character(len=100) :: us
      if (associated(u))  then
        us = expressURI(u)
        print*, us
        print*
        call destroyURI(u)
        call destroyURI(u3)
      else
        print*, "parsing failed"
      endif
    end subroutine check

end program test_rfc2396
  
