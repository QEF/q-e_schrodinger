program test

  use FoX_wkml

  implicit none

  type(xmlf_t) :: myfile

  real :: coords(2, 2)

  coords(1,:) = (/1.0, 2.0/)
  coords(2,:) = (/1.0, 2.0/)
  call kmlBeginFile(myfile, "test.xml", -1)
  call kmlCreatePoints(myfile, coords, (/1000.0, 2000.0/))
  call kmlFinishFile(myfile)

end program test
