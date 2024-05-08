program test

  use FoX_wcml
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call cmlBeginFile(xf, filename, unit=-1)
  call cmlStartCml(xf)
  call cmlAddBandList(xf, spin="up", values=(/1.0d0, 2.0d0, 3.0d0/), units="units:eV")
  call cmlFinishFile(xf)

end program test
