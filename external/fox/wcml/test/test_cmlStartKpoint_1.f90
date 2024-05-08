program test

  use FoX_wcml
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call cmlBeginFile(xf, filename, unit=-1)
  call cmlStartCml(xf)
  call cmlStartKpoint(xf, coords=(/1.0, 2.0, 3.0/))
  call cmlFinishFile(xf)

end program test
