program test

  use FoX_wcml
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call cmlBeginFile(xf, filename, unit=-1)
  call cmlStartCml(xf)
  call cmlAddProperty(xf, title="name", value=reshape((/2.0d0, 3.0d0, -4.0d0, -10.0d0/),(/2,2/)), fmt="s3", units="siUnits:m")

  call cmlFinishFile(xf)

end program test
