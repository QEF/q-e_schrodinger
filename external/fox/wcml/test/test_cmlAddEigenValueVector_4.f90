program test

  use FoX_wcml
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call cmlBeginFile(xf, filename, unit=-1)
  call cmlStartCml(xf)
  call cmlStartBand(xf)
  call cmlAddEigenValueVector(xf, eigval=5.43d0, &
    eigvec=reshape((/(1.0d0,0.0d0), (2.0d0,0.0d0), (3.0d0,0.0d0)/), (/1,3/)), units="units:eV")
  call cmlEndBand(xf)
  call cmlFinishFile(xf)

end program test
