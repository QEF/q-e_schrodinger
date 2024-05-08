program test

  use FoX_wxml, only : xmlf_t
  use FoX_wxml, only : xml_AddPseudoAttribute
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call xml_AddPseudoAttribute(xf, "att", "value")

end program test
