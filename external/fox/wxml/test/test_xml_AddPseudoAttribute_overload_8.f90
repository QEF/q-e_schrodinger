program test

  use FoX_wxml
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call xml_OpenFile(filename, xf)
  call xml_AddXMLPI(xf, "root")
  call xml_AddPseudoAttribute(xf, "att", (/(1.0,-2.3),(1.0,-2.3)/))
  call xml_NewElement(xf, "a")
  call xml_Close(xf)

end program test
