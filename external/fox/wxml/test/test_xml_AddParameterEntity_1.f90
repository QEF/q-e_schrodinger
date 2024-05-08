program test

  use FoX_wxml, only : xmlf_t, xml_OpenFile, xml_Close
  use FoX_wxml, only : xml_NewElement, xml_AddParameterEntity, xml_AddDOCTYPE
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call xml_OpenFile(filename, xf)
  call xml_AddDOCTYPE(xf, 'html')
  call xml_AddParameterEntity(xf, 'copy', "<!ENTITY copy 'COPY'>")
  call xml_NewElement(xf, 'html')
  call xml_Close(xf)

end program test
