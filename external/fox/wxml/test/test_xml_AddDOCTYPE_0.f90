program test

  use FoX_wxml, only : xmlf_t, xml_OpenFile, xml_Close
  use FoX_wxml, only : xml_NewElement, xml_AddDOCTYPE
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call xml_AddDOCTYPE(xf, 'html', public='-//W3C//DTD XHTML 1.0 Strict//EN', &
                      system='http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd')
  call xml_OpenFile(filename, xf)
  call xml_NewElement(xf, 'html')
  call xml_Close(xf)

end program test
