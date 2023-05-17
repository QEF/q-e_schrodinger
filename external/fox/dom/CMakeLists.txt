IF(NOT ${M4_PROGRAM} MATCHES "NOTFOUND")
  # m_dom_dom.F90: $(M4FILES)
  #         m4 -I ../m4 m_dom_dom.m4 > $@ 
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/m_dom_dom.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_dom_dom.m4 > ${CMAKE_CURRENT_BINARY_DIR}/m_dom_dom.F90
    DEPENDS m_dom_dom.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    VERBATIM
  )

  # m_dom_extras.F90: m_dom_extras.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/m_dom_extras.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_dom_extras.m4 > ${CMAKE_CURRENT_BINARY_DIR}/m_dom_extras.F90
    DEPENDS m_dom_extras.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    VERBATIM
  )
  
  # m_dom_parse.f90: m_dom_parse.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/m_dom_parse.f90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_dom_parse.m4 > ${CMAKE_CURRENT_BINARY_DIR}/m_dom_parse.f90
    DEPENDS m_dom_parse.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    VERBATIM
  )
  
  # m_dom_utils.f90: m_dom_utils.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/m_dom_utils.f90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_dom_utils.m4 > ${CMAKE_CURRENT_BINARY_DIR}/m_dom_utils.f90
    DEPENDS m_dom_utils.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    VERBATIM
  )
ENDIF(NOT ${M4_PROGRAM} MATCHES "NOTFOUND")


set(fox_dom_srcs_pp
  ${CMAKE_CURRENT_BINARY_DIR}/m_dom_dom.F90
  ${CMAKE_CURRENT_BINARY_DIR}/m_dom_extras.F90
)
  
set(fox_dom_srcs
  FoX_dom.f90
  m_dom_error.f90
  ${CMAKE_CURRENT_BINARY_DIR}/m_dom_parse.f90
  ${CMAKE_CURRENT_BINARY_DIR}/m_dom_utils.f90
)

add_library(
  ${PROJECT_NAME}_dom
  ${fox_dom_srcs}
  ${fox_dom_srcs_pp}
)

## add_dependencies(
##   fox_dom
##   dom_generate
## )

target_link_libraries(
  ${PROJECT_NAME}_dom
  ${PROJECT_NAME}_wxml
  ${PROJECT_NAME}_sax
)