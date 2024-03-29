configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg
  )

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.site.cfg.in
  ${CMAKE_CURRENT_BINARY_DIR}/Unit/lit.site.cfg
  )

if("${CMAKE_HOST_SYSTEM}" STREQUAL "${CMAKE_SYSTEM}")
  # Run TSan output tests only if we're not cross-compiling,
  # and can be sure that clang would produce working binaries.
  set(TSAN_TEST_DEPS
    clang clang-headers FileCheck count not
    ${TSAN_RUNTIME_LIBRARIES}
    )
  set(TSAN_TEST_PARAMS
    tsan_site_config=${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg
    )
  if(LLVM_INCLUDE_TESTS)
    list(APPEND TSAN_TEST_DEPS TsanUnitTests)
  endif()
  add_lit_testsuite(check-tsan "Running ThreadSanitizer tests"
    ${CMAKE_CURRENT_BINARY_DIR}
    PARAMS ${TSAN_TEST_PARAMS}
    DEPENDS ${TSAN_TEST_DEPS}
    )
  set_target_properties(check-tsan PROPERTIES FOLDER "TSan unittests")
elseif(LLVM_INCLUDE_TESTS)
  # Otherwise run only TSan unit tests.
  add_lit_testsuite(check-tsan "Running ThreadSanitizer tests"
    ${CMAKE_CURRENT_BINARY_DIR}/Unit
    DEPENDS TsanUnitTests)
  set_target_properties(check-tsan PROPERTIES FOLDER "TSan unittests")
endif()
