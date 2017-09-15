.onLoad <- function(libname, pkgname) {
  use_rgl_null <- isTRUE(!capabilities()[["X11"]])

  if (use_rgl_null) Sys.setenv(RGL_USE_NULL=TRUE)
  invisible()
}

.onAttach <- function(libname, pkgname) {
  use_rgl_null <- isTRUE(!capabilities()[["X11"]])

  if (use_rgl_null) Sys.setenv(RGL_USE_NULL=TRUE)
  invisible()
}
