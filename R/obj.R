plot.obj <- function(x, ..., homog = 1) {
  tri <- structure(list(primitivetype = "triangle", material = NULL, normals = NULL, texcoords = NULL), class = c("mesh3d", "shape3d"))
  tri$vb <- t(cbind(as.matrix(x$vertex[, c("X", "Y", "Z")]), homog))
  tri$it <- t(as.matrix(x$face_vert[, c(".vertex0", ".vertex1", ".vertex2")]))
  ## colours with normals is eluding me ...
  tri$normals <- cbind(t(cbind(as.matrix(x$normals[, c("nX", "nY", "nZ")]), 1)), 1)
  tri$material <- list(color = rep("firebrick", ncol(tri$vb)))
  rgl::shade3d(tri, ...)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#' @importFrom dplyr %>%
#' @examples
#' f <- system.file("extdata/greek_bust.obj.zip", package = "obj")
#' obj <- read_obj(f)
read_obj <- function(x, ...) UseMethod("read_obj")
#' @export
read_obj.character <- function(x, ...) {
  raw_text <- readr::read_lines(x)
  vert_text <- paste0(grep("^v ", raw_text, value = TRUE), collapse = "\n")
  vertex <- readr::read_delim(vert_text, delim = " ",
                         col_names = c("v", "X", "Y", "Z"))
  texture_text <- paste0(grep("^vt ", raw_text, value = TRUE), collapse = "\n")
  texture <- readr::read_delim(texture_text, delim = " ",
                                col_names = c("vt", "tX", "tY"))

  norm_text <- paste0(grep("^vn ", raw_text, value = TRUE), collapse = "\n")
  normals <- readr::read_delim(norm_text, delim = " ",
                              col_names = c("vn", "nX", "nY", "nZ"))

  face_text <- grep("^f ", raw_text, value = TRUE)
  face_text_length <- purrr::map_int(strsplit(face_text, " "), length)
  tri_faces <- readr::read_delim(paste0(face_text[face_text_length == 4L], collapse = "\n"), delim = " ",
                                 col_names = c("f", ".vdata0", ".vdata1", ".vdata2"))

  face_vert <- tibble(.vertex0 = unlist(lapply(strsplit(tri_faces$.vdata0, "/"), "[[", 1)),
                      .vertex1 = unlist(lapply(strsplit(tri_faces$.vdata1, "/"), "[[", 1)),
                      .vertex2 = unlist(lapply(strsplit(tri_faces$.vdata2, "/"), "[[", 1))) %>%
    dplyr::mutate_all(as.integer)

  face_texture <- tibble(.vertex0 = unlist(lapply(strsplit(tri_faces$.vdata0, "/"), "[[", 2)),
                      .vertex1 = unlist(lapply(strsplit(tri_faces$.vdata1, "/"), "[[", 2)),
                      .vertex2 = unlist(lapply(strsplit(tri_faces$.vdata2, "/"), "[[", 2))) %>%
    dplyr::mutate_all(as.integer)

  face_normal <- tibble(.vertex0 = unlist(lapply(strsplit(tri_faces$.vdata0, "/"), "[[", 3)),
                         .vertex1 = unlist(lapply(strsplit(tri_faces$.vdata1, "/"), "[[", 3)),
                         .vertex2 = unlist(lapply(strsplit(tri_faces$.vdata2, "/"), "[[", 3))) %>%
    dplyr::mutate_all(as.integer)

  structure(list(vertex = vertex, texture = texture, normals = normals, face_vert = face_vert, face_texture = face_texture, face_normal = face_normal),
            class = c("obj", "list"))
}
