context("read-obj")
f <- system.file("extdata/greek_bust.obj.zip", package = "obj")

test_that("read_obj works", {
  obj <- read_obj(f)
  obj %>% expect_s3_class("obj") %>% expect_named(c("vertex", "texture", "normals", "face_vert", "face_texture",
                                                    "face_normal")
  )
})
