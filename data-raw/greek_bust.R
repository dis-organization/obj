## Greek bust
u <- "https://raw.githubusercontent.com/Cecropia/thehallaframe/master/models/greek_bust.obj"
#dir.create("inst/extdata", recursive = TRUE)
download.file(u, file.path("inst/extdata", basename(u)), mode  = "wb")
zip("inst/extdata/greek_bust.obj.zip", file.path("inst/extdata", basename(u)))
unlink(file.path("inst/extdata", basename(u)))
