##
## Test methods for the surfaceIO class
##
##
##
.setUp <- function() {
   surfDir <- system.file("", package="rmincIO.extras")
   surfDir <- file.path(surfDir, "packageData/surfaces/")
   assign( "surfDir", surfDir, globalenv() )
}

## read a volume and check that all fields are correctly initialized
test.readSurface <- function() {
   
   surfFilename <- "adni32_gray_surface.obj"
   surfFilename_fullpath <- file.path(surfDir, surfFilename)
   surface <- readSurface(surfFilename_fullpath)
   
   checkTrue(class(surface) == "SurfaceIO", msg="readSurface::returned object is of SurfaceIO class")
   #
   checkTrue(surface@fileType == "P",  msg="readSurface::check fileType")
   checkEquals(dim(surface@.Data), c(81924L, 3L), msg="readSurface::check data dimensionality")
   checkEquals(surface@surfaceProperties, c(0.3, 0.3, 0.4, 10, 1), msg="readSurface::check surfaceProperties")
   checkEquals(surface@nVertices, 81924, msg="readSurface::check nVertices")
   checkEquals(surface@nTriangles, 163840, msg="readSurface::check nTriangles")
}
