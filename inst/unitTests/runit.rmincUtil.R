##
## Test rmincIO utility functions
##
##
##
.setUp <- function() {
   dataDir <- system.file("", package="rmincIO.extras")
   dataDir <- file.path(dataDir, "packageData/")
   assign( "dataDir", dataDir, globalenv() )
   #
   volDir <- system.file("", package="rmincIO.extras")
   volDir <- file.path(volDir, "packageData/volumes/")
   assign( "volDir", volDir, globalenv() )
   #
   surfDir <- system.file("", package="rmincIO.extras")
   surfDir <- file.path(surfDir, "packageData/surfaces/")
   assign( "surfDir", surfDir, globalenv() )
}


## all files in vector are readable?
test.isReadable <- function() {
   
   # create a vector of files, known to be readable
   files <- list.files(volDir)
   files <- file.path(volDir, files)
   status <- rmincUtil.isReadable(files)
   checkTrue(status, msg="isReadable::check readable #1")
   
   # append a non-readable file
   newFilename <- file.path(dataDir, "this_file_is_not_readable.txt")
   files <- c(files, newFilename)
   status <- rmincUtil.isReadable(files)
   #checkTrue(!status, msg="isReadable::check readable #2")
   # deactivate this test, as we cannot build the package with 
   # a non-readable file.
   #checkTrue(!status, msg="isReadable::check readable #2")
}


## file is minc1 (netcdf-based)?
test.isMinc1 <- function() {
   
   volname <- "sample_minc1_volume.mnc"
   volname <- file.path(volDir, volname)
   checkTrue(rmincUtil.isMinc1(volname), msg="isMinc1::check minc1 #1")
   
   # test the negative case
   volname <- "sample_minc2_volume.mnc"
   volname <- file.path(volDir, volname)
   checkTrue(!rmincUtil.isMinc1(volname), msg="isMinc1::check not minc1 #1")
}


## file is minc2 (hdf5-based)?
test.isMinc2 <- function() {
   
   volname <- "sample_minc2_volume.mnc"
   volname <- file.path(volDir, volname)
   checkTrue(rmincUtil.isMinc2(volname), msg="isMinc2::check minc2 #1")
   
   # test the negative case
   volname <- "sample_minc1_volume.mnc"
   volname <- file.path(volDir, volname)
   checkTrue(!rmincUtil.isMinc2(volname), msg="isMinc2::check not minc2 #1")
}


## file is minc?
test.isMinc <- function() {
   
   volname <- "sample_minc1_volume.mnc"
   volname <- file.path(volDir, volname)
   checkTrue(rmincUtil.isMinc(volname), msg="isMinc::check minc #1")
   
   volname <- "sample_minc2_volume.mnc"
   volname <- file.path(volDir, volname)
   checkTrue(rmincUtil.isMinc(volname), msg="isMinc::check minc #2")
   
   # test the negative case
   volname <- "adni32_gray_surface.obj"
   volname <- file.path(surfDir, volname)
   checkTrue(!rmincUtil.isMinc(volname), msg="isMinc::check not minc")
}


## convert minc1 file to minc2
test.asMinc2 <- function() {
   
   # pass a minc2 file; should return the same filename (no change)
   volname <- "sample_minc2_volume.mnc"
   volname <- file.path(volDir, volname)
   newVolname <- rmincUtil.asMinc2(volname)
   checkEquals(volname, newVolname, msg="asMinc2::no change")
   
   # pass a minc1 file; should *convert* and return a new filename
   volname <- "sample_minc1_volume.mnc"
   volname <- file.path(volDir, volname)
   newVolname <- rmincUtil.asMinc2(volname)
   checkTrue(volname != newVolname, msg="asMinc2::converted #1")
   #
   # ensure that the returned volume is minc2
   checkTrue(rmincUtil.isMinc2(newVolname), msg="asMinc2::converted #2")

   # now, try to convert a non-minc file
   volname <- "adni32_gray_surface.obj"
   volname <- file.path(surfDir, volname)
   checkException(rmincUtil.asMinc2(volname) != "dummy", msg="asMinc2::converted #3")
}


## test world --> voxel conversion
test.convertWorldToVoxel.vectorIn <- function() {
   
   # check using isotropic voxels
   volname <- "icbm_avg_152_t1_tal_lin.mnc"
   volname <- file.path(volDir, volname)
   worldCoords <- c(0,0,0)
   voxCoords <- rmincUtil.convertWorldToVoxel(volname, worldCoords)
   tgtVoxCoord.0rel <- c(90, 126, 72)
   tgtVoxCoord.1rel <- tgtVoxCoord.0rel +1
   checkEqualsNumeric(voxCoords, tgtVoxCoord.1rel, msg="rmincUtil.convertWorldToVoxel.vectorIn::isotropic")
   
   # check using non-isotropic voxels
   volname <- "average305_PET_t1_tal_lin.mnc"
   volname <- file.path(volDir, volname)
   worldCoords <- c(0,0,0)
   voxCoords <- rmincUtil.convertWorldToVoxel(volname, worldCoords)
   tgtVoxCoord.0rel <- c(64, 73, 25)
   tgtVoxCoord.1rel <- tgtVoxCoord.0rel +1
   checkEqualsNumeric(voxCoords, tgtVoxCoord.1rel, msg="rmincUtil.convertWorldToVoxel.vectorIn::non-isotropic")

   # check using a 4D volume
   volname <- "functional_4D.mnc"
   volname <- file.path(volDir, volname)
   worldCoords <- c(-10, 54, -20)
   voxCoords <- rmincUtil.convertWorldToVoxel(volname, worldCoords)
   tgtVoxCoord.0rel <- c(40, 90, 26)
   tgtVoxCoord.1rel <- tgtVoxCoord.0rel +1
   checkEqualsNumeric(voxCoords, tgtVoxCoord.1rel, msg="rmincUtil.convertWorldToVoxel.vectorIn::4D")
}


test.convertWorldToVoxel.matrixIn <- function() {

   # check using isotropic voxels
   volname <- "icbm_avg_152_t1_tal_lin.mnc"
   volname <- file.path(volDir, volname)

   wCoords1 <- c(-60, -26, 18)       # 0-rel vox: 30,100,90
   wCoords2 <- c(60, -16, 43)        # 0-rel vox: 150,110,115
   wCoords3 <- c(-40, 9, -27)        # 0-rel vox: 50,135,45
   wCoords.m <- matrix(c(wCoords1, wCoords2, wCoords3), byrow=TRUE, ncol=3)
   voxCoords.m <- rmincUtil.convertWorldToVoxel(volname, wCoords.m)
   
   checkTrue(is.matrix(voxCoords.m), msg="convertWorldToVoxel.matrixIn::matrixInput #1")
   checkEqualsNumeric(voxCoords.m[1,], c(31, 101,  91), msg="convertWorldToVoxel.matrixIn::matrixInput #2")
   checkEqualsNumeric(voxCoords.m[2,], c(151, 111, 116), msg="convertWorldToVoxel.matrixIn::matrixInput #3")
   checkEqualsNumeric(voxCoords.m[3,], c(51, 136,  46), msg="convertWorldToVoxel.matrixIn::matrixInput #4")
}


## test voxel --> world conversion
test.convertVoxelToWorld.3dIso <- function() {
   
   # check using vector coordinate input
   volname <- "icbm_avg_152_t1_tal_lin.mnc"
   volname <- file.path(volDir, volname)
   voxelCoords.1rel <- c(91, 127, 73)
   worldCoords.v <- rmincUtil.convertVoxelToWorld(volname, voxelCoords.1rel)
   checkEqualsNumeric(worldCoords.v, c(0,0,0), msg="convertVoxelToWorld.3dIso::vectorInput")
   
   # check using matrix input
   vCoords1.0rel <- c(30,100,90)      # -60, -26, 18
   vCoords2.0rel <- c(150,110,115)    # 60, -16, 43
   vCoords3.0rel <- c(50,135,45)      # -40, 9, -27
   vCoords.0rel.m <- matrix(c(vCoords1.0rel, vCoords2.0rel, vCoords3.0rel), byrow=TRUE, ncol=3)
   vCoords.1rel.m <- vCoords.0rel.m +1
   worldCoords.m <- rmincUtil.convertVoxelToWorld(volname, vCoords.1rel.m)
   checkTrue(is.matrix(worldCoords.m), msg="convertVoxelToWorld.3dIso::matrixInput #1")
   checkEqualsNumeric(worldCoords.m[1,], c(-60, -26, 18), msg="convertVoxelToWorld.3dIso::matrixInput #2")
   checkEqualsNumeric(worldCoords.m[2,], c(60, -16, 43), msg="convertVoxelToWorld.3dIso::matrixInput #3")
   checkEqualsNumeric(worldCoords.m[3,], c(-40, 9, -27), msg="convertVoxelToWorld.3dIso::matrixInput #4")
}


test.convertVoxelToWorld.3dNonIso <- function() {
   
   # check using vector coordinate input
   volname <- "average305_PET_t1_tal_lin.mnc"
   volname <- file.path(volDir, volname)
   vCoords.0rel.v <- c(40, 90, 25)
   vCoords.1rel.v <- vCoords.0rel.v +1
   worldCoords.v <- rmincUtil.convertVoxelToWorld(volname, vCoords.1rel.v)
   tgtCoord <- c(-32.16,28.72,-0.000001810774023)
   checkEqualsNumeric(worldCoords.v, tgtCoord, msg="convertVoxelToWorld.3dNonIso::vectorInput")
   #print(round(worldCoords))

   # check using matrix input
   vCoords1.0rel <- c(30, 65, 13)      # -45.6, -14.3, -18.0
   vCoords2.0rel <- c(83,23,19)        # 25.5, -86.5, -9.0
   vCoords3.0rel <- c(64, 56, 74)      # 0.0, -29.8, 73.5
   vCoords.0rel.m <- matrix(c(vCoords1.0rel, vCoords2.0rel, vCoords3.0rel), byrow=TRUE, ncol=3)
   vCoords.1rel.m <- vCoords.0rel.m +1
   worldCoords.m <- rmincUtil.convertVoxelToWorld(volname, vCoords.1rel.m)
   checkTrue(is.matrix(worldCoords.m), msg="convertVoxelToWorld.3dNonIso::matrixInput #1")
   c0 <- c(-45.56000000, -14.28000000, -18.00000094)
   checkEqualsNumeric(worldCoords.m[1,], c0, msg="convertVoxelToWorld.3dNonIso::matrixInput #2")
   c0 <- c(25.460000000, -86.520000000,  -9.000001376)
   checkEqualsNumeric(worldCoords.m[2,], c0, msg="convertVoxelToWorld.3dNonIso::matrixInput #3")
   c0 <- c(0.00000000, -29.76000000,  73.49999464)
   checkEqualsNumeric(worldCoords.m[3,], c0, msg="convertVoxelToWorld.3dNonIso::matrixInput #4")
}


test.convertVoxelToWorld.4d <- function() {
   #
   # Note: can check coords with the 3d vol "icbm_avg_152_t1_tal_lin_2x2x2.mnc"
   #       since they are in the same space
   #
   
   # check using 4 coordinates as input (not allowed)
   volname <- "functional_4D.mnc"
   volname <- file.path(volDir, volname)
   voxelCoords <- c(1, 1, 1, 1)
   checkException(rmincUtil.convertVoxelToWorld(volname, voxelCoords), msg="convertVoxelToWorld.4d::check nCoords")

   # check using vector coordinate input
   volname <- "functional_4D.mnc"
   volname <- file.path(volDir, volname)
   vCoords.0rel.v <- c(40, 90, 26)
   vCoords.1rel.v <- vCoords.0rel.v +1
   worldCoords.v <- rmincUtil.convertVoxelToWorld(volname, vCoords.1rel.v)
   checkEqualsNumeric(worldCoords.v, c(-10, 54, -20), msg="convertVoxelToWorld.4d::vectorInput")

   # check using matrix input
   vCoords1.0rel <- c(24, 60,25)          # -42, -6, -22
   vCoords2.0rel <- c(45, 42, 10)         # 0, -42, -52
   vCoords3.0rel <- c(70, 75, 60)         # 50, 24, 48
   vCoords.0rel.m <- matrix(c(vCoords1.0rel, vCoords2.0rel, vCoords3.0rel), byrow=TRUE, ncol=3)
   vCoords.1rel.m <- vCoords.0rel.m +1
   worldCoords.m <- rmincUtil.convertVoxelToWorld(volname, vCoords.1rel.m)
   checkTrue(is.matrix(worldCoords.m), msg="convertVoxelToWorld.4d::matrixInput #1")
   checkEqualsNumeric(worldCoords.m[1,], c(-42, -6, -22), msg="convertVoxelToWorld.4d::matrixInput #2")
   checkEqualsNumeric(worldCoords.m[2,], c(0, -42, -52), msg="convertVoxelToWorld.4d::matrixInput #3")
   checkEqualsNumeric(worldCoords.m[3,], c(50, 24, 48), msg="convertVoxelToWorld.4d::matrixInput #4")
}


test.convertNativeSpaceVolumeCoords <- function() {
   #
   # Note: check coordinate conversion using a unconventionally ordered
   #       volume
   #
   
   # convert voxel --> world
   volname <- "xzy_dim_ordering.mnc"
   volname <- file.path(volDir, volname)
   vCoords.0rel.v <- c(30, 110, 130)         # -63.3, 47.0, -25.2
   vCoords.1rel.v <- vCoords.0rel.v +1
   worldCoords.v <- rmincUtil.convertVoxelToWorld(volname, vCoords.1rel.v)
   tgtCoords <- c(-63.31113815,  47.04600525, -25.24455261)
   checkEqualsNumeric(worldCoords.v, tgtCoords, msg="convertNativeSpaceVolumeCoords::v2w")
   
   # convert world --> voxel
   volname <- "xzy_dim_ordering.mnc"
   volname <- file.path(volDir, volname)
   worldCoords.v <- c(-63.31113815,  47.04600525, -25.24455261)   
   voxelCoords.v <- rmincUtil.convertWorldToVoxel(volname, worldCoords.v)
   tgtCoords.v <- c(30, 110, 130) +1
   checkEqualsNumeric(worldCoords.v, tgtCoords, msg="convertNativeSpaceVolumeCoords::w2v")
}


## test for existence of an external program/script
test.checkForExternalProgram <- function() {
   
   # check for an existing program
   program <- "mincinfo"
   progOptions <- "-version"
   test_string <- "program"
   result <- rmincUtil.checkForExternalProgram(program, test_string, progOptions, run_it=TRUE)
   checkTrue(result, msg="checkForExternalProgram::program exists")
   
   # check for an non-existing program
   program <- "mincinfo_nonexisting"
   progOptions <- "-version"
   test_string <- "program"
   result <- rmincUtil.checkForExternalProgram(program, test_string, progOptions, run_it=TRUE)
   checkTrue(!result, msg="checkForExternalProgram::program does not exist -- #1")
   
   # check for an non-existing program
   program <- "mincinfo_nonexisting"
   progOptions <- "-version"
   test_string <- "program"
   result <- rmincUtil.checkForExternalProgram(program, test_string, progOptions, run_it=FALSE)
   checkTrue(!result, msg="checkForExternalProgram::program does not exist -- #2")
}


## test read of linear xfm file
test.readLinearXfmFile <- function() {
   
   # check for an non-existing program
   filename <- "sample_transformation_file.xfm"
   filename_fullpath <- file.path(dataDir, filename)
   xfm.df <- rmincUtil.readLinearXfmFile(filename_fullpath)
   checkEquals(class(xfm.df), "data.frame", msg="readLinearXfmFile::check returned class type")

   # now let's just test a smattering of things returned in the data.frame
   checkEqualsNumeric(as.numeric(xfm.df["translation",]), 
                        c(-7.16315, -38.76522, -6.69634), msg="readLinearXfmFile::check translation")
   checkEqualsNumeric(as.numeric(xfm.df["rotation",]), 
                        c(-16.71111, -3.36955, 3.90984), msg="readLinearXfmFile::check rotation")
   checkEqualsNumeric(as.numeric(xfm.df["scale",]), 
                        c(1.02002, 1.10970, 1.24706), msg="readLinearXfmFile::check scale")
   checkEqualsNumeric(as.numeric(xfm.df["shear",]), 
                        c(-0.04806, -0.03287, -0.04716), msg="readLinearXfmFile::check shear")
}


## binary .obj file tests
test.isMniObjBinary <- function() {
   
   # positive test
   filename <- "adni32_gray_surface_Binary.obj"
   filename_fullpath <- file.path(surfDir, filename)
   checkTrue(rmincUtil.isMniObjBinary(filename_fullpath), msg="isMniObjBinary::positive test")
   
   # negative test
   filename <- "adni32_gray_surface.obj"
   filename_fullpath <- file.path(surfDir, filename)
   checkTrue(!rmincUtil.isMniObjBinary(filename_fullpath), msg="isMniObjBinary::negative test")
}


## ascii .obj file tests
test.isMniObjAscii <- function() {
   
   # positive test
   filename <- "adni32_gray_surface.obj"
   filename_fullpath <- file.path(surfDir, filename)
   checkTrue(rmincUtil.isMniObjAscii(filename_fullpath), msg="isMniObjAscii::positive test")
   
   # negative test
   filename <- "adni32_gray_surface_Binary.obj"
   filename_fullpath <- file.path(surfDir, filename)
   checkTrue(!rmincUtil.isMniObjAscii(filename_fullpath), msg="isMniObjAscii::negative test")
}


## test for a .obj file (of any kind)
test.isMniObj <- function() {
   
   # binary .obj test
   filename <- "adni32_gray_surface_Binary.obj"
   filename_fullpath <- file.path(surfDir, filename)
   checkTrue(rmincUtil.isMniObj(filename_fullpath), msg="isMniObj::positive test -- binary")
   
   # ascii .obj test
   filename <- "adni32_gray_surface.obj"
   filename_fullpath <- file.path(surfDir, filename)
   checkTrue(rmincUtil.isMniObj(filename_fullpath), msg="isMniObj::positive test -- ascii")
   
   # negative test
   filename <- "sample_minc1_volume.mnc"
   filename_fullpath <- file.path(volDir, filename)
   checkTrue(!rmincUtil.isMniObj(filename_fullpath), msg="isMniObj::negative test -- minc volume")
}


## convert a binary .obj to ascii
test.asMniObjAscii <- function() {
   
   # positive test
   filename <- "adni32_gray_surface_Binary.obj"
   binary_filename_fullpath <- file.path(surfDir, filename)
   ascii_filename_fullpath <- rmincUtil.asMniObjAscii(binary_filename_fullpath, keepName=TRUE)
   # got a filename?
   checkTrue(length(ascii_filename_fullpath) > 0, msg="asMniObjAscii::ascii conversion -- #1")
   # ... and it's an ascii .obj?
   checkTrue(rmincUtil.isMniObjAscii(ascii_filename_fullpath), msg="asMniObjAscii::ascii conversion -- #2")
   # ... and the base filename is unchanged? (required by "keepName=TRUE") 
   checkEquals(basename(ascii_filename_fullpath), filename, msg="asMniObjAscii::ascii conversion -- #3")
}








