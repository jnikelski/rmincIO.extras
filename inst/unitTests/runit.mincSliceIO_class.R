##
## Test methods for the mincSliceIO class
##
##
##
.setUp <- function() {
   volDir <- system.file("", package="rmincIO.extras")
   volDir <- file.path(volDir, "packageData/volumes/")
   assign( "volDir", volDir, globalenv() )
}


## read a volume and check that all fields are correctly initialized
test.readbySlice.multiFrames.01 <- function() {
   
   # set the test volume name
   volname <- "functional_4D.mnc"
   volname_fullpath <- file.path(volDir, volname)
   
   # read slice 37 from all frames into a slice array
   slice_array <- mincIO.readBySlice(volname_fullpath, 37, volumeType="functional", colorMap="rainbow")
   checkEqualsNumeric(class(slice_array), "MincSliceIO", msg="mincIO.readBySlice::returned object is of MincSliceIO class")


   # test the embedded MincInfo properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice_array, "nDimensions"), 4L, msg = "mincIO.getProperty::get nDimensions" )
   checkEquals(mincIO.getProperty(slice_array, "nFrames"), 34L, msg = "mincIO.getProperty::get nFrames" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "sizes"), c(91L, 109L, 91L, 34L), msg = "mincIO.getProperty::get dim sizes" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "sizes")[3], 91L, msg = "mincIO.getProperty::get a dim size -- zspace" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "sizes")[4], 34L, msg = "mincIO.getProperty::get a dim size -- time" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "sizes")["zspace"], 91L, msg = "mincIO.getProperty::get zspace dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "sizes")["time"], 34L, msg = "mincIO.getProperty::get time dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "starts"), c(-90.000, -126.000, -72.000, 0.016), msg = "mincIO.getProperty::get dim starts" )

   # test dynamic properties, using the accessor functions
   timeWidths <- c(15, 15, 15, 15, 30, 30, 30, 30, 30, 30)
   checkEquals(mincIO.getProperty(slice_array, "timeWidths")[1:10], timeWidths, msg = "mincIO.getProperty::get timeWidths" )
   timeOffsets <- c(0.016, 15.016, 30.016, 45.016, 60.016, 90.016, 120.016, 150.016, 180.016, 210.016)
   checkEquals(mincIO.getProperty(slice_array, "timeOffsets")[1:10], timeOffsets, msg = "mincIO.getProperty::get timeOffsets" )

   # test the other MincSliceIO properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice_array, "nSlices"), 34L, msg = "mincIO.getProperty::nSlices" )
   checkEquals(mincIO.getProperty(slice_array, "sliceNumber"), 37, msg = "mincIO.getProperty::sliceNumber" )
   checkEquals(mincIO.getProperty(slice_array, "orientation"), "zSlice", msg = "mincIO.getProperty::orientation" )
   checkEquals(mincIO.getProperty(slice_array, "volumeType"), "functional", msg = "mincIO.getProperty::volumeType" )
   checkEquals(mincIO.getProperty(slice_array, "colorMap"), "rainbow", msg = "mincIO.getProperty::colorMap" )
   checkEquals(mincIO.getProperty(slice_array, "sliceOrigin"), "Slices over frames", msg = "mincIO.getProperty::sliceOrigin" )
}


## read a volume and check that all fields are correctly initialized
test.readbySlice.multiFrames.02 <- function() {
   
   # set the test volume name
   volname <- "functional_4D.mnc"
   volname_fullpath <- file.path(volDir, volname)
   
   # read slice 37 from all frames into a slice array
   slice_array <- mincIO.readBySlice(volname_fullpath, 37, volumeType="functional", colorMap="rainbow")
   checkEqualsNumeric(class(slice_array), "MincSliceIO", msg="mincIO.readBySlice::returned object is of MincSliceIO class")

   # extract a specific slice from the slice array, yielding a MincSlice object
   frame10s37 <- mincIO.getSliceFromSliceArray(slice_array, 10)
   checkEqualsNumeric(class(frame10s37), "MincSlice", msg="mincIO.getSliceFromSliceArray::returned object is of MincSlice class")

   # test the other MincSliceIO properties, using the accessor functions
   checkEquals(mincIO.getProperty(frame10s37, "sliceNumber"), 37, msg = "mincIO.getProperty::sliceNumber" )
   checkEquals(mincIO.getProperty(frame10s37, "orientation"), "zSlice", msg = "mincIO.getProperty::orientation" )
   checkEquals(mincIO.getProperty(frame10s37, "volumeType"), "functional", msg = "mincIO.getProperty::volumeType" )
   checkEquals(mincIO.getProperty(frame10s37, "colorMap"), "rainbow", msg = "mincIO.getProperty::colorMap" )
   checkEquals(mincIO.getProperty(frame10s37, "aspectRatio"), 1.197802198, msg = "mincIO.getProperty::aspectRatio" )
   sliceIntensityRange <- c(-135.0972369, 896.8578773)
   checkEquals(mincIO.getProperty(frame10s37, "sliceIntensityRange"), sliceIntensityRange, 
                                                msg = "mincIO.getProperty::sliceIntensityRange" )
}


## read a volume and check that all fields are correctly initialized
test.readbySlice.multiVolumes.01 <- function() {
   
   # set the test volume names
   # ... we're using a single low res structural volume with differtial blurring
   volnames <- c("icbm_avg_152_t1_tal_lin_4x4x4_blur00mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur10mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur20mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur30mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur40mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur50mm.mnc")
   volnames_fullpath <- file.path(volDir, volnames)
   
   # read slice 20 from all volumes into a slice array
   slice_array <- mincIO.readBySlice(volnames_fullpath, 20, volumeType="anatomical", colorMap="gray")
   checkEqualsNumeric(class(slice_array), "MincSliceIO", msg="mincIO.readBySlice::returned object is of MincSliceIO class")


   # test the embedded MincInfo properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice_array, "nDimensions"), 3L, msg = "mincIO.getProperty::get nDimensions" )
   checkEquals(mincIO.getProperty(slice_array, "nFrames"), 0L, msg = "mincIO.getProperty::get nFrames" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "sizes"), c(46L, 55L, 46L), msg = "mincIO.getProperty::get dim sizes" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "sizes")[3], 46L, msg = "mincIO.getProperty::get a dim size -- zspace" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "sizes")["zspace"], 46L, msg = "mincIO.getProperty::get zspace dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice_array, "starts"), c(-90, -126, -72), msg = "mincIO.getProperty::get dim starts" )

   # test the other MincSliceIO properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice_array, "nSlices"), 6L, msg = "mincIO.getProperty::nSlices" )
   checkEquals(mincIO.getProperty(slice_array, "sliceNumber"), 20, msg = "mincIO.getProperty::sliceNumber" )
   checkEquals(mincIO.getProperty(slice_array, "orientation"), "zSlice", msg = "mincIO.getProperty::orientation" )
   checkEquals(mincIO.getProperty(slice_array, "volumeType"), "anatomical", msg = "mincIO.getProperty::volumeType" )
   checkEquals(mincIO.getProperty(slice_array, "colorMap"), "gray", msg = "mincIO.getProperty::colorMap" )
   checkEquals(mincIO.getProperty(slice_array, "sliceOrigin"), "Slices over volumes", msg = "mincIO.getProperty::sliceOrigin" )
}


## read a volume and check that all fields are correctly initialized
test.readbySlice.multiVolumes.02 <- function() {
   
   # set the test volume names
   # ... we're using a single low res structural volume with differtial blurring
   volnames <- c("icbm_avg_152_t1_tal_lin_4x4x4_blur00mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur10mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur20mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur30mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur40mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur50mm.mnc")
   volnames_fullpath <- file.path(volDir, volnames)
   
   # read slice 20 from all volumes into a slice array
   slice_array <- mincIO.readBySlice(volnames_fullpath, 20, volumeType="anatomical", colorMap="gray")
   checkEqualsNumeric(class(slice_array), "MincSliceIO", msg="mincIO.readBySlice::returned object is of MincSliceIO class")

   # extract a specific slice from the slice array, yielding a MincSlice object
   vol03s20 <- mincIO.getSliceFromSliceArray(slice_array, 3)
   checkEqualsNumeric(class(vol03s20), "MincSlice", msg="mincIO.getSliceFromSliceArray::returned object is of MincSlice class")

   # test the other MincSliceIO properties, using the accessor functions
   checkEquals(mincIO.getProperty(vol03s20, "sliceNumber"), 20, msg = "mincIO.getProperty::sliceNumber" )
   checkEquals(mincIO.getProperty(vol03s20, "orientation"), "zSlice", msg = "mincIO.getProperty::orientation" )
   checkEquals(mincIO.getProperty(vol03s20, "volumeType"), "anatomical", msg = "mincIO.getProperty::volumeType" )
   checkEquals(mincIO.getProperty(vol03s20, "colorMap"), "gray", msg = "mincIO.getProperty::colorMap" )
   checkEquals(mincIO.getProperty(vol03s20, "aspectRatio"), 1.195652174, msg = "mincIO.getProperty::aspectRatio" )
   sliceIntensityRange <- c(4759.026, 316274.311)
   checkEquals(mincIO.getProperty(vol03s20, "sliceIntensityRange"), sliceIntensityRange, 
                                                msg = "mincIO.getProperty::sliceIntensityRange" )
}








