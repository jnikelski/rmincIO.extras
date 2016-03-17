##
## Test methods for the mincInfo class
##
##
##
.setUp <- function() {
   volDir <- system.file("", package="rmincIO.extras")
   volDir <- file.path(volDir, "packageData/volumes/")
   assign( "volDir", volDir, globalenv() )
}

## read a volume and check that all fields are correctly initialized
test.readMincInfo <- function() {
   
   #cat(sprintf("The test volumes are located in the %s directory\n", volDir))
   volName <- "icbm_avg_152_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   volInfo <- mincIO.readMincInfo(volName_fullpath)
   #checkEquals(class(volInfo), "MincInfo", msg="readMincInfo::returned object is of MincInfo class")
   checkTrue(class(volInfo) == "MincInfo", msg="readMincInfo::returned object is of MincInfo class")
   #
   # check the objects properties
   checkEquals(volInfo@volumeDataClass, 0L, msg = "readMincInfo::get volume data class" )
   checkEquals(volInfo@volumeDataType, 3L, msg = "readMincInfo::get volume data type" )
   checkEquals(volInfo@nDimensions, 3L, msg = "readMincInfo::get nDimensions" )
   checkEquals(volInfo@nFrames, 0L, msg = "readMincInfo::get nFrames" )
   checkEquals(volInfo@timeWidths, 0, msg = "readMincInfo::get timeWidths" )
   checkEquals(volInfo@timeOffsets, 0, msg = "readMincInfo::get timeOffsets" )
   checkEquals(volInfo@volumeIntensityRange, c(0, 0), msg = "readMincInfo::get volume intensity range" )
   #
   checkEquals(row.names(volInfo@dimInfo), c("zspace", "yspace", "xspace"), msg = "readMincInfo::get dim names" )
   checkEquals(volInfo@dimInfo["sizes"][,1], c(181L, 217L, 181L), msg = "readMincInfo::get dim sizes" )
   checkEquals(volInfo@dimInfo["steps"][,1], c(1,1,1), msg = "readMincInfo::get dim steps" )
   checkEquals(volInfo@dimInfo["starts"][,1], c(-72, -126, -90), msg = "readMincInfo::get dim starts" )
   
   # now, check the fields again, using the accessor functions
   checkEquals(mincIO.getProperty(volInfo, "nDimensions"), 3L, msg = "readMincInfo::get nDimensions" )
   checkEquals(mincIO.getProperty(volInfo, "nFrames"), 0L, msg = "readMincInfo::get nFrames" )
   checkEqualsNumeric(mincIO.getProperty(volInfo, "sizes"), c(181L, 217L, 181L), msg = "readMincInfo::get dim sizes" )
   checkEqualsNumeric(mincIO.getProperty(volInfo, "sizes")[3], 181L, msg = "readMincInfo::get a dim size" )
   checkEqualsNumeric(mincIO.getProperty(volInfo, "sizes")["zspace"], 181L, msg = "readMincInfo::get zspace dim size" )
   checkEqualsNumeric(mincIO.getProperty(volInfo, "starts"), c(-90, -126, -72), msg = "readMincInfo::get dim starts" )

   # ... and now test the setters
   mincIO.setProperty(volInfo, "filename", "My_test_volname.mnc")
   checkTrue(mincIO.getProperty(volInfo, "filename") == "My_test_volname.mnc", msg = "readMincInfo::set filename" )
   mincIO.setProperty(volInfo, "volumeIntensityRange", c(123, 789))
   checkEquals(mincIO.getProperty(volInfo, "volumeIntensityRange"), c(123, 789), msg = "readMincInfo::set volumeIntensityRange" )

}





