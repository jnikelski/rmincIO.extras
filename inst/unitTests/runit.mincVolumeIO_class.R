##
## Test methods for the mincVolumeIO class
##
##
##
.setUp <- function() {
   volDir <- system.file("", package="rmincIO.extras")
   volDir <- file.path(volDir, "packageData/volumes/")
   assign( "volDir", volDir, globalenv() )
}

## read a volume and check that all fields are correctly initialized
test.readVolume.byMincInfo <- function() {
   
   # set the test volume name
   volName <- "icbm_avg_152_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a MincInfo object
   volInfo <- mincIO.readMincInfo(volName_fullpath)
   vol <- mincIO.readVolume(volInfo)
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")
}


test.readVolume.byVolname <- function() {
   
   # set the test volume name
   volName <- "icbm_avg_152_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath)
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")
}


test.readVolume.byVolname.checkVoxelValue <- function() {
   
   # set the test volume name
   volName <- "icbm_avg_152_t1_tal_lin_4x4x4.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath)
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   # check a specific voxel value
   voxValue <- vol[28, 42, 19]
   # Note: target value returned by ...
   #       "mincextract -ascii -start 18,41,27 -count 1,1,1 icbm_avg_152_t1_tal_lin_4x4x4.mnc"
   checkEqualsNumeric(voxValue, 345615.53369889612077, msg="mincIO.readVolume::check a specific voxel value")
}


test.readVolume.castToMincIOVolume <- function() {
   
   # set the test volume name
   volName <- "icbm_avg_152_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath)
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")
   
   # create a mask
   mask3D <- vol[,,]>100000                  # logical volume identifying all voxels with value > 100,000
   checkTrue(is.array(mask3D), msg="creating a logical mask volume") # should be of type "array"
   checkEquals(sum(mask3D), 3393746L, msg="3,393,746 voxels expected in logical array")

   # apply the mask element-wise to the volume
   maskedImage <- vol * mask3D
   checkTrue(is.array(maskedImage), msg="applying a logical mask -- check for array out")
   checkTrue(is.numeric(maskedImage), msg="applying a logical mask -- check for numeric out")
   checkTrue(is.double(maskedImage), msg="applying a logical mask -- check for double out")

   # recast image data into a MincVolumeIO object
   vol2 <- mincIO.asVolume(maskedImage, vol)
   checkEqualsNumeric(class(vol2), "MincVolumeIO", msg="recast array as MincVolumeIO object #1")

   # combining previous 2 lines into 1
   vol3 <- mincIO.asVolume(vol * mask3D, vol)
   checkEqualsNumeric(class(vol3), "MincVolumeIO", msg="recast array as MincVolumeIO object #2")

   # vol2 and vol3 should be identical
   checkEquals(range(vol2), range(vol3), msg="compare vol2/3 -- range")
   checkEquals(sum(vol2), sum(vol3), msg="compare vol2/3 -- sum")
   checkEquals(mean(vol2), mean(vol3), msg="compare vol2/3 -- mean")
}


test.readVolume.basicOperations <- function() {
   
   # set the test volume name
   volName <- "icbm_avg_152_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath)
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   vol_adj <- vol - mean(vol)                # mean center the volume
   checkEquals(mean(vol_adj), 0.0, msg="subtract constant from volume -- check mean")
   #checkEquals(range(vol_adj), c(-120904.9, 318634.5), msg="subtract constant from volume -- check range")

   # set a voxel within the caudate (slice 82) to high intensity
   nVoxels_before <- sum(vol[,,82] > 300000)       # get nVoxels w/ values > 300000
   vol[85, 142, 82] <- 300001                      # set 2 more values above the threshold
   vol[95, 142, 82] <- 300001                      # (left and right caudate)
   nVoxels_after <- sum(vol[,,82] > 300000)        # 
   checkEquals(nVoxels_after, nVoxels_before +2L, msg="updating image data via indexing #1")

   # draw a line through the caudate (slice 82)
   nVoxels_before <- sum(vol[,,82] > 500000)       # get nVoxels w/ values > 500000
   vol[20:160, 142, 82] <- 500001
   nVoxels_after <- sum(vol[,,82] > 500000)        # get updated
   checkEquals(nVoxels_after, nVoxels_before +141L, msg="updating image data via indexing #2")
}


test.readVolume.getProperties <- function() {
   
   # set the test volume name
   volName <- "icbm_avg_152_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath)
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   # test the embedded MincInfo properties, using the accessor functions
   checkEquals(mincIO.getProperty(vol, "nDimensions"), 3L, msg = "mincIO.getProperty::get nDimensions" )
   checkEquals(mincIO.getProperty(vol, "nFrames"), 0L, msg = "mincIO.getProperty::get nFrames" )
   checkEqualsNumeric(mincIO.getProperty(vol, "sizes"), c(181L, 217L, 181L), msg = "mincIO.getProperty::get dim sizes" )
   checkEqualsNumeric(mincIO.getProperty(vol, "sizes")[3], 181L, msg = "mincIO.getProperty::get a dim size" )
   checkEqualsNumeric(mincIO.getProperty(vol, "sizes")["zspace"], 181L, msg = "mincIO.getProperty::get zspace dim size" )
   checkEqualsNumeric(mincIO.getProperty(vol, "starts"), c(-90, -126, -72), msg = "mincIO.getProperty::get dim starts" )

   # test the other MincVolumeIO properties, using the accessor functions
   checkEquals(mincIO.getProperty(vol, "volumeIntensityRange"), c(236.8016, 439776.2277), msg = "mincIO.getProperty::volumeIntensityRange" )
   checkEquals(mincIO.getProperty(vol, "frameNumber"), c(0), msg = "mincIO.getProperty::frameNumber" )
   checkEquals(mincIO.getProperty(vol, "volumeType"), "anatomical", msg = "mincIO.getProperty::volumeType" )
   checkEquals(mincIO.getProperty(vol, "colorMap"), "gray", msg = "mincIO.getProperty::colorMap" )
}


test.readVolume.setProperties <- function() {
   
   # set the test volume name
   volName <- "icbm_avg_152_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath)
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   # test the embedded MincInfo properties, using the accessor functions
   mincIO.setProperty(vol, "filename", "My_test_volname.mnc")
   checkTrue(mincIO.getProperty(vol, "filename") == "My_test_volname.mnc", msg = "mincIO.setProperty::set filename" )
   mincIO.setProperty(vol, "volumeIntensityRange", c(123, 789))
   checkEquals(mincIO.getProperty(vol, "volumeIntensityRange"), c(123, 789), msg = "mincIO.setProperty::set volumeIntensityRange" )

   # test the other MincVolumeIO properties, using the accessor functions
   mincIO.setProperty(vol, "frameNumber", 99)
   checkEquals(mincIO.getProperty(vol, "frameNumber"), 99, msg = "mincIO.setProperty::set frameNumber" )
   #
   mincIO.setProperty(vol, "volumeType", "functional")
   checkEquals(mincIO.getProperty(vol, "volumeType"), "functional", msg = "mincIO.setProperty::set volumeType" )
   #
   mincIO.setProperty(vol, "colorMap", "rainbow")
   checkEquals(mincIO.getProperty(vol, "colorMap"), "rainbow", msg = "mincIO.setProperty::set colorMap" )
}


test.writeVolume.updated <- function() {
   
   # set the test volume name
   volName <- "icbm_avg_152_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath)
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   # update the volume by drawing a line through the caudate (slice 82)
   nVoxels_before_mod <- sum(vol[,,82] > 500000)   # get nVoxels w/ values > 500000
   vol[20:160, 142, 82] <- 500001
   nVoxels_after_mod <- sum(vol[,,82] > 500000)    # get updated
   checkEquals(nVoxels_after_mod, nVoxels_before_mod +141L, msg="updating image data via indexing prior to write")

   # write it out
   filenameOut_fullpath <- file.path(tempdir(), "RUnit.writeVolume.updated.mnc")
   mincIO.writeVolume(vol, filenameOut_fullpath, clobber=TRUE)

   # read the volume in again and ensure that the modifications persist
   volModified <- mincIO.readVolume(filenameOut_fullpath)
   #
   nVoxels_after_write <- sum(vol[,,82] > 500000)
   checkEquals(nVoxels_after_mod, nVoxels_after_write, msg="checking integrity of volume write")
   
   # clean up
   unlink(filenameOut_fullpath)
}


test.writeVolume.newAsIcbm <- function() {
   
   # set the test volume name, and then make a new, empty ICBM-152 volume
   volname_fullpath <- file.path(tempdir(), "RUnit.writeVolume.newAsIcbm.mnc")
   vol <- mincIO.makeNewVolume(filename=volname_fullpath, likeTemplate="icbm152")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.makeNewVolume::returned object is of MincVolumeIO class")

   # update the volume by drawing a line through the caudate (slice 82)
   vol[20:160, 142, 82] <- 500001
   nVoxels_before_write <- sum(vol[,,82] > 500000)    # get updated
   # write it out
   mincIO.writeVolume(vol, volname_fullpath, clobber=TRUE)

   # read the volume in again and ensure that the modifications persist
   volNew <- mincIO.readVolume(volname_fullpath)
   #
   nVoxels_after_write <- sum(volNew[,,82] > 500000)
   checkEquals(nVoxels_before_write, nVoxels_after_write, msg="checking integrity of volume write")
   
   # clean up
   unlink(volname_fullpath)
}


test.writeVolume.newAsMni305linear <- function() {
   
   # set the test volume name, and then make a new, empty mni305linear volume
   volname_fullpath <- file.path(tempdir(), "RUnit.writeVolume.newAsMni305linear.mnc")
   vol <- mincIO.makeNewVolume(filename=volname_fullpath, likeTemplate="mni305linear")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.makeNewVolume::returned object is of MincVolumeIO class")

   # update the volume by drawing a line through the caudate (slice 82)
   vol[27:147, 142, 86] <- 3001
   nVoxels_before_write <- sum(vol[,,86] > 3000)    # get updated
   # write it out
   mincIO.writeVolume(vol, volname_fullpath, clobber=TRUE)

   # read the volume in again and ensure that the modifications persist
   volNew <- mincIO.readVolume(volname_fullpath)
   #
   nVoxels_after_write <- sum(volNew[,,86] > 3000)
   checkEquals(nVoxels_before_write, nVoxels_after_write, msg="checking integrity of volume write")
   
   # clean up
   unlink(volname_fullpath)
}


test.writeVolume.newAsMni305PET <- function() {
   
   # set the test volume name, and then make a new, empty mni305PET volume
   volname_fullpath <- file.path(tempdir(), "RUnit.writeVolume.newAsMni305PET.mnc")
   vol <- mincIO.makeNewVolume(filename=volname_fullpath, likeTemplate="mni305PET")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.makeNewVolume::returned object is of MincVolumeIO class")

   # update the volume by drawing a line through the caudate (slice 82)
   vol[22:110, 87, 34] <- 500001
   nVoxels_before_write <- sum(vol[,,34] > 500000)    # get updated
   # write it out
   mincIO.writeVolume(vol, volname_fullpath, clobber=TRUE)

   # read the volume in again and ensure that the modifications persist
   volNew <- mincIO.readVolume(volname_fullpath)
   #
   nVoxels_after_write <- sum(volNew[,,34] > 500000)
   checkEquals(nVoxels_before_write, nVoxels_after_write, msg="checking integrity of volume write")
   
   # clean up
   unlink(volname_fullpath)
}













