##
## Test methods for the mincSlice class
##
##
##
.setUp <- function() {
   volDir <- system.file("", package="rmincIO.extras")
   volDir <- file.path(volDir, "packageData/volumes/")
   assign( "volDir", volDir, globalenv() )
}


## read an axial slice
test.mincSlice.readSliceZ <- function() {

   # set the test volume name
   volName <- "average305_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath, volumeType="functional", colorMap="rainbow")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   # extract a slice
   slice <- mincIO.getSliceZ(vol, 75)        # extract axial slice 75
   #printslice)                              # print summary info

   # test the embedded MincInfo properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice, "nDimensions"), 3L, msg = "mincIO.getProperty::get nDimensions" )
   checkEquals(mincIO.getProperty(slice, "nFrames"), 0L, msg = "mincIO.getProperty::get nFrames" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes"), c(172L, 220L, 156L), msg = "mincIO.getProperty::get dim sizes" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes")[3], 156L, msg = "mincIO.getProperty::get a dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes")["zspace"], 156L, msg = "mincIO.getProperty::get zspace dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice, "starts"), c(-86.095, -126.510, -68.250), msg = "mincIO.getProperty::get dim starts" )

   # test the other MincSlice properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice, "sliceNumber"), 75, msg = "mincIO.getProperty::sliceNumber" )
   checkEquals(mincIO.getProperty(slice, "orientation"), "zSlice", msg = "mincIO.getProperty::orientation" )
   checkEquals(mincIO.getProperty(slice, "volumeType"), "functional", msg = "mincIO.getProperty::volumeType" )
   checkEquals(mincIO.getProperty(slice, "colorMap"), "rainbow", msg = "mincIO.getProperty::colorMap" )
   checkEquals(mincIO.getProperty(slice, "aspectRatio"), c(1.279069767), msg = "mincIO.getProperty::aspectRatio" )
}


## read a coronal slice
test.mincSlice.readSliceY <- function() {

   # set the test volume name
   volName <- "average305_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath, volumeType="functional", colorMap="rainbow")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   # extract a slice
   slice <- mincIO.getSliceY(vol, 130)       # extract coronal slice 130
   #printslice)                              # print summary info

   # test the embedded MincInfo properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice, "nDimensions"), 3L, msg = "mincIO.getProperty::get nDimensions" )
   checkEquals(mincIO.getProperty(slice, "nFrames"), 0L, msg = "mincIO.getProperty::get nFrames" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes"), c(172L, 220L, 156L), msg = "mincIO.getProperty::get dim sizes" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes")[3], 156L, msg = "mincIO.getProperty::get a dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes")["zspace"], 156L, msg = "mincIO.getProperty::get zspace dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice, "starts"), c(-86.095, -126.510, -68.250), msg = "mincIO.getProperty::get dim starts" )

   # test the other MincSlice properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice, "sliceNumber"), 130, msg = "mincIO.getProperty::sliceNumber" )
   checkEquals(mincIO.getProperty(slice, "orientation"), "ySlice", msg = "mincIO.getProperty::orientation" )
   checkEquals(mincIO.getProperty(slice, "volumeType"), "functional", msg = "mincIO.getProperty::volumeType" )
   checkEquals(mincIO.getProperty(slice, "colorMap"), "rainbow", msg = "mincIO.getProperty::colorMap" )
   checkEquals(mincIO.getProperty(slice, "aspectRatio"), c(0.9069767442), msg = "mincIO.getProperty::aspectRatio" )
}


## read a saggital slice
test.mincSlice.readSliceX <- function() {

   # set the test volume name
   volName <- "average305_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath, volumeType="functional", colorMap="rainbow")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   # extract a slice
   slice <- mincIO.getSliceX(vol, 80)        # extract saggital slice 80
   #printslice)                              # print summary info

   # test the embedded MincInfo properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice, "nDimensions"), 3L, msg = "mincIO.getProperty::get nDimensions" )
   checkEquals(mincIO.getProperty(slice, "nFrames"), 0L, msg = "mincIO.getProperty::get nFrames" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes"), c(172L, 220L, 156L), msg = "mincIO.getProperty::get dim sizes" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes")[3], 156L, msg = "mincIO.getProperty::get a dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice, "sizes")["zspace"], 156L, msg = "mincIO.getProperty::get zspace dim size" )
   checkEqualsNumeric(mincIO.getProperty(slice, "starts"), c(-86.095, -126.510, -68.250), msg = "mincIO.getProperty::get dim starts" )

   # test the other MincSlice properties, using the accessor functions
   checkEquals(mincIO.getProperty(slice, "sliceNumber"), 80, msg = "mincIO.getProperty::sliceNumber" )
   checkEquals(mincIO.getProperty(slice, "orientation"), "xSlice", msg = "mincIO.getProperty::orientation" )
   checkEquals(mincIO.getProperty(slice, "volumeType"), "functional", msg = "mincIO.getProperty::volumeType" )
   checkEquals(mincIO.getProperty(slice, "colorMap"), "rainbow", msg = "mincIO.getProperty::colorMap" )
   checkEquals(mincIO.getProperty(slice, "aspectRatio"), c(0.7090909091), msg = "mincIO.getProperty::aspectRatio" )
}


## read an axial slice, update, and write
test.mincSlice.putSliceZ <- function() {

   # set the test volume name
   volName <- "average305_t1_tal_lin.mnc"
   volName_fullpath <- file.path(volDir, volName)
   
   # read volume using a filename
   vol <- mincIO.readVolume(volName_fullpath, volumeType="functional", colorMap="rainbow")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.readVolume::returned object is of MincVolumeIO class")

   # extract a slice
   slice <- mincIO.getSliceZ(vol, 75)        # extract axial slice 75
   #printslice)                              # print summary info

   # modify the slice
   slice[20:40, 180:200] <- 0                # place square in upper left
   slice[140:160, 20:40] <- 0                # ... and in lower right
   slice[60:120, 107:130] <- 0               # block out the center

   # update vol with moded slice 
   nVoxels_before_put <- sum(vol[,,75] < 1.0)
   vol <- mincIO.putSlice(slice, vol)        # write to original slice position
   vol <- mincIO.putSlice(slice, vol, 80)    # ... and also to a different position
   nVoxels_after_put <- sum(vol[,,75] < 1.0)

   # write it out
   filenameOut_fullpath <- file.path(tempdir(), "RUnit.mincSlice.putSliceZ.mnc")
   mincIO.writeVolume(vol, filenameOut_fullpath, clobber=TRUE)

   # read the volume in again and ensure that the modifications persist
   volModified <- mincIO.readVolume(filenameOut_fullpath)
   #
   nVoxels_after_write <- sum(volModified[,,75] <  1.0)
   checkEquals(nVoxels_after_put, nVoxels_after_write, msg="checking integrity of volume write")
  
   # clean up
   unlink(filenameOut_fullpath)
}


## make a new zSlice, and insert into a volume
test.mincSlice.newSliceZ.randomFill <- function() {

   # set the test volume name, and then make a new, empty mni305linear volume
   volname_fullpath <- file.path(tempdir(), "RUnit.mincSlice.newSliceZ.randomFill.mnc")
   vol <- mincIO.makeNewVolume(filename=volname_fullpath, likeTemplate="mni305linear")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.makeNewVolume::returned object is of MincVolumeIO class")
   
   # change volume display info
   mincIO.setProperty(vol, "volumeType", "functional")
   mincIO.setProperty(vol, "colorMap", "rainbow")

   # retrive some dim info (for convenience)
   nCols <- mincIO.getProperty(vol, "sizes")["xspace"]
   nRows <- mincIO.getProperty(vol, "sizes")["yspace"]
   nSlices <- mincIO.getProperty(vol, "sizes")["zspace"]


   # loop over all slices in the volume, inserting new stuff
   for (ndx in 1:nSlices) {
      #cat(sprintf("loop %d\n", ndx)) 
   
      # create a new empty slice object, matching the newly created volume
      slice <- mincIO.makeNewSliceZ(vol, rnorm(nRows*nCols,20*ndx,7))
   
      # update vol with new slice 
      vol <- mincIO.putSlice(slice, vol, ndx)
   }


   # write it out
   filenameOut_fullpath <- file.path(tempdir(), "RUnit.mincSlice.newSliceZ.randomFill.mnc")
   mincIO.writeVolume(vol, filenameOut_fullpath, clobber=TRUE)

   # clean up
   unlink(filenameOut_fullpath)
}


## make a new zSlice, and insert into a volume
test.mincSlice.newSliceZ.seqFill <- function() {

   # set the test volume name, and then make a new, empty mni305linear volume
   volname_fullpath <- file.path(tempdir(), "RUnit.mincSlice.newSliceZ.seqFill.mnc")
   vol <- mincIO.makeNewVolume(filename=volname_fullpath, likeTemplate="mni305linear")
   checkEqualsNumeric(class(vol), "MincVolumeIO", msg="mincIO.makeNewVolume::returned object is of MincVolumeIO class")
   
   # change volume display info
   mincIO.setProperty(vol, "volumeType", "functional")
   mincIO.setProperty(vol, "colorMap", "rainbow")

   # retrive some dim info (for convenience)
   nCols <- mincIO.getProperty(vol, "sizes")["xspace"]
   nRows <- mincIO.getProperty(vol, "sizes")["yspace"]
   nSlices <- mincIO.getProperty(vol, "sizes")["zspace"]


   # loop over all slices in the volume, inserting new stuff
   for (ndx in 1:nSlices) {
      #cat(sprintf("loop %d\n", ndx)) 
   
      # create a new empty slice object, matching the newly created volume
      slice <- mincIO.makeNewSliceZ(vol, as.numeric(rep(ndx, nRows*nCols)))
   
      # update vol with new slice 
      vol <- mincIO.putSlice(slice, vol, ndx)
   }


   # write it out
   filenameOut_fullpath <- file.path(tempdir(), "RUnit.mincSlice.newSliceZ.seqFill.mnc")
   mincIO.writeVolume(vol, filenameOut_fullpath, clobber=TRUE)

   # read the volume in again and ensure that the modifications persist
   volModified <- mincIO.readVolume(filenameOut_fullpath)
   #
   checkEquals(round(volModified[1,1,1]), 1, msg="checking integrity of volume write -- slice 1")
   checkEquals(round(volModified[1,1,2]), 2, msg="checking integrity of volume write -- slice 2")
   checkEquals(round(volModified[1,1,20]), 20, msg="checking integrity of volume write -- slice 20")
   checkEquals(round(volModified[1,1,50]), 50, msg="checking integrity of volume write -- slice 50")
   checkEquals(round(volModified[1,1,100]), 100, msg="checking integrity of volume write -- slice 100")
   checkEquals(round(volModified[1,1,156]), 156, msg="checking integrity of volume write -- slice 156")

   # clean up
   unlink(filenameOut_fullpath)
}


