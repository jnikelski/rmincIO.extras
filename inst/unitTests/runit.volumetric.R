##
## Test assorted volumetric functions
##
##
##
.setUp <- function() {
   volDir <- system.file("", package="rmincIO.extras")
   volDir <- file.path(volDir, "packageData/volumes/")
   assign( "volDir", volDir, globalenv() )
}


## explode all labels into component volume objects
test.explodeLabelVolume.civetLabels <- function() {
   
   # create a vector of files, known to be readable
   lblVolname <- "colin27-20101130_discrete_classify.mnc"
   lblVolname_fullpath <- file.path(volDir, lblVolname)
   lblVol <- mincIO.readVolume(lblVolname_fullpath, volumeType="label")
   
   # explode and test returned components
   components <- volume.explodeLabelVolume(lblVol)
   checkEquals(length(components), 4L, msg="explodeLabelVolume::4 components returned")
   checkEquals(names(components)[1], "bg", msg="explodeLabelVolume::comp #1 is bg")
   checkEquals(names(components)[2], "csf", msg="explodeLabelVolume::comp #2 is csf")
   checkEquals(names(components)[3], "wm", msg="explodeLabelVolume::comp #3 is wm")
   checkEquals(names(components)[4], "gm", msg="explodeLabelVolume::comp #4 is gm")

   # check mask sizes
   checkEquals(sum(components$bg), 5246134, msg="explodeLabelVolume::check mask size -- bg")
   checkEquals(sum(components$csf), 188584, msg="explodeLabelVolume::check mask size -- csf")
   checkEquals(sum(components$wm), 668707, msg="explodeLabelVolume::check mask size -- wm")
   checkEquals(sum(components$gm), 1005712, msg="explodeLabelVolume::check mask size -- gm")

   # summing all mask elements should equal number of volume voxels in total
   totalVoxels <- prod(mincIO.getProperty(components$bg, "sizes"))
   totalMaskVoxels <- sum(components$bg) + sum(components$csf) + sum(components$wm) + sum(components$gm)
   checkEquals(totalVoxels, totalMaskVoxels, msg="explodeLabelVolume::check total mask voxels")
}


## explode all labels into component volume objects (do *not* use the Civet labels for list components)
test.explodeLabelVolume.noCivetLabels <- function() {
   
   # create a vector of files, known to be readable
   lblVolname <- "colin27-20101130_discrete_classify.mnc"
   lblVolname_fullpath <- file.path(volDir, lblVolname)
   lblVol <- mincIO.readVolume(lblVolname_fullpath, volumeType="label")
   
   # explode and test returned components
   components <- volume.explodeLabelVolume(lblVol, civetLabels=FALSE)
   checkEquals(length(components), 4L, msg="explodeLabelVolume::4 components returned")
   checkEquals(names(components)[1], "label_0", msg="explodeLabelVolume::comp #1 is bg")
   checkEquals(names(components)[2], "label_1", msg="explodeLabelVolume::comp #2 is csf")
   checkEquals(names(components)[3], "label_3", msg="explodeLabelVolume::comp #3 is wm")
   checkEquals(names(components)[4], "label_2", msg="explodeLabelVolume::comp #4 is gm")

   # check mask sizes
   checkEquals(sum(components$label_0), 5246134, msg="explodeLabelVolume::check mask size -- bg")
   checkEquals(sum(components$label_1), 188584, msg="explodeLabelVolume::check mask size -- csf")
   checkEquals(sum(components$label_2), 1005712, msg="explodeLabelVolume::check mask size -- gm")
   checkEquals(sum(components$label_3), 668707, msg="explodeLabelVolume::check mask size -- wm")
}


## explode only 2 labels into component volume objects
test.explodeLabelVolume.partial <- function() {
   
   # create a vector of files, known to be readable
   lblVolname <- "colin27-20101130_discrete_classify.mnc"
   lblVolname_fullpath <- file.path(volDir, lblVolname)
   lblVol <- mincIO.readVolume(lblVolname_fullpath, volumeType="label")
   
   # explode and test returned components
   components <- volume.explodeLabelVolume(lblVol, labels=c(2,3), civetLabels=TRUE)
   checkEquals(length(components), 2L, msg="explodeLabelVolume::2 components returned")
   checkEquals(names(components)[1], "gm", msg="explodeLabelVolume::comp #1 is gm")
   checkEquals(names(components)[2], "wm", msg="explodeLabelVolume::comp #2 is wm")

   # check mask sizes
   checkEquals(sum(components$wm), 668707, msg="explodeLabelVolume::check mask size -- wm")
   checkEquals(sum(components$gm), 1005712, msg="explodeLabelVolume::check mask size -- gm")
}


## combine a number of mask volumes
test.combineMaskVolumes <- function() {
   
   # create a vector of files, known to be readable
   lblVolname <- "colin27-20101130_discrete_classify.mnc"
   lblVolname_fullpath <- file.path(volDir, lblVolname)
   lblVol <- mincIO.readVolume(lblVolname_fullpath, volumeType="label")
   
   # explode into 4 components
   components <- volume.explodeLabelVolume(lblVol, civetLabels=TRUE)
   
   # remove background component -- combine the rest
   volList <- list(components$gm, components$wm, components$csf)
   combinedMaskVol <- volume.combineMaskVolumes(volList)
   checkEqualsNumeric(class(combinedMaskVol), "MincVolumeIO", msg="combineMaskVolumes::check object type/class")
   
   # make sure that the combined mask is correct
   totalVoxels <- prod(mincIO.getProperty(combinedMaskVol, "sizes"))
   nBgVoxels <- 5246134
   totalNonBgVoxels <- totalVoxels - nBgVoxels
   checkEquals(sum(combinedMaskVol), totalNonBgVoxels, msg="combineMaskVolumes::check mask size -- csf+gm+wm")
}

