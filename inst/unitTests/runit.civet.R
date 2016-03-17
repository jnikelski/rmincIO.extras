##
## Test various Civet-related functions
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
   #
   civetDir <- system.file("", package="rmincIO.extras")
   civetDir <- file.path(civetDir, "packageData/civet-11/")
   assign( "civetDir", civetDir, globalenv() )
}


## return the fully-qualified filename of the *_classify.mnc volume
test.getFilenameClassify <- function() {
   
   scanID <- "batman-20050408"
   volname <- civet.getFilenameClassify(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(rmincUtil.isMinc(volname), msg="getFilenameClassify::volume exists?")
   #
   volInfo <- mincIO.readMincInfo(volname)
   checkTrue(class(volInfo) == "MincInfo", msg="getFilenameClassify::volume is readable?")
}


## return the fully-qualified filename of the *_pve_*.mnc volume
test.getFilenameGrayMatterPve <- function() {
   
   scanID <- "batman-20050408"
   volname <- civet.getFilenameGrayMatterPve(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(rmincUtil.isMinc(volname), msg="getFilenameGrayMatterPve::volume exists?")
   #
   volInfo <- mincIO.readMincInfo(volname)
   checkTrue(class(volInfo) == "MincInfo", msg="getFilenameGrayMatterPve::volume is readable?")
}


## return the fully-qualified filename of the *_pve_*.mnc volume
test.getFilenameWhiteMatterPve <- function() {
   
   scanID <- "batman-20050408"
   volname <- civet.getFilenameWhiteMatterPve(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(rmincUtil.isMinc(volname), msg="getFilenameWhiteMatterPve::volume exists?")
   #
   volInfo <- mincIO.readMincInfo(volname)
   checkTrue(class(volInfo) == "MincInfo", msg="getFilenameWhiteMatterPve::volume is readable?")
}


## return the fully-qualified filename of the *_pve_*.mnc volume
test.getFilenameCsfPve <- function() {
   
   scanID <- "batman-20050408"
   volname <- civet.getFilenameCsfPve(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(rmincUtil.isMinc(volname), msg="getFilenameCsfPve::volume exists?")
   #
   volInfo <- mincIO.readMincInfo(volname)
   checkTrue(class(volInfo) == "MincInfo", msg="getFilenameCsfPve::volume is readable?")
}


## return the fully-qualified filename of the *_t1_final.mnc volume
test.getFilenameStxT1 <- function() {
   
   scanID <- "batman-20050408"
   volname <- civet.getFilenameStxT1(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(rmincUtil.isMinc(volname), msg="getFilenameStxT1::volume exists?")
   #
   volInfo <- mincIO.readMincInfo(volname)
   checkTrue(class(volInfo) == "MincInfo", msg="getFilenameStxT1::volume is readable?")
}


## return the fully-qualified filename of the *_brain_mask.mnc volume
test.getFilenameCerebrumMask <- function() {
   
   scanID <- "batman-20050408"
   volname <- civet.getFilenameCerebrumMask(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(rmincUtil.isMinc(volname), msg="getFilenameCerebrumMask::volume exists?")
   #
   volInfo <- mincIO.readMincInfo(volname)
   checkTrue(class(volInfo) == "MincInfo", msg="getFilenameCerebrumMask::volume is readable?")
}


## return the fully-qualified filename of the *_skull_mask.mnc volume
test.getFilenameSkullMask <- function() {
   
   scanID <- "batman-20050408"
   volname <- civet.getFilenameSkullMask(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(rmincUtil.isMinc(volname), msg="getFilenameSkullMask::volume exists?")
   #
   volInfo <- mincIO.readMincInfo(volname)
   checkTrue(class(volInfo) == "MincInfo", msg="getFilenameSkullMask::volume is readable?")
}


## return the fully-qualified gray matter surface filenames
test.getFilenameGrayMatterSurfaces <- function() {
   
   # non-resampled surfaces
   scanID <- "batman-20050408"
   surfaceNames <- civet.getFilenameGrayMatterSurfaces(scanID, civetDir, civetVersion="1.1.11", resampled=FALSE)
   checkTrue(rmincUtil.isMniObjAscii(surfaceNames$left), msg="getFilenameGrayMatterSurfaces::non-rsl surface exists? -- LH")
   checkTrue(rmincUtil.isMniObjAscii(surfaceNames$right), msg="getFilenameGrayMatterSurfaces::non-rsl surface exists? -- RH")

   # resampled surfaces
   scanID <- "batman-20050408"
   rslSurfaceNames <- civet.getFilenameGrayMatterSurfaces(scanID, civetDir, civetVersion="1.1.11", resampled=TRUE)
   checkTrue(rmincUtil.isMniObjAscii(rslSurfaceNames$left), msg="getFilenameGrayMatterSurfaces::rsl surface exists? -- LH")
   checkTrue(rmincUtil.isMniObjAscii(rslSurfaceNames$right), msg="getFilenameGrayMatterSurfaces::rsl surface exists? -- RH")

   # rsl surface names should be different from non-rslSurfaceNames
   checkTrue(surfaceNames$left != rslSurfaceNames$left, msg="getFilenameGrayMatterSurfaces::rsl .ne. non-rsl")
}


## return the fully-qualified white matter surface filenames
test.getFilenameWhiteMatterSurfaces <- function() {
   
   # non-resampled surfaces
   scanID <- "batman-20050408"
   surfaceNames <- civet.getFilenameWhiteMatterSurfaces(scanID, civetDir, civetVersion="1.1.11", resampled=FALSE)
   checkTrue(rmincUtil.isMniObjAscii(surfaceNames$left), msg="getFilenameWhiteMatterSurfaces::non-rsl surface exists? -- LH")
   checkTrue(rmincUtil.isMniObjAscii(surfaceNames$right), msg="getFilenameWhiteMatterSurfaces::non-rsl surface exists? -- RH")

   # resampled surfaces
   scanID <- "batman-20050408"
   rslSurfaceNames <- civet.getFilenameWhiteMatterSurfaces(scanID, civetDir, civetVersion="1.1.11", resampled=TRUE)
   checkTrue(rmincUtil.isMniObjAscii(rslSurfaceNames$left), msg="getFilenameWhiteMatterSurfaces::rsl surface exists? -- LH")
   checkTrue(rmincUtil.isMniObjAscii(rslSurfaceNames$right), msg="getFilenameWhiteMatterSurfaces::rsl surface exists? -- RH")

   # rsl surface names should be different from non-rslSurfaceNames
   checkTrue(surfaceNames$left != rslSurfaceNames$left, msg="getFilenameWhiteMatterSurfaces::rsl .ne. non-rsl")
}


## return the fully-qualified mid surface filenames
test.getFilenameMidSurfaces <- function() {
   
   # non-resampled surfaces
   scanID <- "batman-20050408"
   surfaceNames <- civet.getFilenameMidSurfaces(scanID, civetDir, civetVersion="1.1.11", resampled=FALSE)
   checkTrue(rmincUtil.isMniObjAscii(surfaceNames$left), msg="getFilenameMidSurfaces::non-rsl surface exists? -- LH")
   checkTrue(rmincUtil.isMniObjAscii(surfaceNames$right), msg="getFilenameMidSurfaces::non-rsl surface exists? -- RH")

   # resampled surfaces
   scanID <- "batman-20050408"
   rslSurfaceNames <- civet.getFilenameMidSurfaces(scanID, civetDir, civetVersion="1.1.11", resampled=TRUE)
   checkTrue(rmincUtil.isMniObjAscii(rslSurfaceNames$left), msg="getFilenameMidSurfaces::rsl surface exists? -- LH")
   checkTrue(rmincUtil.isMniObjAscii(rslSurfaceNames$right), msg="getFilenameMidSurfaces::rsl surface exists? -- RH")

   # rsl surface names should be different from non-rslSurfaceNames
   checkTrue(surfaceNames$left != rslSurfaceNames$left, msg="getFilenameMidSurfaces::rsl .ne. non-rsl")
}


## return the fully-qualified cortical thickness filenames 
test.getFilenameCorticalThickness <- function() {
   
   # thickness values derived from **non-resampled surfaces**
   scanID <- "batman-20050408"
   filenames <- civet.getFilenameCorticalThickness(scanID, civetDir, civetVersion="1.1.11", resampled=FALSE)
   checkTrue(filenames$left != "", msg="getFilenameCorticalThickness::non-rsl file exists? -- LH")
   checkTrue(filenames$right != "", msg="getFilenameCorticalThickness::non-rsl file exists? -- RH")
   
   # thickness values derived from **resampled surfaces**
   scanID <- "batman-20050408"
   rslFilenames <- civet.getFilenameCorticalThickness(scanID, civetDir, civetVersion="1.1.11", resampled=TRUE)
   checkTrue(rslFilenames$left != "", msg="getFilenameCorticalThickness::rsl file exists? -- LH")
   checkTrue(rslFilenames$right != "", msg="getFilenameCorticalThickness::rsl file exists? -- RH")

   # rsl thickness filenames should be different from non-rsl
   checkTrue(filenames$left != rslFilenames$left, msg="getFilenameCorticalThickness::rsl .ne. non-rsl")
}


## return the fully-qualified linear transform filenames
test.getFilenameLinearTransform <- function() {
   
   scanID <- "batman-20050408"
   xfmFilename <- civet.getFilenameLinearTransform(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(xfmFilename != "", msg="getFilenameLinearTransform::volume exists?")
   
   # try to read it
   xfm.df <- rmincUtil.readLinearXfmFile(xfmFilename)
   checkEquals(class(xfm.df), "data.frame", msg="readLinearXfmFile::check returned class type")
   checkEquals(xfm.df["rotation","x"], -13.09563, msg="readLinearXfmFile::check returned xfm value")
}


## return the fully-qualified filename of the *_classify.mnc volume
test.getFilenameNonlinearTransform <- function() {
   
   scanID <- "batman-20050408"
   xfmFilenames <- civet.getFilenameNonlinearTransform(scanID, civetDir, civetVersion="1.1.11")
   checkTrue(xfmFilenames$xfm != "", msg="getFilenameNonlinearTransform::xfm exists?")
   checkTrue(xfmFilenames$grid != "", msg="getFilenameNonlinearTransform::grid volume exists?")
   
   # try to read the linear part
   xfm.df <- rmincUtil.readLinearXfmFile(xfmFilenames$xfm)
   checkEquals(class(xfm.df), "data.frame", msg="readLinearXfmFile::check returned class type")
   checkEquals(xfm.df["rotation","x"], 0, msg="readLinearXfmFile::check returned xfm value")

   ## check the grid value
   ## Note: R Aborts! since our C++ code has not been modified to handle 
   ##       volumes with "vector_dimension" dimensions (as used with grid volumes)
   
   
   #xfmVolInfo <- mincIO.readMincInfo(xfmFilenames$grid)
   #checkTrue(class(xfmVolInfo) == "MincInfo", msg="mincIO.readMincInfo::volume is readable?")
   #checkTrue(rmincUtil.isMinc(volname), msg="mincIO.readMincInfo::volume is minc?")
}


## return a selection of Civet-generated *.dat files
test.readCivetDatFiles <- function() {
   
   scanID <- "batman-20050408"
   civetDat.lst <- civet.readCivetDatFiles(scanID, civetDir, civetVersion="1.1.11")
   checkEquals(length(civetDat.lst), 4L, msg="readCivetDatFiles::check returned list length")
   checkEqualsNumeric(civetDat.lst$gyrification_index["lh"], 2.236, msg="readCivetDatFiles::check GI -- LH")
   checkEqualsNumeric(civetDat.lst$gyrification_index["rh"], 2.286, msg="readCivetDatFiles::check GI -- RH")
}


## return a named vector containing the tisse volumes, in stx space,
## derived from the final tissue classification volume.
test.computeStxTissueVolumes <- function() {
   
   scanID <- "batman-20050408"
   stxTissueVolumes <- civet.computeStxTissueVolumes(scanID, civetDir, civetVersion="1.1.11")
   checkEqualsNumeric(stxTissueVolumes["csf"], 385339, msg="computeStxTissueVolumes::check CSF volume")
   checkEqualsNumeric(stxTissueVolumes["gm"], 810850, msg="computeStxTissueVolumes::check GM volume")
   checkEqualsNumeric(stxTissueVolumes["wm"], 608788, msg="computeStxTissueVolumes::check WM volume")
}


## compute a global rescaling factor by reading the individual x,y,z
## rescales from the XFM, and returning the product
test.computeNativeToStxRescalingFactor <- function() {
   
   scanID <- "batman-20050408"
   globalRescaleFactor <- civet.computeNativeToStxRescalingFactor(scanID, civetDir, civetVersion="1.1.11")
   checkEquals(globalRescaleFactor, 1.357917183, msg="computeNativeToStxRescalingFactor::check rescale factor")
}


## return a named vector containing NATIVE space tisse volumes, 
## derived from the final tissue classification volume.
test.computeNativeTissueVolumes <- function() {
   
   scanID <- "batman-20050408"
   nativeTissueVolumes <- civet.computeNativeTissueVolumes(scanID, civetDir, civetVersion="1.1.11")
   checkEqualsNumeric(nativeTissueVolumes["csf"], 283772.0921, msg="computeNativeTissueVolumes::check CSF volume")
   checkEqualsNumeric(nativeTissueVolumes["gm"], 597127.7261, msg="computeNativeTissueVolumes::check GM volume")
   checkEqualsNumeric(nativeTissueVolumes["wm"], 448324.8371, msg="computeNativeTissueVolumes::check WM volume")
}








