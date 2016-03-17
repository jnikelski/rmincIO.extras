##
## Test methods for the mincVoxelIO class
##
##
##
.setUp <- function() {
   volDir <- system.file("", package="rmincIO.extras")
   volDir <- file.path(volDir, "packageData/volumes/")
   assign( "volDir", volDir, globalenv() )
}

## read a given voxel across a range of volumes
test.readByVoxel.3d <- function() {
   
   # set the test volume names
   # ... we're using a single low res structural volume with differtial blurring
   volnames <- c("icbm_avg_152_t1_tal_lin_4x4x4_blur00mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur10mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur20mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur30mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur40mm.mnc",
                  "icbm_avg_152_t1_tal_lin_4x4x4_blur50mm.mnc")
   volnames_fullpath <- file.path(volDir, volnames)
   

   # get voxel coords (given world coords)
   worldCoords <- c(18,38,0)
   voxCoords <- round(rmincUtil.convertWorldToVoxel(volnames_fullpath[1], worldCoords))
   checkEquals(voxCoords, c(28, 42, 19), msg = "mincUtil.convertWorldToVoxel::convert coords" )
   #print(voxCoords)

   # read voxel values from all volumes into a voxel array
   voxel_array <- mincIO.readByVoxel(volnames_fullpath, voxCoords)
   checkEqualsNumeric(class(voxel_array), "MincVoxelIO", msg="mincIO.readByVoxel::returned object is of MincVoxelIO class")
   #print(voxel_array)
   #print(voxel_array[,])
   
   # check 1st and last returned voxel value 
   # Note: target value returned by ...
   #       "mincextract -ascii -start 18,41,27 -count 1,1,1 icbm_avg_152_t1_tal_lin_4x4x4.mnc"
   checkEqualsNumeric(voxel_array[1,1], 345615.53369889612077, msg="mincIO.readByVoxel::check vol1 value returned #1")
   checkEqualsNumeric(voxel_array[6,1], 236099.78035168768838, msg="mincIO.readByVoxel::check vol1 value returned #2")
}


## read a given voxel across all frames (one volume)
test.readByVoxel.4d.oneVolume <- function() {
   
   # set the test volume name
   volnames <- c("functional_4D.mnc")
   volnames_fullpath <- file.path(volDir, volnames)
   

   # get voxel coords (given world coords)
   worldCoords <- c(24, 58, 2)
   voxCoords <- round(rmincUtil.convertWorldToVoxel(volnames_fullpath[1], worldCoords))
   checkEquals(voxCoords, c(58, 93, 38), msg = "mincUtil.convertWorldToVoxel::convert coords" )
   #print(voxCoords)

   # read voxel values from all volumes into a voxel array
   voxel_array <- mincIO.readByVoxel(volnames_fullpath, voxCoords)
   checkEqualsNumeric(class(voxel_array), "MincVoxelIO", msg="mincIO.readByVoxel::returned object is of MincVoxelIO class")
   #print(voxel_array)
   #print(voxel_array[,])
   
   # check a sampling of returned voxel values across frames @ slice 38
   # Note: target value returned by ...
   #       "mincextract -ascii -start [0_rel_frame_no],37,92,57 -count 1,1,1,1 functional_4D.mnc"
   checkEqualsNumeric(voxel_array[1,1], 8.265837317748578883, msg="mincIO.readByVoxel::check frame 1 value returned")
   checkEqualsNumeric(voxel_array[5,1], 280.27176475554443869, msg="mincIO.readByVoxel::check frame 5 value returned")
   checkEqualsNumeric(voxel_array[10,1], 801.51874412333563669, msg="mincIO.readByVoxel::check frame 10 value returned")
   checkEqualsNumeric(voxel_array[15,1], 581.28806285621544703, msg="mincIO.readByVoxel::check frame 15 value returned")
   checkEqualsNumeric(voxel_array[20,1], 458.66426945528235137, msg="mincIO.readByVoxel::check frame 20 value returned")
   checkEqualsNumeric(voxel_array[25,1], 326.27178765699454743, msg="mincIO.readByVoxel::check frame 25 value returned")
   checkEqualsNumeric(voxel_array[30,1], 211.02946069387681405, msg="mincIO.readByVoxel::check frame 30 value returned")
   checkEqualsNumeric(voxel_array[34,1], 161.45908755697664105, msg="mincIO.readByVoxel::check frame 34 value returned")
}



