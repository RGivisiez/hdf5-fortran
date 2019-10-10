! This program is based on the complex_data_with_gzip.f90,
! but now we will select and write in a piece of the dataspace.
!
Program Main

  Use HDF5
  Implicit None

  Integer hdf_err

  Integer(HID_T) :: file_id     ! File identifier
  Integer(HID_T) :: dset_id     ! Dataset identifier 
  Integer(HID_T) :: group_id    ! Group identifier
  Integer(HID_T) :: dspace_id   ! Dataspace identifier
  Integer(HID_T) :: dcpl        ! Dataspace compress identifier

  ! Names for files, groups and other things.

  Character(len=35), Parameter :: file_name = 'unl_complex_dataset_chunk_gzip.h5' 
  Character(len=7), Parameter :: dset_name = 'dataset'
  Character(len=13), Parameter :: group_name = 'first_folder'
  Character(len=60) :: path

  ! Dataset's variables.

  Integer, Parameter :: rank = 2                               ! Dataset rank (in this case 2 dimensions)

  ! Because of the chunk defined below in gzip's variables,
  ! dim0 and dim1 have to be divisible by 5.

    Integer*4, Parameter :: dim0 = 10 , dim1 = 10              ! Dataset dimensions
    Integer(HSIZE_T), Dimension(rank) :: dims = (/dim0, dim1/) ! Dataset dimensions

  ! Attribute's variables.
  INTEGER(HID_T) :: atr_id                                                   ! Attribute identifier
  INTEGER(HID_T) :: atr_type_id                                              ! Attribute Dataspace identifier
  Integer(HID_T) :: atr_space_id                                             ! Attribute dataspace identifier
  INTEGER(SIZE_T) :: attr_len                                                ! Length of the attribute string
  Character(LEN=8), Parameter :: atr_name = "describe"                       ! Attribute name
  Integer, Parameter :: atr_rank = 2                                         ! Attribure rank
  Integer*4, Parameter :: atr_dim0 = 3 , atr_dim1 = 1                        ! Attribute dimension
  Integer(HSIZE_T), Dimension(atr_rank) :: atr_dims = (/atr_dim0, atr_dim1/) ! Attribute dimension
  CHARACTER(LEN=80), DIMENSION(atr_dim0) ::  attr_data                       ! Attribute data

  ! Gzip's variables

  Integer*4, Parameter :: chunk0 = dim0 / 5 , chunk1 = dim1 / 5   
  Integer(HSIZE_T), Dimension(1:2) :: chunk =(/chunk0, chunk1/)

  ! Data's variables to be written in the dataset.
 
  Integer*4 i, j
  Integer*4, Parameter :: array_size = 2   ! Array size
  
  Type sample

    ! Create a new variable type with an array and a scalar.

    Real*8, Dimension(1:array_size) :: config
    Real*8 scalar

  End Type sample

  ! We will create only half of the data.

  Type(sample), Dimension(1:dim0 / 2), Target :: samples         ! Data to be written in the dataset
  Type(c_ptr) :: f_ptr                                           ! Pointer to samples(1)

  Integer(HSIZE_T), Dimension(1) :: array_dims = (/array_size/)  ! Dimension for the arrray type
  Integer(HID_T)   :: array_type_id                              ! Array type identifier
  Integer(HID_T)   :: sample_type_id                             ! Sample type identifier

  ! Chunk's variables

  Integer(HSIZE_T) :: chunck_dims(1:2) = (/5, 5/)          ! Chunk size
  INTEGER(HSIZE_T), DIMENSION(1:2) :: offset = (/5, 0/)  ! Hyperslab offset
  INTEGER(HID_T) :: memspace                          ! memspace identifier 
  INTEGER(HID_T) :: new_dspace_id                          ! memspace identifier 

  ! Unlimited's variables

  INTEGER(HSIZE_T), DIMENSION(1:2) :: inf_dims
  Integer(HSIZE_T), Dimension(rank) :: ext_dims = (/2*dim0, 2*dim1/) ! Dataset dimensions
  INTEGER(HSIZE_T), DIMENSION(1:2) :: ext_offset = (/15, 0/)  ! Hyperslab offset
  INTEGER(HSIZE_T), DIMENSION(1:2) :: ndims

  !
!> Create data to be written in the dataset. 
  ! We will create half of the data now and the other half
  ! will be added later.
  !  
  Do i = 1, dim0 / 2
    samples(i)%config(1:array_size) = (-1)**i
    samples(i)%scalar = i * 100.0d0
  End do

  print*, 'Sample (1)'
  print*, 'Array:', samples(1)%config
  print*, 'Scalar:', samples(1)%scalar
  print*,
  print*, 'Sample (2)'
  print*, 'Array:', samples(2)%config
  print*, 'Scalar:', samples(2)%scalar

  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdf_err)

!> H5S_UNLIMITED_F is a varialbe iniated after calling h5open_f.

  inf_dims = (/H5S_UNLIMITED_F, H5S_UNLIMITED_F/)

  !
  ! Create a file. (It links a file_id to the file)
  !
  CALL h5fcreate_f(file_name, H5F_ACC_TRUNC_F, file_id, hdf_err)

  ! =========================== Basic write ==========================

  !
!> Create an unlimited dataspace. (It links a dspace_id to the dataspace)
  !
  CALL h5screate_simple_f(rank, dims, dspace_id, hdf_err, inf_dims)
  ! =========================== Gzip ====================================

  ! Checking for gzip
  Call gzip_check()

  !
  ! Create the dataset property list, add the gzip
  ! compression filter and set the chunk size.
  ! (9 is the code for the best compression)
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, hdf_err)
  CALL h5pset_deflate_f(dcpl, 9, hdf_err)
  CALL h5pset_chunk_f(dcpl, rank, chunk, hdf_err)

  ! ======================================================================

  ! ================= Create a complex data type ==========================

  !
  ! Create the memory data type.
  ! (C_LOC returns a pointer to the data)
  ! (return a sample_type_id, this will be used to write the data
  ! in h5dwrite_f)
  !
  CALL H5Tcreate_f(H5T_COMPOUND_F, H5OFFSETOF(C_LOC(samples(1)), C_LOC(samples(2))), &
       sample_type_id, hdf_err) 

  ! Create an array type
  CALL h5Tarray_create_f(H5T_NATIVE_DOUBLE, 1, array_dims, array_type_id, hdf_err)

  ! Then use that array type to insert values into
  CALL H5Tinsert_f( sample_type_id, "Array", & ! Não colocar caracteres especiais: ç,á, ...
       H5OFFSETOF(C_LOC(samples(1)),C_LOC(samples(1)%config(1))), array_type_id, hdf_err)

  CALL H5Tinsert_f( sample_type_id, "Scalar", &
       H5OFFSETOF(C_LOC(samples(1)),C_LOC(samples(1)%scalar)), H5T_NATIVE_DOUBLE, hdf_err)

  ! =======================================================================

  !
  ! Create a dataset with default properties.
  ! (It uses the file_id and the dspace_id to create the dataset,
  ! also links a dset_id to the dataset)
  !
  CALL H5Dcreate_f(file_id, dset_name, sample_type_id, dspace_id, &
         dset_id, hdf_err, dcpl)

  !
  ! Write data to the dataset.
  ! (This data will be in the root folder (/) )
  !
  f_ptr = C_LOC(samples(1))
  CALL H5Dwrite_f(dset_id, sample_type_id, f_ptr, hdf_err)

  ! ====================================================================

  ! ============== Group ===============================================

  !
  ! Create a group in the file.
  ! (This creates a folder named 'first_folder' and links a group_id to
  ! this group)
  !
  CALL h5gcreate_f(file_id, group_name, group_id, hdf_err)

  !
  ! Create a dataset with default properties inside the group.
  ! ( path = /first_folder/dataset )
  !

  path = '/' // Trim(group_name) // '/' // Trim(dset_name)

  CALL h5dcreate_f(file_id, path, sample_type_id, dspace_id, &
       dset_id, hdf_err, dcpl)

  !
  ! Write data to the dataset that is inside the folder.
  !
  f_ptr = C_LOC(samples(1))
  CALL H5Dwrite_f(dset_id, sample_type_id, f_ptr, hdf_err)

  ! ===================================================================

  ! ======================== Chunk =====================================

  !
  ! Select the piece of the dataspace that hasn't been written yet. 
  !
  CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, &
       offset, chunck_dims, hdf_err) 

  !
  ! Create a dataspcae
  !
  CALL h5screate_simple_f(2, chunck_dims, memspace, hdf_err)

  !
  ! Write the same data on the other half.
  !
  f_ptr = C_LOC(samples(1))
  CALL h5dwrite_f(dset_id, sample_type_id, f_ptr, hdf_err, &
       memspace, dspace_id)


  CALL h5dset_extent_f(dset_id, ext_dims, hdf_err)

  CALL h5dget_space_f(dset_id, new_dspace_id, hdf_err)

  CALL h5sselect_hyperslab_f(new_dspace_id, H5S_SELECT_SET_F, &
       ext_offset, chunck_dims, hdf_err) 

  CALL h5screate_simple_f(2, chunck_dims, memspace, hdf_err)

  f_ptr = C_LOC(samples(1))
  CALL h5dwrite_f(dset_id, sample_type_id, f_ptr, hdf_err, &
       memspace, new_dspace_id)

 ! ===================================================================

  ! ====================== Attribute ==================================

  ! This isn't the best way to write attributes.

  !
  ! Initialize attribute's data
  !
  attr_data(1) = "Size 40"
  attr_data(2) = "Sample 01"
  attr_data(3) = "Temperature 2.2691"
  attr_len = 80

  !
  ! Create dataspace for the attribute.
  !
  CALL h5screate_simple_f(atr_rank, atr_dims, atr_space_id, hdf_err)

  !
  ! Create datatype for the attribute.
  !
  CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atr_type_id, hdf_err)
  CALL h5tset_size_f(atr_type_id, attr_len, hdf_err)

  !
  ! Create dataset attribute.
  ! (The dset_id is linked to the dataset of the folder /first_folder/dataset)
  !
  CALL h5acreate_f(dset_id, atr_name, atr_type_id, atr_space_id, atr_id, hdf_err)

  !
  ! Write the attribute data.
  !
  CALL h5awrite_f(atr_id, atr_type_id, attr_data, atr_dims, hdf_err)
  CALL h5awrite_f(atr_id, atr_type_id, attr_data, atr_dims, hdf_err)


  ! ===================================================================

  ! ================= Close ===========================================

  CALL h5pclose_f(dcpl , hdf_err)
  CALL h5gclose_f(group_id, hdf_err)
  CALL h5sclose_f(dspace_id, hdf_err)
  CALL h5dclose_f(dset_id, hdf_err)
  CALL h5fclose_f(file_id, hdf_err)
  CALL h5aclose_f(atr_id, hdf_err)
  CALL h5tclose_f(atr_type_id, hdf_err)
  CALL h5sclose_f(atr_space_id, hdf_err)
  CALL h5close_f(hdf_err)

End Program Main

Subroutine gzip_check()

  Use HDF5
  Implicit None

  LOGICAL :: avail
  INTEGER :: filter_info, filter_info_both, hdf_err

  ! Checking for gzip
  CALL h5zfilter_avail_f(H5Z_FILTER_DEFLATE_F, avail, hdf_err)

  If ( .not. avail) Then
     Write(*,'("gzip filter is not available.",/)')
     stop
  End if

  CALL h5zget_filter_info_f(H5Z_FILTER_DEFLATE_F, filter_info, hdf_err)

  filter_info_both=IOR(H5Z_FILTER_ENCODE_ENABLED_F,H5Z_FILTER_DECODE_ENABLED_F)
  
  If (filter_info .NE. filter_info_both) Then
     Write(*,'("gzip filter not available for encoding and decoding.",/)')
     stop
  End if

End Subroutine gzip_check