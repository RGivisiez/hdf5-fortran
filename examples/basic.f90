! In this program, you'll see how to:
! - Create and save a dataset.
! - Create a group.
! - Write comments in a dataset.

Program Main

  Use HDF5
  Implicit None

  Integer hdf_err

  Integer(HID_T) :: file_id     ! File identifier
  Integer(HID_T) :: dset_id     ! Dataset identifier 
  Integer(HID_T) :: group_id    ! Group identifier
  Integer(HID_T) :: dspace_id   ! Dataspace identifier

  ! Names for files, groups and other things.

  Character(len=10), Parameter :: file_name = 'dataset.h5' 
  Character(len=7), Parameter :: dset_name = 'dataset'
  Character(len=13), Parameter :: group_name = 'first_folder'
  Character(len=60) :: path

  ! Dataset's variables.

  Integer, Parameter :: rank = 2                             ! Dataset rank (in this case 2 dimensions)
  Integer*4, Parameter :: dim0 = 3 , dim1 = 4                ! Dataset dimensions
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


  ! Data's variables to be written in the dataset.

  Real*8, Dimension (dim0, dim1) :: matriz
  Integer*4 i, j


  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdf_err)

  !
  ! Create a file. (It links a file_id to the file)
  !
  CALL h5fcreate_f(file_name, H5F_ACC_TRUNC_F, file_id, hdf_err)

  ! =========================== Basic write ==========================

  !
  ! Create a dataspace. (It links a dspace_id to the dataspace)
  !
  CALL h5screate_simple_f(rank, dims, dspace_id, hdf_err)

  !
  ! Create a dataset with default properties.
  ! (It uses the file_id and the dspace_id to create the dataset,
  ! also links a dset_id to the dataset)
  !
  CALL h5dcreate_f(file_id, dset_name, H5T_NATIVE_DOUBLE, dspace_id, &
       dset_id, hdf_err)

  !
  ! Create data to be written in the dataset.
  !

  Do i = 1, dim0
    Do j = 1, dim1

      matriz(i, j) = (i - 1) * 4 + j

    end do
  enddo

  !
  ! Write data to the dataset.
  ! (This data will be in the root folder (/) )
  !
  CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, matriz, dims, hdf_err)

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

  CALL h5dcreate_f(file_id, path, H5T_NATIVE_DOUBLE, dspace_id, &
       dset_id, hdf_err)

  !
  ! Write data to the dataset that is inside the folder.
  !
  CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, matriz, dims, hdf_err)

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

  CALL h5gclose_f(group_id, hdf_err)
  CALL h5sclose_f(dspace_id, hdf_err)
  CALL h5dclose_f(dset_id, hdf_err)
  CALL h5fclose_f(file_id, hdf_err)
  CALL h5aclose_f(atr_id, hdf_err)
  CALL h5tclose_f(atr_type_id, hdf_err)
  CALL h5sclose_f(atr_space_id, hdf_err)
  CALL h5close_f(hdf_err)

End Program Main