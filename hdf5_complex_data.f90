Program Main

  Use HDF5

  Implicit None

  Integer*4, Parameter :: N = 2
  Integer*4 i
  Character*20, Parameter :: file_name = 'Ising_data.h5'
  Character*20, Parameter :: dset_name = 'L=40'

  Type sample

    Real*8, Dimension(1:N) :: config
    Real*8 energ

  End Type sample

  Type(sample), Dimension(1:N), Target :: samples

  !======= Variáveis do HDF5 =========
  Integer hdf_err
  Integer(HID_T)   :: sample_type_id, dset_id, dspace_id, file_id
  Integer(HID_T)   :: array_type_id

  Integer(HSIZE_T), Dimension(1) :: array_dims = (/N/) 
  Integer(HSIZE_T) :: dims(1) = (/N/) 
  Type(c_ptr) :: f_ptr

  !===================================

  Do i = 1, N
    samples(i)%config(1:N) = i
    samples(i)%energ = i * 100.0d0
  End do
  
  ! Initialize FORTRAN interface.
  CALL h5open_f(hdf_err)

  ! Create a new file using default properties.
  CALL h5fcreate_f(file_name, H5F_ACC_TRUNC_F, file_id, hdf_err)

  !
  ! Create the memory data type.
  !
  CALL H5Tcreate_f(H5T_COMPOUND_F, H5OFFSETOF(C_LOC(samples(1)), C_LOC(samples(2))), &
       sample_type_id, hdf_err) 

  ! Create the array type
  CALL h5Tarray_create_f(H5T_NATIVE_DOUBLE, 1, array_dims, array_type_id, hdf_err)

  ! Then use that array type to insert values into
  CALL H5Tinsert_f( sample_type_id, "Configuration", & ! Não colocar caracteres especiais: ç,á, ...
       H5OFFSETOF(C_LOC(samples(1)),C_LOC(samples(1)%config(1))), array_type_id, hdf_err)

  CALL H5Tinsert_f( sample_type_id, "Energy", &
       H5OFFSETOF(C_LOC(samples(1)),C_LOC(samples(1)%energ)), H5T_NATIVE_DOUBLE, hdf_err)

  !
  ! Create dataspace
  !
  CALL h5screate_simple_f(1, dims, dspace_id, hdf_err)
  !
  ! Create the dataset.
  !
  CALL H5Dcreate_f(file_id, dset_name,  sample_type_id, dspace_id, dset_id, hdf_err)
  !
  ! Write data to the dataset
  !
  f_ptr = C_LOC(samples(1))
  CALL H5Dwrite_f(dset_id, sample_type_id, f_ptr, hdf_err)
  ! Close up
  CALL h5dclose_f(dset_id, hdf_err)
  CALL h5sclose_f(dspace_id, hdf_err)
  CALL h5fclose_f(file_id, hdf_err)
  CALL h5close_f(hdf_err)

End Program Main