module InputInit
!!! Initialize Noah-MP input variables (2D forcing, namelist, table, static)
!!! Input variables should be first defined in InputType.f90

  use Machine, only : kind_noahmp
  use InputType

  implicit none

contains

!=== read Noahmp Table values
  subroutine ReadNoahmpTable(input)

    type(input_type), intent(inout)  :: input

  !=== declare temporary variable to store NoahmpTable values
    !--------------------!
    !  soil parameters   !
    !--------------------!
    integer  :: num_soil_types
    real(kind=kind_noahmp), allocatable, dimension(:) :: BB  ! b parameter

    !-----------------------!
    ! Vegetation parameters !
    !-----------------------!
    integer  :: NVEG 
    real(kind=kind_noahmp), allocatable, dimension(:)  ::   LAI_JAN

  !=== arrange structures for reading NoahmpTable.TBL
    namelist / noahmp_modis_veg_categories /  NVEG
    namelist / noahmp_stas_soil_categories / num_soil_types
    namelist / noahmp_soil_stas_parameters /  BB
    namelist / noahmp_modis_parameters / LAI_JAN

    !---------------------------------------------------------------
    !  read NoahmpTable, part 1 for scalars & variable dimension info
    !---------------------------------------------------------------
    open(30, file="NoahmpTable.TBL", form="formatted")
     read(30, noahmp_modis_veg_categories)
     read(30, noahmp_stas_soil_categories)
    close(30)

    allocate( BB(1:num_soil_types) )
    allocate( LAI_JAN(1:NVEG)      ) 

    !--------------------------------------------------
    !  read NoahmpTable, part 2 for vectorized variable
    !--------------------------------------------------
    open(30, file="NoahmpTable.TBL", form="formatted")
     read(30, noahmp_soil_stas_parameters)
     read(30, noahmp_modis_parameters    )
    close(30)

  !=== transfer table values to input variables
    allocate (input%BB(1:num_soil_types))
    allocate (input%LAI_JAN(1:NVEG))

    input%nsoiltype = num_soil_types
    input%nvegtype  = nveg
    input%BB        = BB
    input%LAI_JAN   = LAI_JAN

  end subroutine ReadNoahmpTable

!=== read namelist values
  subroutine ReadNamelist(input)

    type(input_type), intent(inout)  :: input

  !=== declare temporary variable to store namelist values
    character*256 :: output_filename
    integer  ::  nsoil
    integer  ::  dynamic_vegetation_option
    real(kind=kind_noahmp)  :: dt
    real(kind=kind_noahmp), allocatable, dimension(:)  :: zsoil
    
  !=== arrange structures for reading namelist.input
    namelist / NOAHLSM_OFFLINE /  output_filename,nsoil,dt,zsoil,&
                                  dynamic_vegetation_option


!!!!!!! May need to re-structure the current namelist.input to allow multiple "&" sections similar to NoahmpTable.TBL
    !---------------------------------------------------------------
    ! read namelist.input, part 1 for scalars & variable dimension info
    !---------------------------------------------------------------
    open(30, file="namelist.input", form="formatted")
     read(30, NOAHLSM_OFFLINE )
    close(30)

    allocate( zsoil(1:nsoil) )

    !--------------------------------------------------
    ! read namelist.input, part 2 for vectorized variable
    !--------------------------------------------------
    open(30, file="namelist.input", form="formatted")
     read(30, xxxxx    )
    close(30)

  !=== transfer table values to input variables
    allocate(input%zsoil(1:nsoil))

    input%nsoil     = nsoil
    input%output_filename  = output_filename
    input%zsoil     = zsoil
    input%dynamic_vegetation_option = dynamic_vegetation_option

  end subroutine ReadNamelist

!=== read input forcing data
  subroutine ReadInputForcing(input)

    use netcdf
    use error_handling, only : handle_err

    type(input_type), intent(inout)  :: input

    integer :: ncid, dimid, varid, status

  !=== read in forcing netcdf files
    status = nf90_open(input%forcfilename, NF90_NOWRITE, ncid)
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inq_varid(ncid, "time", varid)
    status = nf90_get_var(ncid, varid , now_time)
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inq_varid(ncid, "west_east", varid)
    status = nf90_get_var(ncid, varid , input%nx)
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inq_varid(ncid, "south_north", varid)
    status = nf90_get_var(ncid, varid , input%ny)
    if (status /= nf90_noerr) call handle_err(status)

    allocate( input%U2D(input%nx,input%ny) )
    status = nf90_inq_varid(ncid, "U2D", varid)
    status = nf90_get_var(ncid, varid , input%U2D, start=(/1,1/), count=(/input%nx,input%ny/))
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_close(ncid)
    if (status /= nf90_noerr) call handle_err(status)

  end subroutine ReadInputForcing

!=== read input setup data (static & initial)
  subroutine ReadInputSetup(input)

    use netcdf
    use error_handling, only : handle_err

    type(input_type), intent(inout)  :: input

    integer :: ncid, dimid, varid, status

  !=== read in setup netcdf files (including static & initial data)
    status = nf90_open(input%setupfilename, NF90_NOWRITE, ncid)
    if (status /= nf90_noerr) call handle_err(status)
    
    status = nf90_inq_varid(ncid, "time", varid)
    status = nf90_get_var(ncid, varid , now_time)
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inq_varid(ncid, "west_east", varid)
    status = nf90_get_var(ncid, varid , input%nx)
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inq_varid(ncid, "south_north", varid)
    status = nf90_get_var(ncid, varid , input%ny)
    if (status /= nf90_noerr) call handle_err(status)

    allocate( input%SHDMAX(input%nx,input%ny) )
    status = nf90_inq_varid(ncid, "SHDMAX", varid)
    status = nf90_get_var(ncid, varid , input%SHDMAX, start=(/1,1/), count=(/input%nx,input%ny/))
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_close(ncid)
    if (status /= nf90_noerr) call handle_err(status)

  end subroutine ReadInputSetup

!=== read input restart data
  subroutine ReadInputRestart(input)

    use netcdf
    use error_handling, only : handle_err

    type(input_type), intent(inout)  :: input

    integer :: ncid, dimid, varid, status

  !=== read in restart netcdf files
    status = nf90_open(input%restartfilename, NF90_NOWRITE, ncid)
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inq_varid(ncid, "time", varid)
    status = nf90_get_var(ncid, varid , now_time)
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inq_varid(ncid, "west_east", varid)
    status = nf90_get_var(ncid, varid , input%nx)
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inq_varid(ncid, "south_north", varid)
    status = nf90_get_var(ncid, varid , input%ny)
    if (status /= nf90_noerr) call handle_err(status)

    allocate( input%SWE(input%nx,input%ny) )
    status = nf90_inq_varid(ncid, "SWE", varid)
    status = nf90_get_var(ncid, varid , input%SWE, start=(/1,1/), count=(/input%nx,input%ny/))
    if (status /= nf90_noerr) call handle_err(status)

    status = nf90_close(ncid)
    if (status /= nf90_noerr) call handle_err(status)

  end subroutine ReadInputRestart

!=== initialize with default values
  subroutine InputInitDefault(input)

    type(input_type), intent(inout) :: input

    allocate( input%U2D(input%nx,input%ny) ) ; input%U2D (:,:) = huge(1.0)

  end subroutine InputInitDefault

end module InputInit
