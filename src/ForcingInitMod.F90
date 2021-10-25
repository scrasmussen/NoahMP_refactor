module ForcingInitMod
!!! Initialize column (1-D) Noah-MP forcing variables
!!! Forcing variables should be first defined in ForcingType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine ForcingInitDefault(noahmp)

    type(noahmp_type), intent(inout) :: noahmp

    noahmp%forcing%UU       = huge(1.0)
    noahmp%forcing%VV       = huge(1.0)
    noahmp%forcing%Q2       = huge(1.0)
    noahmp%forcing%SFCTMP   = huge(1.0)
    noahmp%forcing%SOLDN    = huge(1.0)
    noahmp%forcing%LWDN     = huge(1.0)
    noahmp%forcing%LWDN     = huge(1.0)
    noahmp%forcing%SFCPRS   = huge(1.0)
    noahmp%forcing%PRCPCONV = huge(1.0)
    noahmp%forcing%PRCPNONC = huge(1.0)
    noahmp%forcing%PRCPSHCV = huge(1.0)
    noahmp%forcing%PRCPSNOW = huge(1.0)
    noahmp%forcing%PRCPGRPL = huge(1.0)
    noahmp%forcing%PRCPHAIL = huge(1.0)
    noahmp%forcing%QC       = huge(1.0)
    noahmp%forcing%QSFC     = huge(1.0)
    noahmp%forcing%PSFC     = huge(1.0)
    noahmp%forcing%TBOT     = huge(1.0)

  end subroutine ForcingInitDefault

!=== initialize with input data or table values
  subroutine ForcingInitTransfer(noahmp, input)

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type),  intent(in)    :: input

    associate(                                   &
              iloc => noahmp%config%domain%iloc ,&
              jloc => noahmp%config%domain%jloc ,&   
              KTS  => input%KTS                 ,&
              KTE  => input%KTE                  &
             )

    noahmp%forcing%UU       = input%U_PHY (iloc,1,jloc)
    noahmp%forcing%VV       = input%V_PHY (iloc,1,jloc)
    noahmp%forcing%Q2       = input%QV3D (iloc,1,jloc)/(1.0+input%QV3D(iloc,1,jloc))  
    noahmp%forcing%SFCTMP   = input%T3D (iloc,1,jloc)
    noahmp%forcing%SOLDN    = input%SWDOWN (iloc,jloc)
    noahmp%forcing%LWDN     = input%GLW (iloc,jloc)
    noahmp%forcing%SFCPRS   = (input%P8W3D(iloc,KTS+1,jloc)+input%P8W3D(iloc,KTS,jloc))*0.5
    noahmp%forcing%PRCPCONV = input%MP_RAINC (iloc,jloc)
    noahmp%forcing%PRCPNONC = input%MP_RAINNC (iloc,jloc)
    noahmp%forcing%PRCPSHCV = input%MP_SHCV (iloc,jloc)
    noahmp%forcing%PRCPSNOW = input%MP_SNOW (iloc,jloc)
    noahmp%forcing%PRCPGRPL = input%MP_GRAUP (iloc,jloc)
    noahmp%forcing%PRCPHAIL = input%MP_HAIL (iloc,jloc)
    noahmp%forcing%QSFC     = input%QSFC (iloc,jloc)
    noahmp%forcing%PSFC     = input%P8W3D(iloc,1,jloc)
    noahmp%forcing%TBOT     = input%TMN (iloc,jloc)

    endassociate
 
  end subroutine ForcingInitTransfer

end module ForcingInitMod
