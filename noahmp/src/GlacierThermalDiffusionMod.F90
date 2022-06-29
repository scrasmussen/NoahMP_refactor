module GlacierThermalDiffusionMod

!!! Solve glacier ice and snow layer thermal diffusion
!!! Calculate the right hand side of the time tendency term of the glacier
!!! and snow thermal diffusion equation. Currently snow and glacier ice layers
!!! are coupled in solving the equations. Also compute/prepare the matrix
!!! coefficients for the tri-diagonal matrix of the implicit time scheme.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GlacierThermalDiffusion(noahmp, AI, BI, CI, RHSTS)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: HRT_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: RHSTS  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: AI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: BI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: CI     ! left-hand side term of the matrix

! local variable
    integer                :: K         ! loop index
    real(kind=kind_noahmp) :: TEMP1     ! temporary variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: DDZ     ! temporary variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: DZ      ! temporary variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: DENOM   ! temporary variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: DTSDZ   ! temporary variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: EFLUX   ! temporary variable

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,    number of soil layers
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,    maximum number of snow layers
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! in,    actual number of snow layers
              ZSNSO           => noahmp%config%domain%ZSNSO          ,& ! in,    depth of snow/soil layer-bottom (m)
              OptSoilTemperatureBottom => noahmp%config%nmlist%OptSoilTemperatureBottom,& ! in,    options for lower boundary condition of soil temperature
              OptSnowSoilTempTime => noahmp%config%nmlist%OptSnowSoilTempTime,& ! in,    options for snow/soil temperature time scheme
              TemperatureSoilBottom => noahmp%forcing%TemperatureSoilBottom,& ! in,    bottom boundary soil temperature [K]
              ZBOT            => noahmp%energy%state%ZBOTSNO         ,& ! in,    depth of lower boundary condition (m) from snow surface
              STC             => noahmp%energy%state%STC             ,& ! in,    snow and soil layer temperature [K]
              DF              => noahmp%energy%state%DF              ,& ! in,    thermal conductivity [w/m/k] for all soil & snow
              HCPCT           => noahmp%energy%state%HCPCT           ,& ! in,    heat capacity [j/m3/k] for all soil & snow
              SSOIL           => noahmp%energy%flux%SSOIL            ,& ! in,    soil heat flux (w/m2) [+ to soil]
              PHI             => noahmp%energy%flux%PHI              ,& ! in,    light penetrating through soil/snow water (W/m2)
              BOTFLX          => noahmp%energy%flux%EFLXB             & ! out,   energy influx from soil bottom (w/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( DDZ   (-NSNOW+1:NSOIL) )
    allocate( DZ    (-NSNOW+1:NSOIL) )
    allocate( DENOM (-NSNOW+1:NSOIL) )
    allocate( DTSDZ (-NSNOW+1:NSOIL) )
    allocate( EFLUX (-NSNOW+1:NSOIL) )
    RHSTS(:) = 0.0
    AI(:)    = 0.0
    BI(:)    = 0.0
    CI(:)    = 0.0
    DDZ(:)   = 0.0
    DZ(:)    = 0.0
    DENOM(:) = 0.0
    DTSDZ(:) = 0.0
    EFLUX(:) = 0.0

    ! compute gradient and flux of glacier/snow thermal diffusion
    do K = ISNOW+1, NSOIL
       if ( K == (ISNOW+1) ) then
          DENOM(K) = - ZSNSO(K) * HCPCT(K)
          TEMP1    = - ZSNSO(K+1)
          DDZ(K)   = 2.0 / TEMP1
          DTSDZ(K) = 2.0 * (STC(K) - STC(K+1)) / TEMP1
          EFLUX(K) = DF(K) * DTSDZ(K) - SSOIL - PHI(K)
       elseif ( K < NSOIL ) then
          DENOM(K) = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
          TEMP1    = ZSNSO(K-1) - ZSNSO(K+1)
          DDZ(K)   = 2.0 / TEMP1
          DTSDZ(K) = 2.0 * (STC(K) - STC(K+1)) / TEMP1
          EFLUX(K) = ( DF(K)*DTSDZ(K) - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
       elseif ( K == NSOIL ) then
          DENOM(K) = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
          TEMP1    =  ZSNSO(K-1) - ZSNSO(K)
          if ( OptSoilTemperatureBottom == 1 ) then
             BOTFLX = 0.0
          endif
          if ( OptSoilTemperatureBottom == 2 ) then
             DTSDZ(K) = (STC(K) - TemperatureSoilBottom) / (0.5 * (ZSNSO(K-1)+ZSNSO(K)) - ZBOT)
             BOTFLX   = -DF(K) * DTSDZ(K)
          endif
          EFLUX(K) = ( -BOTFLX - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
       endif
    enddo

    ! prepare the matrix coefficients for the tri-diagonal matrix
    do K = ISNOW+1, NSOIL
       if ( K == (ISNOW+1) ) then
          AI(K) =   0.0
          CI(K) = - DF(K)   * DDZ(K) / DENOM(K)
          if ( (OptSnowSoilTempTime == 1) .or. (OptSnowSoilTempTime == 3) ) then
             BI(K) = - CI(K)
          endif
          if ( OptSnowSoilTempTime == 2 ) then
             BI(K) = - CI(K) + DF(K) / ( 0.5*ZSNSO(K)*ZSNSO(K)*HCPCT(K) )
          endif
       elseif ( K < NSOIL ) then
          AI(K) = - DF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K) = - DF(K  ) * DDZ(K  ) / DENOM(K)
          BI(K) = - (AI(K) + CI (K))
       elseif ( K == NSOIL ) then
          AI(K) = - DF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K) = 0.0
          BI(K) = - (AI(K) + CI(K))
       endif
          RHSTS(K) = EFLUX(K) / (-DENOM(K))
    enddo

    end associate

  end subroutine GlacierThermalDiffusion

end module GlacierThermalDiffusionMod
