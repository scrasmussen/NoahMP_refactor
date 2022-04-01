module SnowpackHydrologyGlacierMod

!!! Snowpack hydrology processes (sublimation/frost, evaporation/dew, meltwater)
!!! Update snowpack ice and liquid water content

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SnowLayerCombineGlacierMod, only : SnowLayerCombineGlacier

  implicit none

contains

  subroutine SnowpackHydrologyGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWH2O_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: J         ! do loop/array indices
    real(kind=kind_noahmp) :: QIN       ! water flow into the element (mm/s)
    real(kind=kind_noahmp) :: QOUT      ! water flow out of the element (mm/s)
    real(kind=kind_noahmp) :: WGDIF     ! ice mass after minus sublimation
    real(kind=kind_noahmp) :: PROPOR    ! ratio of SWE after frost & sublimation to original SWE
    real(kind=kind_noahmp) :: TEMP      ! temporary SWE
    real(kind=kind_noahmp), allocatable, dimension(:) :: VOL_LIQ   ! partial volume of liquid water in layer
    real(kind=kind_noahmp), allocatable, dimension(:) :: VOL_ICE   ! partial volume of ice lens in layer

! --------------------------------------------------------------------
    associate(                                                        &
              OPT_GLA         => noahmp%config%nmlist%OPT_GLA        ,& ! in,     option for glacier treatment
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,     maximum number of snow layers
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              FSH             => noahmp%energy%flux%FSH              ,& ! in,     total sensible heat (w/m2) [+ to atm]
              QSNFRO          => noahmp%water%flux%QSNFRO            ,& ! in,     snow surface frost rate[mm/s]
              QSNSUB          => noahmp%water%flux%QSNSUB            ,& ! in,     snow surface sublimation rate[mm/s]
              QRAIN           => noahmp%water%flux%QRAIN             ,& ! in,     snow surface rain rate[mm/s]
              SNLIQMAXFRAC    => noahmp%water%param%SNLIQMAXFRAC     ,& ! in,     maximum liquid water fraction in snow
              SSI             => noahmp%water%param%SSI              ,& ! in,     liquid water holding capacity for snowpack (m3/m3)
              SNOW_RET_FAC    => noahmp%water%param%SNOW_RET_FAC     ,& ! in,     snowpack water release timescale factor (1/s)
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/soil layers (m)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
              SNICE           => noahmp%water%state%SNICE            ,& ! inout,  snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout,  snow layer liquid water [mm]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil liquid moisture (m3/m3)
              SICE            => noahmp%water%state%SICE             ,& ! inout,  soil ice moisture (m3/m3)
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              PONDING1        => noahmp%water%state%PONDING1         ,& ! out,    surface ponding 1 (mm)
              PONDING2        => noahmp%water%state%PONDING2         ,& ! out,    surface ponding 2 (mm)
              EPORE           => noahmp%water%state%EPORE_SNOW       ,& ! out,    snow effective porosity (m3/m3)
              QSNBOT          => noahmp%water%flux%QSNBOT             & ! out,    melting water out of snow bottom [mm/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( VOL_LIQ(-NSNOW+1:0) )
    allocate( VOL_ICE(-NSNOW+1:0) )
    VOL_LIQ(:) = 0.0
    VOL_ICE(:) = 0.0
    EPORE  (:) = 0.0
    QSNBOT     = 0.0
    QIN        = 0.0
    QOUT       = 0.0

    ! for the case when SNEQV becomes '0' after 'COMBINE'
    if ( SNEQV == 0.0 ) then
       if ( OPT_GLA == 1 ) then
          SICE(1) =  SICE(1) + (QSNFRO - QSNSUB) * DT / (DZSNSO(1)*1000.0)  ! Barlage: SH2O->SICE v3.6
       elseif ( OPT_GLA == 2 ) then
          FSH    = FSH - (QSNFRO - QSNSUB) * HSUB
          QSNFRO = 0.0
          QSNSUB = 0.0
       endif
    endif

    ! for shallow snow without a layer
    ! snow surface sublimation may be larger than existing snow mass. To conserve water,
    ! excessive sublimation is used to reduce soil water. Smaller time steps would tend to aviod this problem.
    if ( (ISNOW == 0) .and. (SNEQV > 0.0) ) then
       if ( OPT_GLA == 1 ) then
          TEMP   = SNEQV
          SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
          PROPOR = SNEQV / TEMP
          SNOWH  = max( 0.0, PROPOR*SNOWH )
          SNOWH  = min( max(SNOWH, SNEQV/500.0), SNEQV/50.0 )  ! limit adjustment to a reasonable density
       elseif ( OPT_GLA == 2 ) then
          FSH = FSH - (QSNFRO - QSNSUB) * HSUB
          QSNFRO = 0.0
          QSNSUB = 0.0
       endif
       if ( SNEQV < 0.0 ) then
          SICE(1) = SICE(1) + SNEQV / (DZSNSO(1)*1000.0)
          SNEQV   = 0.0
          SNOWH   = 0.0
       endif
       if ( SICE(1) < 0.0 ) then
          SH2O(1) = SH2O(1) + SICE(1)
          SICE(1) = 0.0
       endif
    endif

    if ( (SNOWH <= 1.0e-8) .or. (SNEQV <= 1.0e-6) ) then
       SNOWH = 0.0
       SNEQV = 0.0
    endif

    ! for multi-layer (>=1) snow
    if ( ISNOW < 0 ) then
      WGDIF          = SNICE(ISNOW+1) - QSNSUB*DT + QSNFRO*DT
      SNICE(ISNOW+1) = WGDIF
      if ( (WGDIF < 1.0e-6) .and. (ISNOW < 0) ) call SnowLayerCombineGlacier(noahmp)
      if ( ISNOW < 0 ) then
         SNLIQ(ISNOW+1) = SNLIQ(ISNOW+1) + QRAIN * DT
         SNLIQ(ISNOW+1) = max( 0.0, SNLIQ(ISNOW+1) )
      endif
    endif

    ! Porosity and partial volume
    do J = ISNOW+1, 0
       VOL_ICE(J) = min( 1.0, SNICE(J)/(DZSNSO(J)*DENICE) )
       EPORE(J)   = 1.0 - VOL_ICE(J)
    enddo

    ! compute inter-layer snow water flow
    do J = ISNOW+1, 0
       SNLIQ(J)   = SNLIQ(J) + QIN
       VOL_LIQ(J) = SNLIQ(J) / (DZSNSO(J)*DENH2O)
       QOUT       = max( 0.0, (VOL_LIQ(J) - SSI*EPORE(J)) * DZSNSO(J) )
       if ( J == 0 ) then
          QOUT = max( (VOL_LIQ(J) - EPORE(J)) * DZSNSO(J), SNOW_RET_FAC * DT * QOUT )
       endif
       QOUT     = QOUT * DENH2O
       SNLIQ(J) = SNLIQ(J) - QOUT
       if ( ( SNLIQ(J) / (SNICE(J)+SNLIQ(J)) ) > SNLIQMAXFRAC ) then
          QOUT     = QOUT + ( SNLIQ(J) - SNLIQMAXFRAC/(1.0-SNLIQMAXFRAC) * SNICE(J) )
          SNLIQ(J) = SNLIQMAXFRAC / (1.0 - SNLIQMAXFRAC) * SNICE(J)
       endif
       QIN = QOUT
    enddo

    ! update snow depth
    do J = ISNOW+1, 0
       DZSNSO(J) = max( DZSNSO(J), SNLIQ(J)/DENH2O + SNICE(J)/DENICE )
    enddo

    ! Liquid water from snow bottom to soil (mm/s)
    QSNBOT = QOUT / DT

    end associate

  end subroutine SnowpackHydrologyGlacier

end module SnowpackHydrologyGlacierMod
