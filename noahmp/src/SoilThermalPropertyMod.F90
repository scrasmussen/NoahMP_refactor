module SoilThermalPropertyMod

!!! Compute soil thermal conductivity based on Peters-Lidard et al. (1998)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilThermalProperty(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: TDFCND
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! If the soil has any moisture content compute a partial sum/product
! otherwise use a constant value which works well with most soils
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: ISOIL         ! loop index
    real(kind=kind_noahmp) :: AKE, GAMMD    ! temporary vars
    real(kind=kind_noahmp) :: THKDRY        ! thermal conductivity for dry soil
    real(kind=kind_noahmp) :: THKO          ! thermal conductivity for other soil components 
    real(kind=kind_noahmp) :: THKQTZ        ! thermal conductivity for quartz
    real(kind=kind_noahmp) :: THKSAT        ! thermal conductivity for saturated soil
    real(kind=kind_noahmp) :: THKS          ! thermal conductivity for the solids
    real(kind=kind_noahmp) :: THKW          ! water thermal conductivity
    real(kind=kind_noahmp) :: SATRATIO      ! saturation ratio
    real(kind=kind_noahmp) :: XU            ! soil water fraction
    real(kind=kind_noahmp) :: XUNFROZ       ! soil water fraction
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilIceTmp   ! temporal soil ice

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              SoilMoistureSat          => noahmp%water%param%SoilMoistureSat           ,& ! in,     saturated value of soil moisture [m3/m3]
              CSOIL           => noahmp%energy%param%CSOIL           ,& ! in,     soil volumetric specific heat (j/m3/k)
              SoilQuartzFrac          => noahmp%energy%param%SoilQuartzFrac          ,& ! in,     soil quartz content
              SoilMoisture            => noahmp%water%state%SoilMoisture       ,& ! in,     total soil moisture [m3/m3]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,     soil water content [m3/m3] 
              CVSOIL          => noahmp%energy%state%CVSOIL          ,& ! out,    soil layer volumetric specific heat (j/m3/k)
              TKSOIL          => noahmp%energy%state%TKSOIL           & ! out,    soil layer thermal conductivity (w/m/k)
             )
! ----------------------------------------------------------------------

    ! initiazliation
    allocate( SoilIceTmp(1:NumSoilLayer) )
    SoilIceTmp(:) = 0.0
    THKW = 0.57              ! water thermal conductivity
    THKO = 2.0               ! thermal conductivity for other soil components
    THKQTZ = 7.7             ! thermal conductivity for quartz

    do ISOIL = 1, NumSoilLayer

       ! ==== soil heat capacity

       SoilIceTmp(ISOIL) = SoilMoisture(ISOIL) - SoilLiqWater(ISOIL)
       CVSOIL(ISOIL)   = SoilLiqWater(ISOIL) * ConstHeatCapacWater + (1.0 - SoilMoistureSat(ISOIL)) * CSOIL + &
                         (SoilMoistureSat(ISOIL) - SoilMoisture(ISOIL)) * ConstHeatCapacAir + &
                         SoilIceTmp(ISOIL) * ConstHeatCapacIce

       ! ==== soil thermal conductivity

       SATRATIO = SoilMoisture(ISOIL) / SoilMoistureSat(ISOIL) ! SATURATION RATIO
       ! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
       THKS = (THKQTZ ** SoilQuartzFrac(ISOIL)) * (THKO ** (1.0 - SoilQuartzFrac(ISOIL)))
       ! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
       XUNFROZ = 1.0    ! Prevent divide by zero (suggested by D. Mocko)
       if ( SoilMoisture(ISOIL) > 0.0 ) XUNFROZ = SoilLiqWater(ISOIL) / SoilMoisture(ISOIL)
       XU = XUNFROZ * SoilMoistureSat(ISOIL)
       ! SATURATED THERMAL CONDUCTIVITY
       THKSAT = THKS ** (1.0-SoilMoistureSat(ISOIL)) * ConstThermConductIce ** (SoilMoistureSat(ISOIL)-XU) * THKW ** (XU)
       ! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
       GAMMD  = (1.0 - SoilMoistureSat(ISOIL)) * 2700.0
       THKDRY = (0.135 * GAMMD + 64.7) / (2700.0 - 0.947 * GAMMD)
       ! THE KERSTEN NUMBER AKE
       if ( (SoilLiqWater(ISOIL)+0.0005) < SoilMoisture(ISOIL) ) then ! FROZEN 
          AKE = SATRATIO
       else  ! UNFROZEN
          ! KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT
          ! LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)
          ! (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).
          if ( SATRATIO > 0.1 ) then
             AKE = log10(SATRATIO) + 1.0
          else
             AKE = 0.0
          endif
       endif

       !  THERMAL CONDUCTIVITY
       TKSOIL(ISOIL) = AKE * (THKSAT - THKDRY) + THKDRY

    enddo ! ISOIL

    end associate

  end subroutine SoilThermalProperty

end module SoilThermalPropertyMod
