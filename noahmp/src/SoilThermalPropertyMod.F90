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
    real(kind=kind_noahmp), allocatable, dimension(:) :: SICE_TMP   ! temporal soil ice

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     maximum number of soil layers
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              CSOIL           => noahmp%energy%param%CSOIL           ,& ! in,     soil volumetric specific heat (j/m3/k)
              QUARTZ          => noahmp%energy%param%QUARTZ          ,& ! in,     soil quartz content
              SMC             => noahmp%water%state%SMC              ,& ! in,     total soil moisture [m3/m3]
              SH2O            => noahmp%water%state%SH2O             ,& ! in,     soil water content [m3/m3] 
              CVSOIL          => noahmp%energy%state%CVSOIL          ,& ! out,    soil layer volumetric specific heat (j/m3/k)
              TKSOIL          => noahmp%energy%state%TKSOIL           & ! out,    soil layer thermal conductivity (w/m/k)
             )
! ----------------------------------------------------------------------

    ! initiazliation
    allocate( SICE_TMP(1:NSOIL) )
    SICE_TMP(:) = 0.0
    THKW = 0.57              ! water thermal conductivity
    THKO = 2.0               ! thermal conductivity for other soil components
    THKQTZ = 7.7             ! thermal conductivity for quartz

    do ISOIL = 1, NSOIL

       ! ==== soil heat capacity

        SICE_TMP(ISOIL) = SMC(ISOIL) - SH2O(ISOIL)
        CVSOIL(ISOIL)   = SH2O(ISOIL) * CWAT + (1.0 - SMCMAX(ISOIL)) * CSOIL &
                          + (SMCMAX(ISOIL) - SMC(ISOIL)) * CPAIR + SICE_TMP(ISOIL) * CICE

       ! ==== soil thermal conductivity

       SATRATIO = SMC(ISOIL) / SMCMAX(ISOIL) ! SATURATION RATIO
       ! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
       THKS = (THKQTZ ** QUARTZ(ISOIL)) * (THKO ** (1.0 - QUARTZ(ISOIL)))
       ! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
       XUNFROZ = 1.0    ! Prevent divide by zero (suggested by D. Mocko)
       if ( SMC(ISOIL) > 0.0 ) XUNFROZ = SH2O(ISOIL) / SMC(ISOIL)
       XU = XUNFROZ * SMCMAX(ISOIL)
       ! SATURATED THERMAL CONDUCTIVITY
       THKSAT = THKS ** (1.0-SMCMAX(ISOIL)) * TKICE ** (SMCMAX(ISOIL)-XU) * THKW ** (XU)
       ! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
       GAMMD  = (1.0 - SMCMAX(ISOIL)) * 2700.0
       THKDRY = (0.135 * GAMMD + 64.7) / (2700.0 - 0.947 * GAMMD)
       ! THE KERSTEN NUMBER AKE
       if ( (SH2O(ISOIL)+0.0005) < SMC(ISOIL) ) then ! FROZEN 
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
