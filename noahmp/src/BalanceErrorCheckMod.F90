module BalanceErrorCheckMod

!!! Check water and energy balance and report error

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

!!!! Water balance check initialization
  subroutine BalanceWaterInit(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in NOAHMP_SFLX)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                      :: IZ      ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,    number of soil layers
              SurfaceType             => noahmp%config%domain%SurfaceType            ,& ! in,    surface type 1-soil; 2-lake
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,    thickness of snow/soil layers (m)
              CanopyLiqWater          => noahmp%water%state%CanopyLiqWater           ,& ! in,    canopy intercepted liquid water [mm]
              CanopyIce          => noahmp%water%state%CanopyIce           ,& ! in,    canopy intercepted ice [mm]
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! in,    snow water equivalent [mm]
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,    total soil moisture [m3/m3]
              WaterStorageAquifer    => noahmp%water%state%WaterStorageAquifer  ,& ! in,    water storage in aquifer [mm]
              WaterStorageTotBeg          => noahmp%water%state%WaterStorageTotBeg            & ! out,   total water storage at the beginning
             )
! ----------------------------------------------------------------------

    ! compute total water storage before NoahMP processes
    if ( SurfaceType == 1 ) then  ! soil
       WaterStorageTotBeg = CanopyLiqWater + CanopyIce + SnowWaterEquiv + WaterStorageAquifer
       do IZ = 1, NumSoilLayer
          WaterStorageTotBeg = WaterStorageTotBeg + SoilMoisture(IZ) * ThicknessSnowSoilLayer(IZ) * 1000.0
       enddo
    endif

    end associate

  end subroutine BalanceWaterInit


!!!! Water balance check and report error
  subroutine BalanceWaterCheck(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ERROR
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                      :: IZ      ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,    number of soil layers
              SurfaceType             => noahmp%config%domain%SurfaceType            ,& ! in,    surface type 1-soil; 2-lake
              GridIndexI      => noahmp%config%domain%GridIndexI     ,& ! in,    grid index in x-direction
              GridIndexJ      => noahmp%config%domain%GridIndexJ     ,& ! in,    grid index in y-direction
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,    thickness of snow/soil layers (m)
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              FlagCropland          => noahmp%config%domain%FlagCropland         ,& ! in,    flag to identify croplands
              IrriFracThreshold        => noahmp%water%param%IrriFracThreshold         ,& ! in,    irrigation fraction parameter
              IrrigationFracGrid          => noahmp%water%state%IrrigationFracGrid           ,& ! in,    total input irrigation fraction
              WaterTableDepth             => noahmp%water%state%WaterTableDepth              ,& ! in,    water table depth [m]
              CanopyLiqWater          => noahmp%water%state%CanopyLiqWater           ,& ! in,    canopy intercepted liquid water [mm]
              CanopyIce          => noahmp%water%state%CanopyIce           ,& ! in,    canopy intercepted ice [mm]
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! in,    snow water equivalent [mm]
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,    total soil moisture [m3/m3]
              WaterStorageAquifer  => noahmp%water%state%WaterStorageAquifer  ,& ! in,    water storage in aquifer [mm]
              WaterStorageTotBeg          => noahmp%water%state%WaterStorageTotBeg           ,& ! in,    total water storage at the beginning
              PrecipTotRefHeight            => noahmp%water%flux%PrecipTotRefHeight              ,& ! in,    total precipitation [mm/s] at reference height
              EvapCanopyNet            => noahmp%water%flux%EvapCanopyNet              ,& ! in,    evaporation of intercepted water [mm/s]
              Transpiration           => noahmp%water%flux%Transpiration             ,& ! in,    transpiration rate [mm/s]
              EvapSoilNet            => noahmp%water%flux%EvapSoilNet              ,& ! in,    net direct soil evaporation (mm/s)
              RunoffSurface          => noahmp%water%flux%RunoffSurface            ,& ! in,    surface runoff [mm/s]
              RunoffSubsurface          => noahmp%water%flux%RunoffSubsurface            ,& ! in,    subsurface runoff [mm/s]
              TileDrain          => noahmp%water%flux%TileDrain            ,& ! in,    tile drainage (mm/s)
              IrrigationRateSprinkler        => noahmp%water%flux%IrrigationRateSprinkler          ,& ! in,    rate of irrigation by sprinkler [m/timestep]
              IrrigationRateMicro        => noahmp%water%flux%IrrigationRateMicro          ,& ! in,    micro irrigation water rate [m/timestep]
              IrrigationRateFlood        => noahmp%water%flux%IrrigationRateFlood          ,& ! in,    flood irrigation water rate [m/timestep]
              WaterStorageTotEnd          => noahmp%water%state%WaterStorageTotEnd           ,& ! out,   total water storage at the end
              WaterBalanceError          => noahmp%water%state%WaterBalanceError            & ! out,   water balance error (mm) per time step
             )
! ----------------------------------------------------------------------

    ! before water balance check add irrigation water to precipitation
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IrriFracThreshold) ) then
       PrecipTotRefHeight = PrecipTotRefHeight + &
                           (IrrigationRateSprinkler + IrrigationRateMicro + IrrigationRateFlood)*1000.0 / MainTimeStep  ! irrigation
    endif

    ! Error in water balance should be < 0.1 mm
    if ( SurfaceType == 1 ) then        !soil
       WaterStorageTotEnd = CanopyLiqWater + CanopyIce + SnowWaterEquiv + WaterStorageAquifer
       do IZ = 1, NumSoilLayer
          WaterStorageTotEnd = WaterStorageTotEnd + SoilMoisture(IZ) * ThicknessSnowSoilLayer(IZ) * 1000.0
       enddo
       WaterBalanceError = WaterStorageTotEnd - WaterStorageTotBeg - (PrecipTotRefHeight - EvapCanopyNet - &
                           Transpiration - EvapSoilNet - RunoffSurface - RunoffSubsurface - TileDrain) * MainTimeStep
#ifndef WRF_HYDRO
       if ( abs(WaterBalanceError) > 0.1 ) then
          if ( WaterBalanceError > 0) then
             !call wrf_message ('The model is gaining water (WaterBalanceError is positive)')
             write(*,*) "The model is gaining water (WaterBalanceError is positive)"
          else
             !call wrf_message('The model is losing water (WaterBalanceError is negative)')
             write(*,*) "The model is losing water (WaterBalanceError is negative)"
          endif
          write(*,*) 'WaterBalanceError =',WaterBalanceError, "kg m{-2} timestep{-1}"
          !call wrf_message(trim(message))
          write(*, &
               '("  GridIndexI    GridIndexJ   WaterStorageTotEnd   WaterStorageTotBeg     PrecipTotRefHeight &
                    EvapCanopyNet     EvapSoilNet    Transpiration   RunoffSurface   RunoffSubsurface   &
                    WaterTableDepth   TileDrain")')
          !call wrf_message(trim(message))
          write(*,'(i6,1x,i6,1x,2f15.3,9f11.5)') GridIndexI,GridIndexJ,WaterStorageTotEnd,WaterStorageTotBeg,&
                                                 PrecipTotRefHeight*MainTimeStep,EvapCanopyNet*MainTimeStep,&
                                                 EvapSoilNet*MainTimeStep,Transpiration*MainTimeStep,&
                                                 RunoffSurface*MainTimeStep,RunoffSubsurface*MainTimeStep,&
                                                 WaterTableDepth,TileDrain*MainTimeStep
          !call wrf_message(trim(message))
          !call wrf_error_fatal("Water budget problem in NOAHMP LSM")
          stop "Error"
       endif
#endif
    else                 !KWM
       WaterBalanceError = 0.0      !KWM
    endif

    end associate

  end subroutine BalanceWaterCheck


!!!! Energy balance check and error report
  subroutine BalanceEnergyCheck(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ERROR
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                      :: IZ      ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              GridIndexI      => noahmp%config%domain%GridIndexI     ,& ! in,    grid index in x-direction
              GridIndexJ      => noahmp%config%domain%GridIndexJ     ,& ! in,    grid index in y-direction
              VegFrac            => noahmp%energy%state%VegFrac            ,& ! in,    greeness vegetation fraction (-)
              RadSwDownRefHeight => noahmp%forcing%RadSwDownRefHeight,& ! in,    downward shortwave radiation [W/m2] at reference height
              RadSwAbsTot             => noahmp%energy%flux%RadSwAbsTot              ,& ! in,    total absorbed solar radiation (w/m2)
              RadSwReflTot             => noahmp%energy%flux%RadSwReflTot              ,& ! in,    total reflected solar radiation (w/m2)
              RadSwReflVeg            => noahmp%energy%flux%RadSwReflVeg             ,& ! in,    reflected solar radiation by vegetation (w/m2)
              RadSwReflGrd            => noahmp%energy%flux%RadSwReflGrd             ,& ! in,    reflected solar radiation by ground (w/m2)
              RadLwNetTot            => noahmp%energy%flux%RadLwNetTot             ,& ! in,    total net LW. rad (w/m2)   [+ to atm]
              HeatSensibleTot             => noahmp%energy%flux%HeatSensibleTot              ,& ! in,    total sensible heat (w/m2) [+ to atm]
              HeatLatentCanopy            => noahmp%energy%flux%HeatLatentCanopy             ,& ! in,    canopy latent heat flux (w/m2) [+ to atm]
              HeatLatentGrdTot            => noahmp%energy%flux%HeatLatentGrdTot             ,& ! in,    total ground latent heat (w/m2) [+ to atm]
              HeatLatentTransp            => noahmp%energy%flux%HeatLatentTransp             ,& ! in,    latent heat flux from transpiration (w/m2) [+ to atm]
              HeatGroundTot           => noahmp%energy%flux%HeatGroundTot            ,& ! in,   total ground heat flux (w/m2) [+ to soil/snow]
              RadSwAbsVeg             => noahmp%energy%flux%RadSwAbsVeg              ,& ! in,    solar radiation absorbed by vegetation (w/m2)
              RadSwAbsGrd             => noahmp%energy%flux%RadSwAbsGrd              ,& ! in,    solar radiation absorbed by ground (w/m2)
              HeatPrecipAdvTot             => noahmp%energy%flux%HeatPrecipAdvTot              ,& ! in,    precipitation advected heat - total (W/m2)
              HeatPrecipAdvBareGrd            => noahmp%energy%flux%HeatPrecipAdvBareGrd             ,& ! in,    precipitation advected heat - bare ground net (W/m2)
              HeatPrecipAdvVegGrd            => noahmp%energy%flux%HeatPrecipAdvVegGrd             ,& ! in,    precipitation advected heat - under canopy net (W/m2)
              HeatPrecipAdvCanopy            => noahmp%energy%flux%HeatPrecipAdvCanopy             ,& ! in,    precipitation advected heat - vegetation net (W/m2)
              HeatLatentIrriEvap            => noahmp%energy%flux%HeatLatentIrriEvap             ,& ! in,    latent heating due to sprinkler evaporation [w/m2]
              ERRENG          => noahmp%energy%state%ERRENG          ,& ! out,   error in surface energy balance [w/m2]
              ERRSW           => noahmp%energy%state%ERRSW            & ! out,   error in shortwave radiation balance [w/m2]
             )
! ----------------------------------------------------------------------

    ! error in shortwave radiation balance should be <0.01 W/m2
    ERRSW = RadSwDownRefHeight - (RadSwAbsTot + RadSwReflTot)
    ! print out diagnostics when error is large
    if ( abs(ERRSW) > 0.01 ) then  ! w/m2
       write(*,*) 'GridIndexI, GridIndexJ =', GridIndexI, GridIndexJ
       write(*,*) 'ERRSW =',  ERRSW
       write(*,*) "VEGETATION!"
       write(*,*) "RadSwDownRefHeight*VegFrac =",RadSwDownRefHeight * VegFrac
       write(*,*) "VegFrac*RadSwAbsVeg + RadSwAbsGrd =",   VegFrac * RadSwAbsVeg + RadSwAbsGrd
       write(*,*) "VegFrac*RadSwReflVeg + RadSwReflGrd =", VegFrac * RadSwReflVeg + RadSwReflGrd
       write(*,*) "GROUND!"
       write(*,*) "(1.-VegFrac)*RadSwDownRefHeight =", (1.0-VegFrac)*RadSwDownRefHeight
       write(*,*) "(1.-VegFrac)*RadSwAbsGrd =",    (1.0-VegFrac) * RadSwAbsGrd
       write(*,*) "(1.-VegFrac)*RadSwReflGrd=",    (1.0-VegFrac) * RadSwReflGrd
       write(*,*) "RadSwReflVeg   =", RadSwReflVeg
       write(*,*) "RadSwReflGrd   =", RadSwReflGrd
       write(*,*) "RadSwReflTot    =", RadSwReflTot
       write(*,*) "RadSwAbsVeg    =", RadSwAbsVeg
       write(*,*) "RadSwAbsGrd    =", RadSwAbsGrd
       write(*,*) "RadSwAbsTot    =", RadSwAbsTot
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Stop in Noah-MP")
       stop "Error"
    endif

    ! error in surface energy balance should be <0.01 W/m2
    ERRENG = RadSwAbsVeg + RadSwAbsGrd + HeatPrecipAdvTot - &
            (RadLwNetTot + HeatSensibleTot + HeatLatentCanopy + HeatLatentGrdTot + &
             HeatLatentTransp + HeatGroundTot + HeatLatentIrriEvap)
    ! print out diagnostics when error is large
    if ( abs(ERRENG) > 0.01 ) then
       write(*,*) 'ERRENG =',ERRENG,' at GridIndexI,GridIndexJ: ',GridIndexI,GridIndexJ
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Net solar:        ",RadSwAbsTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Net longwave:     ",RadLwNetTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Total sensible:   ",HeatSensibleTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Canopy evap:      ",HeatLatentCanopy
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Ground evap:      ",HeatLatentGrdTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Transpiration:    ",HeatLatentTransp
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Total ground:     ",HeatGroundTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Sprinkler:        ",HeatLatentIrriEvap
       !call wrf_message(trim(message))
       write(*,'(a17,4F10.4)') "Precip advected: ",HeatPrecipAdvTot,HeatPrecipAdvCanopy,HeatPrecipAdvVegGrd,HeatPrecipAdvBareGrd
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Veg fraction:     ",VegFrac
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Energy budget problem in NOAHMP LSM")
       stop "Error"
    endif

    end associate

  end subroutine BalanceEnergyCheck

end module BalanceErrorCheckMod
