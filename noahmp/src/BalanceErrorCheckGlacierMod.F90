module BalanceErrorCheckGlacierMod

!!! Check glacier water and energy balance and report error

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

!!!! Water balance check initialization
  subroutine BalanceWaterInitGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in NOAHMP_GLACIER)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                      :: IZ      ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! in,    snow water equivalent [mm]
              WaterStorageTotBeg          => noahmp%water%state%WaterStorageTotBeg            & ! out,   total water storage at the beginning
             )
! ----------------------------------------------------------------------

    ! compute total glacier water storage before NoahMP processes
    ! need more work on including glacier ice mass underneath snow
    WaterStorageTotBeg = SnowWaterEquiv

    end associate

  end subroutine BalanceWaterInitGlacier


!!!! Water balance check and report error
  subroutine BalanceWaterCheckGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ERROR_GLACIER
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
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! in,    snow water equivalent [mm]
              WaterStorageTotBeg          => noahmp%water%state%WaterStorageTotBeg           ,& ! in,    total water storage at the beginning
              PrecipTotRefHeight            => noahmp%water%flux%PrecipTotRefHeight              ,& ! in,    total precipitation [mm/s] at reference height
              EvapSoilNet            => noahmp%water%flux%EvapSoilNet              ,& ! in,    net direct soil evaporation (mm/s)
              RunoffSurface          => noahmp%water%flux%RunoffSurface            ,& ! in,    surface runoff [mm/s]
              RunoffSubsurface          => noahmp%water%flux%RunoffSubsurface            ,& ! in,    subsurface runoff [mm/s]
              WaterStorageTotEnd          => noahmp%water%state%WaterStorageTotEnd           ,& ! out,   total water storage at the end
              WaterBalanceError          => noahmp%water%state%WaterBalanceError            & ! out,   water balance error (mm) per time step
             )
! ----------------------------------------------------------------------

    ! Error in water balance should be < 0.1 mm
    ! compute total glacier water storage before NoahMP processes
    ! need more work on including glacier ice mass underneath snow
    WaterStorageTotEnd = SnowWaterEquiv
    WaterBalanceError = WaterStorageTotEnd - WaterStorageTotBeg - &
                       (PrecipTotRefHeight - EvapSoilNet - RunoffSurface - RunoffSubsurface) * MainTimeStep

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
           '("  GridIndexI   GridIndexJ   WaterStorageTotEnd   WaterStorageTotBeg     PrecipTotRefHeight  &
                EvapSoilNet    RunoffSurface   RunoffSubsurface")')
       !call wrf_message(trim(message))
       write(*,'(i6,1x,i6,1x,2f15.3,9f11.5)') GridIndexI,GridIndexJ,WaterStorageTotEnd,WaterStorageTotBeg,&
                                              PrecipTotRefHeight*MainTimeStep,EvapSoilNet*MainTimeStep,&
                                              RunoffSurface*MainTimeStep,RunoffSubsurface*MainTimeStep
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Water budget problem in NOAHMP LSM")
       stop "Error"
    endif
#endif

    end associate

  end subroutine BalanceWaterCheckGlacier


!!!! Energy balance check and error report
  subroutine BalanceEnergyCheckGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ERROR_GLACIER
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
              RadSwDownRefHeight => noahmp%forcing%RadSwDownRefHeight,& ! in,    downward shortwave radiation [W/m2] at reference height
              RadSwAbsTot             => noahmp%energy%flux%RadSwAbsTot              ,& ! in,    total absorbed solar radiation (w/m2)
              RadSwReflTot             => noahmp%energy%flux%RadSwReflTot              ,& ! in,    total reflected solar radiation (w/m2)
              RadLwNetTot            => noahmp%energy%flux%RadLwNetTot             ,& ! in,    total net LW. rad (w/m2)   [+ to atm]
              HeatSensibleTot             => noahmp%energy%flux%HeatSensibleTot              ,& ! in,    total sensible heat (w/m2) [+ to atm]
              HeatLatentGrdTot            => noahmp%energy%flux%HeatLatentGrdTot             ,& ! in,    total ground latent heat (w/m2) [+ to atm]
              HeatGroundTot           => noahmp%energy%flux%HeatGroundTot            ,& ! in,    total ground heat flux (w/m2) [+ to soil/snow]
              RadSwAbsGrd             => noahmp%energy%flux%RadSwAbsGrd              ,& ! in,    solar radiation absorbed by ground (w/m2)
              HeatPrecipAdvTot             => noahmp%energy%flux%HeatPrecipAdvTot              ,& ! in,    precipitation advected heat - total (W/m2)
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
       write(*,*) "RadSwDownRefHeight =", RadSwDownRefHeight
       write(*,*) "RadSwReflTot    =", RadSwReflTot
       write(*,*) "RadSwAbsGrd    =", RadSwAbsGrd
       write(*,*) "RadSwAbsTot    =", RadSwAbsTot
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Stop in Noah-MP")
       stop "Error"
    endif

    ! error in surface energy balance should be <0.01 W/m2
    ERRENG = RadSwAbsGrd + HeatPrecipAdvTot - (RadLwNetTot + HeatSensibleTot + HeatLatentGrdTot + HeatGroundTot)
    ! print out diagnostics when error is large
    if ( abs(ERRENG) > 0.01 ) then
       write(*,*) 'ERRENG =',ERRENG,' at GridIndexI,GridIndexJ: ',GridIndexI,GridIndexJ
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Net longwave:     ",RadLwNetTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Total sensible:   ",HeatSensibleTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Ground evap:      ",HeatLatentGrdTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Total ground:     ",HeatGroundTot
       !call wrf_message(trim(message))
       write(*,'(a17,4F10.4)') "Precip advected: ",HeatPrecipAdvTot
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "absorbed shortwave: ",RadSwAbsGrd
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Energy budget problem in NOAHMP LSM")
       stop "Error"
    endif

    end associate

  end subroutine BalanceEnergyCheckGlacier

end module BalanceErrorCheckGlacierMod
