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
              IRR_FRAC        => noahmp%water%param%IRR_FRAC         ,& ! in,    irrigation fraction parameter
              IrrigationFracGrid          => noahmp%water%state%IrrigationFracGrid           ,& ! in,    total input irrigation fraction
              WaterTableDepth             => noahmp%water%state%WaterTableDepth              ,& ! in,    water table depth [m]
              CanopyLiqWater          => noahmp%water%state%CanopyLiqWater           ,& ! in,    canopy intercepted liquid water [mm]
              CanopyIce          => noahmp%water%state%CanopyIce           ,& ! in,    canopy intercepted ice [mm]
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! in,    snow water equivalent [mm]
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,    total soil moisture [m3/m3]
              WaterStorageAquifer  => noahmp%water%state%WaterStorageAquifer  ,& ! in,    water storage in aquifer [mm]
              WaterStorageTotBeg          => noahmp%water%state%WaterStorageTotBeg           ,& ! in,    total water storage at the beginning
              PRCP            => noahmp%water%flux%PRCP              ,& ! in,    total precipitation [mm/s]
              ECAN            => noahmp%water%flux%ECAN              ,& ! in,    evaporation of intercepted water (mm/s) [+]
              ETRAN           => noahmp%water%flux%ETRAN             ,& ! in,    transpiration rate (mm/s) [+]
              EDIR            => noahmp%water%flux%EDIR              ,& ! in,    net direct soil evaporation (mm/s)
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! in,    surface runoff [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! in,    subsurface runoff [mm/s]
              QTLDRN          => noahmp%water%flux%QTLDRN            ,& ! in,    tile drainage (mm/s)
              IRSIRATE        => noahmp%water%flux%IRSIRATE          ,& ! in,    rate of irrigation by sprinkler [m/timestep]
              IRMIRATE        => noahmp%water%flux%IRMIRATE          ,& ! in,    micro irrigation water rate [m/timestep]
              IRFIRATE        => noahmp%water%flux%IRFIRATE          ,& ! in,    flood irrigation water rate [m/timestep]
              WaterStorageTotEnd          => noahmp%water%state%WaterStorageTotEnd           ,& ! out,   total water storage at the end
              WaterBalanceError          => noahmp%water%state%WaterBalanceError            & ! out,   water balance error (mm) per time step
             )
! ----------------------------------------------------------------------

    ! before water balance check add irrigation water to precipitation
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IRR_FRAC) ) then
       PRCP = PRCP + (IRSIRATE + IRMIRATE + IRFIRATE) * 1000.0 / MainTimeStep  ! irrigation
    endif

    ! Error in water balance should be < 0.1 mm
    if ( SurfaceType == 1 ) then        !soil
       WaterStorageTotEnd = CanopyLiqWater + CanopyIce + SnowWaterEquiv + WaterStorageAquifer
       do IZ = 1, NumSoilLayer
          WaterStorageTotEnd = WaterStorageTotEnd + SoilMoisture(IZ) * ThicknessSnowSoilLayer(IZ) * 1000.0
       enddo
       WaterBalanceError = WaterStorageTotEnd - WaterStorageTotBeg - (PRCP - ECAN - ETRAN - EDIR - RUNSRF - RUNSUB - QTLDRN) * MainTimeStep
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
               '("  GridIndexI    GridIndexJ   WaterStorageTotEnd   WaterStorageTotBeg     PRCP     ECAN     EDIR    ETRAN   RUNSRF   RUNSUB   WaterTableDepth   QTLDRN")')
          !call wrf_message(trim(message))
          write(*,'(i6,1x,i6,1x,2f15.3,9f11.5)') GridIndexI,GridIndexJ,WaterStorageTotEnd,WaterStorageTotBeg,PRCP*MainTimeStep,ECAN*MainTimeStep,&
                EDIR*MainTimeStep,ETRAN*MainTimeStep,RUNSRF*MainTimeStep,RUNSUB*MainTimeStep,WaterTableDepth,QTLDRN*MainTimeStep
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
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              RadSWDownRefHeight => noahmp%forcing%RadSWDownRefHeight,& ! in,    downward shortwave radiation [W/m2] at reference height
              FSA             => noahmp%energy%flux%FSA              ,& ! in,    total absorbed solar radiation (w/m2)
              FSR             => noahmp%energy%flux%FSR              ,& ! in,    total reflected solar radiation (w/m2)
              FSRV            => noahmp%energy%flux%FSRV             ,& ! in,    reflected solar radiation by vegetation (w/m2)
              FSRG            => noahmp%energy%flux%FSRG             ,& ! in,    reflected solar radiation by ground (w/m2)
              FIRA            => noahmp%energy%flux%FIRA             ,& ! in,    total net LW. rad (w/m2)   [+ to atm]
              FSH             => noahmp%energy%flux%FSH              ,& ! in,    total sensible heat (w/m2) [+ to atm]
              FCEV            => noahmp%energy%flux%FCEV             ,& ! in,    canopy evaporation (w/m2) [+ = to atm]
              FGEV            => noahmp%energy%flux%FGEV             ,& ! in,    ground (soil/snow) evap heat (w/m2) [+ to atm]
              FCTR            => noahmp%energy%flux%FCTR             ,& ! in,    transpiration (w/m2) [+ = to atm]
              SSOIL           => noahmp%energy%flux%SSOIL            ,& ! in,    soil heat flux (w/m2) [+ to soil]
              SAV             => noahmp%energy%flux%SAV              ,& ! in,    solar radiation absorbed by vegetation (w/m2)
              SAG             => noahmp%energy%flux%SAG              ,& ! in,    solar radiation absorbed by ground (w/m2)
              PAH             => noahmp%energy%flux%PAH              ,& ! in,    precipitation advected heat - total (W/m2)
              PAHB            => noahmp%energy%flux%PAHB             ,& ! in,    precipitation advected heat - bare ground net (W/m2)
              PAHG            => noahmp%energy%flux%PAHG             ,& ! in,    precipitation advected heat - under canopy net (W/m2)
              PAHV            => noahmp%energy%flux%PAHV             ,& ! in,    precipitation advected heat - vegetation net (W/m2)
              FIRR            => noahmp%energy%flux%FIRR             ,& ! in,    latent heating due to sprinkler evaporation [w/m2]
              ERRENG          => noahmp%energy%state%ERRENG          ,& ! out,   error in surface energy balance [w/m2]
              ERRSW           => noahmp%energy%state%ERRSW            & ! out,   error in shortwave radiation balance [w/m2]
             )
! ----------------------------------------------------------------------

    ! error in shortwave radiation balance should be <0.01 W/m2
    ERRSW = RadSWDownRefHeight - (FSA + FSR)
    ! print out diagnostics when error is large
    if ( abs(ERRSW) > 0.01 ) then  ! w/m2
       write(*,*) 'GridIndexI, GridIndexJ =', GridIndexI, GridIndexJ
       write(*,*) 'ERRSW =',  ERRSW
       write(*,*) "VEGETATION!"
       write(*,*) "RadSWDownRefHeight*FVEG =",RadSWDownRefHeight * FVEG
       write(*,*) "FVEG*SAV + SAG =",   FVEG * SAV + SAG
       write(*,*) "FVEG*FSRV + FSRG =", FVEG * FSRV + FSRG
       write(*,*) "GROUND!"
       write(*,*) "(1.-FVEG)*RadSWDownRefHeight =", (1.0-FVEG)*RadSWDownRefHeight
       write(*,*) "(1.-FVEG)*SAG =",    (1.0-FVEG) * SAG
       write(*,*) "(1.-FVEG)*FSRG=",    (1.0-FVEG) * FSRG
       write(*,*) "FSRV   =", FSRV
       write(*,*) "FSRG   =", FSRG
       write(*,*) "FSR    =", FSR
       write(*,*) "SAV    =", SAV
       write(*,*) "SAG    =", SAG
       write(*,*) "FSA    =", FSA
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Stop in Noah-MP")
       stop "Error"
    endif

    ! error in surface energy balance should be <0.01 W/m2
    ERRENG = SAV + SAG + PAH - (FIRA + FSH + FCEV + FGEV + FCTR + SSOIL + FIRR)
    ! print out diagnostics when error is large
    if ( abs(ERRENG) > 0.01 ) then
       write(*,*) 'ERRENG =',ERRENG,' at GridIndexI,GridIndexJ: ',GridIndexI,GridIndexJ
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Net solar:        ",FSA
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Net longwave:     ",FIRA
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Total sensible:   ",FSH
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Canopy evap:      ",FCEV
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Ground evap:      ",FGEV
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Transpiration:    ",FCTR
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Total ground:     ",SSOIL
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Sprinkler:        ",FIRR
       !call wrf_message(trim(message))
       write(*,'(a17,4F10.4)') "Precip advected: ",PAH,PAHV,PAHG,PAHB
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Veg fraction:     ",FVEG
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Energy budget problem in NOAHMP LSM")
       stop "Error"
    endif

    end associate

  end subroutine BalanceEnergyCheck

end module BalanceErrorCheckMod
