module BalanceErrorCheckMod

!!! Check water and energy balance and report error

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine BalanceErrorCheck(noahmp)

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
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,    number of soil layers
              IST             => noahmp%config%domain%IST            ,& ! in,    surface type 1-soil; 2-lake
              ILOC            => noahmp%config%domain%ILOC           ,& ! in,    grid index
              JLOC            => noahmp%config%domain%JLOC           ,& ! in,    grid index
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,    thickness of snow/soil layers (m)
              DT              => noahmp%config%domain%DT             ,& ! in,    main noahmp timestep (s)
              CROPLU          => noahmp%config%domain%CROPLU         ,& ! in,    flag to identify croplands
              IRR_FRAC        => noahmp%water%param%IRR_FRAC         ,& ! in,    irrigation fraction parameter
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              SWDOWN          => noahmp%energy%flux%SWDOWN           ,& ! in,    downward solar filtered by sun angle [w/m2]
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
              IRRFRA          => noahmp%water%state%IRRFRA           ,& ! in,    total input irrigation fraction
              ZWT             => noahmp%water%state%ZWT              ,& ! in,    water table depth [m]
              CANLIQ          => noahmp%water%state%CANLIQ           ,& ! in,    canopy intercepted liquid water (mm)
              CANICE          => noahmp%water%state%CANICE           ,& ! in,    canopy intercepted ice mass (mm)
              SNEQV           => noahmp%water%state%SNEQV            ,& ! in,    snow water equivalent [mm]
              SMC             => noahmp%water%state%SMC              ,& ! in,    total soil moisture [m3/m3]
              WA              => noahmp%water%state%WA               ,& ! in,    water storage in aquifer [mm]
              BEG_WB          => noahmp%water%state%BEG_WB           ,& ! in,    total water storage at the beginning
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
              END_WB          => noahmp%water%state%END_WB           ,& ! out,   total water storage at the end
              ERRWAT          => noahmp%water%state%ERRWAT           ,& ! out,   water balance error (mm) per time step
              ERRENG          => noahmp%energy%state%ERRENG          ,& ! out,   error in surface energy balance [w/m2]
              ERRSW           => noahmp%energy%state%ERRSW            & ! out,   error in shortwave radiation balance [w/m2]
             )
! ----------------------------------------------------------------------

    ! before water balance check add irrigation water to precipitation
    if ( (CROPLU .eqv. .true.) .and. (IRRFRA >= IRR_FRAC) ) then
       PRCP = PRCP + (IRSIRATE + IRMIRATE + IRFIRATE) * 1000.0 / DT  ! irrigation
    endif

    ! error in shortwave radiation balance should be <0.01 W/m2
    ERRSW = SWDOWN - (FSA + FSR)
    ! print out diagnostics when error is large
    if ( abs(ERRSW) > 0.01 ) then  ! w/m2
       write(*,*) 'I, J =',  ILOC, JLOC
       write(*,*) 'ERRSW =',  ERRSW
       write(*,*) "VEGETATION!"
       write(*,*) "SWDOWN*FVEG =",      SWDOWN * FVEG
       write(*,*) "FVEG*SAV + SAG =",   FVEG * SAV + SAG
       write(*,*) "FVEG*FSRV + FSRG =", FVEG * FSRV + FSRG
       write(*,*) "GROUND!"
       write(*,*) "(1.-FVEG)*SWDOWN =", (1.0-FVEG) * SWDOWN
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
       write(*,*) 'ERRENG =',ERRENG,' at i,j: ',ILOC,JLOC
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
       write(*,'(a17,F10.4)') "Precip:           ",PRCP
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Veg fraction:     ",FVEG
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Energy budget problem in NOAHMP LSM")
       stop "Error"
    endif

    ! Error in water balance shoudl be < 0.1 mm
    if ( IST == 1 ) then        !soil
       END_WB = CANLIQ + CANICE + SNEQV + WA
       do IZ = 1, NSOIL
          END_WB = END_WB + SMC(IZ) * DZSNSO(IZ) * 1000.0
       enddo
       ERRWAT = END_WB - BEG_WB - (PRCP - ECAN - ETRAN - EDIR - RUNSRF - RUNSUB - QTLDRN) * DT
#ifndef WRF_HYDRO
       if ( abs(ERRWAT) > 0.1 ) then
          if ( ERRWAT > 0) then
             !call wrf_message ('The model is gaining water (ERRWAT is positive)')
             write(*,*) "The model is gaining water (ERRWAT is positive)"
          else
             !call wrf_message('The model is losing water (ERRWAT is negative)')
             write(*,*) "The model is losing water (ERRWAT is negative)"
          endif
          write(*,*) 'ERRWAT =',ERRWAT, "kg m{-2} timestep{-1}"
          !call wrf_message(trim(message))
          write(*, &
               '("  I    J   END_WB   BEG_WB     PRCP     ECAN     EDIR    ETRAN   RUNSRF   RUNSUB   ZWT   QTLDRN")')
          !call wrf_message(trim(message))
          write(*,'(i6,1x,i6,1x,2f15.3,9f11.5)')ILOC,JLOC,END_WB,BEG_WB,PRCP*DT,ECAN*DT,&
                EDIR*DT,ETRAN*DT,RUNSRF*DT,RUNSUB*DT,ZWT,QTLDRN*DT
          !call wrf_message(trim(message))
          !call wrf_error_fatal("Water budget problem in NOAHMP LSM")
          stop "Error"
       endif
#endif
    else                 !KWM
       ERRWAT = 0.0      !KWM
    endif

    end associate

  end subroutine BalanceErrorCheck

end module BalanceErrorCheckMod
