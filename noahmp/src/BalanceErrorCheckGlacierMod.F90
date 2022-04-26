module BalanceErrorCheckGlacierMod

!!! Check glacier water and energy balance and report error

  use Machine, only : kind_noahmp
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
              SNEQV           => noahmp%water%state%SNEQV            ,& ! in,    snow water equivalent [mm]
              BEG_WB          => noahmp%water%state%BEG_WB            & ! out,   total water storage at the beginning
             )
! ----------------------------------------------------------------------

    ! compute total glacier water storage before NoahMP processes
    ! need more work on including glacier ice mass underneath snow
    BEG_WB = SNEQV

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
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,    number of soil layers
              ILOC            => noahmp%config%domain%ILOC           ,& ! in,    grid index
              JLOC            => noahmp%config%domain%JLOC           ,& ! in,    grid index
              DT              => noahmp%config%domain%DT             ,& ! in,    main noahmp timestep (s)
              SNEQV           => noahmp%water%state%SNEQV            ,& ! in,    snow water equivalent [mm]
              SH2O            => noahmp%water%state%SH2O             ,& ! in,    glacier water content [m3/m3]
              SICE            => noahmp%water%state%SICE             ,& ! in,    glacier ice moisture (m3/m3)
              BEG_WB          => noahmp%water%state%BEG_WB           ,& ! in,    total water storage at the beginning
              PRCP            => noahmp%water%flux%PRCP              ,& ! in,    total precipitation [mm/s]
              EDIR            => noahmp%water%flux%EDIR              ,& ! in,    net direct soil evaporation (mm/s)
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! in,    surface runoff [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! in,    subsurface runoff [mm/s]
              END_WB          => noahmp%water%state%END_WB           ,& ! out,   total water storage at the end
              ERRWAT          => noahmp%water%state%ERRWAT            & ! out,   water balance error (mm) per time step
             )
! ----------------------------------------------------------------------

    ! Error in water balance should be < 0.1 mm
    ! compute total glacier water storage before NoahMP processes
    ! need more work on including glacier ice mass underneath snow
    END_WB = SNEQV
    ERRWAT = END_WB - BEG_WB - (PRCP - EDIR - RUNSRF - RUNSUB) * DT

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
           '("  I    J   END_WB   BEG_WB     PRCP     EDIR    RUNSRF   RUNSUB")')
       !call wrf_message(trim(message))
       write(*,'(i6,1x,i6,1x,2f15.3,9f11.5)')ILOC,JLOC,END_WB,BEG_WB,PRCP*DT,&
             EDIR*DT,RUNSRF*DT,RUNSUB*DT
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
              ILOC            => noahmp%config%domain%ILOC           ,& ! in,    grid index
              JLOC            => noahmp%config%domain%JLOC           ,& ! in,    grid index
              SWDOWN          => noahmp%energy%flux%SWDOWN           ,& ! in,    downward solar filtered by sun angle [w/m2]
              FSA             => noahmp%energy%flux%FSA              ,& ! in,    total absorbed solar radiation (w/m2)
              FSR             => noahmp%energy%flux%FSR              ,& ! in,    total reflected solar radiation (w/m2)
              FIRA            => noahmp%energy%flux%FIRA             ,& ! in,    total net LW. rad (w/m2)   [+ to atm]
              FSH             => noahmp%energy%flux%FSH              ,& ! in,    total sensible heat (w/m2) [+ to atm]
              FGEV            => noahmp%energy%flux%FGEV             ,& ! in,    ground (soil/snow) evap heat (w/m2) [+ to atm]
              SSOIL           => noahmp%energy%flux%SSOIL            ,& ! in,    soil heat flux (w/m2) [+ to soil]
              SAG             => noahmp%energy%flux%SAG              ,& ! in,    solar radiation absorbed by ground (w/m2)
              PAH             => noahmp%energy%flux%PAH              ,& ! in,    precipitation advected heat - total (W/m2)
              ERRENG          => noahmp%energy%state%ERRENG          ,& ! out,   error in surface energy balance [w/m2]
              ERRSW           => noahmp%energy%state%ERRSW            & ! out,   error in shortwave radiation balance [w/m2]
             )
! ----------------------------------------------------------------------

    ! error in shortwave radiation balance should be <0.01 W/m2
    ERRSW = SWDOWN - (FSA + FSR)
    ! print out diagnostics when error is large
    if ( abs(ERRSW) > 0.01 ) then  ! w/m2
       write(*,*) 'I, J =',  ILOC, JLOC
       write(*,*) 'ERRSW =',  ERRSW
       write(*,*) "SWDOWN =", SWDOWN
       write(*,*) "FSR    =", FSR
       write(*,*) "SAG    =", SAG
       write(*,*) "FSA    =", FSA
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Stop in Noah-MP")
       stop "Error"
    endif

    ! error in surface energy balance should be <0.01 W/m2
    ERRENG = SAG + PAH - (FIRA + FSH + FGEV + SSOIL)
    ! print out diagnostics when error is large
    if ( abs(ERRENG) > 0.01 ) then
       write(*,*) 'ERRENG =',ERRENG,' at i,j: ',ILOC,JLOC
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Net longwave:     ",FIRA
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Total sensible:   ",FSH
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Ground evap:      ",FGEV
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "Total ground:     ",SSOIL
       !call wrf_message(trim(message))
       write(*,'(a17,4F10.4)') "Precip advected: ",PAH
       !call wrf_message(trim(message))
       write(*,'(a17,F10.4)') "absorbed shortwave: ",SAG
       !call wrf_message(trim(message))
       !call wrf_error_fatal("Energy budget problem in NOAHMP LSM")
       stop "Error"
    endif

    end associate

  end subroutine BalanceEnergyCheckGlacier

end module BalanceErrorCheckGlacierMod
