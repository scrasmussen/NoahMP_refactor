module RunoffSurfaceDynamicVicMod

!!! Compuate inflitration rate at soil surface and estimate surface runoff based on dynamic VIC scheme
!!! Reference: Liang, X., & Xie, Z. (2001). A new surface runoff parameterization with subgrid-scale
!!! soil heterogeneity for land surface models. Advances in Water Resources, 24(9-10), 1173-1193.

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SoilWaterInfilPhilipMod,        only : SoilWaterInfilPhilip
  use SoilWaterInfilGreenAmptMod,     only : SoilWaterInfilGreenAmpt
  use SoilWaterInfilSmithParlangeMod, only : SoilWaterInfilSmithParlange
  use RunoffSurfaceExcessDynamicVicMod

  implicit none

contains

  subroutine SurfaceRunoffDynamicVic(noahmp, DT)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: DYNAMIC_VIC
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

     implicit none

! IN & OUT variabls
     type(noahmp_type)     , intent(inout) :: noahmp
     real(kind=kind_noahmp), intent(in)    :: DT       ! timestep (may not be the same as model timestep)

! local variable
     integer                :: IZ, IZMAX, INFLMAX      ! do-loop index
     real(kind=kind_noahmp) :: BB                      ! B parameter for infiltration scaling curve
     real(kind=kind_noahmp) :: TOP_MOIST               ! actual water depth in top layers (m)
     real(kind=kind_noahmp) :: TOP_MAX_MOIST           ! water depth in top layers (m)
     real(kind=kind_noahmp) :: DP                      ! water input on soil surface (m)
     real(kind=kind_noahmp) :: I_0                     ! initial water depth (m)
     real(kind=kind_noahmp) :: I_MAX                   ! maximum water depth (m)
     real(kind=kind_noahmp) :: FSUR                    ! surface infiltration rate (m/s)
     real(kind=kind_noahmp) :: FMAX                    ! maximum infiltration rate (m/s)
     real(kind=kind_noahmp) :: RUNOFFSAT               ! saturation excess runoff (m/s)
     real(kind=kind_noahmp) :: RUNOFFINF               ! infiltration excess runoff (m/s)
     real(kind=kind_noahmp) :: INFILTRTN               ! infiltration (m/s)
     real(kind=kind_noahmp) :: TEMPR1                  ! temporary saturation excess runoff (m/s)
     real(kind=kind_noahmp) :: TEMPR2                  ! temporary infiltration excess runoff (m/s)
     real(kind=kind_noahmp) :: R1                      ! saturation excess runoff (m/s)
     real(kind=kind_noahmp) :: R2                      ! infiltration excess runoff (m/s)
     real(kind=kind_noahmp) :: YD                      ! initial depth Y (m)
     real(kind=kind_noahmp) :: YD_OLD                  ! initial depth Y (m)
     real(kind=kind_noahmp) :: YD0                     ! initial depth Y (m)
     real(kind=kind_noahmp) :: TEMP1, ERROR

! --------------------------------------------------------------------
     associate(                                                        &
               NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
               ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
               OPT_INFDV       => noahmp%config%nmlist%OPT_INFDV      ,& ! in,     options for infiltration in dynamic VIC runoff scheme
               SMC             => noahmp%water%state%SMC              ,& ! in,     total soil moisture [m3/m3]
               SH2O            => noahmp%water%state%SH2O             ,& ! in,     soil water content [m3/m3]
               SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3] 
               SICEMAX         => noahmp%water%state%SICEMAX          ,& ! in,     maximum soil ice content (m3/m3)
               QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
               SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
               BBVIC           => noahmp%water%param%BBVIC            ,& ! in,     DVIC heterogeniety parameter for infiltration
               GDVIC           => noahmp%water%param%GDVIC            ,& ! in,     DVIC Mean Capillary Drive (m) for infiltration models
               BDVIC           => noahmp%water%param%BDVIC            ,& ! in,     DVIC model infiltration parameter
               FACC            => noahmp%water%flux%FACC              ,& ! inout,  accumulated infiltration rate (m/s)
               RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
               PDDUM           => noahmp%water%flux%PDDUM              & ! out,    infiltration rate at surface (mm/s)
              )
! ----------------------------------------------------------------------

     ! initialization
     TOP_MOIST     = 0.0
     TOP_MAX_MOIST = 0.0
     BB            = 1.0
     DP            = 0.0
     I_MAX         = 0.0
     I_0           = 0.0
     RUNOFFSAT     = 0.0
     RUNOFFINF     = 0.0
     INFILTRTN     = 0.0
     RUNSRF        = 0.0
     PDDUM         = 0.0
     IZMAX         = 20
     ERROR         = 1.388889E-07 * DT ! 0.5 mm per hour time step
     BB            = BBVIC

     do IZ = 1, NSOIL-2
        TOP_MOIST     = TOP_MOIST + SMC(IZ) * -1.0 * ZSOIL(IZ)            ! actual moisture in top layers, [m]
        TOP_MAX_MOIST = TOP_MAX_MOIST + SMCMAX(IZ) * -1.0 * ZSOIL(IZ)     ! maximum moisture in top layers, [m]  
     enddo
     if ( TOP_MOIST > TOP_MAX_MOIST ) TOP_MOIST = TOP_MAX_MOIST

     DP     = QINSUR * DT                      ! precipitation depth, [m]
     I_MAX  = TOP_MAX_MOIST * (BDVIC + 1.0)    ! maximum infiltration capacity, im, [m], Eq. 14
     I_0    = I_MAX * (1.0 - (1.0 - (TOP_MOIST/TOP_MAX_MOIST)**(1.0 / (1.0+BDVIC)) ) )  ! infiltration capacity, i [m] in the Eq. 1
     ! I_MAX = CAP_minf ; I_0 = A  
     INFLMAX = 0

     ! compute surface infiltration
     if ( OPT_INFDV == 1 ) then
        call SoilWaterInfilPhilip(noahmp, DT, INFLMAX, FSUR)
     else if ( OPT_INFDV == 2 ) then
        call SoilWaterInfilGreenAmpt(noahmp, INFLMAX, FSUR)
     else if ( OPT_INFDV == 3 ) then
        call SoilWaterInfilSmithParlange(noahmp, INFLMAX, FSUR)
     endif

     ! I_MM = FSUR; I_M = FMAX  
     FMAX = ( BB + 1.0 ) * FSUR
     if ( DP <= 0.0 ) then
        RUNOFFSAT = 0.0
        RUNOFFINF = 0.0
        INFILTRTN = 0.0
        goto 2001
     else
        if ( (TOP_MOIST >= TOP_MAX_MOIST) .and. (I_0 >= I_MAX) ) then
           TOP_MOIST = TOP_MAX_MOIST
           I_0       = I_MAX
           RUNOFFSAT = DP
           RUNOFFINF = 0.0
           INFILTRTN = 0.0
           goto 2001
        else
           I_0 = I_MAX * (1.0 - (1.0 - (TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+BDVIC))))
           if ( (DP+I_0) > I_MAX ) then
              if ( (FMAX*DT) >= DP) then
                 YD     = I_MAX - I_0
                 TEMPR1 = 0.0
                 call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                 TEMP1  = I_MAX - I_0 - TEMPR1 - (FSUR*DT) * (1.0-(1.0-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0)))
                 if ( TEMP1 <= 0.0 ) then
                    YD        = I_MAX - I_0
                    INFILTRTN = TOP_MAX_MOIST - TOP_MOIST
                    RUNOFFSAT = DP - INFILTRTN
                    RUNOFFINF = 0.0
                    TOP_MOIST = TOP_MAX_MOIST
                    I_0       = I_MAX
                    goto 2001
                 else
                    YD        = 0.0
                    do IZ = 1, IZMAX ! loop : IITERATION1
                       YD_OLD = YD
                       TEMPR1 = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                       YD     = TEMPR1 + (FSUR*DT) * (1.0-(1.0-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0)))
                       if ( (abs(YD-YD_OLD) <= ERROR) .or. (IZ == IZMAX) ) then
                          goto 1003
                       endif
                    enddo
                 endif
              else
                 TEMPR1 = 0.0
                 call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                 if ( (TEMPR1+(FMAX*DT)) <= DP ) then
                    if ( (I_MAX-I_0-TEMPR1-(FMAX*DT)) <= 0.0 ) then
                       YD        = I_MAX - I_0
                       INFILTRTN = TOP_MAX_MOIST - TOP_MOIST
                       RUNOFFSAT = DP - INFILTRTN
                       RUNOFFINF = 0.0
                       TOP_MOIST = TOP_MAX_MOIST
                       I_0       = I_MAX
                       goto 2001
                    else
                       YD        = 0.0
                       do IZ = 1, IZMAX ! loop : IITERATION2
                          YD_OLD = YD
                          TEMPR1 = 0.0
                          call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                          YD     = TEMPR1 + (FSUR*DT)
                          if ( (abs(YD-YD_OLD) <= ERROR) .or. (IZ == IZMAX) ) then
                             goto 1003
                          endif
                       enddo
                    endif
                 else
                    YD = DP/2.0
                    do IZ = 1, IZMAX ! loop : IITERATION30
                       YD_OLD = YD
                       TEMPR1 = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                       YD     = YD - TEMPR1 - (FSUR*DT) + DP
                       if ( YD <= 0.0 ) YD = 0.0
                       if ( YD >= DP  ) YD = DP
                       if ( (abs(YD-YD_OLD) <= ERROR) .or. (IZ == IZMAX) ) then
                          YD0 = YD
                          exit
                       endif
                    enddo
                    do IZ = 1, IZMAX ! loop : IITERATION3
                       YD_OLD = YD
                       TEMPR1 = 0.0
                       TEMPR2 = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                       call RunoffInfilExcessDynamicVic(YD,YD0,TEMPR1,FMAX,FSUR,DT,DP,BB,TEMPR2)
                       YD     = DP - TEMPR2
                       if ( (abs(YD-YD_OLD) <= ERROR) .or. (IZ == IZMAX) ) then
                          goto 1003
                       endif
                    enddo
1003                if ( YD <= 0.0 ) YD = 0.0
                    if ( YD >= DP  ) YD = DP
                    call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,R1)
                    RUNOFFSAT = R1
                    RUNOFFINF = DP - YD
                    INFILTRTN = YD - RUNOFFSAT
                    TOP_MOIST = TOP_MOIST + INFILTRTN
                    YD        = I_0+YD
                    if ( TOP_MOIST <= 0.0 ) TOP_MOIST = 0.0
                    if ( TOP_MOIST >= TOP_MAX_MOIST ) TOP_MOIST = TOP_MAX_MOIST
                    I_0       = I_MAX * (1.0-(1.0-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+BDVIC))))
                    goto 2001
                 endif
              endif
           else
              if ( (FMAX*DT) >= DP) then
                 YD = DP / 2.0
                 do IZ = 1, IZMAX ! ITERATION1
                    YD_OLD = YD
                    TEMPR1 = 0.0
                    call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                    YD = TEMPR1 + ((FSUR*DT) * (1.0 - (1.0-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
                    if ( (abs(YD-YD_OLD) <= ERROR) .or. (IZ == IZMAX) ) then
                       goto 1004
                    endif
                 enddo
              else
                 TEMPR1 = 0.0
                 call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                 if ( (TEMPR1+(FMAX*DT)) <= DP ) then
                    YD = DP / 2.0
                    do IZ = 1, IZMAX ! ITERATION2
                       YD_OLD = YD
                       TEMPR1 = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                       YD     = TEMPR1+(FSUR*DT)
                       if ( (abs(YD-YD_OLD) <= ERROR) .or. (IZ == IZMAX) ) then
                          goto 1004
                       endif
                    enddo
                 else
                    YD = 0.0
                    do IZ = 1, IZMAX ! ITERATION30
                       YD_OLD = YD
                       TEMPR1 = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                       YD     = (DP - (FMAX*DT)) + YD - TEMPR1
                       if ( YD <= 0.0 ) YD = 0.0
                       if ( YD >= DP  ) YD = DP
                       TEMPR1 = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                       if ( (abs(TEMPR1+(FMAX*DT)-DP) <= ERROR) .or. (IZ == IZMAX) ) then
                          YD0 = YD
                          exit
                       endif
                    enddo
                    do IZ = 1, IZMAX ! ITERATION3
                       YD_OLD = YD
                       TEMPR1 = 0.0
                       TEMPR2 = 0.0
                       call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,TEMPR1)
                       call RunoffInfilExcessDynamicVic(YD,YD0,TEMPR1,FMAX,FSUR,DT,DP,BB,TEMPR2)
                       YD     = DP - TEMPR2
                       if ( (abs(YD-YD_OLD) <= ERROR) .or. (IZ == IZMAX) ) then
                          goto 1004
                       endif
                    enddo
                 endif
              endif
1004          if ( YD <= 0.0 ) YD = 0.0
              if ( YD >= DP  ) YD = DP
              R1 = 0.0
              call RunoffSatExcessDynamicVic(noahmp,I_0,I_MAX,YD,R1)
              RUNOFFSAT = R1
              RUNOFFINF = DP - YD
              INFILTRTN = YD - RUNOFFSAT
              TOP_MOIST = TOP_MOIST + INFILTRTN
              if ( TOP_MOIST <= 0.0 ) TOP_MOIST = 0.0
              if ( TOP_MOIST >= TOP_MAX_MOIST ) TOP_MOIST = TOP_MAX_MOIST
              I_0       = I_MAX * (1.0-(1.0-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+BDVIC))))
           endif
        endif
     endif

2001 RUNSRF = (RUNOFFSAT + RUNOFFINF) / DT
     RUNSRF = min( RUNSRF, QINSUR )
     RUNSRF = min( RUNSRF, 0.0    )
     PDDUM  = QINSUR - RUNSRF

    end associate

  end subroutine SurfaceRunoffDynamicVic

end module RunoffSurfaceDynamicVicMod
