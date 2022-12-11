module NoahmpDriverMainMod

! ------------------------ Code history -----------------------------------
! Original Noah-MP module: module_sf_noahmpdrv
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (April 08, 2022)
! -------------------------------------------------------------------------

  use Machine
  use ConstantDefineMod
  use NoahmpVarType
  use NoahmpIOVarType
  use ConfigVarInitMod
  use EnergyVarInitMod
  use ForcingVarInitMod
  use WaterVarInitMod
  use BiochemVarInitMod
  use ConfigVarOutMod
  use EnergyVarOutMod
  use WaterVarOutMod
  use BiochemVarOutMod
  use NoahmpMainMod
  use NoahmpMainGlacierMod
  use module_ra_gfdleta,  only: cal_mon_day
!-------------------------------
#if ( WRF_CHEM == 1 )
  USE module_data_gocart_dust
#endif
!-------------------------------

  implicit none
  
contains  

  subroutine NoahmpDriverMain (NoahmpIO)
  
! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: noahmplsm
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (April 08, 2022)
! ------------------------------------------------------------------------- 
 
    implicit none 
    
    type(NoahmpIO_type)                 :: NoahmpIO
    
    !local
    type(noahmp_type)                   :: noahmp

    integer                             :: I
    integer                             :: J
    integer                             :: K
    integer                             :: JMONTH, JDAY
    LOGICAL                             :: IPRINT = .false.     ! debug printout
    real(kind=kind_noahmp)              :: SOLAR_TIME 
    real(kind=kind_noahmp), parameter   :: undefined_value = -1.E36
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: SAND
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: CLAY
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: ORGM

    !---------------------------------------------------------------------
    !  Treatment of Noah-MP soil timestep
    !---------------------------------------------------------------------
    NoahmpIO%calculate_soil    = .false.
    NoahmpIO%soil_update_steps = nint(NoahmpIO%soiltstep / NoahmpIO%DTBL)
    NoahmpIO%soil_update_steps = max(NoahmpIO%soil_update_steps,1)

    if ( NoahmpIO%soil_update_steps == 1 ) then
       NoahmpIO%ACC_SSOILXY  = 0.0
       NoahmpIO%ACC_QINSURXY = 0.0
       NoahmpIO%ACC_QSEVAXY  = 0.0
       NoahmpIO%ACC_ETRANIXY = 0.0
       NoahmpIO%ACC_DWATERXY = 0.0
       NoahmpIO%ACC_PRCPXY   = 0.0
       NoahmpIO%ACC_ECANXY   = 0.0
       NoahmpIO%ACC_ETRANXY  = 0.0
       NoahmpIO%ACC_EDIRXY   = 0.0
    endif

    if ( NoahmpIO%soil_update_steps > 1 ) then
       if ( mod(NoahmpIO%itimestep, NoahmpIO%soil_update_steps) == 1 ) then
          NoahmpIO%ACC_SSOILXY  = 0.0
          NoahmpIO%ACC_QINSURXY = 0.0
          NoahmpIO%ACC_QSEVAXY  = 0.0
          NoahmpIO%ACC_ETRANIXY = 0.0
          NoahmpIO%ACC_DWATERXY = 0.0
          NoahmpIO%ACC_PRCPXY   = 0.0
          NoahmpIO%ACC_ECANXY   = 0.0
          NoahmpIO%ACC_ETRANXY  = 0.0
          NoahmpIO%ACC_EDIRXY   = 0.0
       end if
    endif

    if ( mod(NoahmpIO%itimestep, NoahmpIO%soil_update_steps) == 0 ) NoahmpIO%calculate_soil = .true.
 
    !---------------------------------------------------------------------
    !  Prepare Noah-MP driver
    !---------------------------------------------------------------------
    
    NoahmpIO%YEARLEN = 365                               ! find length of year for phenology (also S Hemisphere)
    if (mod(NoahmpIO%YR,4) == 0)then
       NoahmpIO%YEARLEN = 366
       if (mod(NoahmpIO%YR,100) == 0)then
          NoahmpIO%YEARLEN = 365
          if (mod(NoahmpIO%YR,400) == 0)then
             NoahmpIO%YEARLEN = 366
          endif
       endif
    endif

    NoahmpIO%ZSOIL(1) = -NoahmpIO%DZS(1)                    ! depth to soil interfaces (<0) [m]
    do K = 2, NoahmpIO%NSOIL
       NoahmpIO%ZSOIL(K) = -NoahmpIO%DZS(K) + NoahmpIO%ZSOIL(K-1)
    enddo
    
    JLOOP : do J=NoahmpIO%jts,NoahmpIO%jte
       NoahmpIO%J = J
       if(NoahmpIO%ITIMESTEP == 1)then
          do I=NoahmpIO%its,NoahmpIO%ite
             if((NoahmpIO%XLAND(I,J)-1.5) >= 0.)then          ! Open water case
                if(NoahmpIO%XICE(I,J) == 1. .AND. IPRINT) print*,' sea-ice at water point, I=',I,'J=',J
                NoahmpIO%SMSTAV(I,J) = 1.0
                NoahmpIO%SMSTOT(I,J) = 1.0
                do K = 1, NoahmpIO%NSOIL
                   NoahmpIO%SMOIS(I,K,J) = 1.0
                   NoahmpIO%TSLB(I,K,J) = 273.16
                enddo
             else
                if(NoahmpIO%XICE(I,J) == 1.)then              ! Sea-ice case
                   NoahmpIO%SMSTAV(I,J) = 1.0
                   NoahmpIO%SMSTOT(I,J) = 1.0
                   do K = 1, NoahmpIO%NSOIL
                      NoahmpIO%SMOIS(I,K,J) = 1.0
                   enddo
                endif
             endif
          enddo
       endif                                                  ! end of initialization over ocean
!-----------------------------------------------------------------------
       ILOOP : do I = NoahmpIO%its, NoahmpIO%ite

       NoahmpIO%I = I
       if (NoahmpIO%XICE(I,J) >= NoahmpIO%XICE_THRESHOLD)then
          NoahmpIO%ICE                          = 1           ! Sea-ice point
          NoahmpIO%SH2O (I,1:NoahmpIO%NSOIL,J)  = 1.0
          NoahmpIO%LAI  (I,J)                   = 0.01
          cycle ILOOP                                         ! Skip any processing at sea-ice points
       else
          if((NoahmpIO%XLAND(I,J)-1.5) >= 0.) cycle ILOOP     ! Open water case

          !---------------------------------------------------------------------
          !  initialize Data Types
          !  Transfer all the inputs from NoahmpIO to noahmp
          !---------------------------------------------------------------------

          call ConfigVarInitDefault  (noahmp)
          call ConfigVarInitTransfer (noahmp, NoahmpIO)
          call ForcingVarInitDefault (noahmp)
          call ForcingVarInitTransfer(noahmp, NoahmpIO)
          call EnergyVarInitDefault  (noahmp)
          call EnergyVarInitTransfer (noahmp, NoahmpIO)
          call WaterVarInitDefault   (noahmp)
          call WaterVarInitTransfer  (noahmp, NoahmpIO)
          call BiochemVarInitDefault (noahmp)
          call BiochemVarInitTransfer(noahmp, NoahmpIO)

          !---------------------------------------------------------------------
          !  hydrological processes for vegetation in urban model
          !  irrigate vegetaion only in urban area, MAY-SEP, 9-11pm
          !---------------------------------------------------------------------
          ! need to be separated from main Noah-MP into urban specific module in the future 
          if(NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_1_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_2_TABLE   .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_3_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_4_TABLE   .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_5_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_6_TABLE   .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_7_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_8_TABLE   .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_9_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_10_TABLE  .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_11_TABLE ) THEN

             if(NoahmpIO%SF_URBAN_PHYSICS > 0 .AND. NoahmpIO%IRI_URBAN == 1 ) then
                SOLAR_TIME = (NoahmpIO%JULIAN - INT(NoahmpIO%JULIAN))*24 + NoahmpIO%XLONG(I,J)/15.0
                if(SOLAR_TIME < 0.) SOLAR_TIME = SOLAR_TIME + 24.
                call CAL_MON_DAY(INT(NoahmpIO%JULIAN),NoahmpIO%YR,JMONTH,JDAY)
                if (SOLAR_TIME >= 21. .AND. SOLAR_TIME <= 23. .AND. JMONTH >= 5 .AND. JMONTH <= 9) then
                    noahmp%water%state%SoilMoisture(1) = &
                      max(noahmp%water%state%SoilMoisture(1),noahmp%water%param%SoilMoistureFieldCap(1))
                    noahmp%water%state%SoilMoisture(2) = &
                      max(noahmp%water%state%SoilMoisture(2),noahmp%water%param%SoilMoistureFieldCap(2))
                endif
             endif
          endif

          !------------------------------------------------------------------------
          !  Call 1D Noah-MP LSM  
          !------------------------------------------------------------------------
         
          ! glacier ice
          if (noahmp%config%domain%VegType == noahmp%config%domain%IndexIcePoint ) then
              noahmp%config%domain%IndicatorIceSfc = -1                          ! Land-ice point      
              noahmp%forcing%TemperatureSoilBottom = min(noahmp%forcing%TemperatureSoilBottom,263.15) ! set deep temp to at most -10C

              call NoahmpMainGlacier(noahmp)

          ! non-glacier land
          else
              noahmp%config%domain%IndicatorIceSfc = 0          ! Neither sea ice or land ice.
         
              call NoahmpMain(noahmp)
              
          endif ! glacial split ends 
 
        !---------------------------------------------------------------------
        !  Transfer Noah-MP states to output      
        !---------------------------------------------------------------------

        call ConfigVarOutTransfer (noahmp, NoahmpIO)
        call EnergyVarOutTransfer (noahmp, NoahmpIO)
        call WaterVarOutTransfer  (noahmp, NoahmpIO)
        call BiochemVarOutTransfer(noahmp, NoahmpIO) 

        endif    ! land-sea split ends

      enddo ILOOP    ! I loop
    enddo  JLOOP     ! J loop
              
  end subroutine NoahmpDriverMain
  
end module NoahmpDriverMainMod  
