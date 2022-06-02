module NoahmpDriverMainMod

! ------------------------ Code history -----------------------------------
! Original Noah-MP module: module_sf_noahmpdrv
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (April 08, 2022)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp
  use ConstantDefineMod
  use NoahmpVarType
  use NoahmpIOVarType
  use NoahmpIOVarInitMod
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
  use module_sf_urban,    only: IRI_SCHEME 
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
    integer                             :: ICE
    integer                             :: JMONTH, JDAY
    LOGICAL                             :: IPRINT    =  .false.     ! debug printout
    real(kind=kind_noahmp)              :: SOLAR_TIME
   
    real(kind=kind_noahmp), parameter   :: undefined_value = -1.E36
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: SAND
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: CLAY
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: ORGM
  
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
          
          if(NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_1_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_2_TABLE   .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_3_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_4_TABLE   .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_5_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_6_TABLE   .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_7_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_8_TABLE   .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_9_TABLE  .or. &
             NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_10_TABLE  .or. NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_11_TABLE ) THEN

             if(NoahmpIO%SF_URBAN_PHYSICS > 0 .AND. IRI_SCHEME == 1 ) then
                SOLAR_TIME = (NoahmpIO%JULIAN - INT(NoahmpIO%JULIAN))*24 + NoahmpIO%XLONG(I,J)/15.0
                if(SOLAR_TIME < 0.) SOLAR_TIME = SOLAR_TIME + 24.
                call CAL_MON_DAY(INT(NoahmpIO%JULIAN),NoahmpIO%YR,JMONTH,JDAY)
                if (SOLAR_TIME >= 21. .AND. SOLAR_TIME <= 23. .AND. JMONTH >= 5 .AND. JMONTH <= 9) then
                    noahmp%water%state%SMC(1) = &
                      max(noahmp%water%state%SMC(1),noahmp%water%param%SMCREF(1))
                    noahmp%water%state%SMC(2) = &
                      max(noahmp%water%state%SMC(2),noahmp%water%param%SMCREF(2))
                endif
             endif
          endif

          !------------------------------------------------------------------------
          !  Call 1D Noah-MP LSM  
          !------------------------------------------------------------------------
          
          if(noahmp%config%domain%VEGTYP == 25) noahmp%energy%state%FVEG = 0.0  ! Set playa, lava, sand to bare
          if(noahmp%config%domain%VEGTYP == 25) noahmp%energy%state%LAI  = 0.0 
          if(noahmp%config%domain%VEGTYP == 26) noahmp%energy%state%FVEG = 0.0  ! hard coded for USGS
          if(noahmp%config%domain%VEGTYP == 26) noahmp%energy%state%LAI  = 0.0
          if(noahmp%config%domain%VEGTYP == 27) noahmp%energy%state%FVEG = 0.0
          if(noahmp%config%domain%VEGTYP == 27) noahmp%energy%state%LAI  = 0.0

          if (noahmp%config%domain%VEGTYP == noahmp%config%domain%ISICE ) then
              noahmp%config%domain%ICE = -1                                       ! Land-ice point      
              noahmp%forcing%TBOT = MIN(noahmp%forcing%TBOT,263.15)               ! set deep temp to at most -10C

              !---------------------------------------------------------------------
              !  Call 1D Noah-MP LSM for glacier points
              !---------------------------------------------------------------------

              call NoahmpMainGlacier(noahmp)

              !---------------------------------------------------------------------
              !  Transfer Noah-MP glacial states for output  
              !---------------------------------------------------------------------
              
              noahmp%water%state%FSNO   = 1.0  
              noahmp%energy%state%FVEG  = 0.0
              noahmp%energy%state%Z0WRF = 0.002 
              noahmp%energy%state%TGB   = noahmp%energy%state%TG   
              noahmp%energy%state%CHB   = noahmp%energy%state%CH 
              noahmp%energy%flux%IRB    = noahmp%energy%flux%FIRA
              noahmp%energy%flux%SHB    = noahmp%energy%flux%FSH
              noahmp%energy%flux%EVB    = noahmp%energy%flux%FGEV
              noahmp%energy%flux%GHB    = noahmp%energy%flux%SSOIL 
              NoahmpIO%QFX (I,J)        = noahmp%water%flux%EDIR
              NoahmpIO%LH  (I,J)        = noahmp%energy%flux%FGEV         
          else

              !---------------------------------------------------------------------
              !  Call 1D Noah-MP LSM for land points
              !---------------------------------------------------------------------
              
              noahmp%config%domain%ICE = 0                                        ! Neither sea ice or land ice.
         
              call NoahmpMain(noahmp)
              
              !---------------------------------------------------------------------
              !  Transfer QFX and LH for output      
              !---------------------------------------------------------------------
              
              NoahmpIO%QFX (I,J)        = noahmp%water%flux%ECAN  + &
                                          noahmp%water%flux%EDIR  + &
                                          noahmp%water%flux%ETRAN + &
                                          noahmp%water%flux%EIRR
                                     
              NoahmpIO%LH  (I,J)        = noahmp%energy%flux%FGEV + &
                                          noahmp%energy%flux%FCEV + &
                                          noahmp%energy%flux%FCTR + &
                                          noahmp%energy%flux%FIRR                                     
         
          endif ! glacial split ends 
       endif    ! land-sea test ends
 
       !---------------------------------------------------------------------
       !  Transfer Noah-MP states to output      
       !---------------------------------------------------------------------

       call ConfigVarOutTransfer (noahmp, NoahmpIO)
       call EnergyVarOutTransfer (noahmp, NoahmpIO)
       call WaterVarOutTransfer  (noahmp, NoahmpIO)
       call BiochemVarOutTransfer(noahmp, NoahmpIO) 

      enddo ILOOP    ! I loop
    enddo  JLOOP     ! J loop
              
  end subroutine NoahmpDriverMain
  
end module NoahmpDriverMainMod  
