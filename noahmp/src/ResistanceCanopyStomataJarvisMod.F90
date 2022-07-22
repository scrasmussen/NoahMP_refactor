module ResistanceCanopyStomataJarvisMod

!!! Compute canopy stomatal resistance and foliage photosynthesis based on Jarvis scheme
!!! Canopy resistance which depends on incoming solar radiation, air temperature,
!!! atmospheric water vapor pressure deficit at the lowest model level, and soil moisture (preferably
!!! unfrozen soil moisture rather than total). 
!!! Source: Jarvis (1976), Noilhan and Planton (1989), Jacquemin and Noilhan (1990). 
!!! See also Chen et al (1996, JGR, Vol 101(D3), 7251-7268): Eqns 12-14 and Table 2 of Sec. 3.1.2

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use HumiditySaturationMod, only : HumiditySaturation

  implicit none

contains

  subroutine ResistanceCanopyStomataJarvis(noahmp, IndexShade)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CANRES
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    integer          , intent(in   ) :: IndexShade   ! index for sunlit/shaded (0=sunlit;1=shaded)
    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: RCQ          ! canopy resistance multiplier
    real(kind=kind_noahmp)           :: RCS          ! canopy resistance multiplier
    real(kind=kind_noahmp)           :: RCT          ! canopy resistance multiplier
    real(kind=kind_noahmp)           :: FF
    real(kind=kind_noahmp)           :: Q2           ! water vapor mixing ratio (kg/kg)
    real(kind=kind_noahmp)           :: Q2SAT        ! saturation Q2
    real(kind=kind_noahmp)           :: DQSDT2       ! d(Q2SAT)/d(T)

! --------------------------------------------------------------------
    associate(                                                        &
              PressureAirRefHeight => noahmp%forcing%PressureAirRefHeight,& ! in,  air pressure [Pa] at reference height
              SoilTranspFacAcc           => noahmp%water%state%SoilTranspFacAcc            ,& ! in,    accumulated soil water transpiration factor (0 to 1)
              RadiationStressFac             => noahmp%energy%param%RadiationStressFac             ,& ! in,    Parameter used in radiation stress function
              ResistanceStomataMin           => noahmp%energy%param%ResistanceStomataMin           ,& ! in,    Minimum stomatal resistance [s m-1]
              ResistanceStomataMax           => noahmp%energy%param%ResistanceStomataMax           ,& ! in,    Maximal stomatal resistance [s m-1]
              AirTempOptimTransp            => noahmp%energy%param%AirTempOptimTransp            ,& ! in,    Optimum transpiration air temperature [K]
              VaporPresDeficitFac              => noahmp%energy%param%VaporPresDeficitFac              ,& ! in,    Parameter used in vapor pressure deficit function
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              EAH             => noahmp%energy%state%EAH             ,& ! in,    canopy air vapor pressure (pa)
              RadPhotoActAbsSunlit          => noahmp%energy%flux%RadPhotoActAbsSunlit           ,& ! in,    average absorbed par for sunlit leaves (w/m2)
              RadPhotoActAbsShade          => noahmp%energy%flux%RadPhotoActAbsShade           ,& ! in,    average absorbed par for shaded leaves (w/m2)
              RSSUN           => noahmp%energy%state%RSSUN           ,& ! out,   sunlit leaf stomatal resistance (s/m)
              RSSHA           => noahmp%energy%state%RSSHA           ,& ! out,   shaded leaf stomatal resistance (s/m)
              PhotosynLeafSunlit          => noahmp%biochem%flux%PhotosynLeafSunlit          ,& ! out,   sunlit leaf photosynthesis (umol co2 /m2 /s)
              PhotosynLeafShade          => noahmp%biochem%flux%PhotosynLeafShade           & ! out,   shaded leaf photosynthesis (umol co2 /m2 /s)
             )
! ----------------------------------------------------------------------

    ! initialization
    RCS    = 0.0
    RCT    = 0.0
    RCQ    = 0.0

    ! Sunlit case
    if ( IndexShade == 0 ) then
       RSSUN  = 0.0

       ! compute Q2 and Q2SAT
       Q2 = 0.622 *  EAH  / (PressureAirRefHeight - 0.378 * EAH) ! specific humidity [kg/kg]
       Q2 = Q2 / (1.0 + Q2)                        ! mixing ratio [kg/kg]
       call HumiditySaturation(TV, PressureAirRefHeight, Q2SAT, DQSDT2)

       ! contribution due to incoming solar radiation
       FF  = 2.0 * RadPhotoActAbsSunlit / RadiationStressFac
       RCS = (FF + ResistanceStomataMin / ResistanceStomataMax) / (1.0 + FF)
       RCS = max( RCS, 0.0001 )

       ! contribution due to air temperature
       RCT = 1.0 - 0.0016 * ( (AirTempOptimTransp - TV)**2.0 )
       RCT = max( RCT, 0.0001 )

       ! contribution due to vapor pressure deficit
       RCQ = 1.0 / ( 1.0 + VaporPresDeficitFac * max(0.0, Q2SAT-Q2) )
       RCQ = max( RCQ, 0.01 )

       ! determine canopy resistance due to all factors
       RSSUN  = ResistanceStomataMin / (RCS * RCT * RCQ * SoilTranspFacAcc)
       PhotosynLeafSunlit = -999.99       ! photosynthesis not applied for dynamic carbon

    endif ! IndexShade == 0

    ! Shaded case
    ! same as Sunlit case but using shaded input and output
    if ( IndexShade == 1 ) then
       RSSHA  = 0.0
       
       ! compute Q2 and Q2SAT
       Q2 = 0.622 *  EAH  / (PressureAirRefHeight - 0.378 * EAH) ! specific humidity [kg/kg]
       Q2 = Q2 / (1.0 + Q2)                        ! mixing ratio [kg/kg]
       call HumiditySaturation(TV, PressureAirRefHeight, Q2SAT, DQSDT2)

       ! contribution due to incoming solar radiation
       FF  = 2.0 * RadPhotoActAbsShade / RadiationStressFac
       RCS = (FF + ResistanceStomataMin / ResistanceStomataMax) / (1.0 + FF)
       RCS = max( RCS, 0.0001 )

       ! contribution due to air temperature
       RCT = 1.0 - 0.0016 * ( (AirTempOptimTransp - TV)**2.0 )
       RCT = max( RCT, 0.0001 )

       ! contribution due to vapor pressure deficit
       RCQ = 1.0 / ( 1.0 + VaporPresDeficitFac * max(0.0, Q2SAT-Q2) )
       RCQ = max( RCQ, 0.01 )

       ! determine canopy resistance due to all factors
       RSSHA  = ResistanceStomataMin / (RCS * RCT * RCQ * SoilTranspFacAcc)
       PhotosynLeafShade = -999.99       ! photosynthesis not applied for dynamic carbon

    endif ! IndexShade == 1

    end associate

  end subroutine ResistanceCanopyStomataJarvis

end module ResistanceCanopyStomataJarvisMod
