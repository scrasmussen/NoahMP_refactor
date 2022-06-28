module ResistanceCanopyStomataBallBerryMod

!!! Compute canopy stomatal resistance and foliage photosynthesis based on Ball-Berry scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceCanopyStomataBallBerry(noahmp, IndexShade)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: STOMATA
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    integer          , intent(in   ) :: IndexShade   ! index for sunlit/shaded (0=sunlit;1=shaded)
    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: ITER        ! iteration index
    integer, parameter               :: NITER = 3   ! number of iterations
    real(kind=kind_noahmp)           :: MPE         ! prevents overflow for division by zero
    real(kind=kind_noahmp)           :: RLB         ! boundary layer resistance (s m2 / umol)
    real(kind=kind_noahmp)           :: TC          ! foliage temperature (degree Celsius)
    real(kind=kind_noahmp)           :: CS          ! co2 concentration at leaf surface (pa)
    real(kind=kind_noahmp)           :: KC          ! co2 Michaelis-Menten constant (pa)
    real(kind=kind_noahmp)           :: KO          ! o2 Michaelis-Menten constant (pa)
    real(kind=kind_noahmp)           :: A,B,C,Q     ! intermediate calculations for RS
    real(kind=kind_noahmp)           :: R1,R2       ! roots for RS
    real(kind=kind_noahmp)           :: FNF         ! foliage nitrogen adjustment factor (0 to 1)
    real(kind=kind_noahmp)           :: PPF         ! absorb photosynthetic photon flux (umol photons/m2/s)
    real(kind=kind_noahmp)           :: WC          ! Rubisco limited photosynthesis (umol co2/m2/s)
    real(kind=kind_noahmp)           :: WJ          ! light limited photosynthesis (umol co2/m2/s)
    real(kind=kind_noahmp)           :: WE          ! export limited photosynthesis (umol co2/m2/s)
    real(kind=kind_noahmp)           :: CP          ! co2 compensation point (pa)
    real(kind=kind_noahmp)           :: CI          ! internal co2 (pa)
    real(kind=kind_noahmp)           :: AWC         ! intermediate calculation for wc
    real(kind=kind_noahmp)           :: VCMX        ! maximum rate of carbonylation (umol co2/m2/s)
    real(kind=kind_noahmp)           :: J           ! electron transport (umol co2/m2/s)
    real(kind=kind_noahmp)           :: CEA         ! constrain ea or else model blows up
    real(kind=kind_noahmp)           :: CF          ! s m2/umol -> s/m
    real(kind=kind_noahmp)           :: T           ! temporary var
! local statement functions
    real(kind=kind_noahmp)           :: F1          ! generic temperature response (statement function)
    real(kind=kind_noahmp)           :: F2          ! generic temperature inhibition (statement function)
    real(kind=kind_noahmp)           :: AB          ! used in statement functions
    real(kind=kind_noahmp)           :: BC          ! used in statement functions
    F1(AB, BC) = AB**( (BC - 25.0) / 10.0 )
    F2(AB)     = 1.0 + exp( (-2.2e05 + 710.0 * (AB + 273.16)) / (8.314 * (AB + 273.16)) )

! --------------------------------------------------------------------
    associate(                                                        &
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight   ,& ! in,    air pressure [Pa] at reference height
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight,& ! in,    air temperature [K] at reference height
              BTRAN           => noahmp%water%state%BTRAN            ,& ! in,    soil water transpiration factor (0 to 1)
              IGS             => noahmp%biochem%state%IGS            ,& ! in,    growing season index (0=off, 1=on)
              FOLN            => noahmp%biochem%state%FOLN           ,& ! in,    foliage nitrogen concentration (%)
              FOLNMX          => noahmp%biochem%param%FOLNMX         ,& ! in,    foliage nitrogen concentration when f(n)=1 (%)
              QE25            => noahmp%biochem%param%QE25           ,& ! in,    quantum efficiency at 25c (umol co2 / umol photon)
              VCMX25          => noahmp%biochem%param%VCMX25         ,& ! in,    maximum rate of carboxylation at 25c (umol co2/m**2/s)
              AVCMX           => noahmp%biochem%param%AVCMX          ,& ! in,    q10 for vcmx25
              C3PSN           => noahmp%biochem%param%C3PSN          ,& ! in,    photosynthetic pathway: 0. = c4, 1. = c3
              MP              => noahmp%biochem%param%MP             ,& ! in,    slope of conductance-to-photosynthesis relationship
              KC25            => noahmp%energy%param%KC25            ,& ! in,    co2 michaelis-menten constant at 25c (pa)
              KO25            => noahmp%energy%param%KO25            ,& ! in,    o2 michaelis-menten constant at 25c (pa)
              AKC             => noahmp%energy%param%AKC             ,& ! in,    q10 for kc25
              AKO             => noahmp%energy%param%AKO             ,& ! in,    q10 for ko25
              BP              => noahmp%energy%param%BP              ,& ! in,    minimum leaf conductance (umol/m**2/s)
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              ESTV            => noahmp%energy%state%ESTV            ,& ! in,    saturation vapor pressure at TV (pa)
              EAH             => noahmp%energy%state%EAH             ,& ! in,    canopy air vapor pressure (pa)
              O2              => noahmp%energy%state%O2AIR           ,& ! in,    atmospheric o2 concentration (pa)
              CO2             => noahmp%energy%state%CO2AIR          ,& ! in,    atmospheric co2 concentration (pa)
              RB              => noahmp%energy%state%RB              ,& ! in,    leaf boundary layer resistance (s/m)
              PARSUN          => noahmp%energy%flux%PARSUN           ,& ! in,    average absorbed par for sunlit leaves (w/m2)
              PARSHA          => noahmp%energy%flux%PARSHA           ,& ! in,    average absorbed par for shaded leaves (w/m2)
              RSSUN           => noahmp%energy%state%RSSUN           ,& ! out,   sunlit leaf stomatal resistance (s/m)
              RSSHA           => noahmp%energy%state%RSSHA           ,& ! out,   shaded leaf stomatal resistance (s/m)
              PSNSUN          => noahmp%biochem%flux%PSNSUN          ,& ! out,   sunlit leaf photosynthesis (umol co2 /m2 /s)
              PSNSHA          => noahmp%biochem%flux%PSNSHA           & ! out,   shaded leaf photosynthesis (umol co2 /m2 /s)
             )
! ----------------------------------------------------------------------

    ! initialization
    MPE = 1.0e-6

    ! Sunlit case
    if ( IndexShade == 0 ) then

       ! initialize RS=RSMAX and PSN=0 because will only do calculations
       ! for PARSUN  > 0, in which case RS <= RSMAX and PSN >= 0
       CF = PressureAirRefHeight / (8.314 * TemperatureAirRefHeight) * 1.0e06  ! unit conversion factor
       RSSUN  = 1.0 / BP * CF  
       PSNSUN = 0.0           

       if ( PARSUN > 0.0 ) then
          FNF  = min( FOLN / max(MPE, FOLNMX), 1.0 )
          TC   = TV - ConstFreezePoint
          PPF  = 4.6 * PARSUN
          J    = PPF * QE25
          KC   = KC25 * F1(AKC, TC)
          KO   = KO25 * F1(AKO, TC)
          AWC  = KC * ( 1.0 + O2 / KO )
          CP   = 0.5 * KC / KO * O2 * 0.21
          VCMX = VCMX25 / F2(TC) * FNF * BTRAN * F1(AVCMX, TC)
          ! first guess ci
          CI = 0.7 * CO2 * C3PSN + 0.4 * CO2 * (1.0 - C3PSN)
          ! rb: s/m -> s m**2 / umol
          RLB = RB / CF
          ! constrain EAH
          CEA = max( 0.25*ESTV*C3PSN + 0.40*ESTV*(1.0-C3PSN), min(EAH,ESTV) )

          ! ci iteration
          do ITER = 1, NITER
             WJ     = max(CI-CP, 0.0) * J / (CI + 2.0*CP) * C3PSN + J * (1.0 - C3PSN)
             WC     = max(CI-CP, 0.0) * VCMX / (CI + AWC) * C3PSN + VCMX * (1.0 - C3PSN)
             WE     = 0.5 * VCMX * C3PSN + 4000.0 * VCMX * CI / PressureAirRefHeight * (1.0 - C3PSN)
             PSNSUN = min( WJ, WC, WE ) * IGS
             CS     = max( CO2 - 1.37*RLB*PressureAirRefHeight*PSNSUN, MPE )
             A      = MP * PSNSUN * PressureAirRefHeight * CEA / (CS * ESTV) + BP
             B      = ( MP * PSNSUN * PressureAirRefHeight / CS + BP ) * RLB - 1.0
             C      = -RLB
             if ( B >= 0.0 ) then
                Q   = -0.5 * ( B + sqrt(B*B - 4.0*A*C) )
             else
                Q   = -0.5 * ( B - sqrt(B*B - 4.0*A*C) )
             endif
             R1     = Q / A
             R2     = C / Q
             RSSUN  = max(R1, R2)
             CI     = max( CS - PSNSUN*PressureAirRefHeight*1.65*RSSUN, 0.0 )
          enddo

          ! rs, rb:  s m**2 / umol -> s/m
          RSSUN = RSSUN * CF
       endif ! PARSUN > 0.0

    endif ! IndexShade == 0

    ! Shaded case
    ! same as Sunlit case but using different input (PARSHA) and output (RSSHA,PSNSHA)
    if ( IndexShade == 1 ) then

       ! initialize RS=RSMAX and PSN=0 because will only do calculations
       ! for PARSHA  > 0, in which case RS <= RSMAX and PSN >= 0
       CF = PressureAirRefHeight / (8.314 * TemperatureAirRefHeight) * 1.0e06  ! unit conversion factor
       RSSHA  = 1.0 / BP * CF
       PSNSHA = 0.0

       if ( PARSHA > 0.0 ) then
          FNF  = min( FOLN / max(MPE, FOLNMX), 1.0 )
          TC   = TV - ConstFreezePoint
          PPF  = 4.6 * PARSHA
          J    = PPF * QE25
          KC   = KC25 * F1(AKC, TC)
          KO   = KO25 * F1(AKO, TC)
          AWC  = KC * ( 1.0 + O2 / KO )
          CP   = 0.5 * KC / KO * O2 * 0.21
          VCMX = VCMX25 / F2(TC) * FNF * BTRAN * F1(AVCMX, TC)
          ! first guess ci
          CI = 0.7 * CO2 * C3PSN + 0.4 * CO2 * (1.0 - C3PSN)
          ! rb: s/m -> s m**2 / umol
          RLB = RB / CF
          ! constrain EAH
          CEA = max( 0.25*ESTV*C3PSN + 0.40*ESTV*(1.0-C3PSN), min(EAH,ESTV) )

          ! ci iteration
          do ITER = 1, NITER
             WJ     = max(CI-CP, 0.0) * J / (CI + 2.0*CP) * C3PSN + J * (1.0 - C3PSN)
             WC     = max(CI-CP, 0.0) * VCMX / (CI + AWC) * C3PSN + VCMX * (1.0 - C3PSN)
             WE     = 0.5 * VCMX * C3PSN + 4000.0 * VCMX * CI / PressureAirRefHeight * (1.0 - C3PSN)
             PSNSHA = min( WJ, WC, WE ) * IGS
             CS     = max( CO2 - 1.37*RLB*PressureAirRefHeight*PSNSHA, MPE )
             A      = MP * PSNSHA * PressureAirRefHeight * CEA / (CS * ESTV) + BP
             B      = ( MP * PSNSHA * PressureAirRefHeight / CS + BP ) * RLB - 1.0
             C      = -RLB
             if ( B >= 0.0 ) then
                Q   = -0.5 * ( B + sqrt(B*B - 4.0*A*C) )
             else
                Q   = -0.5 * ( B - sqrt(B*B - 4.0*A*C) )
             endif
             R1     = Q / A
             R2     = C / Q
             RSSHA  = max(R1, R2)
             CI     = max( CS - PSNSHA*PressureAirRefHeight*1.65*RSSHA, 0.0 )
          enddo

          ! rs, rb:  s m**2 / umol -> s/m
          RSSHA = RSSHA * CF
       endif ! PARSHA > 0.0

    endif ! IndexShade == 1


    end associate

  end subroutine ResistanceCanopyStomataBallBerry

end module ResistanceCanopyStomataBallBerryMod
