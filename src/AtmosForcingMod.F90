module AtmosForcingMod

  use Machine 
  use NoahmpType
  use ConstantDefineMod

  implicit none

contains

  subroutine ProcessAtmosphericForcing(noahmp)    ! Original Subroutine: ATM

    type(noahmp_type), intent(inout) :: noahmp

! locals
    real(kind=kind_noahmp)             :: PAIR              !atm bottom level pressure (pa)
    real(kind=kind_noahmp)             :: PRCP_FROZEN       !total frozen precipitation [mm/s] ! MB/AN : v3.7
    real(kind=kind_noahmp), parameter  :: RHO_GRPL = 500.0  !graupel bulk density [kg/m3] ! MB/AN : v3.7
    real(kind=kind_noahmp), parameter  :: RHO_HAIL = 917.0  !hail bulk density [kg/m3]    ! MB/AN : v3.7

! wet-bulb scheme Wang et al., 2019 GRL, C.He, 12/18/2020
    real(kind=kind_noahmp)             :: ESATAIR           !saturated vapor pressure of air
    real(kind=kind_noahmp)             :: LATHEA            !latent heat of vapor/sublimation
    real(kind=kind_noahmp)             :: GAMMA_b           !(cp*p)/(eps*L)
    real(kind=kind_noahmp)             :: TDC               !air temperature [C]
    real(kind=kind_noahmp)             :: TWET              !wetbulb temperature
    integer                            :: ITER
    integer, parameter                 :: NITER = 10        !iterations for Twet calculation

! -------------------------------------------------------------------------------------------------

    associate(                                          &
              SFCPRS   => noahmp%forcing%SFCPRS        ,& !in
              SFCTMP   => noahmp%forcing%SFCTMP        ,& !in
              Q2       => noahmp%forcing%Q2            ,& !in
              PRCPCONV => noahmp%forcing%PRCPCONV      ,& !in
              PRCPNONC => noahmp%forcing%PRCPNONC      ,& !in
              PRCPSHCV => noahmp%forcing%PRCPSHCV      ,& !in
              PRCPSNOW => noahmp%forcing%PRCPSNOW      ,& !in
              PRCPGRPL => noahmp%forcing%PRCPGRPL      ,& !in
              PRCPHAIL => noahmp%forcing%PRCPHAIL      ,& !in
              SOLDN    => noahmp%forcing%SOLDN         ,& !in
              COSZ     => noahmp%config%domain%COSZ    ,& !in
              OPT_SNF  => noahmp%config%nmlist%OPT_SNF ,& !in
              THAIR    => noahmp%energy%state%THAIR    ,& !out
              QAIR     => noahmp%energy%state%QAIR     ,& !out
              EAIR     => noahmp%energy%state%EAIR     ,& !out
              RHOAIR   => noahmp%energy%state%RHOAIR   ,& !out
              SOLAD    => noahmp%energy%flux%SOLAD     ,& !out
              SOLAI    => noahmp%energy%flux%SOLAI     ,& !out
              SWDOWN   => noahmp%energy%flux%SWDOWN    ,& !out
              RAIN     => noahmp%water%flux%RAIN       ,& !out
              SNOW     => noahmp%water%flux%SNOW       ,& !out
              PRCP     => noahmp%water%flux%PRCP       ,& !out
              QPRECC   => noahmp%water%flux%QPRECC     ,& !out
              QPRECL   => noahmp%water%flux%QPRECL     ,& !out
              FP       => noahmp%water%state%FP        ,& !out
              FPICE    => noahmp%water%state%FPICE     ,& !out
              BDFALL   => noahmp%water%state%BDFALL     & !out
             )

! --------------------------------------------------------------------------------------------------

! jref: seems like PAIR should be P1000mb??
    PAIR   = SFCPRS                                        ! atm bottom level pressure (pa)
    THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 

    QAIR   = Q2                                            ! In WRF, driver converts to specific humidity
 
    EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
    RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)

    if(COSZ <= 0.0)then
      SWDOWN = 0.0
    else
      SWDOWN = SOLDN
    endif 

    SOLAD(1) = SWDOWN*0.7*0.5                              ! direct  vis
    SOLAD(2) = SWDOWN*0.7*0.5                              ! direct  nir
    SOLAI(1) = SWDOWN*0.3*0.5                              ! diffuse vis
    SOLAI(2) = SWDOWN*0.3*0.5                              ! diffuse nir

    PRCP = PRCPCONV + PRCPNONC + PRCPSHCV

    if(OPT_SNF == 4)then
      QPRECC = PRCPCONV + PRCPSHCV
      QPRECL = PRCPNONC
    else
      QPRECC = 0.10 * PRCP                                 ! should be from the atmospheric model
      QPRECL = 0.90 * PRCP                                 ! should be from the atmospheric model
    endif

! fractional area that receives precipitation (see, Niu et al. 2005)
   
    FP = 0.0
    if(QPRECC + QPRECL > 0.0) & 
       FP = (QPRECC + QPRECL) / (10.0*QPRECC + QPRECL)

! partition precipitation into rain and snow. Moved from CANWAT MB/AN: v3.7

! Jordan (1991)

    if(OPT_SNF == 1)then
      if(SFCTMP > TFRZ+2.5)then
        FPICE = 0.0
      else
        if(SFCTMP <= TFRZ+0.5)then
           FPICE = 1.0
        elseif(SFCTMP <= TFRZ+2.0)then
           FPICE = 1.-(-54.632 + 0.2*SFCTMP)
        else
           FPICE = 0.6
        endif
      endif
    endif

    if(OPT_SNF == 2)then
      if(SFCTMP >= TFRZ+2.2)then
         FPICE = 0.0
      else
         FPICE = 1.0
      endif
    endif

    if(OPT_SNF == 3)then
      if(SFCTMP >= TFRZ)then
         FPICE = 0.0
      else
         FPICE = 1.0
      endif
    endif

! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
! fresh snow density

    BDFALL = min(120.,67.92+51.25*exp((SFCTMP-TFRZ)/2.59))   !MB/AN: change to MIN  
    if(OPT_SNF == 4)then
       PRCP_FROZEN = PRCPSNOW + PRCPGRPL + PRCPHAIL
       if(PRCPNONC > 0.0 .and. PRCP_FROZEN > 0.0)then
         FPICE = min(1.0,PRCP_FROZEN/PRCPNONC)
         FPICE = max(0.0,FPICE)
         BDFALL = BDFALL*(PRCPSNOW/PRCP_FROZEN) + RHO_GRPL*(PRCPGRPL/PRCP_FROZEN) + &
                  RHO_HAIL*(PRCPHAIL/PRCP_FROZEN)
       else
         FPICE = 0.0
       endif
    endif

! wet-bulb scheme (Wang et al., 2019 GRL), C.He, 12/18/2020
    if(OPT_SNF == 5)then
      TDC   = min( 50.0, max(-50.0,(SFCTMP-TFRZ)))             !Kelvin to degree Celsius with limit -50 to +50
      if(SFCTMP > TFRZ)then
         LATHEA = HVAP
      else
         LATHEA = HSUB
      endif
      GAMMA_b = CPAIR*SFCPRS/(0.622*LATHEA)
      TWET    = TDC - 5.0                                     ! first guess wetbulb temperature
      do ITER = 1, NITER
         ESATAIR = 610.8 * exp((17.27*TWET)/(237.3+TWET))
         TWET    = TWET - (ESATAIR-EAIR)/ GAMMA_b            ! Wang et al., 2019 GRL Eq.2
      enddo
      FPICE = 1.0/(1.0+6.99E-5*exp(2.0*(TWET+3.97)))         ! Wang et al., 2019 GRL Eq. 1
    endif

    RAIN   = PRCP * (1.0-FPICE)
    SNOW   = PRCP * FPICE

    endassociate
  end subroutine ProcessAtmosphericForcing

end module AtmosForcingMod
