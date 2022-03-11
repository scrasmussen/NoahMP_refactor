module AtmosForcingMod

!!! Process input atmospheric forcing variables

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ProcessAtmosForcing(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ATM
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local varibles
    real(kind=kind_noahmp)             :: PAIR                   ! atm bottom level pressure (pa)
    real(kind=kind_noahmp)             :: PRCP_FROZEN            ! total frozen precipitation [mm/s] ! MB/AN : v3.7
    real(kind=kind_noahmp), parameter  :: RHO_GRPL = 500.0       ! graupel bulk density [kg/m3] ! MB/AN : v3.7
    real(kind=kind_noahmp), parameter  :: RHO_HAIL = 917.0       ! hail bulk density [kg/m3]    ! MB/AN : v3.7
    real(kind=kind_noahmp), parameter  :: dir_frac = 0.7         ! direct solar radiation fraction
    real(kind=kind_noahmp), parameter  :: vis_frac = 0.5         ! visible band solar radiation fraction
    ! wet-bulb scheme Wang et al., 2019 GRL, C.He, 12/18/2020
    real(kind=kind_noahmp)             :: ESATAIR                ! saturated vapor pressure of air
    real(kind=kind_noahmp)             :: LATHEA                 ! latent heat of vapor/sublimation
    real(kind=kind_noahmp)             :: GAMMA_b                ! (cp*p)/(eps*L)
    real(kind=kind_noahmp)             :: TDC                    ! air temperature [C]
    real(kind=kind_noahmp)             :: TWET                   ! wetbulb temperature
    integer                            :: ITER                   ! loop index
    integer, parameter                 :: NITER = 10             ! iterations for Twet calculation

! ------------------------------------------------------------------------
    associate(                                                           &
              COSZ                  => noahmp%config%domain%COSZ        ,& ! in,   cosine solar zenith angle [0-1]
              OPT_SNF               => noahmp%config%nmlist%OPT_SNF     ,& ! in,   rain-snow partition physics option
              SFCPRS                => noahmp%forcing%SFCPRS            ,& ! in,   lowest model-level midpoint pressure (pa)
              SFCTMP                => noahmp%forcing%SFCTMP            ,& ! in,   surface air temperature [k]
              Q2                    => noahmp%forcing%Q2                ,& ! in,   water vapor mixing ratio (kg/kg)
              PRCPCONV              => noahmp%forcing%PRCPCONV          ,& ! in,   convective precipitation entering  [mm/s]
              PRCPNONC              => noahmp%forcing%PRCPNONC          ,& ! in,   non-convective precipitation entering [mm/s]
              PRCPSHCV              => noahmp%forcing%PRCPSHCV          ,& ! in,   shallow convective precip entering  [mm/s]
              PRCPSNOW              => noahmp%forcing%PRCPSNOW          ,& ! in,   snow entering land model [mm/s]
              PRCPGRPL              => noahmp%forcing%PRCPGRPL          ,& ! in,   graupel entering land model [mm/s]
              PRCPHAIL              => noahmp%forcing%PRCPHAIL          ,& ! in,   hail entering land model [mm/s]
              SOLDN                 => noahmp%forcing%SOLDN             ,& ! in,   downward shortwave radiation (w/m2)
              THAIR                 => noahmp%energy%state%THAIR        ,& ! out,  surface potential temperature (k)
              QAIR                  => noahmp%energy%state%QAIR         ,& ! out,  specific humidity (kg/kg) (q2/(1+q2))
              EAIR                  => noahmp%energy%state%EAIR         ,& ! out,  vapor pressure air (pa)
              RHOAIR                => noahmp%energy%state%RHOAIR       ,& ! out,  density air (kg/m3)
              SOLAD                 => noahmp%energy%flux%SOLAD         ,& ! out,  incoming direct solar radiation (w/m2)
              SOLAI                 => noahmp%energy%flux%SOLAI         ,& ! out,  incoming diffuse solar radiation (w/m2)
              SWDOWN                => noahmp%energy%flux%SWDOWN        ,& ! out,  downward solar filtered by sun angle [w/m2]
              RAIN                  => noahmp%water%flux%RAIN           ,& ! out,  rainfall (mm/s)
              SNOW                  => noahmp%water%flux%SNOW           ,& ! out,  liquid equivalent snowfall (mm/s)
              PRCP                  => noahmp%water%flux%PRCP           ,& ! out,  total precipitation [mm/s]
              QPRECC                => noahmp%water%flux%QPRECC         ,& ! out,  convective precipitation (mm/s)
              QPRECL                => noahmp%water%flux%QPRECL         ,& ! out,  large-scale precipitation (mm/s)
              FP                    => noahmp%water%state%FP            ,& ! out,  fraction of area receiving precipitation
              FPICE                 => noahmp%water%state%FPICE         ,& ! out,  snowfall fraction
              BDFALL                => noahmp%water%state%BDFALL         & ! out,  !bulk density of snowfall (kg/m3)
             )
! ------------------------------------------------------------------------

    ! surface air quantities
    PAIR   = SFCPRS                                 ! atm bottom level pressure (pa), jref: seems like PAIR should be P1000mb?
    THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 
    QAIR   = Q2                                     ! In WRF, driver converts to specific humidity
    EAIR   = QAIR * SFCPRS / (0.622 + 0.378*QAIR)
    RHOAIR = (SFCPRS - 0.378*EAIR) / (RAIR * SFCTMP)

    ! downward solar radiation
    if ( COSZ <= 0.0 ) then
       SWDOWN = 0.0
    else
       SWDOWN = SOLDN
    endif 
    SOLAD(1) = SWDOWN * dir_frac       * vis_frac        ! direct  vis
    SOLAD(2) = SWDOWN * dir_frac       * (1.0-vis_frac)  ! direct  nir
    SOLAI(1) = SWDOWN * (1.0-dir_frac) * vis_frac        ! diffuse vis
    SOLAI(2) = SWDOWN * (1.0-dir_frac) * (1.0-vis_frac)  ! diffuse nir

    ! precipitation
    PRCP = PRCPCONV + PRCPNONC + PRCPSHCV
    if ( OPT_SNF == 4 ) then
       QPRECC = PRCPCONV + PRCPSHCV
       QPRECL = PRCPNONC
    else
       QPRECC = 0.10 * PRCP    ! should be from the atmospheric model
       QPRECL = 0.90 * PRCP    ! should be from the atmospheric model
    endif

    ! fractional area that receives precipitation (see, Niu et al. 2005)
    FP = 0.0
    if ( (QPRECC+QPRECL) > 0.0 ) then
       FP = (QPRECC + QPRECL) / (10.0*QPRECC + QPRECL)
    endif

    ! partition precipitation into rain and snow. Moved from CANWAT MB/AN: v3.7

    ! Jordan (1991)
    if ( OPT_SNF == 1 ) then
       if ( SFCTMP > (TFRZ+2.5) ) then
          FPICE = 0.0
       else
          if ( SFCTMP <= (TFRZ+0.5) ) then
             FPICE = 1.0
          elseif ( SFCTMP <= (TFRZ+2.0) ) then
             FPICE = 1.0 - (-54.632 + 0.2*SFCTMP)
          else
             FPICE = 0.6
          endif
       endif
    endif

    ! BATS scheme
    if ( OPT_SNF == 2 ) then
       if ( SFCTMP >= (TFRZ+2.2) ) then
          FPICE = 0.0
       else
          FPICE = 1.0
       endif
    endif

    ! Simple temperature scheme
    if ( OPT_SNF == 3 ) then
       if ( SFCTMP >= TFRZ ) then
          FPICE = 0.0
       else
          FPICE = 1.0
       endif
    endif

    ! Use WRF microphysics output
    ! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
    BDFALL = min( 120.0, 67.92 + 51.25*exp((SFCTMP-TFRZ)/2.59) )   !fresh snow density !MB/AN: change to MIN  
    if ( OPT_SNF == 4 ) then
       PRCP_FROZEN = PRCPSNOW + PRCPGRPL + PRCPHAIL
       if ( (PRCPNONC > 0.0) .and. (PRCP_FROZEN > 0.0) ) then
          FPICE  = min( 1.0, PRCP_FROZEN/PRCPNONC )
          FPICE  = max( 0.0, FPICE )
          BDFALL = BDFALL * (PRCPSNOW/PRCP_FROZEN) + RHO_GRPL * (PRCPGRPL/PRCP_FROZEN) + &
                   RHO_HAIL * (PRCPHAIL/PRCP_FROZEN)
       else
          FPICE = 0.0
       endif
    endif

    ! wet-bulb scheme (Wang et al., 2019 GRL), C.He, 12/18/2020
    if ( OPT_SNF == 5 ) then
       TDC = min( 50.0, max(-50.0,(SFCTMP-TFRZ)) )         ! Kelvin to degree Celsius with limit -50 to +50
       if ( SFCTMP > TFRZ ) then
          LATHEA = HVAP
       else
          LATHEA = HSUB
       endif
       GAMMA_b = CPAIR * SFCPRS / (0.622 * LATHEA)
       TWET    = TDC - 5.0                                     ! first guess wetbulb temperature
       do ITER = 1, NITER
          ESATAIR = 610.8 * exp( (17.27*TWET) / (237.3+TWET) )
          TWET    = TWET - (ESATAIR - EAIR) / GAMMA_b          ! Wang et al., 2019 GRL Eq.2
       enddo
       FPICE = 1.0 / (1.0 + 6.99e-5 * exp(2.0*(TWET+3.97)))    ! Wang et al., 2019 GRL Eq. 1
    endif

    ! rain-snow partitioning
    RAIN = PRCP * (1.0 - FPICE)
    SNOW = PRCP * FPICE

    end associate

  end subroutine ProcessAtmosForcing

end module AtmosForcingMod
