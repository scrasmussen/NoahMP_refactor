module CanopyRadiationTwoStreamMod

!!! Compute canopy radiative transfer using two-stream approximation of Dickinson (1983) Adv Geophysics
!!! Calculate fluxes absorbed by vegetation, reflected by vegetation, and transmitted through vegetation 
!!! for unit incoming direct or diffuse flux given an underlying ground with known albedo.
!!! Reference for the modified two-stream scheme Niu and Yang (2004), JGR

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CanopyRadiationTwoStream(noahmp, IB, IC)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: TWOSTREAM
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    integer          , intent(in   ) :: IB      ! waveband number
    integer          , intent(in   ) :: IC      ! 0=unit incoming direct; 1=unit incoming diffuse

! local variable
    real(kind=kind_noahmp)           :: OMEGA                     ! fraction of intercepted radiation that is scattered
    real(kind=kind_noahmp)           :: OMEGAL                    ! omega for leaves
    real(kind=kind_noahmp)           :: BETAI                     ! upscatter parameter for diffuse radiation
    real(kind=kind_noahmp)           :: BETAIL                    ! betai for leaves
    real(kind=kind_noahmp)           :: BETAD                     ! upscatter parameter for direct beam radiation
    real(kind=kind_noahmp)           :: BETADL                    ! betad for leaves
    real(kind=kind_noahmp)           :: EXT                       ! optical depth of direct beam per unit leaf area
    real(kind=kind_noahmp)           :: AVMU                      ! average diffuse optical depth
    real(kind=kind_noahmp)           :: COSZI                     ! 0.001 <= cosz <= 1.000
    real(kind=kind_noahmp)           :: ASU                       ! single scattering albedo
    real(kind=kind_noahmp)           :: CHIL                      ! -0.4 <= xl <= 0.6
    real(kind=kind_noahmp)           :: TMP0,TMP1,TMP2,TMP3,TMP4  ! temporary vars
    real(kind=kind_noahmp)           :: TMP5,TMP6,TMP7,TMP8,TMP9  ! temporary vars
    real(kind=kind_noahmp)           :: P1,P2,P3,P4,S1,S2,U1,U2,U3! temporary vars
    real(kind=kind_noahmp)           :: B,C,D,D1,D2,F,H,H1,H2,H3  ! temporary vars
    real(kind=kind_noahmp)           :: H4,H5,H6,H7,H8,H9,H10     ! temporary vars 
    real(kind=kind_noahmp)           :: PHI1,PHI2,SIGMA           ! temporary vars
    real(kind=kind_noahmp)           :: FTDS,FTIS,FRES            ! temporary vars
    real(kind=kind_noahmp)           :: DENFVEG                   ! temporary vars
    real(kind=kind_noahmp)           :: VAI_SPREAD                ! temporary vars
    real(kind=kind_noahmp)           :: FREVEG,FREBAR,FTDVEG      ! temporary vars
    real(kind=kind_noahmp)           :: FTIVEG,FTDBAR,FTIBAR      ! temporary vars
    real(kind=kind_noahmp)           :: THETAZ                    ! temporary vars
    real(kind=kind_noahmp)           :: HD                        ! crown depth (m)
    real(kind=kind_noahmp)           :: BB                        ! vertical crown radius (m)
    real(kind=kind_noahmp)           :: THETAP                    ! angle conversion from SZA 
    real(kind=kind_noahmp)           :: FA                        ! foliage volume density (m-1)
    real(kind=kind_noahmp)           :: NEWVAI                    ! effective LSAI (-)

! --------------------------------------------------------------------
    associate(                                                        &
              OPT_RAD         => noahmp%config%nmlist%OPT_RAD        ,& ! in,    options for canopy radiation transfer
              COSZ            => noahmp%config%domain%COSZ           ,& ! in,    cosine solar zenith angle
              FWET            => noahmp%water%state%FWET             ,& ! in,    wetted or snowed fraction of the canopy
              RC              => noahmp%energy%param%RC              ,& ! in,    tree crown radius (m)
              HVT             => noahmp%energy%param%HVT             ,& ! in,    top of canopy (m)
              HVB             => noahmp%energy%param%HVB             ,& ! in,    bottom of canopy (m)
              DEN             => noahmp%energy%param%DEN             ,& ! in,    tree density (no. of trunks per m2)
              XL              => noahmp%energy%param%XL              ,& ! in,    leaf/stem orientation index
              OMEGAS          => noahmp%energy%param%OMEGAS          ,& ! in,    two-stream parameter omega for snow
              BETADS          => noahmp%energy%param%BETADS          ,& ! in,    two-stream parameter betad for snow (dir rad)
              BETAIS          => noahmp%energy%param%BETAIS          ,& ! in,    two-stream parameter betad for snow (dif rad)
              VAI             => noahmp%energy%state%VAI             ,& ! in,    one-sided leaf+stem area index (m2/m2)
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! in,    ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI          ,& ! in,    ground albedo (diffuse: vis, nir)
              RHO             => noahmp%energy%state%RHO             ,& ! in,    leaf/stem reflectance weighted by fraction LAI and SAI
              TAU             => noahmp%energy%state%TAU             ,& ! in,    leaf/stem transmittance weighted by fraction LAI and SAI
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              ALBD            => noahmp%energy%state%ALBD            ,& ! out,   surface albedo (direct)
              ALBI            => noahmp%energy%state%ALBI            ,& ! out,   surface albedo (diffuse)
              GDIR            => noahmp%energy%state%GDIR            ,& ! out,   projected leaf+stem area in solar direction
              BGAP            => noahmp%energy%state%BGAP            ,& ! out,   between canopy gap fraction for beam
              WGAP            => noahmp%energy%state%WGAP            ,& ! out,   within canopy gap fraction for beam
              KOPEN           => noahmp%energy%state%KOPEN           ,& ! out,   gap fraction for diffue light
              GAP             => noahmp%energy%state%GAP             ,& ! out,   total gap fraction for beam ( <=1-shafac )
              FABD            => noahmp%energy%flux%FABD             ,& ! out,   flux abs by veg (per unit direct flux)
              FABI            => noahmp%energy%flux%FABI             ,& ! out,   flux abs by veg (per unit diffuse flux)
              FTDD            => noahmp%energy%flux%FTDD             ,& ! out,   down direct flux below veg (per unit dir flux)
              FTDI            => noahmp%energy%flux%FTDI             ,& ! out,   down direct flux below veg per unit dif flux (= 0)
              FTID            => noahmp%energy%flux%FTID             ,& ! out,   down diffuse flux below veg (per unit dir flux)
              FTII            => noahmp%energy%flux%FTII             ,& ! out,   down diffuse flux below veg (per unit dif flux)
              FREVD           => noahmp%energy%flux%FREVD            ,& ! out,   flux reflected by veg layer (per unit direct flux)
              FREVI           => noahmp%energy%flux%FREVI            ,& ! out,   flux reflected by veg layer (per unit diffuse flux)
              FREGD           => noahmp%energy%flux%FREGD            ,& ! out,   flux reflected by ground (per unit direct flux)
              FREGI           => noahmp%energy%flux%FREGI             & ! out,   flux reflected by ground (per unit diffuse flux)
             )
! ----------------------------------------------------------------------

    ! compute within and between gaps
    VAI_SPREAD = VAI
    if ( VAI == 0.0 ) then
       GAP   = 1.0
       KOPEN = 1.0
    else
       if ( OPT_RAD == 1 ) then
          DENFVEG = -log( max(1.0-FVEG, 0.01) ) / (ConstPI * RC**2)
          HD      = HVT - HVB
          BB      = 0.5 * HD
          THETAP  = atan( BB / RC * tan(acos(max(0.01, COSZ))) )
         !BGAP    = exp( DEN * ConstPI * RC**2 / cos(THETAP) )
          BGAP    = exp( -DENFVEG * ConstPI * RC**2 / cos(THETAP) )
          FA      = VAI / ( 1.33 * ConstPI * RC**3.0 * (BB/RC) * DENFVEG )
          NEWVAI  = HD * FA
          WGAP    = (1.0 - BGAP) * exp(-0.5 * NEWVAI / COSZ)
          GAP     = min( 1.0-FVEG, BGAP+WGAP )
          KOPEN   = 0.05
       endif
       if ( OPT_RAD == 2 ) then
          GAP     = 0.0
          KOPEN   = 0.0
       endif
       if ( OPT_RAD == 3 ) then
          GAP     = 1.0 - FVEG
          KOPEN   = 1.0 - FVEG
       endif
    endif

    ! calculate two-stream parameters OMEGA, BETAD, BETAI, AVMU, GDIR, EXT.
    ! OMEGA, BETAD, BETAI are adjusted for snow. values for OMEGA*BETAD
    ! and OMEGA*BETAI are calculated and then divided by the new OMEGA
    ! because the product OMEGA*BETAI, OMEGA*BETAD is used in solution.
    ! also, the transmittances and reflectances (TAU, RHO) are linear
    ! weights of leaf and stem values.

    COSZI  = max( 0.001, COSZ )
    CHIL   = min( max(XL, -0.4), 0.6 )
    if ( abs(CHIL) <= 0.01 ) CHIL = 0.01
    PHI1   = 0.5 - 0.633 * CHIL - 0.330 * CHIL * CHIL
    PHI2   = 0.877 * (1.0 - 2.0 * PHI1)
    GDIR   = PHI1 + PHI2 * COSZI
    EXT    = GDIR / COSZI
    AVMU   = (1.0 - PHI1/PHI2 * log( (PHI1+PHI2) / PHI1 )) / PHI2
    OMEGAL = RHO(IB) + TAU(IB)
    TMP0   = GDIR + PHI2 * COSZI
    TMP1   = PHI1 * COSZI
    ASU    = 0.5 * OMEGAL * GDIR / TMP0 * (1.0 - TMP1/TMP0 * log((TMP1+TMP0)/TMP1) )
    BETADL = (1.0 + AVMU * EXT) / (OMEGAL * AVMU * EXT) * ASU
    BETAIL = 0.5 * ( RHO(IB) + TAU(IB) + (RHO(IB)-TAU(IB)) * ((1.0+CHIL)/2.0)**2 ) / OMEGAL

    ! adjust omega, betad, and betai for intercepted snow
    if ( TV > ConstFreezePoint ) then  !no snow
       TMP0 = OMEGAL
       TMP1 = BETADL
       TMP2 = BETAIL
    else
       TMP0 =   (1.0 - FWET) * OMEGAL          + FWET * OMEGAS(IB)
       TMP1 = ( (1.0 - FWET) * OMEGAL * BETADL + FWET * OMEGAS(IB) * BETADS ) / TMP0 ! direct
       TMP2 = ( (1.0 - FWET) * OMEGAL * BETAIL + FWET * OMEGAS(IB) * BETAIS ) / TMP0 ! diffuse
    endif
    OMEGA = TMP0
    BETAD = TMP1
    BETAI = TMP2

    ! absorbed, reflected, transmitted fluxes per unit incoming radiation
    B     = 1.0 - OMEGA + OMEGA * BETAI
    C     = OMEGA * BETAI
    TMP0  = AVMU * EXT
    D     = TMP0 * OMEGA * BETAD
    F     = TMP0 * OMEGA * (1.0 - BETAD)
    TMP1  = B * B - C * C
    H     = sqrt(TMP1) / AVMU
    SIGMA = TMP0 * TMP0 - TMP1
    if ( abs(SIGMA) < 1.0e-6 ) SIGMA = sign(1.0e-6, SIGMA)
    P1    = B + AVMU * H
    P2    = B - AVMU * H
    P3    = B + TMP0
    P4    = B - TMP0
    S1    = exp( -H * VAI )
    S2    = exp( -EXT * VAI )
    if ( IC == 0 ) then  ! direct
       U1 = B - C / ALBGRD(IB)
       U2 = B - C * ALBGRD(IB)
       U3 = F + C * ALBGRD(IB)
    else   ! diffuse
       U1 = B - C / ALBGRI(IB)
       U2 = B - C * ALBGRI(IB)
       U3 = F + C * ALBGRI(IB)
    endif
    TMP2  = U1 - AVMU * H
    TMP3  = U1 + AVMU * H
    D1    = P1 * TMP2 / S1 - P2 * TMP3 * S1
    TMP4  = U2 + AVMU * H
    TMP5  = U2 - AVMU * H
    D2    = TMP4 / S1 - TMP5 * S1
    H1    = -D * P4 - C * F
    TMP6  = D - H1 * P3 / SIGMA
    TMP7  = ( D - C - H1 / SIGMA * (U1+TMP0) ) * S2
    H2    = ( TMP6 * TMP2 / S1 - P2 * TMP7 ) / D1
    H3    = - ( TMP6 * TMP3 * S1 - P1 * TMP7 ) / D1
    H4    = -F * P3 - C * D
    TMP8  = H4 / SIGMA
    TMP9  = ( U3 - TMP8 * (U2-TMP0) ) * S2
    H5    = - ( TMP8 * TMP4 / S1 + TMP9 ) / D2
    H6    = ( TMP8 * TMP5 * S1 + TMP9 ) / D2
    H7    = (C * TMP2) / (D1 * S1)
    H8    = (-C * TMP3 * S1) / D1
    H9    = TMP4 / (D2 * S1)
    H10   = (-TMP5 * S1) / D2

    ! downward direct and diffuse fluxes below vegetation Niu and Yang (2004), JGR.
    if ( IC == 0 ) then  ! direct
       FTDS = S2 * (1.0 - GAP) + GAP
       FTIS = (H4 * S2 / SIGMA + H5 * S1 + H6 / S1) * (1.0 - GAP)
    else  ! diffuse
       FTDS = 0.0
       FTIS = (H9 * S1 + H10 / S1) * (1.0 - KOPEN) + KOPEN
    endif
    if ( IC == 0 ) then  ! direct
       FTDD(IB) = FTDS
       FTID(IB) = FTIS
    else  ! diffuse
       FTDI(IB) = FTDS
       FTII(IB) = FTIS
    endif

    ! flux reflected by the surface (veg. and ground)
    if ( IC == 0 ) then ! direct
       FRES   = (H1 / SIGMA + H2 + H3) * (1.0 - GAP) + ALBGRD(IB) * GAP
       FREVEG = (H1 / SIGMA + H2 + H3) * (1.0 - GAP)
       FREBAR = ALBGRD(IB) * GAP    ! separate veg. and ground reflection
    else   ! diffuse
       FRES   = (H7 + H8) * (1.0 - KOPEN) + ALBGRI(IB) * KOPEN
       FREVEG = (H7 + H8) * (1.0 - KOPEN) + ALBGRI(IB) * KOPEN
       FREBAR = 0                   ! separate veg. and ground reflection
    endif
    if ( IC == 0 ) then ! direct
       ALBD(IB)  = FRES
       FREVD(IB) = FREVEG
       FREGD(IB) = FREBAR
    else   ! diffuse
       ALBI(IB)  = FRES
       FREVI(IB) = FREVEG
       FREGI(IB) = FREBAR
    endif

    ! flux absorbed by vegetation
    if ( IC == 0 ) then ! direct
       FABD(IB) = 1.0 - ALBD(IB) - (1.0 - ALBGRD(IB)) * FTDD(IB) - (1.0 - ALBGRI(IB)) * FTID(IB)
    else   ! diffuse
       FABI(IB) = 1.0 - ALBI(IB) - (1.0 - ALBGRD(IB)) * FTDI(IB) - (1.0 - ALBGRI(IB)) * FTII(IB)
    endif

    end associate

  end subroutine CanopyRadiationTwoStream

end module CanopyRadiationTwoStreamMod
