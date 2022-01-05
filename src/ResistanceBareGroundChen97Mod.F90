module ResistanceBareGroundChen97Mod

!!! Compute bare ground resistance and drag coefficient CM for momentum and CH for heat
!!! based on Chen et al. (1997, BLM)
!!! This scheme can handle both over open water and over solid surface

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceBareGroundChen97(noahmp, ITER)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SFCDIF2 for bare ground portion
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    integer               , intent(in   ) :: ITER         ! iteration index
    type(noahmp_type)     , intent(inout) :: noahmp

! local variables
    integer                               :: ILECH, ITR
    real(kind=kind_noahmp)                :: ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    real(kind=kind_noahmp)                :: XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    real(kind=kind_noahmp)                :: ZILFC, ZU, ZT, RDZ, CXCH, DTHV, DU2
    real(kind=kind_noahmp)                :: BTGH, ZSLU, ZSLT, RLOGU, RLOGT, RLMA
    real(kind=kind_noahmp)                :: ZETALT, ZETALU, ZETAU, ZETAT, XLU4
    real(kind=kind_noahmp)                :: XLT4, XU4, XT4, XLU, XLT, XU, XT
    real(kind=kind_noahmp)                :: PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN
! local parameters
    integer               , parameter     :: ITRMX  = 5
    real(kind=kind_noahmp), parameter     :: WWST   = 1.2
    real(kind=kind_noahmp), parameter     :: WWST2  = WWST * WWST
    real(kind=kind_noahmp), parameter     :: VKRM   = 0.40
    real(kind=kind_noahmp), parameter     :: EXCM   = 0.001
    real(kind=kind_noahmp), parameter     :: BETA   = 1.0 / 270.0
    real(kind=kind_noahmp), parameter     :: BTG    = BETA * GRAV
    real(kind=kind_noahmp), parameter     :: ELFC   = VKRM * BTG
    real(kind=kind_noahmp), parameter     :: WOLD   = 0.15
    real(kind=kind_noahmp), parameter     :: WNEW   = 1.0 - WOLD
    real(kind=kind_noahmp), parameter     :: PIHF   = 3.14159265 / 2.0
    real(kind=kind_noahmp), parameter     :: EPSU2  = 1.0e-4
    real(kind=kind_noahmp), parameter     :: EPSUST = 0.07
    real(kind=kind_noahmp), parameter     :: EPSIT  = 1.0e-4
    real(kind=kind_noahmp), parameter     :: EPSA   = 1.0e-8
    real(kind=kind_noahmp), parameter     :: ZTMIN  = -5.0
    real(kind=kind_noahmp), parameter     :: ZTMAX  = 1.0
    real(kind=kind_noahmp), parameter     :: HPBL   = 1000.0
    real(kind=kind_noahmp), parameter     :: SQVISC = 258.2
    real(kind=kind_noahmp), parameter     :: RIC    = 0.183
    real(kind=kind_noahmp), parameter     :: RRIC   = 1.0 / RIC
    real(kind=kind_noahmp), parameter     :: FHNEU  = 0.8
    real(kind=kind_noahmp), parameter     :: RFC    = 0.191
    real(kind=kind_noahmp), parameter     :: RFAC   = RIC / ( FHNEU * RFC * RFC )
! local statement functions
    ! LECH'S surface functions
    PSLMU(ZZ) = -0.96 * log(1.0 - 4.5 * ZZ)
    PSLMS(ZZ) = ZZ * RRIC - 2.076 * (1.0 - 1.0/(ZZ + 1.0))
    PSLHU(ZZ) = -0.96 * log(1.0 - 4.5 * ZZ)
    PSLHS(ZZ) = ZZ * RFAC - 2.076 * (1.0 - 1.0/(ZZ + 1.0))
    ! PAULSON'S surface functions
    PSPMU(XX) = -2.0*log( (XX+1.0)*0.5 ) - log( (XX*XX+1.0)*0.5 ) + 2.0*atan(XX) - PIHF
    PSPMS(YY) = 5.0 * YY
    PSPHU(XX) = -2.0 * log( (XX*XX + 1.0)*0.5 )
    PSPHS(YY) = 5.0 * YY

! --------------------------------------------------------------------
    associate(                                                        &
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              CZIL            => noahmp%energy%param%CZIL            ,& ! in,    Calculate roughness length of heat
              ZLM             => noahmp%energy%state%ZLVL            ,& ! in,    reference height  (m)
              THLM            => noahmp%energy%state%THAIR           ,& ! in,    potential temp at reference height (k)
              SFCSPD          => noahmp%energy%state%UR              ,& ! in,    wind speed (m/s) at reference height ZLVL
              Z0              => noahmp%energy%state%Z0MG            ,& ! in,    roughness length, momentum, (m), ground
              THZ0            => noahmp%energy%state%TGB             ,& ! in,    bare ground temperature (K)
              AKMS            => noahmp%energy%state%CMB             ,& ! inout, drag coefficient for momentum, above ZPD, bare ground
              AKHS            => noahmp%energy%state%CHB             ,& ! inout, drag coefficient for heat, above ZPD, bare ground
              RLMO            => noahmp%energy%state%MOZB            ,& ! inout, Monin-Obukhov stability (z/L), above ZPD, bare ground
              WSTAR2          => noahmp%energy%state%WSTARB          ,& ! inout, friction velocity in vertical direction (m/s), bare ground
              USTAR           => noahmp%energy%state%FVB             ,& ! out,   friction velocity (m/s), bare ground
              RAMB            => noahmp%energy%state%RAMB            ,& ! out,   aerodynamic resistance for momentum (s/m), bare ground
              RAHB            => noahmp%energy%state%RAHB            ,& ! out,   aerodynamic resistance for sensible heat (s/m), bare ground
              RAWB            => noahmp%energy%state%RAWB             & ! out,   aerodynamic resistance for water vapor (s/m), bare ground
             )
! ----------------------------------------------------------------------

    ! ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
    ! C......ZTFC=0.1
    ! CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
    ILECH = 0
    ZILFC = -CZIL * VKRM * SQVISC
    ZU    = Z0
    RDZ   = 1.0 / ZLM
    CXCH  = EXCM * RDZ
    DTHV  = THLM - THZ0

    ! BELJARS correction of friction velocity u*
    DU2   = max( SFCSPD*SFCSPD, EPSU2 )
    BTGH  = BTG * HPBL
    if ( ITER == 1 ) then
       if ( BTGH*AKHS*DTHV /= 0.0 ) then
          WSTAR2 = WWST2 * abs(BTGH * AKHS * DTHV)**(2.0/3.0)
       else
          WSTAR2 = 0.0
       endif
       USTAR = max( sqrt(AKMS * sqrt(DU2+WSTAR2)), EPSUST )
       RLMO  = ELFC * AKHS * DTHV / USTAR**3
    endif

    ! ZILITINKEVITCH approach for ZT
    ZT    = max( 1.0e-6, exp(ZILFC * sqrt(USTAR*Z0)) * Z0 )
    ZSLU  = ZLM + ZU
    ZSLT  = ZLM + ZT
    RLOGU = log(ZSLU / ZU)
    RLOGT = log(ZSLT / ZT)

    ! Monin-Obukhov length scale
    ZETALT = max( ZSLT*RLMO, ZTMIN )
    RLMO   = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU  = ZU * RLMO
    ZETAT  = ZT * RLMO
    if ( ILECH == 0 ) then
       if ( RLMO < 0.0 ) then
          XLU4 = 1.0 - 16.0 * ZETALU
          XLT4 = 1.0 - 16.0 * ZETALT
          XU4  = 1.0 - 16.0 * ZETAU
          XT4  = 1.0 - 16.0 * ZETAT
          XLU  = sqrt( sqrt(XLU4) )
          XLT  = sqrt( sqrt(XLT4) )
          XU   = sqrt( sqrt(XU4) )
          XT   = sqrt( sqrt(XT4) )
          PSMZ = PSPMU(XU)
          SIMM = PSPMU(XLU) - PSMZ + RLOGU
          PSHZ = PSPHU(XT)
          SIMH = PSPHU(XLT) - PSHZ + RLOGT
       else
          ZETALU = min( ZETALU, ZTMAX )
          ZETALT = min( ZETALT, ZTMAX )
          ZETAU  = min( ZETAU, ZTMAX/(ZSLU/ZU) )   ! Barlage: add limit on ZETAU/ZETAT
          ZETAT  = min( ZETAT, ZTMAX/(ZSLT/ZT) )   ! Barlage: prevent SIMM/SIMH < 0
          PSMZ   = PSPMS(ZETAU)
          SIMM   = PSPMS(ZETALU) - PSMZ + RLOGU
          PSHZ   = PSPHS(ZETAT)
          SIMH   = PSPHS(ZETALT) - PSHZ + RLOGT
       endif
    else ! LECH's functions
       if ( RLMO < 0.0 ) then
          PSMZ = PSLMU(ZETAU)
          SIMM = PSLMU(ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHU(ZETAT)
          SIMH = PSLHU(ZETALT) - PSHZ + RLOGT
       else
          ZETALU = min( ZETALU, ZTMAX )
          ZETALT = min( ZETALT, ZTMAX )
          PSMZ   = PSLMS(ZETAU)
          SIMM   = PSLMS(ZETALU) - PSMZ + RLOGU
          PSHZ   = PSLHS(ZETAT)
          SIMH   = PSLHS(ZETALT) - PSHZ + RLOGT
       endif
    endif

    ! BELJARS correction of friction velocity u*
    USTAR = max( sqrt(AKMS * sqrt(DU2+ WSTAR2)), EPSUST )

    ! ZILITINKEVITCH fix for ZT
    ZT     = max( 1.0e-6, exp(ZILFC * sqrt(USTAR * Z0)) * Z0 )
    ZSLT   = ZLM + ZT
    RLOGT  = log(ZSLT / ZT)
    USTARK = USTAR * VKRM

    ! avoid tangent linear problems near zero
    if ( SIMM < 1.0e-6 ) SIMM = 1.0e-6   ! Limit stability function
    AKMS = max( USTARK/SIMM, CXCH )
    if ( SIMH < 1.0e-6 ) SIMH = 1.0e-6   ! Limit stability function
    AKHS = max( USTARK/SIMH, CXCH )

    ! update vertical friction velocity w*
    if ( BTGH*AKHS*DTHV /= 0.0 ) then
       WSTAR2 = WWST2 * abs(BTGH * AKHS * DTHV)**(2.0/3.0)
    else
       WSTAR2 = 0.0
    endif

    ! update M-O stability parameter
    RLMN = ELFC * AKHS * DTHV / USTAR**3
    RLMA = RLMO * WOLD + RLMN * WNEW
    RLMO = RLMA

    ! Undo the multiplication by windspeed that applies to drag coeff CH & CM
    AKHS = AKHS / SFCSPD
    AKMS = AKMS / SFCSPD
    if ( SNOWH > 0.0 ) then
       AKMS = min( 0.01, AKMS )   ! CM & CH are too large, causing
       AKHS = min( 0.01, AKHS )   ! computational instability
    endif

    ! compute aerodynamic resistance
    RAMB = max( 1.0, 1.0 / (AKMS*SFCSPD) )
    RAHB = max( 1.0, 1.0 / (AKHS*SFCSPD) )
    RAWB = RAHB

    end associate

  end subroutine ResistanceBareGroundChen97

end module ResistanceBareGroundChen97Mod
