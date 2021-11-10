module RunoffSurfaceXinAnJiangMod

!!! Compute surface infiltration rate and surface runoff based on XinAnJiang runoff scheme
!!! Reference: Knoben, W. J., et al., (2019): Modular Assessment of Rainfall-Runoff Models 
!!! Toolbox (MARRMoT) v1.2 an open-source, extendable framework providing implementations 
!!! of 46 conceptual hydrologic models as continuous state-space formulations.

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceRunoffXinAnJiang(noahmp, DT)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: COMPUTE_XAJ_SURFRUNOFF
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variables
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: DT          ! timestep (may not be the same as model timestep)

! local variable
    integer                :: IZ                         ! do-loop index
    real(kind=kind_noahmp) :: WM, WM_MAX, SM, SM_MAX     ! temporary soil moisture variable
    real(kind=kind_noahmp) :: IRUNOFF, PRUNOFF           ! temporary runoff variable

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              SMC             => noahmp%water%state%SMC              ,& ! in,     total soil moisture [m3/m3]
              FCR             => noahmp%water%state%FCR              ,& ! in,     fraction of imperviousness due to frozen soil
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              SMCREF          => noahmp%water%param%SMCREF           ,& ! in,     reference soil moisture (field capacity) (m3/m3)
              AXAJ            => noahmp%water%param%AXAJ             ,& ! in,     Tension water distribution inflection parameter
              BXAJ            => noahmp%water%param%BXAJ             ,& ! in,     Tension water distribution shape parameter
              XXAJ            => noahmp%water%param%XXAJ             ,& ! in,     Free water distribution shape parameter
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              PDDUM           => noahmp%water%flux%PDDUM              & ! out,    infiltration rate at surface (mm/s)
             )
! ----------------------------------------------------------------------

    ! initialization 
    WM      = 0.0
    WM_MAX  = 0.0
    SM      = 0.0
    SM_MAX  = 0.0
    IRUNOFF = 0.0
    PRUNOFF = 0.0
    RUNSRF  = 0.0
    PDDUM   = 0.0

    do IZ = 1, NSOIL-2
       if ( (SMC(IZ) - SMCREF(IZ)) > 0.0 ) then   ! soil moisture greater than field capacity
          SM  = SM + ( SMC(IZ) - SMCREF(IZ) ) * -1.0 * ZSOIL(IZ)   !m
          WM  = WM + SMCREF(IZ) * -1.0 * ZSOIL(IZ)                 !m  
       else
          WM  = WM + SMC(IZ) * -1.0 * ZSOIL(IZ)
       endif
       WM_MAX = WM_MAX + SMCREF(IZ) * -1.0 * ZSOIL(IZ)
       SM_MAX = SM_MAX + ( SMCMAX(IZ) - SMCREF(IZ) ) * -1.0 * ZSOIL(IZ)
    enddo
    WM = min( WM, WM_MAX ) ! tension water (m) 
    SM = min( SM, SM_MAX ) ! free water (m)

    ! impervious surface runoff R_IMP    
    IRUNOFF = FCR(1) * QINSUR * DT

    ! solve pervious surface runoff (m) based on Eq. (310)
    if ( (WM/WM_MAX) <= (0.5-AXAJ) ) then
       PRUNOFF = (1.0-FCR(1)) * QINSUR * DT * ( (0.5-AXAJ)**(1.0-BXAJ) ) * ( (WM/WM_MAX)**BXAJ )
    else
       PRUNOFF = (1.0-FCR(1)) * QINSUR * DT * (1.0 - (( (0.5+AXAJ)**(1.0-BXAJ) ) * ( (1.0-(WM/WM_MAX))**BXAJ )))
    endif

    ! estimate surface runoff based on Eq. (313)
    if ( QINSUR == 0.0 ) then
      RUNSRF  = 0.0
    else
      RUNSRF  = PRUNOFF * ( 1.0 - ( (1.0-(SM/SM_MAX))**XXAJ ) ) + IRUNOFF
    endif
    RUNSRF = RUNSRF / DT   ! m/s
    RUNSRF = max( 0.0,    RUNSRF )
    RUNSRF = min( QINSUR, RUNSRF )
    PDDUM  = QINSUR - RUNSRF

    end associate

  end subroutine SurfaceRunoffXinAnJiang

end module RunoffSurfaceXinAnJiangMod
