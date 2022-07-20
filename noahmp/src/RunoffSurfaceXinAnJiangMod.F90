module RunoffSurfaceXinAnJiangMod

!!! Compute surface infiltration rate and surface runoff based on XinAnJiang runoff scheme
!!! Reference: Knoben, W. J., et al., (2019): Modular Assessment of Rainfall-Runoff Models 
!!! Toolbox (MARRMoT) v1.2 an open-source, extendable framework providing implementations 
!!! of 46 conceptual hydrologic models as continuous state-space formulations.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSurfaceXinAnJiang(noahmp, DT)

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
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,   number of soil layers
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,   depth [m] of layer-bottom from soil surface
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,   total soil moisture [m3/m3]
              SoilImpervFrac             => noahmp%water%state%SoilImpervFrac              ,& ! in,   fraction of imperviousness due to frozen soil
              SoilSfcInflow          => noahmp%water%flux%SoilSfcInflow            ,& ! in,   water input on soil surface [mm/s]
              SoilMoistureSat          => noahmp%water%param%SoilMoistureSat           ,& ! in,   saturated value of soil moisture [m3/m3]
              SoilMoistureFieldCap          => noahmp%water%param%SoilMoistureFieldCap           ,& ! in,   reference soil moisture (field capacity) (m3/m3)
              TensionWatDistrInfl            => noahmp%water%param%TensionWatDistrInfl             ,& ! in,   Tension water distribution inflection parameter
              TensionWatDistrShp            => noahmp%water%param%TensionWatDistrShp             ,& ! in,   Tension water distribution shape parameter
              FreeWatDistrShp            => noahmp%water%param%FreeWatDistrShp             ,& ! in,   Free water distribution shape parameter
              RunoffSurface          => noahmp%water%flux%RunoffSurface            ,& ! out,  surface runoff [mm/s]
              InfilRateSfc           => noahmp%water%flux%InfilRateSfc              & ! out,  infiltration rate at surface (mm/s)
             )
! ----------------------------------------------------------------------

    ! initialization 
    WM      = 0.0
    WM_MAX  = 0.0
    SM      = 0.0
    SM_MAX  = 0.0
    IRUNOFF = 0.0
    PRUNOFF = 0.0
    RunoffSurface  = 0.0
    InfilRateSfc   = 0.0

    do IZ = 1, NumSoilLayer-2
       if ( (SoilMoisture(IZ) - SoilMoistureFieldCap(IZ)) > 0.0 ) then   ! soil moisture greater than field capacity
          SM  = SM + ( SoilMoisture(IZ) - SoilMoistureFieldCap(IZ) ) * (-1.0) * DepthSoilLayer(IZ)   !m
          WM  = WM + SoilMoistureFieldCap(IZ) * (-1.0) * DepthSoilLayer(IZ)                 !m  
       else
          WM  = WM + SoilMoisture(IZ) * (-1.0) * DepthSoilLayer(IZ)
       endif
       WM_MAX = WM_MAX + SoilMoistureFieldCap(IZ) * (-1.0) * DepthSoilLayer(IZ)
       SM_MAX = SM_MAX + ( SoilMoistureSat(IZ) - SoilMoistureFieldCap(IZ) ) * (-1.0) * DepthSoilLayer(IZ)
    enddo
    WM = min( WM, WM_MAX ) ! tension water (m) 
    SM = min( SM, SM_MAX ) ! free water (m)

    ! impervious surface runoff R_IMP    
    IRUNOFF = SoilImpervFrac(1) * SoilSfcInflow * DT

    ! solve pervious surface runoff (m) based on Eq. (310)
    if ( (WM/WM_MAX) <= (0.5-TensionWatDistrInfl) ) then
       PRUNOFF = (1.0-SoilImpervFrac(1)) * SoilSfcInflow * DT * &
                 ((0.5-TensionWatDistrInfl)**(1.0-TensionWatDistrShp)) * ((WM/WM_MAX)**TensionWatDistrShp)
    else
       PRUNOFF = (1.0-SoilImpervFrac(1)) * SoilSfcInflow * DT * &
                 (1.0-(((0.5+TensionWatDistrInfl)**(1.0-TensionWatDistrShp)) * ((1.0-(WM/WM_MAX))**TensionWatDistrShp)))
    endif

    ! estimate surface runoff based on Eq. (313)
    if ( SoilSfcInflow == 0.0 ) then
      RunoffSurface  = 0.0
    else
      RunoffSurface  = PRUNOFF * ( 1.0 - ( (1.0-(SM/SM_MAX))**FreeWatDistrShp ) ) + IRUNOFF
    endif
    RunoffSurface = RunoffSurface / DT   ! m/s
    RunoffSurface = max( 0.0,    RunoffSurface )
    RunoffSurface = min( SoilSfcInflow, RunoffSurface )
    InfilRateSfc  = SoilSfcInflow - RunoffSurface

    end associate

  end subroutine RunoffSurfaceXinAnJiang

end module RunoffSurfaceXinAnJiangMod
