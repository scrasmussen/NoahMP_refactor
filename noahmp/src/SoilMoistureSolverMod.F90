module SoilMoistureSolverMod

!!! Compute soil moisture content using based on Richards diffusion & tri-diagonal matrix
!!! Dependent on the output from SoilWaterDiffusionRichards subroutine

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use MatrixSolverTriDiagonalMod, only : MatrixSolverTriDiagonal

  implicit none

contains

  subroutine SoilMoistureSolver(noahmp, DT, AI, BI, CI, RHSTT)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: SSTEP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: DT       ! timestep (may not be the same as model timestep)
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: RHSTT  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: AI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: BI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: CI     ! left-hand side term of the matrix

! local variable
    integer                :: K         ! soil layer loop index 
    real(kind=kind_noahmp) :: WMINUS    ! temporary water deficiency
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHSTTIN  ! temporary RHSTT matrix coefficient
    real(kind=kind_noahmp), allocatable, dimension(:) :: CIIN     ! temporary CI matrix coefficient

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,     depth [m] of layer-bottom from soil surface
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,     thickness of snow/soil layers (m)
              OptRunoffSubsurface => noahmp%config%nmlist%OptRunoffSubsurface ,& ! in,     options for drainage and subsurface runoff
              SoilMoistureSat          => noahmp%water%param%SoilMoistureSat           ,& ! in,     saturated value of soil moisture [m3/m3]
              WaterTableDepth             => noahmp%water%state%WaterTableDepth              ,& ! in,     water table depth [m]
              SoilIce            => noahmp%water%state%SoilIce             ,& ! in,     soil ice content [m3/m3]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout,  soil water content [m3/m3]
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! inout,  total soil moisture [m3/m3]
              SoilMoistureToWT          => noahmp%water%state%SoilMoistureToWT           ,& ! inout,  soil moisture between bottom of the soil and the water table
              RechargeGwDeepWT        => noahmp%water%state%RechargeGwDeepWT         ,& ! inout,  recharge to or from the water table when deep [m]
              DrainSoilBot          => noahmp%water%flux%DrainSoilBot            ,& ! inout,  soil bottom drainage (m/s)
              SoilEffPorosity           => noahmp%water%state%SoilEffPorosity       ,& ! out,    soil effective porosity (m3/m3)
              SoilSaturationExcess           => noahmp%water%state%SoilSaturationExcess             & ! out,    saturation excess of the total soil [m]
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( RHSTTIN(1:NumSoilLayer) )
    allocate( CIIN   (1:NumSoilLayer) )
    RHSTTIN  = 0.0
    CIIN     = 0.0
    SoilSaturationExcess    = 0.0
    SoilEffPorosity(:) = 0.0

    ! update tri-diagonal matrix elements
    do K = 1, NumSoilLayer
       RHSTT (K) =    RHSTT(K) * DT
       AI (K)    =       AI(K) * DT
       BI (K)    = 1.0 + BI(K) * DT
       CI (K)    =       CI(K) * DT
    enddo

    ! copy values for input variables before calling rosr12
    do K = 1, NumSoilLayer
       RHSTTIN(k) = RHSTT(K)
       CIIN(k)    = CI(K)
    enddo

    ! call ROSR12 to solve the tri-diagonal matrix
    call MatrixSolverTriDiagonal(CI,AI,BI,CIIN,RHSTTIN,RHSTT,1,NumSoilLayer,0)

    do K = 1, NumSoilLayer
        SoilLiqWater(K) = SoilLiqWater(K) + CI(K)
    enddo

    !  excessive water above saturation in a layer is moved to
    !  its unsaturated layer like in a bucket

    ! for MMF scheme, there is soil moisture below NumSoilLayer, to the water table
    if ( OptRunoffSubsurface == 5 ) then
       ! update SoilMoistureToWT
       if ( WaterTableDepth < (DepthSoilLayer(NumSoilLayer)-ThicknessSnowSoilLayer(NumSoilLayer)) ) then
          ! accumulate soil drainage to update deep water table and soil moisture later
          RechargeGwDeepWT =  RechargeGwDeepWT + DT * DrainSoilBot
       else
          SoilMoistureToWT = SoilMoistureToWT + DT * DrainSoilBot  / ThicknessSnowSoilLayer(NumSoilLayer)
          SoilSaturationExcess = max( (SoilMoistureToWT - SoilMoistureSat(NumSoilLayer)), 0.0 ) * &
                                 ThicknessSnowSoilLayer(NumSoilLayer)
          WMINUS = max( (1.0e-4 - SoilMoistureToWT), 0.0 ) * ThicknessSnowSoilLayer(NumSoilLayer)
          SoilMoistureToWT      = max( min(SoilMoistureToWT, SoilMoistureSat(NumSoilLayer)), 1.0e-4 )
          SoilLiqWater(NumSoilLayer) = SoilLiqWater(NumSoilLayer) + SoilSaturationExcess / ThicknessSnowSoilLayer(NumSoilLayer)
          ! reduce fluxes at the bottom boundaries accordingly
          DrainSoilBot   = DrainSoilBot - SoilSaturationExcess/DT
          RechargeGwDeepWT = RechargeGwDeepWT - WMINUS
       endif
    endif

    do K = NumSoilLayer, 2, -1
       SoilEffPorosity(K)  = max( 1.0e-4, (SoilMoistureSat(K) - SoilIce(K)) )
       SoilSaturationExcess     = max( (SoilLiqWater(K)-SoilEffPorosity(K)), 0.0 ) * ThicknessSnowSoilLayer(K)
       SoilLiqWater(K)   = min( SoilEffPorosity(K), SoilLiqWater(K) )
       SoilLiqWater(K-1) = SoilLiqWater(K-1) + SoilSaturationExcess / ThicknessSnowSoilLayer(K-1)
    enddo

    SoilEffPorosity(1) = max( 1.0e-4, (SoilMoistureSat(1) - SoilIce(1)) )
    SoilSaturationExcess    = max( (SoilLiqWater(1)-SoilEffPorosity(1)), 0.0 ) * ThicknessSnowSoilLayer(1)
    SoilLiqWater(1)  = min( SoilEffPorosity(1), SoilLiqWater(1) )

    if ( SoilSaturationExcess > 0.0 ) then
       SoilLiqWater(2) = SoilLiqWater(2) + SoilSaturationExcess / ThicknessSnowSoilLayer(2)
       do K = 2, NumSoilLayer-1
          SoilEffPorosity(K)  = max( 1.0e-4, (SoilMoistureSat(K) - SoilIce(K)) )
          SoilSaturationExcess     = max( (SoilLiqWater(K)-SoilEffPorosity(K)), 0.0 ) * ThicknessSnowSoilLayer(K)
          SoilLiqWater(K)   = min( SoilEffPorosity(K), SoilLiqWater(K) )
          SoilLiqWater(K+1) = SoilLiqWater(K+1) + SoilSaturationExcess / ThicknessSnowSoilLayer(K+1)
       enddo
       SoilEffPorosity(NumSoilLayer) = max( 1.0e-4, (SoilMoistureSat(NumSoilLayer) - SoilIce(NumSoilLayer)) )
       SoilSaturationExcess = max( (SoilLiqWater(NumSoilLayer)-SoilEffPorosity(NumSoilLayer)), 0.0 ) * &
                              ThicknessSnowSoilLayer(NumSoilLayer)
       SoilLiqWater(NumSoilLayer)  = min( SoilEffPorosity(NumSoilLayer), SoilLiqWater(NumSoilLayer) )
    endif

    SoilMoisture = SoilLiqWater + SoilIce

    end associate

  end subroutine SoilMoistureSolver

end module SoilMoistureSolverMod
