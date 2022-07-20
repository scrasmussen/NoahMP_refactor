module WaterTableDepthSearchMod

!!! Calculate/search water table depth as on WRF-Hydro/NWM

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine WaterTableDepthSearch(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TD_FINDZWAT
! Original code: P. Valayamkunnath (NCAR)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout)    :: noahmp

! local variable
    integer                :: K, i, j        ! loop index 
    integer                :: SATLYRCHK      ! check saturated layer
    real(kind=kind_noahmp) :: CWATAVAIL      ! temporary available water
    real(kind=kind_noahmp) :: WaterTableDepthTmp        ! temporary water table depth (m)

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              DepthSoilLayer     => noahmp%config%domain%DepthSoilLayer     ,& ! in,    depth [m] of layer-bottom from soil surface
              ThicknessSoilLayer => noahmp%config%domain%ThicknessSoilLayer ,& ! in,    soil layer thickness [m]
              SoilMoistureFieldCap          => noahmp%water%param%SoilMoistureFieldCap           ,& ! in,     reference soil moisture (field capacity) (m3/m3)
              SoilMoistureWilt          => noahmp%water%param%SoilMoistureWilt           ,& ! in,     wilting point soil moisture [m3/m3]
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! inout,  total soil moisture [m3/m3]
              WaterTableDepth             => noahmp%water%state%WaterTableDepth               & ! out,    water table depth [m]
             )
! ----------------------------------------------------------------------

    ! initialization
    SATLYRCHK = 0  !set flag for sat. layers
    CWATAVAIL = 0.0  !set wat avail for subsfc rtng = 0.

    ! calculate/search for water table depth
    do K = NumSoilLayer, 1, -1
       if ( (SoilMoisture(K) >= SoilMoistureFieldCap(K)) .and. (SoilMoistureFieldCap(K) > SoilMoistureWilt(K)) ) then
          if ( (SATLYRCHK == (K+1)) .or. (K == NumSoilLayer) ) SATLYRCHK = K
       endif
    enddo

    if ( SATLYRCHK /= 0 ) then
       if ( SATLYRCHK /= 1 ) then  ! soil column is partially sat.
          WaterTableDepthTmp = -DepthSoilLayer(SATLYRCHK-1)
       else  ! soil column is fully saturated to sfc.
          WaterTableDepthTmp = 0.0
       endif
       do K = SATLYRCHK, NumSoilLayer
          CWATAVAIL = CWATAVAIL + (SoilMoisture(K) - SoilMoistureFieldCap(K)) * ThicknessSoilLayer(K)
       enddo
    else  ! no saturated layers...
       WaterTableDepthTmp   = -DepthSoilLayer(NumSoilLayer)
       SATLYRCHK = NumSoilLayer + 1
    endif

    WaterTableDepth = WaterTableDepthTmp

    end associate

  end subroutine WaterTableDepthSearch

end module WaterTableDepthSearchMod
