module WaterTableDepthSearchMod

!!! Calculate/search water table depth as on WRF-Hydro/NWM

  use Machine, only : kind_noahmp
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
    real(kind=kind_noahmp) :: WATBLED        ! water table depth (m)

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              ZLAYER          => noahmp%config%domain%ZLAYER         ,& ! in,     soil layer thickness (m)
              SMCREF          => noahmp%water%param%SMCREF           ,& ! in,     reference soil moisture (field capacity) (m3/m3)
              SMCWLT          => noahmp%water%param%SMCWLT           ,& ! in,     wilting point soil moisture [m3/m3]
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              ZWT             => noahmp%water%state%ZWT               & ! out,    water table depth [m]
             )
! ----------------------------------------------------------------------

    ! initialization
    SATLYRCHK = 0  !set flag for sat. layers
    CWATAVAIL = 0.0  !set wat avail for subsfc rtng = 0.

    ! calculate/search for water table depth
    do K = NSOIL, 1, -1
       if ( (SMC(K) >= SMCREF(K)) .and. (SMCREF(K) > SMCWLT(K)) ) then
          if ( (SATLYRCHK == (K+1)) .or. (K == NSOIL) ) SATLYRCHK = K
       endif
    enddo

    if ( SATLYRCHK /= 0 ) then
       if ( SATLYRCHK /= 1 ) then  ! soil column is partially sat.
          WATBLED = -ZSOIL(SATLYRCHK-1)
       else  ! soil column is fully saturated to sfc.
          WATBLED = 0.0
       endif
       do K = SATLYRCHK, NSOIL
          CWATAVAIL = CWATAVAIL + (SMC(K) - SMCREF(K)) * ZLAYER(K)
       enddo
    else  ! no saturated layers...
       WATBLED   = -ZSOIL(NSOIL)
       SATLYRCHK = NSOIL + 1
    endif

    ZWT = WATBLED

    end associate

  end subroutine WaterTableDepthSearch

end module WaterTableDepthSearchMod
