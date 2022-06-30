module TileDrainageSimpleMod

!!! Calculate tile drainage discharge (mm) based on simple model

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine TileDrainageSimple(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TILE_DRAIN
! Original code: P. Valayamkunnath (NCAR)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: K       ! soil layer loop index
    real(kind=kind_noahmp) :: TDRVOL  ! temporary variable for drainage volume (mm)
    real(kind=kind_noahmp) :: TDDC    ! temporary variable for drainage
    real(kind=kind_noahmp) :: TDSUM   ! temporary variable for drainage
    real(kind=kind_noahmp), allocatable, dimension(:) :: OVRFC   ! temp variable for volume of water above field capacity
    real(kind=kind_noahmp), allocatable, dimension(:) :: AVFC    ! Available field capacity = FC - SICE (m3/m3)
    real(kind=kind_noahmp), allocatable, dimension(:) :: TDFRAC  ! tile drainage fraction

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,     depth [m] of layer-bottom from soil surface
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     main noahmp timestep [s]
              ZLAYER          => noahmp%config%domain%ZLAYER         ,& ! in,     soil layer thickness (m)
              TD_DC           => noahmp%water%param%TD_DC            ,& ! in,     drainage coefficient (mm d^-1)
              DRAIN_LAYER_OPT => noahmp%water%param%DRAIN_LAYER_OPT  ,& ! in,     starting soil layer for drainage
              TD_DEPTH        => noahmp%water%param%TD_DEPTH         ,& ! in,     depth of drain tube from the soil surface
              TDSMC_FAC       => noahmp%water%param%TDSMC_FAC        ,& ! in,     drainage factor for soil moisture
              SMCREF          => noahmp%water%param%SMCREF           ,& ! in,     reference soil moisture (field capacity) (m3/m3)
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
              QTLDRN          => noahmp%water%flux%QTLDRN             & ! out,    tile drainage (mm/s)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( TDFRAC(1:NumSoilLayer) )
    allocate( AVFC  (1:NumSoilLayer) )
    allocate( OVRFC (1:NumSoilLayer) )
    TDFRAC = 0.0
    AVFC   = 0.0
    TDRVOL = 0.0
    OVRFC  = 0.0
    QTLDRN = 0.0
    ZLAYER = 0.0
    TDSUM  = 0.0
    TDFRAC = 0.0
    TDDC   = TD_DC * MainTimeStep / (24.0 * 3600.0)

    do K = 1, NumSoilLayer
       if ( K == 1 ) then
          ZLAYER(K) = -1.0 * DepthSoilLayer(K)
       else
          ZLAYER(K) = DepthSoilLayer(K-1) - DepthSoilLayer(K)
       endif
    enddo
    if ( DRAIN_LAYER_OPT == 0 ) then ! drainage from one specified layer in NoahmpTable.TBL
       ! print*, "CASE = 1"
       K        = TD_DEPTH
       AVFC(K)  = SMCREF(K) - SICE (K)
       OVRFC(K) = (SH2O(K) - (TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
       if ( OVRFC(K) > 0.0 ) then
          if ( OVRFC(K) > TDDC ) OVRFC(K) = TDDC
          TDRVOL   = TDRVOL  + OVRFC(K)
          SH2O(K)  = SH2O(K) - (OVRFC(K) / (ZLAYER(K) * 1000.0))
          SMC(K)   = SH2O(K) + SICE (K)
       endif
    else if ( DRAIN_LAYER_OPT == 1 ) then
       !print*, "CASE = 2. Draining from layer 1 and 2"
       do K = 1, 2
          AVFC(K)  = SMCREF(K) - SICE (K)
          OVRFC(K) = (SH2O(K) - (TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
          if ( OVRFC(K) < 0.0 ) OVRFC(K) = 0.0
          TDSUM    = TDSUM + OVRFC(K)
       enddo
       do K = 1, 2
          if ( OVRFC(K) /= 0.0 ) then
             TDFRAC(K)   = OVRFC(K) / TDSUM
          endif
       enddo
       if ( TDSUM > 0.0 ) then
          if ( TDSUM > TDDC ) TDSUM = TDDC
          TDRVOL = TDRVOL + TDSUM
          do K = 1, 2
             OVRFC(K) = TDFRAC(K) * TDSUM
             SH2O(K)  = SH2O(K) - (OVRFC(K) / (ZLAYER(K) * 1000.0))
             SMC(K)   = SH2O(K) + SICE (K)
          enddo
       endif
    else if ( DRAIN_LAYER_OPT == 2 ) then
       !print*, "CASE = 3.  Draining from layer 1 2 and 3"
       do K = 1, 3
          AVFC(K)  = SMCREF(K) - SICE (K)
          OVRFC(K) = (SH2O(K) - (TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
          if ( OVRFC(K) < 0.0 ) OVRFC(K) = 0.0
          TDSUM    = TDSUM + OVRFC(K)
       enddo
       do K = 1, 3
          if ( OVRFC(K) /= 0.0 ) then
             TDFRAC(K) = OVRFC(K) / TDSUM
          endif
       enddo
       if ( TDSUM > 0.0 ) then
          if ( TDSUM > TDDC ) TDSUM = TDDC
          TDRVOL = TDRVOL + TDSUM
          do K = 1, 3
             OVRFC(K) = TDFRAC(K) * TDSUM
             SH2O(K)  = SH2O(K) - (OVRFC(K) / (ZLAYER(K) * 1000.0))
             SMC(K)   = SH2O(K) + SICE (K)
          enddo
       endif
    else if ( DRAIN_LAYER_OPT == 3 ) then
       !print*, "CASE = 3.  Draining from layer 2 and 3"
       do K = 2, 3
          AVFC(K)  = SMCREF(K) - SICE (K)
          OVRFC(K) = (SH2O(K) - (TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
          if ( OVRFC(K) < 0.0 ) OVRFC(K) = 0.0
          TDSUM    = TDSUM + OVRFC(K)
       enddo
       do K = 2, 3
          if ( OVRFC(K) /= 0.0 ) then
             TDFRAC(K) = OVRFC(K) / TDSUM
          endif
       enddo
       if ( TDSUM > 0.0 ) then
          if ( TDSUM > TDDC ) TDSUM = TDDC
          TDRVOL = TDRVOL + TDSUM
          do K = 2, 3
             OVRFC(K) = TDFRAC(K) * TDSUM
             SH2O(K)  = SH2O(K) - (OVRFC(K) / (ZLAYER(K) * 1000.0))
             SMC(K)   = SH2O(K) + SICE (K)
          enddo
       endif
    else if ( DRAIN_LAYER_OPT == 4 ) then
       !print*, "CASE = 4.  Draining from layer 3 and 4"
       do K = 3, 4
          AVFC(K)  = SMCREF(K) - SICE (K)
          OVRFC(K) = (SH2O(K) - (TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
          if ( OVRFC(K) < 0.0 ) OVRFC(K) = 0.0
          TDSUM    = TDSUM + OVRFC(K)
       enddo
       do K = 3, 4
          if ( OVRFC(K) /= 0.0 ) then
             TDFRAC(K) = OVRFC(K) / TDSUM
          endif
       enddo
       if ( TDSUM > 0.0 ) then
          if ( TDSUM > TDDC ) TDSUM = TDDC
          TDRVOL = TDRVOL + TDSUM
          do K = 3, 4
             OVRFC(K) = TDFRAC(K) * TDSUM
             SH2O(K)  = SH2O(K) - (OVRFC(K) / (ZLAYER(K) * 1000.0))
             SMC(K)   = SH2O(K) + SICE (K)
          enddo
       endif
    else if ( DRAIN_LAYER_OPT == 5 ) then ! from all the four layers
       !print*, "CASE = 5  Draining from all four layers"
       do K = 1, 4
          AVFC(K)  = SMCREF(K) - SICE (K)
          OVRFC(K) = (SH2O(K) - (TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
          if ( OVRFC(K) < 0.0 ) OVRFC(K) = 0.0
          TDSUM    = TDSUM + OVRFC(K)
       enddo
       do K = 1, 4
          if ( OVRFC(K) /= 0.0 ) then
             TDFRAC(K) = OVRFC(K) / TDSUM
          endif
       enddo
       if ( TDSUM > 0.0 ) then
          if ( TDSUM > TDDC ) TDSUM = TDDC
          TDRVOL = TDRVOL + TDSUM
          do K = 1, 4
             OVRFC(K) = TDFRAC(K) * TDSUM
             SH2O(K)  = SH2O(K) - (OVRFC(K) / (ZLAYER(K) * 1000.0))
             SMC(K)   = SH2O(K) + SICE (K)
          enddo
       endif
    endif

    QTLDRN = TDRVOL / MainTimeStep

    end associate

  end subroutine TileDrainageSimple

end module TileDrainageSimpleMod
