module SnowpackCompactionMod

!!! Snowpack compaction process
!!! Update snow depth via compaction due to destructive metamorphism, overburden, & melt

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowpackCompaction(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMPACT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: J       ! snow layer index
    real(kind=kind_noahmp) :: BURDEN  ! pressure of overlying snow [kg/m2]
    real(kind=kind_noahmp) :: DEXPF   ! EXPF=exp(-c4*(273.15-STC))
    real(kind=kind_noahmp) :: TD      ! STC - TFRZ [K]
    real(kind=kind_noahmp) :: VOID    ! void (1 - SNICE - SNLIQ)
    real(kind=kind_noahmp) :: WX      ! water mass (ice + liquid) [kg/m2]
    real(kind=kind_noahmp) :: BI      ! partial density of ice [kg/m3]

! --------------------------------------------------------------------
    associate(                                                        &
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              STC             => noahmp%energy%state%STC             ,& ! in,     snow and soil layer temperature [k]
              SNICE           => noahmp%water%state%SNICE            ,& ! in,     snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! in,     snow layer liquid water [mm]
              IMELT           => noahmp%water%state%IMELT            ,& ! in,     phase change index [0-none;1-melt;2-refreeze]
              FICEOLD         => noahmp%water%state%FICEOLD          ,& ! in,     ice fraction at last timestep
              C2              => noahmp%water%param%C2_SnowCompact   ,& ! in,     snow overburden compaction parameter (m3/kg)
              C3              => noahmp%water%param%C3_SnowCompact   ,& ! in,     snow desctructive metamorphism compaction parameter1 [1/s]
              C4              => noahmp%water%param%C4_SnowCompact   ,& ! in,     snow desctructive metamorphism compaction parameter2 [1/k]
              C5              => noahmp%water%param%C5_SnowCompact   ,& ! in,     snow desctructive metamorphism compaction parameter3 
              DM              => noahmp%water%param%DM_SnowCompact   ,& ! in,     upper Limit on destructive metamorphism compaction [kg/m3]
              ETA0            => noahmp%water%param%ETA0_SnowCompact ,& ! in,     snow viscosity coefficient [kg-s/m2], Anderson1979: 0.52e6~1.38e6
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/soil layers (m)
              DDZ1            => noahmp%water%flux%DDZ1              ,& ! out,    rate of settling of snowpack due to destructive metamorphism [1/s]
              DDZ2            => noahmp%water%flux%DDZ2              ,& ! out,    rate of compaction of snowpack due to overburden [1/s]
              DDZ3            => noahmp%water%flux%DDZ3              ,& ! out,    rate of compaction of snowpack due to melt [1/s]
              PDZDTC          => noahmp%water%flux%PDZDTC            ,& ! out,    rate of change in fractional-thickness due to compaction [fraction/s]
              FICE            => noahmp%water%state%FICE              & ! out,    fraction of ice at current time step 
             )
! ----------------------------------------------------------------------

! initialization for out-only variables
    DDZ1(:)   = 0.0
    DDZ2(:)   = 0.0
    DDZ3(:)   = 0.0
    PDZDTC(:) = 0.0
    FICE(:)   = 0.0

! start snow compaction
    BURDEN = 0.0
    do J = ISNOW+1, 0
       WX      = SNICE(J) + SNLIQ(J)
       FICE(J) = SNICE(J) / WX
       VOID    = 1.0 - ( SNICE(J)/DENICE + SNLIQ(J)/DENH2O ) / DZSNSO(J)

       ! Allow compaction only for non-saturated node and higher ice lens node.
       if ( VOID > 0.001 .and. SNICE(J) > 0.1) then
          BI    = SNICE(J) / DZSNSO(J)
          TD    = max( 0.0, TFRZ-STC(J) )

          ! Settling/compaction as a result of destructive metamorphism
          DEXPF   = exp( -C4 * TD )
          DDZ1(J) = -C3 * DEXPF
          if ( BI > DM ) DDZ1(J) = DDZ1(J) * exp( -46.0e-3 * (BI-DM) )
          if ( SNLIQ(J) > 0.01*DZSNSO(J) ) DDZ1(J) = DDZ1(J) * C5   ! Liquid water term

          ! Compaction due to overburden
          DDZ2(J) = -(BURDEN + 0.5*WX) * exp(-0.08*TD - C2*BI) / ETA0 ! 0.5*WX -> self-burden

          ! Compaction occurring during melt
          if ( IMELT(J) == 1 ) then
             DDZ3(J) = max( 0.0, (FICEOLD(J)-FICE(J)) / max(1.0e-6,FICEOLD(J)) )
             DDZ3(J) = -DDZ3(J) / DT   ! sometimes too large
          else
             DDZ3(J) = 0.0
          endif

          ! Time rate of fractional change in DZ (units of s-1)
          PDZDTC(J) = ( DDZ1(J) + DDZ2(J) + DDZ3(J) ) * DT
          PDZDTC(J) = max( -0.5, PDZDTC )

          ! The change in DZ due to compaction
          DZSNSO(J) = DZSNSO(J) * ( 1.0 + PDZDTC(J) )
          DZSNSO(J) = max( DZSNSO(J), SNICE(J)/DENICE + SNLIQ(J)/DENH2O )

       endif

       ! Pressure of overlying snow
       BURDEN = BURDEN + WX

    enddo

    end associate

  end subroutine SnowpackCompaction

end module SnowpackCompactionMod
