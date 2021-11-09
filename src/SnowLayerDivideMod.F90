module SnowLayerDivideMod

!!! Snowpack layer division process
!!! Update snow ice, snow water, snow thickness, snow temperature

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SnowLayerWaterComboMod, only: SnowLayerWaterCombo

  implicit none

contains

  subroutine SnowLayerDivide(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: DIVIDE
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: J        ! indices
    integer                :: MSNO     ! number of layer (top) to MSNO (bot)
    real(kind=kind_noahmp) :: DRR      ! thickness of the combined [m]
    real(kind=kind_noahmp) :: ZWICE    ! extra snow ice to be divided compared to allowed layer thickness
    real(kind=kind_noahmp) :: ZWLIQ    ! extra snow liquid water to be divided compared to allowed layer thickness
    real(kind=kind_noahmp) :: PROPOR   ! fraction of extra snow to be divided compared to allowed layer thickness
    real(kind=kind_noahmp) :: DTDZ     ! temperature gradient between two snow layers
    real(kind=kind_noahmp), allocatable, dimension(:) :: DZ      ! snow layer thickness [m]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SWICE   ! partial volume of ice [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SWLIQ   ! partial volume of liquid water [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: TSNO    ! node temperature [k]

! --------------------------------------------------------------------
    associate(                                                        &
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,     maximum number of snow layers
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              SNICE           => noahmp%water%state%SNICE            ,& ! inout,  snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout,  snow layer liquid water [mm]
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/soil layers (m)
             )
! ----------------------------------------------------------------------

! initialization
    allocate( SWICE (1:NSNOW) )
    allocate( SWLIQ (1:NSNOW) )
    allocate( TSNO  (1:NSNOW) )
    allocate( DZ    (1:NSNOW) )
    SWICE(:) = 0.0
    SWLIQ(:) = 0.0
    TSNO (:) = 0.0
    DZ   (:) = 0.0

    do J = 1, NSNOW
       if ( J <= abs(ISNOW) ) then
          DZ(J)    = DZSNSO(J+ISNOW)
          SWICE(J) = SNICE(J+ISNOW)
          SWLIQ(J) = SNLIQ(J+ISNOW)
          TSNO(J)  = STC(J+ISNOW)
       endif
    enddo

! start snow layer division
    MSNO = abs(ISNOW)

    if ( MSNO == 1 ) then
       ! Specify a new snow layer
       if ( DZ(1) > 0.05 ) then
          MSNO     = 2
          DZ(1)    = DZ(1)/2.0
          SWICE(1) = SWICE(1)/2.0
          SWLIQ(1) = SWLIQ(1)/2.0
          DZ(2)    = DZ(1)
          SWICE(2) = SWICE(1)
          SWLIQ(2) = SWLIQ(1)
          TSNO(2)  = TSNO(1)
       endif
    endif

    if ( MSNO > 1 ) then
       if ( DZ(1) > 0.05 ) then     ! maximum allowed thickness (5cm) for top snow layer
          DRR      = DZ(1) - 0.05
          PROPOR   = DRR / DZ(1)
          ZWICE    = PROPOR * SWICE(1)
          ZWLIQ    = PROPOR * SWLIQ(1)
          PROPOR   = 0.05 / DZ(1)
          SWICE(1) = PROPOR*SWICE(1)
          SWLIQ(1) = PROPOR*SWLIQ(1)
          DZ(1)    = 0.05

          ! update combined snow water & temperature
          call SnowLayerWaterCombo(DZ(2), SWLIQ(2), SWICE(2), TSNO(2), &
                                   DRR  , ZWLIQ   , ZWICE   , TSNO(1) )

          ! subdivide a new layer, maximum allowed thickness (20cm) for second snow layer
          if ( MSNO <= 2 .and. DZ(2) > 0.20 ) then  ! MB: change limit
          !if ( MSNO <= 2 .and. DZ(2) > 0.10 ) then
             MSNO     = 3
             DTDZ     = ( TSNO(1) - TSNO(2) ) / ( (DZ(1)+DZ(2)) / 2.0 )
             DZ(2)    = DZ(2) / 2.0
             SWICE(2) = SWICE(2) / 2.0
             SWLIQ(2) = SWLIQ(2) / 2.0
             DZ(3)    = DZ(2)
             SWICE(3) = SWICE(2)
             SWLIQ(3) = SWLIQ(2)
             TSNO(3)  = TSNO(2) - DTDZ * DZ(2) / 2.0
             if ( TSNO(3) >= TFRZ ) then
                TSNO(3)  = TSNO(2)
             else
                TSNO(2) = TSNO(2) + DTDZ * DZ(2) / 2.0
             endif
          endif
       endif ! if(DZ(1) > 0.05)
    endif  ! if (MSNO > 1)

    if ( MSNO > 2 ) then
       if ( DZ(2) > 0.2 ) then
          DRR      = DZ(2) - 0.2
          PROPOR   = DRR / DZ(2)
          ZWICE    = PROPOR * SWICE(2)
          ZWLIQ    = PROPOR * SWLIQ(2)
          PROPOR   = 0.2 / DZ(2)
          SWICE(2) = PROPOR * SWICE(2)
          SWLIQ(2) = PROPOR * SWLIQ(2)
          DZ(2)    = 0.2

          ! update combined snow water & temperature
          call SnowLayerWaterCombo(DZ(3), SWLIQ(3), SWICE(3), TSNO(3), &
                                   DRR  , ZWLIQ   , ZWICE   , TSNO(2) )

       endif
    endif

    ISNOW = -MSNO

    do J = ISNOW+1, 0
       DZSNSO(J) = DZ(J-ISNOW)
       SNICE(J)  = SWICE(J-ISNOW)
       SNLIQ(J)  = SWLIQ(J-ISNOW)
       STC(J)    = TSNO(J-ISNOW)
    enddo

    end associate

  end subroutine SnowLayerDivide

end module SnowLayerDivideMod
