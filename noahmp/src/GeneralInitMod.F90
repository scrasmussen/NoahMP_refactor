module GeneralInitMod

!!! General initialization for variables

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
 
  implicit none

contains

  subroutine GeneralInit(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in NOAHMP_SFLX)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IZ        ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,   number of soil layers
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,   depth [m] of layer-bottom from soil surface
              NROOT           => noahmp%water%param%NROOT            ,& ! in,   number of soil layers with root present
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! in,   actual number of snow layers (negative)
              DepthSnowSoilLayer           => noahmp%config%domain%DepthSnowSoilLayer          ,& ! in,   depth of snow/soil layer-bottom (m)
              STC             => noahmp%energy%state%STC             ,& ! in,   snow and soil layer temperature [k]
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! out,  thickness of snow/soil layers (m)
              TROOT           => noahmp%energy%state%TROOT            & ! out,  root-zone averaged temperature (k)
             )
! ----------------------------------------------------------------------

    ! initialize snow/soil layer thickness (m)
    do IZ = NumSnowLayerNeg+1, NumSoilLayer
       if ( IZ == NumSnowLayerNeg+1 ) then
          ThicknessSnowSoilLayer(IZ) = - DepthSnowSoilLayer(IZ)
       else
          ThicknessSnowSoilLayer(IZ) = DepthSnowSoilLayer(IZ-1) - DepthSnowSoilLayer(IZ)
       endif
    enddo

    ! initialize root-zone soil temperature
    TROOT = 0.0
    do IZ = 1, NROOT
       TROOT = TROOT + STC(IZ) * ThicknessSnowSoilLayer(IZ) / (-DepthSoilLayer(NROOT))
    enddo

    end associate

  end subroutine GeneralInit

end module GeneralInitMod
