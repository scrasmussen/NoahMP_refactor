module GeneralInitGlacierMod

!!! General initialization for glacier variables

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
 
  implicit none

contains

  subroutine GeneralInitGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in NOAHMP_GLACIER)
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
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! in,   actual number of snow layers (negative)
              DepthSnowSoilLayer           => noahmp%config%domain%DepthSnowSoilLayer          ,& ! in,   depth of snow/soil layer-bottom (m)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer          & ! out,  thickness of snow/soil layers (m)
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

    end associate

  end subroutine GeneralInitGlacier

end module GeneralInitGlacierMod
