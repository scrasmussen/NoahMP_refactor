module SurfaceRadiationGlacierMod

!!! Compute glacier surface radiative fluxes (absorption and reflection)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceRadiationGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RADIATION_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IB       ! waveband indices (1=vis, 2=nir)
    real(kind=kind_noahmp)           :: ABSG     ! ground absorbed solar radiation (w/m2)
    real(kind=kind_noahmp)           :: REFG     ! reflected solar radiation (w/m2)

! --------------------------------------------------------------------
    associate(                                                        &
              NumSWRadBand           => noahmp%config%domain%NumSWRadBand          ,& ! in,    number of solar radiation wave bands
              RadSwDownDir           => noahmp%energy%flux%RadSwDownDir            ,& ! in,    incoming direct solar radiation (w/m2)
              RadSwDownDif           => noahmp%energy%flux%RadSwDownDif            ,& ! in,    incoming diffuse solar radiation (w/m2)
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! in,    ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI          ,& ! in,    ground albedo (diffuse: vis, nir)
              RadSwAbsGrd             => noahmp%energy%flux%RadSwAbsGrd              ,& ! out,   solar radiation absorbed by ground (w/m2)
              RadSwAbsTot             => noahmp%energy%flux%RadSwAbsTot              ,& ! out,   total absorbed solar radiation (w/m2)
              RadSwReflTot             => noahmp%energy%flux%RadSwReflTot               & ! out,   total reflected solar radiation (w/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    RadSwAbsGrd    = 0.0
    RadSwAbsTot    = 0.0
    RadSwReflTot    = 0.0

    do IB = 1, NumSWRadBand
       ! solar radiation absorbed by glacier surface
       ABSG = RadSwDownDir(IB) * (1.0 - ALBGRD(IB)) + RadSwDownDif(IB) * (1.0 - ALBGRI(IB))
       RadSwAbsGrd  = RadSwAbsGrd + ABSG
       RadSwAbsTot  = RadSwAbsTot + ABSG
      
       ! solar radiation reflected by glacier surface
       REFG = RadSwDownDir(IB) * ALBGRD(IB) + RadSwDownDif(IB) * ALBGRI(IB)
       RadSwReflTot  = RadSwReflTot + REFG
    enddo

    end associate

  end subroutine SurfaceRadiationGlacier

end module SurfaceRadiationGlacierMod
