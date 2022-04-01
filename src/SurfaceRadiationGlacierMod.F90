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
              NBAND           => noahmp%config%domain%NBAND          ,& ! in,    number of solar radiation wave bands
              SOLAD           => noahmp%energy%flux%SOLAD            ,& ! in,    incoming direct solar radiation (w/m2)
              SOLAI           => noahmp%energy%flux%SOLAI            ,& ! in,    incoming diffuse solar radiation (w/m2)
              ALBGRD          => noahmp%energy%state%ALBGRD          ,& ! in,    ground albedo (direct beam: vis, nir)
              ALBGRI          => noahmp%energy%state%ALBGRI          ,& ! in,    ground albedo (diffuse: vis, nir)
              SAG             => noahmp%energy%flux%SAG              ,& ! out,   solar radiation absorbed by ground (w/m2)
              FSA             => noahmp%energy%flux%FSA              ,& ! out,   total absorbed solar radiation (w/m2)
              FSR             => noahmp%energy%flux%FSR               & ! out,   total reflected solar radiation (w/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    SAG    = 0.0
    FSA    = 0.0
    FSR    = 0.0

    do IB = 1, NBAND
       ! solar radiation absorbed by glacier surface
       ABSG = SOLAD(IB) * (1.0 - ALBGRD(IB)) + SOLAI(IB) * (1.0 - ALBGRI(IB))
       SAG  = SAG + ABSG
       FSA  = FSA + ABSG
      
       ! solar radiation reflected by glacier surface
       REFG = SOLAD(IB) * ALBGRD(IB) + SOLAI(IB) * ALBGRI(IB)
       FSR  = FSR + REFG
    enddo

    end associate

  end subroutine SurfaceRadiationGlacier

end module SurfaceRadiationGlacierMod
