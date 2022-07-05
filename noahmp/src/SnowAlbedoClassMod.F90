module SnowAlbedoClassMod

!!! Compute snow albedo based on the CLASS scheme (Verseghy, 1991)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowAlbedoClass(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWALB_CLASS
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IB          ! waveband class
    real(kind=kind_noahmp)           :: ALB         ! temporal albedo

! --------------------------------------------------------------------
    associate(                                                        &
              NumSWRadBand           => noahmp%config%domain%NumSWRadBand          ,& ! in,     number of solar radiation wave bands
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step (s)
              QSNOW           => noahmp%water%flux%QSNOW             ,& ! in,     snow at ground srf (mm/s) [+]
              SWEMX           => noahmp%water%param%SWEMX            ,& ! in,     new snow mass to fully cover old snow (mm)
              CLASS_ALB_REF   => noahmp%energy%param%CLASS_ALB_REF   ,& ! in,     reference snow albedo in CLASS scheme
              CLASS_SNO_AGE   => noahmp%energy%param%CLASS_SNO_AGE   ,& ! in,     snow aging e-folding time (s)
              CLASS_ALB_NEW   => noahmp%energy%param%CLASS_ALB_NEW   ,& ! in,     fresh snow albedo
              ALBOLD          => noahmp%energy%state%ALBOLD          ,& ! in,     snow albedo at last time step
              ALBSND          => noahmp%energy%state%ALBSND          ,& ! out,    snow albedo for direct(1=vis, 2=nir)
              ALBSNI          => noahmp%energy%state%ALBSNI           & ! out,    snow albedo for diffuse(1=vis, 2=nir)
             )
! ----------------------------------------------------------------------

    ! initialization
    ALBSND(1: NumSWRadBand) = 0.0
    ALBSNI(1: NumSWRadBand) = 0.0

    ! when CosSolarZenithAngle > 0
    ALB = CLASS_ALB_REF + (ALBOLD - CLASS_ALB_REF) * exp( -0.01 * MainTimeStep / CLASS_SNO_AGE )

    ! 1 mm fresh snow(SWE) -- 10mm snow depth, assumed the fresh snow density 100kg/m3
    ! here assume 1cm snow depth will fully cover the old snow
    if ( QSNOW > 0.0 ) then
       ALB = ALB + min(QSNOW, SWEMX/MainTimeStep) * (CLASS_ALB_NEW - ALB) / (SWEMX/MainTimeStep)
    endif

    ALBSNI(1)= ALB         ! vis diffuse
    ALBSNI(2)= ALB         ! nir diffuse
    ALBSND(1)= ALB         ! vis direct
    ALBSND(2)= ALB         ! nir direct

    ALBOLD = ALB

    end associate

  end subroutine SnowAlbedoClass

end module SnowAlbedoClassMod
