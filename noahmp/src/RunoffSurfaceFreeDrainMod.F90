module RunoffSurfaceFreeDrainMod

!!! Calculate inflitration rate at soil surface and surface runoff for free drainage scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod, only : SoilDiffusivityConductivityOpt2

  implicit none

contains

  subroutine RunoffSurfaceFreeDrain(noahmp, DT)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: INFIL
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: DT       ! timestep (may not be the same as model timestep)

! local variable
    integer                :: IALP1, J, JJ,  K        ! do-loop index
    integer, parameter     :: CVFRZ = 3               ! frozen soil pre-factor
    real(kind=kind_noahmp) :: VAL                     ! remaining fraction
    real(kind=kind_noahmp) :: DDT                     ! remaining accumulated maximum holdable soil water (m)
    real(kind=kind_noahmp) :: PX                      ! surface in water (m)
    real(kind=kind_noahmp) :: DT1                     ! time indices
    real(kind=kind_noahmp) :: DD                      ! accumulated maximum holdable soil water (m)
    real(kind=kind_noahmp) :: DICE                    ! maximum soil ice water (m)
    real(kind=kind_noahmp) :: SoilImpervFrac                     ! impervious fraction due to frozen soil
    real(kind=kind_noahmp) :: SUM1                    ! accumulation index
    real(kind=kind_noahmp) :: ACRT                    ! soil ice coefficient
    real(kind=kind_noahmp) :: SoilWatDiffusivity                     ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp) :: SoilWatConductivity                    ! soil water conductivity [m/s]
    real(kind=kind_noahmp) :: SMCAV                   ! soil moisture holding capacity (m3/m3)
    real(kind=kind_noahmp) :: INFMAX                  ! maximum infiltration rate (m/s)
    real(kind=kind_noahmp), allocatable, dimension(:) :: DMAX    ! maximum soil water that can hold (m)

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,   number of soil layers
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,   depth [m] of layer-bottom from soil surface
              FlagUrban      => noahmp%config%domain%FlagUrban     ,& ! in,   logical flag for urban grid
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,   soil water content [m3/m3]
              SoilIce            => noahmp%water%state%SoilIce             ,& ! in,   soil ice content [m3/m3]
              SoilIceMax         => noahmp%water%state%SoilIceMax          ,& ! in,   maximum soil ice content (m3/m3)
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,   water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,   saturated value of soil moisture [m3/m3]
              SMCWLT          => noahmp%water%param%SMCWLT           ,& ! in,   wilting point soil moisture [m3/m3]
              KDT             => noahmp%water%param%KDT              ,& ! in,   parameter to calculate maximum infiltration rate
              FRZX            => noahmp%water%param%FRZX             ,& ! in,   parameter to calculate frozen soil impermeable fraction
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,  surface runoff [mm/s]
              PDDUM           => noahmp%water%flux%PDDUM              & ! out,  infiltration rate at surface (mm/s)
             )
! ----------------------------------------------------------------------

    ! initialize
    allocate( DMAX(1:NumSoilLayer) )
    DMAX(1:NumSoilLayer) = 0.0

    ! start infiltration for free drainage scheme
    if ( QINSUR > 0.0 ) then

       DT1   = DT / 86400.0
       SMCAV = SMCMAX(1) - SMCWLT(1)

       ! compute maximum infiltration rate
       DMAX(1) = -DepthSoilLayer(1) * SMCAV
       DICE    = -DepthSoilLayer(1) * SoilIce(1)
       DMAX(1) =  DMAX(1)  * ( 1.0 - (SoilLiqWater(1) + SoilIce(1) - SMCWLT(1)) / SMCAV )
       DD      =  DMAX(1)
       do K = 2, NumSoilLayer
          DICE    = DICE + ( DepthSoilLayer(K-1) - DepthSoilLayer(K) ) * SoilIce(K)
          DMAX(K) = ( DepthSoilLayer(K-1) - DepthSoilLayer(K) ) * SMCAV
          DMAX(K) = DMAX(K) * ( 1.0 - (SoilLiqWater(K) + SoilIce(K) - SMCWLT(K)) / SMCAV )
          DD      = DD + DMAX(K)
       enddo
       VAL    = 1.0 - exp(-1.0 * KDT * DT1)
       DDT    = DD * VAL
       PX     = max( 0.0, QINSUR * DT )
       INFMAX = ( PX * (DDT / (PX + DDT)) ) / DT

       ! impermeable fraction due to frozen soil
       SoilImpervFrac = 1.0
       if ( DICE > 1.0e-2 ) then
          ACRT  = CVFRZ * FRZX / DICE
          SUM1  = 1.0
          IALP1 = CVFRZ - 1
          do J = 1, IALP1
             K = 1
             do JJ = J+1, IALP1
                K = K * JJ
             enddo
             SUM1 = SUM1 + ( ACRT ** (CVFRZ - J) ) / float(K)
          enddo
          SoilImpervFrac = 1.0 - exp(-ACRT) * SUM1
       endif

       ! correction of infiltration limitation
       INFMAX = INFMAX * SoilImpervFrac
       ! jref for urban areas
       ! if ( FlagUrban .eqv. .true. ) INFMAX == INFMAX * 0.05

       ! soil hydraulic conductivity and diffusivity
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, SoilLiqWater(1), SoilIceMax, 1)

       INFMAX = max( INFMAX, SoilWatConductivity )
       INFMAX = min( INFMAX, PX   )

       ! compute surface runoff and infiltration rate
       RUNSRF = max( 0.0, QINSUR - INFMAX )
       PDDUM  = QINSUR - RUNSRF

    endif ! QINSUR > 0.0

    end associate

  end subroutine RunoffSurfaceFreeDrain

end module RunoffSurfaceFreeDrainMod
