module RunoffSurfaceFreeDrainMod

!!! Calculate inflitration rate at soil surface and surface runoff for free drainage scheme

  use Machine, only : kind_noahmp
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
    real(kind=kind_noahmp) :: FCR                     ! impermeable fraction due to frozen soil
    real(kind=kind_noahmp) :: SUM1                    ! accumulation index
    real(kind=kind_noahmp) :: ACRT                    ! soil ice coefficient
    real(kind=kind_noahmp) :: WDF                     ! soil water diffusivity (m2/s)
    real(kind=kind_noahmp) :: WCND                    ! soil water conductivity [m/s]
    real(kind=kind_noahmp) :: SMCAV                   ! soil moisture holding capacity (m3/m3)
    real(kind=kind_noahmp) :: INFMAX                  ! maximum infiltration rate (m/s)
    real(kind=kind_noahmp), allocatable, dimension(:) :: DMAX    ! maximum soil water that can hold (m)

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
              URBAN_FLAG      => noahmp%config%domain%URBAN_FLAG     ,& ! in,     logical flag for urban grid
              SH2O            => noahmp%water%state%SH2O             ,& ! in,     soil water content [m3/m3]
              SICE            => noahmp%water%state%SICE             ,& ! in,     soil ice content [m3/m3]
              SICEMAX         => noahmp%water%state%SICEMAX          ,& ! in,     maximum soil ice content (m3/m3)
              QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
              SMCWLT          => noahmp%water%param%SMCWLT           ,& ! in,     wilting point soil moisture [m3/m3]
              KDT             => noahmp%water%param%KDT              ,& ! in,     parameter to calculate maximum infiltration rate
              FRZX            => noahmp%water%param%FRZX             ,& ! in,     parameter to calculate frozen soil impermeable fraction
              RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
              PDDUM           => noahmp%water%flux%PDDUM              & ! out,    infiltration rate at surface (mm/s)
             )
! ----------------------------------------------------------------------

    ! initialize
    allocate( DMAX(1:NSOIL) )
    DMAX(1:NSOIL) = 0.0

    ! start infiltration for free drainage scheme
    if ( QINSUR > 0.0 ) then

       DT1   = DT / 86400.0
       SMCAV = SMCMAX(1) - SMCWLT(1)

       ! compute maximum infiltration rate
       DMAX(1) = -ZSOIL(1) * SMCAV
       DICE    = -ZSOIL(1) * SICE(1)
       DMAX(1) =  DMAX(1)  * ( 1.0 - (SH2O(1) + SICE(1) - SMCWLT(1)) / SMCAV )
       DD      =  DMAX(1)
       do K = 2, NSOIL
          DICE    = DICE + ( ZSOIL(K-1) - ZSOIL(K) ) * SICE(K)
          DMAX(K) = ( ZSOIL(K-1) - ZSOIL(K) ) * SMCAV
          DMAX(K) = DMAX(K) * ( 1.0 - (SH2O(K) + SICE(K) - SMCWLT(K)) / SMCAV )
          DD      = DD + DMAX(K)
       enddo
       VAL    = 1.0 - exp(-1.0 * KDT * DT1)
       DDT    = DD * VAL
       PX     = max( 0.0, QINSUR * DT )
       INFMAX = ( PX * (DDT / (PX + DDT)) ) / DT

       ! impermeable fraction due to frozen soil
       FCR = 1.0
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
          FCR = 1.0 - exp(-ACRT) * SUM1
       endif

       ! correction of infiltration limitation
       INFMAX = INFMAX * FCR
       ! jref for urban areas
       ! if ( URBAN_FLAG .eqv. .true. ) INFMAX == INFMAX * 0.05

       ! soil hydraulic conductivity and diffusivity
       call SoilDiffusivityConductivityOpt2(noahmp, WDF, WCND, SH2O(1), SICEMAX, 1)

       INFMAX = max( INFMAX, WCND )
       INFMAX = min( INFMAX, PX   )

       ! compute surface runoff and infiltration rate
       RUNSRF = min( 0.0, QINSUR - INFMAX )
       PDDUM  = QINSUR - RUNSRF

    endif ! QINSUR > 0.0

    end associate

  end subroutine RunoffSurfaceFreeDrain

end module RunoffSurfaceFreeDrainMod
