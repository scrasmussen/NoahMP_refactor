module SoilWaterDiffusionRichardsMod

!!! Solve Richards equation for soil water movement/diffusion
!!! Compute the right hand side of the time tendency term of the soil
!!! water diffusion equation.  also to compute ( prepare ) the matrix
!!! coefficients for the tri-diagonal matrix of the implicit time scheme.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod

  implicit none

contains

  subroutine SoilWaterDiffusionRichards(noahmp, AI, BI, CI, RHSTT)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: SRT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: RHSTT  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: AI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: BI     ! left-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: CI     ! left-hand side term of the matrix

! local variable
    integer                :: K         ! loop index
    real(kind=kind_noahmp) :: TEMP1     ! temporary variable
    real(kind=kind_noahmp) :: SMXWTD    ! temporary soil moisture between bottom of the soil and water table
    real(kind=kind_noahmp) :: SMXBOT    ! temporary soil moisture below bottom to calculate flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: DDZ     ! temporary variable
    real(kind=kind_noahmp), allocatable, dimension(:) :: DENOM   ! temporary soil thickness
    real(kind=kind_noahmp), allocatable, dimension(:) :: DSMDZ   ! temporary soil moisture
    real(kind=kind_noahmp), allocatable, dimension(:) :: WFLUX   ! temporary water flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMX     ! temporary soil moisture

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,     number of soil layers
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,     depth [m] of layer-bottom from soil surface
              OptSoilPermeabilityFrozen => noahmp%config%nmlist%OptSoilPermeabilityFrozen,& ! in,     options for frozen soil permeability
              OptRunoffSubsurface => noahmp%config%nmlist%OptRunoffSubsurface ,& ! in,     options for drainage and subsurface runoff
              SLOPE           => noahmp%water%param%SLOPE            ,& ! in,     slope index for soil drainage
              PDDUM           => noahmp%water%flux%PDDUM             ,& ! in,     infiltration rate at surface (mm/s)
              QSEVA           => noahmp%water%flux%QSEVA             ,& ! in,     evaporation from soil surface [mm/s]
              ETRANI          => noahmp%water%flux%ETRANI            ,& ! in,     evapotranspiration from soil layers [mm/s]
              SH2O            => noahmp%water%state%SH2O             ,& ! in,     soil water content [m3/m3]
              SMC             => noahmp%water%state%SMC              ,& ! in,     total soil moisture [m3/m3]
              ZWT             => noahmp%water%state%ZWT              ,& ! in,     water table depth [m]
              FCR             => noahmp%water%state%FCR              ,& ! in,     fraction of imperviousness due to frozen soil
              FCRMAX          => noahmp%water%state%FCRMAX           ,& ! in,     maximum fraction of imperviousness (FCR)
              SICEMAX         => noahmp%water%state%SICEMAX          ,& ! in,     maximum soil ice content (m3/m3)
              SMCWTD          => noahmp%water%state%SMCWTD           ,& ! in,     soil moisture between bottom of the soil and the water table
              WCND            => noahmp%water%state%WCND             ,& ! out,    soil hydraulic conductivity (m/s)
              WDF             => noahmp%water%state%WDF              ,& ! out,    soil water diffusivity (m2/s)
              QDRAIN          => noahmp%water%flux%QDRAIN             & ! out,    soil bottom drainage (m/s)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( DDZ  (1:NumSoilLayer) )
    allocate( DENOM(1:NumSoilLayer) )
    allocate( DSMDZ(1:NumSoilLayer) )
    allocate( WFLUX(1:NumSoilLayer) )
    allocate( SMX  (1:NumSoilLayer) )
    RHSTT(:) = 0.0
    AI(:)    = 0.0
    BI(:)    = 0.0
    CI(:)    = 0.0
    DDZ(:)   = 0.0
    DENOM(:) = 0.0
    DSMDZ(:) = 0.0
    WFLUX(:) = 0.0
    SMX(:)   = 0.0

    ! compute soil hydraulic conductivity and diffusivity
    if ( OptSoilPermeabilityFrozen == 1 ) then
       do K = 1, NumSoilLayer
          call SoilDiffusivityConductivityOpt1(noahmp,WDF(K),WCND(K),SMC(K),FCR(K),K) 
          SMX(K) = SMC(K)
       enddo
       if ( OptRunoffSubsurface == 5 ) SMXWTD = SMCWTD
    endif

    if ( OptSoilPermeabilityFrozen == 2 ) then
       do K = 1, NumSoilLayer
          call SoilDiffusivityConductivityOpt2(noahmp,WDF(K),WCND(K),SH2O(K),SICEMAX,K)
          SMX(K) = SH2O(K)
       enddo
       if ( OptRunoffSubsurface == 5 ) SMXWTD = SMCWTD * SH2O(NumSoilLayer) / SMC(NumSoilLayer)  !same liquid fraction as in the bottom layer
    endif

    ! compute gradient and flux of soil water diffusion terms
    do K = 1, NumSoilLayer
       if ( K == 1 ) then
          DENOM(K) = - DepthSoilLayer(K)
          TEMP1    = - DepthSoilLayer(K+1)
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K) * DSMDZ(K) + WCND(K) - PDDUM + ETRANI(K) + QSEVA
       else if ( K < NumSoilLayer ) then
          DENOM(k) = (DepthSoilLayer(K-1) - DepthSoilLayer(K))
          TEMP1    = (DepthSoilLayer(K-1) - DepthSoilLayer(K+1))
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K  ) * DSMDZ(K  ) + WCND(K  )         &
                   - WDF(K-1) * DSMDZ(K-1) - WCND(K-1) + ETRANI(K)
       else
          DENOM(K) = (DepthSoilLayer(K-1) - DepthSoilLayer(K))
          if ( (OptRunoffSubsurface == 1) .or. (OptRunoffSubsurface == 2) ) then
             QDRAIN = 0.0
          endif
          if ( (OptRunoffSubsurface == 3) .or. (OptRunoffSubsurface == 6) .or. &
               (OptRunoffSubsurface == 7) .or. (OptRunoffSubsurface == 8) ) then
             QDRAIN = SLOPE * WCND(K)
          endif
          if ( OptRunoffSubsurface == 4 ) then
             QDRAIN = (1.0 - FCRMAX) * WCND(K)
          endif
          if ( OptRunoffSubsurface == 5 ) then   !gmm new m-m&f water table dynamics formulation
             TEMP1  = 2.0 * DENOM(K)
             if ( ZWT < (DepthSoilLayer(NumSoilLayer)-DENOM(NumSoilLayer)) ) then
                ! gmm interpolate from below, midway to the water table, 
                ! to the middle of the auxiliary layer below the soil bottom
                SMXBOT = SMX(K) - (SMX(K) - SMXWTD) * DENOM(K) * 2.0 / (DENOM(K) + DepthSoilLayer(K) - ZWT)
             else
                SMXBOT = SMXWTD
             endif
             DSMDZ(K) = 2.0 * (SMX(K) - SMXBOT) / TEMP1
             QDRAIN   = WDF(K) * DSMDZ(K) + WCND(K)
          endif
          WFLUX(K) = -(WDF(K-1)*DSMDZ(K-1)) - WCND(K-1) + ETRANI(K) + QDRAIN
       endif
    enddo

    ! prepare the matrix coefficients for the tri-diagonal matrix
    do K = 1, NumSoilLayer
       if ( K == 1 ) then
          AI(K)    =   0.0
          BI(K)    =   WDF(K  ) * DDZ(K  ) / DENOM(K)
          CI(K)    = - BI (K)
       else if ( K < NumSoilLayer ) then
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = - WDF(K  ) * DDZ(K  ) / DENOM(K)
          BI(K)    = - ( AI (K) + CI (K) )
       else
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    =   0.0
          BI(K)    = - ( AI (K) + CI (K) )
       endif
       RHSTT(K) = WFLUX(K) / (-DENOM(K))
    enddo

    end associate

  end subroutine SoilWaterDiffusionRichards

end module SoilWaterDiffusionRichardsMod
