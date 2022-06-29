module ResistanceGroundEvaporationMod

!!! Compute soil surface resistance to ground evaporation/sublimation
!!! It represents the resistance imposed by the molecular diffusion in soil 
!!! surface (as opposed to aerodynamic resistance computed elsewhere in the model)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceGroundEvaporation(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! local variables
    real(kind=kind_noahmp)           :: BEVAP        ! soil water evaporation factor (0- 1)
    real(kind=kind_noahmp)           :: L_RSURF      ! Dry-layer thickness for computing RSURF (Sakaguchi and Zeng, 2009)
    real(kind=kind_noahmp)           :: D_RSURF      ! Reduced vapor diffusivity in soil for computing RSURF (SZ09)
    real(kind=kind_noahmp)           :: PSI          ! surface layer soil matrix potential (m)

! --------------------------------------------------------------------
    associate(                                                        &
              IST             => noahmp%config%domain%IST            ,& ! in,    surface type 1-soil; 2-lake
              ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,    depth of layer-bottom from soil surface
              URBAN_FLAG      => noahmp%config%domain%URBAN_FLAG     ,& ! in,    logical flag for urban grid
              OptGroundResistanceEvap => noahmp%config%nmlist%OptGroundResistanceEvap,& ! in,    options for ground resistance to evaporation/sublimation
              RSURF_EXP       => noahmp%energy%param%RSURF_EXP       ,& ! in,    exponent in the shape parameter for soil resistance
              RSURF_SNOW      => noahmp%energy%param%RSURF_SNOW      ,& ! in,    surface resistance for snow(s/m)
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,    saturated value of soil moisture [m3/m3]
              SMCWLT          => noahmp%water%param%SMCWLT           ,& ! in,    wilting point soil moisture [m3/m3]
              BEXP            => noahmp%water%param%BEXP             ,& ! in,    soil B parameter
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,    saturated soil matric potential (m)
              SH2O            => noahmp%water%state%SH2O             ,& ! in,    soil water content [m3/m3]
              FSNO            => noahmp%water%state%FSNO             ,& ! in,    snow cover fraction (-)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (K)
              RSURF           => noahmp%energy%state%RSURF           ,& ! out,   ground surface resistance (s/m)
              RHSUR           => noahmp%energy%state%RHSUR            & ! out,   raltive humidity in surface soil/snow air space (-)
             )
! ----------------------------------------------------------------------

    ! initialization
    BEVAP = max( 0.0, SH2O(1)/SMCMAX(1) )
    if ( IST == 2 ) then  ! lake point
       RSURF = 1.0        ! avoid being divided by 0
       RHSUR = 1.0
    else    ! soil point

       if ( (OptGroundResistanceEvap == 1) .or. (OptGroundResistanceEvap == 4) ) then   ! Sakaguchi and Zeng, 2009
          ! taking the "residual water content" to be the wilting point, 
          ! and correcting the exponent on the D term (typo in SZ09 ?)
          L_RSURF = (-ZSOIL(1)) * (exp( (1.0 - min(1.0,SH2O(1)/SMCMAX(1))) ** RSURF_EXP ) - 1.0) / (2.71828 - 1.0)
          D_RSURF = 2.2e-5 * SMCMAX(1) * SMCMAX(1) * (1.0 - SMCWLT(1)/SMCMAX(1)) ** (2.0 + 3.0/BEXP(1))
          RSURF = L_RSURF / D_RSURF
       elseif ( OptGroundResistanceEvap == 2 ) then  ! Sellers (1992) original
          RSURF = FSNO * 1.0 + (1.0 - FSNO) * exp(8.25 - 4.225*BEVAP)
       elseif ( OptGroundResistanceEvap == 3 ) then  ! Sellers (1992) adjusted to decrease RSURF for wet soil
          RSURF = FSNO * 1.0 + (1.0 - FSNO) * exp(8.25 - 6.0*BEVAP) 
       endif
       if ( OptGroundResistanceEvap == 4 ) then ! FSNO weighted; snow RSURF set in MPTABLE v3.8
          RSURF = 1.0 / ( FSNO * (1.0/RSURF_SNOW) + (1.0 - FSNO) * (1.0/max(RSURF,0.001)) )
       endif
       if ( (SH2O(1) < 0.01) .and. (SNOWH == 0.0) ) RSURF = 1.0e6

       PSI   = -PSISAT(1) * (max(0.01,SH2O(1)) / SMCMAX(1)) ** (-BEXP(1))
       RHSUR = FSNO + (1.0 - FSNO) * exp(PSI * ConstGravityAcc / (ConstGasWaterVapor * TG))
    endif

    ! urban
    if ( (URBAN_FLAG .eqv. .true.) .and. (SNOWH == 0.0) ) then
       RSURF = 1.0e6
    endif

    end associate

  end subroutine ResistanceGroundEvaporation

end module ResistanceGroundEvaporationMod
