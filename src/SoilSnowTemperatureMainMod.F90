module SoilSnowTemperatureMainMod

!!! Main module to compute snow (if exists) and soil layer temperature. 
!!! Note that snow temperatures during melting season may exceed melting 
!!! point (TFRZ) but later in SoilSnowPhaseChange subroutine the snow
!!! temperatures are reset to TFRZ for melting snow.

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SoilSnowTemperatureSolverMod, only : SoilSnowTemperatureSolver
  use SoilSnowThermalDiffusionMod,  only : SoilSnowThermalDiffusion 

  implicit none

contains

  subroutine SoilSnowTemperatureMain(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TSNOSOI
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                :: IZ         ! loop index
    character(len=256)     :: message    ! error message
    real(kind=kind_noahmp) :: ERR_EST    ! heat storage error  (w/m2)
    real(kind=kind_noahmp) :: SSOIL2     ! ground heat flux (w/m2) (for energy check)
    real(kind=kind_noahmp) :: EFLXB2     ! heat flux from the bottom (w/m2) (for energy check)
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHSTS  ! right-hand side term of the matrix
    real(kind=kind_noahmp), allocatable, dimension(:) :: AI     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: BI     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: CI     ! left-hand side term
    real(kind=kind_noahmp), allocatable, dimension(:) :: TBEG   ! temporary temperature term 

! --------------------------------------------------------------------
    associate(                                                        &
              NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,    number of soil layers
              NSNOW           => noahmp%config%domain%NSNOW          ,& ! in,    maximum number of snow layers
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! in,    actual number of snow layers
              DT              => noahmp%config%domain%DT             ,& ! in,    main noahmp timestep (s)
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,    thickness of snow/soil layers (m)
              ZSNSO           => noahmp%config%domain%ZSNSO          ,& ! in,    depth of snow/soil layer-bottom (m)
              OPT_TBOT        => noahmp%config%nmlist%OPT_TBOT       ,& ! in,    options for lower boundary condition of soil temperature
              OPT_STC         => noahmp%config%nmlist%OPT_STC        ,& ! in,    options for snow/soil temperature time scheme
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              ZBOT            => noahmp%energy%param%ZBOT            ,& ! in,    depth of lower boundary condition (m) from soil surface
              SSOIL           => noahmp%energy%flux%SSOIL            ,& ! in,    soil heat flux (w/m2) [+ to soil]
              TBOT            => noahmp%energy%state%TBOT            ,& ! in,    bottom soil temp. at ZBOT (K)
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (k)
              DF              => noahmp%energy%state%DF              ,& ! in,    thermal conductivity [w/m/k] for all soil & snow
              HCPCT           => noahmp%energy%state%HCPCT           ,& ! in,    heat capacity [j/m3/k] for all soil & snow
              STC             => noahmp%energy%state%STC             ,& ! inout, snow and soil layer temperature [K]
              ZBOTSNO         => noahmp%energy%state%ZBOTSNO         ,& ! out,   depth of lower boundary condition (m) from snow surface
              EFLXB           => noahmp%energy%flux%EFLXB            ,& ! out,   energy influx from soil bottom (w/m2)
              PHI             => noahmp%energy%flux%PHI               & ! out,   light penetrating through soil/snow water (W/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    allocate( RHSTS (-NSNOW+1:NSOIL) )
    allocate( AI    (-NSNOW+1:NSOIL) )
    allocate( BI    (-NSNOW+1:NSOIL) )
    allocate( CI    (-NSNOW+1:NSOIL) )
    allocate( TBEG  (-NSNOW+1:NSOIL) )
    RHSTS(:) = 0.0
    AI(:)    = 0.0
    BI(:)    = 0.0
    CI(:)    = 0.0
    TBEG(:)  = 0.0

    ! compute solar penetration through water, needs more work
    PHI(ISNOW+1:NSOIL) = 0.0

    ! adjust ZBOT from soil surface to ZBOTSNO from snow surface
    ZBOTSNO = ZBOT - SNOWH

    ! snow/soil heat storage for energy balance check
    do IZ = ISNOW+1, NSOIL
       TBEG(IZ) = STC(IZ)
    enddo

    ! compute soil temperatures
    call SoilSnowThermalDiffusion(noahmp, AI, BI, CI, RHSTS)
    call SoilSnowTemperatureSolver(noahmp, DT, AI, BI, CI, RHSTS)

    ! update ground heat flux just for energy check, but not for final output
    ! otherwise, it would break the surface energy balance
    if ( OPT_TBOT == 1 ) then
       EFLXB2 = 0.0
    elseif ( OPT_TBOT == 2 ) then
       EFLXB2 = DF(NSOIL) * (TBOT - STC(NSOIL)) / (0.5 * (ZSNSO(NSOIL-1)+ZSNSO(NSOIL)) - ZBOTSNO)
    endif

    ! Skip the energy balance check for now, until we can make it work right for small time steps.
    return

    ! energy balance check
    ERR_EST = 0.0
    do IZ = ISNOW+1, NSOIL
       ERR_EST = ERR_EST + (STC(IZ)-TBEG(IZ)) * DZSNSO(IZ) * HCPCT(IZ) / DT
    enddo
    if ( OPT_STC == 1 .or. OPT_STC == 3 ) then  ! semi-implicit
       ERR_EST = ERR_EST - (SSOIL + EFLXB)
    else  ! full-implicit
       SSOIL2  = DF(ISNOW+1) * (TG - STC(ISNOW+1)) / (0.5 * DZSNSO(ISNOW+1))   !M. Barlage
       ERR_EST = ERR_EST - (SSOIL2 + EFLXB2)
    endif
    if ( abs(ERR_EST) > 1.0 ) then    ! W/m2
       print*,'TSNOSOI is losing(-)/gaining(+) false energy',ERR_EST,' W/m2'
       print*,'ERR_EST,SSOIL,SNOWH,TG,STC(ISNOW+1),EFLXB = ', &
               ERR_EST,SSOIL,SNOWH,TG,STC(ISNOW+1),EFLXB
       stop 'error'
       !WRITE(message,*) 'TSNOSOI is losing(-)/gaining(+) false energy',ERR_EST,' W/m2'
       !call wrf_message(trim(message))
       !WRITE(message,'(i6,1x,i6,1x,i3,F18.13,5F20.12)') &
       !     ILOC, JLOC, ERR_EST,SSOIL,SNOWH,TG,STC(ISNOW+1),EFLXB
       !call wrf_message(trim(message))
    endif

    end associate

  end subroutine SoilSnowTemperatureMain

end module SoilSnowTemperatureMainMod
