module ForcingVarInitMod

!!! Initialize column (1-D) Noah-MP forcing variables
!!! Forcing variables should be first defined in ForcingVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use NoahmpIOVarType
  use NoahmpVarType
  use Machine, only : kind_noahmp

  implicit none

contains

!=== initialize with default values
  subroutine ForcingVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    noahmp%forcing%SFCTMP      = huge(1.0)
    noahmp%forcing%UU          = huge(1.0)
    noahmp%forcing%VV          = huge(1.0)
    noahmp%forcing%Q2          = huge(1.0)
    noahmp%forcing%SFCPRS      = huge(1.0)
    noahmp%forcing%LWDN        = huge(1.0)
    noahmp%forcing%PSFC        = huge(1.0)
    noahmp%forcing%SOLDN       = huge(1.0)
    noahmp%forcing%PRCPCONV    = huge(1.0)
    noahmp%forcing%PRCPNONC    = huge(1.0)
    noahmp%forcing%PRCPSHCV    = huge(1.0)
    noahmp%forcing%PRCPSNOW    = huge(1.0)
    noahmp%forcing%PRCPGRPL    = huge(1.0)
    noahmp%forcing%PRCPHAIL    = huge(1.0)
    noahmp%forcing%PSFC        = huge(1.0)
    noahmp%forcing%TBOT        = huge(1.0)

  end subroutine ForcingVarInitDefault

!=== initialize with input data or table values
  subroutine ForcingVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp
    
    !local
    real(kind=kind_noahmp)              :: PRCPOTHR                                      ! other precip, e.g. fog [mm/s]                ! MB/AN : v3.7   
    real(kind=kind_noahmp)              :: PRCP                                          ! total precipitation entering  [mm/s]         ! MB/AN : v3.7 

    associate(                                                  &
              I           => noahmp%config%domain%ILOC         ,&
              J           => noahmp%config%domain%JLOC         ,&
              KTS         => NoahmpIO%kts                       &
             )
             
    noahmp%forcing%SFCTMP      = NoahmpIO%T_PHY(I,1,J)
    noahmp%forcing%UU          = NoahmpIO%U_PHY(I,1,J)
    noahmp%forcing%VV          = NoahmpIO%V_PHY(I,1,J)
    noahmp%forcing%Q2          = NoahmpIO%QV_CURR(I,1,J)/(1.0 + NoahmpIO%QV_CURR(I,1,J)) ! convert from mixing ratio to specific humidity [kg/kg]
    noahmp%forcing%SFCPRS      = (NoahmpIO%P8W(I,KTS+1,J)+NoahmpIO%P8W(I,KTS,J))*0.5     ! surface pressure defined at intermediate level [Pa] 
                                                                                         ! consistent with temperature, mixing ratio
    noahmp%forcing%LWDN        = NoahmpIO%GLW      (I,J)
    noahmp%forcing%PSFC        = NoahmpIO%P8W      (I,1,J)
    noahmp%forcing%SOLDN       = NoahmpIO%SWDOWN   (I,J)
    noahmp%forcing%TBOT        = NoahmpIO%TMN      (I,J)
    
    ! precipitation preprocess: ! MB/AN : v3.7 
    PRCP                       = NoahmpIO%RAINBL   (I,J) / NoahmpIO%DTBL
    !if (present(NoahmpIO%MP_RAINC) .and. present(NoahmpIO%MP_RAINNC) .and. &
    !    present(NoahmpIO%MP_SHCV)  .and. present(NoahmpIO%MP_SNOW)   .and. &
    !    present(NoahmpIO%MP_GRAUP) .and. present(NoahmpIO%MP_HAIL))  then  
        
       noahmp%forcing%PRCPCONV    = NoahmpIO%MP_RAINC (I,J)
       noahmp%forcing%PRCPNONC    = NoahmpIO%MP_RAINNC(I,J)
       noahmp%forcing%PRCPSHCV    = NoahmpIO%MP_SHCV  (I,J)
       noahmp%forcing%PRCPSNOW    = NoahmpIO%MP_SNOW  (I,J) 
       noahmp%forcing%PRCPGRPL    = NoahmpIO%MP_GRAUP (I,J) 
       noahmp%forcing%PRCPHAIL    = NoahmpIO%MP_HAIL  (I,J) 

       PRCPOTHR  = PRCP - noahmp%forcing%PRCPCONV - &
                   noahmp%forcing%PRCPNONC - noahmp%forcing%PRCPSHCV                     ! take care of other (fog) contained in rainbl
                   
       PRCPOTHR  = max(0.0,PRCPOTHR)
       noahmp%forcing%PRCPNONC  = noahmp%forcing%PRCPNONC + PRCPOTHR
       noahmp%forcing%PRCPSNOW  = noahmp%forcing%PRCPSNOW + NoahmpIO%SR(I,J) * PRCPOTHR 
       
    !elseif
    !   noahmp%forcing%PRCPCONV    = 0.0
    !   noahmp%forcing%PRCPNONC    = PRCP
    !   noahmp%forcing%PRCPSHCV    = 0.0
    !   noahmp%forcing%PRCPSNOW    = NoahmpIO%SR(I,J) * PRCP
    !   noahmp%forcing%PRCPGRPL    = 0.0
    !   noahmp%forcing%PRCPHAIL    = 0.0    
    !endif
    end associate
 
  end subroutine ForcingVarInitTransfer

end module ForcingVarInitMod
