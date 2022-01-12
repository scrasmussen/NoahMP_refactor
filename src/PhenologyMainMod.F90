module PhenologyMainMod


!!! Main Phenology module to estimate vegetation phenology considering vegeation canopy being buries by snow and evolution in time

  use Machine, only : kind_noahmp
  use NoahmpVarType

  implicit none

contains

  subroutine PhenologyMain (noahmp)


! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PHENOLOGY
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable

    real(kind=kind_noahmp)           :: DB     !thickness of canopy buried by snow (m)
    real(kind=kind_noahmp)           :: FB     !fraction of canopy buried by snow
    real(kind=kind_noahmp)           :: SNOWHC !critical snow depth at which short vege
                                                   !is fully covered by snow

    integer                          :: K       !index
    integer                          :: IT1,IT2 !interpolation months
    real(kind=kind_noahmp)           :: DAY     !current day of year ( 0 <= DAY < YEARLEN )
    real(kind=kind_noahmp)           :: WT1,WT2 !interpolation weights
    real(kind=kind_noahmp)           :: T       !current month (1.00, ..., 12.00)

! --------------------------------------------------------------------
    associate(                                               &
              DVEG       => noahmp%config%nmlist%DVEG       ,&
              VEGTYP     => noahmp%config%domain%VEGTYP     ,&  !vegetation type 
              CROPTYPE   => noahmp%config%domain%CROPTYP    ,&  !vegetation type 
              ISICE      => noahmp%config%domain%ISICE      ,&
              ISBARREN   => noahmp%config%domain%ISBARREN   ,&
              ISWATER    => noahmp%config%domain%ISWATER    ,&
              URBAN_FLAG => noahmp%config%domain%URBAN_FLAG ,&
              SNOWH      => noahmp%water%state%SNOWH        ,&  !snow height [m]
              TV         => noahmp%energy%state%TV          ,&  !vegetation temperature (k)
              LAT        => noahmp%config%domain%LAT        ,&  !latitude (radians)
              YEARLEN    => noahmp%config%domain%YEARLEN    ,&  !Number of days in the particular year
              JULIAN     => noahmp%config%domain%JULIAN     ,&  !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )
              TROOT      => noahmp%energy%state%TROOT       ,&  !root-zone averaged temperature (k)
              PGS        => noahmp%biochem%state%PGS        ,&  !plant growing stageend subroutine PhenologyMain(noahmp)
              HVT        => noahmp%energy%param%HVT         ,&
              HVB        => noahmp%energy%param%HVB         ,&
              LAIM       => noahmp%energy%param%LAIM        ,&
              SAIM       => noahmp%energy%param%SAIM        ,&
              TMIN       => noahmp%biochem%param%TMIN       ,&  
              LAI        => noahmp%energy%state%LAI         ,&  !LAI, unadjusted for burying by snow
              SAI        => noahmp%energy%state%SAI         ,&  !SAI, unadjusted for burying by snow
              ELAI       => noahmp%energy%state%ELAI        ,&  !leaf area index, after burying by snow
              ESAI       => noahmp%energy%state%ESAI        ,&  !stem area index, after burying by snow
              IGS        => noahmp%biochem%state%IGS         &  !growing season index (0=off, 1=on)
             )
                        
    !--------------------------------------------------------------------------------------------------

    IF (CROPTYPE == 0) THEN

      IF ( DVEG == 1 .or. DVEG == 3 .or. DVEG == 4 ) THEN

         IF (LAT >= 0.0) THEN
            ! Northern Hemisphere
            DAY = JULIAN
         ELSE
            ! Southern Hemisphere.  DAY is shifted by 1/2 year.
            DAY = MOD ( JULIAN + ( 0.5 * YEARLEN ) , REAL(YEARLEN) )
         ENDIF

         T = 12.0 * DAY / REAL(YEARLEN)
         IT1 = T + 0.5
         IT2 = IT1 + 1
         WT1 = (IT1+0.5) - T
         WT2 = 1.0-WT1
         IF (IT1 .LT.  1) IT1 = 12
         IF (IT2 .GT. 12) IT2 = 1

         LAI = WT1*LAIM(IT1) + WT2*LAIM(IT2)
         SAI = WT1*SAIM(IT1) + WT2*SAIM(IT2)
      ENDIF

      IF(DVEG == 7 .or. DVEG == 8 .or. DVEG == 9) THEN
        SAI = MAX(0.05,0.1 * LAI)  ! when reading LAI, set SAI to 10% LAI, but not below 0.05 MB: v3.8
        IF (LAI < 0.05) SAI = 0.0  ! if LAI below minimum, make sure SAI = 0
      ENDIF

      IF (SAI < 0.05) SAI = 0.0                    ! MB: SAI CHECK, change to 0.05 v3.6
      IF (LAI < 0.05 .OR. SAI == 0.0) LAI = 0.0  ! MB: LAI CHECK

      IF ( ( VEGTYP == ISWATER ) .OR. ( VEGTYP == ISBARREN ) .OR. &
           ( VEGTYP == ISICE   ) .or. ( urban_flag ) ) THEN
         LAI  = 0.0
         SAI  = 0.0
      ENDIF

    ENDIF   ! CROPTYPE == 0

    !buried by snow

    DB = MIN( MAX(SNOWH - HVB,0.0), HVT-HVB )
    FB = DB / MAX(1.E-06, HVT-HVB)

    IF(HVT> 0.0 .AND. HVT <= 1.0) THEN          !MB: change to 1.0 and 0.2 to reflect
      SNOWHC = HVT*EXP(-SNOWH/0.2)             !      changes to HVT in MPTABLE
      FB     = MIN(SNOWH,SNOWHC)/SNOWHC
    ENDIF

    ELAI =  LAI*(1.0-FB)
    ESAI =  SAI*(1.0-FB)
    IF (ESAI < 0.05 .and. CROPTYPE == 0) ESAI = 0.0                   ! MB: ESAI CHECK, change to 0.05 v3.6
    IF ((ELAI < 0.05 .OR. ESAI == 0.0) .and. CROPTYPE == 0) ELAI = 0.0  ! MB: LAI CHECK

    ! set growing season flag

    IF ((TV .GT. TMIN .and. CROPTYPE == 0).or.(PGS > 2 .and. PGS < 7 .and. CROPTYPE > 0)) THEN
        IGS = 1.
    ELSE
        IGS = 0.
    ENDIF        

    end associate

  end subroutine PhenologyMain

end module PhenologyMainMod
