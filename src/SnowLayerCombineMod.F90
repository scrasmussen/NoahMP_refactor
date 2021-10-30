module SnowLayerCombineMod

!!! Snowpack layer combination process
!!! Update snow ice, snow water, snow thickness, snow temperature

  use Machine, only : kind_noahmp
  use NoahmpType
  use ConstantDefineMod
  use SnowLayerWaterComboMod, only: SnowLayerWaterCombo

  implicit none

contains

  subroutine SnowLayerCombine(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMBINE
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: I,J,K,L      ! node indices
    integer                :: ISNOW_OLD    ! number of snow layer
    integer                :: MSSI         ! node index
    integer                :: NEIBOR       ! adjacent node selected for combination
    real(kind=kind_noahmp) :: ZWICE        ! total ice mass in snow
    real(kind=kind_noahmp) :: ZWLIQ        ! total liquid water in snow
    real(kind=kind_noahmp) :: DZMIN(3)     ! minimum thickness of each snow layer
    DATA DZMIN /0.025, 0.025, 0.1/         ! MB: change limit
!   DATA DZMIN /0.045, 0.05, 0.2/

! --------------------------------------------------------------------
    associate(                                                        &
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
              SNICE           => noahmp%water%state%SNICE            ,& ! inout,  snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout,  snow layer liquid water [mm]
              SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil liquid moisture (m3/m3)
              SICE            => noahmp%water%state%SICE             ,& ! inout,  soil ice moisture (m3/m3)
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! inout,  thickness of snow/soil layers (m)
              PONDING1        => noahmp%water%state%PONDING1         ,& ! out,    surface ponding 1 (mm)
              PONDING2        => noahmp%water%state%PONDING2         ,& ! out,    surface ponding 2 (mm)
              DZ2             => noahmp%water%diag%DZ2_COMBO         ,& ! out,    nodal thickness of 2 elements being combined [m]
              WLIQ2           => noahmp%water%diag%WLIQ2_COMBO       ,& ! out,    liquid water of element 2 [kg/m2]
              WICE2           => noahmp%water%diag%WICE2_COMBO       ,& ! out,    ice of element 2 [kg/m2]
              T2              => noahmp%water%diag%T2_COMBO          ,& ! out,    nodal temperature of element 2 [k]
              DZ              => noahmp%water%diag%DZ_COMBO          ,& ! out,    nodal thickness of 1 elements being combined [m]
              WLIQ            => noahmp%water%diag%WLIQ_COMBO        ,& ! out,    liquid water of element 1
              WICE            => noahmp%water%diag%WICE_COMBO        ,& ! out,    ice of element 1 [kg/m2]
              T               => noahmp%water%diag%T_COMBO            & ! out,    node temperature of element 1 [k]
             )
! ----------------------------------------------------------------------

! initialization for out-only variables
    DZ2    = 0.0
    WLIQ2  = 0.0
    WICE2  = 0.0
    T2     = 0.0
    DZ     = 0.0
    WLIQ   = 0.0
    WICE   = 0.0
    T      = 0.0

! check and combine small ice content layer
    ISNOW_OLD = ISNOW

    do J = ISNOW_OLD+1,0
       if ( SNICE(J) <= 0.1 ) then
          if ( J /= 0 ) then
             SNLIQ(J+1)  = SNLIQ(J+1)  + SNLIQ(J)
             SNICE(J+1)  = SNICE(J+1)  + SNICE(J)
             DZSNSO(J+1) = DZSNSO(J+1) + DZSNSO(J)
          else
             if ( ISNOW_OLD < -1 ) then    ! MB/KM: change to ISNOW
                SNLIQ(J-1)  = SNLIQ(J-1)  + SNLIQ(J)
                SNICE(J-1)  = SNICE(J-1)  + SNICE(J)
                DZSNSO(J-1) = DZSNSO(J-1) + DZSNSO(J)
             else
                if ( SNICE(J) >= 0.0 ) then
                   PONDING1 = SNLIQ(J)       ! ISNOW WILL GET SET TO ZERO BELOW; PONDING1 WILL GET 
                   SNEQV    = SNICE(J)       ! ADDED TO PONDING FROM PHASECHANGE PONDING SHOULD BE
                   SNOWH    = DZSNSO(J)      ! ZERO HERE BECAUSE IT WAS CALCULATED FOR THIN SNOW
                else  ! SNICE OVER-SUBLIMATED EARLIER
                   PONDING1 = SNLIQ(J) + SNICE(J)
                   if ( PONDING1 < 0.0 ) then  ! IF SNICE AND SNLIQ SUBLIMATES REMOVE FROM SOIL
                      SICE(1)  = max( 0.0, SICE(1)+PONDING1/(DZSNSO(1)*1000.0) )
                      PONDING1 = 0.0
                   endif
                   SNEQV = 0.0
                   SNOWH = 0.0
                endif ! if(SNICE(J) >= 0.0)
                SNLIQ(J)  = 0.0
                SNICE(J)  = 0.0
                DZSNSO(J) = 0.0
             endif ! if(ISNOW_OLD < -1)

             !SH2O(1) = SH2O(1) + SNLIQ(J)/(DZSNSO(1)*1000.0)
             !SICE(1) = SICE(1) + SNICE(J)/(DZSNSO(1)*1000.0)
          endif ! if(J /= 0)

          ! shift all elements above this down by one.
          if ( J > ISNOW+1 .and. ISNOW < -1 ) then
             do I = J, ISNOW+2, -1
                STC(I)    = STC(I-1)
                SNLIQ(I)  = SNLIQ(I-1)
                SNICE(I)  = SNICE(I-1)
                DZSNSO(I) = DZSNSO(I-1)
             enddo
          endif
          ISNOW = ISNOW + 1

       endif ! if(SNICE(J) <= 0.1)
    enddo ! do J

! to conserve water in case of too large surface sublimation
    if ( SICE(1) < 0.0) then
       SH2O(1) = SH2O(1) + SICE(1)
       SICE(1) = 0.0
    endif

    if ( ISNOW ==0 ) return   ! MB: get out if no longer multi-layer

    SNEQV  = 0.0
    SNOWH  = 0.0
    ZWICE  = 0.0
    ZWLIQ  = 0.0

    do J = ISNOW+1, 0
       SNEQV = SNEQV + SNICE(J) + SNLIQ(J)
       SNOWH = SNOWH + DZSNSO(J)
       ZWICE = ZWICE + SNICE(J)
       ZWLIQ = ZWLIQ + SNLIQ(J)
    enddo

! check the snow depth - all snow gone, the liquid water assumes ponding on soil surface.
    !if ( SNOWH < 0.05 .and. ISNOW < 0 ) then
    if ( SNOWH < 0.025 .and. ISNOW < 0 ) then ! MB: change limit
       ISNOW    = 0
       SNEQV    = ZWICE
       PONDING2 = ZWLIQ                ! LIMIT OF ISNOW < 0 MEANS INPUT PONDING
       if ( SNEQV <= 0.0 ) SNOWH = 0.0 ! SHOULD BE ZERO; SEE ABOVE
    endif

! check the snow depth - snow layers combined
    if ( ISNOW < -1 ) then
       ISNOW_OLD = ISNOW
       MSSI      = 1
       do I = ISNOW_OLD+1, 0
          if ( DZSNSO(I) < DZMIN(MSSI) ) then
             if ( I == ISNOW+1 ) then
                NEIBOR = I + 1
             else if ( I == 0 ) then
                NEIBOR = I - 1
             else
                NEIBOR = I + 1
                if ( (DZSNSO(I-1)+DZSNSO(I)) < (DZSNSO(I+1)+DZSNSO(I)) ) NEIBOR = I-1
             endif
             ! Node l and j are combined and stored as node j.
             if ( NEIBOR > I ) then
                J = NEIBOR
                L = I
             else
                J = I
                L = NEIBOR
             endif

             ! transfer value to variables passed to SnowLayerWaterCombo
             DZ    = DZSNSO(J) 
             WLIQ  = SNLIQ(J)
             WICE  = SNICE(J)
             T     = STC(J)
             DZ2   = DZSNSO(L)
             WLIQ2 = SNLIQ(L)
             WICE2 = SNICE(L)
             T2    = STC(L)

             call SnowLayerWaterCombo(noahmp) ! update combined snow water & temperature

             DZSNSO(J) = DZ
             SNLIQ(J)  = WLIQ
             SNICE(J)  = WICE
             STC(J)    = T
             ! value transfer completed

             ! Now shift all elements above this down one.
             if ( J-1 > ISNOW+1 ) then
                do K = J-1, ISNOW+2, -1
                   STC(K)    = STC(K-1)
                   SNICE(K)  = SNICE(K-1)
                   SNLIQ(K)  = SNLIQ(K-1)
                   DZSNSO(K) = DZSNSO(K-1)
                enddo
             endif
             ! Decrease the number of snow layers
             ISNOW = ISNOW + 1
             if ( ISNOW >= -1 ) EXIT
          else 
             ! The layer thickness is greater than the prescribed minimum value
             MSSI = MSSI + 1
          endif
       enddo
    endif

    end associate

  end subroutine SnowLayerCombine

end module SnowLayerCombineMod
