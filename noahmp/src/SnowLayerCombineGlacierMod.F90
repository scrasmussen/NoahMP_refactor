module SnowLayerCombineGlacierMod

!!! Snowpack layer combination process over glacier
!!! Update snow ice, snow water, snow thickness, snow temperature

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowLayerWaterComboMod, only: SnowLayerWaterCombo

  implicit none

contains

  subroutine SnowLayerCombineGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMBINE_GLACIER
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
    !data DZMIN /0.025, 0.025, 0.1/         ! MB: change limit
    data DZMIN /0.045, 0.05, 0.2/

! --------------------------------------------------------------------
    associate(                                                        &
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! inout,  actual number of snow layers (negative)
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout,  snow depth [m]
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! inout,  snow water equivalent [mm]
              SnowIce           => noahmp%water%state%SnowIce            ,& ! inout,  snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater            ,& ! inout,  snow layer liquid water [mm]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout,  soil liquid moisture (m3/m3)
              SoilIce            => noahmp%water%state%SoilIce             ,& ! inout,  soil ice moisture (m3/m3)
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! inout,  thickness of snow/soil layers (m)
              PondSfcThinSnwComb        => noahmp%water%state%PondSfcThinSnwComb         ,& ! out,   surface ponding [mm] from liquid in thin snow layer combination
              PondSfcThinSnwTrans        => noahmp%water%state%PondSfcThinSnwTrans          & ! out,  surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
             )
! ----------------------------------------------------------------------

! check and combine small ice content layer
    ISNOW_OLD = NumSnowLayerNeg

    do J = ISNOW_OLD+1,0
       if ( SnowIce(J) <= 0.1 ) then
          if ( J /= 0 ) then
             SnowLiqWater(J+1)  = SnowLiqWater(J+1)  + SnowLiqWater(J)
             SnowIce(J+1)  = SnowIce(J+1)  + SnowIce(J)
             ThicknessSnowSoilLayer(J+1) = ThicknessSnowSoilLayer(J+1) + ThicknessSnowSoilLayer(J)
          else
             if ( ISNOW_OLD < -1 ) then    ! MB/KM: change to NumSnowLayerNeg
                SnowLiqWater(J-1)  = SnowLiqWater(J-1)  + SnowLiqWater(J)
                SnowIce(J-1)  = SnowIce(J-1)  + SnowIce(J)
                ThicknessSnowSoilLayer(J-1) = ThicknessSnowSoilLayer(J-1) + ThicknessSnowSoilLayer(J)
             else
                PondSfcThinSnwComb  = PondSfcThinSnwComb +SnowLiqWater(J)       ! NumSnowLayerNeg WILL GET SET TO ZERO BELOW; PondSfcThinSnwComb WILL GET 
                SnowWaterEquiv     = SnowIce(J)                 ! ADDED TO PONDING FROM PHASECHANGE PONDING SHOULD BE
                SnowDepth     = ThicknessSnowSoilLayer(J)                ! ZERO HERE BECAUSE IT WAS CALCULATED FOR THIN SNOW
                SnowLiqWater(J)  = 0.0
                SnowIce(J)  = 0.0
                ThicknessSnowSoilLayer(J) = 0.0
             endif ! if(ISNOW_OLD < -1)

             !SoilLiqWater(1) = SoilLiqWater(1) + SnowLiqWater(J)/(ThicknessSnowSoilLayer(1)*1000.0)
             !SoilIce(1) = SoilIce(1) + SnowIce(J)/(ThicknessSnowSoilLayer(1)*1000.0)
          endif ! if(J /= 0)

          ! shift all elements above this down by one.
          if ( (J > NumSnowLayerNeg+1) .and. (NumSnowLayerNeg < -1) ) then
             do I = J, NumSnowLayerNeg+2, -1
                STC(I)    = STC(I-1)
                SnowLiqWater(I)  = SnowLiqWater(I-1)
                SnowIce(I)  = SnowIce(I-1)
                ThicknessSnowSoilLayer(I) = ThicknessSnowSoilLayer(I-1)
             enddo
          endif
          NumSnowLayerNeg = NumSnowLayerNeg + 1

       endif ! if(SnowIce(J) <= 0.1)
    enddo ! do J

! to conserve water in case of too large surface sublimation
    if ( SoilIce(1) < 0.0) then
       SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
       SoilIce(1) = 0.0
    endif

    if ( NumSnowLayerNeg ==0 ) return   ! MB: get out if no longer multi-layer

    SnowWaterEquiv  = 0.0
    SnowDepth  = 0.0
    ZWICE  = 0.0
    ZWLIQ  = 0.0

    do J = NumSnowLayerNeg+1, 0
       SnowWaterEquiv = SnowWaterEquiv + SnowIce(J) + SnowLiqWater(J)
       SnowDepth = SnowDepth + ThicknessSnowSoilLayer(J)
       ZWICE = ZWICE + SnowIce(J)
       ZWLIQ = ZWLIQ + SnowLiqWater(J)
    enddo

! check the snow depth - all snow gone, the liquid water assumes ponding on soil surface.
    if ( (SnowDepth < 0.05) .and. (NumSnowLayerNeg < 0) ) then
    !if ( (SnowDepth < 0.025) .and. (NumSnowLayerNeg < 0) ) then ! MB: change limit
       NumSnowLayerNeg    = 0
       SnowWaterEquiv    = ZWICE
       PondSfcThinSnwTrans = ZWLIQ                ! LIMIT OF NumSnowLayerNeg < 0 MEANS INPUT PONDING
       if ( SnowWaterEquiv <= 0.0 ) SnowDepth = 0.0 ! SHOULD BE ZERO; SEE ABOVE
    endif

! check the snow depth - snow layers combined
    if ( NumSnowLayerNeg < -1 ) then
       ISNOW_OLD = NumSnowLayerNeg
       MSSI      = 1
       do I = ISNOW_OLD+1, 0
          if ( ThicknessSnowSoilLayer(I) < DZMIN(MSSI) ) then
             if ( I == NumSnowLayerNeg+1 ) then
                NEIBOR = I + 1
             else if ( I == 0 ) then
                NEIBOR = I - 1
             else
                NEIBOR = I + 1
                if ( (ThicknessSnowSoilLayer(I-1)+ThicknessSnowSoilLayer(I)) < (ThicknessSnowSoilLayer(I+1)+ThicknessSnowSoilLayer(I)) ) NEIBOR = I-1
             endif
             ! Node l and j are combined and stored as node j.
             if ( NEIBOR > I ) then
                J = NEIBOR
                L = I
             else
                J = I
                L = NEIBOR
             endif

             ! update combined snow water & temperature
             call SnowLayerWaterCombo(ThicknessSnowSoilLayer(J), SnowLiqWater(J), SnowIce(J), STC(J), &
                                      ThicknessSnowSoilLayer(L), SnowLiqWater(L), SnowIce(L), STC(L) )

             ! Now shift all elements above this down one.
             if ( (J-1) > (NumSnowLayerNeg+1) ) then
                do K = J-1, NumSnowLayerNeg+2, -1
                   STC(K)    = STC(K-1)
                   SnowIce(K)  = SnowIce(K-1)
                   SnowLiqWater(K)  = SnowLiqWater(K-1)
                   ThicknessSnowSoilLayer(K) = ThicknessSnowSoilLayer(K-1)
                enddo
             endif
             ! Decrease the number of snow layers
             NumSnowLayerNeg = NumSnowLayerNeg + 1
             if ( NumSnowLayerNeg >= -1 ) EXIT
          else 
             ! The layer thickness is greater than the prescribed minimum value
             MSSI = MSSI + 1
          endif
       enddo
    endif

    end associate

  end subroutine SnowLayerCombineGlacier

end module SnowLayerCombineGlacierMod
