module CropPhotosynthesisMod

!!! Compute crop photosynthesis

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CropPhotosynthesis(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PSN_CROP  
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------
        
    implicit none
       
    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: PAR      ! photosynthetically active radiation (w/m2) 1 W m-2 = 0.0864 MJ m-2 day-1
    real(kind=kind_noahmp)           :: Amax     ! Maximum CO2 assimulation rate g/co2/s  
    real(kind=kind_noahmp)           :: L1       ! Three Gaussian method
    real(kind=kind_noahmp)           :: L2       ! Three Gaussian method
    real(kind=kind_noahmp)           :: L3       ! Three Gaussian method
    real(kind=kind_noahmp)           :: I1       ! Three Gaussian method
    real(kind=kind_noahmp)           :: I2       ! Three Gaussian method
    real(kind=kind_noahmp)           :: I3       ! Three Gaussian method
    real(kind=kind_noahmp)           :: A1       ! Three Gaussian method
    real(kind=kind_noahmp)           :: A2       ! Three Gaussian method
    real(kind=kind_noahmp)           :: A3       ! Three Gaussian method
    real(kind=kind_noahmp)           :: A        ! CO2 Assimulation 
    real(kind=kind_noahmp)           :: TC       ! temperature degC

!------------------------------------------------------------------------
    associate(                                                        &
              RadSwDownRefHeight => noahmp%forcing%RadSwDownRefHeight,& ! in,    downward shortwave radiation (W/m2) at reference height
              T2M              => noahmp%energy%state%T2M            ,& ! in,    2-m air temperature (K)
              XLAI             => noahmp%energy%state%LAI            ,& ! in,    leaf area index, unadjusted for burying by snow
              PhotosynRadFrac            => noahmp%biochem%param%PhotosynRadFrac         ,& ! in,    Fraction of incoming solar radiation to photosynthetically active radiation
              TempMinCarbonAssim          => noahmp%biochem%param%TempMinCarbonAssim       ,& ! in,    Minimum temperature for CO2 assimulation [C]
              TempMaxCarbonAssim          => noahmp%biochem%param%TempMaxCarbonAssim       ,& ! in,    CO2 assimulation linearly increasing until reaching this temperature [C]
              TempMaxCarbonAssimMax          => noahmp%biochem%param%TempMaxCarbonAssimMax       ,& ! in,    CO2 assmilation rate remain at CarbonAssimRefMax until reaching this temperature [C]
              CarbonAssimRefMax             => noahmp%biochem%param%CarbonAssimRefMax          ,& ! in,    reference maximum CO2 assimulation rate
              LightExtCoeff    => noahmp%biochem%param%LightExtCoeff  ,& ! in,    light extinction coefficient
              LighUseEfficiency             => noahmp%biochem%param%LighUseEfficiency          ,& ! in,    initial light use efficiency
              CarbonAssimReducFac            => noahmp%biochem%param%CarbonAssimReducFac         ,& ! in,    CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
              PhotosynCrop          => noahmp%biochem%flux%PhotosynCrop         & ! out,   crop photosynthesis [umol co2/m2/s]
             )
!------------------------------------------------------------------------

    ! initialize
    TC  = T2M - 273.15
    PAR = PhotosynRadFrac * RadSwDownRefHeight * 0.0036  !w to MJ m-2

    ! compute Maximum CO2 assimulation rate g/co2/s
    if ( TC < TempMinCarbonAssim ) then
       Amax = 1.0e-10
    elseif ( (TC >= TempMinCarbonAssim) .and. (TC < TempMaxCarbonAssim) ) then
       Amax = (TC - TempMinCarbonAssim) * CarbonAssimRefMax / (TempMaxCarbonAssim - TempMinCarbonAssim)
    elseif ( (TC >= TempMaxCarbonAssim) .and. (TC < TempMaxCarbonAssimMax) ) then
       Amax = CarbonAssimRefMax
    else
       Amax= CarbonAssimRefMax - 0.2 * (T2M - TempMaxCarbonAssimMax)
    endif              
    Amax = max(Amax, 0.01)

    ! compute coefficients
    if ( XLAI <= 0.05 ) then
       L1 = 0.1127 * 0.05   !use initial LAI(0.05), avoid error
       L2 = 0.5    * 0.05
       L3 = 0.8873 * 0.05
    else
       L1 = 0.1127 * XLAI
       L2 = 0.5    * XLAI
       L3 = 0.8873 * XLAI
    endif

    I1 = LightExtCoeff * PAR * exp(-LightExtCoeff * L1)
    I2 = LightExtCoeff * PAR * exp(-LightExtCoeff * L2)
    I3 = LightExtCoeff * PAR * exp(-LightExtCoeff * L3)
    I1 = max(I1, 1.0e-10)
    I2 = max(I2, 1.0e-10)
    I3 = max(I3, 1.0e-10)

    A1 = Amax * (1 - exp(-LighUseEfficiency * I1 / Amax))
    A2 = Amax * (1 - exp(-LighUseEfficiency * I2 / Amax)) * 1.6
    A3 = Amax * (1 - exp(-LighUseEfficiency * I3 / Amax))

    ! compute photosynthesis rate
    if ( XLAI <= 0.05 ) then
       A = (A1 + A2 + A3) / 3.6 * 0.05
    elseif ( (XLAI > 0.05) .and. (XLAI <= 4.0) ) then
       A = (A1 + A2 + A3) / 3.6 * XLAI
    else
       A = (A1 + A2 + A3) / 3.6 * 4
    endif    
    A = A * CarbonAssimReducFac ! Attainable 
    PhotosynCrop = 6.313 * A   ! (1/44) * 1000000)/3600 = 6.313

    end associate

  end subroutine CropPhotosynthesis

end module CropPhotosynthesisMod
