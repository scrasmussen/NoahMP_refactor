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
              RadSWDownRefHeight => noahmp%forcing%RadSWDownRefHeight,& ! in,    downward shortwave radiation (W/m2) at reference height
              T2M              => noahmp%energy%state%T2M            ,& ! in,    2-m air temperature (K)
              XLAI             => noahmp%energy%state%LAI            ,& ! in,    leaf area index, unadjusted for burying by snow
              I2PAR            => noahmp%biochem%param%I2PAR         ,& ! in,    Fraction of incoming solar radiation to photosynthetically active radiation
              TASSIM0          => noahmp%biochem%param%TASSIM0       ,& ! in,    Minimum temperature for CO2 assimulation [C]
              TASSIM1          => noahmp%biochem%param%TASSIM1       ,& ! in,    CO2 assimulation linearly increasing until temperature reaches T1 [C]
              TASSIM2          => noahmp%biochem%param%TASSIM2       ,& ! in,    CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
              Aref             => noahmp%biochem%param%AREF          ,& ! in,    reference maximum CO2 assimulation rate
              k                => noahmp%biochem%param%K             ,& ! in,    light extinction coefficient
              epsi             => noahmp%biochem%param%EPSI          ,& ! in,    initial light use efficiency
              PSNRF            => noahmp%biochem%param%PSNRF         ,& ! in,    CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
              PSNCROP          => noahmp%biochem%flux%PSNCROP         & ! out,   crop photosynthesis
             )
!------------------------------------------------------------------------

    ! initialize
    TC  = T2M - 273.15
    PAR = I2PAR * RadSWDownRefHeight * 0.0036  !w to MJ m-2

    ! compute Maximum CO2 assimulation rate g/co2/s
    if ( TC < TASSIM0 ) then
       Amax = 1.0e-10
    elseif ( (TC >= TASSIM0) .and. (TC < TASSIM1) ) then
       Amax = (TC - TASSIM0) * Aref / (TASSIM1 - TASSIM0)
    elseif ( (TC >= TASSIM1) .and. (TC < TASSIM2) ) then
       Amax = Aref
    else
       Amax= Aref - 0.2 * (T2M - TASSIM2)
    endif              
    Amax = max(amax, 0.01)

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

    I1 = k * PAR * exp(-k * L1)
    I2 = k * PAR * exp(-k * L2)
    I3 = k * PAR * exp(-k * L3)
    I1 = max(I1, 1.0e-10)
    I2 = max(I2, 1.0e-10)
    I3 = max(I3, 1.0e-10)

    A1 = Amax * (1 - exp(-epsi * I1 / Amax))
    A2 = Amax * (1 - exp(-epsi * I2 / Amax)) * 1.6
    A3 = Amax * (1 - exp(-epsi * I3 / Amax))

    ! compute photosynthesis rate
    if ( XLAI <= 0.05 ) then
       A = (A1 + A2 + A3) / 3.6 * 0.05
    elseif ( (XLAI > 0.05) .and. (XLAI <= 4.0) ) then
       A = (A1 + A2 + A3) / 3.6 * XLAI
    else
       A = (A1 + A2 + A3) / 3.6 * 4
    endif    
    A = A * PSNRF ! Attainable 
    PSNCROP = 6.313 * A   ! (1/44) * 1000000)/3600 = 6.313

    end associate

  end subroutine CropPhotosynthesis

end module CropPhotosynthesisMod
