module BiochemCropMainMod

!!! Main Biogeochemistry module for dynamic crop (as opposed to natural vegetation)
!!! currently only include carbon processes (RE Dickinson et al.(1998) and Guo-Yue Niu(2004))
 
  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use CarbonFluxCropMod,     only : CarbonFluxCrop
  use CropGrowDegreeDayMod,  only : CropGrowDegreeDay
  use CropPhotosynthesisMod, only : CropPhotosynthesis
        
  implicit none
        
contains
     
  subroutine BiochemCropMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CARBON_CROP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Modified by Xing Liu, 2014
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------
        
    implicit none
        
    type(noahmp_type), intent(inout) :: noahmp
    
! local variables
    integer                          :: J      ! loop index
    real(kind=kind_noahmp)           :: DB     ! thickness of canopy buried by snow (m) 
    
!-------------------------------------------------------------------------
    associate(                                                        &
              VegType          => noahmp%config%domain%VegType       ,& ! in,    vegetation type
              DepthSoilLayer            => noahmp%config%domain%DepthSoilLayer         ,& ! in,    depth [m] of layer-bottom from soil surface
              ThicknessSnowSoilLayer           => noahmp%config%domain%ThicknessSnowSoilLayer        ,& ! in,    snow/soil layer thickness [m]
              IndexWaterPoint          => noahmp%config%domain%IndexWaterPoint       ,& ! in,    water point flag
              IndexIcePoint            => noahmp%config%domain%IndexIcePoint         ,& ! in,    land ice flag
              IndexBarrenPoint         => noahmp%config%domain%IndexBarrenPoint      ,& ! in,    bare soil flag
              FlagUrban       => noahmp%config%domain%FlagUrban    ,& ! in,    urban point flag
              NROOT            => noahmp%water%param%NROOT           ,& ! in,    number of soil layers with root present
              SMCMAX           => noahmp%water%param%SMCMAX          ,& ! in,    saturated value of soil moisture [m3/m3]
              SMC              => noahmp%water%state%SMC             ,& ! in,    soil moisture (ice + liq.) [m3/m3]
              BTRAN            => noahmp%water%state%BTRAN           ,& ! in,    soil water transpiration factor (0 to 1)
              LFMASS           => noahmp%biochem%state%LFMASS        ,& ! inout, leaf mass [g/m2]
              RTMASS           => noahmp%biochem%state%RTMASS        ,& ! inout, mass of fine roots [g/m2]
              STMASS           => noahmp%biochem%state%STMASS        ,& ! inout, stem mass [g/m2]
              WOOD             => noahmp%biochem%state%WOOD          ,& ! inout, mass of wood (incl. woody roots) [g/m2]
              STBLCP           => noahmp%biochem%state%STBLCP        ,& ! inout, stable carbon in deep soil [g/m2]
              FASTCP           => noahmp%biochem%state%FASTCP        ,& ! inout, short-lived carbon in shallow soil [g/m2]
              XLAI             => noahmp%energy%state%LAI            ,& ! inout, leaf area index [-]
              XSAI             => noahmp%energy%state%SAI            ,& ! inout, stem area index [-]
              GPP              => noahmp%biochem%flux%GPP            ,& ! out,   net instantaneous assimilation [g/m2/s C]
              NPP              => noahmp%biochem%flux%NPP            ,& ! out,   net primary productivity [g/m2/s C]
              NEE              => noahmp%biochem%flux%NEE            ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              AUTORS           => noahmp%biochem%flux%AUTORS         ,& ! out,   net ecosystem respiration [g/m2/s C]
              HETERS           => noahmp%biochem%flux%HETERS         ,& ! out,   organic respiration [g/m2/s C]
              TOTSC            => noahmp%biochem%state%TOTSC         ,& ! out,   total soil carbon [g/m2 C]
              TOTLB            => noahmp%biochem%state%TOTLB         ,& ! out,   total living carbon ([g/m2 C]
              GRAIN            => noahmp%biochem%state%GRAIN         ,& ! out,   mass of GRAIN [g/m2] 
              IPA              => noahmp%biochem%state%IPA           ,& ! out,   Planting index
              IHA              => noahmp%biochem%state%IHA           ,& ! out,   Havest index (0=on,1=off)
              WROOT            => noahmp%water%state%WROOT           ,& ! out,   root zone soil water [-]
              WSTRES           => noahmp%water%state%WSTRES           & ! out,   water stress coeficient [-]  (1. for wilting)
             ) 
!------------------------------------------------------------------------

    ! initialize
    NEE = 0.0
    NPP = 0.0
    GPP = 0.0

    ! no biogeochemistry in non-vegetated points
    if ( (VegType == IndexWaterPoint) .or. (VegType == IndexBarrenPoint) .or. &
         (VegType == IndexIcePoint  ) .or. (FlagUrban .eqv. .true.) ) then
       XLAI   = 0.0
       XSAI   = 0.0
       GPP    = 0.0
       NPP    = 0.0
       NEE    = 0.0
       AUTORS = 0.0
       HETERS = 0.0
       TOTSC  = 0.0
       TOTLB  = 0.0
       LFMASS = 0.0
       RTMASS = 0.0
       STMASS = 0.0
       WOOD   = 0.0
       STBLCP = 0.0
       FASTCP = 0.0
       GRAIN  = 0.0
       return
    endif
    
    ! start biogeochemistry process

    ! water stress
    WSTRES = 1.0 - BTRAN
    WROOT  = 0.0
    do J = 1, NROOT
       WROOT = WROOT + SMC(J) / SMCMAX(J) * ThicknessSnowSoilLayer(J) / (-DepthSoilLayer(NROOT))
    enddo

    ! start crop carbon process
    call CropPhotosynthesis(noahmp)
    call CropGrowDegreeDay(noahmp)
    call CarbonFluxCrop(noahmp)
    
    end associate
    
  end subroutine BiochemCropMain
    
end module BiochemCropMainMod
