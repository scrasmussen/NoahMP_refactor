module BiochemNatureVegMainMod

!!! Main Biogeochemistry module for dynamic natural vegetation (as opposed to cropland)
!!! currently only include carbon processes (RE Dickinson et al.(1998) and Guo-Yue Niu(2004))

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod 
  use CarbonFluxNatureVegMod,  only : CarbonFluxNatureVeg
    
  implicit none
    
contains
 
  subroutine BiochemNatureVegMain(noahmp)
    
! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CARBON
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------
    
    implicit none
    
    type(noahmp_type), intent(inout) :: noahmp
    
! local variables
    integer                          :: J      ! loop index
    real(kind=kind_noahmp)           :: DB     ! thickness of canopy buried by snow (m)

!------------------------------------------------------------------------
    associate(                                                        &
              VEGTYP           => noahmp%config%domain%VEGTYP        ,& ! in,    dynamic vegetation option
              ZSOIL            => noahmp%config%domain%ZSOIL         ,& ! in,    depth of layer-bottom from soil surface
              DZSNSO           => noahmp%config%domain%DZSNSO        ,& ! in,    snow/soil layer thickness [m]
              ISWATER          => noahmp%config%domain%ISWATER       ,& ! in,    water point flag
              ISICE            => noahmp%config%domain%ISICE         ,& ! in,    land ice flag
              ISBARREN         => noahmp%config%domain%ISBARREN      ,& ! in,    bare soil flag
              URBAN_FLAG       => noahmp%config%domain%URBAN_FLAG    ,& ! in,    urban point flag
              NROOT            => noahmp%water%param%NROOT           ,& ! in,    number of soil layers with root present
              SMCMAX           => noahmp%water%param%SMCMAX          ,& ! in,    saturated value of soil moisture [m3/m3]
              SMC              => noahmp%water%state%SMC             ,& ! in,    soil moisture (ice + liq.) [m3/m3]
              BTRAN            => noahmp%water%state%BTRAN           ,& ! in,    soil water transpiration factor (0 to 1)
              SLA              => noahmp%biochem%param%SLA           ,& ! in,    single-side leaf area per Kg [m2/kg]
              LFMASS           => noahmp%biochem%state%LFMASS        ,& ! inout, leaf mass [g/m2]
              RTMASS           => noahmp%biochem%state%RTMASS        ,& ! inout, mass of fine roots [g/m2]
              STMASS           => noahmp%biochem%state%STMASS        ,& ! inout, stem mass [g/m2]
              WOOD             => noahmp%biochem%state%WOOD          ,& ! inout, mass of wood (incl. woody roots) [g/m2]
              STBLCP           => noahmp%biochem%state%STBLCP        ,& ! inout, stable carbon in deep soil [g/m2]
              FASTCP           => noahmp%biochem%state%FASTCP        ,& ! inout, short-lived carbon in shallow soil [g/m2]
              GPP              => noahmp%biochem%flux%GPP            ,& ! out,   net instantaneous assimilation [g/m2/s C]
              NPP              => noahmp%biochem%flux%NPP            ,& ! out,   net primary productivity [g/m2/s C]
              NEE              => noahmp%biochem%flux%NEE            ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              AUTORS           => noahmp%biochem%flux%AUTORS         ,& ! out,   net ecosystem respiration [g/m2/s C]
              HETERS           => noahmp%biochem%flux%HETERS         ,& ! out,   organic respiration [g/m2/s C]
              TOTSC            => noahmp%biochem%state%TOTSC         ,& ! out,   total soil carbon [g/m2 C]
              TOTLB            => noahmp%biochem%state%TOTLB         ,& ! out,   total living carbon ([g/m2 C]
              XLAI             => noahmp%energy%state%LAI            ,& ! out,   leaf area index [-]
              XSAI             => noahmp%energy%state%SAI            ,& ! out,   stem area index [-]
              WROOT            => noahmp%water%state%WROOT           ,& ! out,   root zone soil water [-]
              WSTRES           => noahmp%water%state%WSTRES          ,& ! out,   water stress coeficient [-]  (1. for wilting)
              LAPM             => noahmp%biochem%state%LAPM           & ! out,   leaf area per unit mass [m2/g]
             )
!-----------------------------------------------------------------------

    ! no biogeochemistry in non-vegetated points
    if ( (VEGTYP == ISWATER) .or. (VEGTYP == ISBARREN) .or. &
         (VEGTYP == ISICE  ) .or. (URBAN_FLAG .eqv. .true.) ) then
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
       return
    endif

    ! start biogeochemistry process
    LAPM = SLA / 1000.0   ! m2/kg -> m2/g

    ! water stress
    WSTRES = 1.0 - BTRAN
    WROOT  = 0.0
    do J = 1, NROOT
       WROOT = WROOT + SMC(J) / SMCMAX(J) * DZSNSO(J) / (-ZSOIL(NROOT))
    enddo

    ! start carbon process
    call CarbonFluxNatureVeg(noahmp)

    end associate

  end subroutine BiochemNatureVegMain

end module BiochemNatureVegMainMod
