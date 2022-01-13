module BiochemCropMainMod

!!! Main Carbon module for dynamic vegetation
        
    use Machine, only : kind_noahmp
    use NoahmpVarType
    use CarbonAssimilationFluxCropMod
    use GrowingDegreeDaysCropMod
    use PhotosynthesisCropMod 
        
    implicit none
        
contains
     
    subroutine BiochemCropMain (noahmp)
        
    ! ------------------------ Code history -----------------------------------
    ! Original Noah-MP subroutine: CARBON
    ! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
    ! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
    ! -------------------------------------------------------------------------
        
      implicit none
        
        type(noahmp_type), intent(inout) :: noahmp
        
        ! local variable
        
        real(kind=kind_noahmp)           :: DB     !thickness of canopy buried by snow (m)
        integer                          :: J
    
       ! --------------------------------------------------------------------
    
        associate (                                               &
                  VEGTYP     =>  noahmp%config%domain%VEGTYP     ,&
                  ZSOIL      =>  noahmp%config%domain%ZSOIL      ,& 
                  DZSNSO     =>  noahmp%config%domain%DZSNSO     ,& 
                  SMC        =>  noahmp%water%state%SMC          ,&
                  BTRAN      =>  noahmp%water%state%BTRAN        ,& 
                  LFMASS     =>  noahmp%biochem%state%LFMASS     ,& 
                  RTMASS     =>  noahmp%biochem%state%RTMASS     ,& 
                  STMASS     =>  noahmp%biochem%state%STMASS     ,& 
                  WOOD       =>  noahmp%biochem%state%WOOD       ,& 
                  STBLCP     =>  noahmp%biochem%state%STBLCP     ,& 
                  FASTCP     =>  noahmp%biochem%state%FASTCP     ,& 
                  GPP        =>  noahmp%biochem%flux%GPP         ,&  
                  NPP        =>  noahmp%biochem%flux%NPP         ,& 
                  NEE        =>  noahmp%biochem%flux%NEE         ,& 
                  AUTORS     =>  noahmp%biochem%flux%AUTORS      ,& 
                  HETERS     =>  noahmp%biochem%flux%HETERS      ,& 
                  TOTSC      =>  noahmp%biochem%state%TOTSC      ,& 
                  TOTLB      =>  noahmp%biochem%state%TOTLB      ,& 
                  XLAI       =>  noahmp%energy%state%LAI         ,& 
                  XSAI       =>  noahmp%energy%state%SAI         ,& 
                  WROOT      =>  noahmp%water%state%WROOT        ,& 
                  WSTRES     =>  noahmp%water%state%WSTRES       ,&    
                  GRAIN      =>  noahmp%biochem%state%GRAIN      ,& 
                  IPA        =>  noahmp%biochem%state%IPA        ,&
                  IHA        =>  noahmp%biochem%state%IHA        ,&
                  iswater    =>  noahmp%config%domain%ISWATER    ,& 
                  ISICE      =>  noahmp%config%domain%ISICE      ,& 
                  ISBARREN   =>  noahmp%config%domain%ISBARREN   ,& 
                  urban_flag =>  noahmp%config%domain%URBAN_FLAG ,& 
                  NROOT      =>  noahmp%water%param%NROOT        ,& 
                  SMCMAX     =>  noahmp%water%param%SMCMAX        & 
                  )
    
       ! ------------------------------------------------------------------------------------------
    
        if ( ( VEGTYP == iswater ) .or. ( VEGTYP == ISBARREN ) .or. &
             ( VEGTYP == ISICE   ) .or. ( urban_flag) ) then
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
            RETURN
        end if
    
        ! water stress
        WSTRES  = 1.0 - BTRAN
        WROOT   = 0.0
    
        do J = 1,NROOT
            WROOT = WROOT + SMC(J)/SMCMAX(J) *  DZSNSO(J) / (-ZSOIL(NROOT))
        end do

        call PhotosynthesisCrop (noahmp)

        call GrowingDegreeDaysCrop (noahmp)
    
        call CarbonAssimilationFluxCrop (noahmp)
    
        end associate
    
    end subroutine BiochemCropMain
    
end module BiochemCropMainMod
    
