module PhotosynthesisCropMod

    !!! Estimate Crop Photosynthesis
    use Machine, only : kind_noahmp
    use NoahmpVarType
        
    implicit none

contains

    subroutine PhotosynthesisCrop (noahmp)

        ! ------------------------ Code history -----------------------------------
        ! Original Noah-MP subroutine: PSN_CROP  
        ! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
        ! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
        ! -------------------------------------------------------------------------
        
        implicit none
       
        type(noahmp_type), intent(inout) :: noahmp

        !local
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
        real(kind=kind_noahmp)           :: TC

        associate(                                            &
                 T2M       =>  noahmp%energy%state%T2M       ,&
                 SOLDN     =>  noahmp%forcing%SOLDN          ,&
                 XLAI      =>  noahmp%energy%state%LAI       ,&
                 PSNCROP   =>  noahmp%biochem%flux%PSNCROP   ,&
                 I2PAR     =>  noahmp%biochem%param%I2PAR    ,&
                 TASSIM0   =>  noahmp%biochem%param%TASSIM0  ,& 
                 TASSIM1   =>  noahmp%biochem%param%TASSIM1  ,&
                 TASSIM2   =>  noahmp%biochem%param%TASSIM2  ,&
                 Aref      =>  noahmp%biochem%param%AREF     ,&
                 k         =>  noahmp%biochem%param%K        ,&
                 epsi      =>  noahmp%biochem%param%EPSI     ,&
                 PSNRF     =>  noahmp%biochem%param%PSNRF     &
                 )

        TC = T2M - 273.15
        PAR = I2PAR * SOLDN * 0.0036  !w to MJ m-2
        
        IF(TC < parameters%TASSIM0) THEN
           Amax = 1E-10
        ELSEIF(TC >= TASSIM0 .and. TC < TASSIM1) THEN
           Amax = (TC - TASSIM0) * Aref / (TASSIM1 - TASSIM0)
        ELSEIF(TC >= TASSIM1 .and. TC < TASSIM2) THEN
           Amax = Aref
        ELSE
           Amax= Aref - 0.2 * (T2M - TASSIM2)
        ENDIF 
                 
        Amax = max(amax,0.01)

        IF(XLAI <= 0.05) THEN
           L1 = 0.1127 * 0.05   !use initial LAI(0.05), avoid error
           L2 = 0.5    * 0.05
           L3 = 0.8873 * 0.05
        ELSE
           L1 = 0.1127 * XLAI
           L2 = 0.5    * XLAI
           L3 = 0.8873 * XLAI
        END IF
        I1 = k * PAR * exp(-k * L1)
        I2 = k * PAR * exp(-k * L2)
        I3 = k * PAR * exp(-k * L3)
        I1 = max(I1,1E-10)
        I2 = max(I2,1E-10)
        I3 = max(I3,1E-10)
        A1 = Amax * (1 - exp(-epsi * I1 / Amax))
        A2 = Amax * (1 - exp(-epsi * I2 / Amax)) * 1.6
        A3 = Amax * (1 - exp(-epsi * I3 / Amax))
        IF (XLAI <= 0.05) THEN
            A  = (A1+A2+A3) / 3.6 * 0.05
        ELSEIF (XLAI > 0.05 .and. XLAI <= 4.0) THEN
            A  = (A1+A2+A3) / 3.6 * XLAI
        ELSE
            A = (A1+A2+A3) / 3.6 * 4
        END IF
        
        A = A * PSNRF ! Attainable 
        
        PSNCROP = 6.313 * A   ! (1/44) * 1000000)/3600 = 6.313

        end associate

    end subroutine PhotosynthesisCrop

end module PhotosynthesisCropMod