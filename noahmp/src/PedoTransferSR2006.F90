module PedoTransferSR2006

!!! Compute soil water infiltration based on different soil composition

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

  subroutine PedoTransfer_SR2006(NoahmpIO, noahmp, sand, clay, orgm)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PEDOTRANSFER_SR2006
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (July 2022)
! -------------------------------------------------------------------------

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp

    real(kind=kind_noahmp), dimension(1:NoahmpIO%nsoil) &
                                        , intent(inout)     :: SAND
    real(kind=kind_noahmp), dimension(1:NoahmpIO%nsoil) &
                                        , intent(inout)     :: CLAY
    real(kind=kind_noahmp), dimension(1:NoahmpIO%nsoil) &
                                        , intent(inout)     :: ORGM

    ! local
    integer                                                 :: k
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: theta_1500t
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: theta_1500
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: theta_33t
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: theta_33
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: theta_s33t
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: theta_s33
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: psi_et
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: psi_e                                 

    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: smcmax 
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: smcref 
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: smcwlt 
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: smcdry 
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: bexp   
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: psisat 
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: dksat  
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: dwsat  
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil )   :: quartz 

    associate(                                                               & 
              sr2006_theta_1500t_a  =>  NoahmpIO%sr2006_theta_1500t_a_TABLE ,& 
              sr2006_theta_1500t_b  =>  NoahmpIO%sr2006_theta_1500t_b_TABLE ,&
              sr2006_theta_1500t_c  =>  NoahmpIO%sr2006_theta_1500t_c_TABLE ,&
              sr2006_theta_1500t_d  =>  NoahmpIO%sr2006_theta_1500t_d_TABLE ,&
              sr2006_theta_1500t_e  =>  NoahmpIO%sr2006_theta_1500t_e_TABLE ,&
              sr2006_theta_1500t_f  =>  NoahmpIO%sr2006_theta_1500t_f_TABLE ,&
              sr2006_theta_1500t_g  =>  NoahmpIO%sr2006_theta_1500t_g_TABLE ,&
              sr2006_theta_1500_a   =>  NoahmpIO%sr2006_theta_1500_a_TABLE  ,&
              sr2006_theta_1500_b   =>  NoahmpIO%sr2006_theta_1500_b_TABLE  ,&
              sr2006_theta_33t_a    =>  NoahmpIO%sr2006_theta_33t_a_TABLE   ,&
              sr2006_theta_33t_b    =>  NoahmpIO%sr2006_theta_33t_b_TABLE   ,&
              sr2006_theta_33t_c    =>  NoahmpIO%sr2006_theta_33t_c_TABLE   ,&
              sr2006_theta_33t_d    =>  NoahmpIO%sr2006_theta_33t_d_TABLE   ,&
              sr2006_theta_33t_e    =>  NoahmpIO%sr2006_theta_33t_e_TABLE   ,&
              sr2006_theta_33t_f    =>  NoahmpIO%sr2006_theta_33t_f_TABLE   ,&
              sr2006_theta_33t_g    =>  NoahmpIO%sr2006_theta_33t_g_TABLE   ,&
              sr2006_theta_33_a     =>  NoahmpIO%sr2006_theta_33_a_TABLE    ,&
              sr2006_theta_33_b     =>  NoahmpIO%sr2006_theta_33_b_TABLE    ,&
              sr2006_theta_33_c     =>  NoahmpIO%sr2006_theta_33_c_TABLE    ,&
              sr2006_theta_s33t_a   =>  NoahmpIO%sr2006_theta_s33t_a_TABLE  ,&
              sr2006_theta_s33t_b   =>  NoahmpIO%sr2006_theta_s33t_b_TABLE  ,&
              sr2006_theta_s33t_c   =>  NoahmpIO%sr2006_theta_s33t_c_TABLE  ,&
              sr2006_theta_s33t_d   =>  NoahmpIO%sr2006_theta_s33t_d_TABLE  ,&
              sr2006_theta_s33t_e   =>  NoahmpIO%sr2006_theta_s33t_e_TABLE  ,&
              sr2006_theta_s33t_f   =>  NoahmpIO%sr2006_theta_s33t_f_TABLE  ,&
              sr2006_theta_s33t_g   =>  NoahmpIO%sr2006_theta_s33t_g_TABLE  ,&
              sr2006_theta_s33_a    =>  NoahmpIO%sr2006_theta_s33_a_TABLE   ,&
              sr2006_theta_s33_b    =>  NoahmpIO%sr2006_theta_s33_b_TABLE   ,&
              sr2006_psi_et_a       =>  NoahmpIO%sr2006_psi_et_a_TABLE      ,&
              sr2006_psi_et_b       =>  NoahmpIO%sr2006_psi_et_b_TABLE      ,&
              sr2006_psi_et_c       =>  NoahmpIO%sr2006_psi_et_c_TABLE      ,&
              sr2006_psi_et_d       =>  NoahmpIO%sr2006_psi_et_d_TABLE      ,&
              sr2006_psi_et_e       =>  NoahmpIO%sr2006_psi_et_e_TABLE      ,&
              sr2006_psi_et_f       =>  NoahmpIO%sr2006_psi_et_f_TABLE      ,&
              sr2006_psi_et_g       =>  NoahmpIO%sr2006_psi_et_g_TABLE      ,&
              sr2006_psi_e_a        =>  NoahmpIO%sr2006_psi_e_a_TABLE       ,&
              sr2006_psi_e_b        =>  NoahmpIO%sr2006_psi_e_b_TABLE       ,&
              sr2006_psi_e_c        =>  NoahmpIO%sr2006_psi_e_c_TABLE       ,&
              sr2006_smcmax_a       =>  NoahmpIO%sr2006_smcmax_a_TABLE      ,&
              sr2006_smcmax_b       =>  NoahmpIO%sr2006_smcmax_b_TABLE       &
             ) 

    !-------------------------------------------------------------------------

    smcmax  = 0.0
    smcref  = 0.0
    smcwlt  = 0.0
    smcdry  = 0.0
    bexp    = 0.0
    psisat  = 0.0
    dksat   = 0.0
    dwsat   = 0.0
    quartz  = 0.0
    
    do k = 1,4
      if(sand(k) <= 0 .or. clay(k) <= 0) then
         sand(k) = 0.41
         clay(k) = 0.18
      end if
      if(orgm(k) <= 0 ) orgm(k) = 0.0
    end do
        
    theta_1500t =   sr2006_theta_1500t_a*sand       &
                  + sr2006_theta_1500t_b*clay       &
                  + sr2006_theta_1500t_c*orgm       &
                  + sr2006_theta_1500t_d*sand*orgm  &
                  + sr2006_theta_1500t_e*clay*orgm  &
                  + sr2006_theta_1500t_f*sand*clay  &
                  + sr2006_theta_1500t_g

    theta_1500  =   theta_1500t                      &
                  + sr2006_theta_1500_a*theta_1500t  &
                  + sr2006_theta_1500_b

    theta_33t   =   sr2006_theta_33t_a*sand       &
                  + sr2006_theta_33t_b*clay       &
                  + sr2006_theta_33t_c*orgm       &
                  + sr2006_theta_33t_d*sand*orgm  &
                  + sr2006_theta_33t_e*clay*orgm  &
                  + sr2006_theta_33t_f*sand*clay  &
                  + sr2006_theta_33t_g

    theta_33    =   theta_33t                              &
                  + sr2006_theta_33_a*theta_33t*theta_33t  &
                  + sr2006_theta_33_b*theta_33t            &
                  + sr2006_theta_33_c

    theta_s33t  =   sr2006_theta_s33t_a*sand      &
                  + sr2006_theta_s33t_b*clay      &
                  + sr2006_theta_s33t_c*orgm      &
                  + sr2006_theta_s33t_d*sand*orgm &
                  + sr2006_theta_s33t_e*clay*orgm &
                  + sr2006_theta_s33t_f*sand*clay &
                  + sr2006_theta_s33t_g

    theta_s33   = theta_s33t                       &
                  + sr2006_theta_s33_a*theta_s33t  &
                  + sr2006_theta_s33_b

    psi_et      =   sr2006_psi_et_a*sand           &
                  + sr2006_psi_et_b*clay           &
                  + sr2006_psi_et_c*theta_s33      &
                  + sr2006_psi_et_d*sand*theta_s33 &
                  + sr2006_psi_et_e*clay*theta_s33 &
                  + sr2006_psi_et_f*sand*clay      &
                  + sr2006_psi_et_g
 
    psi_e       =   psi_et                        &
                  + sr2006_psi_e_a*psi_et*psi_et  &
                  + sr2006_psi_e_b*psi_et         &
                  + sr2006_psi_e_c
    
    smcwlt = theta_1500
    smcref = theta_33
    smcmax = theta_33                     &
             + theta_s33                  &
             + sr2006_smcmax_a*sand &
             + sr2006_smcmax_b

    bexp   = 3.816712826 / (log(theta_33) - log(theta_1500) )
    psisat = psi_e
    dksat  = 1930.0 * (smcmax - theta_33) ** (3.0 - 1.0/bexp)
    quartz = sand
    
! Units conversion
    
    psisat = max(0.1, psisat)               ! arbitrarily impose a limit of 0.1kpa
    psisat = 0.101997 * psisat              ! convert kpa to m
    dksat  = dksat / 3600000.0              ! convert mm/h to m/s
    dwsat  = dksat * psisat * bexp / smcmax ! units should be m*m/s
    smcdry = smcwlt
  
! Introducing somewhat arbitrary limits (based on NoahmpTable soil) to prevent bad things
  
    smcmax = max(0.32 ,min(smcmax,  0.50 ))
    smcref = max(0.17 ,min(smcref,smcmax ))
    smcwlt = max(0.01 ,min(smcwlt,smcref ))
    smcdry = max(0.01 ,min(smcdry,smcref ))
    bexp   = max(2.50 ,min(bexp,    12.0 ))
    psisat = max(0.03 ,min(psisat,  1.00 ))
    dksat  = max(5.e-7,min(dksat,   1.e-5))
    dwsat  = max(1.e-6,min(dwsat,   3.e-5))
    quartz = max(0.05 ,min(quartz,  0.95 ))

    noahmp%water%param%SMCWLT = smcwlt  
    noahmp%water%param%SMCREF = smcref    
    noahmp%water%param%SMCMAX = smcmax    
    noahmp%water%param%SMCDRY = smcdry    
    noahmp%water%param%BEXP   = bexp    
    noahmp%water%param%PSISAT = psisat    
    noahmp%water%param%DKSAT  = dksat     
    noahmp%water%param%QUARTZ = quartz     

    end associate

  end subroutine PedoTransfer_SR2006

end module PedoTransferSR2006
