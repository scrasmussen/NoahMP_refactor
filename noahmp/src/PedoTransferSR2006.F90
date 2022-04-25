module PedoTransferSR2006

!!! Main water module including all water relevant processes
!!! canopy water -> snowpack water -> soil water -> ground water

  use Machine, only : kind_noahmp
  use InputVarType
  use NoahmpVarType

  implicit none

contains

  subroutine PedoTransfer_SR2006(input, noahmp, SOILTYP, sand, clay, orgm)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PEDOTRANSFER_SR2006
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

    integer,                dimension(1:input%nsoil)     :: SOILTYP
    real(kind=kind_noahmp), dimension( 1:input%nsoil ) &
                                         , intent(inout) :: SAND
    real(kind=kind_noahmp), dimension( 1:input%nsoil ) &
                                         , intent(inout) :: CLAY
    real(kind=kind_noahmp), dimension( 1:input%nsoil ) &
                                         , intent(inout) :: ORGM

    ! local
    integer                          :: k
    real, dimension( 1:input%nsoil ) :: theta_1500t
    real, dimension( 1:input%nsoil ) :: theta_1500
    real, dimension( 1:input%nsoil ) :: theta_33t
    real, dimension( 1:input%nsoil ) :: theta_33
    real, dimension( 1:input%nsoil ) :: theta_s33t
    real, dimension( 1:input%nsoil ) :: theta_s33
    real, dimension( 1:input%nsoil ) :: psi_et
    real, dimension( 1:input%nsoil ) :: psi_e                                 

    associate(    
              sr2006_theta_1500t_a  =>  input%sr2006_theta_1500t_a_TABLE ,& 
              sr2006_theta_1500t_b  =>  input%sr2006_theta_1500t_b_TABLE ,&
              sr2006_theta_1500t_c  =>  input%sr2006_theta_1500t_c_TABLE ,&
              sr2006_theta_1500t_d  =>  input%sr2006_theta_1500t_d_TABLE ,&
              sr2006_theta_1500t_e  =>  input%sr2006_theta_1500t_e_TABLE ,&
              sr2006_theta_1500t_f  =>  input%sr2006_theta_1500t_f_TABLE ,&
              sr2006_theta_1500t_g  =>  input%sr2006_theta_1500t_g_TABLE ,&
              sr2006_theta_1500_a   =>  input%sr2006_theta_1500_a_TABLE  ,&
              sr2006_theta_1500_b   =>  input%sr2006_theta_1500_b_TABLE  ,&
              sr2006_theta_33t_a    =>  input%sr2006_theta_33t_a_TABLE   ,&
              sr2006_theta_33t_b    =>  input%sr2006_theta_33t_b_TABLE   ,&
              sr2006_theta_33t_c    =>  input%sr2006_theta_33t_c_TABLE   ,&
              sr2006_theta_33t_d    =>  input%sr2006_theta_33t_d_TABLE   ,&
              sr2006_theta_33t_e    =>  input%sr2006_theta_33t_e_TABLE   ,&
              sr2006_theta_33t_f    =>  input%sr2006_theta_33t_f_TABLE   ,&
              sr2006_theta_33t_g    =>  input%sr2006_theta_33t_g_TABLE   ,&
              sr2006_theta_33_a     =>  input%sr2006_theta_33_a_TABLE    ,&
              sr2006_theta_33_b     =>  input%sr2006_theta_33_b_TABLE    ,&
              sr2006_theta_33_c     =>  input%sr2006_theta_33_c_TABLE    ,&
              sr2006_theta_s33t_a   =>  input%sr2006_theta_s33t_a_TABLE  ,&
              sr2006_theta_s33t_b   =>  input%sr2006_theta_s33t_b_TABLE  ,&
              sr2006_theta_s33t_c   =>  input%sr2006_theta_s33t_c_TABLE  ,&
              sr2006_theta_s33t_d   =>  input%sr2006_theta_s33t_d_TABLE  ,&
              sr2006_theta_s33t_e   =>  input%sr2006_theta_s33t_e_TABLE  ,&
              sr2006_theta_s33t_f   =>  input%sr2006_theta_s33t_f_TABLE  ,&
              sr2006_theta_s33t_g   =>  input%sr2006_theta_s33t_g_TABLE  ,&
              sr2006_theta_s33_a    =>  input%sr2006_theta_s33_a_TABLE   ,&
              sr2006_theta_s33_b    =>  input%sr2006_theta_s33_b_TABLE   ,&
              sr2006_psi_et_a       =>  input%sr2006_psi_et_a_TABLE      ,&
              sr2006_psi_et_b       =>  input%sr2006_psi_et_b_TABLE      ,&
              sr2006_psi_et_c       =>  input%sr2006_psi_et_c_TABLE      ,&
              sr2006_psi_et_d       =>  input%sr2006_psi_et_d_TABLE      ,&
              sr2006_psi_et_e       =>  input%sr2006_psi_et_e_TABLE      ,&
              sr2006_psi_et_f       =>  input%sr2006_psi_et_f_TABLE      ,&
              sr2006_psi_et_g       =>  input%sr2006_psi_et_g_TABLE      ,&
              sr2006_psi_e_a        =>  input%sr2006_psi_e_a_TABLE       ,&
              sr2006_psi_e_b        =>  input%sr2006_psi_e_b_TABLE       ,&
              sr2006_psi_e_c        =>  input%sr2006_psi_e_c_TABLE       ,&
              sr2006_smcmax_a       =>  input%sr2006_smcmax_a_TABLE      ,&
              sr2006_smcmax_b       =>  input%sr2006_smcmax_b_TABLE      ,&
              smcwlt                =>  input%SMCWLT_TABLE(SOILTYP)      ,&
              smcref                =>  input%SMCREF_TABLE(SOILTYP)      ,&
              smcmax                =>  input%SMCMAX_TABLE(SOILTYP)      ,&
              smcdry                =>  input%SMCDRY_TABLE(SOILTYP)      ,&
              bexp                  =>  input%BEXP_TABLE  (SOILTYP)      ,&
              psisat                =>  input%PSISAT_TABLE(SOILTYP)      ,&
              dksat                 =>  input%DKSAT_TABLE (SOILTYP)      ,&
              quartz                =>  input%QUARTZ_TABLE(SOILTYP)       &
             ) 

    !-------------------------------------------------------------------------
    
    do k = 1,4
      if(sand(k) <= 0 .or. clay(k) <= 0) then
         sand(k) = 0.41
         clay(k) = 0.18
      end if
      if(orgm(k) <= 0 ) orgm(k) = 0.0
    end do
        
    theta_1500t =   input%sr2006_theta_1500t_a*sand       &
                  + input%sr2006_theta_1500t_b*clay       &
                  + input%sr2006_theta_1500t_c*orgm       &
                  + input%sr2006_theta_1500t_d*sand*orgm  &
                  + input%sr2006_theta_1500t_e*clay*orgm  &
                  + input%sr2006_theta_1500t_f*sand*clay  &
                  + input%sr2006_theta_1500t_g

    theta_1500  =   theta_1500t                      &
                  + input%sr2006_theta_1500_a*theta_1500t  &
                  + input%sr2006_theta_1500_b

    theta_33t   =   input%sr2006_theta_33t_a*sand       &
                  + input%sr2006_theta_33t_b*clay       &
                  + input%sr2006_theta_33t_c*orgm       &
                  + input%sr2006_theta_33t_d*sand*orgm  &
                  + input%sr2006_theta_33t_e*clay*orgm  &
                  + input%sr2006_theta_33t_f*sand*clay  &
                  + input%sr2006_theta_33t_g

    theta_33    =   theta_33t                              &
                  + input%sr2006_theta_33_a*theta_33t*theta_33t  &
                  + input%sr2006_theta_33_b*theta_33t            &
                  + input%sr2006_theta_33_c

    theta_s33t  =   input%sr2006_theta_s33t_a*sand      &
                  + input%sr2006_theta_s33t_b*clay      &
                  + input%sr2006_theta_s33t_c*orgm      &
                  + input%sr2006_theta_s33t_d*sand*orgm &
                  + input%sr2006_theta_s33t_e*clay*orgm &
                  + input%sr2006_theta_s33t_f*sand*clay &
                  + input%sr2006_theta_s33t_g

    theta_s33   = theta_s33t                       &
                  + input%sr2006_theta_s33_a*theta_s33t  &
                  + input%sr2006_theta_s33_b

    psi_et      =   input%sr2006_psi_et_a*sand           &
                  + input%sr2006_psi_et_b*clay           &
                  + input%sr2006_psi_et_c*theta_s33      &
                  + input%sr2006_psi_et_d*sand*theta_s33 &
                  + input%sr2006_psi_et_e*clay*theta_s33 &
                  + input%sr2006_psi_et_f*sand*clay      &
                  + input%sr2006_psi_et_g
 
    psi_e       =   psi_et                        &
                  + input%sr2006_psi_e_a*psi_et*psi_et  &
                  + input%sr2006_psi_e_b*psi_et         &
                  + input%sr2006_psi_e_c
    
    smcwlt = theta_1500
    smcref = theta_33
    smcmax = theta_33                     &
             + theta_s33                  &
             + input%sr2006_smcmax_a*sand &
             + input%sr2006_smcmax_b

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
  
! Introducing somewhat arbitrary limits (based on SOILPARM) to prevent bad things
  
    smcmax = max(0.32 ,min(smcmax,  0.50 ))
    smcref = max(0.17 ,min(smcref,smcmax ))
    smcwlt = max(0.01 ,min(smcwlt,smcref ))
    smcdry = max(0.01 ,min(smcdry,smcref ))
    bexp   = max(2.50 ,min(bexp,    12.0 ))
    psisat = max(0.03 ,min(psisat,  1.00 ))
    dksat  = max(5.e-7,min(dksat,   1.e-5))
    dwsat  = max(1.e-6,min(dwsat,   3.e-5))
    quartz = max(0.05 ,min(quartz,  0.95 ))

  end subroutine PedoTransfer_SR2006

end module PedoTransferSR2006
