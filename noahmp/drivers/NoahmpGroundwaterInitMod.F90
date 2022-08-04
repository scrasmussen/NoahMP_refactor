module NoahmpGroundwaterInitMod

!--------------------------------------------------------------------------
!  Module to initialize Noah-MP Groundwater (GW) variables for MMF GW scheme
!  P. Valayamkunnath C. He & refactor team (April 08 2022)
!--------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  
  implicit none
  
contains

  subroutine NoahmpGroundwaterInitMain(grid, NoahmpIO)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: GROUNDWATER_INIT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (April 08 2022)
! ------------------------------------------------------------------------- 

  use GroundWaterMmfMod, only : LATERALFLOW
  use module_domain, only: domain
  
#if (EM_CORE == 1)
#ifdef DM_PARALLEL
  use module_dm     , only : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
  use module_comm_dm, only : halo_em_hydro_noahmp_sub
#endif
#endif

    implicit none 
    
    type(NoahmpIO_type), intent(inout)    :: NoahmpIO
    type(domain), target                  :: grid                                                    ! state
    
! local
    integer                :: I,J,K,ITER,itf,jtf, NITER, NCOUNT,NS
    real(kind=kind_noahmp) :: BEXP,SMCMAX,PSISAT,SMCWLT,DWSAT,DKSAT
    real(kind=kind_noahmp) :: FRLIQ,SMCEQDEEP
    real(kind=kind_noahmp) :: DELTAT,RCOND,TOTWATER
    real(kind=kind_noahmp) :: AA,BBB,CC,DD,DX,FUNC,DFUNC,DDZ,EXPON,SMC,FLUX

    real(kind=kind_noahmp),&            
      dimension(1:NoahmpIO%NSOIL)                                     :: SMCEQ,ZSOIL
    real(kind=kind_noahmp),& 
      dimension(NoahmpIO%ims:NoahmpIO%ime, NoahmpIO%jms:NoahmpIO%jme) :: QLAT, QRF
    integer,               &              
      dimension(NoahmpIO%ims:NoahmpIO%ime, NoahmpIO%jms:NoahmpIO%jme) :: LANDMASK     !-1 for water (ice or no ice) and glacial areas, 1 for land where the LSM does its soil moisture calculations  
    
    associate(                                        &
              NSOIL      => NoahmpIO%num_soil_layers, &
              DZS        => NoahmpIO%dzs,             &
              ISLTYP     => NoahmpIO%isltyp,          &
              IVGTYP     => NoahmpIO%ivgtyp,          &
              WTDDT      => NoahmpIO%wtddt,           &  
              FDEPTH     => NoahmpIO%fdepthxy,        &
              TOPO       => NoahmpIO%terrain,         &
              RIVERBED   => NoahmpIO%riverbedxy,      &
              EQWTD      => NoahmpIO%eqzwt,           &
              RIVERCOND  => NoahmpIO%rivercondxy,     &
              PEXP       => NoahmpIO%pexpxy,          &  
              AREA       => NoahmpIO%areaxy,          & 
              WTD        => NoahmpIO%zwtxy,           & 
              SMOIS      => NoahmpIO%smois,           & 
              SH2O       => NoahmpIO%sh2o,            &  
              SMOISEQ    => NoahmpIO%smoiseq,         &  
              SMCWTDXY   => NoahmpIO%smcwtdxy,        &  
              DEEPRECHXY => NoahmpIO%deeprechxy,      &  
              RECHXY     => NoahmpIO%rechxy,          & 
              QSLATXY    => NoahmpIO%qslatxy,         & 
              QRFSXY     => NoahmpIO%qrfsxy,          & 
              QSPRINGSXY => NoahmpIO%qspringsxy,      &
              rechclim   => NoahmpIO%rechclim,        & 
              ids        => NoahmpIO%ids,             &
              ide        => NoahmpIO%ide,             &
              jds        => NoahmpIO%jds,             &
              jde        => NoahmpIO%jde,             &  
              kds        => NoahmpIO%kds,             &
              kde        => NoahmpIO%kde,             &
              ims        => NoahmpIO%ims,             &
              ime        => NoahmpIO%ime,             &  
              jms        => NoahmpIO%jms,             &
              jme        => NoahmpIO%jme,             &  
              kms        => NoahmpIO%kms,             &
              kme        => NoahmpIO%kme,             &
              ips        => NoahmpIO%ims,             &
              ipe        => NoahmpIO%ime,             &  
              jps        => NoahmpIO%jms,             &
              jpe        => NoahmpIO%jme,             &  
              kps        => NoahmpIO%kms,             &
              kpe        => NoahmpIO%kme,             &
              its        => NoahmpIO%its,             &
              ite        => NoahmpIO%ite,             &  
              jts        => NoahmpIO%jts,             &
              jte        => NoahmpIO%jte,             &  
              kts        => NoahmpIO%kts,             &
              kte        => NoahmpIO%kte              & 
             )

    
    ! Given the soil layer thicknesses (in DZS), calculate the soil layer
    ! depths from the surface.
    ZSOIL(1)         = -DZS(1)          ! negative
    do NS=2, NSOIL
       ZSOIL(NS)       = ZSOIL(NS-1) - DZS(NS)
    enddo

    

    itf=min0(ite,(ide+1)-1)
    jtf=min0(jte,(jde+1)-1)

    

    where(IVGTYP.NE.NoahmpIO%ISWATER_TABLE.AND.IVGTYP.NE.NoahmpIO%ISICE_TABLE)
         LANDMASK=1
    elsewhere
         LANDMASK=-1
    endwhere
        
    PEXP = 1.0

    DELTAT=365.*24*3600. !1 year
    
!readjust the raw aggregated water table from hires, so that it is better compatible with topography

!use WTD here, to use the lateral communication routine
    WTD=EQWTD

    NCOUNT=0
    
    do NITER=1,500

#if (EM_CORE == 1)
#ifdef DM_PARALLEL
#     include "HALO_EM_HYDRO_NOAHMP.inc"
#endif
#endif

!Calculate lateral flow

      if(NCOUNT.GT.0.OR.NITER.eq.1)then

         QLAT = 0.
         call LATERALFLOW(NoahmpIO, ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA &
                          ,ids,ide,jds,jde,kds,kde                         & 
                          ,ims,ime,jms,jme,kms,kme                         &
                          ,its,ite,jts,jte,kts,kte                         )
     
         NCOUNT=0
         do J=jts,jtf
           do I=its,itf
             if(LANDMASK(I,J).GT.0)then
               if(QLAT(i,j).GT.1.e-2)then
                  NCOUNT=NCOUNT+1
                  WTD(i,j)=min(WTD(i,j)+0.25,0.)
               endif
             endif
           enddo
         enddo
      endif
    enddo !NITER

#if (EM_CORE == 1)
#ifdef DM_PARALLEL
#     include "HALO_EM_HYDRO_NOAHMP.inc"
#endif
#endif

    EQWTD=WTD

!after adjusting, where qlat > 1cm/year now wtd is at the surface.
!it may still happen that qlat + rech > 0 and eqwtd-rbed <0. There the wtd can
!rise to the surface (poor drainage) but the et will then increase.


!now, calculate rcond:

    do J=jts,jtf
       do I=its,itf

        DDZ = EQWTD(I,J)- ( RIVERBED(I,J)-TOPO(I,J) )
!dont allow riverbed above water table
        if(DDZ.LT.0.)then
               RIVERBED(I,J)=TOPO(I,J)+EQWTD(I,J)
               DDZ=0.
        endif


        TOTWATER = AREA(I,J)*(QLAT(I,J)+RECHCLIM(I,J)*0.001)/DELTAT

        if (TOTWATER.GT.0) then
              RIVERCOND(I,J) = TOTWATER / max(DDZ,0.05)
        else
              RIVERCOND(I,J)=0.01
!and make riverbed  equal to eqwtd, otherwise qrf might be too big...
              RIVERBED(I,J)=TOPO(I,J)+EQWTD(I,J)
        endif

       enddo
    enddo
    
!make riverbed to be height down from the surface instead of above sea level

    RIVERBED = min( RIVERBED-TOPO, 0.)

!now recompute lateral flow and flow to rivers to initialize deep soil moisture

    DELTAT = WTDDT * 60. !timestep in seconds for this calculation

    
!recalculate lateral flow

    QLAT = 0.
    call LATERALFLOW(NoahmpIO, ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA &
                     ,ids,ide,jds,jde,kds,kde                         & 
                     ,ims,ime,jms,jme,kms,kme                         &
                     ,its,ite,jts,jte,kts,kte                         )
                        
!compute flux from grounwater to rivers in the cell
    
    do J=jts,jtf
       do I=its,itf
          if(LANDMASK(I,J).GT.0)then
             if(WTD(I,J) .GT. RIVERBED(I,J) .AND.  EQWTD(I,J) .GT. RIVERBED(I,J)) then
               RCOND = RIVERCOND(I,J) * EXP(PEXP(I,J)*(WTD(I,J)-EQWTD(I,J)))
             else    
               RCOND = RIVERCOND(I,J)
             endif
             QRF(I,J) = RCOND * (WTD(I,J)-RIVERBED(I,J)) * DELTAT/AREA(I,J)
!for now, dont allow it to go from river to groundwater
             QRF(I,J) = max(QRF(I,J),0.) 
          else
             QRF(I,J) = 0.
          endif
       enddo
    enddo
    
!now compute eq. soil moisture, change soil moisture to be compatible with the water table and compute deep soil moisture

       do J = jts,jtf
          do I = its,itf
             BEXP   = NoahmpIO%BEXP_TABLE(ISLTYP(I,J))
             SMCMAX = NoahmpIO%SMCMAX_TABLE(ISLTYP(I,J))
             SMCWLT = NoahmpIO%SMCWLT_TABLE(ISLTYP(I,J))
                
             if(IVGTYP(I,J)==NoahmpIO%ISURBAN_TABLE)then
                SMCMAX = 0.45         
                SMCWLT = 0.40         
             endif 
                
             DWSAT  = NoahmpIO%DWSAT_TABLE(ISLTYP(I,J))
             DKSAT  = NoahmpIO%DKSAT_TABLE(ISLTYP(I,J))
             PSISAT = -NoahmpIO%PSISAT_TABLE(ISLTYP(I,J))
             if ( ( BEXP > 0.0 ) .AND. ( smcmax > 0.0 ) .AND. ( -psisat > 0.0 ) ) then

                !initialize equilibrium soil moisture for water table diagnostic
                call EquilibriumSoilMoisture(NSOIL,  ZSOIL, SMCMAX, SMCWLT, &
                                             DWSAT,  DKSAT, BEXP  , SMCEQ)  
                
                SMOISEQ (I,1:NSOIL,J) = SMCEQ (1:NSOIL)
                
                !make sure that below the water table the layers are saturated and
                !initialize the deep soil moisture
                if(WTD(I,J) < ZSOIL(NSOIL)-DZS(NSOIL)) then

                  !initialize deep soil moisture so that the flux compensates qlat+qrf
                  !use Newton-Raphson method to find soil moisture
                  EXPON = 2. * BEXP + 3.
                  DDZ   = ZSOIL(NSOIL) - WTD(I,J)
                  CC    = PSISAT/DDZ
                  FLUX  = (QLAT(I,J)-QRF(I,J))/DELTAT
                  SMC   = 0.5 * SMCMAX

                  do ITER = 1, 100
                     DD    = (SMC+SMCMAX)/(2.*SMCMAX)
                     AA    = -DKSAT * DD  ** EXPON
                     BBB   = CC * ( (SMCMAX/SMC)**BEXP - 1. ) + 1. 
                     FUNC  = AA * BBB - FLUX
                     DFUNC = -DKSAT * (EXPON/(2.*SMCMAX)) * DD ** (EXPON - 1.) * BBB &
                             + AA * CC * (-BEXP) * SMCMAX ** BEXP * SMC ** (-BEXP-1.)
                     DX    = FUNC/DFUNC
                     SMC   = SMC - DX
                     if ( ABS (DX) < 1.E-6)exit
                  enddo

                  SMCWTDXY(I,J) = max(SMC,1.E-4)

                elseif(WTD(I,J) < ZSOIL(NSOIL))then
                  SMCEQDEEP     = SMCMAX * ( PSISAT / ( PSISAT - DZS(NSOIL) ) ) ** (1./BEXP)
!                 SMCEQDEEP     = MAX(SMCEQDEEP,SMCWLT)
                  SMCEQDEEP     = MAX(SMCEQDEEP,1.E-4)
                  SMCWTDXY(I,J) = SMCMAX * ( WTD(I,J) -  (ZSOIL(NSOIL)-DZS(NSOIL))) + &
                                  SMCEQDEEP * (ZSOIL(NSOIL) - WTD(I,J))

                else !water table within the resolved layers
                  SMCWTDXY(I,J) = SMCMAX
                  do K=NSOIL,2,-1
                     if(WTD(I,J) .GE. ZSOIL(K-1))then
                          FRLIQ        = SH2O(I,K,J) / SMOIS(I,K,J)
                          SMOIS(I,K,J) = SMCMAX
                          SH2O(I,K,J)  = SMCMAX * FRLIQ
                     else
                          if(SMOIS(I,K,J).LT.SMCEQ(K))then
                             WTD(I,J)  = ZSOIL(K)
                          else
                             WTD(I,J)  = ( SMOIS(I,K,J)*DZS(K) - SMCEQ(K)*ZSOIL(K-1) + SMCMAX*ZSOIL(K) ) / &
                                         (SMCMAX - SMCEQ(K))   
                          endif
                          exit
                     endif
                  enddo
                endif
             else
               SMOISEQ (I,1:NSOIL,J) = SMCMAX
               SMCWTDXY(I,J)         = SMCMAX
               WTD(I,J)              = 0.
             endif
  
!zero out some arrays

             DEEPRECHXY(I,J) = 0.
             RECHXY(I,J)     = 0.
             QSLATXY(I,J)    = 0.
             QRFSXY(I,J)     = 0.
             QSPRINGSXY(I,J) = 0.
  
          enddo
       enddo

    endassociate
    
  end subroutine NoahmpGroundwaterInitMain

  subroutine EquilibriumSoilMoisture(NSOIL ,  ZSOIL,  SMCMAX, &
                                     SMCWLT,  DWSAT,  DKSAT , &
                                     BEXP  ,  SMCEQ)
! ----------------------------------------------------------------------
    implicit none 
! ----------------------------------------------------------------------
! input
    integer,                                     intent(in)  :: NSOIL !no. of soil layers
    real(kind=kind_noahmp),                      intent(in)  :: SMCMAX , SMCWLT, BEXP , DWSAT, DKSAT
    real(kind=kind_noahmp),  dimension(1:NSOIL), intent(in)  :: ZSOIL !depth of soil layer-bottom [m]
!output
    real(kind=kind_noahmp),  dimension(1:NSOIL), intent(out) :: SMCEQ  !equilibrium soil water  content [m3/m3]
!local
    integer                                                  :: K, ITER
    real(kind=kind_noahmp)                                   :: DDZ  , SMC, FUNC, &
                                                                DFUNC, AA , BB  , &
                                                                EXPON, DX
                                                              
                                                              
!gmm compute equilibrium soil moisture content for the layer when wtd=zsoil(k)

     do K=1,NSOIL

        if(K == 1)then
           DDZ = -ZSOIL(K+1) * 0.5
        elseif ( K < NSOIL ) then
           DDZ = ( ZSOIL(K-1) - ZSOIL(K+1) ) * 0.5
        else
           DDZ = ZSOIL(K-1) - ZSOIL(K)
        endif

!use Newton-Raphson method to find eq soil moisture

        EXPON = BEXP +1.
        AA    = DWSAT/DDZ
        BB    = DKSAT / SMCMAX ** EXPON
        SMC   = 0.5 * SMCMAX

        do ITER = 1, 100
          FUNC = (SMC - SMCMAX) * AA +  BB * SMC ** EXPON
          DFUNC = AA + BB * EXPON * SMC ** BEXP 
          DX = FUNC/DFUNC
          SMC = SMC - DX
          if ( ABS (DX) < 1.E-6)exit
        enddo

!       SMCEQ(K) = min(max(SMC,SMCWLT),SMCMAX*0.99)
        SMCEQ(K) = min(max(SMC,1.E-4),SMCMAX*0.99)
      
     enddo

  end subroutine EquilibriumSoilMoisture
  
end module NoahmpGroundwaterInitMod
