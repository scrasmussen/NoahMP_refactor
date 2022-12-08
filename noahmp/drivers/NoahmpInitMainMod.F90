module NoahmpInitMainMod

!--------------------------------------------------------------------------
!  Module to initialize Noah-MP variables
!  P. Valayamkunnath C. He & refactor team (April 08 2022)
!--------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpSnowInitMod
 
  implicit none
  
contains

  subroutine NoahmpInitMain(NoahmpIO)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: NOAHMP_INIT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (April 08 2022)
! ------------------------------------------------------------------------- 

    implicit none 
   
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    !local
    integer                                     :: ids, ide, jds, jde, kds, kde, &
                                                   ims, ime, jms, jme, kms, kme, &
                                                   its, ite, jts, jte, kts, kte
    integer                                     :: errflag,i,j,itf,jtf,ns
    logical                                     :: restart, allowed_to_read

    real(kind=kind_noahmp), dimension(1:NoahmpIO%NSOIL) :: ZSOIL      ! Depth of the soil layer bottom (m) from
    !                                                           the surface (negative)
    real(kind=kind_noahmp)                      :: BEXP, SMCMAX, PSISAT
    real(kind=kind_noahmp)                      :: FK, masslai, masssai
    real(kind=kind_noahmp), parameter           :: BLIM  = 5.5
    real(kind=kind_noahmp), parameter           :: HLICE = 3.335E5
    real(kind=kind_noahmp), parameter           :: GRAV0 = 9.81
    real(kind=kind_noahmp), parameter           :: T0 = 273.15
    character(len=240)                          :: err_message
    character(len=4)                            :: MMINSL
    
    associate(MMINLU            => NoahmpIO%LLANDUSE,          &
              SNOW              => NoahmpIO%SNOW,              &
              SNOWH             => NoahmpIO%SNOWH,             &
              CANWAT            => NoahmpIO%CANWAT,            & 
              ISLTYP            => NoahmpIO%ISLTYP,            &
              IVGTYP            => NoahmpIO%IVGTYP,            &
              TSLB              => NoahmpIO%TSLB,              &
              SMOIS             => NoahmpIO%SMOIS,             &
              SH2O              => NoahmpIO%SH2O,              &
              DZS               => NoahmpIO%DZS,               &
              FNDSOILW          => NoahmpIO%FNDSOILW,          & 
              FNDSNOWH          => NoahmpIO%FNDSNOWH,          &
              TSK               => NoahmpIO%TSK,               &
              isnowxy           => NoahmpIO%ISNOWXY,           & 
              tvxy              => NoahmpIO%TVXY,              &
              tgxy              => NoahmpIO%TGXY,              &
              canicexy          => NoahmpIO%CANICEXY,          &        
              TMN               => NoahmpIO%TMN,               &
              XICE              => NoahmpIO%XICE,              &
              canliqxy          => NoahmpIO%CANLIQXY,          &
              eahxy             => NoahmpIO%EAHXY,             &
              tahxy             => NoahmpIO%TAHXY,             &
              cmxy              => NoahmpIO%CMXY,              &
              chxy              => NoahmpIO%CHXY,              &
              fwetxy            => NoahmpIO%FWETXY,            &
              sneqvoxy          => NoahmpIO%SNEQVOXY,          &
              alboldxy          => NoahmpIO%ALBOLDXY,          &
              qsnowxy           => NoahmpIO%QSNOWXY,           &
              qrainxy           => NoahmpIO%QRAINXY,           &
              wslakexy          => NoahmpIO%WSLAKEXY,          &
              zwtxy             => NoahmpIO%ZWTXY,             &
              waxy              => NoahmpIO%WAXY,              &
              wtxy              => NoahmpIO%WTXY,              &  
              tsnoxy            => NoahmpIO%TSNOXY,            &
              zsnsoxy           => NoahmpIO%ZSNSOXY,           &
              snicexy           => NoahmpIO%SNICEXY,           &
              snliqxy           => NoahmpIO%SNLIQXY,           &
              lfmassxy          => NoahmpIO%LFMASSXY,          &
              rtmassxy          => NoahmpIO%RTMASSXY,          &
              stmassxy          => NoahmpIO%STMASSXY,          &
              woodxy            => NoahmpIO%WOODXY,            &
              stblcpxy          => NoahmpIO%STBLCPXY,          &
              fastcpxy          => NoahmpIO%FASTCPXY,          &
              xsaixy            => NoahmpIO%XSAIXY,            &
              lai               => NoahmpIO%LAI,               &
              grainxy           => NoahmpIO%GRAINXY,           &
              gddxy             => NoahmpIO%GDDXY,             &
              CropType          => NoahmpIO%CROPTYPE,          &
              cropcat           => NoahmpIO%CROPCAT,           &
              irnumsi           => NoahmpIO%irnumsi,           &
              irnummi           => NoahmpIO%irnummi,           &
              irnumfi           => NoahmpIO%irnumfi,           &
              irwatsi           => NoahmpIO%irwatsi,           &
              irwatmi           => NoahmpIO%irwatmi,           &
              irwatfi           => NoahmpIO%irwatfi,           &
              ireloss           => NoahmpIO%ireloss,           &
              irsivol           => NoahmpIO%irsivol,           &
              irmivol           => NoahmpIO%irmivol,           &
              irfivol           => NoahmpIO%irfivol,           &
              irrsplh           => NoahmpIO%irrsplh,           &       
              t2mvxy            => NoahmpIO%T2MVXY,            &
              t2mbxy            => NoahmpIO%T2MBXY,            &
              chstarxy          => NoahmpIO%CHSTARXY,          &
              NSOIL             => NoahmpIO%NSOIL,             &
              IOPT_RUNSRF       => NoahmpIO%IOPT_RUNSRF,       & 
              OptCropModel      => NoahmpIO%IOPT_CROP,       &
              OptIrrigation       => NoahmpIO%IOPT_IRR, &
              OptIrrigationMethod => NoahmpIO%IOPT_IRRM, &
              sf_urban_physics  => NoahmpIO%sf_urban_physics,  &
              smoiseq           => NoahmpIO%smoiseq,           &
              smcwtdxy          => NoahmpIO%smcwtdxy,          &
              rechxy            => NoahmpIO%rechxy,            &
              deeprechxy        => NoahmpIO%deeprechxy,        & 
              qtdrain           => NoahmpIO%qtdrain,           &
              areaxy            => NoahmpIO%areaxy,            &
              dx                => NoahmpIO%dx,                &
              dy                => NoahmpIO%dy,                &
              msftx             => NoahmpIO%msftx,             & 
              msfty             => NoahmpIO%msfty,             &
              wtddt             => NoahmpIO%wtddt,             &
              stepwtd           => NoahmpIO%stepwtd,           &
              dt                => NoahmpIO%dtbl,              &
              qrfsxy            => NoahmpIO%qrfsxy,            &
              qspringsxy        => NoahmpIO%qspringsxy,        &
              qslatxy           => NoahmpIO%qslatxy,           &
              fdepthxy          => NoahmpIO%fdepthxy,          &
              ht                => NoahmpIO%terrain,           &
              riverbedxy        => NoahmpIO%riverbedxy,        &
              eqzwt             => NoahmpIO%eqzwt,             &
              rivercondxy       => NoahmpIO%rivercondxy,       &
              pexpxy            => NoahmpIO%pexpxy,            &
              rechclim          => NoahmpIO%rechclim           &
             )                                                          
    
!--------------------------------------------------------------------------------------- 

    MMINSL          = 'STAS'
    restart         = .false.
    allowed_to_read = .true.

    ids             = NoahmpIO%ids
    ide             = NoahmpIO%ide+1 
    jds             = NoahmpIO%jds
    jde             = NoahmpIO%jde+1 
    kds             = NoahmpIO%kds
    kde             = NoahmpIO%kde 
    ims             = NoahmpIO%ims
    ime             = NoahmpIO%ime 
    jms             = NoahmpIO%jms
    jme             = NoahmpIO%jme 
    kms             = NoahmpIO%kms
    kme             = NoahmpIO%kme 
    its             = NoahmpIO%its
    ite             = NoahmpIO%ite 
    jts             = NoahmpIO%jts
    jte             = NoahmpIO%jte 
    kts             = NoahmpIO%kts
    kte             = NoahmpIO%kte

    if( .NOT. restart ) then

       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)

       !
       ! initialize physical snow height SNOWH
       !
       if(.NOT.FNDSNOWH)then
          ! If no SNOWH do the following
          call wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          do J = jts,jtf
             do I = its,itf
                SNOWH(I,J)=SNOW(I,J)*0.005                     ! SNOW in mm and SNOWH in m
             enddo
          enddo
       endif
   
       ! Check if snow/snowh are consistent and cap SWE at 2000mm
       ! the Noah-MP code does it internally but if we don't do it here, problems ensue
       do J = jts,jtf
          do I = its,itf
             if ( SNOW(i,j) > 0.0 .AND. SNOWH(i,j) == 0.0 .OR. SNOWH(i,j) > 0.0 .AND. SNOW(i,j) == 0.0 ) then
               write(err_message,*)"problem with initial snow fields: snow/snowh>0 while snowh/snow=0 at i,j" &
                                     ,i,j,snow(i,j),snowh(i,j)
               call wrf_message(err_message)
             endif
             if ( SNOW( i,j ) > 2000.0 ) then
               SNOWH(I,J) = SNOWH(I,J) * 2000.0 / SNOW(I,J)      ! SNOW in mm and SNOWH in m
               SNOW (I,J) = 2000.0                               ! cap SNOW at 2000, maintain density
             endif
          enddo
       enddo

       errflag = 0
       do j = jts,jtf
          do i = its,itf
             if ( ISLTYP( i,j ) .LT. 1 ) then
                errflag = 1
                write(err_message,*)"module_sf_noahlsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
                call wrf_message(err_message)
             endif
          enddo
       enddo
       if ( errflag .EQ. 1 ) then
          call wrf_error_fatal( "module_sf_noahlsm.F: lsminit: out of range value "// &
               "of ISLTYP. Is this field in the input?" )
       endif

! initialize soil liquid water content SH2O

       do J = jts , jtf
          do I = its , itf
          if(IVGTYP(I,J)==NoahmpIO%ISICE_TABLE .AND. XICE(I,J) <= 0.0) then
             do NS=1, NSOIL
                SMOIS(I,NS,J) = 1.0                     ! glacier starts all frozen
                SH2O(I,NS,J) = 0.0
                TSLB(I,NS,J) = MIN(TSLB(I,NS,J),263.15) ! set glacier temp to at most -10C
             enddo
            !TMN(I,J) = MIN(TMN(I,J),263.15)           ! set deep temp to at most -10C
             SNOW(I,J) = MAX(SNOW(I,J), 10.0)           ! set SWE to at least 10mm
             SNOWH(I,J)=SNOW(I,J)*0.01                  ! SNOW in mm and SNOWH in m

          else

             BEXP   = NoahmpIO%BEXP_TABLE  (ISLTYP(I,J))
             SMCMAX = NoahmpIO%SMCMAX_TABLE(ISLTYP(I,J))
             PSISAT = NoahmpIO%PSISAT_TABLE(ISLTYP(I,J))

                do NS=1, NSOIL
                  if ( SMOIS(I,NS,J) > SMCMAX )  SMOIS(I,NS,J) = SMCMAX
                enddo

                if ( ( BEXP > 0.0 ) .AND. ( SMCMAX > 0.0 ) .AND. ( PSISAT > 0.0 ) ) then
                  do NS=1, NSOIL

                     if ( TSLB(I,NS,J) < 273.149 ) then    ! Use explicit as initial soil ice
                        FK = (( (HLICE/(GRAV0*(-PSISAT))) *                              &
                             ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BEXP) )*SMCMAX
                        FK = MAX(FK, 0.02)
                        SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )
                     else
                        SH2O(I,NS,J)=SMOIS(I,NS,J)
                     endif

                  enddo

                else

                  do NS=1, NSOIL
                     SH2O(I,NS,J)=SMOIS(I,NS,J)
                  enddo

                endif
             endif
          enddo
       enddo

       do J = jts,jtf
          do I = its,itf
             qtdrain    (I,J) = 0.
             tvxy       (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tvxy(I,J) = 273.15
             tgxy       (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tgxy(I,J) = 273.15
             CANWAT     (I,J) = 0.0
             canliqxy   (I,J) = CANWAT(I,J)
             canicexy   (I,J) = 0.
             eahxy      (I,J) = 2000. 
             tahxy      (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tahxy(I,J) = 273.15
!             tahxy      (I,J) = 287.

             t2mvxy     (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mvxy(I,J) = 273.15
             t2mbxy     (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mbxy(I,J) = 273.15
             chstarxy   (I,J) = 0.1

           cmxy       (I,J) = 0.0
           chxy       (I,J) = 0.0
           fwetxy     (I,J) = 0.0
           sneqvoxy   (I,J) = 0.0
           alboldxy   (I,J) = 0.65
           qsnowxy    (I,J) = 0.0
           qrainxy    (I,J) = 0.0
           wslakexy   (I,J) = 0.0

           if(IOPT_RUNSRF.ne.5) then 
              waxy       (I,J) = 4900.                                       !???
              wtxy       (I,J) = waxy(i,j)                                   !???
              zwtxy      (I,J) = (25. + 2.0) - waxy(i,j)/1000/0.2            !???
           else
              waxy       (I,J) = 0.
              wtxy       (I,J) = 0.
              areaxy     (I,J) = (DX * DY) / ( MSFTX(I,J) * MSFTY(I,J) )
           endif

           if(IVGTYP(I,J) == NoahmpIO%ISBARREN_TABLE .OR. IVGTYP(I,J) == NoahmpIO%ISICE_TABLE .OR. &
             (NoahmpIO%SF_URBAN_PHYSICS == 0 .AND. IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE)     .OR. &
              IVGTYP(I,J) == NoahmpIO%ISWATER_TABLE ) then
             lai        (I,J) = 0.0
             xsaixy     (I,J) = 0.0
             lfmassxy   (I,J) = 0.0
             stmassxy   (I,J) = 0.0
             rtmassxy   (I,J) = 0.0
             woodxy     (I,J) = 0.0
             stblcpxy   (I,J) = 0.0
             fastcpxy   (I,J) = 0.0
             grainxy    (I,J) = 1E-10
             gddxy      (I,J) = 0
             cropcat    (I,J) = 0

           else
     
             lai        (I,J) = max(lai(i,j),0.05)                      ! at least start with 0.05 for arbitrary initialization (v3.7)
             xsaixy     (I,J) = max(0.1*lai(I,J),0.05)                  ! MB: arbitrarily initialize SAI using input LAI (v3.7)
             masslai = 1000. / max(NoahmpIO%SLA_TABLE(IVGTYP(I,J)),1.0) ! conversion from lai to mass  (v3.7)
             lfmassxy   (I,J) = lai(i,j)*masslai                        ! use LAI to initialize (v3.7)
             masssai = 1000. / 3.0                                      ! conversion from lai to mass (v3.7)
             stmassxy   (I,J) = xsaixy(i,j)*masssai                     ! use SAI to initialize (v3.7)
             rtmassxy   (I,J) = 500.0                                   ! these are all arbitrary and probably should be
             woodxy     (I,J) = 500.0                                   ! in the table or read from initialization
             stblcpxy   (I,J) = 1000.0                                  !
             fastcpxy   (I,J) = 1000.0                                  !
             grainxy    (I,J) = 1E-10
             gddxy      (I,J) = 0    

! Initialize crop for Liu crop model

                if(OptCropModel == 1 ) then
                   cropcat    (i,j) = NoahmpIO%default_crop_table
                if(CropType(i,5,j) >= 0.5) then
                   rtmassxy(i,j) = 0.0
                   woodxy  (i,j) = 0.0                    

                if(CropType(i,1,j) > CropType(i,2,j) .and. &
                   CropType(i,1,j) > CropType(i,3,j) .and. &
                   CropType(i,1,j) > CropType(i,4,j) ) then        ! choose corn

                      cropcat (i,j) = 1
                      lfmassxy(i,j) =    lai(i,j)/0.015               ! Initialize lfmass Zhe Zhang 2020-07-13
                      stmassxy(i,j) = xsaixy(i,j)/0.003

                elseif(CropType(i,2,j) > CropType(i,1,j) .and. &
                       CropType(i,2,j) > CropType(i,3,j) .and. &
                       CropType(i,2,j) > CropType(i,4,j) ) then        ! choose soybean
                       cropcat (i,j) = 2
                       lfmassxy(i,j) =    lai(i,j)/0.030               ! Initialize lfmass Zhe Zhang 2020-07-13
                       stmassxy(i,j) = xsaixy(i,j)/0.003

               else
                      cropcat (i,j) = NoahmpIO%default_crop_table
                      lfmassxy(i,j) =    lai(i,j)/0.035
                      stmassxy(i,j) = xsaixy(i,j)/0.003
               end if
            end if
         end if

! Noah-MP irrigation scheme !pvk
             if ( (OptIrrigation >= 1) .and. (OptIrrigation <= 3) ) then
                if ( (OptIrrigationMethod == 0) .or. (OptIrrigationMethod ==1) ) then       ! sprinkler
                   irnumsi(i,j) = 0
                   irwatsi(i,j) = 0.
                   ireloss(i,j) = 0.
                   irrsplh(i,j) = 0.     
                elseif ( (OptIrrigationMethod == 0) .or. (OptIrrigationMethod == 2) ) then ! micro or drip
                   irnummi(i,j) = 0
                   irwatmi(i,j) = 0.
                   irmivol(i,j) = 0.
                elseif ( (OptIrrigationMethod == 0) .or. (OptIrrigationMethod == 3) ) then ! flood 
                   irnumfi(i,j) = 0
                   irwatfi(i,j) = 0.
                   irfivol(i,j) = 0.
                endif
             end if
            endif
          enddo
       enddo
       
       ! Given the soil layer thicknesses (in DZS), initialize the soil layer
       ! depths from the surface.
       ZSOIL(1)       = -DZS(1)          ! negative
       do NS=2, NSOIL
          ZSOIL(NS)   = ZSOIL(NS-1) - DZS(NS)
       enddo
       NoahmpIO%ZSOIL = ZSOIL
       
       ! Initialize snow/soil layer arrays ZSNSOXY, TSNOXY, SNICEXY, SNLIQXY, 
       ! and ISNOWXY
       
       !---------------------------------------------------------------------
       ! Initialize Noah-MP Snow
       !--------------------------------------------------------------------- 
 
       call NoahmpSnowinitMain (NoahmpIO)
 
       !initialize arrays for groundwater dynamics iopt_run=5
 
       if(IOPT_RUNSRF.eq.5) then

             STEPWTD = nint(WTDDT*60./DT)
             STEPWTD = max(STEPWTD,1)

       endif
    endif
 
    endassociate
    
  end subroutine NoahmpInitMain    

end module NoahmpInitMainMod
