module NoahmpSnowinitMod

!--------------------------------------------------------------------------
!  Module to initialize Noah-MP Snow variables
!  P. Valayamkunnath C. He & refactor team (April 08 2022)
!--------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  
  implicit none
  
contains

  subroutine NoahmpSnowinitMain(NoahmpIO)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOW_INIT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (April 08 2022)
! ------------------------------------------------------------------------- 

    implicit none 
    
    type(NoahmpIO_type)    :: NoahmpIO
    
! Local variables:
!   DZSNO   holds the thicknesses of the various snow layers.
!   DZSNOSO holds the thicknesses of the various soil/snow layers.
    integer                                                               :: I,J,IZ,itf,jtf
    real(kind=kind_noahmp),   dimension(-NoahmpIO%NSNOW+1:             0) :: DZSNO
    real(kind=kind_noahmp),   dimension(-NoahmpIO%NSNOW+1:NoahmpIO%NSOIL) :: DZSNSO    
    
    associate(ims      => NoahmpIO%ims,      &
              ime      => NoahmpIO%ime,      & 
              jms      => NoahmpIO%jms,      &
              jme      => NoahmpIO%jme,      & 
              its      => NoahmpIO%its,      &
              ite      => NoahmpIO%ite,      & 
              jts      => NoahmpIO%jts,      &
              jte      => NoahmpIO%jte,      &
              NSNOW    => NoahmpIO%NSNOW,    &
              NSOIL    => NoahmpIO%NSOIL,    &
              ZSOIL    => NoahmpIO%ZSOIL,    &
              SWE      => NoahmpIO%SNOW,     & 
              TGXY     => NoahmpIO%TGXY,     & 
              SNODEP   => NoahmpIO%SNOWH,    &
              ZSNSOXY  => NoahmpIO%ZSNSOXY,  & 
              TSNOXY   => NoahmpIO%TSNOXY,   &
              SNICEXY  => NoahmpIO%SNICEXY,  &
              SNLIQXY  => NoahmpIO%SNLIQXY,  &
              ISNOWXY  => NoahmpIO%ISNOWXY   &
             )
!------------------------------------------------------------------------------------------
!   Initialize snow arrays for Noah-MP LSM, based in input SNOWDEP, NSNOW
!   ISNOWXY is an index array, indicating the index of the top snow layer.  Valid indices
!           for snow layers range from 0 (no snow) and -1 (shallow snow) to (-NSNOW)+1 (deep snow).
!   TSNOXY holds the temperature of the snow layer.  Snow layers are initialized with 
!          temperature = ground temperature [?].  Snow-free levels in the array have value 0.0
!   SNICEXY is the frozen content of a snow layer.  Initial estimate based on SNODEP and SWE
!   SNLIQXY is the liquid content of a snow layer.  Initialized to 0.0
!   ZNSNOXY is the layer depth from the surface.  
!------------------------------------------------------------------------------------------

    itf=min0(NoahmpIO%ite,(NoahmpIO%ide+1)-1)
    jtf=min0(NoahmpIO%jte,(NoahmpIO%jde+1)-1)

    do J = NoahmpIO%jts , jtf
       do I = NoahmpIO%its , itf
          if ( SNODEP(I,J) < 0.025 ) then
             ISNOWXY(I,J) = 0
             DZSNO(-NSNOW+1:0) = 0.
          else
             if ( ( SNODEP(I,J) >= 0.025 ) .AND. ( SNODEP(I,J) <= 0.05 ) ) then
                ISNOWXY(I,J)    = -1
                DZSNO(0)  = SNODEP(I,J)
             elseif ( ( SNODEP(I,J) > 0.05 ) .AND. ( SNODEP(I,J) <= 0.10 ) ) then
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = SNODEP(I,J)/2.
                DZSNO( 0) = SNODEP(I,J)/2.
             elseif ( (SNODEP(I,J) > 0.10 ) .AND. ( SNODEP(I,J) <= 0.25 ) ) then
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = 0.05
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1)
             elseif ( ( SNODEP(I,J) > 0.25 ) .AND. ( SNODEP(I,J) <= 0.45 ) ) then
                ISNOWXY(I,J)    = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.5*(SNODEP(I,J)-DZSNO(-2))
                DZSNO( 0) = 0.5*(SNODEP(I,J)-DZSNO(-2))
             elseif ( SNODEP(I,J) > 0.45 ) then
                ISNOWXY(I,J)     = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.20
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1) - DZSNO(-2)
             else
                call wrf_error_fatal("Problem with the logic assigning snow layers.")
             endif
          endif

          TSNOXY (I,-NSNOW+1:0,J) = 0.
          SNICEXY(I,-NSNOW+1:0,J) = 0.
          SNLIQXY(I,-NSNOW+1:0,J) = 0.
          do IZ = ISNOWXY(I,J)+1 , 0
             TSNOXY(I,IZ,J)  = TGXY(I,J)  ! [k]
             SNLIQXY(I,IZ,J) = 0.00
             SNICEXY(I,IZ,J) = 1.00 * DZSNO(IZ) * (SWE(I,J)/SNODEP(I,J))  ! [kg/m3]
          enddo

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for snow layers
          do IZ = ISNOWXY(I,J)+1 , 0
             DZSNSO(IZ) = -DZSNO(IZ)
          enddo

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for soil layers
          DZSNSO(1) = ZSOIL(1)
          do IZ = 2 , NSOIL
             DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
          enddo

          ! Assign ZSNSOXY, the layer depths, for soil and snow layers
          ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSO(ISNOWXY(I,J)+1)
          do IZ = ISNOWXY(I,J)+2 , NSOIL
             ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSO(IZ)
          enddo

       enddo
    enddo

    endassociate
    
  end subroutine NoahmpSnowinitMain

end module NoahmpSnowinitMod

