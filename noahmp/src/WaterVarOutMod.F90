module WaterVarOutMod

!!! Transfer column (1-D) Noah-MP energy variables to 2D NoahmpIO for output
!!! Energy variables should be first defined in EnergyType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use NoahmpIOVarType
  use NoahmpVarType
  use Machine, only : kind_noahmp

  implicit none

contains

!=== Transfer model states to output=====

  subroutine WaterVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    ! local loop index
    integer                          :: ISOIL

    associate(                                                  &
              I           => noahmp%config%domain%ILOC         ,&
              J           => noahmp%config%domain%JLOC         ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL         &
             )

    NoahmpIO%SMSTAV   (I,J)            = 0.0  ! [maintained as Noah consistency] water
    NoahmpIO%SMSTOT   (I,J)            = 0.0  ! [maintained as Noah consistency] water
    NoahmpIO%SFCRUNOFF(I,J)            = NoahmpIO%SFCRUNOFF(I,J) + (noahmp%water%flux%RUNSRF * NoahmpIO%DTBL)
    NoahmpIO%UDRUNOFF (I,J)            = NoahmpIO%UDRUNOFF (I,J) + (noahmp%water%flux%RUNSUB * NoahmpIO%DTBL)
    NoahmpIO%QTDRAIN  (I,J)            = NoahmpIO%QTDRAIN  (I,J) + (noahmp%water%flux%QTLDRN * NoahmpIO%DTBL)
    NoahmpIO%SNOWC    (I,J)            = noahmp%water%state%FSNO
    NoahmpIO%SMOIS    (I,1:NSOIL,J)    = noahmp%water%state%SMC (1:NSOIL)
    NoahmpIO%SH2O     (I,1:NSOIL,J)    = noahmp%water%state%SH2O(1:NSOIL)
    NoahmpIO%SNOW     (I,J)            = noahmp%water%state%SNEQV
    NoahmpIO%SNOWH    (I,J)            = noahmp%water%state%SNOWH
    NoahmpIO%CANWAT   (I,J)            = noahmp%water%state%CANLIQ + noahmp%water%state%CANICE
    NoahmpIO%ACSNOW   (I,J)            = NoahmpIO%ACSNOW(I,J) + (NoahmpIO%RAINBL (I,J) * noahmp%water%state%FPICE)
    NoahmpIO%ACSNOM   (I,J)            = NoahmpIO%ACSNOM(I,J) + (noahmp%water%flux%QSNBOT * NoahmpIO%DTBL) + &
                                         noahmp%water%state%PONDING + noahmp%water%state%PONDING1 +          &
                                         noahmp%water%state%PONDING2

    NoahmpIO%CANLIQXY (I,J)            = noahmp%water%state%CANLIQ
    NoahmpIO%CANICEXY (I,J)            = noahmp%water%state%CANICE
    NoahmpIO%FWETXY   (I,J)            = noahmp%water%state%FWET
    NoahmpIO%SNEQVOXY (I,J)            = noahmp%water%state%SNEQVO
    NoahmpIO%QSNOWXY  (I,J)            = noahmp%water%flux%QSNOW
    NoahmpIO%QRAINXY  (I,J)            = noahmp%water%flux%QRAIN
    NoahmpIO%WSLAKEXY (I,J)            = noahmp%water%state%WSLAKE
    NoahmpIO%ZWTXY    (I,J)            = noahmp%water%state%ZWT
    NoahmpIO%WAXY     (I,J)            = noahmp%water%state%WA
    NoahmpIO%WTXY     (I,J)            = noahmp%water%state%WT
    NoahmpIO%SNICEXY  (I,-NSNOW+1:0,J) = noahmp%water%state%SNICE (-NSNOW+1:0)
    NoahmpIO%SNLIQXY  (I,-NSNOW+1:0,J) = noahmp%water%state%SNLIQ (-NSNOW+1:0)
    NoahmpIO%RUNSFXY  (I,J)            = noahmp%water%flux%RUNSRF
    NoahmpIO%RUNSBXY  (I,J)            = noahmp%water%flux%RUNSUB
    NoahmpIO%ECANXY   (I,J)            = noahmp%water%flux%ECAN
    NoahmpIO%EDIRXY   (I,J)            = noahmp%water%flux%EDIR
    NoahmpIO%ETRANXY  (I,J)            = noahmp%water%flux%ETRAN
    NoahmpIO%RECHXY   (I,J)            = NoahmpIO%RECHXY(I,J) + (noahmp%water%state%RECH*1.E3)      !RECHARGE TO THE WATER TABLE
    NoahmpIO%DEEPRECHXY(I,J)           = NoahmpIO%DEEPRECHXY(I,J) + noahmp%water%state%DEEPRECH
    NoahmpIO%SMCWTDXY (I,J)            = noahmp%water%state%SMCWTD

! irrigation
    NoahmpIO%IRNUMSI  (I,J)            = noahmp%water%state%IRCNTSI
    NoahmpIO%IRNUMMI  (I,J)            = noahmp%water%state%IRCNTMI
    NoahmpIO%IRNUMFI  (I,J)            = noahmp%water%state%IRCNTFI
    NoahmpIO%IRWATSI  (I,J)            = noahmp%water%state%IRAMTSI
    NoahmpIO%IRWATMI  (I,J)            = noahmp%water%state%IRAMTMI
    NoahmpIO%IRWATFI  (I,J)            = noahmp%water%state%IRAMTFI
    NoahmpIO%IRSIVOL  (I,J)            = NoahmpIO%IRSIVOL(I,J)+(noahmp%water%flux%IRSIRATE*1000.0)
    NoahmpIO%IRMIVOL  (I,J)            = NoahmpIO%IRMIVOL(I,J)+(noahmp%water%flux%IRMIRATE*1000.0)
    NoahmpIO%IRFIVOL  (I,J)            = NoahmpIO%IRFIVOL(I,J)+(noahmp%water%flux%IRFIRATE*1000.0)
    NoahmpIO%IRELOSS  (I,J)            = NoahmpIO%IRELOSS(I,J)+(noahmp%water%flux%EIRR*NoahmpIO%DTBL) ! mm

#ifdef WRF_HYDRO
    NoahmpIO%infxsrt   (I,J)           = max((noahmp%water%flux%RUNSRF * NoahmpIO%DTBL), 0.)             ! mm, surface runoff
    NoahmpIO%soldrain  (I,J)           = max((noahmp%water%flux%RUNSUB * NoahmpIO%DTBL), 0.)             ! mm, underground runoff
    NoahmpIO%qtiledrain(I,J)           = max((noahmp%water%flux%QTLDRN * NoahmpIO%DTBL), 0.)             ! mm, tile drainage
#endif

    end associate

  end subroutine WaterVarOutTransfer

end module WaterVarOutMod
