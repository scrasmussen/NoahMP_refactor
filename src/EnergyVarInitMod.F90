module EnergyVarInitMod

!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine EnergyVarInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    allocate( noahmp%energy%param%LAIM (1:12) ) 
    allocate( noahmp%energy%param%SAIM (1:12) )

    noahmp%energy%state%ELAI   = huge(1.0)
    noahmp%energy%state%ESAI   = huge(1.0)
    noahmp%energy%state%LAI    = huge(1.0)
    noahmp%energy%state%SAI    = huge(1.0)
    noahmp%energy%state%TV     = huge(1.0)
    noahmp%energy%state%TROOT  = huge(1.0)
    noahmp%energy%param%LAIM   = huge(1.0)
    noahmp%energy%param%SAIM   = huge(1.0)

  end subroutine EnergyVarInitDefault

!=== initialize with input data or table values
  subroutine EnergyVarInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input
    
    associate(VEGTYP => noahmp%config%domain%VEGTYP)
  
 
    noahmp%energy%state%TV                 = input%tv
    noahmp%energy%state%TROOT              = input%troot

    ! energy parameter variable
    noahmp%energy%param%HVT                = input%HVT_TABLE(VEGTYP)
    noahmp%energy%param%HVB                = input%HVB_TABLE(VEGTYP)
    noahmp%energy%param%LAIM(1:12)         = input%LAIM_TABLE(VEGTYP,1:12)
    noahmp%energy%param%SAIM(1:12)         = input%SAIM_TABLE(VEGTYP,1:12)

    end associate

  end subroutine EnergyVarInitTransfer

end module EnergyVarInitMod
