module ConfigVarInitMod

!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigType.f90

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine ConfigVarInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%config%nmlist%DVEG            = huge(1)
    noahmp%config%domain%ISWATER         = huge(1)
    noahmp%config%domain%ISBARREN        = huge(1)
    noahmp%config%domain%ISICE           = huge(1)
    noahmp%config%domain%ISCROP          = huge(1)
    noahmp%config%domain%EBLFOREST       = huge(1)
    noahmp%config%domain%VEGTYP          = huge(1)
    noahmp%config%domain%CROPTYP         = huge(1)
    noahmp%config%domain%YEARLEN         = huge(1)
    noahmp%config%domain%JULIAN          = huge(1.0)
    noahmp%config%domain%LAT             = huge(1.0)
    noahmp%config%domain%URBAN_FLAG      = .false.

  end subroutine ConfigVarInitDefault

!=== initialize with input data or table values
  subroutine ConfigVarInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    noahmp%config%nmlist%DVEG            = input%OPT_DVEGIn
    noahmp%config%domain%ISWATER         = input%ISWATER_TABLE
    noahmp%config%domain%ISBARREN        = input%ISBARREN_TABLE
    noahmp%config%domain%ISICE           = input%ISICE_TABLE
    noahmp%config%domain%ISCROP          = input%ISCROP_TABLE
    noahmp%config%domain%EBLFOREST       = input%EBLFOREST_TABLE
    noahmp%config%domain%VEGTYP          = input%VEGTYPEIn
    noahmp%config%domain%CROPTYP         = input%CROPTYPEIn
    noahmp%config%domain%YEARLEN         = input%YEARLEN
    noahmp%config%domain%JULIAN          = input%JULIAN
    noahmp%config%domain%LAT             = input%LAT


  IF( input%VEGTYPEIn == input%ISURBAN_TABLE .or. input%VEGTYPEIn == input%LCZ_1_TABLE .or. &
      input%VEGTYPEIn == input%LCZ_2_TABLE   .or. input%VEGTYPEIn == input%LCZ_3_TABLE .or. &
      input%VEGTYPEIn == input%LCZ_4_TABLE   .or. input%VEGTYPEIn == input%LCZ_5_TABLE .or. &
      input%VEGTYPEIn == input%LCZ_6_TABLE   .or. input%VEGTYPEIn == input%LCZ_7_TABLE .or. &
      input%VEGTYPEIn == input%LCZ_8_TABLE   .or. input%VEGTYPEIn == input%LCZ_9_TABLE .or. &
      input%VEGTYPEIn == input%LCZ_10_TABLE  .or. input%VEGTYPEIn == input%LCZ_11_TABLE ) THEN
      noahmp%config%domain%URBAN_FLAG = .true.
  ENDIF

  end subroutine ConfigVarInitTransfer

end module ConfigVarInitMod
