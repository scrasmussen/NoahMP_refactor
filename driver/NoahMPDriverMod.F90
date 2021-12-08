program NoahMPDriverMod

  use WaterDriverMod
  use NoahmpVarType
  use InputVarType

  implicit none

!---------------------------------------------------------------------
!  types
!---------------------------------------------------------------------
  type(input_type)    :: input
  type(noahmp_type)   :: noahmp


! call process driver
  call WaterDriver(noahmp, input)

   
end program
