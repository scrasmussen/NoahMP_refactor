module CheckNanMod

!!! Check NaN values

  use Machine, only : kind_noahmp

  implicit none

contains

  subroutine CheckRealNaN(NumIn, OutVal)

    real(kind=kind_noahmp), intent(in)  :: NumIn
    logical               , intent(out) :: OutVal
 
    OutVal = (NumIn /= NumIn)

  end subroutine CheckRealNaN

end module CheckNanMod
