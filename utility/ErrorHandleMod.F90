module ErrorHandleMod

!!! define subroutines handling Noah-MP model errors

  implicit none

contains

  subroutine ErrorHandle(status)

    use netcdf

    integer, intent (in) :: status
 
    if(status /= nf90_noerr) then
       print *, trim( nf90_strerror(status) )
       stop "Stopped"
    endif

  end subroutine ErrorHandle

end module ErrorHandleMod
