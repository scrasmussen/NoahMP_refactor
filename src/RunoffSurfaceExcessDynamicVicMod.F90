module RunoffSurfaceExcessDynamicVicMod

!!! Compute infiltration and saturation excess runoff for dyanmic VIC runoff scheme

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSatExcessDynamicVic(noahmp, I_0, I_MAX, YD, R1)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: RR1 for saturation excess runoff
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: I_0      ! initial water depth (m)
    real(kind=kind_noahmp), intent(in)    :: I_MAX    ! maximum water depth (m)
    real(kind=kind_noahmp), intent(in)    :: YD       ! initial depth Y (m)
    real(kind=kind_noahmp), intent(out)   :: R1       ! saturation excess runoff (m/s)

! local variable
    real(kind=kind_noahmp) :: TDEPTH      ! water table depth
 
! --------------------------------------------------------------------
    associate(                                                        &
              BDVIC           => noahmp%water%param%BDVIC             & ! in,     DVIC model infiltration parameter
             )
! ----------------------------------------------------------------------

    TDEPTH = I_0 + YD
    if ( TDEPTH > I_MAX ) TDEPTH = I_MAX

    ! Saturation excess runoff , Eq 5.
    R1 = YD - ( (I_MAX/(BDVIC+1.0)) * ( ((1.0 - (I_0/I_MAX))**(BDVIC+1.0)) &
                                    - ((1.0 - (TDEPTH/I_MAX))**(BDVIC+1.0))))

    if ( R1 < 0.0 ) R1 = 0.0

    end associate

  end subroutine RunoffSatExcessDynamicVic


  subroutine RunoffInfilExcessDynamicVic(YD, Y0, R1, FMAX, FSUR, DT, DP, BB, R2)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: RR2 for infiltration excess runoff
! Original code: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variabls
    real(kind=kind_noahmp), intent(in)    :: YD      ! initial depth Y (m)
    real(kind=kind_noahmp), intent(in)    :: Y0      ! initial depth Y (m)
    real(kind=kind_noahmp), intent(in)    :: R1      ! saturation excess runoff (m/s)
    real(kind=kind_noahmp), intent(in)    :: FMAX    ! maximum infiltration rate (m/s)
    real(kind=kind_noahmp), intent(in)    :: FSUR    ! surface infiltration rate (m/s)
    real(kind=kind_noahmp), intent(in)    :: DT      ! timestep (may not be the same as model timestep)
    real(kind=kind_noahmp), intent(in)    :: DP      ! water input on soil surface (m)
    real(kind=kind_noahmp), intent(in)    :: BB      ! B parameter for infiltration scaling curve
    real(kind=kind_noahmp), intent(out)   :: R2      ! infiltration excess runoff (m/s)
! ----------------------------------------------------------------------

    if ( YD >= Y0 ) then
       R2 = DP - R1 - (FMAX * DT * (1.0 - ((1.0-(DP-R1)/(FMAX*DT))**(BB+1.0))))
    else
       R2 = DP - R1 - (FMAX*DT)
    endif

    if ( R2 < 0.0) R2 =0.0

  end subroutine RunoffInfilExcessDynamicVic

end module RunoffSurfaceExcessDynamicVicMod
