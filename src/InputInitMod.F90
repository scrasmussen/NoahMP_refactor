module InputInitMod

!!! Initialize Noah-MP input variables (2D forcing, namelist, table, static)
!!! Input variables should be first defined in InputType.f90

  use Machine, only : kind_noahmp
  use InputType

  implicit none

contains

!=== initialize with default values
  subroutine InputInitDefault(input)

    type(input_type), intent(inout) :: input

    associate (                         &
               XSTART => input%XSTART  ,&
               XEND   => input%XEND    ,&
               KDS    => input%KDS     ,&
               KDE    => input%KDE     ,&
               YSTART => input%YSTART  ,&
               YEND   => input%YEND     &
               )

    !domain variables allocations
    allocate( input%COSZIN   (XSTART:XEND,YSTART:YEND) )

    !met-forcings allocate
    allocate( input%U_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) ) 
    allocate( input%V_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) ) 
    allocate( input%T3D       (XSTART:XEND,KDS:KDE,YSTART:YEND) ) 
    allocate( input%QV3D      (XSTART:XEND,KDS:KDE,YSTART:YEND) ) 
    allocate( input%P8W3D     (XSTART:XEND,KDS:KDE,YSTART:YEND) ) 
    allocate( input%SWDOWN    (XSTART:XEND,YSTART:YEND) )    
    allocate( input%SWDDIF    (XSTART:XEND,YSTART:YEND) )   
    allocate( input%SWDDIR    (XSTART:XEND,YSTART:YEND) )   
    allocate( input%GLW       (XSTART:XEND,YSTART:YEND) )   
    allocate( input%PRECIP_IN (XSTART:XEND,YSTART:YEND) )   
    allocate( input%SR        (XSTART:XEND,YSTART:YEND) )  
    allocate( input%MP_RAINC  (XSTART:XEND,YSTART:YEND) )   
    allocate( input%MP_RAINNC (XSTART:XEND,YSTART:YEND) )   
    allocate( input%MP_SHCV   (XSTART:XEND,YSTART:YEND) )   
    allocate( input%MP_SNOW   (XSTART:XEND,YSTART:YEND) )   
    allocate( input%MP_GRAUP  (XSTART:XEND,YSTART:YEND) )   
    allocate( input%MP_HAIL   (XSTART:XEND,YSTART:YEND) )
    allocate( input%QSFC      (XSTART:XEND,YSTART:YEND) )
    allocate( input%TMN       (XSTART:XEND,YSTART:YEND) )


    !domain variables initialize
    input%COSZIN    (XSTART:XEND,YSTART:YEND)         = huge(1.0)

    !met-forcing initialize
    input%U_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) = huge(1.0)
    input%V_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) = huge(1.0) 
    input%T3D       (XSTART:XEND,KDS:KDE,YSTART:YEND) = huge(1.0)
    input%QV3D      (XSTART:XEND,KDS:KDE,YSTART:YEND) = huge(1.0)
    input%P8W3D     (XSTART:XEND,KDS:KDE,YSTART:YEND) = huge(1.0)
    input%SWDOWN    (XSTART:XEND,YSTART:YEND)         = huge(1.0) 
    input%SWDDIF    (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%SWDDIR    (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%GLW       (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%PRECIP_IN (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%SR        (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%MP_RAINC  (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%MP_RAINNC (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%MP_SHCV   (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%MP_SNOW   (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%MP_GRAUP  (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%MP_HAIL   (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%QSFC      (XSTART:XEND,YSTART:YEND)         = huge(1.0)
    input%TMN       (XSTART:XEND,YSTART:YEND)         = huge(1.0)

    endassociate

  end subroutine InputInitDefault

end module InputInitMod
