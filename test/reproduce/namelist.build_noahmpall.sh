# define namelist for NoahMP
export namelist_input="
&timing ! and output
  dt              = 600.0   ! timestep [seconds]
  maxtime         = 30      ! total simulation time [hours]
  output_filename = 'output.nc'
  runsnow         = ${snowopt} ! true or false whether to run snow cases
  JULIAN          = 180.0
/

&forcing
  rainrate      = 20.0    ! rain rate, when raining [mm/hr]
  rain_duration = 3       ! duration of rain events [hours]
  dry_duration  = 3       ! duration of no-rain events [hours]
  raining       = .true.  ! start with rain
  uwind         = 6.0     ! wind speed m/s
  vwind         = 6.0     ! wind speed m/s
  sfcpres       = 90000.0 ! surface pressure Pa
  Q2            = 0.01
  SWDOWN        = 500.0
  LWDOWN        = 300.0
/

&structure
 isltyp           = ${soiltp}       ! soil texture class
 vegtype          = ${vegetp}       ! vegetation type modis
 soilcolor        = 4               ! color to decide soil albedo
 slopetype        = 1               ! slope factor for underground runoff
 croptype         = ${croptp}       ! no crop
 nsoil            = 4               ! number of soil levels
 nsnow            = 3               ! number of snow levels
 structure_option = 1               ! 1: use preset zsoil; 2: uniform levels
 soil_depth       = 2.0             ! total soil thickness [m] for structure_option > 1
 vegfra           = 30              ! vegetation fraction 50%
 vegmax           = 60              ! Vegetation fraction annual max [0-1]
 shdmax           = 60              ! yearly max vegetation fraction
 zlvl             = 10.0            ! reference height (m)
/

&fixed_initial
 zsoil     = -0.1, -0.4, -1.0, -2.0  ! depth to level interface [m]
/

&uniform_initial
 initial_uniform    = .true.         ! initial all levels the same
 initial_sh2o_value = 0.3            ! constant soil liquid value [vol]
/

&options
idveg       = ${dvegtp}
iopt_crs    = ${crstp}
iopt_btr    = ${btrtp}
iopt_runsrf = ${runtp}
iopt_runsub = ${runtp}
iopt_sfc    = ${sfctp}
iopt_frz    = ${frztp}
iopt_inf    = ${inftp}
iopt_rad    = ${radtp}
iopt_alb    = ${albtp}
iopt_snf    = ${snftp}
iopt_tbot   = ${tbottp}
iopt_stc    = ${stctp}
iopt_rsf    = ${rsftp}
iopt_soil   = 1
iopt_pedo   = 1
iopt_crop   = ${icroptp}
iopt_irr    = ${irrtp}
iopt_irrm   = ${irrmtp}
iopt_infdv  = ${infdvtp}   ! only for runoff=8
iopt_tdrn   = ${tdrntp}    ! drainage only for runoff=3
iopt_tksno  = 1
/

"
