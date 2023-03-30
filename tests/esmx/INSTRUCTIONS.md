# ESMX WRF-Hydro Test Application Instructions

## Build
Include WRFHYDRO in ESMX Build configuration file
```
  WRFHYDRO:
    source_dir: WRFHYDRO/trunk/NDHMS
    include_dir: WRFHYDRO/mods
    fort_module: wrfhydro_nuopc.mod
    libraries: wrfhydro_nuopc hydro_driver hydro_orchestrator hydro_routing hydro_utils hydro_mpp hydro_debug_utils hydro_routing_overland hydro_routing_subsurface hydro_data_rec hydro_routing_reservoirs_levelpool hydro_routing_reservoirs_hybrid hydro_routing_reservoirs_rfc hydro_routing_reservoirs hydro_netcdf_layer
    build_args: -DWRF_HYDRO=1 -DWRF_HYDRO_NUOPC=1
    test_dir: WRFHYDRO/tests/esmx
```

## Configuration
Edit esmxRun.config

### Component List
Provide list of component names to ESMX Driver.
```
ESMX_component_list: LND HYD
```

### ESMX Data Component
Configure the ESMX Data component
```
LND_model: ESMX_Data
```

| LND\_attributes | Value                    | Description                   |
| --------------- | ------------------------ | ----------------------------- |
| Verbosity       | max                      | Enable all generic output     |
| Diagnostic      | 0                        | No diagnostic output          |
| coordSys        | ESMF\_COORDSYS\_SPH\_DEG | Set coordinates using degrees |
| minx            | -73.916667               | Western longitude of Croton   |
| maxx            | -73.616667               | Eastern longitude of Croton   |
| miny            | 41.350000                | Southern latitude of Croton   |
| maxy            | 41.566667                | Northern latitude of Croton   |

| LND\_import\_fields                            | dim | min | max    |
| ---------------------------------------------- | --- | --- | ------ |
| soil\_moisture\_fraction\_layer\_1             | 2   | 0   | 1      |
| soil\_moisture\_fraction\_layer\_2             | 2   | 0   | 1      |
| soil\_moisture\_fraction\_layer\_3             | 2   | 0   | 1      |
| soil\_moisture\_fraction\_layer\_4             | 2   | 0   | 1      |
| liquid\_fraction\_of\_soil\_moisture\_layer\_1 | 2   | 0   | 1      |
| liquid\_fraction\_of\_soil\_moisture\_layer\_2 | 2   | 0   | 1      |
| liquid\_fraction\_of\_soil\_moisture\_layer\_3 | 2   | 0   | 1      |
| liquid\_fraction\_of\_soil\_moisture\_layer\_4 | 2   | 0   | 1      |
| surface\_water\_depth                          | 2   | 0   | 1      |

| LND\_export\_fields                            | dim | fill value |
| ---------------------------------------------- | --- | ---------- |
| soil\_moisture\_fraction\_layer\_1             | 2   | 0.2        |
| soil\_moisture\_fraction\_layer\_2             | 2   | 0.2        |
| soil\_moisture\_fraction\_layer\_3             | 2   | 0.2        |
| soil\_moisture\_fraction\_layer\_4             | 2   | 0.2        |
| liquid\_fraction\_of\_soil\_moisture\_layer\_1 | 2   | 0.2        |
| liquid\_fraction\_of\_soil\_moisture\_layer\_2 | 2   | 0.2        |
| liquid\_fraction\_of\_soil\_moisture\_layer\_3 | 2   | 0.2        |
| liquid\_fraction\_of\_soil\_moisture\_layer\_4 | 2   | 0.2        |
| soil\_temperature\_layer\_1                    | 2   | 288        |
| soil\_temperature\_layer\_2                    | 2   | 288        |
| soil\_temperature\_layer\_3                    | 2   | 288        |
| soil\_temperature\_layer\_4                    | 2   | 288        |
| time\_step\_infiltration\_excess               | 2   | 0          |
| soil\_column\_drainage                         | 2   | 0          |

### WRF-Hydro Component
Configure the WRF-Hydro component
```
HYD_model: WRFHYDRO
```

| HYD\_attributes      | Description                        |
| -------------------- | ---------------------------------- |
| Verbosity            | integer interpreted as a bit field |
| Diagnostic           | integer interpreted as a bit field |
| Profiling            | turn on NUOPC profiling            |
| realize\_all\_export | do no remove export fields         |
| config\_file         | hydro namelist file                |
| das\_config\_file    | hrldas configuration file          |
| time\_step           | override time step                 |
| forcings\_directory  | directory for forcing files        |
| domain\_id           | domain identifier                  |
| nest\_to\_nest       | use domain id for nest id          |
| import\_dependency   | initialize using import dependency |
| write\_restart       | write out cap restart file         |
| read\_restart        | read in cap restart file           |
| input\_directory     | WRF-Hydro cap input directory      |
| output\_directory    | WRF-Hydro cap output directory     |


## Execution
```
mpirun -np 4 ./esmx
```

## Slurm Workload Manager
1. edit slurm\_template.sh
    - partition=\<partition\_names\>
    - account=\<account\>
    - constraint=\<list\>
    - qos=\<qos\>
    - setup environment as needed
```
sbatch slurm\_template.sh
```

## PBS Workload Manager
1. edit pbs\_template.sh
    - \-A \<account\>
    - \-q \<queue\>
    - setup environment as needed
```
qsub pbs\_template.sh
```

## Validation
Successful execution produces evaptrans, press, and satur files through 00003.

[Slurm Documentation](https://slurm.schedmd.com/documentation.html)

[PBS Pro Documentation](https://www.altair.com/pbs-works-documentation)
