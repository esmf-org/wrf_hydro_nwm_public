#define FILENAME "WRFHydro_NUOPC_Fields.F90"
#define MODNAME "wrfhydro_nuopc_fields"
#include "WRFHydro_NUOPC_Macros.h"

module wrfhydro_nuopc_fields
! !MODULE: wrfhydro_nuopc_fields
!
! !DESCRIPTION:
!   This module connects NUOPC field information for WRFHYDRO
!
! !REVISION HISTORY:
!  21Jul23    Dan Rosen  Initial Specification
!
! !USES:
  use ESMF
  use NUOPC
  use WRFHydro_ESMF_Extensions
  use config_base,      only: nlst
  use module_rt_data,   only: rt_domain
  use overland_data,    only: overland_struct
  use overland_control, only: overland_control_struct

  implicit none

  private

  type cap_fld_type
    sequence
    character(len=64)           :: sd_name    = "dummy" ! standard name
    character(len=64)           :: st_name    = "dummy" ! state name
    character(len=64)           :: units      = "-"     ! units
    logical                     :: ad_import  = .FALSE. ! advertise import
    logical                     :: ad_export  = .FALSE. ! advertise export
    logical                     :: rl_import  = .FALSE. ! realize import
    logical                     :: rl_export  = .FALSE. ! realize export
    real(ESMF_KIND_R8)          :: vl_default = ESMF_DEFAULT_VALUE ! default
  end type cap_fld_type

  type(cap_fld_type),target,dimension(20) :: cap_fld_list = (/          &
    cap_fld_type("inst_total_soil_moisture_content        ","smc     ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("inst_soil_moisture_content              ","slc     ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("inst_soil_temperature                   ","stc     ", &
                 "K     ",.TRUE. ,.FALSE.),                             &
    cap_fld_type("liquid_fraction_of_soil_moisture_layer_1","sh2ox1  ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("liquid_fraction_of_soil_moisture_layer_2","sh2ox2  ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("liquid_fraction_of_soil_moisture_layer_3","sh2ox3  ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("liquid_fraction_of_soil_moisture_layer_4","sh2ox4  ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("soil_moisture_fraction_layer_1          ","smc1    ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("soil_moisture_fraction_layer_2          ","smc2    ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("soil_moisture_fraction_layer_3          ","smc3    ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("soil_moisture_fraction_layer_4          ","smc4    ", &
                 "m3 m-3",.TRUE. ,.TRUE. ),                             &
    cap_fld_type("soil_temperature_layer_1                ","stc1    ", &
                 "K     ",.TRUE. ,.FALSE.),                             &
    cap_fld_type("soil_temperature_layer_2                ","stc2    ", &
                 "K     ",.TRUE. ,.FALSE.),                             &
    cap_fld_type("soil_temperature_layer_3                ","stc3    ", &
                 "K     ",.TRUE. ,.FALSE.),                             &
    cap_fld_type("soil_temperature_layer_4                ","stc4    ", &
                 "K     ",.TRUE. ,.FALSE.),                             &
    cap_fld_type("soil_porosity                           ","smcmax1 ", &
                 "1     ",.FALSE.,.FALSE.),                             &
    cap_fld_type("vegetation_type                         ","vegtyp  ", &
                 "1     ",.FALSE.,.FALSE.),                             &
    cap_fld_type("surface_water_depth                     ","sfchead ", &
                 "mm    ",.FALSE.,.TRUE. ),                             &
    cap_fld_type("time_step_infiltration_excess           ","infxsrt ", &
                 "mm    ",.TRUE. ,.FALSE.),                             &
    cap_fld_type("soil_column_drainage                    ","soldrain", &
                 "mm    ",.TRUE. ,.FALSE.)                              &
    /)

  public cap_fld_list
  public field_dictionary_add
  public field_create
  public field_realize
  public field_advertise
  public check_lsm_forcings
  public field_advertise_log
  public field_realize_log
  public read_impexp_config_flnm
  public field_find_standardname
  public field_find_statename
  public field_fill_state

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_dictionary_add"
  subroutine field_dictionary_add(fieldList, rc)
    type(cap_fld_type), intent(in) :: fieldList(:)
    integer, intent(out)           :: rc
    ! local variables
    integer :: n
    logical :: isPresent

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      isPresent = NUOPC_FieldDictionaryHasEntry( &
        fieldList(n)%sd_name, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry( &
          StandardName=trim(fieldList(n)%sd_name), &
          canonicalUnits=trim(fieldList(n)%units), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
    end do

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_realize"
  subroutine field_realize(fieldList, importState, exportState, grid, &
  did, realizeAllImport, realizeAllExport, rc)
    type(cap_fld_type), intent(inout) :: fieldList(:)
    type(ESMF_State), intent(inout)   :: importState
    type(ESMF_State), intent(inout)   :: exportState
    type(ESMF_Grid), intent(in)       :: grid
    integer, intent(in)               :: did
    logical, intent(in)               :: realizeAllImport
    logical, intent(in)               :: realizeAllExport
    integer, intent(out)              :: rc
    ! local variables
    integer :: n
    logical :: realizeImport
    logical :: realizeExport
    type(ESMF_Field) :: field_import
    type(ESMF_Field) :: field_export

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      ! check realize import
      if (fieldList(n)%ad_import) then
        if (realizeAllImport) then
          realizeImport = .true.
        else
          realizeImport = NUOPC_IsConnected(importState, &
            fieldName=trim(fieldList(n)%st_name),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        end if
      else
        realizeImport = .false.
      end if
      ! create import field
      if ( realizeImport ) then
        field_import=field_create(fld_name=fieldList(n)%st_name, &
          grid=grid, did=did, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(importState, field=field_import, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_import = .true.
      else
        call ESMF_StateRemove(importState, (/fieldList(n)%st_name/), &
          relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_import = .false.
      end if

      ! check realize export
      if (fieldList(n)%ad_export) then
        if (realizeAllExport) then
          realizeExport = .true.
        else
          realizeExport = NUOPC_IsConnected(exportState, &
            fieldName=trim(fieldList(n)%st_name),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        end if
      else
        realizeExport = .false.
      end if
      ! create export field
      if( realizeExport ) then
        field_export=field_create(fld_name=fieldList(n)%st_name, &
          grid=grid, did=did, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(exportState, field=field_export, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_export = .true.
      else
        call ESMF_StateRemove(exportState, (/fieldList(n)%st_name/), &
          relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_export = .false.
      end if
    end do
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "lsm_forcings"

  function check_lsm_forcings(importState,rc)
    ! RETURN
    logical :: check_lsm_forcings
    ! ARGUMENTS
    type(ESMF_State), intent(in) :: importState
    integer, intent(out)         :: rc
    ! LOCAL VARIABLES
    integer                    :: fieldIndex
    type(ESMF_StateItem_Flag)  :: itemType
    integer                    :: s_smc, s_smc1, s_smc2, s_smc3, s_smc4
    integer                    :: s_slc, s_slc1, s_slc2, s_slc3, s_slc4
    integer                    :: s_stc, s_stc1, s_stc2, s_stc3, s_stc4
    integer                    :: s_infxsrt
    integer                    :: s_soldrain
    logical                    :: c_smc
    logical                    :: c_slc
    logical                    :: c_stc
    logical                    :: c_infxsrt
    logical                    :: c_soldrain

    ! total soil moisture content
    call ESMF_StateGet(importState,itemSearch="smc", itemCount=s_smc, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="smc1",itemCount=s_smc1,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="smc2",itemCount=s_smc2,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="smc3",itemCount=s_smc3,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="smc4",itemCount=s_smc4,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_smc.gt.0) then
      c_smc = NUOPC_IsConnected(importState, fieldName="smc")
    elseif ((s_smc1.gt.0) .and. (s_smc2.gt.0) .and. &
            (s_smc3.gt.0) .and. (s_smc4.gt.0)) then
      c_smc = (NUOPC_IsConnected(importState, fieldName="smc1") .and. &
               NUOPC_IsConnected(importState, fieldName="smc2") .and. &
               NUOPC_IsConnected(importState, fieldName="smc3") .and. &
               NUOPC_IsConnected(importState, fieldName="smc4"))
    else
      c_smc = .false.
    endif

    ! liquid soil moisture content
    call ESMF_StateGet(importState,itemSearch="slc", itemCount=s_slc, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="slc1",itemCount=s_slc1,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="slc2",itemCount=s_slc2,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="slc3",itemCount=s_slc3,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="slc4",itemCount=s_slc4,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_slc.gt.0) then
      c_slc = NUOPC_IsConnected(importState, fieldName="slc")
    elseif ((s_slc1.gt.0) .and. (s_slc2.gt.0) .and. &
            (s_slc3.gt.0) .and. (s_slc4.gt.0)) then
      c_slc = (NUOPC_IsConnected(importState, fieldName="slc1") .and. &
               NUOPC_IsConnected(importState, fieldName="slc2") .and. &
               NUOPC_IsConnected(importState, fieldName="slc3") .and. &
               NUOPC_IsConnected(importState, fieldName="slc4"))
    else
      c_slc = .false.
    endif

    ! soil temperature
    call ESMF_StateGet(importState,itemSearch="stc", itemCount=s_stc, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="stc1",itemCount=s_stc1,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="stc2",itemCount=s_stc2,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="stc3",itemCount=s_stc3,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="stc4",itemCount=s_stc4,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_stc.gt.0) then
      c_stc = NUOPC_IsConnected(importState, fieldName="stc")
    elseif ((s_stc1.gt.0) .and. (s_stc2.gt.0) .and. &
            (s_stc3.gt.0) .and. (s_stc4.gt.0)) then
      c_stc = (NUOPC_IsConnected(importState, fieldName="stc1") .and. &
               NUOPC_IsConnected(importState, fieldName="stc2") .and. &
               NUOPC_IsConnected(importState, fieldName="stc3") .and. &
               NUOPC_IsConnected(importState, fieldName="stc4"))
    else
      c_stc = .false.
    endif

    ! infiltration excess
    call ESMF_StateGet(importState,itemSearch="infxsrt",itemCount=s_infxsrt,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_infxsrt.gt.0) then
      c_infxsrt = NUOPC_IsConnected(importState, fieldName="infxsrt")
    else
      c_infxsrt = .false.
    endif

    ! soil drainage
    call ESMF_StateGet(importState,itemSearch="soldrain",itemCount=s_soldrain,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_soldrain.gt.0) then
      c_soldrain = NUOPC_IsConnected(importState, fieldName="soldrain")
    else
      c_soldrain = .false.
    endif

    check_lsm_forcings = c_smc .and. c_slc .and. c_stc .and. &
                         c_infxsrt .and. c_soldrain

  end function

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_advertise"
  subroutine field_advertise(fieldList, importState, exportState, &
  transferOffer, rc)
    type(cap_fld_type), intent(in)    :: fieldList(:)
    type(ESMF_State), intent(inout)   :: importState
    type(ESMF_State), intent(inout)   :: exportState
    character(*), intent(in),optional :: transferOffer
    integer, intent(out)              :: rc
    ! local variables
    integer :: n

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%ad_import) then
        call NUOPC_Advertise(importState, &
          StandardName=fieldList(n)%sd_name, &
          Units=fieldList(n)%units, &
          TransferOfferGeomObject=transferOffer, &
          name=fieldList(n)%st_name, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
      if (fieldList(n)%ad_export) then
        call NUOPC_Advertise(exportState, &
          StandardName=fieldList(n)%sd_name, &
          Units=fieldList(n)%units, &
          TransferOfferGeomObject=transferOffer, &
          name=fieldList(n)%st_name, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
    end do

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_advertise_log"
  subroutine field_advertise_log(fieldList, cname, rc)
    type(cap_fld_type), intent(in) :: fieldList(:)
    character(*), intent(in)       :: cname
    integer, intent(out)           :: rc
    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: n
    character(32)              :: label
    character(ESMF_MAXSTR)     :: logMsg

    rc = ESMF_SUCCESS

    label = trim(cname)

    ! count advertised import and export fields
    cntImp = 0
    cntExp = 0
    do n = lbound(fieldList,1), ubound(fieldList,1)
      if (fieldList(n)%ad_import) cntImp = cntImp + 1
      if (fieldList(n)%ad_export) cntExp = cntExp + 1
    enddo

    ! log advertised import fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of advertised import fields(',cntImp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%ad_import) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntImp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

    ! log advertised export fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of advertised export fields(',cntExp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%ad_export) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntExp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_realize_log"
  subroutine field_realize_log(fieldList, cname, rc)
    type(cap_fld_type), intent(in) :: fieldList(:)
    character(*), intent(in)       :: cname
    integer, intent(out)           :: rc
    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: n
    character(32)              :: label
    character(ESMF_MAXSTR)     :: logMsg

    rc = ESMF_SUCCESS

    label = trim(cname)

    ! count realized import and export fields
    cntImp = 0
    cntExp = 0
    do n = lbound(fieldList,1), ubound(fieldList,1)
      if (fieldList(n)%rl_import) cntImp = cntImp + 1
      if (fieldList(n)%rl_export) cntExp = cntExp + 1
    enddo

    ! log realized import fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of realized import fields(',cntImp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%rl_import) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntImp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

    ! log realized export fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of realized export fields(',cntExp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%rl_export) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntExp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "read_impexp_config_flnm"
  subroutine read_impexp_config_flnm(fname, fieldList, rc)
    character(len=30),intent(in)     :: fname
    type(cap_fld_type),intent(inout) :: fieldList(:)
    integer,intent(out)              :: rc

    ! local variables
    type(ESMF_Config)                  :: fieldsConfig
    type(NUOPC_FreeFormat)             :: attrFF
    integer                            :: lineCount
    integer                            :: tokenCount
    character(len=NUOPC_FreeFormatLen),allocatable :: tokenList(:)
    integer                            :: i,j
    integer                            :: stat

    rc = ESMF_SUCCESS

!   load fname into fieldsConfig
    fieldsConfig = ESMF_ConfigCreate(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_ConfigLoadFile(fieldsConfig, fname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

!   read export fields from config
    attrFF = NUOPC_FreeFormatCreate(fieldsConfig, &
      label="hyd_fields", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_FreeFormatGet(attrFF, lineCount=lineCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    do i=1, lineCount
      call NUOPC_FreeFormatGetLine(attrFF, line=i, &
        tokenCount=tokenCount, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (.not.((tokenCount.eq.5).or.(tokenCount.eq.6))) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="Malformed ocn_export_fields item FORMAT="// &
            "'STATE_NAME' 'STANDARD_NAME' 'UNITS' 'IMPORT' 'EXPORT' "// &
!            "['FILLVAL'] "// &
            "in file: "//trim(fname), &
          CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(tokenList(tokenCount))
      call NUOPC_FreeFormatGetLine(attrFF, line=i, tokenList=tokenList, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call field_find_statename(fieldList, tokenList(1), location=j, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      fieldList(j)%st_name=tokenList(1)
      fieldList(j)%sd_name=tokenList(2)
      fieldList(j)%units=tokenList(3)
      tokenList(4) = ESMF_UtilStringUpperCase(tokenList(4), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      fieldList(j)%ad_import=((tokenList(4).eq.".TRUE.") .or. &
                         (tokenList(4).eq."TRUE"))
      tokenList(5) = ESMF_UtilStringUpperCase(tokenList(5), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      fieldList(j)%ad_export=((tokenList(5).eq.".TRUE.") .or. &
                         (tokenList(5).eq."TRUE"))
      if (tokenCount.eq.6) then
        fieldList(j)%vl_default = ESMF_UtilString2Real(tokenList(6), rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      deallocate(tokenList)
    enddo

!   cleanup
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_ConfigDestroy(fieldsConfig, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

  end subroutine read_impexp_config_flnm

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_find_standardname"
  subroutine field_find_standardname(fieldList, standardName, location, &
  defaultValue, rc)
    type(cap_fld_type), intent(in)          :: fieldList(:)
    character(len=64), intent(in)           :: standardName
    integer, intent(out), optional          :: location
    real(ESMF_KIND_R8),intent(out),optional :: defaultValue
    integer, intent(out)                    :: rc
    ! local variables
    integer :: n

    rc = ESMF_RC_NOT_FOUND

    if (present(location)) location = lbound(fieldList,1) - 1
    if (present(defaultValue)) defaultValue = ESMF_DEFAULT_VALUE

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%sd_name .eq. standardName) then
        if (present(location)) location = n
        if (present(defaultValue)) defaultValue = fieldList(n)%vl_default
        rc = ESMF_SUCCESS
        return
      end if
    end do

    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Field not found in fieldList "//trim(standardName), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_find_statename"
  subroutine field_find_statename(fieldList, stateName, location, &
  defaultValue, rc)
    type(cap_fld_type), intent(in)          :: fieldList(:)
    character(len=64), intent(in)           :: stateName
    integer, intent(out), optional          :: location
    real(ESMF_KIND_R8),intent(out),optional :: defaultValue
    integer, intent(out)                    :: rc
    ! local variables
    integer :: n

    rc = ESMF_RC_NOT_FOUND

    if (present(location)) location = lbound(fieldList,1) - 1
    if (present(defaultValue)) defaultValue = ESMF_DEFAULT_VALUE

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%st_name .eq. stateName) then
        if (present(location)) location = n
        if (present(defaultValue)) defaultValue = fieldList(n)%vl_default
        rc = ESMF_SUCCESS
        return
      end if
    end do

    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Field not found in fieldList "//trim(stateName), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_create"
  function field_create(fld_name,grid,did,rc)
    ! return value
    type(ESMF_Field) :: field_create
    ! arguments
    character(*), intent(in)      :: fld_name
    type(ESMF_Grid), intent(in)   :: grid
    integer, intent(in)           :: did
    integer,          intent(out) :: rc
    ! local variables

    rc = ESMF_SUCCESS

    select case (trim(fld_name))
      case ('smc')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%smc(:,:,:), gridToFieldMap=(/1,2/), &
          ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('slc')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%sh2ox(:,:,:), gridToFieldMap=(/1,2/), &
          ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('stc')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%stc(:,:,:), gridToFieldMap=(/1,2/), &
          ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('sh2ox1')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%sh2ox(:,:,1), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('sh2ox2')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%sh2ox(:,:,2), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('sh2ox3')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%sh2ox(:,:,3), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('sh2ox4')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%sh2ox(:,:,4), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('smc1')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%smc(:,:,1), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('smc2')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%smc(:,:,2), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('smc3')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%smc(:,:,3), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('smc4')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%smc(:,:,4), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('smcmax1')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%smcmax1, &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('stc1')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%stc(:,:,1), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('stc2')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%stc(:,:,2), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('stc3')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%stc(:,:,3), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('stc4')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%stc(:,:,4), &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('vegtyp')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%vegtyp, &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('sfchead')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%overland%control%surface_water_head_lsm, &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      case ('infxsrt')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%infxsrt, &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      case ('soldrain')
        field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
          farray=rt_domain(did)%soldrain, &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      case default
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg=METHOD//": Field hookup missing: "//trim(fld_name), &
          file=FILENAME,rcToReturn=rc)
        return  ! bail out
    end select

  end function

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_fill_state"
  subroutine field_fill_state(state, fieldList, fillValue, rc)
    type(ESMF_State), intent(inout)          :: state
    type(cap_fld_type), intent(in), optional :: fieldList(:)
    real(ESMF_KIND_R8), intent(in), optional :: fillValue
    integer, intent(out)                     :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    real(ESMF_KIND_R8)                     :: defaultValue
    integer                                :: stat

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (.not. present(fillValue)) then
      do n=1, itemCount
        if ( itemTypeList(n) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, field=field, &
            itemName=itemNameList(n),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          call ESMF_FieldFill(field, dataFillScheme="const", &
            const1=0.0_ESMF_KIND_R8, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        endif
      enddo
    else
      do n=1, itemCount
        if ( itemTypeList(n) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, field=field, &
            itemName=itemNameList(n),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          call ESMF_FieldFill(field, dataFillScheme="const", &
            const1=fillValue, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        endif
      enddo
    endif

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine field_fill_state

  !-----------------------------------------------------------------------------

end module wrfhydro_nuopc_fields
