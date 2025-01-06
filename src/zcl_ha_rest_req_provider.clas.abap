class ZCL_HA_REST_REQ_PROVIDER definition
  public
  inheriting from CL_REST_RESOURCE
  final
  create public .

public section.

  methods IF_REST_RESOURCE~GET
    redefinition .
  methods IF_REST_RESOURCE~POST
    redefinition .
protected section.

  methods GET_WEATHER .
  methods GET_TOKEN .
  methods WHOAMI
    importing
      value(NAME) type SYST_UNAME default SY-UNAME .
  methods GET_LIST .
  methods ADD_TO_LIST .
  methods SET_BOUGHT .
  methods ERROR
    importing
      !STATUS type I default 200
      !REASON type STRING default 'General error' .
  methods DELETE_FROM_LIST .
  methods SET_LIST .
private section.
ENDCLASS.



CLASS ZCL_HA_REST_REQ_PROVIDER IMPLEMENTATION.


  method ADD_TO_LIST.

    " Setting the response type for JSON
    TYPES: ty_response type zha_list.

    data: ls_list type ty_response.

    data(lt_parameters) = MO_REQUEST->GET_URI_QUERY_PARAMETERS( iv_encoded = abap_false ).

    LOOP AT lt_parameters ASSIGNING FIELD-SYMBOL(<fs_param>).
      ASSIGN COMPONENT <fs_param>-name OF STRUCTURE ls_list to FIELD-SYMBOL(<fs_key>) ELSE UNASSIGN.
      IF <fs_key> is ASSIGNED.
        <fs_key> = <fs_param>-value.
      ENDIF.
    ENDLOOP.

    modify zha_list from ls_list.
    if sy-subrc eq 0.

    " This is where we convert our response to JSON data
    /ui2/cl_json=>serialize(
      EXPORTING
        data  = ls_list
      RECEIVING
        r_json = DATA(lv_response) ).

      " The line below sets the body of our message. In this case the JSON data we created
      mo_response->create_entity( )->set_string_data( iv_data = lv_response ).

      " We are telling the browser to understand what kind of data we sent to it.
      " Here we say 'We send you a JSON data' to browser
      mo_response->set_header_field( iv_name = if_http_header_fields=>content_type iv_value = |application/json| ).
    else.
    endif.
  endmethod.


  method DELETE_FROM_LIST.
    data(lv_item) = MO_REQUEST->GET_URI_QUERY_PARAMETER( iv_name = 'item' ).

    if lv_item is initial.
      mo_response->set_status( iv_status = 400 iv_reason_phrase = 'Please provide item name' ).
      mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).
      mo_response->create_entity( )->set_string_data( iv_data = '{"error":"Please provide item name"}' ).
      exit.
    endif.

    select item, description, quantity, bought from zha_list where item = @lv_item into @data(ls_item).

      data: begin of ls_response,
              item_data type zha_list,
              status type string,
            end of ls_response.

      delete from zha_list where item = @lv_item.

      ls_response = value #( item_data = CORRESPONDING #( ls_item ) status = |Deleted| ).

      /ui2/cl_json=>serialize(
        EXPORTING
          data  = ls_response
        RECEIVING
          r_json = data(lv_response) ).

      mo_response->create_entity( )->set_string_data( iv_data = lv_response ).
      mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).

    endselect.
    if sy-subrc ne 0.
      mo_response->set_status( iv_status = 200 iv_reason_phrase = 'Item is not in the list' ).
      mo_response->create_entity( )->set_string_data( iv_data = 'Item is not in the list' ).
    endif.
  endmethod.


  method ERROR.
    mo_response->set_status( iv_status = status iv_reason_phrase = reason ).
    mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).
    mo_response->create_entity( )->set_string_data( iv_data = |\{"error":"{ reason }"\}| ).
  endmethod.


  method GET_LIST.

    select item, description, quantity, bought from zha_list into table @data(lt_list).

    /ui2/cl_json=>serialize(
      EXPORTING
        data  = lt_list
      RECEIVING
        r_json = data(lv_response) ).

    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).
    mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).

  endmethod.


  method GET_TOKEN.
        types: begin of ty_response,
             user type bname,
             token type string,
           end of ty_response.

    data(ls_response) = value ty_response( user = sy-uname token = mo_response->get_header_field( 'x-csrf-token' ) ).

    /ui2/cl_json=>serialize(
      EXPORTING
        data  = ls_response
      RECEIVING
        r_json = data(lv_response) ).
    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).
    mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).
  endmethod.


  method GET_WEATHER.
    data: lt_lines type table of tline.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                            = 'ST'
        LANGUAGE                      = sy-langu
        NAME                          = 'ZHA_WEATHER'
        OBJECT                        = 'TEXT'
      TABLES
        LINES                         = lt_lines.

    IF SY-SUBRC <> 0.
*     Implement suitable error handling here
    ENDIF.

    data(lv_response) = reduce string( init str type string for line in lt_lines next str = str && line-tdline ).

    mo_response->set_header_field( iv_name = if_http_header_fields=>content_type iv_value = |text/html; charset=utf-8| ).
    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).
  endmethod.


  method IF_REST_RESOURCE~GET.
*CALL METHOD SUPER->IF_REST_RESOURCE~GET
*    .

    " We have some attributes in this class:
    " MO_REQUEST has everything we need to read from the HTTP request
    " MO_RESPONSE is what we will set to display/return data back to caller

    " Here we are reading path(endpoint) from the request. It'll return the part after
    " the service name.
    data(lv_path) = |{ MO_REQUEST->GET_URI_PATH( ) CASE = lower }|.

    "
    CASE lv_path.
      WHEN '/weather'. " This will redirect to open-meteo api to get weather information
        get_weather( ).
      WHEN '/' or space. " We will use root path for reading token to use in POST requests
        get_token( ).
      WHEN '/whoami'. " This endpoint will return the user information of logged in user
        whoami( ).
      WHEN '/whoisthis'. " This endpoint will query user information from parameter 'user'.
        data: lv_user type SYST_UNAME.
        lv_user = MO_REQUEST->GET_URI_QUERY_PARAMETER( iv_name = 'user' ).
        whoami( lv_user ).
      WHEN '/getlist'. " This endpoint will return the contents of our shopping list
        get_list( ).
      WHEN '/addtolist'. " This endpoint will add new items to our shopping list
        add_to_list( ).
      WHEN '/setbought'. " This endpoint will set/unset an item as bought
        set_bought( ).
      WHEN '/delete'. " This endpoint will delete the item from our shopping list
        delete_from_list( ).
    ENDCASE.

  endmethod.


  method IF_REST_RESOURCE~POST.
*CALL METHOD SUPER->IF_REST_RESOURCE~POST
*  EXPORTING
*    IO_ENTITY =
*    .

    data(lv_path) = |{ MO_REQUEST->GET_URI_PATH( ) CASE = lower }|.

    "
    CASE lv_path.
      WHEN '/setlist'. " This endpoint will set the contents of our shopping list
        set_list( ).
    ENDCASE.


  endmethod.


  method SET_BOUGHT.

    data(lv_item) = MO_REQUEST->GET_URI_QUERY_PARAMETER( iv_name = 'item' ).

    if lv_item is initial.
      mo_response->set_status( iv_status = 400 iv_reason_phrase = 'Please provide item name' ).
      mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).
      mo_response->create_entity( )->set_string_data( iv_data = '{"error":"Please provide item name"}' ).
      exit.
    endif.

    select item, description, quantity, bought from zha_list where item = @lv_item into @data(ls_response).
      if ls_response-bought eq space.
        ls_response-bought = abap_true.
      else.
        ls_response-bought = space.
      endif.

      update zha_list set bought = ls_response-bought where item = lv_item.

      /ui2/cl_json=>serialize(
        EXPORTING
          data  = ls_response
        RECEIVING
          r_json = data(lv_response) ).

      mo_response->create_entity( )->set_string_data( iv_data = lv_response ).
      mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).

    endselect.
    if sy-subrc ne 0.
      mo_response->set_status( iv_status = 200 iv_reason_phrase = 'Item is not in the list' ).
      mo_response->create_entity( )->set_string_data( iv_data = 'Item is not in the list' ).
    endif.

  endmethod.


  method SET_LIST.

    data: lt_request type table of zha_list with empty key.

    data(lv_request) = mo_request->get_entity( )->get_string_data( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_request
      CHANGING
        data = lt_request
     ).

      data: begin of ls_response,
              item_data type table of zha_list with EMPTY KEY,
              status type string,
            end of ls_response.


    ls_response = value #( item_data = CORRESPONDING #( lt_request ) status = |Set| ).


    /ui2/cl_json=>serialize(
      EXPORTING
        data  = ls_response
      RECEIVING
        r_json = data(lv_response) ).


    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).
    mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).


  endmethod.


  METHOD whoami.

    " Setting the response type for JSON
    TYPES: BEGIN OF ty_response,
             firstname TYPE ad_namefir,
             lastname  TYPE ad_namelas,
             e_mail    TYPE ad_smtpadr,
           END OF ty_response.

    DATA: ls_address TYPE bapiaddr3,
          lt_return  TYPE TABLE OF bapiret2.

    " Here we are giving the user name and reading
    " First name, last name and e-mail of the user
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = name
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    " We are assigning the values to our response structure
    DATA(ls_response) = CORRESPONDING ty_response( ls_address ).

    " This is where we convert our response to JSON data
    /ui2/cl_json=>serialize(
      EXPORTING
        data  = ls_response
      RECEIVING
        r_json = DATA(lv_response) ).

    " The line below sets the body of our message. In this case the JSON data we created
    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).

    " We are telling the browser to understand what kind of data we sent to it.
    " Here we say 'We send you a JSON data' to browser
    mo_response->set_header_field( iv_name = if_http_header_fields=>content_type iv_value = |application/json| ).
  ENDMETHOD.
ENDCLASS.
