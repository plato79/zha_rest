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
  methods GENERATE_QR .
  methods EMPTY_LIST .
private section.
ENDCLASS.



CLASS ZCL_HA_REST_REQ_PROVIDER IMPLEMENTATION.


  method ADD_TO_LIST.

    " Setting the response type for JSON
    TYPES: ty_response type zha_list.

    " We'll create an item line. We'll also use that for response.
    data: ls_list type ty_response.

    " We're reading all parameters from the request. It should include 'item' at least because
    " that's our key field on the table.
    data(lt_parameters) = MO_REQUEST->GET_URI_QUERY_PARAMETERS( iv_encoded = abap_false ).

    " Here we are converting our parameter table ( in key-value format ) to our
    " item structure.
    LOOP AT lt_parameters ASSIGNING FIELD-SYMBOL(<fs_param>).
      ASSIGN COMPONENT <fs_param>-name OF STRUCTURE ls_list to FIELD-SYMBOL(<fs_key>) ELSE UNASSIGN.
      IF <fs_key> is ASSIGNED.
        <fs_key> = <fs_param>-value.
      ENDIF.
    ENDLOOP.

    " Adding the item to our table. I'm executing modify here to
    " make sure this also updates if it's already in the table.
    " BTW, if you don't set 'item' in parameters, a record with empty key
    " will be created. So better check if ITEM field is set or not.
    " You can do that but we're ignoring it right now.
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

    " We're reading the 'item' parameter from the request uri
    data(lv_item) = MO_REQUEST->GET_URI_QUERY_PARAMETER( iv_name = 'item' ).

    if lv_item is initial. " Checking 'item' value
      " The item should be filled. We are giving error 400 ( BAD DATA )
      " and also a message for the caller.
      error( status = 400 reason = 'Please provide item name' ).
      exit.
    endif.

    " We query the item in this step. The item should be there to continue further.
    select item, description, quantity, bought from zha_list where item = @lv_item into @data(ls_item).

      " Now, we're sure that the item is there, we'll prepare the response.
      data: begin of ls_response,
              item_data type zha_list,
              status type string,
            end of ls_response.

      " First we're deleting it from the table.
      delete from zha_list where item = @lv_item.

      " Now, the item is no more. We should tell the caller
      " what we deleted.
      ls_response = value #( item_data = CORRESPONDING #( ls_item ) status = |Deleted| ).

      " Converting our response to JSON
      /ui2/cl_json=>serialize(
        EXPORTING
          data  = ls_response
        RECEIVING
          r_json = data(lv_response) ).

      " The line below sets the body of our message. In this case the JSON data we created
      mo_response->create_entity( )->set_string_data( iv_data = lv_response ).

    " We are telling the browser to understand what kind of data we sent to it.
    " Here we say 'We send you a JSON data' to browser
      mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).

    endselect.
    if sy-subrc ne 0. " The item is not in DB.
      " We're telling we got the message ( 200 ) and it's executed
      " but the item is not in the list.
      " This is still a problem, so we give error message.
      error( status = 200 reason = 'Item is not in the list' ).
    endif.
  endmethod.


  method ERROR. " This method will generate an error from HTTP status and reason.
    " The code below will modify the response so requester can act accordingly.
    " You can see this information on response headers
    mo_response->set_status( iv_status = status iv_reason_phrase = reason ).

    " We are telling the browser to understand what kind of data we sent to it.
    " Here we say 'We send you a JSON data' to browser
    mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).

    " We are also returning a JSON to ensure if it's called via an application
    " it could easily detect the problem without reading headers.
    mo_response->create_entity( )->set_string_data( iv_data = |\{"error":"{ reason }"\}| ).

  endmethod.


  method GET_LIST.

    " We are reading entire table and assigning it to an internal table
    select item, description, quantity, bought from zha_list into table @data(lt_list).

    " The command below will convert the table data to JSON.
    /ui2/cl_json=>serialize(
      EXPORTING
        data  = lt_list
      RECEIVING
        r_json = data(lv_response) ).

    " The line below sets the body of our message. In this case the JSON data we created
    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).

    " We are telling the browser to understand what kind of data we sent to it.
    " Here we say 'We send you a JSON data' to browser
    mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).

  endmethod.


  method GET_TOKEN. " This is actually needed for our POST requests.
    " Preparing our response structure
    types: begin of ty_response,
             user type bname,
             token type string,
           end of ty_response.

    " You may see an unusual thing here. The value for the token is read from our response(?!?)
    " You'd think 'Shouldn't we create the response?'
    " In this case, the request handler class has a special case.
    " If the request header has 'x-csrf-token=fetch' value,
    " the response header includes that token in again 'x-csrf-token' key.
    " This value is required for POST requests.
    data(ls_response) = value ty_response( user = sy-uname token = mo_response->get_header_field( 'x-csrf-token' ) ).

    " Converting our response to JSON
    /ui2/cl_json=>serialize(
      EXPORTING
        data  = ls_response
      RECEIVING
        r_json = data(lv_response) ).

    " The line below sets the body of our message. In this case the JSON data we created
    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).

    " We are telling the browser to understand what kind of data we sent to it.
    " Here we say 'We send you a JSON data' to browser
    mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).
  endmethod.


  method GET_WEATHER.

    " We're creating the table to read our TEXT into.
    data: lt_lines type table of tline.

    " The text we read below actually is an HTML file. Here's the actual content:

*<html>
*<head>
*<script>
*var lat = "";
*var long = "";
*if (navigator.geolocation) {
*
*    navigator.geolocation.getCurrentPosition(successFunction,
*                                             errorFunction );
*} else {
*    alert('It seems like Geolocation, which is required for this page, '
*         +'is not enabled in your browser. '
*         +'Please use a browser which supports it.');
*}
*function successFunction(position) {
*    lat = position.coords.latitude;
*    long = position.coords.longitude;
*    console.log('Your latitude is :'+lat+' and longitude is '+long);
*    document.getElementById("lat").innerHTML = lat;
*    document.getElementById("lon").innerHTML = long;
*    /* document.write("You will be redirected in 5 seconds"); */
*    setTimeout(function() { Redirect(); }, 5000 );
*
*}
*function Redirect() {
*    window.location='https://api.open-meteo.com/v1/forecast?latitude='
*+lat+'&longitude='+long+'&hourly=temperature_2m&timezone=auto';
*}
*function errorFunction(position) {
*   alert('Sorry, no position is available');
*}
*
*</script>
*<style>
*  div.inline { display: inline; }
*  .password-item {
*    background-color: #000;
*    color: #000;
*  }
*  .password-item:hover {
*    background-color: inherit;
*    color: inherit;
*  }
*</style>
*</head>
*<BODY>
*<p>Hello,</p>
*<p>Your latitude is <div class='inline' id='lat'></div> and
*your longitude is <div class='inline' id='lon'></div>
*...
*<p>You will be redirected in 5 seconds...</p>
*
*</BODY>
*</html>


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

    " We can merge entire table to one string. This is HTML.
    data(lv_response) = reduce string( init str type string for line in lt_lines next str = str && line-tdline ).

    " Here, we are saying the browser should recognize the data as HTML.
    " This is different from what we do from other endpoints.
    " This also means you can return a lot of different things.
    mo_response->set_header_field( iv_name = if_http_header_fields=>content_type iv_value = |text/html; charset=utf-8| ).

    " Setting the response data to our response body.
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
      WHEN '/qr'.
        generate_qr( ).
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
      WHEN '/empty'.
        empty_list( ).
    ENDCASE.

  endmethod.


  method IF_REST_RESOURCE~POST.
*CALL METHOD SUPER->IF_REST_RESOURCE~POST
*  EXPORTING
*    IO_ENTITY =
*    .

    " POST requests are a bit different from GET requests.
    " SAP requires a 'x-csrf-token' value in the header to execute
    " these requests.

    " You can get this value by executing a 'GET' request with
    " 'x-csrf-token=fetch' set in request header. Returning response
    " will include 'x-csrf-token' value in the header again.

    " While you can read it from header via your application, I
    " also provided a root endpoint to return that value in a JSON
    " structure.

    " This token is generated for the lifetime of the session.
    " You'll need to regenerate it for another session.

    " The GET_URI_PATH method reads the endpoint.
    data(lv_path) = |{ MO_REQUEST->GET_URI_PATH( ) CASE = lower }|.

    " We are determining the method we'll call from the endpoint.
    CASE lv_path.
      WHEN '/setlist'. " This endpoint will set the contents of our shopping list
        set_list( ).
    ENDCASE.


  endmethod.


  method SET_BOUGHT.

    data(lv_item) = MO_REQUEST->GET_URI_QUERY_PARAMETER( iv_name = 'item' ).

    if lv_item is initial.
      error( status = 400 reason = 'Please provide item name' ).
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

    " We are telling the browser to understand what kind of data we sent to it.
    " Here we say 'We send you a JSON data' to browser
      mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).

    endselect.
    if sy-subrc ne 0.
      mo_response->set_status( iv_status = 200 iv_reason_phrase = 'Item is not in the list' ).
      mo_response->create_entity( )->set_string_data( iv_data = 'Item is not in the list' ).
    endif.

  endmethod.


  method SET_LIST.

    " We are first creating an internal table to put the request body data
    data: lt_request type table of zha_list with empty key.

    " We are getting the JSON from the request's body
    data(lv_request) = mo_request->get_entity( )->get_string_data( ).

    " This will turn the JSON data to the table data.
    " It should be formatted correctly for SAP to understand it.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_request
      CHANGING
        data = lt_request
     ).

    " Optionally we can remove all records first
    " DELETE from ZHA_LIST

    " This command will fill our table with the data from JSON
    modify zha_list from table lt_request.


    " Now the data is imported we should prepare a response.
    " The first step is show user what is imported ( in this case the data user provided )
    " then we also give a status message.
    data: begin of ls_response,
            item_data type table of zha_list with EMPTY KEY,
            status type string,
          end of ls_response.

    " Here, we are setting the values.
    ls_response = value #( item_data = CORRESPONDING #( lt_request ) status = |Imported| ).

    " Now converting our response data to JSON
    /ui2/cl_json=>serialize(
      EXPORTING
        data  = ls_response
      RECEIVING
        r_json = data(lv_response) ).

    " Setting response body
    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).

    " We are telling the browser to understand what kind of data we sent to it.
    " Here we say 'We send you a JSON data' to browser
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


  method EMPTY_LIST.

    " Well we don't need to do much. Emptying the DB table
    delete from zha_list.

    data: begin of ls_response,
            status type string,
          end of ls_response.

    " Just sending back a status message.
    ls_response = value #( status = 'Shopping list cleared' ).

    " The command below will convert the table data to JSON.
    /ui2/cl_json=>serialize(
      EXPORTING
        data  = ls_response
      RECEIVING
        r_json = data(lv_response) ).

    " The line below sets the body of our message. In this case the JSON data we created
    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).

    " We are telling the browser to understand what kind of data we sent to it.
    " Here we say 'We send you a JSON data' to browser
    mo_response->set_header_field( iv_name = if_http_header_fields=>CONTENT_TYPE iv_value = |application/json| ).


  endmethod.


  method GENERATE_QR.
    " We're creating the table to read our TEXT into.
    data: lt_lines type table of tline.

    " The text we read below actually is an HTML file. Here's the actual content:

*<!DOCTYPE html>
*<html>
*<body>
*  <div id="qrcode"></div>
*  <script src=
*"https://cdnjs.cloudflare.com/ajax/libs/qrcodejs/1.0.0/qrcode.min.js">
*</script>
*  <script>
*    let qrCode;
*
*    function generateQrCode(qrContent) {
*      return new QRCode("qrcode", {
*        text: qrContent,
*        width: 256,
*        height: 256,
*        colorDark: "#000000",
*        colorLight: "#ffffff",
*        correctLevel: QRCode.CorrectLevel.H,
*      });
*    }
*    let qrContent = {{code}};
*    if (qrCode == null) {
*        // Generate code initially
*        qrCode = generateQrCode(qrContent);
*    } else {
*        // If code already generated then make
*        // again using same object
*        qrCode.makeCode(qrContent);
*    }
*  </script>
*</body>
*</html>

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                            = 'ST'
        LANGUAGE                      = sy-langu
        NAME                          = 'ZHA_QR'
        OBJECT                        = 'TEXT'
      TABLES
        LINES                         = lt_lines.

    IF SY-SUBRC <> 0.
*     Implement suitable error handling here
    ENDIF.

    " We're reading the 'code' parameter from the request uri
    data(lv_code) = MO_REQUEST->GET_URI_QUERY_PARAMETER( iv_name = 'code' ).

    " We can merge entire table to one string. This is HTML.
    data(lv_response) = reduce string( init str type string for line in lt_lines next str = str && line-tdline ).

    replace |\{\{code\}\}| in lv_response with |"{ lv_code }"|.

    " Here, we are saying the browser should recognize the data as HTML.
    " This is different from what we do from other endpoints.
    " This also means you can return a lot of different things.
    " Like in this instance a QR code.
    mo_response->set_header_field( iv_name = if_http_header_fields=>content_type iv_value = |text/html; charset=utf-8| ).

    " Setting the response data to our response body.
    mo_response->create_entity( )->set_string_data( iv_data = lv_response ).
  endmethod.
ENDCLASS.
