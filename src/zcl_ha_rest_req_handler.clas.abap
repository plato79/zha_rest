class ZCL_HA_REST_REQ_HANDLER definition
  public
  inheriting from CL_REST_HTTP_HANDLER
  final
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HA_REST_REQ_HANDLER IMPLEMENTATION.


  method IF_REST_APPLICATION~GET_ROOT_HANDLER.
*CALL METHOD SUPER->IF_REST_APPLICATION~GET_ROOT_HANDLER
*  RECEIVING
*    RO_ROOT_HANDLER =
*    .

    " Creating router for handling the request
    data(lo_router) = new cl_rest_router( ).

    " Assigning paths to router(s).
    " '/' here means: https://a152s4hascs.tr152.corpintra.net/sap/bc/rest/zha_rest
    " '/weather' here means: https://a152s4hascs.tr152.corpintra.net/sap/bc/rest/zha_rest/weather etc...
    lo_router->attach(
                       iv_template = ''
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    lo_router->attach(
                       iv_template = '/'
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    lo_router->attach(
                       iv_template = '/weather'
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    lo_router->attach(
                       iv_template = '/whoami'
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    lo_router->attach(
                       iv_template = '/whoisthis'
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    lo_router->attach(
                       iv_template = '/getlist'
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    lo_router->attach(
                       iv_template = '/setlist'
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    lo_router->attach(
                       iv_template = '/setbought'
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    lo_router->attach(
                       iv_template = '/addtolist'
                       iv_handler_class = 'ZCL_HA_REST_REQ_PROVIDER'
                     ).

    ro_root_handler = lo_router.

  endmethod.
ENDCLASS.
