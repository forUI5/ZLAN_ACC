*&---------------------------------------------------------------------*
*& Report  ZLAN_ACC
*&by lan
*有兴趣和精力一起完善的朋友联系 1510491230@qq.com
*&---------------------------------------------------------------------*

report zlan_acc.

define sql_exsit_check.
  select count( * )
  from &1
  where &2 = p_object.
  if sy-subrc = 0.
    p_gt_rep_out-exsit = 'X'.
  endif.
end-of-definition.

define tab_rep_sel_t. "表的处理方式
  data ls_&1 like gt_&1.
  loop at p_gt_rep-&1 into ls_&1.
    if ls_&1 is not initial.
      read table p_gt_node with key nodekey =  ls_&1-node-key binary search.
      if sy-subrc = 0.
        ls_&1-node-sel = 'X'.
      else.
        ls_&1-node-sel = ''.
      endif.
      modify p_gt_rep-&1 from ls_&1.
    endif.
  endloop.
end-of-definition.

define tab_rep_sel_s. "结构的处理方式
  if p_gt_rep-&1 is not initial.
    read table p_gt_node with key nodekey =  p_gt_rep-&1-node-key binary search.
    if sy-subrc = 0.
      p_gt_rep-&1-node-sel = 'X'.
    else.
      p_gt_rep-&1-node-sel = ''.
    endif.
  endif.
end-of-definition.

define pop_key_append.
  clear gt_pop.
  gt_pop-key = &1.
  gt_pop-text = &2.
  gt_pop-value = &3.
  append gt_pop.
end-of-definition.

define object_add. "像gt_表添加对象
  data ls_&1 like p_gt_&1.

  clear:lv_exsit,ls_&1.

  read table p_gt_&1 transporting no fields with key &2 = &3.
  if sy-subrc ne 0.
    loop at gt_rep.
      read table gt_rep-&1 transporting no fields with key &2 = &3.
      if sy-subrc = 0 .
        lv_exsit = 'X'.
        exit.
      endif.
    endloop.
    if lv_exsit is initial.
      ls_&1-&2 = &3.
      append ls_&1 to p_gt_&1.
    endif.
  endif.
end-of-definition.

define key_parse.
  split lv_string1 at &1 into lv_string2 lv_string3.
  split lv_string3 at lv_c into &2 lv_string2.
end-of-definition.

define global_get.
  assign (&1) to &2.
end-of-definition.

define mac_add_form_field. " form html_viewer_init 中使用
  append initial line to &1 assigning <ls_form_field>.
  <ls_form_field>-name = &2.
  <ls_form_field>-value = &3.
end-of-definition.

class lcl_application definition deferred.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_application definition.

  public section.
    methods:
      handle_node_double_click
      for event node_double_click
                  of cl_gui_alv_tree
        importing node_key.
endclass.                    "LCL_APPLICATION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_application implementation.

  method  handle_node_double_click.
    perform object_display using node_key.
  endmethod.                    "HANDLE_NODE_DOUBLE_CLICK

endclass.                    "LCL_APPLICATION IMPLEMENTATION

*HTML浏览器事件处理器
class cl_myevent_handler definition.

  public section.
    methods: on_sapevent
                for event sapevent of cl_gui_html_viewer
      importing action frame getdata postdata query_table.

endclass.                    "cl_myevent_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_myevent_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_myevent_handler implementation.

  method on_sapevent.

    perform response_parse tables postdata using action.


  endmethod.                    "on_sapevent

endclass.                    "cl_myevent_handler IMPLEMENTATION

type-pools:trwbo,abap,trsel,icon.

types: ty_spaces_tt type standard table of i with default key.

*types:begin of ty_info, "提交到服务器所需的程序信息
*  rep_name(40), "资源库名
*  zip, "压缩标识
*  text(40),
*  url(250),
**  datum like sy-datum,
**  uzeit like sy-uzeit,
*  end of ty_info.

types:begin of ty_node,
        object(40),
        key        type i,
        sel,
        request    type trkorr,
        rtype,
        rtmsg(255),
      end of ty_node.

types: begin of ty_codes, "源代码
*         line(72),
         line(720), "如果程序代码长度超了72，read report会dump掉的
       end of ty_codes.
types:begin of ty_code,
        code type standard table of ty_codes with default key,
        node type ty_node,
      end of ty_code.

types:begin of ty_text,
        text type standard table of textpool with default key,
        node type ty_node,
      end of ty_text.

types:begin of ty_sta,
        sta  type rsmpe_stat,
        node type ty_node,
      end of ty_sta.

types: begin of ty_cua, "status
         adm type rsmpe_adm,
         sta type standard table of ty_sta with default key, "包含node
         fun type standard table of rsmpe_funt with default key,
         men type standard table of rsmpe_men with default key,
         mtx type standard table of rsmpe_mnlt with default key,
         act type standard table of rsmpe_act with default key,
         but type standard table of rsmpe_but with default key,
         pfk type standard table of rsmpe_pfk with default key,
         set type standard table of rsmpe_staf with default key,
         doc type standard table of rsmpe_atrt with default key,
         tit type standard table of rsmpe_titt with default key,
         biv type standard table of rsmpe_buts with default key,
       end of ty_cua.

types: begin of ty_tadir, "程序信息
         obj_type type tadir-object,
         obj_name type tadir-obj_name,
         devclass type devclass,
       end of ty_tadir.

types: begin of ty_dynpro, "屏幕
         header     type rpy_dyhead,
         containers type dycatt_tab,
         fields     type dyfatc_tab,
         flow_logic type swydyflow,
         spaces     type ty_spaces_tt,
         node       type ty_node,
       end of ty_dynpro.

types: begin of ty_table, "表、结构
         tablename  like dd03l-tabname,
         dd02v      type dd02v,
         dd09v      type dd09v,
         tabletitle like dd02t-ddtext,
         istructure type dd03p occurs 0, "不能用type table of（会报错）
         node       type ty_node,
       end of ty_table.

types:begin of ty_ttyp, "表类型,
        typename  type ttypename,
        gotstate  type  ddgotstate,
        dd40v_wa  type  dd40v,
        dd42v_tab type  dd42v occurs 0,
*    dd43v_tab type  dd43v occurs 0,
        node      type ty_node,
      end of ty_ttyp.

types:begin of ty_dtel, "数据元素
        name  type ddobjname,
        dd04v type dd04v,
        tpara type tpara,
        node  type ty_node,
      end of ty_dtel.

types:begin of ty_class, "类
        clsname  type seoclass-clsname,
        class    type vseoclass,
*        interface type vseointerf,
        t_source type standard table of string with default key,
        node     type ty_node,
      end of ty_class.

types:begin of ty_doma, "域
        name  type ddobjname,
        dd01v type dd01v,
        node  type ty_node,
      end of ty_doma.

types:begin of ty_lock, "锁对象
        name  type ddobjname,
        dd25v type dd25v, "Header of the Lock Object
        dd26e type standard table of dd26e with default key, "Base Tables of the Lock Object "必须要这样写，不然会报错不能在结构中使用通用类型定义chenyl for chenyl 14.08.2017 11:37:03
        dd27p type standard table of dd27p with default key, "Lock Parameter of the Lock Object
        ddena type standard table of ddena with default key, "Lock Arguments of the Lock Object
        node  type ty_node,
      end of ty_lock.

types:begin of ty_snro,
        object            type tnro-object,
        interval_exists,
        object_attributes type tnro,
        object_text       type tnrot,
        interval          like table of inriv with default key, "编号间隔范围
        node              type ty_node,
      end of ty_snro.

types:begin of ty_tcode,
        tstc  type tstc,
        tstcp type tstcp,
        tstcc type tstcc, "Additional Attributes for TSTC
        tstct type standard table of tstct with default key,
        tstca type standard table of tstca with default key,
        usott type standard table of usott with default key,
        node  type ty_node,
      end of ty_tcode.

types:begin of ty_fugr,
        area  like tlibt-area, "函数组
        areat type areat, "函数组文本
        node  type ty_node,
      end of ty_fugr.

types:begin of ty_func,
        functionname       type rs38l-name,
        global_flag        like  rs38l-global,
        remote_call        like  rs38l-remote,
        update_task        like  rs38l-utask,
        short_text         like  tftit-stext,
        function_pool      like  rs38l-area,
*  remote_basxml_supported like  rs38l-basxml_enabled, "R3没有这个字段
        import_parameter   type standard table of rsimp with default key,
        changing_parameter like table of rscha with default key,
        export_parameter   like table of rsexp with default key,
        tables_parameter   like table of rstbl with default key,
        exception_list     like table of rsexc with default key,
        documentation      like table of  rsfdo with default key,
        source             like table of rssource with default key,
        new_source         type rsfb_source, "rssource只有72的长度是不够的
        node               type ty_node,
      end of ty_func.

types:begin of ty_w3mi,
        objid  type wwwdatatab-objid,
        key    type wwwdatatab,
        buffer type xstring,
      end of ty_w3mi.

data: cx_root type ref to cx_root. "根异常

data:
  gt_request type trwbo_request_headers with header line,
  gs_cua     type ty_cua,
  gs_code    type ty_code,
  gs_codes   type ty_codes,
  gs_text    type ty_text,
  gt_screen  type standard table of ty_dynpro with header line,
  gt_table   type standard table of ty_table with header line, "多张表、结构 dictionary
  gt_ttyp    type table of ty_ttyp with header line, "表类型
  gt_dtel    type table of ty_dtel with header line, "数据元素
  gt_class   type table of ty_class with header line, "类
  gt_doma    type table of ty_doma with header line, "域
  gt_lock    type table of ty_lock with header line,
  gt_snro    type table of ty_snro with header line,
  gt_tcode   type table of ty_tcode with header line,
  gs_fugr    type ty_fugr,
  gt_func    type table of ty_func with header line,
  gt_w3mi    type table of ty_w3mi with header line.

*资源库
types:begin of ty_rep,
        uname(40), "用户名
        program   like sy-repid, "存放程序名、函数组名等
        type, "对象类型
        typet(20), "对象类型描述
        package   type devclass, "包
        node      type ty_node,
        code      type ty_code, "源代码
        cua       type ty_cua, "pf status
        text      type ty_text, "文本池
        screen2   like standard table of gt_screen with default key, "国际营销701 用screen取名会报错
        ttyp      like table of gt_ttyp with default key, "表类型
        dict      like table of gt_table with default key, "表、结构、视图
        dtel      like table of gt_dtel with default key, "数据元素
        class     like table of gt_class with default key, "类
        doma      like table of gt_doma with default key, "域
        lock      like table of gt_lock with default key,
        snro      like table of gt_snro with default key, "编号范围对象
        tcode     like table of gt_tcode with default key,
        fugr      type ty_fugr , "函数组
        func      like table of gt_func with default key,
      end of ty_rep.
types ty_t_rep type table of ty_rep.

tables sscrfields.

*资源库集
data:
  gs_rep type ty_rep,
  gt_rep like table of gs_rep with header line.
*data:begin of gs_rep_pac, "包起来的资源库
*  info type ty_info,
*  rep like table of gt_rep,
*  end of gs_rep_pac.

*tree 输出的rep
data:begin of gt_rep_out occurs 0,
       object(40),
       otype(40),
       descp      type repti, "程序描述
       exsit,
       request    type trkorr, "请求（可以根据对象类型去抓一把）
       rtype,
       rtmsg(255),
     end of gt_rep_out.
*data gt_rep_out_sel like table of gt_rep_out with header line.

*资源库数据tree展示
data:
  g_alv_tree         type ref to cl_gui_alv_tree,
  g_custom_container type ref to cl_gui_docking_container,
  g_application      type ref to lcl_application.
data: gt_sflight      type sflight occurs 0,      "Output-Table
      gt_fieldcatalog type lvc_t_fcat,
      ok_code         like sy-ucomm,
      save_ok         like sy-ucomm,           "OK-Code
      g_max           type i value 255,
      g_fav_key       type lvc_nkey.
data:
  gs_selected_node type lvc_s_chit,
  gt_selected_node type standard table of lvc_s_chit with header line.

*查询返回的结果
data:begin of gt_result occurs 0,
       sel,
       id(40),
       name(40),
       text(40),
       url(250),
       detail(250),
       datum       like sy-datum,
       uzeit       like sy-uzeit,
       tag(40),
       uname(40),
       count(9),
     end of gt_result.

*html浏览器
data go_docking      type ref to cl_gui_docking_container.
data go_html_viewer type ref to cl_gui_html_viewer .

*table control（修改资源对象属性值）
data:begin of gs_popk,
       title(40),
     end of gs_popk.
data:begin of gt_pop occurs 0,
       key(40),
       text(40),
       value(250),
     end of gt_pop.

*解析json用
data:begin of gs_fields,
       key(40),
       value(256),
     end of gs_fields.

*alv
data gs_styl type lvc_s_styl.
data gt_styl type lvc_t_styl.
data:
  gt_fieldcat      type lvc_t_fcat with header line, "输出alv
  gt_fieldcat2     type lvc_t_fcat with header line, "输出alv或动态内表
  gs_layout        type lvc_s_layo,
  gt_events        type slis_t_event with header line,
  gt_event_exit    type slis_t_event_exit with header line,
  go_grid          type ref to cl_gui_alv_grid,
*  go_event_receiver type ref to lcl_event_receiver,
  gs_grid_settings type lvc_s_glay,
  gt_head          type slis_t_listheader with header line,
  gt_sort          type lvc_t_sort with header line.
*class lcl_event_receiver definition deferred.

*屏幕传值专用全局变量
data:
  gv_url type text132,
  gv_par type string.

*全局变量
data:
  gv_msg(255),
  gv_program        like sy-repid value sy-repid, "程序名
  gv_package        type devclass,
  gv_request        type trkorr,
  gv_xml            type string,
  gv_filename       like ibipparms-path,
  gv_state, "程序执行状态
  gv_rtype,
  gv_rtmsg(255),
  gs_rfcsi          type rfcsi,
  gv_codepage       type cpcodepage,
  gv_namespace(250),
  gv_init,
  gv_notif_time(30), "通知时间
  gv_name_c(30). "当前资源库对象的所属用户

data:begin of gt_repid occurs 0,
       id like gt_result-id,
     end of gt_repid.

*选择屏幕
selection-screen function key 1 .

selection-screen begin of block b1 with frame title text-001.
parameters:
  p_id(40)     modif id m1,
  p_repnam(40) modif id m1, "资源项目名
  p_text(40)   modif id m1,
  p_tag(40)    modif id m1,
  p_url(250)   modif id m1.

select-options:
s_prog for sy-repid no intervals modif id m2.

parameters:
  p_uname(40) modif id m3,
  p_passwd(9) modif id m3.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
parameters:
  p_search radiobutton group g1 default 'X' user-command u1,
  p_export radiobutton group g1,
  p_import radiobutton group g1.
selection-screen end of block b2.

selection-screen begin of block b6 with frame title text-006.
parameters:
  p_server radiobutton group g2 default 'X' user-command u2,
  p_file   radiobutton group g2.
selection-screen end of block b6.

selection-screen begin of block b5 with frame title text-005.
parameters:
*p_debug as checkbox default 'X'. "开发环境
p_debug default '' no-display. "生产环境
parameters: p_comp type c no-display. "编译
selection-screen end of block b5.

selection-screen begin of block b7 with frame title text-010. "关于（通知）
selection-screen comment /1(72) gv_c1.
selection-screen comment /1(72) gv_c2.
selection-screen comment /1(72) gv_c3.
selection-screen comment /1(72) gv_c4.
selection-screen comment /1(72) gv_c5.
selection-screen end of block b7.

*导入 对象重命名屏幕
selection-screen begin of screen 2004 title text-004.
parameters:
  p_func  as checkbox,
  p_tab   as checkbox,
  p_tcode as checkbox.
selection-screen end of screen 2004.

*用户注册屏幕
selection-screen begin of screen 2005 title text-009.
parameters:
  gv_uname(40) obligatory,
  gv_paswd(9)  obligatory,
  gv_phone(11) obligatory,
  gv_email(30) obligatory.
selection-screen end of screen 2005.

initialization.
  perform initialization.

at selection-screen output.
  perform selection_screen_pbo.

at selection-screen.
  perform selection_screen_pai.

start-of-selection.
  perform screen_check.
  perform exec_check.

*查询
  if p_search = 'X'.
    perform rep_search.
    perform result_output. "输出查询结果
*导出
  elseif p_export = 'X' .
    loop at s_prog. "先确定文件夹（同时取表）
      perform rep_scan tables gt_rep using s_prog-low.
    endloop.
    perform rep_get tables gt_rep. "再往下找
*    perform rep_pac. "资源库打包 （现在看来没啥用啊）
    if p_file = 'X'.
      perform rep_download using p_repnam gt_rep[].
    elseif p_server = 'X'.
      perform rep_commit using gt_rep[].
    endif.
*导入
  elseif p_import = 'X' .
    if p_file = 'X'.
      perform rep_upload changing gt_rep[].
    elseif p_server = 'X'.
      perform rep_from_server changing gt_rep[].
    endif.
    perform rep_display tables gt_rep.
  endif.

*&---------------------------------------------------------------------*
*&      Form  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form output .
  perform log_output.
endform.                    " OUTPUT

*&---------------------------------------------------------------------*
*&      Form  DATA_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form data_initialize .
  data current_resources type sy-index.
  data maximal_resources type sy-index.
  data recommended_delay type sy-index.

*取系统信息
*  call function 'RFC_GET_SYSTEM_INFO' "R3没有
  call function 'RFC_SYSTEM_INFO'
    importing
      rfcsi_export      = gs_rfcsi
      current_resources = current_resources
      maximal_resources = maximal_resources
      recommended_delay = recommended_delay.

*系统编码
  call function 'SCP_CODEPAGE_FOR_LANGUAGE'
    exporting
      language    = sy-langu
    importing
      codepage    = gv_codepage
    exceptions
      no_codepage = 1.

*  gv_program(1) = 'Z'.

endform.                    " DATA_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  CODE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form code_check changing o_error_subrc o_error_message.
  data i_global_check   type sy-calld.
  data i_global_program type sy-repid.
  data i_program        type sy-repid.
  data i_with_dialog    type sy-calld.
  data o_error_include  type sy-repid.
  data o_error_line     type sy-tabix.
  data:begin of i_source occurs 0,
         line(72),
       end of i_source.

  i_program = gv_program.

  call function 'RS_SYNTAX_CHECK'
    exporting
*     I_GLOBAL_CHECK  = ' '
*     I_GLOBAL_PROGRAM = ' '
      i_program       = i_program
*     I_WITH_DIALOG   = ' '
    importing
      o_error_include = o_error_include
      o_error_line    = o_error_line
      o_error_message = o_error_message
*     o_error_offset  = o_error_offset
      o_error_subrc   = o_error_subrc
*     o_navigate      = o_navigate
*     cancel          = cancel
    tables
      i_source        = i_source.

endform.                    " CODE_CHECK
*&---------------------------------------------------------------------*
*&      Form  LOCK_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form lock_delete using p_program.
  data:
    lv_grag like seqg3-garg,
    enq     like table of seqg3.

  lv_grag = p_program.
  call function 'ENQUEUE_READ'
    exporting
      gname  = 'TRDIR'
      garg   = lv_grag
      guname = ''
    tables
      enq    = enq
    exceptions
      others = 0.

  describe table enq lines sy-tfill.

  check sy-tfill > 0.

  call function 'ENQUE_DELETE'
    exporting
      suppress_syslog_entry = 'X'
*    importing
*     subrc                 = rc
    tables
      enq                   = enq.
endform.                    " LOCK_DELETE
*&---------------------------------------------------------------------*
*&      Form  CODE_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form code_delete .

  call function 'RS_DELETE_PROGRAM'
    exporting
*     CORRNUMBER = CORRNUMBER
      program = gv_program.

endform.                    " CODE_DELETE

*&---------------------------------------------------------------------*
*&      Form  rep_Log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0057   text
*----------------------------------------------------------------------*
form rep_log  using request rtype rtmsg changing p_node type ty_node.
  p_node-rtype = rtype.
  p_node-rtmsg = rtmsg.
  p_node-request = request.
endform.                    " rep_Log
*&---------------------------------------------------------------------*
*&      Form  LOG_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form log_output .
  data lo_ref type ref to cx_root. "异常处理
  data lv_text type string.
endform.                    " LOG_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  STATUS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p_export        text
*  <--  p_import        text
*----------------------------------------------------------------------*
form status_get using p_program changing p_gs_cua type ty_cua.
  data:
    ls_sta        type ty_sta,
    lt_rsmpe_stat like table of rsmpe_stat with header line.

  call function 'RS_CUA_INTERNAL_FETCH'
    exporting
      program         = p_program
      language        = '1'
      state           = 'A'
    importing
      adm             = p_gs_cua-adm
    tables
*     sta             = p_gs_cua-sta
      sta             = lt_rsmpe_stat
      fun             = p_gs_cua-fun
      men             = p_gs_cua-men
      mtx             = p_gs_cua-mtx
      act             = p_gs_cua-act
      but             = p_gs_cua-but
      pfk             = p_gs_cua-pfk
      set             = p_gs_cua-set
      doc             = p_gs_cua-doc
      tit             = p_gs_cua-tit
      biv             = p_gs_cua-biv
    exceptions
      not_found       = 1
      unknown_version = 2
      others          = 3.
  if sy-subrc ne 0.
    perform msg_sys_into changing gv_msg.
*    concatenate '读取程序' p_program 'STATUS出错：' gv_msg into gv_msg.
*    perform rep_Log using p_program 'E' gv_msg.
  else.
    loop at lt_rsmpe_stat.
      ls_sta-sta = lt_rsmpe_stat.
      append ls_sta to p_gs_cua-sta.
    endloop.
*    concatenate '读取程序' p_program 'STATUS成功' into gv_msg.
*    perform rep_Log using p_program 'S' gv_msg.
  endif.
endform.                    " STATUS_GET

*&---------------------------------------------------------------------*
*&      Form  STATUS_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p_export        text
*  <--  p_import        text
*----------------------------------------------------------------------*
form xml_download using p_gv_filename p_gv_xml.

  data:
    v_stream    type string,
    lcl_xml_doc type ref to cl_xml_document,
    v_subrc     type sysubrc.

  create object lcl_xml_doc.
  if sy-subrc = 0.
    v_stream = p_gv_xml.
    v_subrc = lcl_xml_doc->parse_string( stream = v_stream ).
    check v_subrc = 0.
    v_subrc = lcl_xml_doc->export_to_file( filename = p_gv_filename ).
    if v_subrc = 0.
*      perform rep_Log using space 'S' 'repository下载成功'.
    else.
*      perform rep_Log using space 'S' 'repository下载失败'.
    endif.
  endif.
endform.                    " STATUS_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  file_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_FILENAME  text
*      -->P_GV_XML       text
*----------------------------------------------------------------------*
form file_download using p_gv_filename p_gv_xml.

  data:
    datatab type table_of_strings,
    ld_file type string.

  ld_file = p_gv_filename.

  append p_gv_xml to datatab.

  call method cl_gui_frontend_services=>gui_download
    exporting
      filename = ld_file
    changing
      data_tab = datatab
    exceptions
      others   = 1.
endform.                    " STATUS_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  rep_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rep_upload  changing p_gt_rep.
*取文件路径
  perform filename_get changing gv_filename.

*  perform xml_upload using gv_filename changing gv_xml gv_zip.
*  if gv_zip is initial. "xml文件
*    perform xml_to_data using gv_xml  changing p_gt_rep.
*  else.
  perform file_upload using gv_filename changing gv_xml.
  perform zip_to_data using gv_xml  changing p_gt_rep.
*  endif.

endform.                    " rep_upload

*&---------------------------------------------------------------------*
*&      Form  STATUS_FROM_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zip_to_data using p_gv_xml changing p_gt_rep .
  data lo_ref type ref to cx_root. "异常处理
  data lv_text type string.
  data:
    lv_gzip_in  type xstring,
    lv_text_out type string.

  clear p_gt_rep.

  if p_gv_xml is initial.
    return.
  endif.

*解压
*非unicode系统不能填UTF-8，不填表示ANSI，可以导入UTF-8（乱码）和ANSI，导入 UTF-8 的压缩文件中文会乱码
*unicode只能导入UTF-8，否则会异常，不填默认UTF-8
  lv_gzip_in = p_gv_xml.
  call method cl_abap_gzip=>decompress_text
    exporting
      gzip_in  = lv_gzip_in
*     conversion = 'UTF-8' "源文件是UTF-8
*     conversion = '4102' "源文件是UTF-8
*     conversion = 'ANSI' "源文件是UTF-8
    importing
      text_out = lv_text_out.

  perform xstring_to_string changing lv_text_out.

*转进内表
  try.
      call transformation id
        source xml lv_text_out
*        options
*        clear = 'ALL' "4.7 没有这个options
*        value_handling = 'ACCEPT_DATA_LOSS' "这个参数也没有
        result data = p_gt_rep.
    catch cx_root into lo_ref.
      lv_text = lo_ref->get_text( ).
      concatenate 'rep转换出错：' lv_text into lv_text.
      message lv_text type 'S' display like 'E'.
  endtry.


endform.                    " STATUS_FROM_XML
*&---------------------------------------------------------------------*
*&      Form  xml_to_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_XML   text
*      -->P_GT_REP   text
*----------------------------------------------------------------------*
form xml_to_data using p_gv_xml changing p_gt_rep .
  data lo_ref type ref to cx_root. "异常处理
  data lv_text type string.
  data:
    lv_gzip_in  type xstring,
    lv_text_out type string.

  if p_gv_xml is initial.
    return.
  endif.

*转进内表
  try.
      call transformation id
        source xml p_gv_xml
*        options
*        clear = 'ALL' "4.7 没有这个options
*        value_handling = 'ACCEPT_DATA_LOSS'
        result data = p_gt_rep.
    catch cx_root into lo_ref.
      lv_text = lo_ref->get_text( ).
  endtry.

endform.                    " STATUS_FROM_XML
*&---------------------------------------------------------------------*
*&      Form  STATUS_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form status_set using p_program p_gv_package p_cua type ty_cua.
  data: ls_tr_key type trkey.
  data: ms_item     type ty_tadir.
  data:
    ls_sta        type ty_sta,
    lt_rsmpe_stat like table of rsmpe_stat with header line.

  if p_cua is initial.
    return.
  endif.

  loop at p_cua-sta into ls_sta where node-sel = 'X'.
    lt_rsmpe_stat = ls_sta-sta.
    append lt_rsmpe_stat.
  endloop.

  if lines( lt_rsmpe_stat ) = 0.
    return.
  endif.

  perform request_set using p_gv_package 'CUAD' p_program changing gv_request gv_rtype gv_rtmsg. "会包所有的gui状态和菜单栏
  if gv_rtype = 'S'.
    call function 'RS_CUA_INTERNAL_WRITE'
      exporting
        program   = p_program
        language  = '1'
        tr_key    = ls_tr_key
        adm       = p_cua-adm
        state     = 'A' "为I时出现一个激活了之后还是显示未激活的bug
      tables
*       sta       = p_cua-sta
        sta       = lt_rsmpe_stat
        fun       = p_cua-fun
        men       = p_cua-men
        mtx       = p_cua-mtx
        act       = p_cua-act
        but       = p_cua-but
        pfk       = p_cua-pfk
        set       = p_cua-set
        doc       = p_cua-doc
        tit       = p_cua-tit
        biv       = p_cua-biv
      exceptions
        not_found = 1
        others    = 2.
    if sy-subrc <> 0.
      perform msg_sys_into changing gv_msg.
      loop at p_cua-sta into ls_sta where node-sel = 'X'.
        perform rep_log using space 'E' gv_msg changing ls_sta-node.
        modify p_cua-sta from ls_sta.
      endloop.
    else.

      loop at p_cua-sta into ls_sta where node-sel = 'X'.
        perform rep_log using gv_request 'S' '成功' changing ls_sta-node.
        modify p_cua-sta from ls_sta.
      endloop.
    endif.
  else. "没有包请求
    loop at p_cua-sta into ls_sta where node-sel = 'X'.
      perform rep_log using gv_request gv_rtype gv_rtmsg changing ls_sta-node.
      modify p_cua-sta from ls_sta.
    endloop.
  endif.

*  perform request_set using p_gv_package 'ABAP' p_program.
endform.                    " STATUS_CREATE
*&---------------------------------------------------------------------*
*&      Form  rep_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rep_get tables p_gt_rep structure gt_rep.
  data ls_rep like gt_rep.
  data:
    ls_code type ty_codes,
    lt_code type table of ty_codes.

  loop at p_gt_rep into ls_rep.
    clear:gt_request,gt_request[],gs_cua,
    gs_code,gs_text,gt_screen,gt_screen[],
    gt_table,gt_table[],gt_ttyp,gt_ttyp[],gt_dtel,gt_dtel[],gt_doma,gt_doma[],
    gt_class,gt_class[],
    gt_lock,gt_lock[],gt_func,gt_func[],
    gt_snro,gt_snro[],gt_tcode,gt_tcode[].

    if ls_rep-type = 'F'. "函数
*取函数
      gt_func[] = ls_rep-func.
      perform func_get tables gt_func gt_table gt_ttyp gt_dtel.
      loop at gt_func.
*分析函数代码
        if gt_func-source is not initial.
          lt_code = gt_func-source.
        else.
          lt_code = gt_func-new_source.
        endif.
        perform code_analyze tables lt_code
                                                     gt_lock
                                                     gt_snro
                                                     gt_func
                                                     gt_table
                                                     gt_ttyp
                                                     gt_dtel
                                                     gt_w3mi.

        perform func_get tables gt_func gt_table gt_ttyp gt_dtel.
      endloop.
      perform rep_data_add using 'FUNC' gt_func[] changing ls_rep.
    else. "可执行程序和include
*取代码
      perform code_get using ls_rep-program changing gs_code.
      perform rep_data_add using 'CODE' gs_code changing ls_rep.
*分析程序代码
      perform code_analyze tables gs_code-code
                                                   gt_lock
                                                   gt_snro
                                                   gt_func
                                                   gt_table
                                                   gt_ttyp
                                                   gt_dtel
                                                   gt_w3mi.
    endif.

*根据所用处清单取数据字典（分析程序代码部分可以不要了）（class也在此处处理了）
    perform rep_dict_name_get tables gt_table gt_ttyp gt_dtel gt_class using ls_rep .

*text-pool
    perform text_get using ls_rep-program changing gs_text .
    perform rep_data_add using 'TEXT' gs_text changing ls_rep.
*status
    perform status_get using ls_rep-program changing gs_cua.
    perform rep_data_add using 'CUA' gs_cua changing ls_rep.
*锁对象
    perform rep_data_add using 'LOCK' gt_lock[] changing ls_rep.
*屏幕
    perform screen_get tables gt_screen using ls_rep-program.
    perform rep_data_add using 'SCREEN2' gt_screen[] changing ls_rep.
    do. "表类型可以查出结构，结构的 字段又可以是表类型，递归
*表类型
      perform ttyp_get tables gt_ttyp gt_table.
      perform rep_data_add using 'TTYP' gt_ttyp[] changing ls_rep.
*自建表和结构、这一步分割数据元素和域+同时需要把锁用到的加进来
      perform table_get tables gt_table gt_ttyp gt_dtel.
      perform rep_data_add using 'DICT' gt_table[] changing ls_rep.
      loop at gt_ttyp where dd40v_wa is initial. "结构里没有找到字段参考表类型
        exit.
      endloop.
      if sy-subrc ne 0.
        exit.
      endif.
    enddo.
*倒序（防止依赖无法激活，这种可能有一个问题就是依赖的组件在别的文件夹下出现了，就不能直接倒序处理了）


* 编号范围对象+编号范围
    perform snro_get tables gt_snro gt_dtel.
    perform rep_data_add using 'SNRO' gt_snro[] changing ls_rep.
*数据元素
    perform dtel_get tables gt_dtel.
    perform rep_data_add using 'DTEL' gt_dtel[] changing ls_rep.
*域
    perform doma_get tables gt_dtel gt_doma.
    perform rep_data_add using 'DOMA' gt_doma[] changing ls_rep.
*tcode
    perform tcode_get tables gt_tcode using ls_rep-program.
    perform rep_data_add using 'TCODE' gt_tcode[] changing ls_rep.
*class
    perform class_get tables gt_class.
    perform rep_data_add using 'CLASS' gt_class[] changing ls_rep.
*用户名
    ls_rep-uname = p_uname.

    modify p_gt_rep from ls_rep.
  endloop.
endform.                    " rep_get
*&---------------------------------------------------------------------*
*&      Form  rep_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rep_download using p_program p_gt_rep.

  perform filename_set using p_program changing gv_filename.
*  if p_zip = 'X'.
  perform zip_from_data using p_gt_rep changing gv_xml.
  perform file_download using gv_filename gv_xml.
*  else.
*    perform xml_from_data using p_gt_rep changing gv_xml.
*    perform xml_download using gv_filename gv_xml.
*  endif.

endform.                    " rep_download
*&---------------------------------------------------------------------*
*&      Form  CODE_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CODE  text
*      -->P_PROGRAM  text
*----------------------------------------------------------------------*
form code_get
               using    p_program
               changing p_gs_code structure gs_code.
  data: lt_source type table of text1000 with header line.

  clear p_gs_code.
  read report p_program into p_gs_code-code.
  if sy-subrc <> 0.
    perform msg_sys_into changing gv_msg.
    concatenate '读取程序' p_program '代码出错：' gv_msg into gv_msg.
*    perform rep_Log using p_program 'E' gv_msg.
  else.
    concatenate '读取程序' p_program '代码成功' into gv_msg.
*    perform rep_Log using p_program 'S' gv_msg.
  endif.

endform.                    " CODE_GET
*&---------------------------------------------------------------------*
*&      Form  MSG_INTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_MSG  text
*----------------------------------------------------------------------*
form msg_sys_into  changing p_gv_msg.
  clear p_gv_msg.

  check sy-msgid is not initial.
  message id sy-msgid
    type sy-msgty
    number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into p_gv_msg.
endform.                    " MSG_INTO
*&---------------------------------------------------------------------*
*&      Form  REP_DATA_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1206   text
*      <--P_GS_REP  text
*----------------------------------------------------------------------*
form rep_data_add  using p_type p_data
                   changing p_gs_rep.
  field-symbols <fs>.
  assign component p_type of structure p_gs_rep to <fs>.
  <fs> = p_data.
endform.                    " REP_DATA_ADD
*&---------------------------------------------------------------------*
*&      Form  FILENAME_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_FILENAME  text
*----------------------------------------------------------------------*
form filename_get  changing p_gv_filename.

  data:l_obj type ref to cl_gui_frontend_services.
  data: it_file type filetable with header line.
  data: g_rc type i.
  clear p_gv_filename.

  create object l_obj.
  call method l_obj->file_open_dialog
    exporting
      file_filter = '*.LAN'
*     initial_directory = 'C:\data'
    changing
      file_table  = it_file[]
      rc          = g_rc.
  read table it_file index 1.
  p_gv_filename = it_file-filename.

  if p_gv_filename is initial.
    perform msg_and_leave using '文件名不能为空'.
  endif.
endform.                    " FILENAME_GET
*&---------------------------------------------------------------------*
*&      Form  XML_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_FILENAME  text
*      <--P_GV_XML  text
*----------------------------------------------------------------------*
form xml_upload  using    p_gv_filename
                 changing p_gv_xml p_zip.
  data:
    v_stream    type string,
    lcl_xml_doc type ref to cl_xml_document,
    v_subrc     type sysubrc,
    msg         type string.

  clear p_zip.

  create object lcl_xml_doc.
  if sy-subrc = 0.
    call method lcl_xml_doc->import_from_file
      exporting
        filename = gv_filename
      receiving
        retcode  = v_subrc.
    if v_subrc ne lcl_xml_doc->c_ok.
      case v_subrc.
        when lcl_xml_doc->c_no_ixml.
          p_zip = 'X'.
        when lcl_xml_doc->c_failed.
*          perform rep_Log using space 'E' '解析文件出错'.
        when lcl_xml_doc->c_not_found.
*          perform rep_Log using space 'E' '文件查找失败'.
        when 1. "zip文件
          p_zip = 'X'.
      endcase.
    endif.

    if p_zip is initial.
      lcl_xml_doc->render_2_string(
        exporting
        pretty_print = 'X'
        importing
        retcode = v_subrc
        stream = gv_xml ).
    endif.
  endif.
endform.                    " XML_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  file_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_FILENAME  text
*      -->P_GV_XML       text
*----------------------------------------------------------------------*
form file_upload  using    p_gv_filename
                 changing p_gv_xml.

  data :
    filename type string,
    datatab  type table_of_strings.

  filename = p_gv_filename.

  call method cl_gui_frontend_services=>gui_upload
    exporting
      filename                = filename
      filetype                = 'ASC'
    changing
      data_tab                = datatab
    exceptions
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      others                  = 19.
  if sy-subrc ne 0.
    perform msg_sys.
  endif.

  read table datatab into p_gv_xml index 1.
endform.                    " XML_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  REP_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rep_set tables p_gt_rep structure gt_rep changing p_gv_package.

  loop at p_gt_rep.
    perform code_set using p_gt_rep-program p_gt_rep-type changing p_gv_package p_gt_rep-code.
    perform status_set using p_gt_rep-program p_gv_package p_gt_rep-cua.
    perform text_set using p_gt_rep-program p_gv_package changing p_gt_rep-text.
    perform screen_set tables p_gt_rep-screen2  using p_gt_rep-program p_gv_package. "创建屏幕，屏幕包入请求
    perform doma_set tables p_gt_rep-doma using p_gv_package.
    perform dtel_set tables p_gt_rep-dtel using p_gv_package.
    perform table_set tables p_gt_rep-dict using p_gv_package.
    perform ttyp_set tables p_gt_rep-ttyp using p_gv_package.
    perform lock_set tables p_gt_rep-lock using p_gv_package.
    perform snro_set tables p_gt_rep-snro using p_gv_package.
    perform tcode_set tables p_gt_rep-tcode using p_gv_package.
    perform fugr_set using p_gv_package p_gt_rep-program changing p_gt_rep-fugr.
    perform func_set tables p_gt_rep-func using p_gv_package.
    perform class_set tables p_gt_rep-class using p_gv_package.
    modify p_gt_rep.
  endloop.

  commit work and wait.

endform.                    " REP_SET
*&---------------------------------------------------------------------*
*&      Form  CODE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GS_REP_CODE  text
*      -->P_P_PROGRM  text
*----------------------------------------------------------------------*
form code_set using p_program p_type changing p_gv_package p_code structure gs_code.

*代码
  if p_code is initial or p_code-node-sel is initial.
    return.
  endif.

*对象条目目录
  if p_type = '1'. "可执行程序才需要、可以创建
    perform object_directory_set using p_program changing p_gv_package. "选择包，创建（修改）对象目录条目并包入请求
  endif.

  perform request_set using p_gv_package 'PROG' p_program changing gv_request gv_rtype gv_rtmsg. "程序包入请求 以SE03为准
  if gv_rtype = 'S'.
*生成程序
    insert report p_program
      from p_code-code
      program type p_type.
    if sy-subrc = 0.
      perform rep_log using gv_request 'S' '成功' changing p_code-node.
    else.
      perform msg_sys_into changing gv_msg.
      perform rep_log using space 'E' gv_msg changing p_code-node.
    endif.
  else.
    perform rep_log using space gv_rtype gv_rtmsg changing p_code-node.
  endif.
endform.                    " CODE_SET
*&---------------------------------------------------------------------*
*&      Form  object_directory_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PROGRAM  text
*----------------------------------------------------------------------*
form object_directory_set using p_program changing p_gv_package.

  data:
    fs_tadir    type tadir,        " (Structure) TADIR
    fs_tdevc    type tdevc,        " (Structure) TDEVC
    lv_obj_name like e071-obj_name.

  lv_obj_name = p_program.

*为程序选择包，生成对象目录条目，会弹出请求（只会包进对象目录条目，不会包进程序源代码）
*（无法从请求中删除对象目录条目，删除只是se10看不见了，如果修改包到本地还是会提示对象目录条目被请求锁定）
  call function 'TR_TADIR_POPUP_ENTRY_E071'
    exporting
      wi_e071_pgmid             = 'R3TR'
      wi_e071_object            = 'PROG'
      wi_tadir_devclass         = p_gv_package
      wi_e071_obj_name          = lv_obj_name
    importing
      we_tadir                  = fs_tadir
      es_tdevc                  = fs_tdevc
    exceptions
      display_mode              = 1
      exit                      = 2
      global_tadir_insert_error = 3
      no_repair_selected        = 4
      no_systemname             = 5
      no_systemtype             = 6
      no_tadir_type             = 7
      reserved_name             = 8
      tadir_enqueue_failed      = 9
      devclass_not_found        = 10
      tadir_not_exist           = 11
      object_exists             = 12
      internal_error            = 13
      object_append_error       = 14
      tadir_modify_error        = 15
      object_locked             = 16
      no_object_authority       = 17
      others                    = 18.

  if sy-subrc ne 0.
*    message 'Error while creating TADIR entry' type 'S'.
    message '创建对象条目目录出错' type 'E'.
  else.
    p_gv_package = fs_tadir-devclass.
  endif.                               " IF SY-SUBRC NE 0
endform.                    " object_directory_set
*&---------------------------------------------------------------------*
*&      Form  zip_from_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_REP  text
*      <--P_GV_XML  text
*----------------------------------------------------------------------*
form zip_from_data  using p_gt_rep
                  changing p_gv_xml.

  data lo_ref type ref to cx_root. "异常处理
  data lv_text type string.

  data lv_gzip_out type xstring.

*转xml
  call transformation id
    options value_handling = 'MOVE' "防止内表中有N类型dump
    source data = p_gt_rep
    result xml p_gv_xml.

*转xstring（统一编码）
  perform xstring_from_string changing p_gv_xml.

*压缩（现在转了xstring再压缩，不用考虑编码问题了）
*unicode系统只能导出UTF-8，填ANSI会出异常
*非unicode系统导出时可以填UTF-8和ANSI，不填就是默认ANSI
  data lv_abap_encod type abap_encod.
  lv_abap_encod = 'UTF-8'.

  try.
      call method cl_abap_gzip=>compress_text
        exporting
          text_in    = p_gv_xml
          conversion = lv_abap_encod
        importing
          gzip_out   = lv_gzip_out.
    catch cx_root into lo_ref.
      lv_text = lo_ref->get_text( ).
  endtry.

  p_gv_xml = lv_gzip_out.
endform.                    " zip_from_data
*&---------------------------------------------------------------------*
*&      Form  xml_from_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_REP   text
*      -->P_GV_XML   text
*----------------------------------------------------------------------*
form xml_from_data  using p_gt_rep
                  changing p_gv_xml.

  data lo_ref type ref to cx_root. "异常处理
  data lv_text type string.

  data lv_gzip_out type xstring.

*转xml
  call transformation id
    options value_handling = 'MOVE' "防止内表中有N类型dump
    source data = p_gt_rep
    result xml p_gv_xml.
endform.                    "xml_from_data
*&---------------------------------------------------------------------*
*&      Form  FILENAME_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_FILENAME  text
*----------------------------------------------------------------------*
form filename_set using value(p_program) changing p_gv_filename.
*pop
  data:
        lv_def_filename(255),
        v_filename type localfile.

  if p_program is initial.
    read table s_prog index 1.
    p_program = s_prog-low.
  endif.
  concatenate p_program '.LAN' into lv_def_filename.

  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = lv_def_filename
      def_path         = 'C:'
      mask             = ',*.lan ,*.lan.' "待处理
      mode             = 'S'
      title            = '保存repository文件'
    importing
      filename         = v_filename
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.
  if sy-subrc <> 0.
  else.
    p_gv_filename = v_filename.
  endif.

  if p_gv_filename is initial.
    perform msg_and_leave using '选择路径'.
  endif.
endform.                    " FILENAME_SET
*&---------------------------------------------------------------------*
*&      Form  MSG_AND_LEAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form msg_and_leave using p_msg.
  message p_msg type 'S' display like 'E'.
  leave list-processing and return to screen 0.
endform.                    " MSG_AND_LEAVE
*&---------------------------------------------------------------------*
*&      Form  TEXT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      <--P_GT_TEXT  text
*----------------------------------------------------------------------*
form text_get using  p_program changing p_gs_text structure gs_text.
*  data: lv_len(10) type c.
*
*  types: begin of type_s_txtlang,
*           language type spras,
*         end   of type_s_txtlang.
*
*  data: fs_txtlang type type_s_txtlang,
*        lt_txtlang type table of type_s_txtlang,
*              t_txt   type table of textpool.

**取语言数
*  select language
*    from repotext
*    into table lt_txtlang
*   where progname = p_program.
*  if sy-subrc eq 0.
**遍历不同语言取文本
*    loop at lt_txtlang into fs_txtlang.
*      read textpool p_program into t_txt language fs_txtlang-language.
*      if sy-subrc eq 0.
*        append lines of t_txt to p_gs_text-text.
*      endif.
*    endloop.                           " LOOP AT lt_txtlang
*endif.                               " IF SY-SUBRC EQ 0
  read textpool p_program into p_gs_text-text. "测试可以不指定语言
endform.                    " TEXT_GET
*&---------------------------------------------------------------------*
*&      Form  TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PROGRAM  text
*      -->P_P_GS_REP_TEXT  text
*----------------------------------------------------------------------*
form text_set  using    p_program p_gv_package changing p_text structure gs_text.
  if p_text is initial or p_text-node-sel is initial.
    return.
  endif.

  perform request_set using p_gv_package 'REPT' p_program changing gv_request gv_rtype gv_rtmsg. "文本请求
  if gv_rtype = 'S'.
    insert textpool p_program from p_text-text.
    if sy-subrc = 0.
      perform rep_log using gv_request 'S' '成功' changing p_text-node.
    else.
      perform msg_sys_into changing gv_msg.
      perform rep_log using space 'E' gv_msg p_text-node.
    endif.
  else.
    perform rep_log using space gv_rtype gv_rtmsg p_text-node.
  endif.
endform.                    " TEXT_SET
*&---------------------------------------------------------------------*
*&      Form  SCREEN_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SCREEN  text
*      -->P_PROGRAM  text
*----------------------------------------------------------------------*
form screen_get  tables   p_gt_screen structure gt_screen
                 using    p_program.

  data: ls_header               type rpy_dyhead,
        lt_containers           type dycatt_tab,
        lt_fields_to_containers type dyfatc_tab,
        lt_flow_logic           type swydyflow,
        lt_d020s                type table of d020s.

  field-symbols: <ls_d020s>       like line of lt_d020s,
*                 <lv_outputstyle> type scrpostyle,
                 <lv_outputstyle>,                          "兼容r3
                 <ls_field>       like line of lt_fields_to_containers,
                 <ls_dynpro>      like line of gt_screen.


  call function 'RS_SCREEN_LIST'
    exporting
      dynnr     = ''
      progname  = p_program
    tables
      dynpros   = lt_d020s
    exceptions
      not_found = 1
      others    = 2.
  if sy-subrc = 2.

  endif.

* loop dynpros and skip generated selection screens
  loop at lt_d020s assigning <ls_d020s> where type <> 'S'.

    call function 'RPY_DYNPRO_READ'
      exporting
        progname             = p_program
        dynnr                = <ls_d020s>-dnum
      importing
        header               = ls_header
      tables
        containers           = lt_containers
        fields_to_containers = lt_fields_to_containers
        flow_logic           = lt_flow_logic
      exceptions
        cancelled            = 1
        not_found            = 2
        permission_error     = 3
        others               = 4.
    if sy-subrc <> 0.

    endif.

    loop at lt_fields_to_containers assigning <ls_field>.
* output style is a NUMC field, the XML conversion will fail if it contains invalid value
* field does not exist in all versions
      assign component 'OUTPUTSTYLE' of structure <ls_field> to <lv_outputstyle>.
      if sy-subrc = 0 and <lv_outputstyle> = '  '.
        clear <lv_outputstyle>.
      endif.
    endloop.

    append initial line to gt_screen assigning <ls_dynpro>.
    <ls_dynpro>-header     = ls_header.
    <ls_dynpro>-containers = lt_containers.
    <ls_dynpro>-fields     = lt_fields_to_containers.

*    condense_flow( importing et_spaces = <ls_dynpro>-spaces
*                   changing ct_flow = lt_flow_logic ).
    <ls_dynpro>-flow_logic = lt_flow_logic.

  endloop.

endform.                    " SCREEN_GET
*&---------------------------------------------------------------------*
*&      Form  SCREEN_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GS_REP_SCREEN  text
*      -->P_P_PROGRAM  text
*----------------------------------------------------------------------*
form screen_set  tables   p_screen structure gt_screen
                 using    p_object p_gv_package.

  data: lv_name   type dwinactiv-obj_name,
        ls_dynpro like line of gt_screen.

  if p_screen[] is initial.
    return.
  endif.

* ls_dynpro is changed by the function module, a field-symbol will cause
* the program to dump since gt_screen cannot be changed
  loop at p_screen into ls_dynpro where node-sel = 'X'.
*包入请求（对象不存在都可以包），第一步会自己包入请求，这一步是为了获取请求号
    data:
      lv_object(48),
      lv_prog(40),
      lv_dynm(8).
    lv_prog = p_object.
    lv_dynm = ls_dynpro-header-screen.
*      concatenate lv_prog lv_dynm into lv_object respecting blanks. "debug得来，一个shor8+4，一个long40+8 r3不支持respecting blanks
    lv_object(40) = lv_prog.
    lv_object+40(8) = lv_dynm.
    perform request_set using p_gv_package 'DYNP' lv_object changing gv_request gv_rtype gv_rtmsg.
    if gv_rtype = 'S'.
*生成屏幕
      ls_dynpro-header-program = p_object.
      call function 'RPY_DYNPRO_INSERT'
        exporting
          header                 = ls_dynpro-header
          suppress_exist_checks  = abap_true
        tables
          containers             = ls_dynpro-containers
          fields_to_containers   = ls_dynpro-fields
          flow_logic             = ls_dynpro-flow_logic
        exceptions
          cancelled              = 1
          already_exists         = 2
          program_not_exists     = 3
          not_executed           = 4
          missing_required_field = 5
          illegal_field_value    = 6
          field_not_allowed      = 7
          not_generated          = 8
          illegal_field_position = 9
          others                 = 10.
      if sy-subrc = 0.
        perform rep_log using gv_request 'S' '成功' changing ls_dynpro-node.
*    elseif sy-subrc = 2.
      else.
        perform msg_sys_into changing gv_msg.
        perform rep_log using space 'E' gv_msg changing ls_dynpro-node.
      endif.
    else.
      perform rep_log using space gv_rtype gv_rtmsg changing ls_dynpro-node.
    endif.
    modify p_screen from ls_dynpro.
* todo, RPY_DYNPRO_UPDATE?


  endloop.
endform.                    " SCREEN_SET
*&---------------------------------------------------------------------*
*&      Form  TABLE_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_TABLE  text
*      -->P_PROGRAM  text
*----------------------------------------------------------------------*
form table_get  tables   p_gt_table structure gt_table
                                     p_gt_ttyp structure gt_ttyp
                                     p_gt_dtel structure gt_dtel.

  data: begin of lt_dd02l occurs 0,
          tabname  type dd02l-tabname,
          as4local type dd02l-as4local,
          tabclass type dd02l-tabclass,
        end of lt_dd02l.

  data lv_exsit.
  data ls_strcture like line of gt_table-istructure.

  loop at p_gt_table where dd09v is initial. "没取过的
    perform table_definition_get changing p_gt_table.
    modify p_gt_table.

*添加表中的自建数据元素（或结构，如果是结构就需要递归）
    loop at p_gt_table-istructure into ls_strcture where rollname(1) = 'Z'. "字段类型有可能是结构
      perform dict_add tables p_gt_table p_gt_ttyp p_gt_dtel using ls_strcture-rollname.
    endloop.
  endloop.

endform.                    " TABLE_GET

*&---------------------------------------------------------------------*
*&      Form  table_description_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE             text
*      -->(TABLENAME)       text
*      -->TABLEDESCRIPTION  text
*----------------------------------------------------------------------*
form table_description_get using value(tablename)
                                      changing tabledescription.

  select single ddtext
                from dd02t
                into tabledescription
                where tabname = tablename
                 and ddlanguage = sy-langu.
endform.                                                                                          "table_description_get

*----------------------------------------------------------------------------------------------------------------------
*  table_definition_get... Find the structure of a table from the SAP database.
*----------------------------------------------------------------------------------------------------------------------
form table_definition_get changing wa_table like gt_table.

  data gotstate like dcobjif-gotstate.
  data: lt_dd03p type standard table of dd03p with header line.
  data: wadictstruct type dd03p.
  data:
    ls_dd02v like dd02v,
    ls_dd09l like dd09v.

  check wa_table-dd09v is initial. "只取没取过的

  call function 'DDIF_TABL_GET'
    exporting
      name          = wa_table-tablename
      state         = 'A'
      langu         = sy-langu
    importing
      gotstate      = gotstate
      dd02v_wa      = ls_dd02v
      dd09l_wa      = ls_dd09l
    tables
      dd03p_tab     = lt_dd03p
    exceptions
      illegal_input = 1
      others        = 2.

  if sy-subrc = 0 and gotstate = 'A'.
    clear:wa_table-dd02v,wa_table-dd09v,wa_table-istructure[].
    move-corresponding ls_dd02v to wa_table-dd02v.
    move-corresponding ls_dd09l to wa_table-dd09v.
    loop at lt_dd03p.
      perform removeleadingzeros changing lt_dd03p-position.
      perform removeleadingzeros changing lt_dd03p-leng.
      append lt_dd03p to wa_table-istructure.
    endloop.
  endif.
endform.                                                                                           "table_definition_get

*&---------------------------------------------------------------------*
*&      Form  removeleadingzeros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->MYVALUE    text
*----------------------------------------------------------------------*
form removeleadingzeros changing myvalue.

  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = myvalue
    importing
      output = myvalue
    exceptions
      others = 1.
endform.                                                                             "removeLeadingZeros
*&---------------------------------------------------------------------*
*&      Form  table_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GS_REp_table  text
*----------------------------------------------------------------------*
form table_set  tables   p_table structure gt_table using p_gv_package.
*TRANSP	透明表格
*INTTAB	结构
*CLUSTER  簇表
*POOL	共享表格
*VIEW	一般视图结构
*APPEND	附加结构

  data: lv_rc       like sy-subrc,
        lv_obj_name type tadir-obj_name,
        ls_dd02v    type dd02v,
        ls_dd09l    type dd09l,
        lt_dd03p    type standard table of dd03p with default key.

  data ls_structure type dd03p.
  data:
    lv_msg1(255),
    lv_msg2(255).

  field-symbols: <ls_dd03p> like line of lt_dd03p.

  if p_table[] is initial.
    return.
  endif.

  loop at p_table where node-sel = 'X'.

*修改属性值
    if p_tab = 'X'.
      clear:gs_popk,gt_pop,gt_pop[],sy-ucomm.
      concatenate '表' p_table-tablename into gs_popk-title.
      pop_key_append 'TABLENAME' '表名' p_table-tablename.
      call screen 2003 starting at 30 10 ending at 100 20.
      if sy-ucomm = 'OK'.
        loop at gt_pop.
          case gt_pop-key.
            when 'TABLENAME'.
              p_table-tablename = gt_pop-value.
              ls_dd02v = p_table-dd02v. "抬头
              ls_dd02v-tabname = gt_pop-value.
              p_table-dd02v = ls_dd02v.
              ls_dd09l = p_table-dd09v. "技术设置
              ls_dd09l-tabname = gt_pop-value.
              p_table-dd09v = ls_dd09l.
              loop at p_table-istructure into ls_structure.
                ls_structure-tabname = gt_pop-value.
                modify p_table-istructure from ls_structure.
              endloop.
          endcase.
        endloop.
      endif.
    endif.

*包入请求（对象不存在都可以包）
    data lv_object(40).
    concatenate 'TABL' p_table-tablename into lv_object.
    perform request_set using p_gv_package 'DICT' lv_object changing gv_request gv_rtype gv_rtmsg. "表、结构请求（debug RS_DD_COPY_OBJ得来）
    if gv_rtype = 'S'.
      clear:ls_dd09l,ls_dd02v,lt_dd03p,lt_dd03p[],lv_msg1,lv_msg2.

      ls_dd02v = p_table-dd02v. "抬头
      ls_dd09l = p_table-dd09v. "技术设置

      loop at p_table-istructure into ls_structure.
        append initial line to lt_dd03p assigning <ls_dd03p>.
        <ls_dd03p> = ls_structure.
      endloop.

      call function 'DDIF_TABL_PUT' "可以新建修改表、结构
        exporting
          name              = p_table-tablename
          dd02v_wa          = ls_dd02v
          dd09l_wa          = ls_dd09l
        tables
          dd03p_tab         = lt_dd03p
        exceptions
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          others            = 6.
      if sy-subrc <> 0.
        perform msg_sys_into changing lv_msg1.
*    else.
*      lv_msg1 = '生成表成功'.
      endif.

*激活
      call function 'DDIF_TABL_ACTIVATE' "日志在表里
        exporting
          name        = p_table-tablename
          auth_chk    = abap_false
        importing
          rc          = lv_rc
        exceptions
          not_found   = 1
          put_failure = 2
          others      = 3.
      if sy-subrc <> 0 or lv_rc > 4. "rc = 0 成功 = 4 警告 >4 失败
        lv_msg2 = '激活表出错'.
      endif.

*记录日志
      clear gv_msg.
      concatenate lv_msg1 lv_msg2 into gv_msg separated by space.
      if lv_msg1 is initial and lv_msg2 is initial.
        perform rep_log using gv_request 'S' '成功' changing p_table-node.
      else.
        perform rep_log using space 'E' gv_msg changing p_table-node.
      endif.
    else.
      perform rep_log using space gv_rtype gv_rtmsg changing p_table-node.
    endif.
    modify p_table.
  endloop.
endform.                    " table_set
*&---------------------------------------------------------------------*
*&      Form  REQUEST_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_REQUEST  text
*----------------------------------------------------------------------*
form request_select  changing p_gt_request type trwbo_request_headers.
  data: lrs_trfunction type trsel_trs_function,
        lv_types       type string,
        ls_ranges      type trsel_ts_ranges.

  " Fill all request types
  lv_types = 'KWTCOEMPDRSXQFG'.
  lrs_trfunction-sign   = 'I'.
  lrs_trfunction-option = 'EQ'.
  while lv_types <> space.
    lrs_trfunction-low = lv_types(1).
    append lrs_trfunction to ls_ranges-request_funcs.
    shift lv_types.
  endwhile.

  call function 'TRINT_SELECT_REQUESTS'
    exporting
      iv_username_pattern    = sy-uname
      iv_via_selscreen       = 'X'
      iv_complete_projects   = ''
      iv_title               = 'abapGit: Transport Request Selection'                                                                                                                                                                            "is_popup = ''
    importing
      et_requests            = gt_request[]
    changing
      cs_ranges              = ls_ranges
    exceptions
      action_aborted_by_user = 1
      others                 = 2.
endform.                    " REQUEST_SELECT
*&---------------------------------------------------------------------*
*&      Form  REQUEST_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GV_PACKAGE  text
*      -->P_1532   text
*      -->P_P_PROGRAM  text
*----------------------------------------------------------------------*
form request_set  using p_gv_package p_type p_program
                           changing ordernum rtype rtmsg.

  data:
    lv_ordernum    like  e070-trkorr,
    lv_devclass    like tadir-devclass,
    lv_global_lock.

  clear:rtype,rtmsg.

  if p_type = 'FUGR' "不加锁在包函数组时会报错："这一语法是不能用于对象名称"
    or p_type = 'ENQU'
    or p_type = 'NROB'.
    lv_global_lock = 'X'.
  endif.

  free memory id 'RESUL1'. "重复包函数时会返回空请求号，debug找到缓存位置

  call function 'RS_CORR_INSERT'
    exporting
      korrnum             = ordernum
      global_lock         = lv_global_lock
      object              = p_program
      object_class        = p_type
      devclass            = p_gv_package
      master_language     = '1'
*     mode                = 'INSERT' "如果是insert每次都会创建对象条目目录
      mode                = 'I' "20210318更新：测试 I 传入请求号不传包都不会弹框了
*     object_class_supports_ma = 'X'
    importing
      ordernum            = lv_ordernum
      devclass            = lv_devclass
    exceptions
      cancelled           = 1
      permission_failure  = 2
      unknown_objectclass = 3
      others              = 4.
  if sy-subrc ne 0.
    rtype = 'E'.
    perform msg_sys_into changing rtmsg.
  else.

    "如果函数组在请求中，则函数跟随函数组（且请求中只有函数组没有函数，不返回请求号算了）
*    if lv_devclass is initial.
*      lv_devclass = '$TMP'.
*      lv_ordernum = lv_devclass.
*    endif.

*    p_gv_package = lv_devclass.
    ordernum = lv_ordernum.

    rtype = 'S'.
    rtmsg = '创建请求成功'.
  endif.
endform.                    " REQUEST_SET
*&---------------------------------------------------------------------*
*&      Form  DICT_CHECK_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OBJNAME  text
*      -->P_OBJTYPE  text
*----------------------------------------------------------------------*
*form dict_check_exist  using    p_objname
*                                p_objtype.
*  CALL FUNCTION 'INTERN_DD_CHECK_EXIST'
*    EXPORTING
*      objname     = p_objname
*      objtype     = p_objtype
*      err_message = 'X'
*      objstate    = 'A'
*      exists      = 'X'
*    IMPORTING
*      masterlangu = masterlangu
*      proxy_type  = g_proxy_type.
*endform.                    " DICT_CHECK_EXIST
*&---------------------------------------------------------------------*
*&      Form  rep_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rep_display tables p_gt_rep structure gt_rep.
  if p_gt_rep[] is not initial.
    call screen 100.
  else.
*    message '资源库数据为空' type 'S' display like 'E'.
*    leave to screen 0.
  endif.
endform.                    " rep_display
*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pbo_100 output.

  set pf-status 'REP_DIS'.
  set titlebar 'REP_DIS'.

  if g_custom_container is initial.
    perform tree_init tables gt_rep gt_rep_out.

    call method cl_gui_cfw=>flush
      exceptions
        cntl_system_error = 1
        cntl_error        = 2.
    if sy-subrc ne 0.
      call function 'POPUP_TO_INFORM'
        exporting
          titel = '自动化队列出错'(801)
          txt1  = '内部错误:'(802)
          txt2  = '在自动化队列中的方法'(803)
          txt3  = '触发了一个错误.'(804).
    endif.
  endif.

  call method g_alv_tree->column_optimize.
endmodule.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai_100 input.
  case sy-ucomm.
    when 'EXIT' or 'BACK' or 'CANC'.
      perform program_exit.
    when 'GENERATE'.
      perform tree_checked_items_get tables gt_selected_node using g_alv_tree.
      gv_state = 1.
      perform rep_sel tables gt_rep gt_selected_node.
      perform rep_set tables gt_rep changing gv_package.
      perform tree_refresh tables gt_rep gt_rep_out.
    when 'SEL_ALL' or 'DESEL_ALL'.
      perform tree_refresh tables gt_rep gt_rep_out.
    when 'RENAME'.
      clear:p_func,p_tab,p_tcode.
      call selection-screen 2004 starting at 30 10 ending at 90 20.
    when others.

* Toolbar events are registered in constructur method of
* CL_ALV_TREE_BASE as application events. So the dispatch call
* is a must if you want to use the standard toolbar.
      call method cl_gui_cfw=>dispatch.

  endcase.

  call method cl_gui_cfw=>flush.
endmodule.                 " PAI  INPUT

*&---------------------------------------------------------------------*
*&      Form  tree_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form tree_init tables p_gt_rep structure gt_rep p_gt_rep_out structure gt_rep_out.
*创建容器
  create object g_custom_container
    exporting
      repid                       = sy-repid
      dynnr                       = '100'
      side                        = cl_gui_docking_container=>dock_at_left "容器贴屏幕左边
      extension                   = cl_gui_control=>ws_maximizebox "容器宽度
      style                       = cl_gui_control=>ws_child "可选参数，设置容器是否可用手动拖动大小
    exceptions
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      others                      = 6.

  if sy-subrc <> 0.
    message s001(00) with '屏幕容器初始化失败'.
    leave list-processing.
  endif.

* create tree control
  create object g_alv_tree
    exporting
      parent                      = g_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
*     node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      item_selection              = 'X'
      no_html_header              = 'X'
      no_toolbar                  = ''
    exceptions
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  if sy-subrc <> 0.
    message x208(00) with 'ERROR'.                          "#EC NOTEXT
  endif.

*tree节点标题
  data l_hierarchy_header type treev_hhdr.
  perform tree_hierarchy_header_build changing l_hierarchy_header.
  perform tree_fieldcatalog_build.

  call method g_alv_tree->set_table_for_first_display
    exporting
      is_hierarchy_header = l_hierarchy_header
    changing
      it_fieldcatalog     = gt_fieldcatalog
      it_outtab           = p_gt_rep_out[]. "table must be empty !

*节点
  perform tree_hierarchy_create tables p_gt_rep  p_gt_rep_out.
  perform tree_events_register.

* Send data to frontend.
  call method g_alv_tree->frontend_update.

endform.                               " tree_init

*&---------------------------------------------------------------------*
*&      Form  tree_hierarchy_header_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
form tree_hierarchy_header_build changing
                               p_hierarchy_header type treev_hhdr.

  clear p_hierarchy_header.
  p_hierarchy_header-heading = '对象名'(300).
  p_hierarchy_header-tooltip = '程序、屏幕、表等对象'(400).
  p_hierarchy_header-width = 35.
  p_hierarchy_header-width_pix = ''.

endform.                               " tree_hierarchy_header_build

*&---------------------------------------------------------------------*
*&      Form  tree_fieldcatalog_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form tree_fieldcatalog_build.
  data: ls_fieldcatalog type lvc_s_fcat.

  clear:gt_fieldcatalog,gt_fieldcatalog[].

  perform fieldcat_build tables gt_fieldcatalog using 'OTYPE' '对象类型' space space space space space space space.
*  perform fieldcat_build tables gt_fieldcatalog using 'DESCP' '描述' space space space space space space space.
  perform fieldcat_build tables gt_fieldcatalog using 'RTYPE' '消息类型' space space space space space space space.
  perform fieldcat_build tables gt_fieldcatalog using 'RTMSG' '消息文本' space space space space space space space.
  perform fieldcat_build tables gt_fieldcatalog using 'REQUEST' '请求' space space space space space space space.

* Now change the fieldcatalog to hide fields and to determine
* some initial calculations for chosen fields.
  loop at gt_fieldcatalog into ls_fieldcatalog.
    case ls_fieldcatalog-fieldname.
      when 'PLANETYPE'.
        ls_fieldcatalog-icon = 'X'.
    endcase.
    modify gt_fieldcatalog from ls_fieldcatalog.
  endloop.

endform.                               " tree_fieldcatalog_build

*&---------------------------------------------------------------------*
*&      Form  tree_hierarchy_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form tree_hierarchy_create tables p_gt_rep structure gt_rep p_gt_rep_out structure gt_rep_out.
  data:
    l_top_key    type lvc_nkey,
    lv_sub_key   type lvc_nkey, "第二层文件夹的key
    l_new_key    type lvc_nkey,
    p_node_text  type lvc_value,
    p_node_image type tv_image,
    p_item_image type tv_image.

  data:
    ls_sta    type ty_sta,
    ls_screen like gt_screen,
    ls_dict   like gt_table,
    ls_dtel   like gt_dtel,
    ls_ttyp   like gt_ttyp,
    ls_doma   like gt_doma,
    ls_lock   like gt_lock,
    ls_snro   like gt_snro,
    ls_node   type ty_node,
    ls_tcode  type ty_tcode,
    ls_func   type ty_func,
    ls_class  like gt_class.

*资源库数据输出转换
  loop at p_gt_rep.

*插入文件夹
    clear:p_gt_rep_out.
    p_gt_rep_out-otype = p_gt_rep-typet.
*    p_gt_rep_out-descp = p_gt_rep-descp. "有空了再做
    p_node_text = p_gt_rep-program.
    if p_gt_rep-type = 'F'. "函数组去掉SAPL
      shift p_node_text left by 4 places.
    endif.
    perform tree_add_a_folder using p_gt_rep_out space p_node_text space
                         changing l_top_key.
    p_gt_rep-node-key = l_top_key.

*插入节点
    if p_gt_rep-code is not initial.
      clear:p_gt_rep_out.
      p_node_text = p_gt_rep-program.
      p_gt_rep_out-otype = '源代码'.
      move-corresponding p_gt_rep-code-node to p_gt_rep_out.
      if gv_state = 0. "只上传未执行
        perform object_exsit_check using p_gt_rep-program 'PROGRAM' changing p_gt_rep_out.
      endif.
      perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                space p_item_image
                         changing l_new_key.
      p_gt_rep-code-node-key = l_new_key.
    endif.

    if p_gt_rep-text is not initial.
      clear:p_gt_rep_out.
      p_node_text = p_gt_rep-program.
      p_gt_rep_out-otype = '文本池'.
      move-corresponding p_gt_rep-text-node to p_gt_rep_out.
      if gv_state = 0. "只上传未执行 "还没执行过
        perform object_exsit_check using p_gt_rep-program 'REPT' changing p_gt_rep_out.
      endif.
      perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                space p_item_image
                         changing l_new_key.
      p_gt_rep-text-node-key = l_new_key.
    endif.

    if p_gt_rep-cua is not initial.
      loop at p_gt_rep-cua-sta into ls_sta.
        clear:p_gt_rep_out.
        p_node_text = ls_sta-sta-code.
        p_gt_rep_out-otype = '状态栏'.
        move-corresponding ls_sta-node to p_gt_rep_out.
        perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                  space p_item_image
                           changing l_new_key.
        ls_sta-node-key = l_new_key.
        modify p_gt_rep-cua-sta from ls_sta.
      endloop.
    endif.

    if p_gt_rep-screen2  is not initial.
      loop at p_gt_rep-screen2  into ls_screen.
        clear:p_gt_rep_out.
        p_node_text = ls_screen-header-screen.
        p_gt_rep_out-otype = '屏幕'.
        move-corresponding ls_screen-node to p_gt_rep_out.
        perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                  space p_item_image
                           changing l_new_key.
        ls_screen-node-key = l_new_key.
        modify p_gt_rep-screen2  from ls_screen.
      endloop.
    endif.

*表类型
    loop at p_gt_rep-ttyp into ls_ttyp.
      perform tree_node_add using ls_ttyp-typename '表类型' 'TTYP' l_top_key
                                           changing ls_ttyp.
      modify p_gt_rep-ttyp from ls_ttyp.
    endloop.

    if p_gt_rep-dict is not initial.
      clear:p_gt_rep_out.
      loop at p_gt_rep-dict into ls_dict.
        clear:p_gt_rep_out.
        p_node_text = ls_dict-tablename.
        case ls_dict-dd02v-tabclass.
          when 'TRANSP'.
            p_gt_rep_out-otype = '表'.
          when 'INTTAB'.
            p_gt_rep_out-otype = '结构'.
        endcase.
        move-corresponding ls_dict-node to p_gt_rep_out.
        if gv_state = 0. "只上传未执行 "还没执行过
          perform object_exsit_check using ls_dict-tablename 'DD02L' changing p_gt_rep_out.
        endif.
        perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                  space p_item_image
                           changing l_new_key.
        ls_dict-node-key = l_new_key.
        modify p_gt_rep-dict from ls_dict.
      endloop.
    endif.

    if p_gt_rep-dtel is not initial.
      loop at p_gt_rep-dtel into ls_dtel.
        clear:p_gt_rep_out.
        p_node_text = ls_dtel-name.
        p_gt_rep_out-otype = '数据元素'.
        move-corresponding ls_dtel-node to p_gt_rep_out.
        if gv_state = 0. "只上传未执行 "还没执行过
          perform object_exsit_check using ls_dtel-name 'DTEL' changing p_gt_rep_out.
        endif.
        perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                  space p_item_image
                           changing l_new_key.
        ls_dtel-node-key = l_new_key.
        modify p_gt_rep-dtel from ls_dtel.
      endloop.
    endif.

    if p_gt_rep-doma is not initial.
      loop at p_gt_rep-doma into ls_doma.
        clear:p_gt_rep_out.
        p_node_text = ls_doma-name.
        p_gt_rep_out-otype = '域'.
        move-corresponding ls_doma-node to p_gt_rep_out.
        if gv_state = 0. "只上传未执行 "还没执行过
          perform object_exsit_check using ls_doma-name 'DOMA' changing p_gt_rep_out.
        endif.
        perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                  space p_item_image
                           changing l_new_key.
        ls_doma-node-key = l_new_key.
        modify p_gt_rep-doma from ls_doma.
      endloop.
    endif.

    if p_gt_rep-lock is not initial.
      loop at p_gt_rep-lock into ls_lock.
        clear:p_gt_rep_out.
        p_node_text = ls_lock-name.
        p_gt_rep_out-otype = '锁对象'.
        move-corresponding ls_lock-node to p_gt_rep_out.
        if gv_state = 0. "只上传未执行 "还没执行过
          perform object_exsit_check using ls_lock-name 'LOCK' changing p_gt_rep_out.
        endif.
        perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                  space p_item_image
                           changing l_new_key.
        ls_lock-node-key = l_new_key.
        modify p_gt_rep-lock from ls_lock.
      endloop.
    endif.

    if p_gt_rep-snro is not initial.
      loop at p_gt_rep-snro into ls_snro.
        clear:p_gt_rep_out.
        p_node_text = ls_snro-object.
        p_gt_rep_out-otype = '编号范围对象'.
        move-corresponding ls_snro-node to p_gt_rep_out.
        if gv_state = 0. "只上传未执行 "还没执行过
          perform object_exsit_check using ls_snro-object 'SNRO' changing p_gt_rep_out.
        endif.
        perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                  space p_item_image
                           changing l_new_key.
        ls_snro-node-key = l_new_key.
        modify p_gt_rep-snro from ls_snro.
      endloop.
    endif.

    if p_gt_rep-tcode is not initial.
      loop at p_gt_rep-tcode into ls_tcode.
        clear:p_gt_rep_out.
        p_node_text = ls_tcode-tstc-tcode.
        p_gt_rep_out-otype = '事务码'.
        move-corresponding ls_tcode-node to p_gt_rep_out.
        if gv_state = 0. "只上传未执行 "还没执行过
          perform object_exsit_check using ls_tcode-tstc-tcode 'TCODE' changing p_gt_rep_out.
        endif.
        perform tree_add_a_node using  p_gt_rep_out l_top_key p_node_text
                                  space p_item_image
                           changing l_new_key.
        ls_tcode-node-key = l_new_key.
        modify p_gt_rep-tcode from ls_tcode.
      endloop.
    endif.

*类
    loop at p_gt_rep-class into ls_class.
      perform tree_node_add using ls_class-clsname '类' 'CLASS' l_top_key
                                           changing ls_class.
      modify p_gt_rep-class from ls_class.
    endloop.

*函数组
    if p_gt_rep-fugr-area is not initial.
      perform tree_node_add using p_gt_rep-fugr-area '函数组' 'FUGR' l_top_key
                                           changing p_gt_rep-fugr.
    endif.

*函数模块
    loop at p_gt_rep-func into ls_func.
      perform tree_node_add using ls_func-functionname '函数模块' 'FUNC' l_top_key
                                           changing ls_func.
      modify p_gt_rep-func from ls_func.
    endloop.

    modify p_gt_rep.

    call method g_alv_tree->expand_node
      exporting
        i_node_key = l_top_key.
  endloop.
endform.                               " tree_hierarchy_create

*&---------------------------------------------------------------------*
*&      Form  tree_events_register
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form tree_events_register.
  data: lt_events type cntl_simple_events,
        l_event   type cntl_simple_event.

* Frontend registration: do not forget to register (for ALV Tree
* mandatory) tree events.
*................................................................
* The following four tree events registers ALV Tree in the constructor
* method itself.
*    - cl_gui_column_tree=>eventid_expand_no_children
* (needed to load data to frontend when a user expands a node)
*    - cl_gui_column_tree=>eventid_header_context_men_req
* (needed for header context menu)
*    - cl_gui_column_tree=>eventid_header_click
* (allows selection of columns (only when item selection activated))
*   - cl_gui_column_tree=>eventid_item_keypress
* (needed for F1-Help (only when item selection activated))
*
* Nevertheless you have to provide their IDs again if you register
* additional events with SET_REGISTERED_EVENTS (see below).
* To do so, call first method  GET_REGISTERED_EVENTS (this way,
* all already registered events remain registered, even your own):
  call method g_alv_tree->get_registered_events
    importing
      events = lt_events.

* (If you do not these events will be deregistered!!!).
* You do not have to register events of the toolbar again.

*注册事件
  l_event-eventid = cl_gui_simple_tree=>eventid_node_double_click. "双击
*  l_event-appl_event = 'X'. " process PAI if event occurs
  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change. "勾选checkbox
  append l_event to lt_events.

*给事件分配处理器
  create object g_application.
  set handler g_application->handle_node_double_click for g_alv_tree.

* register events on frontend
  call method g_alv_tree->set_registered_events
    exporting
      events                    = lt_events
    exceptions
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  if sy-subrc <> 0.
    message x208(00) with 'ERROR'.                          "#EC NOTEXT
  endif.
*--------------------

endform.                               " tree_events_register

*&---------------------------------------------------------------------*
*&      Form  program_exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form program_exit.

  call method g_alv_tree->free.
  clear g_alv_tree.

  call method g_custom_container->free.
  clear g_custom_container.

  leave to screen 0.

endform.                               " program_exit

*&---------------------------------------------------------------------*
*&      Form  tree_add_a_folder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RELAT_KEY   text
*      -->P_NODE_TEXT   text
*      -->P_NODE_IMAGE  text
*      -->P_NEW_KEY     text
*----------------------------------------------------------------------*
form tree_add_a_folder using p_gt_rep_out p_relat_key type lvc_nkey
                         p_node_text type lvc_value
                         p_node_image type tv_image
                changing p_new_key.

  data: l_layout_node type lvc_s_layn.
  data: lt_item_layout type lvc_t_layi,
        ls_item_layout type lvc_s_layi.

  l_layout_node-isfolder = 'X'.   "=>add a folder, NOT a leaf
  l_layout_node-n_image = p_node_image. "=>Display an icon

** set item-layout
*  ls_item_layout-fieldname = g_alv_tree->c_hierarchy_column_name.
*  ls_item_layout-class   = cl_gui_column_tree=>item_class_checkbox.
*  ls_item_layout-editable = 'X'.
*  if gv_state = 0.
*    ls_item_layout-chosen   = 'X'.
*  endif.
*  append ls_item_layout to lt_item_layout.

  call method g_alv_tree->add_node
    exporting
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = p_node_text
      is_outtab_line   = p_gt_rep_out
      is_node_layout   = l_layout_node
      it_item_layout   = lt_item_layout
    importing
      e_new_node_key   = p_new_key.

endform.                    "tree_add_a_folder

*&---------------------------------------------------------------------*
*&      Form  insert_icons
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_SFLIGHT text
*----------------------------------------------------------------------*
form insert_icons tables pt_sflight structure sflight.
  data: ls_sflight type sflight.
*§C2.Provide icons for this column. In this example column
*    planetype is taken for icon output. The text is replaced.
  loop at pt_sflight into ls_sflight.
    case ls_sflight-planetype.
      when 'DC-10-10'.
        ls_sflight-planetype = '@7T@'.
      when 'DC-8-72'.
        ls_sflight-planetype = '@AV@'.
      when others.
        concatenate '@' ls_sflight-planetype+1(2) '@'
               into ls_sflight-planetype.
    endcase.
    modify pt_sflight from ls_sflight.
  endloop.

endform.                    " INSERT_ICONS

*&---------------------------------------------------------------------*
*&      Form  tree_add_a_node
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SFLIGHT     text
*      -->P_RELAT_KEY   text
*      -->P_NODE_TEXT   text
*      -->P_NODE_IMAGE  text
*      -->P_ITEM_IMAGE  text
*      -->P_NEW_KEY     text
*----------------------------------------------------------------------*
form tree_add_a_node using  p_rep_out like gt_rep_out
                       p_relat_key type lvc_nkey
                       p_node_text type lvc_value
                       p_node_image type tv_image
                       p_item_image type tv_image
              changing p_new_key.

  data: l_layout_node type lvc_s_layn.
  data: lt_item_layout type lvc_t_layi,
        ls_item_layout type lvc_s_layi.

* set item-layout
  ls_item_layout-fieldname = g_alv_tree->c_hierarchy_column_name.
  ls_item_layout-class   = cl_gui_column_tree=>item_class_checkbox.
  ls_item_layout-editable = 'X'.
  if gv_state = 0.
    ls_item_layout-chosen   = 'X'.
  endif.

*全选/取消全选按钮
  if sy-ucomm = 'SEL_ALL'.
    ls_item_layout-chosen   = 'X'.
  elseif sy-ucomm = 'DESEL_ALL'.
    ls_item_layout-chosen   = ''.
  endif.

  append ls_item_layout to lt_item_layout.

*  l_layout_node-n_image = p_node_image.
*  if not p_item_image is initial.
*    ls_item_layout-fieldname = 'CARRID'.
*    ls_layout_item-t_image = p_item_image.
*    append ls_layout_item to lt_layout_item.
*  endif.

  call method g_alv_tree->add_node
    exporting
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = p_node_text
      is_outtab_line   = p_rep_out
      is_node_layout   = l_layout_node
      it_item_layout   = lt_item_layout
    importing
      e_new_node_key   = p_new_key.

endform.                    "tree_add_a_node

*&---------------------------------------------------------------------*
*&      Form  fieldcat_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_FIELDCAT       text
*      -->FIELDNAME         text
*      -->COLTEXT           text
*      -->REF_TAB           text
*      -->REF_FIELD         text
*      -->CONVEXIT          text
*      -->DROP_DOWN_HANDLE  text
*      -->F4AVAILABL        text
*      -->CHECKTABLE        text
*      -->EDIT              text
*----------------------------------------------------------------------*
form fieldcat_build tables lt_fieldcat structure lvc_s_fcat
  using fieldname coltext ref_tab ref_field convexit drop_down_handle f4availabl checktable edit.
  clear lt_fieldcat.
  lt_fieldcat-fieldname = fieldname.
  lt_fieldcat-coltext = coltext.
  lt_fieldcat-ref_table = ref_tab.
  lt_fieldcat-ref_field = ref_field.
  lt_fieldcat-convexit  = convexit.
  lt_fieldcat-drdn_field = drop_down_handle.
  lt_fieldcat-f4availabl = f4availabl.
  lt_fieldcat-checktable = checktable.
  lt_fieldcat-edit = edit.

  append lt_fieldcat.
endform.                    "fieldcat_build

*&---------------------------------------------------------------------*
*&      Form  object_exsit_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3697   text
*      <--P_P_GT_REP_OUT_EXSIT  text
*----------------------------------------------------------------------*
form object_exsit_check  using p_object p_type
                  changing p_gt_rep_out like gt_rep_out.

*  data:
*        lv_progname type reposrc-progname,
*        ls_dd02l like dd02l,
*        ls_dd04l like dd04l,
*        ls_dd01v  like dd01v,
*        ls_dd25l like dd25l,
*        ls_nriv like nriv,
*        ls_tstc like tstc.
  data t_txt   type table of textpool.

  clear:p_gt_rep_out-rtype,p_gt_rep_out-rtmsg.

  case p_type.
    when 'PROGRAM'.
      sql_exsit_check reposrc progname.
    when 'REPT'.
*      read textpool p_object into t_txt.
    when 'DD02L'.
      sql_exsit_check dd02l tabname.
    when 'DTEL'.
      sql_exsit_check dd04l rollname.
    when 'TTYP'.
      sql_exsit_check dd40l typename.
    when 'DOMA'.
      sql_exsit_check dd01v domname.
    when 'LOCK'.
      sql_exsit_check dd25l viewname.
    when 'SNRO'.
      sql_exsit_check tnro  object.
    when 'TCODE'.
      sql_exsit_check tstc tcode.
    when 'FUGR'.
      sql_exsit_check tlibt area.
    when 'FUNC'.
      sql_exsit_check tfdir funcname.
    when 'CLASS'.
      sql_exsit_check seoclass  clsname.
    when others.
  endcase.

  if p_gt_rep_out-exsit = 'X'.
    p_gt_rep_out-rtype = 'W'.
    if p_type = 'F'. "不更新函数组了（就一个描述）
      p_gt_rep_out-rtmsg = '存在同名对象，不会执行更新'.
    else.
      p_gt_rep_out-rtmsg = '存在同名对象，执行将会直接覆盖'.
    endif.
  endif.

endform.                    " object_exsit_check
*&---------------------------------------------------------------------*
*&      Form  TREE_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_REP  text
*      -->P_GT_REP_OUT  text
*----------------------------------------------------------------------*
form tree_refresh  tables   p_gt_rep structure gt_rep
                            p_gt_rep_out structure gt_rep_out.
  call method g_alv_tree->delete_all_nodes.
  perform tree_hierarchy_create tables p_gt_rep  p_gt_rep_out.
  call method g_alv_tree->frontend_update.
endform.                    " TREE_REFRESH
*&---------------------------------------------------------------------*
*&      Form  DTEL_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_DTEL  text
*      -->P_ls_table_TABLENAME  text
*----------------------------------------------------------------------*
form dtel_add  tables   p_gt_dtel structure gt_dtel
               using    p_name.
  data lv_exsit.

  read table p_gt_dtel transporting no fields with key name = p_name.
  if sy-subrc ne 0.
    loop at gt_rep.
      read table gt_rep-dtel transporting no fields with key name = p_name.
      if sy-subrc = 0 .
        lv_exsit = 'X'.
        exit.
      endif.
    endloop.
    if lv_exsit is initial.
      p_gt_dtel-name = p_name.
      append p_gt_dtel.
    endif.
  endif.
endform.                    " DTEL_ADD
*&---------------------------------------------------------------------*
*&      Form  DTEL_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_DTEL  text
*      -->P_PROGRAM  text
*----------------------------------------------------------------------*
form dtel_get  tables   p_gt_dtel structure gt_dtel.
  data name     type ddobjname.
  data state    type ddobjstate.
  data langu    type sy-langu.
  data gotstate type ddgotstate.
  data dd04v_wa type dd04v.
  data tpara_wa type tpara.

  loop at p_gt_dtel.
    call function 'DDIF_DTEL_GET'
      exporting
        name          = p_gt_dtel-name
        state         = 'A'
        langu         = '1'
      importing
        gotstate      = gotstate
        dd04v_wa      = dd04v_wa
        tpara_wa      = tpara_wa
      exceptions
        illegal_input = 1.
    p_gt_dtel-dd04v = dd04v_wa.
    p_gt_dtel-tpara = tpara_wa.
    modify p_gt_dtel.
  endloop.

endform.                    " DTEL_GET
*&---------------------------------------------------------------------*
*&      Form  DTEL_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GS_REP_DTEL  text
*      -->P_P_GV_PACKAGE  text
*----------------------------------------------------------------------*
form dtel_set  tables   p_dtel structure gt_dtel
               using    p_gv_package.

  data name     type ddobjname.
  data dd04v_wa type dd04v.
  data:
    lv_rc        like sy-subrc,
    lv_msg1(255),
    lv_msg2(255).

  if p_dtel[] is initial.
    return.
  endif.

  loop at p_dtel where node-sel = 'X'.
*包入请求（对象不存在都可以包）
    data lv_object(40).
    concatenate 'DTEL' p_dtel-name into lv_object.
    perform request_set using p_gv_package 'DICT' lv_object changing gv_request gv_rtype gv_rtmsg. "表、结构请求（debug RS_DD_COPY_OBJ得来）
    if gv_rtype = 'S'.
      call function 'DDIF_DTEL_PUT'
        exporting
          name              = p_dtel-name
          dd04v_wa          = p_dtel-dd04v
        exceptions
          dtel_not_found    = 1
          name_inconsistent = 2
          dtel_inconsistent = 3
          put_failure       = 4
          put_refused       = 5.
      if sy-subrc <> 0.
        perform msg_sys_into changing lv_msg1.
*    else.
*      lv_msg1 = '生成数据元素成功'.
      endif.

*激活
      call function 'DDIF_DTEL_ACTIVATE'
        exporting
          name        = p_dtel-name
          auth_chk    = abap_false
*         PRID        = -1
        importing
          rc          = lv_rc
        exceptions
          not_found   = 1
          put_failure = 2.
      if sy-subrc <> 0 or lv_rc > 4. "rc = 0 成功 = 4 警告 >4 失败
        lv_msg2 = '激活数据元素出错'.
      endif.

*记录日志
      clear gv_msg.
      concatenate lv_msg1 lv_msg2 into gv_msg separated by space.
      if lv_msg1 is initial and lv_msg2 is initial.
        perform rep_log using gv_request 'S' '成功' changing p_dtel-node.
      else.
        perform rep_log using space 'E' gv_msg changing p_dtel-node.
      endif.
    else.
      perform rep_log using space gv_rtype gv_rtmsg changing p_dtel-node.
    endif.
    modify p_dtel.
  endloop.
endform.                    " DTEL_SET

form class_set  tables   p_class structure gt_class
               using    p_gv_package.

  data name     type ddobjname.
  data:
        lo_source type ref to cl_oo_source.
  data:
    ls_clskey type seoclskey,
    lt_source type standard table of string with default key.
  data:
    lv_msg1(255),
    lv_msg2(255),
    lv_msg3(255).
  data lo_ref type ref to cx_root. "异常处理
  data lv_text type string.

  if p_class[] is initial.
    return.
  endif.

  loop at p_class where node-sel = 'X'.
*包入请求（对象不存在都可以包）
    data lv_object(40).
    lv_object = p_class-clsname.
    perform request_set using p_gv_package 'CLAS' lv_object changing gv_request gv_rtype gv_rtmsg. "表、结构请求（debug RS_DD_COPY_OBJ得来）
    if gv_rtype = 'S'.
*创建class
      call function 'SEO_CLASS_CREATE_COMPLETE'
        exporting
          version         = 1 "激活
          devclass        = p_gv_package
          overwrite       = 'X'
        changing
          class           = p_class-class
        exceptions
          existing        = 1
          is_class        = 2
          db_error        = 3
          component_error = 4
          no_access       = 5
          other           = 6
          others          = 7.
      if sy-subrc <> 0.
        message id sy-msgid
          type sy-msgty
          number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_msg1.
      endif.

*写入source
      ls_clskey-clsname = p_class-clsname.
      lt_source = p_class-t_source.
*先试新方法
      try.
          data:
            lo_factory    type ref to object,
            lo_source_new type ref to object,
            lo_settings   type ref to object,
            lr_settings   type ref to data.

          field-symbols <lg_settings> type any.

          call function 'SEO_BUFFER_INIT'.
          call function 'SEO_BUFFER_REFRESH'
            exporting
              cifkey  = ls_clskey
              version = seoc_version_inactive.

          call method ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
            receiving
              result = lo_factory.

          call method lo_factory->('CREATE_SETTINGS')
            exporting
              modification_mode_enabled = abap_true
            receiving
              result                    = lo_settings.

          create data lr_settings type ref to ('IF_OO_CLIF_SOURCE_SETTINGS').
          assign lr_settings->* to <lg_settings>.

          <lg_settings> ?= lo_settings.

          call method lo_factory->('CREATE_CLIF_SOURCE')
            exporting
              clif_name = ls_clskey-clsname
              settings  = <lg_settings>
            receiving
              result    = lo_source_new.

          try.
              call method lo_source_new->('IF_OO_CLIF_SOURCE~LOCK').
            catch cx_oo_access_permission.
          endtry.

          call method lo_source_new->('IF_OO_CLIF_SOURCE~SET_SOURCE')
            exporting
              source = lt_source.
          call method lo_source_new->('IF_OO_CLIF_SOURCE~SAVE').
          call method lo_source_new->('IF_OO_CLIF_SOURCE~UNLOCK').
        catch cx_root into lo_ref.
          lv_text = lo_ref->get_text( ).
*老的方法
          create object lo_source
            exporting
              clskey             = ls_clskey
            exceptions
              class_not_existing = 1
              others             = 2.
          if sy-subrc <> 0.
            message id sy-msgid
              type sy-msgty
              number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_msg2.
          else.

            try.
                lo_source->access_permission( seok_access_modify ).
                lo_source->set_source( lt_source ).
                lo_source->save( ).
                lo_source->access_permission( seok_access_free ).
              catch cx_root into lo_ref.
                lv_msg3 = lo_ref->get_text( ).
            endtry.
          endif.
      endtry.

*激活
      data lt_objects               type standard table of dwinactiv with header line.
      data lv_str like dwinactiv-obj_name.
      lv_str = lv_object && '%'.
      select *
        from dwinactiv "未激活的对象，SE24 debug来
        where obj_name like @lv_str and object in ( 'CLSD','CPRI','CPRO','CPUB','METH' )
        into corresponding fields of table @lt_objects.

      call function 'RS_WORKING_OBJECTS_ACTIVATE'
        tables
          objects                = lt_objects
        exceptions
          excecution_error       = 1
          cancelled              = 2
          insert_into_corr_error = 3.
      if sy-subrc ne 0.
        message id sy-msgid
          type sy-msgty
          number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(lv_rtmsg).
      endif.
      if lv_rtmsg is initial.
        lv_rtmsg = '成功'.
      else.
        lv_rtmsg = '创建成功，激活失败：' && lv_rtmsg.
      endif.
*      data(lv_rtmsg) = '创建成功，激活没做，请手工激活'.

*记录日志
      clear gv_msg.
      concatenate lv_msg1 lv_msg2 lv_msg3 into gv_msg separated by space.
      if lv_msg1 is initial and lv_msg2 is initial and lv_msg3 is initial.
        perform rep_log using gv_request 'S' lv_rtmsg changing p_class-node.
      else.
        perform rep_log using space 'E' gv_msg changing p_class-node.
      endif.
    else.
      perform rep_log using space gv_rtype gv_rtmsg changing p_class-node.
    endif.
    modify p_class.
  endloop.
endform.                    " DTEL_SET

*&---------------------------------------------------------------------*
*&      Form  ttyp_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TTYP        text
*      -->P_GV_PACKAGE  text
*----------------------------------------------------------------------*
form ttyp_set  tables   p_ttyp structure gt_ttyp
               using    p_gv_package.

  data name     type ddobjname.
  data dd04v_wa type dd04v.
  data:
    lv_rc        like sy-subrc,
    lv_msg1(255),
    lv_msg2(255).

  if p_ttyp[] is initial.
    return.
  endif.

  loop at p_ttyp where node-sel = 'X'.
*包入请求（对象不存在都可以包）
    data lv_object(40).
    concatenate 'TTYP' p_ttyp-typename into lv_object.
    perform request_set using p_gv_package 'DICT' lv_object changing gv_request gv_rtype gv_rtmsg.
    if gv_rtype = 'S'.
      call function 'DDIF_TTYP_PUT'
        exporting
          name              = p_ttyp-typename
          dd40v_wa          = p_ttyp-dd40v_wa
        tables
          dd42v_tab         = p_ttyp-dd42v_tab
*         dd43v_tab         = p_ttyp-dd43v_tab "国际营销没有这个参数
        exceptions
          ttyp_not_found    = 1
          name_inconsistent = 2
          ttyp_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          others            = 6.
      if sy-subrc <> 0.
        perform msg_sys_into changing lv_msg1.
*    else.
*      lv_msg1 = '生成数据元素成功'.
      endif.

*激活
      call function 'DDIF_TTYP_ACTIVATE'
        exporting
          name        = p_ttyp-typename
*         auth_chk    = abap_false
*         PRID        = -1
        importing
          rc          = lv_rc
        exceptions
          not_found   = 1
          put_failure = 2.
      if sy-subrc <> 0 or lv_rc > 4. "rc = 0 成功 = 4 警告 >4 失败
        lv_msg2 = '激活表类型出错'.
      endif.

*记录日志
      clear gv_msg.
      concatenate lv_msg1 lv_msg2 into gv_msg separated by space.
      if lv_msg1 is initial and lv_msg2 is initial.
        perform rep_log using gv_request 'S' '成功' changing p_ttyp-node.
      else.
        perform rep_log using space 'E' gv_msg changing p_ttyp-node.
      endif.
    else.
      perform rep_log using space gv_rtype gv_rtmsg changing p_ttyp-node.
    endif.
    modify p_ttyp.
  endloop.
endform.                    " DTEL_SET
*&---------------------------------------------------------------------*
*&      Form  DOMA_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_DOMA  text
*      -->P_PROGRAM  text
*----------------------------------------------------------------------*
form doma_get  tables   p_gt_dtel structure gt_dtel
                                        p_gt_doma structure gt_doma.
  data name     type ddobjname.
  data state    type ddobjstate.
  data langu    type sy-langu.
  data gotstate type ddgotstate.
  data dd01v_wa type dd01v.
  data tpara_wa type tpara.

*从数据元素中获取域
  loop at p_gt_dtel where dd04v-domname(1) = 'Z'.
    p_gt_doma-name = p_gt_dtel-dd04v-domname.
    append p_gt_doma.
  endloop.

  loop at p_gt_doma.
    call function 'DDIF_DOMA_GET'
      exporting
        name          = p_gt_doma-name
        state         = 'A'
        langu         = '1'
      importing
        gotstate      = gotstate
        dd01v_wa      = dd01v_wa
      exceptions
        illegal_input = 1.
    p_gt_doma-dd01v = dd01v_wa.
    modify p_gt_doma.
  endloop.

endform.                    " DOMA_GET
*&---------------------------------------------------------------------*
*&      Form  DOMA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GS_REP_DOMA  text
*      -->P_P_GV_PACKAGE  text
*----------------------------------------------------------------------*
form doma_set  tables   p_doma structure gt_doma
               using    p_gv_package.

  data name     type ddobjname.
  data dd01v_wa type dd01v.
  data:
    lv_rc        like sy-subrc,
    lv_msg1(255),
    lv_msg2(255).

  if p_doma[] is initial.
    return.
  endif.

  loop at p_doma where node-sel = 'X'.
*包入请求（对象不存在都可以包）
    data lv_object(40).
    concatenate 'DOMA' p_doma-name into lv_object.
    perform request_set using p_gv_package 'DICT' lv_object changing gv_request gv_rtype gv_rtmsg. "表、结构请求（debug RS_DD_COPY_OBJ得来）
    if gv_rtype = 'S'.
      call function 'DDIF_DOMA_PUT'
        exporting
          name              = p_doma-name
          dd01v_wa          = p_doma-dd01v
        exceptions
          doma_not_found    = 1
          name_inconsistent = 2
          doma_inconsistent = 3
          put_failure       = 4
          put_refused       = 5.
      if sy-subrc <> 0.
        perform msg_sys_into changing lv_msg1.
*    else.
*      lv_msg1 = '生成域成功'.
      endif.

*激活
      call function 'DDIF_DOMA_ACTIVATE'
        exporting
          name        = p_doma-name
          auth_chk    = abap_false
*         PRID        = -1
        importing
          rc          = lv_rc
        exceptions
          not_found   = 1
          put_failure = 2.
      if sy-subrc <> 0 or lv_rc > 4. "rc = 0 成功 = 4 警告 >4 失败
        lv_msg2 = '激活域出错'.
      endif.

*记录日志
      clear gv_msg.
      concatenate lv_msg1 lv_msg2 into gv_msg separated by space.
      if lv_msg1 is initial and lv_msg2 is initial.
        perform rep_log using gv_request 'S' '成功' changing p_doma-node.
      else.
        perform rep_log using space 'E' gv_msg changing p_doma-node.
      endif.
    else.
      perform rep_log using space gv_rtype gv_rtmsg changing p_doma-node.
    endif.
    modify p_doma.
  endloop.
endform.                    " DOMA_SET
*&---------------------------------------------------------------------*
*&      Form  REP_SCAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_REP  text
*----------------------------------------------------------------------*
form rep_scan  tables   p_gt_rep structure gt_rep using p_program.

*查找主程序include程序
  perform rep_scan_include tables p_gt_rep using p_program '1'.

*主程序
  clear p_gt_rep.
  p_gt_rep-program = p_program.
  select single subc as type
    into corresponding fields of  p_gt_rep
    from trdir
    where name = p_gt_rep-program.
  if sy-subrc = 0.
    append p_gt_rep.
  endif.

*函数池及函数模块和include
  perform rep_scan_function_pool tables p_gt_rep.

*程序类型描述
  data lt_dd07v type table of dd07v with header line.
  call function 'DD_DOMVALUES_GET'
    exporting
      domname   = 'SUBC'
      text      = 'X' "TEXT 参数必填，否则默认只取域值不取文本
    tables
      dd07v_tab = lt_dd07v.
  loop at p_gt_rep.
    read table lt_dd07v with key domvalue_l = p_gt_rep-type.
    if sy-subrc = 0.
      p_gt_rep-typet = lt_dd07v-ddtext.
      modify p_gt_rep.
    endif.
  endloop.
endform.                    " REP_SCAN
*&---------------------------------------------------------------------*
*&      Form  MSG_SYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form msg_sys .
  message id sy-msgid
    type sy-msgty
    number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
endform.                    " MSG_SYS
*&---------------------------------------------------------------------*
*&      Form  LOCK_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CODE  text
*      -->P_GT_LOCK  text
*----------------------------------------------------------------------*
form lock_get  tables p_gt_lock structure gt_lock.

  data name      type ddobjname.
  data state     type ddobjstate.
  data langu     type sy-langu.
  data gotstate  type ddgotstate.
  data dd25v_wa  type dd25v.
  data dd26e_tab type standard table of dd26e.
  data dd27p_tab type standard table of dd27p.
  data ddena_tab type standard table of ddena.

  loop at p_gt_lock.
    call function 'DDIF_ENQU_GET'
      exporting
        name          = p_gt_lock-name
*       STATE         = 'A'
*       LANGU         = ' '
      importing
        gotstate      = gotstate
        dd25v_wa      = p_gt_lock-dd25v
      tables
        dd26e_tab     = p_gt_lock-dd26e
        dd27p_tab     = p_gt_lock-dd27p
        ddena_tab     = p_gt_lock-ddena
      exceptions
        illegal_input = 1.
    modify p_gt_lock.
  endloop.

endform.                    " LOCK_GET
*&---------------------------------------------------------------------*
*&      Form  CODE_ANALYZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CODE  text
*      -->P_GT_LOCK  text
*----------------------------------------------------------------------*
form code_analyze  tables   p_gt_code structure gs_codes
                            p_gt_lock structure gt_lock
                            p_gt_snro structure gt_snro
                            p_gt_func structure gt_func
                            p_gt_table structure gt_table
                            p_gt_ttyp structure gt_ttyp
                            p_gt_dtel structure gt_dtel
                            p_gt_w3mi structure gt_w3mi.

  data: begin of keywords occurs 10,
          key(20),
        end of keywords.
  data:  begin of token occurs 1000.
           include structure stokex.
         data:  end of token.

  data:  begin of statements occurs 1000.
           include structure sstmnt.
         data:  end of statements.
  data: overflow(65535).    "überlaufbereich f. Token
  data lv_len type i.
  data lv_exsit.


*抓CALL
  keywords = 'CALL'. "只能用一个关键词（不能两个）
  append keywords.

  scan abap-source p_gt_code
    keywords from keywords
    tokens into token
    statements into statements
    overflow into overflow
    with analysis.

  loop at statements.
    clear lv_exsit.
    statements-from = statements-from + 1.
    read table token index statements-from.
    case token-str.
      when 'FUNCTION'.
        statements-from = statements-from + 1.
        read table token index statements-from.
        lv_len = strlen( token-str ) - 2. "去掉引号
        if token-str+1(10) = 'ENQUEUE_EZ'. "自建锁对象
          lv_len = lv_len - 8.                              "去掉前8位
          clear p_gt_lock.
          p_gt_lock-name = token-str+9(lv_len).
          object_add lock name p_gt_lock-name.

        elseif token-str+1(lv_len) = 'NUMBER_GET_NEXT'. "编号对象
          do.
            statements-from = statements-from + 1.
            read table token index statements-from.
            if token-str = 'OBJECT'.
              exit.
            endif.
          enddo.
          statements-from = statements-from + 2.
          read table token index statements-from.
          lv_len = strlen( token-str ) - 2. "去掉引号
          clear p_gt_snro.
          p_gt_snro-object = token-str+1(lv_len).
          object_add snro object p_gt_snro-object.

*        elseif token-str+1(lv_len) = 'DOWNLOAD_WEB_OBJECT'. "WEB对象（动态传值，目前没有比较好的方法）
*          do.
*            statements-from = statements-from + 1.
*            read table token index statements-from.
*            if token-str = 'KEY'.
*              exit.
*            endif.
*          enddo.
*          statements-from = statements-from + 2.
*          read table token index statements-from.
*          lv_len = strlen( token-str ) - 2. "去掉引号
*          p_gt_w3mi-objid = token-str+1(lv_len).
*          perform rep_exsit_check using 'W3MI' 'OBJID' p_gt_w3mi-objid changing lv_exsit.
*          if lv_exsit is initial.
*            append p_gt_w3mi.
*          endif.

        elseif token-str+1(1) = 'Z'. "自建函数
*          READ TABLE statements INTO ls_statements
          clear p_gt_func.
          p_gt_func-functionname = token-str+1(lv_len).
          object_add func functionname p_gt_func-functionname.
        endif.
    endcase.
  endloop.

*锁明细数据（为了抓锁中的表结构，这一步提前了）
  perform lock_get tables gt_lock. "同时检索锁对象中的表结构

*锁中的表结构
  loop at p_gt_lock.
    clear p_gt_table.
    p_gt_table-tablename = p_gt_lock-dd25v-roottab.
    perform dict_add tables p_gt_table p_gt_ttyp p_gt_dtel using p_gt_table-tablename.
  endloop.

**抓TABLES语句
*  perform code_scan_tables tables p_gt_code p_gt_table.
*
**抓LIKE和TYPE
*  perform code_scan_likeortype tables p_gt_code p_gt_table p_gt_ttyp p_gt_dtel.
*
**抓sql 增删改查（D010TAB中没有函数的）
*  perform code_scan_sql tables p_gt_code p_gt_table.

endform.                    " CODE_ANALYZE
*&---------------------------------------------------------------------*
*&      Form  LOCK_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_REP_LOCK  text
*      -->P_P_GV_PACKAGE  text
*----------------------------------------------------------------------*
form lock_set  tables   p_lock structure gt_lock
               using    p_package.

  data:
    lv_rc        like sy-subrc,
    lv_msg1(255),
    lv_msg2(255).
*DATA NAME      TYPE DDOBJNAME.
*DATA DD25V_WA  TYPE DD25V.
*DATA DD26E_TAB TYPE STANDARD TABLE OF DD26E.
*DATA DD27P_TAB TYPE STANDARD TABLE OF DD27P.

  if p_lock[] is initial.
    return.
  endif.

  loop at p_lock where node-sel = 'X'.
*包入请求（对象不存在都可以包）
    perform request_set using p_package 'ENQU' p_lock-name changing gv_request gv_rtype gv_rtmsg. "表、结构请求（debug RS_DD_COPY_OBJ得来）
    if gv_rtype = 'S'.
      call function 'DDIF_ENQU_PUT'
        exporting
          name              = p_lock-name
          dd25v_wa          = p_lock-dd25v
        tables
          dd26e_tab         = p_lock-dd26e
          dd27p_tab         = p_lock-dd27p
        exceptions
          enqu_not_found    = 1
          name_inconsistent = 2
          enqu_inconsistent = 3
          put_failure       = 4
          put_refused       = 5.
      if sy-subrc <> 0.
        perform msg_sys_into changing lv_msg1.
      endif.

*激活
*DATA NAME TYPE DDOBJNAME.
*DATA PRID TYPE SY-TABIX.
*DATA RC   TYPE SY-SUBRC.

      call function 'DDIF_ENQU_ACTIVATE'
        exporting
          name        = p_lock-name
*         PRID        = -1
        importing
          rc          = lv_rc
        exceptions
          not_found   = 1
          put_failure = 2.
      if sy-subrc <> 0 or lv_rc > 4. "rc = 0 成功 = 4 警告 >4 失败
        lv_msg2 = '激活出错'.
      endif.

*记录日志
      clear gv_msg.
      concatenate lv_msg1 lv_msg2 into gv_msg separated by space.
      if lv_msg1 is initial and lv_msg2 is initial.
        perform rep_log using gv_request 'S' '成功' changing p_lock-node.
      else.
        perform rep_log using space 'E' gv_msg changing p_lock-node.
      endif.
    else.
      perform rep_log using space gv_rtype gv_rtmsg changing p_lock-node.
    endif.
    modify p_lock.
  endloop.

endform.                    " LOCK_SET
*&---------------------------------------------------------------------*
*&      Form  REP_EXSIT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_6205   text
*      -->P_P_GT_SNRO_OBJECT  text
*      <--P_LV_EXSIT  text
*----------------------------------------------------------------------*
form rep_exsit_check  using p_table
                               p_field
                               p_object
                      changing p_exsit.
  field-symbols <lv_fs> type standard table.

  clear p_exsit.
  loop at gt_rep.
    assign component p_table of structure gt_rep to <lv_fs>.
    read table <lv_fs> transporting no fields with key (p_field) = p_object.
    if sy-subrc = 0.
      p_exsit = 'X'.
      exit.
    endif.
  endloop.
endform.                    " REP_EXSIT_CHECK
*&---------------------------------------------------------------------*
*&      Form  SNRO_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SNRO  text
*----------------------------------------------------------------------*
form snro_get  tables   p_gt_snro structure gt_snro p_gt_dtel structure gt_dtel.

  data ls_interval like inriv . "编号间隔范围

  loop at p_gt_snro.
    call function 'NUMBER_RANGE_OBJECT_READ'
      exporting
*       language          = sy-langu
        object            = p_gt_snro-object
      importing
        interval_exists   = p_gt_snro-interval_exists
        object_attributes = p_gt_snro-object_attributes
        object_text       = p_gt_snro-object_text
      exceptions
        object_not_found  = 1.
    if p_gt_snro-interval_exists = 'X'.
      call function 'NUMBER_RANGE_INTERVAL_LIST'
        exporting
*         NR_RANGE_NR1               = ' '
*         NR_RANGE_NR2               = ' '
          object                     = p_gt_snro-object
*         SUBOBJECT                  = ' '
*         CLEAR_LOCAL_MEMORY         = ' '
        tables
          interval                   = p_gt_snro-interval
        exceptions
          nr_range_nr1_not_found     = 1
          nr_range_nr1_not_intern    = 2
          nr_range_nr2_must_be_space = 3
          nr_range_nr2_not_extern    = 4
          nr_range_nr2_not_found     = 5
          object_not_found           = 6
          subobject_must_be_space    = 7
          subobject_not_found        = 8.
      loop at p_gt_snro-interval into ls_interval. "重置编号范围状态
        ls_interval-nrlevel = ls_interval-fromnumber.
        modify p_gt_snro-interval from ls_interval.
      endloop.
    endif.

    modify p_gt_snro.
  endloop.

*添加编号对象中的自建数据元素
  loop at p_gt_snro.
    if p_gt_snro-object_attributes-domlen(1) = 'Z'.
      perform dtel_add tables p_gt_dtel using p_gt_snro-object_attributes-domlen.
    endif.
  endloop.

endform.                    " SNRO_GET
*&---------------------------------------------------------------------*
*&      Form  SNRO_SET
*&---------------------------------------------------------------------*
*       text
*--------------------------------------------------`--------------------*
*      -->P_P_GT_REP_NRO  text
*      -->P_P_GV_PACKAGE  text
*----------------------------------------------------------------------*
form snro_set  tables   p_snro structure gt_snro
               using  p_package.

*DATA OBJECT              TYPE TNRO-OBJECT.
*DATA CHECK_AT_ALL_EVENTS TYPE C.
*DATA ERROR               TYPE INRER.
  data error_iv            type standard table of inriv.
  data ls_interval            type  inriv.
  data lv_indicator.
  data lt_rep_out like gt_rep_out.

  data:
    lv_rc        type c,
    lv_msg1(255),
    lv_msg2(255),
    lt_errors    like table of inoer with header line.

  if p_snro[] is initial.
    return.
  endif.

  loop at p_snro where node-sel = 'X'.
*包入请求（对象不存在都可以包）
    perform request_set using p_package 'NROB' p_snro-object changing gv_request gv_rtype gv_rtmsg. "表、结构请求（debug RS_DD_COPY_OBJ得来）
    if gv_rtype = 'S'.
*更新编号对象
      perform object_exsit_check using p_snro-object 'SNRO' changing lt_rep_out.
      if lt_rep_out-rtype = 'W'.
        lv_indicator = 'U'.
      else.
        lv_indicator = 'I'.
      endif.
      call function 'NUMBER_RANGE_OBJECT_UPDATE'
        exporting
          indicator                 = lv_indicator
          object_attributes         = p_snro-object_attributes
          object_text               = p_snro-object_text
        importing
          returncode                = lv_rc
        tables
          errors                    = lt_errors
        exceptions
          object_already_exists     = 1
          object_attributes_missing = 2
          object_not_found          = 3
          object_text_missing       = 4
          wrong_indicator           = 5.
      if sy-subrc <> 0 or lv_rc = 'E'. "lv_rc可能返回E
        perform msg_sys_into changing lv_msg1.
      endif.

*更新编号范围
      loop at p_snro-interval into ls_interval.
        select count( * )
          from nriv
          where object = p_snro-object and nrrangenr = ls_interval-nrrangenr.
        if sy-subrc = 0.
          lv_indicator = 'U'.
        else.
          lv_indicator = 'I'.
        endif.
        ls_interval-procind = lv_indicator.
        modify p_snro-interval from ls_interval.
      endloop.
      call function 'NUMBER_RANGE_INTERVAL_UPDATE'
        exporting
          object           = p_snro-object
        tables
          error_iv         = error_iv
          interval         = p_snro-interval
        exceptions
          object_not_found = 1.

      call function 'NUMBER_RANGE_UPDATE_CLOSE' "不调用close修改不生效
        exporting
          object                 = p_snro-object
        exceptions
          no_changes_made        = 1
          object_not_initialized = 2
          others                 = 3.
      if sy-subrc <> 0.
      endif.

*传输编号范围（传输请求可以取到，不过没有回写了）
*      if gs_rfcsi-rfcsaprl > 470. "ecc
*        perform transport_intervalls in program sapmsnum using p_snro-object space ' '.
*      else. "r3
*        perform transport_intervals in program sapmsnum using p_snro-object space ' '.
*      endif.
      perform transport_intervalls in program sapmsnum if found using p_snro-object space ' '. "R3
      perform transport_intervals in program sapmsnum if found using p_snro-object space ' '. "ECC

*修改对象之后确保调用对象的地方都得到更新
      call function 'NUMBER_RANGE_OBJECT_CLOSE'
        exporting
          object                 = p_snro-object
        exceptions
          object_not_initialized = 1.
      if sy-subrc <> 0.
        perform msg_sys_into changing lv_msg2.
      endif.

*记录日志
      clear gv_msg.
      concatenate lv_msg1 lv_msg2 into gv_msg separated by space.
      if lv_msg1 is initial and lv_msg2 is initial.
        perform rep_log using gv_request 'S' '成功（编号范围在定制请求里，请求号见左下角消息）' changing p_snro-node.
      else.
        perform rep_log using space 'E' gv_msg changing p_snro-node.
      endif.
    else.
      perform rep_log using space gv_rtype gv_rtmsg changing p_snro-node.
    endif.
    modify p_snro.
  endloop.

endform.                    " SNRO_SET
*&---------------------------------------------------------------------*
*&      Form  TREE_CHECKED_ITEMS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_ALV_TREE  text
*      -->P_LT_SELECTED_NODE  text
*----------------------------------------------------------------------*
form tree_checked_items_get
*                                              tables p_rep_out structure gt_rep_out
                                              tables pt_selected_node structure gt_selected_node
                                              using    p_alv_tree type ref to cl_gui_alv_tree.

  data lt_selected_node type lvc_t_chit.

  call method p_alv_tree->get_checked_items
    importing
      et_checked_items = lt_selected_node.
  pt_selected_node[] = lt_selected_node.

  if pt_selected_node[] is initial.
    message '至少选中一行' type 'E'.
  endif.

*  loop at lt_selected_node into ls_selected_node.
** this method gets the line correspondent to a node code
*    call method p_alv_tree->get_outtab_line
*      exporting
*        i_key    = ls_selected_node-nodekey
*      importing
*        e_outtab_line = p_rep_out.
*    append p_rep_out.
*  endloop.

endform.                    " TREE_CHECKED_ITEMS_GET
*&---------------------------------------------------------------------*
*&      Form  REP_SEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_REP  text
*      -->P_GT_SELECTED_NODE  text
*----------------------------------------------------------------------*
form rep_sel  tables   p_gt_rep structure gt_rep
                       p_gt_node structure gt_selected_node.

  data:
    ls_sta    type ty_sta,
    ls_screen like gt_screen,
    ls_dict   like gt_table,
    ls_dtel   like gt_dtel,
    ls_doma   like gt_doma,
    ls_lock   like gt_lock,
    ls_snro   like gt_snro,
    ls_node   type ty_node,
    ls_tcode  type ty_tcode.

  sort p_gt_node by nodekey.

  loop at p_gt_rep.
*文件夹
    if p_gt_rep is not initial.
      read table p_gt_node with key nodekey = p_gt_rep-node-key binary search.
      if sy-subrc = 0.
        p_gt_rep-node-sel = 'X'.
      else.
        p_gt_rep-node-sel = space.
      endif.
    endif.

*源代码
    if p_gt_rep-code is not initial.
      read table p_gt_node with key nodekey = p_gt_rep-code-node-key binary search.
      if sy-subrc = 0.
        p_gt_rep-code-node-sel = 'X'.
      else.
        p_gt_rep-code-node-sel = space.
      endif.
    endif.

*文本池
    if p_gt_rep-text is not initial.
      read table p_gt_node with key nodekey = p_gt_rep-text-node-key binary search.
      if sy-subrc = 0.
        p_gt_rep-text-node-sel = 'X'.
      else.
        p_gt_rep-text-node-sel = space.
      endif.
    endif.

*状态栏
    loop at p_gt_rep-cua-sta into ls_sta.
      if ls_sta is not initial.
        read table p_gt_node with key nodekey =  ls_sta-node-key binary search.
        if sy-subrc = 0.
          ls_sta-node-sel = 'X'.
        else.
          ls_sta-node-sel = ''.
        endif.
        modify p_gt_rep-cua-sta from ls_sta.
      endif.
    endloop.

*屏幕
    loop at p_gt_rep-screen2  into ls_screen.
      if ls_screen is not initial.
        read table p_gt_node with key nodekey =  ls_screen-node-key binary search.
        if sy-subrc = 0.
          ls_screen-node-sel = 'X'.
        else.
          ls_screen-node-sel = ''.
        endif.
        modify p_gt_rep-screen2  from ls_screen.
      endif.
    endloop.

*表类型
    tab_rep_sel_t ttyp.

*表结构
    loop at p_gt_rep-dict into ls_dict.
      if ls_dict is not initial.
        read table p_gt_node with key nodekey =  ls_dict-node-key binary search.
        if sy-subrc = 0.
          ls_dict-node-sel = 'X'.
        else.
          ls_dict-node-sel = ''.
        endif.
        modify p_gt_rep-dict from ls_dict.
      endif.
    endloop.

*数据元素
    loop at p_gt_rep-dtel into ls_dtel.
      if ls_dtel is not initial.
        read table p_gt_node with key nodekey =  ls_dtel-node-key binary search.
        if sy-subrc = 0.
          ls_dtel-node-sel = 'X'.
        else.
          ls_dtel-node-sel = ''.
        endif.
        modify p_gt_rep-dtel from ls_dtel.
      endif.
    endloop.

*域
    loop at p_gt_rep-doma into ls_doma.
      if ls_doma is not initial.
        read table p_gt_node with key nodekey =  ls_doma-node-key binary search.
        if sy-subrc = 0.
          ls_doma-node-sel = 'X'.
        else.
          ls_doma-node-sel = ''.
        endif.
        modify p_gt_rep-doma from ls_doma.
      endif.
    endloop.

*锁对象
    loop at p_gt_rep-lock into ls_lock.
      if ls_lock is not initial.
        read table p_gt_node with key nodekey =  ls_lock-node-key binary search.
        if sy-subrc = 0.
          ls_lock-node-sel = 'X'.
        else.
          ls_lock-node-sel = ''.
        endif.
        modify p_gt_rep-lock from ls_lock.
      endif.
    endloop.

*编号对象
    loop at p_gt_rep-snro into ls_snro.
      if ls_snro is not initial.
        read table p_gt_node with key nodekey =  ls_snro-node-key binary search.
        if sy-subrc = 0.
          ls_snro-node-sel = 'X'.
        else.
          ls_snro-node-sel = ''.
        endif.
        modify p_gt_rep-snro from ls_snro.
      endif.
    endloop.

*tcode
    loop at p_gt_rep-tcode into ls_tcode.
      if ls_tcode is not initial.
        read table p_gt_node with key nodekey =  ls_tcode-node-key binary search.
        if sy-subrc = 0.
          ls_tcode-node-sel = 'X'.
        else.
          ls_tcode-node-sel = ''.
        endif.
        modify p_gt_rep-tcode from ls_tcode.
      endif.
    endloop.

*函数组
    tab_rep_sel_s fugr.
*函数模块
    tab_rep_sel_t func.
*类
    tab_rep_sel_t class..

    modify p_gt_rep.
  endloop.

endform.                    " REP_SEL
*&---------------------------------------------------------------------*
*&      Form  TCODE_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_TCODE  text
*      -->P_LS_REP_PROGRAM  text
*----------------------------------------------------------------------*
form tcode_get  tables   p_gt_tcode structure gt_tcode
                using    p_program.

  data lt_tstc like table of tstc with header line.

*RPY_TRANSACTION_READ 也可以

  select *
    into corresponding fields of table lt_tstc
    from tstc
    where pgmna = p_program.

  delete lt_tstc where tcode(1) ne 'Z' and tcode(1) ne 'Y'.

  loop at lt_tstc.
    gt_tcode-tstc = lt_tstc.
    select single *
      into gt_tcode-tstcc
      from tstcc
      where tcode = lt_tstc-tcode.
    select single *
      into gt_tcode-tstcp
      from tstcp
      where tcode = lt_tstc-tcode.
    select *
      into table gt_tcode-tstct
      from tstct
      where tcode = lt_tstc-tcode.
    select *
      into table gt_tcode-tstca
      from tstca
      where tcode = lt_tstc-tcode.
    select *
      into table gt_tcode-usott
      from usott
      where name = lt_tstc-tcode and type = 'TR'.
    append gt_tcode.
  endloop.

endform.                    " TCODE_GET
*&---------------------------------------------------------------------*
*&      Form  TCODE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_REP_TCODE  text
*      -->P_P_GV_PACKAGE  text
*----------------------------------------------------------------------*
form tcode_set  tables p_gt_tcode structure gt_tcode
                using    p_package.

  data:
                  lv_type         type rglif-docutype.
  data:
    ls_tstct type tstct,
    ls_tstca type tstca.

  loop at p_gt_tcode where node-sel = 'X'.
*修改属性值（不清楚一共改哪些东西，后面测试看看）
    if p_tcode = 'X'.
      clear:gs_popk,gt_pop,gt_pop[],sy-ucomm.
      concatenate '事务码' p_gt_tcode-tstc-tcode into gs_popk-title.
      pop_key_append 'TCODE' '事务码' p_gt_tcode-tstc-tcode.
      call screen 2003 starting at 30 10 ending at 100 20.
      if sy-ucomm = 'OK'.
        loop at gt_pop.
          case gt_pop-key.
            when 'TCODE'.
              p_gt_tcode-tstc-tcode = gt_pop-value.
              if  p_gt_tcode-tstcp-tcode is not initial.
                p_gt_tcode-tstcp-tcode = gt_pop-value.
              endif.
              if  p_gt_tcode-tstcc-tcode is not initial.
                p_gt_tcode-tstcc-tcode = gt_pop-value.
              endif.
              loop at p_gt_tcode-tstct into ls_tstct.
                ls_tstct-tcode = gt_pop-value.
                modify p_gt_tcode-tstct from ls_tstct.
              endloop.
              loop at p_gt_tcode-tstca into ls_tstca.
                ls_tstca-tcode = gt_pop-value.
                modify p_gt_tcode-tstca from ls_tstca.
              endloop.
          endcase.
        endloop.
      endif.
    endif.

*包入请求（对象不存在都可以包）
    perform request_set using p_package 'TRAN' p_gt_tcode-tstc-tcode changing gv_request gv_rtype gv_rtmsg. "表、结构请求（debug RS_DD_COPY_OBJ得来）
    if gv_rtype = 'S'.

*debug se93 复制得来
*删除
      delete from tstct where tcode = p_gt_tcode-tstc-tcode.
      delete from tstcp where tcode = p_gt_tcode-tstc-tcode.
      delete from tstca where tcode = p_gt_tcode-tstc-tcode.
      delete from tstc  where tcode = p_gt_tcode-tstc-tcode.
      delete from tstcc where tcode = p_gt_tcode-tstc-tcode.
      delete from usott where name  = p_gt_tcode-tstc-tcode and type = 'TR'.
*    perform transaction_authorities_delete(lseukf01) using p_gt_tcode-tstc-tcode.

*插入
      insert tstc from p_gt_tcode-tstc.
      insert tstcp from p_gt_tcode-tstcp.
      insert tstcc from p_gt_tcode-tstcc.
      insert tstct from table p_gt_tcode-tstct.
      insert tstca from table p_gt_tcode-tstca.
      insert usott from table p_gt_tcode-usott.

*添加对象列表
      call function 'RS_TREE_OBJECT_PLACEMENT'
        exporting
          object    = p_gt_tcode-tstc-tcode
          operation = 'INSERT'
          type      = 'OT'
          program   = p_gt_tcode-tstc-pgmna.
*
*    perform transaction_authorities_copy(lseukf01) using p_gt_tcode-tstc-tcode.
*记录日志
      clear gv_msg.
      perform rep_log using gv_request 'S' '成功' changing p_gt_tcode-node.
    else.
      perform rep_log using gv_request gv_rtype gv_rtmsg changing p_gt_tcode-node.
    endif.
    modify p_gt_tcode.
  endloop.

endform.                    " TCODE_SET
*&---------------------------------------------------------------------*
*&      Form  FUNC_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FUNC  text
*----------------------------------------------------------------------*
form func_get  tables   p_gt_func structure gt_func
                                    p_gt_table structure gt_table
                                    p_gt_ttyp structure gt_ttyp
                                    p_gt_dtel structure gt_dtel.

  data new_source              type rsfb_source.
  data:
    ls_import_parameter   like rsimp,
    ls_changing_parameter like rscha,
    ls_export_parameter   like rsexp,
    ls_tables_parameter   like rstbl,
    ls_table              type ty_table.

  data:begin of lt_dbfield occurs 0,
         dbfield type likefield,
       end of lt_dbfield.
  data lv_exsit.

  loop at p_gt_func where short_text is initial. "只抓没取过的
    call function 'RPY_FUNCTIONMODULE_READ_NEW'
      exporting
        functionname       = p_gt_func-functionname
      importing
        global_flag        = p_gt_func-global_flag
        remote_call        = p_gt_func-remote_call
        update_task        = p_gt_func-update_task
        short_text         = p_gt_func-short_text
        function_pool      = p_gt_func-function_pool "有bug
*       remote_basxml_supported = p_gt_func-remote_basxml_supported
      tables
        import_parameter   = p_gt_func-import_parameter
        changing_parameter = p_gt_func-changing_parameter
        export_parameter   = p_gt_func-export_parameter
        tables_parameter   = p_gt_func-tables_parameter
        exception_list     = p_gt_func-exception_list
        documentation      = p_gt_func-documentation
        source             = p_gt_func-source "source长度只有72，超过72时source是空的，在new_source中
      changing
        new_source         = p_gt_func-new_source
      exceptions
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3.
    if sy-subrc ne 0. "取不到（可能是destination）
      delete p_gt_func.
      continue.
    endif.

    select single pname
      into p_gt_func-function_pool "fm取出来的函数组有Bug，会少几个字符
      from tfdir
      where funcname = p_gt_func-functionname.
    shift p_gt_func-function_pool left by 4 places.
    modify p_gt_func.

*参数中的数据字典
    loop at p_gt_func-import_parameter into ls_import_parameter.
      lt_dbfield-dbfield = ls_import_parameter-typ.
      append lt_dbfield.
    endloop.
    loop at p_gt_func-changing_parameter into ls_changing_parameter.
      lt_dbfield-dbfield = ls_changing_parameter-typ.
      append lt_dbfield.
    endloop.
    loop at p_gt_func-export_parameter into ls_export_parameter.
      lt_dbfield-dbfield = ls_export_parameter-typ.
      append lt_dbfield.
    endloop.
    loop at p_gt_func-tables_parameter into ls_tables_parameter.
      lt_dbfield-dbfield = ls_tables_parameter-dbstruct.
      append lt_dbfield.
    endloop.
    delete lt_dbfield where dbfield(1) ne 'Z'.
    sort lt_dbfield by dbfield.
    delete adjacent duplicates from lt_dbfield.

    loop at lt_dbfield.
      perform dict_add tables p_gt_table p_gt_ttyp p_gt_dtel using lt_dbfield-dbfield.
    endloop.

    loop at p_gt_table.
      perform table_definition_get changing p_gt_table. "取表字段（参考的数据元素）
      modify p_gt_table.
    endloop.
  endloop.
endform.                    " FUNC_GET
*&---------------------------------------------------------------------*
*&      Form  TREE_NODE_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5403   text
*      -->P_P_GT_REP_FUNC_FUNCTIONNAME  text
*      -->P_5405   text
*      -->P_5406   text
*      <--P_P_GT_REP_FUNC  text
*----------------------------------------------------------------------*
form tree_node_add  using
                             p_value"对象值
                             p_text "对象类型文本
                             p_mtype "标识（判断是否存在）
                             p_top_key
                    changing p_object. "结构

  data:
    ls_rep_out   like line of gt_rep_out,
    ls_node      type ty_node,
    lv_new_key   type lvc_nkey,
    lv_node_text type lvc_value.
  field-symbols:
    <ls_struc>,
    <lv_var>.

*准备数据
  lv_node_text = p_value.
  ls_rep_out-otype = p_text.
  assign component 'NODE' of structure p_object to <ls_struc>.
  ls_node = <ls_struc>.
  move-corresponding ls_node to ls_rep_out.

*检查对象是否存在
  if gv_state = 0. "只上传未执行 "还没执行过
    perform object_exsit_check using p_value p_mtype changing ls_rep_out.
  endif.

*添加树节点
  perform tree_add_a_node using  ls_rep_out p_top_key lv_node_text
                            space space
                     changing lv_new_key.

*回写node_key
  assign component 'KEY' of structure <ls_struc> to <lv_var>.
  <lv_var> = lv_new_key.

endform.                    " TREE_NODE_ADD
*&---------------------------------------------------------------------*
*&      Form  FUNC_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_REP_FUNC  text
*      -->P_P_GV_PACKAGE  text
*----------------------------------------------------------------------*
form func_set  tables   p_gt_func structure gt_func
               using    p_package.

  data:
    function_include like  rs38l-include,
    corrnum_e        like  e071-trkorr,
    lv_line          type i,
    lv_include       type rs38l-include.
  data ls_new_source like line of p_gt_func-new_source.
  data lo_ref type ref to cx_root. "异常处理
  data lv_text type string.

  clear gv_msg.

  loop at p_gt_func where node-sel = 'X'.

*修改属性值
    if p_func = 'X'.
      clear:gs_popk,gt_pop,gt_pop[],sy-ucomm.
      concatenate '函数' p_gt_func-functionname into gs_popk-title.
      pop_key_append 'FUNCTION_POOL' '函数组' p_gt_func-function_pool.
      pop_key_append 'FUNCTIONNAME' '函数名' p_gt_func-functionname.
      call screen 2003 starting at 30 10 ending at 100 20.
      if sy-ucomm = 'OK'.
        loop at gt_pop.
          case gt_pop-key.
            when 'FUNCTIONNAME'.
              p_gt_func-functionname = gt_pop-value.
            when 'FUNCTION_POOL'.
              p_gt_func-function_pool = gt_pop-value.
          endcase.
        endloop.
      endif.
    endif.

*包入请求（对象不存在都可以包）
    perform request_set using p_package 'FUNC' p_gt_func-functionname changing gv_request gv_rtype gv_rtmsg.
    if gv_rtype = 'S'.
      if p_gt_func-source[] is not initial.
*删掉头尾两行，不知道为啥insert函数不自动去掉
        describe table p_gt_func-source lines lv_line.
        delete p_gt_func-source index lv_line.
        delete p_gt_func-source index 1.
*去掉参数部分的注释
        delete p_gt_func-source where line(2) = '*"'.
      else.
*删掉头尾两行，不知道为啥insert函数不自动去掉
        describe table p_gt_func-new_source lines lv_line.
        delete p_gt_func-new_source index lv_line.
        delete p_gt_func-new_source index 1.
*去掉参数部分的注释
        loop at p_gt_func-new_source  into ls_new_source.
          try. "防止字符为空 string长度为0时，偏移dump
              if ls_new_source(2) = '*"'.
                delete p_gt_func-new_source .
              endif.
            catch cx_root into lo_ref.
              lv_text = lo_ref->get_text( ).
          endtry.
        endloop.
      endif.

*判断函数是否存在
      call function 'FUNCTION_EXISTS'
        exporting
          funcname           = p_gt_func-functionname
        importing
          include            = lv_include
        exceptions
          function_not_exist = 1.
      if sy-subrc = 0.
        call function 'FUNCTION_DELETE'
          exporting
            funcname      = p_gt_func-functionname
*           suppress_success_message = abap_true
          exceptions
            error_message = 1
            others        = 2.
        if sy-subrc <> 0.
          perform msg_sys_into changing gv_msg.
        endif.
      endif.

      if gv_msg is initial.
*插入函数
        call function 'RS_FUNCTIONMODULE_INSERT'
          exporting
            funcname                = p_gt_func-functionname
            function_pool           = p_gt_func-function_pool
            interface_global        = p_gt_func-global_flag
            remote_call             = p_gt_func-remote_call
            short_text              = p_gt_func-short_text
*           SUPPRESS_CORR_CHECK     = 'X'
            update_task             = p_gt_func-update_task
*           CORRNUM                 = ' '
            namespace               = ' '
*           SUPPRESS_LANGUAGE_CHECK = 'X'
*           AUTHORITY_CHECK         = 'X'
            save_active             = 'X'
            new_source              = p_gt_func-new_source
            exception_class         = ' '
*           SUPPRESS_UPGRADE_CHECK  = ' '
*           remote_basxml_supported = p_gt_func-remote_basxml_supported
          importing
            function_include        = function_include
            corrnum_e               = corrnum_e
          tables
            import_parameter        = p_gt_func-import_parameter
            export_parameter        = p_gt_func-export_parameter
            tables_parameter        = p_gt_func-tables_parameter
            changing_parameter      = p_gt_func-changing_parameter
            exception_list          = p_gt_func-exception_list
            parameter_docu          = p_gt_func-documentation
            source                  = p_gt_func-source
          exceptions
            double_task             = 1
            error_message           = 2
            function_already_exists = 3
            invalid_function_pool   = 4
            invalid_name            = 5
            too_many_functions      = 6
            no_modify_permission    = 7
            no_show_permission      = 8
            enqueue_system_failure  = 9
            canceled_in_corr        = 10.
*记录日志
        clear gv_msg.
        if sy-subrc = 0.
          perform rep_log using gv_request 'S' '成功' changing p_gt_func-node.
        else. "插入函数失败
          perform msg_sys_into changing gv_msg.
          perform rep_log using space 'E' gv_msg changing p_gt_func-node.
        endif.
      else. "删除函数失败
        perform rep_log using space 'E' gv_msg changing p_gt_func-node.
      endif.
    else. "未包请求
      perform rep_log using gv_request gv_rtype gv_rtmsg changing p_gt_func-node.
    endif.
    modify p_gt_func.
  endloop.
endform.                    " FUNC_SET
*&---------------------------------------------------------------------*
*&      Form  rep_scan_function_pool
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_REP  text
*----------------------------------------------------------------------*
form rep_scan_function_pool  tables   p_gt_rep structure gt_rep.
  data:
    ls_code  like gs_code,
    lt_lock  type table of ty_lock with header line,
    lt_snro  type table of ty_snro with header line,
    lt_tcode type table of ty_tcode with header line,
    lt_func  type table of ty_func with header line, "程序中的函数
    lt_func2 type table of ty_func with header line, "函数中的函数
    lt_table type table of ty_table with header line,
    lt_ttyp  type table of ty_ttyp with header line,
    lt_dtel  type table of ty_dtel with header line,
    lt_w3mi  type table of ty_w3mi with header line,
    lv_exsit,
    lv_pname type tfdir-pname,
    lv_tabix like sy-tabix,
    ls_func  like lt_func,
    lt_code  type table of ty_codes.

  loop at p_gt_rep.
    clear:ls_code,lt_lock,lt_lock[],lt_snro,lt_snro[],lt_func,lt_func[],lt_table,lt_table[],lt_dtel,lt_dtel[].
    if p_gt_rep-type = 'F'. "函数池
      perform rep_scan_include tables p_gt_rep using p_gt_rep-program 'F'.
    else. "可执行程序和包含程序（函数模块在这一步递归到底）
*取代码
      perform code_get using p_gt_rep-program changing ls_code.
*分析代码
      perform code_analyze tables ls_code-code
                                               lt_lock
                                               lt_snro
                                               lt_func
                                               lt_table
                                               lt_ttyp
                                               lt_dtel
                                               lt_w3mi.

      loop at lt_func.
*递归函数中调用的函数
        clear:lt_func2,lt_func2[],ls_func.
        call function 'RPY_FUNCTIONMODULE_READ_NEW'
          exporting
            functionname       = lt_func-functionname
          tables
            import_parameter   = ls_func-import_parameter
            changing_parameter = ls_func-changing_parameter
            export_parameter   = ls_func-export_parameter
            tables_parameter   = ls_func-tables_parameter
            exception_list     = ls_func-exception_list
            documentation      = ls_func-documentation
            source             = ls_func-source
          changing
            new_source         = ls_func-new_source
          exceptions
            error_message      = 1
            function_not_found = 2
            invalid_name       = 3.
        if ls_func-source is not initial.
          lt_code = ls_func-source.
        else.
          lt_code = ls_func-new_source.
        endif.

*        perform code_analyze tables ls_func-source
        perform code_analyze tables lt_code
                                                 lt_lock
                                                 lt_snro
                                                 lt_func2
                                                 lt_table
                                                 lt_ttyp
                                                 lt_dtel
                                                 lt_w3mi.
        loop at lt_func2. "防止重复操作和死循环
          read table lt_func transporting no fields with key functionname = lt_func2-functionname.
          if sy-subrc ne 0.
            append lt_func2 to lt_func.
          endif.
        endloop.

*追加函数组和函数模块
        clear p_gt_rep.
        select single pname
          into lv_pname
          from tfdir
          where funcname = lt_func-functionname.
        if sy-subrc = 0.
          read table p_gt_rep with key program = lv_pname.
          if sy-subrc = 0. "已有函数池文件夹
            lv_tabix = sy-tabix.
            append lt_func to p_gt_rep-func.
            modify p_gt_rep index lv_tabix.
          else.
            p_gt_rep-program = lv_pname.
            p_gt_rep-type = 'F'. "函数池
            shift lv_pname left by 4 places.
            select single area areat
              into corresponding fields of p_gt_rep-fugr
              from tlibt
              where area = lv_pname and spras = 1.
            append lt_func to p_gt_rep-func.
            append p_gt_rep.
          endif.
        endif.
      endloop.
    endif.
  endloop.


endform.                    " rep_scan_function_pool

*&---------------------------------------------------------------------*
*&      Form  rep_scan_include
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_REP   text
*      -->P_PROGRAM  text
*----------------------------------------------------------------------*
form rep_scan_include  tables p_gt_rep structure gt_rep using value(p_program) value(p_type).
  data: begin of lt_includes occurs 50,
          name like progdir-name,
        end of lt_includes.
  data ls_reposrc type reposrc.
  data functab       type standard table of rs38l_incl with header line.
  data function_pool  like  tlibg-area.

  clear p_gt_rep. "p_program是工作区p_gt_rep的字段

*取所有include（函数池不包含XX）
  call function 'RS_GET_ALL_INCLUDES'
    exporting
      program      = p_program
    tables
      includetab   = lt_includes
    exceptions
      not_existent = 1
      no_program   = 2.
*  if sy-subrc ne 0 and sy-subrc ne 2. "1可能是include
*    perform msg_sys.
*    leave program.
*  endif.
  if sy-subrc = 1..
    message '请检查程序名是否正确' type 'S' display like 'E'.
    leave list-processing and return to screen 0.
  endif.

  if p_type = 'F'.
*函数池下函数对应的include名
    function_pool = p_program.
    shift function_pool left by 4 places. "去掉SAPL
    call function 'RS_FUNCTION_POOL_CONTENTS'
      exporting
        function_pool           = function_pool
      tables
        functab                 = functab
      exceptions
        function_pool_not_found = 1.

*排除函数模块生成的include
    loop at functab.
      delete lt_includes where name = functab-include.
    endloop.
  endif.

*作为文件夹添加到rep
  loop at lt_includes where name(1) = 'Z'. "只抓Z开头的include
    read table p_gt_rep transporting no fields with key program = lt_includes-name.
    if sy-subrc ne 0.
      p_gt_rep-program = lt_includes-name.
      p_gt_rep-type = 'I'.
      append p_gt_rep.
    endif.
  endloop.
endform.                    "rep_scan_include
*&---------------------------------------------------------------------*
*&      Form  FUGR_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GV_PACKAGE  text
*      <--P_P_GT_REP_FUGR  text
*----------------------------------------------------------------------*
form fugr_set  using    p_gv_package p_program
               changing p_fugr structure gs_fugr.

  data:
    corrnum    like  e071-trkorr,
    short_text like  tftit-stext.

  if p_fugr is initial or p_fugr-node-sel is initial.
    return.
  endif.

  perform request_set using p_gv_package 'FUGR' p_fugr-area changing gv_request gv_rtype gv_rtmsg.
  if gv_rtype = 'S'.
    short_text = p_fugr-areat.
    call function 'RS_FUNCTION_POOL_INSERT'
      exporting
        function_pool           = p_fugr-area
        short_text              = short_text
*       namespace               = lv_namespace
        devclass                = p_gv_package
      importing
        corrnum                 = corrnum
      exceptions
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        others                  = 12.
    if sy-subrc = 0.
      perform rep_log using gv_request 'S' '成功' changing p_fugr-node.
    elseif sy-subrc = 1.
      perform rep_log using gv_request 'S' '没有修改' changing p_fugr-node.
    else.
      perform msg_sys_into changing gv_msg.
      perform rep_log using space 'E' gv_msg p_fugr-node.
    endif.
  else.
    perform rep_log using space gv_rtype gv_rtmsg p_fugr-node.
  endif.

endform.                    " FUGR_SET
*&---------------------------------------------------------------------*
*&      Form  CODE_SCAN_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_TABLE  text
*      -->P_P_GT_DTEL  text
*      -->P_P_GT_CODE  text
*----------------------------------------------------------------------*
form code_scan_tables  tables
                                p_gt_code structure gs_codes
                                p_gt_table structure gt_table.

  data: lt_tokens type standard table of stokes with header line.
  data: lt_statements type standard table of sstmnt with header line.
  data: lt_keywords type standard table of text20 with header line.
  data: ls_table type ty_table.
  data: ls_tablecomparison type ty_table.
  data lv_exsit.
  data lt_ttyp type table of ty_ttyp with header line.
  data lt_dtel type table of ty_dtel with header line.

  append 'TABLES' to lt_keywords.

  scan abap-source p_gt_code
  tokens into lt_tokens
  statements into lt_statements
  keywords from lt_keywords.

  sort lt_tokens ascending by str.
  delete lt_tokens where str = 'TABLES'.

  loop at lt_tokens.
    try.
        if ( lt_tokens-str+0(1) <> 'Y' and lt_tokens-str+0(1) <> 'Z' ).
          continue.
        endif.
      catch cx_sy_range_out_of_bounds into cx_root.
    endtry.
    ls_table-tablename = lt_tokens-str.
    perform dict_add tables p_gt_table lt_ttyp lt_dtel using ls_table-tablename. "lt_ttyp和lt_dtel只是为了凑数
  endloop.

endform.                    " CODE_SCAN_TABLES
*&---------------------------------------------------------------------*
*&      Form  CODE_SCAN_LIKEORTYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_CODE  text
*      -->P_P_GT_TABLE  text
*      -->P_P_GT_DTEL  text
*----------------------------------------------------------------------*
form code_scan_likeortype  tables   p_gt_code structure gs_codes
                                    p_gt_table structure gt_table
                                    p_gt_ttyp structure gt_ttyp
                                    p_gt_dtel structure gt_dtel.


  data: lv_head type string.
  data: lv_tail type string.
  data: lv_no_use type string.
  data: lv_line type string.
  data: lv_linetype type string.
  data: lv_len type i value 0.
  data: endofline type i value 1.
  data: ls_table type ty_table.
  data: ls_tablecomparison type ty_table.
  data lv_exsit.

  loop at p_gt_code.
    lv_len = strlen( p_gt_code ).
    if lv_len > 0.
      if p_gt_code(1) = '*'.
        continue.
      endif.

      translate p_gt_code to upper case.

      shift p_gt_code up to 'LIKE'.
      if sy-subrc = 0.
        lv_linetype = 'LIKE'.
      else.
        shift p_gt_code up to 'TYPE'.
        if sy-subrc = 0.
          find 'BEGIN OF' in p_gt_code.
          if sy-subrc <> 0.
            find 'END OF' in p_gt_code.
            if sy-subrc <> 0.
              find 'VALUE' in p_gt_code.
              if sy-subrc <> 0.
                lv_linetype = 'TYPE'.
              endif.
            endif.
          endif.
        else.
          shift p_gt_code up to 'INCLUDE'.
          if sy-subrc = 0.
            split p_gt_code at space into lv_no_use lv_line.
          endif.

          shift p_gt_code up to 'STRUCTURE'.
          if sy-subrc = 0.
            lv_linetype = 'STRUCTURE'.
          else.
            continue.
          endif.
        endif.
      endif.

      case lv_linetype.
        when 'LIKE' or 'TYPE' or 'STRUCTURE'.
          shift p_gt_code up to space.
          shift p_gt_code left deleting leading space.
          if p_gt_code cs 'TABLE'.
            split p_gt_code at 'TABLE' into lv_head lv_tail.
            split lv_tail at 'OF' into lv_head lv_tail.
            split lv_tail at 'WITH' into lv_tail lv_head. "还有with要处理掉
            split lv_tail at 'OCCURS' into lv_tail lv_head. "OCCURS
            p_gt_code = lv_tail.
            shift p_gt_code left deleting leading space.
          endif.

          try.
              if p_gt_code+0(1) = 'Y' or p_gt_code+0(1) = 'Z' .
              else.
                lv_linetype = ''.
                continue.
              endif.
            catch cx_sy_range_out_of_bounds into cx_root.
          endtry.

          if p_gt_code cs ','.
            split p_gt_code at ',' into lv_head lv_tail.
            if p_gt_code cs '-'.
              split lv_head at '-' into lv_head lv_tail.
            endif.
            if p_gt_code cs 'OCCURS'.
              split p_gt_code at space into lv_head lv_tail.
            endif.
          else.
            if p_gt_code cs '.'.
              split p_gt_code at '.' into lv_head lv_tail.
              if p_gt_code cs '-'.
                split lv_head at '-' into lv_head lv_tail.
              endif.
              if p_gt_code cs 'OCCURS'.
                split p_gt_code at space into lv_head lv_tail.
              endif.
            else.
              split p_gt_code at space into lv_head lv_tail.
              if p_gt_code cs '-'.
                split lv_head at '-' into lv_head lv_tail.
              endif.
            endif.
          endif.

          if not lv_head is initial.
            clear ls_table.
            ls_table-tablename = lv_head.
            perform dict_add tables p_gt_table p_gt_ttyp p_gt_dtel using ls_table-tablename.
          endif.
          lv_linetype = ''.
      endcase.
    endif.
  endloop.

endform.                    " CODE_SCAN_LIKEORTYPE
*&---------------------------------------------------------------------*
*&      Form  DICT_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_TABLE  text
*      -->P_P_GT_DTEL  text
*      -->P_LT_DBFIELD_DBFIELD  text
*----------------------------------------------------------------------*
form dict_add  tables
                        p_gt_table structure gt_table
                        p_gt_ttyp structure gt_ttyp
                        p_gt_dtel structure gt_dtel
               using    p_object.
  data:
    ls_table   type ty_table,
    lv_exsit,
    lv_type(4).

*判断对象类型
  perform dict_check using p_object changing lv_type.

  case lv_type.
    when 'TAB'.
      ls_table-tablename = p_object.
      read table p_gt_table transporting no fields with key tablename = ls_table-tablename.
      if sy-subrc ne 0.
        perform rep_exsit_check using 'DICT' 'TABLENAME' ls_table-tablename changing lv_exsit.
        if lv_exsit ne 'X'.
          append ls_table to p_gt_table.
        endif.
      endif.
    when 'TTYP'.
      object_add ttyp typename p_object.
    when 'DTEL'.
*数据元素
*      perform dtel_add tables p_gt_dtel using p_object.
      object_add dtel name p_object.
  endcase.

endform.                    " DICT_ADD
form dict_class_add  tables
                        p_gt_table structure gt_table
                        p_gt_ttyp structure gt_ttyp
                        p_gt_dtel structure gt_dtel
                        p_gt_class structure gt_class
               using    p_object.
  data:
    ls_table   type ty_table,
    lv_exsit,
    lv_type(4).

*判断对象类型
  perform dict_check using p_object changing lv_type.

  case lv_type.
    when 'TAB'.
      ls_table-tablename = p_object.
      read table p_gt_table transporting no fields with key tablename = ls_table-tablename.
      if sy-subrc ne 0.
        perform rep_exsit_check using 'DICT' 'TABLENAME' ls_table-tablename changing lv_exsit.
        if lv_exsit ne 'X'.
          append ls_table to p_gt_table.
        endif.
      endif.
    when 'TTYP'.
      object_add ttyp typename p_object.
    when 'DTEL'.
*数据元素
*      perform dtel_add tables p_gt_dtel using p_object.
      object_add dtel name p_object.
    when 'CLAS'.
      object_add class clsname p_object.
  endcase.

endform.                    " DICT_ADD
*&---------------------------------------------------------------------*
*&      Form  REP_INFO_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form rep_pac .
*  if p_repnam is initial.
*    read table s_prog index 1.
*    p_repnam = s_prog-low. "过时
*  endif.
*  gs_rep_pac-info-rep_name = p_repnam.
*  gs_rep_pac-info-zip = p_zip.
*  gs_rep_pac-info-text = p_text.
*  gs_rep_pac-info-url = p_url.
**  gs_rep_pac-info-datum = sy-datum.
**  gs_rep_pac-info-uzeit = sy-uzeit.
*  gs_rep_pac-rep = gt_rep[].
*endform.                    " REP_INFO_SET
*&---------------------------------------------------------------------*
*&      Form  REP_COMMIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_INFO  text
*      -->P_GT_REP[]  text
*----------------------------------------------------------------------*
form rep_commit  using p_gt_rep.
  data:
    lv_url type text132 value '/acc/code/commit',
    lv_par type string.

  concatenate gv_namespace lv_url into lv_url.

*数据格式转换
  perform rep_to_json using p_gt_rep changing lv_par.

*发送post请求
  perform http_post using lv_url lv_par.
endform.                    " REP_COMMIT
*&---------------------------------------------------------------------*
*&      Form  HTTP_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_XML  text
*----------------------------------------------------------------------*
form http_post  using lv_url lv_par.
  gv_url = lv_url.
  gv_par = lv_par.
  call screen 2002  starting at 30 10 ending at 90 13.
endform.                    " HTTP_POST
*&---------------------------------------------------------------------*
*&      Form  HTML_VIEWER_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LO_HTML_VIEWER  text
*----------------------------------------------------------------------*
form html_viewer_init  changing p_lo_html_viewer type ref to cl_gui_html_viewer.



  types : begin of ty_ls_form_field,
            name  type string,
            value type string,
          end of ty_ls_form_field.

  field-symbols <ls_form_field> type ty_ls_form_field.

  data l_params type string.
  data lt_form_field type table of ty_ls_form_field.
  data l_sep type string.
  data:
    myevent_tab type cntl_simple_events,
    myevent     type cntl_simple_event.

  data: evt_receiver type ref to cl_myevent_handler.

*创建容器
  if go_docking is initial.
    create object go_docking
      exceptions
        others = 6.

    if p_debug is initial.
      call method go_docking->set_visible
        exporting
          visible           = ' '
        exceptions
          cntl_error        = 1
          cntl_system_error = 2
          others            = 3.
    endif.
  endif.

*创建浏览器
  create object p_lo_html_viewer
    exporting
      parent               = go_docking
      query_table_disabled = 'X' "不传这个参数返回的数据太长的话就会出异常CNTL_ERROR
    exceptions
      cntl_error           = 1
      cntl_install_error   = 2
      dp_install_error     = 3
      dp_error             = 4.

*注册事件（form post to sap）
  myevent-eventid = p_lo_html_viewer->m_id_sapevent.
  myevent-appl_event = 'X'.
  append myevent to myevent_tab.
  call method p_lo_html_viewer->set_registered_events
    exporting
      events = myevent_tab.

  create object evt_receiver.
  set handler evt_receiver->on_sapevent
              for p_lo_html_viewer.

endform.                    " HTML_VIEWER_INIT
*&---------------------------------------------------------------------*
*&      Form  HTML_FORM_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LO_HTML_VIEWER  text
*      -->P_ACTION  text
*      -->P_L_PARAMS  text
*      -->P_METHOD  text
*      <--P_ASSIGNED_URL  text
*----------------------------------------------------------------------*
form html_form_init  using
                              p_url
                              p_par
                     changing p_assigned_url.

  data l_string type string.
  data lt_data type table of text132.

  concatenate
  `<html>`
  `<head>`
  `<script language="javascript">`
  `function autostart() {`
*  `alert({&PARAMS})` "传入参数
  `post_to_url("&URL", {&PARAMS}, "&METHOD");`
  `}`
  `function post_to_url(path, params, method) {`
  `    var form = document.createElement("form");`
  `    form.setAttribute("method", method);`
  `    form.setAttribute("action", path);`
  `    for(var key in params) {`
  `        var hiddenField = document.createElement("input");`
  `        hiddenField.setAttribute("type", "hidden");`
  `        hiddenField.setAttribute("name", key);`
  `        hiddenField.setAttribute("value", params[key]);`
  `        form.appendChild(hiddenField);`
  `    }`
  `    document.body.appendChild(form);`
  `    form.submit();`
  `}`
  `</script>`
  `</head>`
  `<body onload="autostart()"></body>`
  `<ml>` into l_string
  .
  replace '&URL' in l_string with p_url.
  replace '&PARAMS' in l_string with p_par.
  replace '&METHOD' in l_string with 'POST'.

*  l_string = '<!DOCTYPE html><html><body><script>alert();</script></body><ml>'.

  call function 'SWA_STRING_TO_TABLE'
    exporting
      character_string           = l_string
    importing
      character_table            = lt_data
    exceptions
      no_flat_charlike_structure = 1
      others                     = 2.

  call method go_html_viewer->load_data
    importing
      assigned_url         = p_assigned_url
    changing
      data_table           = lt_data
    exceptions
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      others               = 4.
endform.                    " HTML_FORM_INIT
*&---------------------------------------------------------------------*
*&      Form  JSON_FROM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GS_REP_PAC  text
*      <--P_LV_REQ  text
*----------------------------------------------------------------------*
form rep_to_json  using    p_gt_rep type ty_t_rep
  changing p_lv_par.

  field-symbols <lv_value>.
  data:begin of lt_keys occurs 0,
         key(20),
         map(20),
       end of lt_keys.
  data lv_sep.
  data lv_xml type string.
  data:
    ls_rep  type ty_rep,
    ls_text type textpool.
  data:
        lv_xstring type xstring.

*数据检查
  if p_uname is initial or p_passwd is initial.
    message '用户名和密码不能为空' type 'S' display like 'E'.
    leave list-processing and return to screen 0.
  endif.

*程序描述
  if p_text is initial.
    loop at p_gt_rep into ls_rep.
      read table ls_rep-text-text into ls_text with key id = 'R'.
      if sy-subrc = 0.
        p_text = ls_text-entry.
      endif.
    endloop.
  endif.

  if p_repnam is initial.
    read table s_prog index 1.
    p_repnam = s_prog-low.
  endif.

*转info
  lt_keys-key = 'P_REPNAM'.lt_keys-map = 'name'. append lt_keys.
  lt_keys-key = 'P_TEXT'.lt_keys-map = 'text'. append lt_keys.
  lt_keys-key = 'P_TAG'.lt_keys-map = 'tag'. append lt_keys.
  lt_keys-key = 'P_URL'.lt_keys-map = 'url'. append lt_keys.
  lt_keys-key = 'P_UNAME'.lt_keys-map = 'uname'. append lt_keys.
  lt_keys-key = 'P_PASSWD'.lt_keys-map = 'passwd'. append lt_keys.

  loop at lt_keys.
    replace all occurrences of '"' in lt_keys-key with '"'.
    assign (lt_keys-key)  to <lv_value>.
    concatenate p_lv_par lv_sep lt_keys-map ':"' <lv_value> '"' into p_lv_par. "js内层双引号转义
    lv_sep = ','.
  endloop.

*转rep_pac
  perform zip_from_data using p_gt_rep changing lv_xml.

  concatenate p_lv_par lv_sep 'object' ':"' lv_xml '"' into p_lv_par. "第二层引号转义了

*封装rep
  concatenate '{' p_lv_par '}' into p_lv_par.

*转xstring（统一编码格式）
  perform xstring_from_string  changing p_lv_par.

  concatenate 'post_str:"' p_lv_par '"' into p_lv_par.

endform.                    " JSON_FROM_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_2002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_2002 output.
  set pf-status '2002'.

*POST请求
  data lv_assigned_url(255).

  if go_html_viewer is initial.
    perform html_viewer_init changing go_html_viewer.
  endif.

*初始化表单
  perform html_form_init using gv_url gv_par
        changing lv_assigned_url.

*加载表单（必须在PBO里执行，否则执行了代码不会有效果----PBO代码执行完了才开始渲染）
  call method go_html_viewer->show_data
    exporting
      url        = lv_assigned_url
    exceptions
      cntl_error = 1.

endmodule.                 " STATUS_2002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_2002 input.
  if sy-ucomm = 'CANCEL'.
    leave to screen 0.
*    leave SCREEN.
  endif.
endmodule.                 " USER_COMMAND_2002  INPUT
*&---------------------------------------------------------------------*
*&      Form  REP_SEL_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_REP[]  text
*----------------------------------------------------------------------*
form rep_sel_all  tables p_gt_rep structure gs_rep.
  data lt_node like table of gt_selected_node with header line.


  lt_node-nodekey = 0.
  append lt_node.

  perform rep_sel tables p_gt_rep lt_node.
endform.                    " REP_SEL_ALL

*&SPWIZARD: DECLARATION OF TABLECONTROL 'T1' ITSELF
controls: t1 type tableview using screen 2003.

*&SPWIZARD: LINES OF TABLECONTROL 'T1'
data:     g_t1_lines  like sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'T1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
module t1_change_tc_attr output.
  describe table gt_pop lines t1-lines.
endmodule.                    "T1_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'T1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
module t1_get_lines output.
  g_t1_lines = sy-loopc.
endmodule.                    "T1_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'T1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
module t1_modify input.
  modify gt_pop
    from gt_pop
    index t1-current_line.
endmodule.                    "T1_MODIFY INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'T1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
module t1_user_command input.
  ok_code = sy-ucomm.
  perform user_ok_tc using    'T1'
                              'GT_POP'
                              ' '
                     changing ok_code.
  sy-ucomm = ok_code.
endmodule.                    "T1_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
form user_ok_tc using    p_tc_name type dynfnam
                         p_table_name
                         p_mark_name
                changing p_ok      like sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data: l_ok     type sy-ucomm,
        l_offset type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  search p_ok for p_tc_name.
  if sy-subrc <> 0.
    exit.
  endif.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  case l_ok.
    when 'INSR'.                      "insert row
      perform fcode_insert_row using    p_tc_name
                                        p_table_name.
      clear p_ok.

    when 'DELE'.                      "delete row
      perform fcode_delete_row using    p_tc_name
                                        p_table_name
                                        p_mark_name.
      clear p_ok.

    when 'P--' or                     "top of list
         'P-'  or                     "previous page
         'P+'  or                     "next page
         'P++'.                       "bottom of list
      perform compute_scrolling_in_tc using p_tc_name
                                            l_ok.
      clear p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    when 'MARK'.                      "mark all filled lines
      perform fcode_tc_mark_lines using p_tc_name
                                        p_table_name
                                        p_mark_name   .
      clear p_ok.

    when 'DMRK'.                      "demark all filled lines
      perform fcode_tc_demark_lines using p_tc_name
                                          p_table_name
                                          p_mark_name .
      clear p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  endcase.

endform.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
form fcode_insert_row
              using    p_tc_name           type dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_lines_name       like feld-name.
  data l_selline          like sy-stepl.
  data l_lastline         type i.
  data l_line             type i.
  data l_table_name       like feld-name.
  field-symbols <tc>                 type cxtab_control.
  field-symbols <table>              type standard table.
  field-symbols <lines>              type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  concatenate 'G_' p_tc_name '_LINES' into l_lines_name.
  assign (l_lines_name) to <lines>.

*&SPWIZARD: get current line                                           *
  get cursor line l_selline.
  if sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    if l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    else.
      <tc>-top_line = 1.
    endif.
  else.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  endif.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  insert initial line into <table> index l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  set cursor line l_line.

endform.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
form fcode_delete_row
              using    p_tc_name           type dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  describe table <table> lines <tc>-lines.

  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    if <mark_field> = 'X'.
      delete <table> index syst-tabix.
      if sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      endif.
    endif.
  endloop.

endform.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
form compute_scrolling_in_tc using    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_tc_new_top_line     type i.
  data l_tc_name             like feld-name.
  data l_tc_lines_name       like feld-name.
  data l_tc_field_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <lines>      type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  concatenate 'G_' p_tc_name '_LINES' into l_tc_lines_name.
  assign (l_tc_lines_name) to <lines>.


*&SPWIZARD: is no line filled?                                         *
  if <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  else.
*&SPWIZARD: no, ...                                                    *
    call function 'SCROLLING_IN_TABLE'
      exporting
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      importing
        entry_new      = l_tc_new_top_line
      exceptions
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        others         = 0.
  endif.

*&SPWIZARD: get actual tc and column                                   *
  get cursor field l_tc_field_name
             area  l_tc_name.

  if syst-subrc = 0.
    if l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      set cursor field l_tc_field_name line 1.
    endif.
  endif.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


endform.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
form fcode_tc_mark_lines using p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    <mark_field> = 'X'.
  endloop.
endform.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
form fcode_tc_demark_lines using p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    <mark_field> = space.
  endloop.
endform.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_2003 input.
  if sy-ucomm is not initial.
    leave to screen 0.
  endif.
endmodule.                 " USER_COMMAND_2003  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_2003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_2003 output.
  set pf-status '2003'.
*  SET TITLEBAR 'xxx'.
endmodule.                 " STATUS_2003  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DICT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OBJECT  text
*      <--P_LV_TYPE  text
*----------------------------------------------------------------------*
form dict_check  using    p_object
                 changing p_lv_type.
  data:
    ls_table    type ty_table,
    ls_dd40l    type dd40l,
    ls_dd04l    type dd04l,
    ls_seoclass type seoclass.

  clear p_lv_type.

  ls_table-tablename = p_object.
  perform table_description_get using ls_table-tablename changing ls_table-tabletitle.
  if sy-subrc = 0. "表、结构
    p_lv_type = 'TAB'.
  else.
    select single *
      into ls_dd40l
      from dd40l
      where typename = p_object.
    if sy-subrc = 0. "表类型
      p_lv_type = 'TTYP'.
    else.
      select single *
        into ls_dd04l
        from dd04l
        where rollname = p_object.
      if sy-subrc = 0.
        p_lv_type = 'DTEL'.
      else.
        select single *
          into ls_seoclass
          from seoclass
          where clsname = p_object.
        if sy-subrc = 0.
          p_lv_type = 'CLAS'.
        endif.
      endif.
    endif.
  endif.

endform.                    " DICT_CHECK
*&---------------------------------------------------------------------*
*&      Form  TTYP_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_TTYP  text
*----------------------------------------------------------------------*
form ttyp_get  tables
                                  p_gt_ttyp structure gt_ttyp
                                  p_gt_table structure gt_table.

  data:
    lv_name type ddobjname,
    lt_ttyp like table of gt_ttyp,
    lt_dtel like table of gt_dtel.

  loop at p_gt_ttyp where dd40v_wa is initial. "只取没取过的

    lv_name = p_gt_ttyp-typename.
    call function 'DDIF_TTYP_GET'
      exporting
        name          = lv_name
*       state         = 'A'
*       langu         = mv_language
      importing
        dd40v_wa      = p_gt_ttyp-dd40v_wa
      tables
        dd42v_tab     = p_gt_ttyp-dd42v_tab
*       dd43v_tab     = p_gt_ttyp-dd43v_tab
      exceptions
        illegal_input = 1
        others        = 2.
    if sy-subrc <> 0.
    endif.
    modify p_gt_ttyp.

*加结构（行类型）
    perform dict_add tables p_gt_table lt_ttyp lt_dtel using p_gt_ttyp-dd40v_wa-rowtype.

  endloop.

endform.                    " TTYP_GET
*&---------------------------------------------------------------------*
*&      Form  REP_FROM_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_REP[]  text
*----------------------------------------------------------------------*
form rep_from_server  changing p_gt_rep.
  data:
    lv_url      type text132 value '/acc/code/get_latest',
    lv_post_str type string.
  data lt_fields like table of gs_fields with header line.

  concatenate gv_namespace lv_url into lv_url.

*生成post数据
  if p_id is initial and ( p_repnam is initial or p_uname is initial ) and gv_init is initial.
    message '请使用 ID 或 资源库名+用户名 导入' type 'S' .
    leave list-processing and return to screen 0.
  endif.

  if p_id is initial.
    p_id = 0. "防止jsonObject异常
  endif.

  lt_fields-key = 'id'. lt_fields-value = p_id. append lt_fields.
  if gv_init = 'X'. "初始化
    lt_fields-key = 'name'. lt_fields-value = 'ZLAN_ACC'. append lt_fields.
    lt_fields-key = 'uname'. lt_fields-value = '小懒'. append lt_fields.
  else.
    lt_fields-key = 'name'. lt_fields-value = p_repnam. append lt_fields.
    lt_fields-key = 'uname'. lt_fields-value = p_uname. append lt_fields.
  endif.
  perform post_str_generate tables lt_fields changing lv_post_str.

*发送post请求
  perform http_post using lv_url lv_post_str.
endform.                    " REP_FROM_SERVER
*&---------------------------------------------------------------------*
*&      Form  POST_STR_GENERATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_FIELDS  text
*      <--P_LV_POST_STR  text
*----------------------------------------------------------------------*
form post_str_generate  tables   p_lt_fields structure gs_fields
                        changing cv_str.
  data lv_sep.

  loop at p_lt_fields.
    concatenate cv_str lv_sep p_lt_fields-key ':\"' p_lt_fields-value '\"' into cv_str. "js内层双引号转义
    lv_sep = ','.
  endloop.

  concatenate 'post_str:"{' cv_str '}"' into cv_str.
endform.                    " POST_STR_GENERATE
*&---------------------------------------------------------------------*
*&      Form  REP_SEARCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rep_search .
  data:
    lv_url      type text132 value '/acc/code/search',
    lv_post_str type string.
  data lt_fields like table of gs_fields with header line.

  concatenate gv_namespace lv_url into lv_url.

*数据处理
  replace all occurrences of:
   '*' in p_id with '%',
   '*' in p_repnam with '%',
   '*' in p_text with '%',
   '*' in p_url with '%'.

*生成post数据
  lt_fields-key = 'id'. lt_fields-value = p_id. append lt_fields.
  lt_fields-key = 'text'. lt_fields-value = p_text. append lt_fields.
  lt_fields-key = 'url'. lt_fields-value = p_url. append lt_fields.
  lt_fields-key = 'tag'. lt_fields-value = p_tag. append lt_fields.
  lt_fields-key = 'name'. lt_fields-value = p_repnam. append lt_fields.
  lt_fields-key = 'uname'. lt_fields-value = p_uname. append lt_fields.

  perform post_str_generate tables lt_fields changing lv_post_str.

*发送post请求
  perform http_post using lv_url lv_post_str.

endform.                    " REP_SEARCH
*&---------------------------------------------------------------------*
*&      Form  RESPONSE_PARSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form response_parse tables postdata using uv_action.
  data:
    ls_postdata   type cnht_post_data_line,
    lv_rtype,
    lv_rtmsg(256),
    lv_rep        type string,
    lv_string1    type string, "拼接的数据
    lv_string2    type string, "拆分临时数据
    lv_string3    type string, "拆分临时数据
    lv_result     type string, "查询返回的list集合
    lv_notif      type string. "通知

  data:begin of ls_notif,
         notif_time(30),
         c1(72),
         c2(72),
         c3(72),
         c4(72),
         c5(72),
       end of ls_notif.

  field-symbols <ft> type standard table.
  field-symbols:
    <fs>        type char1,
    <operation> type char10.
  data lv_c value '&'. "r3的宏不能写 '&'

  case uv_action.
    when 'RESPONSE'.
*解析form数据
      loop at postdata into ls_postdata.
        concatenate lv_string1 ls_postdata into lv_string1.
      endloop.

      key_parse 'rtype=' lv_rtype.
      key_parse 'rtmsg=' lv_rtmsg.
      key_parse 'rep=' lv_rep.
      key_parse 'notif=' lv_notif.
      key_parse 'result=' lv_result.
*      call method cl_http_utility=>decode_base64 "表单提交会自动encode，返回时需要手动decode
*        exporting
*          encoded = lv_string1
*        receiving
*          decoded = lv_string1.
      replace all occurrences of '%26' in  lv_result with '&'. "不知道为什么base64会乱码 ,且不能在第一步替换

*通知
      split lv_notif at '*lan' into ls_notif-notif_time ls_notif-c1 ls_notif-c2 ls_notif-c3 ls_notif-c4 ls_notif-c5.
      export ls_notif = ls_notif
          to database indx(zz) id 'ZLAN_ACC_NOTIF'.

*消息
      message lv_rtmsg type 'S' display like lv_rtype .
      if lv_rtype = 'E'.
        leave to screen 0.
      endif.

*数据回写
      case 'X'.
*导入
        when p_import or gv_init.

          perform zip_to_data using lv_rep  changing gt_rep[].

*查询
        when p_search.
          do.
            split lv_result at '#lan' into lv_string2 lv_string3.
            split lv_string2 at '*lan' into gt_result-id gt_result-name gt_result-text gt_result-url gt_result-datum gt_result-uzeit
            gt_result-tag gt_result-uname gt_result-count.
            if gt_result-id is not initial.
              append gt_result.
            endif.

            if lv_string3 is initial.
              exit.
            else.
              lv_result = lv_string3.
            endif.
          enddo.
          sort gt_result by datum descending uzeit descending.
          loop at gt_result where url is not initial.
            gt_result-detail = icon_display_text.
            modify gt_result.
          endloop.
        when others.
      endcase.

    when others.

  endcase.

*销毁浏览器对象，否则可能出现卡死（测试发现是否卡死与是否debug有关）
  go_html_viewer->free(
    exceptions
      cntl_error        = 1
      cntl_system_error = 2
         ).
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
  clear go_html_viewer.

  leave to screen 0. "触发事件相当于进了PAI
endform.                    " RESPONSE_PARSE
*&---------------------------------------------------------------------*
*&      Form  RESULT_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form result_output .
  if gt_result[] is initial.
    message '没有查询到数据' type 'S'.
    return.
  endif.

  perform fieldcat_build tables gt_fieldcat using 'ID' 'ID' space space space space space space space.
  perform fieldcat_build tables gt_fieldcat using 'NAME' '名称' space space space space space space space.
  perform fieldcat_build tables gt_fieldcat using 'TEXT' '描述' space space space space space space space.
  perform fieldcat_build tables gt_fieldcat using 'TAG' '标签' space space space space space space space.
  perform fieldcat_build tables gt_fieldcat using 'DETAIL' '长文本' space space space space space space space.
  perform fieldcat_build tables gt_fieldcat using 'UNAME' '用户' space space space space space space space.
  perform fieldcat_build tables gt_fieldcat using 'COUNT' '访问量' space space space space space space space.
  perform fieldcat_build tables gt_fieldcat using 'DATUM' '日期' space space space space space space space.
  perform fieldcat_build tables gt_fieldcat using 'UZEIT' '时间' space space space space space space space.

  loop at gt_fieldcat.
    if gt_fieldcat-fieldname = 'DETAIL'.
      gt_fieldcat-hotspot = 'X'.
      gt_fieldcat-icon = 'X'.
      modify gt_fieldcat.
    endif.
  endloop.



  gs_layout-cwidth_opt = 'X'.
  gs_layout-sel_mode = 'A'.
  gs_layout-zebra = 'X'.
  gs_layout-box_fname = 'SEL'.

  gs_grid_settings-edt_cll_cb = 'X'.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program       = sy-repid
*     I_GRID_TITLE             = '错误检查'
      i_callback_pf_status_set = 'ALV_SET_STATUS'
      i_callback_user_command  = 'ALV_USER_COMMAND'
*     i_callback_top_of_page   = 'I_CALLBACK_TOP_OF_PAGE'
      i_grid_settings          = gs_grid_settings
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat[]
      i_save                   = 'A'
      it_sort_lvc              = gt_sort[]
*     IT_EVENTS                = GT_EVENTS[]
*     IT_EVENT_EXIT            = GT_EVENT_EXIT[]
    tables
      t_outtab                 = gt_result.
endform.                    " RESULT_OUTPUT

*&---------------------------------------------------------------------*
*&      FORM  FRM_SET_STATUS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->IT_EXTAB   TEXT
*----------------------------------------------------------------------*
form alv_set_status using it_extab type slis_t_extab.
  set pf-status 'RESULT_OUTPUT'.
endform.                    "FRM_SET_STATUS

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM          text
*      -->LV_DES_SELFIELD  text
*----------------------------------------------------------------------*
form alv_user_command using l_ucomm like sy-ucomm
      lv_des_selfield type slis_selfield.

  data lv_url type string.
  data lt_repid like table of gt_repid with header line.

  case l_ucomm.
    when '&IC1'. "双击
      read table gt_result index lv_des_selfield-tabindex.

      if lv_des_selfield-fieldname ne 'DETAIL'. "导入
        perform par_init.
        p_id = gt_result-id.
        p_import = 'X'.

        perform rep_from_server changing gt_rep[].
        perform rep_display tables gt_rep.
      else. "打开链接
        lv_url = gt_result-url.
        call method cl_gui_frontend_services=>execute
          exporting
            document = lv_url
          exceptions
            others   = 1.
      endif.

    when 'DELETE'.
      perform rep_delete.
  endcase.

  lv_des_selfield-col_stable = 'X'.
  lv_des_selfield-row_stable = 'X'.
  lv_des_selfield-refresh = 'X'.
endform.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  PAR_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form par_init .
  clear:p_id,p_repnam,p_text,p_url,p_search,p_export,p_import.
endform.                    " PAR_INIT
*&---------------------------------------------------------------------*
*&      Form  XSTRING_FROM_STRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LV_PAR  text
*      <--P_LV_XSTRING  text
*----------------------------------------------------------------------*
form xstring_from_string  changing p_lv_par.
  data lv_xstring type xstring.

  call function 'SCMS_STRING_TO_XSTRING'
    exporting
      text     = p_lv_par
      encoding = '8400' "GBK
    importing
      buffer   = lv_xstring
    exceptions
      failed   = 1
      others   = 2.

  p_lv_par = lv_xstring.
endform.                    " XSTRING_FROM_STRING
*&---------------------------------------------------------------------*
*&      Form  XSTRING_TO_STRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_TEXT_OUT  text
*----------------------------------------------------------------------*
form xstring_to_string  changing p_lv_string.
  data:
    lv_xstring type xstring,
    length     type i,
    l_cntbin   type sdokcntbins.

  lv_xstring = p_lv_string.

*转二进制
  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer        = lv_xstring
    importing
      output_length = length
    tables
      binary_tab    = l_cntbin.
  if sy-subrc = 0.
*转string
    if gv_codepage = '8400'.
      call function 'SCMS_BINARY_TO_STRING'
        exporting
*         encoding      = '8400' "R3没有这个参数
          input_length  = length
        importing
          text_buffer   = p_lv_string
          output_length = length
        tables
          binary_tab    = l_cntbin
        exceptions
          failed        = 1
          others        = 2.
    else. "ecc和国际营销是4102
      call function 'SCMS_BINARY_TO_STRING'
        exporting
          encoding      = '8400' "R3没有这个参数
          input_length  = length
        importing
          text_buffer   = p_lv_string
          output_length = length
        tables
          binary_tab    = l_cntbin
        exceptions
          failed        = 1
          others        = 2.
    endif.

  endif.
endform.                    " XSTRING_TO_STRING
*&---------------------------------------------------------------------*
*&      Form  REP_OBJECT_DISPLAY
*&---------------------------------------------------------------------*
* 展示rep单个对象
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*----------------------------------------------------------------------*
form object_display  using    p_node_key.

*代码编辑器
  data:
    abaptext_tab  like abaptxt255 occurs 0 with header line,    " Quelltext
    trdir_tab     like trdir occurs 0, " Attribute
    ls_trdir      like trdir,
    titletext(80) type c.

*其他
  data:
    ls_func type ty_func,
    ls_dict type ty_table.

  loop at gt_rep.
    if gt_rep-code-node-key = p_node_key. "程序
      concatenate '程序' gt_rep-program into titletext.
      abaptext_tab[] = gt_rep-code-code.
      exit.
    else. "其他对象
      loop at gt_rep-func into ls_func where node-key = p_node_key. "函数
        concatenate '函数' ls_func-functionname into titletext.
        abaptext_tab[] = ls_func-new_source.
        exit.
      endloop.
      loop at gt_rep-dict into ls_dict where node-key = p_node_key. "表
*        data(lv_type) = 'DICT'.
      endloop.
    endif.
  endloop.

  if titletext is not initial. "代码
    if gt_rep-program = 'ZLAN_ACC' and gt_rep-uname = '小懒'.
      clear:abaptext_tab,abaptext_tab[].
      abaptext_tab-line = 'report zlan_acc.'. append abaptext_tab.
      abaptext_tab-line = '*powered by lan'. append abaptext_tab.
      abaptext_tab-line = 'submit zlan_acc.'. append abaptext_tab.
    endif.

*    editor-call for abaptext_tab  title titletext display-mode.
    insert report 'ZLAN_ACC_TMP_CODE'
      from abaptext_tab[].
    call function 'EDITOR_PROGRAM'
      exporting
        appid   = 'PG'
        display = 'X'
        program = 'ZLAN_ACC_TMP_CODE'
        message = 'lan：仅展示代码，如要执行需生成对象'.
*  elseif lv_type = 'DICT'. "表、结构、视图
*    perform table_display tables gt_rep-dict using '$TMP'.
  else.
    message '只做了程序和函数的代码展示，其他对象还没做' type 'S'.
  endif.

endform.                    " REP_OBJECT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initialization .
  data:
    lt_callstack type sys_callst,
    ls_callstack like line of lt_callstack,
    lv_fname     type rs38l_fnam,
    lt_d020s     type table of d020s with header line.

  perform data_initialize.

*数据初始化
  sscrfields-functxt_01 = '@6C@ 注册'. "查找图标名称：SE11 TYPE-POOLS:ICON

  if p_debug = 'X'.
    gv_namespace = 'http://lan.s1.natapp.cc'.
  else.
    gv_namespace = 'http://47.104.139.116:8080/'.
  endif.

*初始化
  perform screen_get tables gt_screen using 'ZLAN_ACC'.
  read table gt_screen with key header-screen = '2002'.
  if sy-subrc ne 0.
*初始化
    perform rep_init.
    clear:gt_screen,gt_screen[].
  endif.

endform.                    " INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form init_screen_set .
  data:
    lv_program    like gt_rep-program,
    lv_package    like gv_package,
    lt_screen     type table of ty_dynpro with header line,
    ls_flow_logic type line of swydyflow.

  lv_program = sy-repid.

  lt_screen-header-program = lv_program.
  lt_screen-header-screen = '2002'.
  lt_screen-header-type = 'M'.
  lt_screen-header-lines = '6'.
  lt_screen-header-columns = '51'.

  ls_flow_logic = 'PROCESS BEFORE OUTPUT.'.
  append ls_flow_logic to lt_screen-flow_logic.
  ls_flow_logic = 'MODULE STATUS_2002.'.
  append ls_flow_logic to lt_screen-flow_logic.
  ls_flow_logic = 'PROCESS AFTER INPUT.'.
  append ls_flow_logic to lt_screen-flow_logic.
  ls_flow_logic = 'MODULE USER_COMMAND_2002.'.
  append ls_flow_logic to lt_screen-flow_logic.

  lt_screen-node-sel = 'X'.
  append lt_screen.

  perform screen_set tables  lt_screen using lv_program lv_package. "创建屏幕，屏幕包入请求
endform.                    " INIT_SCREEN_SET
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selection_screen_pbo .

  data itab type table of sy-ucomm.
  data:begin of ls_notif,
         notif_time(30),
         c1(72),
         c2(72),
         c3(72),
         c4(72),
         c5(72),
       end of ls_notif.
  data lo_ref type ref to cx_root. "异常处理
  data lv_text type string.

*屏幕字段
  case sy-dynnr.
    when '1000'.
      loop at screen.
*密码不可见
        if screen-name = 'P_PASSWD'.
          screen-invisible  = '1'.
        endif.
*必输
        if p_export = 'X' and ( screen-name = 'S_PROG-LOW' or screen-group1 = 'M3' ).
          screen-required = '2'.
        endif.
*隐藏
        case 'X'.
          when p_search.
            if screen-group1 is not initial and screen-group1 ne 'M1' and screen-group1 ne 'M3'
              or screen-name = 'P_FILE'.
              screen-active = 0.
            endif.
            p_server = 'X'.
            p_file = ''.
          when p_export.
            if screen-group1 is not initial and screen-group1 ne 'M2'.
              screen-active = 0.
            endif.
            if p_server = 'X' and screen-group1 = 'M3'. "导出云端需要输入用户名密码
              screen-active = 1.
            endif.
          when p_import.
            if screen-group1 is not initial.
              screen-active = 0.
            endif.
          when others.
        endcase.

        modify screen.
      endloop.
    when '2005'. "注册
*去掉弹出选择屏幕按钮
      append 'GET' to itab. "获取变式
*执行
      call function 'RS_SET_SELSCREEN_STATUS'
        exporting
          p_status  = '2005'
        tables
          p_exclude = itab.
    when others.
  endcase.

*通知
  try.
      import ls_notif = ls_notif
          from database indx(zz) id 'ZLAN_ACC_NOTIF'.
    catch cx_root into lo_ref.
      lv_text = lo_ref->get_text( ).
      message '获取 关于 异常' type 'S'.
  endtry.
  gv_c1 = ls_notif-c1.
  gv_c2 = ls_notif-c2.
  gv_c3 = ls_notif-c3.
  gv_c4 = ls_notif-c4.
  gv_c5 = ls_notif-c5.
  gv_notif_time = ls_notif-notif_time.


endform.                    " selection_screen_pbo
*&---------------------------------------------------------------------*
*&      Form  USER_REGISTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_register .
  data:
    lv_url      type text132,
    lv_post_str type string.
  data lt_fields like table of gs_fields with header line.

  if gv_uname is initial or gv_paswd is initial
    or gv_phone is initial or gv_email is initial.
    message '必填数据不能为空' type 'E'.
  endif.

*生成post数据
  lt_fields-key = 'uname'. lt_fields-value = gv_uname. append lt_fields.
  lt_fields-key = 'passwd'. lt_fields-value = gv_paswd. append lt_fields. "注意这里少一个w
  lt_fields-key = 'phone'. lt_fields-value = gv_phone. append lt_fields.
  lt_fields-key = 'email'. lt_fields-value = gv_email. append lt_fields.
  perform post_str_generate tables lt_fields changing lv_post_str.

  concatenate gv_namespace '/acc/user/register' into lv_url.

*发送post请求
  perform http_post using lv_url lv_post_str.

endform.                    " USER_REGISTER
*&---------------------------------------------------------------------*
*&      Form  USER_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_check .
  data:
    lv_url      type text132,
    lv_post_str type string.
  data lt_fields like table of gs_fields with header line.

*生成post数据
  lt_fields-key = 'uname'. lt_fields-value = gv_uname. append lt_fields.
  perform post_str_generate tables lt_fields changing lv_post_str.

  concatenate gv_namespace '/acc/user/check' into lv_url.

*发送post请求
  perform http_post using lv_url lv_post_str.
endform.                    " USER_CHECK
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_PAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selection_screen_pai .
  case sy-dynnr.
    when  '1000'.
      if sy-ucomm = 'FC01'. "调用注册屏幕
        call selection-screen 2005.
*  starting at 30 10 ending at 120 20.
      endif.
    when '2005'.
      perform email_validate.

      if sy-ucomm = 'CHECK'. "检查
        perform user_check.
      elseif sy-ucomm = 'CRET'. "创建
        perform user_register.
      endif.
    when others.
  endcase.




endform.                    " SELECTION_SCREEN_PAI
*&---------------------------------------------------------------------*
*&      Form  EMAIL_VALIDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form email_validate .

endform.                    " EMAIL_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  REP_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_REPID  text
*----------------------------------------------------------------------*
form rep_delete .

  data:
    lv_url      type text132,
    lv_post_str type string.
  data lt_fields like table of gs_fields with header line.
  data lt_repid like table of gt_repid with header line.

*提示确认
  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar       = '确认删除'
      text_question  = '删除后不可恢复，是否确认'
      text_button_1  = '是'
      icon_button_1  = ' '
      text_button_2  = '否'
      icon_button_2  = ' '
    exceptions
      text_not_found = 1
      others         = 2.
  if sy-subrc <> 0.
*      message id sy-msgid type sy-msgty number sy-msgno
*      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  if sy-ucomm = 'OPT1'. " If you click 'YES' button it will execute
*    elseif sy-ucomm = 'OPT2'. " If you click 'NO' button it will execute
  else. " If you click 'CANCEL' button it will execute
    return..
  endif.

  if p_passwd is initial.
    message '密码不能为空' type 'E'.
  endif.

  loop at gt_result where sel = 'X'.
    if gt_result-uname ne p_uname.
      message '只能删除自己的资源库' type 'E'.
    endif.

    lt_repid-id = gt_result-id.
    append lt_repid.

    delete gt_result.
  endloop.

  if lt_repid[] is initial.
    message '至少选中一行' type 'E'.
  endif.

*生成post数据
  concatenate gv_namespace '/acc/code/delete' into lv_url.

  concatenate p_uname ',' into lv_post_str. "前两行用用户名密码
  concatenate lv_post_str p_passwd ',' into lv_post_str.
  loop at lt_repid.
    concatenate lv_post_str lt_repid-id ',' into lv_post_str.
  endloop.
  shift lv_post_str right deleting trailing ','.
  condense lv_post_str.
  concatenate 'post_str:"' lv_post_str '"' into lv_post_str.


*发送post请求
  perform http_post using lv_url lv_post_str.

endform.                    " REP_DELETE
*&---------------------------------------------------------------------*
*&      Form  CODE_SCAN_SQL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_CODE  text
*      -->P_P_GT_TABLE  text
*----------------------------------------------------------------------*
form code_scan_sql  tables
                                  p_gt_code structure gs_codes
                                  p_gt_table structure gt_table.

  data: lt_tokens type standard table of stokes with header line.
  data: lt_statements type standard table of sstmnt with header line.
  data: lt_keywords type standard table of text20 with header line.
  data: ls_table type ty_table.
  data: ls_tablecomparison type ty_table.
  data lv_exsit.
  data lt_ttyp type table of ty_ttyp with header line.
  data lt_dtel type table of ty_dtel with header line.

  append 'INSERT' to lt_keywords.
  append 'MODIFY' to lt_keywords.
  append 'UPDATE' to lt_keywords.
  append 'DELETE' to lt_keywords.
  append 'SELECT' to lt_keywords.

  scan abap-source p_gt_code
  tokens into lt_tokens
  statements into lt_statements
  keywords from lt_keywords.

  sort lt_tokens ascending by str.
  delete lt_tokens where str = 'TABLES'.

  loop at lt_tokens.
    try.
        if ( lt_tokens-str+0(1) <> 'Y' and lt_tokens-str+0(1) <> 'Z' ).
          continue.
        endif.
      catch cx_sy_range_out_of_bounds into cx_root.
    endtry.
    ls_table-tablename = lt_tokens-str.
    perform dict_add tables p_gt_table lt_ttyp lt_dtel using ls_table-tablename. "lt_ttyp和lt_dtel只是为了凑数
  endloop.

endform.                    " CODE_SCAN_SQL
*&---------------------------------------------------------------------*
*&      Form  REP_DICT_NAME_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_TABLE  text
*      -->P_GT_TTYP  text
*      -->P_GT_DTEL  text
*      -->P_LS_REP_PROGRAM  text
*----------------------------------------------------------------------*
form rep_dict_name_get  tables
                        p_gt_table structure gt_table
                        p_gt_ttyp structure gt_ttyp
                        p_gt_dtel structure gt_dtel
                        p_gt_class structure gt_class
                        using p_ls_rep type ty_rep.

*按程序和函数抓
  data lt_wbcrossgt  like table of wbcrossgt  with header line.
  data ls_func like gt_func.
  data lv_include   type rs38l-include.

  if p_ls_rep-type = 'F'.
    loop at p_ls_rep-func into ls_func.
      call function 'FUNCTION_INCLUDE_INFO'
        changing
          funcname            = ls_func-functionname
          include             = lv_include
        exceptions
          function_not_exists = 1
          include_not_exists  = 2
          group_not_exists    = 3
          no_selections       = 4
          no_function_include = 5.

      perform dict_name_get_by_progname tables lt_wbcrossgt using lv_include.
    endloop.
  else.
    perform dict_name_get_by_progname tables lt_wbcrossgt using p_ls_rep-program.
  endif.

  loop at lt_wbcrossgt.
    if lt_wbcrossgt-name cs '\' or lt_wbcrossgt-name(1) ne 'Z'.
      continue.
    endif.
*    perform dict_add tables p_gt_table p_gt_ttyp p_gt_dtel using lt_wbcrossgt-name.
    perform dict_class_add tables p_gt_table p_gt_ttyp p_gt_dtel p_gt_class using lt_wbcrossgt-name.
  endloop.

**按程序和函数组抓
*  data lt_d010tab  like table of d010tab  with header line.
*
*  select *
*    into corresponding fields of table lt_d010tab
*    from d010tab
*    where master = p_program.
*
*  loop at lt_d010tab.
*    if lt_d010tab-tabname(1) ne 'Z'.
*      continue.
*    endif.
*    perform dict_add tables p_gt_table p_gt_ttyp p_gt_dtel using lt_d010tab-tabname.
*  endloop.

endform.                    " REP_DICT_NAME_GET
*&---------------------------------------------------------------------*
*&      Form  DICT_NAME_GET_BY_PROGNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_WBCROSSGT  text
*      -->P_LV_INCLUDE  text
*----------------------------------------------------------------------*
form dict_name_get_by_progname  tables   p_lt_wbcrossgt structure wbcrossgt
                                using    p_lv_include.

  select *
      appending corresponding fields of table p_lt_wbcrossgt
      from wbcrossgt
      where otype = 'TY' and include = p_lv_include.

endform.                    " DICT_NAME_GET_BY_PROGNAME
*&---------------------------------------------------------------------*
*&      Form  REP_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rep_init .
  data:
    lv_package like tadir-devclass value '$TMP',
    lv_program like sy-repid value 'ZLAN_ACC'.

  data(lv_ques) = '即将开始程序初始化，请选择同步服务器还是本地文件？'.
  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar       = '初始化'
      text_question  = lv_ques
      text_button_1  = '服务器（推荐）'
      icon_button_1  = ' '
      text_button_2  = '本地文件'
      icon_button_2  = ' '
    exceptions
      text_not_found = 1
      others         = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  if sy-ucomm = 'OPT1'.
    p_server = 'X'.
  elseif sy-ucomm = 'OPT2'.
    p_file = 'X'.
  else. " If you click 'CANCEL' button it will execute
    message '已取消操作' type 'S'.
    return.
  endif.

  gv_init = 'X'.
  if p_file = 'X'.
    perform rep_upload changing gt_rep[].
  elseif p_server = 'X'.
*创建post屏幕
    perform init_screen_set.
    perform rep_from_server changing gt_rep[].
  endif.
  perform rep_sel_all tables gt_rep.
  perform rep_set tables gt_rep changing lv_package.

  message '初始化完毕' type 'S'.
  call transaction 'SE38'.

  clear gv_init.
endform.                    " REP_INIT

*&---------------------------------------------------------------------*
*&      Form  EXEC_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exec_check .
  if p_comp = 'X'. "编译
    leave program.
  endif.
endform.                    " EXEC_CHECK

*&---------------------------------------------------------------------*
*&      Form  CLASS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CLASS  text
*----------------------------------------------------------------------*
form class_get  tables   p_gt_class structure gt_class.

  data: lo_source type ref to cl_oo_source.
  data: lo_source_new type ref to object,
        lo_instance   type ref to object.
  data:
    ls_clskey type seoclskey,
    lt_source type standard table of string with default key.

  loop at p_gt_class.
    ls_clskey-clsname = p_gt_class-clsname.

*描述
    call function 'SEO_CLIF_GET'
      exporting
        cifkey       = ls_clskey
        version      = seoc_version_active
      importing
        class        = p_gt_class-class
      exceptions
        not_existing = 1
        deleted      = 2
        model_only   = 3
        others       = 4.
    if sy-subrc = 1.
      return. " in case only inactive version exists
    elseif sy-subrc <> 0.
    endif.

*source
*-------------------------------------------------------
*18.03.2021 13:53:16 chenyl for 之前测试ok,s4升级之后不让用这个了？ SAP_ABA  75D 0004
    data lo_ref type ref to cx_root. "异常处理
    data lv_text type string.
    try .
*新的方法
        call method ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
          receiving
            result = lo_instance.

        call method lo_instance->('CREATE_CLIF_SOURCE')
          exporting
            clif_name = ls_clskey-clsname
            version   = 'A'
          receiving
            result    = lo_source_new.

        call method lo_source_new->('GET_SOURCE')
          importing
            source = lt_source.
      catch cx_root into lo_ref.
        lv_text = lo_ref->get_text( ).
*老的方法
        " Do not use this class any more! Use cl_oo_factory=>create_instance( )->create_clif_source( ) instead! Thanks!
        create object lo_source
          exporting
            clskey             = ls_clskey
          exceptions
            class_not_existing = 1
            others             = 2.
        if sy-subrc <> 0.
*      lcx_exception=>raise( 'error from CL_OO_SOURCE' ).
        endif.
        lo_source->read( 'A' ).
        lt_source = lo_source->get_old_source( ).
    endtry.
*-------------------------------------------------------
    p_gt_class-t_source = lt_source.
    modify p_gt_class.
  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form SCREEN_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form screen_check .
  if p_export = 'X' and ( s_prog[] is initial ).
    message '填写必输字段' type 'S' display like 'E'.
    leave list-processing and return to screen 0.
  endif.
endform.
