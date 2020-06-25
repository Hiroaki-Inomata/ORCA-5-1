      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt_h software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt_h software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPI001R10V2.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : �����������μ���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  17/05/18    NACL�����ġ���������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                             DIVISION.
       CONFIGURATION                           SECTION.
       INPUT-OUTPUT                            SECTION.
       FILE-CONTROL.
      *
      *
       DATA                                    DIVISION.
       FILE                                    SECTION.
      *
      *
       WORKING-STORAGE                         SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ��Ǽ��̳���̥ѥ�᥿
           COPY    "S01COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-SYUNOU                      PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DENPNUM                     PIC 9(7).
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD             PIC X(08).
               05  WRK-FILEMEI-HMS             PIC X(06).
               05  WRK-FILEMEI-CD              PIC X(06).
      *
       01  CONST-AREA.
           03  CONST-MODE-G.
               05  CONST-MODE-REPRINT          PIC X(02)   VALUE "09".
               05  CONST-MODE-MONTHLY-PRINT    PIC X(02)   VALUE "10".
      *
      *
           COPY    "CPRPKEY.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    ����饤��Ģɼ�ɣ�Ϣ��
           COPY    "CPORCSRPLNK.INC".
      *
      *    ����饤��Ģɼ̾��������ץ��̾����
           COPY    "CPORCSPRTNM.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ��������ٽ��������
           COPY    "CPSSUB03.INC".
      *
      *    �����ꥯ�����ȥ����å�
           COPY    "CPSSUB06.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ���饤����Ȱ�������
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    API XML�ɤ߹����������
           COPY    "CPIRHV2REQ.INC".
      *
      *    API XML�ɤ߹��߶������
           COPY    "CPAPIV2REQ.INC".
      *
      *    ����ǥ��쥯�ȥ��������
           COPY    "CPORCSTEMPDIR.INC".
      *
      *    API��Ģɼ�ǡ����쥹�ݥ󥹺���
           COPY    "CPORCSAPIFM.INC".
      *
      *    �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
           COPY    "CPSK1010.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ��Ǽ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY     "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***************************"
           DISPLAY   "* invoice_receipt_h start *"
           DISPLAY   "***************************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  SPA-S01KYOUTU
           INITIALIZE                  PTINF-REC
           INITIALIZE                  SSUB03-AREA
           INITIALIZE                  SSUB06-AREA
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           PERFORM 200-MAIN-SEC
      *
      *    �ֵ��ΰ��Խ�
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************************"
           DISPLAY   "* invoice_receipt_h end   *"
           DISPLAY   "***************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    ���ŵ��ؼ����ֹ楻�å�
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
      *        �ӣУ���������
               INITIALIZE              SYS-1010-REC
               INITIALIZE              ORCSCOMMONAREA
               MOVE    "M00"       TO  ORCSCOMMON-PG
               CALL    "ORCSCOMMON"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
      *        IF    ( SPA-ERRCD   NOT =   SPACE )
      *            MOVE   "0089"   TO  SPA-ERRCD
      *        END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        XML������Ф�
               PERFORM 900-XML-READ-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-REQUEST-CHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 2001-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    �ꥯ�����ȹ��ܽ���
      *****************************************************************
       2001-REQUEST-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( IRHREQ-REQNUM    =   "01" OR "02" )
                   CONTINUE
               ELSE
                   MOVE    "0101"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               EVALUATE    IRHREQ-REQNUM
               WHEN    "01"
                   MOVE    FUNCTION UPPER-CASE(IRHREQ-PRINT-MODE)
                                       TO  IRHREQ-PRINT-MODE
                   EVALUATE    IRHREQ-PRINT-MODE
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "PDF"
                       INITIALIZE                  SRPLNK-AREA
                       MOVE    "CHK_MONBLOB"   TO  SRPLNK-MODE
                       CALL    "ORCSRPLNK"         USING
                                                   SRPLNK-AREA
                                                   SPA-AREA
                       IF    ( SRPLNK-RC       NOT =   ZERO )
                           MOVE    "0128"      TO  SPA-ERRCD
                       END-IF
                   WHEN    OTHER
                       MOVE    "0127"      TO  SPA-ERRCD
                   END-EVALUATE
      *
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( IRHREQ-PTNUM     =   SPACE )
                   MOVE    "0102"      TO  SPA-ERRCD
               ELSE
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    IRHREQ-PTNUM     TO  SPTID-PTNUM
                   CALL    "ORCSPTID"      USING
                                           ORCSPTIDAREA
                                           SPA-AREA
                   IF    ( SPTID-RC        NOT =   ZERO )
                       MOVE    "0103"      TO  SPA-ERRCD
                   END-IF
      *
                   MOVE    SPTID-PTNUM     TO  SPA-PTNUM
                   MOVE    SPTID-PTID      TO  SPA-PTID
      *
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF       NOT =   ZERO )
                   MOVE    "0103"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE                  SSUB06-AREA
               MOVE    CONST-MODE-REPRINT
                                       TO  SSUB06-REQMODE
               MOVE    "I"             TO  SSUB06-NYUGAIKBN
               MOVE    IRHREQ-DENPNUM   TO  SSUB06-DENPNUM
               MOVE    "1"             TO  SSUB06-SKYPRT
               CALL    "ORCGSSUB06"        USING
                                           SSUB06-AREA
                                           SPA-AREA
      D        DISPLAY "SPA-ERRCD[" SPA-ERRCD "]"
      *
      *        0033 ����ǯ������Ϥ˸��꤬����ޤ�
      *        0034 ������ʬ�����Ϥ˸��꤬����ޤ�
      * *      0085 �������¸�ߤ��ޤ���
      *        0086 ������ʬ�����Ϥ��Ƥ�������
      * *      0100 ��ɼ�ֹ�����Ϥ��Ƥ�������
      * *      0101 ��ɼ�ֹ�����Ϥ˸��꤬����ޤ�
      *        0116 ���پ���¸�ߤ��ޤ���
      *        0121 �������μ��������ʬ�����Ϥ˸��꤬����ޤ���
      *        0122 ���������ٽ������ʬ�����Ϥ˸��꤬����ޤ�
      *        0123 Ģɼ�ΰ����ؼ�������ޤ���
      *        0125 ���ֻ��곫���������Ϥ˸��꤬����ޤ�
      *        0126 ���ֻ��꽪λ�������Ϥ˸��꤬����ޤ�
      *        0127 ���ֻ��곫��������ֻ��꽪λ���ȤʤäƤ��ޤ�
      *        0128 �����оݤ��������¸�ߤ��ޤ���
      *        0129 �����������μ���ȯ����ˡ�����Ϥ˸��꤬����ޤ�
      *        0130 ����ǯ������Ϥ��Ƥ�������
      *        3005 �����ֹ�����Ϥ˸��꤬����ޤ�
      *
               EVALUATE    SPA-ERRCD
               WHEN    SPACE
                   CONTINUE
               WHEN    "0033"
                   MOVE    "0115"      TO  SPA-ERRCD
               WHEN    "0034"
                   MOVE    "0122"      TO  SPA-ERRCD
               WHEN    "0085"
                   MOVE    "0123"      TO  SPA-ERRCD
               WHEN    "0086"
                   MOVE    "0124"      TO  SPA-ERRCD
               WHEN    "0100"
                   MOVE    "0104"      TO  SPA-ERRCD
               WHEN    "0101"
                   MOVE    "0126"      TO  SPA-ERRCD
               WHEN    "0116"
                   MOVE    "0125"      TO  SPA-ERRCD
               WHEN    "3005"
                   MOVE    "0126"      TO  SPA-ERRCD
      *        WHEN    "0121"
      *        WHEN    "0122"
      *        WHEN    "0123"
      *        WHEN    "0125"
      *        WHEN    "0126"
      *        WHEN    "0127"
      *        WHEN    "0128"
      *        WHEN    "0129"
      *        WHEN    "0130"
               WHEN    OTHER
                   MOVE    "0123"      TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-REQUEST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �������ǡ�����������
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           PERFORM 20011-PRE-PRINT-SEC
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           INITIALIZE                      WRK-FILEMEI
           MOVE    SMCNDATE-YMD        TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS        TO  WRK-FILEMEI-HMS
      *
           INITIALIZE                      SPA-S01KYOUTU
           MOVE    SYU-DENPNUM         TO  SPA-S01-DENPNUM
           MOVE    "API"               TO  SPA-S01-GYOUMUID
           MOVE    SYU-SYUHKNNUM       TO  SPA-S01-HKNNUM
           MOVE    PTINF-KANANAME      TO  SPA-S01-KANANAME
           MOVE    PTINF-NAME          TO  SPA-S01-NAME
           MOVE    SPA-PTID            TO  SPA-S01-PTID
           MOVE    SPA-PTNUM           TO  SPA-S01-PTNUM
           MOVE    SPA-RSI-HKNKBN      TO  SPA-S01-RSI-HKNKBN
           MOVE    SYU-SRYYMD          TO  SPA-S01-SRYYMD
           PERFORM 800-ORCUIDSET-SEC
           MOVE    C-UID               TO  SPA-S01-UID
           MOVE    WRK-FILEMEI         TO  SPA-S01-FILEMEI
      *
           MOVE    "1"                 TO  SPA-S01-NYUGAIKBN
      *
           INITIALIZE                      SSUB03-AREA
           MOVE    3                   TO  SSUB03-REQNO
           MOVE    "1"                 TO  SSUB03-SKYPRT
           CALL    "ORCGSSUB03"        USING
                                       SPA-AREA
                                       SPA-S01KYOUTU
                                       SSUB03-AREA
      D     DISPLAY "SPA-ERRCD[" SPA-ERRCD "]"
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    �������ǡ�����������
      *****************************************************************
       20011-PRE-PRINT-SEC             SECTION.
      *
           IF    ( IRHREQ-REQNUM    =   "01" )
               MOVE    2           TO  SPA-API-FLG
               IF    ( IRHREQ-PRINT-MODE   =   "PDF" )
                   MOVE    "3"     TO  SPA-PRTCONF
               END-IF
           ELSE
               MOVE    3           TO  SPA-API-FLG
           END-IF
      *
           CALL    "ORCSONPRTSET"      USING
                                       SPA-AREA
      *
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING
                                       SPA-AREA
                                       ORCSPRTCTRLAREA
                                       MCPAREA
      *
           .
       20011-PRE-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD    =  "0099" )
               MOVE    404             TO  APILST46-HTTPSTATUS
           ELSE
               INITIALIZE                  SAPIFM-AREA
               MOVE    SPA-ERRCD       TO  SAPIFM-ERRCD
               CALL    "ORCSAPIFM"         USING
                                           SAPIFM-AREA
                                           SPA-AREA
               IF    ( SAPIFM-OBJECT   NOT =   LOW-VALUE )
                   MOVE    SAPIFM-OBJECT
                                       TO  APILST46-BODY
                   MOVE    "application/json"
                                       TO  APILST46-CONTENT-TYPE
               END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    UID��������
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    ���Ծ��󸡺�����
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =  ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *************************************************************
      *    ��Ǽ�ޥ�������������
      *************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SSUB06-DENPNUM      TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    XML���󤫤����Ƥ���Ф�
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST46-BODY    NOT =   LOW-VALUE )
               DISPLAY "invoice_receipt_h object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    "xml_invoice_receipt_hv2req"
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST46-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "invoice_receipt_hv2req"
                                           TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           DISPLAY "MCP-RC " MCP-RC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"            TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE            TO  APILST46-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML�ե��륿��󥰽���
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  IRHREQ-DATA-AREA
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FETCH�Ϥ��ʤ���
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �أͣ��ɹ�����
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.