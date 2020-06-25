      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
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
       PROGRAM-ID.             ORCGW08.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ƥ������������
      *  ����ݡ��ͥ��̾  : �쥻�ץȺ����ؼ��ʥ쥻�ץȼ��̡˾���ʣף�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��ƣ����   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-ƣ��     01/09/11  ��������ν��� 
      * 01.01.01     MCC-ƣ��     01/09/21  JOIN�ν���      
      * 01.01.02     NACL-¿��Ǽ  02/11/13  ������� 
      * 03.05.00     NACL-¿��Ǽ  07/05/02  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ƥ�������������Ѷ��̥ѥ�᥿
           COPY    "W01COMMON-SPA".
      *
      *    �����ѣӣУ�
       01  SPA-W08.
           03  SPA-W08-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).
                   07  SPA-GMN-SYUBETU        PIC X(04).
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-SYUBETUNAME
                                              PIC X(40).
                       09  SPA-GMN-PRTJYUNI   PIC X(02).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD           PIC 9(08).
                   07  SPA-NAI-EDYUKYMD           PIC 9(08).
      *   
           COPY    "ENUM-VALUE".
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    ��������ΰ�
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    ź���ΰ�
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-PATH            PIC X(64).
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *    �쥻�ץȺ����ؼ��ʥ쥻�ץȼ��̡˾���
           COPY  "CPSK2004.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ���ե��֥롼�����ΰ�
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W08
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W08         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W08"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    ¾�Σ̣Ĥ��
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣãɣģ��ˣϣ˽���
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               IF      SPA-WIDCD            =   "1002"
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W08-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-W01-KBNCD       TO  SPA-GMN-SYUBETU
      *
           INITIALIZE                         SPA-GMN-KOUMOKU
      * 
           MOVE    SPACE               TO  SYS-2004-REC
           INITIALIZE                      SYS-2004-REC
           MOVE    "2004"                  TO  SYS-2004-KANRICD
           MOVE    SPA-W01-KBNCD           TO  SYS-2004-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2004-STYUKYMD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2004-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-2004-HOSPNUM
           MOVE    SYS-2004-REC            TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"          TO  WRK-PATH
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2004-REC
               MOVE    SYS-2004-SYUBETUNAME    TO  SPA-GMN-SYUBETUNAME
               MOVE    SYS-2004-PRTJYUNI       TO  SPA-GMN-PRTJYUNI
           END-IF     
      *
           MOVE    2                   TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        ���
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SYUBETUNAME"
                   PERFORM 4100-SYUBETUNAME-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����̾�Υ����å�
      *****************************************************************
       4100-SYUBETUNAME-CHK-SEC       SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    W08-SYUBETUNAME     TO  SPA-GMN-SYUBETUNAME
      *
           IF      SPA-GMN-SYUBETUNAME      =   SPACE
               MOVE    "0001"                      TO  SPA-ERRCD
               GO  TO  4100-SYUBETUNAME-CHK-EXT
           END-IF 
      *  
           MOVE    99                  TO  SPA-GMN-CUR
           .
       4100-SYUBETUNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W08"               TO  SPA-MOTOPG
      *
      *    INITIALIZE                  SPA-W01KYOUTU
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ǧ����ɽ������
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-2004-REC
           INITIALIZE                      SYS-2004-REC
           MOVE    "2004"                  TO  SYS-2004-KANRICD
           MOVE    SPA-W01-KBNCD           TO  SYS-2004-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2004-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-2004-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-2004-HOSPNUM
           MOVE    SYS-2004-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-2004-KEY 
               GO  TO  4031-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       ��Ͽ������
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
              MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       ��Ͽ����
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-2004-REC
           INITIALIZE                      SYS-2004-REC
           MOVE    "2004"                  TO  SYS-2004-KANRICD
           MOVE    SPA-W01-KBNCD           TO  SYS-2004-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2004-STYUKYMD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2004-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-2004-HOSPNUM
           MOVE    SYS-2004-REC            TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"          TO  WRK-PATH
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2004-REC
           ELSE
               MOVE    SPACE                   TO  SYS-2004-REC
               INITIALIZE                      SYS-2004-REC
               MOVE   "2004"                   TO  SYS-2004-KANRICD
               MOVE    SPA-W01-KBNCD           TO  SYS-2004-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD      TO  SYS-2004-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD      TO  SYS-2004-EDYUKYMD
           MOVE    SPA-GMN-SYUBETUNAME   TO  SYS-2004-SYUBETUNAME 
           MOVE    SPA-GMN-PRTJYUNI      TO  SYS-2004-PRTJYUNI
           MOVE    SYS-2004-REC          TO  SYSKANRI-REC
      *     
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W08 DB UPD ERR KEY:" SYS-KEY 
               END-IF
           ELSE
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W08 DB UPD ERR KEY:" SYS-KEY 
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK 
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       ���Ϲ��ܤδ�Ϣ�����å�����
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
           MOVE    SPA-GMN-CUR          TO  WRK-GMN-CUR 
      *
           PERFORM 4100-SYUBETUNAME-CHK-SEC
      *
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-GMN-CUR          TO  SPA-GMN-CUR
           END-IF             
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W08    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
      *
           MOVE    SPA-GMN-STYUKYMD        TO  W08-YUKOSTYMD
           MOVE    SPA-GMN-EDYUKYMD        TO  W08-YUKOEDYMD
           MOVE    SPA-GMN-SYUBETU         TO  W08-SYUBETU
      * 
           MOVE    SPA-GMN-SYUBETUNAME     TO  W08-SYUBETUNAME
           MOVE    SPA-GMN-PRTJYUNI        TO  W08-PRTJYUNI
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ��̳�������������
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE      TO  W08-B03-STATE
      *
           IF      SPA-W01-SELNUM          >   ZERO    
               MOVE    WIDGET-NORMAL      TO  W08-B03-STATE
           END-IF 
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���ϥ��顼"        TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "̤���Ϥι��ܤ�����ޤ�"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "�������Ǥ��ޤ���Ǥ���"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "������Ǥ��ޤ���Ǥ���"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W08"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ɥ�å�����ɽ��
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "�쥻�ץȼ��̾������Ͽ���ޤ�"
                                        TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "�쥻�ץȼ��̾���������ޤ�" 
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W08"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    2
                    MOVE  "SYUBETUNAME"         TO   MCP-WIDGET
                WHEN    99
                    MOVE  "B12"            TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ե����å����Խ�����
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " " BY  "0"
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Ѵ��Խ�����
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ��ʥ�����
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *