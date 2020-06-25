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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW66.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ƥ������������
      *  ����ݡ��ͥ��̾  : �������ŵ��ؾ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/12/04    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ƥ�������������Ѷ��̥ѥ�᥿
           COPY    "W01COMMON-SPA".
      *
      *    ���󥹥������
           COPY    "ENUM-VALUE".
      *
      *    �����ѣӣУ�
       01  SPA-W66.
           03  SPA-GMN-CUR                         PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-KGIIRYOCD                  PIC X(20).
               05  SPA-GMN-KGISKYKBN-G.
                   07  SPA-GMN-KGISKYKBN             PIC X(02).
                   07  SPA-GMN-KGISKYKBNF            PIC X(01).
                   07  SPA-GMN-KGISKYKBNMEI          PIC X(20).
           03  SPA-GMN-COMBO-AREA.
               05  SPA-GMN-KGISKYKBNLST-G.
                   07  SPA-GMN-KGISKYKBN-MAX         PIC 9(03).
                   07  SPA-GMN-KGISKYKBNLST          OCCURS  20.
                       09  SPA-GMN-KGISKYKBNL        PIC X(02).
                       09  SPA-GMN-KGISKYKBNFL       PIC X(01).
                       09  SPA-GMN-KGISKYKBNMEIL     PIC X(20).
      *
      *    �ե饰���ꥢ
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *    ���Ϲ���̾
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-WIDMSG                          PIC X(200).
           03  WRK-KANACHK-INPUT                   PIC X(100).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *
      *    �������ŵ��ؾ�������
           COPY  "CPSK4100.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
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
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W66
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W66         TO  SPA-FREE
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
      *    ���顼���̤��
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W66    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC                  SECTION.
      *
      *    ¾�Σ̣Ĥ��
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               MOVE    SPACE           TO  SPA-WID1-FLG
               MOVE    SPACE           TO  SPA-WIDCD
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W66
      *
      *    ���̽��������
           PERFORM 420-CLEAR-SEC
      *
           PERFORM 3100-COMBO-EDIT-SEC
      *
      *    �������ŵ��ؾ�������
           PERFORM 3101-SYS4100-SEL-SEC
      *****IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYS-4100-KGIIRYOCD
                                   TO  SPA-GMN-KGIIRYOCD
               MOVE    SYS-4100-KGISKYKBN
                                   TO  WRK-CMB-CD
               PERFORM 4200-KGISKYKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-KGISKYKBN-G
      *****END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ܥܥå����Խ�����
      *****************************************************************
       3100-COMBO-EDIT-SEC             SECTION.
      *
           PERFORM 3101-KGISKYKBNLST-SEC
      *
           .
       3100-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ����������������ʬ������Խ�����
      *****************************************************************
       3101-KGISKYKBNLST-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-KGISKYKBNLST-G
      *
           MOVE    "00"            TO  SPA-GMN-KGISKYKBNL   (1)
           MOVE    "ɸ��"          TO  SPA-GMN-KGISKYKBNMEIL(1)
      *
           MOVE    "01"            TO  SPA-GMN-KGISKYKBNL   (2)
           MOVE    "̾�Ų���"      TO  SPA-GMN-KGISKYKBNMEIL(2)
      *
           MOVE    "02"            TO  SPA-GMN-KGISKYKBNL   (3)
           MOVE    "�̶彣��"      TO  SPA-GMN-KGISKYKBNMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-KGISKYKBN-MAX
      *
           .
       3100-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    �������ŵ��ؾ�������
      *****************************************************************
       3101-SYS4100-SEL-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "4100"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-4100-REC
           ELSE
               INITIALIZE                  SYS-4100-REC
           END-IF
      *
           IF      SYS-4100-KGISKYKBN  =   SPACE
               MOVE    "00"                TO  SYS-4100-KGISKYKBN
           END-IF
      *
           .
       3101-SYS4100-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��ǧ���̡ʣãɣģ��ˣϣ˽���
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "3001"
                   PERFORM 4801-TOUROKU-SEC
               WHEN "3002"
                   PERFORM 4301-DEL-SEC
               END-EVALUATE
           END-IF
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    ���
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    ���ꥢ
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 420-CLEAR-SEC
      *    ���
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 430-DEL-SEC
      *    ��Ͽ
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 480-TOUROKU-SEC
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    ���ϸĽ�Υ��å�
           PERFORM 400-GMN-INPUT-SEC
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ���ܥ����å�����
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W66-KGIIRYOCD      TO  SPA-GMN-KGIIRYOCD
           MOVE    W66-KGISKYKBN      TO  SPA-GMN-KGISKYKBN-G
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܥ����å�����
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           PERFORM 4101-KGIIRYOCD-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KGISKYKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �������ŵ��إ����ɥ����å�����
      *****************************************************************
       4101-KGIIRYOCD-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-KGIIRYOCD  =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-KGIIRYOCD   TO  WRK-KANACHK-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-KGIIRYOCD
               IF    ( FLG-ENTRYCHK        NOT =   1 )
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-KGIIRYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ����������������ʬ�����å�����
      *****************************************************************
       4101-KGISKYKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-KGISKYKBN-G =   SPACE )
               MOVE    SPA-GMN-KGISKYKBNLST (1)
                                           TO  SPA-GMN-KGISKYKBN-G
           ELSE
               MOVE    SPA-GMN-KGISKYKBN   TO  WRK-CMB-CD
               PERFORM 4200-KGISKYKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGISKYKBN-G
               END-IF
           END-IF
      *
           .
       4101-KGISKYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���������Ծ��ѹ�����ô�׻��оݶ�ʬ����ܥܥå��������å�����
      *****************************************************************
       4200-KGISKYKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGISKYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGISKYKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KGISKYKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGISKYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W66"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    "W01"               TO  MCP-WINDOW
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
      *    ��說�ꥢ����
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           INITIALIZE  SPA-GMN-AREA
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    �������
      *****************************************************************
       430-DEL-SEC                 SECTION.
      *
           MOVE    "3002"              TO  SPA-WIDCD
      *
           .
       430-DEL-EXT.
           EXIT.
      *****************************************************************
      *    �������
      *****************************************************************
       4301-DEL-SEC                SECTION.
      *
           INITIALIZE                      SYS-4100-REC
           MOVE    SPA-HOSPNUM         TO  SYS-4100-HOSPNUM
           MOVE    "4100"              TO  SYS-4100-KANRICD
           MOVE    "*"                 TO  SYS-4100-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-4100-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-4100-EDYUKYMD
           MOVE    SYS-4100-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "2003"          TO  SPA-ERRCD
               GO  TO  4301-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       4301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
      *    ���ϸĽ�Υ����å�
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       4801-TOUROKU-SEC                SECTION.
      *
           INITIALIZE                  SYS-4100-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-4100-HOSPNUM
           MOVE    "4100"              TO  SYS-4100-KANRICD
           MOVE    "*"                 TO  SYS-4100-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-4100-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-4100-EDYUKYMD
           MOVE    SPA-GMN-KGIIRYOCD   TO  SYS-4100-KGIIRYOCD
           MOVE    SPA-GMN-KGISKYKBN   TO  SYS-4100-KGISKYKBN
           MOVE    SYS-4100-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYS-4100-REC        TO  SYSKANRI-REC
               MOVE    SYS-4100-CREYMD     TO  SYS-CREYMD
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYS-4100-HOSPNUM    TO  SYSUP-HOSPNUM
               MOVE    SYS-4100-KANRICD    TO  SYSUP-KANRICD
               MOVE    SYS-4100-KBNCD      TO  SYSUP-KBNCD
               MOVE    SYS-4100-STYUKYMD   TO  SYSUP-STYUKYMD
               MOVE    SYS-4100-EDYUKYMD   TO  SYSUP-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "2001"      TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SYS-4100-REC        TO  SYSKANRI-REC
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                                               SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "2002"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    ���̴�Ϣ�����å�
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      **   IF    ( SPA-GMN-KGIIRYOCD   =   SPACE )
      **       MOVE    "1001"      TO  SPA-ERRCD
      **       MOVE    1           TO  SPA-GMN-CUR
      **   END-IF
      *
      **   IF    ( SPA-ERRCD           =   SPACE )
      **       IF    ( SPA-GMN-KGISKYKBN-G =   SPACE )
      **           MOVE    "1002"  TO  SPA-ERRCD
      **           MOVE    2       TO  SPA-GMN-CUR
      **       END-IF
      **   END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W66    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W66
           INITIALIZE                      W66
      *
           MOVE    SPA-W01-STYUKYMDH   TO  W66-YUKOSTYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  W66-YUKOEDYMD
      *
           MOVE    SPA-GMN-KGIIRYOCD   TO  W66-KGIIRYOCD
           MOVE    SPA-GMN-KGISKYKBN-G TO  W66-KGISKYKBN
      *
           MOVE    SPA-GMN-KGISKYKBN-MAX   TO  W66-KGISKYKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGISKYKBN-MAX )
                   MOVE    SPA-GMN-KGISKYKBNLST (IDX-CMB)
                                   TO  W66-KGISKYKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    ���ܾ����Խ�
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܾ����Խ�����
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-INSENSITIVE  TO  W66-B03-STATE
      *
           IF    ( SPA-W01-SELNUM      >   ZERO )
               MOVE    WIDGET-NORMAL   TO  W66-B03-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "KGIIRYOCD"         TO  MCP-WIDGET
           WHEN    002
               MOVE    "KGISKYKBN"         TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "KGIIRYOCD"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "KGISKYKBN"
               MOVE    212                 TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "�������ŵ��إ��������ϥ��顼"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "����������������ʬ���ϥ��顼"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "�������ŵ��إ����ɤ����Ϥ��Ƥ�������"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "����������������ʬ�����Ϥ��Ƥ�������"
                                       TO  SPA-ERRMSG
           WHEN    "2001"
           WHEN    "2002"
           WHEN    "2003"
               MOVE    "���������˼��Ԥ��ޤ���"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W66"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
           EVALUATE    SPA-WIDCD
           WHEN    "3001"
               MOVE    "��Ͽ���ޤ�����������Ǥ���"
                                        TO  WRK-WIDMSG
           WHEN    "3002"
               MOVE    "������ޤ�����������Ǥ���"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W66"                TO  SPA-MOTOPG
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
      *    ����Ⱦ�Ѻ��ߥ����å�
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ޥ������ռ�������
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ�����������(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģº������
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¹�������
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɲý���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
