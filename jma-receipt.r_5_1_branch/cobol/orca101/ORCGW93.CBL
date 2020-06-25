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
       PROGRAM-ID.                               ORCGW93.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ƥ������������
      *  ����ݡ��ͥ��̾  : ���ž����������ؾ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  17/12/18    NACL�ݿ���    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
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
       01  SPA-W93.
           03  SPA-GMN-CUR                     PIC 9(03).
           03  SPA-GMN-MODE                    PIC 9(01).
           03  SPA-GMN-AREA.
               05  SPA-GMN-RECEDEN-G.
                   07  SPA-GMN-RECEDEN         PIC X(01).
                   07  SPA-GMN-RECEDEN-F1      PIC X(01).
                   07  SPA-GMN-RECEDEN-MEI     PIC X(18).
               05  SPA-GMN-RECEDEN-LST-G.
                   07  SPA-GMN-RECEDEN-LST     PIC X(20)
                                               OCCURS   10.
               05  SPA-GMN-RECEDEN-MAX         PIC 9(02).
      *
               05  SPA-GMN-JMAMR-G.
                   07  SPA-GMN-JMAMR           PIC X(01).
                   07  SPA-GMN-JMAMR-F1        PIC X(01).
                   07  SPA-GMN-JMAMR-MEI       PIC X(18).
               05  SPA-GMN-JMAMR-LST-G.
                   07  SPA-GMN-JMAMR-LST       PIC X(20)
                                               OCCURS   10.
               05  SPA-GMN-JMAMR-MAX           PIC 9(02).
      *
      *    �ե饰���ꥢ
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-OK                          PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                             PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *    ���Ϲ���̾
           03  WRK-MCP-WIDGET                  PIC X(64).
           03  WRK-WIDMSG                      PIC X(100).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *
      *    ���ž����������ؾ���
           COPY  "CPSK9104.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
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
           MOVE    SPA-FREE        TO  SPA-W93
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
           MOVE    SPA-W93         TO  SPA-FREE
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
           MOVE    "W93    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
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
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W93
      *
      *    �Żҥ쥻�ץȥǡ���
           MOVE    "0 �������ʤ�"      TO  SPA-GMN-RECEDEN-LST(1)
           MOVE    "1 ��������"        TO  SPA-GMN-RECEDEN-LST(2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-MAX
      *    ������ž���ǡ���
           MOVE    "0 �������ʤ�"      TO  SPA-GMN-JMAMR-LST(1)
           MOVE    "1 ��������"        TO  SPA-GMN-JMAMR-LST(2)
           MOVE    2                   TO  SPA-GMN-JMAMR-MAX
      *
      *    �����ƥ��������
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-W01-KANRICD     TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9104-REC
      *        �Żҥ쥻�ץȥǡ���
               MOVE    SYS-9104-RECEDEN    TO  SPA-GMN-RECEDEN
      *        ������ž���ǡ���
               MOVE    SYS-9104-JMAMR      TO  SPA-GMN-JMAMR
           END-IF
      *    �Żҥ쥻�ץȥǡ������
           IF      SPA-GMN-RECEDEN     =   SPACE
               MOVE    "0"             TO  SPA-GMN-RECEDEN
           END-IF
           PERFORM 4102-RECEDEN-CHK-SEC
      *    ������ž���ǡ������
           IF      SPA-GMN-JMAMR       =   SPACE
               MOVE    "0"             TO  SPA-GMN-JMAMR
           END-IF
           PERFORM 4102-JMAMR-CHK-SEC
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    ��ǧ���̡ʣףɣģ��ˣϣ˽���
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
      *        ��Ͽ����
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4801-TOUROKU-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
               WHEN    "1003"
      *        �������
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4301-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    ���
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"        TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    ���ꥢ
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    ���
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 430-DELETE-SEC
      *    ��Ͽ
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
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
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    ���̤�ӣУ����å�
           MOVE    W93-RECEDEN     TO  SPA-GMN-RECEDEN-G
           MOVE    W93-JMAMR       TO  SPA-GMN-JMAMR-G
      *
      *    ���ܥ����å�����
           PERFORM 4102-RECEDEN-CHK-SEC
      *    ������ž���ǡ���
           PERFORM 4102-JMAMR-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �Żҥ쥻�ץȥǡ��������å�����
      *****************************************************************
       4102-RECEDEN-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-RECEDEN-MAX
               IF      SPA-GMN-RECEDEN =   SPA-GMN-RECEDEN-LST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-RECEDEN-LST (IDX)
                                           TO  SPA-GMN-RECEDEN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4102-RECEDEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ������ž���ǡ��������å�����
      *****************************************************************
       4102-JMAMR-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-JMAMR-MAX
               IF      SPA-GMN-JMAMR   =   SPA-GMN-JMAMR-LST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-JMAMR-LST (IDX)
                                           TO  SPA-GMN-JMAMR-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4102-JMAMR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"           TO  SPA-SAKIPG
           MOVE    "W93"           TO  SPA-MOTOPG
      *
           MOVE    "W01"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    ��說�ꥢ����
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 310-SPASET-SEC
           MOVE    SPACE           TO  SPA-ERRCD
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    �������
      *****************************************************************
       430-DELETE-SEC              SECTION.
      *
           MOVE    "1003"          TO  SPA-WIDCD
      *
           .
       430-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    �������
      *****************************************************************
       4301-DELETE-SYORI-SEC       SECTION.
      *
      *    �����ƥ��������
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-W01-KANRICD TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "9003"          TO  SPA-ERRCD
                   DISPLAY "W93 DEL ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4301-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
      *    ���ϸĽ�Υ����å�
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD       =   SPACE
               MOVE    "1001"          TO  SPA-WIDCD
           END-IF
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       4801-TOUROKU-SEC            SECTION.
      *
      *    �����ƥ��������
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-W01-KANRICD TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-9104-REC
           ELSE
               MOVE    SPACE           TO  SYS-9104-REC
               INITIALIZE                  SYS-9104-REC
               MOVE    "9104"          TO  SYS-9104-KANRICD
               MOVE    SPA-W01-KANRICD TO  SYS-9104-KANRICD
               MOVE    "*"             TO  SYS-9104-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-9104-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD
                                   TO  SYS-9104-EDYUKYMD
           MOVE    SPA-GMN-RECEDEN TO  SYS-9104-RECEDEN
           MOVE    SPA-GMN-JMAMR   TO  SYS-9104-JMAMR
      *
           MOVE    SYS-9104-REC    TO  SYSKANRI-REC
      *    �ģ¹�������
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      ******************************************************************
      *       ��Ͽ����
      *****************************************************************
       4910-DBKOUSIN-SEC           SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SPA-OPID        TO  SYS-OPID
               MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD     TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD       TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "9002"          TO  SPA-ERRCD
                   DISPLAY "W93 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
               MOVE    SPA-OPID        TO  SYS-OPID
               MOVE    SMCNDATE-YMD    TO  SYS-CREYMD
                                           SYS-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "9001"          TO  SPA-ERRCD
                   DISPLAY "W93 INS ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC
               END-IF
           END-IF
      *
           .
       4910-DBKOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
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
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "W93    "       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  W93
           INITIALIZE                  W93
      *
      *    �Żҥ쥻�ץȥǡ���������ʬ
           MOVE    SPA-GMN-RECEDEN-G   TO  W93-RECEDEN
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-RECEDEN-MAX
               MOVE    SPA-GMN-RECEDEN-LST(IDX)
                                       TO  W93-RECEDEN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-MAX TO  W93-RECEDEN-COUNT
      *
      *    ������ž���ǡ���������ʬ������ޤ���
           MOVE    SPA-GMN-JMAMR-G TO  W93-JMAMR
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-JMAMR-MAX
               MOVE    SPA-GMN-JMAMR-LST(IDX)
                                       TO  W93-JMAMR-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-JMAMR-MAX   TO  W93-JMAMR-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD       =   SPACE)  AND
                  (SPA-GMN-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "RECEDEN"       TO  MCP-WIDGET
               WHEN    02
                   MOVE    "JMAMR"         TO  MCP-WIDGET
               WHEN    90
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "RECEDEN"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "JMAMR"
                   MOVE    90              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�Żҥ쥻�ץȥǡ���������ʬ������ޤ���"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "������ž���ǡ���������ʬ������ޤ���"
                                           TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "��Ͽ�����˼��Ԥ��ޤ���"
                                           TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "���������˼��Ԥ��ޤ���"
                                           TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "��������˼��Ԥ��ޤ���"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W93"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    �����ɥ�å�����ɽ��
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "��Ͽ���ޤ�����������Ǥ���"
                                            TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "������ޤ�����������Ǥ���"
                                            TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE           TO  WID1
           INITIALIZE                  WID1
           MOVE    SPA-WIDCD       TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG      TO  WID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "W93"           TO  SPA-MOTOPG
           MOVE    "WID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *****************************************************************
      *    �ޥ������ռ�������
      *****************************************************************
       800-MCNDATE-SEC             SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ������ɹ��ʥ�����
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.