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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGU02X.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ 
      *  ����ݡ��ͥ��̾  : �ạ̃��ɣ���³����ʣգ����ء�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/11/20    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-¿��Ǽ  07/05/07  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ���ե����ΰ�
           COPY    "U02COMMON-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-MCP-WIDGET          PIC X(64).
           03  WRK-GMN-SELNUM          PIC 9(02).
      *
      *    ����������
       01  WRK-SOUSIN-AREA.
           03  FILLER              PIC X(02)   VALUE   "1 ".
           03  FILLER              PIC X(18)   VALUE   "�ţգ�".
      *
           03  FILLER              PIC X(02)   VALUE   "2 ".
           03  FILLER              PIC X(18)   VALUE   "�ӡݣ���".
      *
           03  FILLER              PIC X(02)   VALUE   "3 ".
           03  FILLER              PIC X(18)   VALUE   "�գԣơݣ�".
      *
       01  WRK-COMBO-TBL           REDEFINES   WRK-SOUSIN-AREA.
           03  WRK-COMBO-TBLO          OCCURS   3.
               05  WRK-COMBO-SCODEL        PIC X(01).
               05  WRK-COMBO-FIL           PIC X(01).
               05  WRK-COMBO-SCODEMEIL     PIC X(18).
      *
       01  WRK-COMBO-SCODE-MAX         PIC 9(03)   VALUE   3.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *�����ʣạ̃��ɣ���³��
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
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
           COPY    "U02.INC".
           COPY    "U02Q.INC".
           COPY    "U02X.INC".
           COPY    "U2ERR.INC".
           COPY    "U2ID1.INC".
           COPY    "CPAPIMODV2.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-U02
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
           END-EVALUATE
      *
      *    �����Խ��塢ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
      *
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-U02         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "U2ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5011-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "U02X"              TO  MCP-WINDOW.
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
       300-SCREEN-SEC              SECTION.
      *
      *    �ᥤ��ǥ��ꥢ
      *****INITIALIZE                  SPA-U02X-AREA
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-9001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9001-EDYUKYMD
      *    �����ƥ��������
           MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   20   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-9001-REC
               MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-GMN-SELNUM
               MOVE    WRK-GMN-SELNUM        TO  SPA-U02X-GMN-SELNOL
                                                                 (IDX)
               MOVE    SYS-9001-ADDRESS      TO  SPA-U02X-GMN-ADRL
                                                                 (IDX)
               MOVE    SYS-9001-PORT1        TO  SPA-U02X-GMN-PORTL
                                                                  (IDX)
               MOVE    SYS-9001-MEMO         TO  SPA-U02X-GMN-MEMOL
                                                                  (IDX)
               MOVE    IDX                   TO  SPA-U02X-GMN-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    �������륯������
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ����������
           MOVE    SPA-U02X-CODE       TO  SPA-GMN-SCODE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-COMBO-SCODE-MAX
               IF      SPA-GMN-SCODE   =   WRK-COMBO-SCODEL (IDX)
                   MOVE    WRK-COMBO-TBLO (IDX)    TO  SPA-GMN-SCODE-G
               END-IF
           END-PERFORM
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     ���
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE           TO  SPA-U02X-FLG
                   PERFORM 210-BACK
      *     ��Ͽ�Τߡ��������ʤ���
               WHEN    "CLICKED"       ALSO    "B11"
                   MOVE    "NO"            TO  SPA-U02X-FLG
                   PERFORM 400-TOUROKU-SEC
      *     ��Ͽ������
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    "OK"            TO  SPA-U02X-FLG
                   PERFORM 400-TOUROKU-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-U02X-CUR
      *
           MOVE    SPACE               TO  SPA-U02X-FLG
      *    ���ϥ����å�����
           PERFORM 210-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       210-INPUT-CHK-SEC       SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ������
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 203-SELECT-SEC
           END-EVALUATE
      *
      *    ���̤�ӣУ����å�
           PERFORM 2101-GMN-SPA-SET-SEC
      *    ���ܥ����å�����
           PERFORM 2102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      210-INPUT-CHK-EXT
           END-IF
      *
           .
       210-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̤�ӣУ����åȽ���
      *****************************************************************
       2101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    U02X-SCODE          TO  SPA-GMN-SCODE-G
           MOVE    U02X-SELNUM         TO  SPA-U02X-GMN-SEL
      *
           .
       2101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܥ����å�����
      *****************************************************************
       2102-KIHON-CHK-SEC       SECTION.
      *
      *    ������������(����)
           PERFORM 201-SCODE-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      2102-KIHON-CHK-EXT
           END-IF
      *    �ֹ���������
           PERFORM 202-SELNUM-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      2102-KIHON-CHK-EXT
           END-IF
      *
           .
       2102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ�����å�����
      *****************************************************************
       201-SCODE-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-COMBO-SCODE-MAX
               IF      SPA-GMN-SCODE-G(1:1)
                                       =   WRK-COMBO-SCODEL (IDX)
                   MOVE    WRK-COMBO-TBLO (IDX) 
                                           TO  SPA-GMN-SCODE-G
                   MOVE    WRK-COMBO-SCODE-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    1                   TO  SPA-U02X-CUR
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *
           .
       201-SCODE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ�����å�����
      *****************************************************************
       202-SELNUM-SEC               SECTION.
      *
           IF      SPA-U02X-FLG        =   "OK"
               IF      SPA-U02X-GMN-SEL    =   ZERO
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-U02X-CUR
                   GO      TO              202-SELNUM-EXT
               END-IF
           END-IF
      *
      *    �����ֹ�����å�
           IF      SPA-U02X-GMN-SEL    >   SPA-U02X-GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-U02X-CUR
               GO      TO              202-SELNUM-EXT
           END-IF
      *
           .
       202-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������ϡ�����
      *****************************************************************
       203-SELECT-SEC               SECTION.
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-U02X-GMN-MAX
               IF      U02X-LIST-SELECT(IDX)        =  "T"
                   MOVE    SPA-U02X-GMN-SELNOL (IDX)
                                           TO  SPA-U02X-GMN-SEL
               END-IF
           END-PERFORM
      *
           MOVE    SPA-U02X-GMN-SEL    TO  U02X-SELNUM
      *
           MOVE    5                   TO  SPA-U02X-CUR
           .
       203-SELNO-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       400-TOUROKU-SEC              SECTION.
      *
      *    ���ϥ����å�����
           PERFORM 210-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      400-TOUROKU-EXT
           END-IF
      *
           PERFORM 210-BACK
      *
           .
       400-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 502-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "U02X"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  U02X
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX        >   WRK-COMBO-SCODE-MAX)
               MOVE    WRK-COMBO-TBLO (IDX)
                                       TO  U02X-SCODE-ITEM (IDX)
           END-PERFORM
           MOVE    WRK-COMBO-SCODE-MAX TO  U02X-SCODE-COUNT
           MOVE    SPA-GMN-SCODE-G     TO  U02X-SCODE
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-U02X-GMN-MAX
               MOVE    SPA-U02X-GMN-SELNOL (IDX)
                                            TO  U02X-NO (IDX)
               MOVE    SPA-U02X-GMN-ADRL (IDX)
                                            TO  U02X-ADR (IDX)
               MOVE    SPA-U02X-GMN-PORTL (IDX)
                                            TO  U02X-PORT (IDX)
               MOVE    SPA-U02X-GMN-MEMOL (IDX)
                                            TO  U02X-MEMO (IDX)
               IF      SPA-U02X-GMN-SEL     =   IDX
                   MOVE    "T"              TO  U02X-LIST-SELECT (IDX)
               ELSE
                   MOVE    "F"              TO  U02X-LIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-U02X-GMN-MAX    TO  U02X-LIST-COUNT
      *
           MOVE    SPA-U02X-GMN-SEL    TO  U02X-SELNUM 
      *
           PERFORM 5011-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5011-MAPCUR-SEC             SECTION.
      *
           IF      SPA-U02X-CUR        =   ZERO
               PERFORM 50111-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-U02X-CUR
               WHEN    01
                   MOVE    "SCODE"         TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "B01"           TO  MCP-WIDGET
               WHEN    04
                   MOVE    "B11"           TO  MCP-WIDGET
               WHEN    05
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
           .
       5011-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       50111-CUR-SET-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SCODE"
                   MOVE    2               TO  SPA-U02X-CUR
               WHEN    "SELNUM"
                   MOVE    5               TO  SPA-U02X-CUR
               WHEN    OTHER
                   MOVE    2               TO  SPA-U02X-CUR
           END-EVALUATE
           .
       50111-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       502-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "0001"
                    MOVE    "�������������ϥ��顼"
                                                TO  SPA-ERRMSG
                WHEN    "0002"
                    MOVE    "�����ֹ椬ͭ��ޤ���"
                                                TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE               TO  SPA-ERRMSG
            END-EVALUATE
      *
           MOVE    SPACE               TO  U2ERR
           MOVE    SPA-ERRCD           TO  U2ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  U2ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "U02X"              TO  SPA-MOTOPG
           MOVE    "U2ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "U2ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       502-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "U02"               TO  SPA-SAKIPG
           MOVE    "U02X"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
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
      *    �����ƥ�����ޥ������ңţ���
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *