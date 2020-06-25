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
       PROGRAM-ID.                               ORCGI041.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������̳
      *  ����ݡ��ͥ��̾  : ������������ɥ���������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/08/27    NACL��¿��Ǽ  ��������
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
       DATA                                      DIVISION.
      *FILE                                      SECTION.
      *
      *
       WORKING-STORAGE                           SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���ౡ��Ͽ�����ΰ�
           COPY    "I0COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "I0SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-TOROKU             PIC 9(01).
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-D                   PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-MEI           PIC X(20).
           03  WRK-DRCD                PIC X(04).
      *    ���Ϲ���̾
           03  WRK-MCP-WIDGET              PIC X(64).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    �ɥ�����
           COPY    "CPSK1010.INC".
      *    ���Ųʥ�����
           COPY    "CPSK1005.INC".
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *     
           COPY    "CPORCMCP.INC".  
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                                  SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA31SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
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
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "I0ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "I041"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           IF      SPA-NAI-I041-KBN    =   ZERO
      *        �������
               PERFORM 310-SPA-SET-SEC
           END-IF
           MOVE    1                   TO  SPA-NAI-I041-KBN
           MOVE    1                   TO  SPA-GMN-I041-CUR
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   5   )
                         OR   (SPA-NAI-I041-SRYKA (IDX-D)  =   SPACE )
               MOVE    SPA-NAI-I041-SRYKA (IDX-D)
                                           TO  WRK-SRYKA
               PERFORM 3101-SRYKA-MEI-SEC
               MOVE    WRK-SRYKA-MEI       TO  SPA-GMN-I041-SRYKA-MEI
                                                             (IDX-D)
               MOVE    IDX-D               TO  SPA-GMN-I041-MAX
               IF      SPA-NAI-I041-DRCD (IDX-D)   =   SPACE
                   MOVE    SPA-GMN-I04-DR-CD
                                           TO  SPA-NAI-I041-DRCD (IDX-D)
               END-IF
           END-PERFORM
           INITIALIZE                  SPA-GMN-I041-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-I041-MAX )
               MOVE    SPA-NAI-I041-DRCD (IDX)
                                       TO  SPA-GMN-I041-DRCD (IDX)
               MOVE    SPA-GMN-I041-DRCD (IDX) TO  WRK-DRCD
               PERFORM 41021-DRCD-CHK-SEC
           END-PERFORM
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ���Ų�̾�Խ�����
      *****************************************************************
       3101-SRYKA-MEI-SEC                           SECTION.
      *
      *    ������
           INITIALIZE                      SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-I01-I04-KJNYMD  TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           MOVE    SYS-1005-SRYKANAME  TO  WRK-SRYKA-MEI
      *
           .
        3101-SRYKA-MEI-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                               SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-TOUROKU-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    ���ϸĽ�Υ��å�
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-I041-CUR
      *
           MOVE    ZERO                TO  FLG-TOROKU
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ���ܥ����å����̲��̽���
           PERFORM 4102-KIHON-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-I041-MAX
               MOVE    I041-DRCD (IDX)     TO  SPA-GMN-I041-DRCD-G (IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-I041-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE    )
               MOVE    SPA-GMN-I041-DRCD (IDX) TO  WRK-DRCD
               PERFORM 41021-DRCD-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    IDX             TO  SPA-GMN-I041-CUR
               END-IF
           END-PERFORM
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ɥ����������ɥ����å�����
      *****************************************************************
       41021-DRCD-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-I041-DRCD-MEI (IDX)
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-GMN-I04-DR-CNT )
                           OR (FLG-OK  =   1                  )
               IF      WRK-DRCD        =   SPA-GMN-I04-DRL (IDX-D)
                   MOVE    SPA-GMN-I04-DR-ITM (IDX-D)
                                           TO  SPA-GMN-I041-DRCD-G
                                                            (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        �����ƥ��������
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1010"          TO  SYS-KANRICD
               MOVE    "1"             TO  SYS-KBNCD (1:1)
               MOVE    WRK-DRCD        TO  SYS-KBNCD (2:4)
               MOVE    SPA-I01-I04-SKYSTYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    SYSKANRI-REC    TO  SYS-1010-REC
                   MOVE    SYS-1010-NAME   TO  SPA-GMN-I041-DRCD-MEI
                                                                (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-IF
           IF      FLG-OK              =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       41021-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       220-TOUROKU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ���ܥ����å����̲��̽���
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *        ��Ͽ����
               PERFORM 2201-TOUROKU-SYORI-SEC
           END-IF
           .
       220-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       2201-TOUROKU-SYORI-SEC            SECTION.
     *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-I041-MAX )
               MOVE    SPA-GMN-I041-DRCD (IDX) TO  SPA-NAI-I041-DRCD
                                                               (IDX)
           END-PERFORM
           MOVE    1                   TO  SPA-GMN-I041-FLG
      *    ���
           PERFORM 210-BACK
           .
       2201-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                                  SECTION.
      *
           MOVE    "I04"               TO  SPA-SAKIPG
           MOVE    "I041"              TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
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
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I041"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  I041
           INITIALIZE                      I041
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-I041-MAX )
               MOVE    SPA-GMN-I041-SRYKA-MEI (IDX)
                                           TO  I041-SRYKA (IDX)
               MOVE    SPA-GMN-I041-DRCD-G (IDX)
                                           TO  I041-DRCD  (IDX)
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL      (IDX-D   >   SPA-GMN-I04-DR-CNT )
                   MOVE    SPA-GMN-I04-DR-ITM (IDX-D)
                                           TO  I041-DRCD-ITEM
                                                            (IDX IDX-D)
                   MOVE    IDX-D           TO  I041-DRCD-COUNT (IDX)
               END-PERFORM
           END-PERFORM
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
      *    �����������Τʤ��������Ϥ������ܤμ�����
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I041-CUR    =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-I041-CUR
               WHEN    01
                   MOVE    "DRCD01"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "DRCD02"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "DRCD03"        TO  MCP-WIDGET
               WHEN    04
                   MOVE    "DRCD04"        TO  MCP-WIDGET
               WHEN    05
                   MOVE    "DRCD05"        TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "DRCD01"
                   MOVE    02              TO  SPA-GMN-I041-CUR
               WHEN    "DRCD02"
                   MOVE    03              TO  SPA-GMN-I041-CUR
               WHEN    "DRCD03"
                   MOVE    04              TO  SPA-GMN-I041-CUR
               WHEN    "DRCD04"
                   MOVE    05              TO  SPA-GMN-I041-CUR
               WHEN    "DRCD05"
                   MOVE    99              TO  SPA-GMN-I041-CUR
           END-EVALUATE
           IF      SPA-GMN-I041-CUR    >   SPA-GMN-I041-MAX
               MOVE    99              TO  SPA-GMN-I041-CUR
           END-IF
           .
       50011-INPUT-CUR-EXT.
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
               MOVE    "�ɥ����������ɥ��顼"    TO  SPA-ERRMSG
           WHEN    OTHER
               MOVE    SPA-ERRCD           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I041"              TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I0ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ������������(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC    =     ZERO )
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               ELSE
                   INITIALIZE              SYSKANRI-REC
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA.
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *