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
       PROGRAM-ID.                 ORCGXA03.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ư���
      *  ����ݡ��ͥ��̾  : ������ؼ�����
      *  ������            : 
      *  19/05/28    NACL��ƣ��   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
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
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �ư����������̥ѥ�᥿
           COPY    "XA02COMMON-SPA".
      *
           COPY    "XA02SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-DAY             PIC 9(05).
      *    ���Ϲ���̾
           03  WRK-MCP-WIDGET      PIC X(64).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *
      *    �����ǡ����������ǽ����
           COPY  "CPSK8000.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   �����Ѵ�����
      *     COPY    "CPORCSGDAY.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA01SCRAREA.INC".
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
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XA02FREE
           MOVE    SPA-WORK        TO  SPA-XA02KYOUTU
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
           END-EVALUATE
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-XA02KYOUTU  TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XA02FREE    TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "XAERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XA03"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE                      SPA-XA03-AREA
      *
           MOVE    SPACE               TO  SYS-8000-REC
           INITIALIZE                      SYS-8000-REC
           MOVE    "8000"              TO  SYS-8000-KANRICD
           MOVE    "*"                 TO  SYS-8000-KBNCD
           MOVE    SPA-NAI-SMCNDATE-YMD
                                       TO  SYS-8000-STYUKYMD
                                           SYS-8000-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-8000-HOSPNUM
           MOVE    SYS-8000-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-8000-REC
           ELSE
               MOVE    1                   TO  SYS-8000-DAILY
           END-IF
      *
           COMPUTE WRK-DAY =   SYS-8000-DAILY  *   30
           IF     WRK-DAY              >   99
               MOVE    99                  TO  SPA-GMN-XA03-DAY
           ELSE
               MOVE    WRK-DAY             TO  SPA-GMN-XA03-DAY
           END-IF
      *
           MOVE    1               TO  SPA-GMN-XA03-CUR
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE           TO  SPA-XAID1-FLG
                   PERFORM 210-BACK
      *     ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ý���
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���ʬ
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ʬ���Ͻ���
      *****************************************************************
       400-ENTRY-SYORI-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-XA03-CUR
      *
      *    ���ϥ����å�����
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *    ���ܥ����å�����
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        �����������å�����
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
           .
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̤�ӣУ����åȽ���
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    XA03-DAY            TO  SPA-GMN-XA03-DAY
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܥ����å�����
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41021-DAY-CHK-SEC
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ϥ����å�����
      *****************************************************************
       41021-DAY-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-XA03-DAY    =   0
               MOVE    1                   TO  SPA-GMN-XA03-DAY
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-SMCNDATE-YMD
                                       TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           COMPUTE LNK-DAY6-ZOGEN  =   SPA-GMN-XA03-DAY    *   (-1)
                                                           +   1
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-XA03-CREYMD
      *??
           DISPLAY "CREYMD=" SPA-NAI-XA03-CREYMD "#"
                   SPA-NAI-SMCNDATE-YMD "#" SPA-GMN-XA03-DAY "#"
      *??
           .
       41021-DAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������å�����
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      * 
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "XA03"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
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
      *    �������
      *****************************************************************
       450-KAKUTEI-SEC                SECTION.
      *
           MOVE    1                  TO  FLG-TOUROKU
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  450-KAKUTEI-EXT
           END-IF
      *
           MOVE    "OK"            TO  SPA-XAID1-FLG
      *
           PERFORM 210-BACK
           .
       450-KAKUTEI-EXT.
           EXIT.
      *
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "XA03"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  XA03
      *
           MOVE    SPA-GMN-XA03-DAY    TO  XA03-DAY
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-GMN-XA03-CUR    =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-XA03-CUR
               WHEN    1
                   MOVE  "DAY"         TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       50011-INPUT-CUR-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "DAY"
                   MOVE    99              TO  SPA-GMN-XA03-CUR
               WHEN    OTHER
                   MOVE    99              TO  SPA-GMN-XA03-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
     *****************************************************************
      *    ���顼��å�����ɽ��
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�����������ֹ�Ϥ���ޤ���" 
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XAERR
           MOVE    SPA-ERRCD            TO  XAERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  XAERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "XA02"                TO  SPA-MOTOPG
           MOVE    "XAERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
           MOVE    "XAERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɹ�
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
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
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
     *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �УգԽ���
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *