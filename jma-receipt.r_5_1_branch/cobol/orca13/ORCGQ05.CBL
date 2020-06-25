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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQ05.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ȳ�
      *  ����ݡ��ͥ��̾  : �������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  12/08/02    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "Q01SCR-SPA".
      *
      *    ���󥹥������
           COPY    "ENUM-VALUE".
      *
      *    �ե饰���ꥢ
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SHOUKAIKANRI            PIC 9(01).
           03  FLG-JKNLST-ROW              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDXL                        PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-WIDGET                  PIC X(64).
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-SELNUM                  PIC S9(03).
           03  WRK-VALUE                   PIC S9(03).
      *
       01  WRK-ROW.
           05  WRK-JKNLST-ROW              PIC S9(9)   BINARY.
      *
      *    �������ΰ�
       01  CONST-AREA.
           03  CONST-JKNLST-MAX            PIC 9(05)   VALUE   200.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ԾȲ񸡺�����������
       01  SHOUKAIKANRI-REC.
           COPY    "CPSHOUKAIKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *    �桼�����������
           COPY    "CPORCSLDUSER.INC".
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
           COPY    "ORCA13SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
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
           IF    ( SPA-MOTOPG          =   "QERR" )
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
           MOVE    "Q05"               TO  MCP-WINDOW.
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
       300-SCREEN-SEC              SECTION.
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
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-Q01-UID
      *
           INITIALIZE                      SPA-Q05-AREA
      *
           MOVE    1                   TO  FLG-JKNLST-ROW
      *
           PERFORM 900-SHOUKAIKANRI-KEY2-SEL-SEC
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   CONST-JKNLST-MAX )
                    OR   ( FLG-SHOUKAIKANRI    NOT =   ZERO )
      *
               MOVE    IDXL            TO  SPA-GMN-Q05-JKNLST-MAX
                                           SPA-GMN-Q05-TNUM (IDXL)
      *
               MOVE    SHOKANRI-NAME   TO  SPA-GMN-Q05-TNAME (IDXL)
               MOVE    SHOKANRI-UID    TO  SPA-GMN-Q05-TUID (IDXL)
               MOVE    SHOKANRI-TITLEKBN
                                       TO  SPA-GMN-Q05-TTITLEKBN (IDXL)
      *
               PERFORM 900-SHOUKAIKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    1                   TO  SPA-GMN-Q05-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET              TO  WRK-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    ���
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM  210-BACK
           WHEN    "CLICKED"       ALSO    "B05"
           WHEN    "CLICKED"       ALSO    "B05R"
      *
           WHEN    "CLICKED"       ALSO    "B06"
           WHEN    "CLICKED"       ALSO    "B06R"
      *
           WHEN    "CLICKED"       ALSO    "B07"
           WHEN    "CLICKED"       ALSO    "B07R"
      *
           WHEN    "CLICKED"       ALSO    "B08"
           WHEN    "CLICKED"       ALSO    "B08R"
      *    �¤��ؤ�
               PERFORM  480-TOUROKU-SEC
      *
           WHEN    "CLICKED"       ALSO    "B12"
      *    ����
               MOVE    SPA-GMN-Q05-TUID (SPA-GMN-Q05-JKNLST-SEL)
                                   TO      SPA-Q01-UID
               PERFORM 210-BACK
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
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    ANY             ALSO    "LIST"
               PERFORM 4100-JKNLST-CHK-SEC
      *
      *    WHEN    "XXXX"
      *        ���ϸĽ�Υ��å�
      *        PERFORM 400-GMN-INPUT-SEC
      *        ���ϥ����å�����
      *        PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-Q05-CUR
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
           PERFORM 4101-SELNUM-CHK-SEC
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ�����å�����
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    Q05-SELNUM         TO  SPA-GMN-Q05-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ģɼ�ꥹ�ȥ����å�����
      *****************************************************************
       4100-JKNLST-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-Q05-SELNUM
                                       SPA-GMN-Q05-JKNLST-SEL
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-Q05-JKNLST-MAX )
      *
               IF    ( Q05-LIST-SELECT (IDX0) =   "T" )
                   MOVE    SPA-GMN-Q05-TNUM (IDX0)
                                   TO  SPA-GMN-Q05-SELNUM
                   MOVE    IDX0    TO  SPA-GMN-Q05-JKNLST-SEL
                   MOVE    1       TO  SPA-GMN-Q05-CUR
      *
                   MOVE    SPA-GMN-Q05-JKNLST-MAX
                                   TO  IDX0
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4100-JKNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ�����å�����
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-Q05-CUR
           MOVE    ZERO                TO  SPA-GMN-Q05-JKNLST-SEL
      *
           IF    ( SPA-GMN-Q05-SELNUM  NOT =   ZERO )
               MOVE    ZERO                TO  IDXL
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-Q05-JKNLST-MAX )
                        OR   ( IDXL    >   ZERO )
      *
                   IF    ( SPA-GMN-Q05-SELNUM
                                       =   SPA-GMN-Q05-TNUM (IDX0))
                       MOVE    IDX0        TO  IDXL
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXL                >   ZERO )
                   MOVE    IDXL            TO  SPA-GMN-Q05-JKNLST-SEL
               ELSE
                   MOVE    1               TO  SPA-GMN-Q05-CUR
                   MOVE    "0001"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q05"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN  "             TO  MCP-PUTTYPE
           MOVE    "Q01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
      *    PERFORM 800-LDUSER-CHK-SEC
      *    IF    ( SLDUSER-OTHERS  =   ZERO )
      *        CONTINUE
      *    ELSE
      *        MOVE    "0002"      TO  SPA-ERRCD
      *    END-IF
      *
           IF    ( SPA-ERRCD    =  SPACE )
               PERFORM 4801-TOUROKU-SEC
           END-IF
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       4801-TOUROKU-SEC                SECTION.
      *
           MOVE    SPA-GMN-Q05-JKNLST-SEL
                                   TO  WRK-SELNUM
      *
           MOVE    ZERO            TO  WRK-VALUE
      *
           EVALUATE    MCP-WIDGET
           WHEN    "B05"
           WHEN    "B05R"
               COMPUTE WRK-VALUE   = -10
           WHEN    "B06"
           WHEN    "B06R"
               COMPUTE WRK-VALUE   = -1
           WHEN    "B07"
           WHEN    "B07R"
               COMPUTE WRK-VALUE   =  1
           WHEN    "B08"
           WHEN    "B08R"
               COMPUTE WRK-VALUE   = 10
           END-EVALUATE
      *
           COMPUTE WRK-SELNUM = WRK-SELNUM   + WRK-VALUE
      *
           IF    ( WRK-SELNUM      < 1 )
               COMPUTE WRK-SELNUM  = 1
           END-IF
      *
           IF    ( WRK-SELNUM      > SPA-GMN-Q05-JKNLST-MAX )
               COMPUTE WRK-SELNUM  = SPA-GMN-Q05-JKNLST-MAX
           END-IF
      *
           IF    ( SPA-GMN-Q05-JKNLST-SEL
                               NOT =  WRK-SELNUM )
               PERFORM 48011-TOUROKU-SEC
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ����
      *****************************************************************
       48011-TOUROKU-SEC                SECTION.
      *
           IF    ( SPA-GMN-Q05-JKNLST-SEL
                                       >   WRK-SELNUM )
               MOVE    WRK-SELNUM  TO  SHOKANRI-RENNUM-FIRST
               MOVE    SPA-GMN-Q05-JKNLST-SEL
                                   TO  SHOKANRI-RENNUM-LAST
               MOVE    1           TO  SHOKANRI-RENNUM
           ELSE
               MOVE    SPA-GMN-Q05-JKNLST-SEL
                                   TO  SHOKANRI-RENNUM-FIRST
               MOVE    WRK-SELNUM  TO  SHOKANRI-RENNUM-LAST
               MOVE    -1          TO  SHOKANRI-RENNUM
           END-IF
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "upd3"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-SHOUKAIKANRI-KEY-SEL-SEC
           MOVE    WRK-SELNUM          TO  SHOKANRI-RENNUM
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4002"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 310-SPASET-SEC
               MOVE    WRK-SELNUM      TO  SPA-GMN-Q05-JKNLST-SEL
                                           SPA-GMN-Q05-SELNUM
                                           Q05-LIST-ROW
               MOVE    2               TO  FLG-JKNLST-ROW
           END-IF
      *
           .
       48011-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    ���̴�Ϣ�����å�
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    Q05-LIST-ROW        TO  WRK-JKNLST-ROW
      *
           INITIALIZE                      Q05
      *
           EVALUATE    FLG-JKNLST-ROW
           WHEN    1
               MOVE    ZERO            TO  Q05-LIST-ROW
               MOVE    ZERO            TO  Q05-LIST-ROWATTR
           WHEN    2
               MOVE    WRK-JKNLST-ROW
                                       TO  Q05-LIST-ROW
               MOVE    2               TO  Q05-LIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-JKNLST-ROW
                                       TO  Q05-LIST-ROW
               MOVE    ZERO            TO  Q05-LIST-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-Q05-SELNUM  TO  Q05-SELNUM
      *
           MOVE    SPA-GMN-Q05-JKNLST-MAX
                                       TO  Q05-LIST-COUNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-Q05-JKNLST-MAX )
      *
               MOVE    SPA-GMN-Q05-TNUM(IDX0)
                                       TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  Q05-LIST-TNUM (IDX0)
      *
               MOVE    SPA-GMN-Q05-TNAME (IDX0)
                                       TO  Q05-LIST-TNAME (IDX0)
      *
               IF    ( IDX0        =   SPA-GMN-Q05-JKNLST-SEL )
                   MOVE    "T"         TO  Q05-LIST-SELECT (IDX0)
               ELSE
                   MOVE    "F"         TO  Q05-LIST-SELECT (IDX0)
               END-IF
      *
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
            MOVE    WIDGET-INSENSITIVE     TO  Q05-B05-STATE
                                               Q05-B06-STATE
                                               Q05-B07-STATE
                                               Q05-B08-STATE
                                               Q05-B05R-STATE
                                               Q05-B06R-STATE
                                               Q05-B07R-STATE
                                               Q05-B08R-STATE
                                               Q05-B12-STATE
      *
            IF    ( SPA-GMN-Q05-JKNLST-SEL      >   ZERO )
               IF    ( SPA-GMN-Q05-TNAME (SPA-GMN-Q05-JKNLST-SEL) 
                                           NOT =   SPACE )
                    MOVE    WIDGET-NORMAL  TO  Q05-B05-STATE
                                               Q05-B06-STATE
                                               Q05-B07-STATE
                                               Q05-B08-STATE
                                               Q05-B05R-STATE
                                               Q05-B06R-STATE
                                               Q05-B07R-STATE
                                               Q05-B08R-STATE
                                               Q05-B12-STATE
               END-IF
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
                  (SPA-GMN-Q05-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-Q05-CUR
           WHEN    001
               MOVE    "SELNUM"        TO  MCP-WIDGET
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
      *
           EVALUATE    WRK-WIDGET
           WHEN    "SELNUM"
               MOVE    001             TO  SPA-GMN-Q05-CUR
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
               MOVE    "�����ֹ�����Ϥ˸��꤬����ޤ�"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               STRING  "¾ü���ǾȲ��̳������ΰ١�"
                                               DELIMITED BY  SIZE
                       "�¹ԤǤ��ޤ���"
                                               DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "4001" THRU "4002"
               MOVE    "���������˼��Ԥ��ޤ���"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  QERR
           MOVE    SPA-ERRCD           TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  QERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "Q05"              TO  SPA-MOTOPG
           MOVE    "QERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "QERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
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
      *    �̣ĥ桼���������å�����
      *****************************************************************
       800-LDUSER-CHK-SEC              SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    13              TO  SLDUSER-MY-LD
           MOVE    1               TO  SLDUSER-KBN
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       800-LDUSER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���ԾȲ񸡺����������󸡺�����
      *****************************************************************
       900-SHOUKAIKANRI-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           INITIALIZE                      SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    SPA-GMN-Q05-TUID (SPA-GMN-Q05-JKNLST-SEL)
                                       TO  SHOKANRI-UID
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       900-SHOUKAIKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ���ԾȲ񸡺����������󸡺�����
      *****************************************************************
       900-SHOUKAIKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           INITIALIZE                      SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    "0"                 TO  SHOKANRI-NYUGAIKBN
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
           .
      *
       900-SHOUKAIKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ���ԾȲ񸡺�����������FETCH����
      *****************************************************************
       900-SHOUKAIKANRI-KEY2-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
           .
      *
       900-SHOUKAIKANRI-KEY2-FET-EXT.
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
       910-DBFETCH-SEC                  SECTION.
      *
           MOVE    "DBFETCH"       TO   MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£գУģ��ԣŽ���
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
