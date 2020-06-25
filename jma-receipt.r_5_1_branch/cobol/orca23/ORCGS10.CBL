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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGS10.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��Ǽ
      *  ����ݡ��ͥ��̾  : ���԰�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07.07.20    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
           COPY    "S01COMMON-SPA".
      *
       01  SPA-S10.
           03  SPA-GMN-CUR                     PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-SELNUM              PIC 9(03).
               05  SPA-GMN-LBLSRYYM            PIC X(20).
           03  SPA-GMN-PTLIST-AREA.
               05  SPA-GMN-PTLIST-MAX          PIC 9(05).
               05  SPA-GMN-PTLIST-SEL          PIC 9(05).
               05  SPA-GMN-PTLIST-OCC          OCCURS  101.
                   07  SPA-GMN-PTLIST-SRT-KEY.
                       09  SPA-GMN-TPTNUM      PIC X(20).
                   07  SPA-GMN-TPTID           PIC 9(10).
                   07  SPA-GMN-TNAME           PIC X(50).
                   07  SPA-GMN-TSEX            PIC X(2).
                   07  SPA-GMN-TBIRHDAY        PIC X(10).
                   07  SPA-GMN-TAGE            PIC X(10).
                   07  SPA-GMN-TKBNMEI         PIC X(20).
                   07  SPA-GMN-TSECMONEY       PIC 9(07).
                   07  SPA-GMN-TKANANAME       PIC X(50).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ROW                     PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTLIST-ROW              PIC 9(01).
           03  FLG-ARI                     PIC 9(01).
           03  FLG-MAX                     PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDXL                        PIC 9(05).
           03  IDXS                        PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-EDTYMD1                 PIC X(09).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-AGE-G.
               05  WRK-AGE                 PIC ZZ9.
               05  WRK-LBLAGE              PIC X(04).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-ZZZZ9                   PIC ZZZZ9.
      *
       01  WRK-ROW.
           03  WRK-PTLIST-ROW              PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-PTLIST-MAX            PIC 9(5) VALUE 100.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��Ǽ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    �����ֹ�
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ݸ�������ǯ�𡦳��׻�����
           COPY    "CPORCSAGECHK.INC".
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     �ơ��֥륢������
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    ��    ��    ��
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-S10
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD.
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
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-S01KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA         TO  SPA-COMMON
           MOVE    SPA-S10          TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    ���顼���̤��
           IF    ( SPA-MOTOPG          =   "SERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
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
           MOVE    "S10"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
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
           INITIALIZE                      SPA-S10
      *
           MOVE    SPA-S01-TUKI-SRYYM  TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3(1:16)   TO  SPA-GMN-LBLSRYYM
      *
           MOVE    1                   TO  FLG-PTLIST-ROW
      *
           PERFORM 3101-SPASET-HENSYU-SEC
      *
           COMPUTE IDXS    =   CONST-PTLIST-MAX +  1
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-PTLIST-MAX - 1 )
      *
               COMPUTE WRK-STIDX   =   IDX0    +   1
      *
               PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX1    >   SPA-GMN-PTLIST-MAX )
                   IF    ( SPA-GMN-PTLIST-SRT-KEY (IDX0)
                                   >   SPA-GMN-PTLIST-SRT-KEY (IDX1))
                       MOVE    SPA-GMN-PTLIST-OCC (IDX0)
                                   TO  SPA-GMN-PTLIST-OCC (IDXS)
                       MOVE    SPA-GMN-PTLIST-OCC (IDX1)
                                   TO  SPA-GMN-PTLIST-OCC (IDX0)
                       MOVE    SPA-GMN-PTLIST-OCC (IDXS)
                                   TO  SPA-GMN-PTLIST-OCC (IDX1)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       3101-SPASET-HENSYU-SEC              SECTION.
      *
           MOVE   ZERO                         TO  SPA-GMN-PTLIST-MAX
           MOVE   ZERO                         TO  FLG-MAX
      *
           INITIALIZE                              SYUNOU-REC
           MOVE   SPA-HOSPNUM                  TO  SYU-HOSPNUM
           MOVE   "2"                          TO  SYU-NYUGAIKBN
           MOVE   SPA-S01-TUKI-SRYYM           TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   "tbl_syunou"                 TO  MCP-TABLE
           MOVE   "key97"                      TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_syunou"            TO  MCP-TABLE
             MOVE      "key97"                 TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL (FLG-SYUNOU  =  1)
                         OR   (FLG-MAX     =  1)
               IF  SYU-DENPJTIKBN  NOT = "3"
                 MOVE     ZERO     TO  FLG-ARI
                 PERFORM  VARYING  IDXL  FROM  1  BY  1
                           UNTIL  (IDXL      >  CONST-PTLIST-MAX)
                            OR    (SPA-GMN-TPTID(IDXL)  =  ZERO )
                            OR    (FLG-ARI              =   1   )
                   IF  SYU-PTID  =  SPA-GMN-TPTID(IDXL)
                     MOVE    1     TO  FLG-ARI
                     PERFORM  31012-SPASET-HENSYU3-SEC
                   END-IF
                 END-PERFORM
                 IF    FLG-ARI   =  ZERO
                   IF  IDXL  <=  CONST-PTLIST-MAX
                     MOVE    IDXL  TO  SPA-GMN-PTLIST-MAX
                     PERFORM  31011-SPASET-HENSYU2-SEC
                     PERFORM  31012-SPASET-HENSYU3-SEC
                   ELSE
                     MOVE    1     TO  FLG-MAX
                   END-IF
                 END-IF
               END-IF
               MOVE    "tbl_syunou"            TO  MCP-TABLE
               MOVE    "key97"                 TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE        "tbl_syunou"            TO  MCP-TABLE
           MOVE        "key97"                 TO  MCP-PATHNAME
           PERFORM      910-DBCLOSECURSOR-SEC
      *
           .
       3101-SPASET-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       31011-SPASET-HENSYU2-SEC              SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           PERFORM 900-PTNUM-KEY-SEL-SEC
           PERFORM 800-ORCSAGECHK-SEC
      *
           MOVE    PTNUM-PTNUM     TO  SPA-GMN-TPTNUM   (IDXL)
           MOVE    SYU-PTID        TO  SPA-GMN-TPTID    (IDXL)
           MOVE    PTINF-NAME      TO  SPA-GMN-TNAME    (IDXL)
           MOVE    PTINF-KANANAME  TO  SPA-GMN-TKANANAME(IDXL)
      *
           EVALUATE    PTINF-SEX
             WHEN    "1"
               MOVE    "��"        TO  SPA-GMN-TSEX     (IDXL)
             WHEN    "2"
               MOVE    "��"        TO  SPA-GMN-TSEX     (IDXL)
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1     TO  SPA-GMN-TBIRHDAY (IDXL)
      *
           IF    ( AGECHK-NENREI2-YY   =   ZERO )
               MOVE    AGECHK-NENREI2-MM
                                   TO  WRK-AGE
               MOVE    "����"      TO  WRK-LBLAGE
               MOVE    WRK-AGE-G   TO  SPA-GMN-TAGE     (IDXL)
           ELSE
               MOVE    AGECHK-NENREI2-YY
                                   TO  WRK-AGE
               MOVE    "��"        TO  WRK-LBLAGE
               MOVE    WRK-AGE-G   TO  SPA-GMN-TAGE     (IDXL)
           END-IF
      *
           .
       31011-SPASET-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       31012-SPASET-HENSYU3-SEC              SECTION.
      *
           IF    SYU-SECMONEY  =  4 OR 6
               MOVE    "�軰�Թ԰ٸ�ʪ����"
                               TO  SPA-GMN-TKBNMEI(IDXL)
               MOVE     1      TO  SPA-GMN-TSECMONEY(IDXL)
           END-IF
           IF    SYU-SECMONEY  =  1 OR 3
             IF  SPA-GMN-TSECMONEY(IDXL)  NOT =  1
               MOVE    "������̤����ʪ����"
                               TO  SPA-GMN-TKBNMEI(IDXL)
               MOVE     2      TO  SPA-GMN-TSECMONEY(IDXL)
             END-IF
           END-IF
           IF    SYU-SECMONEY  =  2
             IF  SPA-GMN-TSECMONEY(IDXL)  NOT =  1 AND 2
               MOVE    "����Ը�ʪ����"
                               TO  SPA-GMN-TKBNMEI(IDXL)
               MOVE     3      TO  SPA-GMN-TSECMONEY(IDXL)
             END-IF
           END-IF
           IF    SYU-SECMONEY  =  5
             IF  SPA-GMN-TSECMONEY(IDXL)  NOT =  1 AND 2 AND 3
               MOVE    "�ޥ�Ĺ�׻�"
                               TO  SPA-GMN-TKBNMEI(IDXL)
               MOVE     4      TO  SPA-GMN-TSECMONEY(IDXL)
             END-IF
           END-IF
           IF    SYU-ACCT-UPDKBN  NOT =  SPACE
             IF  SPA-GMN-TSECMONEY(IDXL)  NOT =  1 AND 2 AND 3 AND 4
               MOVE    "��Ǽ����̤����"
                               TO  SPA-GMN-TKBNMEI(IDXL)
               MOVE     5      TO  SPA-GMN-TSECMONEY(IDXL)
             END-IF
           END-IF
           IF    SYU-SECMONEY  =  7
             IF  SPA-GMN-TSECMONEY(IDXL)  NOT =  1 AND 2 AND 3 AND 4
                                                               AND 5
               MOVE    "�����ԥ����å����ʬ"
                               TO  SPA-GMN-TKBNMEI(IDXL)
               MOVE     6      TO  SPA-GMN-TSECMONEY(IDXL)
             END-IF
           END-IF
      *
           .
       31012-SPASET-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO        TO      SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    SPACE           TO   SPA-S10-PTNUM
               PERFORM 210-BACK
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-CUR
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    �����ֹ�
           WHEN    ANY                 ALSO    "PTLIST"
               PERFORM 4100-PTLIST-CHK-SEC
           WHEN    "ACTIVATE"          ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    OTHER
      *        ���ϸĽ�Υ��å�
               PERFORM 400-GMN-INPUT-SEC
      *        ���ϥ����å�����
               PERFORM 410-INPUT-CHK-SEC
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
           IF    ( SPA-ERRCD   =   SPACE )
             AND ( FLG-END     =   ZERO  )
               PERFORM 4102-KIHON-CHK-SEC
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
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���԰����������
      *****************************************************************
       4100-PTLIST-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-PTLIST-SEL
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   SPA-GMN-PTLIST-MAX )
                    OR   ( SPA-GMN-PTLIST-SEL
                                   NOT =   ZERO )
      *
               IF    ( S10-PTLIST-SELECT (IDXL) =   "T" )
                   MOVE    IDXL    TO  SPA-GMN-PTLIST-SEL
                                       S10-SELNUM
                   PERFORM 4100-SELNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       4100-PTLIST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ�����å�����
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    S10-SELNUM      TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM  >   SPA-GMN-PTLIST-MAX )
               MOVE    "0001"      TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-SELNUM
                                   TO  SPA-GMN-PTLIST-SEL
               MOVE    SPA-GMN-TPTNUM (SPA-GMN-SELNUM)
                                   TO  SPA-S10-PTNUM
               PERFORM 210-BACK
           END-IF
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "S02"               TO  SPA-SAKIPG
           MOVE    "S10"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
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
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "S10    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    S10-PTLIST-ROW      TO  WRK-PTLIST-ROW
      *
           MOVE    SPACE           TO  S10
           INITIALIZE                  S10
      *
           EVALUATE    FLG-PTLIST-ROW
           WHEN    1
               MOVE    ZERO            TO  S10-PTLIST-ROW
               MOVE    ZERO            TO  S10-PTLIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-PTLIST-ROW  TO  S10-PTLIST-ROW
               MOVE    ZERO            TO  S10-PTLIST-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-LBLSRYYM    TO  S10-LBLSRYYM
           MOVE    SPA-GMN-SELNUM      TO  S10-SELNUM
           MOVE    SPA-GMN-PTLIST-MAX  TO  S10-PTLIST-COUNT
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   SPA-GMN-PTLIST-MAX )
      *
               MOVE    IDXL            TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  S10-TNUM (IDXL)
               MOVE    SPA-GMN-TPTNUM (IDXL)
                                       TO  S10-TPTNUM (IDXL)
               MOVE    SPA-GMN-TNAME  (IDXL)
                                       TO  S10-TNAME (IDXL)
               MOVE    SPA-GMN-TSEX(IDXL)
                                       TO  S10-TSEX(IDXL)
               MOVE    SPA-GMN-TBIRHDAY(IDXL)
                                       TO  S10-TBIRHDAY(IDXL)
               MOVE    SPA-GMN-TAGE(IDXL)
                                       TO  S10-TAGE(IDXL)
               MOVE    SPA-GMN-TKBNMEI(IDXL)
                                       TO  S10-TKBNMEI(IDXL)
               IF    ( SPA-GMN-PTLIST-SEL  =   IDXL )
                   MOVE    "T"         TO  S10-PTLIST-SELECT (IDXL)
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-PTLIST-MAX  =   ZERO )
               MOVE    "�����Ԥʤ�"    TO  S10-LBLMSG01
           END-IF
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
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "SELNUM"        TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    001             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "�����ֹ�����������Ϥ��Ƥ�������"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  SERR
           MOVE    SPA-ERRCD            TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  SERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "S10"                TO  SPA-MOTOPG
           MOVE    "SERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ǯ�𻻽н���
      *****************************************************************
       800-ORCSAGECHK-SEC              SECTION.
      *
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SYU-HOSPNUM     TO  AGECHK-HOSPNUM
           MOVE    SYU-PTID        TO  AGECHK-PTID
           MOVE    SPA-SYSYMD      TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF    ( AGECHK-RC       NOT =   ZERO )
               DISPLAY "AGECHK-ERR !!! "
                       "PTNUM["    PTNUM-PTNUM "]"
                       "PTID["     AGECHK-PTID "]"
                       "SRYYMD["   AGECHK-SRYYMD "]"
                       "BIRTHDAY[" AGECHK-BIRTHDAY "]"
           END-IF
      *
           .
       800-ORCSAGECHK-EXT.
           EXIT.
      *****************************************************************
      *    �������ܸ��Ѵ�����
      *****************************************************************
       800-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ�ǡ����ɹ�
      *****************************************************************
       900-SYUNOU-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
      *
       900-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ���Ծ��󸡺�����
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SYU-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SYU-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               INITIALIZE              PTINF-REC
               MOVE    1           TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ渡������
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SYU-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SYU-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               INITIALIZE              PTNUM-REC
               MOVE    1           TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������(FETCH�ϹԤ�ʤ���
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
      *    �ģ��ɲý���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
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
