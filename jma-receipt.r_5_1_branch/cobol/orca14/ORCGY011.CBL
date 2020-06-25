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
       PROGRAM-ID.             ORCGY011.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ͽ����Ͽ
      *  ����ݡ��ͥ��̾  : ͽ����������Ͽ�ʣ٣�������
      *  ������            : 
      *  09/01/16    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ���̿���
           COPY    "ENUM-VALUE".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ͽ���ѥ�������ΰ�
           COPY    "Y01COMMON-SPA".
      *
      *    ͽ����ΰ�
           COPY    "Y01SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-YYKEXAMPLE      PIC 9(01).
           03  FLG-YYKCOM          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-MOJI                PIC X(200).
           03  WRK-COMMENT             PIC X(100).
           03  WRK-Z9                  PIC ZZ.
           03  WRK-YID1MSG             PIC X(80).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *
      *    ͽ��ʸ������ޥ�����
       01  YYKEXAMPLE-REC.
           COPY    "CPYYKEXAMPLE.INC".
      *
      *    ͽ����ޥ�����
       01  YYKCOM-REC.
           COPY    "CPYYKCOM.INC".
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
           COPY  "CPORCSGDAY.INC".
      *
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
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
           COPY    "Y01.INC".
           COPY    "Y011.INC".
           COPY    "Y012.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "Y05.INC".
           COPY    "YERR.INC".
           COPY    "YID1.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Y01
           MOVE    SPA-WORK        TO  SPA-Y01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 210-GMNSENI
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
           MOVE    SPA-Y01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Y01         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "YERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "Y011"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    �������̤��
           EVALUATE    SPA-MOTOPG
               WHEN    "Y012"
      *            ʸ��������̤��
                   PERFORM 3002-Y012-SYORI-SEC
               WHEN    "YID1"
                   PERFORM 3001-YID1-SYORI-SEC
               WHEN    OTHER
                   PERFORM 3001-INIT-SYORI-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
        3001-INIT-SYORI-SEC              SECTION.
      *
           INITIALIZE              SPA-Y011-AREA
           PERFORM 310-SPASET-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >       6  )
               IF      SPA-Y011-COMMENT (IDX)  =   SPACE
                   MOVE    IDX             TO  SPA-Y011-GMN-CUR
                   MOVE    6               TO  IDX
               END-IF
           END-PERFORM
      *
           .
        3001-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-Y011-COMM-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >      6  )
               MOVE    SPA-NAI-COMMENT (IDX)
                                       TO  SPA-Y011-COMMENT (IDX)
           END-PERFORM
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    ʸ��������̤��
      *****************************************************************
       3002-Y012-SYORI-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >       6  )
               IF      SPA-Y011-COMMENT (IDX)  =   SPACE
                   MOVE    IDX             TO  SPA-Y011-GMN-CUR
                   MOVE    6               TO  IDX
               END-IF
           END-PERFORM
           .
       3002-Y012-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ��ǧ�������ܽ���
      *****************************************************************
       3001-YID1-SYORI-SEC                  SECTION.
      *
           EVALUATE    SPA-YID1CD
               WHEN    "1001"
      *            �����
                   IF      SPA-YID1-FLG        =   "OK"
                       PERFORM 4210-COMMENT-DEL-SEC
                   ELSE
                       MOVE    1               TO  SPA-Y011-GMN-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-YID1CD
           MOVE    SPACE               TO  SPA-YID1-FLG
           .
       3001-YID1-SYORI-SEC-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       210-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    ���ꥢ
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *    ���
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 420-YYKDEL-SEC
      *    ʸ����ɽ��ʣ��
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-COMCOPY-SEC
      *    ʸ������
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4100-Y012-SET-SEC
      *    �����Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4120-TOROKU-SEC
           END-EVALUATE
      *
           .
       210-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-Y011-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
      *        ����ֹ�����
                   PERFORM 41011-SELNUM-SEC
      *        ʸ���ֹ�����
               WHEN    "ACTIVATE"      ALSO    "BUNNUM"
                   PERFORM 41012-BUNNUM-SEC
      *        �������ʬ
               WHEN    OTHER
      *           ���ϥ����å�����
                  MOVE    ZERO                TO  FLG-TOUROKU
                  PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
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
           MOVE    Y011-COMMENT01      TO  SPA-Y011-COMMENT (1)
           MOVE    Y011-COMMENT02      TO  SPA-Y011-COMMENT (2)
           MOVE    Y011-COMMENT03      TO  SPA-Y011-COMMENT (3)
           MOVE    Y011-COMMENT04      TO  SPA-Y011-COMMENT (4)
           MOVE    Y011-COMMENT05      TO  SPA-Y011-COMMENT (5)
           MOVE    Y011-COMMENT06      TO  SPA-Y011-COMMENT (6)
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
      *    �������
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41011-COMMENT-CHK-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ������ƥ����å�����
      *****************************************************************
       41011-COMMENT-CHK-SEC           SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >      6  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-Y011-COMMENT (IDX)  NOT =   SPACE
                   MOVE    SPA-Y011-COMMENT (IDX)  TO  WRK-MOJI
                   PERFORM 410111-KANJICHK-SEC
                   MOVE    WRK-MOJI        TO  SPA-Y011-COMMENT (IDX)
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    IDX             TO  SPA-Y011-GMN-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        ����Ԥ���
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX             >=  6  )
                   IF      SPA-Y011-COMMENT (IDX)  =   SPACE
                       MOVE    SPA-Y011-COMMENT (IDX + 1)
                                            TO  SPA-Y011-COMMENT (IDX)
                       MOVE    SPACE        TO  SPA-Y011-COMMENT
                                                             (IDX + 1)
                   END-IF
               END-PERFORM
           END-IF
           .
       41011-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����Ⱦ�ѥ����å�����
      *****************************************************************
       410111-KANJICHK-SEC            SECTION.
      *
      *    ����Ⱦ�ѥ����å�
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  ) 
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MOJI
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-MOJI
           END-IF
      *
           .
       410111-KANJICHK-EXT.
           EXIT.
      *****************************************************************
      *    ����ֹ�����å�����
      *****************************************************************
       41011-SELNUM-SEC            SECTION.
      *
           MOVE    Y011-SELNUM         TO  SPA-Y011-SELNUM
      *
           IF      SPA-Y011-SELNUM     >   ZERO
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX             >   6  )
                             OR   (FLG-OK          =   1  )
                   IF      IDX              =   SPA-Y011-SELNUM
                       IF      SPA-Y011-COMMENT (IDX)  NOT =   SPACE
                           MOVE    1            TO  FLG-OK
                       END-IF
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-Y011-GMN-CUR
               END-IF
           END-IF
           .
       41011-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ɽ����ʬ�����å�����
      *****************************************************************
       41012-BUNNUM-SEC            SECTION.
      *
           MOVE    Y011-BUNNUM         TO  SPA-Y011-BUNNUM
      *    ʸ���ֹ�¸�ߥ����å�
           PERFORM 410121-BUNNUM-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
      *        ʸ��ʣ��
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX             >   6  )
                   IF      SPA-Y011-COMMENT (IDX)  =   SPACE
                       MOVE    WRK-COMMENT  TO  SPA-Y011-COMMENT (IDX)
                       MOVE    6            TO  IDX
                   END-IF
               END-PERFORM
               MOVE    ZERO                TO  SPA-Y011-BUNNUM
           END-IF
           .
       41012-BUNNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ɽ����ʬ�����å�����
      *****************************************************************
       410121-BUNNUM-CHK-SEC            SECTION.
      *
           INITIALIZE                  YYKEXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  YYKEXAMPLE-HOSPNUM
           MOVE    SPA-Y011-BUNNUM     TO  YYKEXAMPLE-RENNUM
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
           ELSE
               INITIALIZE                  YYKEXAMPLE-REC
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
           IF      FLG-YYKEXAMPLE      =   ZERO
               MOVE    YYKEXAMPLE-COMMENT  TO  WRK-COMMENT
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-Y011-GMN-CUR
           END-IF
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       410121-BUNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ꥢ������
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
           IF     (SPA-Y011-SELNUM     =   ZERO )
           AND    (SPA-Y011-BUNNUM     =   ZERO )
               PERFORM 310-SPASET-SEC
           ELSE
               MOVE    ZERO                TO  SPA-Y011-SELNUM
               MOVE    ZERO                TO  SPA-Y011-BUNNUM
           END-IF
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ͽ������
      *****************************************************************
       4120-TOROKU-SEC           SECTION.
      *
      *    ���ϥ����å�����
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE              SPA-NAI-COMMENT-G
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX             >      6  )
                   MOVE    SPA-Y011-COMMENT (IDX)
                                           TO  SPA-NAI-COMMENT (IDX)
               END-PERFORM
      *
               PERFORM 210-BACK
           END-IF
      *
           .
       490-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    ��⥯�ꥢ����
      *****************************************************************
       420-YYKDEL-SEC                  SECTION.
      *
           IF     (SPA-Y011-SELNUM     >   ZERO  )  AND
                  (SPA-Y011-SELNUM     <=  6     )
               MOVE    SPACE           TO  SPA-Y011-COMMENT
                                                (SPA-Y011-SELNUM)
               MOVE    ZERO            TO  SPA-Y011-SELNUM
      *        ������
               PERFORM 41011-COMMENT-CHK-SEC
           ELSE
               MOVE    "1001"          TO  SPA-YID1CD
      *        MOVE    "0002"          TO  SPA-ERRCD
      *        MOVE    7               TO  SPA-Y011-GMN-CUR
           END-IF
           .
       420-YYKDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ƥ��ꥢ����
      *****************************************************************
       4210-COMMENT-DEL-SEC      SECTION.
      *
           MOVE    SPACE               TO  SPA-Y011-COMM-G
           MOVE    1                   TO  SPA-Y011-GMN-CUR
           .
       4210-COMMENT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ʸ����ɽ��ʣ�̽���
      *****************************************************************
       480-COMCOPY-SEC      SECTION.
      *
      *    �ɲó��Ϲ�
           MOVE    ZERO                TO  IDX-STR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   6  )
               IF      SPA-Y011-COMMENT (IDX)  =   SPACE
                   MOVE    IDX                 TO  IDX-STR
                   MOVE    6                   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      IDX-STR             >   ZERO
               PERFORM 4801-YYKEXAMPLE-COPY-SEC
           END-IF
           .
       480-COMCOPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʸ����ʣ�̽���
      *****************************************************************
       4801-YYKEXAMPLE-COPY-SEC           SECTION.
      *
           INITIALIZE                  YYKEXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  YYKEXAMPLE-HOSPNUM
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
           ELSE
               INITIALIZE                  YYKEXAMPLE-REC
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
           MOVE    IDX-STR             TO  IDX
           PERFORM
                   UNTIL      (IDX     >=  6           )  OR
                              (FLG-YYKEXAMPLE  =   1   )
               IF      YYKEXAMPLE-HYOKBN   NOT =   ZERO
                   MOVE    YYKEXAMPLE-COMMENT TO  SPA-Y011-COMMENT
                                                                (IDX)
                   ADD     1                  TO  IDX
               END-IF
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
           END-PERFORM
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       480-COMCOPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "Y011"              TO  SPA-MOTOPG
      *
           MOVE   "JOIN"                TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʸ������Ͽ����
      *****************************************************************
       4100-Y012-SET-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    "Y011"              TO  SPA-MOTOPG
           MOVE    "Y012"              TO  SPA-SAKIPG
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Y01             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGY012"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Y01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Y01
      *
      *    ���������ư
           MOVE    "BUNNUM"            TO  MCP-WIDGET
      *
           MOVE    "Y011"              TO  SPA-MOTOPG
           MOVE    "Y012"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "Y012"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4100-Y012-SET-EXT.
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
           IF      SPA-YID1CD       NOT =   SPACE
               PERFORM 510-YID1CDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "Y011"              TO  MCP-WINDOW
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
           MOVE    SPACE               TO  Y011
           INITIALIZE                      Y011
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   6
               IF      SPA-Y011-COMMENT (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    IDX                 TO  WRK-Z9
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    WRK-Z9          TO  Y011-NUM1
                       MOVE    SPA-Y011-COMMENT (IDX)
                                               TO  Y011-COMMENT01
                   WHEN    2
                       MOVE    WRK-Z9          TO  Y011-NUM2
                       MOVE    SPA-Y011-COMMENT (IDX)
                                               TO  Y011-COMMENT02
                   WHEN    3
                       MOVE    WRK-Z9          TO  Y011-NUM3
                       MOVE    SPA-Y011-COMMENT (IDX)
                                               TO  Y011-COMMENT03
                   WHEN    4
                       MOVE    WRK-Z9          TO  Y011-NUM4
                       MOVE    SPA-Y011-COMMENT (IDX)
                                               TO  Y011-COMMENT04
                   WHEN    5
                       MOVE    WRK-Z9          TO  Y011-NUM5
                       MOVE    SPA-Y011-COMMENT (IDX)
                                               TO  Y011-COMMENT05
                   WHEN    6
                       MOVE    WRK-Z9          TO  Y011-NUM6
                       MOVE    SPA-Y011-COMMENT (IDX)
                                               TO  Y011-COMMENT06
                   END-EVALUATE
               END-IF
           END-PERFORM
           MOVE    SPA-Y011-SELNUM     TO  Y011-SELNUM
           MOVE    SPA-Y011-BUNNUM     TO  Y011-BUNNUM
      *
           PERFORM 5001-MAPCUR-SEC
      *    ���������
           PERFORM 510-GSRAUTH-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *    MOVE    WIDGET-NORMAL       TO  Y01-PTNUM-STATE
      *    MOVE    WIDGET-NORMAL       TO  Y01-YYKNAME-STATE
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-Y011-GMN-CUR     =   ZERO
               PERFORM 50011-GMN-CURSET-SEC
          END-IF
      *
          EVALUATE    SPA-Y011-GMN-CUR
                WHEN    01
                    MOVE  "COMMENT01"      TO   MCP-WIDGET
                WHEN    02
                    MOVE  "COMMENT02"      TO   MCP-WIDGET
                WHEN    03
                    MOVE  "COMMENT03"      TO   MCP-WIDGET
                WHEN    04
                    MOVE  "COMMENT04"      TO   MCP-WIDGET
                WHEN    05
                    MOVE  "COMMENT05"      TO   MCP-WIDGET
                WHEN    06
                    MOVE  "COMMENT06"      TO   MCP-WIDGET
                WHEN    07
                    MOVE  "SELNUM"         TO   MCP-WIDGET
                WHEN    08
                    MOVE  "BUNNUM"         TO   MCP-WIDGET
                WHEN    11
                    MOVE  "B03"            TO   MCP-WIDGET
                WHEN    12
                    MOVE  "B12"            TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ϸ奫�����륻�åȽ���
      *****************************************************************
       50011-GMN-CURSET-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "COMMENT01"
                   MOVE    2                   TO  SPA-Y011-GMN-CUR
               WHEN    "COMMENT02"
                   MOVE    3                   TO  SPA-Y011-GMN-CUR
                   IF      SPA-Y011-COMMENT (2)    =   SPACE
                       MOVE    12                  TO  SPA-Y011-GMN-CUR
                   END-IF
               WHEN    "COMMENT03"
                   MOVE    4                   TO  SPA-Y011-GMN-CUR
                   IF      SPA-Y011-COMMENT (3)    =   SPACE
                       MOVE    12                  TO  SPA-Y011-GMN-CUR
                   END-IF
               WHEN    "COMMENT04"
                   MOVE    5                   TO  SPA-Y011-GMN-CUR
                   IF      SPA-Y011-COMMENT (4)    =   SPACE
                       MOVE    12                  TO  SPA-Y011-GMN-CUR
                   END-IF
               WHEN    "COMMENT05"
                   MOVE    6                   TO  SPA-Y011-GMN-CUR
                   IF      SPA-Y011-COMMENT (5)    =   SPACE
                       MOVE    12                  TO  SPA-Y011-GMN-CUR
                   END-IF
               WHEN    "COMMENT06"
                   MOVE    12                  TO  SPA-Y011-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    11                  TO  SPA-Y011-GMN-CUR
           END-EVALUATE
      *
           .
       50011-GMN-CURSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�������Ѥ����Ϥ��Ʋ�������"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "�����ֹ椬����ޤ���"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "���Ϥ��줿ʸ���ֹ����Ͽ����Ƥ��ޤ���"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  YERR
           MOVE    SPA-ERRCD            TO  YERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  YERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Y011"               TO  SPA-MOTOPG
           MOVE    "YERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-YID1CDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-YID1MSG
           MOVE    SPACE               TO  SPA-YID1-FLG
      *
           EVALUATE    SPA-YID1CD
               WHEN    "1001"
                   STRING  "ͽ�������Ƥ򤹤٤ƺ�����ޤ���"
                           "��������Ǥ�����"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-YID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-YID1CD           TO  WRK-YID1MSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  YID1
           INITIALIZE                       YID1
           MOVE    SPA-YID1CD           TO  YID1-ID1CODE
           MOVE    WRK-YID1MSG          TO  YID1-ID1MSG
           MOVE    "���"               TO  YID1-B01
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Y011"               TO  SPA-MOTOPG
           MOVE    "YID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       510-YID1CD-EXT.
           EXIT.
      *****************************************************************
      *    ͽ��ʸ�㡡�ңţ���
      *****************************************************************
       900-YYKEXAMPLE-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  YYKEXAMPLE-REC
               MOVE    ZERO            TO  FLG-YYKEXAMPLE
           ELSE
               INITIALIZE                  YYKEXAMPLE-REC
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
      *
           .
       900-YYKEXAMPLE-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
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
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
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
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �Уգԡ�����
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