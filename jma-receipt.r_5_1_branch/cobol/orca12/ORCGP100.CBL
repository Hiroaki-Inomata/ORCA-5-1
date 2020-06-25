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
       PROGRAM-ID.                     ORCGP100.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : ������̾����ʣУ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/05/28    MCC�����ġ�   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  02/11/20  GO TO ����
      * 01.00.02     NACL-¿��Ǽ  05/11/14  MONFUNC �б� ¾
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
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
      *
      *    ������ɽ��
           COPY    "ENUM-VALUE".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ������Ͽ�����ΰ�
           COPY    "P02COMMON-SPA".
      *    ������Ͽ�����̥����ΰ�
           COPY    "P02SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SHINKI                          PIC 9(01).
           03  FLG-JINMEI                          PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-KANJINAME                       PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-Z9                              PIC Z9.
           03  WRK-NAME-X-AREA.
               05  WRK-NAME-X      PIC X(02)
                                   OCCURS  25.
      *
      *    Ⱦ�������Ѵ�
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT       PIC X(100).
           03  WRK-ATO-INPUT       PIC X(100).
           03  WRK-MAX             PIC 9(04).
      *
       01  WRK-WIDGET-AREA.
           03  WRK-WIDGET                          PIC X(20).
      *
           03  WRK-WIDGET-R1   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R1-DAMMY1            PIC X(02).
               05  WRK-WIDGET1                     PIC 9(01).
               05  WRK-WIDGET-R1-DAMMY2            PIC X(17).
      *
           03  WRK-WIDGET-R2   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R2-DAMMY1            PIC X(03).
               05  WRK-WIDGET2                     PIC 9(01).
               05  WRK-WIDGET-R2-DAMMY2            PIC X(16).
      *
           03  WRK-WIDGET-R3   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R3-DAMMY1            PIC X(04).
               05  WRK-WIDGET3                     PIC 9(01).
               05  WRK-WIDGET-R3-DAMMY2            PIC X(15).
      *
           03  WRK-WIDGET-R4   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R4-DAMMY1            PIC X(05).
               05  WRK-WIDGET4                     PIC 9(01).
               05  WRK-WIDGET-R4-DAMMY2            PIC X(14).
      *
           03  WRK-WIDGET-R5   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R5-DAMMY1            PIC X(06).
               05  WRK-WIDGET5                     PIC 9(01).
               05  WRK-WIDGET-R5-DAMMY2            PIC X(13).
      *
           03  WRK-WIDGET-R6   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R6-DAMMY1            PIC X(07).
               05  WRK-WIDGET6                     PIC 9(01).
               05  WRK-WIDGET-R6-DAMMY2            PIC X(12).
      *
           03  WRK-WIDGET-R7   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R7-DAMMY1            PIC X(08).
               05  WRK-WIDGET7                     PIC 9(01).
               05  WRK-WIDGET-R7-DAMMY2            PIC X(11).
      *
           03  WRK-WIDGET-R8   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R8-DAMMY1            PIC X(09).
               05  WRK-WIDGET8                     PIC 9(01).
               05  WRK-WIDGET-R8-DAMMY2            PIC X(10).
      *
      *    �������ΰ�
      *01  CONST-AREA.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��̾
       01  JINMEI-REC.
           COPY    "CPJINMEI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *    ���ѥ����å��ѥ�᥿�ʳ�ĥ��
           COPY    "CPORCSKANACHK2.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****COPY    "ORCA-DBPATH".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
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
      *    �����Խ��塢ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
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
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
      *    ���顼���̤��
           IF  ( SPA-MOTOPG            =   "PERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "P100"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
           INITIALIZE              P100
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    ����
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-KAKUTEI
      *    ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU
           END-EVALUATE
      *
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    �����ֹ�����å�
               WHEN    "ACTIVATE"      ALSO    "SELNUM1"
               WHEN    "ACTIVATE"      ALSO    "SELNUM2"
                   MOVE    MCP-WIDGET  TO      WRK-WIDGET
                   PERFORM 4100-SELNUM-CHK-SEC
      *    ������̾�����å�
               WHEN    "ACTIVATE"      ALSO    "KANJINAME1"
               WHEN    "ACTIVATE"      ALSO    "KANJINAME2"
                   MOVE    MCP-WIDGET  TO      WRK-WIDGET
                   PERFORM 4100-KANJINAME-CHK-SEC
      *    ����������å�
               WHEN    ANY             ALSO    "KANJILST1"
               WHEN    ANY             ALSO    "KANJILST2"
                   MOVE    MCP-WIDGET  TO      WRK-WIDGET
                   PERFORM 4101-KANJILSTSEL-CHK-SEC
            END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ�����å�
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           COMPUTE SPA-GMN-P100-CUR    =   ( WRK-WIDGET5 - 1 ) * 2 + 1
      *
           MOVE    WRK-WIDGET5         TO  IDX1
      *
           MOVE    P100-SELNUM (IDX1)
                                       TO  SPA-GMN-P100-SELNUM (IDX1)
      *
      *    ̤���ϻ��ϥ����å���Ԥ�ʤ�
           IF  ( SPA-GMN-P100-SELNUM (IDX1)
                                       =   ZERO )
               PERFORM 4100-SELNUM-CUR-SET-SEC
               GO  TO  4100-SELNUM-CHK-EXT
      *
           END-IF
      *
           IF      ( SPA-GMN-P100-SELNUM (IDX1)
                               >=   1 )
            AND    ( SPA-GMN-P100-SELNUM (IDX1)
                               <=   SPA-GMN-P100-KANJILST-COUNT (IDX1))
               MOVE  SPA-GMN-P100-SELNUM (IDX1)
                               TO   SPA-GMN-P100-KANJILST-SEL (IDX1)
               MOVE  SPA-GMN-P100-SELNUM (IDX1)
                               TO  IDX2
               MOVE  SPA-GMN-P100-KANJILST-KANJI (IDX1 IDX2)
                               TO  SPA-GMN-P100-KANJINAME (IDX1)
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           PERFORM 4100-SELNUM-CUR-SET-SEC
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���������������
      *****************************************************************
       4100-SELNUM-CUR-SET-SEC         SECTION.
      *
           IF  ( IDX1  =   1 )
               MOVE    2           TO  SPA-GMN-P100-CUR
           ELSE
               MOVE    4           TO  SPA-GMN-P100-CUR
           END-IF
      *
           .
       4100-SELNUM-CUR-SET-EXT.
           EXIT.
      *****************************************************************
      *    ������̾�����å�
      *****************************************************************
       4100-KANJINAME-CHK-SEC          SECTION.
      *
      *    �������
           MOVE    ZERO                TO  SPA-GMN-P100-CUR
           MOVE    P100-KANJINAME (01)
                                       TO  SPA-GMN-P100-KANJINAME (01)
           MOVE    P100-KANJINAME (02)
                                       TO  SPA-GMN-P100-KANJINAME (02)
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >    2   )  OR
                              (SPA-ERRCD    NOT =   SPACE )
      *        ̤���ϻ��ϥ����å���Ԥ�ʤ� 
               IF    ( SPA-GMN-P100-KANJINAME (IDX1)
                                       NOT =   SPACE )
      *            Ⱦ�Ѻ��������å�
                   INITIALIZE  WRK-HENKAN-AREA
                   MOVE    SPA-GMN-P100-KANJINAME (IDX1)
                                               TO  WRK-MAE-INPUT
                   MOVE    50                  TO  WRK-MAX
                   PERFORM 890-HALF-ZEN2-SEC
                   MOVE    WRK-ATO-INPUT   TO  SPA-GMN-P100-KANJINAME
                                                               (IDX1)
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    WRK-ATO-INPUT
                                           TO  SPA-GMN-P100-KANJINAME
                                                                (IDX1)
                   ELSE
                       IF      IDX1        =   1
                           MOVE    2           TO  SPA-GMN-P100-CUR
                       ELSE
                           MOVE    4           TO  SPA-GMN-P100-CUR
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD           =   SPACE
	       IF    ( WRK-WIDGET8     NOT =   ZERO )
                   MOVE    WRK-WIDGET8         TO  IDX1
                   PERFORM 4100-KANJINAME-CUR-SET-SEC
	       END-IF
           END-IF
      *
      **** COMPUTE SPA-GMN-P100-CUR    =   ( WRK-WIDGET8 - 1 ) * 2 + 2
      *
      *    MOVE    WRK-WIDGET8         TO  IDX1
      *
      *    MOVE    P100-KANJINAME (IDX1)
      *                                TO  SPA-GMN-P100-KANJINAME (IDX1)
      *
      *    ̤���ϻ��ϥ����å���Ԥ�ʤ�
      *    IF  ( SPA-GMN-P100-KANJINAME (IDX1)
      *                                =   SPACE )
      *        PERFORM 4100-KANJINAME-CUR-SET-SEC
      *        GO  TO  4100-KANJINAME-CHK-EXT
      *
      *    END-IF
      *
      *    Ⱦ�Ѻ��������å�
      *    INITIALIZE  WRK-HENKAN-AREA
      *    MOVE    SPA-GMN-P100-KANJINAME (IDX1)
      *                                TO  WRK-MAE-INPUT
      *    MOVE    50                  TO  WRK-MAX
      *    PERFORM 890-HALF-ZEN-SEC
      *    MOVE    WRK-ATO-INPUT       TO  SPA-GMN-P100-KANJINAME (IDX1)
      *    IF      SPA-ERRCD           =   SPACE
      *        MOVE    WRK-ATO-INPUT   TO  SPA-GMN-P100-KANJINAME (IDX1)
      *    ELSE
      *        GO  TO  4100-KANJINAME-CHK-EXT
      *    END-IF
      *
      ***  PERFORM 4100-KANJINAME-CUR-SET-SEC
      *
           .
       4100-KANJINAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���������������
      *****************************************************************
       4100-KANJINAME-CUR-SET-SEC      SECTION.
      *
           IF  ( IDX1  =   1 )
               IF  ( SPA-GMN-P100-SELNUM-STATE (2)
                                   =   WIDGET-NORMAL )
                   MOVE    3       TO  SPA-GMN-P100-CUR
               ELSE
                   MOVE    4       TO  SPA-GMN-P100-CUR
               END-IF
           ELSE
               MOVE    5           TO  SPA-GMN-P100-CUR
           END-IF
      *
           .
       4100-KANJINAME-CUR-SET-EXT.
           EXIT.
      *****************************************************************
      *    ���������
      *****************************************************************
       4101-KANJILSTSEL-CHK-SEC        SECTION.
      *
           MOVE    WRK-WIDGET7     TO  IDX1
      *
      *    �ꥹ�Ȥ˳����ǡ�����¸�ߤ��ʤ����Ͻ�����Ԥ�ʤ�
      *    �������ֹ�γ������֤�Ƚ�Ǥ����
           IF  ( SPA-GMN-P100-SELNUM-STATE (IDX1)
                                   =   WIDGET-INSENSITIVE )
               GO  TO  4101-KANJILSTSEL-CHK-EXT
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >       50 )
               IF      (   P100-KANJILST-SELECT (IDX1 IDX2) =   "T"  )
                   MOVE    IDX2    TO  SPA-GMN-P100-SELNUM (IDX1)
                                       SPA-GMN-P100-KANJILST-SEL (IDX1)
                   MOVE  SPA-GMN-P100-KANJILST-KANJI (IDX1 IDX2)
                                   TO  SPA-GMN-P100-KANJINAME (IDX1)
                   MOVE    50      TO  IDX2
               END-IF
           END-PERFORM
      *    ������������
           PERFORM 4100-SELNUM-CUR-SET-SEC
      *
           .
       4101-KANJILSTSEL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-P100-FLG-BTN
      *
           MOVE    "P02"               TO  SPA-SAKIPG
           MOVE    "P100"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
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
      *    ���ꡡ����
      *****************************************************************
       490-KAKUTEI                 SECTION.
      *
      *    ������̾�������
           PERFORM 490-KANJINAME-SET-SEC
           IF  ( SPA-ERRCD     NOT =   SPACE )
               COMPUTE SPA-GMN-P100-CUR    =   IDX1    +   2
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *
           MOVE    1                   TO  SPA-GMN-P100-FLG-BTN
      *
           MOVE    "P02"               TO  SPA-SAKIPG
           MOVE    "P100"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ͽ������
      *****************************************************************
       490-TOUROKU                 SECTION.
      *
      *    ������̾�������
           PERFORM 490-KANJINAME-SET-SEC
           IF  ( SPA-ERRCD     NOT =   SPACE )
               COMPUTE SPA-GMN-P100-CUR    =   IDX1    +   2
               GO  TO  490-TOUROKU-EXT
           END-IF
      *
      *    ��̾�ģ��Խ�����
           PERFORM 490-JINMEIDB-EDIT-SEC
      *
           MOVE    1                   TO  SPA-GMN-P100-FLG-BTN
      *
           MOVE    "P02"               TO  SPA-SAKIPG
           MOVE    "P100"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̾�������
      *****************************************************************
       490-KANJINAME-SET-SEC        SECTION.
      *
      ***  PERFORM VARYING IDX1    FROM    1   BY  1
      *        UNTIL     ( IDX1    >   2 )
      *        Ⱦ�Ѻ��������å�
      *        INITIALIZE  WRK-HENKAN-AREA
      *        MOVE    SPA-GMN-P100-KANJINAME (IDX1)
      *                        TO  WRK-MAE-INPUT
      *        MOVE    50                  TO  WRK-MAX
      *        PERFORM 890-HALF-ZEN-SEC
      *        MOVE    WRK-ATO-INPUT
      *                            TO  SPA-GMN-P100-KANJINAME (IDX1)
      *        IF  ( SPA-ERRCD     NOT =   SPACE )
      *            GO  TO  490-KANJINAME-SET-EXT
      *        END-IF
      *****END-PERFORM
      *    ��̾�����å�
           MOVE    ZERO            TO  WRK-WIDGET
           PERFORM 4100-KANJINAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO  TO  490-KANJINAME-SET-EXT
           END-IF
      *
      *    ������̾�����ꤷ���У��������
           MOVE    1               TO  CNT-KANJINAME
           MOVE    SPACE           TO  SPA-GMN-P100-KAKUTEI-NAME
      *
           IF  ( SPA-GMN-P100-KANJINAME (1)
                                   NOT =   SPACE )
               STRING  SPA-GMN-P100-KANJINAME (1)
                                   DELIMITED   BY  SPACE
               INTO    SPA-GMN-P100-KAKUTEI-NAME
               WITH    POINTER     CNT-KANJINAME
               END-STRING
           END-IF
      *
           IF  ( SPA-GMN-P100-KANJINAME (2)
                                   NOT =   SPACE )
               STRING  "��"        DELIMITED   BY  SIZE
                       SPA-GMN-P100-KANJINAME (2)
                                   DELIMITED   BY  SPACE
               INTO    SPA-GMN-P100-KAKUTEI-NAME
               WITH    POINTER     CNT-KANJINAME
               END-STRING
           END-IF
      *
           .
       490-KANJINAME-SET-EXT.
           EXIT.
      *****************************************************************
      *    ��̾�ģ��Խ�����
      *****************************************************************
       490-JINMEIDB-EDIT-SEC        SECTION.
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   2 )
               IF    (SPA-GMN-P100-SELNUM-STATE (IDX1)
                                   =   WIDGET-NORMAL )
                AND  (SPA-GMN-P100-KANJINAME (IDX1)
                               NOT =   SPACE )
                   MOVE    SPA-GMN-P100-KANANAME (IDX1)
                                       TO  WRK-NAME-X-AREA
                   PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   (   IDX3    >   25 )
                        OR     ( WRK-NAME-X (IDX3) =   SPACE )
                       IF  ( WRK-NAME-X (IDX3) = "��" )
                           MOVE    "@@"    TO  WRK-NAME-X (IDX3)
                       END-IF
                   END-PERFORM
      *
                   INITIALIZE      JINMEI-REC
      *
                   UNSTRING    WRK-NAME-X-AREA
                   DELIMITED   BY  "@@" OR SPACE
                   INTO        JINMEI-KANA
                   END-UNSTRING
      *
                   MOVE    SPA-GMN-P100-KANJINAME (IDX1)
                                           TO  JINMEI-KANJI
      *
                   MOVE    SMCNDATE-YMD    TO  JINMEI-UPYMD
      *
                   MOVE    SMCNDATE-HMS    TO  JINMEI-UPHMS
      *************ACCEPT  JINMEI-UPHMS    FROM    TIME
      *
      *            ��̾�ơ��֥��ɤ߹��߽���
                   PERFORM 900-JINMEI-READ-SEC
      *
                   IF  (   FLG-JINMEI      =   ZERO )
      *                �����ॹ����פ򹹿�����
                       MOVE    SPA-HOSPNUM         TO  JINMEI-HOSPNUM
                       MOVE    JINMEI-REC          TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_jinmei"        TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                   ELSE
      *
                       DISPLAY "DBINSERT " JINMEI-REC
      *                ��̾�ơ��֥��ɲý���
                       MOVE    SPA-HOSPNUM         TO  JINMEI-HOSPNUM
                       MOVE    JINMEI-REC          TO  MCPDATA-REC
                       MOVE    "DBINSERT"          TO  MCP-FUNC
                       MOVE    "tbl_jinmei"        TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                   END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       490-JINMEIDB-EDIT-EXT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P100"              TO  MCP-WINDOW.
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
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2)
      *
               MOVE    SPA-GMN-P100-KANANAME (IDX1)
                                   TO  P100-KANANAME (IDX1)
               MOVE    SPA-GMN-P100-KANJILST-COUNT (IDX1)
                                   TO  P100-KANJILST-COUNT (IDX1)
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-P100-KANJILST-COUNT
                                                            (IDX1))
      *
                   IF  ( SPA-GMN-P100-SELNUM-STATE (IDX1)
                                       =   WIDGET-NORMAL )
                       MOVE    IDX2    TO  WRK-Z9
                       MOVE    WRK-Z9  TO  P100-KANJILST-NO (IDX1 IDX2)
                   END-IF
      *
                   MOVE    SPA-GMN-P100-KANJILST-KANJI (IDX1 IDX2)
                                   TO  P100-KANJILST-KANJI (IDX1 IDX2)
      *
                   IF  ( SPA-GMN-P100-SELNUM (IDX1)    =   IDX2 )
                       MOVE    "T" TO  P100-KANJILST-SELECT (IDX1 IDX2)
                   ELSE
                       MOVE    "F" TO  P100-KANJILST-SELECT (IDX1 IDX2)
                   END-IF
      *
               END-PERFORM
      *
               MOVE    SPA-GMN-P100-SELNUM (IDX1)
                                       TO  P100-SELNUM (IDX1)
               MOVE    SPA-GMN-P100-SELNUM-STATE (IDX1)
                                       TO  P100-SELNUM-STATE (IDX1)
               MOVE    SPA-GMN-P100-KANJINAME (IDX1)
                                       TO  P100-KANJINAME (IDX1)
      *
           END-PERFORM
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
           EVALUATE    SPA-GMN-P100-CUR
      *
      *    �����ֹ棱
           WHEN   001
               MOVE  "SELNUM1"     TO  MCP-WIDGET
      *    ������
           WHEN   002
               MOVE  "KANJINAME1"   TO  MCP-WIDGET
      *    �����ֹ棲
           WHEN   003
               MOVE  "SELNUM2"     TO  MCP-WIDGET
      *    ������
           WHEN   004
               MOVE  "KANJINAME2"   TO  MCP-WIDGET
           WHEN   005
               MOVE  "B12"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
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
               MOVE    "���Ϥ��줿�ֹ������Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
                   MOVE    "���٤����Ѥ����Ϥ��Ʋ�������"
                                        TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  PERR
           MOVE    SPA-ERRCD            TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  PERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "P100"               TO  SPA-MOTOPG
           MOVE    "PERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "PERR"               TO  MCP-WINDOW
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
      *    Ⱦ�Ѷ�������Ѷ�����Ѵ�����
      *****************************************************************
       890-HALF-ZEN-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    WRK-MAX             TO  KANACHK-MAX-CNT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ⱦ�Ѷ�������Ѷ�����Ѵ�����
      *****************************************************************
       890-HALF-ZEN2-SEC         SECTION.
      *
           IF     SPA-ENCODING         =   2
      *        ��ĥ�����б�
               PERFORM 8910-HALF-ZENC2-SEC
           ELSE
      *        ��ĥ�����Բ�
               PERFORM 890-HALF-ZEN-SEC
           END-IF
      *
           .
       891-HALF-ZEN2-EXT.
           EXIT.
      *****************************************************************
      *    Ⱦ�Ѷ�������Ѷ�����Ѵ������ʳ��������
      *****************************************************************
       8910-HALF-ZENC2-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           MOVE    WRK-MAX             TO  KANACHK2-MAX-CNT
           IF     (KANACHK2-RC      NOT =   ZERO  )  OR
                  (KANACHK2-SYUBETU     =   1     )
               MOVE    "0002"               TO  SPA-ERRCD
               MOVE    KANACHK2-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    KANACHK2-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       8910-HALF-ZENC2-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       900-JINMEI-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-JINMEI
      *
      *    ��̾�ơ��֥븡��
           MOVE    SPA-HOSPNUM         TO  JINMEI-HOSPNUM
           MOVE    JINMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_jinmei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DB-SELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jinmei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DB-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-JINMEI
               ELSE
                   MOVE    1               TO  FLG-JINMEI
               END-IF
           ELSE
               MOVE    1               TO  FLG-JINMEI
           END-IF
      *
      *    ��̾�ơ��֥륯������
           MOVE    "tbl_jinmei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DB-CLOSE-SEC
      *
           .
      *
       900-JINMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DB-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DB-READ-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DB-CLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
