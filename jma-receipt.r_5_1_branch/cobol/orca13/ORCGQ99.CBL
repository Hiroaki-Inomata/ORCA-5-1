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
       PROGRAM-ID.                     ORCGQ99.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ԾȲ�
      *  ����ݡ��ͥ��̾  : ���긡����Į��������̡ʣѣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/04/26    MCC�����ġ�   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.02     MCC-����     02/06/26  ����ִ����ģ��ɲ�
      *                                     ���̣ѣɣģ���
      *                                     �ѣ�������̾���ѹ�
      *                                     �����ˣѣɣģ����ɲ�
      * 01.00.03     NACL-����    03/06/04  ����ܥ�����
      * 03.05.00     NACL-����    07.05.23  ���롼�׿����б�
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
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-EOF                             PIC 9(01).
           03  FLG-ADRS                            PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ADRS                            PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-ZZZ9                            PIC ZZZ9.
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
      *    �������ΰ�
      *01  CONST-AREA.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ޥ�����
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *    ��ʬ����ʸ�����Խ�
           COPY    "CPORCSADDSIGN.INC".
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
      *----(01.00.02) LINE ADD START ----------------------------------
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *----(01.00.02) LINE ADD END   ----------------------------------
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
           IF  ( FLG-END           =   ZERO )
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
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
      *    ���顼���̤��
           IF  ( SPA-MOTOPG            =   "QERR" )
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
           MOVE    "Q99"               TO  MCP-WINDOW
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
      *
           MOVE    SPACE           TO  SPA-Q99
           INITIALIZE                  SPA-Q99
      *
           MOVE    SPACE           TO  SPA-Q01-Q99-JYUSHO
      *
           INITIALIZE                  ADRS-REC
           MOVE    SPA-Q01-Q99-POST
                                   TO  ADRS-POST
      *
      *    ���긡������
           PERFORM 900-ADRS-KEY4-SEL-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL ( FLG-ADRS    =   1 )
                OR   ( IDX1        >   200 )
      *
      *        �����Խ�����
               PERFORM 3101-ADRS-EDIT-SEC
      *
      *        �����ɹ�����
               PERFORM 900-ADRS-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    �������륭���������
           MOVE   1                TO  SPA-GMN-Q99-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       3101-ADRS-EDIT-SEC          SECTION.
      *
           MOVE    IDX1            TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9        TO  SPA-GMN-Q99-NUM (IDX1)
           MOVE    ADRS-POST       TO  SPA-GMN-Q99-POST (IDX1)
           MOVE    ADRS-EDITADRS-NAME
                                   TO  SPA-GMN-Q99-ADRS (IDX1)
           MOVE    IDX1            TO  SPA-GMN-Q99-MAX
      *
           .
       3101-ADRS-EDIT-EXT.
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
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   MOVE    MCP-WIDGET  TO  WRK-WIDGET
                   PERFORM 4100-SELNUM-CHK-SEC
      *    ����������å�
               WHEN    ANY             ALSO    "ADRSLIST"
                   PERFORM 4101-ADRSLISTSEL-SEC
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
           MOVE    1                   TO  SPA-GMN-Q99-CUR
      *
           MOVE    Q99-SELNUM          TO  SPA-GMN-Q99-SELNUM
      *
      *    ̤���ϻ��ϥ����å���Ԥ�ʤ�
           IF  ( SPA-GMN-Q99-SELNUM    =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           IF      ( SPA-GMN-Q99-SELNUM   >=   1 )
            AND    ( SPA-GMN-Q99-SELNUM   <=   SPA-GMN-Q99-MAX )
      *
               PERFORM 490-KAKUTEI-SEC
      *
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-Q99-CUR
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���������
      *****************************************************************
       4101-ADRSLISTSEL-SEC            SECTION.
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >      200 )
               IF      (   Q99-ADRSLIST-SELECT (IDX1)  =   "T"  )
                   MOVE    IDX1            TO      SPA-GMN-Q99-SELNUM
                   PERFORM 490-KAKUTEI-SEC
                   MOVE   200              TO  IDX1
               END-IF
           END-PERFORM
      *
           .
       4101-KEKKALSTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q99"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "Q01"               TO  MCP-WINDOW
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
       490-KAKUTEI-SEC             SECTION.
      *
      *    �����ֹ�����å�
      *    ����̤���ϥ����å�
           IF  ( SPA-GMN-Q99-SELNUM    =   ZERO )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    �����ϰϥ����å�
           IF      ( SPA-GMN-Q99-SELNUM   >=   1 )
            AND    ( SPA-GMN-Q99-SELNUM   <=   SPA-GMN-Q99-MAX )
      *
               CONTINUE
      *
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    ��������ꤷ���ѣ��������
           MOVE    SPA-GMN-Q99-POST (SPA-GMN-Q99-SELNUM)
                                       TO  SPA-Q01-Q99-POST
           MOVE    SPA-GMN-Q99-ADRS (SPA-GMN-Q99-SELNUM)
                                       TO  SPA-Q01-Q99-JYUSHO
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q99"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "Q01"               TO  MCP-WINDOW
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
           MOVE    "Q99"               TO  MCP-WINDOW.
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
      *    �������
           MOVE    SPA-GMN-Q99-MAX
                                   TO  Q99-ADRSLIST-COUNT
      *
      *    �������
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-Q99-MAX)
      *        Ϣ��
               MOVE    SPA-GMN-Q99-NUM  (IDX1)
                                   TO  Q99-NUM (IDX1)
      *        ͹���ֹ�
               MOVE    SPA-GMN-Q99-POST (IDX1)
                                   TO  Q99-POST (IDX1)
      *        ����
               MOVE    SPA-GMN-Q99-ADRS (IDX1)
                                   TO  Q99-ADRS (IDX1)
      *
               IF      ( SPA-GMN-Q99-SEL =   IDX1 )
                   MOVE    "T"     TO  Q99-ADRSLIST-SELECT (IDX1)
               ELSE
                   MOVE    "F"     TO  Q99-ADRSLIST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
      *    �����ֹ�
           MOVE    SPA-GMN-Q99-SELNUM  TO  Q99-SELNUM
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
           EVALUATE    SPA-GMN-Q99-CUR
      *
      *    �����ֹ�
           WHEN   001
               MOVE  "SELNUM"      TO  MCP-WIDGET
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
               MOVE    "�ϰϳ����ֹ椬���Ϥ���Ƥ��ޤ�"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "�����ֹ椬̤���ϤǤ�"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q99"                TO  SPA-MOTOPG
           MOVE    "QERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "QERR"               TO  MCP-WINDOW
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
      *    ����ޥ�����������͹���ֹ��
      *****************************************************************
       900-ADRS-KEY4-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
      *
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    ADRS-POST           TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING   ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR        TO  ADRS-POST
      *
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
      *
           .
       900-ADRS-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ����ޥ������ɹ���͹���ֹ��
      *****************************************************************
       900-ADRS-KEY4-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           .
       900-ADRS-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
