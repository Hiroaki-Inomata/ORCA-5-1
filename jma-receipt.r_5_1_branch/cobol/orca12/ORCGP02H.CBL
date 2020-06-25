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
       PROGRAM-ID.             ORCGP02H.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : ���ٸ���������̡ʣУ����ȡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  11/07/XX    NACL-¿��Ǽ   ��������
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
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ������ɽ��
           COPY    "ENUM-VALUE".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ������Ͽ�����ΰ�
           COPY    "P02COMMON-SPA".
      *    ���Բ��̥����ΰ�
           COPY    "P02SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-MAX-EDANUM          PIC 9(03).
           03  WRK-RENNUM              PIC 9(02).
           03  WRK-Z9                  PIC Z9.
      *
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-LIST-ROW            PIC S9(09).
           03  WRK-LIST-INIT           PIC 9(01).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ݸ��ֹ�
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    �ݸ��Թ�ޥ���
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *   ���������Ѵ�����
          COPY    "CPORCSGDAY.INC".
      *
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
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            �����Խ�
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02H"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
           EVALUATE    SPA-MOTOPG
      *        ��ǧ��å�����
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
               WHEN    OTHER
      *        �ӣУ��Խ�
                   PERFORM 300-SPA-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����Խ�����
      *****************************************************************
       300-SPA-INIT-SEC                SECTION.
      *
           MOVE    1                   TO  WRK-LIST-INIT
           INITIALIZE                  SPA-P02H-KOHNUM-LST
           MOVE    ZERO                TO  SPA-P02H-MAX
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-KOHMAX )
                          OR  (IDY     >=  50             )
               IF      SPA-NAI-HBTNUM (IDX)    =   SPA-P02H-HBTNUM
                   ADD     1                   TO  IDY
                   MOVE    SPA-GMN-KOHNUML (IDX)
                                               TO  SPA-P02H-TKOHNUM(IDY)
                   MOVE    SPA-GMN-KOHNUMMEIL(IDX)
                                               TO  SPA-P02H-TKOHNUMMEI
                                                                  (IDY)
                   MOVE    IDY                 TO  SPA-P02H-MAX
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-P02H-SELNUM
           MOVE    1                   TO  SPA-P02H-CUR
           .
       300-SPA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̤�����
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-PIDCD
           MOVE    SPACE               TO  SPA-PID1-FLG
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF     (WRK-LIST-INIT       =   1  )  OR
                  (SPA-P02H-MAX        =   ZERO )
               MOVE    ZERO                TO  WRK-LIST-ROW
           ELSE
               MOVE    P02H-LIST-ROW       TO  WRK-LIST-ROW
           END-IF
      *
           MOVE    SPACE               TO  P02H
           INITIALIZE                      P02H
      *
           MOVE    WRK-LIST-ROW        TO  P02H-LIST-ROW
      *
           MOVE    SPA-P02H-HBTNUM     TO  P02H-HBTNUM
           MOVE    SPA-P02H-SELNUM     TO  P02H-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-P02H-MAX   )
                MOVE    IDX                TO  WRK-Z9
                MOVE    WRK-Z9             TO  P02H-TNUM    (IDX)
                MOVE    SPA-P02H-TKOHNUM (IDX)
                                           TO  P02H-TKOHNUM (IDX)
                MOVE    SPA-P02H-TKOHNUMMEI (IDX)
                                           TO  P02H-TKOHNUMMEI(IDX)
                IF      IDX                =   SPA-P02H-SELNUM
                    MOVE    "T"            TO  P02H-LIST-SELECT (IDX)
                    MOVE    IDX            TO  P02H-LIST-ROW
                ELSE
                    MOVE    "F"            TO  P02H-LIST-SELECT  (IDX)
                END-IF
           END-PERFORM
           MOVE    SPA-P02H-MAX        TO  P02H-LIST-COUNT
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
           IF      SPA-P02H-CUR        =   ZERO
               MOVE    1                   TO  SPA-P02H-CUR
           END-IF
           EVALUATE    SPA-P02H-CUR
               WHEN    01
                   MOVE    "SELNUM"           TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"              TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK-SYORI-SEC
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
           MOVE    ZERO                TO  SPA-P02H-CUR
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ����
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM  420-SELNUM-CHK-SEC
      *        ����
               WHEN    ANY              ALSO    "HBTNUMLST"
                   PERFORM  430-LIST-CHK-SEC
      *        ����¾
      *        WHEN    OTHER
      *            ���ϥ����å�����
      *            PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE.
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    ���������
      *****************************************************************
       420-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    P02H-SELNUM         TO  SPA-P02H-SELNUM
           MOVE    1                   TO  SPA-P02H-CUR
      *
           IF      SPA-P02H-SELNUM     >=  1    AND
                                       <=  SPA-P02H-MAX
               MOVE    SPA-P02H-TKOHNUM (SPA-P02H-SELNUM)
                                           TO  SPA-P02H-KOHNUM
               MOVE    1                   TO  FLG-OK
           END-IF
           IF      FLG-OK              =   ZERO
               IF      SPA-P02H-SELNUM     NOT =   ZERO
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        ���
               PERFORM 210-BACK
           END-IF
           .
       420-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���������
      *****************************************************************
       430-LIST-CHK-SEC       SECTION.
      *
      *    ������
           MOVE    ZERO                TO  SPA-P02H-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02H-MAX
               IF      P02H-LIST-SELECT  (IDX)  =   "T"
                   MOVE    IDX              TO  SPA-P02H-SELNUM
                   MOVE    SPA-P02H-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-P02H-SELNUM     >   ZERO
               MOVE    SPA-P02H-SELNUM     TO  P02H-SELNUM
               PERFORM 420-SELNUM-CHK-SEC
           END-IF
           .
       430-LIST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *    ���ܥ����å����̲��̽���
           PERFORM 4102-KIHON-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܥ����å����̲��̽���
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-P02H-KOHNUM
           PERFORM 210-BACK
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02H"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           MOVE    ZERO                TO  SPA-FLG-END
           MOVE    "P02"               TO  SPA-P02-GAMEN
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    ��������FORMAT�����Խ�
      *****************************************************************
       4001-SEIWA-YMDEDIT-SEC           SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE           TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "0000000"       TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "9999999"       TO  WRK-HENYMD
               WHEN    OTHER
      *
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   ELSE
                       MOVE    SPACE               TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
           .
       4001-SEIWA-YMDEDIT-EXT.
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
      *    IF      SPA-PIDCD       NOT =   SPACE
      **       PERFORM 520-JIDSET-SEC
      *        GO  TO  500-SET-SCREEN-EXT
      *    END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P02H"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
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
                   MOVE    "�����ֹ楨�顼"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02H"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  PID1
      *    EVALUATE    SPA-PIDCD
      *        WHEN    OTHER
      *            MOVE    SPA-PIDCD
      *                                TO  PID1-ID1MSG
      *    END-EVALUATE
      *
           INITIALIZE                      PID1
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    "���"              TO  PID1-B01
           MOVE    "�ϣ�"              TO  PID1-B12
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P02H"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
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
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *