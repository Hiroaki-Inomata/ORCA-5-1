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
       PROGRAM-ID.             ORCGP02T.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �ݸ���ǧǯ����������̡ʣУ����ԡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  11/08/XX    NACL-¿��Ǽ   ��������
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
           03  FLG-PTHKNRRK        PIC 9(01).
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
      *    �ݸ���ǧǯ��������ޥ���
       01  PTHKNRRK-REC.
           COPY    "CPPTHKNRRK.INC".
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
               MOVE    "P02T"              TO  MCP-WINDOW
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
           INITIALIZE                  SPA-P02T-HKNRRK-LST
      *
           INITIALIZE                  PTHKNRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTHKNRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTHKNRRK-PTID
           MOVE    SPA-P02T-HKNID      TO  PTHKNRRK-HKNID
      *
           MOVE    PTHKNRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNRRK
               INITIALIZE                  PTHKNRRK-REC
           END-IF
      *
           MOVE    ZERO                TO  SPA-P02T-MAX
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL     (FLG-PTHKNRRK     =   1    )
                       OR    (IDX              >=  5    )
               ADD     1                   TO  IDX
               MOVE    PTHKNRRK-KAKUNINYMD TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P02T-KAKUNINYMD
                                                                 (IDX)
               ADD     1                   TO  SPA-P02T-MAX
      *
               MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
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
           MOVE    SPACE               TO  P02T
           INITIALIZE                      P02T
      *
           MOVE    SPA-P02T-HKNMSG     TO  P02T-HKNMSG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5   )
                MOVE    SPA-P02T-KAKUNINYMD (IDX)
                                           TO  P02T-KAKUNINYMD (IDX)
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
           IF      SPA-P02H-CUR        =   ZERO
               MOVE    1                   TO  SPA-P02H-CUR
           END-IF
           EVALUATE    SPA-P02H-CUR
               WHEN    01
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
      *
           .
       200-ENTRY-EXT.
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
           MOVE    "P02T"              TO  SPA-MOTOPG
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
      **** MOVE    "P02"               TO  SPA-P02-GAMEN
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P02T"              TO  MCP-WINDOW
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
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
           MOVE    "P02T"              TO  SPA-MOTOPG
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
      *    �ݸ���ǧǯ��������ޥ�����������
      *****************************************************************
       900-PTHKNRRK-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTHKNRRK-REC
               MOVE    ZERO                TO  FLG-PTHKNRRK
           ELSE
               INITIALIZE                  PTHKNRRK-REC
               MOVE    1                   TO  FLG-PTHKNRRK
           END-IF
           .
       900-PTHKNRRK-READ-EXT.
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