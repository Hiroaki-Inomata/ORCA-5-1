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
       PROGRAM-ID.                 ORCGJ026.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��ץ�����
      *  ����ݡ��ͥ��̾  : ����ƣ��������ؼ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  04/12/10    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  05/11/30  MONFUNC �б�
      * 03.05.00     NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      * 04.07.00     NACL-¿��Ǽ  12/09/25  �ץ�����४�ץ�����ݸ��б�
      * 04.08.00     NACL-¿��Ǽ  14/08/12  ���饤����Ȱ����б�
      *****************************************************************
      *
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
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ��׾Ȳ��̥ѥ�᥿
           COPY    "J01COMMON-SPA".
      *
      *    ���̥����ΰ�
           COPY    "J02SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KAKUTEI         PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-JIDMSG.
               05  WRK-JIDMSG1              PIC X(40).
               05  WRK-JIDMSG2              PIC X(60).
      *    ����ƣ����ץ������̾
           03  WRK-PRINT-PG            PIC X(20).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-ERRCD           PIC X(04).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    Ģɼ�Խ���ʬ����
           COPY  "CPSK1030.INC".
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
      *    ����ƣ����ͼ����ϥѥ�᥿
           COPY    "CPORCHC33.INC".
      *
      *    ����饤��Ģɼ̾��������ץ��̾�����ѥ�
           COPY    "CPORCSPRTNM.INC".
      *
      *    ���饤����Ȱ�������
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    �ģ¸���
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           COPY    "ORCA24SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *H24.9
           MOVE    SPA-ERRCD       TO  WRK-ERRCD
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J02         TO  SPA-FREE
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
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "JERR"
      *H24.9
               IF      WRK-ERRCD           =   "9001"
                                           OR  "9002"
      *            �ץ�����४�ץ����ȤΥ��顼
                   MOVE    SPACE               TO  WRK-ERRCD
                   MOVE    10                  TO  SPA-GMN-CUR
                   PERFORM 210-BACK
                   GO      TO   100-INIT-EXT
               END-IF
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END         =   ZERO
      *            �����Խ�
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "J026"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG(1:4)
      *        ��ǧ���̤��
               WHEN    "JID1"
                   PERFORM 3003-JID1-SET-SEC
               WHEN    OTHER
      *            ��������Խ�
                   PERFORM                     310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-J026-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-J026-AREA
      *
           STRING      "��ȯ�Կ��Ųʡ�"    DELIMITED  BY  SIZE
                       SPA-GMN-SRYKAMEI    DELIMITED  BY  SPACE
                       "��"                DELIMITED  BY  SIZE
                                   INTO    SPA-J026-GMN-MIDASI
           END-STRING
      *
           MOVE    1                   TO  SPA-J026-GMN-STRDD
           MOVE    ZERO                TO  SPA-J026-GMN-ENDPAGE
           MOVE    ZERO                TO  SPA-J026-GMN-ENDLINE
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     ��ǧ���̤��ν���
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
      *            ������ǧ
                   IF      SPA-JID1-FLG        =   "OK"
                       MOVE    1               TO  FLG-PRINT
                       PERFORM 2301-PRINT-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-J026-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-JIDCD
                   MOVE    SPACE               TO  SPA-JID1-FLG
               WHEN    "0102"
      *            ������ǧ
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 2201-HEAD-PRINT-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-J026-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-JIDCD
                   MOVE    SPACE               TO  SPA-JID1-FLG
           END-EVALUATE
           .
       3003-JID1-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  J026
      *
      *    �����Խ�
           INITIALIZE                      J026
           MOVE    SPA-J026-GMN-MIDASI     TO  J026-MIDASI
      *
           MOVE    SPA-J026-GMN-STRDD      TO  J026-STRDD
           MOVE    SPA-J026-GMN-ENDPAGE    TO  J026-ENDPAGE
           MOVE    SPA-J026-GMN-ENDLINE    TO  J026-ENDLINE
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-J026-CUR        =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-J026-CUR
               WHEN    1
                   MOVE    "STDD"              TO  MCP-WIDGET
               WHEN    2
                   MOVE    "ENDPAGE"           TO  MCP-WIDGET
               WHEN    3
                   MOVE    "ENDLINE"           TO  MCP-WIDGET
               WHEN    4
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "STRDD"
                   MOVE    2               TO  SPA-J026-CUR
               WHEN    "ENDPAGE"
                   MOVE    3               TO  SPA-J026-CUR
               WHEN    "ENDLINE"
                   MOVE    4               TO  SPA-J026-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        Ƭ�����
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 220-HEAD-PRINT-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 230-PRINT-SEC
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
           MOVE    ZERO                TO  SPA-J026-CUR
      *
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
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
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-KAKUTEI         =   1     )
               PERFORM 4103-KANREN-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̤�ӣУ����åȽ���
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
           MOVE    J026-STRDD          TO  SPA-J026-GMN-STRDD
           MOVE    J026-ENDPAGE        TO  SPA-J026-GMN-ENDPAGE
           MOVE    J026-ENDLINE        TO  SPA-J026-GMN-ENDLINE
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
           PERFORM 41021-STDD-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41022-ENDPAGE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41022-ENDLINE-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������å�����
      *****************************************************************
       41021-STDD-SEC                SECTION.
      *
           IF      SPA-J026-GMN-STRDD  =   ZERO
               MOVE    1                   TO  SPA-J026-GMN-STRDD
           END-IF
           IF      SPA-J026-GMN-STRDD  >=  01  AND
                                       <=  SPA-SRYMATU
               CONTINUE
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-J026-CUR
           END-IF
      *
           .
       41021-STDD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǽ��ڡ�������
      *****************************************************************
       41022-ENDPAGE-SEC                SECTION.
      *
           .
       41022-ENDPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǽ��Խ���
      *****************************************************************
       41022-ENDLINE-SEC                SECTION.
      *
           IF      SPA-J026-GMN-ENDLINE    >=  40
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-J026-CUR
           END-IF
      *
           .
       41022-ENDLINE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������å�����
      *****************************************************************
       4103-KANREN-CHK-SEC                SECTION.
      *
      *
           .
       4103-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ƭ���������
      *****************************************************************
       220-HEAD-PRINT-SEC                SECTION.
      *
           MOVE    "0102"              TO  SPA-JIDCD
      *
           .
       220-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ƭ���������
      *****************************************************************
       2201-HEAD-PRINT-SYORI-SEC                SECTION.
      *
           MOVE    2                   TO  FLG-PRINT
           PERFORM 2301-PRINT-SYORI-SEC
      *
           .
       2201-HEAD-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����������
      *****************************************************************
       230-PRINT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-J026-CUR
           MOVE    1                   TO  FLG-KAKUTEI
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-JIDCD
           END-IF
      *
           .
       230-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ����������
      *****************************************************************
       2301-PRINT-SYORI-SEC            SECTION.
      *
      *    ���Ų�
           MOVE    SPACE               TO  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1030-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1030-STYUKYMD(7:2)
           MOVE    SYS-1030-STYUKYMD   TO  SYS-1030-EDYUKYMD
      *    �����ƥ��������
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1030-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1030-REC
           ELSE
               INITIALIZE                  SYS-1030-REC
           END-IF
           IF      SYS-1030-KARUTE3KBN     =   SPACE
               MOVE    ZERO                TO  SYS-1030-KARUTE3KBN
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    ������������������
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
           MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *
      *ver.4.8
      *    ���饤����Ȱ���
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        ���饤����Ȱ�����������������
               PERFORM 23010-CLIENT-PRT-SEC
           END-IF
      *
           IF      SPA-GMN-NYUGAIKBN    =   "2"
               PERFORM 23011-PRINT-GAIRAI-SEC
           ELSE
               PERFORM 23012-PRINT-NYUIN-SEC
           END-IF
      *
      *ver.4.8
      *    ���饤����Ȱ���
           IF     (SPA-CLIENT-PRT-FLG   =   1     )
             AND  (SPA-ERRCD            =   SPACE )
      *        �������ߡ�����
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 210-BACK
           ELSE
               PERFORM 510-ERRSET-SEC
           END-IF
           .
       2301-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���饤����Ȱ�����������
      *****************************************************************
       23010-CLIENT-PRT-SEC         SECTION.
      *
      *    ���饤����Ȱ�����������������
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
           .
       23010-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���� ����ƣ�����������
      *****************************************************************
       23011-PRINT-GAIRAI-SEC            SECTION.
      *
      *    ����ƣ����
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000005"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  ORCSPRTNM-SRYYMD(1:6)
           MOVE    "01"                TO  ORCSPRTNM-SRYYMD(7:2)
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-PRINT-PG
           ELSE
               MOVE    "ORCHC33"           TO  WRK-PRINT-PG
           END-IF
      *
           INITIALIZE                  ORCHC33AREA
           MOVE    "1"                 TO  ORCHC33-KBN
           MOVE    SPA-GMN-PTNUM       TO  ORCHC33-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCHC33-PTID
           MOVE    SPA-GMN-NAME        TO  ORCHC33-NAME
           MOVE    SPA-GMN-BIRTHDAY    TO  ORCHC33-BIRTHDAY
           MOVE    SPA-GMN-SEX         TO  ORCHC33-SEX
           MOVE    SPA-NAI-SRYYM       TO  ORCHC33-SRYYM
           MOVE    SPACE               TO  ORCHC33-SRYYM-END
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCHC33-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  ORCHC33-SRYKA
      *H24.9
           MOVE    SPA-GMN-HHKNCOMBI   TO  ORCHC33-HKNCOMBI
      *
           IF      FLG-PRINT           =   1
      *        ����
               MOVE    ZERO                TO  ORCHC33-PRINT-FLG
      *        ����������
               MOVE    SPA-J026-GMN-STRDD  TO  ORCHC33-STRDD
      *        �ǽ�������
               MOVE    SPA-J026-GMN-ENDPAGE    TO  ORCHC33-ENDPAGE
      *        �ǽ�������
               MOVE    SPA-J026-GMN-ENDLINE    TO  ORCHC33-ENDLINE
           ELSE
      *        Ƭ��
               MOVE    1                   TO  ORCHC33-PRINT-FLG
           END-IF
      *
           MOVE    SYS-1030-KARUTE3KBN
                                       TO  ORCHC33-KARUTE3KBN
      *
           MOVE    "01"                TO  SPA-PRT-FLG
      *
           CALL    WRK-PRINT-PG        USING
                                       SPA-AREA
                                       ORCHC33AREA
      *
           IF      ORCHC33-RC      NOT =   ZERO
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
           IF      ORCHC33-RC          =   8
      *        �ץ�����४�ץ������ݸ�����Υ��顼
               MOVE    "9001"              TO  SPA-ERRCD
           END-IF
      *v4.8
      *    �����ʤ�
           IF      ORCHC33-RC          =   1
               MOVE    "9002"              TO  SPA-ERRCD
           END-IF
      *
           .
       23011-PRINT-GAIRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���� ����ƣ�����������
      *****************************************************************
       23012-PRINT-NYUIN-SEC            SECTION.
      *
      *    ����ƣ����
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000004"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  ORCSPRTNM-SRYYMD(1:6)
           MOVE    "01"                TO  ORCSPRTNM-SRYYMD(7:2)
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-PRINT-PG
           ELSE
               MOVE    "ORCHCN33"          TO  WRK-PRINT-PG
           END-IF
      *
           INITIALIZE                  ORCHC33AREA
           MOVE    "1"                 TO  ORCHC33-KBN
           MOVE    SPA-GMN-PTNUM       TO  ORCHC33-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCHC33-PTID
           MOVE    SPA-GMN-NAME        TO  ORCHC33-NAME
           MOVE    SPA-GMN-BIRTHDAY    TO  ORCHC33-BIRTHDAY
           MOVE    SPA-GMN-SEX         TO  ORCHC33-SEX
           MOVE    SPA-NAI-SRYYM       TO  ORCHC33-SRYYM
           MOVE    SPACE               TO  ORCHC33-SRYYM-END
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCHC33-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  ORCHC33-SRYKA
      *H24.9
           MOVE    SPA-GMN-HHKNCOMBI   TO  ORCHC33-HKNCOMBI
           IF      FLG-PRINT           =   1
      *        ����
               MOVE    ZERO                TO  ORCHC33-PRINT-FLG
      *        ����������
               MOVE    SPA-J026-GMN-STRDD  TO  ORCHC33-STRDD
      *        �ǽ�������
               MOVE    SPA-J026-GMN-ENDPAGE    TO  ORCHC33-ENDPAGE
      *        �ǽ�������
               MOVE    SPA-J026-GMN-ENDLINE    TO  ORCHC33-ENDLINE
           ELSE
      *        Ƭ��
               MOVE    1                   TO  ORCHC33-PRINT-FLG
           END-IF
           MOVE    SYS-1030-KARUTE3KBN
                                       TO  ORCHC33-KARUTE3KBN
      *
           MOVE    "01"                TO  SPA-PRT-FLG
      *
           CALL    WRK-PRINT-PG        USING
                                       SPA-AREA
                                       ORCHC33AREA
      *
           IF      ORCHC33-RC      NOT =   ZERO
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
           IF      ORCHC33-RC          =   8
      *        �ץ�����४�ץ������ݸ�����Υ��顼
               MOVE    "9001"              TO  SPA-ERRCD
           END-IF
      *v4.8
      *    �����ʤ�
           IF      ORCHC33-RC          =   1
               MOVE    "9002"              TO  SPA-ERRCD
           END-IF
      *
           .
       23012-PRINT-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J026"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "J02"               TO  MCP-WINDOW
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
           IF      SPA-JIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J026"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
                   MOVE    "�������������㤤�ޤ���"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "�ǽ������Ԥϣ�����̤���ˤ��Ʋ�����"
                                       TO  SPA-ERRMSG
               WHEN    "9000"
                   MOVE    "�������顼�Ǥ���"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
                   STRING  "�ݸ��ȹ礻�Ǥΰ����Ȥʤ�ޤ���"
                           "�ݸ��ȹ礻���ǧ���Ʋ�������"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9002"
                   MOVE    "�����оݤ�����ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J026"              TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
                   MOVE    "����ƣ�����������ޤ�����������Ǥ�����"
                                       TO  WRK-JIDMSG
               WHEN    "0102"
                   MOVE    "����ƣ�����Ƭ���������ޤ���"
                                       TO  WRK-JIDMSG
                   MOVE    "��������Ǥ�����"
                                       TO  WRK-JIDMSG(33:)
      *
               WHEN    OTHER
                   MOVE    SPA-JIDCD
                                       TO  WRK-JIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-JID1-FLG
      *
           MOVE    SPACE               TO  JID1
           INITIALIZE                      JID1
           MOVE    SPA-JIDCD           TO  JID1-ID1CODE
           MOVE    WRK-JIDMSG          TO  JID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J026"              TO  SPA-MOTOPG
           MOVE    "JID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JID1"              TO  MCP-WINDOW.
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
      *    �����ƥ�����ޥ����ɤ߹��߽���
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
      *
           .
       900-SYSKANRI-READ-EXT.
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