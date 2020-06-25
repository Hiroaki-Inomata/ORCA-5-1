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
       PROGRAM-ID.                 ORCGJ022.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��ץ�����
      *  ����ݡ��ͥ��̾  : ������ϡʣʣ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/30  MONFUNC �б�
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      *  04.05.00    NACL-¿��Ǽ  09/06/29  ��������б�
      *  04.07.00    NACL-¿��Ǽ  11/10/XX  Ʊ���������б�
      *  04.08.00    NACL-¿��Ǽ  13/01/15  ���Ų���ơ��֥룱���б�
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
      *    ���̿���
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDZ2                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-ENDDD           PIC 9(02).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
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
      *    �ģ¸���
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
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
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "J022"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
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
      *
           PERFORM                     310-SPA-SET-SEC
      *
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    SPA-J022-MAX        TO  IDY
           INITIALIZE                  SPA-J022-AREA
      *
           MOVE    IDY                 TO  SPA-J022-MAX
      *
           MOVE    SPA-DAY-IDX         TO  SPA-J022-IDX
           MOVE    1                   TO  SPA-J022NAI-DAYCHK
                                                        (SPA-DAY-IDX)
     **    MOVE    SPA-GNAI-DAY (2 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY1 (SPA-DAY-IDX)
      *    MOVE    SPA-GNAI-DAY (3 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY2 (SPA-DAY-IDX)
      *    MOVE    SPA-GNAI-DAY (4 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY3 (SPA-DAY-IDX)
      *
      *    MOVE    SPA-GNAI-DAY (2 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY1
      *    MOVE    SPA-GNAI-DAY (3 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY2
      *    MOVE    SPA-GNAI-DAY (4 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY3
      *    IF     (SPA-J022-DAY1   >   ZERO )  AND
      *           (SPA-J022-MAX    <   1    )
      *        MOVE    1               TO  SPA-J022-MAX
      *    END-IF
      *    IF     (SPA-J022-DAY2   >   ZERO )  AND
      *           (SPA-J022-MAX    <   2    )
      *        MOVE    2               TO  SPA-J022-MAX
      *    END-IF
      *    IF     (SPA-J022-DAY3   >   ZERO )  AND
      *           (SPA-J022-MAX    <   3    )
      *        MOVE    3               TO  SPA-J022-MAX
      **** END-IF
      *ver.4.8
      *    �ơ��֥룹���б�
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING    IDX      FROM    2   BY  1
                   UNTIL      IDX      >   10
               ADD     1               TO  IDZ
               MOVE    SPA-GNAI-DAY (IDX SPA-DAY-IDX)
                                       TO  SPA-J022NAI-DAY
                                                   (SPA-DAY-IDX IDZ)
               MOVE    SPA-GNAI-DAY (IDX SPA-J022-IDX) 
                                       TO  SPA-J022-DAY (IDZ)
               IF      SPA-J022-DAY (IDZ)
                                       >   ZERO
                   MOVE    IDZ             TO  SPA-J022-MAX
               END-IF
           END-PERFORM
      *
      *    �����Ѵ�
           MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
           MOVE    SPA-J022-IDX        TO  WRK-SDD
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE  LNK-DAY2-EDTYMD3      TO  SPA-HENNYMD
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  J022
           INITIALIZE                      J022
      *
      *    �����Խ�
           MOVE    SPA-HENNYMD(11:)    TO  J022-SRYYMD
      *
           PERFORM VARYING    IDZ      FROM    1   BY  1
                   UNTIL      IDZ      >   9
               MOVE    SPA-J022-DAY (IDZ)  TO  J022-DAY (IDZ)
               MOVE    WIDGET-NORMAL       TO  J022-DAY-STATE (IDZ)
           END-PERFORM
      *    MOVE    SPA-J022-DAY2       TO  J022-DAY2
      *    MOVE    SPA-J022-DAY3       TO  J022-DAY3
      *
      *    MOVE    WIDGET-NORMAL       TO  J022-DAY1-STATE
      *    MOVE    WIDGET-NORMAL       TO  J022-DAY2-STATE
      *    MOVE    WIDGET-NORMAL       TO  J022-DAY3-STATE
           IF      SPA-NYUGAIKBN       =   "1"
      ******   MOVE    WIDGET-INSENSITIVE  TO  J022-DAY3-STATE
               MOVE    "���ౡʬ��"        TO  J022-MSG1
               MOVE    "������ʬ��"        TO  J022-MSG2
               PERFORM VARYING    IDZ      FROM    3   BY  1
                       UNTIL      IDZ      >   9
                   MOVE    WIDGET-INSENSITIVE  TO  J022-DAY-STATE (IDZ)
               END-PERFORM
           ELSE
      **       IF      SPA-J022-MAX        =   2
      *            MOVE    WIDGET-INSENSITIVE  TO  J022-DAY3-STATE
      ***      END-IF
               COMPUTE IDZ2                =   SPA-J022-MAX  +  1
               PERFORM VARYING    IDZ      FROM    IDZ2   BY  1
                       UNTIL      IDZ      >   9
                   MOVE    WIDGET-INSENSITIVE  TO  J022-DAY-STATE (IDZ)
               END-PERFORM
           END-IF
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
           IF      SPA-J022-CUR        =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "DAY1"
                       MOVE    2                   TO  SPA-J022-CUR
                   WHEN    "DAY2"
                       MOVE    3                   TO  SPA-J022-CUR
                       IF     (SPA-NYUGAIKBN       =   "1"  )
                       OR     (SPA-J022-MAX        =   2    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY3"
                       MOVE    4                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   3    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY4"
                       MOVE    5                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   4    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY5"
                       MOVE    6                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   5    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY6"
                       MOVE    7                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   6    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY7"
                       MOVE    8                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   7    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY8"
                       MOVE    9                   TO  SPA-J022-CUR
                       IF     (SPA-J022-MAX        =   8    )
                           MOVE    10                  TO  SPA-J022-CUR
                       END-IF
                   WHEN    "DAY9"
                       MOVE    10                  TO  SPA-J022-CUR
                   WHEN    OTHER
                       MOVE    1                   TO  SPA-J022-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-J022-CUR
               WHEN    1
                   MOVE    "DAY1"              TO  MCP-WIDGET
               WHEN    2
                   MOVE    "DAY2"              TO  MCP-WIDGET
               WHEN    3
                   MOVE    "DAY3"              TO  MCP-WIDGET
               WHEN    4
                   MOVE    "DAY4"              TO  MCP-WIDGET
               WHEN    5
                   MOVE    "DAY5"              TO  MCP-WIDGET
               WHEN    6
                   MOVE    "DAY6"              TO  MCP-WIDGET
               WHEN    7
                   MOVE    "DAY7"              TO  MCP-WIDGET
               WHEN    8
                   MOVE    "DAY8"              TO  MCP-WIDGET
               WHEN    9
                   MOVE    "DAY9"              TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 230-MODORU
      *        ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-KAKUTEI-SEC
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
           MOVE    ZERO                TO  SPA-J022-CUR
      *
           PERFORM 410-INPUT-CHK-SEC
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
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      ***  MOVE    J022-DAY1           TO  SPA-J022-DAY1
      *    MOVE    J022-DAY2           TO  SPA-J022-DAY2
      ***  MOVE    J022-DAY3           TO  SPA-J022-DAY3
           PERFORM VARYING    IDZ      FROM    1   BY  1
                   UNTIL      IDZ      >   9
               MOVE    J022-DAY (IDZ)      TO  SPA-J022-DAY (IDZ)
           END-PERFORM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܥ����å�����
      *****************************************************************
       4102-KIHON-CHK-SEC                   SECTION.
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ͽ������
      *****************************************************************
       220-KAKUTEI-SEC                   SECTION.
      *
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
      *
      ***  MOVE    SPA-J022-DAY1       TO  SPA-NAI-DAY1 (SPA-J022-IDX)
      *    MOVE    SPA-J022-DAY2       TO  SPA-NAI-DAY2 (SPA-J022-IDX)
      **** MOVE    SPA-J022-DAY3       TO  SPA-NAI-DAY3 (SPA-J022-IDX)
           PERFORM VARYING    IDZ      FROM    1   BY  1
                   UNTIL      IDZ      >   9
               MOVE    SPA-J022-DAY (IDZ)  TO  SPA-J022NAI-DAY
                                                     (SPA-J022-IDX IDZ)
           END-PERFORM
      *
           COMPUTE IDY                 =   SPA-J022-IDX  +  1
           MOVE    ZERO                TO  SPA-J022-IDX
           PERFORM VARYING     IDX     FROM    IDY BY 1
                   UNTIL       IDX     >   31
               IF      SPA-J022NAI-DAYCHK (IDX)    =   1
                   MOVE    IDX             TO  SPA-J022-IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-J022-IDX        =   ZERO
               PERFORM 210-BACK
           ELSE
      ***      MOVE    SPA-GNAI-DAY (2 SPA-J022-IDX)
      *                                TO  SPA-J022-DAY1
      *        MOVE    SPA-GNAI-DAY (3 SPA-J022-IDX)
      *                                TO  SPA-J022-DAY2
      *        MOVE    SPA-GNAI-DAY (4 SPA-J022-IDX)
      ****                             TO  SPA-J022-DAY3
               PERFORM VARYING    IDZ      FROM    1   BY  1
                       UNTIL      IDZ      >   9
                   COMPUTE IDX             =   IDZ     +   1
                   MOVE    SPA-GNAI-DAY (IDX SPA-J022-IDX)
                                           TO  SPA-J022-DAY (IDZ)
               END-PERFORM
      *
      *        �����Ѵ�
               MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
               MOVE    SPA-J022-IDX        TO  WRK-SDD
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                                  LNK-DAY2-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE  LNK-DAY2-EDTYMD3      TO  SPA-HENNYMD
               END-IF
           END-IF
           .
       220-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       230-MODORU                  SECTION.
      *
      *    �������ä������
      *    PERFORM VARYING     IDX     FROM    IDY BY 1
      *            UNTIL       IDX     >   31
      *        MOVE    ZERO            TO  SPA-NAI-DAYCHK (IDX)
      *    END-PERFORM
      *
           PERFORM 210-BACK
           .
       230-MODORU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J022"              TO  SPA-MOTOPG
      *
      *****MOVE   "BACK"               TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   "J02"                TO  MCP-WINDOW
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J022"              TO  MCP-WINDOW.
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���ϥ��顼"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J022"              TO  SPA-MOTOPG
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