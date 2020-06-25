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
       PROGRAM-ID.                 ORCGK052.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ���Ź԰٥��åȰ���ɽ���ϡʣˣ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  01/04/23    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/17  MONFUNC �б�
      *  03.04.00    NACL-¿��Ǽ  07/03/13  �в��������ɲ�
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *  04.07.00    NACL-¿��Ǽ  12/01/06  ���饤����Ȱ����б�
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
           COPY    "K01COMMON-SPA".
      *
      *    ���̥����ΰ�
           COPY    "K02SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
      *    ���饤����Ȱ���ͭ
      **** 03  FLG-CLIENT-OK       PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
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
      *
           03  WRK-STR-SETCD       PIC X(06).
           03  WRK-END-SETCD       PIC X(06).
      *
      *
           03  WRK-KIDMSG              PIC X(70).
      *
           03  WRK-PATH            PIC X(64).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ϥ����ɥޥ�����
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    ���ϥ��åȥ�����
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
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
      *   ���������Ѵ�����
           COPY    "CPORCSGDAY.INC".
      *
      *    ���������Ѵ��ΰ�
           COPY    "CPORCHCSET.INC".
      *
      *    ���饤����Ȱ�������
           COPY    "CPORCSPRTCTRL.INC".
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
      *
       01  SCRAREA.
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
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
      *    ���饤����Ȱ���
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
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
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "K052"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
      *    �������̤��
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
                   PERFORM 3001-KID1-SYORI-SEC
                   MOVE    SPACE               TO  SPA-KIDCD
               WHEN    OTHER
                   PERFORM 310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-K052-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̤��ϣ˽���
      *****************************************************************
       3001-KID1-SYORI-SEC          SECTION.
      *
            EVALUATE    SPA-KIDCD
      *         ������ǧ�ϣ�
                WHEN    "0001"
                    IF      SPA-KID1-FLG        =   "OK"
      *                ����ɽ������
                        PERFORM 3001-PRINT-SYORI-SEC
                    ELSE
                        MOVE    1               TO  SPA-K052-CUR
                    END-IF
           END-EVALUATE
      *
           .
       3001-KID1-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-K052-AREA
      *
           MOVE    SPACE               TO  SPA-K052-INPUTCD1
           MOVE    ALL "9"             TO  SPA-K052-INPUTCD2
           MOVE    SPACE               TO  SPA-K052-KEIKAYMD
           MOVE    SPACE               TO  SPA-K052-NAI-KEIKAYMD
      *
           MOVE    1                   TO  SPA-K052-CUR
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
           MOVE    SPACE               TO  K052
           INITIALIZE                  K052
      *
      *    �����Խ�
           MOVE    SPA-K052-INPUTCD1    TO  K052-INPUTCD1
           MOVE    SPA-K052-INPUTCD2    TO  K052-INPUTCD2
           MOVE    SPA-K052-KEIKAYMD    TO  K052-KEIKAYMD
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
           IF      SPA-K052-CUR        =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "INPUTCD1"
                       MOVE    2               TO  SPA-K052-CUR
                   WHEN    "INPUTCD2"
                       MOVE    3               TO  SPA-K052-CUR
                   WHEN    "KEIKAYMD"
                       MOVE    9               TO  SPA-K052-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-K052-CUR
               WHEN    1
                   MOVE    "INPUTCD1"          TO  MCP-WIDGET
               WHEN    2
                   MOVE    "INPUTCD2"          TO  MCP-WIDGET
               WHEN    3
                   MOVE    "KEIKAYMD"          TO  MCP-WIDGET
               WHEN    9
                   MOVE    "B09"               TO  MCP-WIDGET
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
                   PERFORM 210-BACK
      *        ���ꥢ
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 310-SPA-SET-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 300-PRINT-SEC
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
           MOVE    ZERO                TO  SPA-K052-CUR
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
       410-INPUT-CHK-SEC          SECTION.
      *
           MOVE    K052-INPUTCD1       TO  SPA-K052-INPUTCD1
           MOVE    K052-INPUTCD2       TO  SPA-K052-INPUTCD2
           MOVE    K052-KEIKAYMD       TO  SPA-K052-KEIKAYMD
      *
      *    ���åȥ��������ϣ�
           PERFORM 2001-INPUTCD1-SEC
      *
      *    ���åȥ��������ϣ�
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2002-INPUTCD2-SEC
           END-IF
      *    �в�������
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2002-KEIKAYMD-SEC
           END-IF
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���åȥ��������Ͻ���
      *****************************************************************
       2001-INPUTCD1-SEC                SECTION.
      *
           IF     (SPA-K052-INPUTCD1       =   SPACE  )  OR
                  (SPA-K052-INPUTCD1(1:1)  =   "S"    OR
                                               "P"    )
               CONTINUE
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-K052-CUR
           END-IF
      *
           .
       2001-INPUTCD1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���åȥ��������Ͻ���
      *****************************************************************
       2002-INPUTCD2-SEC                SECTION.
      *
           IF      SPA-K052-INPUTCD2   =   SPACE
               MOVE    ALL "9"             TO  SPA-K052-INPUTCD2
           END-IF
           IF      SPA-K052-INPUTCD2(1:1)  =   "9"    OR
                                               "S"    OR
                                               "P"
               CONTINUE
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K052-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K052-INPUTCD2   NOT =   ALL "9"
                   IF      SPA-K052-INPUTCD1   >  SPA-K052-INPUTCD2
                       MOVE    "0002"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-K052-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       2002-INPUTCD2-EXT.
           EXIT.
      *
      *****************************************************************
      *    �в����ֽ���
      *****************************************************************
       2002-KEIKAYMD-SEC                   SECTION.
      *
           IF      SPA-K052-KEIKAYMD   =   SPACE
               MOVE    SPACE               TO  SPA-K052-NAI-KEIKAYMD
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-K052-KEIKAYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-K052-KEIKAYMD
                   MOVE    SGDAY-SDAY          TO  SPA-K052-NAI-KEIKAYMD
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-K052-CUR
               END-IF
           END-IF
      *
           .
       2002-KEIKAYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����������
      *****************************************************************
       300-PRINT-SEC                   SECTION.
      *
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      300-PRINT-EXT
           END-IF
      *
      *    �оݷ�������å�
           IF      SPA-K052-INPUTCD1   =   SPACE
               MOVE    "P     "            TO  WRK-STR-SETCD
           ELSE
               MOVE    SPA-K052-INPUTCD1   TO  WRK-STR-SETCD
           END-IF
           IF      SPA-K052-INPUTCD2   =   ALL "9"
               MOVE    "SXXXXX"            TO  WRK-END-SETCD
           ELSE
               MOVE    SPA-K052-INPUTCD2   TO  WRK-END-SETCD
           END-IF
      *
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-STR-SETCD       TO  ISET-SETCD
           MOVE    WRK-END-SETCD       TO  ISET-UPHMS
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-INPUTSET        =   1
      *        �оݤʤ�
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
      *        ����ɽ������
               MOVE    "0001"              TO  SPA-KIDCD
           END-IF
      *
           MOVE    1                   TO  SPA-K052-CUR
      *
           .
       300-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ɽ��������
      *****************************************************************
       3001-PRINT-SYORI-SEC            SECTION.
      *
      *    ������������������
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        ������������������
      ***      CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *        ���饤����Ȱ�����������������
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
      *    �оݷ�������å�
           IF      SPA-K052-INPUTCD1   =   SPACE
               MOVE    "P     "            TO  WRK-STR-SETCD
           ELSE
               MOVE    SPA-K052-INPUTCD1   TO  WRK-STR-SETCD
           END-IF
           IF      SPA-K052-INPUTCD2   =   ALL "9"
               MOVE    "SXXXXX"            TO  WRK-END-SETCD
           ELSE
               MOVE    SPA-K052-INPUTCD2   TO  WRK-END-SETCD
           END-IF
      *
           INITIALIZE                  ORCHCSETAREA
           MOVE    WRK-STR-SETCD       TO  HCSET-STR-SETCD
           MOVE    WRK-END-SETCD       TO  HCSET-END-SETCD
           MOVE    SPA-K052-NAI-KEIKAYMD   TO  HCSET-KEIKAYMD
      *
           MOVE    "01"                TO  SPA-PRT-FLG
      *    �ư����ʤ�
           MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *
           CALL    "ORCHCSET"          USING   SPA-AREA
                                               ORCHCSETAREA
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        �������ߡ�����
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    SPACE               TO  SPA-PRT-FLG
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
           .
       3001-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���饤����Ȱ�����������
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      ***  INITIALIZE                  KDMY01
      **** MOVE    SPACE               TO  KDMY01-PANDAPRINT1
      *    ���饤����Ȱ�����������������
           INITIALIZE                  ORCSPRTCTRLAREA
      *****MOVE    "���åȰ���ɽ"      TO  ORCSPRTCTRL-TITLE-I(01)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *****MOVE    ORCSPRTCTRL-OUT     TO  KDMY01-PANDAPRINT1
      *
      **** MOVE    1                   TO  FLG-CLIENT-OK
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���饤����Ȱ������̽���
      *****************************************************************
      *600-CLIENT-DMY-SEC              SECTION.
      *
      *    MOVE    MCP-WINDOW          TO  SPA-SAKIPG
      *    MOVE    MCP-PUTTYPE         TO  SPA-PUTTYPE
      *
      *    MOVE    "_KDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      *    PERFORM 900-PUT-WINDOW
      *    .
      *600-CLIENT-DMY-EXT.
      *    EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "K05"               TO  SPA-SAKIPG
           MOVE    "K052"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "K05"               TO  MCP-WINDOW
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
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K052"              TO  MCP-WINDOW.
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
                   MOVE    "���åȥ����ɤ����Ϥ��Ʋ�������"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "���ϥ����ɡ佪λ�����ɤǤ���"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "���åȥ����ɤ��оݥǡ���������ޤ���"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "�в��������������顼"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K052"              TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
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
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0001"
                   MOVE    "�����򳫻Ϥ��ޤ�����������Ǥ�����"
                                       TO  WRK-KIDMSG
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "���"              TO  KID1-B01
           MOVE    "�ϣ�"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K052"              TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���ϥ��åȥǡ������ɹ�
      *****************************************************************
       900-INPUTSET-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-READ-EXT.
           EXIT.
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