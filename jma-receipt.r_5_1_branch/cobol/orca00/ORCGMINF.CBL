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
       PROGRAM-ID.                 ORCGMINF.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��˥塼
      *  ����ݡ��ͥ��̾  : �����ɥ����٥����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/12/04    NACL�ݿ���    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      * 04.06.00     NACL-��ƣ    10/02/13  �ꥢ�륿��������
      *                                     ���ɽ�������������
      *                                     �������������
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    �����ΰ�
           COPY    "M00COMMON-SPA".
      *    �����ΰ�
           COPY    "M01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  SPA-XA01.
           03  SPA-MINF-AREA.
               05  SPA-MINF-TAISYOYMD          PIC X(22).
               05  SPA-NAI-TAISYOYMD           PIC X(08).
               05  SPA-MINF-TBL.
                   07  SPA-MINF-OCC            OCCURS  100.
                       09  SPA-MINF-TYMD       PIC X(6).
                       09  SPA-MINF-TBYOGEN    PIC X(22).
                       09  SPA-MINF-TNAME      PIC X(18).
                       09  SPA-MINF-TSEX       PIC X(2).
                       09  SPA-MINF-TSHINRYO   PIC X(2).
                       09  SPA-MINF-TYAKUZAI   PIC X(2).
                       09  SPA-MINF-TBYOMEI    PIC X(60).
                       09  SPA-MINF-TSTRYMD    PIC X(10).
               05  SPA-MINF-MAX                PIC 9(04).
               05  SPA-MINF-SRYYMD             PIC X(09).
               05  SPA-MINF-STR                PIC 9(04).
               05  SPA-MINF-END                PIC 9(01).
               05  SPA-MINF-FLG                PIC 9(01).
      *
      *    �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
           COPY  "CPMKPASS.INC".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-LIST-ROW        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-INFECT          PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PATH            PIC X(64).
           03  WRK-LIST-ROW        PIC S9(9)   BINARY.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02). 
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
       01  INFECT-REC.
           COPY    "CPINFECTION.INC".
      *
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ�����
           COPY    "CPORCSGDAY.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC".
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-M01
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        ��������
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "MINF"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
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
           EVALUATE    SPA-MOTOPG
      *    ��ɽ����
               WHEN    "MID1"
                   PERFORM 330-MID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
      *    ¾�Σ̣Ĥ��
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
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
           MOVE    1               TO  SPA-MINF-FLG
           MOVE    1               TO  SPA-MINF-STR
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           IF      SPA-DATAINFECTIONKBN    =   2
      *        ����
               PERFORM 31012-ZENJITU-HEN-SEC
           END-IF
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-MINF-TAISYOYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-TAISYOYMD
      *
           PERFORM 320-LIST-SET-SEC
      *
           MOVE    1               TO  FLG-LIST-ROW
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ꥹ�ȥ��åȽ���
      *****************************************************************
       320-LIST-SET-SEC            SECTION.
      *
           INITIALIZE                  INFECT-REC
      *
           EVALUATE    SPA-MINF-FLG
               WHEN    1
                   MOVE    SPA-NAI-TAISYOYMD   TO  INFECT-SRYYMD
                   MOVE    "key3"              TO  WRK-PATH
               WHEN    2
                   MOVE    SPA-NAI-TAISYOYMD   TO  INFECT-SENDYMD
                   MOVE    "key2"              TO  WRK-PATH
           END-EVALUATE
      *
           MOVE    SPA-HOSPNUM     TO  INFECT-HOSPNUM
           MOVE    INFECT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_infection" TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-INFECT-FET-SEC
           ELSE 
               MOVE    1               TO  FLG-INFECT
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL ( FLG-INFECT  =   1 )
                   OR    ( IDX         >=  100 )
               IF      IDY         >=  SPA-MINF-STR
                   ADD     1                   TO  IDX
                   EVALUATE    SPA-MINF-FLG
                       WHEN    1
                           MOVE    INFECT-SENDYMD  TO  WRK-SYMD
                       WHEN    2
                           MOVE    INFECT-SRYYMD   TO  WRK-SYMD
                   END-EVALUATE
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD1(5:5)
                                               TO  SPA-MINF-TYMD(IDX)
                   IF      INFECT-DISEASE  =   1
                       MOVE    "����ե륨��"
                                               TO  SPA-MINF-TBYOGEN(IDX)
                   ELSE
                       MOVE    INFECT-DISEASE  TO  SPA-MINF-TBYOGEN(IDX)
                   END-IF
                   PERFORM 900-PTINF-READ-SEC
                   MOVE    PTINF-NAME          TO  SPA-MINF-TNAME(IDX)
                   EVALUATE    PTINF-SEX
                       WHEN    "1"
                           MOVE    "��"        TO  SPA-MINF-TSEX(IDX)
                       WHEN    "2"
                           MOVE    "��"        TO  SPA-MINF-TSEX(IDX)
                   END-EVALUATE
                   IF      INFECT-SRYCD    NOT =   SPACE
                       MOVE    "��"        TO  SPA-MINF-TSHINRYO(IDX)
                   ELSE
                       MOVE    SPACE       TO  SPA-MINF-TSHINRYO(IDX)
                   END-IF
                   IF      INFECT-MEDSRYCD NOT =   SPACE
                       MOVE    "��"        TO  SPA-MINF-TYAKUZAI(IDX)
                   ELSE
                       MOVE    SPACE       TO  SPA-MINF-TYAKUZAI(IDX)
                   END-IF
                   PERFORM 900-BYOMEI-READ-SEC
                   MOVE    BYO-BYOMEI          TO  SPA-MINF-TBYOMEI(IDX)
                   MOVE    INFECT-BYOSRYYMD    TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD1    TO  SPA-MINF-TSTRYMD(IDX)
               END-IF
               PERFORM 900-INFECT-FET-SEC
           END-PERFORM
      *
           MOVE    IDX                 TO  SPA-MINF-MAX
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_infection" TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      FLG-INFECT      =   ZERO
               MOVE    ZERO            TO  SPA-MINF-END
           ELSE
               MOVE    1               TO  SPA-MINF-END
           END-IF
      *
           .
       320-LIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣãɣģ��ˣϣ˽���
      *****************************************************************
       330-MID1-SET-SEC            SECTION.
      *
           IF      SPA-MID1-FLG        =   "OK"
               CONTINUE
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       330-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        ��λ
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE   "CHANGE"         TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        ɽ������
               WHEN    "CLICKED"   ALSO    "B04"
                   PERFORM 210-KIRIKAE-SEC
      *        ����
               WHEN    "CLICKED"   ALSO    "B05"
                   PERFORM 210-ZENJITU-SEC
      *        ����
               WHEN    "CLICKED"   ALSO    "B06"
                   PERFORM 210-ZENPAGE-SEC
      *        ����
               WHEN    "CLICKED"   ALSO    "B07"
                   PERFORM 210-JIPAGE-SEC
      *        ����
               WHEN    "CLICKED"   ALSO    "B08"
                   PERFORM 210-JIJITU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        ������
               WHEN    "ACTIVATE"  ALSO    "SRYYMD"
                   PERFORM 201-SRYYMD-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����������
      *****************************************************************
       201-SRYYMD-SEC              SECTION.
      *
           MOVE    MINF-SRYYMD     TO  SPA-MINF-SRYYMD
      *
           IF      MINF-SRYYMD         =   SPACE
               GO  TO  201-SRYYMD-EXT
           END-IF
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    SPA-MINF-SRYYMD TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-MINF-TAISYOYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-TAISYOYMD
               MOVE    SPACE           TO  SPA-MINF-SRYYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 320-LIST-SET-SEC
           END-IF
      *
           .
       201-SRYYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M98"           TO  SPA-SAKIPG
           MOVE    "MINF"          TO  SPA-MOTOPG
      *
      *    MOVE    SPACE           TO  SPA-WORK
      *    INITIALIZE                  SPA-KYOTUKEY
      *                                SPA-KYOTU-SET
      *    MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ؽ���
      *****************************************************************
       210-KIRIKAE-SEC             SECTION.
      *
           EVALUATE    SPA-MINF-FLG
               WHEN    1
                   MOVE    2               TO  SPA-MINF-FLG
               WHEN    2
                   MOVE    1               TO  SPA-MINF-FLG
           END-EVALUATE
      *
           MOVE    1               TO  SPA-MINF-STR
      *
           PERFORM 320-LIST-SET-SEC
      *
           MOVE    1               TO  FLG-LIST-ROW
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       210-KIRIKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       210-ZENJITU-SEC             SECTION.
      *
           MOVE    SPA-NAI-TAISYOYMD   TO  WRK-SYMD
           PERFORM 31012-ZENJITU-HEN-SEC
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-MINF-TAISYOYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-TAISYOYMD
      *
           IF      SPA-ERRCD       =   SPACE
               PERFORM 320-LIST-SET-SEC
           END-IF
      *
           .
       210-ZENJITU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       210-ZENPAGE-SEC             SECTION.
      *
           COMPUTE SPA-MINF-STR
               =   SPA-MINF-STR     -   100
           PERFORM 320-LIST-SET-SEC
      *
           .
       210-ZENPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       210-JIPAGE-SEC              SECTION.
      *
           COMPUTE SPA-MINF-STR
               =   SPA-MINF-STR     +   100
           PERFORM 320-LIST-SET-SEC
      *
           .
       210-JIPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       210-JIJITU-SEC              SECTION.
      *
           MOVE    SPA-NAI-TAISYOYMD   TO  WRK-SYMD
           PERFORM 31012-JIJITU-HEN-SEC
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-MINF-TAISYOYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-TAISYOYMD
      *
           IF      SPA-ERRCD       =   SPACE
               PERFORM 320-LIST-SET-SEC
           END-IF
      *
           .
       210-JIJITU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Ѵ��Խ�����
      *****************************************************************
       5002-HIZUKE-HEN-SEC         SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
      *
           .
       5002-HIZUKE-HEN-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "MINF     "         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    MINF-ROW        TO  WRK-LIST-ROW
      *
           INITIALIZE  MINF
      *
           EVALUATE    FLG-LIST-ROW
               WHEN    1
                   MOVE    ZERO            TO  MINF-ROW
               WHEN    OTHER
                   MOVE    WRK-LIST-ROW    TO  MINF-ROW
           END-EVALUATE
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX             >   SPA-MINF-MAX
               MOVE    SPA-MINF-TYMD(IDX)      TO  MINF-YMD(IDX)
               MOVE    SPA-MINF-TBYOGEN(IDX)   TO  MINF-BYOGEN(IDX)
               MOVE    SPA-MINF-TNAME(IDX)     TO  MINF-NAME(IDX)
               MOVE    SPA-MINF-TSEX(IDX)      TO  MINF-SEX(IDX)
               MOVE    SPA-MINF-TSHINRYO(IDX)  TO  MINF-SHINRYO(IDX)
               MOVE    SPA-MINF-TYAKUZAI(IDX)  TO  MINF-YAKUZAI(IDX)
               MOVE    SPA-MINF-TBYOMEI(IDX)   TO  MINF-BYOMEI(IDX)
               MOVE    SPA-MINF-TSTRYMD(IDX)   TO  MINF-STRYMD(IDX)
           END-PERFORM
      *
           MOVE    SPA-MINF-MAX    TO  MINF-COUNT
      *
           EVALUATE    SPA-MINF-FLG
               WHEN    1
                   MOVE    "������"    TO  MINF-LABEL
               WHEN    2
                   MOVE    "�����"    TO  MINF-LABEL
           END-EVALUATE
      *
           MOVE    SPA-MINF-TAISYOYMD  TO  MINF-TAISYOYMD
           MOVE    SPA-MINF-SRYYMD     TO  MINF-SRYYMD
      *
           IF      SPA-MINF-STR    =   1
               MOVE    WIDGET-INSENSITIVE  TO  MINF-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  MINF-B06-STATE
           END-IF
      *
           IF      SPA-MINF-END    =   1
               MOVE    WIDGET-INSENSITIVE  TO  MINF-B07-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  MINF-B07-STATE
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
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SRYYMD"        TO  MCP-WIDGET
           END-EVALUATE
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
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���դ�����������ޤ���"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  MERR
           MOVE    SPA-ERRCD           TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  MERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "MINF"              TO  SPA-MOTOPG
           MOVE    "MERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
     *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       31012-ZENJITU-HEN-SEC       SECTION.
      *
      *    ����
           MOVE    SPACE           TO  STS-AREA-DAY
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY6-KIJUN
           MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
           MOVE    -1              TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                        STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
      *
           .
       31012-ZENJITU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       31012-JIJITU-HEN-SEC        SECTION.
      *
      *    ����
           MOVE    SPACE           TO  STS-AREA-DAY
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY6-KIJUN
           MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
           MOVE    1               TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
      *
           .
       31012-JIJITU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܸ��Ѵ�����
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "��"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ɥץ롼���ɤ߹���
      *****************************************************************
       900-INFECT-FET-SEC          SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_infection" TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INFECT-REC
               MOVE    ZERO            TO  FLG-INFECT
           ELSE
               MOVE    1               TO  FLG-INFECT
           END-IF
      *
           .
       900-INFECT-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����̾�ɹ�
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    SPACE           TO  PTINF-REC
           MOVE    INFECT-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    INFECT-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��̾�ɹ�
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    SPACE           TO  BYOMEI-REC
           MOVE    INFECT-BYOMEICD TO  BYO-BYOMEICD
           MOVE    BYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_byomei"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-BYOMEI
                   MOVE    MCPDATA-REC     TO  BYOMEI-REC
               ELSE
                   MOVE    1               TO  FLG-BYOMEI
               END-IF
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.