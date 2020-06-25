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
       PROGRAM-ID.             ORCGP014.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �����ֹ�ȯ�ԡʳ�ĥ��P014��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  01/12/05    MCC-¿��Ǽ    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/11  MONFUNC �б� ¾
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      *  05.00.00    NACL-¿��Ǽ  18/10/XX  �����ֹ�����������å��ɲ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ������Ͽ�����ΰ�
           COPY    "P02COMMON-SPA".
      *    �У������̥����ΰ�
           COPY    "P02SCR-SPA".
      *    ������ɽ��
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTGETNUM        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-KANRIMST        PIC 9(01).
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
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(02).
           03  IDX-LINE            PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-KNJID           PIC 9(10).
           03  WRK-END             PIC 9(04).
      *
           03  WRK-WAYMD.
               05  WRK-WAGO         PIC 9(01).
               05  WRK-WAYY         PIC 9(02).
               05  WRK-WAMM         PIC 9(02).
               05  WRK-WADD         PIC 9(02).
      *
           03  WRK-SEYMD.
               05  WRK-SEYY         PIC 9(04).
               05  WRK-SEMM         PIC 9(02).
               05  WRK-SEDD         PIC 9(02).
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
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1009.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �����ֹ��Ѵ��ʴ�����
       01  PTNUM-REC.
           COPY  "CPPTNUM.INC".
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
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *   �����å��ǥ��å�
           COPY    "CPORCCHKDGT.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *   �����ֹ�ȯ������
           COPY    "CPORCSP01.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
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
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "PERR"
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
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "P014"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
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
           EVALUATE    SPA-1009-KKCKSIKBN
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ� + �ե꡼>
               WHEN    "1"
                   MOVE    1                   TO  SPA-GMN014-CUR
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ�>
               WHEN    "2"
                   MOVE    1                   TO  SPA-GMN014-CUR
      *        ��ĥ���� <Ϣ�ֹ� + �ե꡼>
               WHEN    "3"
                   MOVE    2                   TO  SPA-GMN014-CUR
           END-EVALUATE
      *
           
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      P014
      *
           MOVE    SPA-GMN014-PTNUM1   TO  P014-PTNUM1
           MOVE    SPA-GMN014-PTNUM2   TO  P014-PTNUM2
           MOVE    SPA-GMN014-PTNUM3   TO  P014-PTNUM3
      *
      *    ��ĥ������ʬ
           MOVE    WIDGET-INSENSITIVE  TO  P014-PTNUM1-STATE
           MOVE    WIDGET-INSENSITIVE  TO  P014-PTNUM2-STATE
           MOVE    WIDGET-INSENSITIVE  TO  P014-PTNUM3-STATE
      *
           EVALUATE    SPA-1009-KKCKSIKBN
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ� + �ե꡼>
               WHEN    "1"
                   MOVE    WIDGET-NORMAL       TO  P014-PTNUM1-STATE
                   MOVE    WIDGET-NORMAL       TO  P014-PTNUM3-STATE
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ�>
               WHEN    "2"
                   MOVE    WIDGET-NORMAL       TO  P014-PTNUM1-STATE
      *        ��ĥ���� <Ϣ�ֹ� + �ե꡼>
               WHEN    "3"
                   MOVE    WIDGET-NORMAL       TO  P014-PTNUM2-STATE
           END-EVALUATE
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
         EVALUATE    SPA-GMN014-CUR
              WHEN    01
                  MOVE    "PTNUM1"       TO  MCP-WIDGET
              WHEN    02
                  MOVE    "PTNUM2"       TO  MCP-WIDGET
              WHEN    03
                  MOVE    "PTNUM3"       TO  MCP-WIDGET
              WHEN    04
                  MOVE    "B12"          TO  MCP-WIDGET
      *----(01.00.01) LINE ADD START -------------------------------
              WHEN    05
                  MOVE    "B04"          TO  MCP-WIDGET
      *----(01.00.01) LINE ADD END   -------------------------------
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
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        ��ȯ��
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 420-SAIHAKKOU-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "PTNUM1"
                   PERFORM 4001-PTNUM1-SEC
               WHEN    "ACTIVATE"      ALSO    "PTNUM2"
                   PERFORM 4002-PTNUM2-SEC
               WHEN    "ACTIVATE"      ALSO    "PTNUM3"
                   PERFORM 4003-PTNUM3-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    �ֹ棱����
      *****************************************************************
       4001-PTNUM1-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN014-CUR
           MOVE    P014-PTNUM1         TO  SPA-GMN014-PTNUM1
      *
      *    ��������å�
           MOVE    ZERO              TO  WRK-END
      *H30.10
           PERFORM   VARYING   IDX   FROM    20  BY  -1
                     UNTIL    (IDX           <   1    )
                         OR   (WRK-END       >   ZERO )
               IF      (SPA-GMN014-PTNUM1(IDX:1) NOT =   SPACE)
                   MOVE    IDX                 TO  WRK-END
               END-IF
           END-PERFORM
      *
      *    ��ĥ��������������å�
           IF      WRK-END         NOT =   SPA-1009-KKCKSIMAEKETA
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     SPA-GMN014-PTNUM1(1:1)   =   "*"
                   MOVE    "0004"              TO  SPA-ERRCD
               END-IF
           END-IF
      *H30.10
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-END )
                         OR   (SPA-ERRCD   NOT =   SPACE)
      *        \ �ϥ��顼
               IF      SPA-GMN014-PTNUM1(IDX:1)    =   "\"
                  MOVE    "1013"           TO  SPA-ERRCD
               END-IF
      *        ���򥨥顼
               IF      SPA-GMN014-PTNUM1(IDX:1)    =   SPACE
                  MOVE    "0001"           TO  SPA-ERRCD
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4001-PTNUM1-EXT
           END-IF
      *
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-1009-KKCKSIKBN
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ� + �ե꡼>
                   WHEN    "1"
                       MOVE    3               TO  SPA-GMN014-CUR
                   WHEN    OTHER
                       MOVE    4               TO  SPA-GMN014-CUR
               END-EVALUATE
           END-IF
           .
       4001-PTNUM1-EXT.
           EXIT.
      *****************************************************************
      *    �ֹ棲����
      *****************************************************************
       4002-PTNUM2-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN014-CUR
           MOVE    P014-PTNUM2         TO  SPA-GMN014-PTNUM2
      *
      *    ��������å�
           MOVE    ZERO              TO  WRK-END
           PERFORM   VARYING   IDX   FROM    20  BY  -1
                     UNTIL    (IDX           <   1    )
                         OR   (WRK-END       >   ZERO )
               IF      (SPA-GMN014-PTNUM2(IDX:1) NOT =   SPACE)
                   MOVE    IDX                 TO  WRK-END
               END-IF
           END-PERFORM
      *    ��ĥ�������������å�
           IF      WRK-END         NOT =   SPA-1009-KKCKSIATOKETA
               MOVE    "0001"          TO  SPA-ERRCD
      ***  ELSE
      ***      MOVE    4               TO  SPA-GMN014-CUR
           END-IF
      *H30.10
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-END )
                         OR   (SPA-ERRCD   NOT =   SPACE)
      *        \ �ϥ��顼
               IF      SPA-GMN014-PTNUM2(IDX:1)    =   "\"
                  MOVE    "1013"           TO  SPA-ERRCD
               END-IF
      *        ���򥨥顼
               IF      SPA-GMN014-PTNUM2(IDX:1)    =   SPACE
                  MOVE    "0001"           TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    4               TO  SPA-GMN014-CUR
           END-IF
      *
           .
       4002-PTNUM2-EXT.
           EXIT.
      *****************************************************************
      *    �ֹ棳����
      *****************************************************************
       4003-PTNUM3-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-GMN014-CUR
           MOVE    P014-PTNUM3         TO  SPA-GMN014-PTNUM3
      *
      *    ��������å�
           MOVE    ZERO              TO  WRK-END
           PERFORM   VARYING   IDX   FROM    20  BY  -1
                     UNTIL    (IDX           <   1 )
                           OR (WRK-END       >   ZERO )
               IF      (SPA-GMN014-PTNUM3(IDX:1) NOT =   SPACE)
                   MOVE    IDX                 TO  WRK-END
               END-IF
           END-PERFORM
      *
      *    ��ĥ�������������å�
           IF      WRK-END         NOT =   SPA-1009-KKCKSIATOKETA
               MOVE    "0001"          TO  SPA-ERRCD
      ***  ELSE
      ***      MOVE    4               TO  SPA-GMN014-CUR
           END-IF
      *
      *H30.10
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-END )
                         OR   (SPA-ERRCD   NOT =   SPACE)
      *        \ �ϥ��顼
               IF      SPA-GMN014-PTNUM3(IDX:1)    =   "\"
                  MOVE    "1013"           TO  SPA-ERRCD
               END-IF
      *        ���򥨥顼
               IF      SPA-GMN014-PTNUM3(IDX:1)    =   SPACE
                  MOVE    "0001"           TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    4               TO  SPA-GMN014-CUR
           END-IF
           .
       4003-PTNUM3-EXT.
           EXIT.
      *****************************************************************
      *    ��ȯ�Խ���
      *****************************************************************
       420-SAIHAKKOU-SEC             SECTION.
      *
           MOVE    5                   TO  SPA-GMN014-CUR
      *
           INITIALIZE                  ORCSP01AREA
           MOVE    "3"                 TO  ORCSP01-SYORI
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
           MOVE    SPA-GMN-PTNUM       TO  ORCSP01-PTNUM
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      420-SAIHAKKOU-EXT
           END-IF
      *
           EVALUATE    SPA-1009-KKCKSIKBN
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ� + �ե꡼>
               WHEN    "1"
                   MOVE    ORCSP01-PTNUM2      TO  SPA-GMN014-PTNUM2
                   MOVE    1                   TO  SPA-GMN014-CUR
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ�>
               WHEN    "2"
                   MOVE    ORCSP01-PTNUM2      TO  SPA-GMN014-PTNUM2
                   MOVE    1                   TO  SPA-GMN014-CUR
      *        ��ĥ���� <Ϣ�ֹ� + �ե꡼>
               WHEN    "3"
                   MOVE    ORCSP01-PTNUM1      TO  SPA-GMN014-PTNUM1
                   MOVE    2                   TO  SPA-GMN014-CUR
           END-EVALUATE
      *
           MOVE    4                   TO  SPA-GMN014-CUR
           .
       420-SAIHAKKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *
      *    �����������å�
           EVALUATE    SPA-1009-KKCKSIKBN
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ� + �ե꡼>
               WHEN    "1"
                   PERFORM 4001-PTNUM1-SEC
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4003-PTNUM3-SEC
                   END-IF
      *        ��ĥ���� <�ե꡼ + Ϣ�ֹ�>
               WHEN    "2"
                   PERFORM 4001-PTNUM1-SEC
      *        ��ĥ���� <Ϣ�ֹ� + �ե꡼>
               WHEN    "3"
                   PERFORM 4002-PTNUM2-SEC
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4901-KAKUTEI-HEN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        ���
               PERFORM 210-BACK
           END-IF
      *
          .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ��Խ�����
      *****************************************************************
       4901-KAKUTEI-HEN-SEC           SECTION.
      *
      *    �����ֹ��Խ�
           MOVE    SPACE               TO  SPA-GMN014-PTNUM
           STRING  SPA-GMN014-PTNUM1   DELIMITED  BY  " "
                   SPA-GMN014-PTNUM2   DELIMITED  BY  " "
                   SPA-GMN014-PTNUM3   DELIMITED  BY  " "
                   INTO                SPA-GMN014-PTNUM
           END-STRING
      *
      *    �����ֹ��Ѵ�(�����ˤ򸡺�
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    SPA-GMN014-PTNUM    TO  SPTID-PTNUM
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
      *        ���˴����ֹ椬��Ͽ����Ƥ�����
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    05                  TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-GMN014-PTNUM
           END-IF
      *
          .
       4901-KAKUTEI-HEN-EXT.
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
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P014"              TO  MCP-WINDOW.
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
                   MOVE    "����Ȱ㤤�ޤ�"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "�����ֹ椬�����Ǥ��ޤ���Ǥ�����"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "�����ܤ� �� ������Ǥ��ޤ���"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "������Ͽ�Ѥδ����ֹ�Ǥ�"  TO  SPA-ERRMSG
      *H30.10
               WHEN    "1013"
                   MOVE    "�����ֹ�˻��ѤǤ��ʤ�ʸ��������ޤ���"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P014"              TO  SPA-MOTOPG
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
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
      *
           MOVE    "P014"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "P02"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
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