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
       PROGRAM-ID.             ORCGP02S.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/02/XX    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      *  04.08.00    NACL-¿��Ǽ  15/06/16  ���Ųʡ��ݸ��ȹ礻����б�
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
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
      *
           03  FLG-HEN             PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PIDMSG          PIC X(100).
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
      *    ���Ų��ܾ���
           COPY    "CPSK1005.INC".
      *    ��ʥơ��֥�
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
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
      *    ����󥢥������ѥѥ�᥿
           01  ORCSSYUACCAREA.
               COPY    "CPORCSSYUACC.INC".
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
           INITIALIZE                  STS-AREA
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
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "P02S"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
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
               WHEN    "PID1"
                   IF      SPA-PID1-FLG        =   SPACE
                       CONTINUE
                   ELSE
                       PERFORM 4902-KOUSIN-SYORI-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  SPA-PIDCD
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
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
           INITIALIZE                      SPA-P02S-AREA
      *    �����ƥ�ǯ�����ɽ���Ȥ���
           MOVE    SPA-SYSYMD (1:6)    TO  SPA-P02S-NAI-SRYYM
           MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-P02S-GMN-SRYYM
      *    ����������Ѥ�
           IF      SPA-GMN-PTID        =   ZERO
               IF      SPA-NAI-SYUKAYM     NOT =   SPACE
                   MOVE    SPA-NAI-SYUKAYM     TO  SPA-P02S-NAI-SRYYM
                   MOVE    SPA-NAI-SYUKAYMG    TO  SPA-P02S-GMN-SRYYM
               END-IF
           END-IF
      *
      *    ��ʰ����Խ�
           PERFORM 3101-SYUKA-HEN-SEC
      *
           IF      SPA-GMN-PTID        =   ZERO
      *        ������Ͽ������������
               MOVE    SPACE               TO  SPA-P02S-SYUKA-G
               MOVE    SPA-NAI-SYUKA       TO  SPA-P02S-SYUKA
           ELSE
               PERFORM 3102-SYUKASUB-CALL-SEC
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02S-SYUKA-MAX
               IF      SPA-P02S-SYUKA  =   SPA-P02S-SYUKA-LST(IDX)(1:2)
                   MOVE    SPA-P02S-SYUKA-LST(IDX)
                                       TO  SPA-P02S-SYUKA-G
                   MOVE    SPA-P02S-SYUKA-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-P02S-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ʰ����Խ�����
      *****************************************************************
       3101-SYUKA-HEN-SEC              SECTION.
      *
           INITIALIZE                      SPA-P02S-SYUKA-COB
           MOVE    ZERO                TO  SPA-P02S-SYUKA-MAX
      *    ���Ųʰ���
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-P02S-NAI-SRYYM  TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING IDX
                   FROM    1       BY  1
      *********    UNTIL      (IDX             >   40    )   OR
                   UNTIL      (IDX             >   99    )   OR
                              (FLG-SYSKANRI    =   1     )
               MOVE    MCPDATA-REC             TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)    TO  SPA-P02S-SYUKA-LST
                                                        (IDX)
               MOVE    SYS-1005-SRYKANAME1     TO  SPA-P02S-SYUKA-LST
                                                        (IDX)(4:)
               MOVE    IDX                     TO  SPA-P02S-SYUKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-SYUKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ʸ������ֽ���
      *****************************************************************
       3102-SYUKASUB-CALL-SEC              SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
      *    ������ʬ
           MOVE    "01"                TO  LNK-SYUACC-I-KBN
      *    ����ǯ��
           MOVE    SPA-P02S-NAI-SRYYM  TO  LNK-SYUACC-I-SRYYM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
      *    ���Ų�(�����Ͼ���)
           MOVE    SPACE               TO  SPA-P02S-SYUKA-G
           IF       LNK-SYUACC-RC      =   ZERO
               MOVE    LNK-SYUACC-IO-H-SRYKA   TO  SPA-P02S-SYUKA
           END-IF
           .
       3102-SYUKASUB-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      P02S
      *
           MOVE    SPA-P02S-GMN-SRYYM  TO  P02S-SRYYM
      *
           MOVE    SPA-P02S-SYUKA-G    TO  P02S-SYUKA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02S-SYUKA-MAX
               MOVE    SPA-P02S-SYUKA-LST(IDX)
                                       TO  P02S-SYUKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-P02S-SYUKA-MAX  TO  P02S-SYUKA-COUNT
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
         EVALUATE    SPA-P02S-CUR
              WHEN    01
                  MOVE    "SRYYM"       TO  MCP-WIDGET
              WHEN    02
                  MOVE    "SYUKA"       TO  MCP-WIDGET
              WHEN    03
                  MOVE    "B12"          TO  MCP-WIDGET
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
                   MOVE    SPACE               TO  MCP-PUTTYPE
                   PERFORM 210-BACK
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
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 4001-SRYYM-SEC
               WHEN    "ACTIVATE"      ALSO    "SYUKA"
                   PERFORM 4002-SYUKA-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    ����ǯ�����
      *****************************************************************
       4001-SRYYM-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-P02S-CUR
           MOVE    P02S-SRYYM          TO  SPA-P02S-GMN-SRYYM
      *
           MOVE    ZERO                TO  FLG-HEN
      *    ���ղ��̥����å�����
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P02S-GMN-SRYYM  TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               IF      SGDAY-SDAY (1:6)    =   SPA-P02S-NAI-SRYYM
                   MOVE    ZERO            TO  FLG-HEN
               ELSE
                   MOVE    1               TO  FLG-HEN
               END-IF
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-P02S-GMN-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-P02S-NAI-SRYYM
      *        ������ʹߤǤ��뤳��
               IF      SPA-P02S-NAI-SRYYM  <   SPA-NAI-BIRTHDAY(1:6)
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-P02S-CUR
               END-IF
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P02S-CUR
               MOVE    SPACE               TO  SPA-P02S-NAI-SRYYM
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-HEN         =   1
                   PERFORM 40011-SRYYM-HEN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-P02S-CUR
           END-IF
      *
           .
       4001-SRYYM-EXT.
           EXIT.
      *****************************************************************
      *    ����ǯ���ѹ�����
      *****************************************************************
       40011-SRYYM-HEN-SEC             SECTION.
      *
      *    ��ʰ����Խ�
           PERFORM 3101-SYUKA-HEN-SEC
      *    ����Խ�
           PERFORM 3102-SYUKASUB-CALL-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02S-SYUKA-MAX
               IF      SPA-P02S-SYUKA  =   SPA-P02S-SYUKA-LST(IDX)(1:2)
                   MOVE    SPA-P02S-SYUKA-LST(IDX)
                                       TO  SPA-P02S-SYUKA-G
                   MOVE    SPA-P02S-SYUKA-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           .
       40011-SRYYM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ��ʽ���
      *****************************************************************
       4002-SYUKA-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-P02S-CUR
           MOVE    P02S-SYUKA          TO  SPA-P02S-SYUKA-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-P02S-SYUKA-MAX)
                          OR  (FLG-OK  =   1   )
               IF      SPA-P02S-SYUKA  =   SPA-P02S-SYUKA-LST(IDX)(1:2)
                   MOVE    SPA-P02S-SYUKA-LST(IDX)
                                               TO  SPA-P02S-SYUKA-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-P02S-SYUKA      =   SPACE
               MOVE    SPACE               TO  SPA-P02S-SYUKA-G
               MOVE    1                   TO  FLG-OK
           END-IF
           IF      FLG-OK              =   1
               MOVE    3                   TO  SPA-P02S-CUR
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
      *
           .
       4002-SYUKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *    �����������å�
           IF      SPA-P02S-NAI-SRYYM  =   SPACE
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P02S-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4002-SYUKA-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-P02-SYORI       =   1  ) AND
                      (SPA-GMN-PTID        =   ZERO)
      *            ������Ͽ�����ӣУ���
                   PERFORM 4901-SINKI-SYORI-SEC
               ELSE
                   MOVE    "0101"          TO  SPA-PIDCD
               END-IF
           END-IF
      *
          .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������Ͽ����
      *****************************************************************
       4901-SINKI-SYORI-SEC           SECTION.
      *
           MOVE    SPA-P02S-SYUKA       TO  SPA-NAI-SYUKA
           MOVE    SPA-P02S-NAI-SRYYM   TO  SPA-NAI-SYUKAYM
           MOVE    SPA-P02S-GMN-SRYYM   TO  SPA-NAI-SYUKAYMG
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4901-SINKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       4902-KOUSIN-SYORI-SEC           SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
      *    ������ʬ
           MOVE    "02"                TO  LNK-SYUACC-I-KBN
      *    ����ǯ��
           MOVE    SPA-P02S-NAI-SRYYM  TO  LNK-SYUACC-I-SRYYM
           MOVE    SPA-P02S-SYUKA      TO  LNK-SYUACC-IO-H-SRYKA
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
           IF       LNK-SYUACC-RC  NOT =   ZERO
               MOVE    "1000"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-NAI-SYUKA
               MOVE    SPACE               TO  SPA-NAI-SYUKAYM
               PERFORM 210-BACK
           END-IF
           .
       4902-KOUSIN-SYORI-EXT.
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
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P02S"              TO  MCP-WINDOW
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
                   MOVE    "����ǯ�����ϥ��顼"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "��ʤ�����ޤ���"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "����ǯ���ǯ������Ǥ���"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "����ǯ������Ϥ��Ʋ�������"
                                           TO  SPA-ERRMSG
               WHEN    "1000"
                   MOVE    "�ޥ����������Ǥ��ޤ���Ǥ�����"
                                           TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02S"              TO  SPA-MOTOPG
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
      *    ���顼��å�����ɽ����
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   MOVE    "��ʤ���Ͽ���ޤ�����������Ǥ�����"
                                               TO  WRK-PIDMSG
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "���"              TO  PID1-B01
           MOVE    "�ϣ�"              TO  PID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P02S"              TO  SPA-MOTOPG
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
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02S"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
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
      *    �����ƥ�����ޥ������ңţ���
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
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