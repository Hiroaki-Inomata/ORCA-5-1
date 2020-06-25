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
       PROGRAM-ID.             ORCGP02A.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �����Ͽ�����������̡ʣУ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC-����      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/11  MONFUNC �б�
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
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
      *    ������Ͽ�����ΰ�
           COPY    "P02COMMON-SPA".
      *    ���Բ��̥����ΰ�
           COPY    "P02SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
      *
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
           03  WRK-PIDMSG          PIC X(100).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-RENNUMZ         PIC ZZZZ.
      *
           03  WRK-MCP-PATHNAME      PIC X(64).
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
      *    ���Ծ���
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *    ����ʸ���ɲ�
           COPY    "CPORCSADDSIGN.INC".
      *
      *    �ģ¸���
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
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
           MOVE    SPA-P02KYOUTU     TO  SPA-WORK
           MOVE    SPA-AREA          TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA    TO  SPA-FREE
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "PERR"
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
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "P02A"              TO  MCP-WINDOW.
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
           EVALUATE    SPA-MOTOPG
      *        ��ǧ��å�����
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����Խ�����
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
      *    �ӣУ��Խ�
           MOVE    ZERO                TO  SPA-GMN2A-MAX
      *
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-KANANAME    TO  PTINF-KANANAME
           IF     (SPA-P02A-SYORI          =   2  )  AND
                  (SPA-NAI-JYUFUKU-CHKKBN  =   "2")
               MOVE    SPA-NAI-BIRTHDAY    TO  PTINF-BIRTHDAY
               MOVE    SPA-GMN-SEX         TO  PTINF-SEX
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           ELSE
      *        LIKE����
               INITIALIZE                  ORCSADDSIGNAREA
               MOVE    PTINF-KANANAME      TO  SADDSIGN-STR
               CALL    "ORCSADDSIGN"       USING
                                           ORCSADDSIGNAREA
               MOVE    SADDSIGN-STR        TO  PTINF-KANANAME
      *
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM   UNTIL  ( FLG-PTINF    =   1  )
                     OR     ( IDY          >=  20 )
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-CHK
               IF      SPA-P02A-SYORI      =   1
      *            ���ʻ�̾�Τ߰���
                   IF     (PTINF-KANANAME  =   SPA-GMN-KANANAME ) AND
                          (PTINF-PTID  NOT =   SPA-GMN-PTID     )
                       MOVE    1               TO  FLG-CHK
                   END-IF
               ELSE
                   EVALUATE    SPA-NAI-JYUFUKU-CHKKBN
                       WHEN    "0"
      *                    ��̾�����̡���ǯ��������
                           IF     (PTINF-KANANAME  =   SPA-GMN-KANANAME)
                             AND  (PTINF-SEX       =   SPA-GMN-SEX )
                             AND  (PTINF-BIRTHDAY  =   SPA-NAI-BIRTHDAY)
                             AND  (PTINF-PTID  NOT =   SPA-GMN-PTID   )
                               MOVE    1               TO  FLG-CHK
                           END-IF
                       WHEN    "1"
      *                    ��̾��������̾�����̡���ǯ��������
                           IF     (PTINF-KANANAME  =   SPA-GMN-KANANAME)
                             AND  (PTINF-NAME      =   SPA-GMN-NAME    )
                             AND  (PTINF-SEX       =   SPA-GMN-SEX )
                             AND  (PTINF-BIRTHDAY  =   SPA-NAI-BIRTHDAY)
                             AND  (PTINF-PTID  NOT =   SPA-GMN-PTID   )
                               MOVE    1               TO  FLG-CHK
                           END-IF
                       WHEN    "2"
      *                    ���̡���ǯ��������
                           IF     (PTINF-SEX       =   SPA-GMN-SEX )
                             AND  (PTINF-BIRTHDAY  =   SPA-NAI-BIRTHDAY)
                             AND  (PTINF-PTID  NOT =   SPA-GMN-PTID   )
                               MOVE    1               TO  FLG-CHK
                           END-IF
                   END-EVALUATE
               END-IF
      *
               IF      FLG-CHK             =   1
                   ADD     1               TO  IDY
                   MOVE    IDY             TO  SPA-GMN2A-NUM      (IDY)
                   MOVE    PTINF-PTID      TO  SPA-GMN2A-PTID     (IDY)
      *
      *            �����ֹ��Ѵ�(�����ˤ򸡺���������ID��괵��NO�����
                   INITIALIZE                      ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                   MOVE    PTINF-PTID          TO  SPTID-PTID
                   CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                   SPA-AREA
                   IF      SPTID-RC        NOT =   ZERO
                       MOVE    SPACE           TO  SPA-GMN2A-PTNUM(IDY)
                   ELSE
                       MOVE    SPTID-PTNUM     TO  SPA-GMN2A-PTNUM(IDY)
                   END-IF
                   MOVE    PTINF-NAME          TO  SPA-GMN2A-NAME (IDY)
      *************MOVE    PTINF-HOME-ADRS     TO  SPA-GMN2A-ADRS (IDY)
                   STRING  PTINF-HOME-ADRS     DELIMITED  BY  SPACE
                           PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                           INTO    SPA-GMN2A-ADRS (IDY)
                   END-STRING
                   MOVE    PTINF-HOME-TEL1     TO  SPA-GMN2A-TEL1 (IDY)
      *            ��ǯ����
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    PTINF-BIRTHDAY      TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1
                                               TO  SPA-GMN2A-BIRTHDAY
                                                                 (IDY)
                   ELSE
                       MOVE    PTINF-BIRTHDAY  TO  SPA-GMN2A-BIRTHDAY
                                                                 (IDY)
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           END-PERFORM
           MOVE    IDY                 TO  SPA-GMN2A-MAX
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̤�����
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   IF      SPA-PID1-FLG        =   "OK"
      *                �����ִ�
                       MOVE    SPA-GMN2A-PTNUM(SPA-GMN2A-SELNUM)
                                               TO  SPA-NAI2A-PTNUM
                   ELSE
                       MOVE    SPACE           TO  SPA-NAI2A-PTNUM
                   END-IF
                   PERFORM 210-BACK
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P02A
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-GMN2A-MAX
               MOVE    SPA-GMN2A-NUM   (IDX)  TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ            TO  P02A-NUM   (IDX)
               MOVE    SPA-GMN2A-PTNUM (IDX)  TO  P02A-PTNUM (IDX)
               MOVE    SPA-GMN2A-NAME  (IDX)  TO  P02A-NAME  (IDX)
               MOVE    SPA-GMN2A-ADRS  (IDX)  TO  P02A-ADRS  (IDX)
               MOVE    SPA-GMN2A-TEL1  (IDX)  TO  P02A-TEL   (IDX)
               MOVE    SPA-GMN2A-BIRTHDAY  (IDX)
                                              TO  P02A-BIRTHDAY(IDX)
               IF      IDX                 =   SPA-GMN2A-SELNUM
                   MOVE    "T"             TO  P02A-DUALLIST-SELECT
                                                               (IDX)
               ELSE
                   MOVE    "F"             TO  P02A-DUALLIST-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN2A-MAX       TO  P02A-DUALLIST-COUNT
           IF      SPA-GMN2A-MAX       >   ZERO
               IF      SPA-P02A-SYORI      =   1
                   MOVE    "Ʊ��Ʊ̾�ԥꥹ��"      TO  P02A-MIDASI
               ELSE
                   MOVE    "�����Ͽ�Ե����ꥹ��"  TO  P02A-MIDASI
               END-IF
           END-IF
      *
           MOVE    SPA-GMN2A-SELNUM    TO  P02A-SELNUM
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
           EVALUATE    SPA-GMN2A-CUR
               WHEN    01
                   MOVE    "B01"       TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM"    TO  MCP-WIDGET
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
                   PERFORM 210-BACK
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
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN2A-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        �ꥹ������
               WHEN    ANY             ALSO    "DUALLIST"
                   PERFORM 4001-DUALIST-SEL-SEC
               WHEN    OTHER
      *            ���ϥ����å�����
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻�������
      *****************************************************************
       4001-DUALIST-SEL-SEC          SECTION.
      *
      *    ������
           MOVE    ZERO                TO  SPA-GMN2A-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN2A-MAX
               IF      P02A-DUALLIST-SELECT (IDX)    =   "T"
                   MOVE    IDX             TO  SPA-GMN2A-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN2A-SELNUM    NOT =   ZERO
               PERFORM 4102-SELNUM-CHK-SEC
           END-IF
      *
           .
      *
       4001-DUALIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4102-SELNUM-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02A-SELNUM         TO  SPA-GMN2A-SELNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����ֹ����
      *****************************************************************
       4102-SELNUM-CHK-SEC                 SECTION.
      *
           MOVE    2                   TO  SPA-GMN2A-CUR
      *
      *    �����λ��Τ��ִ�����Ԥ�
           IF      SPA-P02-SYORI       =   1
               IF     (SPA-GMN2A-SELNUM    =   ZERO )  OR
                      (SPA-GMN2A-SELNUM    >   SPA-GMN2A-MAX)
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                  MOVE    "0101"          TO  SPA-PIDCD
               END-IF
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
           .
      *
       4102-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02A"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
      *****MOVE    "BACK"              TO  MCP-PUTTYPE.
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "P02"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    ZERO                TO  SPA-FLG-END
           MOVE    "P02"               TO  SPA-P02-GAMEN
      *
           .
       210-BACK-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P02A"              TO  MCP-WINDOW.
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
                   MOVE    "�����ֹ����ϥ��顼"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "����Ͽ�������Ͽ�λ��Τ����ϤǤ��ޤ�"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02A"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   MOVE    "���Ԥ��ѹ����ޤ������ߤδ����ֹ��"
                                               TO  WRK-PIDMSG
                   MOVE    "���֤Ȥʤ�ޤ�����������Ǥ�����"
                                               TO  WRK-PIDMSG(35:)
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
           MOVE    "P02A"              TO  SPA-MOTOPG
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
      *****************************************************************
      **    ���ԥޥ������ɤ߹���
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
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