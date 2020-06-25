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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGP982.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���긡��
      *  ����ݡ��ͥ��̾  : ����������̡�P982��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/04/04    MCC-¿��Ǽ    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  05/11/14  MONFUNC �б�
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      *  04.04.00    NACL-¿��Ǽ  08/12/03  ���꤫��͹���ֹ������ɲ�
      * 04.06.00     NACL-¿��Ǽ  11/02/10  �������̤�̾���Խ��б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ������Ͽ�Ѷ��̥ѥ�᥿
           COPY    "P02COMMON-SPA".
      *
      *    ���Բ��̥����ΰ�
           COPY    "P02SCR-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-ADRS            PIC 9(01).
           03  FLG-KENSAKU         PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SEYMD.
               05  WRK-SEYY        PIC 9(04).
               05  WRK-SEMM        PIC 9(02).
               05  WRK-SEDD        PIC 9(02).
      *
           03  WRK-TOWNNAME            PIC X(160).
           03  WRK-ADRSNAME            PIC X(162).
           03  WRK-MAX-CNT             PIC 9(04).
           03  WRK-Z9                  PIC ZZ9.
      *
           03  WRK-DBPATH              PIC X(64).
      *
           03  WRK-ERRMSG              PIC X(40).
      *
           03  WRK-P982-ROW        PIC S9(09).
           03  FLG-GMN-INIT        PIC  9(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *    �ʼ���Į������
           COPY    "CPSK1015.INC".
      *    ����ޥ���
       01  ADRS-REC.
           COPY    "CPADRS.INC".
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
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"         ALSO   "CLICKED"
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
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "PERR"
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
           PERFORM 5001-MAPCUR-SEC
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "P982"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
       300-SCREEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN982-SELNUM
           MOVE    ZERO                TO  SPA-NAI982-SELNUM
           MOVE    ZERO                TO  SPA-GMN982-NEXT
           MOVE    1                   TO  SPA-GMN982-PAGE
           MOVE    SPACE               TO  SPA-GMN982-KEYNAME
      *
           PERFORM 310-SPA-SET-SEC
      *    ����Τߤλ����оݤȤ���
           IF      SPA-GMN982-MAX          =   1
               MOVE    1                   TO  IDX
               MOVE    SPA-GMN982-TPOST   (IDX)
                                           TO  SPA-GMN982-POST
               MOVE    SPA-GMN982-TLPUBCD (IDX)
                                           TO  SPA-GMN982-LPUBCD
               MOVE    SPA-GMN982-TRENNUM (IDX)
                                           TO  SPA-GMN982-RENNUM
               MOVE    SPA-GMN982-TTOWNNAME (IDX)
                                           TO  SPA-GMN982-TOWNNAME
               MOVE    SPA-GMN982-TCITYNAMET (IDX)
                                           TO  SPA-GMN982-CITYNAMET
               MOVE    SPA-GMN982-TCITYNAMET2 (IDX)
                                           TO  SPA-GMN982-TOWNNAME
           END-IF
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
           MOVE    1                  TO  FLG-GMN-INIT
           IF      SPA-GMN982-KBN      =   2
      *        ���꤫�鸡��
               PERFORM 3102-ADRS-HEN-SEC
           ELSE
      *        ͹���ֹ������鸡��
               PERFORM 3101-POST-HEN-SEC
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ͹���ֹ�����������
      *****************************************************************
       3101-POST-HEN-SEC               SECTION.
      *
           INITIALIZE                      ADRS-REC
           IF      SPA-GMN982-LPUBCD(5:1)  =   SPACE
               STRING    SPA-GMN982-LPUBCD DELIMITED   BY  SPACE
                         "%"               DELIMITED   BY  SIZE
                                           INTO    ADRS-LPUBCD
               END-STRING
           ELSE
               MOVE    SPA-GMN982-LPUBCD       TO  ADRS-LPUBCD
           END-IF
           IF      SPA-GMN982-POST(7:1)    =   SPACE
               STRING    SPA-GMN982-POST   DELIMITED   BY  SPACE
                         "%"               DELIMITED   BY  SIZE
                                           INTO    ADRS-POST
               END-STRING
           ELSE
               MOVE    SPA-GMN982-POST         TO  ADRS-POST
           END-IF
      *    �����������Υ����ɤΤ�
           IF     (SPA-GMN982-LPUBCD   NOT =   SPACE )  AND
                  (SPA-GMN982-POST         =   SPACE )
               MOVE    "key3"              TO  WRK-DBPATH
           END-IF
      *    ͹���ֹ�Τ�
           IF     (SPA-GMN982-LPUBCD       =   SPACE )  AND
                  (SPA-GMN982-POST     NOT =   SPACE )
               MOVE    "key4"              TO  WRK-DBPATH
           END-IF
      *    ξ��
           IF     (SPA-GMN982-LPUBCD   NOT =   SPACE )  AND
                  (SPA-GMN982-POST     NOT =   SPACE )
               MOVE    "key5"              TO  WRK-DBPATH
           END-IF
      *
      *    ����ɽ�Խ�����
           PERFORM 3101-TBLHENSYU-SEC 
           .
       3101-POST-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ɽ�Խ�����
      *****************************************************************
       3101-TBLHENSYU-SEC               SECTION.
      *
           INITIALIZE                      SPA-GMN982-TBL-G
      *
           COMPUTE IDX-STR             =  (SPA-GMN982-PAGE  -  1 )
                                       *   100
                                       +   1
           MOVE    ZERO                TO  SPA-GMN982-NEXT
      *
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 900-ADRS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-ADRS
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN982-MAX
           PERFORM UNTIL    (FLG-ADRS          =   1   )
                       OR   (SPA-GMN982-NEXT   =   1   )
               ADD     1                   TO  IDX2
               IF      IDX2                >=  IDX-STR
                   ADD     1               TO  IDX
                   IF      IDX             >   100
                       MOVE    1               TO  SPA-GMN982-NEXT
                   ELSE
                       ADD     1               TO  SPA-GMN982-MAX
                       MOVE    IDX             TO  SPA-GMN982-SEQNO(IDX)
                       MOVE    ADRS-POST       TO  SPA-GMN982-TPOST(IDX)
                       MOVE    ADRS-LPUBCD     TO  SPA-GMN982-TLPUBCD
                                                                   (IDX)
                       MOVE    ADRS-EDITADRS-NAME
                                               TO  SPA-GMN982-TTOWNNAME
                                                                   (IDX)
                       MOVE    ADRS-EDANUM     TO  SPA-GMN982-TRENNUM
                                                                   (IDX)
      *                �̤�̾
                       MOVE    ADRS-TOWNNAME2  TO  SPA-GMN982-TTOWNNAME2
                                                                   (IDX)
      *                ��Į¼̾��Į��̾
                       STRING  ADRS-CITYNAME
                               ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    SPA-GMN982-TCITYNAMET
                                                                  (IDX)
                       END-STRING
                       IF      ADRS-TOWNNAME
                                           =   "�ʲ��˷Ǻܤ��ʤ����"
                           MOVE    SPACE       TO  SPA-GMN982-TCITYNAMET
                                                                  (IDX)
                           MOVE    ADRS-CITYNAME
                                               TO  SPA-GMN982-TCITYNAMET
                                                                  (IDX)
                       END-IF
                       MOVE    ADRS-EDITADRS-NAME
                                           TO  SPA-GMN982-TCITYNAMET2
                                                                   (IDX)
      *                ���Ԥ��̤�̾�Τ��Խ������
                       IF     (ADRS-LPUBCD(1:2)    =   "26"  )
                         AND  (ADRS-TOWNNAME2  NOT =   SPACE )
                         AND  (SPA-NAI-TOWNNAME2KBN-FLG    =   "1" )
                           IF      ADRS-TOWNNAME2(1:2)     =   "��" 
                               CONTINUE
                           ELSE
      *                        ��Į¼̾���̤�̾�Ρ�Į��̾
                               STRING  ADRS-CITYNAME
                                       ADRS-TOWNNAME2
                                       ADRS-TOWNNAME
                                                   DELIMITED  BY  SPACE
                                           INTO    SPA-GMN982-TCITYNAMET
                                                                  (IDX)
                               END-STRING
      *                        ��ƻ�ܸ�̾�ܻ�Į¼̾���̤�̾�Ρ�Į��̾
                               STRING  ADRS-PREFNAME
                                       ADRS-CITYNAME
                                       ADRS-TOWNNAME2
                                       ADRS-TOWNNAME
                                                   DELIMITED  BY  SPACE
                                           INTO  SPA-GMN982-TCITYNAMET2
                                                                  (IDX)
                               END-STRING
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 900-ADRS-READ-SEC
           END-PERFORM
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-TBLHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���꤫��θ�������
      *****************************************************************
       3102-ADRS-HEN-SEC               SECTION.
      *
           IF      SPA-GMN982-KEYNAME  =   SPACE
               PERFORM 31021-ADRS-INITHEN-SEC
           ELSE
      *        ���ڡ���
               INITIALIZE                      ADRS-REC
               MOVE    SPA-GMN982-KEYNAME  TO  ADRS-EDITADRS-NAME
               MOVE    "key8"              TO  WRK-DBPATH
      *        ����ɽ�Խ�����
               PERFORM 3101-TBLHENSYU-SEC
           END-IF
           .
       3102-ADRS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���꤫��θ�������
      *****************************************************************
       31021-ADRS-INITHEN-SEC              SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN982-TOWNNAME TO  KANACHK-MAE-INPUT
           MOVE    160                 TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-MAX-CNT
      *
           MOVE    SPA-GMN982-TOWNNAME TO  WRK-TOWNNAME
           MOVE    ZERO                TO  FLG-KENSAKU
           PERFORM UNTIL   FLG-KENSAKU     =   1
      *
               MOVE    SPACE               TO  WRK-ADRSNAME
               STRING  "%"
                       SPA-GMN982-TOWNNAME(1:WRK-MAX-CNT)
                                               DELIMITED  BY  SIZE
                       "%"                     DELIMITED  BY  SIZE
                                           INTO    WRK-ADRSNAME
               END-STRING
      *
               INITIALIZE                      ADRS-REC
               MOVE    WRK-ADRSNAME        TO  ADRS-EDITADRS-NAME
               MOVE    "key8"              TO  WRK-DBPATH
      *        ����ɽ�Խ�����
               PERFORM 3101-TBLHENSYU-SEC
               IF      SPA-GMN982-MAX      =   ZERO
                   IF      WRK-MAX-CNT     >   6
                       COMPUTE WRK-MAX-CNT     =   WRK-MAX-CNT   -   2
                   ELSE
                       MOVE    1               TO  FLG-KENSAKU
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-KENSAKU
                   MOVE    WRK-ADRSNAME    TO  SPA-GMN982-KEYNAME
               END-IF
           END-PERFORM
           .
       31021-ADRS-INITHEN-EXT.
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
                   INITIALIZE                  SPA-GMN982-KEY
                   MOVE    SPACE               TO  SPA-GMN982-TOWNNAME
                   MOVE    SPACE               TO  SPA-GMN982-CITYNAMET
                   PERFORM 210-BACK
      *        ����
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 260-MAE-PAGE-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 220-NEXT-HEN-SEC
      *        ͹���ֹ�Τ�
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 2100-B10-SYORI-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 2120-B12-SYORI-SEC
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
      *        ��������
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-SELNUM-CHK-SEC
      *        ������
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 41012-LISTSEL-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    �ֹ����򡡽���
      *****************************************************************
       41011-SELNUM-CHK-SEC                SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    P982-SELNUM         TO  SPA-GMN982-SELNUM
      *
      *    �����ֹ�����
           INITIALIZE                      ORCSNUMAREA
           MOVE    P982-SELNUM         TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41011-SELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  SPA-NAI982-SELNUM
           END-IF
      *
      *    ����ΤߤȤ������Ȥ���
           IF     (SPA-GMN982-MAX      =   1  )  AND
                  (SPA-NAI982-SELNUM   =   ZERO)
               MOVE    1               TO  SPA-NAI982-SELNUM
           END-IF
      *
           IF      SPA-NAI982-SELNUM   <   1   OR
                                       >   SPA-GMN982-MAX
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-NAI982-SELNUM   TO  WRK-Z9
               MOVE    WRK-Z9              TO  SPA-GMN982-SELNUM
               IF      SPA-GMN982-KBN      =   2
                   MOVE    2                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    SPA-NAI982-SELNUM   TO  IDX
                   MOVE    SPA-GMN982-TPOST   (IDX)
                                           TO  SPA-GMN982-POST
                   MOVE    SPA-GMN982-TLPUBCD (IDX)
                                           TO  SPA-GMN982-LPUBCD
                   MOVE    SPA-GMN982-TRENNUM (IDX)
                                           TO  SPA-GMN982-RENNUM
                   MOVE    SPA-GMN982-TTOWNNAME (IDX)
                                           TO  SPA-GMN982-TOWNNAME
      *
                   MOVE    SPA-GMN982-TCITYNAMET (IDX)
                                           TO  SPA-GMN982-CITYNAMET
                   MOVE    SPA-GMN982-TCITYNAMET2(IDX)
                                           TO  SPA-GMN982-TOWNNAME
      *
                   PERFORM 210-BACK
               END-IF
           END-IF
           .
       41011-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *        ������
           MOVE    ZERO                TO  SPA-NAI982-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   100
               IF      P982-SELECT (IDX)    =   "T"
                   MOVE    SPA-GMN982-SEQNO (IDX)
                                            TO  SPA-NAI982-SELNUM
                   MOVE    200              TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-NAI982-SELNUM   >   ZERO
               MOVE    SPA-NAI982-SELNUM   TO  P982-SELNUM
               PERFORM 41011-SELNUM-CHK-SEC
           END-IF
           .
       41012-LISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ͹���ֹ�Τ߽���
      *****************************************************************
       2100-B10-SYORI-SEC       SECTION.
      *
           IF      SPA-NAI982-SELNUM   <   1   OR
                                       >   SPA-GMN982-MAX
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-NAI982-SELNUM   TO  IDX
               MOVE    SPA-GMN982-TPOST   (IDX)
                                           TO  SPA-GMN982-POST
               MOVE    SPACE               TO  SPA-GMN982-TOWNNAME
               MOVE    SPACE               TO  SPA-GMN982-CITYNAMET
               PERFORM 210-BACK
           END-IF
           .
       2100-B10-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    �������
      *****************************************************************
       2120-B12-SYORI-SEC       SECTION.
      *
           IF      SPA-NAI982-SELNUM   <   1   OR
                                       >   SPA-GMN982-MAX
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-NAI982-SELNUM   TO  IDX
               MOVE    SPA-GMN982-TPOST   (IDX)
                                           TO  SPA-GMN982-POST
               MOVE    SPA-GMN982-TLPUBCD (IDX)
                                           TO  SPA-GMN982-LPUBCD
               MOVE    SPA-GMN982-TRENNUM (IDX)
                                           TO  SPA-GMN982-RENNUM
               MOVE    SPA-GMN982-TTOWNNAME (IDX)
                                           TO  SPA-GMN982-TOWNNAME
               MOVE    SPA-GMN982-TCITYNAMET (IDX)
                                           TO  SPA-GMN982-CITYNAMET
               MOVE    SPA-GMN982-TCITYNAMET2(IDX)
                                           TO  SPA-GMN982-TOWNNAME
      *
               PERFORM 210-BACK
           END-IF
           .
       2120-B12-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       220-NEXT-HEN-SEC       SECTION.
      *
           IF      SPA-GMN982-NEXT     =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
           ELSE
               ADD     1               TO  SPA-GMN982-PAGE
               PERFORM 310-SPA-SET-SEC
           END-IF
           .
       220-NEXT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       260-MAE-PAGE-SEC       SECTION.
      *
           IF      SPA-GMN982-PAGE     =   1
               MOVE    "0004"          TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-GMN982-PAGE     =   SPA-GMN982-PAGE  -  1
               PERFORM 310-SPA-SET-SEC
           END-IF
           .
       260-MAE-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
      *****MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    SPA-GMN982-MOTOPG   TO  SPA-SAKIPG
           MOVE    "P982"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P982"              TO  MCP-WINDOW.
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
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-GMN982-MAX      =   ZERO )
               MOVE    ZERO                TO  WRK-P982-ROW
           ELSE
               MOVE    P982-ROW            TO  WRK-P982-ROW
           END-IF
      *
           MOVE    SPACE               TO  P982
           INITIALIZE                      P982
           IF      WRK-P982-ROW    NOT =   ZERO
               MOVE    WRK-P982-ROW         TO  P982-ROW
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN982-MAX
               MOVE    SPA-GMN982-SEQNO (IDX)  TO  WRK-Z9
               MOVE    WRK-Z9                  TO  P982-SEQNO   (IDX)
               MOVE    SPA-GMN982-TPOST (IDX)  TO  P982-POST   (IDX)
               MOVE    SPA-GMN982-TLPUBCD (IDX)
                                               TO  P982-LPUBCD (IDX)
               MOVE    SPA-GMN982-TTOWNNAME (IDX)
                                               TO  P982-TOWNNAME(IDX)
               MOVE    SPA-GMN982-TTOWNNAME2(IDX)
                                               TO  P982-TOWNNAME2(IDX)
               IF      SPA-GMN982-SEQNO (IDX)  =   SPA-NAI982-SELNUM
                   MOVE    "T"                 TO  P982-SELECT (IDX)
               ELSE
                   MOVE    "F"                 TO  P982-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN982-MAX      TO  P982-COUNT
      *
           MOVE    SPA-GMN982-SELNUM   TO  P982-SELNUM
      *
           IF      SPA-GMN982-NEXT     =   1
               IF      SPA-GMN982-PAGE     =   1
                   MOVE    "���Ǥ���"          TO  P982-NEXT-VALUE
                   MOVE    "red"               TO  P982-NEXT-STYLE
               ELSE
                   MOVE    "�������Ǥ���"      TO  P982-NEXT-VALUE
                   MOVE    "red"               TO  P982-NEXT-STYLE
               END-IF
           ELSE
               IF      SPA-GMN982-PAGE     >   1
                   MOVE    "���Ǥ���"          TO  P982-NEXT-VALUE
                   MOVE    "red"               TO  P982-NEXT-STYLE
               END-IF
           END-IF
      *    ����ǽ
           IF      SPA-GMN982-KBN      =   2
               MOVE    WIDGET-NORMAL       TO  P982-B10-STATE
               MOVE    WIDGET-NORMAL       TO  P982-B12-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  P982-B10-STATE
               MOVE    WIDGET-INSENSITIVE  TO  P982-B12-STATE
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
               WHEN    01
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���ϥ��顼"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "�����ֹ����ϥ��顼"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "���ڡ����Ϥ���ޤ���"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "�����ڡ����Ϥ���ޤ���"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "�оݤΥǡ�������������ʾ夢��ޤ�"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P982"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ޥ������ɹ�����
      *****************************************************************
       900-ADRS-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ADRS-REC
               MOVE    ZERO                TO  FLG-ADRS
           ELSE
               INITIALIZE                      ADRS-REC
               MOVE    1                   TO  FLG-ADRS
           END-IF
      *
           .
       900-ADRS-READ-EXT.
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