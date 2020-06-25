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
       PROGRAM-ID.                 ORCSSKJHKN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �����ݸ�������ʿ������ǯ������ʹߡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  17/10/04    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-TANKI2                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
           03  IDX-TK3                     PIC 9(02).
      *
      *    ���󥹥������
       01  CONSTANT-AREA.
      *    �����ݸ��ȹ礻����
           03   CONS-HKN-SITEI             PIC X(09) VALUE "099999930".
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNSYU                  PIC X(01).
      *
           03  WRK-NACCT-DAY9              PIC 9(04)   OCCURS 31.
           03  WRK-NACCT-DAY               PIC X(04)   OCCURS 31.
      *
      *    Ʊ����������������������
           03  WRK-DOUJITSU-YMD            PIC X(08).
           03  WRK-DOUJITSU-YMDR    REDEFINES   WRK-DOUJITSU-YMD.
               05  WRK-DOUJITSU-YYMM       PIC 9(06).
               05  WRK-DOUJITSU-DD         PIC 9(02).
      *
      *    Ʊ����������������������
           03  WRK-YOKUJITSU-YMD          PIC 9(08).
           03  WRK-YOKUJITSU-YMDR   REDEFINES   WRK-YOKUJITSU-YMD.
               05  WRK-YOKUJITSU-YYMM      PIC 9(06).
               05  WRK-YOKUJITSU-DD        PIC 9(02).
      *
      *    ������������
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *    û���ں߰ʳ������������ꤷ�Ƥ�����
           03  WRK-TANKI-IGAI              PIC X(01)  OCCURS 31.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �������Ų�ץޥ�����
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    �������Ź԰٥ޥ�����
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ���Ų�ץޥ�����
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ���Ź԰٥ޥ�����
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ������������ޥ�����
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ���ԥޥ�����
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �ݸ��Ȥ߹�碌
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    �����ޥ�����
           COPY    "CPTENSU.INC".
      *
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSSKJHKN.INC".
      *    ������̤��������������
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSSKJHKN-AREA
           ORCSNYUINDAY-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-SKJHKN-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-SKJHKN-RC
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-SKJHKN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    �����
           IF      LNK-SKJHKN-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *     PERFORM    VARYING IDX   FROM   1   BY  1
      *                UNTIL IDX   >   31
      *       DISPLAY "IDX = " IDX
      *               " NYUIN-HKNCOMBI = "
      *                LNK-SKJHKN-HKNCOMBINUM(IDX 1)
      *               " HKNSYU = "
      *                LNK-SKJHKN-HKNSYU(IDX 1)
      *               " DOUJITSU-HKNCOMBI = "
      *                LNK-SKJHKN-HKNCOMBINUM(IDX 2)
      *               " HKNSYU = "
      *                LNK-SKJHKN-HKNSYU(IDX 2)
      *     END-PERFORM
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �ѥ�᡼�������å�
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    ����ǯ������å�
           IF     (LNK-SKJHKN-SANTEIYM     =  SPACE)      OR
                  (LNK-SKJHKN-SANTEIYM    <=  "201709")
               MOVE    1                  TO  LNK-SKJHKN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ���ԥޥ���������
           INITIALIZE                         PTINF-REC
           MOVE    LNK-SKJHKN-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    LNK-SKJHKN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF              NOT =   ZERO
               MOVE    2                  TO  LNK-SKJHKN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ������Υ����å���������ץޥ�����������
           INITIALIZE                         NYUINACCT-REC
           MOVE    LNK-SKJHKN-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    LNK-SKJHKN-PTID        TO  NACCT-PTID
           MOVE    LNK-SKJHKN-SANTEIYM    TO  NACCT-SRYYM
           MOVE    "5"                    TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT          NOT =   ZERO
               MOVE    3                  TO  LNK-SKJHKN-RC
           ELSE
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE    "key17"            TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT         NOT =   ZERO
                    MOVE    3             TO  LNK-SKJHKN-RC
               ELSE
                  PERFORM    VARYING IDX   FROM   1   BY  1
                    UNTIL    IDX   >   31
                      MOVE   NACCT-DAY(IDX)      TO  WRK-NACCT-DAY9(IDX)
                      MOVE   WRK-NACCT-DAY9(IDX) TO  WRK-NACCT-DAY(IDX)
                  END-PERFORM
               END-IF
           END-IF
      **     PERFORM    VARYING IDX   FROM   1   BY  1
      **                UNTIL IDX   >   31
      **       DISPLAY "IDX = " IDX
      **               " WRK-NACCT-DAY = "
      **                WRK-NACCT-DAY(IDX)
      **     END-PERFORM
      *
      *    ������ץ�������
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    �����������
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    LNK-SKJHKN-SANTEIYM    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"          USING   STS-AREA-DAY
                                              LNK-DAY7-AREA
           IF      STS-DAY-RC1            =   ZERO
               MOVE    LNK-DAY7-KOYOMI    TO  WRK-GETSUMATSU-YMD
           END-IF
      *
      *    Ʊ�������������ݸ��ȹ礻����
      **     INITIALIZE                         ORCSNYUINDAY-AREA
      **     MOVE    LNK-SKJHKN-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
      **     MOVE    LNK-SKJHKN-PTID        TO  LNK-NYUINDAY-PTID
      **     MOVE    LNK-SKJHKN-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
      **     CALL    "ORCSNYUINDAY"         USING   ORCSNYUINDAY-AREA
      **                                            SPA-AREA
      **     PERFORM    VARYING IDX   FROM   1   BY  1
      **                UNTIL IDX   >   31
      **       DISPLAY "IDX = " IDX
      **               " DOUJITSU = "
      **                LNK-NYUINDAY-DOUJITSU-KBN(IDX)
      **               " TAI = "
      **                LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX)
      **               " SAINYU = "
      **                LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
      **     END-PERFORM
      *
      *    �����ƥ�ͽ������Ϥ����
           INITIALIZE                          SRYACT-REC
           MOVE    LNK-SKJHKN-HOSPNUM      TO  SRY-HOSPNUM
           MOVE    "1"                     TO  SRY-NYUGAIKBN
           MOVE    LNK-SKJHKN-PTID         TO  SRY-PTID
           MOVE    LNK-SKJHKN-SANTEIYM     TO  SRY-SRYYM
           MOVE    CONS-HKN-SITEI          TO  SRY-SRYCD(1)
                                               SRY-SRYCD(2)
                                               SRY-SRYCD(3)
                                               SRY-SRYCD(4)
                                               SRY-SRYCD(5)
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT             =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM   910-SRYACT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACT  =   1)
                   INITIALIZE                  SRYACCT-REC
                   MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
                   MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                   MOVE    SRY-PTID        TO  ACCT-PTID
                   MOVE    SRY-SRYKA       TO  ACCT-SRYKA
                   MOVE    SRY-SRYYM       TO  ACCT-SRYYM
                   MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-SELECT-SEC
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
                   IF    FLG-SRYACCT       =   0
                       PERFORM    VARYING   IDX   FROM   1   BY   1
                         UNTIL     IDX   >   31
                           IF   ACCT-DAY(1 IDX)   NOT =   ZERO
                             IF   ACCT-DAY(2 IDX)   NOT =   ZERO
                                 IF   ACCT-HKNCOMBI  NOT =
                                                WRK-NACCT-DAY9(IDX)
                                     IF   LNK-SKJHKN-HKNCOMBINUM(IDX 1)
                                                      =   SPACE
                                         MOVE   ACCT-HKNCOMBI
                                           TO   LNK-SKJHKN-HKNCOMBINUM
                                                                (IDX 1)
                                     ELSE
                                         MOVE   5  TO   LNK-SKJHKN-RC
                                     END-IF
                                 END-IF
                             END-IF
      *                      Ʊ��������ʬ
                             IF   ACCT-DAY(3 IDX)   NOT =   ZERO
      **                           DISPLAY "IDX = " IDX
      **                                   "ACCT-HKMCOMBI = "
      **                                    ACCT-HKNCOMBI
                                 IF   ACCT-HKNCOMBI  NOT =
                                     LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
                                     IF   LNK-SKJHKN-HKNCOMBINUM(IDX 2)
                                                      =   SPACE
                                         MOVE   ACCT-HKNCOMBI
                                           TO   LNK-SKJHKN-HKNCOMBINUM
                                                                (IDX 2)
                                     ELSE
                                         MOVE   5  TO   LNK-SKJHKN-RC
                                     END-IF
                                 END-IF
                             END-IF
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *            ���Ų�ץ�������
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
      *
                   MOVE    "tbl_sryact"    TO  MCP-TABLE
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 910-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ���Ź԰٥�������
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    �ݸ�����Υ��å�
           PERFORM    VARYING IDX   FROM   1   BY  1
                      UNTIL IDX   >   31
              IF  LNK-SKJHKN-HKNCOMBINUM (IDX 1)  NOT = SPACE
                 MOVE    LNK-SKJHKN-HKNCOMBINUM (IDX 1)
                                           TO  WRK-HKNCOMBINUM
                 PERFORM   210-HKNSYURUI-GET-SEC
                 MOVE    WRK-HKNSYU        TO  LNK-SKJHKN-HKNSYU(IDX 1)
              END-IF
              IF  LNK-SKJHKN-HKNCOMBINUM (IDX 2)  NOT = SPACE
                 MOVE    LNK-SKJHKN-HKNCOMBINUM (IDX 2)
                                           TO  WRK-HKNCOMBINUM
                 PERFORM   210-HKNSYURUI-GET-SEC
                 MOVE    WRK-HKNSYU        TO  LNK-SKJHKN-HKNSYU(IDX 2)
              END-IF
           END-PERFORM
      *
           IF       LNK-SKJHKN-TBL     =    SPACE
               MOVE     4              TO   LNK-SKJHKN-RC
           END-IF
           DISPLAY  "LNK-SKJHKN-RC = " LNK-SKJHKN-RC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ʊ��������������ʬ�ˤ��ݸ��������
      *****************************************************************
       210-HKNSYURUI-GET-SEC            SECTION.
      *
           MOVE    SPACE                TO   WRK-HKNSYU
           MOVE    "1"                  TO   WRK-HKNSYU
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-SKJHKN-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    LNK-SKJHKN-PTID         TO  COMB-PTID
           MOVE    WRK-HKNCOMBINUM         TO  COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
               IF   FLG-HKNCOMBI           =   ZERO
      *            ����
                   IF   COMB-HKNNUM        =   "060"  OR
                                               "067"  OR
                                               "068"  OR
                                               "069"
                       MOVE    "2"         TO  WRK-HKNSYU
                   END-IF
      *            ��������
                   IF   COMB-HKNNUM        =   "039"  OR
                                               "040"
                       MOVE    "3"         TO  WRK-HKNSYU
                   END-IF
      *            ϫ���ݸ�
                   IF   COMB-HKNNUM        =   "971"
                       MOVE    "4"         TO  WRK-HKNSYU
                   END-IF
      *            �������ݸ�
                   IF   COMB-HKNNUM        =   "973"
                       MOVE    "5"         TO  WRK-HKNSYU
                   END-IF
      *            �����ݸ�
                   IF   COMB-HKNNUM        =   "975"
                       MOVE    "6"         TO  WRK-HKNSYU
                   END-IF
      *            ����
                   IF   COMB-HKNNUM(1:2)   =   "98"
                       MOVE    "7"         TO  WRK-HKNSYU
                   END-IF
      *            ����
                   IF   COMB-HKNNUM(1:2)   =   "90"  OR
                                               "91"
                       MOVE    "8"         TO  WRK-HKNSYU
                   END-IF
               END-IF
      *        �ݸ��ȹ礻��������
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           ELSE
      *        �ݸ��ȹ礻��������
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           END-IF
      *
           .
       210-HKNSYURUI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Ź԰ټ���
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    �����԰٥�������
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���ԥޥ������ɹ�
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������ޥ���������
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������ޥ������ɹ�
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ץޥ���������
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ץޥ������ɹ�
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Ź԰٥ޥ���������
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Ź԰٥ޥ������ɹ�
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų�ץޥ���������
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų�ץޥ������ɹ�
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ź԰٥ޥ���������
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACT
           ELSE
               MOVE    1                  TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ź԰٥ޥ������ɹ�
      *****************************************************************
       910-SRYACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻�ޥ���������
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻�ޥ������ɹ�
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ��ʼ祭����
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
                   MOVE    ZERO        TO  FLG-TENSU
               ELSE
                   MOVE    1           TO  FLG-TENSU
                   INITIALIZE              TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-EXT.
           EXIT.