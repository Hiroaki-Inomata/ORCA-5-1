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
       PROGRAM-ID.             ORCSLASTYMD.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �ǽ��象���������
      *  �ֵѥ��顼������  : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03.05.00    NACL-¿��Ǽ   ��������
      *****************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
      *
           03  FLG-NYUIN               PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX2                    PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-LASTYMD             PIC X(08).
           03  WRK-TAIINYMD            PIC X(08).
           03  WRK-DRCD                PIC X(05).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
      *
           03  WRK-HKNCOMBI-MEI        PIC X(10).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HONKZKKBN-MEI       PIC X(02).
      *
          03  WRK-NYUINYMD             PIC X(08).
          03  WRK-BRMNUM.
               05  WRK-BRMNUM1             PIC X(02).
               05  WRK-BRMNUM-H            PIC X(01).
               05  WRK-BRMNUM2             PIC X(06).
           03  WRK-Z6-X.
               05  WRK-Z6              PIC ZZZZZZ.
           03  WRK-BRMNUM-9            PIC 9(06).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ΰ�
           COPY    "MCPAREA".
      *
           COPY    "CPSK1005.INC".
      *
      *    ���Ų�����
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    ��������
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ������������
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    �ݸ��Ȥ߹�碌
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
      *    ���ե��֥롼�����ΰ�
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSLASTYMD.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSLASTYMDAREA
           SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *    �������
           PERFORM 100-INIT-SEC
      *    �ᥤ�����
           PERFORM 200-MAIN-SEC
      *    ��λ��
           PERFORM 300-END-SEC
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  ORCSLASTYMD-OUT
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ᥤ�����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      SPA-BEDFLG          >   ZERO
      *        �¾��������硢�������Ƚ��
               PERFORM 2001-NYUIN-CHK-SEC
           END-IF
      *    ������ʳ�
           PERFORM 2002-JYURRK-CHK-SEC
      *    ���Ų�����
           IF     (FLG-NYUIN           =   ZERO ) AND
                  (WRK-LASTYMD         =   SPACE)
               PERFORM 2003-SRYKARRK-CHK-SEC
           END-IF
      *
      *    ���������Խ�
           MOVE    WRK-LASTYMD         TO  ORCSLASTYMD-LASTYMD
           IF      WRK-LASTYMD     NOT =   SPACE
               MOVE    WRK-LASTYMD         TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  ORCSLASTYMD-LASTYMDW
           END-IF
           MOVE    WRK-SRYKA           TO  ORCSLASTYMD-SRYKA
           MOVE    WRK-DRCD            TO  ORCSLASTYMD-DRCD
           MOVE    WRK-BRMNUM          TO  ORCSLASTYMD-BRMNUM
           MOVE    WRK-NYUINYMD        TO  ORCSLASTYMD-NYUINYMD
           MOVE    WRK-TAIINYMD        TO  ORCSLASTYMD-TAIINYMD
      *    �ݸ��ȹ礻̾���Խ�
           PERFORM 2004-HKNCOMBI-HEN-SEC
      *    ���Ų�̾��
           IF     (WRK-SRYKA       NOT =   SPACE )  AND
                  (ORCSLASTYMD-SRYKAFLG    =   "1" )
               PERFORM 2005-SRYKAMEI-HEN-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������å��Խ�����
      *****************************************************************
       2001-NYUIN-CHK-SEC                SECTION.
      *
           MOVE    SPACE                   TO  WRK-BRMNUM
           MOVE    SPACE                   TO  WRK-SRYKA
           MOVE    SPACE                   TO  WRK-HKNCOMBI
           MOVE    SPACE                   TO  WRK-LASTYMD
           MOVE    SPACE                   TO  WRK-TAIINYMD
           MOVE    SPACE                   TO  WRK-DRCD
           MOVE    ZERO                    TO  FLG-NYUIN
      *
      *    ��������Ƚ��
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-TAIINYMD
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           PERFORM UNTIL    FLG-PTNYUINRRK =   1
      *
               IF     (PTNYUINRRK-NYUINYMD <=  SPA-SYSYMD) AND
                      (PTNYUINRRK-TAIINYMD >=  SPA-SYSYMD)
                   PERFORM 20011-NYUIN-HENSYU-SEC
                   MOVE    1                   TO  FLG-PTNYUINRRK
               ELSE
      *            �ǽ����������ౡ��
                   IF      WRK-NYUINYMD        =   SPACE
                       MOVE    PTNYUINRRK-TAIINYMD TO  WRK-TAIINYMD
                       MOVE    PTNYUINRRK-NYUINYMD TO  WRK-NYUINYMD
                   END-IF
               END-IF
      *        �����ƥ����������ౡ
               IF     (PTNYUINRRK-TAIINYMD <   SPA-SYSYMD)
                   MOVE    1                   TO  FLG-PTNYUINRRK
               END-IF
               IF      FLG-PTNYUINRRK          =   ZERO
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key29"             TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2001-NYUIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       20011-NYUIN-HENSYU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-BRMNUM
           MOVE    PTNYUINRRK-BRMNUM(1:2)      TO  WRK-BRMNUM1
           MOVE    "-"                         TO  WRK-BRMNUM-H
           IF      PTNYUINRRK-BRMNUM(3:6)  NUMERIC
               MOVE    PTNYUINRRK-BRMNUM(3:6)  TO  WRK-BRMNUM-9
               MOVE    WRK-BRMNUM-9            TO  WRK-Z6
               MOVE    ZERO                    TO  IDX2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   6
                   IF      WRK-Z6-X(IDX:1)     NOT =   SPACE
                       ADD     1               TO  IDX2
                       MOVE    WRK-Z6-X(IDX:1) TO  WRK-BRMNUM2
                                                            (IDX2:1)
                   END-IF
               END-PERFORM
           ELSE
               MOVE    PTNYUINRRK-BRMNUM(3:6)  TO  WRK-BRMNUM2
           END-IF
           MOVE    PTNYUINRRK-NYUINKA      TO  WRK-SRYKA
           MOVE    PTNYUINRRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
           MOVE    PTNYUINRRK-NYUINYMD     TO  WRK-NYUINYMD
      *    ����ɥ�����
           MOVE    PTNYUINRRK-DRCD1        TO  WRK-DRCD
           MOVE    1                       TO  FLG-NYUIN
      *    �����ౡ�λ�
           MOVE    SPACE                   TO  WRK-TAIINYMD
           IF      PTNYUINRRK-TAIINYMD     =   SPA-SYSYMD
               MOVE    PTNYUINRRK-TAIINYMD TO  WRK-LASTYMD
           END-IF
      *
           .
       20011-NYUIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       2002-JYURRK-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  JYURRK-PTID
      **** MOVE    SPA-SYSYMD          TO  JYURRK-SRYYMD
           MOVE    "99999999"          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               IF      FLG-NYUIN           =   ZERO
                   MOVE    JYURRK-SRYYMD       TO  WRK-LASTYMD
                   MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
                   MOVE    JYURRK-DRCD         TO  WRK-DRCD
               END-IF
           END-IF
      *
      *    �ౡ�����������ǽ����ϡ��ౡ��
           IF      (FLG-NYUIN          =   ZERO  )  AND
                   (WRK-LASTYMD    NOT =   SPACE )  AND
                   (WRK-TAIINYMD   NOT =   SPACE )
               IF      WRK-LASTYMD     <   WRK-TAIINYMD
                   MOVE    WRK-TAIINYMD    TO  WRK-LASTYMD
              END-IF
           END-IF
      *
           .
       2002-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������å�����
      *****************************************************************
       2003-SRYKARRK-CHK-SEC                SECTION.
      *
      *    ���Ų������ɹ�
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           PERFORM UNTIL   FLG-SRYKARRK    =   1
               IF     (KARRK-LASTYMD   NOT =   SPACE )  AND
                      (KARRK-SRYKA     NOT =   SPACE )
                   MOVE    KARRK-LASTYMD       TO  WRK-LASTYMD
                   MOVE    KARRK-SRYKA         TO  WRK-SRYKA
                   MOVE    1                   TO  FLG-SRYKARRK
               ELSE
                   MOVE    "tbl_srykarrk"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-SRYKARRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2003-SRYKARRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ�ɽ������
      *****************************************************************
       2004-HKNCOMBI-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
           MOVE    SPACE               TO  WRK-HKNNUM
           MOVE    SPACE               TO  WRK-HONKZKKBN-MEI
      *
           IF      WRK-HKNCOMBI    NOT =   SPACE
               PERFORM 20041-HKNCOMBI-CHK1-SEC
           END-IF
           IF      WRK-HKNCOMBI-MEI    =   SPACE
               PERFORM 20042-HKNCOMBI-CHK2-SEC
           END-IF
      *
           MOVE    WRK-HKNCOMBI        TO  ORCSLASTYMD-HKNCOMBI
           MOVE    WRK-HKNNUM          TO  ORCSLASTYMD-HKNNUM
           MOVE    WRK-HKNCOMBI-MEI    TO  ORCSLASTYMD-HKNCOMBI-MEI
           MOVE    WRK-HONKZKKBN-MEI   TO  ORCSLASTYMD-HONKZKKBN
           .
       2004-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ�ɽ������
      *****************************************************************
       20041-HKNCOMBI-CHK1-SEC          SECTION.
      *
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-HKNCOMBI        =   ZERO )  AND
                  (COMB-DELKBN     NOT =   "1"  )  AND
                 ((SPA-SYSYMD          >=  COMB-TEKSTYMD ) AND
                  (SPA-SYSYMD          <=  COMB-TEKEDYMD ))
               PERFORM 200411-HKNCOMBI-MEI-SEC
           END-IF
      *
           .
       20041-HKNCOMBI-CHK1-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ�ɽ������
      *****************************************************************
       20042-HKNCOMBI-CHK2-SEC          SECTION.
      *
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           PERFORM
               UNTIL       FLG-HKNCOMBI        =   1
               IF     (COMB-DELKBN     NOT =   "1"  )  AND
                     ((SPA-SYSYMD          >=  COMB-TEKSTYMD ) AND
                      (SPA-SYSYMD          <=  COMB-TEKEDYMD ))
                   IF     (WRK-HKNNUM      =   SPACE )  OR
                          (COMB-HKNNUM     =   "971"  OR  "973")
                       PERFORM 200411-HKNCOMBI-MEI-SEC
                   END-IF
               END-IF
               IF      FLG-HKNCOMBI        =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20042-HKNCOMBI-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻̾���Խ�����
      *****************************************************************
       200411-HKNCOMBI-MEI-SEC               SECTION.
      *
      *    ���ݸ����ʤ���
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    COMB-KOH1-TANSEIDONAME  TO  WRK-HKNCOMBI-MEI
               MOVE    COMB-KOH1HKNNUM         TO  WRK-HKNNUM
           ELSE
               MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNCOMBI-MEI
               MOVE    COMB-HKNNUM             TO  WRK-HKNNUM
           END-IF
      *    �ܿͲ�²
           EVALUATE    COMB-HONKZKKBN
               WHEN    "1"
                   MOVE    "��"            TO  WRK-HONKZKKBN-MEI
               WHEN    "2"
                   MOVE    "��"            TO  WRK-HONKZKKBN-MEI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-HONKZKKBN-MEI
           END-EVALUATE
           .
       200411-HKNCOMBI-MEI-EXT.
           EXIT.
      *****************************************************************
      *    ���Ų�̾���Խ�����
      *****************************************************************
       2005-SRYKAMEI-HEN-SEC             SECTION.
      *    ���Ų�
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *    �����ƥ��������
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME3 TO  ORCSLASTYMD-SRYKA-MEI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2005-SRYKAMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       700-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ����ɤ߹��߽���
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *    ���Ų���ǡ����祭������
      *****************************************************************
       900-SRYKARRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       900-SRYKARRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    ����������ɤ�
      *****************************************************************
       920-JYURRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       920-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������ޥ������ɹ�
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *****************************************************************
      *    �ݸ��ȹ礻�ɹ��߽���
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *