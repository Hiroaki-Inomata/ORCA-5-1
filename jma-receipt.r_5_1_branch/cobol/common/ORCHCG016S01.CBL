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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCHCG016S01.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : ������Ź԰ٷ��Ĵ����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/06/21    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07/06/05  ���롼�׿����б�
      *****************************************************************
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
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-JYURRK                          PIC 9(01).
           03  FLG-SRYKARRK                        PIC 9(01).
           03  FLG-PTNYUINRRK                      PIC 9(01).
           03  FLG-HKNCOMBI                        PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-NYUIN                           PIC 9(01).
           03  FLG-PTRSIINF                        PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX-PNT                             PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-HKNCOMBI                        PIC X(04).
           03  WRK-HKNNUM                          PIC X(03).
           03  WRK-HKNKBN-MEI                      PIC X(02).
           03  WRK-HKNCOMBI-MEI                    PIC X(100).
           03  WRK-SRYKA                           PIC X(02).
           03  WRK-LASTYMD                         PIC X(08).
           03  WRK-BRMNUM                          PIC X(09).
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �¼�̾�Խ�
           COPY    "CPORCSBRMNUM.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��������
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ���Ų�����
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    ������������
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ����ϫ���ݸ�����
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    �ݸ��Ȥ߹�碌
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCHCG016S01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           HCG016S01-AREA
           SPA-AREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              HCG016S01-OT-AREA
                                   FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUIN
      *
           MOVE    SPACE           TO  WRK-HKNCOMBI
      *
           PERFORM 2001-NYUIN-CHK-SEC
      *
           IF    ( FLG-NYUIN       =   ZERO )
               PERFORM 2001-GAIRAI-CHK-SEC
           END-IF
      *
           PERFORM 2001-HKNCOMBI-HEN-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ������������å�����
      *****************************************************************
       2001-NYUIN-CHK-SEC              SECTION.
      *
      *    ��������Ƚ��
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    HCG016S01-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    HCG016S01-PTID      TO  PTNYUINRRK-PTID
           MOVE    HCG016S01-KJNYMD    TO  PTNYUINRRK-NYUINYMD
           MOVE    HCG016S01-KJNYMD    TO  PTNYUINRRK-TAIINYMD
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               MOVE    PTNYUINRRK-BTUNUM (1:2)
                                       TO  WRK-BRMNUM (1:2)
               MOVE    "-"             TO  WRK-BRMNUM (3:1)
      *
               INITIALIZE  ORCSBRMNUMAREA
               MOVE   PTNYUINRRK-BRMNUM ( 3 : )
                                       TO  SBRMNUM-IN-BRMNUM
               CALL   "ORCSBRMNUM"     USING   ORCSBRMNUMAREA
               MOVE   SBRMNUM-OT-BRMNUM
                                       TO  WRK-BRMNUM (4:)
      *
               MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-SRYKA
               MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBI
               MOVE    1               TO  FLG-NYUIN
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-NYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����������å�����
      *****************************************************************
       2001-GAIRAI-CHK-SEC              SECTION.
      *
      *    �ݸ���Ƚ��
           PERFORM 20011-JYURRK-HKN-HEN-SEC
      *
      *
           IF    ( WRK-LASTYMD     =    SPACE )
      *        ���Ų����򤫤�
               PERFORM 20011-SRYKARRK-HEN-SEC
           END-IF
      *
           .
       2001-GAIRAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��Խ������ʳ����
      *****************************************************************
       20011-JYURRK-HKN-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURRK
      *
           INITIALIZE                      JYURRK-REC
           MOVE    HCG016S01-HOSPNUM    TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
           MOVE    HCG016S01-PTID      TO  JYURRK-PTID
           MOVE    HCG016S01-KJNYMD    TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME

           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  JYURRK-REC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           IF ( FLG-JYURRK             =   ZERO )
               MOVE    JYURRK-SRYYMD   TO  WRK-LASTYMD
               MOVE    JYURRK-SRYKA    TO  WRK-SRYKA
               MOVE    JYURRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-JYURRK-HKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ���Ų��Խ������ʿ��Ų��������
      *****************************************************************
       20011-SRYKARRK-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYKARRK
      *
      *    ���Ų���򸡺�
           INITIALIZE                      SRYKARRK-REC
           MOVE    HCG016S01-HOSPNUM    TO  KARRK-HOSPNUM
           MOVE    HCG016S01-PTID      TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYKARRK-REC
           ELSE
               INITIALIZE                  SRYKARRK-REC
               MOVE    1               TO  FLG-SRYKARRK
           END-IF
      *
           PERFORM UNTIL       FLG-SRYKARRK    =   1
      *
      *        �ǽ�������
               IF    ( KARRK-SRYKA     NOT =   SPACE       )
                 AND ( KARRK-LASTYMD   NOT =   SPACE       )
                 AND ( KARRK-LASTYMD      >=   WRK-LASTYMD )
      *
                   MOVE    KARRK-LASTYMD   TO  WRK-LASTYMD
                   MOVE    KARRK-SRYKA     TO  WRK-SRYKA
      *
               END-IF
      *
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SRYKARRK-REC
               ELSE
                   INITIALIZE                  SRYKARRK-REC
                   MOVE    1               TO  FLG-SRYKARRK
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SRYKARRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻�Խ�����
      *****************************************************************
       2001-HKNCOMBI-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
           MOVE    SPACE               TO  WRK-HKNNUM
      *
           IF    ( WRK-HKNCOMBI    NOT =   SPACE )
               PERFORM 20011-HKNCOMBI-CHK1-SEC
           END-IF
      *
           IF    ( WRK-HKNCOMBI-MEI    =   SPACE )
               PERFORM 20011-HKNCOMBI-CHK2-SEC
           END-IF
      *
           .
       2001-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻�Խ�����
      *****************************************************************
       20011-HKNCOMBI-CHK1-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    HCG016S01-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    HCG016S01-PTID      TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
      *
               IF       (  COMB-DELKBN NOT =   "1"  )
                  AND   (( HCG016S01-KJNYMD  >=  COMB-TEKSTYMD )
                  AND    ( HCG016S01-KJNYMD  <=  COMB-TEKEDYMD ))
                   PERFORM 200111-HKNCOMBI-MEI-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-HKNCOMBI-CHK1-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻�Խ�����
      *****************************************************************
       20011-HKNCOMBI-CHK2-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    HCG016S01-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    HCG016S01-PTID      TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-HKNCOMBI    =   1 )
      *
               IF   (  COMB-DELKBN     NOT =   "1"  )  
                AND (( HCG016S01-KJNYMD   >=   COMB-TEKSTYMD ) 
                 AND ( HCG016S01-KJNYMD   <=   COMB-TEKEDYMD ))
                   IF    ( WRK-HKNNUM      =   SPACE )
                    OR   ( COMB-HKNNUM     =   "971"  OR  "973")
                       PERFORM 200111-HKNCOMBI-MEI-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  HKNCOMBI-REC
               ELSE
                   MOVE    1           TO  FLG-HKNCOMBI
                   INITIALIZE              HKNCOMBI-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-HKNCOMBI-CHK2-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻̾���Խ�����
      *****************************************************************
       200111-HKNCOMBI-MEI-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
      *
           MOVE    1                   TO  IDX-PNT
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
           INTO    WRK-HKNCOMBI-MEI
           WITH    POINTER IDX-PNT
           END-STRING
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   4 )
      *
               IF    ( COMB-KOH-TANSEIDONAME (IDX0)   NOT =    SPACE )
                   IF    ( IDX-PNT >   1 )
                       STRING  "��"        DELIMITED  BY  SIZE
                               COMB-KOH-TANSEIDONAME (IDX0)
                                           DELIMITED  BY  SPACE
                       INTO    WRK-HKNCOMBI-MEI
                       WITH    POINTER IDX-PNT
                       END-STRING
                   ELSE
                       STRING  COMB-KOH-TANSEIDONAME (IDX0)
                                           DELIMITED  BY  SPACE
                       INTO    WRK-HKNCOMBI-MEI
                       WITH    POINTER IDX-PNT
                       END-STRING
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    ϫ�Ҥλ��������Ȥ��ɲ�
           IF    ( COMB-HKNNUM         =   "971"  OR  "973" )
      *
               PERFORM 900-PTRSIINF-KEY-SEL-SEC
      *
               MOVE    SPACE           TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
                   MOVE    "û"        TO  WRK-HKNKBN-MEI
               WHEN    "2"
                   MOVE    "��"        TO  WRK-HKNKBN-MEI
               WHEN    "3"
                   MOVE    "��"        TO  WRK-HKNKBN-MEI
               WHEN    "5"
                   MOVE    "��"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
      *
               IF    ( PTRSI-COMMENT       =   SPACE )
                   MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           "��"                    DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                   INTO    WRK-HKNCOMBI-MEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           "��"                    DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "��"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "��"                    DELIMITED  BY  SIZE
                   INTO              WRK-HKNCOMBI-MEI
                   END-STRING
               END-IF
           END-IF
      *
           .
       200111-HKNCOMBI-MEI-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    WRK-HKNCOMBI        TO  HCG016S01-HKNCOMBI
           MOVE    WRK-HKNNUM          TO  HCG016S01-HKNNUM
           MOVE    WRK-HKNCOMBI-MEI    TO  HCG016S01-HKNCOMBIMEI
           MOVE    WRK-SRYKA           TO  HCG016S01-SRYKA
           MOVE    WRK-LASTYMD         TO  HCG016S01-LASTYMD
           MOVE    WRK-BRMNUM          TO  HCG016S01-BRMNUM
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ����ϫ���ݸ����󸡺�����(KEY)
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    COMB-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    COMB-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH�ϹԤ�ʤ�)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.