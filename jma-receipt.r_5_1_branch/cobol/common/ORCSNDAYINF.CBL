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
       PROGRAM-ID.                     ORCSNDAYINF.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ������ñ�������������
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  06/06/20    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07/05/28  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-NYUINCHU            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXD                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SRYYMD              PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-DAYS-MAX          PIC 9(05)   VALUE 366.
      *
       01  TNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //TNRRK-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ��������
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSNDAYINF.INC".
       01  LNKNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"      REPLACING   //PTNYUINRRK-//
                                           BY          //LNKNRRK-//.
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SNDAYINF-AREA
           LNKNRRK-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           MOVE    SNDAYINF-STSRYYMD   TO  WRK-SRYYMD
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD        >   CONST-DAYS-MAX )
                    OR   ( WRK-SRYYMD  >   SNDAYINF-EDSRYYMD )
      *
               MOVE    IDXD            TO  SNDAYINF-DAYS-MAX
               MOVE    WRK-SRYYMD      TO  SNDAYINF-SRYYMD (IDXD)
               MOVE    SNDAYINF-STSRYYMD
                                       TO  WRK-YMD
               MOVE    IDXD            TO  WRK-ZOGEN
               MOVE    "1"             TO  WRK-ZOGENPTN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-SRYYMD
      *
           END-PERFORM
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SNDAYINF-DAYS-MAX )
      *
                   IF    ( PTNYUINRRK-RRKEDANUM    =   1 )
                       IF    ( PTNYUINRRK-NYUINYMD
                                       =   SNDAYINF-SRYYMD (IDXD))
                           MOVE    PTNYUINRRK-REC  TO  TNRRK-AREA
                           PERFORM 900-PTNYUINRRK-KEY12-SEL-SEC
                           MOVE    TNRRK-AREA      TO  PTNYUINRRK-REC
                           IF    ( FLG-NYUINCHU    =   1 )
                               CONTINUE
                           ELSE
                               MOVE    "1" TO  SNDAYINF-NYUINKBN (IDXD)
                           END-IF
                       END-IF
                       IF    ( PTNYUINRRK-TAIINYMD
                                       =   SNDAYINF-SRYYMD (IDXD))
                           MOVE    "1"     TO  SNDAYINF-TAIINKBN (IDXD)
                       END-IF
                   END-IF
      *
                   IF    ( PTNYUINRRK-TENNYUYMD
                                          <=   SNDAYINF-SRYYMD (IDXD))
                    AND  ( PTNYUINRRK-TENSTUYMD
                                          >=   SNDAYINF-SRYYMD (IDXD))
      *
                       MOVE    "1"         TO  SNDAYINF-ZAIINKBN (IDXD)
                       PERFORM 2001-TENSTU-CHK-SEC
      *
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-PTNYUINRRK-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD        >   SNDAYINF-DAYS-MAX )
      *
               IF    ( SNDAYINF-BTUNUM (IDXD)
                                   =   SNDAYINF-BTUNUM-SAKI (IDXD) )
                   MOVE    SPACE   TO  SNDAYINF-BTUNUM-SAKI (IDXD)
               END-IF
      *
               IF    ( SNDAYINF-BRMNUM (IDXD)
                                   =   SNDAYINF-BRMNUM-SAKI (IDXD) )
                   MOVE    SPACE   TO  SNDAYINF-BRMNUM-SAKI (IDXD)
               END-IF
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ž�Х����å�����
      *****************************************************************
       2001-TENSTU-CHK-SEC                    SECTION.
      *
           MOVE    PTNYUINRRK-BTUNUM   TO  SNDAYINF-BTUNUM (IDXD)
      *
           IF    ( SNDAYINF-BTUNUM-SAKI (IDXD)
                                       =   SPACE )
               MOVE    PTNYUINRRK-BTUNUM
                                       TO  SNDAYINF-BTUNUM-SAKI (IDXD)
           END-IF
      *
           MOVE    PTNYUINRRK-BRMNUM   TO  SNDAYINF-BRMNUM (IDXD)
      *
           IF    ( SNDAYINF-BRMNUM-SAKI (IDXD)
                                       =   SPACE )
               MOVE    PTNYUINRRK-BRMNUM
                                       TO  SNDAYINF-BRMNUM-SAKI (IDXD)
           END-IF
      *
           IF    ( SNDAYINF-SRYKA (IDXD)
                                       =   SPACE )
               MOVE    PTNYUINRRK-NYUINKA
                                       TO  SNDAYINF-SRYKA (IDXD)
           END-IF
      *
           .
       2001-TENSTU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ����������������å�����
      *****************************************************************
       200-LASTRRK-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINCHU
      *
           PERFORM 900-PTNYUINRRK-KEY12-SEL-SEC
           IF    ( FLG-PTNYUINRRK      =   ZERO )
               IF    ( PTNYUINRRK-JTIKBN       =   "6" )
                AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                   MOVE    1           TO  FLG-NYUINCHU
               END-IF
           END-IF
      *
           .
       200-LASTRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                    SECTION.
      *
      *    PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    DISPLAY����
      *****************************************************************
       800-DISPLAY-SEC                 SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   SNDAYINF-DAYS-MAX )
               IF    ( SNDAYINF-ZAIINKBN (IDXD)  =   "1" )
                   DISPLAY
                   "PTID[" LNKNRRK-PTID "]"
                   "RRKNUM[" LNKNRRK-RRKNUM "]"
                   "SRYYMD[" SNDAYINF-SRYYMD (IDXD) "]"
                   "BTUNUM[" SNDAYINF-BTUNUM (IDXD) "]"
                   "BTUNUM-SAKI[" SNDAYINF-BTUNUM-SAKI (IDXD) "]"
                   "BRMNUM[" SNDAYINF-BRMNUM (IDXD) "]"
                   "BRMNUM-SAKI[" SNDAYINF-BRMNUM-SAKI (IDXD) "]"
                   "SRYKA[" SNDAYINF-SRYKA (IDXD) "]"
                   "NYUINKBN[" SNDAYINF-NYUINKBN  (IDXD) "]"
                   "TAIINKBN[" SNDAYINF-TAIINKBN (IDXD) "]"
               END-IF
           END-PERFORM
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    ������������
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    �������򸡺�����
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    LNKNRRK-AREA        TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��������FETCH����
      *****************************************************************
       900-PTNYUINRRK-KEY4-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    �������򸡺�����
      *****************************************************************
       900-PTNYUINRRK-KEY12-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY12-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ���Ծ��󸡺�����
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
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
       910-DBCLOSECURSOR-SEC                 SECTION.
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
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.