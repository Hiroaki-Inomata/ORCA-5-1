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
       PROGRAM-ID.             ORCSS008.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ���Ŷ�ʬ����(2,3�Τ�)
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/08/21    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-RYOCOM                      PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-END                         PIC 9(01).
           03  FLG-NYUINDAY-G.
               05  FLG-NYUINDAY                PIC 9(01)
                                               OCCURS  31.
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX-DAY                         PIC 9(05).
           03  IDX-MED                         PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SRYYM                       PIC X(08).
           03  WRK-MEDKBN                      PIC X(01).
      *
           COPY    "CPMEDKBN.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSS008.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           SS008-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SS008-OT
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    ��λ����
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-END
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
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM 900-RYOCOM-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-RYOCOM  NOT =   ZERO )
                    OR   ( SS008-RC    NOT =   ZERO )
      *
               IF    ( RYOCOM-ZAISKBKBN    =   "2" )
                   PERFORM 2001-EDIT-MEDKBN-SEC
               END-IF
      *
               PERFORM 900-RYOCOM-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ŷ�ʬ�Խ�����
      *****************************************************************
       2001-EDIT-MEDKBN-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-MEDKBN
           PERFORM VARYING IDX-MED FROM    1   BY  1
                   UNTIL ( IDX-MED     >   MEDH20-MAX )
                    OR   ( WRK-MEDKBN  NOT =   SPACE )
               IF   ( RYOCOM-SRYYM >=  MEDH20-STYM   (IDX-MED))
                AND ( RYOCOM-SRYYM <=  MEDH20-EDYM   (IDX-MED))
                AND ( RYOCOM-ZAINUM
                                   =   MEDH20-ZAINUM (IDX-MED))
                   MOVE    MEDH20-MEDKBN (IDX-MED)
                                   TO  WRK-MEDKBN
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( NACCT-DAY (IDX-DAY)     >   ZERO )
      *
                   IF    ( RYOCOM-MONTH        >   ZERO )
                    OR   ( RYOCOM-DAY(IDX-DAY) >   ZERO )
                       IF    ( SS008-MEDKBNDAY (IDX-DAY)
                                               =   SPACE )
                           MOVE    WRK-MEDKBN  TO  SS008-MEDKBNDAY
                                                          (IDX-DAY)
                       ELSE
                           IF    ( WRK-MEDKBN  >   SS008-MEDKBNDAY
                                                             (IDX-DAY))
                               MOVE    WRK-MEDKBN
                                               TO  SS008-MEDKBNDAY
                                                             (IDX-DAY)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2001-EDIT-MEDKBN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    ZERO            TO  SS008-RC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ���ܥ쥻�ץȥ����ȥޥ�����������
      *****************************************************************
       900-RYOCOM-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-PTID        TO  RYOCOM-PTID
           MOVE    SS008-SRYYM     TO  RYOCOM-SRYYM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           .
       900-RYOCOM-KEY2-SEL-EXT.
            EXIT.
      *****************************************************************
      *    ���ܥ쥻�ץȥ����ȥޥ���FETCH����
      *****************************************************************
       900-RYOCOM-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           .
       900-RYOCOM-KEY2-FET-EXT.
            EXIT.
      *****************************************************************
      *    ������׸�������
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
      *
           INITIALIZE                          NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SS008-SRYYM         TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.