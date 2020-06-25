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
       PROGRAM-ID.                     ORCSODRS01.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ���������Խ����֣�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/03/17    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07/06/05  ���롼�׿����б�
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
       WORKING-STORAGE                 SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
      *
      *    ��������ΰ�
      *01  CNT-AREA.
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-STR                 PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-ITEMNUM-MAX               PIC 9(05)   VALUE 30.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ������
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    ����������
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *    ������ư�����å�����
           COPY    "CPORCSODR.INC".
           COPY    "CPORCSODRS01.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                               SODR-AREA
                                               SODRS01-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *    INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SODRS01-RC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2101-ODRITEM-RECONF-SEC
      *
           INITIALIZE                      ORDER-REC
           MOVE    SODR-HOSPNUM         TO  ORDER-HOSPNUM
           MOVE    SODR-KARTE-KEY      TO  ORDER-KARTE-KEY
           MOVE    SODR-ORDERNUM       TO  ORDER-ORDERNUM
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSODRS01 ERROR:ORDER DELETE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������ܾ������������
      *****************************************************************
       2101-ODRITEM-RECONF-SEC         SECTION.
      *
           INITIALIZE                      ODRITEM-REC
           MOVE    SODR-HOSPNUM         TO  ODRITEM-HOSPNUM
           MOVE    SODR-KARTE-KEY      TO  ODRITEM-KARTE-KEY
           MOVE    SODR-ORDERNUM       TO  ODRITEM-ORDERNUM
           MOVE    3                   TO  ODRITEM-STATUS
           PERFORM 900-ODRITEM-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ODRITEM   NOT =   ZERO )
                     OR  ( LNK-ODRITEM-MAX  >=   CONST-ITEMNUM-MAX )
      *
               COMPUTE LNK-ODRITEM-MAX
                   =   LNK-ODRITEM-MAX   +   1
      *
               MOVE    LNK-ODRITEM-MAX TO  ODRITEM-ITEMNUM
      *
               MOVE    ODRITEM-REC     TO  LNK-ODRITEM-OCC
                                                   (LNK-ODRITEM-MAX)
      *
               PERFORM 900-ODRITEM-KEY3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                      ODRITEM-REC
           MOVE    SODR-HOSPNUM        TO  ODRITEM-HOSPNUM
           MOVE    SODR-KARTE-KEY      TO  ODRITEM-KARTE-KEY
           MOVE    SODR-ORDERNUM       TO  ODRITEM-ORDERNUM
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSODRS01 ERROR:ODRITEM DELETE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-ODRITEM-RECONF-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    FLG-END         TO  SODRS01-RC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����������ܸ�������(KEY3)
      *****************************************************************
       900-ODRITEM-KEY3-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ODRITEM
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ODRITEM-REC
           ELSE
               INITIALIZE                  ODRITEM-REC
               MOVE    1               TO  FLG-ODRITEM
           END-IF
      *
           .
       900-ODRITEM-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ������������FETCH����(KEY3)
      *****************************************************************
       900-ODRITEM-KEY3-FET-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ODRITEM
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ODRITEM-REC
           ELSE
               INITIALIZE                  ODRITEM-REC
               MOVE    1               TO  FLG-ODRITEM
           END-IF
      *
           .
       900-ODRITEM-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
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
       910-DB-SELECT-EXT.
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
      *    �ģ¥��󥵡��Ƚ���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¹�������
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    �ģº������
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.