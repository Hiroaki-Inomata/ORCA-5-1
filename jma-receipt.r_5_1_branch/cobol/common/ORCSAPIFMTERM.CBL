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
       PROGRAM-ID.             ORCSAPIFMTERM.
      *****************************************************************
      *  �����ƥ�̾        : ORCA
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : API��Ģɼ�ǡ������������
      *  ��������    ��ȼ�        ����
      *  09/02/12    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
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
       01  FLG-AREA.
           03  FLG-APIPARA             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDXF                    PIC 9(03).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-TEXT                PIC X(32).
      *
       01  CONST-AREA.
           03  CONST-FORM-MAX          PIC 9(03)   VALUE 10.
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ����饤��Ģɼ�ɣ�Ϣ��
           COPY    "CPORCSRPLNK.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    Ģɼ�ǡ���PUSH�̿���
           COPY    "CPPUSHPRINT001.INC".
      *
      *    ���Уɥѥ�᥿
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
           COPY    "CPPARA001.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
            COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SPA-AREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           PERFORM 200-MAIN-SEC
      *
      *    �ֵ��ΰ��Խ�
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( SPA-API-FLG     =   1 OR 2 )
               IF    ( SPA-PRTCONF =   "2" OR "3" )
                   CONTINUE
               ELSE
                   PERFORM 2001-PUSH-SEC
               END-IF
           END-IF
      *
           PERFORM 2001-APIPARA-DEL-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    PUSH����
      *****************************************************************
       2001-PUSH-SEC                   SECTION.
      *
           MOVE    ZERO                TO  IDXF
      *
           INITIALIZE                      PUSHP001-REC
      *
           MOVE    "print001"          TO  PUSHP001-EVENT
           MOVE    SPA-OPID            TO  PUSHP001-OPID
      *
           PERFORM 900-APIPARA-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-APIPARA NOT =   ZERO )
                    OR   ( IDXF           >=  CONST-FORM-MAX )
      *
               COMPUTE IDXF  = IDXF  + 1
      *
               MOVE    APIPARA-DATA-REC TO PARA001-REC
      *
               MOVE    PARA001-FORMID      TO  PUSHP001-FORMID  (IDXF)
               MOVE    PARA001-CUSTOMID    TO  PUSHP001-CUSTOMID(IDXF)
               EVALUATE    TRUE
               WHEN    ( PARA001-FORMNAME (1:8)    =    "��ʧ����" )
                   MOVE    "��ʧ����������̡�"
                                           TO  PUSHP001-FORMNAME (IDXF)
               WHEN    ( PARA001-FORMNAME (1:8)    =    "��ʧ����" )
                   MOVE    "��ʧ������ʷ��̡�"
                                           TO  PUSHP001-FORMNAME (IDXF)
               WHEN    ( PARA001-FORMNAME (1:8)    =    "��ʧ����" )
                   MOVE    "��ʧ������"    TO  PUSHP001-FORMNAME (IDXF)
               WHEN    OTHER
                   MOVE  PARA001-FORMNAME  TO  PUSHP001-FORMNAME(IDXF)
               END-EVALUATE
               MOVE    PARA001-DATAID      TO  PUSHP001-DATAID  (IDXF)
      *
               PERFORM 900-APIPARA-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDXF            >   ZERO )
               MOVE    "push_print001"     TO  MCP-TABLE
               MOVE    LOW-VALUE           TO  MCP-PATHNAME
               MOVE    "PUSHEVENT"         TO  MCP-FUNC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               PUSHP001-REC
           END-IF
      *
           .
       2001-PUSH-EXT.
           EXIT.
      *****************************************************************
      *    APIPARA�������
      *****************************************************************
       2001-APIPARA-DEL-SEC            SECTION.
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api001"        TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID      TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           IF    ( SPA-PRT-UUID    NOT =   SPACE )
               INITIALIZE                  SRPLNK-AREA
               MOVE    "DEL_GROUP"     TO  SRPLNK-MODE
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
           END-IF
      *
           .
       2001-APIPARA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    API�ѥ�᥿��������
      *****************************************************************
       900-APIPARA-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                  APIPARA-REC
      *
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api001"        TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID      TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    APIPARA-REC     TO MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  APIPARA-REC
               MOVE    ZERO        TO  FLG-APIPARA
           ELSE
               MOVE    1           TO  FLG-APIPARA
               INITIALIZE              APIPARA-REC
           END-IF
      *
           .
       900-APIPARA-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    API�ѥ�᥿FETCH����
      *****************************************************************
       900-APIPARA-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  APIPARA-REC
               MOVE    ZERO        TO  FLG-APIPARA
           ELSE
               MOVE    1           TO  FLG-APIPARA
               INITIALIZE              APIPARA-REC
           END-IF
      *
           .
       900-APIPARA-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
      *****************************************************************
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.