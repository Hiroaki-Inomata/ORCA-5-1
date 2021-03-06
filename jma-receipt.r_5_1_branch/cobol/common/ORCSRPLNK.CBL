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
       PROGRAM-ID.             ORCSRPLNK.
      *****************************************************************
      *  �����ƥ�̾        : ORCA
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ����饤��Ģɼ̾��¸
      *  ��������    ��ȼ�        ����
      *  19/09/03    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-APIPARA         PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-EDANUM          PIC 9(02).
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
       COPY    "CPORCSACCESS.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSRPLNK.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SRPLNK-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   WRK-AREA
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
           MOVE    ZERO            TO  SRPLNK-RC
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
      D    DISPLAY "SRPLNK-MODE = " SRPLNK-MODE
      D    DISPLAY "SPA-PRT-UUID= " SPA-PRT-UUID
      D    DISPLAY "SPA-OPID    = " SPA-OPID
      *
           EVALUATE    SRPLNK-MODE
           WHEN    "CHK_MONBLOB"
               INITIALIZE              SACCESS-AREA
               MOVE    "/usr/lib/panda/MONBLOB.so"
                                   TO  SACCESS-IN-FILENAME
               CALL    "ORCSACCESS"    USING
                                       SACCESS-AREA
      *
               MOVE    SACCESS-RC  TO  SRPLNK-RC
           WHEN    "INS"
               IF    ( SPA-PRT-UUID    NOT =   SPACE )
                AND  ( SPA-PRT-FLG     NOT =   SPACE )
                AND  ( SRPLNK-VAL      NOT =   SPACE )
      *
                   INITIALIZE                  APIPARA-REC
                   MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
                   MOVE    "RPLNK"         TO  APIPARA-GYOUMUID
                   MOVE    SPA-PRT-UUID    TO  APIPARA-TERMID
                   MOVE    SPA-OPID        TO  APIPARA-FILEMEI
                   PERFORM 800-EDANUM-SET-SEC
                   MOVE    WRK-EDANUM      TO  APIPARA-EDANUM
                   MOVE    SRPLNK-AREA     TO  APIPARA-DATA-REC
                   MOVE    APIPARA-REC     TO  MCPDATA-REC
                   MOVE    "tbl_api_para"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
      *
                   MOVE    APIPARA-REC     TO  MCPDATA-REC
                   MOVE    "tbl_api_para"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
      *
               END-IF
      *
           WHEN    "UPD"
      *
                   INITIALIZE                  APIPARA-REC
                   MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
                   MOVE    "RPLNK"         TO  APIPARA-GYOUMUID
                   MOVE    SPA-PRT-UUID    TO  APIPARA-TERMID
                   MOVE    SPA-OPID        TO  APIPARA-FILEMEI
                   PERFORM 800-EDANUM-SET-SEC
                   MOVE    WRK-EDANUM      TO  APIPARA-EDANUM
                   MOVE    SRPLNK-AREA     TO  APIPARA-DATA-REC
                   MOVE    APIPARA-REC     TO  MCPDATA-REC
                   MOVE    "tbl_api_para"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
      *
           WHEN    "SEL"
      *
               MOVE    SPACE               TO  SRPLNK-VAL
      *
               PERFORM 900-APIPARA-KEY-SEL-SEC
               IF    ( FLG-APIPARA     =   ZERO )
                   MOVE    APIPARA-DATA-REC
                                           TO  SRPLNK-AREA
      *
               ELSE
                   INITIALIZE              SRPLNK-AREA
                   STRING  "unknown_"      DELIMITED   BY  SIZE
                           SPA-PRT-FLG     DELIMITED   BY  SIZE
                   INTO    SRPLNK-VAL
                   END-STRING
               END-IF
           WHEN    "DEL_GROUP"
               INITIALIZE                  APIPARA-REC
               MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
               MOVE    "RPLNK"         TO  APIPARA-GYOUMUID
               MOVE    SPA-PRT-UUID    TO  APIPARA-TERMID
               MOVE    SPA-OPID        TO  APIPARA-FILEMEI
               MOVE    APIPARA-REC     TO  MCPDATA-REC
               MOVE    "tbl_api_para"  TO  MCP-TABLE
               MOVE    "del1"          TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
      *
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ���ֹ��������
      *****************************************************************
       800-EDANUM-SET-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-EDANUM
      *
           IF    ( FUNCTION TRIM(SPA-PRT-FLG) IS  NUMERIC )
               MOVE    FUNCTION NUMVAL (
                           FUNCTION TRIM(SPA-PRT-FLG)
                       )           TO  WRK-EDANUM
           END-IF
      *
           .
       800-EDANUM-SET-EXT.
           EXIT.
      *****************************************************************
      *     ���Уɥѥ�᥿��������
      *****************************************************************
       900-APIPARA-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-APIPARA
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "RPLNK"         TO  APIPARA-GYOUMUID
           MOVE    SPA-PRT-UUID    TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           PERFORM 800-EDANUM-SET-SEC
           MOVE    WRK-EDANUM      TO  APIPARA-EDANUM
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC
                                   TO  APIPARA-REC
           ELSE
               MOVE    1           TO  FLG-APIPARA
           END-IF
      *
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY-SEL-EXT.
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
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ɣΣӣţңԽ���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
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
      *    �ģ£գУģ��ԣŽ���
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
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
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
