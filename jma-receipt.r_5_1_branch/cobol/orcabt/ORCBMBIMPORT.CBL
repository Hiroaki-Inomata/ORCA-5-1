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
       PROGRAM-ID.             ORCBMBIMPORT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����
      *  ����ݡ��ͥ��̾  : ������API�ؤ���Ͽ
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  19/02/08    NACL-����     ��������
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
       01  WRK-AREA.
           03  WRK-CONTENT-TYPE    PIC X(100).
           03  WRK-LIFETYPE        PIC X(16).
           03  WRK-RETURN-CODE     PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "MONBLOB".
      *
      ****************************************************************
       LINKAGE                     SECTION.
           COPY    "CPCOMMAND-PARAM.INC".
      ****************************************************************
       PROCEDURE                   DIVISION
           USING
           COMMAND-PARAM.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
                                       MONBLOB
      *
           MOVE    ZERO            TO  WRK-RETURN-CODE
      *
           MOVE    SPACE           TO  MCPDATA-REC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED BY ","
           INTO                    MONBLOB-ID
                                   MONBLOB-FILENAME
                                   WRK-CONTENT-TYPE
                                   WRK-LIFETYPE
           END-UNSTRING
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    MONBLOB-FILENAME    TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  MONBLOB-FILENAME
           STRING  "application/"      DELIMITED   BY  SIZE
                   WRK-CONTENT-TYPE    DELIMITED   BY  SPACE
           INTO    MONBLOB-CONTENT-TYPE
           END-STRING
           MOVE    "MONBLOBIMPORT"     TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "monblob"           TO  MCP-TABLE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MONBLOB
      *
           IF    ( MCP-RC              =   ZERO )
               IF    ( FUNCTION UPPER-CASE(WRK-LIFETYPE)
                                       =   "SHORT" )
                   INITIALIZE              APIPARA-REC
                   MOVE    MONBLOB-ID  TO  APIPARA-TERMID
                   MOVE    APIPARA-REC TO  MCPDATA-REC
                   MOVE    "tbl_api_para"
                                       TO  MCP-TABLE
                   MOVE    "upd_monblob_short_life"
                                       TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    1       TO  WRK-RETURN-CODE
                   END-IF
               END-IF
           ELSE
               MOVE    1               TO  WRK-RETURN-CODE
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           MOVE    WRK-RETURN-CODE     TO  RETURN-CODE
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£գУģ��ԣŽ���
      *****************************************************************
       910-DBUPDATE-SEC            SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBUPDATE-EXT.
           EXIT.