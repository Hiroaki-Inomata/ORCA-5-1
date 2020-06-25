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
       PROGRAM-ID.             ORCBQ04.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ԾȲ�
      *  ����ݡ��ͥ��̾  : ������λ�����ʣ��Уɡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/10/31    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����       ����
      *  03.05.00    NACL-����    07.05.23 ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    �Уգӣ��ѥե�����
           SELECT  PUSHFOUND-FILE  ASSIGN  PUSHFOUNDNAME
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PUSH.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    �Уգӣ��ѥե�����
       FD  PUSHFOUND-FILE.
       COPY    "CPPUSHFOUND.INC".
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //PUSHFOUNDNAME//.
      *
      *    ���ơ������ΰ�
       01  STS-AREA.
           03  STS-PUSH                PIC X(02).
      *
       01  WRK-PARA-AREA.
           03  WRK-PARA-ID             PIC X(36).
           03  WRK-PARA-KARTE-UID      PIC X(36).
           03  WRK-PARA-TBL-KEY        PIC X(08).
           03  WRK-PARA-STATE          PIC X(02).
           03  WRK-PARA-PUSHBASENAME   PIC X(32).
           03  WRK-PARA-MESSAGE        PIC X(80).
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ѥ�᡼���ե�����
           COPY    "CPBQ01.INC".
      *
      *    ���Уɥѥ�᥿
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA013.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ���饤�������¸�ģ����極��
           COPY    "CPORCSFILESV.INC".
      *
      *    �ѥ�᥿��������¸
           COPY    "CPORCSPARA.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPCOMMAND-PARAM.INC".
      *
      ******************************************************************
      *
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  SPA-AREA
      *                                WRK-AREA
                                       WRK-PARA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    SPA-HOSPNUM
                                               WRK-PARA-ID
                                               WRK-PARA-KARTE-UID
                                               SPA-TERMID
                                               WRK-PARA-TBL-KEY
                                               WRK-PARA-STATE
                                               WRK-PARA-PUSHBASENAME
                                               WRK-PARA-MESSAGE
           END-UNSTRING
      *
           PERFORM 120-DBOPEN-SEC
      *
           INITIALIZE                      SPARA-AREA
           MOVE    WRK-PARA-ID         TO  SPARA-ID
           MOVE    "DELETE"            TO  SPARA-REQUEST
           CALL    "ORCSPARA"          USING
                                       SPARA-AREA
                                       SPA-AREA
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    WRK-PARA-PUSHBASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  PUSHFOUNDNAME
           MOVE    SGETTEMP-TEMPDIR    TO  MCP-TEMPDIR
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    WRK-PARA-TBL-KEY    TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
      *    MCP-USER��DBOPEN�κݤ��ֵѤ���롣
           MOVE    MCP-USER            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  APIPARA-REC
      *
               INITIALIZE                  PARA013-REC
               MOVE    WRK-PARA-STATE  TO  PARA013-STATE
               IF    ( WRK-PARA-STATE  =   "02" )
                   OPEN INPUT  PUSHFOUND-FILE
                   READ PUSHFOUND-FILE
                   END-READ
                   IF    ( STS-PUSH    =   ZERO )
                       MOVE    PUSHFOUND-NUMBER-OF-RECORDS
                                       TO  PARA013-NUMBER-OF-RECORDS
                       MOVE    PUSHFOUND-NUMBER-OF-SHEETS
                                       TO  PARA013-NUMBER-OF-SHEETS
                   END-IF
                   CLOSE       PUSHFOUND-FILE
               END-IF
               MOVE    WRK-PARA-MESSAGE TO PARA013-MESSAGE
      *
               MOVE    PARA013-REC     TO  APIPARA-DATA-REC
               MOVE    APIPARA-REC     TO  MCPDATA-REC
               MOVE    "tbl_api_para"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
      *
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    �ģ¡���³����
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    SPACE           TO  MCPDATA-REC
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    SPACE           TO  MCPDATA-REC
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       120-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡����ǽ���
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    SPACE           TO  MCPDATA-REC
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    SPACE           TO  MCPDATA-REC
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
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
      *    �ģ£գУģ��ԣŽ���
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
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