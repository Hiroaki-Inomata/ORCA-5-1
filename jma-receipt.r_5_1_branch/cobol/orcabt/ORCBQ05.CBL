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
       PROGRAM-ID.             ORCBQ05.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ԾȲ�
      *  ����ݡ��ͥ��̾  : ������λ����
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //CSVNAME//.
      *
       01  WRK-AREA.
           03  WRK-RENNUM              PIC 9(03).
      *
       01  WRK-PARA-AREA.
           03  WRK-PARA-ID             PIC X(36).
           03  WRK-PARA-CSVFILENAME    PIC X(100).
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
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ѥ�᥿��������¸
           COPY    "CPORCSPARA.INC".
      *
      *    ���饤�������¸�ģ����極��
           COPY    "CPORCSFILESV.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
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
                                       WRK-AREA
                                       WRK-PARA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    SPA-HOSPNUM
                                               WRK-PARA-ID
                                               WRK-PARA-CSVFILENAME
           END-UNSTRING
      *
           PERFORM 120-DBOPEN-SEC
      *
           INITIALIZE                      SPARA-AREA
           MOVE    "LOAD_AND_DELETE"   TO  SPARA-REQUEST
           MOVE    WRK-PARA-ID         TO  SPARA-ID
           CALL    "ORCSPARA"          USING
                                       SPARA-AREA
                                       SPA-AREA
           MOVE    SPARA-DATA          TO  BQ01
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    WRK-PARA-CSVFILENAME
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  CSVNAME
           MOVE    SGETTEMP-TEMPDIR    TO  MCP-TEMPDIR
      *
           IF    ( BQ01-FILESVKBN        =   "1" )
               MOVE    1               TO  WRK-RENNUM
      *
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "I"             TO  SFILESV-MODE
               MOVE    "ORCBQ01"       TO  SFILESV-TBL-KEY
               MOVE    "ORCBQ01.sh"    TO  SFILESV-SHELLID  (WRK-RENNUM)
               MOVE    1               TO  SFILESV-SHORI-RENNUM
                                                            (WRK-RENNUM)
               MOVE    BQ01-SYSTIME    TO  SFILESV-SRYYM (WRK-RENNUM)
               MOVE    WRK-RENNUM      TO  SFILESV-RENNUM   (WRK-RENNUM)
               MOVE    MCP-TEMPDIR     TO  SFILESV-FOR-FOLDER
                                                            (WRK-RENNUM)
               INITIALIZE                  ORCA-BLOB
               MOVE    "BLOBIMPORT"    TO  MCP-FUNC
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "blob"          TO  MCP-TABLE
               MOVE    CSVNAME         TO  ORCA-BLOB-FILE
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           ORCA-BLOB
                                           SPA-AREA
               MOVE    ORCA-BLOB-OBJECT
                                       TO  SFILESV-FOR-DATA (WRK-RENNUM)
               MOVE    BQ01-DIRNAME    TO  SFILESV-TO-FOLDER(WRK-RENNUM)
               MOVE    BQ01-FILENAME   TO  SFILESV-TO-DATA  (WRK-RENNUM)
               MOVE    BQ01-CSV-ROWS   TO  SFILESV-CNT-ALL  (WRK-RENNUM)
               EVALUATE                    BQ01-OUTKBN
               WHEN    "2"
                   MOVE    "���԰�����������"
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
               WHEN    "3"
                   MOVE    "���԰����ʿ��Ź԰١�"
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
               WHEN    OTHER
                   MOVE    "���԰���"  TO  SFILESV-TITLE    (WRK-RENNUM)
               END-EVALUATE
      *
               CALL    "ORCSFILESV"        USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
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