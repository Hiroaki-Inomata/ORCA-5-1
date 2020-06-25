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
       PROGRAM-ID.                     ORCBDELTEMPDIR.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Хå�����
      *  ����ݡ��ͥ��̾  : ����ǥ��쥯�ȥ���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  17/11/17    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
       WORKING-STORAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ����ǥ��쥯�ȥ��������
           COPY "CPORCSTEMPDIR.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 120-DBOPEN-SEC
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
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
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    SPACE           TO  MCPDATA-REC
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    SPACE           TO  MCPDATA-REC
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
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
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    SPACE           TO  MCPDATA-REC
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.