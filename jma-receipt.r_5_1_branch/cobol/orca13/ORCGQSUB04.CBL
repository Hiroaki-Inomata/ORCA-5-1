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
       PROGRAM-ID.                     ORCGQSUB04.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ԾȲ�
      *  ����ݡ��ͥ��̾  : ���ԾȲ񥵥֡ʾȲ�HAORI��λ��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  16/10/04    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
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
      *
       01  CONST-AREA.
           03  CONST-FINDID            PIC X(08)   VALUE   "api013".
           03  CONST-FOUNDID           PIC X(08)   VALUE   "api01301".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ԾȲ�
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    �Хå����������
           COPY    "CPORCSMONBATCH.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           APIPARA-REC
           SPA-AREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
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
       100-INIT-SEC                SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    APIPARA-TERMID  TO  PTSRH-TERMID
           MOVE    "2"             TO  PTSRH-DATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del6"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4009"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    CONST-FINDID    TO  APIPARA-GYOUMUID
               MOVE    APIPARA-REC     TO  MCPDATA-REC
               MOVE    "tbl_api_para"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4010"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    CONST-FOUNDID   TO  APIPARA-GYOUMUID
               MOVE    APIPARA-REC     TO  MCPDATA-REC
               MOVE    "tbl_api_para"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4016"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               INITIALIZE                  SMONBATCH-AREA
               MOVE    "ORCBQ03"       TO  SMONBATCH-SHELLID
               MOVE    APIPARA-TERMID  TO  SMONBATCH-TERMID
               MOVE    "DEL"           TO  SMONBATCH-MODE
               CALL    "ORCSMONBATCH"      USING
                                           SMONBATCH-AREA
                                           SPA-AREA
           END-IF
      *
           .
       200-MAIN-EXT.
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
      *****************************************************************
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.