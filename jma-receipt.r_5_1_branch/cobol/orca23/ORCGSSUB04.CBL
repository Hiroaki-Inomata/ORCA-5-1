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
       PROGRAM-ID.                     ORCGSSUB04.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��Ǽ
      *  ����ݡ��ͥ��̾  : ��Ǽ���֡ʼ�ǼHAORI��λ��
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
       01  WRK-AREA.
           03  WRK-TERM                PIC X(64).
           03  WRK-WINDOW              PIC X(64).
           03  WRK-MOTOPG              PIC X(08).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    ��¾����
           COPY    "CPORCSLOCK.INC".
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
           INITIALIZE              WRK-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 990-LOCK-OUT-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    APIPARA-REC     TO  MCPDATA-REC
               MOVE    "tbl_api_para"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4003"      TO  SPA-ERRCD
               END-IF
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
      *****************************************************************
      *    ��¾����������
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
           MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
           MOVE    MCP-WINDOW          TO  WRK-WINDOW
           MOVE    MCP-TERM            TO  WRK-TERM
           MOVE    APIPARA-TERMID      TO  MCP-TERM
      *
           MOVE    "S02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
      *
      *    ��¾����������
           MOVE    3                   TO  SLOCK-MODE
           MOVE    ZERO                TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
           MOVE    WRK-WINDOW          TO  MCP-WINDOW
           MOVE    WRK-TERM            TO  MCP-TERM
      *
           .
       990-LOCK-OUT-EXT.
           EXIT.