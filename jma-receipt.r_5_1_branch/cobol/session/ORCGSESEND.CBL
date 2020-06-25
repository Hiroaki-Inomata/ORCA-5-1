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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGSESEND.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������󥨥�ɽ���
      *  ����ݡ��ͥ��̾  : ü�����󥯥ꥢ
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  02/08/08    MCC�����ġ�   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION           SECTION.
       DATA                DIVISION.
       WORKING-STORAGE         SECTION.
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-LOCK            PIC 9(01).
           COPY    "COMMON-SPA".
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
           COPY    "CPTBLLOCK.INC".
      *    �ģ¸���
           COPY    "MCPDATA.INC".
       LINKAGE                 SECTION.
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "SESSION.INC".
      ************************************************************************
       PROCEDURE           DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
       000-MAIN                SECTION.
           DISPLAY   '==== ORCGSESEND'
           DISPLAY   MCP-WINDOW
           DISPLAY   MCP-WIDGET
           DISPLAY   MCP-EVENT
           DISPLAY   MCP-TERM
           DISPLAY   MCP-USER
           PERFORM 910-LOCK-DLT-SEC
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           EXIT    PROGRAM.
      *****************************************************************
      *    ��¾����������
      *****************************************************************
       910-LOCK-DLT-SEC            SECTION.
      *
           INITIALIZE                  SPA-AREA
           MOVE   "1"                  TO  SPA-NOCHK-HOSPNUM
           MOVE    MCP-TERM            TO  LOCK-TERMID
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_lock"      TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
               DISPLAY "FETCH=" MCP-RC
               IF  MCP-RC              =   ZERO
                 MOVE    "DBDELETE"      TO  MCP-FUNC
                 MOVE    "tbl_lock"      TO  MCP-TABLE
                 MOVE    "key"           TO  MCP-PATHNAME
                 CALL "ORCDBMAIN" USING  MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
                 DISPLAY "DELETE = " MCP-RC
               END-IF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
      *
           .
       910-LOCK-DLT-EXT.
           EXIT.
      *