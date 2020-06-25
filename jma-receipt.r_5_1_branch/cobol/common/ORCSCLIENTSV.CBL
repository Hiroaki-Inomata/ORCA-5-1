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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSCLIENTSV.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���饤�������¸
      *  ����ݡ��ͥ��̾  : ���饤�������¸���ݾ������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/08/01    NACL��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    14/08/28  �������ߥɥ��б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01). 
           03  FLG-OK                  PIC 9(01). 
      *
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��������
           COPY  "CPSK1010.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *     
      ******************************************************************
      *
       LINKAGE                 SECTION.
      *    ���饤�������¸���ݾ����������
           COPY    "CPORCSCLIENTSV.INC".
      *
           COPY    "COMMON-SPA".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                               USING   ORCSCLIENTSVAREA
                                       SPA-AREA
               .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    ��������
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           STRING  SPA-OPID            DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1010-TBL
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1010-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM         UNTIL   FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SPA-OPID            =   SYS-1010-USERID
                   MOVE    1                   TO  FLG-OK
                   MOVE    1                   TO  FLG-SYSKANRI
               ELSE
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *    ��������ʤ�
           IF     FLG-OK               =   ZERO
               MOVE    1                   TO  SCLIENTSV-RETURN
               INITIALIZE                  SYS-1010-REC
           ELSE
               MOVE    0                   TO  SCLIENTSV-RETURN
           END-IF
      *
           IF      SYS-1010-CLIENT-SAVE
                                       =   SPACE
               IF      SPA-MW          =   SPA-GINBEE
                   MOVE    "1"                 TO  SYS-1010-CLIENT-SAVE
               ELSE
                   MOVE    "0"                 TO  SYS-1010-CLIENT-SAVE
               END-IF 
           END-IF
           MOVE    SYS-1010-CLIENT-SAVE
                                       TO  SCLIENTSV-CLIENT-SAVE
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �����ޥ����ɤ߹���
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.