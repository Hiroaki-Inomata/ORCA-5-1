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
       PROGRAM-ID.             ORCSLDUSER.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �桼��������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  12/08/07    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  04.08.00    NACL�ݿ���    13/04/08  �����ƥ�桼������¤�ѹ�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
      *
       01  IDX-AREA.
           03  IDXT                PIC 9(05).
           03  IDXU                PIC 9(05).
      *
       01  SGYOUMU-CODE            PIC 9(03).
      *
           COPY    "COMMON-SPA"    REPLACING  //SPA-//
                                   BY         //WSPA-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�桼��
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
           COPY    "ORCA-SYSTEM".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSLDUSER.INC".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   SLDUSER-AREA
                                   SPA-AREA.
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
      *
           PERFORM 200-MAIN-SEC
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    SLDUSER-KBN
      *        ���������
               WHEN    1
                   PERFORM 2001-LISTENTRY-SEC
      *        ���̾������
               WHEN    2
                   PERFORM 2001-GETDATA-SEC
      *        ����å�����ɽ��
               WHEN    3
               WHEN    5
               WHEN    7
                   PERFORM 2001-SETMESSAGEALL-SEC
      *        ���̥�å�����ɽ��
               WHEN    4
               WHEN    6
               WHEN    8
                   PERFORM 2001-SETMESSAGE-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ���̥�å���������
      *****************************************************************
       2001-SETMESSAGE-SEC         SECTION.
      *
           IF      SLDUSER-ID      =   SPACE
               GO  TO  2001-SETMESSAGE-EXT
           END-IF
      *
           IF      SLDUSER-MESSAGE =   SPACE
               GO  TO  2001-SETMESSAGE-EXT
           END-IF
      *
           INITIALIZE                  ORCA-SYSTEM
           MOVE    SLDUSER-ID      TO  ORCA-SYSTEM-ID
      *
           EVALUATE    SLDUSER-KBN
      *        ���̥ݥåץ��å�
               WHEN    4
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-POPUP
      *        ���̥���������
               WHEN    6
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-DIALOG
      *        ���̥��ܡ���
               WHEN    8
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-ABORT
           END-EVALUATE
      *
           MOVE    "SETMESSAGE"    TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-SYSTEM
      *
           .
       2001-SETMESSAGE-EXT.
           EXIT.
      *****************************************************************
      *    ���ƥ�å���������
      *****************************************************************
       2001-SETMESSAGEALL-SEC      SECTION.
      *
           IF      SLDUSER-MESSAGE =   SPACE
               GO  TO  2001-SETMESSAGEALL-EXT
           END-IF
      *
           INITIALIZE                  ORCA-SYSTEM
      *
           EVALUATE    SLDUSER-KBN
      *        ���ƥݥåץ��å�
               WHEN    3
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-POPUP
      *        ���ƥ���������
               WHEN    5
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-DIALOG
      *        ���ƥ��ܡ���
               WHEN    7
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-ABORT
           END-EVALUATE
      *
           MOVE    "SETMESSAGEALL" TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-SYSTEM
      *
           .
       2001-SETMESSAGEALL-EXT.
           EXIT.
      *****************************************************************
      *    ���̾����������
      *****************************************************************
       2001-GETDATA-SEC            SECTION.
      *
           IF      SLDUSER-ID      =   SPACE
               GO  TO  2001-GETDATA-EXT
           END-IF
      *
           INITIALIZE                  ORCA-SYSTEM
           MOVE    SLDUSER-ID      TO  ORCA-SYSTEM-ID
           MOVE    "GETDATA"       TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-SYSTEM
           IF      MCP-RC          =   ZERO
               MOVE    1               TO  IDXT
               PERFORM 20011-GETVALUE-SEC
           END-IF
      *
           .
       2001-GETDATA-EXT.
           EXIT.
      *****************************************************************
      *    ü��������������
      *****************************************************************
       2001-LISTENTRY-SEC              SECTION.
      *
           INITIALIZE                  ORCA-SYSTEM
           MOVE    "SELECTALL"     TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-SYSTEM
           PERFORM VARYING IDXT    FROM    1   BY  1
                   UNTIL ( MCP-RC  NOT =   ZERO )
               MOVE    "FETCH"         TO  MCP-FUNC
               MOVE    "system"        TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           ORCA-SYSTEM
               IF      MCP-RC      =   ZERO
                   PERFORM 20011-GETVALUE-SEC
               END-IF
           END-PERFORM
      *
           MOVE    ZERO            TO  SLDUSER-OTHERS
           PERFORM VARYING IDXU    FROM    1   BY  1
                   UNTIL ( IDXU    >   SLDUSER-MAX )
      *
               IF    ( SLDUSER-HOSPNUM (IDXU)      =   SPA-HOSPNUM )
                AND  ( SLDUSER-LD      (IDXU)      =   SLDUSER-MY-LD )
                AND  ( SLDUSER-TERMID  (IDXU)  NOT =   MCP-TERM )
      *
                   COMPUTE SLDUSER-OTHERS  =  SLDUSER-OTHERS + 1
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2001-LISTENTRY-EXT.
           EXIT.
      *****************************************************************
      *    ü�������������
      *****************************************************************
       20011-GETVALUE-SEC          SECTION.
      *
           MOVE    ORCA-SYSTEM-ID  TO  SLDUSER-TERMID(IDXT)
           MOVE    ORCA-SYSTEM-IN-PROCESS
                                   TO  SLDUSER-IN-PROCESS(IDXT)
           MOVE    ORCA-SYSTEM-WINDOW
                                   TO  SLDUSER-WINDOW(IDXT)
           MOVE    ORCA-SYSTEM-USER
                                   TO  SLDUSER-USER(IDXT)
           MOVE    ORCA-SYSTEM-EVENT
                                   TO  SLDUSER-EVENT(IDXT)
           MOVE    ORCA-SYSTEM-ACCESS-TIME
                                   TO  SLDUSER-ACCESS-TIME(IDXT)
           MOVE    ORCA-SYSTEM-PROCESS-TIME
                                   TO  SLDUSER-PROCESS-TIME(IDXT)
           MOVE    ORCA-SYSTEM-TOTAL-PROCESS-TIME
                                   TO  SLDUSER-TOTAL-PROC(IDXT)
           MOVE    ORCA-SYSTEM-AGENT
                                   TO  SLDUSER-AGENT(IDXT)
           MOVE    ORCA-SYSTEM-HOST
                                   TO  SLDUSER-HOST(IDXT)
           MOVE    ORCA-SYSTEM-WIDGET
                                   TO  SLDUSER-WIDGET(IDXT)
           MOVE    ORCA-SYSTEM-CREATE-TIME
                                   TO  SLDUSER-CREATE-TIME(IDXT)
           PERFORM 900-SYSUSER-KEY-SEL-SEC
           MOVE    SYSUSER-HOSPNUM TO  SLDUSER-HOSPNUM(IDXT)
      *
           MOVE    SPA-AREA        TO  WSPA-AREA
           PERFORM 800-GYOUMUGET-SEC
           MOVE    WSPA-AREA       TO  SPA-AREA
           MOVE    SGYOUMU-CODE    TO  SLDUSER-LD(IDXT)
      *
           MOVE    IDXT            TO  SLDUSER-MAX
      *
           .
       20011-GETVALUE-EXT.
           EXIT.
      *****************************************************************
      *    ��̳��������
      *****************************************************************
       800-GYOUMUGET-SEC           SECTION.
      *
           MOVE    ZERO            TO  SGYOUMU-CODE
      *
           MOVE    SLDUSER-WINDOW(IDXT)
                                   TO  SPA-SAKIPG
      *
           EVALUATE    TRUE
           WHEN  ( SPA-SAKIPG(1:1)     =   "X" )
               IF    (  SPA-SAKIPG(2:1)     IS  NUMERIC )
                  OR (  SPA-SAKIPG(1:4)     =   "XERR" )
                  OR (  SPA-SAKIPG(1:3)     =   "XID" )
                   CONTINUE
               ELSE
                   MOVE    "?"         TO  SPA-SAKIPG(1:1)
               END-IF
           WHEN  ( SPA-SAKIPG               =   "P97" )
                   MOVE    "?"         TO  SPA-SAKIPG(1:1)
           END-EVALUATE
      *
           CALL    "ORCSGYOUMUGET"     USING
                                       SPA-AREA
                                       SGYOUMU-CODE
      *
           .
       800-GYOUMUGET-EXT.
           EXIT.
      *****************************************************************
      *    �桼�����Ķ���������
      *****************************************************************
       900-SYSUSER-KEY-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSUSER
      *
           INITIALIZE                  SYSUSER-REC
           MOVE   ORCA-SYSTEM-USER TO  SYSUSER-USERID
           MOVE    SYSUSER-REC     TO  MCPDATA-REC
           MOVE    "tbl_sysuser"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
      *
           MOVE    "tbl_sysuser"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSUSER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.