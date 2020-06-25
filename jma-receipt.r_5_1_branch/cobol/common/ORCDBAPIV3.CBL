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
       PROGRAM-ID.             ORCDBAPIV3.
      *****************************************************************
      *  �����ƥ�̾        : ORCA
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ORCA API V3 �ѥ�᥿���������⥸�塼��
      *  �ֵѥ��顼������  : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  15/03/XX    NACL-¿��Ǽ     ��������
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
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FUNC                PIC X(64).
           03  WRK-TABLE               PIC X(64).
           03  WRK-RENNUMMAX           PIC 9(03).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *    �����ΰ�
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA3.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           MCPDATA3-REC
           SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
           MOVE    MCP-FUNC            TO  WRK-FUNC
           MOVE    ZERO                TO  MCP-RC
      *
           EVALUATE    TRUE
           WHEN  ( MCP-FUNC            =   "XMLOPEN"
                                    OR     "XMLCLOSE" )
               CONTINUE
           WHEN    OTHER
               PERFORM 100-INIT-SEC
               PERFORM 200-MAIN-SEC
           END-EVALUATE
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           MOVE    WRK-FUNC            TO  MCP-FUNC
           MOVE    ZERO                TO  MCP-LOGFLAG
      *
           .
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           .
      *
       100-INIT-EXT.
           EXIT.
           .
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *
           EVALUATE    MCP-FUNC
               WHEN    "DBOPEN"
               WHEN    "DBDISCONNECT"
               WHEN    "DBSTART"
               WHEN    "DBCOMMIT"
               WHEN    "PUTWINDOW"
                   MOVE    LOW-VALUE       TO  MCP-TABLE
                                               MCP-PATHNAME
               WHEN    OTHER
                   IF  MCP-FUNC            =   "DBFETCH"
                       MOVE    LOW-VALUE   TO  MCPDATA3-REC
                   END-IF
                   IF    ( SPA-NOCHK-HOSPNUM   =   "1" )
                       CONTINUE
                   ELSE
                       IF    ( SPA-HOSPNUM     =   ZERO )
                        OR   ( SPA-HOSPNUM(1:2)    IS  NOT NUMERIC )
      *
                           MOVE   1            TO  FLG-ERR
                           MOVE   9            TO  MCP-RC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF    ( FLG-ERR         =   ZERO )
      *
               MOVE    1           TO  MCP-VERSION
               MOVE    1           TO  MCP-REDIRECT
      *
               CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA3-REC
           END-IF
           .
      *
       200-MAIN-EXT.
           EXIT.