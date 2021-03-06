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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGKAID2.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ��ǧ���̣��ʣˣɣģ���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/05/22    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL��¿��Ǽ 02/08/26  ���޾���ȯ�Ԥ��ɲ�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���Ź԰��Ѷ��̥ѥ�᥿
           COPY    "KA01COMMON-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "KA01.INC".
           COPY    "KA02.INC".
           COPY    "KA021.INC".
           COPY    "KAERR.INC".
           COPY    "KAID1.INC".
           COPY    "KAID2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-KA01KYOUTU
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
           MOVE    SPA-KA01KYOUTU  TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE           TO  SPA-KAID2-FLG
                   PERFORM 210-BACK
      *     ���޾������
               WHEN    "CLICKED"       ALSO    "B05"
                   MOVE    "F5"            TO  SPA-KAID2-FLG
                   PERFORM 210-BACK
      *     �����������
               WHEN    "CLICKED"       ALSO    "B08"
                   MOVE    "F8"            TO  SPA-KAID2-FLG
                   PERFORM 210-BACK
      *     �����������޾������
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    "F9"            TO  SPA-KAID2-FLG
                   PERFORM 210-BACK
      *     ������Ģ�����޾������
               WHEN    "CLICKED"       ALSO    "B06"
                   MOVE    "F6"            TO  SPA-KAID2-FLG
                   PERFORM 210-BACK
      *     ������Ģ�����޾������
               WHEN    "CLICKED"       ALSO    "B07"
                   MOVE    "F7"            TO  SPA-KAID2-FLG
                   PERFORM 210-BACK
      *     ����
               WHEN    "CLICKED"       ALSO    "B10"
                   MOVE    "F10"            TO  SPA-KAID2-FLG
                   PERFORM 210-BACK
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "KAID2"             TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL   "ORCDBMAIN"          USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
