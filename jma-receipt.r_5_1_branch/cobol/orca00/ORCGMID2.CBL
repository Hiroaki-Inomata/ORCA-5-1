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
       PROGRAM-ID.             ORCGMID2.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��̳����
      *  ����ݡ��ͥ��̾  : ��ǧ���̣��ʣͣɣģ���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  09/01/28    NACL�ݿ���    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    �����ΰ�
           COPY    "M00COMMON-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *     
      **** COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
      *
       PROCEDURE               DIVISION    USING
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
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
      * 
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"     ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           EXIT    PROGRAM.
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
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *     ��ǧ
               WHEN    "CLICKED"   ALSO    "B12"
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
           MOVE    SPA-MOTOPG      TO  SPA-SAKIPG
           MOVE    "MID2"          TO  SPA-MOTOPG
      *
     ***   MOVE    "BACK"          TO  MCP-PUTTYPE
           MOVE   "JOIN"           TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *