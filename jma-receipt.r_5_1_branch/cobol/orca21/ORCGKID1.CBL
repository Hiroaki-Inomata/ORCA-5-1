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
       PROGRAM-ID.                 ORCGKID1.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ��ǧ���̣��ʣˣɣģ���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  05/11/17  MONFUNC �б�
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
           COPY    "K01COMMON-SPA".
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
      *****COPY    "CPORCMCP.INC".
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
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
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
                   MOVE    SPACE           TO  SPA-KID1-FLG
                   PERFORM 210-BACK
      *     ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    "OK"            TO  SPA-KID1-FLG
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
           MOVE    "KID1"              TO  SPA-MOTOPG
      *
      *****MOVE    "BACK"              TO  MCP-PUTTYPE
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *