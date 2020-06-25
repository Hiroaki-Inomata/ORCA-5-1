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
       PROGRAM-ID.             ORCGW142.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ƥ������������
      *  ����ݡ��ͥ��̾  : ��ñ�Ȼ�������ʣģ����������
      *  ������            :
      *  ��������    ��ȼ�       ����
      *  16/11/21    NACL-ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      ****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ƥ�������������Ѷ��̥ѥ�᥿
           COPY    "W01COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "W14SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION
                                   USING       MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W14FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W14FREE     TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF 
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W142"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"   ALSO    "B01"
                   PERFORM 210-BACK
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ý���
      *****************************************************************
       200-ENTRY                   SECTION.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W14"           TO  SPA-SAKIPG
           MOVE    "W142"          TO  SPA-MOTOPG
      *
           MOVE   "JOIN"           TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *     
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "W142"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      * 
           INITIALIZE              W142
      *
           MOVE    ZERO            TO  IDY
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       25
               IF      SPA-GMN-NAME (IDX) 
                                       NOT =   SPACE
                   ADD     1                       TO  IDY
                   MOVE    SPA-GMN-TCD     (IDX)   TO  W142-CD     (IDY)
                   MOVE    SPA-GMN-SHELLID (IDX)   TO  W142-SHELLID(IDY)
                   MOVE    SPA-GMN-NAME    (IDX)   TO  W142-NAME   (IDY)
               END-IF
           END-PERFORM
           MOVE    IDY             TO  W142-DOCER-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           MOVE    "B01"           TO  MCP-WIDGET
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    �УգԽ���
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.