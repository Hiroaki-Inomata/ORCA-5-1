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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGR06.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץȰ����ؼ�
      *  ����ݡ��ͥ��̾  : �ץ�󥿰������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/01/16    NACL��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �쥻�ץȺ����ؼ����̥ѥ�᥿
           COPY    "R01COMMON-SPA".
      *
      *    �쥻�ץȺ����ݰ����ؼ����̣ӣУ�
           COPY    "R02SCR-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDY                 PIC 9(04).
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
           COPY    "ORCA42SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-R02
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "RERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "R06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-R06-AREA
      *
      *    ���������������
           MOVE    "T"             TO  SPA-GMN-R06-SETTEI1
           MOVE    "F"             TO  SPA-GMN-R06-SETTEI2
      *
           EVALUATE    SPA-R01-MOTOPG
               WHEN    "R02"
                   MOVE    SPA-R01-PRTNM-G
                                   TO  SPA-GMN-R06-PRTNM-G
                   MOVE    SPA-R01-PRTNM-N-G
                                   TO  SPA-GMN-R06-PRTNM-N-G
               WHEN    "R04"
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                       IF      SPA-R01-PRTNMMEI
                                           =   SPA-GMN-PRTNMMEIL (IDY)
                           MOVE    SPA-GMN-PRTNMLST (IDY)
                                          TO  SPA-GMN-R06-PRTNM-G
                           MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDY
                       END-IF
                   END-PERFORM
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                       IF      SPA-R01-PRTNM-NMEI
                                           =   SPA-GMN-PRTNMMEIL (IDY)
                           MOVE    SPA-GMN-PRTNMLST (IDY)
                                          TO  SPA-GMN-R06-PRTNM-N-G
                           MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDY
                       END-IF
                   END-PERFORM
           END-EVALUATE      
      *
           MOVE    1               TO  SPA-GMN-R06-CUR
           .
       310-SPASET-EXT.
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
                   PERFORM 210-BACK
      *        ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-SETTEI-SEC
      *        ��������
               WHEN    "CLICKED"       ALSO    "SETTEI1"
                   PERFORM 400-SETTEI1-SEC
      *        ������
               WHEN    "CLICKED"       ALSO    "SETTEI2"
                   PERFORM 400-SETTEI2-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "PRTNM"
                   PERFORM 4110-PRTNM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTNM_N"
                   PERFORM 4120-PRTNM-N-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ץ�����Ͻ���
      *****************************************************************
       4110-PRTNM-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-R06-CUR
      *
           MOVE    R06-PRTNM           TO  SPA-GMN-R06-PRTNM-G
      * 
           IF      SPA-GMN-R06-PRTNM   =   SPACE   
               MOVE    SPACE           TO  SPA-GMN-R06-PRTNM-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                   IF      SPA-GMN-R06-PRTNM
                                           =   SPA-GMN-PRTNML (IDY)
                       MOVE    SPA-GMN-PRTNMLST (IDY)
                                              TO  SPA-GMN-R06-PRTNM-G
                       MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-GMN-R06-PRTNMMEI   =   SPACE
               MOVE    SPA-GMN-PRTNMLST (1)   TO  SPA-GMN-R06-PRTNM-G
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-GMN-R06-CUR
           END-IF 
           .
       4110-PRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ץ�󥿡�³������Ͻ���
      *****************************************************************
       4120-PRTNM-N-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-R06-CUR
      *
           MOVE    R06-PRTNM-N         TO  SPA-GMN-R06-PRTNM-N-G
      * 
           IF      SPA-GMN-R06-PRTNM-N =   SPACE   
               MOVE    SPACE           TO  SPA-GMN-R06-PRTNM-N-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                   IF      SPA-GMN-R06-PRTNM-N
                                           =   SPA-GMN-PRTNML (IDY)
                       MOVE    SPA-GMN-PRTNMLST (IDY)
                                              TO  SPA-GMN-R06-PRTNM-N-G
                       MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-GMN-R06-PRTNM-NMEI =   SPACE
               MOVE    SPA-GMN-PRTNMLST (1)   TO  SPA-GMN-R06-PRTNM-N-G
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    199                 TO  SPA-GMN-R06-CUR
           END-IF 
           .
       4120-PRTNM-N-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������������
      *****************************************************************
       400-SETTEI1-SEC          SECTION.
      *
           MOVE    R06-SETTEI1         TO  SPA-GMN-R06-SETTEI1
      *
           MOVE    1                   TO  SPA-GMN-R06-CUR
           .
       400-SETTEI1-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������������
      *****************************************************************
       400-SETTEI2-SEC          SECTION.
      *
           MOVE    R06-SETTEI2         TO  SPA-GMN-R06-SETTEI2
      *
           MOVE    1                   TO  SPA-GMN-R06-CUR
           .
       400-SETTEI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPACE               TO  SPA-RID1-FLG
           MOVE    SPACE               TO  SPA-R01-PRTNM-G
                                           SPA-R01-PRTNM-N-G
      *
           MOVE    SPA-R01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "R06"               TO  SPA-MOTOPG
     *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
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
      *    �������
      *****************************************************************
       450-SETTEI-SEC                  SECTION.
      *
           EVALUATE    TRUE
               WHEN    SPA-GMN-R06-SETTEI1 =   "T"
                   MOVE    "AL"                TO  SPA-RID1-FLG
               WHEN    SPA-GMN-R06-SETTEI2 =   "T"
                   MOVE    "PG"                TO  SPA-RID1-FLG
           END-EVALUATE
           MOVE    SPA-GMN-R06-PRTNM-G
                                       TO  SPA-R01-PRTNM-G
           MOVE    SPA-GMN-R06-PRTNM-N-G
                                       TO  SPA-R01-PRTNM-N-G
      *
           MOVE    SPA-R01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "R06"               TO  SPA-MOTOPG
     *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R06"               TO  MCP-WINDOW
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
           INITIALIZE                      R06
      *
           EVALUATE    SPA-R01-MOTOPG
               WHEN    "R02"
                   MOVE    WIDGET-NORMAL   TO  R06-SETTEI1-STATE
                                               R06-SETTEI2-STATE
               WHEN    "R04"
                   MOVE    WIDGET-INSENSITIVE
                                           TO  R06-SETTEI1-STATE
                                               R06-SETTEI2-STATE
           END-EVALUATE      
           MOVE    SPA-GMN-R06-SETTEI1 TO  R06-SETTEI1
           MOVE    SPA-GMN-R06-SETTEI2 TO  R06-SETTEI2
      *
           MOVE    SPA-GMN-R06-PRTNM-G TO  R06-PRTNM
           MOVE    SPA-GMN-R06-PRTNM-N-G
                                       TO  R06-PRTNM-N
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-PRTNM-MAX
               MOVE    SPA-GMN-PRTNMLST (IDY)
                                           TO  R06-PRTNMLST   (IDY)
                                               R06-PRTNM-NLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTNM-MAX   TO  R06-PRTNM-COUNT
                                           R06-PRTNM-N-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-R06-CUR
               WHEN    1
                   MOVE    "PRTNM"     TO  MCP-WIDGET
               WHEN    2
                   MOVE    "PRTNM_N"   TO  MCP-WIDGET
               WHEN    199
                   MOVE    "B12"       TO  MCP-WIDGET
           END-EVALUATE  
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.