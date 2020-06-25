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
       PROGRAM-ID.             ORCSPRTNMGET.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �ץ�󥿡�̾��������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/05/29    NACL-ƣ��     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.01.01    NACL-ƣ��    06/07/11  ϫ�ҡ������դΥץ�󥿻����б�
      *                                     ����������������
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.02.01    NACL-ƣ��    08/03/04  ʿ������ǯ��������б�
      *                                     ����Ϣ��ץ��������ɲ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  WRK-AREA. 
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      * 
           03  WRK-PRTNM           PIC X(16).
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ������ץ��̾������ƾ���
           COPY    "CPSK1031.INC".
      *
      *    �����ץ�󥿾���
           COPY    "CPSK1034.INC".
      *
      *    ������ץ��̾������ƾ��� ��������
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC".
      *
      *    ��ñ�Ȼ��Ⱦ���
           COPY    "CPSK3004.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY  "CPORCSPRTNMGET.INC".
grpsys*
grpsys COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                   ORCSPRTNMGETAREA
                                   SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
      *
      *    ������ץ��̾������ƾ���
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                      SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    PRTNMGET-YMD        TO  SYS-1031-STYUKYMD
                                           SYS-1031-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1031-HOSPNUM
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-SELECT-SEC
      *
           PERFORM UNTIL   FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       20
                   MOVE    SYS-1031-PRTNM (IDX)    TO  WRK-PRTNM
                   PERFORM 100-PRTNM-HENSYU-SEC
               END-PERFORM
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ������ץ��̾������ƾ��� ��������
           MOVE    SPACE               TO  SYS-1034-REC
           INITIALIZE                      SYS-1034-REC
           MOVE    "1034"              TO  SYS-1034-KANRICD
           MOVE    PRTNMGET-YMD        TO  SYS-1034-STYUKYMD
                                           SYS-1034-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1034-HOSPNUM
           MOVE    SYS-1034-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-SELECT-SEC
      *
           PERFORM UNTIL   FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1034-REC
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       20
                   MOVE    SYS-1034-PRTNM (IDX)    TO  WRK-PRTNM
                   PERFORM 100-PRTNM-HENSYU-SEC
               END-PERFORM
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    �쥻�ץȡ������Խ�����
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    PRTNMGET-YMD        TO  SYS-2005-STYUKYMD
                                           SYS-2005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-SELECT-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
      *
               MOVE    SYS-2005-SYAPRTNM   TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-KKHPRTNM   TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-SYAPRTNM-NEXT
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-KKHPRTNM-NEXT
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-SYAPRTNM-N 
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-KKHPRTNM-N TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-SYAPRTNM-NEXT-N
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-KKHPRTNM-NEXT-N
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-ROUPRTNM   TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-JBAIPRTNM  TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-ROUPRTNM-NEXT
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-JBAIPRTNM-NEXT
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-ROUPRTNM-N 
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-JBAIPRTNM-N
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-ROUPRTNM-NEXT-N
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-2005-JBAIPRTNM-NEXT-N
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
           END-IF     
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    �쥻�ץȡ������Խ�����ʶ�ʬ�����ɣ�����
           MOVE    SPACE               TO  SYS-200501-REC
           INITIALIZE                      SYS-200501-REC
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    PRTNMGET-YMD        TO  SYS-200501-STYUKYMD
                                           SYS-200501-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-SELECT-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-200501-REC
      *
               MOVE    SYS-200501-KOUIKIPRTNM
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-200501-KOUIKIPRTNM-NEXT
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-200501-KOUIKIPRTNM-N 
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
               MOVE    SYS-200501-KOUIKIPRTNM-NEXT-N
                                           TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
           END-IF     
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ��ñ�Ȼ��Ⱦ���
           MOVE    SPACE               TO  SYS-3004-REC 
           INITIALIZE                      SYS-3004-REC
           MOVE    "3004"              TO  SYS-3004-KANRICD
           MOVE    "*"                 TO  SYS-3004-KBNCD
           MOVE    PRTNMGET-YMD        TO  SYS-3004-STYUKYMD
                                           SYS-3004-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-3004-HOSPNUM
           MOVE    SYS-3004-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-SELECT-SEC
      *
           PERFORM         UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-3004-REC
               MOVE    SYS-3004-PRTNM      TO  WRK-PRTNM
               PERFORM 100-PRTNM-HENSYU-SEC
      * 
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    �ץ��̾�¤��Ѥ�
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       PRTNMGET-PRTNM-MAX
               IF      IDX         <   PRTNMGET-PRTNM-MAX
                   PERFORM VARYING IDY     FROM    IDX BY  1
                           UNTIL   IDY     >       PRTNMGET-PRTNM-MAX
                       IF    ( IDY             >   IDX                 )
                       AND   ( PRTNMGET-PRTNM (IDX)
                                               >   PRTNMGET-PRTNM (IDY))
                           MOVE     PRTNMGET-PRTNM (IDX)   TO  WRK-PRTNM
                           MOVE     PRTNMGET-PRTNM (IDY)   TO
                                                   PRTNMGET-PRTNM (IDX)
                           MOVE     WRK-PRTNM              TO
                                                   PRTNMGET-PRTNM (IDY)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF      PRTNMGET-PRTNM-MAX  =   ZERO
               MOVE    1                   TO  PRTNMGET-RC
           END-IF
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �ץ�󥿡�̾�Խ�����
      *****************************************************************
       100-PRTNM-HENSYU-SEC            SECTION.
      *
           IF      WRK-PRTNM           =   SPACE
               GO  TO  100-PRTNM-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       100
               IF      PRTNMGET-PRTNM (IDY)    =   SPACE
                   MOVE     WRK-PRTNM      TO  PRTNMGET-PRTNM (IDY)
                   ADD     1               TO  PRTNMGET-PRTNM-MAX
               END-IF
               IF      PRTNMGET-PRTNM (IDY)    =   WRK-PRTNM
                   MOVE   100              TO  IDY
               END-IF
           END-PERFORM
           .
       100-PRTNM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ�����������
      *****************************************************************
       900-SYSKANRI-SELECT-SEC     SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɤ߹��߽���
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      