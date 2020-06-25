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
       PROGRAM-ID.             ORCSDAILYKEY.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�
      *  ����ݡ��ͥ��̾  : ���󥭡�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/07/25    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-¿��Ǽ  07/05/08  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HOURS               PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD                    PIC X(10).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-DAILYKEY.
               05  WRK-DAILYKEY-YMD    PIC X(08).
               05  WRK-DAILYKEY-HM.
                   07  WRK-DAILYKEY-HH PIC 9(02).
                   07  WRK-DAILYKEY-MM PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
      *
      *    ����������
       01  HOURS-REC.
           COPY    "CPHOURS.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *D   COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY "CPORCSDAILYKEY.INC".
grpsys*
           COPY    "COMMON-SPA".
      *
        PROCEDURE                   DIVISION    USING
           SDAILYKEYAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SDAILYKEY-OT
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM.
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SDAILYKEY-CREYMD    =   SDAILYKEY-BASEYMD )
            OR   ( SDAILYKEY-BASEYMD   =   "FORCE" )
               MOVE    SDAILYKEY-CREYMD    TO  WRK-DAILYKEY-YMD
               MOVE    SDAILYKEY-CREHMS    TO  WRK-DAILYKEY-HM
               PERFORM 900-HOURS-KEY-SEL-SEC
               IF    ( FLG-HOURS           =   ZERO )

                   COMPUTE WRK-DAILYKEY-MM
                       =   WRK-DAILYKEY-MM     +   1
      *
                   IF    ( WRK-DAILYKEY-MM     >=  60 )
                       COMPUTE WRK-DAILYKEY-HH
                           =   WRK-DAILYKEY-HH +   1
                       MOVE    ZERO        TO  WRK-DAILYKEY-MM
                   END-IF
      *
                   IF    ( WRK-DAILYKEY-HH     >=  24 )
                       MOVE    "1"          TO  WRK-ZOGENPTN
                       MOVE    1            TO  WRK-ZOGEN
                       MOVE    WRK-DAILYKEY-YMD
                                            TO  WRK-SYMD
                       PERFORM 800-CALENDAR-SEC
                       MOVE    WRK-SYMD     TO  WRK-DAILYKEY-YMD
                       MOVE    ZERO         TO  WRK-DAILYKEY-HH
                   END-IF
               END-IF
           ELSE
               MOVE    SDAILYKEY-BASEYMD   TO  WRK-DAILYKEY-YMD
      *
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1039"              TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    SDAILYKEY-BASEYMD   TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC    TO  SYS-1039-REC
                   IF    ( SYS-1039-STIME  IS  NUMERIC )
                       MOVE    SYS-1039-STIME
                                           TO  WRK-DAILYKEY-HM
                   ELSE
                       MOVE    "0000"      TO  WRK-DAILYKEY-HM
                   END-IF
               ELSE
                       MOVE    "0000"      TO  WRK-DAILYKEY-HM
               END-IF
           END-IF
      *
           MOVE    WRK-DAILYKEY            TO  SDAILYKEY-DAILYKEY
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ������������
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ������ɹ���KEY10��
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ������������������
      *****************************************************************
       900-HOURS-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-HOURS
      *
           INITIALIZE                      HOURS-REC
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           MOVE    WRK-DAILYKEY-YMD    TO  HOURS-SRYYMD
           MOVE    WRK-DAILYKEY-HM     TO  HOURS-TIME
           MOVE    HOURS-REC           TO  MCPDATA-REC
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HOURS-REC
           ELSE
               MOVE    1               TO  FLG-HOURS
               MOVE    MCPDATA-REC     TO  HOURS-REC
           END-IF
      *
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HOURS-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.