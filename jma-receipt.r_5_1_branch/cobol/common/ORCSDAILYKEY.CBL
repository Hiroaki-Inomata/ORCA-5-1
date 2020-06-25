      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 日報キー作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/07/25    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HOURS               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    一時領域
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
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
      *
      *    締め時刻管理
       01  HOURS-REC.
           COPY    "CPHOURS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *D   COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
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
      *    主　　処理
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
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
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
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
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
      *    管理マスター読込（KEY10）
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
      *    締め時刻管理検索処理
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
      *    ＤＢ検索処理
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
      *    ＤＢ検索処理
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
      *    ＤＢ読み込み処理
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
      *    ＤＢクローズ処理
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
