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
       PROGRAM-ID.             ORCGL06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 日次統計締め処理画面
      *  管理者            :
      *  05/07/13    NACL-太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07.05.15  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    日次統計共通パラメタ
           COPY    "L01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "L01SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-HOURS                   PIC 9(01).
           03  FLG-KARITIME                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX-TERM                    PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-CHK                     PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-PATHNAME            PIC X(64).
           03  WRK-SYMD                    PIC X(10).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-YOBI                    PIC 9(01).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-FROMYMD.
               05  WRK-FROMYMD-YY          PIC 9(04).
               05  WRK-FROMYMD-MM          PIC 9(02).
               05  WRK-FROMYMD-DD          PIC 9(02).
           03  WRK-FROMTIME.
               05  WRK-FROMTIME-HH         PIC 9(02).
               05  WRK-FROMTIME-MM         PIC 9(02).
           03  WRK-KJNYMD.
               05  WRK-KJNYMD-YY           PIC 9(04).
               05  WRK-KJNYMD-MM           PIC 9(02).
               05  WRK-KJNYMD-DD           PIC 9(02).
           03  WRK-KJNHM.
               05  WRK-KJNHM-HH            PIC 9(02).
               05  WRK-KJNHM-MM            PIC 9(02).
           03  WRK-Z                       PIC Z.
           03  WRK-Z9                      PIC Z9.
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-IDX             PIC 9(05).
               05  WRK-CMB-ITM             PIC X(200).
               05  WRK-CMB-ITM2            PIC X(200).
           03  WRK-YMD-G.
               05  WRK-YMD-YY              PIC 9(04).
               05  WRK-YMD-MM              PIC 9(02).
               05  WRK-YMD-DD              PIC 9(02).
           03  WRK-HMS-G.
               05  WRK-HMS-HH              PIC 9(02).
               05  WRK-HMS-MM              PIC 9(02).
               05  WRK-HMS-SS              PIC 9(02).
           03  WRK-TIME-G.
               05  WRK-TIME-HH             PIC 99.
               05  WRK-TIME-SEP            PIC X.
               05  WRK-TIME-MM             PIC 99.
           03  WRK-SRYYMD                  PIC X(08).
           03  WRK-LIDMSG                  PIC X(200).
      *
       01  CONST-AREA.
           03  CONST-TERM-MAX              PIC 9(03)   VALUE   10.
           03  CONST-GOBACK-DAYS           PIC S9(03)  VALUE   -10.
           03  CONST-GOBACK-MONTH          PIC S9(03)  VALUE   -1.
           03  CONST-PROCKBN-G.
               05  CONST-SIME              PIC X(01)   VALUE   "1".
               05  CONST-SIME-KARI         PIC X(01)   VALUE   "2".
               05  CONST-TORIKESI          PIC X(01)   VALUE   "3".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK3005.INC".
      *
      *    締め処理
       01  HOURS-REC.
           COPY    "CPHOURS.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    カナチェック
           COPY    "CPORCXKANACONV.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-L01FREE
           MOVE    SPA-WORK        TO  SPA-L01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-L01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-L01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "LERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "L06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "LID1"
               PERFORM 330-LID1-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                      SPA-L06-AREA
           MOVE    SPACE               TO  SPA-L01-SAKIPG
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1039"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-1039-REC
           ELSE
               INITIALIZE                  SYS-1039-REC
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-YOBI-HEN-SEC
      *
           MOVE    ZERO                TO  FLG-KARITIME
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   2 )
                    OR   ( FLG-KARITIME    =   1 )
      *
               IF    ( SYS-1039-KARIDAY (IDX3 WRK-YOBI)
                                       =   "1" )
                   PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   2 )
                        OR   ( FLG-KARITIME    =   1 )
                       IF    ( SYS-1039-KARISTTIME (IDX3 IDX4)
                                           =   SPACE OR ZERO )
                        AND  ( SYS-1039-KARIEDTIME (IDX3 IDX4)
                                           =   SPACE OR ZERO )
                           CONTINUE
                       ELSE
                           IF    ( SMCNDATE-HMS (1:4)
                                  >=  SYS-1039-KARISTTIME (IDX3 IDX4))
                            AND  ( SMCNDATE-HMS (1:4)
                                  <=  SYS-1039-KARIEDTIME (IDX3 IDX4))
                               MOVE    1   TO  FLG-KARITIME
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           PERFORM 41011-SIME-SYOKI-SEC
      *
           IF    ( FLG-KARITIME            =   ZERO )
            OR   ( SPA-NAI-L06-STSRYYMD    =   SPACE )
               MOVE    SPA-GMN-L06-HOURSKBNLST (1)
                                       TO  SPA-GMN-L06-HOURSKBN-G
           ELSE
               MOVE    SPA-GMN-L06-HOURSKBNLST (2)
                                       TO  SPA-GMN-L06-HOURSKBN-G
           END-IF
      *
           MOVE    7               TO  SPA-GMN-L06-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＬＩＤ１）ＯＫ処理
      *****************************************************************
       330-LID1-SET-SEC            SECTION.
      *
           IF    ( SPA-LID1-FLG    =   "OK" )
               EVALUATE    SPA-LIDCD
               WHEN    "3001"
               WHEN    "3002"
                   PERFORM 2301-TORIKESI-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  SPA-L99-FLG
           MOVE    SPACE               TO  SPA-LID1-FLG
           MOVE    SPACE               TO  SPA-LIDCD
      *
           .
       330-LID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
           PERFORM 31011-HOURSKBNLST-SET-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    締め区分コンボ処理
      *****************************************************************
       31011-HOURSKBNLST-SET-SEC   SECTION.
      *
           INITIALIZE                  SPA-GMN-L06-HOURSKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-L06-HOURSKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-L06-HOURSKBNL   (1)
           MOVE    "本締め"        TO  SPA-GMN-L06-HOURSKBNMEIL(1)
      *
           MOVE    "2"             TO  SPA-GMN-L06-HOURSKBNL   (2)
           MOVE    "仮締め"        TO  SPA-GMN-L06-HOURSKBNMEIL(2)
      *
           .
       31011-HOURSKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    開始診療日・時刻編集処理(+1秒)
      *****************************************************************
       31011-FROMDATE-EDIT-SEC         SECTION.
      *
           COMPUTE WRK-FROMTIME-MM
               =   WRK-FROMTIME-MM     +   1
      *
           IF    ( WRK-FROMTIME-MM     >=  60 )
               COMPUTE WRK-FROMTIME-HH
                   =   WRK-FROMTIME-HH +   1
               MOVE    ZERO        TO  WRK-FROMTIME-MM
           END-IF
      *
           IF    ( WRK-FROMTIME-HH     >=  24 )
               MOVE    "1"          TO  WRK-ZOGENPTN
               MOVE    1            TO  WRK-ZOGEN
               MOVE    WRK-FROMYMD  TO  WRK-SYMD
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD     TO  WRK-FROMYMD
               MOVE    ZERO         TO  WRK-FROMTIME-HH
           END-IF
      *
           .
       31011-FROMDATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    開始診療日・時刻編集処理(-1秒)
      *****************************************************************
       31012-FROMDATE-EDIT-SEC         SECTION.
      *
           IF    ( WRK-FROMTIME-MM      =  0 )
      *
               MOVE    59           TO  WRK-FROMTIME-MM
      *
               IF    ( WRK-FROMTIME-HH  =  0 )
                   MOVE    "1"          TO  WRK-ZOGENPTN
                   MOVE    -1           TO  WRK-ZOGEN
                   MOVE    WRK-FROMYMD  TO  WRK-SYMD
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-SYMD     TO  WRK-FROMYMD
                   MOVE    23           TO  WRK-FROMTIME-HH
               ELSE
                   COMPUTE WRK-FROMTIME-HH
                       =   WRK-FROMTIME-HH     -   1
               END-IF
           ELSE
               COMPUTE WRK-FROMTIME-MM
                   =   WRK-FROMTIME-MM     -   1
           END-IF
      *
           .
       31012-FROMDATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 230-CLEAR-SEC
      *    前回取消
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 230-TORIKESI-SEC
      *    詳細
           WHEN    "CLICKED"       ALSO    "B04"
               MOVE    "L04"       TO  SPA-L01-SAKIPG
               MOVE    "JOIN"      TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 212-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
      *    WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *    END-EVALUATE
      *
           .
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-L06-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    L06-HOURSKBN    TO  SPA-GMN-L06-HOURSKBN-G
           MOVE    L06-EDSRYYMD    TO  SPA-GMN-L06-EDSRYYMD
           MOVE    L06-EDHR        TO  SPA-GMN-L06-EDHR
           MOVE    L06-EDMIN       TO  SPA-GMN-L06-EDMIN
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HOURSKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-EDSRYYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-EDHR-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-EDMIN-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め初期処理
      *****************************************************************
       41011-SIME-SYOKI-SEC            SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                  HOURS-REC
      *
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           MOVE    "1"                 TO  HOURS-HOURSKBN
           MOVE    1                   TO  HOURS-LIMIT
           PERFORM 900-HOURS-KEY5-SEL-SEC
           IF    ( FLG-HOURS           =   ZERO )
               MOVE    "0"             TO  SPA-GMN-L06-PREHOURSKBN
               MOVE    HOURS-SRYYMD    TO  WRK-FROMYMD
               MOVE    HOURS-TIME      TO  WRK-FROMTIME
               PERFORM 31011-FROMDATE-EDIT-SEC
      *
               MOVE    WRK-FROMYMD     TO  WRK-SYMD
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-L06-STSRYYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-L06-STSRYYMD
      *
               MOVE    WRK-FROMTIME    TO  WRK-HMS-G
               MOVE    WRK-HMS-HH      TO  SPA-GMN-L06-STHR
               MOVE    WRK-HMS-MM      TO  SPA-GMN-L06-STMIN
           END-IF
      *
           INITIALIZE                  SPA-GMN-L06-PRE-AREA
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           MOVE    "%"                 TO  HOURS-HOURSKBN
           MOVE    1                   TO  HOURS-LIMIT
           PERFORM 900-HOURS-KEY5-SEL-SEC
           IF    ( FLG-HOURS           =   ZERO )
               MOVE    HOURS-HOURSKBN  TO  SPA-GMN-L06-PREHOURSKBN
               MOVE    HOURS-SRYYMD    TO  WRK-SYMD
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-L06-PREEDSRYYMD
               MOVE    HOURS-TIME      TO  SPA-GMN-L06-PREEDTIME
               IF    ( HOURS-HOURSKBN  =   "1" )
                   PERFORM 900-HOURS-KEY3-SEL-SEC
                   IF    ( FLG-HOURS           =   ZERO )
                       MOVE    HOURS-SRYYMD  TO  WRK-FROMYMD
                       MOVE    HOURS-TIME    TO  WRK-FROMTIME
                       PERFORM 31011-FROMDATE-EDIT-SEC
                       MOVE    WRK-FROMYMD   TO  WRK-SYMD
                       PERFORM 800-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG   TO  SPA-GMN-L06-PRESTSRYYMD
                       MOVE    WRK-FROMTIME  TO  SPA-GMN-L06-PRESTTIME
                   ELSE
                       MOVE    "0"           TO  SPA-GMN-L06-PREHOURSKBN
                       MOVE    SPA-GMN-L06-STSRYYMD
                                             TO  SPA-GMN-L06-PRESTSRYYMD
                       MOVE    SPA-GMN-L06-STTIME
                                             TO  SPA-GMN-L06-PRESTTIME
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  SPA-NAI-L06-EDSRYYMD
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-L06-EDSRYYMD
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS-G
           MOVE    WRK-HMS-HH          TO  SPA-GMN-L06-EDHR
           MOVE    WRK-HMS-MM          TO  SPA-GMN-L06-EDMIN
      *
           MOVE    212                 TO  SPA-GMN-L06-CUR
      *
           .
       41011-SIME-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    診療日至チェック処理
      *****************************************************************
       4101-EDSRYYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-L06-EDSRYYMD
                                   TO  WRK-SRYYMD
           MOVE    SPACE           TO  SPA-NAI-L06-EDSRYYMD
      *
           MOVE    SPA-GMN-L06-EDSRYYMD    TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( SGDAY-RC        =   ZERO )
               MOVE    SGDAY-OTDAY TO  SPA-GMN-L06-EDSRYYMD
               MOVE    SGDAY-SDAY  TO  SPA-NAI-L06-EDSRYYMD
           ELSE
               MOVE    WRK-SRYYMD  TO  WRK-SYMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    SGDAY-OTDAY TO  SPA-GMN-L06-EDSRYYMD
               MOVE    SGDAY-SDAY  TO  SPA-NAI-L06-EDSRYYMD
      *
               MOVE    "0001"      TO  SPA-ERRCD
               MOVE    7           TO  SPA-GMN-L06-CUR
           END-IF
      *
           .
       4101-EDSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    時至編集処理
      *****************************************************************
       4101-EDHR-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-L06-EDHR    >   23 )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-L06-CUR
           END-IF
      *
           .
       4101-EDHR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分至編集処理
      *****************************************************************
       4101-EDMIN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-L06-EDMIN   >   59 )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    9               TO  SPA-GMN-L06-CUR
           END-IF
      *
           .
       4101-EDMIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め区分チェック処理
      *****************************************************************
       4101-HOURSKBN-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-L06-HOURSKBN-G  NOT =   SPACE )
               MOVE    SPA-GMN-L06-HOURSKBN    TO  WRK-CMB-CD
               PERFORM 4200-HOURSKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM             =   SPACE )
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN-L06-CUR
               END-IF
           END-IF
      *
           .
       4101-HOURSKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め区分コンボチェック処理
      *****************************************************************
       4200-HOURSKBNLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-L06-HOURSKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-L06-HOURSKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-L06-HOURSKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-HOURSKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "L01"               TO  SPA-SAKIPG
           MOVE    "L06"               TO  SPA-MOTOPG
      *
           MOVE    "L01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       230-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                  SPA-GMN-L06-INPUT-AREA
                                       SPA-NAI-L06-INPUT-AREA
      *
           MOVE    ZERO            TO  SPA-GMN-L06-CUR
      *
           PERFORM 310-SPASET-SEC
      *
           .
       230-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    取消　処理
      *****************************************************************
       230-TORIKESI-SEC                SECTION.
      *
           IF    ( SPA-GMN-L06-PREHOURSKBN    =   "0" OR "1" )
               MOVE    "3001"       TO  SPA-LIDCD
           ELSE
               MOVE    "3002"       TO  SPA-LIDCD
           END-IF
      *
           .
       230-TORIKESI-EXT.
           EXIT.
      *****************************************************************
      *    取消　処理
      *****************************************************************
       2301-TORIKESI-SEC                SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           MOVE    "%"                 TO  HOURS-HOURSKBN
           MOVE    1                   TO  HOURS-LIMIT
           PERFORM 900-HOURS-KEY5-SEL-SEC
           IF    ( FLG-HOURS           =   ZERO )
               MOVE    HOURS-REC           TO  MCPDATA-REC
               MOVE    "tbl_hours"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC              =   ZERO )
                   PERFORM 310-SPASET-SEC
               ELSE
                   MOVE    "4004"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2301-TORIKESI-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       212-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 212-TOUROKU-SIME-CHK-SEC
           END-IF
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録時締め処理チェック処理
      *****************************************************************
       212-TOUROKU-SIME-CHK-SEC         SECTION.
      *
           IF    ( SPA-NAI-L06-STSRYYMD =   SPACE )
               IF    ( SPA-GMN-L06-HOURSKBN    NOT =   "1" )
                   MOVE    "1001"      TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN-L06-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-HOURS-KEY2-SEL-SEC
               IF    ( FLG-HOURS       =   ZERO )
                   IF    ( SPA-NAI-L06-EDSRYYMD    <   HOURS-SRYYMD )
                       MOVE    "1002"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-L06-CUR
                   END-IF
      *
                   IF    ( SPA-ERRCD   =   SPACE )
                       IF    ( SPA-NAI-L06-EDSRYYMD =  HOURS-SRYYMD )
                           IF    ( SPA-GMN-L06-EDTIME <=   HOURS-TIME )
                               MOVE    "1003"  TO  SPA-ERRCD
                               MOVE    8       TO  SPA-GMN-L06-CUR
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-HOURS-KEY-SEL-SEC
               IF    ( FLG-HOURS       =   ZERO )
                   MOVE    "1008"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-L06-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2121-HOURS-INS-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-L06-STSRYYMD    NOT =   SPACE )
                   PERFORM 2121-SYS3005-UPD-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-L06-STSRYYMD    NOT =   SPACE )
                   MOVE    "CHANGE"    TO  MCP-PUTTYPE
                   PERFORM 210-BACK
               ELSE
                   PERFORM 310-SPASET-SEC
               END-IF
           END-IF
      *
           .
       212-TOUROKU-SIME-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    締め時刻管理追加処理
      *****************************************************************
       2121-HOURS-INS-SEC              SECTION.
      *
           INITIALIZE                  HOURS-REC
           MOVE    SPA-HOSPNUM             TO  HOURS-HOSPNUM
           IF    ( SPA-NAI-L06-STSRYYMD    =   SPACE )
               MOVE    SPA-NAI-L06-EDSRYYMD    TO  WRK-FROMYMD
               MOVE    SPA-GMN-L06-EDTIME      TO  WRK-FROMTIME
               PERFORM 31012-FROMDATE-EDIT-SEC
               MOVE    WRK-FROMYMD             TO  HOURS-SRYYMD
               MOVE    WRK-FROMTIME            TO  HOURS-TIME
               MOVE    WRK-FROMYMD             TO  HOURS-ID (1:8)
               MOVE    WRK-FROMTIME            TO  HOURS-ID (9:)
           ELSE
               MOVE    SPA-NAI-L06-EDSRYYMD    TO  HOURS-SRYYMD
               MOVE    SPA-GMN-L06-EDTIME      TO  HOURS-TIME
               MOVE    SPA-NAI-L06-EDSRYYMD    TO  HOURS-ID (1:8)
               MOVE    SPA-GMN-L06-EDTIME      TO  HOURS-ID (9:)
           END-IF
      *
           MOVE    SPA-GMN-L06-HOURSKBN        TO  HOURS-HOURSKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  HOURS-CREYMD
                                           HOURS-UPYMD
           MOVE    SMCNDATE-HMS        TO  HOURS-UPHMS
           MOVE    HOURS-REC           TO  MCPDATA-REC
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           .
       2121-HOURS-INS-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理追加処理
      *****************************************************************
       2121-SYS3005-UPD-SEC            SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                      SYS-3005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3005-HOSPNUM
           MOVE    "3005"              TO  SYS-3005-KANRICD
           MOVE    "*"                 TO  SYS-3005-KBNCD
           MOVE    "00000000"          TO  SYS-3005-STYUKYMD
           MOVE    "99999999"          TO  SYS-3005-EDYUKYMD
           MOVE    SPA-GMN-L06-HOURSKBN    TO  SYS-3005-PROCKBN
           MOVE    SPA-NAI-L06-STSRYYMD    TO  SYS-3005-STSRYYMD
           MOVE    SPA-GMN-L06-STTIME      TO  SYS-3005-STTIME
           MOVE    SPA-NAI-L06-EDSRYYMD    TO  SYS-3005-EDSRYYMD
           MOVE    SPA-GMN-L06-EDTIME      TO  SYS-3005-EDTIME
      *
           MOVE    SYS-3005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYS-3005-REC    TO  SYSKANRI-REC
               MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4002"      TO  SPA-ERRCD
                   GO  TO  2121-SYS3005-UPD-EXT
               END-IF
           ELSE
               MOVE    SYS-3005-REC    TO  SYSKANRI-REC
               MOVE    SMCNDATE-YMD    TO  SYS-CREYMD
                                           SYS-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4003"      TO  SPA-ERRCD
                   GO  TO  2121-SYS3005-UPD-EXT
               END-IF
           END-IF
      *
           .
       2121-SYS3005-UPD-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-LIDCD       NOT =   SPACE )
               PERFORM 520-LIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "L06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE                   TO  L06
           INITIALIZE                          L06
      *
           IF    ( SPA-NAI-L06-STSRYYMD    NOT =   SPACE )
               MOVE    "black"             TO  L06-LBLSTDATE-STYLE
               STRING  "　　　　期間　"        DELIMITED   BY  SIZE
                       SPA-GMN-L06-STSRYYMD    DELIMITED   BY  SIZE
                       "　"                    DELIMITED   BY  SIZE
                       SPA-GMN-L06-STHR        DELIMITED   BY  SIZE
                       ":"                     DELIMITED   BY  SIZE
                       SPA-GMN-L06-STMIN       DELIMITED   BY  SIZE
                       "　　〜"                DELIMITED   BY  SIZE
               INTO    L06-LBLSTDATE
               END-STRING
           ELSE
               MOVE    "blue"              TO  L06-LBLSTDATE-STYLE
               MOVE    "最初に期間の開始を登録してください"
                                           TO  L06-LBLSTDATE
           END-IF
      *
           EVALUATE    SPA-GMN-L06-PREHOURSKBN
           WHEN    "0"
               STRING  "（前回の本締め　"        DELIMITED   BY  SIZE
                       SPA-GMN-L06-PRESTSRYYMD DELIMITED   BY  SIZE
                       " "                     DELIMITED   BY  SIZE
                       SPA-GMN-L06-PRESTHR     DELIMITED   BY  SIZE
                       ":"                     DELIMITED   BY  SIZE
                       SPA-GMN-L06-PRESTMIN    DELIMITED   BY  SIZE
                       "）"                    DELIMITED   BY  SIZE
               INTO    L06-PREDATE
               END-STRING
           WHEN    "1"
               STRING  "（前回の本締め　"        DELIMITED   BY  SIZE
                       SPA-GMN-L06-PRESTSRYYMD DELIMITED   BY  SIZE
                       " "                    DELIMITED   BY  SIZE
                       SPA-GMN-L06-PRESTHR     DELIMITED   BY  SIZE
                       ":"                     DELIMITED   BY  SIZE
                       SPA-GMN-L06-PRESTMIN    DELIMITED   BY  SIZE
                       " 〜 "                  DELIMITED   BY  SIZE
                       SPA-GMN-L06-PREEDSRYYMD DELIMITED   BY  SIZE
                       " "                     DELIMITED   BY  SIZE
                       SPA-GMN-L06-PREEDHR     DELIMITED   BY  SIZE
                       ":"                     DELIMITED   BY  SIZE
                       SPA-GMN-L06-PREEDMIN    DELIMITED   BY  SIZE
                       "）"                    DELIMITED   BY  SIZE
               INTO    L06-PREDATE
               END-STRING
           WHEN    "2"
               STRING  "（前回の仮締め　"        DELIMITED   BY  SIZE
                       SPA-GMN-L06-PREEDSRYYMD DELIMITED   BY  SIZE
                       " "                     DELIMITED   BY  SIZE
                       SPA-GMN-L06-PREEDHR     DELIMITED   BY  SIZE
                       ":"                     DELIMITED   BY  SIZE
                       SPA-GMN-L06-PREEDMIN    DELIMITED   BY  SIZE
                       "）"                    DELIMITED   BY  SIZE
               INTO    L06-PREDATE
               END-STRING
           END-EVALUATE
      *
           MOVE    SPA-GMN-L06-EDSRYYMD    TO  L06-EDSRYYMD
           MOVE    SPA-GMN-L06-EDHR        TO  L06-EDHR
           MOVE    SPA-GMN-L06-EDMIN       TO  L06-EDMIN
      *
           MOVE    SPA-GMN-L06-HOURSKBN-G  TO  L06-HOURSKBN
      *
           MOVE    SPA-GMN-L06-HOURSKBN-MAX TO  L06-HOURSKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-L06-HOURSKBN-MAX )
      *
               MOVE    SPA-GMN-L06-HOURSKBNLST (IDX1)
                                   TO  L06-HOURSKBN-ITEM (IDX1)
      *
           END-PERFORM
      *
           MOVE    WIDGET-INSENSITIVE      TO  L06-B03-STATE
           IF    ( SPA-GMN-L06-PREHOURSKBN NOT =   SPACE )
               MOVE    WIDGET-NORMAL       TO  L06-B03-STATE
           END-IF
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-L06-CUR     =   ZERO )
      *
               PERFORM 5011-INPUT-CUR-SEC
      *
           END-IF
      *
           EVALUATE    SPA-GMN-L06-CUR
           WHEN    07
               MOVE    "EDSRYYMD"      TO  MCP-WIDGET
           WHEN    08
               MOVE    "EDHR"          TO  MCP-WIDGET
           WHEN    09
               MOVE    "EDMIN"         TO  MCP-WIDGET
           WHEN    10
               MOVE    "HOURSKBN"      TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "EDSRYYMD"
               MOVE    08              TO  SPA-GMN-L06-CUR
           WHEN    "EDHR"
               MOVE    09              TO  SPA-GMN-L06-CUR
           WHEN    "EDMIN"
               MOVE    10              TO  SPA-GMN-L06-CUR
           WHEN    "HOURSKBN"
               MOVE    212             TO  SPA-GMN-L06-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "診療日入力エラー"          TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "時刻入力エラー"            TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "締め区分入力エラー"        TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "締め区分は本締めで登録してください"
                                                   TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "最新の締め日より過去の日付が入力されています"
                                                   TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "最新の締め時刻より先の時刻で入力してください"
                                                   TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "締め区分を入力してください"
                                                   TO  SPA-ERRMSG
           WHEN    "1005"
               STRING  "本日より遡って１０日以内の日付で入力してく"
                                           DELIMITED   BY  SIZE
                       "ください"          DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1006"
               MOVE    "先日付での登録は行なえません"
                                                   TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "先時刻での登録は行なえません"
                                                   TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "日付、時刻の等しいデータが登録済みです"
                                                   TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "番号を入力してください"    TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    "期間の指定に誤りがあります"
                                                   TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    "期間の指定に誤りがあります"
                                                   TO  SPA-ERRMSG
           WHEN    "1012"
               MOVE    "期間は１０日間以内で指定してください"
                                                   TO  SPA-ERRMSG
           WHEN    "1013"
               MOVE    "日付を入力してください"    TO  SPA-ERRMSG
           WHEN    "4001"          THRU    "4004"
               MOVE    "更新処理に失敗しました"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  LERR
           MOVE    SPA-ERRCD           TO  LERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  LERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "L06"               TO  SPA-MOTOPG
           MOVE    "LERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "LERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-LIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-LIDMSG
      *
           EVALUATE    SPA-LIDCD
           WHEN    "3001"
               MOVE    "前回の本締めを削除します"
                                        TO  WRK-LIDMSG
           WHEN    "3002"
               MOVE    "前回の仮締めを削除します"
                                        TO  WRK-LIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  LID1
           INITIALIZE                       LID1
           MOVE    SPA-LIDCD            TO  LID1-ID1CODE
           MOVE    WRK-LIDMSG           TO  LID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "L06"                TO  SPA-MOTOPG
           MOVE    "LID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "LID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-LIDSET-EXT.
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
      *    和暦西暦変換編集処理
      *****************************************************************
       800-HIZUKE-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
      *
           IF    ( WRK-SYMD        NOT =   SPACE )
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
      *
           END-IF
      *
           .
       800-HIZUKE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    曜日編集処理
      *****************************************************************
       800-YOBI-HEN-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-YOBI
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY3-AREA
           MOVE    "31"            TO  LNK-DAY3-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY3-AREA
           IF    ( STS-DAY-RC1         =   ZERO )
               IF    ( LNK-DAY3-YOBI   >=  7 )
                   MOVE    1       TO  WRK-YOBI
               ELSE
                   COMPUTE WRK-YOBI
                       =   LNK-DAY3-YOBI   +   1
               END-IF
           END-IF
      *
           .
       800-YOBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    年月日チェック処理
      *****************************************************************
       800-HIZUKE-CHK-SEC              SECTION.
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
           CALL    "ORCSGDAY"              USING
                                           ORCSGDAYAREA
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
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
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理検索処理
      *****************************************************************
       900-HOURS-SEL-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-HOURS
      *
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           MOVE    HOURS-REC           TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  HOURS-REC
           ELSE
               MOVE    1               TO  FLG-HOURS
               INITIALIZE                  HOURS-REC
           END-IF
      *
           .
       900-HOURS-SEL-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理検索処理
      *****************************************************************
       900-HOURS-FET-SEC               SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  HOURS-REC
           ELSE
               MOVE    1               TO  FLG-HOURS
               INITIALIZE                  HOURS-REC
           END-IF
      *
           .
       900-HOURS-FET-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理検索処理(KEY)
      *****************************************************************
       900-HOURS-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-HOURS
      *
           INITIALIZE                  HOURS-REC
           MOVE    SPA-HOSPNUM             TO  HOURS-HOSPNUM
           MOVE    SPA-NAI-L06-EDSRYYMD    TO  HOURS-SRYYMD
           MOVE    SPA-GMN-L06-EDTIME      TO  HOURS-TIME
           MOVE    HOURS-REC           TO  MCPDATA-REC
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  HOURS-REC
           ELSE
               MOVE    1               TO  FLG-HOURS
               INITIALIZE                  HOURS-REC
           END-IF
      *
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-HOURS-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理検索処理(KEY2)
      *****************************************************************
       900-HOURS-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HOURS
      *
           INITIALIZE                  HOURS-REC
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           MOVE    SPA-NAI-L06-EDSRYYMD    TO  HOURS-SRYYMD
           MOVE    SPA-GMN-L06-EDTIME      TO  HOURS-TIME
      *
           MOVE    HOURS-REC           TO  MCPDATA-REC
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  HOURS-REC
           ELSE
               MOVE    1               TO  FLG-HOURS
               INITIALIZE                  HOURS-REC
           END-IF
      *
           .
       900-HOURS-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理FETCH処理(KEY2)
      *****************************************************************
       900-HOURS-KEY2-FET-SEC          SECTION.
      *
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  HOURS-REC
           ELSE
               MOVE    1               TO  FLG-HOURS
               INITIALIZE                  HOURS-REC
           END-IF
      *
           .
       900-HOURS-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理検索処理(KEY3)
      *****************************************************************
       900-HOURS-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HOURS
      *
           MOVE    "1"                 TO  HOURS-HOURSKBN
           MOVE    HOURS-REC           TO  MCPDATA-REC
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  HOURS-REC
           ELSE
               MOVE    1               TO  FLG-HOURS
               INITIALIZE                  HOURS-REC
           END-IF
      *
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-HOURS-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理検索処理(KEY5)
      *****************************************************************
       900-HOURS-KEY5-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HOURS
      *
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           MOVE    HOURS-REC           TO  MCPDATA-REC
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  HOURS-REC
           ELSE
               MOVE    1               TO  FLG-HOURS
               INITIALIZE                  HOURS-REC
           END-IF
      *
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-HOURS-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
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
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
