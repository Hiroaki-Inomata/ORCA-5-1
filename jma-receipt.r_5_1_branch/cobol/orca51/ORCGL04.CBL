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
       PROGRAM-ID.             ORCGL04.
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
           03  FLG-EDIT                    PIC 9(01).
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
           03  IDXL                        PIC 9(05).
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
           03  WRK-STTIME.
               05  WRK-STHR                PIC 9(02).
               05  WRK-STMIN               PIC 9(02).
           03  WRK-EDTIME.
               05  WRK-EDHR                PIC 9(02).
               05  WRK-EDMIN               PIC 9(02).
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
           03  WRK-NUM                     PIC 9(10).
           03  WRK-NUMX                    PIC X(10).
      *
       01  CONST-AREA.
           03  CONST-TERM-MAX              PIC 9(03)   VALUE   10.
           03  CONST-GOBACK-DAYS           PIC S9(03)  VALUE   -10.
           03  CONST-GOBACK-MONTH          PIC S9(03)  VALUE   -1.
           03  CONST-PROCKBN-G.
               05  CONST-VIEW              PIC X(01)   VALUE   "0".
               05  CONST-SIME              PIC X(01)   VALUE   "1".
               05  CONST-SIME-KARI         PIC X(01)   VALUE   "2".
               05  CONST-SELTERM           PIC X(01)   VALUE   "3".
               05  CONST-INS               PIC X(01)   VALUE   "4".
               05  CONST-DEL               PIC X(01)   VALUE   "5".
               05  CONST-UPD               PIC X(01)   VALUE   "6".
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
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
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
           MOVE    "L04"               TO  MCP-WINDOW
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
           PERFORM 310-SPASET-SEC
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
           INITIALIZE                  SPA-L04-AREA
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
      *    メッセージ編集処理
           PERFORM 3101-MES-HEN-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           MOVE    SPA-GMN-L04-PROCKBNLST (1)
                                       TO  SPA-GMN-L04-PROCKBN-G
           PERFORM 41001-PROCKBN-CHK-SEC
      *
           MOVE    1               TO  SPA-GMN-L04-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    メッセージ編集処理
      *****************************************************************
       3101-MES-HEN-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-L04-MES-AREA
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3005"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    "00000000"          TO  SYS-STYUKYMD
           MOVE    "99999999"          TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3005-REC
      *
               EVALUATE    SYS-3005-PROCKBN
               WHEN    CONST-SIME
                   MOVE    "本締め"    TO  SPA-GMN-L04-MESPROC
               WHEN    CONST-SIME-KARI
                   MOVE    "仮締め"    TO  SPA-GMN-L04-MESPROC
               WHEN    CONST-SELTERM
                   MOVE    "期間指定"    TO  SPA-GMN-L04-MESPROC
               END-EVALUATE
               MOVE    SYS-3005-PROCKBN  TO  SPA-NAI-L04-MESPROCKBN
      *
               MOVE    SYS-3005-STSRYYMD   TO  WRK-SYMD
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-L04-MESSTSRYYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-L04-MESSTSRYYMD
      *
               MOVE    SYS-3005-STTIME TO  SPA-GMN-L04-MESSTTIME
      *
               MOVE    SYS-3005-EDSRYYMD   TO  WRK-SYMD
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-L04-MESEDSRYYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-L04-MESEDSRYYMD
      *
               MOVE    SYS-3005-EDTIME TO  SPA-GMN-L04-MESEDTIME
           END-IF
      *
           .
       3101-MES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
           PERFORM 31011-PROCKBNLST-SET-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ処理
      *****************************************************************
       31011-PROCKBNLST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-L04-PROCKBNLST-G
      *
           MOVE    7               TO  SPA-GMN-L04-PROCKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-L04-PROCKBNL   (1)
           MOVE    "履歴表示"      TO  SPA-GMN-L04-PROCKBNMEIL(1)
      *
           MOVE    "1"             TO  SPA-GMN-L04-PROCKBNL   (2)
           MOVE    "本締め"        TO  SPA-GMN-L04-PROCKBNMEIL(2)
      *
           MOVE    "2"             TO  SPA-GMN-L04-PROCKBNL   (3)
           MOVE    "仮締め"        TO  SPA-GMN-L04-PROCKBNMEIL(3)
      *
           MOVE    "3"             TO  SPA-GMN-L04-PROCKBNL   (4)
           MOVE    "期間指定"      TO  SPA-GMN-L04-PROCKBNMEIL(4)
      *
           MOVE    "4"             TO  SPA-GMN-L04-PROCKBNL   (5)
           MOVE    "追加"          TO  SPA-GMN-L04-PROCKBNMEIL(5)
      *
           MOVE    "5"             TO  SPA-GMN-L04-PROCKBNL   (6)
           MOVE    "削除"          TO  SPA-GMN-L04-PROCKBNMEIL(6)
      *
           MOVE    "6"             TO  SPA-GMN-L04-PROCKBNL   (7)
           MOVE    "締め変更"      TO  SPA-GMN-L04-PROCKBNMEIL(7)
      *
           .
       31011-PROCKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    締め区分コンボ処理
      *****************************************************************
       31011-HOURSKBNLST-SET-SEC   SECTION.
      *
           INITIALIZE                  SPA-GMN-L04-HOURSKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-L04-HOURSKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-L04-HOURSKBNL   (1)
           MOVE    "本締め"        TO  SPA-GMN-L04-HOURSKBNMEIL(1)
      *
           MOVE    "2"             TO  SPA-GMN-L04-HOURSKBNL   (2)
           MOVE    "仮締め"        TO  SPA-GMN-L04-HOURSKBNMEIL(2)
      *
           .
       31011-HOURSKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    表示項目編集処理
      *****************************************************************
       3101-VIEW-AREA-SET-SEC          SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                  SPA-GMN-L04-VIEW-AREA
                                       SPA-NAI-L04-VIEW-AREA
                                       WRK-FROMYMD
                                       WRK-FROMTIME
      *
           MOVE    ZERO                TO  IDX-TERM
      *
           IF    ( SPA-GMN-L04-VIEW-MODE   =   3 )
               IF ( SPA-NAI-L04-MESSTSRYYMD    NOT =   SPACE )
                   COMPUTE IDX-TERM    =   IDX-TERM    +   1
                   MOVE    SPA-NAI-L04-MESSTSRYYMD
                                   TO  SPA-NAI-L04-SRYYMD (IDX-TERM 1)
                   MOVE    SPA-GMN-L04-MESSTSRYYMD
                                   TO  SPA-GMN-L04-SRYYMD (IDX-TERM 1)
                   MOVE    SPA-GMN-L04-MESSTTIME
                                   TO  SPA-GMN-L04-TIME   (IDX-TERM 1)
                   MOVE    SPA-NAI-L04-MESEDSRYYMD
                                   TO  SPA-NAI-L04-SRYYMD (IDX-TERM 2)
                   MOVE    SPA-GMN-L04-MESEDSRYYMD
                                   TO  SPA-GMN-L04-SRYYMD (IDX-TERM 2)
                   MOVE    SPA-GMN-L04-MESEDTIME
                                   TO  SPA-GMN-L04-TIME   (IDX-TERM 2)
                   MOVE    "3"     TO  SPA-NAI-L04-HOURSKBN(IDX-TERM)
                   MOVE    SPA-GMN-L04-MESEDSRYYMD
                                   TO  SPA-NAI-L04-ID (IDX-TERM)
                   MOVE    SPA-GMN-L04-MESEDTIME
                                   TO  SPA-NAI-L04-ID (IDX-TERM)(9:)
               END-IF
           END-IF
      *
           INITIALIZE                  HOURS-REC
      *
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           IF    ( SPA-GMN-L04-VIEW-MODE   =   1 OR 2 )
               MOVE    "1"             TO  HOURS-HOURSKBN
           ELSE
               MOVE    "%"             TO  HOURS-HOURSKBN
           END-IF
           MOVE    20                  TO  HOURS-LIMIT
           MOVE    "tbl_hours"         TO  WRK-MCP-TABLE
           MOVE    "key5"              TO  WRK-MCP-PATHNAME
           PERFORM 900-HOURS-SEL-SEC
      *
           PERFORM UNTIL ( IDX-TERM        >=  CONST-TERM-MAX )
                    OR   ( FLG-HOURS   NOT =   ZERO )
      *
               COMPUTE IDX-TERM    =   IDX-TERM    +   1
      *
               MOVE    HOURS-SRYYMD
                                   TO  WRK-SYMD
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-SYMD    TO  SPA-NAI-L04-SRYYMD (IDX-TERM 2)
               MOVE    WRK-HENYMDG
                                   TO  SPA-GMN-L04-SRYYMD (IDX-TERM 2)
               MOVE    HOURS-TIME
                                   TO  SPA-GMN-L04-TIME   (IDX-TERM 2)
               MOVE    HOURS-HOURSKBN
                                   TO  SPA-NAI-L04-HOURSKBN(IDX-TERM)
               MOVE    HOURS-ID    TO  SPA-NAI-L04-ID      (IDX-TERM)
      *
               PERFORM 900-HOURS-FET-SEC
      *
               IF    ( SPA-GMN-L04-VIEW-MODE   =   2 )
                   IF    ( IDX-TERM            >=  1 )
                       MOVE    1   TO  FLG-HOURS
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX-TERM         TO  SPA-GMN-L04-TERM-MAX
      *
           MOVE    WRK-MCP-TABLE    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           INITIALIZE                  HOURS-REC
      *
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
           MOVE    "2"                 TO  WRK-ZOGENPTN
           MOVE    CONST-GOBACK-MONTH  TO  WRK-ZOGEN
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD            TO  HOURS-SRYYMD
           IF    ( SPA-GMN-L04-VIEW-MODE   =   3 )
               MOVE    "%"             TO  HOURS-HOURSKBN
           ELSE
               MOVE    "1"             TO  HOURS-HOURSKBN
           END-IF
           MOVE    20                  TO  HOURS-LIMIT
      *
           MOVE    "tbl_hours"         TO  WRK-MCP-TABLE
           MOVE    "key5"              TO  WRK-MCP-PATHNAME
           PERFORM 900-HOURS-SEL-SEC
      *
           MOVE    1                   TO  IDX-TERM
      *
           IF    ( SPA-GMN-L04-VIEW-MODE   =   3 )
            AND  ( SPA-NAI-L04-MESSTSRYYMD NOT =   SPACE )
                   MOVE    2           TO  IDX-TERM
           END-IF
      *
           PERFORM UNTIL ( IDX-TERM        >   SPA-GMN-L04-TERM-MAX )
                    OR   ( FLG-HOURS   NOT =   ZERO )
      *
      *
               IF    ( HOURS-ID   <    SPA-NAI-L04-ID (IDX-TERM))
      *
                   PERFORM 3101-VIEW-FROM-SET-SEC
                   COMPUTE IDX-TERM    =   IDX-TERM    +   1
               ELSE
                   PERFORM 900-HOURS-FET-SEC
               END-IF
      *
      *
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       3101-VIEW-AREA-SET-EXT.
           EXIT.
      *****************************************************************
      *    表示項目診療開始日編集処理
      *****************************************************************
       3101-VIEW-FROM-SET-SEC          SECTION.
      *
           MOVE    HOURS-SRYYMD    TO  WRK-FROMYMD
           MOVE    HOURS-TIME      TO  WRK-FROMTIME
      *
           PERFORM 31011-FROMDATE-EDIT-SEC
           MOVE    WRK-FROMYMD     TO  WRK-SYMD
           PERFORM 800-HIZUKE-HEN-SEC
           MOVE    WRK-SYMD        TO  SPA-NAI-L04-SRYYMD (IDX-TERM 1)
           MOVE    WRK-HENYMDG     TO  SPA-GMN-L04-SRYYMD (IDX-TERM 1)
           MOVE    WRK-FROMTIME    TO  SPA-GMN-L04-TIME   (IDX-TERM 1)
      *
           .
       3101-VIEW-FROM-SET-EXT.
           EXIT.
      *****************************************************************
      *    開始診療日・時刻編集処理
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
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "CHK_MESSTSRYYMD"
           WHEN    "CLICKED"       ALSO    "CHK_MESSTTIME"
               PERFORM 4101-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_MESEDSRYYMD"
           WHEN    "CLICKED"       ALSO    "CHK_MESEDTIME"
               PERFORM 4101-CHK-TO-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD011"
           WHEN    "CLICKED"       ALSO    "CHK_TIME011"
               MOVE    1               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD012"
           WHEN    "CLICKED"       ALSO    "CHK_TIME012"
               MOVE    1               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD021"
           WHEN    "CLICKED"       ALSO    "CHK_TIME021"
               MOVE    2               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD022"
           WHEN    "CLICKED"       ALSO    "CHK_TIME022"
               MOVE    2               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD031"
           WHEN    "CLICKED"       ALSO    "CHK_TIME031"
               MOVE    3               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD032"
           WHEN    "CLICKED"       ALSO    "CHK_TIME032"
               MOVE    3               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD041"
           WHEN    "CLICKED"       ALSO    "CHK_TIME041"
               MOVE    4               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD042"
           WHEN    "CLICKED"       ALSO    "CHK_TIME042"
               MOVE    4               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD051"
           WHEN    "CLICKED"       ALSO    "CHK_TIME051"
               MOVE    5               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD052"
           WHEN    "CLICKED"       ALSO    "CHK_TIME052"
               MOVE    5               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD061"
           WHEN    "CLICKED"       ALSO    "CHK_TIME061"
               MOVE    6               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD062"
           WHEN    "CLICKED"       ALSO    "CHK_TIME062"
               MOVE    6               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD071"
           WHEN    "CLICKED"       ALSO    "CHK_TIME071"
               MOVE    7               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD072"
           WHEN    "CLICKED"       ALSO    "CHK_TIME072"
               MOVE    7               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD081"
           WHEN    "CLICKED"       ALSO    "CHK_TIME081"
               MOVE    8               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD082"
           WHEN    "CLICKED"       ALSO    "CHK_TIME082"
               MOVE    8               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD091"
           WHEN    "CLICKED"       ALSO    "CHK_TIME091"
               MOVE    9               TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD092"
           WHEN    "CLICKED"       ALSO    "CHK_TIME092"
               MOVE    9               TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD101"
           WHEN    "CLICKED"       ALSO    "CHK_TIME101"
               MOVE    10              TO  IDX-CHK
               PERFORM 4100-CHK-FROM-CHK-SEC
           WHEN    "CLICKED"       ALSO    "CHK_SRYYMD102"
           WHEN    "CLICKED"       ALSO    "CHK_TIME102"
               MOVE    10              TO  IDX-CHK
               PERFORM 4100-CHK-TO-CHK-SEC
      *
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 230-CLEAR-SEC
      *    履歴表示変更
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 240-VIEW-MODE-CHK-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               IF    ( SPA-GMN-L04-PROCKBN NOT =   CONST-VIEW )
                   PERFORM 212-TOUROKU-SEC
               END-IF
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
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
           WHEN    "ACTIVATE"      ALSO    "PROCKBN"
               PERFORM 4100-PROCKBN-CHK-SEC
      *
           WHEN    "ACTIVATE"      ALSO    "STNO"
               PERFORM 4100-STNO-CHK-SEC
      *
           WHEN    "ACTIVATE"      ALSO    "EDNO"
               PERFORM 4100-EDNO-CHK-SEC
      *
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
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
           MOVE    ZERO            TO  SPA-GMN-L04-CUR
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
           MOVE    L04-STHR        TO  WRK-NUMX
           PERFORM 800-NUM-SEC
           MOVE    WRK-NUM         TO  WRK-STHR
      *
           MOVE    L04-STMIN       TO  WRK-NUMX
           PERFORM 800-NUM-SEC
           MOVE    WRK-NUM         TO  WRK-STMIN
      *
           MOVE    L04-EDHR        TO  WRK-NUMX
           PERFORM 800-NUM-SEC
           MOVE    WRK-NUM         TO  WRK-EDHR
      *
           MOVE    L04-EDMIN       TO  WRK-NUMX
           PERFORM 800-NUM-SEC
           MOVE    WRK-NUM         TO  WRK-EDMIN
      *
           EVALUATE    SPA-GMN-L04-PROCKBN
           WHEN    CONST-SIME
           WHEN    CONST-SIME-KARI
               MOVE    L04-EDSRYYMD    TO  SPA-GMN-L04-EDSRYYMD
               MOVE    L04-EDHR        TO  SPA-GMN-L04-EDHR
               MOVE    L04-EDMIN       TO  SPA-GMN-L04-EDMIN
           WHEN    CONST-SELTERM
               MOVE    L04-STSRYYMD    TO  SPA-GMN-L04-STSRYYMD
               MOVE    WRK-STTIME      TO  SPA-GMN-L04-STTIME
               MOVE    L04-EDSRYYMD    TO  SPA-GMN-L04-EDSRYYMD
               MOVE    WRK-EDTIME      TO  SPA-GMN-L04-EDTIME
           WHEN    CONST-INS
               MOVE    L04-EDSRYYMD    TO  SPA-GMN-L04-EDSRYYMD
               MOVE    WRK-EDTIME      TO  SPA-GMN-L04-EDTIME
               MOVE    L04-HOURSKBN    TO  SPA-GMN-L04-HOURSKBN-G
           WHEN    CONST-UPD
               MOVE    L04-HOURSKBN    TO  SPA-GMN-L04-HOURSKBN-G
           WHEN    CONST-VIEW
           WHEN    CONST-DEL
               CONTINUE
           END-EVALUATE
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
           EVALUATE    SPA-GMN-L04-PROCKBN
           WHEN    CONST-VIEW
               PERFORM 4102-VIEW-KIHON-CHK-SEC
           WHEN    CONST-SIME
           WHEN    CONST-SIME-KARI
               PERFORM 4102-SIME-KIHON-CHK-SEC
           WHEN    CONST-SELTERM
               PERFORM 4102-SELTERM-KIHON-CHK-SEC
           WHEN    CONST-INS
               PERFORM 4102-INS-KIHON-CHK-SEC
           WHEN    CONST-UPD
               PERFORM 4102-UPD-KIHON-CHK-SEC
           WHEN    CONST-DEL
               PERFORM 4102-DEL-KIHON-CHK-SEC
           END-EVALUATE
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴表示基本チェック処理
      *****************************************************************
       4102-VIEW-KIHON-CHK-SEC           SECTION.
      *
      *
           .
      *
       4102-VIEW-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め処理基本チェック処理
      *****************************************************************
       4102-SIME-KIHON-CHK-SEC          SECTION.
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
           .
      *
       4102-SIME-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間指定基本チェック処理
      *****************************************************************
       4102-SELTERM-KIHON-CHK-SEC      SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-STNO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-STSRYYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-STHR-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-STMIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-EDNO-CHK-SEC
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
           .
      *
       4102-SELTERM-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    追加基本チェック処理
      *****************************************************************
       4102-INS-KIHON-CHK-SEC          SECTION.
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
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HOURSKBN-CHK-SEC
           END-IF
           .
      *
       4102-INS-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    変更基本チェック処理
      *****************************************************************
       4102-UPD-KIHON-CHK-SEC          SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HOURSKBN-CHK-SEC
           END-IF
           .
      *
       4102-UPD-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    削除基本チェック処理
      *****************************************************************
       4102-DEL-KIHON-CHK-SEC          SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-EDNO-CHK-SEC
           END-IF
           .
      *
       4102-DEL-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       4100-PROCKBN-CHK-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-L04-INPUT-AREA
                                       SPA-NAI-L04-INPUT-AREA
                                       SPA-GMN-L04-TERM-STSEL
                                       SPA-GMN-L04-TERM-EDSEL
                                       SPA-GMN-L04-VIEW-MODE
      *
           MOVE    ZERO            TO  SPA-GMN-L04-CUR
      *
           MOVE    L04-PROCKBN     TO  SPA-GMN-L04-PROCKBN-G
      *
           PERFORM 4101-PROCKBN-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41001-PROCKBN-CHK-SEC
           END-IF
      *
           .
       4100-PROCKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       41001-PROCKBN-CHK-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-L04-HOURSKBNLST-G
                                       SPA-GMN-L04-HOURSKBN-G
      *
           EVALUATE    SPA-GMN-L04-PROCKBN
           WHEN    CONST-VIEW
               PERFORM 41011-VIEW-SYOKI-SEC
           WHEN    CONST-SIME
           WHEN    CONST-SIME-KARI
               PERFORM 41011-SIME-SYOKI-SEC
           WHEN    CONST-SELTERM
               PERFORM 41011-SELTERM-SYOKI-SEC
           WHEN    CONST-INS
               PERFORM 41011-INS-SYOKI-SEC
           WHEN    CONST-UPD
               PERFORM 41011-UPD-SYOKI-SEC
           WHEN    CONST-DEL
               PERFORM 41011-DEL-SYOKI-SEC
           END-EVALUATE
      *
           PERFORM 3101-VIEW-AREA-SET-SEC
      *
           .
       41001-PROCKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め自チェックチェック処理
      *****************************************************************
       4100-CHK-FROM-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-L04-PROCKBN =   CONST-SELTERM )
               IF    ( IDX-CHK             >=  1 )
                AND  ( IDX-CHK             <=  SPA-GMN-L04-TERM-MAX )
                   IF    ( SPA-NAI-L04-SRYYMD (IDX-CHK 1)
                                       NOT  =   SPACE )
                       MOVE    IDX-CHK      TO  SPA-GMN-L04-STNO
                       PERFORM 41001-STNO-CHK-SEC
                       IF    ( SPA-NAI-L04-EDSRYYMD    =   SPACE )
                        AND  ( SPA-GMN-L04-EDTIME      =   ZERO  )
                           MOVE    IDX-CHK  TO  SPA-GMN-L04-EDNO
                           PERFORM 41001-EDNO-CHK-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4100-CHK-FROM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め至チェックチェック処理
      *****************************************************************
       4100-CHK-TO-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-L04-PROCKBN =   CONST-SELTERM
                                        OR CONST-UPD 
                                        OR CONST-DEL )
      *
               IF    ( IDX-CHK         >=  1 )
                AND  ( IDX-CHK         <=  SPA-GMN-L04-TERM-MAX )
                   MOVE    IDX-CHK  TO  SPA-GMN-L04-EDNO
                   PERFORM 41001-EDNO-CHK-SEC
               END-IF
           END-IF
      *
           .
       4100-CHK-TO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号自チェック処理
      *****************************************************************
       4100-STNO-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-L04-CUR
           MOVE    ZERO            TO  SPA-GMN-L04-TERM-STSEL
      *
           IF    ( SPA-GMN-L04-PROCKBN =   CONST-SELTERM )
      *
               MOVE    L04-STNO        TO  SPA-GMN-L04-STNO
      *
               IF    ( SPA-GMN-L04-STNO    NOT =   ZERO )
                   PERFORM 4101-STNO-CHK-SEC
                   IF    ( SPA-ERRCD       =   SPACE )
      *
                       PERFORM 41001-STNO-CHK-SEC
      *
                       IF    ( SPA-NAI-L04-EDSRYYMD    =   SPACE )
                        AND  ( SPA-GMN-L04-EDTIME      =   ZERO  )
                           MOVE    SPA-GMN-L04-STNO
                                       TO  SPA-GMN-L04-EDNO
                           PERFORM 41001-EDNO-CHK-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4100-STNO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号自チェック処理
      *****************************************************************
       41001-STNO-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-L04-STNO    TO  SPA-GMN-L04-TERM-STSEL
      *
           MOVE    SPA-GMN-L04-STNO    TO  IDX-TERM
           MOVE    SPA-GMN-L04-SRYYMD (IDX-TERM 1)
                                       TO  SPA-GMN-L04-STSRYYMD
           MOVE    SPA-NAI-L04-SRYYMD (IDX-TERM 1)
                                       TO  SPA-NAI-L04-STSRYYMD
           MOVE    SPA-GMN-L04-TIME   (IDX-TERM 1)
                                       TO  WRK-HMS-G
           MOVE    WRK-HMS-HH  TO  SPA-GMN-L04-STHR
           MOVE    WRK-HMS-MM  TO  SPA-GMN-L04-STMIN
      *
           .
       41001-STNO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号至チェック処理
      *****************************************************************
       4100-EDNO-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-L04-CUR
           MOVE    ZERO            TO  SPA-GMN-L04-TERM-EDSEL
      *
           IF    ( SPA-GMN-L04-PROCKBN =   CONST-SELTERM
                                        OR CONST-UPD 
                                        OR CONST-DEL )
               MOVE    L04-EDNO    TO  SPA-GMN-L04-EDNO
               IF    ( SPA-GMN-L04-EDNO    NOT =   ZERO )
                   PERFORM 4101-EDNO-CHK-SEC
                   IF    ( SPA-ERRCD       =   SPACE )
                       PERFORM 41001-EDNO-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       4100-EDNO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号至チェック処理
      *****************************************************************
       41001-EDNO-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-L04-EDNO    TO  SPA-GMN-L04-TERM-EDSEL
      *
           MOVE    SPA-GMN-L04-EDNO    TO  IDX-TERM
           MOVE    SPA-GMN-L04-SRYYMD (IDX-TERM 2)
                                       TO  SPA-GMN-L04-EDSRYYMD
           MOVE    SPA-NAI-L04-SRYYMD (IDX-TERM 2)
                                       TO  SPA-NAI-L04-EDSRYYMD
           MOVE    SPA-GMN-L04-TIME   (IDX-TERM 2)
                                       TO  WRK-HMS-G
           MOVE    WRK-HMS-HH  TO  SPA-GMN-L04-EDHR
           MOVE    WRK-HMS-MM  TO  SPA-GMN-L04-EDMIN
           MOVE    SPA-NAI-L04-HOURSKBN (IDX-TERM)
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOURSKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-L04-HOURSKBN-G
      *
           .
       41001-EDNO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め自チェックチェック処理
      *****************************************************************
       4101-CHK-FROM-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-L04-PROCKBN =   CONST-SELTERM )
      *
               IF    ( SPA-NAI-L04-MESSTSRYYMD
                                   NOT  =   SPACE )
      *
                   MOVE    ZERO        TO  SPA-GMN-L04-STNO
                                           SPA-GMN-L04-TERM-STSEL
      *
                   MOVE    SPA-GMN-L04-MESSTSRYYMD
                                       TO  SPA-GMN-L04-STSRYYMD
                   MOVE    SPA-NAI-L04-MESSTSRYYMD
                                       TO  SPA-NAI-L04-STSRYYMD
                   MOVE    SPA-GMN-L04-MESSTTIME
                                       TO  WRK-HMS-G
                   MOVE    WRK-HMS-HH  TO  SPA-GMN-L04-STHR
                   MOVE    WRK-HMS-MM  TO  SPA-GMN-L04-STMIN
      *
                   IF    ( SPA-NAI-L04-EDSRYYMD    =   SPACE )
                    AND  ( SPA-GMN-L04-EDTIME      =   ZERO  )
                       PERFORM 4101-CHK-TO-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-CHK-FROM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め至チェックチェック処理
      *****************************************************************
       4101-CHK-TO-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-L04-PROCKBN =   CONST-SELTERM )
      *
               IF    ( SPA-NAI-L04-MESEDSRYYMD
                                   NOT  =   SPACE )
                   MOVE    ZERO        TO  SPA-GMN-L04-EDNO
                                           SPA-GMN-L04-TERM-EDSEL
      *
                   MOVE    SPA-GMN-L04-MESEDSRYYMD
                                       TO  SPA-GMN-L04-EDSRYYMD
                   MOVE    SPA-NAI-L04-MESEDSRYYMD
                                       TO  SPA-NAI-L04-EDSRYYMD
                   MOVE    SPA-GMN-L04-MESEDTIME
                                       TO  WRK-HMS-G
                   MOVE    WRK-HMS-HH  TO  SPA-GMN-L04-EDHR
                   MOVE    WRK-HMS-MM  TO  SPA-GMN-L04-EDMIN
               END-IF
           END-IF
      *
      *
           .
       4101-CHK-TO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       4101-PROCKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-L04-PROCKBN TO  WRK-CMB-CD
           PERFORM 4200-PROCKBNLST-CHK-SEC
           IF    ( WRK-CMB-CD          =   SPACE )
               MOVE    SPA-GMN-L04-PROCKBNLST (1)
                                       TO  SPA-GMN-L04-PROCKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-L04-PROCKBN-G
           END-IF
      *
           .
       4101-PROCKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め初期処理
      *****************************************************************
       41011-VIEW-SYOKI-SEC            SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF    ( SPA-NAI-L04-MESSTSRYYMD NOT =   SPACE )
               MOVE    SPA-NAI-L04-MESSTSRYYMD
                                       TO  SPA-NAI-L04-STSRYYMD
               MOVE    SPA-GMN-L04-MESSTSRYYMD
                                       TO  SPA-GMN-L04-STSRYYMD
               MOVE    SPA-GMN-L04-MESSTTIME
                                       TO  SPA-GMN-L04-STTIME
               MOVE    SPA-NAI-L04-MESEDSRYYMD
                                       TO  SPA-NAI-L04-EDSRYYMD
               MOVE    SPA-GMN-L04-MESEDSRYYMD
                                       TO  SPA-GMN-L04-EDSRYYMD
               MOVE    SPA-GMN-L04-MESEDTIME
                                       TO  SPA-GMN-L04-EDTIME
           END-IF
      *
           MOVE    2                   TO  SPA-GMN-L04-VIEW-MODE
      *
           MOVE    1                   TO  SPA-GMN-L04-CUR
      *
           .
       41011-VIEW-SYOKI-EXT.
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
               MOVE    HOURS-SRYYMD    TO  WRK-FROMYMD
               MOVE    HOURS-TIME      TO  WRK-FROMTIME
               PERFORM 31011-FROMDATE-EDIT-SEC
      *
               MOVE    WRK-FROMYMD     TO  WRK-SYMD
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-L04-STSRYYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-L04-STSRYYMD
      *
               MOVE    WRK-FROMTIME    TO  WRK-HMS-G
               MOVE    WRK-HMS-HH      TO  SPA-GMN-L04-STHR
               MOVE    WRK-HMS-MM      TO  SPA-GMN-L04-STMIN
      *
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  SPA-NAI-L04-EDSRYYMD
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-L04-EDSRYYMD
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS-G
           MOVE    WRK-HMS-HH          TO  SPA-GMN-L04-EDHR
           MOVE    WRK-HMS-MM          TO  SPA-GMN-L04-EDMIN
      *
           MOVE    2                   TO  SPA-GMN-L04-VIEW-MODE
      *
           MOVE    7                   TO  SPA-GMN-L04-CUR
      *
           .
       41011-SIME-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    期間指定初期処理
      *****************************************************************
       41011-SELTERM-SYOKI-SEC         SECTION.
      *
           MOVE    3                   TO  SPA-GMN-L04-VIEW-MODE
      *
           MOVE    2                   TO  SPA-GMN-L04-CUR
      *
           .
       41011-SELTERM-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    追加初期処理
      *****************************************************************
       41011-INS-SYOKI-SEC             SECTION.
      *
           PERFORM 31011-HOURSKBNLST-SET-SEC
           MOVE    "1"                 TO  WRK-CMB-CD
           PERFORM 4200-HOURSKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-L04-HOURSKBN-G
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SPA-NAI-L04-EDSRYYMD
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-L04-EDSRYYMD
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS-G
           MOVE    WRK-HMS-HH          TO  SPA-GMN-L04-EDHR
           MOVE    WRK-HMS-MM          TO  SPA-GMN-L04-EDMIN
      *
           MOVE    7                   TO  SPA-GMN-L04-CUR
      *
           .
       41011-INS-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    削除初期処理
      *****************************************************************
       41011-DEL-SYOKI-SEC             SECTION.
      *
           PERFORM 31011-HOURSKBNLST-SET-SEC
      *
           MOVE    6                   TO  SPA-GMN-L04-CUR
      *
           .
       41011-DEL-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    変更初期処理
      *****************************************************************
       41011-UPD-SYOKI-SEC             SECTION.
      *
           PERFORM 31011-HOURSKBNLST-SET-SEC
      *
           MOVE    6                   TO  SPA-GMN-L04-CUR
      *
           .
       41011-UPD-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    番号自編集処理
      *****************************************************************
       4101-STNO-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-L04-STNO    NOT =   ZERO )
               IF    ( SPA-GMN-L04-STNO    >=  1 )
                AND  ( SPA-GMN-L04-STNO    <=  SPA-GMN-L04-TERM-MAX )
                   IF    ( SPA-NAI-L04-SRYYMD (SPA-GMN-L04-STNO 1)
                                        =  SPACE )
                       MOVE    "0005"      TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN-L04-CUR
                   END-IF
               ELSE
                       MOVE    "0005"      TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           .
       4101-STNO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号至編集処理
      *****************************************************************
       4101-EDNO-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-L04-EDNO    NOT =   ZERO )
               IF    ( SPA-GMN-L04-EDNO    >=  1 )
                AND  ( SPA-GMN-L04-EDNO    <=  SPA-GMN-L04-TERM-MAX )
                   CONTINUE
               ELSE
                   MOVE    "0005"          TO  SPA-ERRCD
                   MOVE    6               TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           .
       4101-EDNO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療日自チェック処理
      *****************************************************************
       4101-STSRYYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-L04-STSRYYMD
                                   TO  WRK-SRYYMD
           MOVE    SPACE           TO  SPA-NAI-L04-STSRYYMD
      *
           IF    ( SPA-GMN-L04-PROCKBN     =   CONST-SELTERM )
            AND  ( SPA-GMN-L04-STSRYYMD    =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-L04-STSRYYMD    TO  WRK-SYMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF    ( SGDAY-RC        =   ZERO )
                   MOVE    SGDAY-OTDAY TO  SPA-GMN-L04-STSRYYMD
                   MOVE    SGDAY-SDAY  TO  SPA-NAI-L04-STSRYYMD
               ELSE
                   MOVE    WRK-SRYYMD  TO  WRK-SYMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    SGDAY-OTDAY TO  SPA-GMN-L04-STSRYYMD
                   MOVE    SGDAY-SDAY  TO  SPA-NAI-L04-STSRYYMD
      *
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           .
       4101-STSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    時自編集処理
      *****************************************************************
       4101-STHR-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-L04-STHR    >   23 )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-L04-CUR
           END-IF
      *
           .
       4101-STHR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分自編集処理
      *****************************************************************
       4101-STMIN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-L04-STMIN   >   59 )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-L04-CUR
           END-IF
      *
           .
       4101-STMIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療日至チェック処理
      *****************************************************************
       4101-EDSRYYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-L04-EDSRYYMD
                                   TO  WRK-SRYYMD
           MOVE    SPACE           TO  SPA-NAI-L04-EDSRYYMD
      *
           IF    ( SPA-GMN-L04-PROCKBN     =   CONST-SELTERM )
            AND  ( SPA-GMN-L04-EDSRYYMD    =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-L04-EDSRYYMD    TO  WRK-SYMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF    ( SGDAY-RC        =   ZERO )
                   MOVE    SGDAY-OTDAY TO  SPA-GMN-L04-EDSRYYMD
                   MOVE    SGDAY-SDAY  TO  SPA-NAI-L04-EDSRYYMD
               ELSE
                   MOVE    WRK-SRYYMD  TO  WRK-SYMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    SGDAY-OTDAY TO  SPA-GMN-L04-EDSRYYMD
                   MOVE    SGDAY-SDAY  TO  SPA-NAI-L04-EDSRYYMD
      *
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-L04-CUR
               END-IF
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
           IF    ( SPA-GMN-L04-EDHR    >   23 )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-L04-CUR
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
           IF    ( SPA-GMN-L04-EDMIN   >   59 )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    9               TO  SPA-GMN-L04-CUR
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
           IF    ( SPA-GMN-L04-HOURSKBN-G  NOT =   SPACE )
               MOVE    SPA-GMN-L04-HOURSKBN    TO  WRK-CMB-CD
               PERFORM 4200-HOURSKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM             =   SPACE )
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           .
       4101-HOURSKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボチェック処理
      *****************************************************************
       4200-PROCKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-L04-PROCKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-L04-PROCKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-L04-PROCKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-PROCKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め区分コンボチェック処理
      *****************************************************************
       4200-HOURSKBNLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-L04-HOURSKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-L04-HOURSKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-L04-HOURSKBNLST (IDX-CMB)
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
           MOVE    "L04"               TO  SPA-MOTOPG
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
           INITIALIZE                  SPA-GMN-L04-INPUT-AREA
                                       SPA-NAI-L04-INPUT-AREA
                                       SPA-GMN-L04-TERM-STSEL
                                       SPA-GMN-L04-TERM-EDSEL
                                       SPA-GMN-L04-VIEW-MODE
      *
           MOVE    ZERO            TO  SPA-GMN-L04-CUR
      *
           PERFORM 41001-PROCKBN-CHK-SEC
      *
           .
       230-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴表示変更処理
      *****************************************************************
       240-VIEW-MODE-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-L04-PROCKBN =   CONST-VIEW
                                        OR CONST-SIME
                                        OR CONST-SIME-KARI    )
               EVALUATE    SPA-GMN-L04-VIEW-MODE
               WHEN    ZERO
                   MOVE    2           TO  SPA-GMN-L04-VIEW-MODE
               WHEN    1
                   MOVE    ZERO        TO  SPA-GMN-L04-VIEW-MODE
               WHEN    2
                   MOVE    1           TO  SPA-GMN-L04-VIEW-MODE
               WHEN    OTHER
                   MOVE    ZERO        TO  SPA-GMN-L04-VIEW-MODE
               END-EVALUATE
      *
               PERFORM 3101-VIEW-AREA-SET-SEC
      *
           END-IF
      *
           .
       240-VIEW-MODE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       212-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   =   SPACE )
               EVALUATE    SPA-GMN-L04-PROCKBN
               WHEN    CONST-SIME
               WHEN    CONST-SIME-KARI
                   PERFORM 212-TOUROKU-SIME-CHK-SEC
               WHEN    CONST-SELTERM
                   PERFORM 212-TOUROKU-SELTERM-CHK-SEC
               WHEN    CONST-INS
                   PERFORM 212-TOUROKU-INS-CHK-SEC
               WHEN    CONST-UPD
                   PERFORM 212-TOUROKU-UPD-CHK-SEC
               WHEN    CONST-DEL
                   PERFORM 212-TOUROKU-DEL-CHK-SEC
              END-EVALUATE
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
           IF    ( SPA-NAI-L04-STSRYYMD =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-L04-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 900-HOURS-KEY2-SEL-SEC
               IF    ( FLG-HOURS       =   ZERO )
                   IF    ( SPA-NAI-L04-EDSRYYMD    <   HOURS-SRYYMD )
                       MOVE    "1002"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-L04-CUR
                   END-IF
      *
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF    ( SPA-NAI-L04-EDSRYYMD  = HOURS-SRYYMD )
                           IF    ( SPA-GMN-L04-EDTIME <=   HOURS-TIME )
                               MOVE    "1003"  TO  SPA-ERRCD
                               MOVE    8       TO  SPA-GMN-L04-CUR
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 900-HOURS-KEY-SEL-SEC
               IF    ( FLG-HOURS       =   ZERO )
                   MOVE    "1008"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2121-HOURS-INS-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2121-SYS3005-UPD-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       212-TOUROKU-SIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録時期間指定処理チェック処理
      *****************************************************************
       212-TOUROKU-SELTERM-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-L04-STSRYYMD    =   SPACE )
               MOVE    "1013"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-L04-CUR
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-L04-EDSRYYMD
                                           =   SPACE )
                   MOVE    "1013"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-L04-STSRYYMD
                                           >   SPA-NAI-L04-EDSRYYMD )
                   MOVE    "1010"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-L04-STSRYYMD
                                           =   SPA-NAI-L04-EDSRYYMD )
                   IF    ( SPA-GMN-L04-STTIME
                                           >   SPA-GMN-L04-EDTIME )
                       MOVE    "1011"  TO  SPA-ERRCD
                       MOVE    4       TO  SPA-GMN-L04-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               PERFORM 2121-SYS3005-UPD-SEC
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       212-TOUROKU-SELTERM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録時追加処理チェック処理
      *****************************************************************
       212-TOUROKU-INS-CHK-SEC         SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF    ( SPA-GMN-L04-HOURSKBN    =   SPACE )
               MOVE    "1004"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN-L04-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-NAI-L04-EDSRYYMD
                                           >   SMCNDATE-YMD )
                   MOVE    "1006"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-NAI-L04-EDSRYYMD
                                           =   SMCNDATE-YMD)
                   IF    ( SPA-GMN-L04-EDTIME
                                           >   SMCNDATE-HMS)
                       MOVE    "1007"  TO  SPA-ERRCD
                       MOVE    8       TO  SPA-GMN-L04-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 900-HOURS-KEY-SEL-SEC
               IF    ( FLG-HOURS       =   ZERO )
                   MOVE    "1008"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2121-HOURS-INS-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 230-CLEAR-SEC
           END-IF
      *
      *
           .
       212-TOUROKU-INS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録時変更処理チェック処理
      *****************************************************************
       212-TOUROKU-UPD-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-L04-EDNO    =   ZERO )
               MOVE    "1009"      TO  SPA-ERRCD
               MOVE    06          TO  SPA-GMN-L04-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-L04-HOURSKBN    =   SPACE )
                   MOVE    "1004"  TO  SPA-ERRCD
                   MOVE    10      TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2121-HOURS-DEL-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2121-HOURS-INS-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 230-CLEAR-SEC
           END-IF
      *
           .
       212-TOUROKU-UPD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録時削除処理チェック処理
      *****************************************************************
       212-TOUROKU-DEL-CHK-SEC         SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-L04-EDNO    =   ZERO )
                   MOVE    "1009"          TO  SPA-ERRCD
                   MOVE    06              TO  SPA-GMN-L04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2121-HOURS-DEL-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 230-CLEAR-SEC
           END-IF
      *
           .
       212-TOUROKU-DEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    締め時刻管理追加処理
      *****************************************************************
       2121-HOURS-INS-SEC              SECTION.
      *
           INITIALIZE                  HOURS-REC
           MOVE    SPA-HOSPNUM             TO  HOURS-HOSPNUM
           MOVE    SPA-NAI-L04-EDSRYYMD    TO  HOURS-SRYYMD
           MOVE    SPA-GMN-L04-EDTIME      TO  HOURS-TIME
           MOVE    SPA-NAI-L04-EDSRYYMD    TO  HOURS-ID (1:8)
           MOVE    SPA-GMN-L04-EDTIME      TO  HOURS-ID (9:)
      *
           EVALUATE    SPA-GMN-L04-PROCKBN
           WHEN    CONST-SIME
               MOVE    "1"                 TO  HOURS-HOURSKBN
           WHEN    CONST-SIME-KARI
               MOVE    "2"                 TO  HOURS-HOURSKBN
           WHEN    CONST-INS
           WHEN    CONST-UPD
               MOVE    SPA-GMN-L04-HOURSKBN
                                           TO  HOURS-HOURSKBN
           END-EVALUATE
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
      *    締め時刻管理削除処理
      *****************************************************************
       2121-HOURS-DEL-SEC              SECTION.
      *
           INITIALIZE                  HOURS-REC
           MOVE    SPA-HOSPNUM             TO  HOURS-HOSPNUM
           MOVE    SPA-NAI-L04-EDSRYYMD    TO  HOURS-SRYYMD
           MOVE    SPA-GMN-L04-EDTIME      TO  HOURS-TIME
      *
           MOVE    HOURS-REC           TO  MCPDATA-REC
           MOVE    "tbl_hours"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4004"          TO  SPA-ERRCD
           END-IF
      *
           .
       2121-HOURS-DEL-EXT.
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
           MOVE    SPA-GMN-L04-PROCKBN     TO  SYS-3005-PROCKBN
           MOVE    SPA-NAI-L04-STSRYYMD    TO  SYS-3005-STSRYYMD
           MOVE    SPA-GMN-L04-STTIME      TO  SYS-3005-STTIME
           MOVE    SPA-NAI-L04-EDSRYYMD    TO  SYS-3005-EDSRYYMD
           MOVE    SPA-GMN-L04-EDTIME      TO  SYS-3005-EDTIME
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "L04    "           TO  MCP-WINDOW.
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
           MOVE    SPACE                   TO  L04
           INITIALIZE                          L04
      *
           MOVE    "black"             TO      L04-STSRYYMD-STYLE
                                               L04-STHR-STYLE
                                               L04-STMIN-STYLE
      *
           MOVE    SPA-GMN-L04-PROCKBN-G   TO  L04-PROCKBN
           MOVE    SPA-GMN-L04-PROCKBN-MAX TO  L04-PROCKBN-COUNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-L04-PROCKBN-MAX )
      *
               MOVE    SPA-GMN-L04-PROCKBNLST (IDX1)
                                   TO  L04-PROCKBN-ITEM (IDX1)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-TERM    FROM    1   BY  1
                   UNTIL ( IDX-TERM    >   CONST-TERM-MAX )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   2 )
                   MOVE    "whitebutton"   TO  L04-CHK-SRYYMD-STYLE
                                                         (IDX-TERM IDX1)
                   MOVE    "whitebutton"   TO  L04-CHK-TIME-STYLE
                                                         (IDX-TERM IDX1)
               END-PERFORM
           END-PERFORM
      *
           EVALUATE SPA-GMN-L04-PROCKBN
           WHEN    CONST-VIEW
               PERFORM 5001-GMNHEN-VIEW-SEC
           WHEN    CONST-SIME
           WHEN    CONST-SIME-KARI
               PERFORM 5001-GMNHEN-SIME-SEC
           WHEN    CONST-SELTERM
               PERFORM 5001-GMNHEN-SELTERM-SEC
           WHEN    CONST-INS
           WHEN    CONST-UPD
           WHEN    CONST-DEL
               PERFORM 5001-GMNHEN-UPD-SEC
           END-EVALUATE
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理（履歴表示）
      *****************************************************************
       5001-GMNHEN-VIEW-SEC            SECTION.
      *
           PERFORM 50011-GMNHEN-VIEW-SEC
      *
           MOVE    "前回"              TO      L04-LBLKIKAN1
           MOVE    "〜"                TO      L04-LBLKIKAN2
      *
           MOVE    WIDGET-NORMAL       TO      L04-STSRYYMD-STATE
                                               L04-STHR-STATE
                                               L04-STMIN-STATE
                                               L04-EDSRYYMD-STATE
                                               L04-EDHR-STATE
                                               L04-EDMIN-STATE
                                               L04-HOURSKBN-STATE
                                               L04-HOURSKBN-ITEM-STATE
      *
           MOVE    WIDGET-INSENSITIVE  TO      L04-STNO-STATE
                                               L04-EDNO-STATE
      *
           EVALUATE   SPA-NAI-L04-MESPROCKBN
           WHEN    "1"
               MOVE    "本締め"
                                   TO  L04-HOURSKBN
                                       L04-HOURSKBN-ITEM (1)
               MOVE    1           TO  L04-HOURSKBN-COUNT
           WHEN    "2"
               MOVE    "仮締め"
                                   TO  L04-HOURSKBN
                                       L04-HOURSKBN-ITEM (1)
               MOVE    1           TO  L04-HOURSKBN-COUNT
           WHEN    "3"
               MOVE    WIDGET-INSENSITIVE
                                   TO  L04-HOURSKBN-STATE
                                       L04-HOURSKBN-ITEM-STATE
           END-EVALUATE
      *
           .
       5001-GMNHEN-VIEW-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理（履歴表示）
      *****************************************************************
       50011-GMNHEN-VIEW-SEC            SECTION.
      *
           MOVE    ZERO            TO  IDXL
           PERFORM VARYING IDX-TERM    FROM    1   BY  1
                   UNTIL ( IDX-TERM    >   CONST-TERM-MAX )
                    OR   ( SPA-NAI-L04-SRYYMD (IDX-TERM 2)
                                       =   SPACE )
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   2 )
      *
                   MOVE    1           TO  FLG-EDIT
                   IF    ( IDX1    =   1 )
                       IF    ( SPA-NAI-L04-HOURSKBN (IDX-TERM)
                                           =   "1" )
                         OR  ( IDX-TERM    =   1 )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-EDIT
                       END-IF
                   END-IF
      *
                   IF    ( FLG-EDIT    =   1 )
                       MOVE    SPA-GMN-L04-SRYYMD (IDX-TERM IDX1)
                                       TO  L04-SRYYMD (IDX-TERM IDX1)
                       IF    ( L04-SRYYMD (IDX-TERM IDX1)
                                       NOT =   SPACE )
                           MOVE    SPA-GMN-L04-TIME (IDX-TERM IDX1)
                                       TO  WRK-KJNHM
                           MOVE    SPACE   TO  WRK-TIME-G
                           MOVE    WRK-KJNHM-HH
                                       TO  WRK-TIME-HH
                           MOVE    ":" TO  WRK-TIME-SEP
                           MOVE    WRK-KJNHM-MM
                                       TO  WRK-TIME-MM
                           MOVE    WRK-TIME-G
                                       TO  L04-TIME (IDX-TERM IDX1)
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( L04-TIME (IDX-TERM 1)   NOT =   SPACE )
                   COMPUTE IDXL    =   IDXL    +   1
                   MOVE    IDXL        TO  WRK-Z9
                   MOVE    WRK-Z9      TO  L04-LBLNO (IDX-TERM 1)
      *
                   MOVE    "〜"        TO  L04-LBLMID (IDX-TERM)
               END-IF
      *
               EVALUATE    SPA-NAI-L04-HOURSKBN (IDX-TERM)
               WHEN    "1"
                   MOVE    "red"       TO  L04-LBLMARK-STYLE (IDX-TERM)
                   MOVE    "本締め"    TO  L04-LBLMARK       (IDX-TERM)
               WHEN    "2"
                   MOVE    "blue"      TO  L04-LBLMARK-STYLE (IDX-TERM)
                   MOVE    "仮締め"    TO  L04-LBLMARK       (IDX-TERM)
               END-EVALUATE
      *
           END-PERFORM
      *
           EVALUATE    SPA-GMN-L04-VIEW-MODE
           WHEN    1
               MOVE    "本締めの履歴"
                                       TO  L04-LBLMES01
               MOVE    "全て表示"
                                       TO  L04-B04-LABEL
           WHEN    ZERO
               MOVE    "本締め・仮締めの履歴"
                                       TO  L04-LBLMES01
               MOVE    "前回本締め"    TO  L04-B04-LABEL
           WHEN    2
                MOVE    "前回の本締め"
                                       TO  L04-LBLMES01
                MOVE    "本締めのみ"   TO  L04-B04-LABEL
           END-EVALUATE
      *
           MOVE    SPA-GMN-L04-STSRYYMD    TO  L04-STSRYYMD
           MOVE    SPA-GMN-L04-STNO        TO  L04-STNO
           MOVE    SPA-GMN-L04-STHR        TO  L04-STHR
           MOVE    SPA-GMN-L04-STMIN       TO  L04-STMIN
           MOVE    SPA-GMN-L04-EDSRYYMD    TO  L04-EDSRYYMD
           MOVE    SPA-GMN-L04-EDNO        TO  L04-EDNO
           MOVE    SPA-GMN-L04-EDHR        TO  L04-EDHR
           MOVE    SPA-GMN-L04-EDMIN       TO  L04-EDMIN
      *
           .
       50011-GMNHEN-VIEW-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理（本締め・仮締め）
      *****************************************************************
       5001-GMNHEN-SIME-SEC            SECTION.
      *
           PERFORM 50011-GMNHEN-VIEW-SEC
      *
           MOVE    "期間"              TO      L04-LBLKIKAN1
      *
           MOVE    "〜"                TO      L04-LBLKIKAN2
      *
           MOVE    WIDGET-NORMAL       TO      L04-STSRYYMD-STATE
                                               L04-STHR-STATE
                                               L04-STMIN-STATE
                                               L04-EDSRYYMD-STATE
                                               L04-EDHR-STATE
                                               L04-EDMIN-STATE
                                               L04-HOURSKBN-STATE
                                               L04-HOURSKBN-ITEM-STATE
      *
           MOVE    WIDGET-INSENSITIVE  TO      L04-STNO-STATE
                                               L04-EDNO-STATE
      *
           MOVE    "darkgray"          TO      L04-STSRYYMD-STYLE
                                               L04-STHR-STYLE
                                               L04-STMIN-STYLE
      *
           EVALUATE   SPA-GMN-L04-PROCKBN
           WHEN    "1"
               MOVE    "本締め"
                                   TO  L04-HOURSKBN
                                       L04-HOURSKBN-ITEM (1)
               MOVE    1           TO  L04-HOURSKBN-COUNT
           WHEN    "2"
               MOVE    "仮締め"
                                   TO  L04-HOURSKBN
                                       L04-HOURSKBN-ITEM (1)
               MOVE    1           TO  L04-HOURSKBN-COUNT
           END-EVALUATE
      *
           MOVE    "登録"                  TO  L04-B12-LABEL
      *
           .
       5001-GMNHEN-SIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理（期間指定）
      *****************************************************************
       5001-GMNHEN-SELTERM-SEC     SECTION.
      *
           MOVE    ZERO            TO  IDXL
           PERFORM VARYING IDX-TERM    FROM    1   BY  1
                   UNTIL ( IDX-TERM    >   CONST-TERM-MAX )
                    OR   ( SPA-NAI-L04-SRYYMD (IDX-TERM 2)
                                       =   SPACE )
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   2 )
      *
                   MOVE    SPA-GMN-L04-SRYYMD (IDX-TERM IDX1)
                                   TO  L04-SRYYMD (IDX-TERM IDX1)
      *
                   IF    ( L04-SRYYMD (IDX-TERM IDX1)
                                       NOT =   SPACE )
                       MOVE    SPA-GMN-L04-TIME (IDX-TERM IDX1)
                                   TO  WRK-KJNHM
                       MOVE    SPACE   TO  WRK-TIME-G
                       MOVE    WRK-KJNHM-HH
                                       TO  WRK-TIME-HH
                       MOVE    ":"     TO  WRK-TIME-SEP
                       MOVE    WRK-KJNHM-MM
                                       TO  WRK-TIME-MM
                       MOVE    WRK-TIME-G
                                       TO  L04-TIME (IDX-TERM IDX1)
                   END-IF
      *
               END-PERFORM
      *
               IF    ( L04-TIME (IDX-TERM 2)   NOT =   SPACE )
                   COMPUTE IDXL    =   IDXL    +   1
                   MOVE    IDXL        TO  WRK-Z9
                   MOVE    WRK-Z9      TO  L04-LBLNO (IDX-TERM 1)
                   MOVE    "〜"        TO  L04-LBLMID (IDX-TERM)
               END-IF
      *
               EVALUATE    SPA-NAI-L04-HOURSKBN (IDX-TERM)
               WHEN    "1"
                   MOVE    "red"       TO  L04-LBLMARK-STYLE (IDX-TERM)
                   MOVE    "本締め"    TO  L04-LBLMARK       (IDX-TERM)
               WHEN    "2"
                   MOVE    "blue"      TO  L04-LBLMARK-STYLE (IDX-TERM)
                   MOVE    "仮締め"    TO  L04-LBLMARK       (IDX-TERM)
               WHEN    "3"
                   MOVE    "blue"      TO  L04-LBLMARK-STYLE (IDX-TERM)
                   EVALUATE   SPA-NAI-L04-MESPROCKBN
                   WHEN    "1"
                       MOVE    "red"   TO  L04-LBLMARK-STYLE (IDX-TERM)
                       MOVE    "前回（本締め）"
                                       TO  L04-LBLMARK       (IDX-TERM)
                   WHEN    "2"
                       MOVE    "前回（仮締め）"
                                       TO  L04-LBLMARK       (IDX-TERM)
                   WHEN    "3"
                       MOVE    "前回"
                                       TO  L04-LBLMARK       (IDX-TERM)
                   END-EVALUATE
               END-EVALUATE
      *
               IF    ( SPA-GMN-L04-TERM-STSEL  =   IDX-TERM )
                   IF    ( SPA-NAI-L04-SRYYMD (IDX-TERM 1)
                                           =   SPA-NAI-L04-STSRYYMD )
                    AND  ( SPA-GMN-L04-TIME   (IDX-TERM 1)
                                           =   SPA-GMN-L04-STTIME )
                       MOVE    "blue-revbutton"
                                       TO  L04-CHK-SRYYMD-STYLE
                                                            (IDX-TERM 1)
                       MOVE    "blue-revbutton"
                                       TO  L04-CHK-TIME-STYLE  
                                                            (IDX-TERM 1)
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-L04-TERM-EDSEL  =   IDX-TERM )
                   IF    ( SPA-NAI-L04-SRYYMD (IDX-TERM 2)
                                       =   SPA-NAI-L04-EDSRYYMD )
                    AND  ( SPA-GMN-L04-TIME   (IDX-TERM 2)
                                       =   SPA-GMN-L04-EDTIME )
                       MOVE    "blue-revbutton"
                                   TO  L04-CHK-SRYYMD-STYLE 
                                                            (IDX-TERM 2)
                       MOVE    "blue-revbutton"
                                   TO  L04-CHK-TIME-STYLE
                                                        (IDX-TERM 2)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    "期間"                  TO  L04-LBLKIKAN1
           MOVE    "〜"                    TO  L04-LBLKIKAN2
      *
           MOVE    WIDGET-NORMAL           TO  L04-STSRYYMD-STATE
                                               L04-STNO-STATE
                                               L04-STHR-STATE
                                               L04-STMIN-STATE
                                               L04-EDSRYYMD-STATE
                                               L04-EDNO-STATE
                                               L04-EDHR-STATE
                                               L04-EDMIN-STATE
      *
           MOVE    WIDGET-INSENSITIVE      TO  L04-HOURSKBN-STATE
                                               L04-HOURSKBN-ITEM-STATE
      *
           MOVE    SPA-GMN-L04-STSRYYMD    TO  L04-STSRYYMD
           MOVE    SPA-GMN-L04-STNO        TO  L04-STNO
           MOVE    SPA-GMN-L04-STHR        TO  L04-STHR
           MOVE    SPA-GMN-L04-STMIN       TO  L04-STMIN
           MOVE    SPA-GMN-L04-EDSRYYMD    TO  L04-EDSRYYMD
           MOVE    SPA-GMN-L04-EDNO        TO  L04-EDNO
           MOVE    SPA-GMN-L04-EDHR        TO  L04-EDHR
           MOVE    SPA-GMN-L04-EDMIN       TO  L04-EDMIN
      *
           MOVE    "登録"                  TO  L04-B12-LABEL
      *
           .
       5001-GMNHEN-SELTERM-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理（追加・変更・削除）
      *****************************************************************
       5001-GMNHEN-UPD-SEC             SECTION.
      *
      *
           MOVE    ZERO            TO  IDXL
           PERFORM VARYING IDX-TERM    FROM    1   BY  1
                   UNTIL ( IDX-TERM    >   CONST-TERM-MAX )
                    OR   ( SPA-NAI-L04-SRYYMD (IDX-TERM 2)
                                       =   SPACE )
      *
               MOVE    SPA-GMN-L04-SRYYMD (IDX-TERM 2)
                                       TO  L04-SRYYMD (IDX-TERM 2)
               IF    ( L04-SRYYMD (IDX-TERM 2)
                                       NOT =   SPACE )
                   MOVE    SPA-GMN-L04-TIME (IDX-TERM 2)
                                       TO  WRK-KJNHM
                   MOVE    SPACE       TO  WRK-TIME-G
                   MOVE    WRK-KJNHM-HH
                                       TO  WRK-TIME-HH
                   MOVE    ":"         TO  WRK-TIME-SEP
                   MOVE    WRK-KJNHM-MM
                                       TO  WRK-TIME-MM
                   MOVE    WRK-TIME-G  TO  L04-TIME (IDX-TERM 2)
               END-IF
      *
               IF    ( L04-TIME (IDX-TERM 2)   NOT =   SPACE )
                   COMPUTE IDXL    =   IDXL    +   1
                   MOVE    IDXL        TO  WRK-Z9
                   MOVE    WRK-Z9      TO  L04-LBLNO (IDX-TERM 2)
               END-IF
      *
               EVALUATE    SPA-NAI-L04-HOURSKBN (IDX-TERM)
               WHEN    "1"
                   MOVE    "red"       TO  L04-LBLMARK-STYLE (IDX-TERM)
                   MOVE    "本締め"    TO  L04-LBLMARK       (IDX-TERM)
               WHEN    "2"
                   MOVE    "blue"      TO  L04-LBLMARK-STYLE (IDX-TERM)
                   MOVE    "仮締め"    TO  L04-LBLMARK       (IDX-TERM)
               END-EVALUATE
      *
               IF    ( SPA-GMN-L04-TERM-EDSEL  =   IDX-TERM )
                   EVALUATE    SPA-GMN-L04-PROCKBN
                   WHEN    CONST-UPD
                           MOVE    "blue-revbutton"
                                       TO  L04-CHK-SRYYMD-STYLE
                                                            (IDX-TERM 2)
                           MOVE    "blue-revbutton"
                                       TO  L04-CHK-TIME-STYLE
                                                            (IDX-TERM 2)
                   WHEN    CONST-DEL
                           MOVE    "yellow-revbutton"
                                       TO  L04-CHK-SRYYMD-STYLE
                                                            (IDX-TERM 2)
                           MOVE    "yellow-revbutton"
                                       TO  L04-CHK-TIME-STYLE
                                                            (IDX-TERM 2)
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           MOVE    WIDGET-INSENSITIVE  TO  L04-STSRYYMD-STATE
                                           L04-STNO-STATE
                                           L04-STHR-STATE
                                           L04-STMIN-STATE
           MOVE    WIDGET-NORMAL       TO  L04-EDSRYYMD-STATE
                                           L04-EDNO-STATE
                                           L04-EDHR-STATE
                                           L04-EDMIN-STATE
                                           L04-HOURSKBN-STATE
                                           
           EVALUATE    SPA-GMN-L04-PROCKBN
           WHEN    CONST-INS
               MOVE    WIDGET-INSENSITIVE  TO  L04-EDNO-STATE
           END-EVALUATE
      *
           MOVE    SPA-GMN-L04-EDSRYYMD    TO  L04-EDSRYYMD
           MOVE    SPA-GMN-L04-EDNO        TO  L04-EDNO
           MOVE    SPA-GMN-L04-EDHR        TO  L04-EDHR
           MOVE    SPA-GMN-L04-EDMIN       TO  L04-EDMIN
      *
           MOVE    SPA-GMN-L04-HOURSKBN-G  TO  L04-HOURSKBN
      *
           MOVE    SPA-GMN-L04-HOURSKBN-MAX TO  L04-HOURSKBN-COUNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-L04-HOURSKBN-MAX )
      *
               MOVE    SPA-GMN-L04-HOURSKBNLST (IDX1)
                                       TO  L04-HOURSKBN-ITEM (IDX1)
      *
           END-PERFORM
      *
           MOVE    "登録"              TO  L04-B12-LABEL
      *
           .
       5001-GMNHEN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-L04-CUR     =   ZERO )
      *
               PERFORM 5011-INPUT-CUR-SEC
      *
           END-IF
      *
           PERFORM 5012-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-L04-CUR
           WHEN    01
               MOVE    "PROCKBN"       TO  MCP-WIDGET
           WHEN    02
               MOVE    "STNO"          TO  MCP-WIDGET
           WHEN    03
               MOVE    "STSRYYMD"      TO  MCP-WIDGET
           WHEN    04
               MOVE    "STHR"          TO  MCP-WIDGET
           WHEN    05
               MOVE    "STMIN"         TO  MCP-WIDGET
           WHEN    06
               MOVE    "EDNO"          TO  MCP-WIDGET
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
           WHEN    "PROCKBN"
               MOVE    02              TO  SPA-GMN-L04-CUR
           WHEN    "STNO"
               MOVE    03              TO  SPA-GMN-L04-CUR
           WHEN    "STSRYYMD"
               MOVE    04              TO  SPA-GMN-L04-CUR
           WHEN    "STHR"
               MOVE    05              TO  SPA-GMN-L04-CUR
           WHEN    "STMIN"
               MOVE    06              TO  SPA-GMN-L04-CUR
           WHEN    "EDNO"
               MOVE    07              TO  SPA-GMN-L04-CUR
           WHEN    "EDSRYYMD"
               MOVE    08              TO  SPA-GMN-L04-CUR
           WHEN    "EDHR"
               MOVE    09              TO  SPA-GMN-L04-CUR
           WHEN    "EDMIN"
               MOVE    10              TO  SPA-GMN-L04-CUR
           WHEN    "HOURSKBN"
               MOVE    212             TO  SPA-GMN-L04-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-CUR-SKIP-SEC               SECTION.
      *
           EVALUATE    SPA-GMN-L04-PROCKBN
           WHEN    CONST-VIEW
               MOVE    1           TO  SPA-GMN-L04-CUR
           WHEN    CONST-SIME
           WHEN    CONST-SIME-KARI
               IF    ( SPA-GMN-L04-CUR     = 2 OR 3 OR 4 OR 5 OR 6)
                   MOVE    7       TO  SPA-GMN-L04-CUR
               END-IF
               IF    ( SPA-GMN-L04-CUR     = 10 )
                   MOVE    212     TO  SPA-GMN-L04-CUR
               END-IF
           WHEN    CONST-INS
               IF    ( SPA-GMN-L04-CUR     = 2 OR 3 OR 4 OR 5 OR 6)
                   MOVE    7       TO  SPA-GMN-L04-CUR
               END-IF
           WHEN    CONST-SELTERM
               IF    ( SPA-GMN-L04-CUR     = 10 )
                   MOVE    212     TO  SPA-GMN-L04-CUR
               END-IF
           WHEN    CONST-UPD
               IF    ( SPA-GMN-L04-CUR     = 2 OR 3 OR 4 OR 5 )
                   MOVE    6       TO  SPA-GMN-L04-CUR
               END-IF
               IF    ( SPA-GMN-L04-CUR     = 7 OR 8 OR 9  )
                   MOVE    10      TO  SPA-GMN-L04-CUR
               END-IF
           WHEN    CONST-DEL
               IF    ( SPA-GMN-L04-CUR     = 2 OR 3 OR 4 OR 5 )
                   MOVE    6       TO  SPA-GMN-L04-CUR
               END-IF
               IF    ( SPA-GMN-L04-CUR NOT = 6 )
                   MOVE    212     TO  SPA-GMN-L04-CUR
               END-IF
           END-EVALUATE
      *
           .
      *
       5012-CUR-SKIP-EXT.
           EXIT.
      *
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
           WHEN    "0002"
               MOVE    "対象データが存在しません"  TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "時刻入力エラー"            TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "締め区分入力エラー"        TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "番号入力エラー"            TO  SPA-ERRMSG
           WHEN    "1001"
               STRING  "追加処理にて開始となる本締めの日付、時刻を"
                                           DELIMITED   BY  SIZE
                       "追加してください"  DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
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
           WHEN    "4001"          THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  LERR
           MOVE    SPA-ERRCD           TO  LERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  LERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "L04"               TO  SPA-MOTOPG
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
      *    数値変換処理
      *****************************************************************
       800-NUM-SEC                     SECTION.
      *
           MOVE    ZERO            TO  WRK-NUM
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-NUMX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO  )
               MOVE    SNUM-OUTNUM TO  WRK-NUM
           END-IF
      *
           .
       800-NUM-EXT.
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
           MOVE    SPA-NAI-L04-EDSRYYMD    TO  HOURS-SRYYMD
           MOVE    SPA-GMN-L04-EDTIME      TO  HOURS-TIME
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
           MOVE    SPA-NAI-L04-EDSRYYMD    TO  HOURS-SRYYMD
           MOVE    SPA-GMN-L04-EDTIME      TO  HOURS-TIME
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
           MOVE    SPA-HOSPNUM         TO  HOURS-HOSPNUM
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
