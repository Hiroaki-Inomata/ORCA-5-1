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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGW061.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 職員情報　職員一覧情報（Ｗ０６１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/11/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "W06SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PASSWD          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-SKIP            PIC 9(01).
           03  FLG-SELNUM          PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-COLOR           PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
           03  WRK-WIDMSG              PIC X(80).
           03  WRK-SDATE               PIC X(06).
           03  WRK-STIME.
               05  WRK-SHH              PIC 9(02).
               05  WRK-SMM              PIC 9(02).
               05  WRK-SSS              PIC 9(02).
      *
           03  WRK-SELNUM              PIC 9(04).
           03  WRK-ZZ9                 PIC ZZZ9.
           03  WRK-ZZZZ                PIC ZZZZ.
           03  WRK-MIDASI              PIC X(100).
           03  WRK-MIDASI2             PIC X(20).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *    コラムリスト位置
           03  WRK-W061-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-YMD                 PIC X(09). 
           03  WRK-H-STYMD             PIC X(09).
           03  WRK-H-EDYMD             PIC X(09).
           03  WRK-YUKOSTYMD           PIC X(08).
           03  WRK-YUKOEDYMD           PIC X(08).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *   最大件数
       01  CONS-MAX-LINE           PIC 9(03)   VALUE    100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *    職員情報２
           COPY  "CPSK1050.INC".
      *
      *    パスワード
           COPY    "CPPASSWD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA101SCRAREA.INC".
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
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W06
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W06         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
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
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      SPA-WIDCD       NOT =   SPACE
                   PERFORM 520-WIDSET-SEC
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "W061"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
      *            終了日変更登録
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2401-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W061-CUR
                   END-IF
               WHEN    "1002"
      *            非表示区分登録
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2402-TOUROKU-SYORI02-SEC
                   ELSE
                       MOVE    1           TO  SPA-W061-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE           TO  SPA-WID1-FLG
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
           INITIALIZE                  SPA-W061-AREA
      *
           MOVE    1                   TO  SPA-W061-PAGE
           PERFORM 3101-SPA-TBLHEN-SEC
      *    見出し編集処理
           PERFORM 3101-SPA-MIDASIHEN-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3101-SPA-TBLHEN-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
           INITIALIZE                  SPA-W061-DATA-LST-G
           INITIALIZE                  SPA-W061-INPUT-G
           INITIALIZE                  SPA-W061-SEL-G
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    "%"                 TO  SYS-1010-KBNCD (2:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           COMPUTE IDX-STR             =  (SPA-W061-PAGE  -  1)
                                       *   CONS-MAX-LINE
                                       +   1
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  SPA-W061-NEXT
           PERFORM
                   UNTIL       (SPA-W061-NEXT   =   1 )  OR
                               (FLG-SYSKANRI    =   1  )
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    ZERO                TO  FLG-SKIP
               IF      SPA-W061-HYOJIKBN   =   "1"
      *            最終履歴のみ表示
                   IF      (IDX                >   ZERO )
                    AND    (SYS-1010-KBNCD(2:4)
                                               =   SPA-W061-SYOKUCD
                                                               (IDX))
                       MOVE    2               TO  FLG-SKIP
                   END-IF
               END-IF
               IF      SPA-W061-HYOJIKBN   =   SPACE
      *            当日有効分のみ表示
                   IF      (SYS-1010-STYUKYMD  <=  SPA-SYSYMD)
                    AND    (SYS-1010-EDYUKYMD  >=  SPA-SYSYMD)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-SKIP
                   END-IF
               END-IF
      *        編集対象
               IF      FLG-SKIP            =   ZERO
                   ADD     1               TO  IDY
                   IF      IDY             <   IDX-STR
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX
                       IF      IDX             >   CONS-MAX-LINE
                           MOVE    1               TO  SPA-W061-NEXT
                       END-IF
                   END-IF
                   IF     (IDX             >   ZERO )
                      AND (SPA-W061-NEXT   =   ZERO )
                       PERFORM 31011-TBL-HENSYU-SEC
                   END-IF
               END-IF
      *        編集
               IF      FLG-SKIP            =   2
                   IF     (IDX             >   ZERO )
                      AND (SPA-W061-NEXT   =   ZERO )
                       PERFORM 31011-TBL-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-SPA-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       31011-TBL-HENSYU-SEC             SECTION.
      *
           MOVE    IDY                 TO  SPA-W061-TNUM     (IDX)
      *
           MOVE    SYS-1010-KBNCD(2:4) TO  SPA-W061-SYOKUCD  (IDX)
           MOVE    SYS-1010-KANANAME   TO  SPA-W061-KANANAME (IDX)
           MOVE    SYS-1010-NAME       TO  SPA-W061-NAME     (IDX)
           MOVE    SYS-1010-USERID     TO  SPA-W061-USERID     (IDX)
           MOVE    SYS-1010-STYUKYMD   TO  SPA-NAI061-STYUKYMD (IDX)
           MOVE    SYS-1010-EDYUKYMD   TO  SPA-NAI061-EDYUKYMD (IDX)
      *
           MOVE    SYS-1010-EDYUKYMD   TO  SPA-MAE061-EDYUKYMD (IDX)
      *
           MOVE    SYS-1010-STYUKYMD   TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-W061-STYUKYMD  (IDX)
           MOVE    SYS-1010-EDYUKYMD   TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-W061-EDYUKYMD (IDX)
           MOVE    WRK-HENYMDG         TO  SPA-MAEW061-EDYUKYMD(IDX)
      *    非表示
           IF      SYS-1010-HYOJIFLG   =   SPACE
               MOVE    "0"             TO  SYS-1010-HYOJIFLG
           END-IF
           MOVE    SYS-1010-HYOJIFLG   TO  SPA-W061-HIHYOUJI (IDX)
           MOVE    SYS-1010-HYOJIFLG   TO  SPA-MAE061-HIHYOUJI(IDX)
      *    管理者区分
           MOVE    SYS-1010-KANRIAUTHFLG
                                       TO  SPA-NAI061-KANRIAUTHFLG(IDX)
      *    パスワード判定
           PERFORM 900-PASSWD-READ-SEC
           IF      FLG-PASSWD          =   ZERO
               MOVE    "1"                 TO  SPA-W061-PASSWD (IDX)
           ELSE
               MOVE    SPACE               TO  SPA-W061-PASSWD (IDX)
           END-IF
      *    備考編集
           MOVE    SPACE               TO  SPA-W061-BIKOU (IDX)
           IF      SYS-1010-KANRIAUTHFLG   =   "T"
               MOVE    "管理者"            TO  SPA-W061-BIKOU (IDX)
               IF      SPA-W061-PASSWD (IDX)   =   "1"
                   MOVE    "管理者 PW"         TO  SPA-W061-BIKOU (IDX)
               END-IF
           ELSE
               IF      SPA-W061-PASSWD (IDX)   =   "1"
                   MOVE    "PW"                TO  SPA-W061-BIKOU (IDX)
               END-IF
           END-IF
      *
           MOVE    IDX                 TO  SPA-W061-MAX
           .
       3202-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       3101-SPA-MIDASIHEN-SEC                  SECTION.
      *
           EVALUATE    SPA-GMN-SYOKUKBN
               WHEN    "1"
                   MOVE    "【医師　一覧】"      TO  SPA-W061-MIDASI1
               WHEN    "2"
                   MOVE    "【看護師一覧】"      TO  SPA-W061-MIDASI1
               WHEN    "3"
                   MOVE    "【技師　一覧】"      TO  SPA-W061-MIDASI1
               WHEN    "4"
                   MOVE    "【事務職一覧】"      TO  SPA-W061-MIDASI1
               WHEN    "5"
                   MOVE    "【管理職一覧】"      TO  SPA-W061-MIDASI1
           END-EVALUATE
      *
           MOVE    "職員ＣＤ順"        TO  WRK-MIDASI2
           EVALUATE    SPA-W061-HYOJIKBN
               WHEN    "2"
                   MOVE    SPACE           TO  WRK-MIDASI
                   STRING    SPA-W061-MIDASI1
                             "（全部）"
                             "　職員ＣＤ順"
                                       DELIMITED  BY  SPACE
                                       INTO    WRK-MIDASI
                   END-STRING
                   MOVE    WRK-MIDASI      TO  SPA-W061-MIDASI1
               WHEN    "1"
                   MOVE    SPACE           TO  WRK-MIDASI
                   STRING    SPA-W061-MIDASI1
                             "（最終履歴）"
                             "　職員ＣＤ順"
                                       DELIMITED  BY  SPACE
                                       INTO    WRK-MIDASI
                   END-STRING
                   MOVE    WRK-MIDASI      TO  SPA-W061-MIDASI1
               WHEN    SPACE
                   MOVE    SPACE           TO  WRK-MIDASI
                   STRING    SPA-W061-MIDASI1
                             "（当日有効）"
                             "　職員ＣＤ順"
                                       DELIMITED  BY  SPACE
                                       INTO    WRK-MIDASI
                   END-STRING
                   MOVE    WRK-MIDASI      TO  SPA-W061-MIDASI1
           END-EVALUATE
           .
       3101-SPA-MIDASIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-W061-ROW
           ELSE
               MOVE    W061-DATALIST-ROW   TO  WRK-W061-ROW
           END-IF
      *
           MOVE    SPACE               TO  W061
           INITIALIZE                      W061
      *    コラムリストの位置指定
           MOVE    ZERO                TO  W061-DATALIST-ROW
           IF      WRK-W061-ROW    NOT =   ZERO
               MOVE    WRK-W061-ROW    TO  W061-DATALIST-ROW
      ****     MOVE    2               TO  W061-DATALIST-ROWATTR
           END-IF
      *
           MOVE    ZERO                TO  FLG-COLOR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-W061-MAX
               MOVE    SPA-W061-TNUM (IDX) TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  W061-TNUM    (IDX)
               MOVE    SPA-W061-SYOKUCD(IDX)
                                           TO  W061-SYOKUCD (IDX)
      *        同じ職員コードは表示なし
               IF     (IDX             >   1  )
                 AND  (SPA-W061-SYOKUCD(IDX)
                                       =   SPA-W061-SYOKUCD(IDX - 1))
                   MOVE    SPACE           TO  W061-SYOKUCD (IDX)
               END-IF
      *
               MOVE    SPA-W061-NAME (IDX)
                                           TO  W061-NAME  (IDX)
               MOVE    SPA-W061-USERID (IDX)
                                           TO  W061-USERID  (IDX)
               MOVE    SPA-W061-STYUKYMD (IDX)
                                           TO  W061-STYUKYMD  (IDX)
               MOVE    SPA-W061-EDYUKYMD (IDX)
                                           TO  W061-EDYUKYMD  (IDX)
      *        終了日変更
               IF     (SPA-NAI061-EDYUKYMD(IDX)
                                   NOT =   SPA-MAE061-EDYUKYMD(IDX))
                   MOVE    "*"             TO  W061-EDYUKYMD(IDX)(11:1)
               END-IF
      *
               IF      SPA-W061-HIHYOUJI(IDX)  =   "1"
                   MOVE    "×"            TO  W061-HIHHYOJI (IDX)
               END-IF
      *        備考
               MOVE    SPA-W061-BIKOU (IDX)    TO  W061-BIKOU (IDX)
      *
               IF     (SPA-W061-SELNUM     >   ZERO )
                  AND (SPA-W061-SELNUM     =   SPA-W061-TNUM (IDX) )
      ***          MOVE    IDX             TO  W061-DATALIST-ROW
      ***          MOVE    2               TO  W061-DATALIST-ROWATTR
                   MOVE    "T"             TO  W061-DATALIST-SELECTDATA
                                                                (IDX)
               ELSE
                   MOVE    "F"             TO  W061-DATALIST-SELECTDATA
                                                                (IDX)
               END-IF
      *        職員毎の背景色
               IF     (IDX             >   1  )
                 AND  (SPA-W061-SYOKUCD(IDX)
                                   NOT =   SPA-W061-SYOKUCD(IDX - 1))
                   IF      FLG-COLOR        =   ZERO
                       MOVE    1               TO  FLG-COLOR
                   ELSE
                       MOVE    ZERO            TO  FLG-COLOR
                   END-IF
               END-IF
      ******** IF      FLG-COLOR       =   1
      *            MOVE    "#E5FFD1"       TO  W061-DATALIST-BGCOLOR
      *                                                        (IDX)
      ******** END-IF
           END-PERFORM
           MOVE    SPA-W061-MAX        TO  W061-DATALIST-COUNT
      *
           MOVE    SPA-W061-SELNUM     TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  W061-SELNUM
           MOVE    SPA-GMN061-EDYUYMD  TO  W061-EDYUYMD
      *
           MOVE    SPA-W061-MIDASI1    TO  W061-MIDASI1
      *
           EVALUATE    SPA-W061-SYORIKBN
           WHEN    "1"
               MOVE    "≪終了日変更≫"
                                            TO  W061-MIDASI2
               MOVE    "red"                TO  W061-MIDASI2-STYLE
               MOVE    "終了日付"           TO  W061-MIDASI3
               STRING  "※「F2 クリア」「F12 登録」押下で"
                       "終了日変更を終了します"
                                            DELIMITED  BY  SIZE
                                            INTO    W061-MIDASI4
               END-STRING
           WHEN    "2"
               MOVE    "≪表示変更≫"
                                            TO  W061-MIDASI2
               MOVE    "red"                TO  W061-MIDASI2-STYLE
               MOVE    SPACE                TO  W061-MIDASI3
               STRING  "※「F2 クリア」「F12 登録」押下で"
                       "表示変更を終了します"
                                            DELIMITED  BY  SIZE
                                            INTO    W061-MIDASI4
               END-STRING
           WHEN    SPACE
               MOVE    "≪職員選択≫"
                                            TO  W061-MIDASI2
               MOVE    "black"              TO  W061-MIDASI2-STYLE
               MOVE    SPACE                TO  W061-MIDASI3
               STRING  "※「F9 表示変更」「F10 終了日変更」押下で"
                       "変更を開始します"
                                            DELIMITED  BY  SIZE
                                            INTO    W061-MIDASI4
               END-STRING
           END-EVALUATE
      *
           IF      SPA-W061-SYORIKBN   =   "1"
                                       OR  "2"
               MOVE    "F12 登録"          TO  W061-B12-LABEL
               MOVE    WIDGET-NORMAL       TO  W061-B12-STATE
           ELSE
               MOVE    SPACE               TO  W061-B12-LABEL
               MOVE    WIDGET-INSENSITIVE  TO  W061-B12-STATE
           END-IF
      *
           IF      SPA-W061-NEXT       =   1
               IF      SPA-W061-PAGE       =   1
                   MOVE    "次頁あり"          TO  W061-MIDASI5
               ELSE
                   MOVE    "前次頁あり"        TO  W061-MIDASI5
               END-IF
               MOVE    "red"               TO  W061-MIDASI5-STYLE
           ELSE
               IF      SPA-W061-PAGE       >   1
                   MOVE    "前頁あり"          TO  W061-MIDASI5
                   MOVE    "red"               TO  W061-MIDASI5-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-W061-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "SELNUM"
                       MOVE    1               TO  SPA-W061-CUR
                   WHEN    "EDYUYMD"
                       IF      SPA-W061-SYORIKBN   =   "1"
                           MOVE    1               TO  SPA-W061-CUR
                       ELSE
                           MOVE    1               TO  SPA-W061-CUR
                       END-IF
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-W061-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "EDYUYMD"           TO  MCP-WIDGET
               WHEN    3
                   MOVE    "B12"               TO  MCP-WIDGET
               WHEN    4
                   MOVE    "B10"               TO  MCP-WIDGET
               WHEN    6
                   MOVE    "B11"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE           TO  SPA-W061-SEL-G
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLREA-SEC
      *        切替え
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 240-KIRIKAE-SEC
      *        変更
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 2100-ENDYMD-SEC
      *        非表示
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 209-HIHYOJI-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 260-MAE-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 270-NEXT-PAGE-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 2120-TOUROKU-SEC
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
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *
           EVALUATE    WRK-WIDGET
               WHEN    "DATALIST"
                   PERFORM 2001-MEILIST-SEC
               WHEN    "SELNUM"
                   PERFORM 2002-SELNUM-CHK-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 220-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-W061-CUR
      *    入力項目の決定
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    画面をＳＰＡへ編集
           PERFORM 2201-GMNSPA-SET-SEC
      *
      *    終了日チェック
           PERFORM 2003-EDYUYMD-CHK-SEC
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト選択処理
      *****************************************************************
       2001-MEILIST-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-W061-MAX
              IF      W061-DATALIST-SELECTDATA (IDX)    =  "T"
                  IF       SPA-W061-TNUM (IDX)  =   SPA-W061-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-W061-TNUM (IDX)  TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-W061-SELNUM-X
               MOVE    1                   TO  FLG-SELNUM
               PERFORM 2002-SELNUM-SEC
           END-IF
      *
           .
       2001-MEILIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2201-GMNSPA-SET-SEC               SECTION.
      *
           MOVE    W061-EDYUYMD         TO  SPA-GMN061-EDYUYMD
           .
       2201-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       2002-SELNUM-CHK-SEC               SECTION.
      *
           MOVE    W061-SELNUM          TO  SPA-W061-SELNUM-X
           PERFORM 2002-SELNUM-SEC
      *
           .
       2002-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       2002-SELNUM-SEC               SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-W061-SELNUM-X   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-W061-CUR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-W061-SELNUM
           END-IF
           IF      (SPA-W061-SELNUM    >   ZERO)  AND
                   (SPA-ERRCD          =   SPACE) 
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  IDX-T
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-W061-MAX ) OR
                                  (FLG-CHK =   1    )
                   IF      SPA-W061-TNUM (IDX) =   SPA-W061-SELNUM
                       MOVE    IDX                 TO  IDX-T
                       MOVE    1                   TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W061-CUR
               END-IF
      *
               IF     (IDX-T               >   ZERO )
      *            選択内容
                   MOVE    SPA-W061-SYOKUCD (IDX-T)
                                               TO  SPA-W061-SEL-SYOKUCD
                   MOVE    SPA-NAI061-STYUKYMD(IDX-T)
                                               TO  SPA-W061-SEL-STYUKYMD
                   MOVE    SPA-NAI061-EDYUKYMD(IDX-T)
                                               TO  SPA-W061-SEL-EDYUKYMD
                   IF     (SPA-W061-SYORIKBN   =   SPACE)
                       PERFORM 210-BACK
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE)
            AND   (FLG-END             =   ZERO )
            AND   (SPA-W061-SELNUM     >   ZERO )
               IF      SPA-W061-SYORIKBN   =   "1"
      *            終了日編集
                   PERFORM 20021-LASTYMD-HEN-SEC
               END-IF
      *
               IF      SPA-W061-SYORIKBN   =   "2"
      *            非表示設定
                   PERFORM 20022-HIHYOUJI-HEN-SEC
               END-IF
           END-IF
      *
           IF      SPA-W061-SELNUM     =   ZERO
               MOVE    1                  TO  SPA-W061-CUR
           END-IF
           .
       2002-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了日編集処理
      *****************************************************************
       20021-LASTYMD-HEN-SEC                 SECTION.
      *
      *    終了日を元に戻す
           IF     (SPA-NAI061-EDYUKYMD(IDX-T)
                                   NOT =   SPA-MAE061-EDYUKYMD(IDX-T))
               MOVE    SPA-MAE061-EDYUKYMD(IDX-T)
                                       TO  SPA-NAI061-EDYUKYMD(IDX-T)
               MOVE    SPA-MAEW061-EDYUKYMD(IDX-T)
                                       TO  SPA-W061-EDYUKYMD  (IDX-T)
               MOVE    ZERO            TO  SPA-W061-SELNUM
               MOVE    SPACE           TO  SPA-W061-SELNUM-X
               GO      TO      20021-LASTYMD-HEN-EXT
           END-IF
      *
      *    終了日チェック
           PERFORM 2003-EDYUYMD-CHK-SEC
      *    終了日の入力があること
           IF     (SPA-NAI061-EDYUYMD  =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "0004"              TO  SPA-ERRCD
           END-IF
      *    開始日＜終了日
           IF     (SPA-NAI061-STYUKYMD(IDX-T)  >=  SPA-NAI061-EDYUYMD)
              AND (SPA-ERRCD               =   SPACE   )
               MOVE    "0005"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-W061-CUR
               GO      TO      20021-LASTYMD-HEN-EXT
           END-IF
      *
      *    最終履歴であること
           IF     (SPA-NAI061-EDYUKYMD(IDX-T)  =   "99999999")
              OR  (SPA-W061-HYOJIKBN   =   "1"  )
               CONTINUE
           ELSE
      *        最終履歴以外のチェック
               PERFORM 200211-RIREKI-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        有効終了日編集
               MOVE    SPA-NAI061-EDYUYMD
                                       TO  SPA-NAI061-EDYUKYMD(IDX-T)
               MOVE    SPA-GMN061-EDYUYMD
                                       TO  SPA-W061-EDYUKYMD(IDX-T)
      *
               MOVE    ZERO            TO  SPA-W061-SELNUM
               MOVE    SPACE           TO  SPA-W061-SELNUM-X
           END-IF
           .
       20021-LASTYMD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終履歴チェック処理
      *****************************************************************
       200211-RIREKI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           IF      SPA-W061-HYOJIKBN   =   "2"
      *        全部
               IF     (IDX-T           <   SPA-W061-MAX  )
                  AND (SPA-W061-SYOKUCD(IDX-T)
                                       NOT =   SPA-W061-SYOKUCD
                                                        (IDX-T + 1))
                   MOVE    1               TO  FLG-OK
               ELSE
                   IF     (IDX-T           >   SPA-W061-MAX  )
                       MOVE    "0003"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *    システム管理検索
           IF      (FLG-OK             =   ZERO )
             AND   (SPA-ERRCD          =   SPACE)
               MOVE    SPACE               TO  SYS-1010-REC
               INITIALIZE                      SYS-1010-REC
               MOVE    "1010"              TO  SYS-1010-KANRICD
               MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
               MOVE    SPA-W061-SYOKUCD(IDX-T)
                                           TO  SYS-1010-KBNCD (2:4)
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
               PERFORM
                       UNTIL   (FLG-SYSKANRI       =   1  )
                           OR  (SPA-ERRCD      NOT =   SPACE )
                   MOVE    SYSKANRI-REC        TO  SYS-1010-REC
                   IF      SYS-1010-EDYUKYMD   >
                                           SPA-NAI061-EDYUKYMD(IDX-T)
      *                選択終了日より、後の終了日あり
                       MOVE    "0003"          TO  SPA-ERRCD
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       200211-RIREKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    非表示設定処理
      *****************************************************************
       20022-HIHYOUJI-HEN-SEC                  SECTION.
      *
      *    非表示変更
           IF      SPA-W061-HIHYOUJI (IDX-T) =   "1"
               MOVE    "0"             TO  SPA-W061-HIHYOUJI (IDX-T)
           ELSE
               MOVE    "1"             TO  SPA-W061-HIHYOUJI (IDX-T)
           END-IF
           MOVE    ZERO                TO  SPA-W061-SELNUM
           MOVE    SPACE               TO  SPA-W061-SELNUM-X
           .
       20022-HIHYOUJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了日チェック処理
      *****************************************************************
       2003-EDYUYMD-CHK-SEC                  SECTION.
      *
           IF     (SPA-W061-SYORIKBN   =   "1"  )
               CONTINUE
           ELSE
               MOVE    SPACE               TO  SPA-NAI061-EDYUYMD
               MOVE    SPACE               TO  SPA-GMN061-EDYUYMD
               GO      TO      2003-EDYUYMD-CHK-EXT
           END-IF
      *
           IF     (WRK-WIDGET          =   "EDYUYMD" )
              AND (SPA-GMN061-EDYUYMD  =   SPACE     )
               MOVE    "99999999"          TO  SPA-NAI061-EDYUYMD
               MOVE    "99999999"          TO  SPA-GMN061-EDYUYMD
           END-IF
      *
           MOVE    SPA-GMN061-EDYUYMD      TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI061-EDYUYMD
               MOVE    WRK-HENYMDG         TO  SPA-GMN061-EDYUYMD
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-W061-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  SPA-W061-CUR
           END-IF
           .
       2003-EDYUYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLREA-SEC                  SECTION.
      *
           IF      SPA-W061-SELNUM     =   ZERO
      *        初期画面表示
               PERFORM 310-SPASET-SEC
           ELSE
               MOVE    ZERO                TO  SPA-W061-SELNUM
               MOVE    SPACE               TO  SPA-W061-SELNUM-X
           END-IF
           MOVE    1                   TO  SPA-W061-CUR
           .
       220-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *    切替え処理
      *****************************************************************
       240-KIRIKAE-SEC                  SECTION.
      *
           IF     (SPA-W061-SYORIKBN   =   "2"  )
               MOVE    "0013"              TO  SPA-ERRCD
           END-IF
           IF     (SPA-W061-SYORIKBN   =   "1"  )
               MOVE    "0014"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3                   TO  SPA-W061-CUR
               GO      TO      240-KIRIKAE-EXT
           END-IF
      *
           EVALUATE    SPA-W061-HYOJIKBN
               WHEN    "1"
                   MOVE    "2"                 TO  SPA-W061-HYOJIKBN
               WHEN    "2"
                   MOVE    SPACE               TO  SPA-W061-HYOJIKBN
               WHEN    SPACE
                   MOVE    "1"                 TO  SPA-W061-HYOJIKBN
           END-EVALUATE
      *
      *
           MOVE    1                   TO  SPA-W061-PAGE
           PERFORM 3101-SPA-TBLHEN-SEC
      *    見出し編集処理
           PERFORM 3101-SPA-MIDASIHEN-SEC
           MOVE    1                   TO  SPA-W061-CUR
      *
           .
       240-KIRIKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了日変更切替え処理
      *****************************************************************
       2100-ENDYMD-SEC                  SECTION.
      *
           MOVE    1                   TO  SPA-W061-CUR
           IF      SPA-W061-SYORIKBN   =   SPACE
               MOVE    "1"             TO  SPA-W061-SYORIKBN
               MOVE    2               TO  SPA-W061-CUR
           ELSE
               IF      SPA-W061-SYORIKBN   =   "2"
                   MOVE    "0013"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-W061-CUR
               END-IF
           END-IF
           .
       2100-ENDYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    非表示切替え処理
      *****************************************************************
       209-HIHYOJI-SEC                  SECTION.
      *
           MOVE    1                   TO  SPA-W061-CUR
           IF      SPA-W061-SYORIKBN   =   SPACE
               MOVE    "2"             TO  SPA-W061-SYORIKBN
               MOVE    1               TO  SPA-W061-CUR
           ELSE
               IF      SPA-W061-SYORIKBN   =   "1"
                   MOVE    "0014"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-W061-CUR
               END-IF
           END-IF
           .
       209-HIHYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-MAE-PAGE-SEC                  SECTION.
      *
           IF      SPA-W061-PAGE       =   1
               MOVE    "0006"              TO  SPA-ERRCD
           ELSE
               IF      SPA-W061-SELNUM     >   ZERO
                   MOVE    "0002"         TO   SPA-ERRCD
               END-IF
           END-IF
      *
           IF     (SPA-W061-SYORIKBN   =   "2"  )
              AND (SPA-ERRCD           =   SPACE )
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-W061-CUR
           END-IF
           IF     (SPA-W061-SYORIKBN   =   "1"  )
              AND (SPA-ERRCD           =   SPACE )
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-W061-CUR
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-WIDCD           =   SPACE )
               PERFORM 2601-MAE-PAGE-SYORI-SEC
           END-IF
           .
       260-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *     前頁処理
      *****************************************************************
       2601-MAE-PAGE-SYORI-SEC                   SECTION.
      *
           COMPUTE SPA-W061-PAGE       =   SPA-W061-PAGE   -  1
           PERFORM 3101-SPA-TBLHEN-SEC
           MOVE    1                   TO  SPA-W061-CUR
           .
       2601-MAE-PAGE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-NEXT-PAGE-SEC                  SECTION.
      *
           IF      SPA-W061-NEXT       =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
           ELSE
               IF      SPA-W061-SELNUM     >   ZERO
                   MOVE    "0002"         TO   SPA-ERRCD
               END-IF
           END-IF
           IF     (SPA-W061-SYORIKBN   =   "2"  )
              AND (SPA-ERRCD           =   SPACE )
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-W061-CUR
           END-IF
           IF     (SPA-W061-SYORIKBN   =   "1"  )
              AND (SPA-ERRCD           =   SPACE )
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-W061-CUR
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-WIDCD           =   SPACE )
               PERFORM 2701-NEXT-PAGE-SYORI-SEC
           END-IF
           .
       270-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       2701-NEXT-PAGE-SYORI-SEC                   SECTION.
      *
           COMPUTE SPA-W061-PAGE       =   SPA-W061-PAGE   +  1
           PERFORM 3101-SPA-TBLHEN-SEC
           MOVE    1                   TO  SPA-W061-CUR
           .
       2701-NEXT-PAGE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    更新処理
      *****************************************************************
       2120-TOUROKU-SEC                   SECTION.
      *
           IF     (SPA-W061-SYORIKBN   =   SPACE)
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-W061-CUR
               GO      TO      2120-TOUROKU-EXT
           END-IF
      *
           IF     (SPA-W061-SYORIKBN   =   "2"  )
               MOVE    "1002"          TO  SPA-WIDCD
               GO      TO      2120-TOUROKU-EXT
           END-IF
      *
           IF     (SPA-W061-SYORIKBN   =   "1"  )
               MOVE    "1001"          TO  SPA-WIDCD
               GO      TO      2120-TOUROKU-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *
           IF     (SPA-W061-SYORIKBN   =   "1"  )
            AND   (SPA-ERRCD           =   SPACE)
      *        関連チェック
               PERFORM 230-KANREN-CHK-SEC
      *        更新処理
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "1001"          TO  SPA-WIDCD
               END-IF
           END-IF
      *
           .
       2120-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       230-KANREN-CHK-SEC          SECTION.
      *
           IF      SPA-W061-SELNUM     =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-W061-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI061-EDYUYMD  =   SPA-W061-SEL-EDYUKYMD
                   MOVE    "0011"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-W061-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        期間重複チェック
               MOVE    SPACE               TO  SYS-1010-REC
               INITIALIZE                      SYS-1010-REC
               MOVE    "1010"              TO  SYS-1010-KANRICD
               MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
               MOVE    SPA-W061-SEL-SYOKUCD
                                           TO  SYS-1010-KBNCD(2:4)
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
               PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1  )
                          OR   (SPA-ERRCD  NOT =   SPACE)
                   MOVE    SYSKANRI-REC        TO  SYS-1010-REC
      *
                   IF      SYS-1010-STYUKYMD
                                        NOT =   SPA-W061-SEL-STYUKYMD
                       IF      (SYS-1010-EDYUKYMD
                                               >  SPA-W061-SEL-STYUKYMD)
                        AND    (SYS-1010-STYUKYMD
                                               <=  SPA-NAI061-EDYUYMD)
                           MOVE    "0012"          TO  SPA-ERRCD
                           MOVE    2               TO  SPA-W061-CUR
                       END-IF
                   END-IF
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               END-PERFORM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       230-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       2401-TOUROKU-SYORI-SEC          SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-W061-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-NAI061-EDYUKYMD (IDX)
                                   NOT =   SPA-MAE061-EDYUKYMD(IDX)
      *            終了日変更
                   MOVE    SPACE               TO  SYS-1010-REC
                   INITIALIZE                      SYS-1010-REC
                   MOVE    "1010"              TO  SYS-1010-KANRICD
                   MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD(1:1)
                   MOVE    SPA-W061-SYOKUCD(IDX)
                                               TO  SYS-1010-KBNCD(2:4)
                   MOVE    SPA-NAI061-STYUKYMD(IDX)
                                               TO  SYS-1010-STYUKYMD
                   MOVE    SPA-MAE061-EDYUKYMD (IDX)
                                               TO  SYS-1010-EDYUKYMD
      *
                   MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
                   MOVE    SYS-1010-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-KEY-SEC
                   IF      FLG-SYSKANRI            =   ZERO
                       MOVE    SYSKANRI-REC        TO  SYS-1010-REC
                       PERFORM 2401-SYSKANRI-EDYMDUPD-SEC
                   ELSE
                       MOVE    "0013"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-W061-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-W061-SYORIKBN
      *
               MOVE    1                   TO  SPA-W061-PAGE
               PERFORM 3101-SPA-TBLHEN-SEC
      *        見出し編集処理
               PERFORM 3101-SPA-MIDASIHEN-SEC
               MOVE    1                   TO  SPA-W061-CUR
           END-IF
           .
       2401-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報更新処理
      *****************************************************************
       2401-SYSKANRI-EDYMDUPD-SEC      SECTION.
      *
      *    終了日　更新
           MOVE    SPA-NAI061-EDYUKYMD (IDX)
                                       TO  SYS-1010-EDYUKYMD
      *
           MOVE    SMCNDATE-YMD        TO  SYS-1010-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-1010-UPHMS
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
           MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
           MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
           MOVE    SYS-STYUKYMD        TO  SYSUP-STYUKYMD
           MOVE    SPA-MAE061-EDYUKYMD (IDX)
                                       TO  SYSUP-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "W061 DB UPD ERR KEY:" SYS-1010-KEY 
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4905-SYS1050-UPD-SEC
           END-IF
      *
           .
       2401-SYSKANRI-EDYMDUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報２更新処理
      *****************************************************************
       4905-SYS1050-UPD-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-1050-REC
           INITIALIZE                      SYS-1050-REC
           MOVE    "1050"              TO  SYS-1050-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1050-KBNCD(1:1)
           MOVE    SPA-W061-SYOKUCD(IDX)
                                       TO  SYS-1050-KBNCD(2:4)
           MOVE    SPA-NAI061-STYUKYMD(IDX)
                                       TO  SYS-1050-STYUKYMD
           MOVE    SPA-NAI061-STYUKYMD(IDX)
                                       TO  SYS-1050-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1050-HOSPNUM
           MOVE    SYS-1050-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC        TO  SYS-1050-REC
               MOVE    SPA-NAI061-EDYUKYMD (IDX)
                                           TO  SYS-1050-EDYUKYMD
      *
               MOVE    SMCNDATE-YMD        TO  SYS-1050-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1050-UPHMS
               MOVE    SYS-1050-REC        TO  SYSKANRI-REC
      *
               MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SYS-STYUKYMD        TO  SYSUP-STYUKYMD
               MOVE    SPA-MAE061-EDYUKYMD (IDX)
                                           TO  SYSUP-EDYUKYMD
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W061 DB UPD ERR KEY:" SYS-1050-KEY 
               END-IF
           END-IF
      *
          .
       4905-SYS1050-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    非表示区分登録登録処理
      *****************************************************************
       2402-TOUROKU-SYORI02-SEC        SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-W061-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-W061-HIHYOUJI (IDX)
                                   NOT =   SPA-MAE061-HIHYOUJI(IDX)
      *            非表示区分変更
                   MOVE    SPACE               TO  SYS-1010-REC
                   INITIALIZE                      SYS-1010-REC
                   MOVE    "1010"              TO  SYS-1010-KANRICD
                   MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD(1:1)
                   MOVE    SPA-W061-SYOKUCD(IDX)
                                               TO  SYS-1010-KBNCD(2:4)
                   MOVE    SPA-NAI061-STYUKYMD(IDX)
                                               TO  SYS-1010-STYUKYMD
                   MOVE    SPA-NAI061-EDYUKYMD (IDX)
                                               TO  SYS-1010-EDYUKYMD
      *
                   MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
                   MOVE    SYS-1010-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-KEY-SEC
                   IF      FLG-SYSKANRI            =   ZERO
                       MOVE    SYSKANRI-REC        TO  SYS-1010-REC
                       MOVE    SPA-W061-HIHYOUJI (IDX)
                                              TO  SYS-1010-HYOJIFLG
                       PERFORM 24021-SYSKANRI-UPD-SEC
                   ELSE
                       MOVE    "0013"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-W061-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-W061-SYORIKBN
      *
               MOVE    1                   TO  SPA-W061-PAGE
               PERFORM 3101-SPA-TBLHEN-SEC
      *        見出し編集処理
               PERFORM 3101-SPA-MIDASIHEN-SEC
               MOVE    1                   TO  SPA-W061-CUR
           END-IF
           .
       2402-TOUROKU-SYORI02-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       24021-SYSKANRI-UPD-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  SYS-1010-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-1010-UPHMS
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "W061 DB UPD ERR KEY:" SYS-1010-KEY 
           END-IF
           .
       24021-SYSKANRI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W06"               TO  SPA-SAKIPG
           MOVE    "W061"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "W06"               TO  MCP-WINDOW
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
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W061"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING  "選択中です。登録または、クリアを行って"
                           "下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "最終履歴ではありません。"
                           "終了日変更はできません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   MOVE    "終了日暦日エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "開始日＜終了日で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "前頁はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "次頁はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   STRING  "終了日変更を行ってから"
                           "登録を押下して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "終了日の変更がありません。"
                           "登録処理は行いません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   MOVE    "期間が重複します。"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   STRING  "表示変更中です。"
                           "「クリア」か「登録」を行って下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0014"
                   STRING  "終了日変更中です。"
                           "「クリア」か「登録」を行って下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "登録エラーです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W061"              TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "WERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   STRING  "画面内の終了日付を変更します。"
                           "よろしいですか？"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1002"
                   STRING  "画面内の表示区分を変更します。"
                           "よろしいですか？"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
          END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W061"              TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           EVALUATE    WRK-YMD(1:8)
               WHEN    ZERO
                   MOVE    ZERO                TO  WRK-SYMD
                   MOVE    WRK-YMD(1:8)        TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"               TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)          TO  WRK-HENYMDG  
               WHEN    OTHER
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-YMD             TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    ZERO
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    OTHER
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-EVALUATE
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
                   MOVE    SPACE               TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               MOVE    SPACE               TO  SYSKANRI-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
            .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-PASSWD-READ-SEC             SECTION.
      *
           INITIALIZE                  PASSWD-REC
           MOVE    SYS-1010-USERID     TO  PASSWD-USERID
           MOVE    PASSWD-REC          TO  MCPDATA-REC
           MOVE    "tbl_passwd"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_passwd"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE   ZERO             TO  FLG-PASSWD
               ELSE
                   MOVE    1               TO  FLG-PASSWD
               END-IF
           ELSE
               MOVE    1               TO  FLG-PASSWD
           END-IF
      *
           MOVE    "tbl_passwd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PASSWD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               MOVE    SPACE           TO  SYSKANRI-REC
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
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
