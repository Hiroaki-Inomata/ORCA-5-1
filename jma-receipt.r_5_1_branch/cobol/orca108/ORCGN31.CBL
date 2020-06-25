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
       PROGRAM-ID.             ORCGN31.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 住所マスタ設定
      *  コンポーネント名  : 住所マスタ登録
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/08/04    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 01.01.01     NACL-森脇     04/10/04  全角半角チェック追加
      * 04.06.00     NACL-多々納   11/03/02  通り名称追加
      *                                      一括取り込み他追加
      * 04.06.00     NACL-多々納   11/03/22  LD104 ,LD105,LD106,LD108,LD109　統合
      * 05.01.00     NACL-多々納   17/09/XX  マスター分離対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "N31COMMON-SPA".
      *
           COPY    "N31SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ADRS            PIC 9(01).
           03  FLG-USERADRS        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-SYORI-END       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(05).
           03  IDXC                PIC 9(04).
           03  IDXL                PIC 9(04).
      *
           03  IDX-STR             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH-TABLE      PIC X(64).
           03  WRK-PATH-PATHNAME   PIC X(64).
           03  WRK-N31IDMSG        PIC X(70).
      *
           03  WRK-LPUBCD          PIC X(05).
           03  WRK-RENNUM          PIC 9(02).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-KEY-AREA.
           03  WRK-ADRS-KEY.
               05  WRK-ADRS-POST           PIC X(07).
               05  WRK-ADRS-LPUBCD         PIC X(05).
               05  WRK-ADRS-EDANUM         PIC 9(02).
      *
           03  WRK-USER-KEY.
               05  WRK-USER-POST           PIC X(07).
               05  WRK-USER-LPUBCD         PIC X(05).
               05  WRK-USER-EDANUM         PIC 9(02).
      *
      *
       01  WRK-ADRS-AREA.
           03  WRK-ADRS-G.
               05  WRK-ADRS1           PIC X(32)   VALUE
                   "01ホッカイドウ        北海道    ".
               05  WRK-ADRS2           PIC X(32)   VALUE
                   "02アオモリケン        青森県    ".
               05  WRK-ADRS3           PIC X(32)   VALUE
                   "03イワテケン          岩手県    ".
               05  WRK-ADRS4           PIC X(32)   VALUE
                   "04ミヤギケン          宮城県    ".
               05  WRK-ADRS5           PIC X(32)   VALUE
                   "05アキタケン          秋田県    ".
               05  WRK-ADRS6           PIC X(32)   VALUE
                   "06ヤマガタケン        山形県    ".
               05  WRK-ADRS7           PIC X(32)   VALUE
                   "07フクシマケン        福島県    ".
               05  WRK-ADRS8           PIC X(32)   VALUE
                   "08イバラキケン        茨城県    ".
               05  WRK-ADRS9           PIC X(32)   VALUE
                   "09トチギケン          栃木県    ".
               05  WRK-ADRS10          PIC X(32)   VALUE
                   "10グンマケン          群馬県    ".
               05  WRK-ADRS11          PIC X(32)   VALUE
                   "11サイタマケン        埼玉県    ".
               05  WRK-ADRS12          PIC X(32)   VALUE
                   "12チバケン            千葉県    ".
               05  WRK-ADRS13          PIC X(32)   VALUE
                   "13トウキョウト        東京都    ".
               05  WRK-ADRS14          PIC X(32)   VALUE
                   "14カナガワケン        神奈川県  ".
               05  WRK-ADRS15          PIC X(32)   VALUE
                   "15ニイガタケン        新潟県    ".
               05  WRK-ADRS16          PIC X(32)   VALUE
                   "16トヤマケン          富山県    ".
               05  WRK-ADRS17          PIC X(32)   VALUE
                   "17イシカワケン        石川県    ".
               05  WRK-ADRS18          PIC X(32)   VALUE
                   "18フクイケン          福井県    ".
               05  WRK-ADRS19          PIC X(32)   VALUE
                   "19ヤマナシケン        山梨県    ".
               05  WRK-ADRS20          PIC X(32)   VALUE
                   "20ナガノケン          長野県    ".
               05  WRK-ADRS21          PIC X(32)   VALUE
                   "21ギフケン            岐阜県    ".
               05  WRK-ADRS22          PIC X(32)   VALUE
                   "22シズオカケン        静岡県    ".
               05  WRK-ADRS23          PIC X(32)   VALUE
                   "23アイチケン          愛知県    ".
               05  WRK-ADRS24          PIC X(32)   VALUE
                   "24ミエケン            三重県    ".
               05  WRK-ADRS25          PIC X(32)   VALUE
                   "25シガケン            滋賀県    ".
               05  WRK-ADRS26          PIC X(32)   VALUE
                   "26キョウトフ          京都府    ".
               05  WRK-ADRS27          PIC X(32)   VALUE
                   "27オオサカフ          大阪府    ".
               05  WRK-ADRS28          PIC X(32)   VALUE
                   "28ヒョウゴケン        兵庫県    ".
               05  WRK-ADRS29          PIC X(32)   VALUE
                   "29ナラケン            奈良県    ".
               05  WRK-ADRS30          PIC X(32)   VALUE
                   "30ワカヤマケン        和歌山県  ".
               05  WRK-ADRS31          PIC X(32)   VALUE
                   "31トットリケン        鳥取県    ".
               05  WRK-ADRS32          PIC X(32)   VALUE
                   "32シマネケン          島根県    ".
               05  WRK-ADRS33          PIC X(32)   VALUE
                   "33オカヤマケン        岡山県    ".
               05  WRK-ADRS34          PIC X(32)   VALUE
                   "34ヒロシマケン        広島県    ".
               05  WRK-ADRS35          PIC X(32)   VALUE
                   "35ヤマグチケン        山口県    ".
               05  WRK-ADRS36          PIC X(32)   VALUE
                   "36トクシマケン        徳島県    ".
               05  WRK-ADRS37          PIC X(32)   VALUE
                   "37カガワケン          香川県    ".
               05  WRK-ADRS38          PIC X(32)   VALUE
                   "38エヒメケン          愛媛県    ".
               05  WRK-ADRS39          PIC X(32)   VALUE
                   "39コウチケン          高知県    ".
               05  WRK-ADRS40          PIC X(32)   VALUE
                   "40フクオカケン        福岡県    ".
               05  WRK-ADRS41          PIC X(32)   VALUE
                   "41サガケン            佐賀県    ".
               05  WRK-ADRS42          PIC X(32)   VALUE
                   "42ナガサキケン        長崎県    ".
               05  WRK-ADRS43          PIC X(32)   VALUE
                   "43クマモトケン        熊本県    ".
               05  WRK-ADRS44          PIC X(32)   VALUE
                   "44オオイタケン        大分県    ".
               05  WRK-ADRS45          PIC X(32)   VALUE
                   "45ミヤザキケン        宮崎県    ".
               05  WRK-ADRS46          PIC X(32)   VALUE
                   "46カゴシマケン        鹿児島県  ".
               05  WRK-ADRS47          PIC X(32)   VALUE
                   "47オキナワケン        沖縄県    ".
           03  WRK-ADRS-R          REDEFINES   WRK-ADRS-G.
               05  WRK-ADRS-TBL        OCCURS  47.
                   07  WRK-ADRSCD          PIC X(02).
                   07  WRK-ADRSKANA        PIC X(20).
                   07  WRK-ADRSNAME        PIC X(10).
      *
      *
       01  CONS-AREA.
           03  CONS-LINE-MAX               PIC 9(03)   VALUE   200.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理基本情報
           COPY    "CPSK1001.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    住所マスタ（ユーザ）
       01  USER-ADRS-REC.
           COPY    "CPADRS.INC"    REPLACING
                                       //ADRS-// BY //USER-ADRS-//.
      *    住所マスタ（ワーク）
       01  HEN-ADRS-REC.
           COPY    "CPADRS.INC"    REPLACING
                                       //ADRS-// BY //HEN-ADRS-//.
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *    LIKE使用KEY対応
           COPY    "CPORCSADDSIGN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
      *orca104
           COPY    "V01.INC".
           COPY    "V02.INC".
           COPY    "V03.INC".
           COPY    "VERR.INC".
           COPY    "VID1.INC".
      *orca105
           COPY    "N01.INC".
           COPY    "NERR.INC".
           COPY    "NID1.INC".
      *orca106
           COPY    "N11.INC".
           COPY    "N11ERR.INC".
           COPY    "N11ID1.INC".
      *orca108
           COPY    "N31.INC".
           COPY    "N31ERR.INC".
           COPY    "N31ID1.INC".
      *orca109
           COPY    "N41.INC".
           COPY    "N41ERR.INC".
           COPY    "N41ID1.INC".
      *
       PROCEDURE               DIVISION    USING
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
           INITIALIZE                  WRK-KEY-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-N31KYOUTU
           MOVE    SPA-FREE        TO  SPA-N31SCRAREA
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
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
           MOVE    SPA-N31SCRAREA  TO  SPA-FREE
           MOVE    SPA-N31KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "N31ERR"
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
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "N31"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
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
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "N31ID1"
                   PERFORM 330-NID1-SET-SEC
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＮＩＤ１）ＯＫ処理
      *****************************************************************
       330-NID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-N31IDCD
      *        削除処理
               WHEN    "1002"
                   IF      SPA-N31ID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   END-IF
               WHEN    "1001"
      *        更新処理
                   IF      SPA-N31ID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-N31ID1-FLG
           MOVE    SPACE           TO  SPA-N31IDCD
           .
       330-NID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-N31-AREA
                                       SPA-N31KYOUTU
      *
      *    都道府県ＣＤコンボセット
           PERFORM VARYING IDXC    FROM    1   BY      1
                   UNTIL   IDXC    >       47
               MOVE    WRK-ADRSCD(IDXC)    TO  SPA-GMN-PREFCDL(IDXC)
               MOVE    WRK-ADRSNAME(IDXC)  TO  SPA-GMN-PREFMEIL(IDXC)
           END-PERFORM
           MOVE    47              TO  SPA-GMN-PREFCD-MAX
      *    システム管理基本情報読み込み
           PERFORM 900-SYS1001-READ-SEC
      *    都道府県コード初期値セット
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SYS-1001-PREFNUM        TO  SPA-NAI-PREFNUM
           ELSE
               MOVE    SPACE                   TO  SPA-NAI-PREFNUM
           END-IF
      *
           PERFORM 320-SPASET-PREFCD-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県初期編集処理
      *****************************************************************
       320-SPASET-PREFCD-SEC       SECTION.
      *
           IF      SPA-NAI-PREFNUM     =   SPACE
               MOVE    SPACE               TO  SPA-GMN-PREFCD-G
               MOVE    SPACE               TO  SPA-GMN-PREFKANA
               MOVE    SPACE               TO  SPA-GMN-PREFNAME
           ELSE
               MOVE    SPA-NAI-PREFNUM     TO  SPA-GMN-PREFCD
      *        都道府県編集
               PERFORM 3201-PREFCD-HEN-SEC
           END-IF
           . 
       32O-SPASET-PREFCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県編集処理
      *****************************************************************
       3201-PREFCD-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  SPA-GMN-PREFKANA
           MOVE    SPACE               TO  SPA-GMN-PREFNAME
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL  (IDXC        >   SPA-GMN-PREFCD-MAX )
                       OR (FLG-OK      =   1   )
               IF      SPA-GMN-PREFCD      =   SPA-GMN-PREFCDL(IDXC)
                   MOVE    SPA-GMN-PREFCD-LIST(IDXC)
                                               TO  SPA-GMN-PREFCD-G
                   MOVE    WRK-ADRSKANA(IDXC)  TO  SPA-GMN-PREFKANA
                   MOVE    WRK-ADRSNAME(IDXC)  TO  SPA-GMN-PREFNAME
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           . 
       3201-PREFCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 403-DEL-SEC
      *        前ページ
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 404-BACKPAGE-SEC
      *        次ページ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 405-NEXTPAGE-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "POST"
      *            郵便番号入力
                   PERFORM 4100-POST-SET-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
      *            選択番号処理
                   PERFORM 4100-SELNUM-SET-SEC
               WHEN    ANY             ALSO    "CYOIKILST"
      *            町域リストチェック
                   PERFORM 4100-CYOIKILST-CHK-SEC
      *           入力チェック処理
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
      *    登録時、必須入力チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               IF      FLG-TOUROKU         =   1
                   PERFORM 4109-TOUROKU-CHK-SEC
               END-IF
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
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    N31-PREFCD      TO  SPA-GMN-PREFCD-G
      *    MOVE    N31-POST        TO  SPA-GMN-POST
      *    MOVE    N31-SELNUM      TO  SPA-GMN-SELNUM
      **   MOVE    N31-PREFKANA    TO  SPA-GMN-PREFKANA
      **   MOVE    N31-PREFNAME    TO  SPA-GMN-PREFNAME
      *
           MOVE    N31-CITYKANA    TO  SPA-GMN-CITYKANA
           MOVE    N31-TOWNKANA    TO  SPA-GMN-TOWNKANA
           MOVE    N31-CITYNAME    TO  SPA-GMN-CITYNAME
           MOVE    N31-TOWNNAME    TO  SPA-GMN-TOWNNAME
           MOVE    N31-TOWNNAME2   TO  SPA-GMN-TOWNNAME2
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    都道府県番号チェック
           PERFORM 4100-PREFCD-CHK-SEC
      *    市町村名カナチェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-CITYKANA-CHK-SEC
           END-IF
      *    町域名カナチェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-TOWNKANA-CHK-SEC
           END-IF
      *    市町村名チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-CITYNAME-CHK-SEC
           END-IF
      *    町域名チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-TOWNNAME-CHK-SEC
           END-IF
      *    通り名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-TOWNNAME2-CHK-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    郵便番号入力
      *****************************************************************
       4100-POST-SET-SEC           SECTION.
      *
           MOVE    N31-POST        TO  SPA-GMN-POST
      *
           MOVE    ZERO            TO  FLG-TOUROKU
      *    郵便番号チェック
           PERFORM 4100-POST-CHK-SEC
           IF    SPA-ERRCD         NOT =   SPACE
               GO  TO  4100-POST-SET-EXT
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           INITIALIZE                  SPA-GMN-INPUT-G
      *
      *    都道府県番号初期値セット
           PERFORM 320-SPASET-PREFCD-SEC
      *
           MOVE    1               TO  SPA-NAI-PAGE
           MOVE    ZERO            TO  SPA-NAI-NEXTKBN
      *
      *    該当町域リストセット
           PERFORM 4100-CYOIKILST-SET-SEC
           .
      *
       4100-POST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     郵便番号チェック
      *****************************************************************
       4100-POST-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-POST    =   SPACE )
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO  TO  4100-POST-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-POST        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-POST
           IF    ( KANACHK-RC      NOT =   ZERO )  OR
                 ( KANACHK-RC2     NOT =   ZERO )  OR
                 ( KANACHK-SYUBETU     =   2    )
      *        半角で間に空白がないこと
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
      *        すべて数値であること
               IF     (FLG-TOUROKU         =   1   )
                   IF    (KANACHK-MAX          =   7   )   AND
                         (SPA-GMN-POST(1:KANACHK-MAX) NUMERIC )
                       CONTINUE
                   ELSE
                       MOVE    "0011"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
      *
       4100-POST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域リストセット
      *****************************************************************
       4100-CYOIKILST-SET-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOIKI-G
           MOVE    ZERO            TO  IDX
           MOVE    ZERO            TO  IDX2
      *
           COMPUTE IDX-STR         =   (SPA-NAI-PAGE   -   1 )
                                   *   CONS-LINE-MAX
                                   +   1
           MOVE    ZERO            TO  SPA-NAI-NEXTKBN
      *
           INITIALIZE                  ADRS-REC
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    SPA-GMN-POST    TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING
                                       ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR    TO  ADRS-POST
           MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
      *    ユーザ検索
           MOVE    ADRS-REC        TO  USER-ADRS-REC
      *
           MOVE    ADRS-REC        TO  MCPDATA-REC
           MOVE    "tbl_adrs_master"   TO  WRK-PATH-TABLE
           MOVE    "key2"          TO  WRK-PATH-PATHNAME
           PERFORM 900-ADRS-SELECT-SEC
      *    標準キー編集
           PERFORM 41009-ADRS-KEY-SEC
      *
      *    ユーザ検索
           MOVE    USER-ADRS-REC   TO  MCPDATA-REC
           PERFORM 902-USER-ADRS-SELECT-SEC
      *
           MOVE    ZERO            TO  FLG-SYORI-END
      *
           IF     (FLG-ADRS           =   1  )
             AND  (FLG-USERADRS       =   1  )
               MOVE    1              TO  FLG-SYORI-END
           END-IF
      *
           PERFORM 41002-ADRS-HENSYU-SEC
                   UNTIL      (FLG-SYORI-END   =   1  )
                          OR  (SPA-NAI-NEXTKBN =   1  )
      *    CLOSE
           PERFORM 900-ADRS-CLOSE-SEC
      *
           MOVE    "tbl_adrs_user"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
      *    該当する町域がないときは新規登録
           EVALUATE    SPA-GMN-CYOIKI-MAX
             WHEN      ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
             WHEN      OTHER
               MOVE    2               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           MOVE    SPA-GMN-POST        TO  SPA-NAI-POST
           MOVE    SPA-NAI-PAGE        TO  SPA-MAE-PAGE
           .
       4100-CYOIKILST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準キー編集
      *****************************************************************
       41009-ADRS-KEY-SEC      SECTION.
      *
           IF      FLG-ADRS        =   ZERO
               MOVE    ADRS-POST       TO  WRK-ADRS-POST
               MOVE    ADRS-LPUBCD     TO  WRK-ADRS-LPUBCD
               MOVE    ADRS-EDANUM     TO  WRK-ADRS-EDANUM
           ELSE
               MOVE    ALL "9"         TO  WRK-ADRS-KEY
           END-IF
           .
       41009-ADRS-KEYEXT.
           EXIT.
      *
      *****************************************************************
      *    町域リストセット
      *****************************************************************
       41002-ADRS-HENSYU-SEC      SECTION.
      *
           EVALUATE    TRUE
               WHEN    WRK-ADRS-KEY        =   WRK-USER-KEY
                   PERFORM 4100021-MAST-ADRS-SEC
                   IF      SPA-NAI-NEXTKBN     =   ZERO
                       PERFORM 4100022-USER-ADRS-SEC
                       PERFORM 900-USERADRS-FETCH-SEC
                   END-IF
                   PERFORM 900-ADRS-FETCH-SEC
      *            標準キー編集
                   PERFORM 41009-ADRS-KEY-SEC
               WHEN    WRK-ADRS-KEY       <   WRK-USER-KEY
      *            標準のみ
                   PERFORM 4100021-MAST-ADRS-SEC
                   PERFORM 900-ADRS-FETCH-SEC
      *            標準キー編集
                   PERFORM 41009-ADRS-KEY-SEC
               WHEN    WRK-ADRS-KEY        >   WRK-USER-KEY
      *            ユーザのみ
                   PERFORM 4100022-USER-ADRS-SEC
                   PERFORM 900-USERADRS-FETCH-SEC
           END-EVALUATE
      *
           IF     (FLG-ADRS           =   1  )
             AND  (FLG-USERADRS       =   1  )
               MOVE    1              TO  FLG-SYORI-END
           END-IF
           .
       41002-ADRS-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準　住所編集処理
      *****************************************************************
       4100021-MAST-ADRS-SEC      SECTION.
      *
           ADD     1               TO  IDX2
           IF      IDX-STR         <=  IDX2
               ADD     1               TO  IDX
               IF      IDX             >   CONS-LINE-MAX
                   MOVE    1               TO  SPA-NAI-NEXTKBN
               ELSE
                   MOVE    ADRS-REC        TO  HEN-ADRS-REC
                   PERFORM 41001-CYOIKILST-HEN-SEC
                   MOVE    "1"         TO  SPA-NAI-TDATAKBN
                                                          (IDX)
               END-IF
           END-IF
           .
       4100021-MAST-ADRS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ　住所編集処理
      *****************************************************************
       4100022-USER-ADRS-SEC      SECTION.
      *
           ADD     1               TO  IDX2
           IF      IDX-STR         <=  IDX2
               ADD     1               TO  IDX
               IF      IDX             >   CONS-LINE-MAX
                   MOVE    1               TO  SPA-NAI-NEXTKBN
               ELSE
                   MOVE    USER-ADRS-REC   TO  HEN-ADRS-REC
                   PERFORM 41001-CYOIKILST-HEN-SEC
                   MOVE    "2"             TO  SPA-NAI-TDATAKBN
                                                          (IDX)
               END-IF
           END-IF
           .
       4100022-USER-ADRS-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域リストセット
      *****************************************************************
       41001-CYOIKILST-HEN-SEC      SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN-CYOIKI-MAX
           MOVE    IDX                 TO  SPA-GMN-CYO-CD    (IDX)
           MOVE    HEN-ADRS-POST           TO  SPA-GMN-CYO-POST  (IDX)
           MOVE    HEN-ADRS-EDITADRS-NAME  TO  SPA-GMN-CYO-ADRS  (IDX)
           MOVE    HEN-ADRS-LPUBCD         TO  SPA-GMN-CYO-LPUBCD(IDX)
           MOVE    HEN-ADRS-EDANUM         TO  SPA-GMN-CYO-EDANUM(IDX)
           MOVE    HEN-ADRS-TOWNNAME2      TO  SPA-GMN-CYO-TOWNNAME2
                                                              (IDX)
           IF      HEN-ADRS-LPUBCD         =   "00000"
               MOVE    "ユ"            TO  SPA-GMN-CYO-HEN(IDX)
           END-IF
      *
           .
       41001-CYOIKILST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    町域リスト選択
      *****************************************************************
       4100-CYOIKILST-CHK-SEC      SECTION.
      *
      *    行選択
           PERFORM VARYING     IDXL    FROM    1   BY  1
                   UNTIL       IDXL    >       CONS-LINE-MAX
               IF    N31-CYOIKI-SELECT(IDXL)   =   "T"
                   MOVE    IDXL            TO  SPA-GMN-SELNUM
                   MOVE    CONS-LINE-MAX   TO  IDXL
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-SELNUM      TO  N31-SELNUM
      *    選択番号入力
           PERFORM 4100-SELNUM-SET-SEC
      *
           .
      *
       4100-CYOIKILST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力
      *****************************************************************
       4100-SELNUM-SET-SEC         SECTION.
      *
           MOVE    N31-SELNUM      TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               MOVE    SPACE               TO  SPA-NAI-DATAKBN
               GO  TO  4100-SELNUM-SET-EXT
           END-IF
      *
      *    選択番号チェック
           IF    ( SPA-GMN-SELNUM      >   SPA-GMN-CYOIKI-MAX )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-SELNUM-SET-EXT
           END-IF
      *
           INITIALIZE                  ADRS-REC
           MOVE    SPA-GMN-CYO-LPUBCD(SPA-GMN-SELNUM)
                                   TO  ADRS-LPUBCD
           MOVE    SPA-GMN-CYO-POST(SPA-GMN-SELNUM)
                                   TO  ADRS-POST
           MOVE    SPA-GMN-CYO-EDANUM(SPA-GMN-SELNUM)
                                   TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
      *H29.9
      *    検索対象判定
           IF      SPA-NAI-TDATAKBN(SPA-GMN-SELNUM)
                                       =   "1"
               MOVE    "tbl_adrs_master"   TO  WRK-PATH-TABLE
           ELSE
               MOVE    "tbl_adrs_user"     TO  WRK-PATH-TABLE
           END-IF
           MOVE    ADRS-REC        TO  MCPDATA-REC
           MOVE    "key"           TO  WRK-PATH-PATHNAME
           PERFORM 900-ADRS-SELECT-SEC
      *
           PERFORM 900-ADRS-CLOSE-SEC
      *
           IF      FLG-ADRS        NOT =   ZERO
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  4100-SELNUM-SET-EXT
           END-IF
      *
           MOVE    ADRS-POST           TO  SPA-GMN-POST
           MOVE    ADRS-CITYKANA       TO  SPA-GMN-CITYKANA
           MOVE    ADRS-TOWNKANA       TO  SPA-GMN-TOWNKANA
           MOVE    ADRS-CITYNAME       TO  SPA-GMN-CITYNAME
           MOVE    ADRS-TOWNNAME       TO  SPA-GMN-TOWNNAME
           MOVE    ADRS-TOWNNAME2      TO  SPA-GMN-TOWNNAME2
           MOVE    SPA-NAI-TDATAKBN(SPA-GMN-SELNUM)
                                       TO  SPA-NAI-DATAKBN
      *    都道府県編集
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL   IDXC    >       SPA-GMN-PREFCD-MAX
               IF      ADRS-PREFNAME   =   SPA-GMN-PREFMEIL(IDXC)
                   MOVE    SPA-GMN-PREFCD-LIST(IDXC)
                                               TO  SPA-GMN-PREFCD-G
                   MOVE    WRK-ADRSKANA(IDXC)  TO  SPA-GMN-PREFKANA
                   MOVE    WRK-ADRSNAME(IDXC)  TO  SPA-GMN-PREFNAME
                   MOVE    SPA-GMN-PREFCD-MAX  TO  IDXC
               END-IF
           END-PERFORM
      *
      *    町域リストクリア
           INITIALIZE                  SPA-GMN-CYOIKI-G
           MOVE    1                   TO  SPA-GMN-CYOIKI-MAX
           MOVE    1                   TO  SPA-GMN-CYO-CD(1)
           MOVE    ADRS-POST           TO  SPA-GMN-CYO-POST(1)
           MOVE    ADRS-EDITADRS-NAME  TO  SPA-GMN-CYO-ADRS(1)
           MOVE    ADRS-LPUBCD         TO  SPA-GMN-CYO-LPUBCD(1)
           MOVE    ADRS-EDANUM         TO  SPA-GMN-CYO-EDANUM(1)
           MOVE    ADRS-TOWNNAME2      TO  SPA-GMN-CYO-TOWNNAME2(1)
           IF      ADRS-LPUBCD         =   "00000"
               MOVE    "ユ"            TO  SPA-GMN-CYO-HEN(1)
           END-IF
           MOVE    SPA-NAI-DATAKBN     TO  SPA-NAI-TDATAKBN(1)
      *
           MOVE    1               TO  SPA-NAI-PAGE
           MOVE    ZERO            TO  SPA-NAI-NEXTKBN
      *
           MOVE    1               TO  SPA-GMN-SELNUM
      *
           MOVE    4               TO  SPA-GMN-CUR
           .
       4100-SELNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県番号チェック
      *****************************************************************
       4100-PREFCD-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-PREFCD      =   SPACE )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
      *
      *    都道府県編集・チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3201-PREFCD-HEN-SEC
               IF      FLG-OK          =   ZERO
                   MOVE    "0013"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4100-PREFCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    市町村名カナチェック
      *****************************************************************
       4100-CITYKANA-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-CITYKANA    =   SPACE )
               CONTINUE
           ELSE
      *        全角チェック
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-CITYKANA    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-CITYKANA
               IF    ( KANACHK-RC      NOT =   ZERO )  OR
                     ( KANACHK-RC2         =   7    )  OR
                     ( KANACHK-SYUBETU     =   1    )
      *            全角半角混在エラー
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4100-CITYKANA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域名カナチェック
      *****************************************************************
       4100-TOWNKANA-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-TOWNKANA    =   SPACE )
               CONTINUE
           ELSE
      *
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-TOWNKANA    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TOWNKANA
               IF    ( KANACHK-RC      NOT =   ZERO )  OR
                     ( KANACHK-RC2         =   7    )  OR
                     ( KANACHK-SYUBETU     =   1    )
      *            全角半角混在エラー
                   MOVE    "0005"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4100-TOWNKANA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    市町村名チェック
      *****************************************************************
       4100-CITYNAME-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-CITYNAME    =   SPACE )
               CONTINUE
           ELSE
      *
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-CITYNAME    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-CITYNAME
               IF    ( KANACHK-RC      NOT =   ZERO )  OR
                     ( KANACHK-RC2         =   7    )  OR
                     ( KANACHK-SYUBETU     =   1    )
      *            全角半角混在エラー
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    6               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4100-CITYNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域名チェック
      *****************************************************************
       4100-TOWNNAME-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-TOWNNAME    =   SPACE )
               CONTINUE
           ELSE
      *
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-TOWNNAME    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TOWNNAME
               IF    ( KANACHK-RC      NOT =   ZERO )  OR
                     ( KANACHK-RC2         =   7    )  OR
                     ( KANACHK-SYUBETU     =   1    )
      *            全角半角混在エラー
                   MOVE    "0007"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4100-TOWNNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域名チェック
      *****************************************************************
       4100-TOWNNAME2-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-TOWNNAME    =   SPACE )
               CONTINUE
           ELSE
      *
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-TOWNNAME2   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TOWNNAME2
               IF    ( KANACHK-RC      NOT =   ZERO )  OR
                     ( KANACHK-RC2         =   7    )  OR
                     ( KANACHK-SYUBETU     =   1    )
      *            全角半角混在エラー
                   MOVE    "0008"          TO  SPA-ERRCD
                   MOVE    8               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4100-TOWNNAME2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック
      *****************************************************************
       4109-TOUROKU-CHK-SEC        SECTION.
      *
      *    郵便番号チェック
           PERFORM 4100-POST-CHK-SEC
      *
           IF     (SPA-ERRCD       NOT =   SPACE)
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    標準はユーザに登録済みであれば、更新なし
           IF      SPA-NAI-DATAKBN     =   "1"
               CONTINUE
           ELSE
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           INITIALIZE                  ADRS-REC
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    SPA-GMN-POST        TO  ADRS-POST
           MOVE    SPA-GMN-CYO-LPUBCD(1)
                                       TO  ADRS-LPUBCD
           MOVE    SPA-GMN-CYO-EDANUM(1)
                                       TO  ADRS-EDANUM
           MOVE    ADRS-REC            TO  MCPDATA-REC
      *    検索対象判定
           MOVE    "tbl_adrs_user"     TO  WRK-PATH-TABLE
           MOVE    "key"               TO  WRK-PATH-PATHNAME
           PERFORM 900-ADRS-SELECT-SEC
      *
           PERFORM 900-ADRS-CLOSE-SEC
      *
           IF      FLG-ADRS            =   ZERO
      *        ユーザで登録があればエラー
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    98                  TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           INITIALIZE                  ADRS-REC
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    SPA-GMN-POST        TO  ADRS-POST
           MOVE    SPA-GMN-CYO-LPUBCD(1)
                                       TO  ADRS-LPUBCD
           MOVE    SPA-GMN-CYO-EDANUM(1)
                                       TO  ADRS-EDANUM
           MOVE    ADRS-REC            TO  MCPDATA-REC
      *    検索対象判定
           MOVE    "tbl_adrs_master"   TO  WRK-PATH-TABLE
           MOVE    "key"               TO  WRK-PATH-PATHNAME
           PERFORM 900-ADRS-SELECT-SEC
      *
           PERFORM 900-ADRS-CLOSE-SEC
           IF      FLG-ADRS            =   ZERO
      *        変更があること
                IF      (ADRS-CITYKANA     =   SPA-GMN-CITYKANA)
                   AND  (ADRS-TOWNKANA     =   SPA-GMN-TOWNKANA)
                   AND  (ADRS-CITYNAME     =   SPA-GMN-CITYNAME)
                   AND  (ADRS-TOWNNAME     =   SPA-GMN-TOWNNAME)
                   AND  (ADRS-TOWNNAME2    =   SPA-GMN-TOWNNAME2)
                   MOVE    "0016"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M02"           TO  SPA-SAKIPG
           MOVE    "N31"           TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"         TO  MCP-PUTTYPE
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
      *    削除確認画面表示処理
      *****************************************************************
       402-CLEAR-SEC               SECTION.
      *
           IF     (SPA-NAI-POST    NOT =   SPACE )  AND
                  (SPA-GMN-SELNUM      >   ZERO  )
      *        選択前へ戻る
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               INITIALIZE                      SPA-GMN-INPUT-G
               MOVE    SPA-NAI-POST        TO  SPA-GMN-POST
               MOVE    SPA-MAE-PAGE        TO  SPA-NAI-PAGE
               PERFORM 4100-CYOIKILST-SET-SEC
           ELSE
               PERFORM 310-SPASET-SEC
           END-IF
           .
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                 SECTION.
      *
           PERFORM 4100-POST-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  403-DEL-EXT
           END-IF
      *
           IF    ( SPA-GMN-SELNUM  =   ZERO )  OR
                 ( SPA-GMN-SELNUM  >   SPA-GMN-CYOIKI-MAX )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               IF     (SPA-NAI-TDATAKBN (SPA-GMN-SELNUM)
                                       =   "1"    )
                   MOVE    "0014"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1002"          TO  SPA-N31IDCD
           END-IF
           .
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                SECTION.
      *
           MOVE    SPACE           TO  ADRS-REC
           INITIALIZE                  ADRS-REC
           MOVE    SPA-GMN-CYO-LPUBCD(SPA-GMN-SELNUM)
                                   TO  ADRS-LPUBCD
           MOVE    SPA-GMN-POST    TO  ADRS-POST
           MOVE    SPA-GMN-CYO-EDANUM(SPA-GMN-SELNUM)
                                   TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
     ****  MOVE    "tbl_adrs"      TO  MCP-TABLE
           MOVE    "tbl_adrs_user" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "N31 ADRS DEL ERR:"  MCP-RC
                        ",KEY:"  ADRS-KEY
               MOVE    "1003"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               PERFORM 310-SPASET-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前ページ表示処理
      *****************************************************************
       404-BACKPAGE-SEC            SECTION.
      *
           IF      SPA-NAI-PAGE        >   1
      *        前頁
               COMPUTE SPA-NAI-PAGE    =   SPA-NAI-PAGE    -   1
               PERFORM 4100-CYOIKILST-SET-SEC
           END-IF
      *
           .
       404-BACKPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次ページ表示処理
      *****************************************************************
       405-NEXTPAGE-SEC            SECTION.
      *
           IF      SPA-NAI-NEXTKBN     =   1
      *        次頁
               COMPUTE SPA-NAI-PAGE    =   SPA-NAI-PAGE    +   1
               PERFORM 4100-CYOIKILST-SET-SEC
           END-IF
           .
       405-NEXTPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD       =   SPACE
               MOVE    "1001"          TO  SPA-N31IDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-RENNUM
           MOVE    SPACE           TO  WRK-LPUBCD
      *
      *    連番と地方公共団体コードセット
           IF      SPA-GMN-SELNUM  =   ZERO
               MOVE    "00000"         TO  WRK-LPUBCD
      *
               MOVE    SPACE           TO  ADRS-REC
               INITIALIZE                  ADRS-REC
               MOVE    SPA-GMN-POST    TO  ADRS-POST
               MOVE    WRK-LPUBCD      TO  ADRS-LPUBCD
               MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
               MOVE    ADRS-REC        TO  MCPDATA-REC
      *****    MOVE    "tbl_adrs"      TO  WRK-PATH-TABLE
               MOVE    "tbl_adrs_user" TO  WRK-PATH-TABLE
               MOVE    "key7"          TO  WRK-PATH-PATHNAME
               PERFORM 900-ADRS-SELECT-SEC
               PERFORM 900-ADRS-CLOSE-SEC
      *
               IF      FLG-ADRS        =   ZERO
                   IF      ADRS-EDANUM     >=  99
                       MOVE    "0012"          TO  SPA-ERRCD
                   ELSE
                       COMPUTE WRK-RENNUM  =   ADRS-EDANUM +   1
                   END-IF
               ELSE
                   COMPUTE WRK-RENNUM  =   ADRS-EDANUM +   1
               END-IF
           ELSE
               MOVE    SPA-GMN-CYO-EDANUM(SPA-GMN-SELNUM)
                                       TO  WRK-RENNUM
               MOVE    SPA-GMN-CYO-LPUBCD(SPA-GMN-SELNUM)
                                       TO  WRK-LPUBCD
           END-IF
      *
      *    追加処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 490-INSERT-SEC
           END-IF
      *
      *    更新後セット
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-KOUSHINGO-SET-SEC
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       490-INSERT-SEC              SECTION.
      *
           MOVE    SPACE           TO  ADRS-REC
           INITIALIZE                  ADRS-REC
           MOVE    WRK-LPUBCD      TO  ADRS-LPUBCD
           MOVE    SPA-GMN-POST    TO  ADRS-POST
           MOVE    WRK-RENNUM      TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
      *
           MOVE    ADRS-REC        TO  MCPDATA-REC
      **** MOVE    "tbl_adrs"      TO  WRK-PATH-TABLE
           MOVE    "tbl_adrs_user" TO  WRK-PATH-TABLE
           MOVE    "key"           TO  WRK-PATH-PATHNAME
           PERFORM 900-ADRS-SELECT-SEC
           PERFORM 900-ADRS-CLOSE-SEC
      *
      *    該当データがあるときは更新、ないときは追加
           IF      FLG-ADRS        =   ZERO
      *        更新処理
               PERFORM 4901-ADRS-UPDATE-SEC
           ELSE
      *        追加処理
               PERFORM 4902-ADRS-INSERT-SEC
           END-IF
           .
       490-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所更新処理
      *****************************************************************
       4901-ADRS-UPDATE-SEC      SECTION.
      *
           PERFORM 4902-ADRS-HENSYU-SEC
      *
           MOVE    ADRS-REC        TO  MCPDATA-REC
      **** MOVE    "tbl_adrs"      TO  MCP-TABLE
           MOVE    "tbl_adrs_user" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              NOT =   ZERO
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "N31 ADRS UPD ERR:"  MCP-RC
                    ",KEY:"  ADRS-KEY
           END-IF
           .
       4901-ADRS-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    住所更新処理
      *****************************************************************
       4902-ADRS-INSERT-SEC      SECTION.
      *
           MOVE    SPACE           TO  ADRS-REC
           INITIALIZE                  ADRS-REC
           MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
           MOVE    WRK-LPUBCD      TO  ADRS-LPUBCD
           MOVE    SPA-GMN-POST    TO  ADRS-POST
           MOVE    WRK-RENNUM      TO  ADRS-EDANUM
      *
           PERFORM 4902-ADRS-HENSYU-SEC
      *
           MOVE    ADRS-REC        TO  MCPDATA-REC
     ***** MOVE    "tbl_adrs"      TO  MCP-TABLE
           MOVE    "tbl_adrs_user" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              NOT =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "N31 ADRS INS ERR:"  MCP-RC
                    ",KEY:"  ADRS-KEY
           END-IF
           .
       4902-ADRS-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所編集処理
      *****************************************************************
       4902-ADRS-HENSYU-SEC      SECTION.
      *
           MOVE    SPA-GMN-PREFKANA    TO  ADRS-PREFKANA
           MOVE    SPA-GMN-CITYKANA    TO  ADRS-CITYKANA
           MOVE    SPA-GMN-TOWNKANA    TO  ADRS-TOWNKANA
           MOVE    SPA-GMN-PREFNAME    TO  ADRS-PREFNAME
           MOVE    SPA-GMN-CITYNAME    TO  ADRS-CITYNAME
           MOVE    SPA-GMN-TOWNNAME    TO  ADRS-TOWNNAME
           MOVE    SPA-GMN-TOWNNAME2   TO  ADRS-TOWNNAME2
      *
      *    編集カナ住所
           MOVE    SPACE           TO  ADRS-EDITADRS-KANA
           STRING  ADRS-PREFKANA       DELIMITED  BY  SPACE
                   ADRS-CITYKANA       DELIMITED  BY  SPACE
                   ADRS-TOWNKANA       DELIMITED  BY  SPACE
                                       INTO    ADRS-EDITADRS-KANA
           END-STRING
      *
      *    編集住所
           MOVE    SPACE           TO  ADRS-EDITADRS-NAME
           STRING  ADRS-PREFNAME       DELIMITED  BY  SPACE
                   ADRS-CITYNAME       DELIMITED  BY  SPACE
                   ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                       INTO    ADRS-EDITADRS-NAME
           END-STRING
           .
       4902-ADRS-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新後セット
      *****************************************************************
       4100-KOUSHINGO-SET-SEC      SECTION.
      *
           MOVE    1               TO  SPA-NAI-PAGE
           IF     (SPA-NAI-POST    NOT =   SPACE )
              AND (SPA-GMN-SELNUM      >   ZERO  )
               MOVE    SPA-MAE-PAGE        TO  SPA-NAI-PAGE
           END-IF
           MOVE    SPA-NAI-POST    TO  SPA-GMN-POST
           MOVE    SPACE           TO  SPA-NAI-POST
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           INITIALIZE                  SPA-GMN-INPUT-G
      *
      *    都道府県番号初期値セット
           PERFORM 320-SPASET-PREFCD-SEC
      *
      *    該当町域リストセット
           PERFORM 4100-CYOIKILST-SET-SEC
           .
       4100-KOUSHINGO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    エラー
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *    メッセージ
           IF      SPA-N31IDCD     NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "N31    "       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  N31
           INITIALIZE                      N31
      *
           MOVE    SPA-GMN-POST        TO  N31-POST
           MOVE    SPA-GMN-SELNUM      TO  N31-SELNUM
           MOVE    SPA-GMN-PREFCD-G    TO  N31-PREFCD
           MOVE    SPA-GMN-PREFKANA    TO  N31-PREFKANA
           MOVE    SPA-GMN-CITYKANA    TO  N31-CITYKANA
           MOVE    SPA-GMN-TOWNKANA    TO  N31-TOWNKANA
           MOVE    SPA-GMN-PREFNAME    TO  N31-PREFNAME
           MOVE    SPA-GMN-CITYNAME    TO  N31-CITYNAME
           MOVE    SPA-GMN-TOWNNAME    TO  N31-TOWNNAME
           MOVE    SPA-GMN-TOWNNAME2   TO  N31-TOWNNAME2
      *
      *    都道府県ＣＤコンボセット
           PERFORM VARYING IDXC    FROM    1   BY      1
                   UNTIL   IDXC    >       SPA-GMN-PREFCD-MAX
               MOVE    SPA-GMN-PREFCD-LIST(IDXC)
                                       TO  N31-PREFCD-ITEM(IDXC)
           END-PERFORM
           MOVE    SPA-GMN-PREFCD-MAX  TO  N31-PREFCD-COUNT
      *
      *    町域リスト
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL   IDXL    >       SPA-GMN-CYOIKI-MAX
               MOVE    SPA-GMN-CYO-HEN(IDXL)
                                       TO  N31-CYO-HEN(IDXL)
               MOVE    SPA-GMN-CYO-CD(IDXL)
                                       TO  N31-CYO-CD(IDXL)
               MOVE    SPA-GMN-CYO-POST(IDXL)
                                       TO  N31-CYO-POST(IDXL)
               MOVE    SPA-GMN-CYO-ADRS(IDXL)
                                       TO  N31-CYO-ADRS(IDXL)
               MOVE    SPA-GMN-CYO-TOWNNAME2(IDXL)
                                       TO  N31-CYO-TOWNNAME2(IDXL)
      *H29.9
               IF     (SPA-NAI-TDATAKBN (IDXL)   =   "2"   )
                 AND  (SPA-GMN-CYO-HEN(IDXL)     =   SPACE )
                  MOVE    "更"         TO  N31-CYO-HEN(IDXL)
               END-IF
      *        背景色
               IF      SPA-NAI-TDATAKBN (IDXL)   =   "2"
                   MOVE    "#FFCC99"       TO  N31-CYOIKI-BGCOLOR
                                                       (IDXL)
               END-IF
      *H29.9
      *        IF      IDXL     =   SPA-GMN-SELNUM
      *            MOVE    "T"         TO  N31-CYOIKI-SELECT(IDXL)
      *        ELSE
      *            MOVE    "F"         TO  N31-CYOIKI-SELECT(IDXL)
      *        END-IF
           END-PERFORM
           MOVE    SPA-GMN-CYOIKI-MAX  TO  N31-CYOIKI-COUNT
      *
           IF      SPA-NAI-NEXTKBN     =   1
               IF      SPA-NAI-PAGE        =   1
                   MOVE    "次頁有り"          TO  N31-NEXT-VALUE
               ELSE
                   MOVE    "前頁・次頁有り"    TO  N31-NEXT-VALUE
               END-IF
               MOVE    "red"               TO  N31-NEXT-STYLE
           ELSE
               IF      SPA-NAI-PAGE        >   1
                   MOVE    "前頁有り"          TO  N31-NEXT-VALUE
                   MOVE    "red"               TO  N31-NEXT-STYLE
               ELSE
                   MOVE    SPACE               TO  N31-NEXT-VALUE
                   MOVE    "black"             TO  N31-NEXT-STYLE
               END-IF
           END-IF
      *
      *    前頁ボタン非活性
           IF      SPA-NAI-PAGE    <=  1
               MOVE    WIDGET-INSENSITIVE  TO  N31-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  N31-B06-STATE
           END-IF
      *
      *    次頁ボタン非活性
           IF      SPA-NAI-NEXTKBN =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  N31-B07-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  N31-B07-STATE
           END-IF
      *
      *    カーソルのセット
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
             WHEN      "0001"
               MOVE    "入力された郵便番号は登録されていません"
                                       TO  SPA-ERRMSG
             WHEN      "0002"
               MOVE    "範囲内の番号を選択してください"
                                       TO  SPA-ERRMSG
             WHEN      "0003"
               MOVE    "都道府県番号を入力してください"
                                       TO  SPA-ERRMSG
             WHEN      "0004"
               MOVE    "市町村名カナ入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0005"
               MOVE    "町域名カナ入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0006"
               MOVE    "市町村名入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0007"
               MOVE    "町域名入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0008"
               MOVE    "通り名称入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0011"
               MOVE    "郵便番号を正しく入力してください"
                                       TO  SPA-ERRMSG
             WHEN      "0012"
               MOVE    "連番は９９までしか登録できません"
                                       TO  SPA-ERRMSG
             WHEN      "0013"
               MOVE    "正しい都道府県番号を選択して下さい"
                                       TO  SPA-ERRMSG
             WHEN      "0014"
               MOVE    "この住所は削除が許可されていません"
                                       TO  SPA-ERRMSG
      *H29.9
             WHEN      "0015"
                 STRING    "すでにユーザ登録されています。"
                           "ユーザ登録分を更新して下さい。"
                                       INTO  SPA-ERRMSG
                 END-STRING
             WHEN      "0016"
                 STRING    "変更箇所がありません。"
                           "更新できません。"
                                       INTO  SPA-ERRMSG
                 END-STRING
             WHEN      "1001"
               MOVE    "更新処理ができませんでした"
                                       TO  SPA-ERRMSG
             WHEN      "1002"
               MOVE    "追加処理ができませんでした"
                                       TO  SPA-ERRMSG
             WHEN      "1003"
               MOVE    "削除処理ができませんでした"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N31ERR
           MOVE    SPA-ERRCD       TO  N31ERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  N31ERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "N31"           TO  SPA-MOTOPG
           MOVE    "N31ERR"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N31ERR"        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-N31IDMSG
      *
           EVALUATE    SPA-N31IDCD
             WHEN      "1001"
               MOVE    "登録処理をします"  TO  WRK-N31IDMSG
             WHEN      "1002"
               MOVE    "削除処理をします"  TO  WRK-N31IDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N31ID1
           MOVE    SPA-N31IDCD     TO  N31ID1-ID1CODE
           MOVE    WRK-N31IDMSG    TO  N31ID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "N31"           TO  SPA-MOTOPG
           MOVE    "N31ID1"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N31ID1"        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次へ
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
               MOVE    "POST"           TO   MCP-WIDGET
           WHEN    02
               MOVE    "SELNUM"         TO   MCP-WIDGET
           WHEN    03
               MOVE    "PREFCD"         TO   MCP-WIDGET
           WHEN    04
               MOVE    "CITYKANA"       TO   MCP-WIDGET
           WHEN    05
               MOVE    "TOWNKANA"       TO   MCP-WIDGET
           WHEN    06
               MOVE    "CITYNAME"       TO   MCP-WIDGET
           WHEN    07
               MOVE    "TOWNNAME"       TO   MCP-WIDGET
           WHEN    08
               MOVE    "TOWNNAME2"      TO   MCP-WIDGET
           WHEN    98
               MOVE    "B02"            TO   MCP-WIDGET
           WHEN    99
               MOVE    "B12"            TO   MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "POST"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "PREFCD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "CITYKANA"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "TOWNKANA"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "CITYNAME"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "TOWNNAME"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "TOWNNAME2"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
           .
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理基本情報読込
      *****************************************************************
       900-SYS1001-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
           .
      *
       900-SYS1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスター読込
      *****************************************************************
       900-ADRS-SELECT-SEC         SECTION.
      *
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ADRS-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
           .
      *
       900-ADRS-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスターFETCH
      *****************************************************************
       900-ADRS-FETCH-SEC          SECTION.
      *
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-ADRS
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
           .
      *
       900-ADRS-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ住所マスター読込
      *****************************************************************
       902-USER-ADRS-SELECT-SEC         SECTION.
      *
           MOVE    "tbl_adrs_user"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-USERADRS-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-USERADRS
           END-IF
           .
      *
       902-USER-ADRS-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ住所マスターFETCH
      *****************************************************************
       900-USERADRS-FETCH-SEC          SECTION.
      *
           MOVE    "tbl_adrs_user"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-USERADRS
               MOVE    MCPDATA-REC     TO  USER-ADRS-REC
      *        キー編集
               MOVE    USER-ADRS-POST      TO  WRK-USER-POST
               MOVE    USER-ADRS-LPUBCD    TO  WRK-USER-LPUBCD
               MOVE    USER-ADRS-EDANUM    TO  WRK-USER-EDANUM
           ELSE
               MOVE    1               TO  FLG-USERADRS
      *        キー編集
               MOVE     ALL "9"         TO  WRK-USER-KEY
           END-IF
           .
      *
       900-USERADRS-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスターCLOSE
      *****************************************************************
       900-ADRS-CLOSE-SEC          SECTION.
      *
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
           .
      *
       900-ADRS-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL   "ORCDBMAIN"      USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
