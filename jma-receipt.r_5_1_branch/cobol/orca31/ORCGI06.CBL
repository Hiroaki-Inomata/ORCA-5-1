      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      * This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      * Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGI06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入退院登録
      *  コンポーネント名  : 入院オーダー変更
      *  管理者            : 
      *  05/03/09    NACL-太田    新規作成
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
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ設定用共通パラメタ
           COPY    "I0COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "I06SCR-SPA.INC".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-ALLIDOLST-ROW           PIC 9(01).
           03  FLG-PTIDOLST-ROW           PIC 9(01).
           03  FLG-ERRMSGLST-ROW           PIC 9(01).
           03  FLG-CMB-EDIT                PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-ORDER                   PIC 9(01).
           03  FLG-ODRITEM                 PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-ORCSNUM                 PIC 9(01).
           03  FLG-KHNERR                  PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-TARGET                  PIC 9(01).
           03  FLG-RYOTARGET               PIC 9(01).
           03  FLG-ROUJIN                  PIC 9(01).
           03  FLG-LIFE-RJN                PIC 9(01).
           03  FLG-RYOYOKHN                PIC 9(01).
           03  FLG-SKIP                    PIC 9(01).
           03  FLG-EDIT                    PIC 9(01).
           03  FLG-BUNBENCHK               PIC 9(01).
           03  FLG-BUNBEN                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-MAX                     PIC 9(05).
           03  CNT-STR                     PIC 9(05).
           03  CNT-ORDER                   PIC 9(05).
           03  CNT-DRCD                    PIC 9(05).
           03  CNT-KSNCD                   PIC 9(05).
           03  CNT-SKIP                    PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX10                       PIC 9(05).
           03  IDX17                       PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-MAX                     PIC 9(05).
           03  IDX-HEAD                    PIC 9(05).
           03  IDX-ITM                     PIC 9(05).
           03  IDX-LIFE                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(20).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-IDX             PIC 9(05).
               05  WRK-CMB-ITM             PIC X(200).
               05  WRK-CMB-ITM2            PIC X(200).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG                 PIC X(10).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-NUM-EDIT.
               05  WRK-99                  PIC 9(02).
               05  WRK-99999               PIC 9(05).
               05  WRK-999999              PIC 9(06).
               05  WRK-ZZ9                 PIC ZZ9.
               05  WRK-ZZZ9                PIC ZZZ9.
               05  WRK-ZZZZ9               PIC ZZZZ9.
               05  WRK-ZZZZZ9              PIC ZZZZZ9.
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-NUM          PIC ZZZZZ9.
               05  WRK-SAGAKU-EN           PIC X(02).
           03  WRK-TNS-SRYCD               PIC X(09).
           03  WRK-TNS-KJNYMD              PIC X(08).
           03  WRK-TOKNYUIN.
               05  WRK-TOKNYUIN-KBN        PIC X(01).
               05  WRK-TOKNYUIN-CD         PIC X(02).
           03  WRK-CHKTOKNYUIN.
               05  WRK-CHKTOKNYUIN-KBN     PIC X(01).
               05  WRK-CHKTOKNYUINCD       PIC X(02).
           03  WRK-TAIHI-HKN               PIC X(04).
           03  WRK-I0IDMSG                 PIC X(80). 
           03  WRK-INPUT                   PIC X(100).
           03  WRK-PTINF-PTID              PIC 9(10).
           03  WRK-AGE-G.
               05  WRK-AGE-Z               PIC ---9.
               05  WRK-AGE-MEI             PIC X(04).
           03  WRK-SHONUM                  PIC 9(02).
           03  WRK-LBL-SHONYUINYMD         PIC X(20).
           03  WRK-SAGAKUCD                PIC X(02).
           03  WRK-SAGAKUNM                PIC 9(08).
           03  WRK-SNUM-INX                PIC X(26).
           03  WRK-SNUM-OUTNUM             PIC 9(12).
           03  WRK-STR                     PIC X(500).
           03  WRK-BTUDEF-G.
               05  WRK-BTUDEF-SAGAKU       PIC X(02).
               05  WRK-BTUDEF-NYUINKA      PIC X(02).
           03  WRK-TAIINCD                 PIC X(02).
           03  WRK-INF-DIALOG              PIC 9(02).
           03  WRK-I0ID1-FLG               PIC X(02).
           03  WRK-I0IDCD                  PIC X(04).
           03  WRK-PTNUM-PTID              PIC 9(10).
           03  WRK-PTNUM-PTNUM             PIC X(20).
           03  WRK-ALLORDERID              PIC X(04).
           03  WRK-SKIP-MAX                PIC 9(05).
           03  WRK-TALLNUM                 PIC 9(05).
           03  WRK-NUMSRYCD                PIC 9(09).
           03  WRK-TOKNYUIN-SRYCDTTL       PIC 9(14).
           03  WRK-ERRMSG                  PIC X(80).
           03  WRK-TOK-G.
               05  WRK-TOK-SRYCD           PIC X(09).
               05  WRK-TOK-NAME            PIC X(60).
               05  WRK-TOK-NYUINRYOKBN     PIC X(02).
           03  WRK-TOKHIGAITO-G.
               05  WRK-TOKHIGAITO-CD       PIC X(09).
           03  WRK-STATE                   PIC S9(9).
           03  WRK-SELNUMBER               PIC 9(9).
           03  WRK-BIRTHDAY.
               05  WRK-BIRTHDAY-YY         PIC 9(04).
               05  WRK-BIRTHDAY-MM         PIC 9(02).
               05  WRK-BIRTHDAY-DD         PIC 9(02).
           03  WRK-NOW.
               05  WRK-NOW-YY              PIC 9(04).
               05  WRK-NOW-MM              PIC 9(02).
               05  WRK-NOW-DD              PIC 9(02).
           03  WRK-AGE                     PIC S9(03).
           03  WRK-BUNBENKBN               PIC X(01).
      *
       01  WRK-ROW-AREA.
           03  WRK-ALLIDOLST-ROW           PIC S9(9).
           03  WRK-PTIDOLST-ROW           PIC S9(9).
           03  WRK-ERRMSGLST-ROW           PIC S9(9).
      *
       01  ITEM-AREA.
           03  ITEM-LASTNYINYMD            PIC X(08).
           03  ITEM-TOKNYUIN-G.
               05  ITEM-TOKNYUIN-KBN       PIC X(01).
               05  ITEM-TOKNYUIN-NYUIN     PIC X(02).
      *
       01  CONST-AREA.
           03  CONST-H280401               PIC X(08)   VALUE "20160401".
           03  CONST-H261001               PIC X(08)   VALUE "20141001".
           03  CONST-H260401               PIC X(08)   VALUE "20140401".
           03  CONST-H241001               PIC X(08)   VALUE "20141001".
           03  CONST-H240401               PIC X(08)   VALUE "20120401".
           03  CONST-H220401               PIC X(08)   VALUE "20100401".
           03  CONST-H200401               PIC X(08)   VALUE "20080401".
           03  CONST-H181001               PIC X(08)   VALUE "20061001".
           03  CONST-H180701               PIC X(08)   VALUE "20060701".
           03  CONST-ERRCD9001             PIC X(04)   VALUE "9001".
           03  CONST-BTUNAME-MAX           PIC 9(05)   VALUE 50.
           03  CONST-SAGAKU-MAX            PIC 9(05)   VALUE 99.
           03  CONST-NYUINKA-MAX           PIC 9(05)   VALUE 99.
           03  CONST-TAIINCD-MAX           PIC 9(05)   VALUE 50.
           03  CONST-DR-MAX                PIC 9(05)   VALUE 99.
           03  CONST-HKNCOMBI-MAX          PIC 9(05)   VALUE 30.
           03  CONST-TOKNYUIN-MAX          PIC 9(05)   VALUE 100.
           03  CONST-ALLIDOLST-MAX         PIC 9(05)   VALUE 100.
           03  CONST-ALLIDOLST-LIMIT       PIC 9(05)   VALUE 5000.
           03  CONST-PTIDOLST-MAX         PIC 9(05)   VALUE 50.
           03  CONST-ERRCD-MAX             PIC 9(05)   VALUE 10.
           03  CONST-ODRITEM-MAX           PIC 9(05)   VALUE 30.
           03  CONST-KANSATU               PIC X(02)   VALUE "10".
           03  CONST-DTYPE-API             PIC X(01)   VALUE "1".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
           COPY    "CPRYOYOCD.INC".
      *
      *    項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *    エラーコード
           COPY    "CPNCLAIMERRCD.INC".
      *
       01  ERR-ORDER-ERRCD             PIC X(04).
       01  ERR-ODRITEM-REC.
           02  ERR-ODRITEM-MAX         PIC 9(03).
           02  ERR-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //ERR-ODRITEM-//.
      *
           COPY    "I06SCR-SPA.INC"    REPLACING  //SPA-//
                                       BY         //TSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    （診療科入院科情報）
           COPY    "CPSK1005.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （医療機関情報）
           COPY    "CPSK5000.INC".
      *    （病棟情報）
           COPY    "CPSK5001.INC".
      *    （病室情報）
           COPY    "CPSK5002.INC".
      *    （短期３）
           COPY    "CPSK5003.INC".
      *    （室料差額）
           COPY    "CPSK5113.INC".
      *    （特定入院料）
           COPY    "CPSK5110.INC".
           COPY    "CPSK5111.INC".
           COPY    "CPSK5112.INC".
      *
           COPY    "CPSK5013.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    診療行為算定日一括入力
           COPY    "CPORCSNDAYCHG.INC".
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
      *
      *    入院オーダーエラーメッセージ取得
           COPY    "CPORCSODRS03.INC".
      *
      *    病室名称編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    年齢計算
           COPY    "CPORCSAGECHK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    特定入院料コード取得サブ
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    入院料加算算定可否チェックサブ
           COPY    "CPORCSI01.INC".
      *
      *    入院料取得サブ
           COPY "CPORCSNYUINRYO.INC".
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
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA31SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
           MOVE    SPA-FREE        TO  SPA-I06
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
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
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I06         TO  SPA-FREE
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
           IF    ( SPA-MOTOPG          =   "I0ERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "I06"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
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
      *
               MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
      *
               MOVE    SPACE           TO  SPA-I0ID1-FLG
               MOVE    SPACE           TO  SPA-I0IDCD
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "I0ID1"
               PERFORM 330-I0ID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
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
           INITIALIZE                  SPA-I06
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-SELUKEYMD
           MOVE    SPA-NAI-SELUKEYMD  TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-SELUKEYMD
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-KJNYMD
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
      *    検索状態設定処理
           PERFORM 3101-SELSTSLST-SET-SEC
           MOVE    SPA-GMN-SELSTSLST (1)
                                   TO  SPA-GMN-SELSTS-G
      *
      *    並び順設定処理
           PERFORM 3101-SELSORTLST-SET-SEC
           MOVE    SPA-GMN-SELSORTLST (1)
                                   TO  SPA-GMN-SELSORT-G
      *
           MOVE    1               TO  SPA-GMN-ALLIDOLST-PAGE
      *
      *    異動リスト編集処理
           PERFORM 3101-ALLIDOLST-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
      *    室料差額設定処理
           PERFORM 3101-SAGAKULST-SET-SEC
      *
      *    選定入院区分設定処理
           PERFORM 3101-SENTEIKBNLST-SET-SEC
      *
      *    入院科設定処理
           PERFORM 3101-NYUINKALST-SET-SEC
      *
      *    初回設定処理
           PERFORM 3101-SHOKAILST-SET-SEC
      *
      *    介護区分設定処理
           PERFORM 3101-KAIGOKBNLST-SET-SEC
      *
      *    分娩区分設定処理
           PERFORM 3101-BUNBENKBNLST-SET-SEC
      *
      *    直接利用制度利用区分設定処理
           PERFORM 3101-RIYOUKBNLST-SET-SEC
      *
      *    担当医設定処理
           PERFORM 3101-DRLST-SET-SEC
      *
      *    入院会計区分設定処理
           PERFORM 3101-NACCTKBNLST-SET-SEC
      *
      *    入院加算設定処理
           PERFORM 3101-KSNCDLST-SET-SEC
      *
      *    定期請求区分設定処理
           PERFORM 3101-TEIKIKBNLST-SET-SEC
      *
      *    検索時患者表示区分設定処理
           PERFORM 3101-DISPKBNLST-SET-SEC
      *
      *    食事時間設定処理
           PERFORM 3101-SKJTIMELST-SET-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    病棟（病室）基本情報編集処理
      *****************************************************************
       3101-BTUDEF-SEC                 SECTION.
      *
           INITIALIZE                  SPA-NAI-BTUDEF-AREA
      *
           INITIALIZE              ORCSBRMNUMAREA
           MOVE    "2"             TO  SBRMNUM-IN-PROCKBN
           MOVE    SPA-GMN-BRMNUM  TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-MAX
      *
           PERFORM UNTIL ( FLG-SYSKANRI        =   1 )
                   OR    ( IDX-MAX            >= CONST-BTUNAME-MAX  )
      *
               MOVE    MCPDATA-REC TO  SYS-5002-REC
      *
               IF    ( SYS-5002-KBNCD (3:)     =   SBRMNUM-OT-BRMNUM )
                   COMPUTE IDX-MAX =   IDX-MAX +   1
                   MOVE    SYS-5002-KBNCD (1 : 2)
                                           TO  SPA-NAI-BTUDEF-BTUNUM
                                                           (IDX-MAX)
                   MOVE    SYS-5002-BRM-SAGAKU
                                           TO  SPA-NAI-BTUDEF-SAGAKU
                                                           (IDX-MAX)
                   MOVE    SYS-5002-BRM-KA TO  SPA-NAI-BTUDEF-NYUINKA
                                                           (IDX-MAX)
                   MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                           TO  SPA-NAI-BTUDEF-BRMTOK
                                                           (IDX-MAX)
                   MOVE    SYS-5002-KHNKBNCD
                                       TO  SPA-NAI-BTUDEF-BRMKHNKBNCD
                                                             (IDX-MAX)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDX-MAX         TO  SPA-NAI-BTUDEF-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       3101-BTUDEF-EXT.
           EXIT.
      *****************************************************************
      *    病棟設定処理
      *****************************************************************
       3101-BTUNAMELST-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-BTUNAMELST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-BTUNAME-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-NAI-BTUDEF-MAX )
      *
                   IF    ( SPA-NAI-BTUDEF-BTUNUM (IDX-CMB)
                                       =   SYS-5001-KBNCD )
                       COMPUTE CNT-MAX =   CNT-MAX +   1
                       MOVE    SYS-5001-BTU-NUM
                                   TO  SPA-GMN-BTUNAMEL (CNT-MAX)
                       MOVE    SYS-5001-BTU-NAME
                                   TO  SPA-GMN-BTUNAMEMEIL (CNT-MAX)
      *
                       IF    ( SPA-NAI-BTUDEF-NYUINKA (IDX-CMB)
                                       =   SPACE )
                           MOVE    SYS-5001-BTU-KA
                                   TO  SPA-NAI-BTUDEF-NYUINKA (IDX-CMB)
                       END-IF
      *
                       MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  SPA-NAI-BTUDEF-KHNSRYCD (IDX-CMB)
                       MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                   TO  SPA-NAI-BTUDEF-BTUTOK (IDX-CMB)
                       MOVE    SYS-5001-BTU-SBT
                                   TO  SPA-NAI-BTUDEF-SBT (IDX-CMB)
                       MOVE    SYS-5001-KHNKBNCD
                                   TO  SPA-NAI-BTUDEF-BTUKHNKBNCD
                                                             (IDX-CMB)
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-BTUNAME-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-BTUNAMELST-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額設定処理
      *****************************************************************
       3101-SAGAKULST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SAGAKULST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-SAGAKU-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
               COMPUTE CNT-MAX =   CNT-MAX +   1
      *
               MOVE    SYS-5113-KBNCD
                               TO  SPA-GMN-SAGAKUL (CNT-MAX)
               MOVE    SYS-5113-BRM-SAGAKU-NM
                               TO  WRK-SAGAKU-NUM
               MOVE    "円"    TO  WRK-SAGAKU-EN
               MOVE    WRK-SAGAKU
                               TO  SPA-GMN-SAGAKUMEIL (CNT-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-SAGAKU-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-SAGAKULST-SET-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分設定処理
      *****************************************************************
       3101-SENTEIKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-SENTEIKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SENTEIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SENTEIKBNL (1)
           MOVE    "選定対象"      TO  SPA-GMN-SENTEIKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SENTEIKBNL (2)
           MOVE    "選定対象外"    TO  SPA-GMN-SENTEIKBNMEIL (2)
      *
           . 
       3101-SENTEIKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院科設定処理
      *****************************************************************
       3101-NYUINKALST-SET-SEC         SECTION.
      *
      *
           INITIALIZE                  SPA-GMN-NYUINKALST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-NYUINKA-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
               COMPUTE CNT-MAX =   CNT-MAX +   1
      *
               MOVE    SYS-1005-KBNCD
                               TO  SPA-GMN-NYUINKAL    (CNT-MAX)
               MOVE    SYS-1005-SRYKANAME
                               TO  SPA-GMN-NYUINKAMEIL (CNT-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-NYUINKA-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-NYUINKALST-SET-EXT.
           EXIT.
      *****************************************************************
      *    初回設定処理
      *****************************************************************
       3101-SHOKAILST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SHOKAILST-G
      *
           MOVE    2                   TO  SPA-GMN-SHOKAI-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-SHOKAIL    (1)
           MOVE    "初回"              TO  SPA-GMN-SHOKAIMEIL (1)
      *
           MOVE    "2"                 TO  SPA-GMN-SHOKAIL    (2)
           MOVE    "継続"              TO  SPA-GMN-SHOKAIMEIL (2)
      *
           . 
       3101-SHOKAILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    介護区分設定処理
      *****************************************************************
       3101-KAIGOKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-KAIGOKBNLST-G
      *
           MOVE    2                   TO  SPA-GMN-KAIGOKBN-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-KAIGOKBNL (01)
           MOVE    "対象外"            TO  SPA-GMN-KAIGOKBNMEIL (01)
      *
           MOVE    "2"                 TO  SPA-GMN-KAIGOKBNL (02)
           MOVE    "急性増悪により"    TO  SPA-GMN-KAIGOKBNMEIL (02)
      *
           . 
       3101-KAIGOKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院事由設定処理
      *****************************************************************
       3101-TAIINCDLST-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-TAIINCDLST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5013"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-TAIINCD-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5013-REC
      *
               COMPUTE CNT-MAX =   CNT-MAX +   1
      *
               MOVE    SYS-5013-KBNCD
                               TO  SPA-GMN-TAIINCDL    (CNT-MAX)
               MOVE    SYS-5013-NAME
                               TO  SPA-GMN-TAIINCDMEIL (CNT-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-TAIINCD-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-TAIINCDLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分設定処理
      *****************************************************************
       3101-BUNBENKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-BUNBENKBNLST-G
      *
           MOVE    3                   TO  SPA-GMN-BUNBENKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-BUNBENKBNL (01)
           MOVE    "分娩入院でない"    TO  SPA-GMN-BUNBENKBNMEIL (01)
      *
           MOVE    "1"                 TO  SPA-GMN-BUNBENKBNL (02)
           MOVE    "正常分娩"          TO  SPA-GMN-BUNBENKBNMEIL (02)
      *
           MOVE    "2"                 TO  SPA-GMN-BUNBENKBNL (03)
           MOVE    "異常分娩"          TO  SPA-GMN-BUNBENKBNMEIL (03)
      *
           . 
       3101-BUNBENKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分設定処理
      *****************************************************************
       3101-RIYOUKBNLST-SET-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-RIYOUKBNLST-G
      *
           MOVE    2                   TO  SPA-GMN-RIYOUKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-RIYOUKBNL (01)
           MOVE    "利用しない"        TO  SPA-GMN-RIYOUKBNMEIL (01)
      *
           MOVE    "1"                 TO  SPA-GMN-RIYOUKBNL (02)
           MOVE    "利用する"          TO  SPA-GMN-RIYOUKBNMEIL (02)
      *
           . 
       3101-RIYOUKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    担当医設定処理
      *****************************************************************
       3101-DRLST-SET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-DRLST-G
      *
           MOVE    ZERO            TO  CNT-MAX
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT  =  ZERO )
                    OR   ( CNT-MAX     >=  CONST-DR-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
               IF    ( SYS-1010-KBNCD (1 : 1)  =   "1" )
      *
                   COMPUTE CNT-MAX =   CNT-MAX +   1
      *
                   MOVE    SYS-1010-KBNCD (2 : 4)
                               TO  SPA-GMN-DRL    (CNT-MAX)
                   MOVE    SYS-1010-NAME
                               TO  SPA-GMN-DRMEIL (CNT-MAX)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    CNT-MAX                 TO  SPA-GMN-DR-MAX
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-DRLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       3101-HKNCOMBILST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNCOMBILST-G
                                       SPA-NAI-HKNCOMBILST-G
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           INITIALIZE                  WRK-SPA-KYOTUKEY
                                       WRK-SPA-KYOTU-SET
                                       ORCSHKNSETAREA
      *
           MOVE    SPA-NAI-KJNYMD      TO  WRK-SPA-SRYYMD
           MOVE    SPA-GMN-NYUINKA
                                       TO  WRK-SPA-SRYKA
           MOVE    "1"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-NAI-PTID        TO  WRK-SPA-PTID
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-SPA-BIRTHDAY
           MOVE    SPA-GMN-HKNCOMBI    TO  WRK-SPA-HKNCOMBINUM
           MOVE    "1"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   WRK-SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               GO  TO  3101-HKNCOMBILST-SET-EXT
           END-IF
      *
           IF    ( ORCSHKN-HKN-MAX     >   CONST-HKNCOMBI-MAX )
               MOVE    30              TO  SPA-GMN-HKNCOMBI-MAX
           ELSE
               MOVE    ORCSHKN-HKN-MAX TO  SPA-GMN-HKNCOMBI-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-HKNCOMBI-MAX )
      *
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX0)
                                       TO  SPA-GMN-HKNCOMBIL   (IDX0)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX0)
                                       TO  SPA-GMN-HKNCOMBIMEIL(IDX0)
               MOVE    ORCSHKN-NAI-HKNNUM (IDX0)
                                       TO  SPA-NAI-HKNNUML   (IDX0)
      *
           END-PERFORM
      *
      *    IF    ( ORCSHKN-IDX    >=   1 )
      *     AND  ( ORCSHKN-IDX    <=   SPA-GMN-HKNCOMBI-MAX )
      *        MOVE    SPA-GMN-HKNCOMBILST (ORCSHKN-IDX)
      *                                TO  SPA-GMN-HKNCOMBI-G
      *    END-IF
      *
           . 
       3101-HKNCOMBILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    算定要件非該当区分設定処理
      *****************************************************************
       3101-TOKHIGAITOKBNLST-SET-SEC   SECTION.
      *
           INITIALIZE                  SPA-GMN-TOKHIGAITOKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-TOKHIGAITOKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-TOKHIGAITOKBNL    (01)
           MOVE    "算定要件に該当する患者" 
                                   TO  SPA-GMN-TOKHIGAITOKBNMEIL (01)
      *
           MOVE    "1"             TO  SPA-GMN-TOKHIGAITOKBNL    (02)
           MOVE    "算定要件に該当しない患者"
                                   TO  SPA-GMN-TOKHIGAITOKBNMEIL (02)
      *
           . 
       3101-TOKHIGAITOKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計区分設定処理
      *****************************************************************
       3101-NACCTKBNLST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NACCTKBNLST-G
      *
           MOVE    2                   TO  SPA-GMN-NACCTKBN-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-NACCTKBNL (01)
           MOVE    "入院料を算定しない"
                                       TO  SPA-GMN-NACCTKBNMEIL(01)
      *
           MOVE    "2"                 TO  SPA-GMN-NACCTKBNL (02)
           MOVE    "入院料を算定する"  TO  SPA-GMN-NACCTKBNMEIL(02)
      *
           . 
       3101-NACCTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院加算設定処理
      *****************************************************************
       3101-KSNCDLST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KSNCDLST-G
      *
           INITIALIZE                  SI01-AREA
           MOVE    SPA-NAI-KJNYMD  TO  SI01-SRYYMD
           CALL    "ORCSI01"           USING
                                       SI01-AREA
                                       SPA-AREA
           MOVE    SI01-KSNLST-MAX TO  SPA-GMN-KSNCD-MAX
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-KSNCD-MAX )
               MOVE    IDX0        TO  WRK-99
               MOVE    WRK-99      TO  SPA-GMN-KSNCDL (IDX0)
               MOVE    SI01-KSNLST-NAME (IDX0)
                                   TO  SPA-GMN-KSNCDMEIL (IDX0)
               MOVE    SI01-KSNLST-SRYCD(IDX0)
                                   TO  SPA-GMN-KSNCDL-SRYCD (IDX0)
           END-PERFORM
      *
           . 
       3101-NYUKSNCDLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分設定処理
      *****************************************************************
       3101-TEIKIKBNLST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-TEIKIKBNLST-G
      *
           MOVE    3                   TO  SPA-GMN-TEIKIKBN-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-TEIKIKBNL (01)
           MOVE    "医療機関での設定"  TO  SPA-GMN-TEIKIKBNMEIL (01)
      *
           MOVE    "2"                 TO  SPA-GMN-TEIKIKBNL (02)
           MOVE    "月末時のみ請求"    TO  SPA-GMN-TEIKIKBNMEIL (02)
      *
           MOVE    "3"                 TO  SPA-GMN-TEIKIKBNL (03)
           MOVE    "定期請求しない"    TO  SPA-GMN-TEIKIKBNMEIL (03)
      *
           . 
       3101-TEIKIKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表時区分設定処理
      *****************************************************************
       3101-DISPKBNLST-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-DISPKBNLST-G
      *
           MOVE    2                   TO  SPA-GMN-DISPKBN-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-DISPKBNL    (1)
           MOVE    "表示可"            TO  SPA-GMN-DISPKBNMEIL (1)
      *
           MOVE    "2"                 TO  SPA-GMN-DISPKBNL    (2)
           MOVE    "表示不可"          TO  SPA-GMN-DISPKBNMEIL (2)
      *
           . 
       3101-DISPKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事時間設定処理
      *****************************************************************
       3101-SKJTIMELST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJTIMELST-G
      *
           MOVE    3                   TO  SPA-GMN-SKJTIME-MAX
      *
           MOVE    "01"                TO  SPA-GMN-SKJTIMEL    (1)
           MOVE    "朝"                TO  SPA-GMN-SKJTIMEMEIL (1)
      *
           MOVE    "02"                TO  SPA-GMN-SKJTIMEL    (2)
           MOVE    "昼"                TO  SPA-GMN-SKJTIMEMEIL (2)
      *
           MOVE    "03"                TO  SPA-GMN-SKJTIMEL    (3)
           MOVE    "夕"                TO  SPA-GMN-SKJTIMEMEIL (3)
      *
           . 
       3101-SKJTIMELST-SET-EXT.
           EXIT.
      *****************************************************************
      *    検索状態設定処理
      *****************************************************************
       3101-SELSTSLST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SELSTSLST-G
      *
           MOVE    2                   TO  SPA-GMN-SELSTS-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-SELSTSL    (1)
           MOVE    "全て表示"          TO  SPA-GMN-SELSTSMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-SELSTSL    (2)
           MOVE    "エラーのみ表示"    TO  SPA-GMN-SELSTSMEIL (2)
      *
           . 
       3101-SELSTSLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    並び順設定処理
      *****************************************************************
       3101-SELSORTLST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SELSORTLST-G
      *
           MOVE    4                   TO  SPA-GMN-SELSORT-MAX
      *
           MOVE    "01"                TO  SPA-GMN-SELSORTL    (1)
           MOVE    "診療日の古い順"
                                       TO  SPA-GMN-SELSORTMEIL (1)
      *
           MOVE    "02"                TO  SPA-GMN-SELSORTL    (2)
           MOVE    "診療日の新しい順"
                                       TO  SPA-GMN-SELSORTMEIL (2)
      *
           MOVE    "03"                TO  SPA-GMN-SELSORTL    (3)
           MOVE    "受付日時の古い順"
                                       TO  SPA-GMN-SELSORTMEIL (3)
      *
           MOVE    "04"                TO  SPA-GMN-SELSORTL    (4)
           MOVE    "受付日時の新しい順"
                                       TO  SPA-GMN-SELSORTMEIL (4)
      *
           . 
       3101-SELSORTLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料設定処理
      *****************************************************************
       3101-TOKNYUINLST-SET-SEC        SECTION.
      *
           INITIALIZE  SPA-GMN-TOKNYUINLST-G
                       SPA-NAI-KHNNYUIN
                       SPA-NAI-KHNSRYCD
                       SPA-NAI-SELNYUKHN
                       SPA-GMN-TOKHIGAITOKBNLST-G
      *
           MOVE    ZERO            TO  CNT-MAX
           MOVE    ZERO            TO  FLG-CMB-EDIT
      *
           PERFORM 800-ROUJINAGE-CHK-SEC
      *
           IF    ( SPA-NAI-BTU-KHNSRYCD    NOT =   SPACE )
               MOVE    SPA-NAI-BTU-KHNSRYCD    TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-KJNYMD          TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME    TO   SPA-NAI-KHNNYUIN
               MOVE    TNS-SRYCD   TO   SPA-NAI-KHNSRYCD
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-GMN-BTUNAME TO  SYS-KBNCD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    MCPDATA-REC TO  SYS-5001-REC
      *
           PERFORM 31010-TOKNYUINLST-SET-SEC
      *
           PERFORM 3101-TOKHIGAITOKBNLST-SET-SEC
      *
           . 
       3101-TOKNYUINLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料設定処理
      *****************************************************************
       31010-TOKNYUINLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SNYUINRYO-AREA
           MOVE    SPA-NAI-KJNYMD  TO  SNYUINRYO-SRYYMD
           IF    ( SPA-NAI-ORDERID =   NORDERID-NYUINID
                                    OR NORDERID-TEISEIID )
               MOVE    "01"        TO  SNYUINRYO-SYORIKBN
           ELSE
               MOVE    "08"        TO  SNYUINRYO-SYORIKBN
           END-IF
           MOVE    SPA-GMN-BTUNAME TO  SNYUINRYO-BTUNUM
           MOVE    SPA-GMN-BRMNUM  TO  SNYUINRYO-BRMNUM
           MOVE    SPA-GMN-HKNCOMBI TO SNYUINRYO-HKNCOMBI
      *
           IF    ( SPA-NAI-ORDERID =   NORDERID-IDOBTUID
                                    OR NORDERID-IDOBRMID
                                    OR NORDERID-IDOKAID
                                    OR NORDERID-IDOALLID )
               CONTINUE
           ELSE
               IF    ( SPA-NAI-TOKCD-KBN   =   "2" OR "3" )
                AND  ( SPA-GMN-BTUNAME     =   SPA-NAI-BTUNUM-MOT )
                AND  ( SPA-GMN-BRMNUM      =   SPA-NAI-BRMNUM-MOT )
                   MOVE    SPA-NAI-TOKCD-G
                                   TO  SNYUINRYO-EXTRA-G
               END-IF
           END-IF
      *
           MOVE    SPA-AREA        TO  WRK-SPA-AREA
           MOVE    SPA-NAI-PTID    TO  WRK-SPA-PTID
           CALL    "ORCSNYUINRYO"  USING
                                   SNYUINRYO-AREA
                                   WRK-SPA-AREA

      *
           MOVE    SNYUINRYO-MAX   TO  SPA-GMN-TOKNYUIN-MAX
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SNYUINRYO-MAX )
      *
               MOVE    SNYUINRYO-NO (IDX0)
                                   TO  SPA-GMN-TOKNYUINL (IDX0)
               MOVE    SNYUINRYO-NAME (IDX0)
                                   TO  SPA-GMN-TOKNYUINMEIL(IDX0)
               MOVE    SNYUINRYO-NYUINRYO-KBN (IDX0)
                                   TO  SPA-GMN-TOKNYUINL2-KBN (IDX0)
               MOVE    SNYUINRYO-NYUINRYO-CD (IDX0)
                                   TO  SPA-GMN-TOKNYUINL2-NYUIN
                                                           (IDX0)
               MOVE    SNYUINRYO-TYPE (IDX0)
                                   TO  SPA-GMN-TOKNYUINL-NYUINRYOKBN
                                                           (IDX0)
               MOVE    SNYUINRYO-SRYCD(IDX0)
                                   TO  SPA-GMN-TOKNYUINL-SRYCD
                                                           (IDX0)
               MOVE    SNYUINRYO-NASRYCD(IDX0)
                                   TO  SPA-GMN-TOKNYUINL-NASRYCD
                                                           (IDX0)
      *
           END-PERFORM
      *
           MOVE    SNYUINRYO-SRYCDTTL
                                   TO  SPA-GMN-TOKNYUIN-SRYCDTTL
           MOVE    SNYUINRYO-SELECT
                                   TO  SPA-NAI-SELNYUKHN
      *
           . 
       31010-TOKNYUINLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    老人チェック処理
      *****************************************************************
       31011-ROUJIN-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-RJN
      *
      *    入院患者の老人保険判定
           INITIALIZE                       HKNCOMBI-REC
      *
           MOVE    SPA-HOSPNUM          TO   COMB-HOSPNUM
           MOVE    SPA-NAI-PTID        TO   COMB-PTID
           MOVE    SPA-GMN-HKNCOMBI    TO   COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC 
      *
      *    入院患者の年齢計算
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM          TO  AGECHK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  AGECHK-PTID
           MOVE    SPA-NAI-KJNYMD      TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING   ORCSAGECHKAREA
                                               SPA-AREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
           IF    ( SPA-NAI-KJNYMD     >=   CONST-H200401 )
      *
      *        患者保険情報から老人保健の登録チェック
               INITIALIZE                      PTHKNINF-REC
               MOVE    SPA-HOSPNUM          TO  PTHKN-HOSPNUM
               MOVE    SPA-NAI-PTID        TO  PTHKN-PTID
               MOVE    SPA-NAI-KJNYMD      TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
               PERFORM 900-PTHKNINF-KEY11-SEL-SEC
               IF    ( FLG-PTHKNINF        =   ZERO )
                   MOVE    1               TO  FLG-RJN
               END-IF
      *
      *        患者公費情報から老人保健の登録チェック
               INITIALIZE                      PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-NAI-PTID        TO  PTKOH-PTID
               MOVE    "968"               TO  PTKOH-KOHNUM
               MOVE    SPA-NAI-KJNYMD      TO  PTKOH-TEKSTYMD
                                               PTKOH-TEKEDYMD
               PERFORM 900-PTKOHINF-KEY6-SEL-SEC
               IF    ( FLG-PTKOHINF        =   ZERO )
                   MOVE    1               TO  FLG-RJN
               END-IF
           ELSE
      *
      *        老人対象患者判定
               IF    ((FLG-HKNCOMBI        =   ZERO)
                  AND (COMB-KOH1HKNNUM     =   "027"))
                 OR  ( AGECHK-NENREI      >=    75)
                 OR  ((AGECHK-NENREI      >=    70)
                  AND (SPA-NAI-BIRTHDAY   <=   "19320930"))
                 OR  (AGECHK-ROUJIN        =    1)
                   MOVE    1               TO  FLG-RJN
               END-IF
           END-IF
      *
           .
       31011-ROUJIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    異動リスト編集処理
      *****************************************************************
       3101-ALLIDOLST-SEC              SECTION.
      *
           MOVE    1              TO   FLG-ALLIDOLST-ROW
      *
           INITIALIZE                  SPA-GMN-SELNUM
                                       SPA-GMN-ALLIDOLST-AREA
                                       SPA-NAI-ALLIDOLST-AREA
      *
           PERFORM 900-ORDER-KEY8-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-MAX
      *
           PERFORM UNTIL ( FLG-ORDER   NOT =   ZERO )
                    OR   ( IDX-MAX     >=  CONST-ALLIDOLST-MAX )
      *
               COMPUTE IDX-MAX =   IDX-MAX +   1
      *
               MOVE    ORDER-HOSPNUM
                                   TO  SPA-NAI-TALLHOSPNUM    (IDX-MAX)
               MOVE    ORDER-KARTE-KEY
                                   TO  SPA-NAI-TALLKARTE-KEY (IDX-MAX)
               MOVE    ORDER-ORDERNUM
                                   TO  SPA-NAI-TALLORDERNUM  (IDX-MAX)
      *
               COMPUTE WRK-TALLNUM
                   =   CONST-ALLIDOLST-MAX
                   * ( SPA-GMN-ALLIDOLST-PAGE    -   1 )
                   +   IDX-MAX
      *
               MOVE    WRK-TALLNUM TO  SPA-GMN-TALLNUM    (IDX-MAX)
      *
               MOVE    ORDER-UKEYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO   SPA-GMN-TALLUKEYMD (IDX-MAX)
               MOVE    WRK-SYMD    TO   SPA-NAI-TALLUKEYMD (IDX-MAX)
               MOVE    ORDER-UKEHMS(1:2)
                                   TO   SPA-GMN-TALLUKEHMS(IDX-MAX)(1:2)
               MOVE    ":"         TO   SPA-GMN-TALLUKEHMS(IDX-MAX)(3:1)
               MOVE    ORDER-UKEHMS(3:2)
                                   TO   SPA-GMN-TALLUKEHMS(IDX-MAX)(4:2)
      *
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-TALLIDOYMD (IDX-MAX)
               MOVE    WRK-SYMD    TO  SPA-NAI-TALLIDOYMD (IDX-MAX)
      *
               MOVE    ORDER-ORDERID
                                   TO  SPA-NAI-TALLORDERID(IDX-MAX)
               MOVE    ORDER-ORDERID   TO  WRK-CMB-CD
               PERFORM 4200-ORDERID-CHK-SEC
               MOVE    WRK-CMB-ITM2    TO  SPA-GMN-TALLIDOJIYU(IDX-MAX)
      *
               MOVE    ORDER-PTNUM TO  SPA-GMN-TALLPTNUM  (IDX-MAX)
               MOVE    ORDER-PTNUM TO  WRK-PTNUM-PTNUM
               PERFORM 900-PTNUM-KEY2-SEL-SEC
               IF    ( FLG-PTNUM   =   ZERO )
                   MOVE    PTNUM-PTID
                                       TO  SPA-NAI-TALLPTID   (IDX-MAX)
               END-IF
      *
               MOVE    SPA-NAI-TALLPTID   (IDX-MAX)
                                   TO  WRK-PTINF-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               MOVE    PTINF-NAME  TO  SPA-GMN-TALLNAME   (IDX-MAX)
      *
               MOVE    ORDER-STATUS    TO  SPA-GMN-TALLSTS (IDX-MAX)
      *
               EVALUATE    TRUE
               WHEN  ( ORDER-ERRCD1    NOT =   SPACE )
                   MOVE ORDER-ERRCD1   TO  SPA-GMN-TALLERRCD (IDX-MAX)
               WHEN  ( ORDER-ERRCD2    NOT =   SPACE )
                 AND ( ORDER-ERRCD2    NOT =   "9001" )
                   MOVE ORDER-ERRCD2   TO  SPA-GMN-TALLERRCD (IDX-MAX)
               END-EVALUATE
      *
               INITIALIZE              SODRS03-AREA
               MOVE    SPA-GMN-TALLERRCD (IDX-MAX)
                                       TO  SODRS03-ERRCD
               CALL    "ORCSODRS03"    USING
                                       SODRS03-AREA
               MOVE    SODRS03-ERRCD   TO  SPA-GMN-TALLERRCD (IDX-MAX)
      *
               PERFORM 900-ORDER-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDX-MAX         TO  SPA-GMN-ALLIDOLST-MAX
      *
           MOVE    "tbl_order"             TO  MCP-TABLE
           MOVE    "key8"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SPA-GMN-ALLIDOLST-MAX   =   ZERO )
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-GMN-ALLIDOLST-PAGE  *   CONST-ALLIDOLST-MAX
                                           <   CONST-ALLIDOLST-LIMIT )
            AND  ( FLG-ORDER       =   ZERO )
               MOVE    1                   TO  SPA-GMN-ALLIDOLST-NEXT
           END-IF
      *
           .
       3101-ALLIDOLST-EXT.
           EXIT.
      *****************************************************************
      *    異動リスト編集処理
      *****************************************************************
       3200-ALLIDOLST-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUMBER
      *
           IF    ( SPA-NAI-SELPTID     NOT =   ZERO )
               IF    ( SPA-GMN-ALLIDOLST-SEL   NOT =   ZERO )
                   PERFORM 900-ORDER-KEY9-SEL-SEC
                   MOVE    ORDER-SELNUMBER TO  WRK-SELNUMBER
               END-IF
           END-IF
      *
           PERFORM 220-CLEAR-SEC
      *
           IF    ( WRK-SELNUMBER   >   ZERO )
            AND  ( WRK-SELNUMBER   <=  CONST-ALLIDOLST-LIMIT )
               COMPUTE SPA-GMN-ALLIDOLST-PAGE
                   =   WRK-SELNUMBER / CONST-ALLIDOLST-MAX + 1
               PERFORM 3101-ALLIDOLST-SEC
               MOVE    WRK-SELNUMBER   TO  I06-SELNUM
               PERFORM 4100-SELNUM-CHK-SEC
           ELSE
               MOVE    1               TO  SPA-GMN-ALLIDOLST-PAGE
               PERFORM 3101-ALLIDOLST-SEC
           END-IF
      *
           .
       3200-ALLIDOLST-EXT.
           EXIT.
      *****************************************************************
      *    異動リスト編集処理
      *****************************************************************
       3101-PTIDOLST-SEC              SECTION.
      *
           MOVE    1              TO   FLG-PTIDOLST-ROW
      *
           INITIALIZE                  SPA-GMN-PTIDOLST-AREA
                                       SPA-NAI-PTIDOLST-AREA
      *
           PERFORM 900-ORDER-KEY4-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-MAX
      *
           PERFORM UNTIL ( FLG-ORDER   NOT =   ZERO )
                    OR   ( IDX-MAX     >=  CONST-PTIDOLST-MAX )
      *
               COMPUTE IDX-MAX =   IDX-MAX +   1
      *
               MOVE    ORDER-KARTE-KEY
                                   TO  SPA-NAI-TPTKARTE-KEY (IDX-MAX)
               MOVE    ORDER-ORDERNUM
                                   TO  SPA-NAI-TPTORDERNUM  (IDX-MAX)
               MOVE    ORDER-UKEYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-TPTUKEYMD (IDX-MAX)
               MOVE    WRK-SYMD    TO  SPA-NAI-TPTUKEYMD (IDX-MAX)
      *
               MOVE    ORDER-UKEHMS(1:2)
                                   TO   SPA-GMN-TPTUKEHMS(IDX-MAX)(1:2)
               MOVE    ":"         TO   SPA-GMN-TPTUKEHMS(IDX-MAX)(3:1)
               MOVE    ORDER-UKEHMS(3:2)
                                   TO   SPA-GMN-TPTUKEHMS(IDX-MAX)(4:2)
      *
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-TPTIDOYMD (IDX-MAX)
               MOVE    WRK-SYMD    TO  SPA-NAI-TPTIDOYMD (IDX-MAX)
      *
               MOVE    ORDER-ORDERID
                                   TO  SPA-NAI-TPTORDERID(IDX-MAX)
               MOVE    ORDER-ORDERID   TO  WRK-CMB-CD
               PERFORM 4200-ORDERID-CHK-SEC
               MOVE    WRK-CMB-ITM2    TO  SPA-GMN-TPTIDOJIYU(IDX-MAX)
      *
               MOVE    ORDER-STATUS    TO  SPA-GMN-TPTSTS (IDX-MAX)
      *
               PERFORM 900-ORDER-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDX-MAX             TO  SPA-GMN-PTIDOLST-MAX
      *
           MOVE    "tbl_order"             TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-PTIDOLST-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       3101-ERRMSGLST-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-ERRMSGLST-ROW
      *
           INITIALIZE                  SPA-GMN-ERRMSGLST-AREA
           MOVE    ZERO                TO  IDX-MAX
      *
           IF    ( ERR-ORDER-ERRCD  NOT =   SPACE )
               INITIALIZE                  SODRS03-AREA
               MOVE    ERR-ORDER-ERRCD TO  SODRS03-ERRCD
               CALL    "ORCSODRS03"    USING
                                       SODRS03-AREA
               MOVE    SODRS03-ERRCD   TO  SPA-GMN-TERRCD (1)
               MOVE    SODRS03-ERRMSG  TO  SPA-GMN-TERRMSG(1)
               MOVE    1               TO  IDX-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   ERR-ODRITEM-MAX )
      *
               MOVE    ERR-ODRITEM-OCC (IDX0)  TO  ODRITEM-REC
      *
               IF    ( ODRITEM-ERRCD   NOT =   SPACE )
                AND  ( ODRITEM-ERRCD   NOT =   CONST-ERRCD9001 )
                   IF    ( IDX-MAX    >=   CONST-ERRCD-MAX )
                       CONTINUE
                   ELSE
                       COMPUTE IDX-MAX =   IDX-MAX +   1
      *
                       MOVE    ODRITEM-ERRCD
                                           TO  SPA-GMN-TERRCD (IDX-MAX)
      *
                       INITIALIZE              SODRS03-AREA
                       MOVE    ODRITEM-ERRCD   TO  SODRS03-ERRCD
                       CALL    "ORCSODRS03"    USING
                                               SODRS03-AREA
                       MOVE    SODRS03-ERRCD
                                           TO  SPA-GMN-TERRCD (IDX-MAX)
                       MOVE    SODRS03-ERRMSG
                                           TO  SPA-GMN-TERRMSG(IDX-MAX)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX-MAX             TO  SPA-GMN-ERRMSGLST-MAX
      *
           .
       3101-ERRMSGLST-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（Ｉ０ＩＤ１）ＯＫ処理
      *****************************************************************
       330-I0ID1-SET-SEC               SECTION.
      *
           MOVE    SPA-I0ID1-FLG   TO  WRK-I0ID1-FLG
           MOVE    SPA-I0IDCD      TO  WRK-I0IDCD
      *
           MOVE    SPACE           TO  SPA-I0ID1-FLG
           MOVE    SPACE           TO  SPA-I0IDCD
      *
           IF    ( WRK-I0ID1-FLG       =   "OK" )
      *
               MOVE    SPACE           TO  SPA-I0ID1-FLG
               MOVE    SPACE           TO  SPA-I0IDCD
      *
               EVALUATE    WRK-I0IDCD
               WHEN    "2004"
                   MOVE    1       TO  WRK-INF-DIALOG
                   PERFORM 2121-NYUIN-TOUROKU-SEC
               WHEN    "2005"
                   MOVE    2       TO  WRK-INF-DIALOG
                   PERFORM 2121-NYUIN-TOUROKU-SEC
               WHEN    "2001"
               WHEN    "2001"
                   PERFORM 2301-DEL-SEC
               WHEN    "2002"
                   MOVE    1       TO  WRK-INF-DIALOG
                   PERFORM 2121-TAIIN-TOUROKU-SEC
               WHEN    "2003"
                   MOVE    2       TO  WRK-INF-DIALOG
                   PERFORM 2121-TAIIN-TOUROKU-SEC
               END-EVALUATE
           END-IF
      *
           .
       330-I0ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO      WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"     TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               IF    ( SPA-GMN-ALLIDOLST-SEL   =   ZERO )
                   PERFORM 310-SPASET-SEC
               ELSE
                   PERFORM 220-CLEAR-SEC
               END-IF
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 230-DEL-SEC
      *    前日
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 260-PREVDAY-SEC
      *    翌日
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 270-NEXTDAY-SEC
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06S"
               PERFORM 260-PREVPAGE-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07S"
               PERFORM 270-NEXTPAGE-SEC
      *    歴表示更新
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 220-CLEAR-SEC
               MOVE    1           TO  SPA-GMN-ALLIDOLST-PAGE
               PERFORM 3101-ALLIDOLST-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 212-TOUROKU-SEC
      *    検索患者番号複写
           WHEN    "CLICKED"       ALSO    "BSELPTNUM"
               PERFORM 299-EDIT-SELPTNUM-SEC
      *    受付日複写
           WHEN    "CLICKED"       ALSO    "BSELUKEYMD"
               PERFORM 299-EDIT-SELUKEYMD-SEC
      *    異動日複写
           WHEN    "CLICKED"       ALSO    "BSELIDOYMD"
               PERFORM 299-EDIT-SELIDOYMD-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "SELUKEYMD"
           WHEN    "ACTIVATE"      ALSO    "SELIDOYMD"
           WHEN    "ACTIVATE"      ALSO    "SELPTNUM"
           WHEN    "ACTIVATE"      ALSO    "SELSTS"
           WHEN    "ACTIVATE"      ALSO    "SELSORT"
               PERFORM 4100-SELITEMS-CHK-SEC
           WHEN        ANY         ALSO    "ALLIDOLST"
               PERFORM 4101-ALLIDOLST-SEL-SEC
           WHEN    OTHER
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    I06-BRMNUM          TO  SPA-GMN-BRMNUM
           MOVE    I06-BTUNAME         TO  SPA-GMN-BTUNAME-G
           MOVE    I06-SAGAKU          TO  SPA-GMN-SAGAKU-G
           MOVE    I06-SENTEIKBN       TO  SPA-GMN-SENTEIKBN-G
           MOVE    I06-NYUINYMD        TO  SPA-GMN-NYUINYMD
           MOVE    I06-NYUINKA         TO  SPA-GMN-NYUINKA-G
           MOVE    I06-SHOKAI          TO  SPA-GMN-SHOKAI-G
           MOVE    I06-SHONUM          TO  SPA-GMN-SHONUM
           MOVE    I06-KAIGOKBN        TO  SPA-GMN-KAIGOKBN-G
           MOVE    I06-TAIINYMD        TO  SPA-GMN-TAIINYMD
           MOVE    I06-TAIINCD         TO  SPA-GMN-TAIINCD-G
           MOVE    I06-DR(1)           TO  SPA-GMN-DR-G (1)
           MOVE    I06-DR(2)           TO  SPA-GMN-DR-G (2)
           MOVE    I06-DR(3)           TO  SPA-GMN-DR-G (3)
           MOVE    I06-HKNCOMBI        TO  SPA-GMN-HKNCOMBI-G
           MOVE    I06-TOKNYUIN        TO  SPA-GMN-TOKNYUIN-G
           MOVE    I06-TOKHIGAITOKBN   TO  SPA-GMN-TOKHIGAITOKBN-G
           MOVE    I06-NACCTKBN        TO  SPA-GMN-NACCTKBN-G
           MOVE    I06-KSNCD (1)       TO  SPA-GMN-KSNCD-G (1)
           MOVE    I06-KSNCD (2)       TO  SPA-GMN-KSNCD-G (2)
           MOVE    I06-KSNCD (3)       TO  SPA-GMN-KSNCD-G (3)
           MOVE    I06-TEIKIKBN        TO  SPA-GMN-TEIKIKBN-G
           MOVE    I06-DISPKBN         TO  SPA-GMN-DISPKBN-G
           MOVE    I06-SKJTIME         TO  SPA-GMN-SKJTIME-G
           MOVE    I06-NISSU           TO  SPA-GMN-NISSU
           MOVE    I06-BUNBENKBN       TO  SPA-GMN-BUNBENKBN-G
           MOVE    I06-RIYOUKBN        TO  SPA-GMN-RIYOUKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    異動リスト選択編集処理
      *****************************************************************
       4101-ALLIDOLST-SEL-SEC          SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-ALLIDOLST-MAX )
      *
               IF    ( I06-ALLIDOLST-SELECT (IDX-LST)  =   "T" )
                   MOVE    IDX-LST TO  SPA-GMN-ALLIDOLST-SEL
                   MOVE    SPA-GMN-TALLNUM  (IDX-LST)
                                   TO  SPA-GMN-SELNUM
      *
                   PERFORM 41011-INPUT-AREA-SEC
      *
                   MOVE    SPA-GMN-ALLIDOLST-MAX
                               TO  IDX-LST
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4101-ALLIDOLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力エリア処理
      *****************************************************************
       41011-INPUT-AREA-SEC            SECTION.
      *
           IF    ( SPA-GMN-TALLSTS (SPA-GMN-ALLIDOLST-SEL) =   4 )
               PERFORM 41011-DIRECT-UPD-KHNCHK-SEC
               IF    ( FLG-KHNERR  NOT =   ZERO )
                   GO  TO  41011-INPUT-AREA-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-TALLSTS (SPA-GMN-ALLIDOLST-SEL)
           WHEN    1
               PERFORM 41011-KHNERR-HEN-SEC
           WHEN    3
               CONTINUE
           WHEN    OTHER
               EVALUATE    SPA-NAI-TALLORDERID (SPA-GMN-ALLIDOLST-SEL)
               WHEN    NORDERID-NYUINID
               WHEN    NORDERID-TEISEIID
               WHEN    NORDERID-TAIINID
               WHEN    NORDERID-HENKOID
               WHEN    NORDERID-GAIHKID
               WHEN    NORDERID-GBACKID
               WHEN    NORDERID-FNORLID
               WHEN    NORDERID-FSPCLID
               WHEN    NORDERID-FSELEID
               WHEN    NORDERID-FSTOPID
               WHEN    NORDERID-FSPSLID
               WHEN    NORDERID-FLIQUIDID
               WHEN    NORDERID-IDODRID
               WHEN    NORDERID-IDOKAID
               WHEN    NORDERID-IDOBTUID
               WHEN    NORDERID-IDOBRMID
               WHEN    NORDERID-IDOALLID
               WHEN    NORDERID-DELNYUINID
               WHEN    NORDERID-ALLDELNYUINID
               WHEN    NORDERID-CANTAIINID
               WHEN    NORDERID-CANIDOID
               WHEN    NORDERID-GPTSTID
               WHEN    NORDERID-FOODID
               WHEN    NORDERID-PTEVALID
               WHEN    NORDERID-BUNBENID
               WHEN    NORDERID-CRENACCTID
               WHEN    NORDERID-SAGAKUID
                   PERFORM 41011-INPUT-AREA-HEN-SEC
               END-EVALUATE
           END-EVALUATE
      *
           .
       41011-INPUT-AREA-EXT.
           EXIT.
      *****************************************************************
      *    直接更新時基本項目チェック処理
      *****************************************************************
       41011-DIRECT-UPD-KHNCHK-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-KHNERR
      *
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-TALLHOSPNUM    (SPA-GMN-ALLIDOLST-SEL)
                                   TO  SODR-HOSPNUM
           MOVE    SPA-NAI-TALLKARTE-KEY (SPA-GMN-ALLIDOLST-SEL)
                                   TO  SODR-KARTE-KEY
           MOVE    SPA-NAI-TALLORDERNUM (SPA-GMN-ALLIDOLST-SEL)
                                   TO  SODR-ORDERNUM
           CALL    "ORCSODRS02"        USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
           IF    ( SODR-ERRFLG NOT =   ZERO )
      *
               MOVE    1           TO  FLG-KHNERR
      *
               PERFORM 41011-HEAD-AREA-HEN-SEC
      *
               MOVE    1           TO  SPA-NAI-DIRECT-KHNERR
      *
               INITIALIZE                  SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
      *         PERFORM 3101-PTIDOLST-SEC
      *
               MOVE    SODR-ERRCD        TO  ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC   TO  ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
      *
           END-IF
      *
           .
       41011-DIRECT-UPD-KHNCHK-EXT.
           EXIT.
      *****************************************************************
      *    基本項目エラー時編集処理
      *****************************************************************
       41011-KHNERR-HEN-SEC            SECTION.
      *
           PERFORM 41011-HEAD-AREA-HEN-SEC
      *
           INITIALIZE                  SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
                                       ERR-ODRITEM-REC
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           MOVE    ORDER-ERRCD1    TO  ERR-ORDER-ERRCD
           MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           IF    ( ORDER-KEYGENERATE   =   2 )
               MOVE    "1"         TO  SPA-NAI-DTYPE
           END-IF
           IF    ( SPA-NAI-DTYPE       NOT =   CONST-DTYPE-API )
            OR   ( WRK-ALLORDERID          =   NORDERID-NYUINID
                                            OR NORDERID-TEISEIID
                                            OR NORDERID-IDOALLID )
               MOVE    "1"         TO  SPA-NAI-INPUT-COMB-SRYKA
           END-IF
      *
      *
           PERFORM 900-ODRITEM-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ODRITEM   NOT =   ZERO )
      *
               IF    ( ERR-ODRITEM-MAX   >=  CONST-ODRITEM-MAX )
                   CONTINUE
               ELSE
                   IF    ( ODRITEM-ERRCD   NOT =   SPACE )
                    AND  ( ODRITEM-ERRCD   NOT =   CONST-ERRCD9001 )
                       COMPUTE ERR-ODRITEM-MAX =   ERR-ODRITEM-MAX + 1
                       MOVE    ODRITEM-REC TO  ERR-ODRITEM-OCC
                                                      (ERR-ODRITEM-MAX)
                   END-IF
               END-IF
      *
               PERFORM 900-ODRITEM-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *     PERFORM 3101-PTIDOLST-SEC
      *
           PERFORM 3101-ERRMSGLST-SEC
      *
           .
       41011-KHNERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       41011-INPUT-AREA-HEN-SEC        SECTION.
      *
           MOVE    SPA-NAI-TALLORDERID (SPA-GMN-ALLIDOLST-SEL)
                                   TO  WRK-ALLORDERID
      *
           PERFORM 41011-HEAD-AREA-HEN-SEC
      *
           INITIALIZE                  SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
                                       ERR-ORDER-ERRCD
                                       ERR-ODRITEM-REC
                                       ITEM-AREA
      *
           MOVE    ALL "9"         TO  SPA-NAI-STATUS-G
           MOVE    ZERO            TO  CNT-ORDER
           MOVE    ZERO            TO  CNT-DRCD
           MOVE    ZERO            TO  CNT-KSNCD
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           MOVE    ORDER-ERRCD1    TO  ERR-ORDER-ERRCD
           MOVE    ORDER-REC       TO  SPA-NAI-ORDER-DAT
           IF    ( ORDER-KEYGENERATE   =   2 )
               MOVE    "1"         TO  SPA-NAI-DTYPE
           END-IF
           IF    ( SPA-NAI-DTYPE       NOT =   CONST-DTYPE-API )
            OR   ( WRK-ALLORDERID          =   NORDERID-NYUINID
                                            OR NORDERID-TEISEIID
                                            OR NORDERID-IDOALLID )
               MOVE    "1"         TO  SPA-NAI-INPUT-COMB-SRYKA
           END-IF
      *
           PERFORM 410111-INPUT-AREA-HEN-SEC
      *
           PERFORM 900-ODRITEM-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ODRITEM     NOT =   ZERO )
      *
               IF    ( ODRITEM-STATUS      NOT =   3 )
      *
                   EVALUATE    WRK-ALLORDERID
                   WHEN    NORDERID-NYUINID
                   WHEN    NORDERID-TEISEIID
                       PERFORM 410111-NYUIN-HEN-SEC
                   WHEN    NORDERID-TAIINID
                       PERFORM 410111-TAIIN-HEN-SEC
                   WHEN    NORDERID-HENKOID
                       PERFORM 410111-HENKO-HEN-SEC
                   WHEN    NORDERID-GAIHKID
                   WHEN    NORDERID-GBACKID
                       PERFORM 410111-GAIHAKU-HEN-SEC
                   WHEN    NORDERID-FNORLID
                   WHEN    NORDERID-FSPCLID
                   WHEN    NORDERID-FSELEID
                   WHEN    NORDERID-FSTOPID
                   WHEN    NORDERID-FSPSLID
                   WHEN    NORDERID-FLIQUIDID
                       PERFORM 410111-SYOKUJI-HEN-SEC
                   WHEN    NORDERID-IDODRID
                       PERFORM 410111-DOCTOR-HEN-SEC
                   WHEN   NORDERID-IDOBTUID
                   WHEN   NORDERID-IDOBRMID
                   WHEN   NORDERID-IDOKAID
                       PERFORM 410111-IDOKABTUBRM-HEN-SEC
                   WHEN    NORDERID-IDOALLID
                       PERFORM 410111-IDOALL-HEN-SEC
                   WHEN    NORDERID-DELNYUINID
                   WHEN    NORDERID-ALLDELNYUINID
                       PERFORM 410111-DELNYUIN-HEN-SEC
                   WHEN    NORDERID-CANTAIINID
                       PERFORM 410111-CANTAIIN-HEN-SEC
                   WHEN    NORDERID-CANIDOID
                       PERFORM 410111-CANIDO-HEN-SEC
                   WHEN    NORDERID-GPTSTID
                       PERFORM 410111-GPTST-HEN-SEC
                   WHEN    NORDERID-FOODID
                       PERFORM 410111-FOOD-HEN-SEC
                   WHEN    NORDERID-PTEVALID
                       PERFORM 410111-PTEVAL-HEN-SEC
                   WHEN    NORDERID-BUNBENID
                       PERFORM 410111-BUNBEN-HEN-SEC
                   WHEN    NORDERID-CRENACCTID
                   WHEN    NORDERID-SAGAKUID
                   WHEN    NORDERID-TEISEIID
                       PERFORM 410111-OTHERS-HEN-SEC
                   END-EVALUATE
      *
               END-IF
      *
               IF    ( ERR-ODRITEM-MAX      >=   CONST-ODRITEM-MAX )
                   CONTINUE
               ELSE
                   IF    ( ODRITEM-ERRCD   NOT =   SPACE )
                    AND  ( ODRITEM-ERRCD   NOT =   CONST-ERRCD9001 )
                       COMPUTE ERR-ODRITEM-MAX =   ERR-ODRITEM-MAX + 1
                       MOVE    ODRITEM-REC TO  ERR-ODRITEM-OCC
                                                      (ERR-ODRITEM-MAX)
                   END-IF
               END-IF
      *
               PERFORM 900-ODRITEM-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 41011-DIRECT-UPD-CHK-SEC
      *
           PERFORM 410112-INPUT-AREA-HEN-SEC
      *
      *     PERFORM 3101-PTIDOLST-SEC
      *
           IF    ( SPA-NAI-STATUS-ORDER    =   ZERO )
               CONTINUE
           ELSE
               PERFORM 3101-ERRMSGLST-SEC
           END-IF
      *
           .
       41011-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       410111-INPUT-AREA-HEN-SEC       SECTION.
      *
           EVALUATE    WRK-ALLORDERID
           WHEN    NORDERID-NYUINID
      *
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-NYUINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUINYMD
      *
               MOVE    "1"             TO  SPA-GMN-SENTEIKBN
               MOVE    "1"             TO  SPA-GMN-KAIGOKBN
               MOVE    "0"             TO  SPA-GMN-TOKHIGAITOKBN
               MOVE    "2"             TO  SPA-GMN-NACCTKBN
               MOVE    "1"             TO  SPA-GMN-TEIKIKBN
               MOVE    "1"             TO  SPA-GMN-DISPKBN
      *
           WHEN    NORDERID-TEISEIID
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-NYUINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUINYMD
      *
           WHEN    NORDERID-TAIINID
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-TAIINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TAIINYMD
           WHEN    NORDERID-DELNYUINID
           WHEN    NORDERID-ALLDELNYUINID
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-NYUINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUINYMD
           WHEN    NORDERID-CANTAIINID
               MOVE    ORDER-SRYYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-SYMD        TO  SPA-NAI-TAIINYMD
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TAIINYMD
           END-EVALUATE
      *
           .
       410111-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院時編集処理
      *****************************************************************
       410111-NYUIN-HEN-SEC            SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-SHOKAIJMA
               PERFORM 4101111-SHOKAI-HEN-SEC
      *
           WHEN    NCLAIMID-BTUNUMJMA
               PERFORM 4101111-BTUNUM-HEN-SEC
      *
           WHEN    NCLAIMID-BRMNUMJMA
               PERFORM 4101111-BRMNUM-HEN-SEC
      *
           WHEN    NCLAIMID-SAGAKUJMA
               PERFORM 4101111-SAGAKU-HEN-SEC
      *
           WHEN    NCLAIMID-DRJMA
               PERFORM 4101111-DR-HEN-SEC
      *
           WHEN    NCLAIMID-TOKNYUINJMA
               PERFORM 4101111-TOKNYUIN-HEN-SEC
      *
           WHEN    NCLAIMID-SENTEIKBNJMA
               PERFORM 4101111-SENTEIKBN-HEN-SEC
      *
           WHEN    NCLAIMID-KAIGOKBNJMA
               PERFORM 4101111-KAIGOKBN-HEN-SEC
      *
           WHEN    NCLAIMID-TOKHIGAITOKBNJMA
               PERFORM 4101111-TOKHIGAITOKBN-HEN-SEC
      *
           WHEN    NCLAIMID-NACCTKBNJMA
               PERFORM 4101111-NACCTKBN-HEN-SEC
      *
           WHEN    NCLAIMID-KSNCDJMA
               PERFORM 4101111-KSNCD-HEN-SEC
      *
           WHEN    NCLAIMID-TEIKIKBNJMA
               PERFORM 4101111-TEIKIKBN-HEN-SEC
      *
           WHEN    NCLAIMID-DISPKBNJMA
               PERFORM 4101111-DISPKBN-HEN-SEC
      *
           WHEN    NCLAIMID-BUNBENKBNJMA
               PERFORM 4101111-BUNBENKBN-HEN-SEC
      *
           WHEN    NCLAIMID-RIYOUKBNJMA
               PERFORM 4101111-RIYOUKBN-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-NYUIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    退院時編集処理
      *****************************************************************
       410111-TAIIN-HEN-SEC            SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-TAIINCDJMA
               PERFORM 4101111-TAIINCD-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-TAIIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    変更時編集処理
      *****************************************************************
       410111-HENKO-HEN-SEC            SECTION.
      *
           COMPUTE CNT-ORDER   =   CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-DRJMA
               PERFORM 4101111-DR-HEN-SEC
      *
           WHEN    NCLAIMID-TEIKIKBNJMA
               PERFORM 4101111-TEIKIKBN-HEN-SEC
      *
           WHEN    NCLAIMID-DISPKBNJMA
               PERFORM 4101111-DISPKBN-HEN-SEC
      *
           WHEN    NCLAIMID-BUNBENKBNJMA
               PERFORM 4101111-BUNBENKBN-HEN-SEC
      *
           WHEN    NCLAIMID-RIYOUKBNJMA
               PERFORM 4101111-RIYOUKBN-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-HENKO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外泊時編集処理
      *****************************************************************
       410111-GAIHAKU-HEN-SEC          SECTION.
      *
           COMPUTE CNT-ORDER   =   CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-GAIHAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事時編集処理
      *****************************************************************
       410111-SYOKUJI-HEN-SEC          SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-SKJTIMEJMA
               PERFORM 4101111-SKJTIME-HEN-SEC
      *
           WHEN    NCLAIMID-NISSUJMA
               PERFORM 4101111-NISSU-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    担当医時編集処理
      *****************************************************************
       410111-DOCTOR-HEN-SEC           SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-DRJMA
               PERFORM 4101111-DR-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-DOCTOR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    転科転棟転室編集処理
      *****************************************************************
       410111-IDOKABTUBRM-HEN-SEC      SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-BTUNUMJMA
               PERFORM 4101111-BTUNUM-HEN-SEC
      *
           WHEN    NCLAIMID-BRMNUMJMA
               PERFORM 4101111-BRMNUM-HEN-SEC
      *
           WHEN    NCLAIMID-SAGAKUJMA
               PERFORM 4101111-SAGAKU-HEN-SEC
      *
           WHEN    NCLAIMID-DRJMA
               PERFORM 4101111-DR-HEN-SEC
      *
           WHEN    NCLAIMID-TOKNYUINJMA
               PERFORM 4101111-TOKNYUIN-HEN-SEC
      *
           WHEN    NCLAIMID-TOKHIGAITOKBNJMA
               PERFORM 4101111-TOKHIGAITOKBN-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-IDOKABTUBRM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    転科転棟転室編集処理（APIより）
      *****************************************************************
       410111-IDOALL-HEN-SEC           SECTION.
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-BTUNUMJMA
               PERFORM 4101111-BTUNUM-HEN-SEC
      *
           WHEN    NCLAIMID-BRMNUMJMA
               PERFORM 4101111-BRMNUM-HEN-SEC
      *
           WHEN    NCLAIMID-SAGAKUJMA
               PERFORM 4101111-SAGAKU-HEN-SEC
      *
           WHEN    NCLAIMID-SENTEIKBNJMA
               PERFORM 4101111-SENTEIKBN-HEN-SEC
      *
           WHEN    NCLAIMID-DRJMA
               PERFORM 4101111-DR-HEN-SEC
      *
           WHEN    NCLAIMID-TOKNYUINJMA
               PERFORM 4101111-TOKNYUIN-HEN-SEC
      *
           WHEN    NCLAIMID-TOKHIGAITOKBNJMA
               PERFORM 4101111-TOKHIGAITOKBN-HEN-SEC
      *
           WHEN    NCLAIMID-NACCTKBNJMA
               PERFORM 4101111-NACCTKBN-HEN-SEC
      *
           WHEN    NCLAIMID-KSNCDJMA
               PERFORM 4101111-KSNCD-HEN-SEC
      *
           WHEN    NCLAIMID-BUNBENKBNJMA
               PERFORM 4101111-BUNBENKBN-HEN-SEC
      *
           WHEN    NCLAIMID-RIYOUKBNJMA
               PERFORM 4101111-RIYOUKBN-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-IDOALL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院取消時編集処理
      *****************************************************************
       410111-DELNYUIN-HEN-SEC         SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-DELNYUIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    退院取消時編集処理
      *****************************************************************
       410111-CANTAIIN-HEN-SEC    SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-CANTAIIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    異動取消時編集処理
      *****************************************************************
       410111-CANIDO-HEN-SEC      SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-CANIDO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外泊等編集処理
      *****************************************************************
       410111-GPTST-HEN-SEC      SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-GPTST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事編集処理
      *****************************************************************
       410111-FOOD-HEN-SEC        SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-FOOD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    医療区分・ＡＤＬ点数登録編集処理
      *****************************************************************
       410111-PTEVAL-HEN-SEC      SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-PTEVAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    出産育児一時金情報登録編集処理
      *****************************************************************
       410111-BUNBEN-HEN-SEC      SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-BUNBEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    その他オーダー編集処理
      *****************************************************************
       410111-OTHERS-HEN-SEC      SECTION.
      *
           COMPUTE CNT-ORDER   =  CNT-ORDER    +   1
      *
           PERFORM 410111-INPUT-COMMON-HEN-SEC
      *
           .
       410111-OTHERS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    共通入力項目編集処理
      *****************************************************************
       410111-INPUT-COMMON-HEN-SEC     SECTION.
      *
           EVALUATE    ODRITEM-ITEMID
      *
           WHEN    NCLAIMID-NYUINKAJMA
               PERFORM 4101111-NYUINKA-HEN-SEC
      *
           WHEN    NCLAIMID-HKNCOMBIJMA
               PERFORM 4101111-HKNCOMBI-HEN-SEC
      *
           END-EVALUATE
      *
           .
       410111-INPUT-COMMON-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       410112-INPUT-AREA-HEN-SEC       SECTION.
      *
           IF    ( SPA-NAI-INPUT-COMB-SRYKA    =   "1" )
               PERFORM 4101112-NYUINKAMEI-HEN-SEC
               PERFORM 4101112-HKNCOMBIMEI-HEN-SEC
           END-IF
      *
           EVALUATE    WRK-ALLORDERID
      *
           WHEN    NORDERID-NYUINID
           WHEN    NORDERID-TEISEIID
      *
               PERFORM 4101112-SHOKAI-HEN-SEC
               PERFORM 4101112-BTUNUM-HEN-SEC
               PERFORM 4101112-SAGAKUMEI-HEN-SEC
               PERFORM 4101112-SHOKAIMEI-HEN-SEC
               PERFORM 4101112-DRMEI-HEN-SEC
               PERFORM 4101112-TOKNYUINMEI-HEN-SEC
               PERFORM 4101112-SENTEIKBN-HEN-SEC
               PERFORM 4101112-KAIGOKBN-HEN-SEC
               PERFORM 4101112-RIYOUKBN-HEN-SEC
               PERFORM 4101112-BUNBENKBN-HEN-SEC
               PERFORM 4101112-NACCTKBN-HEN-SEC
               PERFORM 4101112-KSNCD-HEN-SEC
               PERFORM 4101112-TEIKIKBN-HEN-SEC
               PERFORM 4101112-DISPKBN-HEN-SEC
      *
           WHEN    NORDERID-TAIINID
      *
               PERFORM 4101112-TAIINCDMEI-HEN-SEC
           WHEN    NORDERID-HENKOID
               PERFORM 4101112-DRMEI-HEN-SEC
               PERFORM 4101112-RIYOUKBN-HEN-SEC
               PERFORM 4101112-BUNBENKBN-HEN-SEC
               PERFORM 4101112-TEIKIKBN-HEN-SEC
               PERFORM 4101112-DISPKBN-HEN-SEC
      *
           WHEN    NORDERID-GAIHKID
           WHEN    NORDERID-GAIHKID
           WHEN    NORDERID-GBACKID
               CONTINUE
           WHEN    NORDERID-FNORLID
           WHEN    NORDERID-FSPCLID
           WHEN    NORDERID-FSELEID
           WHEN    NORDERID-FSTOPID
           WHEN    NORDERID-FSPSLID
           WHEN    NORDERID-FLIQUIDID
      *
               PERFORM 4101112-SKJTIME-HEN-SEC
           WHEN    NORDERID-IDODRID
      *
               PERFORM 4101112-DRMEI-HEN-SEC
           WHEN    NORDERID-IDOKAID
           WHEN    NORDERID-IDOBTUID
           WHEN    NORDERID-IDOBRMID
      *
               PERFORM 4101112-BTUNUM-HEN-SEC
               PERFORM 4101112-SAGAKUMEI-HEN-SEC
               PERFORM 4101112-DRMEI-HEN-SEC
               PERFORM 4101112-TOKNYUINMEI-HEN-SEC
           WHEN    NORDERID-IDOALLID
               PERFORM 4101112-BTUNUM-HEN-SEC
               PERFORM 4101112-SAGAKUMEI-HEN-SEC
               PERFORM 4101112-SENTEIKBN-HEN-SEC
               PERFORM 4101112-RIYOUKBN-HEN-SEC
               PERFORM 4101112-BUNBENKBN-HEN-SEC
               PERFORM 4101112-DRMEI-HEN-SEC
               PERFORM 4101112-TOKNYUINMEI-HEN-SEC
               PERFORM 4101112-NACCTKBN-HEN-SEC
               PERFORM 4101112-KSNCD-HEN-SEC
           WHEN    NORDERID-DELNYUINID
           WHEN    NORDERID-ALLDELNYUINID
           WHEN    NORDERID-CANTAIINID
           WHEN    NORDERID-CANIDOID
           WHEN    NORDERID-GPTSTID
           WHEN    NORDERID-FOODID
           WHEN    NORDERID-PTEVALID
           WHEN    NORDERID-BUNBENID
           WHEN    NORDERID-CRENACCTID
           WHEN    NORDERID-SAGAKUID
               CONTINUE
           END-EVALUATE
      *
           .
       410112-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回入院日編集処理
      *****************************************************************
       41011-SHONYUINYMD-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-LBL-SHONYUINYMD
           MOVE    ZERO                TO  WRK-SHONUM
      *
           INSPECT ITEM-LASTNYINYMD REPLACING  ALL "-" BY  "."
           MOVE    ITEM-LASTNYINYMD    TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF    ( FLG-HIZUKE-ERR  =   ZERO )
               MOVE    WRK-SYMD        TO  ITEM-LASTNYINYMD
               INITIALIZE                  PTNYUINRRK-REC
               IF    ( WRK-ALLORDERID  =   NORDERID-TEISEIID )
                   PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
                   MOVE    ITEM-LASTNYINYMD
                                           TO  PTNYUINRRK-NYUINYMD
                   PERFORM 900-PTNYUINRRK-KEY79-SEL-SEC
               ELSE
                   MOVE    SPA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
                   MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
                   MOVE    ITEM-LASTNYINYMD
                                           TO  PTNYUINRRK-NYUINYMD
                   PERFORM 900-PTNYUINRRK-KEY79-SEL-SEC
               END-IF
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-SHONUM
                                       TO  WRK-SHONUM
                   PERFORM 900-PTNYUINRRK-KEY80-SEL-SEC
                   IF    ( FLG-PTNYUINRRK  =   ZERO )
                       MOVE    PTNYUINRRK-NYUINYMD     TO   WRK-YMD
                       PERFORM 800-SEIWA-HEN-SEC
                       STRING  "入院日 "   DELIMITED   BY  SIZE
                               WRK-HENYMDG DELIMITED   BY  SIZE
                       INTO    WRK-LBL-SHONYUINYMD
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           .
       41011-SHONYUINYMD-EXT.
           EXIT.
      *****************************************************************
      *    入院科編集処理
      *****************************************************************
       4101111-NYUINKA-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-NYUINKA
           MOVE    ODRITEM-ITEM        TO  SPA-GMN-NYUINKA-G
      *
           .
       4101111-NYUINKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ処理編集処理
      *****************************************************************
       4101111-HKNCOMBI-HEN-SEC        SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-HKNCOMBI
           IF    ( ODRITEM-ITEM        =   "ERR" )
               MOVE    SPACE           TO  ODRITEM-ITEM
           END-IF
           MOVE    ODRITEM-ITEM        TO  SPA-GMN-HKNCOMBI-G
                                           SPA-NAI-HKNCOMBI-MOT
      *
           .
       4101111-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回区分編集処理
      *****************************************************************
       4101111-SHOKAI-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-SHOKAI
           IF    ( ODRITEM-ITEM        =   "01" )
               MOVE    "2"             TO  SPA-GMN-SHOKAI
               MOVE    ODRITEM-MEMO   TO  ITEM-LASTNYINYMD
           END-IF
      *
           .
       4101111-SHOKAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号編集処理
      *****************************************************************
       4101111-BTUNUM-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-BTUNUM
           MOVE    ODRITEM-ITEM        TO  SPA-GMN-BTUNAME-G
                                           SPA-NAI-BTUNUM-MOT
      *
           .
       4101111-BTUNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       4101111-BRMNUM-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS   TO  SPA-NAI-STS-BRMNUM
           MOVE    ODRITEM-ITEM     TO  SPA-GMN-BRMNUM
           INITIALIZE              ORCSBRMNUMAREA
           MOVE    "1"             TO  SBRMNUM-IN-PROCKBN
           MOVE    ODRITEM-ITEM    TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
           IF    ( SBRMNUM-RC      =   ZERO )
               MOVE    SBRMNUM-OT-BRMNUM
                                    TO  SPA-NAI-BRMNUM-MOT
           END-IF
      *
           .
       4101111-BRMNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額編集処理
      *****************************************************************
       4101111-SAGAKU-HEN-SEC          SECTION.
      *
           MOVE    ODRITEM-STATUS   TO  SPA-NAI-STS-SAGAKU
           MOVE    ODRITEM-ITEM     TO  SPA-GMN-SAGAKU-G
      *
           .
       4101111-SAGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    担当医編集処理
      *****************************************************************
       4101111-DR-HEN-SEC              SECTION.
      *
           IF    ( CNT-DRCD    <   3 )
               COMPUTE CNT-DRCD    =   CNT-DRCD    +   1
               MOVE    ODRITEM-STATUS
                                       TO  SPA-NAI-STS-DR(CNT-DRCD)
               MOVE    ODRITEM-ITEM (2:) TO  SPA-GMN-DR-G  (CNT-DRCD)
           END-IF
      *
           .
       4101111-DR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    退院事由編集処理
      *****************************************************************
       4101111-TAIINCD-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS   TO  SPA-NAI-STS-TAIINCD
           MOVE    ODRITEM-ITEM     TO  SPA-GMN-TAIINCD-G
      *
           .
       4101111-TAIINCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料編集処理
      *****************************************************************
       4101111-TOKNYUIN-HEN-SEC        SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-TOKNYUIN
           MOVE    ODRITEM-ITEM        TO  SPA-NAI-TOKCD-G
      *
           .
       4101111-TOKNYUIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事時間編集処理
      *****************************************************************
       4101111-SKJTIME-HEN-SEC         SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-SKJTIME
           MOVE    ODRITEM-ITEM        TO  SPA-GMN-SKJTIME-G
      *
           .
       4101111-SKJTIME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理
      *****************************************************************
       4101111-NISSU-HEN-SEC           SECTION.
      *
           MOVE    ODRITEM-STATUS      TO  SPA-NAI-STS-NISSU
           MOVE    ODRITEM-MEMO        TO  SPA-GMN-NISSU
      *
      *
           .
       4101111-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分編集処理
      *****************************************************************
       4101111-SENTEIKBN-HEN-SEC       SECTION.
      *
           IF    ( ODRITEM-ITEM        NOT =   SPACE )
               MOVE    ODRITEM-ITEM    TO  SPA-GMN-SENTEIKBN
           END-IF
      *
           .
       4101111-SENTEIKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    介護区分編集処理
      *****************************************************************
       4101111-KAIGOKBN-HEN-SEC        SECTION.
      *
           IF    ( ODRITEM-ITEM        NOT =   SPACE )
               MOVE    ODRITEM-ITEM    TO  SPA-GMN-KAIGOKBN
           END-IF
      *
           .
       4101111-KAIGOKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料算定要件非該当区分編集処理
      *****************************************************************
       4101111-TOKHIGAITOKBN-HEN-SEC   SECTION.
      *
           IF    ( ODRITEM-ITEM        NOT =   SPACE )
               MOVE    ODRITEM-ITEM    TO  SPA-GMN-TOKHIGAITOKBN
           END-IF
      *
           .
       4101111-TOKHIGAITOKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計区分編集処理
      *****************************************************************
       4101111-NACCTKBN-HEN-SEC        SECTION.
      *
           IF    ( ODRITEM-ITEM        NOT =   SPACE )
               MOVE    ODRITEM-ITEM    TO  SPA-GMN-NACCTKBN
           END-IF
      *
           .
       4101111-NACCTKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院加算編集処理
      *****************************************************************
       4101111-KSNCD-HEN-SEC              SECTION.
      *
           IF    ( CNT-KSNCD    <   3 )
               COMPUTE CNT-KSNCD =   CNT-KSNCD    +   1
               MOVE    ODRITEM-ITEM
                                   TO  SPA-NAI-KSNCD-SRYCD (CNT-KSNCD)
           END-IF
      *
           .
       4101111-KSNCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分編集処理
      *****************************************************************
       4101111-TEIKIKBN-HEN-SEC        SECTION.
      *
           IF    ( ODRITEM-ITEM        NOT =   SPACE )
               MOVE    ODRITEM-ITEM    TO  SPA-GMN-TEIKIKBN
           END-IF
      *
           .
       4101111-TEIKIKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示区分編集処理
      *****************************************************************
       4101111-DISPKBN-HEN-SEC         SECTION.
      *
           IF    ( ODRITEM-ITEM        NOT =   SPACE )
               MOVE    ODRITEM-ITEM    TO  SPA-GMN-DISPKBN
           END-IF
      *
           .
       4101111-DISPKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分編集処理
      *****************************************************************
       4101111-BUNBENKBN-HEN-SEC       SECTION.
      *
           IF    ( ODRITEM-ITEM        NOT =   SPACE )
               MOVE    ODRITEM-ITEM    TO  SPA-GMN-BUNBENKBN
           END-IF
      *
           .
       4101111-BUNBENKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分編集処理
      *****************************************************************
       4101111-RIYOUKBN-HEN-SEC        SECTION.
      *
           IF    ( ODRITEM-ITEM        NOT =   SPACE )
               MOVE    ODRITEM-ITEM    TO  SPA-GMN-RIYOUKBN
           END-IF
      *
           .
       4101111-RIYOUKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回編集処理
      *****************************************************************
       4101112-SHOKAI-HEN-SEC          SECTION.
      *
           IF    ( SPA-GMN-SHOKAI  =   "2" )
               PERFORM 41011-SHONYUINYMD-SEC
               MOVE    WRK-SHONUM      TO  SPA-GMN-SHONUM
               MOVE    WRK-LBL-SHONYUINYMD
                                       TO  SPA-GMN-LBL-SHONYUINYMD
           ELSE
               MOVE    "1"             TO  SPA-GMN-SHOKAI
           END-IF
      *
           .
       4101112-SHOKAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病棟編集処理
      *****************************************************************
       4101112-BTUNUM-HEN-SEC          SECTION.
      *
           PERFORM 3101-BTUDEF-SEC
           PERFORM 3101-BTUNAMELST-SET-SEC
           MOVE    SPA-GMN-BTUNAME     TO  WRK-CMB-CD
           PERFORM 4200-BTUNAMELST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-BTUNAME-G
               MOVE    SPA-NAI-BTUDEF-KHNSRYCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-KHNSRYCD
               MOVE    SPA-NAI-BTUDEF-SBT (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-SBT
               MOVE    SPA-NAI-BTUDEF-BTUTOK (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BTUTOK
               MOVE    SPA-NAI-BTUDEF-BRMTOK (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BRMTOK
               MOVE    SPA-NAI-BTUDEF-BTUKHNKBNCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BTUKHNKBNCD
               MOVE    SPA-NAI-BTUDEF-BRMKHNKBNCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BRMKHNKBNCD
           END-IF
      *
           .
       4101112-BTUNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額名編集処理
      *****************************************************************
       4101112-SAGAKUMEI-HEN-SEC       SECTION.
      *
           PERFORM 3101-SAGAKULST-SET-SEC
           MOVE    SPA-GMN-SAGAKU      TO  WRK-CMB-CD
           PERFORM 4200-SAGAKULST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SAGAKU-G
           END-IF
      *
           .
       4101112-SAGAKUMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院科名編集処理
      *****************************************************************
       4101112-NYUINKAMEI-HEN-SEC      SECTION.
      *
           PERFORM 3101-NYUINKALST-SET-SEC
           MOVE    SPA-GMN-NYUINKA     TO  WRK-CMB-CD
           PERFORM 4200-NYUINKALST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUINKA-G
           END-IF
      *
           .
       4101112-NYUINKAMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回名編集処理
      *****************************************************************
       4101112-SHOKAIMEI-HEN-SEC       SECTION.
      *
           PERFORM 3101-SHOKAILST-SET-SEC
           MOVE    SPA-GMN-SHOKAI      TO  WRK-CMB-CD
           PERFORM 4200-SHOKAILST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SHOKAI-G
           END-IF
      *
           .
       4101112-SHOKAIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    担当医名編集処理
      *****************************************************************
       4101112-DRMEI-HEN-SEC           SECTION.
      *
           PERFORM 3101-DRLST-SET-SEC
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
               MOVE    SPA-GMN-DR (IDX1)   TO  WRK-CMB-CD
               PERFORM 4200-DRLST-CHK-SEC
               IF    ( WRK-CMB-ITM NOT =   SPACE )
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-DR-G (IDX1)
               END-IF
           END-PERFORM
      *
           .
       4101112-DRMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名編集処理
      *****************************************************************
       4101112-HKNCOMBIMEI-HEN-SEC     SECTION.
      *
           PERFORM 3101-HKNCOMBILST-SET-SEC
           MOVE    SPA-GMN-HKNCOMBI    TO  WRK-CMB-CD
           PERFORM 4200-HKNCOMBILST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-HKNCOMBI-G
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-HKNNUM
           END-IF
      *
           .
       4101112-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料名編集処理
      *****************************************************************
       4101112-TOKNYUINMEI-HEN-SEC     SECTION.
      *
           PERFORM 3101-TOKNYUINLST-SET-SEC
           MOVE    SPA-NAI-TOKCD-G     TO WRK-CMB-CD
           PERFORM 4201-TOKNYUINLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TOKNYUIN-G
               MOVE    SPA-GMN-TOKNYUINL2 (WRK-CMB-IDX)
                                   TO  SPA-NAI-TOKUTEI-G
               MOVE    SPA-GMN-TOKNYUINL-SRYCD (WRK-CMB-IDX)
                                   TO  SPA-NAI-TOKSRYCD
               MOVE    SPA-GMN-TOKNYUINL-NASRYCD (WRK-CMB-IDX)
                                   TO  SPA-NAI-NATOKSRYCD
           END-IF
      *
           PERFORM 4101-TOKHIGAITOKBN-CHK-SEC
      *
           IF    ( SPA-GMN-TOKNYUINMEI NOT =   SPACE )
               IF    ( SPA-NAI-NATOKSRYCD  NOT =   SPACE )
                AND  ( SPA-GMN-TOKHIGAITOKBN   =   "1" )
                   MOVE    SPA-NAI-KJNYMD      TO  WRK-TNS-KJNYMD
                   MOVE    SPA-NAI-NATOKSRYCD  TO  WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME            TO  SPA-GMN-SNTNYUIN
                   MOVE    TNS-SRYCD           TO  SPA-NAI-SNTSRYCD
               ELSE
                   MOVE    SPA-GMN-TOKNYUINMEI TO  SPA-GMN-SNTNYUIN
                   MOVE    SPA-NAI-TOKSRYCD    TO  SPA-NAI-SNTSRYCD
               END-IF
           ELSE
               MOVE    SPA-NAI-KHNNYUIN    TO  SPA-GMN-SNTNYUIN
               MOVE    SPA-NAI-KHNSRYCD    TO  SPA-NAI-SNTSRYCD
           END-IF
      *
           .
       4101112-TOKNYUINMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    退院事由名編集処理
      *****************************************************************
       4101112-TAIINCDMEI-HEN-SEC      SECTION.
      *
           PERFORM 3101-TAIINCDLST-SET-SEC
           MOVE    SPA-GMN-TAIINCD     TO  WRK-CMB-CD
           PERFORM 4200-TAIINCDLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TAIINCD-G
           END-IF
      *
           .
       4101112-TAIINCDMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事時間名編集処理
      *****************************************************************
       4101112-SKJTIME-HEN-SEC         SECTION.
      *
           PERFORM 3101-SKJTIMELST-SET-SEC
           MOVE    SPA-GMN-SKJTIME     TO  WRK-CMB-CD
           PERFORM 4200-SKJTIMELST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJTIME-G
           END-IF
      *
           .
       4101112-SKJTIME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分編集処理
      *****************************************************************
       4101112-SENTEIKBN-HEN-SEC       SECTION.
      *
           PERFORM 3101-SENTEIKBNLST-SET-SEC
           MOVE    SPA-GMN-SENTEIKBN   TO  WRK-CMB-CD
           PERFORM 4200-SENTEIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SENTEIKBN-G
           END-IF
      *
           .
       4101112-SENTEIKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    介護区分編集処理
      *****************************************************************
       4101112-KAIGOKBN-HEN-SEC       SECTION.
      *
           PERFORM 3101-KAIGOKBNLST-SET-SEC
           MOVE    SPA-GMN-KAIGOKBN   TO  WRK-CMB-CD
           PERFORM 4200-KAIGOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-KAIGOKBN-G
           END-IF
      *
           .
       4101112-KAIGOKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分編集処理
      *****************************************************************
       4101112-BUNBENKBN-HEN-SEC       SECTION.
      *
           PERFORM 3101-BUNBENKBNLST-SET-SEC
           MOVE    SPA-GMN-BUNBENKBN   TO  WRK-CMB-CD
           PERFORM 4200-BUNBENKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-BUNBENKBN-G
           END-IF
      *
           .
       4101112-BUNBENKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分編集処理
      *****************************************************************
       4101112-RIYOUKBN-HEN-SEC       SECTION.
      *
           PERFORM 3101-RIYOUKBNLST-SET-SEC
           MOVE    SPA-GMN-RIYOUKBN   TO  WRK-CMB-CD
           PERFORM 4200-RIYOUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-RIYOUKBN-G
           END-IF
      *
           .
       4101112-RIYOUKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計区分編集処理
      *****************************************************************
       4101112-NACCTKBN-HEN-SEC       SECTION.
      *
           PERFORM 3101-NACCTKBNLST-SET-SEC
           MOVE    SPA-GMN-NACCTKBN   TO  WRK-CMB-CD
           PERFORM 4200-NACCTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NACCTKBN-G
           END-IF
      *
           .
       4101112-NACCTKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院加算編集処理
      *****************************************************************
       4101112-KSNCD-HEN-SEC       SECTION.
      *
           PERFORM 3101-KSNCDLST-SET-SEC
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
               MOVE    SPA-NAI-KSNCD-SRYCD (IDX1)
                                   TO  WRK-CMB-CD
               PERFORM 4201-KSNCDLST-CHK-SEC
               IF    ( WRK-CMB-ITM NOT =   SPACE )
                   MOVE    WRK-CMB-ITM
                                   TO  SPA-GMN-KSNCD-G (IDX1)
                   MOVE    WRK-CMB-ITM2
                                   TO  SPA-NAI-KSNCD-SRYCD (IDX1)
               ELSE
                   MOVE    SPACE   TO  SPA-GMN-KSNCD-G (IDX1)
                   MOVE    SPA-NAI-KSNCD-SRYCD (IDX1)
                                   TO  SPA-GMN-KSNCD-G (IDX1)(2:)
                   MOVE    SPACE   TO  SPA-NAI-KSNCD-SRYCD (IDX1)
               END-IF
           END-PERFORM
      *
           .
       4101112-KSNCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分編集処理
      *****************************************************************
       4101112-TEIKIKBN-HEN-SEC       SECTION.
      *
           PERFORM 3101-TEIKIKBNLST-SET-SEC
           MOVE    SPA-GMN-TEIKIKBN   TO  WRK-CMB-CD
           PERFORM 4200-TEIKIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TEIKIKBN-G
           END-IF
      *
           .
       4101112-TEIKIKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示区分編集処理
      *****************************************************************
       4101112-DISPKBN-HEN-SEC       SECTION.
      *
           PERFORM 3101-DISPKBNLST-SET-SEC
           MOVE    SPA-GMN-DISPKBN   TO  WRK-CMB-CD
           PERFORM 4200-DISPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-DISPKBN-G
           END-IF
      *
           .
       4101112-DISPKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    直接更新時入院科保険組合せ設定処理
      *****************************************************************
       41011-DIRECT-UPD-CHK-SEC        SECTION.
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           IF    ( SPA-NAI-STATUS-ORDER    =   4 )
               IF    ( SPA-NAI-STS-NYUINKA =   9 )
                   MOVE    ORDER-SRYKA     TO  SPA-GMN-NYUINKA
               END-IF
      *
               IF    ( SPA-NAI-STS-HKNCOMBI    =   9 )
                   MOVE    ORDER-HKNCOMBI  TO  SPA-GMN-HKNCOMBI
               END-IF
           END-IF
      *
           .
       41011-DIRECT-UPD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者基本情報編集処理
      *****************************************************************
       41011-HEAD-AREA-HEN-SEC         SECTION.
      *
           MOVE    SPA-GMN-ALLIDOLST-SEL
                                       TO  IDX-HEAD
      *
           INITIALIZE                  SPA-GMN-HEAD-AREA
                                       SPA-NAI-HEAD-AREA
      *
           MOVE    SPA-NAI-TALLKARTE-KEY (IDX-HEAD)
                                       TO  SPA-NAI-KARTE-KEY
           MOVE    SPA-NAI-TALLORDERNUM (IDX-HEAD)
                                       TO  SPA-NAI-ORDERNUM
           MOVE    SPA-GMN-TALLSTS (IDX-HEAD)
                                       TO  SPA-NAI-STATUS-ORDER
      *
           MOVE    SPA-NAI-TALLPTID (IDX-HEAD)  TO  WRK-PTINF-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPA-NAI-TALLIDOYMD (IDX-HEAD)
                                       TO  SPA-NAI-HIDOYMD
                                           SPA-NAI-KJNYMD
      *
           MOVE    SPA-NAI-TALLORDERID(IDX-HEAD)
                                       TO  SPA-NAI-ORDERID
      *
           MOVE    SPA-NAI-TALLPTID (IDX-HEAD)
                                       TO  SPA-NAI-PTID
      *
           MOVE    SPA-GMN-TALLPTNUM (IDX-HEAD)
                                       TO  SPA-GMN-PTNUM
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *
           MOVE    PTINF-SEX           TO  SPA-NAI-SEX
      *
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"            TO  SPA-GMN-SEX
           WHEN    "2"
               MOVE    "女"            TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
           MOVE    WRK-SYMD            TO  SPA-NAI-BIRTHDAY
      *
           IF    ( SPA-NAI-BIRTHDAY    NOT =   ZERO )
               INITIALIZE ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM          TO  AGECHK-HOSPNUM
               MOVE    SPA-NAI-PTID        TO  AGECHK-PTID
               MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
               MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
               CALL    "ORCSAGECHK"        USING   ORCSAGECHKAREA
                                                   SPA-AREA
               IF    ( AGECHK-RC       =   ZERO )
                   IF    ( AGECHK-NENREI2-YY   =   ZERO )
                       MOVE    AGECHK-NENREI2-MM
                                           TO  WRK-AGE-Z
                       MOVE    "ヶ月"      TO  WRK-AGE-MEI
                   ELSE
                       MOVE    AGECHK-NENREI2-YY
                                           TO  WRK-AGE-Z
                       MOVE    "才"        TO  WRK-AGE-MEI
                   END-IF
               END-IF
      *
               MOVE    WRK-AGE-G   TO  SPA-GMN-AGE
      *
           END-IF
      *
           IF    ( SPA-NAI-HIDOYMD  NOT =   ZERO )
               PERFORM 900-PTNYUINRRK-KEY42-SEL-SEC
           END-IF
      *
           IF    ( FLG-PTNYUINRRK      =   ZERO )
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   CONTINUE
               ELSE
                   MOVE     PTNYUINRRK-NYUINKA TO  SPA-NAI-HSRYKA
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    "1005"              TO  SYS-KANRICD
                   MOVE     PTNYUINRRK-NYUINKA TO  SYS-KBNCD
                   MOVE     SPA-NAI-HIDOYMD    TO  SYS-STYUKYMD
                                                   SYS-EDYUKYMD
                   PERFORM  900-SYSKANRI-KEY10-SEL-SEC
                   MOVE     SYSKANRI-REC       TO  SYS-1005-REC
                   MOVE     SYS-1005-SRYKANAME1    TO  SPA-GMN-HSRYKA
      *
                   MOVE    PTNYUINRRK-HKNCOMBINUM
                                               TO  SPA-NAI-HHKNCOMBI
      *
                   MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
                   INITIALIZE                      WRK-SPA-KYOTUKEY
                                                   WRK-SPA-KYOTU-SET
                                                   ORCSHKNSETAREA
      *
                   MOVE    SPA-NAI-HIDOYMD     TO  WRK-SPA-SRYYMD
                   MOVE    SPA-NAI-HSRYKA      TO  WRK-SPA-SRYKA
                   MOVE    "1"                 TO  WRK-SPA-NYUGAIKBN
                   MOVE    SPA-NAI-PTID        TO  WRK-SPA-PTID
                   MOVE    SPA-NAI-BIRTHDAY    TO  WRK-SPA-BIRTHDAY
                   MOVE    PTNYUINRRK-HKNCOMBINUM
                                               TO  WRK-SPA-HKNCOMBINUM
                   MOVE    "1"                 TO  ORCSHKN-KBN
                   CALL    "ORCSHKNSET"    USING   WRK-SPA-AREA
                                                   ORCSHKNSETAREA
                   IF    ( ORCSHKN-ERRCD       =   ZERO )
                       IF    ( ORCSHKN-IDX    >=   1 )
                        AND  ( ORCSHKN-IDX    <=   ORCSHKN-HKN-MAX )
                           MOVE    ORCSHKN-GMN-HKNCOMBIMEI (ORCSHKN-IDX)
                                               TO  SPA-GMN-HHKNCOMBI
                       END-IF
                   END-IF
      *
                   PERFORM 900-BUNBEN-KEY3-SEL-SEC
                   IF    ( FLG-BUNBEN           =  ZERO )
                       MOVE    BUNBEN-BUNBENKBN TO SPA-NAI-BUNBENKBN
                   END-IF
      *
               END-IF
           END-IF
      *
           PERFORM 900-PTNYUINRRK-KEY63-SEL-SEC
           IF    ( FLG-PTNYUINRRK      =   ZERO )
               EVALUATE    TRUE
               WHEN  ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
                   MOVE    "退院日"        TO  SPA-GMN-LBL-LASTIDOYMD
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-LASTIDOYMD
                   MOVE    WRK-SYMD            TO  SPA-NAI-LASTIDOYMD
                   IF    ( PTNYUINRRK-JTIKBN       =   "6" )
                    AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                       MOVE    "*"         TO  SPA-GMN-LASTIDOYMD (10:)
                   END-IF
               WHEN  OTHER
                   MOVE    "前回異動日"        TO SPA-GMN-LBL-LASTIDOYMD
                   IF    ( PTNYUINRRK-RRKEDANUM    >   1 )
                       MOVE    PTNYUINRRK-TENNYUYMD    TO  WRK-YMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-LASTIDOYMD
                       MOVE    WRK-SYMD        TO  SPA-NAI-LASTIDOYMD
                   END-IF
               END-EVALUATE
      *
               MOVE    PTNYUINRRK-NYUINYMD     TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG             TO  SPA-GMN-HNYUINYMD
               MOVE    WRK-SYMD                TO  SPA-NAI-HNYUINYMD
               PERFORM 900-PTNYUINRRK-KEY64-SEL-SEC
               IF    ( FLG-PTNYUINRRK      =   ZERO )
                   IF    ( PTNYUINRRK-JTIKBN       =   "6" )
                    AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                       MOVE    PTNYUINRRK-NYUINYMD
                                               TO  WRK-YMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-HNYUINYMD
                       MOVE    WRK-SYMD        TO  SPA-NAI-HNYUINYMD
                   END-IF
               END-IF
      *
           END-IF
      *
           MOVE    SPA-NAI-ORDERID     TO  WRK-CMB-CD
           PERFORM 4200-ORDERID-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IDOJIYU
      *
           IF    ( SPA-NAI-PTID        NOT =   ZERO )
               MOVE    SPA-GMN-PTNUM   TO  SPA-LAST-PTNUM
               MOVE    SPA-NAI-PTID    TO  SPA-LAST-PTID
           END-IF
      *
           .
       41011-HEAD-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    I06-SELNUM      TO  SPA-GMN-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( SPA-GMN-SELNUM  NOT =   ZERO )
               MOVE    IDX-LST     TO  SPA-GMN-ALLIDOLST-SEL
                                       I06-ALLIDOLST-ROW
               MOVE    2           TO  FLG-ALLIDOLST-ROW
               PERFORM 41011-INPUT-AREA-HEN-SEC
               MOVE    ZERO        TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索項目チェック処理
      *****************************************************************
       4100-SELITEMS-CHK-SEC           SECTION.
      *
           MOVE    SPA-I06         TO  TSPA-I06
      *
           MOVE    I06-SELUKEYMD   TO  SPA-GMN-SELUKEYMD
           MOVE    I06-SELIDOYMD   TO  SPA-GMN-SELIDOYMD
           MOVE    I06-SELPTNUM    TO  SPA-GMN-SELPTNUM
           MOVE    I06-SELSTS      TO  SPA-GMN-SELSTS-G
           MOVE    I06-SELSORT     TO  SPA-GMN-SELSORT-G
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SELUKEYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SELIDOYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SELPTNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SELSTS-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4100-SELSORT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 3200-ALLIDOLST-SEC
      *
           ELSE
               MOVE    TSPA-I06    TO  SPA-I06
           END-IF
      *
           .
       4100-SELITEMS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受付日チェック処理
      *****************************************************************
       4100-SELUKEYMD-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-SELUKEYMD
      *
           IF    ( SPA-GMN-SELUKEYMD   NOT =   SPACE )
      *
               MOVE    SPA-GMN-SELUKEYMD
                                   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               IF    ( FLG-HIZUKE-ERR  =   ZERO )
      *
                   MOVE    WRK-HENYMDG TO  SPA-GMN-SELUKEYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-SELUKEYMD
      *
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4100-SELUKEYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    異動日チェック処理
      *****************************************************************
       4100-SELIDOYMD-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-SELIDOYMD
      *
           IF    ( SPA-GMN-SELIDOYMD   NOT =   SPACE )
      *
               MOVE    SPA-GMN-SELIDOYMD
                                   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               IF    ( FLG-HIZUKE-ERR  =   ZERO )
      *
                   MOVE    WRK-HENYMDG TO  SPA-GMN-SELIDOYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-SELIDOYMD
      *
               ELSE
                   MOVE    "0023"      TO  SPA-ERRCD
                   MOVE    20          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4100-SELIDOYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       4100-SELPTNUM-CHK-SEC              SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-SELNAME
           MOVE    ZERO        TO  SPA-NAI-SELPTID
      *
           IF    ( SPA-GMN-SELPTNUM    NOT =   SPACE )
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    SPA-GMN-SELPTNUM
                                       TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC    NOT =   ZERO )
                   MOVE    "0024"      TO  SPA-ERRCD
                   MOVE    21          TO  SPA-GMN-CUR
               ELSE
                   MOVE    SPTID-PTNUM TO  SPA-GMN-SELPTNUM
                   MOVE    SPTID-PTID  TO  SPA-NAI-SELPTID
      *
      *            患者マスター存在チェック
                   MOVE    SPA-NAI-SELPTID
                                       TO  WRK-PTINF-PTID
                   PERFORM 900-PTINF-KEY-SEL-SEC
                   IF    ( FLG-PTINF   NOT =  ZERO )
                       MOVE    "0025"  TO  SPA-ERRCD
                       MOVE    21      TO  SPA-GMN-CUR
                   ELSE
                       MOVE    PTINF-NAME
                                       TO  SPA-GMN-SELNAME
                   END-IF
               END-IF
           END-IF
      *
           .
       4100-SELPTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索状態チェック処理
      *****************************************************************
       4100-SELSTS-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SELSTS  TO  WRK-CMB-CD
           PERFORM 4200-SELSTSLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0022"      TO  SPA-ERRCD
               MOVE    23          TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SELSTS-G
           END-IF
      *
           .
       4100-SELSTS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順チェック処理
      *****************************************************************
       4100-SELSORT-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SELSORT  TO  WRK-CMB-CD
           PERFORM 4200-SELSORTLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0026"      TO  SPA-ERRCD
               MOVE    24          TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SELSORT-G
           END-IF
      *
           .
       4100-SELSORT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-GMN-SELNUM  NOT =   ZERO )
               PERFORM 41021-KIHON-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       41021-KIHON-CHK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-BRMNUM      NOT =   SPACE )
                   PERFORM 4101-BRMNUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-BTUNAME-G   NOT =   SPACE )
                   PERFORM 4101-BTUNAME-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SAGAKU-G    NOT =   SPACE )
                   PERFORM 4101-SAGAKU-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SENTEIKBN-G NOT =   SPACE )
                   PERFORM 4101-SENTEIKBN-CHK-SEC
               END-IF
           END-IF
      *
      *
           IF    ( SPA-GMN-NYUINKA-G       NOT =   SPACE )
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 4101-NYUINKA-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SHOKAI-G    NOT =   SPACE )
                   PERFORM 4101-SHOKAI-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "SHONUM" )
                OR   ( WRK-MCP-WIDGET  =   "B12" )
                   PERFORM 4101-SHONUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-KAIGOKBN-G      NOT =   SPACE )
                   PERFORM 4101-KAIGOKBN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-TAIINCD-G       NOT =   SPACE )
                   PERFORM 4101-TAIINCD-CHK-SEC
               END-IF
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-DR-G (IDX2) NOT =   SPACE )
                   PERFORM 4101-DR-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNCOMBI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TOKNYUIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TOKHIGAITOKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SNTNYUIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-NACCTKBN    NOT =   SPACE )
                   PERFORM 4101-NACCTKBN-CHK-SEC
               END-IF
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               PERFORM 4101-KSNCD-CHK-SEC
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-TEIKIKBN    NOT =   SPACE )
                   PERFORM 4101-TEIKIKBN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-DISPKBN     NOT =   SPACE )
                   PERFORM 4101-DISPKBN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SKJTIME-G   NOT =   SPACE )
                   PERFORM 4101-SKJTIME-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-NISSU       NOT =   SPACE )
                   PERFORM 4101-NISSU-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-BUNBENKBN   NOT =   SPACE )
                   PERFORM 4101-BUNBENKBN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN    NOT =   SPACE )
                   PERFORM 4101-RIYOUKBN-CHK-SEC
               END-IF
           END-IF
      *
           .
       41021-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SELNUM  =   ZERO )
               CONTINUE
           ELSE
               MOVE    ZERO        TO  IDX-LST
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >   SPA-GMN-ALLIDOLST-MAX )
                        OR   ( IDX-LST NOT =   ZERO )
                   IF    ( SPA-GMN-SELNUM  =   SPA-GMN-TALLNUM (IDX3))
                       MOVE    IDX3    TO  IDX-LST
                   END-IF
               END-PERFORM
      *
               IF    ( IDX-LST     =   ZERO )
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
      *
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室チェック
      *****************************************************************
       4101-BRMNUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-BTUDEF-G
      *
           IF    ( SPA-NAI-DIFF-INIT   =   SPACE )
               MOVE    SPA-NAI-BRMNUM-MOT  TO  SPA-GMN-BRMNUM-ORG
               MOVE    SPA-NAI-BTUNUM-MOT  TO  SPA-GMN-BTUNAME-ORG
               MOVE    "1"             TO  SPA-NAI-DIFF-INIT
           END-IF
      *
           INITIALIZE              ORCSBRMNUMAREA
           MOVE    "1"             TO  SBRMNUM-IN-PROCKBN
           MOVE    SPA-GMN-BRMNUM  TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
           IF    ( SBRMNUM-RC      =   ZERO )
               MOVE    SBRMNUM-OT-BRMNUM
                                   TO  SPA-GMN-BRMNUM
           END-IF
      *
           PERFORM 3101-BTUDEF-SEC
           IF    ( SPA-NAI-BTUDEF-MAX  =   ZERO )
               MOVE    "0003"      TO  SPA-ERRCD
               MOVE   3            TO  SPA-GMN-CUR
           END-IF
      *
           IF   (  SPA-ERRCD       =   SPACE )
            AND (( WRK-MCP-WIDGET          =   "BRMNUM" )
            OR   ( WRK-MCP-WIDGET          =   "BTUNAME" )
            OR   ( WRK-MCP-WIDGET          =   "B12" ))
               PERFORM 41011-BRMNUM-CHK-SEC
           END-IF
      *
           MOVE    SPA-GMN-BRMNUM      TO  SPA-GMN-BRMNUM-ORG
           MOVE    SPA-GMN-BTUNAME     TO  SPA-GMN-BTUNAME-ORG
      *
           .
       4101-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室チェック
      *****************************************************************
       41011-BRMNUM-CHK-SEC            SECTION.
      *
           PERFORM 3101-BTUNAMELST-SET-SEC
           MOVE    SPA-GMN-BTUNAME     TO  WRK-CMB-CD
           PERFORM 4200-BTUNAMELST-CHK-SEC
           IF    ( SPA-GMN-BTUNAME-MAX   =   1 )
            OR   ( WRK-CMB-IDX           =   ZERO )
                MOVE   1               TO  WRK-CMB-IDX
                IF   ( SPA-GMN-BTUNAME-MAX   = 1 )
                    MOVE   5           TO  SPA-GMN-CUR
                END-IF
           END-IF
      *
           MOVE    SPA-GMN-BTUNAMELST      (WRK-CMB-IDX)
                                       TO  SPA-GMN-BTUNAME-G
           MOVE    SPA-NAI-BTUDEF-KHNSRYCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-KHNSRYCD
           MOVE    SPA-NAI-BTUDEF-SBT      (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-SBT
           MOVE    SPA-NAI-BTUDEF-BTUTOK   (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BTUTOK
           MOVE    SPA-NAI-BTUDEF-BRMTOK   (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BRMTOK
           MOVE    SPA-NAI-BTUDEF-BTUKHNKBNCD   (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BTUKHNKBNCD
           MOVE    SPA-NAI-BTUDEF-BRMKHNKBNCD   (WRK-CMB-IDX)
                                       TO  SPA-NAI-BTU-BRMKHNKBNCD
           MOVE    SPA-NAI-BTUDEF-SAGAKU   (WRK-CMB-IDX)
                                       TO  WRK-BTUDEF-SAGAKU
           MOVE    SPA-NAI-BTUDEF-NYUINKA  (WRK-CMB-IDX)
                                       TO  WRK-BTUDEF-NYUINKA
      *
      *    室料差額、診療科の初期値を設定
           IF    ( WRK-MCP-WIDGET      NOT =  "B12" ) 
               IF    ( SPA-GMN-BRMNUM      =  SPA-GMN-BRMNUM-ORG )
                AND  ( SPA-GMN-BTUNAME     =  SPA-GMN-BTUNAME-ORG )
                    MOVE    SPA-GMN-SAGAKU      TO  WRK-CMB-CD
                    PERFORM 4200-SAGAKULST-CHK-SEC
                    MOVE    WRK-CMB-ITM         TO  SPA-GMN-SAGAKU-G
      *
                    MOVE    SPA-GMN-NYUINKA     TO  WRK-CMB-CD
                    PERFORM 4200-NYUINKALST-CHK-SEC
                    MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUINKA-G
      *
                ELSE
                    MOVE    WRK-BTUDEF-SAGAKU   TO  WRK-CMB-CD
                    PERFORM 4200-SAGAKULST-CHK-SEC
                    MOVE    WRK-CMB-ITM         TO  SPA-GMN-SAGAKU-G
      *
                    MOVE    WRK-BTUDEF-NYUINKA   TO  WRK-CMB-CD
                    PERFORM 4200-NYUINKALST-CHK-SEC
                    MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUINKA-G
                END-IF
           END-IF
      *
           PERFORM 3101-TOKNYUINLST-SET-SEC
           IF    ( WRK-MCP-WIDGET      NOT =  "B12" ) 
               IF    ( SPA-GMN-BRMNUM-ORG  NOT =   SPA-GMN-BRMNUM )
                OR   ( SPA-GMN-BTUNAME-ORG NOT =   SPA-GMN-BTUNAME )
                       PERFORM 4101-TOKNYUIN-SYOKI-SEC
               END-IF
           END-IF
      *
           .
       41011-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟チェック
      *****************************************************************
       4101-BTUNAME-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-BTUNAME     TO  WRK-CMB-CD
           PERFORM 4200-BTUNAMELST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-BTUNAME-G
           END-IF
      *
           .
       4101-BTUNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック
      *****************************************************************
       4101-SAGAKU-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SAGAKU      TO  WRK-CMB-CD
           PERFORM 4200-SAGAKULST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SAGAKU-G
           END-IF
      *
           .
       4101-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分チェック
      *****************************************************************
       4101-SENTEIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SENTEIKBN   TO  WRK-CMB-CD
           PERFORM 4200-SENTEIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0027"          TO  SPA-ERRCD
               MOVE    24              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SENTEIKBN-G
           END-IF
      *
           .
       4101-SENTEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック
      *****************************************************************
       4101-NYUINKA-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-NYUINKA      TO  WRK-CMB-CD
           PERFORM 4200-NYUINKALST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUINKA-G
           END-IF
      *
           .
       4101-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回区分チェック処理
      *****************************************************************
       4101-SHOKAI-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SHOKAI      TO  WRK-CMB-CD
           PERFORM 4200-SHOKAILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SHOKAI-G
               IF    ( SPA-GMN-SHOKAI  =   "1" )
                   MOVE    ZERO        TO  SPA-GMN-SHONUM
                   MOVE    SPACE       TO  SPA-GMN-LBL-SHONYUINYMD
                   MOVE    "1"         TO  SPA-GMN-KAIGOKBN
               END-IF
           END-IF
      *
           .
       4101-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初歴チェック処理
      *****************************************************************
       4101-SHONUM-CHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-LBL-SHONYUINYMD
      *
           IF    ( SPA-GMN-SHONUM  NOT =   ZERO )
      *
               INITIALIZE                  PTNYUINRRK-REC
      *
               IF    ( SPA-NAI-ORDERID =   NORDERID-TEISEIID )
                   PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
                   MOVE    SPA-GMN-SHONUM  TO  PTNYUINRRK-SHONUM
                   PERFORM 900-PTNYUINRRK-KEY80-SEL-SEC
               ELSE
                   MOVE    SPA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
                   MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
                   MOVE    SPA-GMN-SHONUM  TO  PTNYUINRRK-SHONUM
                   PERFORM 900-PTNYUINRRK-KEY80-SEL-SEC
               END-IF
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-NYUINYMD     TO   WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   STRING  "入院日 "   DELIMITED   BY  SIZE
                           WRK-HENYMDG DELIMITED   BY  SIZE
                   INTO    SPA-GMN-LBL-SHONYUINYMD
                   END-STRING
               ELSE
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    9               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-SHONUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護区分チェック
      *****************************************************************
       4101-KAIGOKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-KAIGOKBN    TO  WRK-CMB-CD
           PERFORM 4200-KAIGOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0028"          TO  SPA-ERRCD
               MOVE    25              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-KAIGOKBN-G
           END-IF
      *
           .
       4101-KAIGOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由チェック処理
      *****************************************************************
       4101-TAIINCD-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-TAIINCD     TO  WRK-CMB-CD
           PERFORM 4200-TAIINCDLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TAIINCD-G
           END-IF
      *
           .
       4101-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医チェック処理
      *****************************************************************
       4101-DR-CHK-SEC                 SECTION.
      *
           MOVE    SPA-GMN-DR (IDX2)   TO  WRK-CMB-CD
           PERFORM 4200-DRLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0012"          TO  SPA-ERRCD
               EVALUATE    IDX2
               WHEN    1
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    14          TO  SPA-GMN-CUR
               END-EVALUATE
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-DR-G (IDX2)
           END-IF
      *
           .
       4101-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       4101-HKNCOMBI-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-HKNNUM
      *
           IF    ( SPA-GMN-HKNCOMBI-G  NOT =   SPACE )
      *
               MOVE    SPA-GMN-HKNCOMBI     TO  WRK-CMB-CD
               PERFORM 4200-HKNCOMBILST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0015"          TO  SPA-ERRCD
                   MOVE    15              TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-HKNCOMBI-G
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-HKNNUM
               END-IF
      *
               IF    ( WRK-MCP-WIDGET  =   "HKNCOMBI" )
                   MOVE    SPA-GMN-TOKNYUIN-SRYCDTTL
                                       TO  WRK-TOKNYUIN-SRYCDTTL
                   PERFORM 3101-TOKNYUINLST-SET-SEC
                   IF    ( SPA-GMN-TOKNYUIN-SRYCDTTL
                                   NOT =   WRK-TOKNYUIN-SRYCDTTL  )
                       PERFORM 4101-TOKNYUIN-SYOKI-SEC
                   END-IF
      *
               END-IF
      *
           END-IF
      *
           .
       4101-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料チェック処理
      *****************************************************************
       4101-TOKNYUIN-CHK-SEC           SECTION.
      *
           INITIALIZE  SPA-NAI-TOKUTEI-G
                       SPA-NAI-TOKSRYCD
                       SPA-NAI-NATOKSRYCD
      *
           IF    ( SPA-GMN-TOKNYUIN-G  =   SPACE )
               MOVE    "0"             TO  SPA-GMN-TOKHIGAITOKBN
           ELSE
      *
               MOVE    SPA-GMN-TOKNYUIN    TO  WRK-CMB-CD
               PERFORM 4200-TOKNYUINLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0017"          TO  SPA-ERRCD
                   MOVE    17              TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-TOKNYUIN-G
                   MOVE    SPA-GMN-TOKNYUINL2 (WRK-CMB-IDX)
                                           TO  SPA-NAI-TOKUTEI-G
                   MOVE    SPA-GMN-TOKNYUINL-SRYCD (WRK-CMB-IDX)
                                           TO  SPA-NAI-TOKSRYCD
                   MOVE    SPA-GMN-TOKNYUINL-NASRYCD (WRK-CMB-IDX)
                                           TO  SPA-NAI-NATOKSRYCD
               END-IF
           END-IF
      *
           .
       4101-TOKNYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料非該当区分チェック処理
      *****************************************************************
       4101-TOKHIGAITOKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-TOKHIGAITOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TOKHIGAITOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TOKHIGAITOKBN-G
           ELSE
               MOVE    "0"         TO  WRK-CMB-CD
               PERFORM 4200-TOKHIGAITOKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TOKHIGAITOKBN-G
           END-IF
      *
           .
       4101-TOKHIGAITOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定入院料チェック処理
      *****************************************************************
       4101-SNTNYUIN-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-TOKNYUINMEI       NOT =   SPACE )
               IF    ( SPA-NAI-NATOKSRYCD    NOT =   SPACE )
                AND  ( SPA-GMN-TOKHIGAITOKBN     =   "1" )
                   MOVE    SPA-NAI-KJNYMD
                                       TO  WRK-TNS-KJNYMD
                   MOVE    SPA-NAI-NATOKSRYCD
                                       TO  WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME    TO  SPA-GMN-SNTNYUIN
                   MOVE    TNS-SRYCD   TO  SPA-NAI-SNTSRYCD
               ELSE
                   MOVE    SPA-GMN-TOKNYUINMEI
                                       TO  SPA-GMN-SNTNYUIN
                   MOVE    SPA-NAI-TOKSRYCD
                                       TO  SPA-NAI-SNTSRYCD
               END-IF
           ELSE
               MOVE    SPA-NAI-KHNNYUIN    TO  SPA-GMN-SNTNYUIN
               MOVE    SPA-NAI-KHNSRYCD    TO  SPA-NAI-SNTSRYCD
           END-IF
      *
           .
       4101-SNTNYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料初期値設定処理
      *****************************************************************
       4101-TOKNYUIN-SYOKI-SEC         SECTION.
      *
           INITIALIZE  SPA-GMN-TOKNYUIN-G
                       SPA-NAI-TOKUTEI-G
                       SPA-NAI-TOKSRYCD
                       SPA-GMN-SNTNYUIN
                       SPA-NAI-SNTSRYCD
                       SPA-NAI-NATOKSRYCD
      *
           PERFORM VARYING IDX-CMB
                   FROM    SPA-GMN-TOKNYUIN-MAX    BY  -1
                   UNTIL ( IDX-CMB     <   1 )
                    OR   ( SPA-GMN-TOKNYUIN-G NOT    =  SPACE )
      *
               EVALUATE    TRUE
               WHEN  ( SPA-GMN-TOKNYUINL2 (IDX-CMB) (1:1)  =   "2" )
                AND  ( SPA-GMN-TOKNYUINL2 (IDX-CMB) (2:2)
                                           =   SPA-NAI-BTU-BTUTOK  )
               WHEN  ( SPA-GMN-TOKNYUINL2 (IDX-CMB) (1:1)  =   "3" )
                AND  ( SPA-GMN-TOKNYUINL2 (IDX-CMB) (2:2)
                                           =   SPA-NAI-BTU-BRMTOK )
      *
                   MOVE    SPA-GMN-TOKNYUINLST (IDX-CMB)
                                       TO  SPA-GMN-TOKNYUIN-G
                   MOVE    SPA-GMN-TOKNYUINL2 (IDX-CMB)
                                       TO  SPA-NAI-TOKUTEI-G
                   MOVE    SPA-GMN-TOKNYUINL-SRYCD (IDX-CMB)
                                           TO  SPA-NAI-TOKSRYCD
                   MOVE    SPA-GMN-TOKNYUINL-NASRYCD (IDX-CMB)
                                           TO  SPA-NAI-NATOKSRYCD
               END-EVALUATE
      *
           END-PERFORM
      *
      *
           IF    ( SPA-GMN-TOKNYUINMEI     =   SPACE )
               IF    ( SPA-NAI-BTU-BRMKHNKBNCD NOT =   SPACE )
                   MOVE    SPA-NAI-BTU-BRMKHNKBNCD
                                           TO  WRK-CMB-CD
                   IF    ( SPA-NAI-KJNYMD  >=  CONST-H181001 )
                       PERFORM 41011-TOKNYUIN-SYOKI-SEC
                   END-IF
                   PERFORM 4201-TOKNYUINLST-CHK-SEC
                   IF    ( WRK-CMB-ITM NOT =   SPACE )
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-TOKNYUIN-G
                       MOVE    SPA-GMN-TOKNYUINL2 (WRK-CMB-IDX)
                                           TO  SPA-NAI-TOKUTEI-G
                       MOVE    SPA-GMN-TOKNYUINL-SRYCD (WRK-CMB-IDX)
                                           TO  SPA-NAI-TOKSRYCD
                       MOVE    SPA-GMN-TOKNYUINL-NASRYCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-NATOKSRYCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-TOKNYUINMEI    =   SPACE )
               IF    ( SPA-NAI-BTU-BTUKHNKBNCD NOT =   SPACE )
                   MOVE    SPA-NAI-BTU-BTUKHNKBNCD
                                           TO  WRK-CMB-CD
                   IF    ( SPA-NAI-KJNYMD  >=  CONST-H181001 )
                       PERFORM 41011-TOKNYUIN-SYOKI-SEC
                   END-IF
                   PERFORM 4201-TOKNYUINLST-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-TOKNYUIN-G
                   MOVE    SPA-GMN-TOKNYUINL2 (WRK-CMB-IDX)
                                       TO  SPA-NAI-TOKUTEI-G
                   MOVE    SPA-GMN-TOKNYUINL-SRYCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-TOKSRYCD
                   MOVE    SPA-GMN-TOKNYUINL-SRYCD (WRK-CMB-IDX)
                                       TO  SPA-NAI-NATOKSRYCD
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-TOKNYUINMEI NOT =   SPACE )
               MOVE    SPA-GMN-TOKNYUINMEI TO  SPA-GMN-SNTNYUIN
               MOVE    SPA-NAI-TOKSRYCD    TO  SPA-NAI-SNTSRYCD
           ELSE
               MOVE    SPA-NAI-KHNNYUIN    TO  SPA-GMN-SNTNYUIN
               MOVE    SPA-NAI-KHNSRYCD    TO  SPA-NAI-SNTSRYCD
           END-IF
      *
           MOVE    "0"                 TO  SPA-GMN-TOKHIGAITOKBN
           PERFORM 4101-TOKHIGAITOKBN-CHK-SEC
      *
           .
       4101-TOKNYUIN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料初期値設定処理
      *****************************************************************
       41011-TOKNYUIN-SYOKI-SEC        SECTION.
      *
           IF    ( SPA-NAI-BTU-SBT =   "04" OR "09" )
               PERFORM 800-ROUJINAGE-CHK-SEC
               IF    ( FLG-LIFE-RJN        =   1 OR 2 )
                   PERFORM VARYING IDX10   FROM    1   BY  1
                           UNTIL ( IDX10 > CONST-TOK-LIFECD-MAX)
                       IF    ( WRK-CMB-CD
                                   =   CONST-TOK-LIFECD-IPN (IDX10))
                           MOVE    CONST-TOK-LIFECD-LIFE (IDX10)
                                   TO  WRK-CMB-CD
                           MOVE    CONST-TOK-LIFECD-MAX
                                   TO  IDX10
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       41011-TOKNYUIN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院会計区分チェック処理
      *****************************************************************
       4101-NACCTKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-NACCTKBN    TO  WRK-CMB-CD
           PERFORM 4200-NACCTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0030"          TO  SPA-ERRCD
               MOVE    27              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NACCTKBN-G
           END-IF
      *
           .
       4101-NACCTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院加算チェック処理
      *****************************************************************
       4101-KSNCD-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-KSNCD-SRYCD (IDX2)
      *
           IF    ( SPA-GMN-KSNCD-G (IDX2)  NOT =   SPACE )
      *
               MOVE    SPA-GMN-KSNCD (IDX2)    TO  WRK-CMB-CD
               PERFORM 4200-KSNCDLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0031"          TO  SPA-ERRCD
                   EVALUATE    IDX2
                   WHEN    1
                       MOVE    28          TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    29          TO  SPA-GMN-CUR
                   WHEN    3
                       MOVE    30          TO  SPA-GMN-CUR
                   END-EVALUATE
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-KSNCD-G (IDX2)
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-KSNCD-SRYCD(IDX2)
               END-IF
           END-IF
      *
           .
       4101-KSNCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分チェック処理
      *****************************************************************
       4101-TEIKIKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-TEIKIKBN    TO  WRK-CMB-CD
           PERFORM 4200-TEIKIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0034"          TO  SPA-ERRCD
               MOVE    31              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TEIKIKBN-G
           END-IF
      *
           .
       4101-TEIKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表時チェック処理
      *****************************************************************
       4101-DISPKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-DISPKBN    TO  WRK-CMB-CD
           PERFORM 4200-DISPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0035"          TO  SPA-ERRCD
               MOVE    32              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-DISPKBN-G
           END-IF
      *
           .
       4101-DISPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間チェック処理
      *****************************************************************
       4101-SKJTIME-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-SKJTIME     TO  WRK-CMB-CD
           PERFORM 4200-SKJTIMELST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0018"          TO  SPA-ERRCD
               MOVE    18              TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJTIME-G
           END-IF
      *
           .
       4101-SKJTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数チェック
      *****************************************************************
       4101-NISSU-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NISSU       TO  WRK-INPUT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-ERR             =   ZERO )
               CONTINUE
           ELSE
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    19              TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NISSU (1:1) =   "*" )
                   CONTINUE
               ELSE
                   MOVE    "0021"      TO  SPA-ERRCD
                   MOVE    19          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE                  ORCSNDAYCHGAREA
               MOVE    SPA-GMN-NISSU   TO  ORCSNDAYCHG-INPUTCD
               MOVE    SPA-NAI-KJNYMD  TO  ORCSNDAYCHG-SRYYMD
               MOVE    "1"             TO  ORCSNDAYCHG-KBN
               MOVE    50              TO  ORCSNDAYCHG-KETA
               CALL    "ORCSNDAYCHG"       USING
                                           SPA-AREA
                                           ORCSNDAYCHGAREA
               IF    ( ORCSNDAYCHG-ERRCD   =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    "0019"      TO  SPA-ERRCD
                   MOVE    ORCSNDAYCHG-ERRMSG
                                       TO  WRK-ERRMSG
                   MOVE    19          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分チェック
      *****************************************************************
       4101-BUNBENKBN-CHK-SEC          SECTION.
      *
           PERFORM 41011-BUNBEN-NYUIN-CHK-SEC
           IF    ( FLG-BUNBENCHK       =   1 )
               MOVE    SPA-GMN-BUNBENKBN
                                       TO  WRK-CMB-CD
               PERFORM 4200-BUNBENKBNLST-CHK-SEC
               IF    ( WRK-CMB-CD      =   SPACE )
                   MOVE    "0036"      TO  SPA-ERRCD
                   MOVE    33          TO  SPA-GMN-CUR
               END-IF
           ELSE
                   MOVE    SPACE       TO  SPA-GMN-BUNBENKBN-G
           END-IF
      *
           .
       4101-BUNBENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩入院チェック
      *****************************************************************
       41011-BUNBEN-NYUIN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                    TO  FLG-BUNBENCHK
      *
           IF    ( SPA-NAI-PTID            =   ZERO )
               CONTINUE
           ELSE
      *
               INITIALIZE                  SYS-1005-REC
               IF    ( SPA-GMN-NYUINKA     NOT =   SPACE )
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    SPA-GMN-NYUINKA TO  SYS-KBNCD
                   MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-1005-REC
               END-IF
      *
               MOVE    ZERO                TO  WRK-AGE
      *
               MOVE    SPA-NAI-BIRTHDAY    TO  WRK-BIRTHDAY
               MOVE    SPA-NAI-KJNYMD      TO  WRK-NOW
               COMPUTE WRK-AGE         =   WRK-NOW-YY
                                       -   WRK-BIRTHDAY-YY
               IF    ( WRK-BIRTHDAY (5:4)  >   WRK-NOW (5:4) )
                   COMPUTE WRK-AGE = WRK-AGE   -   1
               END-IF
      *
               IF    ( SYS-1005-RECESRYKA  =   "23" OR "24" )
                AND  ( SPA-NAI-SEX         =   "2" )
                AND  ( WRK-AGE             >   10 )
                   MOVE    1                   TO  FLG-BUNBENCHK
               END-IF
      *
               IF    ( SPA-NAI-BUNBENKBN   NOT =   SPACE )
                   MOVE    1               TO  FLG-BUNBENCHK
               END-IF
           END-IF
      *
           .
       41011-BUNBEN-NYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分チェック
      *****************************************************************
       4101-RIYOUKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-BUNBENKBN   TO  WRK-BUNBENKBN
           IF    ( SPA-GMN-BUNBENKBN   =   SPACE  )
               MOVE    SPA-NAI-BUNBENKBN   TO  WRK-BUNBENKBN
           END-IF
      *
           IF    ( WRK-BUNBENKBN       =   "1" OR "2" )
               MOVE    SPA-GMN-RIYOUKBN TO WRK-CMB-CD
               PERFORM 4200-RIYOUKBNLST-CHK-SEC
               IF    ( WRK-CMB-CD      =   SPACE )
                   MOVE    "0037"      TO  SPA-ERRCD
                   MOVE    34          TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-RIYOUKBN-G
           END-IF
      *
           .
       4101-RIYOUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟名コンボチェック処理
      *****************************************************************
       4200-BTUNAMELST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-BTUNAME-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-BTUNAMEL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-BTUNAMELST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BTUNAMELST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コンボチェック処理
      *****************************************************************
       4200-SAGAKULST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SAGAKU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SAGAKUL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SAGAKULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SAGAKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科コンボチェック処理
      *****************************************************************
       4200-NYUINKALST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-NYUINKA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NYUINKAL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NYUINKALST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NYUINKALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回コンボチェック処理
      *****************************************************************
       4200-SHOKAILST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SHOKAI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SHOKAIL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SHOKAILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SHOKAILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由コンボチェック処理
      *****************************************************************
       4200-TAIINCDLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TAIINCD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TAIINCDL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TAIINCDLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TAIINCDLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医コンボチェック処理
      *****************************************************************
       4200-DRLST-CHK-SEC              SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-DR-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-DRL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-DRLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1010"          TO  SYS-KANRICD
               MOVE    WRK-CMB-CD      TO  SYS-KBNCD
               MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC    TO  SYS-1010-REC
                   MOVE    SYS-1010-KBNCD (2:4)
                                           TO  WRK-CMB-ITM (1:4)
                   MOVE    SYS-1010-NAME   TO  WRK-CMB-ITM (6:)
               END-IF
      *
           END-IF
      *
           .
       4200-DRLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せコンボチェック処理
      *****************************************************************
       4200-HKNCOMBILST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-HKNCOMBI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-HKNCOMBIL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-HKNCOMBILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-HKNNUML (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-HKNCOMBILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間コンボチェック処理
      *****************************************************************
       4200-SKJTIMELST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SKJTIME-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJTIMEL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SKJTIMELST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SKJTIMELST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボチェック処理
      *****************************************************************
       4200-TOKNYUINLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TOKNYUINL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TOKNYUINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TOKNYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボチェック処理
      *****************************************************************
       4201-TOKNYUINLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TOKNYUINL2 (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TOKNYUINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4201-TOKNYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料非該当区分コンボチェック処理
      *****************************************************************
       4200-TOKHIGAITOKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TOKHIGAITOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TOKHIGAITOKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TOKHIGAITOKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TOKHIGAITOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダーＩＤチェック処理
      *****************************************************************
       4200-ORDERID-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NORDERID-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NORDERID-ORCAID (IDX-CMB) =   WRK-CMB-CD )
                   MOVE    NORDERID-ORCAMES1 (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    NORDERID-ORCAMES2 (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ORDERID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダーエラーコードチェック処理
      *****************************************************************
       4200-ERRCD-CHK-SEC              SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NCLAIMERRCD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NCLAIMERRCDL (IDX-CMB) =   WRK-CMB-CD )
                   MOVE    NCLAIMERRCDLMEI  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ERRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダー項目名称チェック処理
      *****************************************************************
       4200-NCLAIMID-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NCLAIMID-ITEM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NCLAIMID-ITEMJMA (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    NCLAIMID-ITEMMES  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NCLAIMID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索状態チェック処理
      *****************************************************************
       4200-SELSTSLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SELSTS-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SELSTSL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SELSTSLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SELSTSLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順チェック処理
      *****************************************************************
       4200-SELSORTLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SELSORT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SELSORTL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SELSORTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SELSORTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分チェック処理
      *****************************************************************
       4200-SENTEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SENTEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SENTEIKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SENTEIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SENTEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護区分チェック処理
      *****************************************************************
       4200-KAIGOKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-KAIGOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAIGOKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KAIGOKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KAIGOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計区分チェック処理
      *****************************************************************
       4200-NACCTKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-NACCTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NACCTKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NACCTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NACCTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院加算チェック処理
      *****************************************************************
       4200-KSNCDLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-KSNCD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KSNCDL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KSNCDLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-KSNCDL-SRYCD (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KSNCDLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院加算チェック処理
      *****************************************************************
       4201-KSNCDLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-KSNCD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KSNCDL-SRYCD (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KSNCDLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-KSNCDL-SRYCD (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4201-KSNCDLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分チェック処理
      *****************************************************************
       4200-TEIKIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TEIKIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TEIKIKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TEIKIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TEIKIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示区分チェック処理
      *****************************************************************
       4200-DISPKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-DISPKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-DISPKBNL (IDX-CMB)
                                           =   WRK-CMB-CD )
                   MOVE    SPA-GMN-DISPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-DISPKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分コンボチェック処理
      *****************************************************************
       4200-BUNBENKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-BUNBENKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-BUNBENKBNL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-BUNBENKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BUNBENKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分コンボチェック処理
      *****************************************************************
       4200-RIYOUKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-RIYOUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-RIYOUKBNL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-RIYOUKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-RIYOUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I01"               TO  SPA-SAKIPG
           MOVE    "I06"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                      SPA-GMN-HEAD-AREA
                                           SPA-GMN-INPUT-AREA
                                           SPA-GMN-SELNUM
                                           SPA-GMN-ALLIDOLST-SEL
                                           SPA-GMN-PTIDOLST-AREA
                                           SPA-GMN-ERRMSGLST-AREA
                                           SPA-NAI-HEAD-AREA
                                           SPA-NAI-INPUT-AREA
                                           SPA-NAI-PTIDOLST-AREA
                                           SPA-GMN-COMBO-AREA
                                           SPA-NAI-COMBO-AREA
      *
           IF    ( SPA-NAI-SELUKEYMD   NOT =   SPACE )
               MOVE    SPA-NAI-SELUKEYMD   TO  SPA-NAI-KJNYMD
           ELSE
               MOVE    SPA-SYSYMD          TO  SPA-NAI-KJNYMD
           END-IF
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           IF    ( SPA-GMN-ALLIDOLST-MAX   =   ZERO )
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       230-DEL-SEC                     SECTION.
      *
           IF    ( SPA-GMN-ALLIDOLST-SEL   =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
           ELSE
               MOVE    "2001"          TO  SPA-I0IDCD
           END-IF
      *
           .
       230-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理（削除状態に更新）
      *****************************************************************
       2301-DEL-SEC                    SECTION.
      *
           MOVE    SPA-GMN-ALLIDOLST-SEL   TO  IDX-LST
      *
           INITIALIZE                  ORDER-REC
           MOVE    SPA-HOSPNUM             TO  ORDER-HOSPNUM
           MOVE    SPA-NAI-TALLKARTE-KEY (IDX-LST)
                                           TO  ORDER-KARTE-KEY
           MOVE    SPA-NAI-TALLORDERNUM  (IDX-LST)
                                           TO  ORDER-ORDERNUM
           MOVE    ORDER-REC               TO  MCPDATA-REC
           MOVE    "tbl_order"             TO  MCP-TABLE
           MOVE    "upd1"                  TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      =   ZERO )
      *
               PERFORM 220-CLEAR-SEC
      *
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           ELSE
               MOVE    "4001"              TO  SPA-ERRCD
           END-IF
      *
           .
       2301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    前日処理
      *****************************************************************
       260-PREVDAY-SEC                 SECTION.
      *
           MOVE    SPA-NAI-SELUKEYMD      TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    -1                  TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD       TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-SELUKEYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-SELUKEYMD
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    1                   TO  SPA-GMN-ALLIDOLST-PAGE
      *
      *    異動リスト編集処理
           PERFORM 3101-ALLIDOLST-SEC
      *
           .
       260-PREVDAY-EXT.
           EXIT.
      *****************************************************************
      *    翌日処理
      *****************************************************************
       270-NEXTDAY-SEC                 SECTION.
      *
           MOVE    SPA-NAI-SELUKEYMD      TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD       TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-SELUKEYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-SELUKEYMD
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    1                   TO  SPA-GMN-ALLIDOLST-PAGE
      *
      *    異動リスト編集処理
           PERFORM 3101-ALLIDOLST-SEC
      *
           .
       270-NEXTDAY-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-PREVPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-ALLIDOLST-PAGE  >   1 )
      *
               PERFORM 220-CLEAR-SEC
      *
               COMPUTE SPA-GMN-ALLIDOLST-PAGE
                   =   SPA-GMN-ALLIDOLST-PAGE  -   1
      *
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-IF
      *
           .
       260-PREVPAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-NEXTPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-ALLIDOLST-NEXT  =   1 )
      *
               PERFORM 220-CLEAR-SEC
      *
               COMPUTE SPA-GMN-ALLIDOLST-PAGE
                   =   SPA-GMN-ALLIDOLST-PAGE  +   1
      *
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-IF
      *
           .
       270-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    検索患者ボタンクリック時処理
      *****************************************************************
       299-EDIT-SELPTNUM-SEC           SECTION.
      *
           IF    ( SPA-GMN-SELPTNUM    =   SPACE )
               IF    ( SPA-GMN-ALLIDOLST-SEL   NOT =   ZERO )
                   MOVE    SPA-GMN-ALLIDOLST-SEL
                                       TO  IDX-LST
                   MOVE    SPA-GMN-TALLPTNUM (IDX-LST)
                                       TO  SPA-GMN-SELPTNUM
                   PERFORM 4100-SELPTNUM-CHK-SEC
                   PERFORM 3200-ALLIDOLST-SEC
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-SELPTNUM
               PERFORM 4100-SELPTNUM-CHK-SEC
               PERFORM 3200-ALLIDOLST-SEC
           END-IF
      *
      *
           .
       299-EDIT-SELPTNUM-EXT.
           EXIT.
      *****************************************************************
      *    検索受付日ボタンクリック時処理
      *****************************************************************
       299-EDIT-SELUKEYMD-SEC           SECTION.
      *
           IF    ( SPA-GMN-SELUKEYMD    =   SPACE )
               IF    ( SPA-GMN-ALLIDOLST-SEL   NOT =   ZERO )
                   MOVE    SPA-GMN-ALLIDOLST-SEL
                                       TO  IDX-LST
                   MOVE    SPA-NAI-TALLUKEYMD (IDX-LST)
                                       TO  SPA-GMN-SELUKEYMD
               ELSE
                   MOVE    SPA-SYSYMD  TO  SPA-GMN-SELUKEYMD
               END-IF
      *
               PERFORM 4100-SELUKEYMD-CHK-SEC
               PERFORM 3200-ALLIDOLST-SEC
      *
           ELSE
               MOVE    SPACE           TO  SPA-GMN-SELUKEYMD
               PERFORM 4100-SELUKEYMD-CHK-SEC
               PERFORM 3200-ALLIDOLST-SEC
           END-IF
      *
           .
       299-EDIT-SELUKEYMD-EXT.
           EXIT.
      *****************************************************************
      *    検索異動日ボタンクリック時処理
      *****************************************************************
       299-EDIT-SELIDOYMD-SEC           SECTION.
      *
           IF    ( SPA-GMN-SELIDOYMD    =   SPACE )
               IF    ( SPA-GMN-ALLIDOLST-SEL   NOT =   ZERO )
                   MOVE    SPA-GMN-ALLIDOLST-SEL
                                       TO  IDX-LST
                   MOVE    SPA-NAI-TALLIDOYMD (IDX-LST)
                                       TO  SPA-GMN-SELIDOYMD
                   PERFORM 4100-SELIDOYMD-CHK-SEC
                   PERFORM 3200-ALLIDOLST-SEC
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-SELIDOYMD
               PERFORM 4100-SELIDOYMD-CHK-SEC
               PERFORM 3200-ALLIDOLST-SEC
           END-IF
      *
           .
       299-EDIT-SELIDOYMD-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       212-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  212-TOUROKU-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-ORDERID
           WHEN    NORDERID-NYUINID
           WHEN    NORDERID-TEISEIID
               PERFORM 2121-NYUIN-KANREN-CHK-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( SPA-I0IDCD  =   SPACE )
                   PERFORM 2121-NYUIN-TOUROKU-SEC
               END-IF
           WHEN    NORDERID-TAIINID
               PERFORM 2121-TAIIN-TOUROKU-SEC
           WHEN    NORDERID-HENKOID
               PERFORM 2121-HENKO-TOUROKU-SEC
           WHEN    NORDERID-GAIHKID
           WHEN    NORDERID-GBACKID
               PERFORM 2121-GAIHAKU-TOUROKU-SEC
           WHEN    NORDERID-FNORLID
           WHEN    NORDERID-FSPCLID
           WHEN    NORDERID-FSELEID
           WHEN    NORDERID-FSTOPID
           WHEN    NORDERID-FSPSLID
           WHEN    NORDERID-FLIQUIDID
               PERFORM 2121-SYOKUJI-TOUROKU-SEC
           WHEN    NORDERID-IDODRID
               PERFORM 2121-DOCTOR-TOUROKU-SEC
           WHEN   NORDERID-IDOBTUID
           WHEN   NORDERID-IDOBRMID
           WHEN   NORDERID-IDOKAID
           WHEN   NORDERID-IDOALLID
               PERFORM 2121-IDOKABTUBRM-TOUROKU-SEC
           WHEN    NORDERID-DELNYUINID
           WHEN    NORDERID-ALLDELNYUINID
               PERFORM 2121-DELNYUIN-TOUROKU-SEC
           WHEN    NORDERID-CANTAIINID
               PERFORM 2121-CANTAIIN-TOUROKU-SEC
           WHEN    NORDERID-CANIDOID
               PERFORM 2121-CANIDO-TOUROKU-SEC
           WHEN    NORDERID-GPTSTID
           WHEN    NORDERID-FOODID
           WHEN    NORDERID-PTEVALID
           WHEN    NORDERID-BUNBENID
           WHEN    NORDERID-CRENACCTID
           WHEN    NORDERID-SAGAKUID
               CONTINUE
           END-EVALUATE
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    入院関連チェック
      *****************************************************************
       2121-NYUIN-KANREN-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-SHOKAI-G    =   SPACE )
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       2121-NYUIN-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    訂正関連チェック
      *****************************************************************
       2121-TEISEI-KANREN-CHK-SEC      SECTION.
      *
           IF    ( SPA-GMN-SHOKAI-G    =   SPACE )
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       2121-TEISEI-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院登録処理
      *****************************************************************
       2121-NYUIN-TOUROKU-SEC          SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SHOKAI-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BTUNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BRMNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SAGAKU-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SENTEIKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-KAIGOKBN-SET-SEC
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TOKNYUIN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TOKHIGAITOKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NACCTKBN-SET-SEC
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-KSNCD-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TEIKIKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-DISPKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BUNBENKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-RIYOUKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           IF    ( SPA-NAI-ORDERID =   NORDERID-TEISEIID )
               CALL   "ORCSODR12"      USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
           ELSE
               CALL   "ORCSODR01"      USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
           END-IF
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4002"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
               EVALUATE    SODR-DIALOG
               WHEN    1
                   MOVE    "2004"       TO  SPA-I0IDCD
               WHEN    2
                   MOVE    "2005"       TO  SPA-I0IDCD
               END-EVALUATE
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-NYUIN-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    退院登録処理
      *****************************************************************
       2121-TAIIN-TOUROKU-SEC          SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TAIINCD-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR02"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4003"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
               EVALUATE    SODR-DIALOG
               WHEN    1
                   MOVE    "2002"       TO  SPA-I0IDCD
               WHEN    2
                   MOVE    "2003"       TO  SPA-I0IDCD
               END-EVALUATE
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-TAIIN-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    変更登録処理
      *****************************************************************
       2121-HENKO-TOUROKU-SEC          SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BUNBENKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-RIYOUKBN-SET-SEC
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TEIKIKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-DISPKBN-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR05"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4009"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-HENKO-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    外泊登録処理
      *****************************************************************
       2121-GAIHAKU-TOUROKU-SEC        SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR03"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4004"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-GAIHAKU-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    食事登録処理
      *****************************************************************
       2121-SYOKUJI-TOUROKU-SEC        SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SKJTIME-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NISSU-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR04"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4005"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-SYOKUJI-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    担当医登録処理
      *****************************************************************
       2121-DOCTOR-TOUROKU-SEC         SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR05"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4006"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-DOCTOR-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    転科転棟転室処理
      *****************************************************************
       2121-IDOKABTUBRM-TOUROKU-SEC          SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BTUNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BRMNUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SAGAKU-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SENTEIKBN-SET-SEC
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-DR-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TOKNYUIN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-TOKHIGAITOKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NACCTKBN-SET-SEC
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
                   PERFORM 2300-KSNCD-SET-SEC
               END-IF
           END-PERFORM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BUNBENKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-RIYOUKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR06"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4007"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-IDOKABTUBRM-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    入院取消登録処理
      *****************************************************************
       2121-DELNYUIN-TOUROKU-SEC       SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-NYUINKA-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR07"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4008"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-DELNYUIN-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    退院取消登録処理
      *****************************************************************
       2121-CANTAIIN-TOUROKU-SEC       SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR08"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4010"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-DELNYUIN-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    異動取消登録処理
      *****************************************************************
       2121-CANIDO-TOUROKU-SEC         SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           MOVE    SPA-NAI-ORDER-DAT   TO  ORDER-REC
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR09"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC      NOT =   ZERO )
               MOVE    "4011"   TO  SPA-ERRCD
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               MOVE    SODR-ERRCD       TO ERR-ORDER-ERRCD
               MOVE    LNK-ODRITEM-REC  TO ERR-ODRITEM-REC
               PERFORM 3101-ERRMSGLST-SEC
           WHEN  OTHER
               PERFORM 220-CLEAR-SEC
      *        異動リスト編集処理
               PERFORM 3101-ALLIDOLST-SEC
           END-EVALUATE
      *
           .
       2121-CANIDO-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    初回設定処理
      *****************************************************************
       2300-SHOKAI-SET-SEC             SECTION.
      *
           IF    ( SPA-GMN-SHOKAI  = "2" )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-SHOKAIJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
               MOVE    "01"        TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
               INITIALIZE                  PTNYUINRRK-REC
      *
               IF    ( SPA-NAI-ORDERID =   NORDERID-TEISEIID )
                   PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
                   MOVE    SPA-NAI-NYUINYMD
                                           TO  PTNYUINRRK-NYUINYMD
                   MOVE    SPA-GMN-SHONUM  TO  PTNYUINRRK-SHONUM
                   PERFORM 900-PTNYUINRRK-KEY78-SEL-SEC
                   MOVE    PTNYUINRRK-NYUINYMD
                                           TO  LNK-ODRITEM-MEMO(IDX-ITM)
               ELSE
                   MOVE    SPA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
                   MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
                   MOVE    SPA-NAI-NYUINYMD
                                           TO  PTNYUINRRK-NYUINYMD
                   MOVE    SPA-GMN-SHONUM  TO  PTNYUINRRK-SHONUM
                   PERFORM 900-PTNYUINRRK-KEY78-SEL-SEC
                   MOVE    PTNYUINRRK-NYUINYMD
                                           TO  LNK-ODRITEM-MEMO(IDX-ITM)
               END-IF
           END-IF
      *
           .
       2300-SHOKAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号設定処理
      *****************************************************************
       2300-BTUNUM-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-BTUNUMJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    SPA-GMN-BTUNAME TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-BTUNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    病室番号設定処理
      *****************************************************************
       2300-BRMNUM-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-BRMNUMJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    SPA-GMN-BRMNUM  TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-BRMNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額設定処理
      *****************************************************************
       2300-SAGAKU-SET-SEC             SECTION.
      *
           IF    ( SPA-GMN-SAGAKU          NOT = SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-SAGAKUJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
               MOVE    SPA-GMN-SAGAKU
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
               MOVE    SPA-GMN-SAGAKUMEI-NUM
                                   TO  LNK-ODRITEM-MEMO (IDX-ITM)
           END-IF
      *
           .
       2300-SAGAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    選定入院設定処理
      *****************************************************************
       2300-SENTEIKBN-SET-SEC      SECTION.
      *
           IF    ( SPA-GMN-SENTEIKBN   NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-SENTEIKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
               MOVE    SPA-GMN-SENTEIKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-SENTEIKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    介護からの異動設定処理
      *****************************************************************
       2300-KAIGOKBN-SET-SEC       SECTION.
      *
           IF    ( SPA-GMN-KAIGOKBN    NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-KAIGOKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
               MOVE    SPA-GMN-KAIGOKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-KAIGOKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分設定処理
      *****************************************************************
       2300-BUNBENKBN-SET-SEC      SECTION.
      *
           IF    ( SPA-GMN-BUNBENKBN   NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-BUNBENKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
               MOVE    SPA-GMN-BUNBENKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-BUNBENKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分設定処理
      *****************************************************************
       2300-RIYOUKBN-SET-SEC      SECTION.
      *
           IF    ( SPA-GMN-RIYOUKBN   NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-RIYOUKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
               MOVE    SPA-GMN-RIYOUKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-RIYOUKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    担当医設定処理
      *****************************************************************
       2300-DR-SET-SEC                 SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-DRJMA      TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
           IF    ( SPA-GMN-DR     (IDX2)   NOT =   SPACE )
               MOVE    "1"             TO  LNK-ODRITEM-ITEM(IDX-ITM)
               MOVE    SPA-GMN-DR (IDX2)
                                       TO  LNK-ODRITEM-ITEM(IDX-ITM)
                                                              (2:)
           END-IF
      *
           .
       2300-DR-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料設定処理
      *****************************************************************
       2300-TOKNYUIN-SET-SEC           SECTION.
      *
           IF    ( SPA-GMN-TOKNYUIN    NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-TOKNYUINJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
      *
               MOVE    SPA-NAI-TOKUTEI-G
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-TOKNYUIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料算定要件非該当区分設定処理
      *****************************************************************
       2300-TOKHIGAITOKBN-SET-SEC      SECTION.
      *
           IF    ( SPA-GMN-TOKHIGAITOKBN   NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-TOKHIGAITOKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
               MOVE    SPA-GMN-TOKHIGAITOKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-TOKHIGAITOKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計区分設定処理
      *****************************************************************
       2300-NACCTKBN-SET-SEC      SECTION.
      *
           IF    ( SPA-GMN-NACCTKBN    NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-NACCTKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
               MOVE    SPA-GMN-NACCTKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-NACCTKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院加算設定処理
      *****************************************************************
       2300-KSNCD-SET-SEC              SECTION.
      *
           IF    ( SPA-NAI-KSNCD-SRYCD (IDX2)  NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-KSNCDJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
               MOVE    SPA-NAI-KSNCD-SRYCD (IDX2)
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-KSNCD-SET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分設定処理
      *****************************************************************
       2300-TEIKIKBN-SET-SEC      SECTION.
      *
           IF    ( SPA-GMN-TEIKIKBN    NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-TEIKIKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
               MOVE    SPA-GMN-TEIKIKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-TEIKIKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表時設定処理
      *****************************************************************
       2300-DISPKBN-SET-SEC      SECTION.
      *
           IF    ( SPA-GMN-DISPKBN     NOT =   SPACE )
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-DISPKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
               MOVE    SPA-GMN-DISPKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-DISPKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事時間設定処理
      *****************************************************************
       2300-SKJTIME-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-SKJTIMEJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    SPA-GMN-SKJTIME TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-SKJTIME-SET-EXT.
           EXIT.
      *****************************************************************
      *    日数設定処理
      *****************************************************************
       2300-NISSU-SET-SEC              SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-NISSUJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    SPA-GMN-NISSU     TO  LNK-ODRITEM-MEMO   (IDX-ITM)
      *
           .
       2300-NISSU-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院事由設定処理
      *****************************************************************
       2300-TAIINCD-SET-SEC            SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-TAIINCDJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    SPA-GMN-TAIINCD TO  LNK-ODRITEM-ITEM (IDX-ITM)
           MOVE    SPA-GMN-TAIINCDMEI
                                   TO  LNK-ODRITEM-MEMO (IDX-ITM)
      *
           .
       2300-TAIINCD-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院科設定処理
      *****************************************************************
       2300-NYUINKA-SET-SEC            SECTION.
      *
           IF    ( SPA-NAI-INPUT-COMB-SRYKA    =   "1" )
      *
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-NYUINKAJMA
                                       TO  LNK-ODRITEM-ITEMID (IDX-ITM)
               MOVE    SPA-GMN-NYUINKA TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-NYUINKA-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       2300-HKNCOMBI-SET-SEC           SECTION.
      *
           IF    ( SPA-NAI-INPUT-COMB-SRYKA    =   "1" )
      *
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-HKNCOMBIJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
               MOVE    SPA-GMN-HKNCOMBI
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
           END-IF
      *
           .
       2300-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-SODR-COMMON-SET-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM          TO  SODR-HOSPNUM
           MOVE    ORDER-KARTE-KEY     TO  SODR-KARTE-KEY
           MOVE    ORDER-ORDERNUM      TO  SODR-ORDERNUM
           MOVE    ORDER-NYUGAIKBN     TO  SODR-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  SODR-PTNUM
           MOVE    SPA-NAI-PTID        TO  SODR-PTID
           MOVE    ORDER-SRYKA         TO  SODR-SRYKA
           MOVE    ORDER-SRYYMD        TO  SODR-SRYYMD
           MOVE    ORDER-HKNCOMBI      TO  SODR-HKNCOMBI
           MOVE    ORDER-ORDERID       TO  SODR-ORDERID
           MOVE    ORDER-UKEYMD        TO  SODR-UKEYMD
           MOVE    ORDER-UKEHMS        TO  SODR-UKEHMS
           MOVE    "1"                 TO  SODR-PROCKBN
           MOVE    ORDER-KEYGENERATE   TO  SODR-KEYGENERATE
           MOVE    WRK-INF-DIALOG      TO  SODR-INF-DIALOG
           MOVE    "2"                 TO  SODR-SRCKBN
      *
           MOVE    SPA-OPID            TO  SODR-OPID
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SODR-UPYMD
           MOVE    SMCNDATE-HMS        TO  SODR-UPHMS
      *
           .
       2300-SODR-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-ODRITEM-COMMON-SET-SEC       SECTION.
      *
           COMPUTE    IDX-ITM  =   IDX-ITM +   1
      *
           MOVE    ORDER-HOSPNUM   TO  LNK-ODRITEM-HOSPNUM  (IDX-ITM)
           MOVE    ORDER-KARTE-KEY TO  LNK-ODRITEM-KARTE-KEY(IDX-ITM)
           MOVE    ORDER-ORDERNUM  TO  LNK-ODRITEM-ORDERNUM(IDX-ITM)
           MOVE    IDX-ITM         TO  LNK-ODRITEM-ITEMNUM (IDX-ITM)
      *
           .
       2300-ODRITEM-COMMON-SET-EXT.
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
           IF    ( SPA-I0IDCD      NOT =   SPACE )
               PERFORM 520-I0IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE   I06-ALLIDOLST-ROW    TO  WRK-ALLIDOLST-ROW
           MOVE   I06-PTIDOLST-ROW     TO  WRK-PTIDOLST-ROW
           MOVE   I06-ERRMSGLST-ROW    TO  WRK-ERRMSGLST-ROW
      *
           MOVE    SPACE               TO  I06
           INITIALIZE                      I06
      *
           EVALUATE    FLG-ALLIDOLST-ROW
           WHEN    1
               MOVE    ZERO                TO  I06-ALLIDOLST-ROW
                                               I06-ALLIDOLST-ROWATTR
           WHEN    2
               MOVE    WRK-ALLIDOLST-ROW   TO  I06-ALLIDOLST-ROW
               MOVE    2                   TO  I06-ALLIDOLST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-ALLIDOLST-ROW   TO  I06-ALLIDOLST-ROW
               MOVE    ZERO                TO  I06-ALLIDOLST-ROWATTR
           END-EVALUATE
      *
           IF    ( FLG-PTIDOLST-ROW   =   1 )
               MOVE    ZERO                TO  I06-PTIDOLST-ROW
                                               I06-PTIDOLST-ROWATTR
           ELSE
               MOVE    WRK-PTIDOLST-ROW   TO  I06-PTIDOLST-ROW
               MOVE    ZERO                TO  I06-PTIDOLST-ROWATTR
           END-IF
      *
           IF    ( FLG-ERRMSGLST-ROW   =   1 )
               MOVE    ZERO                TO  I06-ERRMSGLST-ROW
                                               I06-ERRMSGLST-ROWATTR
           ELSE
               MOVE    WRK-ERRMSGLST-ROW   TO  I06-ERRMSGLST-ROW
               MOVE    ZERO                TO  I06-ERRMSGLST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  I06-PTNUM
           MOVE    SPA-GMN-NAME        TO  I06-NAME
           MOVE    SPA-GMN-SEX         TO  I06-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  I06-BIRTHDAY
           MOVE    SPA-GMN-AGE         TO  I06-AGE
           MOVE    SPA-GMN-HSRYKA      TO  I06-HSRYKA
           MOVE    SPA-GMN-HHKNCOMBI   TO  I06-HHKNCOMBI
           MOVE    SPA-GMN-IDOJIYU     TO  I06-IDOJIYU
           MOVE    SPA-GMN-HNYUINYMD   TO  I06-HNYUINYMD
           MOVE    SPA-GMN-LBL-LASTIDOYMD
                                       TO  I06-LBL-LASTIDOYMD
           MOVE    SPA-GMN-LASTIDOYMD  TO  I06-LASTIDOYMD
      *
           MOVE    SPA-GMN-SELNUM      TO  I06-SELNUM
           MOVE    SPA-GMN-SELUKEYMD   TO  I06-SELUKEYMD
           MOVE    SPA-GMN-SELIDOYMD   TO  I06-SELIDOYMD
           MOVE    SPA-GMN-SELPTNUM    TO  I06-SELPTNUM
           MOVE    "blue"              TO  I06-SELNAME-STYLE
           MOVE    SPA-GMN-SELNAME     TO  I06-SELNAME
      *
           MOVE    SPA-GMN-SELSTS-G    TO  I06-SELSTS
           MOVE    SPA-GMN-SELSTS-MAX  TO  I06-SELSTS-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SELSTS-MAX )
               MOVE    SPA-GMN-SELSTSLST (IDX-CMB)
                                       TO  I06-SELSTS-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SELSORT-G    TO  I06-SELSORT
           MOVE    SPA-GMN-SELSORT-MAX  TO  I06-SELSORT-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SELSORT-MAX )
               MOVE    SPA-GMN-SELSORTLST (IDX-CMB)
                                       TO  I06-SELSORT-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-BRMNUM      TO  I06-BRMNUM
      *
           MOVE    SPA-GMN-BTUNAME-G   TO  I06-BTUNAME
           MOVE    SPA-GMN-BTUNAME-MAX TO  I06-BTUNAME-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-BTUNAME-MAX )
               MOVE    SPA-GMN-BTUNAMELST (IDX-CMB)
                                       TO  I06-BTUNAME-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SAGAKU-G    TO  I06-SAGAKU
           MOVE    SPA-GMN-SAGAKU-MAX  TO  I06-SAGAKU-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SAGAKU-MAX )
               MOVE    SPA-GMN-SAGAKULST (IDX-CMB)
                                       TO  I06-SAGAKU-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SENTEIKBN-G    TO  I06-SENTEIKBN
           MOVE    SPA-GMN-SENTEIKBN-MAX  TO  I06-SENTEIKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SENTEIKBN-MAX )
               MOVE    SPA-GMN-SENTEIKBNLST (IDX-CMB)
                                       TO  I06-SENTEIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYUINYMD    TO  I06-NYUINYMD
      *
           MOVE    SPA-GMN-NYUINKA-G   TO  I06-NYUINKA
           MOVE    SPA-GMN-NYUINKA-MAX TO  I06-NYUINKA-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-NYUINKA-MAX )
               MOVE    SPA-GMN-NYUINKALST (IDX-CMB)
                                       TO  I06-NYUINKA-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SHOKAI-G    TO  I06-SHOKAI
           MOVE    SPA-GMN-SHOKAI-MAX  TO  I06-SHOKAI-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SHOKAI-MAX )
               MOVE    SPA-GMN-SHOKAILST (IDX-CMB)
                                       TO  I06-SHOKAI-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SHONUM      TO  I06-SHONUM
      *
           MOVE    SPA-GMN-LBL-SHONYUINYMD
                                       TO  I06-LBL-SHONYUINYMD
      *
           MOVE    SPA-GMN-KAIGOKBN-G    TO  I06-KAIGOKBN
           MOVE    SPA-GMN-KAIGOKBN-MAX  TO  I06-KAIGOKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-KAIGOKBN-MAX )
               MOVE    SPA-GMN-KAIGOKBNLST (IDX-CMB)
                                       TO  I06-KAIGOKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-TAIINYMD    TO  I06-TAIINYMD
      *
           MOVE    SPA-GMN-TAIINCD-G   TO  I06-TAIINCD
           MOVE    SPA-GMN-TAIINCD-MAX TO  I06-TAIINCD-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TAIINCD-MAX )
               MOVE    SPA-GMN-TAIINCDLST (IDX-CMB)
                                       TO  I06-TAIINCD-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-DR-G (1)    TO  I06-DR (1)
           MOVE    SPA-GMN-DR-G (2)    TO  I06-DR (2)
           MOVE    SPA-GMN-DR-G (3)    TO  I06-DR (3)
           MOVE    SPA-GMN-DR-MAX  TO  I06-DR-COUNT (1)
                                       I06-DR-COUNT (2)
                                       I06-DR-COUNT (3)
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-DR-MAX )
               MOVE    SPA-GMN-DRLST (IDX-CMB)
                                   TO  I06-DR-ITEM (1 IDX-CMB)
                                       I06-DR-ITEM (2 IDX-CMB)
                                       I06-DR-ITEM (3 IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-HKNCOMBI-G  TO  I06-HKNCOMBI
           MOVE    SPA-GMN-HKNCOMBI-MAX
                                       TO  I06-HKNCOMBI-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-HKNCOMBI-MAX )
               MOVE    SPA-GMN-HKNCOMBILST (IDX-CMB)
                                       TO  I06-HKNCOMBI-ITEM (IDX-CMB)
           END-PERFORM
      *
           PERFORM 41011-BUNBEN-NYUIN-CHK-SEC
           IF    ( FLG-BUNBENCHK           =   1 )
      *
      *        分娩入院
               MOVE    SPA-GMN-BUNBENKBN-G     TO  I06-BUNBENKBN
               MOVE    SPA-GMN-BUNBENKBN-MAX   TO  I06-BUNBENKBN-COUNT
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL     ( IDX-CMB >       SPA-GMN-BUNBENKBN-MAX )
                   MOVE    SPA-GMN-BUNBENKBNLST (IDX-CMB)
                                       TO  I06-BUNBENKBN-ITEM (IDX-CMB)
               END-PERFORM
      *
               MOVE    SPA-GMN-BUNBENKBN       TO  WRK-BUNBENKBN
               IF    ( SPA-GMN-BUNBENKBN   =   SPACE  )
                   MOVE    SPA-NAI-BUNBENKBN   TO  WRK-BUNBENKBN
               END-IF
      *
               IF    ( WRK-BUNBENKBN       =   "1" OR "2" )
      *
      *            直接支払制度利用区分
                   MOVE    SPA-GMN-RIYOUKBN      TO  I06-RIYOUKBN
                   MOVE    SPA-GMN-RIYOUKBN-MAX  TO  I06-RIYOUKBN-COUNT
                   PERFORM VARYING IDX-CMB FROM  1   BY  1
                           UNTIL ( IDX-CMB >   SPA-GMN-RIYOUKBN-MAX )
                       MOVE    SPA-GMN-RIYOUKBNLST (IDX-CMB)
                                       TO  I06-RIYOUKBN-ITEM (IDX-CMB)
                   END-PERFORM
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-TOKNYUIN-G  TO  I06-TOKNYUIN
           MOVE    SPA-GMN-TOKNYUIN-MAX
                                       TO  I06-TOKNYUIN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TOKNYUIN-MAX )
               MOVE    SPA-GMN-TOKNYUINLST (IDX-CMB)
                                       TO  I06-TOKNYUIN-ITEM (IDX-CMB)
           END-PERFORM
      *
           IF    ( SPA-NAI-NATOKSRYCD  NOT =   SPACE )
               MOVE    SPA-GMN-TOKHIGAITOKBN-G  TO  I06-TOKHIGAITOKBN
               MOVE    SPA-GMN-TOKHIGAITOKBN-MAX
                                       TO  I06-TOKHIGAITOKBN-COUNT
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-GMN-TOKHIGAITOKBN-MAX )
                   MOVE    SPA-GMN-TOKHIGAITOKBNLST (IDX-CMB)
                                   TO  I06-TOKHIGAITOKBN-ITEM (IDX-CMB)
               END-PERFORM
           END-IF
      *
           MOVE    SPA-GMN-SNTNYUIN    TO  I06-SNTNYUIN
      *
           MOVE    SPA-GMN-NACCTKBN-G    TO  I06-NACCTKBN
           MOVE    SPA-GMN-NACCTKBN-MAX  TO  I06-NACCTKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-NACCTKBN-MAX )
               MOVE    SPA-GMN-NACCTKBNLST (IDX-CMB)
                                       TO  I06-NACCTKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-KSNCD-G (1) TO  I06-KSNCD (1)
           MOVE    SPA-GMN-KSNCD-G (2) TO  I06-KSNCD (2)
           MOVE    SPA-GMN-KSNCD-G (3) TO  I06-KSNCD (3)
           MOVE    SPA-GMN-KSNCD-MAX   TO  I06-KSNCD-COUNT (1)
                                           I06-KSNCD-COUNT (2)
                                           I06-KSNCD-COUNT (3)
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-KSNCD-MAX )
               MOVE    SPA-GMN-KSNCDLST (IDX-CMB)
                                       TO  I06-KSNCD-ITEM (1 IDX-CMB)
                                           I06-KSNCD-ITEM (2 IDX-CMB)
                                           I06-KSNCD-ITEM (3 IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-TEIKIKBN-G    TO  I06-TEIKIKBN
           MOVE    SPA-GMN-TEIKIKBN-MAX  TO  I06-TEIKIKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TEIKIKBN-MAX )
               MOVE    SPA-GMN-TEIKIKBNLST (IDX-CMB)
                                       TO  I06-TEIKIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-DISPKBN-G    TO  I06-DISPKBN
           MOVE    SPA-GMN-DISPKBN-MAX  TO  I06-DISPKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-DISPKBN-MAX )
               MOVE    SPA-GMN-DISPKBNLST (IDX-CMB)
                                       TO  I06-DISPKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKJTIME-G    TO  I06-SKJTIME
           MOVE    SPA-GMN-SKJTIME-MAX  TO  I06-SKJTIME-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SKJTIME-MAX )
               MOVE    SPA-GMN-SKJTIMELST (IDX-CMB)
                                       TO  I06-SKJTIME-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NISSU       TO  I06-NISSU
      *
           MOVE    SPA-GMN-ALLIDOLST-MAX   TO  I06-ALLIDOLST-COUNT
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-ALLIDOLST-MAX )
      *
               MOVE    SPA-GMN-TALLNUM    (IDX-LST)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  I06-TALLNUM    (IDX-LST)
               MOVE    SPA-GMN-TALLUKEYMD-G (IDX-LST)
                                           TO  I06-TALLUKEYMD-G(IDX-LST)
               MOVE    SPA-GMN-TALLIDOYMD (IDX-LST)
                                           TO  I06-TALLIDOYMD (IDX-LST)
               MOVE    SPA-GMN-TALLIDOJIYU(IDX-LST)
                                           TO  I06-TALLIDOJIYU(IDX-LST)
               MOVE    SPA-GMN-TALLPTNUM  (IDX-LST) 
                                           TO  I06-TALLPTNUM  (IDX-LST)
               MOVE    SPA-GMN-TALLNAME   (IDX-LST) 
                                           TO  I06-TALLNAME   (IDX-LST)
               EVALUATE    SPA-GMN-TALLSTS (IDX-LST)
               WHEN    1
                   MOVE    "エラー"        TO  I06-TALLERR    (IDX-LST)
               WHEN    2
                   MOVE    "エラー"        TO  I06-TALLERR    (IDX-LST)
               WHEN    3
                   MOVE    "削除"          TO  I06-TALLERR    (IDX-LST)
               WHEN    4
                   MOVE    "未処理"        TO  I06-TALLERR    (IDX-LST)
               WHEN    5
                   MOVE    "保留"          TO  I06-TALLERR    (IDX-LST)
               END-EVALUATE
      *
               MOVE  SPA-GMN-TALLERRCD (IDX-LST)
                                           TO  I06-TALLERRCD  (IDX-LST)
      *
               IF    ( SPA-GMN-ALLIDOLST-SEL   =   IDX-LST )
                   MOVE    "T"             TO  I06-ALLIDOLST-SELECT
                                                             (IDX-LST)
               ELSE
                   MOVE    "F"             TO  I06-ALLIDOLST-SELECT
                                                             (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-PTIDOLST-MAX   TO  I06-PTIDOLST-COUNT
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-PTIDOLST-MAX )
      *
               MOVE    IDX-LST             TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  I06-TPTNUM     (IDX-LST)
               MOVE    SPA-GMN-TPTUKEYMD-G  (IDX-LST)
                                           TO  I06-TPTUKEYMD-G (IDX-LST)
               MOVE    SPA-GMN-TPTIDOYMD  (IDX-LST)
                                           TO  I06-TPTIDOYMD  (IDX-LST)
               MOVE    SPA-GMN-TPTIDOJIYU (IDX-LST)
                                           TO  I06-TPTIDOJIYU (IDX-LST)
               EVALUATE    SPA-GMN-TPTSTS (IDX-LST)
               WHEN    1
                   MOVE    "エラー"        TO  I06-TPTERR    (IDX-LST)
               WHEN    2
                   MOVE    "エラー"        TO  I06-TPTERR    (IDX-LST)
               WHEN    3
                   MOVE    "削除"          TO  I06-TPTERR    (IDX-LST)
               WHEN    4
                   MOVE    "未処理"        TO  I06-TPTERR    (IDX-LST)
               WHEN    5
                   MOVE    "保留"          TO  I06-TPTERR    (IDX-LST)
               END-EVALUATE
      *
               IF    ( SPA-GMN-PTIDOLST-SEL   =   IDX-LST )
                   MOVE    "T"             TO  I06-PTIDOLST-SELECT
                                                               (IDX-LST)
               ELSE
                   MOVE    "F"             TO  I06-PTIDOLST-SELECT
                                                               (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-ERRMSGLST-MAX   TO  I06-ERRMSGLST-COUNT
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-ERRMSGLST-MAX )
      *
               MOVE    SPA-GMN-TERRCD (IDX-LST)
                                           TO  I06-TERRCD (IDX-LST)
               MOVE    SPA-GMN-TERRMSG(IDX-LST)
                                           TO  I06-TERRMSG(IDX-LST)
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-SELNYUKHN       =   1 )
               MOVE    "入院料選択"        TO  I06-LBL-TOKNYUIN
           ELSE
               MOVE    "特定入院料"        TO  I06-LBL-TOKNYUIN
           END-IF
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    "F"                 TO      I06-PTIDOLST-VISIBLE
      *
           MOVE    WIDGET-INSENSITIVE      TO  WRK-STATE
           IF    ( SPA-NAI-INPUT-COMB-SRYKA    =   "1" )
                 MOVE    WIDGET-NORMAL     TO  WRK-STATE
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE  TO      I06-BRMNUM-STATE
                                               I06-BTUNAME-STATE
                                               I06-BTUNAME-ITEM-STATE
                                               I06-SAGAKU-STATE
                                               I06-SAGAKU-ITEM-STATE
                                               I06-SENTEIKBN-STATE
                                               I06-SENTEIKBN-ITEM-STATE
                                               I06-NYUINYMD-STATE
                                               I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-SHOKAI-STATE
                                               I06-SHOKAI-ITEM-STATE
                                               I06-SHONUM-STATE
                                               I06-KAIGOKBN-STATE
                                               I06-KAIGOKBN-ITEM-STATE
                                               I06-TAIINYMD-STATE
                                               I06-TAIINCD-STATE
                                               I06-TAIINCD-ITEM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-TOKNYUIN-STATE
                                               I06-TOKNYUIN-ITEM-STATE
                                               I06-TOKHIGAITOKBN-STATE
                                            I06-TOKHIGAITOKBN-ITEM-STATE
                                               I06-SNTNYUIN-STATE
                                               I06-NACCTKBN-STATE
                                               I06-NACCTKBN-ITEM-STATE
                                               I06-KSNCD-STATE (1)
                                               I06-KSNCD-ITEM-STATE (1)
                                               I06-KSNCD-STATE (2)
                                               I06-KSNCD-ITEM-STATE (2)
                                               I06-KSNCD-STATE (3)
                                               I06-KSNCD-ITEM-STATE (3)
                                               I06-TEIKIKBN-STATE
                                               I06-TEIKIKBN-ITEM-STATE
                                               I06-DISPKBN-STATE
                                               I06-DISPKBN-ITEM-STATE
                                               I06-SKJTIME-STATE
                                               I06-SKJTIME-ITEM-STATE
                                               I06-NISSU-STATE
                                               I06-BUNBENKBN-STATE
                                               I06-BUNBENKBN-ITEM-STATE
                                               I06-RIYOUKBN-STATE
                                               I06-RIYOUKBN-ITEM-STATE
                                               I06-B06-STATE
                                               I06-B06S-STATE
                                               I06-B07-STATE
                                               I06-B07S-STATE
                                               I06-B12-STATE
      *
           IF    ( SPA-GMN-ALLIDOLST-SEL       =   ZERO )
               MOVE    WIDGET-NORMAL   TO      I06-BRMNUM-STATE
                                               I06-BTUNAME-STATE
                                               I06-BTUNAME-ITEM-STATE
                                               I06-SAGAKU-STATE
                                               I06-SAGAKU-ITEM-STATE
                                               I06-SENTEIKBN-STATE
                                               I06-SENTEIKBN-ITEM-STATE
                                               I06-NYUINYMD-STATE
                                               I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-SHOKAI-STATE
                                               I06-SHOKAI-ITEM-STATE
                                               I06-SHONUM-STATE
                                               I06-KAIGOKBN-STATE
                                               I06-KAIGOKBN-ITEM-STATE
                                               I06-TAIINYMD-STATE
                                               I06-TAIINCD-STATE
                                               I06-TAIINCD-ITEM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
                                               I06-TOKNYUIN-STATE
                                               I06-TOKNYUIN-ITEM-STATE
                                               I06-TOKHIGAITOKBN-STATE
                                            I06-TOKHIGAITOKBN-ITEM-STATE
                                               I06-SNTNYUIN-STATE
                                               I06-NACCTKBN-STATE
                                               I06-NACCTKBN-ITEM-STATE
                                               I06-KSNCD-STATE (1)
                                               I06-KSNCD-ITEM-STATE (1)
                                               I06-KSNCD-STATE (2)
                                               I06-KSNCD-ITEM-STATE (2)
                                               I06-KSNCD-STATE (3)
                                               I06-KSNCD-ITEM-STATE (3)
                                               I06-TEIKIKBN-STATE
                                               I06-TEIKIKBN-ITEM-STATE
                                               I06-DISPKBN-STATE
                                               I06-DISPKBN-ITEM-STATE
                                               I06-SKJTIME-STATE
                                               I06-SKJTIME-ITEM-STATE
                                               I06-NISSU-STATE
                                               I06-BUNBENKBN-STATE
                                               I06-BUNBENKBN-ITEM-STATE
                                               I06-RIYOUKBN-STATE
                                               I06-RIYOUKBN-ITEM-STATE
               MOVE    WIDGET-INSENSITIVE  TO  I06-B12-STATE
           ELSE
               IF    ( SPA-NAI-STATUS-ORDER    =   "2" )
                OR   ( SPA-NAI-STATUS-ORDER    =   "5" )
                OR  (( SPA-NAI-STATUS-ORDER    =   "4" )
                 AND ( SPA-NAI-DIRECT-KHNERR   =   ZERO ))
                   EVALUATE    SPA-NAI-ORDERID
                   WHEN    NORDERID-NYUINID
                   WHEN    NORDERID-TEISEIID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-BRMNUM-STATE
                                               I06-BTUNAME-STATE
                                               I06-BTUNAME-ITEM-STATE
                                               I06-SAGAKU-STATE
                                               I06-SAGAKU-ITEM-STATE
                                               I06-SENTEIKBN-STATE
                                               I06-SENTEIKBN-ITEM-STATE
                                               I06-SHOKAI-STATE
                                               I06-SHOKAI-ITEM-STATE
                                               I06-SHONUM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-TOKNYUIN-STATE
                                               I06-TOKNYUIN-ITEM-STATE
                                               I06-NACCTKBN-STATE
                                               I06-NACCTKBN-ITEM-STATE
                                               I06-KSNCD-STATE (1)
                                               I06-KSNCD-ITEM-STATE (1)
                                               I06-KSNCD-STATE (2)
                                               I06-KSNCD-ITEM-STATE (2)
                                               I06-KSNCD-STATE (3)
                                               I06-KSNCD-ITEM-STATE (3)
                                               I06-TEIKIKBN-STATE
                                               I06-TEIKIKBN-ITEM-STATE
                                               I06-DISPKBN-STATE
                                               I06-DISPKBN-ITEM-STATE
                                               I06-B12-STATE
                                               I06-SNTNYUIN-STATE
                       IF    ( SPA-NAI-NATOKSRYCD  NOT =   SPACE )
                           MOVE    WIDGET-NORMAL
                                       TO  I06-TOKHIGAITOKBN-STATE
                                           I06-TOKHIGAITOKBN-ITEM-STATE
                       END-IF
      *
                       IF   ( SPA-GMN-SHOKAI      =   "2" )
                          MOVE    WIDGET-NORMAL
                                           TO  I06-KAIGOKBN-STATE
                          MOVE    WIDGET-NORMAL
                                           TO  I06-KAIGOKBN-ITEM-STATE
                       END-IF
      *
                       PERFORM 5001-GMNHEN-STATE-EDIT-SEC
      *
                   WHEN    NORDERID-TAIINID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-TAIINCD-STATE
                                               I06-TAIINCD-ITEM-STATE
                                               I06-B12-STATE
                   WHEN    NORDERID-HENKOID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-TEIKIKBN-STATE
                                               I06-TEIKIKBN-ITEM-STATE
                                               I06-DISPKBN-STATE
                                               I06-DISPKBN-ITEM-STATE
                                               I06-B12-STATE
      *
                       PERFORM 5001-GMNHEN-STATE-EDIT-SEC
      *
                   WHEN    NORDERID-GAIHKID
                   WHEN    NORDERID-GBACKID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-B12-STATE
                   WHEN    NORDERID-FNORLID
                   WHEN    NORDERID-FSPCLID
                   WHEN    NORDERID-FSELEID
                   WHEN    NORDERID-FSTOPID
                   WHEN    NORDERID-FSPSLID
                   WHEN    NORDERID-FLIQUIDID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-SKJTIME-STATE
                                               I06-SKJTIME-ITEM-STATE
                                               I06-NISSU-STATE
                                               I06-B12-STATE
                   WHEN    NORDERID-IDODRID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-B12-STATE
                   WHEN    NORDERID-IDOBTUID
                   WHEN    NORDERID-IDOBRMID
                   WHEN    NORDERID-IDOKAID
                   WHEN    NORDERID-IDOALLID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-BRMNUM-STATE
                                               I06-BTUNAME-STATE
                                               I06-BTUNAME-ITEM-STATE
                                               I06-SAGAKU-STATE
                                               I06-SAGAKU-ITEM-STATE
                                               I06-DR-STATE      (1)
                                               I06-DR-ITEM-STATE (1)
                                               I06-DR-STATE      (2)
                                               I06-DR-ITEM-STATE (2)
                                               I06-DR-STATE      (3)
                                               I06-DR-ITEM-STATE (3)
                                               I06-TOKNYUIN-STATE
                                               I06-TOKNYUIN-ITEM-STATE
                                               I06-B12-STATE
                                               I06-SNTNYUIN-STATE
                       IF    ( SPA-NAI-NATOKSRYCD  NOT =   SPACE )
                           MOVE    WIDGET-NORMAL
                                           TO  
                                            I06-TOKHIGAITOKBN-STATE
                                            I06-TOKHIGAITOKBN-ITEM-STATE
                       END-IF
                       IF    ( SPA-NAI-ORDERID =   NORDERID-IDOALLID )
                          MOVE    WIDGET-NORMAL
                                           TO  I06-SENTEIKBN-STATE
                                               I06-SENTEIKBN-ITEM-STATE
                                               I06-NACCTKBN-STATE
                                               I06-NACCTKBN-ITEM-STATE
                                               I06-KSNCD-STATE (1)
                                               I06-KSNCD-ITEM-STATE (1)
                                               I06-KSNCD-STATE (2)
                                               I06-KSNCD-ITEM-STATE (2)
                                               I06-KSNCD-STATE (3)
                                               I06-KSNCD-ITEM-STATE (3)
      *
                           PERFORM 5001-GMNHEN-STATE-EDIT-SEC
      *
                       END-IF
                   WHEN    NORDERID-DELNYUINID
                   WHEN    NORDERID-ALLDELNYUINID
                   WHEN    NORDERID-CANTAIINID
                   WHEN    NORDERID-CANIDOID
                       MOVE    WIDGET-NORMAL
                                           TO  I06-B12-STATE
                   WHEN    NORDERID-GPTSTID
                   WHEN    NORDERID-FOODID
                   WHEN    NORDERID-PTEVALID
                   WHEN    NORDERID-BUNBENID
                   WHEN    NORDERID-CRENACCTID
                   WHEN    NORDERID-SAGAKUID
                       CONTINUE
                   END-EVALUATE
      *
                   MOVE    WRK-STATE       TO  I06-NYUINKA-STATE
                                               I06-NYUINKA-ITEM-STATE
                                               I06-HKNCOMBI-STATE
                                               I06-HKNCOMBI-ITEM-STATE
      *
                   IF    ( SPA-GMN-SHOKAI      = "1" )
                       MOVE    WIDGET-INSENSITIVE
                                           TO  I06-SHONUM-STATE
                   END-IF
      *
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-SELUKEYMD   NOT =   SPACE )
               MOVE    WIDGET-NORMAL       TO  I06-B06-STATE
                                               I06-B07-STATE
           END-IF
      *
           IF    ( SPA-GMN-ALLIDOLST-PAGE  >   1 )
               MOVE    WIDGET-NORMAL       TO  I06-B06S-STATE
           END-IF
      *
           IF    ( SPA-GMN-ALLIDOLST-NEXT  =   1 )
               MOVE    WIDGET-NORMAL       TO  I06-B07S-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       5001-GMNHEN-STATE-EDIT-SEC      SECTION.
      *
           IF    ( I06-RIYOUKBN-COUNT  >   ZERO )
               MOVE    WIDGET-NORMAL   TO  I06-RIYOUKBN-STATE
                                           I06-RIYOUKBN-ITEM-STATE
           END-IF
      *
           IF    ( I06-BUNBENKBN-COUNT >   ZERO )
               MOVE    WIDGET-NORMAL   TO  I06-BUNBENKBN-STATE
                                           I06-BUNBENKBN-ITEM-STATE
           END-IF
      *
           .
       5001-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5011-INPUT-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    02
               MOVE    "SELUKEYMD"     TO  MCP-WIDGET
           WHEN    03
               MOVE    "BRMNUM"        TO  MCP-WIDGET
           WHEN    04
               MOVE    "BTUNAME"       TO  MCP-WIDGET
           WHEN    05
               MOVE    "SAGAKU"        TO  MCP-WIDGET
           WHEN    06
               MOVE    "NYUINYMD"      TO  MCP-WIDGET
           WHEN    07
               MOVE    "NYUINKA"       TO  MCP-WIDGET
           WHEN    08
               MOVE    "SHOKAI"        TO  MCP-WIDGET
           WHEN    09
               MOVE    "SHONUM"        TO  MCP-WIDGET
           WHEN    10
               MOVE    "TAIINYMD"      TO  MCP-WIDGET
           WHEN    11
               MOVE    "TAIINCD"       TO  MCP-WIDGET
           WHEN    12
               MOVE    "DR1"           TO  MCP-WIDGET
           WHEN    13
               MOVE    "DR2"           TO  MCP-WIDGET
           WHEN    14
               MOVE    "DR3"           TO  MCP-WIDGET
           WHEN    15
               MOVE    "HKNCOMBI"      TO  MCP-WIDGET
           WHEN    17
               MOVE    "TOKNYUIN"      TO  MCP-WIDGET
           WHEN    18
               MOVE    "SKJTIME"       TO  MCP-WIDGET
           WHEN    19
               MOVE    "NISSU"         TO  MCP-WIDGET
           WHEN    20
               MOVE    "SELIDOYMD"     TO  MCP-WIDGET
           WHEN    21
               MOVE    "SELPTNUM"      TO  MCP-WIDGET
           WHEN    22
               MOVE    "SELSTS"        TO  MCP-WIDGET
           WHEN    23
               MOVE    "SELSORT"       TO  MCP-WIDGET
           WHEN    24
               MOVE    "SENTEIKBN"     TO  MCP-WIDGET
           WHEN    25
               MOVE    "KAIGOKBN"      TO  MCP-WIDGET
           WHEN    26
               MOVE    "TOKHIGAITOKBN" TO  MCP-WIDGET
           WHEN    27
               MOVE    "NACCTKBN"      TO  MCP-WIDGET
           WHEN    28
               MOVE    "KSNCD01"       TO  MCP-WIDGET
           WHEN    29
               MOVE    "KSNCD02"       TO  MCP-WIDGET
           WHEN    30
               MOVE    "KSNCD03"       TO  MCP-WIDGET
           WHEN    31
               MOVE    "TEIKIKBN"      TO  MCP-WIDGET
           WHEN    32
               MOVE    "DISPKBN"       TO  MCP-WIDGET
           WHEN    33
               MOVE    "BUNBENKBN"     TO  MCP-WIDGET
           WHEN    34
               MOVE    "RIYOUKBN"      TO  MCP-WIDGET
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
           WHEN    "SELNUM"
               MOVE    03              TO  SPA-GMN-CUR
           WHEN    "SELUKEYMD"
               MOVE    20              TO  SPA-GMN-CUR
           WHEN    "SELIDOYMD"
               MOVE    21              TO  SPA-GMN-CUR
           WHEN    "SELPTNUM"
               MOVE    22              TO  SPA-GMN-CUR
           WHEN    "SELSTS"
               MOVE    23              TO  SPA-GMN-CUR
           WHEN    "SELSORT"
               MOVE    03              TO  SPA-GMN-CUR
           WHEN    "BRMNUM"
               MOVE    04              TO  SPA-GMN-CUR
           WHEN    "BTUNAME"
               MOVE    05              TO  SPA-GMN-CUR
           WHEN    "SAGAKU"
               MOVE    24              TO  SPA-GMN-CUR
           WHEN    "SENTEIKBN"
               MOVE    06              TO  SPA-GMN-CUR
           WHEN    "NYUINYMD"
               MOVE    07              TO  SPA-GMN-CUR
           WHEN    "NYUINKA"
               MOVE    08              TO  SPA-GMN-CUR
           WHEN    "SHOKAI"
               MOVE    09              TO  SPA-GMN-CUR
           WHEN    "SHONUM"
               MOVE    25              TO  SPA-GMN-CUR
           WHEN    "KAIGOKBN"
               MOVE    10              TO  SPA-GMN-CUR
           WHEN    "TAIINYMD"
               MOVE    11              TO  SPA-GMN-CUR
           WHEN    "TAIINCD"
               MOVE    12              TO  SPA-GMN-CUR
           WHEN    "DR1"
               MOVE    13              TO  SPA-GMN-CUR
           WHEN    "DR2"
               MOVE    14              TO  SPA-GMN-CUR
           WHEN    "DR3"
               MOVE    15              TO  SPA-GMN-CUR
           WHEN    "HKNCOMBI"
               MOVE    33              TO  SPA-GMN-CUR
           WHEN    "BUNBENKBN"
               MOVE    34              TO  SPA-GMN-CUR
           WHEN    "RIYOUKBN"
               MOVE    17              TO  SPA-GMN-CUR
           WHEN    "TOKNYUIN"
               MOVE    26              TO  SPA-GMN-CUR
           WHEN    "TOKHIGAITOKBN"
               MOVE    27              TO  SPA-GMN-CUR
           WHEN    "NACCTKBN"
               MOVE    28              TO  SPA-GMN-CUR
           WHEN    "KSNCD01"
               MOVE    29              TO  SPA-GMN-CUR
           WHEN    "KSNCD02"
               MOVE    30              TO  SPA-GMN-CUR
           WHEN    "KSNCD03"
               MOVE    31              TO  SPA-GMN-CUR
           WHEN    "TEIKIKBN"
               MOVE    32              TO  SPA-GMN-CUR
           WHEN    "DISPKBN"
               MOVE    18              TO  SPA-GMN-CUR
           WHEN    "SKJTIME"
               MOVE    19              TO  SPA-GMN-CUR
           WHEN    "NISSU"
               MOVE    212             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5011-INPUT-CUR-SKIP-SEC         SECTION.
      *
           IF    ( SPA-GMN-CUR         =   3 )
               IF    ( I06-BRMNUM-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   4 )
               IF    ( I06-BTUNAME-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   5 )
               IF    ( I06-SAGAKU-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    24          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   24 )
               IF    ( I06-SENTEIKBN-STATE =   WIDGET-INSENSITIVE )
                   MOVE    6           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   6 )
               IF    ( I06-NYUINYMD-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    7           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   7 )
               IF    ( I06-NYUINKA-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   8 )
               IF    ( I06-SHOKAI-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    9           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   9 )
               IF    ( I06-SHONUM-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    25          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   25 )
               IF    ( I06-KAIGOKBN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    10           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   10 )
               IF    ( I06-TAIINYMD-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    11           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   11 )
               IF    ( I06-TAIINCD-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    12           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   12 )
               IF    ( I06-DR-STATE (1)    =   WIDGET-INSENSITIVE )
                   MOVE    13           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   13 )
               IF    ( I06-DR-STATE (2)    =   WIDGET-INSENSITIVE )
                   MOVE    14          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   14 )
               IF    ( I06-DR-STATE (3)    =   WIDGET-INSENSITIVE )
                   MOVE    15          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   15 )
               IF    ( I06-HKNCOMBI-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    33          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   33 )
               IF    ( I06-BUNBENKBN-STATE =   WIDGET-INSENSITIVE )
                   MOVE    34          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   34 )
               IF    ( I06-RIYOUKBN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    17          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   17 )
               IF    ( I06-TOKNYUIN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    26          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   26 )
               IF    ( I06-TOKHIGAITOKBN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    27          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   27 )
               IF    ( I06-NACCTKBN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    28          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   28 )
               IF    ( I06-KSNCD-STATE (1) =   WIDGET-INSENSITIVE )
                   MOVE    29          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   29 )
               IF    ( I06-KSNCD-STATE (2) =   WIDGET-INSENSITIVE )
                   MOVE    30          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   30 )
               IF    ( I06-KSNCD-STATE (3) =   WIDGET-INSENSITIVE )
                   MOVE    31          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   31 )
               IF    ( I06-TEIKIKBN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    32          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   32 )
               IF    ( I06-DISPKBN-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    18          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   18 )
               IF    ( I06-SKJTIME-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    19          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   19 )
               IF    ( I06-NISSU-STATE     =   WIDGET-INSENSITIVE )
                   MOVE    212         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   212 )
               IF    ( I06-B12-STATE   =   WIDGET-INSENSITIVE )
                   IF    ( SPA-GMN-ALLIDOLST-MAX   =   ZERO )
                       MOVE    2       TO  SPA-GMN-CUR
                   ELSE
                       MOVE    1       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       5011-INPUT-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "受付日の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "病室番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "病棟名の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "室料差額の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "入院科の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "初回の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "初歴の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "退院事由の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "担当医の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "保険組合せの入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "入院区分の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "特定入院料の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "食事時間の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0019"
               IF    ( WRK-ERRMSG      =   SPACE )
                   MOVE    "一括入力欄の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
               ELSE
                   MOVE    WRK-ERRMSG  TO  SPA-ERRMSG
               END-IF
           WHEN    "0020"
               MOVE    "一括入力欄は半角文字で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "一括入力欄の一文字目は＊を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0022"
               MOVE    "状態の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0023"
               MOVE    "異動日の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0024"
               MOVE    "患者番号の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0025"
               MOVE    "該当する患者が存在しません" 
                                       TO  SPA-ERRMSG
           WHEN    "0026"
               MOVE    "並び順の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0027"
               MOVE    "選定入院の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0028"
               MOVE    "介護からの異動の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0029"
               MOVE    "算定要件非該当区分の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0030"
               MOVE    "入院会計の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0031"
               MOVE    "入院料加算の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0034"
               MOVE    "定期請求の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0035"
               MOVE    "検索時患者表示の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0036"
               MOVE    "分娩区分の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "0037"
               MOVE    "直接支払制度利用区分の入力に誤りがあります" 
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "異動歴を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4011"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I06"               TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I0ERR"             TO  MCP-WINDOW
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
       520-I0IDSET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-I0IDMSG
      *
           EVALUATE    SPA-I0IDCD
           WHEN    "2001"
               MOVE    "異動歴を削除します"
                                       TO  WRK-I0IDMSG
           WHEN    "2002"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "定期請求が月末のみに変更されています。"
                       "月途中で作成した請求データは取消します"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "2003"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "退院後の診療日で入力された診療行為は"
                       "削除されます。確認してください"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "2004"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院日が他院退院日と重複します。"
                       "他院歴の通算日数を減算しますか？"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "2005"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院日が他院退院日と重複します。"
                       "よろしいですか？"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ID1
           INITIALIZE                      I0ID1
           MOVE    SPA-I0IDCD          TO  I0ID1-ID1CODE
           MOVE    WRK-I0IDMSG         TO  I0ID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I06"               TO  SPA-MOTOPG
           MOVE    "I0ID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I0ID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-I0IDSET-EXT.
           EXIT.
      *****************************************************************
      *    老人年齢チェック
      *****************************************************************
       800-ROUJINAGE-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-ROUJIN
                                       FLG-LIFE-RJN
      *
           PERFORM 31011-ROUJIN-CHK-SEC
      *
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM          TO  AGECHK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  AGECHK-PTID
           MOVE    SPA-NAI-KJNYMD      TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
           MOVE    SPA-GMN-HKNCOMBI    TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING   ORCSAGECHKAREA
                                               SPA-AREA
           IF    ( AGECHK-RC       NOT =   ZERO )
               INITIALIZE              ORCSAGECHKAREA
           END-IF
           IF    ( AGECHK-ROUJIN2      =   1 )
               MOVE    1               TO  FLG-ROUJIN
           END-IF
      *
           IF    ( SPA-NAI-HKNNUM  NOT =  "971" AND "973" )
             OR  ( AGECHK-HKNKBN       =  4 OR 5 OR 7 )
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-KJNYMD         >=  CONST-H200401 )
                   IF    ( AGECHK-NENREI2-YY  >=  65 )
                    OR   ( FLG-RJN             =  1 )
                       MOVE    1           TO  FLG-LIFE-RJN
                   END-IF
               WHEN  ( SPA-NAI-KJNYMD         >=  CONST-H181001 )
                   MOVE    FLG-ROUJIN      TO  FLG-LIFE-RJN
                   IF    ( AGECHK-NENREI2-YY  >=  70 )
                    OR   ( FLG-RJN             =  1 )
                       MOVE    1           TO  FLG-LIFE-RJN
                   END-IF
               END-EVALUATE
      *
               IF    ( FLG-LIFE-RJN    =   1 )
                   IF    ( AGECHK-HKNKBN   =   4 OR 5 )
                       MOVE    2       TO  FLG-LIFE-RJN
                   END-IF
               END-IF
           END-IF
      *
           .
       800-ROUJINAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
      *
           MOVE    ZERO                TO  WRK-SYMD
           MOVE    SPACE               TO  WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           IF  ( WRK-INPUT             =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-INPUT       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-INPUT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1       TO  FLG-ERR
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料非該当チェック処理
      *****************************************************************
       800-TOKHIGAITO-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-TOKHIGAITO-G
      *
           INITIALIZE                      ORCSTOKNYUIN-AREA
           MOVE    WRK-TOKNYUIN-KBN    TO  LNK-TOKNYUIN-KBN
           MOVE    WRK-TOKNYUIN-CD     TO  LNK-TOKNYUIN-NYUIN
           MOVE    SPA-GMN-BTUNAME     TO  LNK-TOKNYUIN-BTUNUM
           MOVE    "1"                 TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
           MOVE    SPA-NAI-KJNYMD      TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF    ( LNK-TOKNYUIN-RC          =   ZERO )
            AND  ( LNK-TOKNYUIN-GAI-SRYCD   NOT =  SPACE )
               MOVE    LNK-TOKNYUIN-GAI-SRYCD
                                       TO  WRK-TOKHIGAITO-CD
           END-IF
      *
           .
       800-TOKHIGAITO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY15-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY15-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY15-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY15-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO   HKNCOMBI-REC
           ELSE
               INITIALIZE                   HKNCOMBI-REC
               MOVE    1               TO   FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理(KEY6)
      *****************************************************************
       900-PTKOHINF-KEY6-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKOHINF
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTKOHINF-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報検索処理(KEY11)
      *****************************************************************
       900-PTHKNINF-KEY11-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTHKNINF
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTHKNINF-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダー検索
      *****************************************************************
       900-ORDER-KEY8-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-ORDER
      *
           INITIALIZE                  ORDER-REC
           MOVE    SPA-HOSPNUM     TO  ORDER-HOSPNUM
           MOVE    SPA-NAI-SELUKEYMD
                                   TO  ORDER-UKEYMD
           MOVE    SPA-NAI-SELIDOYMD
                                   TO  ORDER-SRYYMD
           MOVE    SPA-NAI-SELPTID TO  ORDER-PTID
      *
           IF    ( SPA-GMN-SELSTS  =   "1" )
               MOVE    SPA-GMN-SELSTS
                                   TO  ORDER-SELSTATUS
               MOVE    ZERO        TO  ORDER-STATUS
           END-IF
      *
           MOVE    SPA-GMN-SELSORT TO  ORDER-SELSORT
      *
           COMPUTE ORDER-SELLIMIT
               =   CONST-ALLIDOLST-MAX +   1
      *
           COMPUTE ORDER-SELOFFSET
               =   CONST-ALLIDOLST-MAX
               * ( SPA-GMN-ALLIDOLST-PAGE  -   1 )
      *
           MOVE    ORDER-REC       TO  MCPDATA-REC
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
      *
           .
       900-ORDER-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダーFETCH
      *****************************************************************
       900-ORDER-KEY8-FET-SEC          SECTION.
      *
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
           .
       900-ORDER-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダー検索
      *****************************************************************
       900-ORDER-KEY9-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-ORDER
      *
           MOVE    SPA-NAI-ORDER-DAT
                                   TO  ORDER-REC
      *
           MOVE    SPA-HOSPNUM     TO  ORDER-HOSPNUM
           MOVE    SPA-NAI-SELUKEYMD
                                   TO  ORDER-UKEYMD
           MOVE    SPA-NAI-SELIDOYMD
                                   TO  ORDER-SRYYMD
           MOVE    SPA-NAI-SELPTID TO  ORDER-PTID
      *
           IF    ( SPA-GMN-SELSTS  =   "1" )
               MOVE    SPA-GMN-SELSTS
                                   TO  ORDER-SELSTATUS
               MOVE    ZERO        TO  ORDER-STATUS
           END-IF
      *
           MOVE    SPA-GMN-SELSORT TO  ORDER-SELSORT
      *
           MOVE    ORDER-REC       TO  MCPDATA-REC
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダー検索(KEY4)
      *****************************************************************
       900-ORDER-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-ORDER
      *
           INITIALIZE                  ORDER-REC
           MOVE    SPA-HOSPNUM      TO  ORDER-HOSPNUM
           MOVE    SPA-GMN-PTNUM   TO  ORDER-PTNUM
           MOVE    ORDER-REC       TO  MCPDATA-REC
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
      *
           .
       900-ORDER-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダーFETCH(KEY4)
      *****************************************************************
       900-ORDER-KEY4-FET-SEC          SECTION.
      *
           MOVE    "tbl_order"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ORDER-REC
           ELSE
               INITIALIZE              ORDER-REC
               MOVE    1           TO  FLG-ORDER
           END-IF
      *
           .
       900-ORDER-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダー検索(KEY)
      *****************************************************************
       900-ORDER-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           INITIALIZE                      ORDER-REC
           MOVE    SPA-HOSPNUM          TO  ORDER-HOSPNUM
           MOVE    SPA-NAI-KARTE-KEY   TO  ORDER-KARTE-KEY
           MOVE    SPA-NAI-ORDERNUM    TO  ORDER-ORDERNUM
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  ORDER-REC
           ELSE
               INITIALIZE                  ORDER-REC
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダ項目検索(KEY4)
      *****************************************************************
       900-ODRITEM-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-ODRITEM
      *
           INITIALIZE                      ODRITEM-REC
           MOVE    SPA-HOSPNUM          TO  ODRITEM-HOSPNUM
           MOVE    SPA-NAI-KARTE-KEY   TO  ODRITEM-KARTE-KEY
           MOVE    SPA-NAI-ORDERNUM    TO  ODRITEM-ORDERNUM
           MOVE    ODRITEM-REC     TO  MCPDATA-REC
           MOVE    "tbl_odritem"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ODRITEM-REC
           ELSE
               INITIALIZE              ODRITEM-REC
               MOVE    1           TO  FLG-ODRITEM
           END-IF
      *
           .
       900-ODRITEM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダ項目FETCH(KEY4)
      *****************************************************************
       900-ODRITEM-KEY4-FET-SEC          SECTION.
      *
           MOVE    "tbl_odritem"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ODRITEM-REC
           ELSE
               INITIALIZE              ODRITEM-REC
               MOVE    1           TO  FLG-ODRITEM
           END-IF
      *
           .
       900-ODRITEM-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    WRK-PTINF-PTID  TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY3)
      *****************************************************************
       900-PTNYUINRRK-KEY3-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY42)
      *****************************************************************
       900-PTNYUINRRK-KEY42-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-HIDOYMD TO  PTNYUINRRK-TENNYUYMD
                                       PTNYUINRRK-TENSTUYMD
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY42-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY79-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key79"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key79"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY79-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY80-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key80"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key80"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY80-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY63-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-HIDOYMD     TO  PTNYUINRRK-NYUINYMD
                                           PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key63"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key63"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY63-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY64-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key64"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key64"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY64-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY78-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key78"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key78"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY78-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    WRK-PTNUM-PTID      TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY2)
      *****************************************************************
       900-PTNUM-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           IF    ( WRK-PTNUM-PTNUM     =   SPACE )
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           ELSE
      *
               INITIALIZE                  PTNUM-REC
               MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
               STRING  WRK-PTNUM-PTNUM     DELIMITED   BY  SPACE
                       "%"                 DELIMITED   BY  SIZE
               INTO    PTNUM-PTNUM
               END-STRING
      *
               MOVE    PTNUM-REC           TO  MCPDATA-REC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
               ELSE
                   INITIALIZE                  PTNUM-REC
                   MOVE    1               TO  FLG-PTNUM
               END-IF
      *
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-PTNUM-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY23-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY7)
      *****************************************************************
       900-SYSKANRI-KEY23-FET-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
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
       911-DBSELECT-EXT.
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
