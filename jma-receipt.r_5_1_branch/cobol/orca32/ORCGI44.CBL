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
      ******************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGI44.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 診療行為入力（Ｉ４４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.11.17  更新日付編集対応
      *  02.00.00    NACL-多々納  04.05.06  チェック追加等
      *  02.00.01    NACL-小豆沢  06.03.06  主科設定対応追加
      *  02.00.02    NACL-多々納  06/06/19  MONFUNC対応他
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
      *  04.08.00    NACL-多々納  18/05/18  施設基準チェック修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "I4SCR-NYU.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "I44SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SRYKBN          PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-KNJBYO          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
      *
           03  FLG-GMN             PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SST             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
      *    セット処理有無
           03  FLG-SETSYORI        PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
      *    漢字短縮
           03  FLG-TANSYUKU        PIC 9(01).
      *    手技
           03  FLG-SYUGI           PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *    空白入力
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-INS             PIC 9(01).
           03  FLG-MEI             PIC 9(01).
      *
           03  FLG-ZENKAKU         PIC 9(01).
           03  FLG-HANKAKU         PIC 9(01).
      *
      *    施設基準チェック
           03  FLG-SST1                PIC 9(01).
           03  FLG-SST2                PIC 9(01).
           03  FLG-SST3                PIC 9(01).
           03  FLG-SSA1                PIC 9(01).
           03  FLG-SSA2                PIC 9(01).
           03  FLG-SSA3                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDS                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
           03  IDY-STR             PIC 9(04).
           03  IDX-SIN             PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX-SS2             PIC 9(04).
           03  IDX-D1              PIC 9(02).
           03  IDX-D2              PIC 9(02).
           03  IDX-ATO             PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
           03  IDX-I48             PIC 9(04).
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-FREE            PIC 9(04).
           03  WRK-IDX             PIC 9(03).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-IN-MAP-IDX      PIC 9(02).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZZZ.
               05  WRK-SURYO-Z19   REDEFINES  WRK-SURYO-Z1
                                   PIC ZZZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZ.
           03  WRK-TENSU-X.
               05  WRK-TEN-F1      PIC X(08).
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-TENSUS-X.
               05  WRK-TENSUS-Z    PIC ZZZZ9.99.
      *
      **** 03  WRK-ZAITEN          PIC 9(08).
      **** 03  WRK-ZAITEN-90       PIC 9(08)V9(03).
           03  WRK-ZAITEN-90       PIC 9(08).
      *
      *    点数計算
      **** 03  WRK-ZAITEN-G            PIC 9(08).
           03  WRK-KASAN               PIC 9(08)V999.
           03  WRK-KASAN-ALL           PIC 9(08)V999.
           03  WRK-GENZAN              PIC 9(08)V999.
           03  WRK-GENZAN-ALL          PIC 9(08)V999.
      *
           03  WRK-KINGAK          PIC 9(08).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
      *
           03  WRK-SRYCD           PIC X(09).
      *    会計情報
           03  WRK-SRYACCT-AREA.
               05  WRK-SRYCD-9                 PIC 9(09).
               05  WRK-TEN                     PIC S9(08).
               05  WRK-TENSU                   PIC S9(08).
      *
               05  WRK-ZAITEN                  PIC 9(08).
               05  WRK-ZAITEN-G                PIC 9(08).
               05  WRK-SRYCDTOTAL              PIC 9(14).
               05  WRK-SURYOUTOTAL             PIC 9(07)V9(03).
               05  WRK-MEISAISU                PIC 9(07).
               05  WRK-ZAIKAISU                PIC 9(07).
      *
      ***  03  WRK-SURYOKEI        PIC 9(07)V9(03).
      *    03  WRK-SRYCDKEI        PIC 9(14).
      *    03  WRK-MEISAISU        PIC 9(07).
      *    03  WRK-SRYCD-9         PIC 9(09).
      *
           03  WRK-MEINUM          PIC 9(04).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
      *
           03  WRK-MKBN            PIC X(01).
      *    警告済フラグ保存
      **** 03  WRK-TIMEFLG         PIC X(01).
      *    入力位置
           03  WRK-FLG-IN          PIC X(01).
      *    入力セット処理
      **** 03  WRK-SETCD           PIC X(06).
      *
           03  WRK-GMN-IDX         PIC 9(04).
      *    入力位置判定用
           03  WRK-IN-IDX4         PIC 9(04).
           03  WRK-IN-IDX2         PIC 9(04).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-DAY             PIC 9(03).
      *    入力時表示用
           03  FLG-NYURYOKU        PIC 9(01).
           03  FLG-IN-ATAI         PIC 9(01).
      *    画面制御
           03  WRK-PUTTYPE         PIC X(08).
           03  WRK-JIDMSG          PIC X(80).
      *
           03  WRK-INPUTCD         PIC X(22).
           03  WRK-INPUTCD2        PIC X(22).
           03  WRK-INPUTCD3        PIC X(22).
           03  WRK-CHK-FLG         PIC X(01).
           03  WRK-STR             PIC 9(02).
           03  WRK-END             PIC 9(02).
           03  WRK-HENKAN          PIC X(22).
           03  WRK-CD              PIC X(22).
           03  WRK-WK-CD           PIC X(22).
           03  WRK-CD-MCNT         PIC 9(02).
           03  WRK-ICD-CDSYU       PIC X(01).
      *
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1   PIC X(10).
               05  WRK-HZN-ATAI2   PIC X(10).
               05  WRK-HZN-ATAI3   PIC X(10).
               05  WRK-HZN-ATAI4   PIC X(10).
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH        PIC X(02).
               05  WRK-MEISYO      PIC X(60).
      *    施設規準テーブル
       01  WRK-TBL-SSTKIJUN-AREA.
           03  WRK-TBL-SSTKIJUN-OCC       OCCURS  9999.
               05  WRK-TBL-SSTKIJUN       PIC 9(01).
      *
      *    行位置編集
       01  WRK-GMNGYO-AREA.
           03  WRK-MAP-MAX         PIC 9(04).
           03  WRK-MAP-CUR         PIC 9(04).
           03  WRK-MAP-CUR2        PIC 9(04).
           03  WRK-MAP-CUR3        PIC 9(04).
           03  WRK-PAGE            PIC 9(04).
           03  WRK-MAP-PAGE        PIC 9(04).
           03  WRK-MAP-PAGE2       PIC 9(04).
           03  WRK-MAP-PAGE3       PIC 9(04).
           03  WRK-MAP-MAXPAGE     PIC 9(04).
      *
      *    画面数量編集用
       01  WRK-GMN-SURYO-AREA.
           03  WRK-SURYO-HENSYU.
               05  WRK-H-SURYO     PIC X(09).
               05  WRK-H-TANINAME  PIC X(04).
               05  WRK-H-F1        PIC X(02).
               05  WRK-H-ZAIKEI    PIC X(08).
           03  WRK-ZZ              PIC ZZ.
      *
      *    診療行為内容比較用テーブル
       01  TBL-AREA.
           03  TBL-MAE-AREA.
               05  TBL-MAE-REC     OCCURS   30.
                   07  TBL-MAE-SRYCD       PIC  X(09).
                   07  TBL-MAE-SRYSURYO    PIC  9(05)V9(03).
                   07  TBL-MAE-SRYKAISU    PIC  9(03).
                   07  TBL-MAE-INPUTCOMENT PIC  X(40).
                   07  TBL-MAE-ATAI-G.
                       09  TBL-MAE-ATAI    PIC  X(08)  OCCURS  3.
           03  TBL-ATO-AREA.
               05  TBL-ATO-REC     OCCURS   30.
                   07  TBL-ATO-SRYCD       PIC  X(09).
                   07  TBL-ATO-SRYSURYO    PIC  9(05)V9(03).
                   07  TBL-ATO-SRYKAISU    PIC  9(03).
                   07  TBL-ATO-INPUTCOMENT PIC  X(40).
                   07  TBL-ATO-ATAI-G.
                       09  TBL-ATO-ATAI    PIC  X(08)  OCCURS  3.
      *
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
      *
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-JKNKSN          PIC 9(02).
           03  WRK-IDX-FT          PIC 9(02).
           03  WRK-KAI             PIC X(20).
           03  WRK-SUR             PIC X(20).
           03  IDM                 PIC 9(04).
      *
           03  WRK-HENKAN-MAE      PIC X(20).
           03  WRK-HENKAN-ATO      PIC X(20).
           03  WRK-JODOU           PIC 9(01).
           03  WRK-KENSAKU         PIC X(01).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  WRK-TEN-GMN-IDX     PIC 9(04).
           03  WRK-TEN-SRYCD       PIC X(09).
           03  WRK-TEN-NENREI.
               05  WRK-TEN-NENREI-YY     PIC 9(03).
               05  WRK-TEN-NENREI-MM     PIC 9(02).
               05  WRK-TEN-NENREI-DD     PIC 9(02).
           03  WRK-TEN-ROUJIN            PIC 9(01).
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
           03  WRK-MAX             PIC 9(04).
           03  WRK-TIME            PIC 9(01).
      *    老人・一般変換用
           03  WRK-SRYCD-CHG-AREA.
               05  WRK-SRYCDCHG        PIC X(09)   OCCURS  5.
           03  WRK-ATAI-X.
               05  WRK-ATAI-9          PIC 9(15).
      *
           03  WRK-KOUMOKU         PIC X(20).
           03  WRK-LEN             PIC 9(04).
      *
      *
      *    禁忌チェック用テーブル
      *01  WRK-KNKCHK-AREA.
      *    03  WRK-KNK-TBL                  OCCURS   30.
      *        05  WRK-KNK-TOUMEI           PIC X(40). 
      *        05  WRK-KNK-TOUSRYCD         PIC X(09). 
      *        05  WRK-KNK-TBL2A.
      *            07  WRK-KNK-TBL2         OCCURS   30.
      *                09  WRK-KNK-KNKMEI   PIC X(40).
      *                09  WRK-KNK-SRYYM    PIC X(06).
      *                09  WRK-KNK-SRYYMW   PIC X(07).
      *                09  WRK-KNK-SRYCD    PIC X(09).
      *
      *01  WRK-SORT-AREA.
      *    03  WRK-SRT-TBL2A.
      *        05  WRK-SRT-TBL2            OCCURS   30.
      *            07  WRK-SRT-KNKMEI      PIC X(40).
      *            07  WRK-SRT-SRYYM       PIC X(06).
      *            07  WRK-SRT-SRYYMW      PIC X(07).
      *            07  WRK-SRT-SRYCD       PIC X(09).
      *    03  WRK-KEY-SRYYM               PIC X(06).
      *****03  WRK-KEY-SRYCD               PIC X(09).
      *
      *    コメントの入力値用
      *01  WRK-INPUT-SET-AREA.
      *    03  WRK-INPUT-TBL.
      *        05  WRK-INPUT-SET       PIC 9(03)   OCCURS  8.
      *    03  WRK-INPUT-TBL-R         REDEFINES   WRK-INPUT-TBL.
      *        05  WRK-INPUT-TBL2      OCCURS  4.
      *            07  WRK-INPUT-ICHI  PIC 9(03).
      *            07  WRK-INPUT-KETA  PIC 9(03).
      *    03  WRK-KETA                PIC 9(02).
      *    03  WRK-KETA2               PIC 9(02).
      *    03  WRK-ICHI                PIC 9(02).
      *    03  WRK-ICHI2               PIC 9(02).
      *    03  IDX-KNJ                 PIC 9(02).
      *    03  IDX-KNJ1                PIC 9(01).
      *****03  IDX-KNJ2                PIC 9(04).
      *
      *    ＴＢＬ行を２００とする
       01  WRK-LINE-MAX                PIC 9(04)   VALUE   200.
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "I4SCR-NYU.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *  診療行為テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    入院行為マスタ−
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
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
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
      **** COPY    "CPORCSC91.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    全角・半角チェック
           COPY    "CPORCXKANACONV.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "ORCA32SCRAREA.INC".
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
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-I44
           MOVE    SPA-WORK            TO  SPA-I4KYOUTU
           MOVE    SPA-I44GMN-AREA     TO  SPA-SCR-REC
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC.
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
           END-IF.
      *
           MOVE    SPA-SCR-REC         TO  SPA-I44GMN-AREA
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
           MOVE    SPA-I44             TO  SPA-FREE.
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA.
           INITIALIZE                  STS-AREA.
           INITIALIZE                  WRK-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 500-GMNHEN-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF.
      *
           MOVE   SPACE                TO  LNK-KYOUTU.
      *
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "I44"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC.
      *
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-GMN.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "I49"
                   IF      SPA-I49-DATA    NOT =   SPACE
                       MOVE    SPA-COM-GYOU(SPA-GMN-IDX)
                                               TO  WRK-IN-MAP-IDX
                       MOVE    SPA-GMN-IDX     TO  WRK-STR
                       MOVE    SPA-I49-DATA    TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
                       MOVE    "1"             TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
      *
      *                入力コード・名称チェック処理
                       MOVE    SPACE           TO  FLG-TOROKU
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    1               TO  FLG-GMN
                   ELSE
                       MOVE    "1"             TO  SPA-COM-CUR
                                                          (SPA-GMN-IDX)
                       MOVE    2               TO  FLG-GMN
                   END-IF
               WHEN    "I48"
                   IF      SPA-I48-SRYCD-G   NOT =   SPACE
                       PERFORM 3011-I48-SET-SEC
                       MOVE    1               TO  FLG-GMN
                   ELSE
                       MOVE    "1"             TO  SPA-COM-CUR
                                                          (SPA-GMN-IDX)
                       MOVE    2               TO  FLG-GMN
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "I4ID1"
      *            確認画面より
                   PERFORM 3003-JID1-SET-SEC
                   MOVE    SPACE           TO  SPA-IIDCD
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE.
           MOVE    SPACE           TO  SPA-IIDCD.
      *
           IF      FLG-GMN         NOT =   ZERO
               GO      TO      300-SCREEN-EXT
           END-IF
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-SET-SEC.
      *
           MOVE    1               TO  SPA-GMN-PAGE.
           MOVE    1               TO  SPA-GMN-CUR
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0101"
      *            戻るの確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       MOVE    SPACE           TO  SPA-IIDCD
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
      *                更新処理
                       PERFORM 41002-KOUSIN-SYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *
           END-EVALUATE.
      *
       3003-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                      SPA-I44
           INITIALIZE                      SPA-SCR-REC
      *
      *    システム管理検索
           PERFORM 3100-SYSKANRI-HEN-SEC
      *
           MOVE    ZERO                TO  IDX2
      *
           MOVE    "2"                 TO  FLG-TOROKU.
      *
      *    入院行為マスターを編集する
           PERFORM 3103-NYUIN-HEN-SEC
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU.
           PERFORM 510-TBLHEN-SEC.
           MOVE    SPACE           TO  FLG-TOROKU.
      *
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｉ４８）ＯＫ処理
      *****************************************************************
       3011-I48-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-MAP-IDX.
           MOVE    SPA-GMN-IDX         TO  WRK-STR
      *
           MOVE    SPA-I48-SRYCD       TO  SPA-GMN-INPUTCD
                                                   (SPA-GMN-IDX)
      *
      *    入力コード・名称チェック処理
           MOVE    SPACE           TO  FLG-TOROKU.
           PERFORM 410112-KOUI-SPACHK-SEC.
      *
       3011-I48-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   システム管理検索処理
      *****************************************************************
       3100-SYSKANRI-HEN-SEC               SECTION.
      *
      *    システム管理検索（医療機関情報）
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-I40-SRYYMD(1:6) TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD(1:6)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *        病院種別
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
      *        病床数
               MOVE    SYS-1001-BEDSU          TO  SPA-BEDSU
      *        病床数（一般）
               IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                      (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
                   MOVE    ZERO                TO  SPA-BEDSUIPN
               ELSE
                   MOVE    SYS-1001-BEDSUIPN   TO  SPA-BEDSUIPN
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3100-SYSKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院行為マスタより編集処理
      *****************************************************************
       3103-NYUIN-HEN-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX.
           MOVE    ZERO                TO  IDX2.
      *
           INITIALIZE                      NYUINACT-REC.
           MOVE    SPA-HOSPNUM         TO  NSRY-HOSPNUM.
           MOVE    "1"                 TO  NSRY-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NSRY-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NSRY-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NSRY-SRYYM.
           MOVE    SPA-ZAINUM          TO  NSRY-ZAINUM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF.
      *
           PERFORM     UNTIL     FLG-NYUINACT      =   1
      *
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS             >   5     )  OR
                                  (NSRY-SRYCD (IDS) =   SPACE)
                   PERFORM 31011-SPA-HEN2-SEC
               END-PERFORM
      *
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
           END-PERFORM.
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX2                TO  SPA-GMN-MAX
           .
      *
       3103-NYUIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       31011-SPA-HEN2-SEC             SECTION.
      *
      *    診療種別セット
           IF     (IDS                 =   1)  AND
                  (NSRY-RENNUM         =   1)
               IF      NSRY-SRYSYUKBN  NOT =   SPACE
                   ADD     1                   TO  IDX2
                   MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
                   MOVE    NSRY-SRYSYUKBN      TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
                   MOVE    NSRY-SRYSYUKBN      TO  SPA-COM-SRYSYUKBN
                                                            (IDX2)
                   MOVE    NSRY-SRYKBN         TO  SPA-COM-SRYKBN(IDX2)
               END-IF
           END-IF.
      *
      *    明細編集
           ADD     1                   TO  IDX2.
           MOVE    NSRY-SRYKBN         TO  SPA-COM-SRYKBN    (IDX2).
           MOVE    NSRY-SRYSYUKBN      TO  SPA-COM-SRYSYUKBN (IDX2).
           MOVE    NSRY-SRYCD   (IDS)  TO  SPA-COM-INPUTCD   (IDX2).
           MOVE    NSRY-SRYSURYO(IDS)  TO  SPA-COM-SURYO     (IDX2).
           MOVE    NSRY-SRYSURYO(IDS)  TO  SPA-COM-SURYO2    (IDX2).
           MOVE    NSRY-AUTOKBN (IDS)  TO  SPA-COM-AUTOKBN   (IDX2).
      *
           MOVE    SPA-COM-INPUTCD (IDX2)  TO  SPA-GMN-INPUTCD  (IDX2).
      *
      *    点数マスタ内容編集、チェック
           MOVE    SPA-COM-INPUTCD(IDX2)
                                       TO  WRK-SRYCD
           MOVE    IDX2                TO  IDX-T
           PERFORM 31011-TEN-HENKAN-SEC
      *
           .
       31011-SPA-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数編集変換処理
      *****************************************************************
       31011-TEN-HENKAN-SEC             SECTION.
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU           =   1
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
           MOVE    TNS-NAME      TO  SPA-GMN-MEISYO   (IDX-T).
      *
      *    基本チェック
           PERFORM 310111-KIHON-CHK-SEC
      *
      *    ＴＢＬ編集
           PERFORM 310112-TBL-HENSYU-SEC
      *
      *    併用算定チェック（算定回数がＯＫの時）
           IF     (SPA-COM-CHKFLG (IDX-T)  =   SPACE) AND
                  (SPA-ERRCD               =   SPACE) AND
                  (WRK-SRYCD(1:1)          =   "1"  )
               PERFORM 31011-TEN-HEIYOU-CHK-SEC
           END-IF.
      *
      *    基本チェック済フラグ
           IF      SPA-ERRCD               =   SPACE
               MOVE    "1"             TO  SPA-COM-CHKFLG (IDX-T)
           ELSE
               MOVE    SPACE           TO  SPA-COM-CHKFLG (IDX-T)
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
           END-IF
           .
      *
       31011-TEN-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       310111-KIHON-CHK-SEC        SECTION.
      *
      *    入院料のみ
           IF     (TNS-SRYCD(1:1)      =   "1" )  AND
                  (TNS-SRYKBN(1:1)     =   "9" )
               CONTINUE
           ELSE
               MOVE    "1016"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF
      *    病院診療所区分チェック
           IF     (TNS-HOSPSRYKBN      =   1)   AND
                  (SPA-HOSPSBT     NOT =   1)
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF.
           IF     (TNS-HOSPSRYKBN      =   2)   AND
                  (SPA-HOSPSBT     NOT =   2)
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF.
      *
      *    入外適用区分チェック
           IF     (TNS-NYUGAITEKKBN    =   1 )   AND
                  (SPA-NYUGAIKBN   NOT =  "1")
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF.
           IF     (TNS-NYUGAITEKKBN    =   2 )   AND
                  (SPA-NYUGAIKBN   NOT =  "2")
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF.
      *
      *    老人適用区分チェック
           IF     (TNS-ROUTEKKBN        =   1   )   AND
                  (SPA-ROUJIN       NOT =   ZERO)
               PERFORM 31011-TEN-SRYCDCHG-HEN-SEC
           END-IF.
           IF     (TNS-ROUTEKKBN        =   2)   AND
                  (SPA-ROUJIN       NOT =   1)
               PERFORM 31011-TEN-SRYCDCHG-HEN-SEC
           END-IF.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF.
      *
      *    施設基準適合分チェック（コメント・用法は対象外）
           IF     (WRK-SRYCD  (1:1)    =   "8"  )  OR
                  (WRK-SRYCD  (1:3)    =   "001")  OR
                  (TNS-SSTKIJUNCD(01)  =   ZERO )
               CONTINUE
           ELSE
               PERFORM 3101119-SSTKIJUNCD-CHK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF
      *
      *    対象年齢チェック
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    TNS-KGNAGE          TO  WRK-KGNAGE(2:2)
           MOVE    TNS-JGNAGE          TO  WRK-JGNAGE(2:2)
      *    下限年齢チェック
           IF      TNS-KGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-KGNAGE          =   "AA"
      *            新生児判定
                   IF     (SPA-NAI-NENREI-YY   =   ZERO )  AND
                          (SPA-NAI-NENREI-MM   =   ZERO )  AND
                          (SPA-NAI-NENREI-DD   <   28   )
                       MOVE    "0032"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF      SPA-NAI-NENREI-YY   >=  WRK-KGNAGE
                      CONTINUE
                   ELSE
                       MOVE    "0032"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      TNS-JGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-JGNAGE          =   "AA"
      *            新生児判定
                   IF     (SPA-NAI-NENREI-YY     =   ZERO )  AND
                          (SPA-NAI-NENREI-MM     =   ZERO )  AND
                          (SPA-NAI-NENREI-DD     <   28   )
                       CONTINUE
                   ELSE
                       MOVE    "0032"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF      SPA-NAI-NENREI-YY       <   WRK-JGNAGE
                       CONTINUE
                   ELSE
                       MOVE    "0032"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF.
      *
      *    病床数チェック
           IF      TNS-BYOSYOKBN   NOT =   ZERO
               PERFORM 310113-BYOSYOKBN-CHK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF.
      *
      *    労災・自賠責チェック
      *    労災のコード
           IF     (WRK-SRYCD (1:1)     =   "1" )  AND
                  (WRK-SRYCD (2:2)     =   "01")  AND
                  (SPA-HKNKBN      NOT =   1   )
               MOVE    "8000"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      310111-KIHON-CHK-EXT
           END-IF
      *
      *    注加算コード、注加算連番チェック
           IF     (TNS-CHUKSNCD        =   ZERO  OR  SPACE)  OR
                  (TNS-CHUKSNTUBAN     =   ZERO  OR  SPACE)
               CONTINUE
           ELSE
      *        追加挿入の時のチェックとする
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   WRK-LINE-MAX ) OR
                                 ((SPA-GMN-INPUTCD (IDX)   =   SPACE)
                              AND (SPA-COM-INPUTCD (IDX)   =   SPACE))
                   IF      IDX         NOT =   IDX-T
                       IF      (SPA-COM-CHUKSNCD   (IDX)
                                                   =  TNS-CHUKSNCD) AND
                               (SPA-COM-CHUKSNTUBAN(IDX)
                                                   =   TNS-CHUKSNTUBAN)
                           MOVE    "0042"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR
                                                             (IDX-T)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
      *
       310111-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準チェック
      *****************************************************************
       3101119-SSTKIJUNCD-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SST
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )   OR
                              (FLG-SST     =   1   )
               IF      TNS-SSTKIJUNCD (IDX)    NOT =   ZERO
                   MOVE    1               TO  FLG-SST
               END-IF
           END-PERFORM
           IF      FLG-SST             =   ZERO
               GO      TO      3101119-SSTKIJUNCD-CHK-EXT
           END-IF
      *
           INITIALIZE                  WRK-TBL-SSTKIJUN-AREA
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC.
           MOVE    "1006"              TO  SYS-1006-KANRICD.
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-SYSKANRI   =   1    )
     ********      OR      (IDX            >=  3500 )
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               EVALUATE     SYS-1006-KBNCD
                   WHEN     "1"
                        MOVE    0          TO  IDX
                   WHEN     "2"
                        MOVE    500        TO  IDX
                   WHEN     "7"
                        MOVE    3000       TO  IDX
      *H28.4
                   WHEN    "8"
                       MOVE    3500        TO  IDX
      *H26.4
                   WHEN    "17"
                       MOVE    8000        TO  IDX
               END-EVALUATE
      *H24.4
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >       500 )
                           OR (IDX         >=      9999)
                   ADD     1               TO  IDX
                   MOVE    SYS-1006-SSTKIJUN  (IDY)
                                           TO  WRK-TBL-SSTKIJUN (IDX)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H30.5
      *Ｈ２６年４月から
           IF      SPA-SRYYMD          >=  "20140401"
               PERFORM 20021-SSTKIJUN-CHK02-SEC
           ELSE
      *
      *    １〜１０までのいずれかの設定有
           MOVE    ZERO                TO  FLG-SST
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )   OR
                              (FLG-SST     =   1   )
               IF      TNS-SSTKIJUNCD (IDX)    =   ZERO
                   CONTINUE
               ELSE
                   MOVE    TNS-SSTKIJUNCD (IDX)    TO  IDY
                   IF      WRK-TBL-SSTKIJUN (IDY)  =   1
                       MOVE    1               TO  FLG-SST
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-SST             =   ZERO
               MOVE    "0024"          TO  SPA-ERRCD
           END-IF
      *
           END-IF
           .
      *
       3101119-SSTKIJUNCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準適合分チェック２処理
      *****************************************************************
       20021-SSTKIJUN-CHK02-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SST1
           MOVE    ZERO                TO  FLG-SST2
           MOVE    ZERO                TO  FLG-SST3
           MOVE    ZERO                TO  FLG-SSA1
           MOVE    ZERO                TO  FLG-SSA2
           MOVE    ZERO                TO  FLG-SSA3
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )
               IF      TNS-SSTKIJUNCD (IDX)    =   ZERO
                   CONTINUE
               ELSE
                   MOVE    TNS-SSTKIJUNCD (IDX)    TO  IDY
                   EVALUATE    IDX
                       WHEN    1   THRU    6
                           MOVE    1               TO  FLG-SSA1
      *                    施設基準あり
                           IF      WRK-TBL-SSTKIJUN (IDY)  =   1
                               MOVE    1               TO  FLG-SST1
                           END-IF
                       WHEN    7   THRU    9
                           MOVE    1               TO  FLG-SSA2
      *                    施設基準あり
                           IF      WRK-TBL-SSTKIJUN (IDY)  =   1
                               MOVE    1               TO  FLG-SST2
                           END-IF
                       WHEN    10
                           MOVE    1               TO  FLG-SSA3
      *                    施設基準あり
                           IF      WRK-TBL-SSTKIJUN (IDY)  =   1
                               MOVE    1               TO  FLG-SST3
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
           IF    ((FLG-SSA1            =   1  )  AND
                  (FLG-SST1            =   ZERO))
             OR  ((FLG-SSA2            =   1  )  AND
                  (FLG-SST2            =   ZERO))
             OR  ((FLG-SSA3            =   1  )  AND
                  (FLG-SST3            =   ZERO))
               MOVE    "0024"          TO  SPA-ERRCD
           END-IF
      *
           .
       20021-SSTKIJUN-CHK02-EXT.
           EXIT.
      *
      *****************************************************************
      *    病床数チェック
      *****************************************************************
       310113-BYOSYOKBN-CHK-SEC      SECTION.
      *
      *    病床数チェック（ＳＰＡの病床数）
           EVALUATE    TNS-BYOSYOKBN
               WHEN    1
                   IF      SPA-BEDSU           >=  1  AND  <=  99
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  SPA-ERRCD
                   END-IF
               WHEN    2
                   IF      SPA-BEDSU           >=  100 AND  <=  199
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  SPA-ERRCD
                   END-IF
               WHEN    3
                   IF      SPA-BEDSU           <   200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  SPA-ERRCD
                   END-IF
               WHEN    4
                   IF      SPA-BEDSU           >=  200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  SPA-ERRCD
                   END-IF
      *        一般病床数判定
               WHEN    5
                   IF      SPA-BEDSUIPN        >=  0  AND  <=  199
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  SPA-ERRCD
                   END-IF
               WHEN    6
                   IF      SPA-BEDSUIPN        >=  200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
      *
           .
       310113-BYOSYOKBN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＳＰＡＴＢＬ編集処理
      *****************************************************************
       310112-TBL-HENSYU-SEC        SECTION.
      *
      *    ＳＰＡ編集
      *    基礎点数
           MOVE    TNS-TEN             TO  SPA-COM-TEN      (IDX-T).
           MOVE    TNS-TEN             TO  SPA-COM-KISOTEN  (IDX-T).
      *
           IF      SPA-HKNKBN          =   1
      *        労災・自賠責 編集
               PERFORM 31011-TEN-RSIJIB-HEN-SEC
           END-IF.
      *
           MOVE    TNS-NAME            TO  SPA-GMN-MEISYO   (IDX-T)
      *
           MOVE    SPACE               TO  SPA-GMN-TANINAME (IDX-T)
      *
      *    診療種別区分
           IF      SPA-COM-INPUTCD (IDX-T)(1:1)    =   "1"
               MOVE    TNS-SRYSYUKBN   TO  SPA-COM-TNSSRYSYUKBN(IDX-T)
               MOVE    TNS-SRYSYUKBN   TO  SPA-COM-SRYSYUKBN   (IDX-T)
           END-IF
      *    データ区分
           MOVE    TNS-DATAKBN         TO  SPA-COM-DATAKBN    (IDX-T).
      *    診療区分
           MOVE    TNS-SRYKBN          TO  SPA-COM-TNSSRYKBN  (IDX-T).
      *    注加算コード
           MOVE    TNS-CHUKSNCD        TO  SPA-COM-CHUKSNCD   (IDX-T).
      *    注加算通番
           MOVE    TNS-CHUKSNTUBAN     TO  SPA-COM-CHUKSNTUBAN
                                                              (IDX-T).
      *    通則加算対象外区分
           MOVE    TNS-TUSOKUGAIKBN    TO  SPA-COM-TUSOKUGAIKBN
                                                              (IDX-T).
      *    通則年齢区分
           MOVE    TNS-TSUSOKUAGE      TO  SPA-COM-TSUSOKUAGE (IDX-T).
      *    算定履歴区分
           MOVE    TNS-SANTEIRRKKBN    TO  SPA-COM-SANTEIRRKKBN
                                                              (IDX-T).
      *    指導管理料
           MOVE    TNS-SDOKNRYO        TO  SPA-COM-SDOKNRYO   (IDX-T).
      *    点数識別
           MOVE    TNS-TENSIKIBETU     TO  SPA-COM-TENSIKIBETU
                                                              (IDX-T).
      *    上限点数（器材の上限）
           MOVE    TNS-JGNTEN          TO  SPA-COM-JGNTEN     (IDX-T).
      *    保険適用区分
           MOVE    TNS-HKNTEKKBN       TO  SPA-COM-HKNTEKKBN  (IDX-T).
      *    点数欄集計先識別
           MOVE    TNS-GAITENTTLKBN    TO  SPA-COM-GAITENTTLKBN
                                                              (IDX-T).
      *    点数欄集計先識別
           MOVE    TNS-NYUTENTTLKBN    TO  SPA-COM-NYUTENTTLKBN
                                                              (IDX-T).
      *    きざみ値計算識別判定
           MOVE    TNS-KZMCOMPSIKIBETU TO  SPA-COM-KZMCOMPSIKIBETU
                                                              (IDX-T).
      *    きざみ値計上限
           MOVE    TNS-KZMJGN          TO  SPA-COM-KZMJGN     (IDX-T).
      *    検査等実施判定区分
           MOVE    TNS-HOUKATUTEIGENKBN    TO  SPA-COM-HOUKATUTEIGENKBN
                                                              (IDX-T).
      *    年齢加算
           MOVE    TNS-AGEKSNTBL(1)    TO  SPA-COM-AGEKSNTBL(IDX-T 1).
           MOVE    TNS-AGEKSNTBL(2)    TO  SPA-COM-AGEKSNTBL(IDX-T 2).
           MOVE    TNS-AGEKSNTBL(3)    TO  SPA-COM-AGEKSNTBL(IDX-T 3).
           MOVE    TNS-AGEKSNTBL(4)    TO  SPA-COM-AGEKSNTBL(IDX-T 4).
      *
      *    レセプト用実日数（実日数＝２の日数・回数をセット）
           IF      TNS-JITUDAY         =   2
               MOVE    TNS-DAYCNT      TO  SPA-COM-DAYCNT (IDX-T)
           END-IF
      *
      *    基準不適合逓減区分
           MOVE    TNS-KIJUNTEIGENKBN  TO  SPA-COM-KIJUNTEIGENKBN
                                                               (IDX-T).
      *    基準不適合逓減対象施設基準
           MOVE    TNS-KIJUNTEIGENCD   TO  SPA-COM-KIJUNTEIGENCD
                                                               (IDX-T).
      *
      *    点数マスタ付加情報より編集
           MOVE    SPACE               TO  TENSUPLUS-REC.
           INITIALIZE                      TENSUPLUS-REC.
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD.
           MOVE    SPA-I40-SRYYMD      TO  TNSP-YUKOSTYMD.
           MOVE    SPA-I40-SRYYMD      TO  TNSP-YUKOEDYMD.
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC.
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-TENSUPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF.
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入力チェック区分
           MOVE    TNSP-INPCHKKBN      TO  SPA-COM-INPCHKKBN  (IDX-T).
      *
       310112-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード変換処理
      *****************************************************************
       31011-TEN-SRYCDCHG-HEN-SEC        SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC.
      *    コード変換読込
           EVALUATE    TNS-ROUTEKKBN
      *        一般
               WHEN    1
                   MOVE    TNS-SRYCD           TO  CHG-IPNSRYCD
                   MOVE    "key2"              TO  WRK-PATH
               WHEN    2
                   MOVE    TNS-SRYCD           TO  CHG-RJNSRYCD
                   MOVE    "key3"              TO  WRK-PATH
           END-EVALUATE.
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC.
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF.
      *
           INITIALIZE                  WRK-SRYCD-CHG-AREA.
           MOVE    ZERO                TO  IDX.
           PERFORM UNTIL     FLG-SRYCDCHG      =   1
               ADD     1               TO  IDX
               IF      TNS-ROUTEKKBN   =   1
                   MOVE    CHG-RJNSRYCD    TO  WRK-SRYCDCHG (IDX)
               ELSE
                   MOVE    CHG-IPNSRYCD    TO  WRK-SRYCDCHG (IDX)
               END-IF
      *
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           END-PERFORM.
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           IF      IDX                 =   ZERO
               MOVE    "0015"          TO  SPA-ERRCD
               GO      TO      31011-TEN-SRYCDCHG-HEN-EXT
           END-IF.
      *
      *    点数マスタ編集
           MOVE    WRK-SRYCDCHG (01)   TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU           =   1
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-SRYCDCHG-HEN-EXT
           END-IF.
      *
           MOVE    WRK-SRYCDCHG (01)   TO  SPA-COM-INPUTCD (IDX-T).
      *    画面再表示
           MOVE    WRK-SRYCDCHG (01)   TO  SPA-GMN-INPUTCD (IDX-T)
                                                           (1:9)
      *
           .
       31011-TEN-SRYCDCHG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    併用算定チェック処理
      *****************************************************************
       31011-TEN-HEIYOU-CHK-SEC        SECTION.
      *
      *    チェックテーブル検索
           INITIALIZE                  CHK-REC.
           MOVE    "5"                 TO  CHK-CHKKBN.
           MOVE    WRK-SRYCD           TO  CHK-SRYCD.
           MOVE    "2"                 TO  CHK-CDKBN.
           MOVE    ZERO                TO  CHK-RENNUM.
           MOVE    SPA-SRYYMD          TO  CHK-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  CHK-YUKOEDYMD
      *
           MOVE    ZERO                TO  FLG-CHKMST.
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC.
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF.
           PERFORM UNTIL     (FLG-CHKMST       =   1     )  OR
                             (SPA-ERRCD     NOT =   SPACE)
      ******** PERFORM VARYING     IDX-C       FROM    1   BY  1
      *                UNTIL      (IDX-C            >   10   )  OR
      *                           (CHK-CD (IDX-C)   =   SPACE)  OR
      *************               (SPA-ERRCD    NOT =   SPACE)
      *            入力値とのチェック
                   PERFORM 31011-TEN-CHK-INPUT-SEC
      ******** END-PERFORM
      *
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           END-PERFORM
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31011-TEN-HEIYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力値とのチェック処理
      *****************************************************************
       31011-TEN-CHK-INPUT-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   WRK-MAX)  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               IF      IDX                 =   IDX-T
                   CONTINUE
               ELSE
                   IF      SPA-COM-INPUTCD(IDX)  =   CHK-CD
                       MOVE    "0033"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM.
      *
       31011-TEN-CHK-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *     労災・自賠責 編集処理
      *****************************************************************
       31011-TEN-RSIJIB-HEN-SEC     SECTION.
      *
           IF      SPA-COM-INPUTCD (IDX-T)(1:1)    =   "1"
      *        労災のコード
               IF      SPA-COM-INPUTCD (IDX-T)(2:2)    =   "01"
                   MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
               END-IF
      *        金額を点数へ置換える
               IF      TNS-TENSIKIBETU     =   1
                   COMPUTE SPA-COM-KISOTEN  (IDX-T)    =   TNS-TEN
                                                       /   10
               END-IF
           END-IF.
      *
       31011-TEN-RSIJIB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 4100-TOROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC.
      *
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR.
      *    入力項目の決定
           PERFORM 2001-INPUT-SET-SEC.
      *
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC.
      *
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2.
           IF      MCP-WIDGET(1:7)     =   "INPUTCD"
               MOVE    MCP-WIDGET(8:2)     TO  WRK-IDX2
           END-IF.
           IF      MCP-WIDGET(1:6)     =   "MEISYO"
               MOVE    MCP-WIDGET(7:2)     TO  WRK-IDX2
           END-IF.
      *
           MOVE    ZERO                TO  SPA-GMN-IDX.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE)  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM.
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX        =   ZERO
               COMPUTE SPA-GMN-IDX    =   (SPA-GMN-PAGE   -   1)
                                       *   20
                                       +   WRK-IDX2
           END-IF.
      *
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2002-GMNSPA-SET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE") AND
                  (MCP-WIDGET(1:7)     =   "INPUTCD" )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF.
      *
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               MOVE    SPACE               TO  SPA-COM-CUR (IDX)
               MOVE    SPACE               TO  SPA-COM-IN  (IDX)
               MOVE    SPACE               TO  SPA-COM-IN2 (IDX)
           END-PERFORM.
      *
           MOVE    ZERO                TO  WRK-STR.
           MOVE    ZERO                TO  FLG-KUHAKU.
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL       WRK-IDX2    >   20
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF      (I44-INPUTCD (WRK-IDX2)  NOT =
                                   SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                       (I44-MEISYO  (WRK-IDX2)   NOT =
                                   SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                          (SPA-GMN-IDX)
               END-IF
      *
               MOVE    I44-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
               MOVE    I44-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                          (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   IF     (I44-INPUTCD (WRK-IDX2)  =   SPACE)  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE)  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE)  AND
                          (SPA-GMN-IDX            >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      I44-INPUTCD (WRK-IDX2)  NOT =   SPACE
                   IF      WRK-STR             =   ZERO
                       MOVE    SPA-GMN-IDX        TO  WRK-STR
                   END-IF
               END-IF
           END-PERFORM.
      *
       2002-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-GYOU-SET-SEC               SECTION.
      *    行決定
           MOVE    ZERO                TO  SPA-GMN-IDX.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE)  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM.
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX        =   ZERO
               COMPUTE SPA-GMN-IDX    =   (SPA-GMN-PAGE  -   1)
                                       *   20
                                       +   WRK-IDX2
           END-IF.
      *
       41011-GYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC           SECTION.
      *
      *    画面ＳＰＡチェック処理
           MOVE    SPACE           TO  FLG-TOROKU.
           PERFORM 410112-KOUI-SPACHK-SEC.
      *
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR.
           IF      WRK-STR             =   ZERO
               MOVE    1                   TO  WRK-STR
           END-IF
      *
      *    変換・削除処理
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  WRK-IDX2.
           MOVE    ZERO                TO  FLG-INS.
           PERFORM VARYING SPA-GMN-IDX     FROM    1   BY  1
                   UNTIL   SPA-GMN-IDX     >   200
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                      SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1")
                   PERFORM 41011-KOUI-HENDEL-SEC
               END-IF
           END-PERFORM.
           IF      FLG-INS             =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
           END-IF
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  FLG-INS
           PERFORM UNTIL      (SPA-GMN-IDX    >   200  )  OR
                              (SPA-ERRCD   NOT =   SPACE)  OR
                              (FLG-END     NOT =   ZERO )
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                      SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1")
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
      *
               IF     (SPA-ERRCD       =   SPACE)  AND
                      (FLG-END         =   ZERO )
                   ADD     1                   TO  SPA-GMN-IDX
               END-IF
           END-PERFORM.
      *
           IF     (SPA-ERRCD       NOT =   SPACE)  OR
                  (FLG-END             =   1    )
               IF      FLG-END         =   1
      *            ＳＰＡの内容を画面へ編集する。カーソル設定なし
                   PERFORM 500-GMNHEN-SEC
               END-IF
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF.
      *    行挿入、警告あり
           IF      FLG-INS             =   1
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF.
      *
      *    点数計算・剤別チェック処理
           PERFORM 510-TBLHEN-SEC
           IF     (SPA-ERRCD       =   SPACE)  AND
                  (FLG-END         =   ZERO )
               CONTINUE
           ELSE
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF.
      *
      *    診療コード入力
           PERFORM 410119-SAIINPUT-SEC
           .
      *
       410112-KOUI-SPACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       41011-KOUI-HENDEL-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA.
      *
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX) TO  WRK-HENKAN
      *    １桁目が空白は削除とする
           IF      WRK-HENKAN(1:1)     =   SPACE
               MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                      (SPA-GMN-IDX)
               MOVE    1               TO  FLG-INS
               MOVE    1               TO  FLG-MEI
               GO      TO     41011-KOUI-HENDEL-EXT
           END-IF
      *
      *    １桁目チェック
           PERFORM 41011-MAE-HANKAKU-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-HENKAN      TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    ZERO            TO  SPA-GMN-INCNT  (SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-GMNFLG (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX)
           END-IF
      *
      *    削除時
           IF      WRK-HENKAN(1:1)   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    1               TO  FLG-INS
               MOVE    1               TO  FLG-MEI
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF.
      *
      *    剤削除時
      *    '-'入力がある時、剤削除ができなので、エラーとする
           IF      WRK-HENKAN(1:1)   =   "-"
      *********MOVE    WRK-HENKAN      TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    "1030"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX)
           END-IF.
      *
       41011-KOUI-HENDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-HEN2-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA.
      *
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)  TO  WRK-HENKAN
      *    １桁目チェック
           PERFORM 41011-MAE-HANKAKU-SEC
           IF      WRK-INPUTCD         =   SPACE
      *        入力コード変換
               PERFORM 41011-KOUI-INPUTCD-SYORI-SEC
           END-IF
      *
           MOVE    WRK-HENKAN         TO  SPA-GMN-INPUTCD(SPA-GMN-IDX).
      *    '_'を削除した時、クリアする
           IF      WRK-JODOU      >   ZERO
               MOVE    "3"             TO  SPA-COM-IN  (SPA-GMN-IDX)
           END-IF.
      *
      *    行挿入
           IF      WRK-HENKAN(1:1)   =   "+"
               MOVE     SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41014-GYOINSN-SEC
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               ADD     1               TO  SPA-GMN-IDX
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HEN2-EXT
           END-IF.
      *    一覧へ
           IF      WRK-HENKAN(1:2)   =   "//"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               INITIALIZE                      SPA-I48-AREA
               INITIALIZE                      SPA-I48GMN-AREA
               MOVE    WRK-HENKAN          TO  SPA-I48-INPUTCD
               PERFORM 410-I48-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF.
      *    Ｐ入力へ
           IF      WRK-HENKAN(1:1)   =   "/"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 430-I49-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF.
      *
           IF      FLG-NYURYOKU    =   1
               MOVE    SPA-COM-IN     (SPA-GMN-IDX)   TO  WRK-FLG-IN
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)  TO  WRK-MEISYO-G
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-FLG-IN      TO  SPA-COM-IN  (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
           ELSE
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
           END-IF.
      *
           MOVE    WRK-HENKAN       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX).
      *
      *    診療行為一覧へ
           IF      WRK-KENSAKU         =   "1"
               INITIALIZE                 SPA-I48-AREA
               INITIALIZE                 SPA-I48GMN-AREA
               MOVE    WRK-CD         TO  SPA-I48-INPUTCD
               MOVE    "1"            TO  SPA-I48-TYPE
               MOVE    WRK-ICD-CDSYU  TO  SPA-I48-CDSYUBETU
               MOVE    "1"            TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
      *        診療行為一覧へ
               PERFORM 410-I48-SYORI-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *
      *    診療種別入力
           IF      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   ".") AND
                   (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)  NUMERIC)
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-HENKAN(1:4)     TO
                                           SPA-GMN-INPUTCD (SPA-GMN-IDX)
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                                   TO  WRK-SRYSYUKBN
      *        診療種別区分名称チェック
               PERFORM 510-SRYKBN-MEI-SEC
           ELSE
      *        文字変換・編集処理
               PERFORM 410112-INPUTCD-HENKAN-SEC
      *
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF.
      *
       41011-KOUI-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力処理
      *****************************************************************
       410119-SAIINPUT-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-IDX4.
           MOVE    ZERO                TO  WRK-IN-IDX2.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      SPA-COM-IN  (IDX)   =   "1"
                   MOVE    IDX                 TO  WRK-IN-IDX4
               END-IF
               IF      SPA-COM-IN  (IDX)   =   "3"
                   MOVE    IDX                 TO  WRK-IN-IDX4
                   MOVE    IDX                 TO  WRK-IN-IDX2
               END-IF
           END-PERFORM.
      *
           IF      WRK-IN-IDX4         =   ZERO
               MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX4         TO  SPA-GMN-IDX
           END-IF.
      *
           IF     WRK-MKBN     =   "1"
               MOVE    WRK-STR             TO  SPA-GMN-IDX
           END-IF.
           MOVE    SPACE               TO  WRK-MKBN.
      *
           IF     FLG-MEI   NOT =   1
               IF     (SPA-COM-SURFLG (SPA-GMN-IDX)  =   SPACE) AND
                      (SPA-COM-IN2    (SPA-GMN-IDX)  =   "2"  ) AND
                      (SPA-COM-KAIFLG (SPA-GMN-IDX)  =   SPACE) AND
                      (WRK-IN-IDX2                    =   ZERO )
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                                TO  WRK-INPUTCD
                   MOVE    "1"                  TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                   IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =  "001"
                       MOVE    SPACE            TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                   ELSE
                       STRING  WRK-INPUTCD      DELIMITED  BY  SPACE
                               "_"              DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                       END-STRING
                   END-IF
                   MOVE    SPACE                TO  SPA-ERRCD
               END-IF
      *        名称入力へのカーソル移動
               IF      WRK-IN-IDX4         >   ZERO
                   IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "81"  OR  "83"  OR  "89"
                       MOVE    "3"        TO  SPA-COM-IN2 (SPA-GMN-IDX)
                   END-IF
                   IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "84"
                       MOVE    "3"        TO  SPA-COM-IN2 (SPA-GMN-IDX)
                       MOVE    "1"        TO  SPA-COM-IN  (SPA-GMN-IDX)
                   END-IF
               END-IF
           END-IF.
      *
      *    空白行入力時、前の行へ
           IF      FLG-KUHAKU          =   1
      *        空白行入力時、最終行へ
               MOVE    "1"                 TO  SPA-COM-CUR
                                                     (SPA-GMN-IDX)
           END-IF.
      *
       410119-SAIINPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字列変換・編集処理
      *****************************************************************
       410112-INPUTCD-HENKAN-SEC      SECTION.
      *
      *    セット入力処理へ
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
                                                       OR  "P"
               MOVE    "0008"          TO  SPA-ERRCD
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF.
      *
      *    入力内容チェック、編集
           IF       FLG-NYURYOKU   =   1
               MOVE    WRK-CD          TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           END-IF.
           PERFORM 4101113-INPUTCD-CHK-SEC
           .
      *
       410112-INPUTCD-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    入力内容チェック、編集処理
      *****************************************************************
       4101113-INPUTCD-CHK-SEC       SECTION.
      *
      *    診療行為コードをもとに点数マスター検索
           MOVE    ZERO                TO  FLG-TENSU.
           MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                       TO  TNS-SRYCD.
      *
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU           =   1
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF.
      *
           MOVE    SPACE               TO  SPA-COM-IN3 (SPA-GMN-IDX).
      *
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC.
      *
           MOVE    SPA-GMN-IDX         TO  IDX-T
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  WRK-SRYCD
           PERFORM 31011-TEN-HENKAN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-COM-CHKFLG (SPA-GMN-IDX)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF.
      *
      *    その他値の入力の有無
           IF     (WRK-HZN-ATAI1   =   SPA-COM-ATAI1 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI2   =   SPA-COM-ATAI2 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI3 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI4 (SPA-GMN-IDX))
               MOVE    ZERO                TO  FLG-IN-ATAI
           ELSE
               MOVE    1                   TO  FLG-IN-ATAI
           END-IF.
      *
           IF      SPA-COM-HKNTEKKBN(SPA-GMN-IDX)  =   2
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
                   MOVE    "1"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-COM-KEIFLG (SPA-GMN-IDX)
           END-IF.
      *
           .
      *
       4101113-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角、半角変更編集処理
      *****************************************************************
       41011-MAE-HANKAKU-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-JODOU
      *    半角カナの全角変換
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    22                  TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    WRK-HENKAN          TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           MOVE    KANACONV-OUTDATA    TO  WRK-HENKAN
      *
      *    '_'をなくす
           INSPECT WRK-HENKAN
                   TALLYING    WRK-JODOU  FOR  ALL "_"
                   REPLACING   ALL "_"  BY  " ".
      *
           MOVE    SPACE               TO  WRK-INPUTCD2
           MOVE    SPACE               TO  WRK-INPUTCD
      *    漢字変換
           MOVE    1                   TO  IDH1
           MOVE    ZERO                TO  IDH2
           PERFORM
                   UNTIL       IDH1    >   22
      *      全角判定
             IF      IDH1             =    22
                 MOVE    ZERO              TO  FLG-ZENKAKU
             ELSE
                 MOVE    WRK-HENKAN(IDH1:2)    TO  WRK-KOUMOKU
                 MOVE    2                     TO  WRK-LEN
                 PERFORM 1009-KANACONV-SEC
             END-IF
             IF      FLG-ZENKAKU         =   1
      *******IF      WRK-HENKAN(IDH1:1)
      *******                             >=  X"A1"
               EVALUATE    WRK-HENKAN(IDH1:2)
                   WHEN    "−"
                       ADD     1           TO  IDH2
                       MOVE    "-"         TO  WRK-INPUTCD2(IDH2:1)
                   WHEN    "＋"
                       ADD     1           TO  IDH2
                       MOVE    "+"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "！"
                       ADD     1           TO  IDH2
                       MOVE    "!"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "／"
                       ADD     1           TO  IDH2
                       MOVE    "/"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "　"
                       ADD     1           TO  IDH2
                       MOVE    SPACE       TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    OTHER
                       ADD     1           TO  IDH2
                       MOVE    WRK-HENKAN(IDH1:2)
                                           TO  WRK-INPUTCD2 (IDH2:2)
                       ADD     1           TO  IDH2
               END-EVALUATE
               ADD     2                   TO  IDH1
             ELSE
               ADD     1                   TO  IDH2
               MOVE    WRK-HENKAN(IDH1:1)
                                           TO  WRK-INPUTCD2 (IDH2:1)
               ADD     1                   TO  IDH1
             END-IF
           END-PERFORM
      *
      *    ’＋’、’−’のチェック
           MOVE    SPACE               TO  WRK-CHK-FLG
           MOVE    SPACE               TO  WRK-INPUTCD3
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX    >   22
               IF      WRK-INPUTCD2(IDX:1)     =   "+"  OR  "-"
                   COMPUTE IDX3                =   IDX  +  1
      *            入院の回数指定はそのまま
                   IF     (WRK-INPUTCD2(1:1)   =   "*"  )  AND
                          (WRK-INPUTCD2(IDX3:) NOT =   SPACE)
                       ADD     1                   TO  IDX2
                       MOVE    WRK-INPUTCD2(IDX:1) TO  WRK-INPUTCD3
                                                             (IDX2:1)
                   ELSE
                       IF     (WRK-CHK-FLG         =   SPACE)  AND
                              (IDX2                =   ZERO   OR
                               IDX                 =   22     OR
                               WRK-INPUTCD2(IDX3:) =   SPACE )
                           MOVE    WRK-INPUTCD2(IDX:1) TO  WRK-CHK-FLG
                       END-IF
                   END-IF
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    WRK-INPUTCD2(IDX:1) TO  WRK-INPUTCD3
                                                             (IDX2:1)
      *            １，２桁目以外の '/'は、空白とする
                   IF     (WRK-INPUTCD2(IDX:1)     =   "/" )  AND
                          (IDX                 NOT =   1  AND   2)
                       IF      WRK-INPUTCD2(1:1)   =   "*"
                           CONTINUE
                       ELSE
                           MOVE    SPACE       TO  WRK-INPUTCD2(IDX:1)
                           MOVE    SPACE       TO  WRK-INPUTCD3(IDX2:1)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-INPUTCD2        TO  WRK-HENKAN
      *
      *    １桁目へ−，＋編集
           IF      WRK-CHK-FLG     NOT =   SPACE
               MOVE    WRK-CHK-FLG         TO  WRK-INPUTCD(1:1)
               MOVE    WRK-INPUTCD3        TO  WRK-INPUTCD(2:)
               MOVE    WRK-INPUTCD         TO  WRK-HENKAN
           END-IF
      *
      *
           IF      WRK-HENKAN(1:2)     =   "!!"
               MOVE    WRK-HENKAN      TO  WRK-INPUTCD
           END-IF
           IF      WRK-HENKAN(1:1)     =   "!"  OR
                                           " "  OR
                                           "/"  OR
                                           "-"  OR
                                           "+"
               MOVE    WRK-HENKAN      TO  WRK-INPUTCD
           END-IF
      *
           .
       41011-MAE-HANKAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角判定処理
      *****************************************************************
       1009-KANACONV-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-ZENKAKU
           MOVE    ZERO                TO  FLG-HANKAKU
           MOVE    ZERO                TO  FLG-ERR
           IF      WRK-KOUMOKU     NOT =   SPACE
      *        半角カナの全角変換
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    WRK-LEN             TO  KANACONV-LEN
               MOVE    1000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    15                  TO  KANACONV-CHAR-TYPE
               MOVE    WRK-KOUMOKU         TO  KANACONV-INDATA
               CALL    "kanaconv"          USING ORCXKANACONVAREA
               IF      KANACONV-RETURN     =   ZERO
      *            全角
                   MOVE    1                   TO  FLG-ZENKAKU
               ELSE
      *            半角のみの判定
                   INITIALIZE                  ORCXKANACONVAREA
                   MOVE    WRK-LEN             TO  KANACONV-LEN
                   MOVE    1000                TO  KANACONV-STLEN
                   MOVE    0                   TO  KANACONV-CONV-FLAG
                   MOVE    112                 TO  KANACONV-CHAR-TYPE
                   MOVE    WRK-KOUMOKU         TO  KANACONV-INDATA
                   CALL    "kanaconv"          USING
                                               ORCXKANACONVAREA
                   IF      KANACONV-RETURN     =   ZERO
                       MOVE    1                   TO  FLG-HANKAKU
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
           .
       1009-KANACONV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    全角、半角変更編集処理
      *****************************************************************
       41011-KOUI-INPUTCD-SYORI-SEC     SECTION.
      *
           MOVE    WRK-HENKAN          TO  WRK-INPUTCD
      *
      *    入力コードの判定
      *    １桁が’．’の時、種別入力とする
           IF      WRK-HENKAN(1:2)   =   "．" 
               MOVE    WRK-HENKAN          TO  WRK-HENKAN-MAE
               MOVE    1                   TO  WRK-STR
               MOVE    22                  TO  WRK-END
               PERFORM 4101-KOUI-MOJI-HENKAN-SEC
               MOVE    WRK-HENKAN-ATO      TO  WRK-HENKAN
           END-IF.
      *    診療種別
           IF      WRK-HENKAN(1:1)   =   "."
               GO      TO      41011-KOUI-INPUTCD-SYORI-EXT
           END-IF.
      *
      *    入力コード文字列調査処理
           PERFORM 4101-KOUI-INPUTCD-CHOSA-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41011-KOUI-INPUTCD-SYORI-EXT
           END-IF.
      *
      *    入力コード変更時、点数マスタの決定
           IF      FLG-NYURYOKU   =   1
               PERFORM 4101-KOUI-INPUTCD-KENSAKU-SEC
           END-IF.
      *
       41011-KOUI-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード文字列調査処理
      *****************************************************************
       4101-KOUI-INPUTCD-CHOSA-SEC       SECTION.
      *
      *--- 文字列調査の開始位置　取得
           IF  (WRK-INPUTCD(1:1)       IS  NUMERIC) AND
               (WRK-INPUTCD(2:1)       =   SPACE  ) AND
               (WRK-INPUTCD(3:1)   NOT =   SPACE  )
      *        時間エラー
               MOVE    "0005"          TO  SPA-ERRCD
               GO      TO      4101-KOUI-INPUTCD-CHOSA-EXT
           ELSE
               MOVE    1               TO  WRK-IDX-FT
           END-IF.
      *
           MOVE    ZERO                TO  FLG-KANJI.
           MOVE    ZERO                TO  FLG-KANA.
           MOVE    ZERO                TO  FLG-SUJI.
           MOVE    ZERO                TO  WRK-CD-MCNT.
           MOVE    SPACE               TO  WRK-CD.
           PERFORM VARYING IDX     FROM    WRK-IDX-FT   BY  1
                   UNTIL  (IDX         >   22     )  OR
                          (WRK-INPUTCD(IDX:1)
                                       =   SPACE  OR  "*" )
      *      全角の＊を半角の*とする
             IF      WRK-INPUTCD(IDX:2)    =  "＊" OR "　"
               MOVE    22                  TO  IDX
             ELSE
      *
               ADD     1                   TO  WRK-CD-MCNT
               MOVE    WRK-INPUTCD(IDX:1)  TO  WRK-CD (WRK-CD-MCNT:1)
      **********
      *        IF      WRK-INPUTCD (IDX:1)     >=  X"A1"
      *            MOVE    1               TO  FLG-KANJI
      *        ELSE
      *            IF     (WRK-INPUTCD (IDX:1)
      *                                   IS  ALPHABETIC)  OR
      *                   (WRK-INPUTCD (IDX:1)
      *                                   IS  NUMERIC   )  OR
      *                   (WRK-INPUTCD (IDX:1)
      *                                   =   "."  OR  "/" )
      *                MOVE    1              TO  FLG-SUJI
      *            ELSE
      *                MOVE    1              TO  FLG-KANA
      *            END-IF
      ******** END-IF
             END-IF
           END-PERFORM.
      *
      *    混在エラー
           MOVE    WRK-CD              TO  WRK-KOUMOKU
           MOVE    WRK-CD-MCNT         TO  WRK-LEN
           PERFORM 1009-KANACONV-SEC
           IF      FLG-ZENKAKU         =   1
               MOVE    1               TO  FLG-KANJI
           END-IF
           IF      FLG-HANKAKU         =   1
               MOVE    1               TO  FLG-SUJI
           END-IF
      *
      **** 半角カナは入力不可
      *    IF      FLG-KANA            =   1
      *        MOVE    "0005"          TO  SPA-ERRCD
      *        GO      TO      4101-KOUI-INPUTCD-CHOSA-EXT
      **** END-IF.
      *    半角、全角混在は入力不可
           IF     (FLG-KANJI           =   1)  AND
                  (FLG-SUJI            =   1)
               MOVE    "0005"          TO  SPA-ERRCD
               GO      TO      4101-KOUI-INPUTCD-CHOSA-EXT
           END-IF.
      *
      *    点数コードを表示する
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           =   WRK-CD(1:WRK-CD-MCNT)
               MOVE    ZERO                TO  FLG-NYURYOKU
           ELSE
               MOVE    1                   TO  FLG-NYURYOKU
           END-IF
           .
      *
       4101-KOUI-INPUTCD-CHOSA-EXT.
           EXIT.
      *
      *****************************************************************
      *    漢字変換処理
      *****************************************************************
       4101-KOUI-MOJI-HENKAN-SEC    SECTION.
      *    漢字変換
           MOVE    SPACE               TO  WRK-HENKAN-ATO.
           MOVE    WRK-STR             TO  IDH1.
           MOVE    ZERO                TO  IDH2.
           PERFORM   UNTIL       IDH1    >   WRK-END
      *      全角判定
             IF      IDH1             =    WRK-END
                 MOVE    ZERO              TO  FLG-ZENKAKU
             ELSE
                 MOVE    WRK-HENKAN-MAE(IDH1:2)    TO  WRK-KOUMOKU
                 MOVE    2                     TO  WRK-LEN
                 PERFORM 1009-KANACONV-SEC
             END-IF
             IF      FLG-ZENKAKU         =   1
      *******IF      WRK-HENKAN-MAE(IDH1:1)
      *******                             >=  X"A1"
               EVALUATE    WRK-HENKAN-MAE(IDH1:2)
                   WHEN    "０"
                       ADD     1           TO  IDH2
                       MOVE    "0"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "１"
                       ADD     1           TO  IDH2
                       MOVE    "1"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "２"
                       ADD     1           TO  IDH2
                       MOVE    "2"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "３"
                       ADD     1           TO  IDH2
                       MOVE    "3"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "４"
                       ADD     1           TO  IDH2
                       MOVE    "4"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "５"
                       ADD     1           TO  IDH2
                       MOVE    "5"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "６"
                       ADD     1           TO  IDH2
                       MOVE    "6"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "７"
                       ADD     1           TO  IDH2
                       MOVE    "7"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "８"
                       ADD     1           TO  IDH2
                       MOVE    "8"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "９"
                       ADD     1           TO  IDH2
                       MOVE    "9"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "．"
                       ADD     1           TO  IDH2
                       MOVE    "."         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "＊"
                       ADD     1           TO  IDH2
                       MOVE    "*"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "　"
                       ADD     1           TO  IDH2
                       MOVE    SPACE       TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    OTHER
                       ADD     1           TO  IDH2
                       MOVE    WRK-HENKAN-MAE(IDH1:2)
                                           TO  WRK-HENKAN-ATO (IDH2:2)
                       ADD     1           TO  IDH2
               END-EVALUATE
               ADD     2                   TO  IDH1
             ELSE
               ADD     1                   TO  IDH2
               MOVE    WRK-HENKAN-MAE(IDH1:1)
                                           TO  WRK-HENKAN-ATO (IDH2:1)
               ADD     1                   TO  IDH1
             END-IF
           END-PERFORM.
      *
       4101-KOUI-MOJI-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       4101-KOUI-INPUTCD-KENSAKU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-ICD-CDSYU.
           IF      FLG-KANJI           =   1
      *        全角のコードチェック
               MOVE    "J"             TO  WRK-ICD-CDSYU
           ELSE
      *        診療コードチェック
               IF      WRK-CD(1:1)         =   "P"  OR  "S"
                   MOVE    "0005"              TO  SPA-ERRCD
                   GO  TO                 4101-KOUI-INPUTCD-KENSAKU-EXT
               ELSE
                   IF      WRK-CD(1:WRK-CD-MCNT)   NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    9
      *                    診療行為コード入力
                               MOVE    WRK-CD(1:WRK-CD-MCNT)
                                                   TO  WRK-WK-CD
                               MOVE    WRK-WK-CD   TO  WRK-CD
      *                    短縮３桁
                           WHEN    3
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *                    ４桁
                           WHEN    4
                               MOVE    "4"         TO  WRK-ICD-CDSYU
      *                    ５桁
                           WHEN    5
                               MOVE    "5"         TO  WRK-ICD-CDSYU
      *                    ６桁
                           WHEN    6
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *
                           WHEN    OTHER
                               MOVE    "A"         TO  WRK-ICD-CDSYU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  WRK-ICD-CDSYU
                   END-IF
               END-IF
           END-IF.
      *
      *    入力コード存在チェック
           IF     WRK-ICD-CDSYU    NOT =   SPACE
      *
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    WRK-ICD-CDSYU       TO  ICD-CDSYU
               MOVE    WRK-CD              TO  ICD-INPUTCD
      *
               PERFORM 930-INPUTCD-READ-SEC
      *        入力コードなし
               IF      FLG-INPUTCD     NOT =   ZERO
                   IF      WRK-ICD-CDSYU       =   "4" OR "5" OR "6"
                       MOVE    "0050"          TO  SPA-ERRCD
                       GO  TO            4101-KOUI-INPUTCD-KENSAKU-EXT
                   END-IF
               END-IF
      *
      *        全桁対象がある時
               IF     (WRK-CD              =  ICD-INPUTCD  )  AND
                      (ICD-SRYCD(1:1)      =   "S"  OR  "P")
      *
      *            セットの時、時間外がないこと
                   IF      FLG-JKNKSN      NOT =   ZERO
                       MOVE    "0010"              TO  SPA-ERRCD
                       GO  TO            4101-KOUI-INPUTCD-KENSAKU-EXT
                   END-IF
      *
                   MOVE    WRK-HENKAN          TO  WRK-INPUTCD
                   MOVE    ICD-SRYCD(1:6)      TO  WRK-HENKAN(1:6)
                   COMPUTE IDY                 =   WRK-CD-MCNT   +  1
                   MOVE    WRK-INPUTCD(IDY:)   TO  WRK-HENKAN(7:)
               ELSE
                   IF     (WRK-CD              =   ICD-INPUTCD)
      *                点数コード決定
                       MOVE    ICD-SRYCD           TO  WRK-CD
                   ELSE
      *                一覧表示へ
                       MOVE    "1"                 TO  WRK-KENSAKU
                   END-IF
               END-IF
           END-IF.
      *
       4101-KOUI-INPUTCD-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｐ入力画面へ移行
      *****************************************************************
       430-I49-SYORI-SEC       SECTION.
      *
      *    画面編集
           INITIALIZE                  SPA-I49GMN-AREA
           MOVE    SPACE               TO  SPA-I49-DATA
           MOVE    SPACE               TO  I49
           INITIALIZE                      I49
           MOVE    SPACE               TO  I49-PIN
           MOVE    SPACE               TO  I49-PED
      *
      *    画面カーソルセット処理
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK
      *
           MOVE    "I49"               TO  SPA-SAKIPG
           MOVE    "I44"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I49"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END.
      *
       430-I49-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為一覧へ
      *****************************************************************
       410-I48-SYORI-SEC          SECTION.
      *
           INITIALIZE                  SPA-I48GMN-AREA
           IF      SPA-I48-INPUTCD     NOT =   SPACE
               MOVE    "8"                 TO  SPA-I48-GMN-SYORI
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-SCR-REC         TO  SPA-I44GMN-AREA
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-I44             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGI48"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-FREE            TO  SPA-I44
           MOVE    SPA-I44GMN-AREA     TO  SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-I4KYOUTU
      *
           IF      SPA-I48-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
           END-IF
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "I48"               TO  SPA-SAKIPG
           MOVE    "I44"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I48"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410-I48-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    数量
           MOVE    1                   TO  SPA-COM-SURYO(SPA-GMN-IDX).
           MOVE    SPACE               TO  SPA-COM-SURFLG(SPA-GMN-IDX).
           MOVE    1                   TO  SPA-COM-SURYO2(SPA-GMN-IDX).
      *
      *    回数（未入力時、１回）
           IF      SPA-COM-KAISU(SPA-GMN-IDX)  =  ZERO
               MOVE    1               TO  SPA-COM-KAISU (SPA-GMN-IDX)
           END-IF.
           MOVE    SPACE               TO  SPA-COM-KAIFLG(SPA-GMN-IDX).
      *
      *    分画数（未入力時、１回）
           MOVE    1                   TO  SPA-COM-BUNGSU(SPA-GMN-IDX).
      *
      *    入力値の入力判定用
      *    その他値
           MOVE    SPA-COM-ATAI1 (SPA-GMN-IDX) TO  WRK-HZN-ATAI1.
           MOVE    SPA-COM-ATAI2 (SPA-GMN-IDX) TO  WRK-HZN-ATAI2.
           MOVE    SPA-COM-ATAI3 (SPA-GMN-IDX) TO  WRK-HZN-ATAI3.
           MOVE    SPA-COM-ATAI4 (SPA-GMN-IDX) TO  WRK-HZN-ATAI4.
      *
           MOVE    SPACE               TO  SPA-COM-ATAI1 (SPA-GMN-IDX).
           MOVE    SPACE               TO  SPA-COM-ATAI2 (SPA-GMN-IDX).
           MOVE    SPACE               TO  SPA-COM-ATAI3 (SPA-GMN-IDX).
           MOVE    SPACE               TO  SPA-COM-ATAI4 (SPA-GMN-IDX).
      *
       4101122-KOMOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SEC           SECTION.
      *
      *    空白行をなくす
           PERFORM 420-SYOKYO-SEC.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-LINE-MAX
               MOVE    SPACE           TO  SPA-COM-ZAIEND (IDX)
      *        カーソルのクリア
               MOVE    SPACE           TO  SPA-COM-CUR    (IDX)
      *
               IF      (SPA-GMN-INPUTCD(IDX) =   SPACE)   AND
                       (SPA-COM-INPUTCD(IDX) =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-GMN-MAX
               END-IF
           END-PERFORM.
      *
      *    診療種別区分編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX              >   SPA-GMN-MAX )   OR
                              (SPA-ERRCD        NOT =   SPACE)
      *        剤分離チェック
               PERFORM 5101-SYRKBN-SET-SEC
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      510-TBLHEN-EXT
           END-IF
      *
      *    点数計算
           PERFORM 5102-TENSU-KEISEN-SEC
           MOVE    WRK-ZAITEN          TO  SPA-GMN-KEI (SPA-GMN-MAX)
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE 
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1)
                                       /   20     +    1
           END-IF.
      *
      *
        510-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別区分編集決定処理
      *****************************************************************
       5101-SYRKBN-SET-SEC             SECTION.
      *
      *    診療種別入力時、剤の終了とする
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
               MOVE    SPA-GMN-INPUTCD(IDX)(2:3)   TO  WRK-SRYSYUKBN
      *
      *        診療種別区分名称、診療行為区分セット
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                       AT   END
                           MOVE    SPACE           TO  WRK-SRYKBN
                           MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                           MOVE    "0006"          TO  SPA-ERRCD
                       WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                            (MSYU-IDX)
                           MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                   TO  WRK-SRYKBN
                           MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                   TO  WRK-SRYSYUKBNMEI
               END-SEARCH
               MOVE    WRK-SRYSYUKBNMEI    TO  SPA-GMN-MEISYO (IDX)
               MOVE    WRK-SRYKBN          TO  SPA-GMN-SRYKBN (IDX)
      *
               MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN (IDX)
      *H28.11
      *        診療区分変更はエラー
               IF     (SPA-SRYKBN      NOT =   WRK-SRYKBN)
                   MOVE    "1019"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               END-IF
      *
               IF      IDX                 >   1
                   MOVE    "1017"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               END-IF
               GO      TO      5101-SYRKBN-SET-EXT
           END-IF.
      *
      *    手技料は１つとする
           IF      SPA-COM-DATAKBN(IDX)    =   "1"
               IF      FLG-SYUGI           =   1
                   MOVE    "1017"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               ELSE
                   MOVE    1                   TO  FLG-SYUGI
               END-IF
           END-IF
      *    加算の前に手技があること
           IF      SPA-COM-DATAKBN(IDX)    =   "2"
               IF      FLG-SYUGI           =   ZERO
                   MOVE    "1018"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               END-IF
           END-IF
      *
      *    最終行を剤終了とする
           IF      IDX                 =   SPA-GMN-MAX
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
           END-IF.
      *
       5101-SYRKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       5102-TENSU-KEISEN-SEC             SECTION.
      *
      *    剤点数・剤内容編集
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-ZAITEN-G
           MOVE    ZERO                TO  IDX-SYU
      *
           INITIALIZE                  WRK-SRYACCT-AREA
           MOVE    ZERO                TO  IDX-ATO
      *    点数算定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "."
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  SPA-GMN-KEI (IDX)
      *
      *            点数集計
                   EVALUATE    SPA-COM-TENSIKIBETU (IDX)
      *                ％加算・減算
                       WHEN    5
                       WHEN    6
                           CONTINUE
      *                点数（マイナス）
                       WHEN    8
                           COMPUTE WRK-ZAITEN-G    =   WRK-ZAITEN-G
                                                   +   SPA-COM-TEN (IDX)
      *                点数
                       WHEN    OTHER
                           COMPUTE WRK-ZAITEN      =   WRK-ZAITEN
                                                   +   SPA-COM-TEN (IDX)
                   END-EVALUATE
      *            手技料判定
                   IF      SPA-COM-DATAKBN (IDX)   =   "1"
                       IF      IDX-SYU             =   ZERO
                           MOVE    IDX             TO  IDX-SYU
                       END-IF
                   END-IF
      *
      *            各集計
                   MOVE    SPA-COM-INPUTCD(IDX)    TO  WRK-SRYCD-9
                   COMPUTE WRK-SRYCDTOTAL      =   WRK-SRYCDTOTAL
                                               +   WRK-SRYCD-9
                   ADD     1                   TO  WRK-MEISAISU
                   ADD     1                   TO  WRK-SURYOUTOTAL
                   ADD     1                   TO  IDX-ATO
      *
               END-IF
           END-PERFORM
      *
      *    点数集計処理
           MOVE    ZERO                TO  WRK-KASAN
           MOVE    ZERO                TO  WRK-KASAN-ALL
           MOVE    ZERO                TO  WRK-GENZAN
           MOVE    ZERO                TO  WRK-GENZAN-ALL
      *    点数計算
           IF      WRK-ZAITEN-G        >   WRK-ZAITEN
               MOVE    ZERO                TO  WRK-ZAITEN
           ELSE
               COMPUTE WRK-ZAITEN          =   WRK-ZAITEN
                                           -   WRK-ZAITEN-G
           END-IF
      *    点数算定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-MAX
               EVALUATE    SPA-COM-TENSIKIBETU (IDX)
      *            ％加算
                   WHEN    5
                       COMPUTE WRK-KASAN       =   WRK-ZAITEN
                                               *   SPA-COM-TEN (IDX)
                                               /   100
                                               +   0.5
                       ADD     WRK-KASAN       TO  WRK-KASAN-ALL
      *            ％減算
                   WHEN    6
                       COMPUTE WRK-GENZAN      =   WRK-ZAITEN
                                               *   SPA-COM-TEN (IDX)
                                               /   100
                       ADD     WRK-GENZAN      TO  WRK-GENZAN-ALL
               END-EVALUATE
           END-PERFORM
      *
      *    点数計算
           COMPUTE WRK-ZAITEN          =   WRK-ZAITEN
                                       +   WRK-KASAN-ALL
                                       -   WRK-GENZAN-ALL
      *
           .
      *
       5102-TENSU-KEISEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1.
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                       MOVE    "0006"          TO  SPA-ERRCD
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH.
      *
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         =   200
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  IDY.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      IDX             =   SPA-GMN-IDX
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    WRK-COM-TBLREC (IDY) 
                                           TO  SPA-COM-TBLREC(IDX)
                   MOVE    WRK-GMN-TBLREC (IDY) 
                                           TO  SPA-GMN-TBLREC(IDX)
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-PERFORM.
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1)
                                       /   20     +    1
           END-IF.
      *
           MOVE    1                   TO  SPA-GMN-CUR.
      *
       41014-GYOINSN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC.
           INITIALIZE                      SPA-GMN-REC.
           MOVE    ZERO                TO  SPA-GMN-MAX.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
      *            削除行が加算料を自動追加した時、自動追加分も削除
                   IF     (WRK-COM-INPUTCD(IDX)    NOT =   SPACE)  AND
                          (IDX                         <   200  )  AND
                          (WRK-COM-AUTOKBN  (IDX + 1)  =   "1"  )
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX + 1)
                   END-IF
               ELSE
                   ADD     1                      TO  SPA-GMN-MAX
                   MOVE    WRK-COM-TBLREC (IDX)   TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                   MOVE    WRK-GMN-TBLREC (IDX)   TO  SPA-GMN-TBLREC
                                                        (SPA-GMN-MAX)
               END-IF
           END-PERFORM.
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1)
                                       /   20     +    1
           END-IF.
      *
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I41"               TO  SPA-SAKIPG.
           MOVE    "I44"               TO  SPA-MOTOPG.
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE    "I41"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC.
           IF     (SPA-ERRCD       NOT =   SPACE)  OR
                  (FLG-END             =   1    )
               GO      TO      450-MAEGMN-EXT
           END-IF.
      *
           IF      SPA-GMN-PAGE       =   1
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE  -   1
           END-IF.
      *
           MOVE    1                   TO  SPA-MAP-IDX
           MOVE    1                   TO  SPA-GMN-CUR.
      *
       450-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-ATOGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC.
           IF     (SPA-ERRCD       NOT =   SPACE)  OR
                  (FLG-END             =   1    )
               GO      TO      460-ATOGMN-EXT
           END-IF.
      *
           COMPUTE WRK-FREE            =   SPA-GMN-PAGE  *  20.
           IF      SPA-GMN-INPUTCD(WRK-FREE)   =   SPACE
               GO  TO                 460-ATOGMN-EXT
           END-IF.
      *
           ADD     1                   TO  SPA-GMN-PAGE.
      *
           MOVE    1                   TO  SPA-GMN-CUR.
      *
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       41001-KOUI-ALLCHK-SEC       SECTION.
      *
      *    画面をＳＰＡへ編集
           MOVE    1                   TO  WRK-IDX2
           PERFORM 2002-GMNSPA-SET-SEC.
      *
      *    入力コード・名称チェック処理
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-MAX     =   ZERO
                   MOVE    "1021"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
      *
       41001-KOUI-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE)  OR
                  (FLG-END             =   1    )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    剤のチェック
           PERFORM 45011-NAIYOU-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
      *        同一剤が存在している時、エラーとする
               IF      FLG-OK              =   1
                   MOVE    "1025"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (FLG-OK2             =   1   )  AND
                      (WRK-ZAINUM          =   ZERO)
      *            剤変更なしとして、処理を終了する
                   PERFORM 210-BACK
               ELSE
                   MOVE    "0101"              TO  SPA-IIDCD
               END-IF
           END-IF
           .
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
      *    入院行為マスタ更新
           PERFORM 4501-NYUKOUI-KOU-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF.
      *
      *    入院会計マスタ更新（追加はないこととする）
           PERFORM 4502-NYUKAI-KOU-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
           MOVE    "I41"               TO  SPA-SAKIPG
           MOVE    "I44"               TO  SPA-MOTOPG
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF.
      *
           MOVE    "I41"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスタ更新処理
      *****************************************************************
       4501-NYUKOUI-KOU-SEC            SECTION.
      *
      *    対象の剤をクリアする
           PERFORM 45012-DBDELETE-SEC
           MOVE    SPA-ZAINUM          TO  WRK-ZAINUM
      *
      *    点数計算・剤内容集計
           PERFORM 5102-TENSU-KEISEN-SEC
      *
      *    入院行為新規作成
           MOVE    ZERO                TO  IDS.
           MOVE    ZERO                TO  WRK-RENNUM.
           INITIALIZE                  NYUINACT-REC.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "."
                   CONTINUE
               ELSE
                   ADD     1               TO  IDS
      *            入院行為内容編集
                   PERFORM 45101-NYUKOUI-HEN-SEC
      *
                   IF      IDS             >=  5
      *                新規レコード追加
                       PERFORM 45102-NYUKOU-INS-SEC
                   END-IF
               END-IF
           END-PERFORM.
      *
           IF      IDS                 >   ZERO
      *        新規レコード追加
               PERFORM 45102-NYUKOU-INS-SEC
           END-IF.
      *
       4501-NYUKOUI-KOU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    剤のチェック処理
      *****************************************************************
       45011-NAIYOU-CHK-SEC              SECTION.
      *
      *    点数計算・剤内容集計
           PERFORM 5102-TENSU-KEISEN-SEC
      *
           IF       WRK-MEISAISU       =   ZERO
               MOVE    "1021"              TO  SPA-ERRCD
               GO      TO      45011-NAIYOU-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  FLG-OK
      *    診療会計マスターを特定する
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SPA-I40-SRYKA-READ  TO  NACCT-SRYKA
           MOVE    SPA-I40-SRYYMD(1:6) TO  NACCT-SRYYM
           MOVE    SPA-SRYKBN          TO  NACCT-SRYKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 940-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           PERFORM
               UNTIL   (FLG-NYUINACCT      =   1    ) OR
                       (FLG-OK             =   1    )
      *
      *        診療コード計を比較する
               IF     (NACCT-SRYCDTOTAL    =   WRK-SRYCDTOTAL ) AND
                      (NACCT-ZAITEN        =   WRK-ZAITEN     ) AND
                      (NACCT-MEISAISU      =   WRK-MEISAISU   ) AND
                      (NACCT-SURYOUTOTAL   =   WRK-SURYOUTOTAL)
                   MOVE    NACCT-ZAINUM        TO  WRK-H-ZAINUM
      *            診療行為内容の比較処理
                   PERFORM 45012-NYUINACCT-CHK-SEC
      *
      *            同一診療会計マスターあり
                   IF      FLG-OK          =   1
      *                新規追加
                       MOVE    WRK-H-ZAINUM        TO  WRK-ZAINUM
                   END-IF
               END-IF
               IF      FLG-OK              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 940-NYUINACCT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-ZAINUM          =   ZERO
               MOVE    ZERO                TO  FLG-OK
           END-IF
           .
      *
       45011-NAIYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院行為内容の比較処理
      *****************************************************************
       45012-NYUINACCT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-SIN.
           INITIALIZE                      TBL-MAE-AREA. 
      *    入院会計マスターから入院行為マスターを検索する
           INITIALIZE                      NYUINACT-REC.
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM.
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN.
           MOVE    NACCT-PTID          TO  NSRY-PTID.
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA.
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM.
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
           PERFORM UNTIL     (FLG-NYUINACT     =   1)
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5)  OR
                                  (IDX-SIN     >=  30  )  OR
                                  (NSRY-SRYCD(IDX2)    =   SPACE)
      *            入力コードのみの判定とする
                   ADD     1                   TO  IDX-SIN
                   MOVE    NSRY-SRYCD  (IDX2)  TO  TBL-MAE-SRYCD
                                                             (IDX-SIN)
               END-PERFORM
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
           END-PERFORM.
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入院行為数が違う
           IF      IDX-SIN         NOT =   IDX-ATO
               GO      TO      45012-NYUINACCT-CHK-EXT
           END-IF
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   IDX-ATO)  OR
                              (FLG-ERR     =   1      )
               MOVE    ZERO                TO  FLG-ARI
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   IDX-SIN)  OR
                                  (FLG-ARI     =   1      )
                   IF      SPA-COM-INPUTCD(IDX1)   =   TBL-MAE-SRYCD
                                                               (IDX2)
                        MOVE    1                  TO  FLG-ARI
                   END-IF
               END-PERFORM
               IF      FLG-ARI             =   ZERO
                    MOVE    1                  TO  FLG-ERR
               END-IF
           END-PERFORM.
      *
           IF      FLG-ERR             =   ZERO
               MOVE    1               TO  FLG-OK
           END-IF.
      *
       45012-NYUINACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の剤クリア処理
      *****************************************************************
       45012-DBDELETE-SEC              SECTION.
      *
      *    入院行為データ削除
           INITIALIZE                      NYUINACT-REC.
           MOVE    SPA-HOSPNUM         TO  NSRY-HOSPNUM.
           MOVE    SPA-NYUGAIKBN       TO  NSRY-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NSRY-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NSRY-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NSRY-SRYYM.
           MOVE    SPA-ZAINUM          TO  NSRY-ZAINUM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "DBDELETE"          TO  MCP-FUNC.
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I44 NYUINACT DEL ERR;"    MCP-RC
           END-IF.
      *
       45012-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    入院行為内容編集処理
      *****************************************************************
       45101-NYUKOUI-HEN-SEC              SECTION.
      *
      *    行為コード
           MOVE    SPA-COM-INPUTCD(IDX)    TO  NSRY-SRYCD (IDS)
      *    数量
           MOVE    1                   TO  NSRY-SRYSURYO (IDS)
      *    回数
           MOVE    ZERO                TO  NSRY-SRYKAISU (IDS)
           MOVE    SPACE               TO  NSRY-AUTOKBN  (IDS)
           .
      *
       45101-NYUKOUI-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院行為新規レコード編集
      *****************************************************************
       45102-NYUKOU-INS-SEC            SECTION.
      *
           ADD     1                     TO  WRK-RENNUM.
           MOVE    SPA-HOSPNUM           TO  NSRY-HOSPNUM.
           MOVE    SPA-NYUGAIKBN         TO  NSRY-NYUGAIKBN.
           MOVE    SPA-GMN-PTID          TO  NSRY-PTID.
           MOVE    SPA-I40-SRYKA-READ    TO  NSRY-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6)   TO  NSRY-SRYYM.
           MOVE    WRK-ZAINUM            TO  NSRY-ZAINUM.
           MOVE    WRK-RENNUM            TO  NSRY-RENNUM.
      *
           MOVE    SPA-COM-SRYSYUKBN(01) TO  NSRY-SRYSYUKBN
           MOVE    SPA-SRYKBN            TO  NSRY-SRYKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD          TO  NSRY-CREYMD
                                             NSRY-UPYMD
           MOVE    SMCNDATE-HMS          TO  NSRY-UPHMS
      *
           MOVE    NYUINACT-REC          TO  MCPDATA-REC.
           MOVE    "DBINSERT"            TO  MCP-FUNC.
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I44 NYUINACT INS ERR;"    MCP-RC
                                     ",KEY:"  NSRY-KEY 
               MOVE    "1029"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    ZERO                  TO  IDS.
           INITIALIZE                    NYUINACT-REC.
      *
       45102-NYUKOU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ更新処理
      *****************************************************************
       4502-NYUKAI-KOU-SEC          SECTION.
      *
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM.
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NACCT-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NACCT-SRYYM.
           MOVE    SPA-SRYKBN          TO  NACCT-SRYKBN.
           MOVE    SPA-ZAINUM          TO  NACCT-ZAINUM.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-NYUINACCT      NOT =  ZERO
               MOVE    "1027"              TO  SPA-ERRCD
               GO      TO      4502-NYUKAI-KOU-EXT
           END-IF
      *
      *    剤点数
           MOVE    WRK-ZAITEN          TO  NACCT-ZAITEN.
      *    診療コード計
           MOVE    WRK-SRYCDTOTAL      TO  NACCT-SRYCDTOTAL.
      *    数量計
           MOVE    WRK-SURYOUTOTAL     TO  NACCT-SURYOUTOTAL.
      *    明細数
           MOVE    WRK-MEISAISU        TO  NACCT-MEISAISU.
      *    手技点数１
           MOVE    WRK-ZAITEN          TO  NACCT-SYUTEN1.
      *    手技点数２
           MOVE    ZERO                TO  NACCT-SYUTEN2
      *    薬剤点数
           MOVE    ZERO                TO  NACCT-YKZTEN
      *    器材点数
           MOVE    ZERO                TO  NACCT-KIZAITEN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  NACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  NACCT-UPHMS
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBUPDATE"          TO  MCP-FUNC.
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I44 NYUINACCT UPD ERR:"  MCP-RC
                       ",KEY:"  NACCT-KEY
           ELSE
               PERFORM   600-SYUKA-SET-SEC
           END-IF
           .
       4502-NYUKAI-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF.
      *
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF.
      *
           IF      SPA-IIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF.
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I44    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    行位置とカーソル位置の決定
           PERFORM 5002-GMNSPA-HEN-SEC.
      *
           MOVE    SPACE               TO  I44
      *
           INITIALIZE                      I44
           MOVE    ZERO                TO  IDX.
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   200)  OR
                              (IDX         >=  20 )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1                TO  IDX
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                            TO  I44-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                            TO  I44-INPUTCD(IDX)
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                            TO  I44-MEISYO(IDX)
      *            剤点数
                   IF      SPA-GMN-KEI (WRK-IDX)   NOT =   ZERO
                       MOVE    SPA-GMN-KEI(WRK-IDX)
                                                TO  WRK-TENSU-Z
                       MOVE    WRK-TENSU-X      TO  I44-SURYO (IDX)
                   END-IF
               END-IF
      *
           END-PERFORM.
      *
           MOVE    SPA-GMN-PTNUM       TO  I44-PTNUM.
           MOVE    SPA-GMN-KANANAME    TO  I44-KANANAME.
           MOVE    SPA-GMN-NAME        TO  I44-NAME.
           MOVE    SPA-GMN-SEX         TO  I44-SEX.
           MOVE    SPA-I40-SRYYMDW(1:6)    TO  I44-SRYYM.
           MOVE    SPA-GMN-BIRTHDAYW   TO  I44-BIRTHDAY.
           MOVE    SPA-I40-SRYKAMEI    TO  I44-SRYKA.
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               MOVE    SPA-SEL-DAY (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ               TO  I44-DAY(IDX)
           END-PERFORM.
      *
           IF      FLG-END             =   ZERO
               PERFORM 5001-MAPCUR-SEC
           END-IF
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行位置とカーソル位置の決定処理
      *****************************************************************
       5002-GMNSPA-HEN-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX.
           MOVE    ZERO                TO  WRK-MAP-MAX.
           MOVE    ZERO                TO  WRK-MAP-CUR.
           MOVE    ZERO                TO  WRK-MAP-CUR2.
           MOVE    ZERO                TO  WRK-MAP-CUR3.
           MOVE    ZERO                TO  WRK-MAP-MAXPAGE.
           MOVE    ZERO                TO  WRK-MAP-PAGE.
           MOVE    ZERO                TO  WRK-MAP-PAGE2.
           MOVE    ZERO                TO  WRK-MAP-PAGE3.
           MOVE    1                   TO  WRK-PAGE.
           MOVE    ZERO                TO  SPA-GMN-MAX.
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   200
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   20
                       ADD     1               TO  WRK-PAGE
                       MOVE    1               TO  IDX
                   END-IF
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1"
                           IF      WRK-MAP-CUR         =   ZERO
                              MOVE    IDX             TO  WRK-MAP-CUR
                              MOVE    WRK-PAGE        TO  WRK-MAP-PAGE
                           END-IF
                       END-IF
      *                警告位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "2"  OR  "3"
                           IF      WRK-MAP-CUR2        =   ZERO
                               MOVE    IDX             TO  WRK-MAP-CUR2
                               MOVE    WRK-PAGE        TO  WRK-MAP-PAGE2
                           END-IF
                       END-IF
      *                フリーコメントで未入力の行
                       IF     SPA-COM-IN2 (IDX)   =   "3"
                           MOVE    IDX             TO  WRK-MAP-CUR3
                           MOVE    WRK-PAGE        TO  WRK-MAP-PAGE3
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPACE           TO  SPA-COM-CUR(WRK-IDX)
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM.
      *
      *    画面カーソル位置決定（警告はエラーの次に）
           MOVE    ZERO                TO  SPA-MAP-IDX.
           IF      WRK-MAP-CUR          =   ZERO
               IF      WRK-MAP-CUR2    NOT =   ZERO
                   MOVE    WRK-MAP-CUR2        TO  SPA-MAP-IDX
                   MOVE    WRK-MAP-PAGE2       TO  SPA-GMN-PAGE
               END-IF
           ELSE
               MOVE    WRK-MAP-CUR         TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE        TO  SPA-GMN-PAGE
           END-IF.
      *
           IF      SPA-MAP-IDX     NOT =   ZERO
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF.
      *    名称入力へ
           IF      WRK-MAP-CUR3    NOT =   ZERO
               MOVE    WRK-MAP-CUR3        TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE3       TO  SPA-GMN-PAGE
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF.
      *
      *    カーソル指定のない時、次ぎの空白行へ
      *    後画面へ
           IF      SPA-GMN-CUR        =   88  OR  99
               IF      WRK-MAP-MAXPAGE     =   SPA-GMN-PAGE
                   COMPUTE SPA-MAP-IDX        =   WRK-MAP-MAX   +   1
               ELSE
                   MOVE    1                   TO  SPA-MAP-IDX
               END-IF
           ELSE
               COMPUTE SPA-MAP-IDX        =   WRK-MAP-MAX   +   1
               IF      SPA-MAP-IDX        >   20
                   OR  SPA-GMN-PAGE       <   WRK-MAP-MAXPAGE
                   MOVE    99                  TO  SPA-MAP-IDX
               ELSE
                   IF      SPA-GMN-PAGE   >   WRK-MAP-MAXPAGE
                       MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                   END-IF
               END-IF
           END-IF.
           IF      SPA-GMN-PAGE       =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF.
      *
       5002-GMNSPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソルセット
           MOVE    SPA-MAP-IDX        TO  WRK-IDX2
           IF      WRK-IDX2            =   ZERO
               MOVE    1               TO  WRK-IDX2
           END-IF.
      *
           IF      WRK-IDX2            >   20
               MOVE    00              TO  SPA-GMN-CUR
           END-IF.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    00
                   MOVE    "B06"           TO  MCP-WIDGET
               WHEN    01
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
               WHEN    02
                   MOVE    "MEISYO"        TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当する点数マスターが存在しません"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "診療行為は数量を入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "入力コードの文字エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "約束セットは入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "診療種別がありません"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE
                   "２００行を超えます。これ以上挿入はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "時間外の入力はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "入外適用エラー" 
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "老人適用エラー" 
                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "病院診療所適用エラー" 
                                       TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "施設基準ではありません。算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "コメントの入力値エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "時間加算はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "年齢関連誤り"
                                       TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "併用算定チェック"
                                       TO  SPA-ERRMSG
               WHEN    "0042"
                   MOVE    "注加算連番チェック"
                                       TO  SPA-ERRMSG
               WHEN    "0048"
                   MOVE    "約束セットの時、診療種別を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0050"
                   MOVE    "入力コードがありません"
                                       TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE    "病床数関連誤り"
                                       TO  SPA-ERRMSG
               WHEN    "0080"
                   MOVE    "名称は全角で入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "1020"
                   MOVE    "区分が異なります"
                                       TO  SPA-ERRMSG
               WHEN    "1021"
                   MOVE    "剤の内容がありません。登録できません。"
                                       TO  SPA-ERRMSG
               WHEN    "1025"
                   MOVE    "既に同じ診療行為の剤が存在します。"
                                       TO  SPA-ERRMSG(1:34)
                   MOVE    "回数の変更で修正して下さい"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1027"
                   MOVE    "診療会計マスタ読込みエラー"
                                       TO  SPA-ERRMSG
               WHEN    "1028"
                   MOVE    "剤番号取り込みエラー"
                                       TO  SPA-ERRMSG
               WHEN    "1029"
                   MOVE    "コメントマスター更新エラー"
                                       TO  SPA-ERRMSG
               WHEN    "1030"
                   MOVE    "剤削除は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "8000"
                   MOVE    "保険区分が異なります"
                                       TO  SPA-ERRMSG
               WHEN    "1016"
                   MOVE    "入院料のみ入力可能です。"
                                       TO  SPA-ERRMSG
               WHEN    "1017"
                   MOVE    "剤は１剤のみです"
                                       TO  SPA-ERRMSG
               WHEN    "1018"
                   MOVE    "加算の前に手技料がありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "手技料を入力して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "1019"
                   MOVE    "診療区分は変更できません。"
                                       TO  SPA-ERRMSG
               WHEN    "1020"
                   MOVE    "内容がありませんので、更新はできません。"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  I4ERR
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I44"               TO  SPA-MOTOPG
           MOVE    "I4ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I4ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END.
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0101"
                   MOVE    "入院内容を変更登録します。"
                                       TO  WRK-JIDMSG(1:26)
                   MOVE
                   "回数も登録されますので取消ができなくなります"
                                       TO  WRK-JIDMSG(27:)
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-JIDMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  SPA-I4ID-FLG.
      *
           MOVE    SPACE               TO  I4ID1.
           MOVE    SPA-IIDCD           TO  I4ID1-ID1CODE.
           MOVE    WRK-JIDMSG          TO  I4ID1-ID1MSG.
      *
           MOVE    "戻る"              TO  I4ID1-B01
           MOVE    "ＯＫ"              TO  I4ID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET.
      *
           MOVE    "I44"               TO  SPA-MOTOPG.
           MOVE    "I4ID1"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科設定処理
      *****************************************************************
       600-SYUKA-SET-SEC              SECTION.
      *
      *     DISPLAY "== SYUKA-SET-SEC ==="
      *     DISPLAY "NACCT-SRYYM   = " NACCT-SRYYM
      *     DISPLAY "SPA-HOSPNUM    = " SPA-HOSPNUM
      *     DISPLAY "SPA-PREFNUM   = " SPA-PREFNUM
      *     DISPLAY "SPA-PTID      = " SPA-PTID
      *     DISPLAY "SPA-NYUGAIKBN = " SPA-NYUGAIKBN
      *     DISPLAY "SPA-BIRTHDAY  = " SPA-BIRTHDAY
      *
           INITIALIZE                 ORCSSYUACCAREA
           MOVE    "04"           TO  LNK-SYUACC-I-KBN
           MOVE    NACCT-SRYYM    TO  LNK-SYUACC-I-SRYYM
           CALL    "ORCSSYUKAMAIN"    USING
                                      SPA-AREA
                                      ORCSSYUACCAREA
      *
           .
      *
       600-SYUKA-SET-EXT.
           EXIT.
      *
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
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
      *
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       900-NYUINACT-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
      *
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ読み込み
      *****************************************************************
       940-NYUINACCT-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
      *
       940-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-I40-SRYYMD      TO  TNS-YUKOSTYMD.
           MOVE    SPA-I40-SRYYMD      TO  TNS-YUKOEDYMD.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC.
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF.
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF.
      *
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           IF      WRK-ICD-CDSYU       =   "J"
               IF      WRK-CD-MCNT     NOT =   20
                   MOVE    SPACE               TO  ICD-INPUTCD
                   STRING  WRK-CD          DELIMITED  BY  SPACE
                           "%"             DELIMITED  BY  SIZE
                                           INTO    ICD-INPUTCD
                   END-STRING
               END-IF
           END-IF
           IF      WRK-CD-MCNT         =   20
               MOVE    "key2"              TO  WRK-PATH
           ELSE
               MOVE    "key"               TO  WRK-PATH
           END-IF.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC.
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   INITIALIZE                  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF.
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント読込
      *****************************************************************
       960-PTCOM-READ-SEC         SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC.
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   INITIALIZE                  PTCOM-REC
                   MOVE    1                   TO  FLG-PTCOM
               END-IF
           ELSE
               INITIALIZE                  PTCOM-REC
               MOVE    1                   TO  FLG-PTCOM
           END-IF.
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       960-PTCOM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックデータ読込
      *****************************************************************
       950-CHK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  CHK-REC
               MOVE    ZERO                TO  FLG-CHKMST
           ELSE
               INITIALIZE                      CHK-REC
               MOVE    1                   TO  FLG-CHKMST
           END-IF.
      *
       950-CHK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       975-INPUTSET-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF.
      *
       975-INPSET-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       980-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF.
      *
       980-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード変換読込
      *****************************************************************
       990-SRYCDCHG-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF.
      *
       990-SRYCDCHG-READ-EXT.
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
      *    クロース処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
