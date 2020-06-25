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
       PROGRAM-ID.                 ORCGI41.
      ********************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 入院会計カード入力（Ｉ４１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-小豆沢  03.06.24  診療所老人医療管理料（３０日
      *                                     以内再入院）の考慮を追加
      *  01.00.02    NACL-小豆沢  03.06.25  特定集中治療室管理料の次月会計
      *                                     作成に対応
      *  01.00.03    NACL-小豆沢  03.10.24  労災（自賠）入院時に老人点数無効
      *  01.00.04    NACL-小豆沢  03.10.27  労災（自賠）入院時に選定入院なし
      *  01.00.05    NACL-太田    03.11.17  更新日付編集対応
      *  01.00.06    NACL-小豆沢  04.03.10  老人性痴呆疾患治療病棟入院料２の
      *                                     算定対応追加
      *  01.00.07    NACL-小豆沢  04.03.15  広範囲熱傷特定集中治療室管理料の
      *                                     算定対応追加
      *  01.00.08    NACL-小豆沢  04.03.15  亜急性期入院医療管理料の算定対応追加
      *  01.00.51    NACL-小豆沢  04.03.16  １５歳未満の患者は選定入院対象外
      *  02.00.00    NACL-多々納  04.05.06  仕様統一による改造
      *  02.00.01    NACL-多々納  04.08.31  食事加算の削除追加
      *  02.00.02    NACL-太田    04.09.09  前月ボタン押下時フリーズする
      *                                     不具合の修正
      *  02.00.03    NACL-多々納  05.05.11  北海道療養担当手当て追加
      *  02.00.04    NACL-多々納  06.01.20  外泊の削除追加
      *  02.00.05    NACL-小豆沢  06.03.06  主科設定対応追加
      *平成１８年４月改定対応
      *  02.08.01    NACL-小豆沢  06/03/16  入院料老人コード置き換えは療養病棟
      *                                     または療養病床のみを対象とする
      *              NACL-小豆沢  06/03/17  特定入院料対応
      *                                       脳卒中ケアユニット入院医療管理料
      *                                       精神科救急入院料
      *                                       精神科急性期治療病棟入院料１
      *                                       精神科急性期治療病棟入院料２
      *                                     看護補助加算の変更
      *                                     選択食の廃止対応
      *                                     改定による特定入院料次月作成変更
      *  02.09.01    NACL-小豆沢  06/05/16  老人特定入院基本料を特定入院料
      *                                     より外す
      *  02.09.02    NACL-小豆沢  06/05/22  他医療期間は外泊実更新の条件から
      *                                     外す
      *  02.09.03    NACL-多々納  06/05/16  コメント入力画面変更
      *                                     MONFUNC対応
      *平成１８年７月改定対応
      *  03.00.01    NACL-小豆沢  06/06/19  療養病棟、療養病床改定対応
      *  03.02.01    NACL-小豆沢  06/08/09  入院時食事療養２の場合は特食
      *                                     を入力不可とする
      *  03.02.02    NACL-多々納  06/08/21  食事一括変更追加
      *  03.04.01    NACL-小豆沢  07/02/22  当月食堂加算未算定時は次月会計
      *                                     の食堂加算はゼロ設定で作成する
      *  03.04.02    NACL-小豆沢  07/02/26  病棟別の入院日数（今回入院分）
      *                                     を表示
      *  03.04.03    NACL-小豆沢  07/03/07  病棟設定と算定入院料不一致警告
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
      *  03.05.00    NACL-小豆沢  07.08.02  療養病棟（病床）入院会計
      *                                     表示区分対応
      *  03.05.00    NACL-多々納  07/08/06  入院料重複チェック追加
      *  04.01.00    NACL-小豆沢  07/12/13  ９１日以上の表示に他院歴（特別な
      *                                     関係にある医療機関）を加算
      *平成２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/03/05  亜急性期入院医療管理料２
      *                                     精神科救急入院料２
      *                                     小児入院医療管理料１
      *                                     回復期リハビリテーション入院料１
      *                                     回復期リハビリテーション入院料２
      *                                     精神科救急・合併症入院料
      *                                     短期滞在手術基本料３
      *  04.02.00    NACL-小豆沢  08/03/07  シス管病棟設定の加算判定変更
      *  04.02.00    NACL-小豆沢  08/03/10  療養病棟（病床）入院料の名称
      *                                     表示変更
      *  04.02.00    NACL-小豆沢  08/03/14  会計点数の不整合メッセージ追加
      *  04.02.00    NACL-小豆沢  08/06/26  他院（特別な関係の医療機関）通算日数
      *                                     カウント変更
      *  04.03.00    NACL-小豆沢  08/07/08  褥瘡患者管理加算対応
      *  04.03.00    NACL-小豆沢  08/07/29  最終入院病室の表示追加
      *  04.03.00    NACL-小豆沢  08/08/11  特定入院料コード取得をサブプロに変更
      *  04.05.00    NACL-多々納  09/07/21  入院料削除の入院加算対応
      *  04.06.00    NACL-多々納  10/09/17  一括修正入力変更対応
      *  04.07.00    NACL-多々納  12/11/09  プレビュー追加対応
      *平成２６年４月改定対応
      *  00.00.00    NACL-小豆沢  14/02/27  有床診療所入院基本料１
      *                                     有床診療所入院基本料２
      *                                     有床診療所入院基本料３追加対応
      *  05.00.00    NACL-太田    17/12/08  室料差額最大件数変更(30 -> 99)
      *平成３０年４月改正対応
      *  05.00.00    NACL-小豆沢  18/02/28  他医受診の追加
      *  05.00.00    NACL-小豆沢  18/03/05  療養病棟の読み替えコード追加
      *  05.00.00    NACL-小豆沢  18/03/07  短期滞在手術等基本料の判定変更
      *  05.00.00    NACL-小豆沢  18/03/08  療養病棟入院料の名称読み替えは
      *                                     平成３０年３月まで
      *  05.00.00    NACL-小豆沢  18/04/16  療養病棟入院料２（経過措置）
      *                                     の判定追加
      *  05.00.00    NACL-小豆沢  18/04/18  食事個別指示対応
      *  04.08.00    NACL-多々納  18/05/18  施設基準チェック廃止
      *令和２年４月改正対応
      *  05.00.00    NACL-小豆沢  20/03/10  入院基本料加算区分廃止に伴い
      *                                     療養病棟入院料の判定方法変更
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
      *
      *    画面コントロール
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "I4SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
      *
           03  FLG-SET             PIC 9(01).
           03  FLG-DBRC            PIC 9(01).
           03  FLG-NYUKSN          PIC 9(01).
           03  FLG-SST             PIC 9(01).
           03  FLG-SST-CHK         PIC 9(01).
           03  FLG-NYUINKHNRYO     PIC 9(01).
           03  FLG-RJN             PIC 9(01).
      *
           03  FLG-IKKATUFLG       PIC 9(01).
           03  FLG-K1              PIC 9(01).
           03  FLG-K2              PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
      *
           03  FLG-SYORI-END       PIC 9(01).
      *
           03  FLG-ALL             PIC 9(01).
           03  FLG-SYOKUJI         PIC 9(01).
           03  FLG-WWK-PTNYUINRRK  PIC 9(01).
      *
           03  FLG-SKJ-ASA         PIC 9(01).
           03  FLG-SKJ-HIRU        PIC 9(01).
           03  FLG-SKJ-YUU         PIC 9(01).
      *    プレビュー用
           03  FLG-HKN-OK          PIC 9(01).
           03  FLG-KOGHKN-OK       PIC 9(01).
           03  FLG-RSIHKN-OK       PIC 9(01).
      *
           03  FLG-DAISAN          PIC 9(01).
      *
           03  FLG-NYUINKA-SET     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(03).
           03  IDX9                PIC 9(03).
           03  IDX10               PIC 9(03).
           03  IDY                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDI                 PIC 9(04).
           03  IDJ                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDZ3                PIC 9(04).
           03  IDZ4                PIC 9(04).
           03  IDZ5                PIC 9(04).
           03  IDX-N               PIC 9(03).
           03  IDX-D               PIC 9(02).
           03  IDX-DS              PIC 9(02).
           03  IDX-DE              PIC 9(02).
           03  IDXS1               PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDW1                PIC 9(04).
           03  IDW2                PIC 9(04).
           03  IDW3                PIC 9(04).
           03  IDW4                PIC 9(04).
           03  IDW5                PIC 9(04).
           03  IDW6                PIC 9(04).
           03  IDW7                PIC 9(04).
      *
           03  IDW8                PIC 9(02).
           03  IDW9                PIC 9(02).
      *
           03  IDV                 PIC 9(04).
           03  IDX-HKN             PIC 9(02).
           03  TN-IDX              PIC 9(02).
      *
           03  IDX-D2              PIC 9(02).
      *
           03  IDX-KSN             PIC 9(04).
           03  KIKAN-IDX1          PIC 9(04).
           03  KIKAN-IDX2          PIC 9(04).
           03  IDX-SHOKUDOU        PIC 9(02).
           03  IDU                 PIC 9(04).
           03  SC-IDX              PIC 9(02).
           03  SC-IDX2             PIC 9(02).
           03  IDXD                PIC 9(02).
           03  IDX-SKJ-ASA         PIC 9(02).
           03  IDX-SKJ-HIRU        PIC 9(02).
           03  IDX-SKJ-YUU         PIC 9(02).
      *
      *    点数マスタ予約コード領域（選定入院料）
       01  WRK-YOYAKU.
           03  W-YOYAKU            PIC X(09)   VALUE   "099999916".
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
               05  WRK-SRYDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-GMN-SRYYM       PIC X(06).
           03  WRK-NAI-SRYYM       PIC X(06).
           03  WRK-RRKNUM          PIC 9(03).
      *    月末日算定領域
           03  WRK-MATSUYMD.
               05  WRK-MATSUYY     PIC 9(04).
               05  WRK-MATSUMM     PIC 9(02).
               05  WRK-MATSUDD     PIC 9(02).
      *    精神科急性期治療病棟入院料の３月期間算定領域
           03  WRK-SEISHIN-START   PIC 9(08).
           03  WRK-SEISHIN-END     PIC 9(08).
           03  WRK-SEISHIN-NISSU   PIC 9(08).
      *    パス名領域
           03  WRK-DBPATH          PIC X(20).
           03  WRK-PATHNAME        PIC X(20).
      *    室料差額表示
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-Z1   PIC ------,--9.
               05  WRK-SAGAKU-X1   PIC X(02).
           03  WRK-NYUINNISSU.
               05  WRK-NYUINNISSU-Z PIC ZZZZZZ.
           03  WRK-TSUSANNISSU.
               05  WRK-TSUSANNISSU-Z PIC ZZZZZZ.
      *    剤回数等画面編集用
           03  WRK-IDG             PIC 9(04).
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
      *
           03  WRK-Z99-X.
               05  WRK-Z99         PIC Z99.
           03  WRK-ZZZZ-X.
               05  WRK-ZZZZ        PIC ZZZ.
           03  WRK-ZZ-X.
               05  WRK-ZZ          PIC ZZ.
      *    行選択用
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SELNUM2         PIC 9(03).
      *
           03  WRK-TBANGO          PIC 9(04).
           03  WRK-MOTOPG          PIC X(08).
      *
           03  WRK-ERRCD           PIC X(08).
           03  WRK-ERRMSG          PIC X(80).
           03  WRK-JIDMSG.
               05  WRK-JIDMSG1     PIC X(40).
               05  WRK-JIDMSG2     PIC X(40).
      *
      *    入院会計作成用ワーク
           03  WRK-YMD             PIC X(10). 
           03  WRK-NAI-SRYYM-41.
               05  WRK-NAI-SRYYM-41-YY  PIC 9(04).
               05  WRK-NAI-SRYYM-41-MM  PIC 9(02).
           03  WRK-TSUSAN          PIC 9(05).
           03  WRK-SHONUM          PIC 9(03).
           03  WRK-STYMD           PIC X(08).
           03  WRK-EDYMD           PIC X(08).
           03  WRK-NISSU           PIC 9(05).
           03  WRK-NISSU1          PIC 9(05).
           03  WRK-NISSU2          PIC 9(05).
           03  WRK-SENTEI-STYMD    PIC X(08).
           03  WRK-BIRTHDAY-15                 PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y            PIC 9(04).
               05  WRK-BIRTHDAY-15MD           PIC 9(04).
           03  WRK-SANTEISTYMD     PIC X(08).
           03  WRK-TOKUTEI-NYUIN   PIC X(02).
           03  WRK-SAGAKU-KBN      PIC X(02).
           03  WRK-SAGAKU-KBN9     REDEFINES  WRK-SAGAKU-KBN
                                   PIC 9(02).
           03  WRK-HKNCOMBINUM     PIC X(04).
           03  WRK-HKNCOMBINUM9    REDEFINES  WRK-HKNCOMBINUM
                                   PIC 9(04).
           03  WRK-TAIHI-SRYKA     PIC X(02).
           03  WRK-ZOGENPTN        PIC X(01).
           03  WRK-ZOGEN           PIC S9(05).
           03  WRK-KEISANYMD       PIC X(08).
      **** 03  WRK-TAIHI-SRYYM-41  PIC X(06).
      *    医療機関設定の入院料加算チェック用
      *****03  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 20.
           03  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 60.
           03  WRK-NYUKSNKBN       PIC X(03).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-STYUKYMD        PIC X(08).
           03  WRK-NYUINYM         PIC X(06).
           03  WRK-TOKUBETSU-NYUIN PIC X(01).
           03  WRK-TOKUTEI-TENNYUYMD  PIC X(08).
           03  WRK-STUSAN-NISSU03  PIC 9(05).
      *    特定入院料の会計作成後に一般の入院料で会計を作成するための
      *    領域
           03  WK-TOKUTEI-DAY              PIC 9(04).
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(07).
           03  WRK-KEI-X.
               05  WRK-KEI         PIC Z(07).
      *
           03  WRK-MEISYO          PIC X(40).
           03  WRK-TUSAN           PIC S9(08).
           03  WRK-MEISYO-G        PIC X(100).
      *    診療日
           03  WRK-SRYYM.
               05  WRK-SRYYM-YY         PIC 9(04).
               05  WRK-SRYYM-MM         PIC 9(02).
      *
           03  WRK-BANGO           PIC 9(04).
           03  WRK-IDX             PIC 9(04).
      *    保険期間チェック
           03  WRK-HKN-ST          PIC 9(02).
           03  WRK-HKN-ED          PIC 9(02).
           03  WRK-HKN-YMD.
               05  WRK-HKN-YM          PIC X(06).
               05  WRK-HKN-DD          PIC 9(02).
      *
           03  WRK-MATU                PIC 9(02).
           03  WRK-TAIINYMD            PIC X(08).
           03  WRK-TAIINYMD-R          REDEFINES   WRK-TAIINYMD.
               05  WRK-TAIINYMD-YM     PIC X(06).
               05  WRK-TAIINYMD-DD     PIC X(02).
           03  WRK-SRYYM-N.
               05  WRK-SRYYY-N         PIC 9(04).
               05  WRK-SRYMM-N         PIC 9(02).
      *
      *    コラムリスト位置
           03  WRK-DATALIST-ROW        PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *ver.4.7
      *    レセプトプレビュー業務区分
           03  WRK-GYOUMUKBN           PIC X(01).
      *
      *    病棟別入院日数算出用
           03  WRK-BTU-NISSU-AREA.
               05  WRK-LAST-RRKNUM     PIC 9(03).
               05  WRK-NYUIN-BTUNUM    PIC X(02)   OCCURS  6.
               05  WRK-NYUIN-BTUTANNAME
                                       PIC X(10)   OCCURS  6.
               05  WRK-NYUIN-BTUDAY    PIC 9(04)   OCCURS  6.
               05  WRK-LAST-RECNO      PIC 9(01).
               05  WRK-NYUIN-BTUDAY-Z  PIC ZZZ9.
      *
      *    入院料重複チェック
           03  WRK-NYUDAY-TBL.
               05  WRK-NYUDAY-OCC      OCCURS   31.
                   07  WRK-NYUDAY      PIC 9(01).
      *
      *    最終入院病室表示用
           03  WRK-BRMNUM              PIC X(06).
           03  WRK-Z6-X.
               05  WRK-Z6              PIC ZZZZZZ.
           03  WRK-BRMNUM-9            PIC 9(06).
      *
      *    プレビュー用
           03  WRK-HKNCOMBI-CHK        PIC X(04).
           03  WRK-RSI-CNT             PIC 9(03).
           03  WRK-JIB-CNT             PIC 9(03).
           03  WRK-KOG-CNT             PIC 9(03).
      *
      *    出生児体重
           03  WRK-WEIGHT              PIC 9(04).
      *
      *    入院科表示用
           03  WRK-NYUINKA-AREA.
               05  WRK-NYUINKA1        PIC X(02).
               05  WRK-NYUINKAMEI1     PIC X(08).
               05  WRK-NYUINKA2        PIC X(02).
               05  WRK-NYUINKAMEI2     PIC X(08).
               05  WRK-NYUINKA3        PIC X(02).
               05  WRK-NYUINKAMEI3     PIC X(08).
               05  WRK-NYUINKA-ETC     PIC X(01).
               05  WRK-NYUINKAMEI      PIC X(08).
      *
      *    ９１日超表示用
           03  WRK-STUSAN-RRK-CNT              PIC 9(05).
           03  WRK-STUSAN-RRK-OCC              OCCURS  50.
               05  WRK-STUSAN-RRK-JTIKBN       PIC X(01).
               05  WRK-STUSAN-RRK-TAINRELKBN   PIC X(01).
               05  WRK-STUSAN-RRK-NYUINYMD     PIC X(08).
               05  WRK-STUSAN-RRK-TAIINYMD     PIC X(08).
               05  WRK-STUSAN-RRK-DJKBN        PIC X(01).
      *        一般入院料で入院していた日数
      *        （歴作成データの場合は対象日数）
               05  WRK-STUSAN-RRK-NISSU        PIC 9(08).
      *
      *    回数一括変更用
       01  WRK-DAY-AREA.
           03  WRK-DAY-TBL.
               05  WRK-DAY-OCC         OCCURS   31.
                   07  WRK-DAY         PIC 9(03).
                   07  WRK-DAYDEL      PIC 9(01).
           03  WRK-KOMENT-TBL.
               05  WRK-KOMENT-OCC          OCCURS   60.
                   07  WRK-KAISUT          PIC X(60).
                   07  WRK-DAYG            OCCURS   30.
                       09  WRK-DAY1            PIC X(60).
                       09  WRK-DAY2            PIC X(60).
           03  WRK-KAISU-9              PIC 9(03).
           03  WRK-DAYG-9               OCCURS   30.
               05  WRK-DAY1-9               PIC 9(02).
               05  WRK-DAY2-9               PIC 9(02).
      *
           03  WRK-IKKATUDAY            PIC 9(02).
           03  WRK-IKKATUKAI            PIC 9(03).
      *
           03  WRK-HZNSU              PIC X(60).
           03  WRK-GRP-HKNCOMBI-G.
               05  WRK-GRP-HKNCOMBI   PIC 9(04)
                                      OCCURS   31.
           03  WRK-HKNNUM             PIC X(03).
           03  WRK-PAYKBN             PIC X(02).
      *
      *    診療行為テーブル
       01  TBL-SRYCD-HEN-AREA.
           03  TBL-SRYCD-HEN-G.
               05  TBL-SRYCD-HEN-OCC   OCCURS      100.
                   07  TBL-SRYCD           PIC X(09).
                   07  TBL-HKNCOMBI        PIC X(04).
                   07  TBL-SRYKBN          PIC X(02).
                   07  TBL-SRYSYUKBN       PIC X(03).
                   07  TBL-DAY-G.
                       09  TBL-DAY         PIC 9(03)   OCCURS   31.
      *    施設規準テーブル
       01  WRK-TBL-SSTKIJUN-AREA.
           03  WRK-TBL-SSTKIJUN-OCC       OCCURS  3500.
               05  WRK-TBL-SSTKIJUN       PIC 9(01).
      *
      *    患者年齢算出用エリア
       01  WRK-NENREI-AREA.
           03  WRK-NENREI          PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
      *
      *    年齢表示
           03  WRK-NENREI-G.
               05  WRK-NENREI-Z    PIC ZZ9.
               05  WRK-NENREI-MEI  PIC X(04).
           03  WRK-NENREI-GR       REDEFINES   WRK-NENREI-G.
               05  WRK-NENREI-ZN   PIC Z9.
               05  WRK-NENREI-MEIN PIC X(04).
               05  FILLER          PIC X(01).
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
      *    表示明細変更作業
       01  WRK-GMN-DSP-TBL.
           03  WRK-GMN-DAY-G.
               05  WRK-GMN-DAY     PIC 9(03)  OCCURS  31.
           03  WRK-CHK-TBL.
               05  WRK-CHK-DAY     PIC 9(03)  OCCURS  31.
           03  WRK-CHK-ZAISKBKBN   PIC X(01).
           03  WRK-SKB-TBL.
               05  WRK-KUMINO      PIC X(02)  OCCURS  30.
               05  WRK-KUMISTYMD   PIC X(08)  OCCURS  30.
               05  WRK-KUMIEDYMD   PIC X(08)  OCCURS  30.
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"           REPLACING
                                //SPA-// BY //WRK-SPA-//.
      *    入院履歴退避用ワーク
       01  WK-PTNYU-REC.
           COPY    "CPPTNYUINRRK.INC"     REPLACING
                                //PTNYUINRRK-// BY //WK-PTNYU-//.
      *    入院履歴退避用ワーク
       01  WWK-PTNYU-REC.
           COPY    "CPPTNYUINRRK.INC"     REPLACING
                                //PTNYUINRRK-// BY //WWK-PTNYU-//.
      *    特定入院期間算定用ワーク
       01  WRK-SCNYUACCT-AREA.
           03  WRK-SCNYUACCT-KAGENCD-TBL         OCCURS  7.
               05  WRK-SCNYUACCT-KAGEN-SRYCD     PIC  X(09).
               05  WRK-SCNYUACCT-KAGEN-STYUKYMD  PIC  X(08).
               05  WRK-SCNYUACCT-KAGEN-EDYUKYMD  PIC  X(08).
      *
       01  CONST-AREA.
           03  CONST-SATURDAY                PIC  X(01)    VALUE "6".
           03  CONST-SUNDAY                  PIC  X(01)    VALUE "7".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK5001.INC".
      *
           COPY    "CPSK5002.INC".
      *
           COPY    "CPSK5113.INC".
      *ver.4.7
           COPY    "CPSK1043.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院行為マスタ−
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者個別設定
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    初期加算算定サブ
           COPY    "CPORCSSYOKIKSN.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSNYUACCT.INC".
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *    通算日数チェックサブ
           COPY "CPORCSTUSANCHK.INC".
      *
      *    排他制御パラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    入院基本点数チェック
           COPY    "CPORCSS007.INC".
      *
      *    入院行為照会用テーブル作成
           COPY  "CPORCSS010.INC".
      *
      *    特定入院料コード取得サブ
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    医療観察入院料コード取得パラメタ
           COPY  "CPORCSIRYOKNS.INC".
      *
      *    亜急性期入院医療管理料判定パラメタ
           COPY  "CPORCSAKYUSEI.INC".
      *
      *    特定入院料算定期間取得パラメタ
           COPY  "CPORCSTOKNYUIN2.INC".
      *
      *    入院料加算取得パラメタ
           COPY  "CPORCSNYUKSNGET.INC".
      *
      *    夜勤時間超過減算入院料コード取得サブ
           COPY "CPORCSYAKINGEN.INC".
      *
      *    夜勤時間特別入院基本料コード取得サブ
           COPY "CPORCSYAKINTOKU.INC".
      *
      *    看護職員数等入院基本料コード取得サブ
           COPY "CPORCSKANGOCHG.INC".
      *
      *    入院時生活療養費負担額取得サブ
           COPY    "CPORCSNYULIFE2.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
       01  WRK-PREV-AREA.
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPTX".
      *
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I41.
           MOVE    SPA-WORK        TO  SPA-I4KYOUTU.
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO    "CLICKED"
                   MOVE    SPACE           TO  SPA-ERRCD
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   MOVE    SPACE           TO  SPA-ERRCD
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I4KYOUTU    TO  SPA-WORK.
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-I41         TO  SPA-FREE
           .
      *
           EXIT    PROGRAM.
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
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
      *????    MOVE   SPACE                TO  LNK-KYOUTU
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "I41"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
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
      *    他画面より
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *        元画面表示
               IF      WRK-MOTOPG          =   "Y01"  OR  "U01"
                                                      OR  "P97"
                                                      OR  "C50"
                                                      OR  "XC01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-I41
                   MOVE    SPA-WORK        TO  SPA-I4KYOUTU
               END-IF
      *        予約、受付の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        コメントの時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "C50"
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        プレビューから
               IF      WRK-MOTOPG          =   "XC01"
                   MOVE    SPACE               TO  LINKAREA
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
      *
           EVALUATE    SPA-MOTOPG(1:5)
      ****     レセコメントより
      *        WHEN    "I43"
      ****         GO      TO      300-SCREEN-EXT
               WHEN    "I4ID1"
      *            確認画面より
                   PERFORM 3003-JID1-SET-SEC
                   IF     (SPA-ERRCD       NOT =   SPACE ) OR
                          (SPA-IIDCD       NOT =   SPACE )
                       PERFORM 500-GMNHEN-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                       END-IF
                       IF      SPA-IIDCD       NOT =   SPACE
                           PERFORM 520-JIDSET-SEC
                       END-IF
                   END-IF
                   GO      TO      300-SCREEN-EXT
               WHEN    "I411"
      *            プレビュー保険一覧より
                   IF      SPA-I411-SELNUM     NOT =   SPACE
      *                プレビュー処理
      ****             PERFORM 4130-PREVIEW-SEC
      *               第三者行為あり
                      IF     (SPA-XC01-HKNKBN     =   "0"  )
                        AND  (SPA-XC01-DAISANFLG  =   "1"  )
                       MOVE    "0120"          TO  SPA-IIDCD
                       PERFORM 520-JIDSET-SEC
                     ELSE
      *                プレビュー処理
                       MOVE    SPACE           TO  SPA-I4ID-FLG
                       PERFORM 4130-PREVIEW-SEC
                     END-IF
                   END-IF
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-IIDCD.
           INITIALIZE                  SPA-I4-AREA.
           INITIALIZE                  SPA-SCR-REC-41.
      *
           EVALUATE    SPA-MOTOPG(1:4)
      *        業務選択より
               WHEN    "M01"
                   INITIALIZE                 SPA-I4KYOUTU
                   MOVE    SPA-MOTOPG         TO  SPA-MOTOPG2
                   MOVE    SPA-MOTOPG         TO  SPA-I40-MOTOPG
                   MOVE    "0"                TO  SPA-I40-MOTOFLG
      *            初期表示
                   MOVE    SPA-SYSYMDWH        TO  SPA-I40-SRYYMDW
                   MOVE    SPA-SYSYMD          TO  SPA-I40-SRYYMD
                   PERFORM 3001-INIT-HENSYU-SEC
      *
                   MOVE    ZERO                TO  SPA-GMN-CUR-41
      *
      *        入院登録より
               WHEN    "I01"
               WHEN    "K02N"
                   INITIALIZE                 SPA-I4KYOUTU
                   MOVE    SPA-MOTOPG         TO  SPA-MOTOPG2
                   MOVE    SPA-MOTOPG         TO  SPA-I40-MOTOPG
                   MOVE    SPACE              TO  SPA-I40-MOTOFLG
      *            初期表示
                   MOVE    SPA-SYSYMDWH        TO  SPA-I40-SRYYMDW
                   MOVE    SPA-SYSYMD          TO  SPA-I40-SRYYMD
                   PERFORM 3001-INIT-HENSYU-SEC
                   IF      SPA-PTID            =   ZERO
                       MOVE    ZERO                TO  SPA-GMN-CUR-41
      *                患者番号なし
                       MOVE    "1"                 TO  SPA-I40-MOTOFLG
                   ELSE
                       MOVE    SPA-PTNUM           TO  I41-PTNUM
                       MOVE    SPA-PTID            TO  SPA-GMN-PTID
                       PERFORM 41010-PTNUM-SYORI-SEC
                       GO      TO      300-SCREEN-EXT
                   END-IF
      *
               WHEN     OTHER
                   MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM
                   MOVE    "1"                  TO  SPA-NYUGAIKBN
      *
                   MOVE    1                    TO  FLG-HENSYU
                   PERFORM 310-SPA-SET-SEC
      *            初期表示
      *            患者入院履歴編集
                   PERFORM 3103-PTNYUINRRK-HEN-SEC
      *            入院会計マスターより編集をする
                   PERFORM 3101-NYUINACCT-HEN-SEC
      *            保険組合一覧編集
                   PERFORM 3105-HKNCOMBI-HEN-SEC
      *            保険組合期限切れチェック、外泊の再編集
                   PERFORM 3106-DAY-KIKANCHK-SEC
      *
                   MOVE    1                    TO  SPA-GMN-CUR-41
           END-EVALUATE.
      *
           MOVE    1                   TO  SPA-GMN-PAGE-41.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     初期表示処理
      *****************************************************************
       3001-INIT-HENSYU-SEC                  SECTION.
      *
           INITIALIZE                  SPA-I41
           MOVE    SPA-I40-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
           MOVE    SPA-I40-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    曜日編集処理
           PERFORM 3006-YOBI-HEN-SEC
      *
      *    室料差額テーブルセット
           PERFORM 3113-SAGAKU-HEN-SEC
      *
      *    入院基本テーブルセット
           PERFORM 3500-KIHON-HEN-SEC
      *
           .
       3001-INIT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0102"
                   MOVE    SPACE           TO  SPA-IIDCD
      *            戻るの確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       MOVE    1                  TO  SPA-GMN-CUR-41
                       MOVE    SPACE              TO  SPA-UPDFLG-41
                       PERFORM 210-BACK-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR-41
                   END-IF
      *
               WHEN    "0103"
                   MOVE    SPACE           TO  SPA-IIDCD
      *            明細選択の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       MOVE    1                  TO  SPA-GMN-CUR-41
                       MOVE    SPA-NAI-NUM-41     TO  I41-NUM
                       PERFORM 41011-BANGO-CHK-SEC
                       MOVE    SPACE              TO  SPA-GMN-KOMENTO-41
                   ELSE
                       MOVE    3                  TO  SPA-GMN-CUR-41
                       MOVE    SPA-MAE-BANGO-41   TO  SPA-NAI-NUM-41
                   END-IF
      *
               WHEN    "0104"
                   MOVE    SPACE           TO  SPA-IIDCD
      *            剤変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       MOVE     1             TO  SPA-GMN-CUR-41
                       MOVE     1             TO  SPA-I4-SYORI
                       PERFORM 420-ZAIHENKOU-SYORI-SEC
                   ELSE
                       MOVE     1             TO  SPA-GMN-CUR-41
                   END-IF
      *
               WHEN    "0106"
                   MOVE    SPACE           TO  SPA-IIDCD
      *            診療年月変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       IF      SPA-I4-SEL         =   1
      *                    前月処理へ
                           PERFORM 41018-ZENGETU-SYORI-SEC
                       ELSE
      *                    次月処理へ
                           PERFORM 41019-JIGETU-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR-41
                   END-IF
      *
               WHEN    "0107"
                   MOVE    SPACE           TO  SPA-IIDCD
      *            入院科変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 41013-NYUINKA-CHANGE-SEC
                   END-IF
                   MOVE    1                  TO  SPA-GMN-CUR-41
      *
               WHEN    "0108"
                   MOVE    SPACE           TO  SPA-IIDCD
      *            診療年月変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 410181-SRYYM-CHANGE-SEC
                       MOVE    1                    TO  SPA-GMN-CUR-41
                   ELSE
      *                MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM
      *                MOVE    SPA-I40-SRYYMD(1:6)  TO  SPA-NAI-SRYYM
      *H29.3
                       MOVE    SPA-MAE-GMN-SRYYM    TO  SPA-GMN-SRYYM
                       MOVE    SPA-MAE-NAI-SRYYM    TO  SPA-NAI-SRYYM
                       MOVE    1                    TO  SPA-GMN-CUR-41
                   END-IF
      *
               WHEN    "0110"
                   MOVE    SPACE           TO  SPA-IIDCD
      *            最新入院履歴の変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 4701-KAKUTEI-SYORI-SEC
                   END-IF
      *
               WHEN    "0120"
      *            第三者行為あり
                   MOVE    SPACE           TO  SPA-IIDCD
      *            プレビュー処理
                   PERFORM 4130-PREVIEW-SEC
      *
           END-EVALUATE.
      *
       3003-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU.
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  I41-PTNUM
                   PERFORM 41010-PTNUM-SYORI-SEC
               END-IF
           END-IF.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR-41
      **** ELSE
      ******** MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    曜日編集処理
      *****************************************************************
       3006-YOBI-HEN-SEC               SECTION.
      *
           INITIALIZE                  SPA-NAI-YOBI-G
      *
      *    曜日編集
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY3-AREA
               MOVE    "31"            TO  LNK-DAY3-IRAI
               MOVE    SPA-NAI-SRYYM   TO  WRK-SRYYMD
               MOVE    IDXD            TO  WRK-SRYDD
               MOVE    WRK-SRYYMD      TO  LNK-DAY3-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
               IF    ( STS-DAY-RC1         =   ZERO )
                   MOVE    LNK-DAY3-YOBI
                                       TO  SPA-NAI-YOBI (IDXD)
               END-IF
      *
           END-PERFORM
      *
           .
       3006-YOBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額初期編集
      *****************************************************************
       3113-SAGAKU-HEN-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-GMN-B05-FLG-41
      *    Ｂ０５ボタンラベル
           MOVE    "食事"              TO  SPA-GMN-B05-LABEL-41
      *
           INITIALIZE                  SPA-GMN-SAGAKUTBL-41.
      *
           MOVE    "室料差額"
                                       TO  SPA-GMN-SAGAKU-TITLE-41
      *
      *    室料差額
           MOVE    SPACE               TO  SYS-5113-REC.
           INITIALIZE                      SYS-5113-REC
           MOVE    "5113"              TO  SYS-5113-KANRICD.
           MOVE    SPA-NAI-SRYYM       TO  SYS-5113-STYUKYMD(1:6).
           MOVE    "01"                TO  SYS-5113-STYUKYMD(7:2).
           MOVE    SYS-5113-STYUKYMD   TO  SYS-5113-EDYUKYMD.
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-5113-HOSPNUM
           MOVE    SYS-5113-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-5113-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL       (IDX            >=  99)  OR
                               (FLG-SYSKANRI   =   1)
               MOVE    MCPDATA-REC             TO  SYS-5113-REC
               ADD     1                       TO  IDX
               MOVE    SYS-5113-KBNCD(1:4)     TO
                                               SPA-GMN-SAGAKUNO-41 (IDX)
               MOVE    SYS-5113-BRM-SAGAKU-NM  TO  WRK-SAGAKU-Z1
               MOVE    "円"                    TO  WRK-SAGAKU-X1
               MOVE    WRK-SAGAKU              TO
                                               SPA-GMN-SAGAKU-41 (IDX)
               MOVE    IDX                     TO  SPA-SAGAKU-MAX-41
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3113-SAGAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事初期編集
      *****************************************************************
       3113-SHOKUJI-HEN-SEC               SECTION.
      *
           MOVE    2                   TO  SPA-GMN-B05-FLG-41
      *    Ｂ０５ボタンラベル
           MOVE    "外泊"          TO  SPA-GMN-B05-LABEL-41
      *
           INITIALIZE                  SPA-GMN-SAGAKUTBL-41.
      *
           MOVE    "食事"
                                       TO  SPA-GMN-SAGAKU-TITLE-41
      *
           MOVE    SPACE               TO  SPA-GMN-SAGAKUNO-41 (1)
           MOVE    "食事なし"          TO  SPA-GMN-SAGAKU-41   (1)
      *
           MOVE    "01"                TO  SPA-GMN-SAGAKUNO-41 (2)
           MOVE    "食事療養のみ"      TO  SPA-GMN-SAGAKU-41   (2)
      *
           MOVE    "02"                TO  SPA-GMN-SAGAKUNO-41 (3)
           MOVE    "食事療養＋特食"    TO  SPA-GMN-SAGAKU-41   (3)
      *
           IF      SPA-NAI-SRYYM       <=   "200603"
               MOVE    "03"                TO  SPA-GMN-SAGAKUNO-41 (4)
               MOVE    "食事療養＋選択食"  TO  SPA-GMN-SAGAKU-41   (4)
      *
               MOVE    "04"                TO  SPA-GMN-SAGAKUNO-41 (5)
               MOVE    "食事療養＋特食＋選択食"
                                           TO  SPA-GMN-SAGAKU-41   (5)
      *
               MOVE    5                   TO  SPA-SAGAKU-MAX-41
           ELSE
             IF   SPA-NAI-SRYYM       <=   "201603"
               MOVE    SPACE               TO  SPA-GMN-SAGAKUNO-41 (4)
               MOVE    SPACE               TO  SPA-GMN-SAGAKU-41   (4)
      *
               MOVE    SPACE               TO  SPA-GMN-SAGAKUNO-41 (5)
               MOVE    SPACE               TO  SPA-GMN-SAGAKU-41   (5)
      *
               MOVE    3                   TO  SPA-SAGAKU-MAX-41
             ELSE
               MOVE    "03"                TO  SPA-GMN-SAGAKUNO-41 (4)
               MOVE    "流動食"            TO  SPA-GMN-SAGAKU-41   (4)
      *
               MOVE    SPACE               TO  SPA-GMN-SAGAKUNO-41 (5)
               MOVE    SPACE               TO  SPA-GMN-SAGAKU-41   (5)
      *
               MOVE    4                   TO  SPA-SAGAKU-MAX-41
             END-IF
           END-IF
      *
           .
       3113-SHOKUJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊初期編集
      *****************************************************************
       3113-GAIHAKU-HEN-SEC               SECTION.
      *
           MOVE    3                   TO  SPA-GMN-B05-FLG-41
      *    Ｂ０５ボタンラベル
           MOVE    "室料差額"          TO  SPA-GMN-B05-LABEL-41
      *
           INITIALIZE                  SPA-GMN-SAGAKUTBL-41.
      *
           MOVE    "外泊"
                                       TO  SPA-GMN-SAGAKU-TITLE-41
      *
           MOVE    "01"                TO  SPA-GMN-SAGAKUNO-41 (1)
           MOVE    "外泊"              TO  SPA-GMN-SAGAKU-41   (1)
      *
           MOVE    "02"                TO  SPA-GMN-SAGAKUNO-41 (2)
           MOVE    "治療の為の外泊"    TO  SPA-GMN-SAGAKU-41   (2)
      *
           MOVE    "03"                TO  SPA-GMN-SAGAKUNO-41 (3)
           MOVE    "選定入院中の外泊"  TO  SPA-GMN-SAGAKU-41   (3)
      *
           IF       SPA-NAI-SRYYM      >=  "201604"
             MOVE    "04"              TO  SPA-GMN-SAGAKUNO-41 (4)
             MOVE    "他医療機関受診４０％減"
                                       TO  SPA-GMN-SAGAKU-41   (4)
      *
             MOVE    4                   TO  SPA-SAGAKU-MAX-41
           ELSE
             MOVE    "04"              TO  SPA-GMN-SAGAKUNO-41 (4)
             MOVE    "他医療機関受診７０％減"
                                       TO  SPA-GMN-SAGAKU-41   (4)
      *
             MOVE    4                   TO  SPA-SAGAKU-MAX-41
           END-IF
      *
           IF      (SPA-NAI-SRYYM      >=  "201004")   AND
                   (SPA-NAI-SRYYM      <=  "201203")
               MOVE    "05"                TO  SPA-GMN-SAGAKUNO-41 (5)
               MOVE    "他医療機関受診３０％減"
                                       TO  SPA-GMN-SAGAKU-41   (5)
      *
               MOVE    5                   TO  SPA-SAGAKU-MAX-41
           END-IF
      *
           IF       SPA-NAI-SRYYM      >=  "201604"
               MOVE    "05"                TO  SPA-GMN-SAGAKUNO-41 (5)
               MOVE    "他医療機関受診１０％減"
                                           TO  SPA-GMN-SAGAKU-41   (5)
               MOVE    "06"                TO  SPA-GMN-SAGAKUNO-41 (6)
               MOVE    "他医療機関受診２０％減"
                                           TO  SPA-GMN-SAGAKU-41   (6)
               MOVE    "08"                TO  SPA-GMN-SAGAKUNO-41 (7)
               MOVE    "特定時間退院減算"  TO  SPA-GMN-SAGAKU-41   (7)
               MOVE    "09"                TO  SPA-GMN-SAGAKUNO-41 (8)
               MOVE    "特定曜日入退院減算"
                                           TO  SPA-GMN-SAGAKU-41   (8)
               MOVE    "13"                TO  SPA-GMN-SAGAKUNO-41 (9)
               MOVE    "特定曜日＋他受１０％減"
                                           TO  SPA-GMN-SAGAKU-41   (9)
      *
               MOVE    9                   TO  SPA-SAGAKU-MAX-41
               IF       SPA-NAI-SRYYM      >=  "201804"
                   MOVE    "14"            TO  SPA-GMN-SAGAKUNO-41 (10)
                   MOVE    "他医療機関受診５％減"
                                           TO  SPA-GMN-SAGAKU-41   (10)
                   MOVE    "15"            TO  SPA-GMN-SAGAKUNO-41 (11)
                   MOVE    "他医療機関受診３５％減"
                                           TO  SPA-GMN-SAGAKU-41   (11)
                   MOVE    "16"            TO  SPA-GMN-SAGAKUNO-41 (12)
                   MOVE    "他医療機関受診１５％減"
                                           TO  SPA-GMN-SAGAKU-41   (12)
                   MOVE    "17"            TO  SPA-GMN-SAGAKUNO-41 (13)
                   MOVE    "特定曜日＋他受５％減"
                                           TO  SPA-GMN-SAGAKU-41   (13)
      *
                   MOVE    13              TO  SPA-SAGAKU-MAX-41
               END-IF
           ELSE
             IF       SPA-NAI-SRYYM      >=  "201204"
               MOVE    "05"                TO  SPA-GMN-SAGAKUNO-41 (5)
               MOVE    "他医療機関受診３０％減"
                                       TO  SPA-GMN-SAGAKU-41   (5)
               MOVE    "06"                TO  SPA-GMN-SAGAKUNO-41 (6)
               MOVE    "他医療機関受診５５％減"
                                       TO  SPA-GMN-SAGAKU-41   (6)
               MOVE    "07"                TO  SPA-GMN-SAGAKUNO-41 (7)
               MOVE    "他医療機関受診１５％減"
                                       TO  SPA-GMN-SAGAKU-41   (7)
      *
               MOVE    7                   TO  SPA-SAGAKU-MAX-41
             END-IF
      *
             IF       SPA-NAI-SRYYM      >=  "201210"
               MOVE    "08"                TO  SPA-GMN-SAGAKUNO-41 (8)
               MOVE    "特定時間退院減算"
                                       TO  SPA-GMN-SAGAKU-41   (8)
               MOVE    "09"                TO  SPA-GMN-SAGAKUNO-41 (9)
               MOVE    "特定曜日入退院減算"
                                       TO  SPA-GMN-SAGAKU-41   (9)
               MOVE    "13"                TO  SPA-GMN-SAGAKUNO-41 (10)
               MOVE    "特定曜日＋他受３０％減"
                                       TO  SPA-GMN-SAGAKU-41   (10)
      *
               MOVE    10                  TO  SPA-SAGAKU-MAX-41
             END-IF
           END-IF
      *
           .
       3113-GAIHAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院基本情報初期編集
      *****************************************************************
       3500-KIHON-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-5000-GENSAN-INF-TBL-41.
      *    入院基本情報
           MOVE    SPACE               TO  SYS-5000-REC.
           MOVE    "5000"              TO  SYS-5000-KANRICD.
           MOVE    "*"                 TO  SYS-5000-KBNCD.
           MOVE    SPA-NAI-SRYYM       TO  SYS-5000-STYUKYMD(1:6).
           MOVE    "01"                TO  SYS-5000-STYUKYMD(7:2).
           MOVE    SYS-5000-STYUKYMD   TO  SYS-5000-EDYUKYMD.
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
           MOVE    SYS-5000-REC        TO  MCPDATA-REC.
           PERFORM 900-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-5000-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX             >   10
                   MOVE    SYS-5000-DSPCD (IDX)
                                           TO  SPA-5000-DSPCD-41 (IDX)
               END-PERFORM
      *        入院会計外泊区分
               MOVE    SYS-5000-NACCT-GAIHAKUKBN
                                           TO  SPA-5000-NACCT-GAIHAKUKBN
      *        入院時食事療養区分
               EVALUATE   SYS-5000-SHOKUJIRYOYOCD
                   WHEN   "197000110"
                       MOVE   "1"          TO  SPA-5000-NACCT-SKJKBN
                   WHEN   "197000710"
                       MOVE   "2"          TO  SPA-5000-NACCT-SKJKBN
                   WHEN   OTHER
                       MOVE   SPACE        TO  SPA-5000-NACCT-SKJKBN
               END-EVALUATE
      *
               MOVE    SYS-5000-ADLINPUTKBN TO SPA-5000-ADLINPUTKBN
      *
               MOVE    SYS-5000-RYOYODSPKBN TO SPA-5000-RYOYODSPKBN
      *
           END-IF.
      *
       3500-KIHON-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
      *    入院基本テーブルセット
           PERFORM 3500-KIHON-HEN-SEC
      *    室料差額テーブルセット
           PERFORM 3113-SAGAKU-HEN-SEC
      *
           PERFORM 3006-YOBI-HEN-SEC
      *
      *    診療会計マスターより編集をする
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY.
           INITIALIZE                      LNK-DAY7-AREA.
           MOVE    "71"                TO  LNK-DAY7-IRAI.
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY7-YM.
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA.
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-NAI-SRYMATU.
      *
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAY
      *
      *    年齢計算（算定月の１日の年齢）
           MOVE    SPA-BIRTHDAY        TO  WRK-YMDS1.
           MOVE    SPA-NAI-SRYYM       TO  WRK-YMDS2(1:6).
           MOVE    01                  TO  WRK-DDS2.
           COMPUTE SPA-NAI-NENREI-YY   =   WRK-YYS2
                                       -   WRK-YYS1.
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE SPA-NAI-NENREI-YY   =   SPA-NAI-NENREI-YY
                                           -   1
               COMPUTE SPA-NAI-NENREI-MM   =  (WRK-MMS2  +  12)
                                           -   WRK-MMS1
           ELSE
               COMPUTE SPA-NAI-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF.
      *    該当月で６歳になる時、年齢フラグを立てる
           IF     (SPA-NAI-NENREI-YY   =   05)  AND
                  (SPA-NAI-NENREI-MM   =   11)  AND
                  (WRK-YMDS1(5:2)      =   WRK-YMDS2(5:2))
               MOVE    1                   TO  SPA-FLG-NENREI
           END-IF.
      *
      *    乳幼児チェック
           IF     (SPA-NAI-NENREI-YY       =   ZERO)  AND
                  (SPA-NAI-NENREI-MM       =   ZERO  OR  1)
      *        年齢期間算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY5-AREA
               MOVE    "51"                TO  LNK-DAY5-IRAI
               MOVE    SPA-BIRTHDAY        TO  LNK-DAY5-START
               MOVE    SPA-NAI-SRYYM       TO  LNK-DAY5-END(1:6)
               MOVE    SPA-NAI-SRYMATU     TO  LNK-DAY5-END(7:2)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY5-AREA
               IF      LNK-DAY5-NISSU2     >   28
                   MOVE    1                   TO  SPA-FLG-NENREI
                   IF      LNK-DAY5-NISSU2     <=  31
                       MOVE    LNK-DAY5-NISSU2 TO  SPA-NAI-NENREI-DD
                       MOVE    ZERO            TO  SPA-NAI-NENREI-MM
                   END-IF
               END-IF
           END-IF.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX-41.
           .
      *
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   患者入院履歴編集処理
      *****************************************************************
       3103-PTNYUINRRK-HEN-SEC          SECTION.
      *
      *    患者入院履歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  PTNYUINRRK-REC.
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM.
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC.
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
           IF      FLG-PTNYUINRRK      =   ZERO
               IF      PTNYUINRRK-NYUINYMD NOT =   SPACE
                   INITIALIZE                  ORCSTUSANAREA
                   MOVE    SPA-HOSPNUM     TO  STUSAN-HOSPNUM
                   MOVE    PTNYUINRRK-PTID TO  STUSAN-PTID
                   MOVE    PTNYUINRRK-NYUINYMD
                                           TO  STUSAN-KJNYMD
                   CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                                   SPA-AREA
      *
      *            他院（特別な関係にある医療機関）の日数を加算
                   ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU03
                   ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU05
      *
      *            ９１日編集処理
                   PERFORM 3103-91DAYS-EDIT-SEC
      *           １８０日編集処理
                   PERFORM 3103-180DAYS-EDIT-SEC
      *
                   MOVE    PTNYUINRRK-NYUINYMD TO  SPA-NAI-NYUINBI
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
                   PERFORM 41012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUINBI
               END-IF
               MOVE    PTNYUINRRK-TAIINYMD TO  SPA-NAI-TAINBI
               IF      PTNYUINRRK-TAIINYMD NOT =   SPACE
                   IF      PTNYUINRRK-TAIINYMD     =   "99999999"
                       MOVE    SPACE               TO  SPA-GMN-TAINBI
                   ELSE
                       MOVE    PTNYUINRRK-TAIINYMD TO WRK-SYMD
                       PERFORM 41012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO SPA-GMN-TAINBI
                   END-IF
               END-IF
           ELSE
      *        入院なし
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      3103-PTNYUINRRK-HEN-EXT
           END-IF.
      *
      *    入院日数の表示
           INITIALIZE                  PTNYUINRRK-REC.
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM.
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID.
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-NYUINYMD.
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-TAIINYMD.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC.
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
           IF      FLG-PTNYUINRRK      =   ZERO
               INITIALIZE                  ORCSTUSANAREA
               MOVE    SPA-HOSPNUM     TO  STUSAN-HOSPNUM
               MOVE    PTNYUINRRK-PTID TO  STUSAN-PTID
               MOVE    SPA-SYSYMD      TO  STUSAN-KJNYMD
               CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                               SPA-AREA
               MOVE    STUSAN-NISSU01  TO  SPA-GMN-NYUINNISSU
      *        自院再入院時に日数をプラス１する
               IF      PTNYUINRRK-DOUJITSUKBN    =   "1"
                   ADD    1            TO  SPA-GMN-NYUINNISSU
               END-IF
      *
               MOVE    STUSAN-NISSU03  TO  SPA-GMN-TSUSANNISSU
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NYUINNISSU
               MOVE    ZERO            TO  SPA-GMN-TSUSANNISSU
           END-IF.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
      *    病棟別日数編集
           PERFORM      3103-BTUDAY-EDIT-SEC
      *
           .
      *
       3103-PTNYUINRRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   ９１日編集処理
      *****************************************************************
       3103-91DAYS-EDIT-SEC            SECTION.
      *
      **     DISPLAY "3103-91DAYS-EDIT-SEC"
           MOVE  "black"    TO   SPA-GMN-IJYOU91-STYLE
           IF    ( STUSAN-RRK-FLG-OVER NOT =   ZERO )
               MOVE    "********"  TO  SPA-GMN-IJYOU91
               GO  TO  3103-91DAYS-EDIT-EXT
           END-IF
      *
      **     DISPLAY "STUSAN-KJNYMD = " STUSAN-KJNYMD
           MOVE    STUSAN-NISSU03  TO  WRK-TUSAN
      **     DISPLAY "STUSAN-NISSU03 = " STUSAN-NISSU03
      **     DISPLAY "STUSAN-RRK-CNT = " STUSAN-RRK-CNT
      *
           MOVE    STUSAN-RRK-CNT  TO  WRK-STUSAN-RRK-CNT
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   STUSAN-RRK-CNT )
               MOVE  STUSAN-RRK-OCC(IDX10)  TO WRK-STUSAN-RRK-OCC(IDX10)
           END-PERFORM
      *
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   WRK-STUSAN-RRK-CNT )
      **         DISPLAY "IDX10 = " IDX10
      **         DISPLAY "WRK-STUSAN-RRK-JTIKBN (IDX10) = "
      **                  WRK-STUSAN-RRK-JTIKBN (IDX10)
      **         DISPLAY "WRK-STUSAN-RRK-TAINRELKBN (IDX10) = "
      **                  WRK-STUSAN-RRK-TAINRELKBN (IDX10)
      **         DISPLAY "WRK-STUSAN-RRK-NYUINYMD (IDX10) = "
      **                  WRK-STUSAN-RRK-NYUINYMD (IDX10)
      **         DISPLAY "WRK-STUSAN-RRK-TAIINYMD (IDX10) = "
      **                  WRK-STUSAN-RRK-TAIINYMD (IDX10)
      **         DISPLAY "WRK-STUSAN-RRK-DJKBN (IDX10) = "
      **                  WRK-STUSAN-RRK-DJKBN (IDX10)
      **         DISPLAY "WRK-STUSAN-RRK-NISSU (IDX10) = "
      **                  WRK-STUSAN-RRK-NISSU (IDX10)
               IF    ( WRK-STUSAN-RRK-JTIKBN (IDX10)
                                           NOT = "5" )
                   IF (WRK-STUSAN-RRK-NYUINYMD (IDX10)  NOT =  SPACE)
                        INITIALIZE                  ORCSTUSANAREA
                        MOVE    SPA-HOSPNUM     TO  STUSAN-HOSPNUM
                        MOVE    SPA-PTID        TO  STUSAN-PTID
                        MOVE    WRK-STUSAN-RRK-NYUINYMD (IDX10)
                                                TO  STUSAN-KJNYMD
                        CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                                        SPA-AREA
      **                  DISPLAY "STUSAN-RC = " STUSAN-RC
      **                  DISPLAY "STUSAN-NISSU03 = " STUSAN-NISSU03
                   END-IF
      *
                   IF    ( STUSAN-NISSU03  <    91 )
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    WRK-STUSAN-RRK-NYUINYMD (IDX10)
                                               TO  LNK-DAY6-KIJUN
                       MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                       IF    ( STUSAN-RRK-DJKBN (IDX10) = SPACE OR "2" )
      **                     DISPLAY "92 - STUSAN-NISSU03"
                           COMPUTE LNK-DAY6-ZOGEN
                               =   92   -          STUSAN-NISSU03
                       ELSE
      **                     DISPLAY "92 - STUSAN-NISSU03"
                           COMPUTE LNK-DAY6-ZOGEN
                               =   92   -          STUSAN-NISSU03
                       END-IF
      **                 DISPLAY "LNK-DAY6-KIJUN = " LNK-DAY6-KIJUN
      **                 DISPLAY "LNK-DAY6-ZOGEN = " LNK-DAY6-ZOGEN
      *AZU-ADD START
                       IF     STUSAN-NISSU03   <=    ZERO
                            MOVE   91     TO    LNK-DAY6-ZOGEN
                       END-IF
      *AZU-ADD END
      *
      *AZU-ADD START
                       IF (STUSAN-RRK-CNT     =   1 )   AND
                          (WRK-STUSAN-RRK-TAIINYMD (IDX10) = "99999999")
                            MOVE   91     TO    LNK-DAY6-ZOGEN
                       END-IF
      *AZU-ADD END
                       CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KOYOMI TO  SPA-GMN-IJYOU91
                       MOVE    LNK-DAY6-KOYOMI TO  WRK-SYMD
                       IF      SPA-SYSYMD   >=   SPA-GMN-IJYOU91
                           MOVE  "red"      TO   SPA-GMN-IJYOU91-STYLE
                       ELSE
                           MOVE  "black"    TO   SPA-GMN-IJYOU91-STYLE
                       END-IF
                       PERFORM 41012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-IJYOU91
                       MOVE    WRK-STUSAN-RRK-CNT  TO  IDX10
                   END-IF
               END-IF
           END-PERFORM
      *    通算日数チェックサブ呼出
           INITIALIZE                      ORCSTUSANCHKAREA
grpsys     MOVE    SPA-HOSPNUM         TO  STUSANCHK-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  STUSANCHK-PTID
           MOVE    PTNYUINRRK-NYUINYMD TO  STUSANCHK-NYUINYMD
           IF    ( PTNYUINRRK-TAIINYMD >=  SPA-SYSYMD )
               MOVE    SPA-SYSYMD      TO  STUSANCHK-KJNYMD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                       TO  STUSANCHK-KJNYMD
           END-IF
      *
           MOVE    PTNYUINRRK-RRKNUM   TO  STUSANCHK-RRKNUM
      *
           CALL    "ORCSTUSANCHK"          USING
                                           ORCSTUSANCHKAREA
grpsys                                     SPA-AREA
           IF    ( STUSANCHK-DAY-91   NOT =   SPACE )
      **         DISPLAY "STUSANCHK-DAY-91 = " STUSANCHK-DAY-91
               MOVE    STUSANCHK-DAY-91 TO  SPA-GMN-IJYOU91
               MOVE    STUSANCHK-DAY-91 TO  WRK-SYMD
               IF      SPA-SYSYMD   >=   SPA-GMN-IJYOU91
                  MOVE  "red"      TO   SPA-GMN-IJYOU91-STYLE
               ELSE
                  MOVE  "black"    TO   SPA-GMN-IJYOU91-STYLE
               END-IF
               PERFORM 41012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-IJYOU91
           END-IF
      *
      **     DISPLAY "3103-91DAYS-EDIT-EXT"
      *
           .
       3103-91DAYS-EDIT-EXT.
           EXIT.
      *****************************************************************
      *   １８０日編集処理
      *****************************************************************
       3103-180DAYS-EDIT-SEC           SECTION.
      *
      **     DISPLAY "3103-180DAYS-EDIT-SEC"
           IF    ( STUSAN-RRK-FLG-OVER NOT =   ZERO )
               MOVE    "********"  TO  SPA-GMN-IJYOU180
               GO  TO  3103-180DAYS-EDIT-EXT
           END-IF
      *
      **     MOVE    STUSAN-NISSU04  TO  WRK-TUSAN
      **     DISPLAY "STUSAN-NISSU01 = " STUSAN-NISSU01
      **     DISPLAY "STUSAN-NISSU04 = " STUSAN-NISSU04
           MOVE    STUSAN-NISSU02  TO  WRK-TUSAN
           COMPUTE   WRK-TUSAN  =  WRK-TUSAN -   1
      **     DISPLAY "STUSAN-NISSU02 = " STUSAN-NISSU02
      **     DISPLAY "WRK-TUSAN = " WRK-TUSAN
      *
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   STUSAN-RRK-CNT )
               IF    WRK-TUSAN     NOT =    ZERO
      **            DISPLAY "STUSAN-RRK-NISSU (IDX10) = "
      **                     STUSAN-RRK-NISSU (IDX10)
      *AZU-ADD START(2018/01/17)
                  IF   STUSAN-RRK-NISSU (IDX10) =  ZERO
                      MOVE  STUSAN-NISSU01 TO  STUSAN-RRK-NISSU (IDX10)
                  END-IF
      *AZU-ADD END(2018/01/17)
                  COMPUTE WRK-TUSAN
                       =   WRK-TUSAN   -   STUSAN-RRK-NISSU (IDX10)
               END-IF
               IF    ( WRK-TUSAN   <   180 )
                   IF    ( STUSAN-RRK-JTIKBN (IDX10)   = "5" )
                       MOVE    "********"  TO  SPA-GMN-IJYOU180
                   ELSE
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    STUSAN-RRK-NYUINYMD (IDX10)
                                               TO  LNK-DAY6-KIJUN
                       MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                       IF    ( STUSAN-RRK-DJKBN (IDX10) = SPACE OR "2")
                           COMPUTE LNK-DAY6-ZOGEN
                               =   179   -         WRK-TUSAN
                       ELSE
                           COMPUTE LNK-DAY6-ZOGEN
                               =   180   -         WRK-TUSAN
                       END-IF
      **                 DISPLAY "MAE WRK-TUSAN = " WRK-TUSAN
      *AZU-ADD START
                       IF     WRK-TUSAN   <=    ZERO
                            MOVE   180      TO    LNK-DAY6-ZOGEN
                       END-IF
      *AZU-ADD END
      **                 DISPLAY "ATO WRK-TUSAN = " WRK-TUSAN
      **                 DISPLAY "LNK-DAY6-ZOGEN = "  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KOYOMI TO  SPA-GMN-IJYOU180
                       MOVE    LNK-DAY6-KOYOMI TO  WRK-SYMD
                       PERFORM 41012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-IJYOU180
                   END-IF
                   MOVE    STUSAN-RRK-CNT      TO  IDX10
               END-IF
           END-PERFORM
      **     DISPLAY "3103-180DAYS-EDIT-EXT"
      *
      *    通算日数チェックサブ呼出
           INITIALIZE                      ORCSTUSANCHKAREA
grpsys     MOVE    SPA-HOSPNUM         TO  STUSANCHK-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  STUSANCHK-PTID
           MOVE    PTNYUINRRK-NYUINYMD TO  STUSANCHK-NYUINYMD
           IF    ( PTNYUINRRK-TAIINYMD >=  SPA-SYSYMD )
               MOVE    SPA-SYSYMD      TO  STUSANCHK-KJNYMD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                       TO  STUSANCHK-KJNYMD
           END-IF
      *
           MOVE    PTNYUINRRK-RRKNUM   TO  STUSANCHK-RRKNUM
      *
           CALL    "ORCSTUSANCHK"          USING
                                           ORCSTUSANCHKAREA
grpsys                                     SPA-AREA
           IF    ( STUSANCHK-DAY-180   NOT =   SPACE )
             IF  ( STUSANCHK-DAY-180-STS   =   ZERO OR "2"  )
      **         DISPLAY "STUSANCHK-DAY-180 = " STUSANCHK-DAY-180
               MOVE    STUSANCHK-DAY-180 TO  SPA-GMN-IJYOU180
               MOVE    STUSANCHK-DAY-180 TO  WRK-SYMD
               PERFORM 41012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-IJYOU180
             ELSE
               MOVE    "********"  TO  SPA-GMN-IJYOU180
             END-IF
           END-IF
      *
           .
       3103-180DAYS-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院科編集処理（表示月の入院科編集）
      *****************************************************************
       3103-NYUINKA-EDIT-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-NYUINKA
           MOVE    SPACE               TO  WRK-NYUINKA-AREA
      *
      *    表示月の入院科取得
      *     DISPLAY "ZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
      *     DISPLAY "SPA-NAI-SRYYM = " SPA-NAI-SRYYM
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID             TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-SRYYM        TO  PTNYUINRRK-TENSTUYMD(1:6)
           MOVE    "01"                 TO  PTNYUINRRK-TENSTUYMD(7:2)
      *    月末日を取得
           INITIALIZE                       STS-AREA-DAY
           INITIALIZE                       LNK-DAY7-AREA
           MOVE      "71"               TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"     USING      STS-AREA-DAY
                                            LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI      TO  PTNYUINRRK-TENNYUYMD
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key44"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
               MOVE    "key44"          TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
               PERFORM    UNTIL   FLG-PTNYUINRRK  =   1
      *            DISPLAY  "NYUINKA = " PTNYUINRRK-NYUINKA
                  IF   (PTNYUINRRK-NYUINKA  =  WRK-NYUINKA1)  OR
                       (PTNYUINRRK-NYUINKA  =  WRK-NYUINKA2)  OR
                       (PTNYUINRRK-NYUINKA  =  WRK-NYUINKA3)
                      CONTINUE
                  ELSE
      *               表示用診療科名をシステム管理から取得
                      MOVE    SPACE           TO  WRK-NYUINKAMEI
                      INITIALIZE                  SYSKANRI-REC
                      MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                      MOVE    "1005"          TO  SYS-KANRICD
                      MOVE    PTNYUINRRK-NYUINKA
                                              TO  SYS-KBNCD
                      MOVE    SPA-NAI-SRYYM   TO  SYS-STYUKYMD(1:6)
                      MOVE    "01"            TO  SYS-STYUKYMD(7:2)
                      MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
                      MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                      MOVE    "tbl_syskanri"  TO  MCP-TABLE
                      MOVE    "key10"         TO  MCP-PATHNAME
                      PERFORM 910-DBSELECT-SEC
                      MOVE    "tbl_syskanri"  TO  MCP-TABLE
                      MOVE    "key10"         TO  MCP-PATHNAME
                      PERFORM 900-KANRIMST-READ-SEC
                      IF      FLG-SYSKANRI    =   ZERO
                          MOVE    MCPDATA-REC TO  SYS-1005-REC
                          MOVE    SYS-1005-SRYKANAME2
                                              TO  WRK-NYUINKAMEI
                      END-IF
      *
      *                DISPLAY "WRK-NYUINKAMEI = " WRK-NYUINKAMEI
                      MOVE    "tbl_syskanri"  TO  MCP-TABLE
                      MOVE    "key10"         TO  MCP-PATHNAME
                      PERFORM 990-DBCLOSE-SEC
      *
                      MOVE  ZERO              TO   FLG-NYUINKA-SET
                      IF    WRK-NYUINKA1      =    SPACE
                         MOVE   PTNYUINRRK-NYUINKA
                                              TO   WRK-NYUINKA1
                         MOVE   WRK-NYUINKAMEI
                                              TO   WRK-NYUINKAMEI1
                         MOVE   1             TO   FLG-NYUINKA-SET
                      END-IF
                      IF   (WRK-NYUINKA2      =    SPACE)   AND
                           (FLG-NYUINKA-SET   =    ZERO)
                         MOVE   PTNYUINRRK-NYUINKA
                                              TO   WRK-NYUINKA2
                         MOVE   WRK-NYUINKAMEI
                                              TO   WRK-NYUINKAMEI2
                         MOVE   1             TO   FLG-NYUINKA-SET
                      END-IF
                      IF   (WRK-NYUINKA3      =    SPACE)   AND
                           (FLG-NYUINKA-SET   =    ZERO)
                         MOVE   PTNYUINRRK-NYUINKA
                                              TO   WRK-NYUINKA3
                         MOVE   WRK-NYUINKAMEI
                                              TO   WRK-NYUINKAMEI3
                         MOVE   1             TO   FLG-NYUINKA-SET
                      END-IF
                  END-IF
      *
                  IF      FLG-NYUINKA-SET =    ZERO
                      MOVE   "1"          TO   WRK-NYUINKA-ETC
                  END-IF
      *
                  MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
                  MOVE    "key44"          TO  MCP-PATHNAME
                  PERFORM 970-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"       TO   MCP-TABLE
           MOVE    "key44"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *     DISPLAY  "WRK-NYUINKA1 = "  WRK-NYUINKA1
      *     DISPLAY  "WRK-NYUINKAMEI1 = "  WRK-NYUINKAMEI1
      *     DISPLAY  "WRK-NYUINKA2 = "  WRK-NYUINKA2
      *     DISPLAY  "WRK-NYUINKAMEI2 = "  WRK-NYUINKAMEI2
      *     DISPLAY  "WRK-NYUINKA3 = "  WRK-NYUINKA3
      *     DISPLAY  "WRK-NYUINKAMEI3 = "  WRK-NYUINKAMEI3
      *     DISPLAY  "WRK-NYUINKA-ETC = " WRK-NYUINKA-ETC
      *
           IF      (WRK-NYUINKAMEI1    NOT =   SPACE)   OR
                   (WRK-NYUINKAMEI2    NOT =   SPACE)   OR
                   (WRK-NYUINKAMEI3    NOT =   SPACE)
              IF       WRK-NYUINKA-ETC    =    SPACE
                  STRING   "【"
                           WRK-NYUINKAMEI1  DELIMITED BY SPACE
                           "　"
                           WRK-NYUINKAMEI2  DELIMITED BY SPACE
                           "　"
                           WRK-NYUINKAMEI3  DELIMITED BY SPACE
                           "】"
                  INTO     SPA-GMN-NYUINKA
                  END-STRING
              ELSE
                  STRING   "【"
                           WRK-NYUINKAMEI1  DELIMITED BY SPACE
                           "　"
                           WRK-NYUINKAMEI2  DELIMITED BY SPACE
                           "　"
                           WRK-NYUINKAMEI3  DELIMITED BY SPACE
                           "　"
                           "等"            DELIMITED BY SIZE
                           "】"
                  INTO     SPA-GMN-NYUINKA
                  END-STRING
              END-IF
           END-IF
      *     DISPLAY  "SPA-GMN-NYUINKA = " SPA-GMN-NYUINKA
      *
           .
       3103-NYUINKA-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *   病棟別日数編集処理（最終入院病室の編集も併せて行う）
      *****************************************************************
       3103-BTUDAY-EDIT-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-SYUBETSU1
           MOVE    SPACE               TO  SPA-GMN-SYUBETSU2
           MOVE    SPACE               TO  SPA-GMN-SYUBETSU3
           MOVE    SPACE               TO  SPA-GMN-SYUBETSU4
           MOVE    SPACE               TO  SPA-GMN-SYUBETSU5
           MOVE    SPACE               TO  SPA-GMN-SYUBETSU6
           MOVE    SPACE               TO  SPA-GMN-LASTNYUIN
      *
      *    最終入院履歴を確定
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
      *
      *        最終入院病室表示エリアの編集
               IF     PTNYUINRRK-JTIKBN  NOT =  "5"  AND "6"
                   MOVE   "最終入院【"  TO  SPA-GMN-LASTNYUIN(1:10)
                   PERFORM    3103-BTU-TANNAME-EDIT-SEC
                   MOVE   SYS-5001-BTU-TANNAME
                                       TO  SPA-GMN-LASTNYUIN(11:10)
                   IF      PTNYUINRRK-BRMNUM(3:6)  NUMERIC
                       MOVE    PTNYUINRRK-BRMNUM(3:6)  TO  WRK-BRMNUM-9
                       MOVE    WRK-BRMNUM-9            TO  WRK-Z6
                       MOVE    ZERO                    TO  IDX2
                       PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   6
                             IF      WRK-Z6-X(IDX:1)     NOT =   SPACE
                                ADD     1               TO  IDX2
                                MOVE    WRK-Z6-X(IDX:1) TO  WRK-BRMNUM
                                                            (IDX2:1)
                             END-IF
                       END-PERFORM
                       MOVE   WRK-BRMNUM
                                       TO  SPA-GMN-LASTNYUIN(21:IDX2)
                       COMPUTE   IDX3   =  21 + IDX2  +  1
                       MOVE   "号室】"   TO  SPA-GMN-LASTNYUIN(IDX3:6)
                   ELSE
                       MOVE   PTNYUINRRK-BRMNUM(3:6)
                                       TO  SPA-GMN-LASTNYUIN(21:6)
                       COMPUTE   IDX3   =  21 + IDX2  +  1
                       MOVE   "号室】"   TO  SPA-GMN-LASTNYUIN(27:6)
                   END-IF
               END-IF
      *
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-BTU-NISSU-AREA
           INITIALIZE                      WRK-BTU-NISSU-AREA
      *
           IF      FLG-PTNYUINRRK          =   ZERO
               MOVE    PTNYUINRRK-RRKNUM
                                           TO  WRK-LAST-RRKNUM
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
               MOVE    WRK-LAST-RRKNUM     TO  PTNYUINRRK-RRKNUM
      *
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 970-PTNYUINRRK-READ-SEC
                   PERFORM    UNTIL   FLG-PTNYUINRRK  =   1
                       MOVE    ZERO        TO  IDX
                       EVALUATE    PTNYUINRRK-JTIKBN
                           WHEN    "0"
                           WHEN    "1"
                           WHEN    "2"
                           WHEN    "3"
                           WHEN    "4"
                           WHEN    "7"
      *                        病棟短縮名の取得
                               PERFORM    3103-BTU-TANNAME-EDIT-SEC
      *
                               IF    (PTNYUINRRK-BTUNUM   =
                                               WRK-NYUIN-BTUNUM(1)) AND
                                     (SYS-5001-BTU-TANNAME =
                                               WRK-NYUIN-BTUTANNAME(1))
                                   MOVE   1      TO   IDX
                               END-IF
                               IF    (PTNYUINRRK-BTUNUM   =
                                               WRK-NYUIN-BTUNUM(2)) AND
                                     (SYS-5001-BTU-TANNAME =
                                               WRK-NYUIN-BTUTANNAME(2))
                                   MOVE   2      TO   IDX
                               END-IF
                               IF    (PTNYUINRRK-BTUNUM   =
                                               WRK-NYUIN-BTUNUM(3)) AND
                                     (SYS-5001-BTU-TANNAME =
                                               WRK-NYUIN-BTUTANNAME(3))
                                   MOVE   3      TO   IDX
                               END-IF
                               IF    (PTNYUINRRK-BTUNUM   =
                                               WRK-NYUIN-BTUNUM(4)) AND
                                     (SYS-5001-BTU-TANNAME =
                                               WRK-NYUIN-BTUTANNAME(4))
                                   MOVE   4      TO   IDX
                               END-IF
                               IF    (PTNYUINRRK-BTUNUM   =
                                               WRK-NYUIN-BTUNUM(5)) AND
                                     (SYS-5001-BTU-TANNAME =
                                               WRK-NYUIN-BTUTANNAME(5))
                                   MOVE   5      TO   IDX
                               END-IF
                               IF    (PTNYUINRRK-BTUNUM   =
                                               WRK-NYUIN-BTUNUM(6)) AND
                                     (SYS-5001-BTU-TANNAME =
                                               WRK-NYUIN-BTUTANNAME(6))
                                   MOVE   6      TO   IDX
                               END-IF
      *
                               IF   IDX   =   ZERO
                                  IF   WRK-LAST-RECNO   <   6
                                     ADD     1    TO   WRK-LAST-RECNO
                                     MOVE    WRK-LAST-RECNO   TO  IDX
                                     MOVE  PTNYUINRRK-BTUNUM
                                                TO  WRK-NYUIN-BTUNUM
                                                                  (IDX)
                                     MOVE  SYS-5001-BTU-TANNAME
                                                TO  WRK-NYUIN-BTUTANNAME
                                                                  (IDX)
                                  ELSE
                                     MOVE    6    TO    IDX
                                     MOVE  "XX" TO  WRK-NYUIN-BTUNUM
                                                                  (IDX)
                                     MOVE  "その他計"
                                                TO  WRK-NYUIN-BTUTANNAME
                                                                  (IDX)
                                  END-IF
                               END-IF
      *                        期間算出
                               INITIALIZE   STS-AREA-DAY
                               INITIALIZE   LNK-DAY5-AREA
                               MOVE  "51"   TO  LNK-DAY5-IRAI
                               MOVE  PTNYUINRRK-TENNYUYMD
                                            TO  LNK-DAY5-START
                               IF  PTNYUINRRK-TENSTUYMD  <=
                                                        SPA-SYSYMD
                                IF   PTNYUINRRK-RRKEDANUM  NOT =
                                              PTNYUINRRK-MAXEDANUM
      *                             転出日のマイナス１日
                                    INITIALIZE   LNK-DAY6-AREA
                                    MOVE  "61"   TO  LNK-DAY6-IRAI
                                    MOVE  PTNYUINRRK-TENSTUYMD
                                                 TO  LNK-DAY6-KIJUN
                                    MOVE  "1"    TO  LNK-DAY6-ZOGENPTN
                                    MOVE  -1     TO  LNK-DAY6-ZOGEN
                                    CALL  "ORCSDAY"    USING
                                                       STS-AREA-DAY
                                                       LNK-DAY6-AREA
                                    MOVE   LNK-DAY6-KEISAN
                                                 TO  LNK-DAY5-END
                                ELSE
                                    MOVE  PTNYUINRRK-TAIINYMD
                                                 TO  LNK-DAY5-END
                                END-IF
                               ELSE
                                  MOVE  SPA-SYSYMD TO  LNK-DAY5-END
                               END-IF
      *
                               CALL  "ORCSDAY"    USING
                                                  STS-AREA-DAY
                                                  LNK-DAY5-AREA
                               IF   STS-DAY-RC1   =   ZERO
                                   ADD   LNK-DAY5-NISSU2
                                             TO  WRK-NYUIN-BTUDAY(IDX)
                               END-IF
                           WHEN  OTHER
                               CONTINUE
                       END-EVALUATE
                       MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
                       MOVE    "key5"          TO  MCP-PATHNAME
                       PERFORM 970-PTNYUINRRK-READ-SEC
                   END-PERFORM
               ELSE
                   INITIALIZE                  PTNYUINRRK-REC
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           PERFORM   VARYING    IDX   FROM   1    BY   1
                     UNTIL     (IDX     >    6)    OR
                               (WRK-NYUIN-BTUNUM(IDX)  =  SPACE)
      *
               MOVE   WRK-NYUIN-BTUDAY(IDX)
                                       TO   WRK-NYUIN-BTUDAY-Z
      *
               EVALUATE    IDX
                   WHEN    1
                       MOVE   WRK-NYUIN-BTUTANNAME(1)
                                          TO   SPA-GMN-SYUBETSU1(1:10)
                       MOVE   WRK-NYUIN-BTUDAY-Z
                                          TO   SPA-GMN-SYUBETSU1(11:4)
                       MOVE  "日"         TO   SPA-GMN-SYUBETSU1(15:2)
                   WHEN    2
                       MOVE   WRK-NYUIN-BTUTANNAME(2)
                                          TO   SPA-GMN-SYUBETSU2(1:10)
                       MOVE   WRK-NYUIN-BTUDAY-Z
                                          TO   SPA-GMN-SYUBETSU2(11:4)
                       MOVE  "日"         TO   SPA-GMN-SYUBETSU2(15:2)
                   WHEN    3
                       MOVE   WRK-NYUIN-BTUTANNAME(3)
                                          TO   SPA-GMN-SYUBETSU3(1:10)
                       MOVE   WRK-NYUIN-BTUDAY-Z
                                          TO   SPA-GMN-SYUBETSU3(11:4)
                       MOVE  "日"         TO   SPA-GMN-SYUBETSU3(15:2)
                   WHEN    4
                       MOVE   WRK-NYUIN-BTUTANNAME(4)
                                          TO   SPA-GMN-SYUBETSU4(1:10)
                       MOVE   WRK-NYUIN-BTUDAY-Z
                                          TO   SPA-GMN-SYUBETSU4(11:4)
                       MOVE  "日"         TO   SPA-GMN-SYUBETSU4(15:2)
                   WHEN    5
                       MOVE   WRK-NYUIN-BTUTANNAME(5)
                                          TO   SPA-GMN-SYUBETSU5(1:10)
                       MOVE   WRK-NYUIN-BTUDAY-Z
                                          TO   SPA-GMN-SYUBETSU5(11:4)
                       MOVE  "日"         TO   SPA-GMN-SYUBETSU5(15:2)
                   WHEN    6
                       MOVE   WRK-NYUIN-BTUTANNAME(6)
                                          TO   SPA-GMN-SYUBETSU6(1:10)
                       MOVE   WRK-NYUIN-BTUDAY-Z
                                          TO   SPA-GMN-SYUBETSU6(11:4)
                       MOVE  "日"         TO   SPA-GMN-SYUBETSU6(15:2)
                   WHEN   OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
      *
           .
       3103-BTUDAY-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *   病棟短縮名称の取得
      *****************************************************************
       3103-BTU-TANNAME-EDIT-SEC            SECTION.
      *
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           ELSE
               INITIALIZE                  SYS-5001-REC
           END-IF
      *
           .
       3103-BTU-TANNAME-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスターより編集　処理
      *****************************************************************
       3101-NYUINACCT-HEN-SEC           SECTION.
      *
      *    コラムリスト位置
           MOVE    1                   TO  FLG-GMN-INIT
      *
           MOVE    ZERO                TO  FLG-NYUINKHNRYO
           MOVE    ZERO                TO  SPA-TAIHI-HKN
           MOVE    ZERO                TO  SPA-TAIHI-SHOKUJI
           MOVE    ZERO                TO  SPA-TAIHI-SHOKUJI-ASA
           MOVE    ZERO                TO  SPA-TAIHI-SHOKUJI-HIRU
           MOVE    ZERO                TO  SPA-TAIHI-SHOKUJI-YORU
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
      *
           INITIALIZE                  SPA-GMN-INPUT-41
           INITIALIZE                  SPA-SCR-REC-AREA
           MOVE    ZERO                TO  SPA-NAI-MAX-41
           MOVE    ZERO                TO  SPA-GMN-MAX-41
      *
      *    入院履歴より当月の対象科判定
           PERFORM 31010-PTNYUINRRK-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *        入院会計マスタの当月分編集
               PERFORM 31011-NYUINACCT-HENSYU-SEC
           END-IF
      *
      *    特定の診療行為コードの編集
           PERFORM 31012-DSPCD-HENSYU-SEC
      *
           MOVE    IDX                 TO  SPA-NAI-MAX-41
           MOVE    IDG                 TO  SPA-GMN-MAX-41
      *    対象なし
           IF      IDG                 =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
               GO      TO       3101-NYUINACCT-HEN-EXT
           ELSE
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
      *    すべての行を選択可能とする
           MOVE    ZERO                TO  WRK-TBANGO.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX-41
               IF      SPA-GMN-TBLREC (IDX)(1:3)   NOT =   "---"
                   IF      SPA-GMN-TBANGO (IDX)    NOT =   ZERO
                       MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-TBANGO
                   END-IF
                   MOVE    WRK-TBANGO      TO  SPA-GMN-TBANGO2 (IDX)
               END-IF
           END-PERFORM.
      *
           .
       3101-NYUINACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタの当月分編集処理
      *****************************************************************
       31011-NYUINACCT-HENSYU-SEC              SECTION.
      *
           IF      SPA-5000-RYOYODSPKBN   =    "1"
               MOVE   "key36"          TO  WRK-PATHNAME
           ELSE
               MOVE   "key39"          TO  WRK-PATHNAME
           END-IF
      *    入院会計マスターを編集（入院基本料）
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL      (FLG-NYUINACCT     =   1)
      *
               ADD     1                   TO  IDX
      *        ＳＰＡ編集
               PERFORM 3101-NYUINACCT-SPA-SEC
      *
               EVALUATE    NACCT-ZAISKBKBN
      *            入院料
                   WHEN    "1"
                       MOVE   1            TO  FLG-NYUINKHNRYO
               END-EVALUATE
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
      *    入院会計マスターを編集（選定入院料）
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key37"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL      (FLG-NYUINACCT     =   1)
      *
               ADD     1                   TO  IDX
      *        ＳＰＡ編集
               PERFORM 3101-NYUINACCT-SPA-SEC
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key37"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
      *    月末日を取得
           INITIALIZE                        STS-AREA-DAY
           INITIALIZE                        LNK-DAY7-AREA
           MOVE    "71"                TO    LNK-DAY7-IRAI
           MOVE    SPA-NAI-SRYYM       TO    LNK-DAY7-YM
           CALL    "ORCSDAY"     USING       STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO    WRK-MATSUYMD
      *
      *    入院会計マスターを編集（入院料以外）
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key38"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-UPYMD
      *
           MOVE    ZERO                TO  SPA-TAIHI-SHOKUJI-ASA
           MOVE    ZERO                TO  SPA-TAIHI-SHOKUJI-HIRU
           MOVE    ZERO                TO  SPA-TAIHI-SHOKUJI-YORU
      *
           PERFORM UNTIL      (FLG-NYUINACCT     =   1)
      *
               ADD     1                   TO  IDX
      *        ＳＰＡ編集
               PERFORM 3101-NYUINACCT-SPA-SEC
      *
               EVALUATE    NACCT-ZAISKBKBN
      *            最終日の食事を退避（次月会計の作成用）
                   WHEN    "4"
                       IF      NACCT-ZAITEN    =    ZERO
                           MOVE    ZERO           TO  SPA-TAIHI-SHOKUJI
                           IF    NACCT-SRYCDTOTAL   =   099999913  OR
                                                        099999920
                               MOVE   NACCT-DAY(WRK-MATSUDD)
                                                  TO  SPA-TAIHI-SHOKUJI
                           END-IF
                           IF    NACCT-SRYCDTOTAL   =   099999918
                               MOVE   NACCT-DAY(WRK-MATSUDD)
                                             TO  SPA-TAIHI-SHOKUJI-ASA
                           END-IF
                           IF    NACCT-SRYCDTOTAL   =   099999919
                               MOVE   NACCT-DAY(WRK-MATSUDD)
                                             TO  SPA-TAIHI-SHOKUJI-HIRU
                           END-IF
                           IF    NACCT-SRYCDTOTAL   =   099999920
                               MOVE   NACCT-DAY(WRK-MATSUDD)
                                             TO  SPA-TAIHI-SHOKUJI-YORU
                           END-IF
      *                    食事剤の更新日を退避
                           IF    NACCT-SRYCDTOTAL   =   099999918  OR
                                                        099999919  OR
                                                        099999920
                               IF   SPA-GMN-UPYMD  <=   NACCT-UPYMD
                                   MOVE   NACCT-UPYMD
                                                  TO  SPA-GMN-UPYMD
                               END-IF
                           END-IF
                       ELSE
      *                    食堂加算
                           IF    NACCT-SRYCDTOTAL   =   197000570
                             MOVE      1          TO   SPA-SHOKUDOU-OFF
                             IF   NACCT-DAY(WRK-MATSUDD)  NOT = ZERO
                                 MOVE   ZERO  TO  SPA-SHOKUDOU-OFF
                             END-IF
                           END-IF
                       END-IF
      *            最終日の保険組合せを退避（次月会計の作成用）
                   WHEN    "5"
                       MOVE    ZERO            TO  SPA-TAIHI-HKN
                       PERFORM VARYING IDX-HKN     FROM   1   BY  1
                               UNTIL   IDX-HKN     >   31
                           IF      NACCT-DAY (IDX-HKN)  NOT =   ZERO
                               MOVE   NACCT-DAY(IDX-HKN)
                                                   TO  SPA-TAIHI-HKN
                          END-IF
                       END-PERFORM
               END-EVALUATE
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key38"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       31011-NYUINACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴より当月の対象科判定処理
      *****************************************************************
       31010-PTNYUINRRK-CHK-SEC              SECTION.
      *
           INITIALIZE                      SPA-CHK-TBL
      *
      *    患者入院履歴を検索する
      *    医療機関、患者ＩＤ、転入日、転出日を参照
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
           MOVE    "00"                TO  SPA-GMN-SRYKA-I-41
           IF      SPA-GMN-SRYKA-I-41  =   "00"
               MOVE    "key19"             TO  WRK-DBPATH
           ELSE
               MOVE    SPA-GMN-SRYKA-I-41  TO  PTNYUINRRK-NYUINKA
               MOVE    "key18"             TO  WRK-DBPATH
           END-IF.
           MOVE    SPA-NAI-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  PTNYUINRRK-TENSTUYMD(1:6)
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:2)
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
           IF      FLG-PTNYUINRRK      =   1
               IF      SPA-GMN-SRYKA-I-41  =   "00"
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
           END-IF
           PERFORM
                   UNTIL      FLG-PTNYUINRRK    =   1
               IF   PTNYUINRRK-JTIKBN   NOT = "5"  AND  "6"
                   MOVE    ZERO                TO  IDX-DS
                   MOVE    ZERO                TO  IDX-DE
      *            転入日
                   IF      SPA-NAI-SRYYM  =   PTNYUINRRK-TENNYUYMD(1:6)
                       MOVE    PTNYUINRRK-TENNYUYMD(7:2)   TO IDX-DS
                   ELSE
                       MOVE    1                   TO  IDX-DS
                   END-IF
      *            転出日
                   IF      SPA-NAI-SRYYM  =   PTNYUINRRK-TENSTUYMD(1:6)
                       MOVE    PTNYUINRRK-TENSTUYMD(7:2)   TO  IDX-DE
                       IF  PTNYUINRRK-TAIINYMD  =  PTNYUINRRK-TENSTUYMD
                           CONTINUE
                       ELSE
      *                    退院日でない時、前日
                           COMPUTE IDX-DE          =   IDX-DE  -   1
                       END-IF
                   ELSE
                       MOVE    SPA-NAI-SRYMATU      TO  IDX-DE
                   END-IF
      *
                   PERFORM VARYING    IDK      FROM    IDX-DS   BY  1
                           UNTIL      IDK      >   IDX-DE
                       MOVE    "1"             TO  SPA-CHK-DAY (IDK)
                       IF      SPA-CHK-SRYKA (IDK) =   SPACE
                           MOVE   PTNYUINRRK-NYUINKA  TO  SPA-CHK-SRYKA
                                                                  (IDK)
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       31010-PTNYUINRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院会計マスターより画面内容編集処理
      *****************************************************************
       3101-NYUINACCT-SPA-SEC           SECTION.
      *
      *    入院会計マスタ内容編集
           MOVE    IDX                  TO  SPA-NAI-RENNUM-41  (IDX).
           MOVE    NACCT-SRYKBN         TO  SPA-NAI-SRYKBN-41  (IDX).
           MOVE    NACCT-ZAINUM         TO  SPA-NAI-ZAINUM-41  (IDX).
           MOVE    NACCT-HKNCOMBI       TO  SPA-NAI-HKNCOMBI-41 (IDX).
           MOVE    NACCT-ZAITEN         TO  SPA-NAI-ZAITEN-41  (IDX).
           MOVE    NACCT-ZAIKAISU       TO  SPA-NAI-ZAIKAISU-41 (IDX).
           MOVE    NACCT-DAY-AREA       TO  SPA-NAI-DAY-AREA-41 (IDX).
      *
      *****MOVE    NACCT-SYUTEN2        TO  SPA-NAI-SYUTEN2 (IDX).
           MOVE    NACCT-NYUGAIKBN      TO  SPA-NAI-TNYUGAIKBN (IDX).
           MOVE    NACCT-SRYKA          TO  SPA-NAI-TSRYKA-41  (IDX).
           MOVE    NACCT-ZAISKBKBN      TO  SPA-NAI-TZAISKBKBN-41 (IDX).
      *
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      3101-NYUINACCT-SPA-EXT
           END-IF.
      *
           MOVE    IDX                 TO  SPA-GMN-TBANGO  (IDG).
           MOVE    IDG                 TO  SPA-NAI-TBANGO  (IDX).
      *
           MOVE    ZERO                TO  WRK-IDG.
      *    入院行為マスター読み込み
           INITIALIZE                      NYUINACT-REC.
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM.
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN.
           MOVE    NACCT-PTID          TO  NSRY-PTID.
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA.
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM.
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM.
      *    点数検索用日付編集
           MOVE    NACCT-SRYYM         TO  WRK-SRYYMD(1:6).
           MOVE    01                  TO  WRK-SRYDD.
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      NACCT-DAY (IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
           MOVE    ZERO                TO  FLG-CHK
           PERFORM UNTIL     ( FLG-NYUINACT    =   1 )  OR
                             (IDG              >=  400)
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1  BY  1
                       UNTIL      (IDZ         >   5  )  OR
                                  (IDG         >=  400)  OR
                                  (NSRY-SRYCD (IDZ) =   SPACE)
                   PERFORM 31012-ZAIMEI-HEN-SEC
               END-PERFORM
      *
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           END-PERFORM.
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
      *    点数
           MOVE    SPA-NAI-ZAITEN-41 (IDX) TO  WRK-TENSU
           MOVE    WRK-TENSU-X         TO  SPA-GMN-TTENSU-41   (IDG)
           MOVE    IDG                 TO  SPA-NAI-ENDBANGO-41 (IDX)
      *
      *    剤終了編集
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      3101-NYUINACCT-SPA-EXT
           END-IF.
           MOVE    ALL "-"             TO  SPA-GMN-TBLREC (IDG)
           .
      *
       3101-NYUINACCT-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       31012-ZAIMEI-HEN-SEC                  SECTION.
      *
           IF      FLG-CHK             =   1
               ADD     1                   TO  IDG
               IF      IDG                 >   400
                   GO      TO      31012-ZAIMEI-HEN-EXT
               END-IF
           END-IF
           MOVE    1                   TO  FLG-CHK
      *
      *    診療コードより診療行為名称を取得
           MOVE    NSRY-SRYCD (IDZ)    TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    NSRY-SRYCD (IDZ)    TO  TNS-NAME
           END-IF.
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO-41 (IDG).
      *    診療コード
           MOVE    NSRY-SRYCD (IDZ)    TO  SPA-GMN-TSRYCD (IDG)
      *    識別区分
           MOVE    NACCT-ZAISKBKBN     TO  SPA-GMN-TZAISKBKBN-41 (IDG).
      *
      *    老人適用区分（同一剤は同じ）
           IF      TNS-ROUTEKKBN       =   ZERO
               IF      SPA-NAI-ROUTEKKBN-41 (IDX)  =   ZERO
                   MOVE    TNS-ROUTEKKBN  TO  SPA-NAI-ROUTEKKBN-41 (IDX)
               END-IF
           ELSE
               MOVE    TNS-ROUTEKKBN      TO  SPA-NAI-ROUTEKKBN-41 (IDX)
           END-IF
      *
           .
      *
       31012-ZAIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   特定の診療行為コードの編集処理
      *****************************************************************
       31012-DSPCD-HENSYU-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDZ3
           INITIALIZE                  TBL-SRYCD-HEN-AREA
      *
           PERFORM VARYING     IDZ2    FROM    1   BY  1
                   UNTIL       IDZ2    >   10
               IF      SPA-5000-DSPCD-41 (IDZ2) NOT = SPACE
                   PERFORM 310121-SRYACT-CHK-SEC
               END-IF
           END-PERFORM
      *
      *    ＳＰＡテーブルへ編集
           PERFORM VARYING    IDZ2     FROM    1   BY  1
                   UNTIL     (IDZ2     >   100 )   OR
                             (TBL-SRYCD (IDZ2) =   SPACE )  OR
                             (IDX              >=  400   )  OR
                             (IDG              >=  400   )
               ADD     1              TO  IDX
               MOVE    IDZ            TO  SPA-NAI-RENNUM-41  (IDX)
               MOVE    TBL-HKNCOMBI (IDZ2)
                                      TO  SPA-NAI-HKNCOMBI-41 (IDX)
               MOVE    TBL-DAY-G    (IDZ2)
                                      TO  SPA-NAI-DAY-AREA-41 (IDX)
      *        剤識別を’Ｓ’とする
               MOVE    "S"            TO  SPA-NAI-TZAISKBKBN-41(IDX)
      *        画面表示テーブルへ編集
               PERFORM 310122-DSPCD-TBL-HEN-SEC
           END-PERFORM
           .
      *
       31012-DSPCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面表示テーブルへ編集処理
      *****************************************************************
       310122-DSPCD-TBL-HEN-SEC               SECTION.
      *
      *    １行目
      *    診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1.
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    TBL-SRYSYUKBN (IDZ2)    =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH.
      *    診療種別がない時、診療行為区分より
           IF      TBL-SRYSYUKBN (IDZ2)    =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    TBL-SRYKBN (IDZ2)   =   MSYU-SRYKBN(MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   400
               GO      TO      310122-DSPCD-TBL-HEN-EXT
           END-IF
      *
           MOVE    IDX                 TO  SPA-GMN-TBANGO  (IDG).
           MOVE    IDG                 TO  SPA-NAI-TBANGO  (IDX).
      *
           MOVE    "S"                TO  SPA-GMN-TZAISKBKBN-41(IDG)
      *
           IF      TBL-SRYSYUKBN (IDZ2)   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TMEISYO-41 (IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO-41 (IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-GMN-TMEISYO-41 (IDG)(1:1)
               MOVE    TBL-SRYSYUKBN (IDZ2)
                                       TO  SPA-GMN-TMEISYO-41 (IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO-41 (IDG)(7:)
           END-IF
      *    保険組合せ
           MOVE    "（保）"            TO  SPA-GMN-TMEISYO-41 (IDG)(19:)
           MOVE    TBL-HKNCOMBI (IDZ2) TO  SPA-GMN-TMEISYO-41 (IDG)(25:)
      *
      *    ２行目
           ADD     1                   TO  IDG
           IF      IDG                 >   400
               GO      TO      310122-DSPCD-TBL-HEN-EXT
           END-IF
      *    診療コードより診療行為名称を取得
           MOVE    TBL-SRYCD (IDZ2)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    TBL-SRYCD (IDZ2)    TO  TNS-NAME
           END-IF.
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO-41 (IDG)
      *    診療コード
           MOVE    TBL-SRYCD (IDZ2)    TO  SPA-GMN-TSRYCD (IDG)
           MOVE    "S"                 TO  SPA-GMN-TZAISKBKBN-41(IDG)
      *
      *    剤終了編集
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      310122-DSPCD-TBL-HEN-EXT
           END-IF.
           MOVE    ALL "-"             TO  SPA-GMN-TBLREC (IDG)
           .
      *
       310122-DSPCD-TBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスターチェック　処理
      *****************************************************************
       310121-SRYACT-CHK-SEC               SECTION.
      *
      *    診療行為マスターチェック
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    "1"                 TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-NAI-SRYYM    TO  SRY-SRYYM
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (1)
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (2)
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (3)
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (4)
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC.
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM
               UNTIL       FLG-SRYACT      =   1
               IF     (SPA-GMN-SRYKA-I-41     =   "00")  OR
                      (SPA-GMN-SRYKA-I-41     =   SRY-SRYKA)
      *            診療会計検索編集
                   PERFORM 3101211-SRYACCT-HEN-SEC
               END-IF
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
           .
      *
       310121-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスターより編集　処理
      *****************************************************************
       3101211-SRYACCT-HEN-SEC              SECTION.
      *
      *    診療会計マスターを編集
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
           IF     (FLG-SRYACCT         =   1   )  OR
                  (ACCT-ZAIKAISU       =   ZERO)
              GO      TO      3101211-SRYACCT-HEN-EXT
           END-IF
      *
           PERFORM VARYING     IDZ4   FROM    1   BY  1
                   UNTIL       IDZ4       >   100
               IF      TBL-SRYCD (IDZ4)    =   SPACE
                   MOVE    SPA-5000-DSPCD-41 (IDZ2)
                                           TO  TBL-SRYCD    (IDZ4)
                   MOVE    ACCT-HKNCOMBI   TO  TBL-HKNCOMBI (IDZ4)
                   MOVE    ACCT-SRYKBN     TO  TBL-SRYKBN   (IDZ4)
                   MOVE    SRY-SRYSYUKBN   TO  TBL-SRYSYUKBN (IDZ4)
               END-IF
               IF     (TBL-SRYCD (IDZ4)    =   SPA-5000-DSPCD-41
                                                             (IDZ2)) AND
                      (TBL-HKNCOMBI(IDZ4)  =   ACCT-HKNCOMBI    )  AND
                      (TBL-SRYKBN  (IDZ4)  =   SRY-SRYKBN       )  AND
                      (TBL-SRYSYUKBN(IDZ4) =   SRY-SRYSYUKBN    )
                   PERFORM VARYING     IDZ5   FROM    1   BY  1
                           UNTIL       IDZ5   >   31
                       ADD     ACCT-DAY (1 IDZ5)  TO  TBL-DAY(IDZ4 IDZ5)
                   END-PERFORM
                   MOVE    100                TO  IDZ4
               END-IF
           END-PERFORM
           .
      *
       3101211-SRYACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       3105-HKNCOMBI-HEN-SEC          SECTION.
      *
           INITIALIZE                  SPA-HKNCOMBI-AREA-41
      *
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAY
           MOVE    SPA-NAI-SRYYM       TO  SPA-SRYYMD(1:6)
           MOVE    "00"                TO  SPA-SRYYMD(7:2)
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    SPACE               TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKNCOMBI-MAX
      *
           MOVE    ZERO                TO  IDH
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )  OR
                              (IDH     >=  30              )
               IF     (ORCSHKN-NAI-RSI-HKNKBN (IDX)  =  "3")
                      CONTINUE
               ELSE
                  ADD     1                   TO  IDH
                  MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-I41-THKNCOMBI (IDH)
                  MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-I41-THKNCOMBIMEI(IDH)
      *
      ****        MOVE    ORCSHKN-NAI-HKNID (IDX)
      ****                             TO  SPA-NAI-THKNID  (IDH)
                  MOVE    ORCSHKN-NAI-HKNNUM (IDX)
                                       TO  SPA-I41-HKNNUM  (IDH)
                  MOVE    ORCSHKN-NAI-HKNKBN (IDX)
                                       TO  SPA-I41-HKNKBN  (IDH)
      *           適用期間
                  MOVE    ORCSHKN-NAI-TEKSTYMD (IDX)
                                       TO  SPA-I41-TTEKSTYMD (IDH)
                  MOVE    ORCSHKN-NAI-TEKEDYMD (IDX)
                                       TO  SPA-I41-TTEKEDYMD (IDH)
      *           四肢区分
                  MOVE    ORCSHKN-NAI-RSI-SISIKBN (IDX)
                                       TO  SPA-I41-SISIKBN (IDH)
      *           労災区分
                  MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX)
                                       TO  SPA-I41-RSI-HKNKBN  (IDH)
      *
                  MOVE    IDH             TO  SPA-HKNCOMBI-MAX
               END-IF
           END-PERFORM
      *
           .
      *
       3105-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合期限切れチェック、外泊による編集修正処理
      *****************************************************************
       3106-DAY-KIKANCHK-SEC           SECTION.
      *
           INITIALIZE                  SPA-CHK-TBL2
           PERFORM VARYING IDX         FROM  1  BY 1
                   UNTIL   IDX         >   SPA-GMN-MAX-41
             IF     (SPA-GMN-TBLREC (IDX)(1:3) =   ALL "-" )  OR
                    (SPA-GMN-TBANGO (IDX)      =   ZERO    )
                 CONTINUE
             ELSE
      *
               MOVE    SPA-GMN-TBANGO (IDX) TO  IDX-N
      *
      *        入院基本料
               IF     (SPA-GMN-TZAISKBKBN-41 (IDX) =   "1"  )  AND
                      (SPA-GMN-TSRYCD (IDX)    NOT =   W-YOYAKU)
                   PERFORM VARYING IDK     FROM    1   BY  1
                           UNTIL   IDK     >   31
                       IF      (SPA-NAI-DAY-41 (IDX-N IDK)  >   ZERO )
                         AND   (SPA-CHK-DAY (IDK)      =   "1"  )
      *                    入院基本料算定日
                           MOVE    "1"         TO  SPA-CHK-NYUIN (IDK)
      *                    医療観察法の入院算定日
                           IF   SPA-GMN-TSRYCD (IDX)(1:3)  =  "188"
                                MOVE   "1"     TO  SPA-CHK-IRYOKNS(IDK)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        保険組合せ
               IF      SPA-GMN-TZAISKBKBN-41 (IDX) =   "5"
                   PERFORM VARYING IDK     FROM    1   BY  1
                           UNTIL   IDK     >   31
                       IF      SPA-NAI-DAY-41 (IDX-N IDK)  NOT =  ZERO
                           MOVE    "1"     TO  SPA-CHK-HKNCOMBI (IDK)
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        選定入院料
               IF     (SPA-GMN-TZAISKBKBN-41 (IDX) =   "1"  )  AND
                      (SPA-GMN-TSRYCD (IDX)     =   W-YOYAKU)
                   PERFORM VARYING IDK     FROM    1   BY  1
                           UNTIL   IDK     >   31
                       IF      (SPA-NAI-DAY-41 (IDX-N IDK)  >   ZERO )
                         AND   (SPA-CHK-DAY (IDK)      =   "1"  )
      *                    入院基本料算定日
                           MOVE    "1"         TO  SPA-CHK-SENTEI (IDK)
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        外泊
               IF      SPA-GMN-TZAISKBKBN-41 (IDX) =   "2"
                   PERFORM VARYING IDK     FROM    1   BY  1
                           UNTIL   IDK     >   31
                       IF      SPA-NAI-DAY-41 (IDX-N IDK)
                                           =   1  OR  2  OR  3
      *                    外泊中
                           MOVE    "1"         TO  SPA-CHK-GAIHAKU (IDK)
                       END-IF
                   END-PERFORM
               END-IF
      *
             END-IF
           END-PERFORM
      *
           MOVE    SPACE           TO  SPA-GMN-KEIKOKU
      *
      *    画面の日表示
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-NAI-MAX-41
      *        画面表示位置
               MOVE    SPA-NAI-TBANGO (IDX) TO  IDG
      *
               PERFORM VARYING IDK     FROM    1   BY  1
                       UNTIL   IDK     >   31
                   MOVE    SPA-NAI-DAY-41 (IDX IDK)
                                           TO  WRK-ZZ
                   MOVE    WRK-ZZ-X        TO  SPA-GMN-TDAY-41 (IDG IDK)
                   IF   SPA-NAI-DAY-41 (IDX IDK)  >=  100
                       MOVE    SPA-NAI-DAY-41 (IDX IDK)
                                           TO  WRK-Z99
                       MOVE    WRK-Z99-X   TO  SPA-GMN-TDAY-41 (IDG IDK)
                   END-IF
               END-PERFORM
      *
               EVALUATE     SPA-NAI-TZAISKBKBN-41 (IDX)
      *            外泊日の食事を表示しない
                   WHEN     "4"
                       PERFORM VARYING IDK     FROM    1   BY  1
                               UNTIL   IDK     >   31
                           IF      SPA-CHK-GAIHAKU (IDK)   NOT =   SPACE
                               MOVE    SPACE       TO  SPA-GMN-TDAY-41
                                                           (IDG IDK)
                           END-IF
                       END-PERFORM
      *            外泊日の加算を表示しない
                   WHEN     "6"
                       PERFORM VARYING IDK     FROM    1   BY  1
                               UNTIL   IDK     >   31
                           IF      SPA-CHK-GAIHAKU (IDK)   NOT =   SPACE
                               MOVE    SPACE       TO  SPA-GMN-TDAY-41
                                                           (IDG IDK)
                           END-IF
                       END-PERFORM
      *            外泊日の療養担当手当を表示しない
                   WHEN     "7"
                       PERFORM VARYING IDK     FROM    1   BY  1
                               UNTIL   IDK     >   31
                           IF      SPA-CHK-GAIHAKU (IDK)   NOT =   SPACE
                               MOVE    SPACE       TO  SPA-GMN-TDAY-41
                                                           (IDG IDK)
                           END-IF
                       END-PERFORM
      *            保険の期間判定
                   WHEN    "5"
                       IF   SPA-GMN-KEIKOKU   =    SPACE
                           PERFORM 31061-HKN-KIKANCHK-SEC
                       END-IF
      *            病棟設定と算定入院料のチェック
                   WHEN    "1"
                       PERFORM 31062-SANTEI-NYUINCHK-SEC
               END-EVALUATE
           END-PERFORM
           .
      *
       3106-DAY-KIKANCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険機関切れ編集処理
      *****************************************************************
       31061-HKN-KIKANCHK-SEC                  SECTION.
      *
           MOVE    ZERO            TO  WRK-HKN-ST
           MOVE    ZERO            TO  WRK-HKN-ED
           INITIALIZE                  WRK-GRP-HKNCOMBI-G
      ***     MOVE    SPACE           TO  SPA-GMN-KEIKOKU
      *
           PERFORM VARYING IDK     FROM    1   BY  1
                   UNTIL   IDK     >   SPA-NAI-SRYMATU
               IF      SPA-NAI-DAY-41 (IDX IDK)    NOT =   ZERO
                   MOVE    ZERO            TO  WRK-HKNCOMBINUM
                   MOVE    SPA-NAI-DAY-41 (IDX IDK)
                                           TO  WRK-HKNCOMBINUM(2:3)
                   MOVE    ZERO            TO  FLG-CHK2
                   MOVE    SPA-NAI-SRYYM   TO  WRK-HKN-YM
                   MOVE    IDK             TO  WRK-HKN-DD
                   PERFORM VARYING IDH     FROM    1   BY  1
                           UNTIL   IDH     >   SPA-HKNCOMBI-MAX
                       IF     (SPA-I41-THKNCOMBI(IDH)
                                               =   WRK-HKNCOMBINUM) AND
                              (SPA-I41-TTEKSTYMD (IDH)
                                               <=  WRK-HKN-YMD) AND
                              (SPA-I41-TTEKEDYMD (IDH)
                                               >=  WRK-HKN-YMD)
                           MOVE    SPA-HKNCOMBI-MAX    TO  IDH
                           MOVE    1                   TO  FLG-CHK2
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK2            =   ZERO
                       MOVE    IDK             TO  WRK-HKN-ED
                       IF      WRK-HKN-ST      =   ZERO
                           MOVE    IDK         TO  WRK-HKN-ST
                       END-IF
                   END-IF
               END-IF
               PERFORM VARYING IDI    FROM    1   BY  1
                       UNTIL ( IDI    >   31 )
                        OR   ( WRK-HKNCOMBINUM
                                       =   WRK-GRP-HKNCOMBI (IDI))
                   IF    ( WRK-GRP-HKNCOMBI (IDI) =   ZERO )
                       MOVE    WRK-HKNCOMBINUM
                                           TO WRK-GRP-HKNCOMBI (IDI)
                       MOVE    31          TO IDI
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF     (WRK-HKN-ST          =   ZERO )  AND
                  (WRK-HKN-ED          =   ZERO )
               CONTINUE
           ELSE
               IF     (WRK-HKN-ST          =   WRK-HKN-ED ) 
                   MOVE    SPACE           TO   SPA-GMN-KEIKOKU
                   STRING  "保険の適用が切れている期間があります。"
                                            DELIMITED  BY  SIZE
                       WRK-HKN-ST           DELIMITED  BY  SIZE
                       "日"                 DELIMITED  BY  SIZE
                                       INTO      SPA-GMN-KEIKOKU
                   END-STRING
               ELSE
                   MOVE    SPACE           TO   SPA-GMN-KEIKOKU
                   STRING  "保険の適用が切れている期間があります。"
                                            DELIMITED  BY  SIZE
                       WRK-HKN-ST           DELIMITED  BY  SIZE
                       " - "                DELIMITED  BY  SIZE
                       WRK-HKN-ED           DELIMITED  BY  SIZE
                       "日"                 DELIMITED  BY  SIZE
                                       INTO      SPA-GMN-KEIKOKU
                   END-STRING
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-KEIKOKU     =   SPACE )
               PERFORM VARYING IDI    FROM    1   BY  1
                       UNTIL ( IDI    >   31 )
                        OR   ( WRK-GRP-HKNCOMBI (IDI) =   ZERO )
                        OR   ( SPA-GMN-KEIKOKU     NOT =   SPACE )
                 INITIALIZE              HKNCOMBI-REC
                 MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
                 MOVE    SPA-GMN-PTID    TO  COMB-PTID
                 MOVE    WRK-GRP-HKNCOMBI (IDI)
                                         TO  COMB-HKNCOMBINUM
                 MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                 MOVE    "tbl_hkncombi"  TO  MCP-TABLE
                 MOVE    "key"           TO  MCP-PATHNAME
                 PERFORM 910-DBSELECT-SEC
                 IF    ( MCP-RC          =   ZERO )
                   MOVE    "tbl_hkncombi"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF    ( MCP-RC      =   ZERO )
                     MOVE    MCPDATA-REC TO  HKNCOMBI-REC
                     IF    ( COMB-HKNNUM NOT =   SPACE )
                       MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
                       MOVE    ZERO            TO  WRK-PAYKBN
                       PERFORM 980-HKNNUM-SEL-SEC
                       IF    ( FLG-HKNNUM  NOT =   ZERO )
                           STRING "有効期間が切れた保険での"
                                                   DELIMITED BY SIZE
                                  "登録があります。"
                                                   DELIMITED BY SIZE
                           INTO   SPA-GMN-KEIKOKU
                           END-STRING
                       END-IF
                       PERFORM VARYING IDJ   FROM  1   BY  1
                               UNTIL ( IDJ   >   4 )
                                OR   ( COMB-KOHHKNNUM(IDJ) =   SPACE )
                                OR   ( SPA-GMN-KEIKOKU NOT =   SPACE )
                           MOVE    COMB-KOHHKNNUM(IDJ) TO  WRK-HKNNUM
                           MOVE    COMB-KOHPAYKBN(IDJ) TO  WRK-PAYKBN
                           PERFORM 980-HKNNUM-SEL-SEC
                           IF    ( FLG-HKNNUM  NOT =   ZERO )
                               STRING "有効期間が切れた保険での"
                                                   DELIMITED BY SIZE
                                      "登録があります。"
                                                   DELIMITED BY SIZE
                               INTO   SPA-GMN-KEIKOKU
                               END-STRING
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
                 END-IF
                 MOVE    "tbl_hkncombi"  TO  MCP-TABLE
                 MOVE    "key"           TO  MCP-PATHNAME
                 PERFORM 990-DBCLOSE-SEC
               END-PERFORM
           END-IF
      *
           .
      *
       31061-HKN-KIKANCHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟設定と算定入院料のチェック
      *****************************************************************
       31062-SANTEI-NYUINCHK-SEC                  SECTION.
      *
      *    入院履歴から該当月１日時点の情報検索
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
           MOVE    "01"                TO  PTNYUINRRK-TENNYUYMD(7:2)
           MOVE    PTNYUINRRK-TENNYUYMD TO PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE   "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE   "key41"          TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
               IF  (FLG-PTNYUINRRK     =   ZERO)   AND
                   (PTNYUINRRK-TOKUTEI-KBN  =   SPACE)
                IF (SPA-NAI-SRYYM      =  PTNYUINRRK-TENSTUYMD(1:6)) AND
      **             (PTNYUINRRK-TENSTUYMD(7:2)  =  "01")   AND
      **             (PTNYUINRRK-TAIINYMD(7:2)   =  "99")
                   (PTNYUINRRK-TENSTUYMD(7:2)  =  "01")
                    CONTINUE
                ELSE
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    "5001"              TO  SYS-KANRICD
                   MOVE    PTNYUINRRK-BTUNUM   TO  SYS-KBNCD
                   MOVE    SPA-NAI-SRYYM       TO  SYS-STYUKYMD(1:6)
                   MOVE    "01"                TO  SYS-STYUKYMD(7:2)
                   MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                      MOVE    "tbl_syskanri"  TO  MCP-TABLE
                      MOVE    "key10"         TO  MCP-PATHNAME
                      PERFORM 900-KANRIMST-READ-SEC
                   ELSE
                      MOVE    1               TO  FLG-SYSKANRI
                   END-IF
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-5001-REC
      *              入院料の比較
                     PERFORM    VARYING   IDU   FROM   1  BY  1
                                UNTIL    (IDU   >    10)  OR
                                         (SPA-GMN-TSRYCD(IDU) = SPACE)
                      IF  SPA-GMN-TDAY-41(IDU 1)   =  "  1"
                        IF (SPA-GMN-TSRYCD(IDU) = SYS-5001-BTU-KHNSRYCD)
                                               OR
                          ((SPA-GMN-TSRYCD (IDU)  = "190097010"   OR
                                                    "190097110"   OR
                                                    "190119510"   OR
                                                    "190119610")  AND
                           (SYS-5001-BTU-KHNSRYCD = "099999950"))
                                               OR
                          ((SPA-GMN-TSRYCD (IDU)  = "190097210"   OR
                                                    "190097310"   OR
                                                    "190119710"   OR
                                                    "190119810")  AND
                           (SYS-5001-BTU-KHNSRYCD = "099999951"))
                                               OR
                          ((SPA-GMN-TSRYCD (IDU)  = "190135110"   OR
                                                    "190135210"   OR
                                                    "190135310")  AND
                           (SYS-5001-BTU-KHNSRYCD = "099999952"))
                                               OR
                          ((SPA-GMN-TSRYCD (IDU)  = "190169610"   OR
                                                    "190169710"   OR
                                                    "190169810")  AND
                           (SYS-5001-BTU-KHNSRYCD = "099999955"))
                                               OR
                          ((SPA-GMN-TSRYCD (IDU)  = "190169910"   OR
                                                    "190170010"   OR
                                                    "190170110")  AND
                           (SYS-5001-BTU-KHNSRYCD = "099999956"))
                                               OR
                          ((SPA-GMN-TSRYCD (IDU)  = "190170210"   OR
                                                    "190170310"   OR
                                                    "190170410")  AND
                           (SYS-5001-BTU-KHNSRYCD = "099999957"))
                               CONTINUE
                        ELSE
                           MOVE   SPACE  TO  SPA-GMN-KEIKOKU
                           MOVE
               "算定入院料と病棟設定が異なります。確認してください。"
                                       TO      SPA-GMN-KEIKOKU
                        END-IF
                      END-IF
                     END-PERFORM
                   ELSE
                       INITIALIZE                  SYS-5001-REC
                        MOVE   SPACE   TO  SPA-GMN-KEIKOKU
                           MOVE
                     "病棟の情報が取得できません。確認してください。"
                                       TO      SPA-GMN-KEIKOKU
                   END-IF
                END-IF
               END-IF
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    サブプロによる入院点数チェック（改正時等の点数チェック）
           IF      SPA-GMN-KEIKOKU     =   SPACE
               MOVE    SPA-NAI-SRYYM   TO  SS007-SRYYM
               CALL    "ORCSS007"      USING  SS007-AREA
                                              SPA-AREA
               IF    SS007-RC          =   0
                   MOVE    SPACE       TO  SPA-GMN-KEIKOKU
               ELSE
            MOVE "入院点数に誤りがあります。異動処理をしてください。"
                                       TO  SPA-GMN-KEIKOKU
               END-IF
           END-IF
      *
      *    精神科急性期治療病棟入院料の３月越えチェック
           IF      SPA-GMN-KEIKOKU     =   SPACE
      *        入院履歴から当日の入院料が精神科急性期治療病棟入院料で
      *        あるかチェック
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-TENNYUYMD
               MOVE    PTNYUINRRK-TENNYUYMD TO PTNYUINRRK-TENSTUYMD
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key41"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE   "tbl_ptnyuinrrk" TO  MCP-TABLE
                   MOVE   "key41"          TO  MCP-PATHNAME
                   PERFORM 970-PTNYUINRRK-READ-SEC
                   IF  (FLG-PTNYUINRRK     =   ZERO)   AND
                       (PTNYUINRRK-TOKUTEI-KBN  =      "2")  AND
                       (PTNYUINRRK-TOKUTEI-NYUIN = "11" OR "12") AND
                       (PTNYUINRRK-TOKU-SANTEIYOUKENKBN  =  SPACE)
      *               精神科急性期治療病棟入院料の通算日数取得
                      INITIALIZE               ORCSTUSANAREA
                      MOVE    SPA-HOSPNUM  TO  STUSAN-HOSPNUM
                      MOVE    SPA-GMN-PTID TO  STUSAN-PTID
                      PERFORM 800-MCNDATE-SEC
                      MOVE    SMCNDATE-YMD TO  STUSAN-KJNYMD
                      EVALUATE    PTNYUINRRK-TOKUTEI-NYUIN
                          WHEN    "11"
                              MOVE   "190121110"
                                           TO  STUSAN-TOKUTEI-SRYCD(1)
                              MOVE   "190066910"
                                           TO  STUSAN-TOKUTEI-SRYCD(2)
                          WHEN    "12"
                              MOVE   "190121210"
                                           TO  STUSAN-TOKUTEI-SRYCD(1)
                              MOVE   "190067010"
                                           TO  STUSAN-TOKUTEI-SRYCD(2)
                      END-EVALUATE
                      CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                                      SPA-AREA
                      IF    ( STUSAN-RC       NOT =   ZERO )
      **                     DISPLAY "STUSAN-RC = " STUSAN-RC
                           MOVE    ZERO    TO  STUSAN-NISSU07
                      END-IF
      *               当該入院料算定日から３月経過時点の通算日数取得
                      MOVE    ZERO         TO     WRK-SEISHIN-START
                      MOVE    ZERO         TO     WRK-SEISHIN-END
                      MOVE    ZERO         TO     WRK-SEISHIN-NISSU
                      INITIALIZE                  STS-AREA-DAY
                      INITIALIZE                  LNK-DAY6-AREA
                      MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
                      MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                      MOVE    "key53"             TO  MCP-PATHNAME
                      PERFORM 910-DBSELECT-SEC
                      IF      MCP-RC              =   ZERO
                          MOVE   "tbl_ptnyuinrrk" TO  MCP-TABLE
                          MOVE   "key53"          TO  MCP-PATHNAME
                          PERFORM 970-PTNYUINRRK-READ-SEC
                          PERFORM  UNTIL
                                  (FLG-PTNYUINRRK NOT =  ZERO)  OR
                                  (LNK-DAY6-KIJUN NOT =  ZERO)
                              IF  (PTNYUINRRK-TOKUTEI-KBN = "2") AND
                                  (PTNYUINRRK-TOKUTEI-NYUIN
                                                =   "11"  OR  "12") AND
                                  (PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                                     = SPACE)
                                  MOVE   PTNYUINRRK-TENNYUYMD
                                                  TO  LNK-DAY6-KIJUN
      **                            DISPLAY "LNK-DAY6-KIJUN(SEI) = "
      **                                     LNK-DAY6-KIJUN
                                  MOVE     LNK-DAY6-KIJUN
                                                  TO  WRK-SEISHIN-START
                              END-IF
                              MOVE   "tbl_ptnyuinrrk" TO  MCP-TABLE
                              MOVE   "key53"      TO  MCP-PATHNAME
                              PERFORM 970-PTNYUINRRK-READ-SEC
                          END-PERFORM
                      END-IF
                      MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                      MOVE    "key53"             TO  MCP-PATHNAME
                      PERFORM 990-DBCLOSE-SEC
                      MOVE    "61"              TO  LNK-DAY6-IRAI
                      MOVE    "2"               TO  LNK-DAY6-ZOGENPTN
                      MOVE    3                 TO  LNK-DAY6-ZOGEN
                      CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                        LNK-DAY6-AREA
      **                DISPLAY "LNK-DAY6-KEISAN(SEI3) = " LNK-DAY6-KEISAN
                      MOVE    LNK-DAY6-KEISAN   TO  WRK-SEISHIN-END
      *               精神入院の３月マイナス１日の算出
                      INITIALIZE                    STS-AREA-DAY
                      INITIALIZE                    LNK-DAY6-AREA
                      MOVE    "61"              TO  LNK-DAY6-IRAI
                      MOVE    WRK-SEISHIN-END   TO  LNK-DAY6-KIJUN
                      MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                      MOVE    -1                TO  LNK-DAY6-ZOGEN
                      CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                        LNK-DAY6-AREA
      **                DISPLAY "LNK-DAY6-KEISAN(SEI3-1) = "
      **                         LNK-DAY6-KEISAN
                      MOVE    LNK-DAY6-KEISAN   TO  WRK-SEISHIN-END
      *               精神入院の３月に該当する通算日数の算出
                      INITIALIZE                    STS-AREA-DAY
                      INITIALIZE                    LNK-DAY5-AREA
                      MOVE    "51"              TO  LNK-DAY5-IRAI
                      MOVE    WRK-SEISHIN-START TO  LNK-DAY5-START
                      MOVE    WRK-SEISHIN-END   TO  LNK-DAY5-END
                      CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                        LNK-DAY5-AREA
      **                DISPLAY "LNK-DAY5-NISSU2 = " LNK-DAY5-NISSU2
      *               当日の通算日数算出の為、１日加算
                      ADD       1     TO     STUSAN-NISSU07
      **                DISPLAY   "SEISHINNKA STUSAN-NISSU07 = "
      **                           STUSAN-NISSU07
      **                DISPLAY   "LNK-DAY5-NISSU2 = "
      **                           LNK-DAY5-NISSU2
      *               通算日数取得の比較
                      IF   STUSAN-NISSU07        <=  LNK-DAY5-NISSU2
                          MOVE    SPACE          TO  SPA-GMN-KEIKOKU
                      ELSE
            MOVE "精神科急性期治療病棟入院料の算定が３月を越えています"
                                                 TO  SPA-GMN-KEIKOKU
                      END-IF
                   END-IF
               END-IF
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key41"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
      *    生活療養でない入院料算定時の警告チェック
           IF      SPA-GMN-KEIKOKU     =   SPACE
               IF    SS007-RC2          =   0
                   MOVE    SPACE       TO  SPA-GMN-KEIKOKU
               ELSE
            MOVE "生活療養の入院料が算定されていない期間があります。"
                                       TO  SPA-GMN-KEIKOKU
               END-IF
           END-IF
           .
      *
       31062-SANTEI-NYUINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-DATALIST-ROW
           ELSE
               MOVE    I41-DATELIST-ROW    TO  WRK-DATALIST-ROW
           END-IF
      *
           MOVE    SPACE               TO  I41.
           INITIALIZE                      I41
      *    コラムリストの位置指定
           MOVE    ZERO                TO  I41-DATELIST-ROW
           MOVE    ZERO                TO  I41-DATELIST-ROWATTR
           IF      WRK-DATALIST-ROW    NOT =   ZERO
               MOVE    WRK-DATALIST-ROW    TO  I41-DATELIST-ROW
           END-IF
     *
           MOVE    SPA-GMN-B05-LABEL-41
                                       TO  I41-B05-LABEL
      *
           MOVE    SPA-GMN-PTNUM       TO  I41-PTNUM.
           MOVE    SPA-GMN-KANANAME    TO  I41-KANANAME.
           MOVE    SPA-GMN-NAME        TO  I41-NAME.
           MOVE    SPA-GMN-SEX         TO  I41-SEX.
           MOVE    SPA-GMN-SRYYM       TO  I41-SRYYM.
           MOVE    SPA-GMN-BIRTHDAYW   TO  I41-BIRTHDAY.
           MOVE    SPA-GMN-AGE         TO  I41-AGE.
           MOVE    SPA-GMN-IJYOU91-STYLE
                                       TO  I41-IJYOU91-STYLE.
           MOVE    SPA-GMN-IJYOU91     TO  I41-IJYOU91.
           MOVE    SPA-GMN-IJYOU180    TO  I41-IJYOU180.
           MOVE    SPA-GMN-NYUINBI     TO  I41-NYUINBI.
           MOVE    SPA-GMN-TAINBI      TO  I41-TAINBI.
           MOVE    SPA-GMN-NYUINNISSU  TO  I41-NYUINNISSU.
           MOVE    SPA-GMN-TSUSANNISSU TO  I41-TSUSANNISSU.
      *    入院科
           PERFORM      3103-NYUINKA-EDIT-SEC
           MOVE    "black"             TO  I41-NYUINKA-STYLE.
           MOVE    SPA-GMN-NYUINKA     TO  I41-NYUINKA.
      *    病棟種別日数
           MOVE    "black"             TO  I41-SYUBETSU1-STYLE.
           MOVE    "black"             TO  I41-SYUBETSU2-STYLE.
           MOVE    "black"             TO  I41-SYUBETSU3-STYLE.
           MOVE    "black"             TO  I41-SYUBETSU4-STYLE.
           MOVE    "black"             TO  I41-SYUBETSU5-STYLE.
           MOVE    "black"             TO  I41-SYUBETSU6-STYLE.
           MOVE    "black"             TO  I41-LASTNYUIN-STYLE.
           MOVE    SPA-GMN-SYUBETSU1   TO  I41-SYUBETSU1.
           MOVE    SPA-GMN-SYUBETSU2   TO  I41-SYUBETSU2.
           MOVE    SPA-GMN-SYUBETSU3   TO  I41-SYUBETSU3.
           MOVE    SPA-GMN-SYUBETSU4   TO  I41-SYUBETSU4.
           MOVE    SPA-GMN-SYUBETSU5   TO  I41-SYUBETSU5.
           MOVE    SPA-GMN-SYUBETSU6   TO  I41-SYUBETSU6.
           MOVE    SPA-GMN-LASTNYUIN   TO  I41-LASTNYUIN.
      *    食事更新日（改正作業中の為、更新日表示を保留とする）
      **     MOVE    "blue"              TO  I41-UPYMD-STYLE.
      **     IF      SPA-GMN-UPYMD       NOT =  SPACE
      **         MOVE  "会計更新日"      TO  I41-UPYMD(1:10)
      **         MOVE    SPA-GMN-UPYMD   TO  WRK-SYMD
      **         PERFORM 41012-SEIWA-HEN-SEC
      **         MOVE    WRK-HENYMDG     TO  I41-UPYMD(12:9)
      **     END-IF
      *    保険組合せ
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-HKNCOMBI-MAX
               MOVE    SPA-I41-THKNCOMBI (IDX)
                                           TO  I41-KUMINO (IDX)
               MOVE    SPA-I41-THKNCOMBIMEI (IDX)(1:32)
                                           TO  I41-KUMI (IDX)
           END-PERFORM.
           MOVE    SPA-HKNCOMBI-MAX    TO  I41-KUMILIST-COUNT
      *
      *    室料差額
           MOVE    SPA-GMN-SAGAKU-TITLE-41 TO  I41-SAGAKU-TITLE
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-SAGAKU-MAX-41
               MOVE    SPA-GMN-SAGAKULIST-41 (IDX)
                                          TO  I41-SAGAKU-ITEM (IDX)
           END-PERFORM.
           MOVE    SPA-SAGAKU-MAX-41   TO  I41-SAGAKULIST-COUNT.
      *
      *    曜日
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   31
               MOVE    "black"         TO  I41-TLBLDAY-STYLE (IDX)
               EVALUATE    SPA-NAI-YOBI (IDX)
               WHEN    CONST-SATURDAY
                   MOVE    "blue"      TO  I41-TLBLDAY-STYLE (IDX)
               WHEN    CONST-SUNDAY
                   MOVE    "red"       TO  I41-TLBLDAY-STYLE (IDX)
               END-EVALUATE
           END-PERFORM
      *
      *    明細
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-GMN-MAX-41
               IF      SPA-GMN-TMEISYO-41 (IDX)    =   ALL "-"
                   MOVE    ALL "-"         TO  I41-DATELIST-ITEM (IDX)
                   MOVE    SPACE           TO  I41-TMEISYO (IDX)
                   MOVE    ALL "-"         TO  I41-TMEISYO (IDX)(1:30)
               ELSE
                   IF      SPA-GMN-TBANGO (IDX) >   ZERO
                       MOVE    SPA-GMN-TBANGO  (IDX)
                                                   TO  WRK-ZZZZ
                       MOVE    WRK-ZZZZ-X          TO  I41-TNO  (IDX)
                   END-IF
                   MOVE    SPA-GMN-TMEISYO-41 (IDX)
                                                   TO  I41-TMEISYO(IDX)
      **             DISPLAY "SPA-GMN-TMEISYO-41 (IDX) = "
      **                      SPA-GMN-TMEISYO-41 (IDX)
                   MOVE    SPA-GMN-TTENSU-41  (IDX)
                                                   TO  I41-TTENSU (IDX)
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
      **                 DISPLAY "IDY = " IDY
      **                 DISPLAY "SPA-GMN-TDAY-41 (IDX IDY) = "
      **                          SPA-GMN-TDAY-41 (IDX IDY)
                       IF   SPA-GMN-TDAY-41 (IDX IDY)(3:1)  NOT =  SPACE
                           MOVE    SPA-GMN-TDAY-41 (IDX IDY)(2:2)
                                               TO  I41-TDAY (IDX IDY)
                       ELSE
                           MOVE    SPA-GMN-TDAY-41 (IDX IDY)(1:2)
                                               TO  I41-TDAY (IDX IDY)
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF  (SPA-GMN-TMEISYO-41 (IDX) NOT =   ALL "-")  AND
                   (SPA-GMN-TBANGO (IDX)  NOT =   ZERO)     AND
                   (SPA-GMN-TBANGO (IDX)      =   SPA-NAI-NUM-41)
                   MOVE    "T"             TO  I41-DATELIST-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  I41-DATELIST-SELECT (IDX)
               END-IF
           END-PERFORM.
           MOVE    SPA-GMN-MAX-41      TO  I41-DATELIST-COUNT.
      *
      *    変更番号
           MOVE    SPA-GMN-NUM-41      TO  I41-NUM
           MOVE    SPA-GMN-MEISYO-41   TO  I41-MEISYO.
      *    診療回数
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   31
      *
               MOVE    IDX             TO  WRK-ZZ
               IF    ( IDX             <   10 )
                   MOVE    WRK-ZZ-X (2:1)  TO  I41-LDAY (IDX)
               ELSE
                   MOVE    WRK-ZZ-X        TO  I41-LDAY (IDX)
               END-IF
      *
               MOVE    "black"             TO  I41-LDAY-STYLE (IDX)
               EVALUATE    SPA-NAI-YOBI (IDX)
               WHEN    CONST-SATURDAY
                   MOVE    "blue"          TO  I41-LDAY-STYLE (IDX)
               WHEN    CONST-SUNDAY
                   MOVE    "red"           TO  I41-LDAY-STYLE (IDX)
               END-EVALUATE
      *
               IF      SPA-GMN-DAY-41 (IDX)    >=  100
                   MOVE    SPA-GMN-DAY-41 (IDX)    TO  WRK-Z99
                   MOVE    WRK-Z99-X               TO  I41-DAY (IDX)
               ELSE
                   MOVE    SPA-GMN-DAY-41 (IDX)    TO  WRK-ZZ
                   MOVE    WRK-ZZ-X              TO  I41-DAY (IDX)
               END-IF
           END-PERFORM
      *    一括修正
           MOVE    SPA-GMN-KOMENTO-41  TO  I41-KOMENTO.
      *    保険切れ警告（または病棟設定と算定入院料不一致警告）
           IF      SPA-GMN-KEIKOKU     NOT =   SPACE
               MOVE    "red"               TO  I41-KEIKOKU-STYLE
               MOVE    SPA-GMN-KEIKOKU     TO  I41-KEIKOKU
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  I41-B05S-STATE
           MOVE    "ＡＤＬ入力"        TO  I41-B05S-LABEL
           IF    ( SPA-5000-ADLINPUTKBN    =   "0" )
               MOVE    SPACE               TO  I41-B05S-LABEL
               MOVE    WIDGET-INSENSITIVE  TO  I41-B05S-STATE
           END-IF
      *
           IF    ( SPA-NAI-LOCK            =   1 )
               MOVE    WIDGET-INSENSITIVE  TO  I41-B05S-STATE
           END-IF
      *
      *    カーソル
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR-41
               WHEN    00
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    01
                   MOVE    "NUM"           TO  MCP-WIDGET
               WHEN    02
                   MOVE    "DAY"           TO  MCP-WIDGET
                   IF      SPA-DAY-IDX-41      =   ZERO  OR
                                               >   SPA-NAI-SRYMATU
                       MOVE    1               TO  SPA-DAY-IDX-41
                   END-IF
                   IF     (SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM) AND
                          (SPA-DAY-IDX-41      <   SPA-BIRTHDAY(7:2))
                       MOVE    SPA-BIRTHDAY(7:2)   TO  SPA-DAY-IDX-41
                   END-IF
                   MOVE    SPA-DAY-IDX-41      TO  MCP-WIDGET(4:2)
               WHEN    03
                   MOVE    "KOMENTO"       TO  MCP-WIDGET
               WHEN    05
                   MOVE    "B08"           TO  MCP-WIDGET
               WHEN    06
                   MOVE    "NYUINKA"       TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SRYYM"         TO  MCP-WIDGET
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEA-SEC
      *    剤変更へ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 420-ZAIHENKOU-SEC
      *    前回患者処理
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-MAEKPTNUM-SEC
      *    チェック処理
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 430-CHEKU-SEC
      *    フリーコメント処理
               WHEN    "CLICKED"       ALSO    "B04S"
                   PERFORM 430-FREEMSG-SEC
      *    室料差額リスト編集処理
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-SAGAKULIST-HEN-SEC
      *    ＡＤＬ区分・医療区分入力画面へ
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 450-I47-SENI-SEC
      *    前月選択
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 41018-ZENGETU-HEN-SEC
      *    次月選択
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 41019-JIGETU-HEN-SEC
      *    変更確定
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-KAKUTEI-SEC
      *    氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOROKU-SEC
      *ver.4.7
      *    プレビュー
               WHEN    "CLICKED"       ALSO    "B01S"
                   PERFORM 4010-PREVIEW-SYORI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41010-PTNUM-SYORI-SEC
                   PERFORM 3006-YOBI-HEN-SEC
      *        入院科入力
               WHEN    "ACTIVATE"      ALSO    "NYUINKA"
                   PERFORM 41013-NYUINKA-SYORI-SEC
      *        診療年月
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 41018-SRYYMD-SYORI-SEC
                   PERFORM 3006-YOBI-HEN-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "NUM"
                   PERFORM 41011-BANGO-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "DATELIST"
                   PERFORM 41012-DATALIST-SEC
      *        一括修正
               WHEN    "ACTIVATE"      ALSO    "KOMENTO"
                   PERFORM 41017-KOMENTO-CHK-SEC
      *
               WHEN    OTHER
      *        剤回数
                   IF    (MCP-EVENT       =   "ACTIVATE") AND
                         (MCP-WIDGET(1:3) =   "DAY")
                       MOVE    MCP-WIDGET(4:2)     TO  SPA-DAY-IDX-41
                       PERFORM 41012-DAY-CHK-SEC
                   END-IF
           END-EVALUATE.
      *
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       41010-PTNUM-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR-41
      *
           MOVE    I41-PTNUM           TO  SPA-GMN-PTNUM.
      *
           INITIALIZE                      ORCSPTNUMAREA.
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM.
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA.
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
               WHEN   10
                   MOVE    ZERO                TO  SPA-GMN-CUR-41
                   INITIALIZE                  SPA-I4-AREA
                   INITIALIZE                  SPA-SCR-REC-41
      *            表示対象診療会計作業クリア
                   INITIALIZE                  SPA-I40-SET
                   INITIALIZE                  SPA-HKNCOMBI-AREA-41
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM
                   MOVE    "0017"              TO  SPA-ERRCD
                   GO      TO     41010-PTNUM-SYORI-EXT
               WHEN    20
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
      *            氏名検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-I4KYOUTU        TO  SPA-WORK
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-I41             TO  LNK-SCRSPA
      *
                   MOVE    "I41"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
      *            氏名セット
                   MOVE    SPA-GMN-PTNUM       TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
                   GO      TO     41010-PTNUM-SYORI-EXT
               WHEN    OTHER
                   MOVE    ZERO                TO  SPA-GMN-CUR-41
                   INITIALIZE                  SPA-I4-AREA
                   INITIALIZE                  SPA-SCR-REC-41
      *            表示対象診療会計作業クリア
                   INITIALIZE                  SPA-I40-SET
                   INITIALIZE                  SPA-HKNCOMBI-AREA-41
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM
                   MOVE    "0017"              TO  SPA-ERRCD
                   GO      TO     41010-PTNUM-SYORI-EXT
           END-EVALUATE.
      *
           INITIALIZE                  SPA-I4-AREA
           INITIALIZE                  SPA-SCR-REC-AREA
           INITIALIZE                  SPA-GMN-NYUIN-AREA
           INITIALIZE                  SPA-HKNCOMBI-AREA-41
           MOVE    SPA-I40-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
           MOVE    SPA-I40-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
      *
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM.
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID.
      *    患者マスター存在チェック
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM.
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID.
           PERFORM 900-PTINF-READ-SEC.
           IF      FLG-PTINF      NOT =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR-41
               INITIALIZE                  SPA-I4-AREA
               INITIALIZE                  SPA-SCR-REC-41
      *        表示対象診療会計作業クリア
               INITIALIZE                  SPA-I40-SET
               INITIALIZE                  SPA-HKNCOMBI-AREA-41
               MOVE    SPTID-PTNUM             TO  SPA-GMN-PTNUM
               MOVE    SPA-I40-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-I40-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
               MOVE    "0017"              TO  SPA-ERRCD
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF.
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME.
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME.
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE.
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-GMN-BIRTHDAY.
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD.
           PERFORM 41012-SEIWA-HEN-SEC.
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAYW.
      *
      *    年齢計算
           INITIALIZE                ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM       TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID      TO  AGECHK-PTID
           MOVE    SPA-SYSYMD        TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY    TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"      USING
                                     ORCSAGECHKAREA
                                     SPA-AREA
           MOVE    SPACE             TO  WRK-NENREI-G
           IF      AGECHK-NENREI2-YY   =   ZERO
                MOVE    AGECHK-NENREI2-MM   TO  WRK-NENREI-ZN
                MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
                MOVE    AGECHK-NENREI2-YY   TO  WRK-NENREI-Z
                MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G      TO  SPA-GMN-AGE
      *
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1       TO  SPA-NAI-LOCK
               GO      TO      41010-PTNUM-SYORI-EXT
           ELSE
               MOVE    ZERO    TO  SPA-NAI-LOCK
           END-IF
      *
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
      *    前回患者へ待避
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
      *    初期表示
           PERFORM 310-SPA-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    患者入院履歴編集
           PERFORM 3103-PTNYUINRRK-HEN-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    入院会計マスターより編集をする
           PERFORM 3101-NYUINACCT-HEN-SEC.
      *
      *    保険組合一覧編集
           PERFORM 3105-HKNCOMBI-HEN-SEC.
      *
      *    保険組合期限切れチェック、外泊の再編集
           PERFORM 3106-DAY-KIKANCHK-SEC
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    07                  TO  SPA-GMN-CUR-41
      *********MOVE    SPACE               TO  SPA-ERRCD
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       41010-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院科入力処理
      *****************************************************************
       41013-NYUINKA-SYORI-SEC        SECTION.
      *
           IF      SPA-NAI-NUM-41  NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41013-NYUINKA-SYORI-EXT
           END-IF.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41.
      *
           MOVE    SPA-GMN-SRYKA-IG-41 TO  SPA-MAE-SRYKA-G-41.
           MOVE    I41-NYUINKA         TO  SPA-GMN-SRYKA-IG-41.
      *    入院科選択なし
           IF     (SPA-GMN-SRYKA-I-41  =   SPACE)   OR
                  (SPA-GMN-PTID        =   ZERO)
               GO      TO      41013-NYUINKA-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SRYKA-MAX-41) OR
                              (FLG-CHK     =   1   )
               IF      SPA-GMN-SRYKA-I-41  =   SPA-GMN-SRYKA-L-41(IDX)
                   MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  SPA-GMN-SRYKA-IG-41
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *    科が無い時、前の科に戻す
           IF      FLG-CHK             =   ZERO
               MOVE    SPA-MAE-SRYKA-G-41  TO  SPA-GMN-SRYKA-IG-41
               GO      TO      41013-NYUINKA-SYORI-EXT
           END-IF
      *
      *    入院科選択
           IF     (SPA-MAE-SRYKA-41    =   SPACE)  OR
                  (SPA-UPDFLG-41       =   SPACE)
               PERFORM 41013-NYUINKA-CHANGE-SEC
               GO      TO      41013-NYUINKA-SYORI-EXT
           END-IF.
      *
      *    確認表示
           MOVE    "0107"              TO  SPA-IIDCD.
      *
       41013-NYUINKA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院科変更処理
      *****************************************************************
       41013-NYUINKA-CHANGE-SEC            SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-GMN-MAX-41   >   ZERO
               IF      SPA-UPDFLG-41          =   "1"
                   PERFORM 4901-NYUINACCT-UPD-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO       41013-NYUINKA-CHANGE-EXT
                   END-IF
               END-IF
           END-IF.
      *
           MOVE    SPA-GMN-SRYKAMEI-I-41 TO  SPA-I40-SRYKAMEI.
           MOVE    SPA-GMN-SRYKA-I-41    TO  SPA-I40-SRYKA.
      *
      *    入院会計マスターより編集をする
           PERFORM 3101-NYUINACCT-HEN-SEC.
      *
      *    保険組合期限切れチェック、外泊の再編集
           PERFORM 3106-DAY-KIKANCHK-SEC
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    06                  TO  SPA-GMN-CUR-41
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       41013-NYUINKA-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月入力処理
      *****************************************************************
       41018-SRYYMD-SYORI-SEC             SECTION.
      *
           IF      SPA-NAI-NUM-41  NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41018-SRYYMD-SYORI-EXT
           END-IF
      *
           MOVE    07                  TO  SPA-GMN-CUR-41
      *H29.3
           MOVE    SPA-GMN-SRYYM       TO  SPA-MAE-GMN-SRYYM
           MOVE    SPA-NAI-SRYYM       TO  SPA-MAE-NAI-SRYYM
      *
           MOVE    I41-SRYYM           TO  SPA-GMN-SRYYM
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYM       TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
      *        患者の確定してない時
               IF      SPA-GMN-PTID        =   ZERO
                   MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM
                   MOVE    ZERO                TO  SPA-GMN-CUR-41
                   GO      TO      41018-SRYYMD-SYORI-EXT
               END-IF
      *        誕生日以前の時、エラー
               IF      SGDAY-SDAY(1:6)     <   SPA-BIRTHDAY(1:6)
                    MOVE    "0015"             TO  SPA-ERRCD
                    GO      TO      41018-SRYYMD-SYORI-EXT
               END-IF
      *        変更のない時、以降の処理はそのまま
               IF      SGDAY-SDAY(1:6)     =   SPA-NAI-SRYYM
                   IF      SPA-GMN-MAX-41      >   ZERO
                        MOVE    1                   TO  SPA-GMN-CUR-41
                        GO      TO      41018-SRYYMD-SYORI-EXT
                   END-IF
               END-IF
               MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM
           ELSE
               MOVE    "0018"          TO  SPA-ERRCD
               GO      TO      41018-SRYYMD-SYORI-EXT
           END-IF
      *
           IF      SPA-UPDFLG-41       =   SPACE
               PERFORM 410181-SRYYM-CHANGE-SEC
           ELSE
               MOVE    "0108"              TO  SPA-IIDCD
           END-IF
      *
           .
       41018-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月変更の確認処理
      *****************************************************************
       410181-SRYYM-CHANGE-SEC             SECTION.
      *
      *    入院会計マスター更新
           IF      SPA-UPDFLG-41       =   "1"
               MOVE    SPA-NAI-SRYYM       TO  WRK-NAI-SRYYM
               MOVE    SPA-GMN-SRYYM       TO  WRK-GMN-SRYYM
      **       MOVE    SPA-I40-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
      **       MOVE    SPA-I40-SRYYMD(1:6)     TO  SPA-NAI-SRYYM
      *H29.3
               MOVE    SPA-MAE-GMN-SRYYM    TO  SPA-GMN-SRYYM
               MOVE    SPA-MAE-NAI-SRYYM    TO  SPA-NAI-SRYYM
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO       410181-SRYYM-CHANGE-EXT
               END-IF
               MOVE    WRK-NAI-SRYYM       TO  SPA-NAI-SRYYM
               MOVE    WRK-GMN-SRYYM       TO  SPA-GMN-SRYYM
           END-IF.
      *
           MOVE    SPACE               TO  SPA-I40-SRYKA
           MOVE    SPACE               TO  SPA-I40-SRYKAMEI.
           INITIALIZE                  SPA-GMN-AREA2
      *
           MOVE    SPA-NAI-SRYYM       TO  SPA-I40-SRYYMD(1:6)
           MOVE    SPA-GMN-SRYYM       TO  SPA-I40-SRYYMDW(1:6)
      *
      *
      *    初期表示
           PERFORM 310-SPA-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410181-SRYYM-CHANGE-EXT
           END-IF
      *
      *    患者入院履歴編集
           PERFORM 3103-PTNYUINRRK-HEN-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410181-SRYYM-CHANGE-EXT
           END-IF
      *
      *    入院会計マスターより編集をする
           PERFORM 3101-NYUINACCT-HEN-SEC.
      *
      *    保険組合一覧編集
           PERFORM 3105-HKNCOMBI-HEN-SEC.
      *
      *    保険組合期限切れチェック、外泊の再編集
           PERFORM 3106-DAY-KIKANCHK-SEC
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    07                  TO  SPA-GMN-CUR-41
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       410181-SRYYM-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択入力処理
      *****************************************************************
       41011-BANGO-CHK-SEC             SECTION.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41
      *
           IF      I41-NUM             =   SPACE
               MOVE    ZERO            TO  I41-NUM
           END-IF
           MOVE    I41-NUM             TO  SPA-GMN-NUM-41
           INITIALIZE                      ORCSNUMAREA.
           MOVE    SPA-GMN-NUM-41      TO  SNUM-INX.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF     (SNUM-RC         NOT =   ZERO)    OR
                  (SNUM-MINKBN     NOT =   SPACE)   OR
                  (SNUM-SYOKBN     NOT =   SPACE)
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI-NUM-41
               MOVE    SPA-NAI-NUM-41      TO  WRK-ZZZZ 
               MOVE    WRK-ZZZZ            TO  SPA-GMN-NUM-41
           END-IF.
      *
           IF      SPA-NAI-NUM-41    =   ZERO
               INITIALIZE                  SPA-GMN-INPUT-41
               MOVE    ZERO                TO  SPA-I4-SYORI
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    ZERO                TO  SPA-DAY-IDX-41
               GO      TO      41011-BANGO-CHK-EXT
           END-IF
      *
      *    選択番号チェック
           MOVE    ZERO                TO  SPA-NAI-BANGO
           MOVE    ZERO                TO  WRK-BANGO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX-41
               IF      SPA-GMN-TBLREC(IDX)(1:3)    =   "---"
                   CONTINUE
               ELSE
                   IF      SPA-NAI-NUM-41      =   SPA-GMN-TBANGO  (IDX)
                       MOVE    IDX             TO  WRK-BANGO
                       MOVE    SPA-GMN-MAX-41  TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-BANGO           =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NAI-MAX-41
               IF      WRK-BANGO       =   SPA-NAI-TBANGO (IDX)
                   MOVE    IDX             TO  SPA-NAI-BANGO
                   MOVE    SPA-NAI-MAX-41  TO  IDX
               END-IF
           END-PERFORM
      *R01.5
           IF     (SPA-NAI-NUM-41  NOT =   ZERO )
           AND    (SPA-NAI-BANGO       =   ZERO )
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           END-IF
      *
      *    入院料であること
           IF      SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                       =   "S"
               MOVE    "0045"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           END-IF
      *
      *    内容編集
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)
                                           TO  IDG
           MOVE    SPA-GMN-TMEISYO-41  (IDG)
                                           TO  SPA-GMN-MEISYO-41
           MOVE    SPA-NAI-DAY-AREA-41 (SPA-NAI-BANGO)
                                           TO  SPA-GMN-DAY-G-41
      *    一括修正（９９）可能判定
           IF     (SPA-GMN-TZAISKBKBN-41(IDG)  =   "3"
                                               OR  "4"  OR  "5") OR
                 ((SPA-GMN-TZAISKBKBN-41 (IDG) =  "1" ) AND
                  (SPA-GMN-TSRYCD (IDG)     =  W-YOYAKU ))
               MOVE    1               TO  SPA-NAI-IKKATUFLG
           ELSE
               MOVE    ZERO            TO  SPA-NAI-IKKATUFLG
           END-IF
      *
           EVALUATE    SPA-GMN-TZAISKBKBN-41  (IDG)
      *        外泊
               WHEN "2"
                   PERFORM 3113-GAIHAKU-HEN-SEC
      *        室料差額
               WHEN "3"
                   PERFORM 3113-SAGAKU-HEN-SEC
      *        食事
               WHEN "4"
                   PERFORM 3113-SHOKUJI-HEN-SEC
           END-EVALUATE
      *
           MOVE    3                   TO  SPA-GMN-CUR-41
           MOVE    ZERO                TO  SPA-DAY-IDX-41
           .
      *
       41011-BANGO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤回数入力処理
      *****************************************************************
       41012-DAY-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR-41
      *
      *    誕生日以前はエラー
           IF      SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM
               IF      SPA-DAY-IDX-41  <   SPA-BIRTHDAY(7:2)
                   MOVE    "0015"              TO  SPA-ERRCD
                   GO      TO      41012-DAY-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-NAI-NUM-41      =   ZERO
      *R01.5
             OR   (SPA-NAI-BANGO       =   ZERO )
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      41012-DAY-CHK-EXT
           END-IF
      *
      *    一括入力とする
           MOVE    I41-KOMENTO         TO  SPA-GMN-KOMENTO-41
      *
           MOVE    ZERO                TO  WRK-IDX
           INITIALIZE                  SPA-GMN-DAY-G-41
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   31
               INITIALIZE                      ORCSNUMAREA
               MOVE    I41-DAY(IDX)        TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      WRK-IDX             =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                       MOVE    IDX                 TO  WRK-IDX
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM     TO  SPA-GMN-DAY-41 (IDX)
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-IDX         TO  SPA-DAY-IDX-41
               GO      TO     41012-DAY-CHK-EXT
           END-IF
      *
      *    日付のチェック
      *    入院時生活療養費の情報取得
           INITIALIZE                      ORCSNYULIFE2-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYULIFE2-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  LNK-NYULIFE2-PTID
           MOVE    SPA-NAI-SRYYM       TO  LNK-NYULIFE2-SANTEIYM
           CALL    "ORCSNYULIFE2"   USING  ORCSNYULIFE2-AREA
                                           SPA-AREA
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  WRK-IDX
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-NAI-SRYMATU )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-DAY-41 (IDX-D)  NOT =   ZERO
                   PERFORM 410121-DAY-NAI-CHK-SEC
               ELSE
                   IF      SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO) =  "5"
                       PERFORM 410122-DAY-NASI-CHK-SEC
                   END-IF
               END-IF
      *        入院期間外等の時、元へ戻す
               IF      SPA-ERRCD           =   "0033"
                                           OR  "0032"
                                           OR  "0039"
                                           OR  "0042"
                                           OR  "0046"
                                           OR  "0053"
                                           OR  "0054"
                                           OR  "0055"
                   MOVE    SPA-NAI-DAY-41(SPA-NAI-BANGO IDX-D)
                                           TO  SPA-GMN-DAY-41 (IDX-D)
                   MOVE    SPA-ERRCD       TO  WRK-ERRCD
                   MOVE    IDX-D           TO  WRK-IDX
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    IDX-D           TO  SPA-DAY-IDX-41
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     41012-DAY-CHK-EXT
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
               MOVE    WRK-IDX             TO  SPA-DAY-IDX-41
               GO      TO     41012-DAY-CHK-EXT
           END-IF
      *
           ADD     1                   TO  SPA-DAY-IDX-41.
           IF      SPA-DAY-IDX-41      >   SPA-NAI-SRYMATU
               MOVE    5                   TO  SPA-GMN-CUR-41
           END-IF
           .
      *
       41012-DAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    識別毎の診療回数チェック処理
      *****************************************************************
       410121-DAY-NAI-CHK-SEC             SECTION.
      *
           EVALUATE    SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
      *        入院料
               WHEN    "1"
                   IF      SPA-GMN-TSRYCD (IDG)    =   W-YOYAKU
      *                選定入院料
                       IF      SPA-CHK-NYUIN (IDX-D)   =   SPACE
                            MOVE    "0033"             TO  SPA-ERRCD
                       END-IF
                   ELSE
      *                入院基本料
                       IF      SPA-CHK-DAY (IDX-D)     =   SPACE
                            MOVE    "0032"             TO  SPA-ERRCD
                       END-IF
                   END-IF
                   IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO  OR  1
                       CONTINUE
                   ELSE
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "0034"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *        外泊
               WHEN    "2"
      *            入院基本料算定なし
                   IF      SPA-CHK-NYUIN (IDX-D)   =   SPACE
                        MOVE    "0033"             TO  SPA-ERRCD
                        GO      TO      410121-DAY-NAI-CHK-EXT
                   END-IF
      *            医療観察法の入院時は外泊セット不可
                   IF      SPA-CHK-IRYOKNS (IDX-D)   NOT =   SPACE
                        MOVE    "0054"             TO  SPA-ERRCD
                        GO      TO      410121-DAY-NAI-CHK-EXT
                   END-IF
      *
                   IF      SPA-CHK-SENTEI(IDX-D)   =   SPACE
                      IF   SPA-NAI-SRYYM         <=    "201003"
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  1
                                                           OR  2
                                                           OR  3
                                                           OR  4
                               CONTINUE
                           ELSE
                               MOVE    "0035"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF     (SPA-NAI-SRYYM      >=    "201004") AND
                             (SPA-NAI-SRYYM      <=    "201203")
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  1
                                                           OR  2
                                                           OR  3
                                                           OR  4
                                                           OR  5
                               CONTINUE
                           ELSE
                               MOVE    "0035"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF  (SPA-NAI-SRYYM         >=    "201204") AND
                          (SPA-NAI-SRYYM         <=    "201209")
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  1
                                                           OR  2
                                                           OR  3
                                                           OR  4
                                                           OR  5
                                                           OR  6
                                                           OR  7
                               CONTINUE
                           ELSE
                               MOVE    "0035"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF  (SPA-NAI-SRYYM         >=    "201210") AND
                          (SPA-NAI-SRYYM         <=    "201603")
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  1
                                                           OR  2
                                                           OR  3
                                                           OR  4
                                                           OR  5
                                                           OR  6
                                                           OR  7
                                                           OR  8
                                                           OR  9
                                                           OR 13
                               CONTINUE
                           ELSE
                               MOVE    "0035"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF  (SPA-NAI-SRYYM         >=    "201604") AND
                          (SPA-NAI-SRYYM         <=    "201803")
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  1
                                                           OR  2
                                                           OR  3
                                                           OR  4
                                                           OR  5
                                                           OR  6
                                                           OR  8
                                                           OR  9
                                                           OR 13
                               CONTINUE
                           ELSE
                               MOVE    "0035"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF      SPA-NAI-SRYYM         >=    "201804"
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  1
                                                           OR  2
                                                           OR  3
                                                           OR  4
                                                           OR  5
                                                           OR  6
                                                           OR  8
                                                           OR  9
                                                           OR 13
                                                           OR 14
                                                           OR 15
                                                           OR 16
                                                           OR 17
                               CONTINUE
                           ELSE
                               MOVE    "0035"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                   ELSE
      *               選定入院料算定日
                      IF      SPA-NAI-SRYYM      <=    "201003"
                           IF          SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  3
                                                           OR  4
                               CONTINUE
                           ELSE
                               MOVE    "0036"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF     (SPA-NAI-SRYYM      >=    "201004") AND
                             (SPA-NAI-SRYYM      <=    "201203")
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  3
                                                           OR  4
                                                           OR  5
                               CONTINUE
                           ELSE
                               MOVE    "0036"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF  (SPA-NAI-SRYYM         >=    "201204") AND
                          (SPA-NAI-SRYYM         <=    "201209")
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  3
                                                           OR  4
                                                           OR  5
                                                           OR  6
                                                           OR  7
                               CONTINUE
                           ELSE
                               MOVE    "0036"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF  (SPA-NAI-SRYYM         >=    "201210") AND
                          (SPA-NAI-SRYYM         <=    "201603")
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  3
                                                           OR  4
                                                           OR  5
                                                           OR  6
                                                           OR  7
                                                           OR  8
                                                           OR  9
                                                           OR 13
                               CONTINUE
                           ELSE
                               MOVE    "0036"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF  (SPA-NAI-SRYYM         >=    "201604") AND
                          (SPA-NAI-SRYYM         <=    "201803")
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  3
                                                           OR  4
                                                           OR  5
                                                           OR  6
                                                           OR  8
                                                           OR  9
                                                           OR 13
                               CONTINUE
                           ELSE
                               MOVE    "0036"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                      IF   SPA-NAI-SRYYM         >=    "201804"
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  3
                                                           OR  4
                                                           OR  5
                                                           OR  6
                                                           OR  8
                                                           OR  9
                                                           OR 13
                                                           OR 14
                                                           OR 15
                                                           OR 16
                                                           OR 17
                               CONTINUE
                           ELSE
                               MOVE    "0036"         TO  SPA-ERRCD
                           END-IF
                      END-IF
                   END-IF
      *
      *            特定曜日入退院減算の土日曜日チェック
                   IF     SPA-NAI-SRYYM      >=    "201210"
                     IF  SPA-GMN-DAY-41 (IDX-D)   =   9   OR  13
                       IF   SPA-NAI-YOBI(IDX-D)   =  "6"  OR  "7"
                           CONTINUE
                       ELSE
                           MOVE  "0055"           TO  SPA-ERRCD
                       END-IF
                     END-IF
                   END-IF
                   IF     SPA-NAI-SRYYM      >=    "201804"
                     IF  SPA-GMN-DAY-41 (IDX-D)   =   17
                       IF   SPA-NAI-YOBI(IDX-D)   =  "6"  OR  "7"
                           CONTINUE
                       ELSE
                           MOVE  "0055"           TO  SPA-ERRCD
                       END-IF
                     END-IF
                   END-IF
      *
      *        室料差額診療コード
               WHEN    "3"
      *            入院基本料算定なし
      *!           IF      SPA-CHK-NYUIN (IDX-D)   =   SPACE
      *!                MOVE    "0033"             TO  SPA-ERRCD
      *!                GO      TO      410121-DAY-NAI-CHK-EXT
      *!           END-IF
      *            入院期間
                   IF      SPA-CHK-DAY (IDX-D)     =   SPACE
                        MOVE    "0047"             TO  SPA-ERRCD
                   END-IF
      *            一覧にあること
                   MOVE    ZERO            TO  FLG-CHK
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL  (IDY     >   SPA-SAGAKU-MAX-41 )
                               OR (SPA-ERRCD   NOT =   SPACE  )
                       IF      SPA-GMN-DAY-41(IDX-D)(2:2)
                                           =  SPA-GMN-SAGAKUNO-41(IDY)
                                                           (1:2)
                           MOVE    1                   TO  FLG-CHK
                           MOVE    SPA-SAGAKU-MAX-41   TO  IDY
                       END-IF
                   END-PERFORM
                   IF     (FLG-CHK             =   ZERO ) AND
                          (SPA-ERRCD           =   SPACE )
                       MOVE    "0037"          TO  SPA-ERRCD
                   END-IF
      *        食事
               WHEN    "4"
      *            保険組合せなし
                   IF      SPA-CHK-HKNCOMBI (IDX-D)   =   SPACE
                       MOVE    "0053"             TO  SPA-ERRCD
                   END-IF
                   IF      SPA-NAI-SRYYM         <=  "200603"
                       IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                       OR  1
                                                       OR  2
                                                       OR  3
                                                       OR  4
                           CONTINUE
                       ELSE
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "0038"         TO  SPA-ERRCD
                           END-IF
                       END-IF
                   ELSE
                       IF      SPA-NAI-SRYYM         <=  "201603"
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  1
                                                           OR  2
                               CONTINUE
                           ELSE
                               IF      SPA-ERRCD       =   SPACE
                                   MOVE    "0049"         TO  SPA-ERRCD
                               END-IF
                           END-IF
                       ELSE
                           IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                           OR  1
                                                           OR  2
                                                           OR  3
                               CONTINUE
                           ELSE
                               IF      SPA-ERRCD       =   SPACE
                                   MOVE    "0049"         TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *            入院時食事療養２の場合、特食を入力不可とする
                   IF   SPA-5000-NACCT-SKJKBN   =    "2"
                       IF   SPA-GMN-DAY-41 (IDX-D)   =   2
                           MOVE    "0050"         TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            外泊中でないこと
                   IF      SPA-GMN-DAY-41 (IDX-D)  =   SPA-NAI-DAY-41
                                                   (SPA-NAI-BANGO IDX-D)
                       CONTINUE
                   ELSE
                       IF      SPA-CHK-GAIHAKU (IDX-D) NOT =   SPACE 
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "0039"         TO  SPA-ERRCD
                           END-IF
                        END-IF
                   END-IF
      *            入院時食事療養２で生活療養算定時は、流動食を入力不可とする
                   IF      SPA-NAI-SRYYM         >=  "201604"
                       IF  (LNK-NYULIFE2-KBN(IDX-D)      =  "1")   AND
                           (LNK-NYULIFE2-SKJKBN(IDX-D)   =  "2")   AND
                           (SPA-GMN-DAY-41 (IDX-D)       =   3)
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "0056"      TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
      *        保険
               WHEN    "5"
      *!           入院基本料算定なし
      *!           IF      SPA-CHK-NYUIN (IDX-D)   =   SPACE
      *!                MOVE    "0033"             TO  SPA-ERRCD
      *!           END-IF
      *            入院期間
                   IF      SPA-CHK-DAY (IDX-D)     =   SPACE
                        MOVE    "0047"             TO  SPA-ERRCD
                   END-IF
      *            一覧にあること
                   MOVE    ZERO            TO  FLG-CHK
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL  (IDY     >   SPA-HKNCOMBI-MAX )
                               OR (SPA-ERRCD   NOT =   SPACE    )
                       IF      SPA-GMN-DAY-41(IDX-D)
                                           =  SPA-I41-THKNCOMBI(IDY)
                           MOVE    1                   TO  FLG-CHK
                           MOVE    SPA-HKNCOMBI-MAX    TO  IDY
                       END-IF
                   END-PERFORM
                   IF     (FLG-CHK             =   ZERO ) AND
                          (SPA-ERRCD           =   SPACE )
      *                入力変更がなかった時、エラーとしない
                       IF      SPA-GMN-DAY-41 (IDX-D)
                                               =   SPA-NAI-DAY-41
                                                  (SPA-NAI-BANGO IDX-D)
                           CONTINUE
                       ELSE
                           MOVE    "0040"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *        入院料加算
               WHEN    "6"
      *            入院基本料算定なし
                   IF      SPA-CHK-NYUIN (IDX-D)   =   SPACE
                        MOVE    "0033"             TO  SPA-ERRCD
                   END-IF
                   IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                   OR  1
                       CONTINUE
                   ELSE
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "0041"         TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            外泊中でないこと
                   IF      SPA-GMN-DAY-41 (IDX-D)  =   SPA-NAI-DAY-41
                                                   (SPA-NAI-BANGO IDX-D)
                       CONTINUE
                   ELSE
                       IF      SPA-CHK-GAIHAKU (IDX-D) NOT =   SPACE
                           IF      SPA-ERRCD           =   SPACE
                               MOVE    "0042"         TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
      *        北海道療養担当手当て
               WHEN    "7"
      *            入院基本料算定なし
                   IF      SPA-CHK-NYUIN (IDX-D)   =   SPACE
                       MOVE    "0033"             TO  SPA-ERRCD
                   END-IF
                   IF      SPA-GMN-DAY-41 (IDX-D)  =   ZERO
                                                   OR  1
                       CONTINUE
                   ELSE
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    "0034"         TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            外泊中でないこと
                   IF      SPA-GMN-DAY-41 (IDX-D)  =   SPA-NAI-DAY-41
                                                   (SPA-NAI-BANGO IDX-D)
                       CONTINUE
                   ELSE
                       IF      SPA-CHK-GAIHAKU (IDX-D) NOT =   SPACE 
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "0048"         TO  SPA-ERRCD
                           END-IF
                        END-IF
                   END-IF
           END-EVALUATE
      *
           .
      *
       410121-DAY-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せのクリアチェック処理
      *****************************************************************
       410122-DAY-NASI-CHK-SEC             SECTION.
      *
      *    入院期間中
           IF      SPA-CHK-DAY (IDX-D)    NOT =   SPACE
               MOVE    "0046"             TO  SPA-ERRCD
           END-IF
      *
           .
      *
       410122-DAY-NASI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       41012-DATALIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM.
           MOVE    ZERO                TO  WRK-SELNUM2.
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX-41
              IF     (I41-DATELIST-SELECT (IDX)   =  "T"   )  AND
                     (SPA-GMN-TBLREC (IDX)(1:3)   NOT =   "---")
                  IF      SPA-GMN-TBANGO2 (IDX)   =   SPA-NAI-NUM-41
                      MOVE    SPA-GMN-TBANGO2 (IDX)   TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-GMN-TBANGO2 (IDX)   TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          =   ZERO)  AND
                  (WRK-SELNUM2         =   ZERO)
               GO      TO      41012-DATALIST-EXT
            END-IF
           IF     (WRK-SELNUM          =   ZERO)  AND
                  (WRK-SELNUM2     NOT =   ZERO)
               MOVE    WRK-SELNUM2         TO  SPA-NAI-NUM-41
           ELSE
               IF      WRK-SELNUM2         =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-NUM-41      TO  SPA-MAE-BANGO-41
                   MOVE    WRK-SELNUM2         TO  SPA-NAI-NUM-41
                   MOVE    "0103"              TO  SPA-IIDCD
                   GO      TO      41012-DATALIST-EXT
               END-IF
           END-IF.
      *
           MOVE    1                   TO  SPA-GMN-CUR-41.
           MOVE    SPA-NAI-NUM-41      TO  I41-NUM.
           PERFORM 41011-BANGO-CHK-SEC.
      *
       41012-DATALIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括修正入力処理
      *****************************************************************
       41017-KOMENTO-CHK-SEC              SECTION.
      *
           IF      SPA-NAI-NUM-41      =   ZERO
      *R01.5
             OR   (SPA-NAI-BANGO       =   ZERO )
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF
      *
           MOVE    I41-KOMENTO         TO  SPA-GMN-KOMENTO-41.
           MOVE    03                  TO  SPA-GMN-CUR-41.
      *
           IF     SPA-GMN-KOMENTO-41   =   SPACE
               MOVE    03                  TO  SPA-GMN-CUR-41
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF
      *
      *    食事判定
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "4"   )  AND
                  (SPA-GMN-TSRYCD (IDG)    =   "099999918" 
                                           OR  "099999919"
                                           OR  "099999920")
               MOVE    1                   TO  FLG-SYOKUJI
           ELSE
               MOVE    ZERO                TO  FLG-SYOKUJI
           END-IF
      *
           INITIALIZE                      WRK-DAY-AREA
           MOVE    ZERO                TO  IDW3
           MOVE    ZERO                TO  IDW2
           MOVE    ZERO                TO  IDW3
           MOVE    ZERO                TO  IDW4
           MOVE    ZERO                TO  FLG-K1
           MOVE    ZERO                TO  FLG-K2
           MOVE    ZERO                TO  FLG-ALL
           MOVE    1                   TO  IDW1
           MOVE    ZERO                TO  FLG-IKKATUFLG
      *
           MOVE    SPACE               TO  WRK-HZNSU
           MOVE    ZERO                TO  IDW1
           MOVE    ZERO                TO  IDW9
           PERFORM VARYING    IDW5     FROM    1   BY  1
                   UNTIL     (IDW5     >   60  )  OR
                             (SPA-ERRCD    NOT =   SPACE )
      *
               EVALUATE    SPA-GMN-KOMENTO-41 (IDW5:1)
      **           WHEN    "/"
      *                MOVE    1           TO  FLG-K1
      *            WHEN    ","
      *            WHEN    "."
      *                MOVE    ZERO        TO  FLG-K1
      *                MOVE    ZERO        TO  FLG-K2
      *                ADD     1           TO  IDW1
      *                MOVE    ZERO        TO  IDW2
      *                MOVE    ZERO        TO  IDW3
      *                MOVE    ZERO        TO  IDW4
      *            WHEN    "-"
      *                MOVE    1           TO  FLG-K2
      *            WHEN    "0"  THRU   "9"
      *                IF      FLG-K1      =   ZERO
      *                    回数
      *                    ADD     1           TO  IDW2
      *                    MOVE    SPA-GMN-KOMENTO-41 (IDW5:1)
      *                                        TO  WRK-KAISU (IDW1)
      *                                                      (IDW2:1)
      *                ELSE
      *                    IF      FLG-K2      =   ZERO
      *                    開始日
      *                        ADD     1           TO  IDW3
      *                        MOVE    SPA-GMN-KOMENTO-41 (IDW5:1)
      *                                        TO  WRK-DAY1 (IDW1)
      *                                                     (IDW3:1)
      *                    ELSE
      *                    終了日
      *                        ADD     1           TO  IDW4
      *                        MOVE    SPA-GMN-KOMENTO-41 (IDW5:1)
      *                                        TO  WRK-DAY2 (IDW1)
      *                                                     (IDW4:1)
      *                    END-IF
      **************   END-IF
                   WHEN    "/"
                       MOVE    1           TO  FLG-K1
                       IF      IDW9        >   ZERO
      *                回数
                           ADD     1              TO  IDW1
                           MOVE    WRK-HZNSU      TO  WRK-KAISUT(IDW1)
                           MOVE    SPACE          TO  WRK-HZNSU
                           MOVE    ZERO           TO  IDW9
                           MOVE    ZERO           TO  IDW2
                       ELSE
                           IF      IDW1            =   ZERO
                               ADD     1               TO  IDW1
                           END-IF
                       END-IF
                   WHEN    ","
                   WHEN    "."
      *H30.8
      *            回数がない
                     IF     (FLG-K1              =   ZERO  )
                        MOVE    "0062"              TO  SPA-ERRCD
                     ELSE
      *
                       IF      FLG-K2      =   1
      *                終了日
                           MOVE    WRK-HZNSU       TO  WRK-DAY2
                                                            (IDW1 IDW2)
                           MOVE    SPACE           TO  WRK-HZNSU
                           MOVE    ZERO            TO  IDW9
                       ELSE
      *                開始日
                           IF      IDW9        >   ZERO
                               ADD     1               TO  IDW2
                               MOVE    WRK-HZNSU       TO  WRK-DAY1
                                                           (IDW1 IDW2)
                               MOVE    SPACE           TO  WRK-HZNSU
                               MOVE    ZERO            TO  IDW9
                           END-IF
                       END-IF
                       MOVE    ZERO        TO  FLG-K2
      *H30.8
                     END-IF
                   WHEN    "-"
      *H30.8
      *            回数がない
                     IF     (FLG-K1              =   ZERO  )
                        MOVE    "0062"              TO  SPA-ERRCD
                     ELSE
      *
                       MOVE    1           TO  FLG-K2
      *                開始日
                       IF      IDW9        >   ZERO
                           ADD     1               TO  IDW2
                           MOVE    WRK-HZNSU       TO  WRK-DAY1
                                                           (IDW1 IDW2)
                           MOVE    SPACE           TO  WRK-HZNSU
                           MOVE    ZERO            TO  IDW9
                       END-IF
      *H30.8
                     END-IF
                   WHEN    "0"  THRU   "9"
                       ADD     1               TO  IDW9
                       MOVE    SPA-GMN-KOMENTO-41 (IDW5:1)
                                               TO  WRK-HZNSU (IDW9:1)
      *!!!
                   WHEN    "A"
                   WHEN    "a"
      *                食事のみ 最後に'Ａ'指定
                       IF      FLG-SYOKUJI         =   1
                           COMPUTE IDW6            =  IDW5  +  1
                           IF     ((IDW6           >  60  ) OR
                                   (SPA-GMN-KOMENTO-41(IDW6:)
                                                   =   SPACE  ))
                               MOVE    1               TO  FLG-ALL
                           ELSE
                               MOVE    "0124"              TO  SPA-ERRCD
                           END-IF
                       ELSE
                           MOVE    "0022"              TO  SPA-ERRCD
                       END-IF
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "0022"              TO  SPA-ERRCD
               END-EVALUATE
           END-PERFORM
      *
      *H30.8
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF
      *
           IF      IDW9            >   ZERO
               IF      IDW1            =   ZERO
                   ADD     1               TO  IDW1
               END-IF
               IF      FLG-K2          =   1
      *        終了日
                   MOVE    WRK-HZNSU       TO  WRK-DAY2(IDW1 IDW2)
               ELSE
      *        開始日
                   ADD     1               TO  IDW2
                   MOVE    WRK-HZNSU       TO  WRK-DAY1(IDW1 IDW2)
               END-IF
           END-IF
      *    回数がない
           IF     (FLG-K1              =   ZERO  )  AND
                  (SPA-GMN-KOMENTO-41  NOT =   SPACE )
               MOVE    "0061"              TO  SPA-ERRCD
           END-IF
      *!!
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF
      *
      *!!!
           PERFORM VARYING    IDW1     FROM    1   BY  1
                   UNTIL     (IDW1     >   60  )  OR
                             (SPA-ERRCD    NOT =   SPACE ) OR
                            ((WRK-KAISUT(IDW1) =   SPACE )  AND
                             (WRK-DAY1  (IDW1 1) =   SPACE )  AND
                             (WRK-DAY2  (IDW1 1) =   SPACE ))
      *        回数
               IF      WRK-KAISUT(IDW1)    =   SPACE
                   MOVE    1               TO  WRK-KAISU-9
               ELSE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-KAISUT(IDW1)    TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )   OR
                          (SNUM-SEISUU         >   3     )
                       MOVE    "0122"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-KAISU-9
                   END-IF
               END-IF
               PERFORM VARYING    IDW2     FROM    1   BY  1
                       UNTIL     (IDW2     >   30  )  OR
                             (SPA-ERRCD    NOT =   SPACE ) OR
                            ((WRK-DAY1  (IDW1 IDW2) =   SPACE )  AND
                             (WRK-DAY2  (IDW1 IDW2) =   SPACE ))
      *        開始日
               IF      WRK-DAY1  (IDW1 IDW2)   =   SPACE
                   MOVE    ZERO            TO  WRK-DAY1-9(IDW2)
               ELSE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-DAY1 (IDW1 IDW2)     TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )   OR
                          (SNUM-SEISUU         >   2     )
                       MOVE    "0122"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-DAY1-9(IDW2)
                   END-IF
               END-IF
      *        終了日
               IF      WRK-DAY2  (IDW1 IDW2)    =   SPACE
                   MOVE    ZERO            TO  WRK-DAY2-9(IDW2)
               ELSE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-DAY2 (IDW1 IDW2)     TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )   OR
                          (SNUM-SEISUU         >   3     )
                       MOVE    "0122"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-DAY2-9(IDW2)
                   END-IF
               END-IF
      *        回数編集
               IF     (WRK-DAY1-9 (IDW2)   >   ZERO )  AND
                      (WRK-DAY2-9 (IDW2)   =   ZERO )
                   MOVE    WRK-DAY1-9 (IDW2)   TO  WRK-DAY2-9(IDW2)
               END-IF
               IF     (WRK-DAY1-9(IDW2)    >   WRK-DAY2-9(IDW2) )
                   MOVE    "0125"              TO  SPA-ERRCD
               END-IF
      *        回数編集
               IF     (WRK-DAY1-9 (IDW2)   =   ZERO
                                           OR  99   )
                   MOVE    "0125"              TO  SPA-ERRCD
               END-IF
      *        一括変更チェック
               IF      WRK-DAY2-9 (IDW2)   =   99
                   IF      SPA-NAI-IKKATUFLG   =   ZERO
                       OR  FLG-ALL             =   1
                       MOVE    "0043"              TO  SPA-ERRCD
                   ELSE
                       MOVE    1                   TO  FLG-IKKATUFLG
                       MOVE    WRK-DAY1-9 (IDW2)   TO  WRK-IKKATUDAY
                       MOVE    WRK-KAISU-9         TO  WRK-IKKATUKAI
                   END-IF
               END-IF
      *
               PERFORM VARYING IDW3    FROM    WRK-DAY1-9(IDW2)  BY  1
                       UNTIL  (IDW3        >   WRK-DAY2-9 (IDW2))  OR
                              (IDW3        >   31         )  OR
                              (SPA-ERRCD   NOT =   SPACE )
                   IF      WRK-DAY(IDW3)   NOT =   ZERO
                       MOVE    "0123"              TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-KAISU-9     TO  WRK-DAY(IDW3)
                       IF      WRK-KAISU-9     =   ZERO
                           MOVE    1               TO  WRK-DAYDEL (IDW3)
                       END-IF
                   END-IF
               END-PERFORM
      *
               END-PERFORM
      *!!!
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-DAY-G-41    TO  WRK-GMN-DAY-G
      **** INITIALIZE                      SPA-GMN-DAY-G-41
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-NAI-SRYMATU)
                         OR   (IDX-D   >   31             )
               IF      WRK-DAY(IDX-D)  >   ZERO
                   MOVE    WRK-DAY(IDX-D)  TO  SPA-GMN-DAY-41(IDX-D)
               ELSE
                   IF      WRK-DAYDEL(IDX-D)   =   1
                       MOVE    ZERO            TO  SPA-GMN-DAY-41(IDX-D)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    日付のチェック
      *    入院時生活療養費の情報取得
           INITIALIZE                      ORCSNYULIFE2-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYULIFE2-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  LNK-NYULIFE2-PTID
           MOVE    SPA-NAI-SRYYM       TO  LNK-NYULIFE2-SANTEIYM
           CALL    "ORCSNYULIFE2"   USING  ORCSNYULIFE2-AREA
                                           SPA-AREA
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  WRK-IDX
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-NAI-SRYMATU)
                         OR   (IDX-D   >   31             )
                         OR   (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-DAY-41 (IDX-D)  NOT =   ZERO
                   PERFORM 410121-DAY-NAI-CHK-SEC
               ELSE
                   IF      SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO) =  "5"
                       PERFORM 410122-DAY-NASI-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-GMN-DAY-G   TO   SPA-GMN-DAY-G-41
               GO      TO     41017-KOMENTO-CHK-EXT
           END-IF
      *    一括修正有無
           IF      FLG-IKKATUFLG       =   1
               MOVE    1               TO  SPA-NAI-IKKATUOK
               MOVE    WRK-IKKATUDAY   TO  SPA-NAI-IKKATUDAY 
               MOVE    WRK-IKKATUKAI   TO  SPA-NAI-IKKATUKAI
           ELSE
               MOVE    ZERO            TO  SPA-NAI-IKKATUOK
               MOVE    ZERO            TO  SPA-NAI-IKKATUDAY
               MOVE    ZERO            TO  SPA-NAI-IKKATUKAI
           END-IF
      *    食事一括
           IF      FLG-ALL             =   1
               MOVE    1               TO  SPA-NAI-ALL
           ELSE
               MOVE    ZERO            TO  SPA-NAI-ALL
           END-IF
      *
           .
      *
       41017-KOMENTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更確定　処理
      *****************************************************************
       470-KAKUTEI-SEC             SECTION.
      *
           IF      SPA-NAI-NUM-41       =   ZERO
               GO      TO      470-KAKUTEI-EXT
           END-IF.
      *R01.5
           IF     (SPA-NAI-BANGO       =   ZERO )
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      470-KAKUTEI-EXT
           END-IF
      *
      *
      *    最終入院日以前
           IF      SPA-NAI-SRYYM       <   SPA-NAI-NYUINBI(1:6)
               MOVE    "0110"          TO  SPA-IIDCD
               GO      TO      470-KAKUTEI-EXT
           END-IF
      *    入院時食事療養２の場合、特食を入力不可とする
           IF   SPA-NAI-TZAISKBKBN-41(SPA-NAI-BANGO)  =  "4"
               IF      SPA-5000-NACCT-SKJKBN  =   "2"
                   PERFORM   VARYING   IDX   FROM   1   BY  1
                             UNTIL     IDX   >   31
                       IF    SPA-GMN-DAY-41 (IDX)   =   2
                           MOVE   "0050"     TO   SPA-ERRCD
                           GO      TO      470-KAKUTEI-EXT
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *    入院時食事療養２で生活療養算定時は、流動食を入力不可とする
           IF      SPA-NAI-SRYYM         >=  "201604"
             IF   SPA-NAI-TZAISKBKBN-41(SPA-NAI-BANGO)  =  "4"
               IF      SPA-5000-NACCT-SKJKBN  =   "2"
                   PERFORM   VARYING   IDX   FROM   1   BY  1
                             UNTIL     IDX   >   31
                       IF   (LNK-NYULIFE2-KBN(IDX)      =  "1")   AND
                            (LNK-NYULIFE2-SKJKBN(IDX)   =  "2")   AND
                            (SPA-GMN-DAY-41 (IDX)       =   3)
                           MOVE   "0056"     TO   SPA-ERRCD
                           GO      TO      470-KAKUTEI-EXT
                       END-IF
                   END-PERFORM
               END-IF
             END-IF
           END-IF
      *
           IF      SPA-NAI-SRYYM       =   SPA-NAI-NYUINBI(1:6)
               MOVE    SPA-NAI-NYUINBI(7:2)    TO  IDX-D
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >=  IDX-D
                   IF      SPA-GMN-DAY-41 (IDX)  NOT =
                                           SPA-NAI-DAY-41
                                                   (SPA-NAI-BANGO IDX)
                       MOVE    "0110"          TO  SPA-IIDCD
                       MOVE    31              TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-IIDCD            =   SPACE
      *        変更確定処理
               PERFORM 4701-KAKUTEI-SYORI-SEC
           END-IF.
      *
       470-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更確定　処理
      *****************************************************************
       4701-KAKUTEI-SYORI-SEC             SECTION.
      *
      *    食事の時、全食事・食堂加算の変更
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "4"   )  AND
                  (SPA-NAI-ALL             =   1     )
               PERFORM 47011-SYOKUJI-ALLCHK-SEC
           END-IF
      *    入院料加算削除の為
           MOVE    SPA-NAI-DAY-AREA-41(SPA-NAI-BANGO)  TO  SPA-SEL-DAY-G
      *
      *    回数更新
           MOVE    SPA-GMN-DAY-G-41    TO  SPA-NAI-DAY-AREA-41
                                                       (SPA-NAI-BANGO)
           MOVE    SPA-NAI-IKKATUOK    TO  SPA-NAI-TIKKATUFLG
                                                       (SPA-NAI-BANGO)
           MOVE    SPA-NAI-IKKATUDAY   TO  SPA-NAI-TIKKATUDAY
                                                       (SPA-NAI-BANGO)
           MOVE    SPA-NAI-IKKATUKAI   TO  SPA-NAI-TIKKATUKAI
                                                       (SPA-NAI-BANGO)
      *
           MOVE    ZERO                TO  FLG-SKJ-ASA
                                           FLG-SKJ-HIRU
                                           FLG-SKJ-YUU
      *
      *    食事の時、食堂加算の変更
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "4"   )  AND
                  (SPA-GMN-TSRYCD (IDG)    =   "099999913")
               PERFORM 47011-SYOKUJI-CHK-SEC
           END-IF
      *
      *    食事（朝）の時、食堂加算の変更
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "4"   )  AND
                  (SPA-GMN-TSRYCD (IDG)    =   "099999918")
               MOVE    1                   TO  FLG-SKJ-ASA
               PERFORM 47011-SYOKUJI-CHK-SEC
           END-IF
      *
      *    食事（昼）の時、食堂加算の変更
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "4"   )  AND
                  (SPA-GMN-TSRYCD (IDG)    =   "099999919")
               MOVE    1                   TO  FLG-SKJ-HIRU
               PERFORM 47011-SYOKUJI-CHK-SEC
           END-IF
      *
      *    食事（夕）の時、食堂加算の変更
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "4"   )  AND
                  (SPA-GMN-TSRYCD (IDG)    =   "099999920")
               MOVE    1                   TO  FLG-SKJ-YUU
               PERFORM 47011-SYOKUJI-CHK-SEC
           END-IF
      *
      *    外泊の時、他の削除
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "2"   )  AND
                  (SPA-5000-NACCT-GAIHAKUKBN   =   "2")
               PERFORM 47012-GAIHAKU-CHK-SEC
           END-IF
      *
      *    入院料の時、加算他の削除（短期滞在、選定入院、療養病床は削除しない）
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
      *
      *    短期滞在、療養病床の判定用に点数マスタ読み込み
           INITIALIZE                           TNS-TENSU-REC
           MOVE    SPA-HOSPNUM             TO   TNS-HOSPNUM
           MOVE    SPA-GMN-TSRYCD (IDG)    TO   TNS-SRYCD
           MOVE    SPA-NAI-SRYYM           TO   TNS-YUKOSTYMD(1:6)
           MOVE    "01"                    TO   TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD           TO   TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC           TO   MCPDATA-REC
           MOVE    "tbl_tensu"             TO   MCP-TABLE
           MOVE    "key"                   TO   MCP-PATHNAME
           PERFORM    910-DBSELECT-SEC
           IF      MCP-RC                  =    ZERO
               MOVE    "tbl_tensu"             TO   MCP-TABLE
               MOVE    "key"                   TO   MCP-PATHNAME
               PERFORM    920-DBFETCH-SEC
               IF      MCP-RC              =    ZERO
                   MOVE   MCPDATA-REC      TO   TNS-TENSU-REC
               END-IF
           END-IF
      *
           MOVE    "tbl_tensu"             TO   MCP-TABLE
           MOVE    "key"                   TO   MCP-PATHNAME
           PERFORM    990-DBCLOSE-SEC
      *
      *    選定入院料（システム予約コード)
           IF     (SPA-GMN-TSRYCD (IDG)    =  "099999916")
      *    短期滞在２（生活療養含む）
      *    短期滞在３（生活療養含む）
           OR    ((TNS-CDKBN-KBN     =   "A")   AND
                  (TNS-CDKBN-KBNNUM  =   400)   AND
                  (TNS-CDKBN-KOUBAN  =   2  OR 3))
      *    療養病床の判定
      *    療養病棟入院基本料１（ＡからＩ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "200804")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    101)       AND
                   (TNS-CDKBN-KOUBAN        =    1))
      *    一般病棟・療養病棟入院基本料１（ＡからＩ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201210")  AND
                   (SPA-NAI-SRYYM          <=    "201403")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    100)       AND
                   (TNS-CDKBN-KOUBAN        =    17))
      *    一般病棟・療養病棟入院基本料１（ＡからＩ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201404")  AND
                   (SPA-NAI-SRYYM          <=    "201803")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    100)       AND
                   (TNS-CDKBN-KOUBAN        =    16))
      *    一般病棟・療養病棟入院基本料１（ＡからＩ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201804")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    100)       AND
                   (TNS-CDKBN-KOUBAN        =    14))
      *    療養病棟入院基本料２（ＡからＩ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "200607")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    101)       AND
                   (TNS-CDKBN-KOUBAN        =    2))
      *    療養病棟入院基本料２（ＡからＩ）（夜勤時間超過減算）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201404")  AND
                   (SPA-NAI-SRYYM          <=    "201803")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    101)       AND
                   (TNS-CDKBN-KOUBAN        =    3))
      *    療養病棟入院基本料２（ＡからＩ）（夜勤時間特別入院基本料・経過措置１）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201604")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    101)       AND
                   (TNS-CDKBN-KOUBAN        =    10))
      *    療養病棟入院基本料２（ＡからＩ）（経過措置２）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201804")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    101)       AND
                   (TNS-CDKBN-KOUBAN        =    10))
      *    有床診療所療養病床入院基本料（ＡからＥ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "200804")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    109)       AND
                   (TNS-CDKBN-KOUBAN        =    1 OR 2 OR 3 OR 4 OR 5))
      *    特定一般病棟・療養病棟入院基本料１（ＡからＩ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201204")  AND
                   (SPA-NAI-SRYYM          <=    "201403")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    317)       AND
                   (TNS-CDKBN-KOUBAN        =    10))
      *    特定一般病棟・療養病棟入院基本料１（ＡからＩ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201404")  AND
                   (SPA-NAI-SRYYM          <=    "201803")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    317)       AND
                   (TNS-CDKBN-KOUBAN        =    11))
      *    特定一般病棟・療養病棟入院基本料１（ＡからＩ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201804")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    317)       AND
                   (TNS-CDKBN-KOUBAN        =    8))
      *    療養病床以外・有床診療所療養病床入院（ＡからＥ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201204")  AND
                   (SPA-NAI-SRYYM          <=    "201403")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    108)       AND
                   (TNS-CDKBN-KOUBAN        =    10))
      *    療養病床以外・有床診療所療養病床入院（ＡからＥ）
           OR     ((SPA-5000-RYOYODSPKBN    =    "1")       AND
                   (SPA-NAI-SRYYM          >=    "201404")  AND
                   (TNS-CDKBN-KBN           =    "A")       AND
                   (TNS-CDKBN-KBNNUM        =    108)       AND
                   (TNS-CDKBN-KOUBAN        =    13))
               CONTINUE
           ELSE
      *        短期滞在手術基本料以外とする
               IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "1"   ) 
                   PERFORM 47013-NYUINDEL-CHK-SEC
               END-IF
           END-IF
      *    変更入力欄
           INITIALIZE                  SPA-GMN-INPUT-41
      *
      *    保険組合期限切れチェック、外泊の再編集
           PERFORM 3106-DAY-KIKANCHK-SEC
      *
      *    番号選択へカーソル移動
           MOVE    1                   TO  SPA-GMN-CUR-41
      *
      *    剤確定あり
           MOVE    "1"                 TO  SPA-UPDFLG-41.
      *
           .
       4701-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    食堂加算の変更処理
      *****************************************************************
       47011-SYOKUJI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-KSN
                                           IDX-SKJ-ASA
                                           IDX-SKJ-HIRU
                                           IDX-SKJ-YUU
      *
      *    朝食変更時に昼と夕の状態を取得
           IF     FLG-SKJ-ASA          =   1
             PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NAI-MAX-41)
                         OR   (IDX-SKJ-HIRU >   ZERO           )
               IF      SPA-NAI-BANGO       NOT =   IDX
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   IF     (SPA-NAI-TZAISKBKBN-41 (IDX)
                                               =   "4"   )  AND
                          (SPA-GMN-TSRYCD (IDG)    =   "099999919")
                       MOVE    IDX                 TO  IDX-SKJ-HIRU
                   END-IF
               END-IF
             END-PERFORM
             PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NAI-MAX-41)
                         OR   (IDX-SKJ-YUU >   ZERO           )
               IF      SPA-NAI-BANGO       NOT =   IDX
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   IF     (SPA-NAI-TZAISKBKBN-41 (IDX)
                                               =   "4"   )  AND
                          (SPA-GMN-TSRYCD (IDG)    =   "099999920")
                       MOVE    IDX                 TO  IDX-SKJ-YUU
                   END-IF
               END-IF
             END-PERFORM
           END-IF
      *
      *    昼食変更時に朝と夕の状態を取得
           IF     FLG-SKJ-HIRU          =   1
             PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NAI-MAX-41)
                         OR   (IDX-SKJ-ASA >   ZERO           )
               IF      SPA-NAI-BANGO       NOT =   IDX
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   IF     (SPA-NAI-TZAISKBKBN-41 (IDX)
                                               =   "4"   )  AND
                          (SPA-GMN-TSRYCD (IDG)    =   "099999918")
                       MOVE    IDX                 TO  IDX-SKJ-ASA
                   END-IF
               END-IF
             END-PERFORM
             PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NAI-MAX-41)
                         OR   (IDX-SKJ-YUU >   ZERO           )
               IF      SPA-NAI-BANGO       NOT =   IDX
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   IF     (SPA-NAI-TZAISKBKBN-41 (IDX)
                                               =   "4"   )  AND
                          (SPA-GMN-TSRYCD (IDG)    =   "099999920")
                       MOVE    IDX                 TO  IDX-SKJ-YUU
                   END-IF
               END-IF
             END-PERFORM
           END-IF
      *
      *    夕食変更時に朝と昼の状態を取得
           IF     FLG-SKJ-YUU          =   1
             PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NAI-MAX-41)
                         OR   (IDX-SKJ-ASA >   ZERO           )
               IF      SPA-NAI-BANGO       NOT =   IDX
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   IF     (SPA-NAI-TZAISKBKBN-41 (IDX)
                                               =   "4"   )  AND
                          (SPA-GMN-TSRYCD (IDG)    =   "099999918")
                       MOVE    IDX                 TO  IDX-SKJ-ASA
                   END-IF
               END-IF
             END-PERFORM
             PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NAI-MAX-41)
                         OR   (IDX-SKJ-HIRU >   ZERO           )
               IF      SPA-NAI-BANGO       NOT =   IDX
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   IF     (SPA-NAI-TZAISKBKBN-41 (IDX)
                                               =   "4"   )  AND
                          (SPA-GMN-TSRYCD (IDG)    =   "099999919")
                       MOVE    IDX                 TO  IDX-SKJ-HIRU
                   END-IF
               END-IF
             END-PERFORM
           END-IF
      *
      *    食堂加算の添字検索
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NAI-MAX-41)
                         OR   (IDX-KSN >   ZERO           )
               IF      SPA-NAI-BANGO       NOT =   IDX
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   IF     (SPA-NAI-TZAISKBKBN-41 (IDX)
                                               =   "4"   )  AND
                          (SPA-GMN-TSRYCD (IDG)    =   "197000570")
                       MOVE    IDX                 TO  IDX-KSN
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     IDX-KSN              >   ZERO
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL       IDX-D   >   31  
                   IF      SPA-GMN-DAY-41(IDX-D)   =   ZERO
                     IF   FLG-SKJ-ASA               =   1
                      IF (SPA-NAI-DAY-41(IDX-SKJ-HIRU IDX-D) = ZERO) AND
                         (SPA-NAI-DAY-41(IDX-SKJ-YUU IDX-D)  = ZERO)
                        MOVE    ZERO           TO  SPA-NAI-DAY-41
                                                       (IDX-KSN IDX-D)
                      END-IF
                     END-IF
                     IF   FLG-SKJ-HIRU              =   1
                      IF (SPA-NAI-DAY-41(IDX-SKJ-ASA IDX-D)  = ZERO) AND
                         (SPA-NAI-DAY-41(IDX-SKJ-YUU IDX-D)  = ZERO)
                        MOVE    ZERO           TO  SPA-NAI-DAY-41
                                                       (IDX-KSN IDX-D)
                      END-IF
                     END-IF
                     IF   FLG-SKJ-YUU               =   1
                      IF (SPA-NAI-DAY-41(IDX-SKJ-ASA IDX-D)  = ZERO) AND
                         (SPA-NAI-DAY-41(IDX-SKJ-HIRU IDX-D) = ZERO)
                        MOVE    ZERO           TO  SPA-NAI-DAY-41
                                                       (IDX-KSN IDX-D)
                      END-IF
                     END-IF
                   ELSE
                       IF   SS007-RC3(IDX-D)  =  "1"
                           MOVE    1              TO  SPA-NAI-DAY-41
                                                      (IDX-KSN IDX-D)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       47011-SYOKUJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全食事・食堂加算の変更処理
      *****************************************************************
       47011-SYOKUJI-ALLCHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NAI-MAX-41)
               MOVE    ZERO                TO  IDX-KSN
               IF      SPA-NAI-BANGO       NOT =   IDX
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
                   IF     (SPA-NAI-TZAISKBKBN-41 (IDX)
                                               =   "4"   ) 
                       MOVE    IDX                 TO  IDX-KSN
                       PERFORM VARYING     IDX-D   FROM    1   BY  1
                               UNTIL       IDX-D   >   31
      *                    今回変更のみ
                           IF      SPA-GMN-DAY-41(IDX-D) 
                                               NOT =   SPA-NAI-DAY-41
                                                 (SPA-NAI-BANGO IDX-D)
                              IF    SPA-GMN-TSRYCD (IDG)
                                                   =   "197000570"
                                   IF      SPA-GMN-DAY-41(IDX-D)
                                                   =   ZERO
      *                                食堂加算のゼロ変更
                                       MOVE    ZERO
                                                   TO  SPA-NAI-DAY-41
                                                      (IDX-KSN IDX-D)
                                   ELSE
      *                                食堂加算のセット
                                       IF   SS007-RC3(IDX-D)  =  "1"
                                           MOVE    1
                                                   TO  SPA-NAI-DAY-41
                                                      (IDX-KSN IDX-D)
                                       END-IF
                                   END-IF
                               ELSE
                                   MOVE    SPA-GMN-DAY-41(IDX-D)
                                                   TO  SPA-NAI-DAY-41
                                                      (IDX-KSN IDX-D)
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       47011-SYOKUJI-ALLCHK-EXT.
           EXIT.
      *****************************************************************
      *    外泊の時、他の削除処理
      *****************************************************************
        47012-GAIHAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-KSN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NAI-MAX-41
               MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
               EVALUATE     SPA-NAI-TZAISKBKBN-41 (IDX)
      *            食事
                   WHEN     "4"
      *            加算
                   WHEN     "6"
      *            療養担当手当
                   WHEN     "7"
      *                外泊日の削除
                       PERFORM VARYING     IDX-D   FROM    1   BY  1
                               UNTIL       IDX-D   >   31
                           IF  SPA-GMN-DAY-41(IDX-D)  = ZERO OR 4 OR 5
                                                             OR 6 OR 7
                                                             OR 8 OR 9
                                                             OR 10 OR 11
                                                             OR 12 OR 13
                               CONTINUE
                           ELSE
                               MOVE    ZERO         TO  SPA-NAI-DAY-41
                                                           (IDX IDX-D)
                               MOVE    SPACE        TO  SPA-GMN-TDAY-41
                                                           (IDG IDX-D)
                           END-IF
                       END-PERFORM
               END-EVALUATE
           END-PERFORM
           .
        47012-GAIHAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料と入院加算の削除処理
      *****************************************************************
       47013-NYUINDEL-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NAI-MAX-41
               MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
               EVALUATE     SPA-NAI-TZAISKBKBN-41 (IDX)
      *            入院料加算
                   WHEN     "6"
      *                今回入院料なしの時、加算を削除
                       PERFORM VARYING     IDX-D   FROM    1   BY  1
                               UNTIL       IDX-D   >   31
                           IF     (SPA-SEL-DAY (IDX-D)
                                               NOT =   SPA-GMN-DAY-41
                                                            (IDX-D))
                           AND    (SPA-GMN-DAY-41(IDX-D)  =   ZERO )
                               MOVE    ZERO         TO  SPA-NAI-DAY-41
                                                           (IDX IDX-D)
                               MOVE    SPACE        TO  SPA-GMN-TDAY-41
                                                           (IDG IDX-D)
                           END-IF
                       END-PERFORM
               END-EVALUATE
           END-PERFORM
           .
       47013-NYUINDEL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       41012-SEIWA-HEN-SEC             SECTION.
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
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF.
      *
       41012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア画面へ
      *****************************************************************
       420-CLEA-SEC        SECTION.
      *
           IF      SPA-NAI-NUM-41    =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *        クリア
               INITIALIZE                   SPA-I40-SET
      *        初期表示
               PERFORM 3001-INIT-HENSYU-SEC
           ELSE
      *        変更項目クリア
               INITIALIZE                   SPA-GMN-INPUT-41
      *******  INITIALIZE                   SPA-NAIGMN-AREA
               MOVE    ZERO                 TO  SPA-I4-SYORI
               MOVE    1                    TO  SPA-GMN-CUR-41
           END-IF.
      *
       420-CLEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       420-ZAIHENKOU-SEC        SECTION.
      *
           IF      SPA-NAI-NUM-41      =   ZERO
      *R01.5
             OR   (SPA-NAI-BANGO       =   ZERO )
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF
      *
      *    剤修正のチェック
           IF     (SPA-GMN-KOMENTO-41  NOT =   SPACE)   OR
                  (SPA-NAI-DAY-AREA-41(SPA-NAI-BANGO)
                                       NOT =   SPA-GMN-DAY-G-41)
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0009"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF
      *    入院基本料のみ変更可能
           MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)  TO  IDG
           IF     (SPA-NAI-TZAISKBKBN-41 (SPA-NAI-BANGO)
                                           =   "1"    ) AND
                  (SPA-GMN-TSRYCD(IDG) NOT =   W-YOYAKU )
               CONTINUE
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0029"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF
      *
           INITIALIZE                  WRK-TBL-SSTKIJUN-AREA
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC.
           MOVE    "1006"              TO  SYS-1006-KANRICD.
           MOVE    SPA-NAI-SRYYM       TO  SYS-1006-STYUKYMD(1:6).
           MOVE    "01"                TO  SYS-1006-STYUKYMD(7:2).
           MOVE    SYS-1006-STYUKYMD   TO  SYS-1006-EDYUKYMD.
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
                   OR      (IDX            >=  3500 )
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               EVALUATE     SYS-1006-KBNCD
                   WHEN     "1"
                        MOVE    0          TO  IDX
                   WHEN     "2"
                        MOVE    500        TO  IDX
                   WHEN     "7"
                        MOVE    3000       TO  IDX
               END-EVALUATE
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL   IDY         >       500
                   ADD     1               TO  IDX
                   MOVE    SYS-1006-SSTKIJUN  (IDY)
                                           TO  WRK-TBL-SSTKIJUN (IDX)
               END-PERFORM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入院基本料の施設規準判定
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYMD(1:6).
           MOVE    01                  TO  WRK-SRYDD.
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      SPA-NAI-DAY-41(SPA-NAI-BANGO IDX-D)
                                       NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-TSRYCD(IDG) TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           MOVE    ZERO                TO  FLG-SST
           MOVE    ZERO                TO  FLG-SST-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )   OR
                              (FLG-SST     =   1   )
               IF      TNS-SSTKIJUNCD (IDX)    =   ZERO
                   CONTINUE
               ELSE
                   MOVE    TNS-SSTKIJUNCD (IDX)    TO  IDY
                   IF      WRK-TBL-SSTKIJUN (IDY)  =   1
                       MOVE    1               TO  FLG-SST
                   ELSE
                       MOVE    1               TO  FLG-SST-CHK
                   END-IF
               END-IF
           END-PERFORM.
      *
      *H30.5
      *    施設基準チェック廃止
      *    IF     (FLG-SST-CHK         =   1    )  AND
      *           (FLG-SST             =   ZERO )
      *        MOVE    1                   TO  SPA-GMN-CUR-41
      *        MOVE    "0030"              TO  SPA-ERRCD
      *        GO      TO      420-ZAIHENKOU-EXT
      **** END-IF.
      *
           MOVE    "0104"              TO  SPA-IIDCD
           .
      *
       420-ZAIHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       420-ZAIHENKOU-SYORI-SEC        SECTION.
      *
      *    現在までの内容を更新
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      420-ZAIHENKOU-SYORI-EXT
               END-IF
           END-IF.
      *
      *    選択の内容をパラメタに編集
           MOVE    SPA-NAI-SRYYM       TO  SPA-I40-SRYYMD(1:6).
           MOVE    SPA-GMN-SRYYM       TO  SPA-I40-SRYYMDW(1:6).
      *
           MOVE    SPA-NAI-BANGO       TO  IDX
      *    診療区分剤番号
           MOVE    SPA-NAI-SRYKBN-41 (IDX) TO  SPA-SRYKBN.
           MOVE    SPA-NAI-ZAINUM-41 (IDX) TO  SPA-ZAINUM.
      *    枝番
           MOVE    SPA-NAI-RENNUM-41 (IDX) TO  SPA-RENNUM.
      *    剤点数
           MOVE    SPA-NAI-ZAITEN-41 (IDX) TO  SPA-ZAITEN.
      *    回数テーブル
           MOVE    SPA-NAI-DAY-AREA-41(IDX) TO  SPA-SEL-DAY-G.
      *    診療開始年月日
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYMD(1:6).
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      SPA-NAI-DAY-41(SPA-NAI-BANGO IDX-D)
                                       NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *    保険組合番号（診療開始日時点の保険）
           MOVE    ZERO                TO  WRK-HKNCOMBINUM
           PERFORM VARYING     IDH     FROM  1  BY 1
                   UNTIL       IDH     >   SPA-NAI-MAX-41
               IF      SPA-NAI-TZAISKBKBN-41 (IDH) =  "5"
                   MOVE    ZERO            TO  WRK-HKNCOMBINUM
                   MOVE    SPA-NAI-DAY-41(IDH WRK-SRYDD)
                                           TO  WRK-HKNCOMBINUM(2:3)
               END-IF
           END-PERFORM
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    WRK-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    WRK-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *    負担率
      *****MOVE    AGECHK-FTNRATEMEI   TO  SPA-J02-FTNRATEMEI
      *    老人区分
           IF      AGECHK-HKNKBN       =   1  OR  2
      *        労災・自賠責
               MOVE    ZERO                TO  SPA-ROUJIN
           ELSE
      *        年齢、老人公費判定
               MOVE    AGECHK-ROUJIN2      TO  SPA-ROUJIN
           END-IF
      *    開始日の年齢
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NAI-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NAI-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NAI-NENREI-DD
      *
      *    診療開始年月日
           MOVE    WRK-SRYYMD          TO  SPA-I40-SRYYMD
      *
      *!   MOVE    SPA-SRYYMD          TO  SPA-MAE-SRYYMD.
      *!   MOVE    SPA-PTID            TO  SPA-MAE-PTID.
           MOVE    SPA-I40-SRYYMD      TO  SPA-SRYYMD.
           MOVE    SPA-GMN-PTID        TO  SPA-PTID.
      *
           MOVE    SPA-GMN-SRYKA-I-41      TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41   TO  SPA-I40-SRYKAMEI.
           MOVE    SPA-NAI-TSRYKA-41 (IDX) TO  SPA-I40-SRYKA-READ.
      *
           MOVE    SPACE               TO  SPA-IIDCD.
           INITIALIZE                  SPA-GMN-INPUT-41.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    "I44"               TO  SPA-SAKIPG.
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE.
           MOVE   "I44"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       420-ZAIHENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    更新前のチェック
           IF      SPA-UPDFLG-41          =   "1"
               MOVE    "0102"               TO  SPA-IIDCD
               MOVE    99                   TO  SPA-GMN-CUR-41
               GO      TO      210-BACK-EXT
           END-IF.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC.
      *
      *    前回患者へ待避
           IF      SPA-GMN-PTNUM    NOT =   SPACE
               MOVE    SPA-GMN-PTNUM        TO  SPA-LAST-PTNUM
               MOVE    SPA-GMN-PTID         TO  SPA-LAST-PTID
           END-IF.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
      *??? MOVE    SPA-MOTOPG2         TO  SPA-SAKIPG.
           MOVE    SPA-I40-MOTOPG      TO  SPA-SAKIPG.
           IF      SPA-I40-MOTOPG      =   "M01"
      *        業務選択へ
               MOVE   "M01"                TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
      *        元画面へ
               MOVE    SPACE               TO  LINKAREA
               IF      SPA-I40-MOTOFLG     =   "0"
                                           OR  "1"
                   INITIALIZE                  SPA-KYOTUKEY
               END-IF
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    SPACE               TO  SPA-MOTOPG2
           END-IF.
      *
           MOVE    SPACE               TO  SPA-MOTOPG2.
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC           SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    SPA-I40-MOTOPG      TO  SPA-SAKIPG.
           IF      SPA-I40-MOTOFLG     =   "0"
                                       OR  "1"
               INITIALIZE                  SPA-KYOTUKEY
           END-IF.
           IF      SPA-I40-MOTOPG      =   "M01"
      *        業務選択へ
               MOVE   "M01"                TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE 
      *        元画面へ
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    SPACE               TO  SPA-MOTOPG2
           END-IF.
      *
           MOVE    SPACE               TO  SPA-MOTOPG2.
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       430-MAEKPTNUM-SEC              SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      430-MAEKPTNUM-EXT
           END-IF.
      *
           MOVE    SPA-LAST-PTNUM      TO  I41-PTNUM.
           PERFORM 41010-PTNUM-SYORI-SEC.
      *
       430-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック画面へ
      *****************************************************************
       430-CHEKU-SEC              SECTION.
      *
      *    剤修正のチェック
           IF      SPA-NAI-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-CHEKU-EXT
           END-IF.
      *    患者選択チェック
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      430-CHEKU-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      430-CHEKU-EXT
               END-IF
           END-IF.
      *
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-I40-SRYYMDW(1:6).
           MOVE    SPA-NAI-SRYYM       TO  SPA-I40-SRYYMD (1:6).
           MOVE    SPA-GMN-SRYKA-I-41  TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41  TO  SPA-I40-SRYKAMEI.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    "I42"               TO  SPA-SAKIPG.
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "I42"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       430-CHEKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    フリーコメント画面へ
      *****************************************************************
       430-FREEMSG-SEC              SECTION.
      *
      *    剤修正のチェック
           IF      SPA-NAI-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF.
      *****IF      SPA-GMN-PTNUM       =   SPACE
      *        MOVE    "0019"          TO  SPA-ERRCD
      *        GO      TO      430-FREEMSG-EXT
      *****END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      430-FREEMSG-EXT
               END-IF
           END-IF.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I41             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "I41"               TO  SPA-MOTOPG
           MOVE    "C50"               TO  SPA-SAKIPG
      *
      *****MOVE    SPACE               TO  SPA-KYOTUKEY
      *    遷移後の初期表示
           MOVE    SPA-GMN-SRYYM       TO  SPA-SRYYMDW(1:6)
           MOVE    SPA-NAI-SRYYM       TO  SPA-SRYYMD (1:6)
           MOVE    SPACE               TO  SPA-SRYKA
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-BIRTHDAYW
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAY
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "C50"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *以下削除
      *    MOVE    SPA-GMN-SRYYM      TO  SPA-I40-SRYYMDW(1:6).
      *    MOVE    SPA-NAI-SRYYM      TO  SPA-I40-SRYYMD (1:6).
      *    MOVE    SPA-GMN-SRYKA-I-41    TO  SPA-I40-SRYKA.
      *    MOVE    SPA-GMN-SRYKAMEI-I-41 TO  SPA-I40-SRYKAMEI.
      *
      *    MOVE    SPACE               TO  SPA-MOTOPG
      *    MOVE    SPA-I4KYOUTU        TO  SPA-WORK
      *    MOVE    SPA-AREA            TO  SPA-COMMON
      *    MOVE    SPA-I41             TO  SPA-FREE
      *
      *    MOVE    "LINK"              TO  MCP-STATUS
      *    MOVE    SPACE               TO  MCP-EVENT
      *
      *    CALL    "ORCGI43"           USING
      *                                MCPAREA
      *                                SPAAREA
      *                                LINKAREA
      *                                SCRAREA.
      *
      *    MOVE    SPA-COMMON      TO  SPA-AREA.
      *    MOVE    SPA-FREE        TO  SPA-I41
      *    MOVE    SPA-WORK        TO  SPA-I4KYOUTU
      *
      *    カーソル移動
      *    MOVE    "COMMENT"           TO  MCP-WIDGET
      *
      *    MOVE    "I41"               TO  SPA-MOTOPG
      *    MOVE    "I43"               TO  SPA-SAKIPG
      *
      *    MOVE    "NEW"               TO  MCP-PUTTYPE.
      *    MOVE    "I43"               TO  MCP-WINDOW.
      *
      *    PERFORM 900-PUT-WINDOW.
      *
      *****MOVE    1                   TO  FLG-END
      *
           .
      *
       430-FREEMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    室料差額コラムリスト編集処理
      *****************************************************************
       450-SAGAKULIST-HEN-SEC            SECTION.
      *
           EVALUATE    SPA-GMN-B05-FLG-41
           WHEN    1
               PERFORM 3113-SHOKUJI-HEN-SEC
           WHEN    2
               PERFORM 3113-GAIHAKU-HEN-SEC
           WHEN    3
               PERFORM 3113-SAGAKU-HEN-SEC
           END-EVALUATE
      *
           .
       450-SAGAKULIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分・医療区分入力画面へ
      *****************************************************************
       450-I47-SENI-SEC             SECTION.
      *
      *    剤修正のチェック
           IF      SPA-NAI-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      450-I47-SENI-EXT
           END-IF.
      *    患者選択チェック
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      450-I47-SENI-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      450-I47-SENI-EXT
               END-IF
           END-IF.
      *
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-I40-SRYYMDW(1:6).
           MOVE    SPA-NAI-SRYYM       TO  SPA-I40-SRYYMD (1:6).
           MOVE    SPA-GMN-SRYKA-I-41  TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41  TO  SPA-I40-SRYKAMEI.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    "I47"               TO  SPA-SAKIPG.
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "I47"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       450-I47-SENI-EXT.
           EXIT.
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       41018-ZENGETU-HEN-SEC             SECTION.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41.
      *
      *    剤修正のチェック
           IF      SPA-NAI-NUM-41  NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41018-ZENGETU-HEN-EXT
           END-IF.
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      41018-ZENGETU-HEN-EXT
           END-IF.
      *
           MOVE    1                   TO  SPA-I4-SEL.
      *
           IF      SPA-UPDFLG-41       =   "1"
               MOVE    "0106"              TO  SPA-IIDCD
           ELSE
               PERFORM 41018-ZENGETU-SYORI-SEC
           END-IF.
      *
       41018-ZENGETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       41018-ZENGETU-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    入院会計マスター更新
           IF      SPA-UPDFLG-41          =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
           END-IF.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO       41018-ZENGETU-SYORI-EXT
           END-IF.
      *
           IF      SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM
               MOVE    "0015"          TO  SPA-ERRCD
               GO      TO       41018-ZENGETU-SYORI-EXT
            END-IF.
      *
           MOVE    ZERO                TO  FLG-SET
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
      *
      *    入院会計マスターより対象の判定
           PERFORM 41018-ZENGETU-COMPUTE-SEC
      *
      *    対象なし
           IF      FLG-SET             =   ZERO
               MOVE    "0027"              TO  SPA-ERRCD
               MOVE    07                  TO  SPA-GMN-CUR-41
               GO      TO       41018-ZENGETU-SYORI-EXT
           END-IF
      *
      *    前月内容編集
           MOVE    WRK-SRYYM           TO  SPA-NAI-SRYYM
           MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC.
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           MOVE    SPACE               TO  SPA-I40-SRYKA
           MOVE    SPACE               TO  SPA-I40-SRYKAMEI
           INITIALIZE                  SPA-GMN-AREA2
      *    初期表示
           PERFORM 310-SPA-SET-SEC
      *    入院会計マスターより編集をする
           PERFORM 3101-NYUINACCT-HEN-SEC.
      *
      *    保険組合一覧編集
           PERFORM 3105-HKNCOMBI-HEN-SEC.
      *
      *    保険組合期限切れチェック、外泊の再編集
           PERFORM 3106-DAY-KIKANCHK-SEC
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    07                  TO  SPA-GMN-CUR-41
               MOVE    SPACE               TO  SPA-ERRCD
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
      *
       41018-ZENGETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日前月計算処理
      *****************************************************************
       41018-ZENGETU-COMPUTE-SEC       SECTION.
      *
      *    入院会計マスター判定
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key25"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
           END-IF
      *
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    1               TO  FLG-SET
               MOVE    NACCT-SRYYM     TO  WRK-SRYYM
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       41018-ZENGETU-COMPUTE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       41019-JIGETU-HEN-SEC             SECTION.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41.
      *
      *    剤修正のチェック
           IF      SPA-NAI-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41019-JIGETU-HEN-EXT
           END-IF.
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      41019-JIGETU-HEN-EXT
           END-IF.
      *
           MOVE    2                   TO  SPA-I4-SEL.
           IF      SPA-UPDFLG-41       =   "1"
               MOVE    "0106"              TO  SPA-IIDCD
           ELSE
               PERFORM 41019-JIGETU-SYORI-SEC
           END-IF.
      *
       41019-JIGETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       41019-JIGETU-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    入院会計マスター更新
           IF      SPA-UPDFLG-41          =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
           END-IF.
           IF      SPA-ERRCD      NOT  =   SPACE
               GO      TO       41019-JIGETU-SYORI-EXT
           END-IF.
      *    退院月は対象外
           IF      SPA-NAI-SRYYM       =   SPA-NAI-TAINBI(1:6)
               MOVE    "0028"              TO  SPA-ERRCD
               MOVE    07                  TO  SPA-GMN-CUR-41
               GO      TO       41019-JIGETU-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-SET
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
      *
      *    入院会計マスターより対象の判定
           PERFORM 41019-JIGETU-COMPUTE-SEC
      *
      *    対象なし
           IF      FLG-SET             =   ZERO
               IF      SPA-NAI-SRYYM       =   SPA-NAI-TAINBI(1:6)
                   MOVE    "0027"              TO  SPA-ERRCD
                   MOVE    07                  TO  SPA-GMN-CUR-41
                   GO      TO       41019-JIGETU-SYORI-EXT
               ELSE
                   IF     (SPA-NAI-TAINBI      =   ALL "9") AND
                          (SPA-NAI-TBANGO(1)   NOT = ZERO ) AND
                          (SPA-NAI-TZAISKBKBN-41(1) = "1" ) 
      *                翌月の会計を作成する
                       MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
                       PERFORM 410191-KAIKEI-SAKUSEI-SEC
                   ELSE
      *                退院日がある時、作成しない
                       MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
                   END-IF
               END-IF
           END-IF
      *
      *    翌月内容編集
           MOVE    WRK-SRYYM           TO  SPA-NAI-SRYYM
           MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC.
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           MOVE    SPACE               TO  SPA-I40-SRYKA
           MOVE    SPACE               TO  SPA-I40-SRYKAMEI
           INITIALIZE                  SPA-GMN-AREA2
      *    初期表示
           PERFORM 310-SPA-SET-SEC
      *    入院会計マスターより編集をする
           PERFORM 3101-NYUINACCT-HEN-SEC.
      *
      *    保険組合一覧編集
           PERFORM 3105-HKNCOMBI-HEN-SEC.
      *
      *    保険組合期限切れチェック、外泊の再編集
           PERFORM 3106-DAY-KIKANCHK-SEC
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    07                  TO  SPA-GMN-CUR-41
               MOVE    SPACE               TO  SPA-ERRCD
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
           .
       41019-JIGETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   次月計算処理
      *****************************************************************
       41019-JIGETU-COMPUTE-SEC        SECTION.
      *
      *    入院会計マスター判定
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
           END-IF
      *
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    1               TO  FLG-SET
               MOVE    NACCT-SRYYM     TO  WRK-SRYYM
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       41019-JIGETU-COMPUTE-EXT.
           EXIT.
      *
      *****************************************************************
      *   会計作成処理
      *****************************************************************
       410191-KAIKEI-SAKUSEI-SEC            SECTION.
      *
           COMPUTE WRK-SRYYM-MM        =   WRK-SRYYM-MM   +   1
           IF      WRK-SRYYM-MM        >   12
               MOVE    01              TO  WRK-SRYYM-MM
               COMPUTE WRK-SRYYM-YY    =   WRK-SRYYM-YY   +   1
           END-IF
           MOVE    WRK-SRYYM           TO  SPA-NAI-SRYYM
      *
      *    入院会計テーブル作成処理
           PERFORM 410191-NYUACCT-SAKUSEI-SEC
           IF      IPN-NYUKAIKEI-SAKUSEI-FLG   =   "1"
               MOVE    SPACE   TO   WRK-TOKUTEI-NYUIN
               PERFORM 410191-IPNNYUACCT-SAKUSEI-SEC
           END-IF
      *
           .
      *
       410191-KAIKEI-SAKUSEI-EXT.
           EXIT.
      *
      *021118
      *****************************************************************
      *    入院会計テーブル作成処理                                    
      *****************************************************************
       410191-NYUACCT-SAKUSEI-SEC   SECTION.
      *
           MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
           INITIALIZE      ORCSSYOKIKSNAREA
           MOVE    SPA-NAI-SRYYM       TO  WRK-NAI-SRYYM-41.
      *
      *    通算日数を求める（入院履歴を検索）
           MOVE    ZERO                TO  WRK-TSUSAN
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =  ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key62"             TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                           TO  WRK-TOKUTEI-NYUIN
      *        老人性認知症疾患療養病棟入院料は平成１８年３月迄
               IF     (PTNYUINRRK-TOKUTEI-KBN   =   "2")   AND
                      (PTNYUINRRK-TOKUTEI-NYUIN =   "06")  AND
                      (SPA-NAI-SRYYM           >=  "200604")
                       MOVE      SPACE     TO  WRK-TOKUTEI-NYUIN
               END-IF
               MOVE    PTNYUINRRK-REC      TO  WK-PTNYU-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
      *    通算日数を求める（通算日数取得サブを使用）
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD       NOT =  SPACE )
               GO  TO  410191-NYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYU-REC        TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN          TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    SPA-NAI-SRYYM       TO  SYOKIKSN-IN-SANTEISTYMD(1:6)
           MOVE    "01"                TO  SYOKIKSN-IN-SANTEISTYMD(7:2)
      *
      *     DISPLAY "====AZU  KAIKEI-SAKUSEIL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    平成２４年３月３１日時点で亜急性期入院医療管理料１を算定
      *    している状態か判定するサブを呼び出し
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE    PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
      **             DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *
           IF      WRK-TOKUTEI-NYUIN           NOT =   SPACE
               PERFORM 4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
      *        以下入院料を追加（平成２８年改正で対応）
      *        ・療養病棟・一般病棟７対１入院基本料
      *        ・療養病棟・一般病棟１０対１入院基本料
      *        ・療養病棟・一般病棟１３対１入院基本料
      *        ・療養病棟・一般病棟１５対１入院基本料
      *        以下入院料を追加（平成３０年改正で対応）
      *        ・療養病棟・急性期一般入院料２
      *        ・療養病棟・急性期一般入院料３
      *        ・療養病棟・急性期一般入院料４
      *        ・療養病棟・急性期一般入院料５
      *        ・療養病棟・急性期一般入院料６
      *        ・療養病棟・地域一般入院料１
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         平成２８年改正対応
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         平成３０年改正対応
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"      TO  SPA-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               END-IF
           ELSE
               PERFORM 4801-BTUSEL-SEC
               IF      SYS-5001-BTU-KHNSRYCD   NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM 4801-RJNCD-CHG-SEC
                   PERFORM 4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"          TO  SPA-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
                   MOVE    "2006"          TO  SPA-ERRCD
                   GO  TO  410191-NYUACCT-SAKUSEI-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し（処理区分９により次月分のみ作成）
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE    "9"                 TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                       TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      **     MOVE    SYOKIKSN-IN-SANTEISTYMD
      **                     TO   LNK-SCNYUACCT-STSRYYMD
      **     MOVE    SPA-NAI-I01-IDOYMD
      **                     TO   LNK-SCNYUACCT-EDSRYYMD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-EDSRYYMD
           PERFORM VARYING IDX6        FROM    1   BY   1
                   UNTIL  (IDX6            >     7 )    OR
                          (SYOKIKSN-OT-KGNSRYCD(IDX6)   =  SPACE)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                (IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (IDX6)
      *         DISPLAY "SYOKIKSN-OT-KGNSRYCD(IDX6) = "
      *                                SYOKIKSN-OT-KGNSRYCD(IDX6)
      *         DISPLAY "SYOKIKSN-OT-YUKOSTYMD(IDX6) = "
      *                                SYOKIKSN-OT-YUKOSTYMD(IDX6)
      *         DISPLAY "SYOKIKSN-OT-YUKOEDYMD(IDX6) = "
      *                                SYOKIKSN-OT-YUKOEDYMD(IDX6)
      *         DISPLAY  "IDX6 = " IDX6
           END-PERFORM
      *
      *    医療観察入院セット
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     PTNYUINRRK-TOKUTEI-KBN
                                         TO  LNK-IRYOKNS-KBN
           MOVE     PTNYUINRRK-TOKUTEI-NYUIN
                                         TO  LNK-IRYOKNS-NYUIN
           MOVE     PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
      **     DISPLAY  "==  ORCSIRYOKNS CALL ==="
      **     DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
      **     DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                  TO  LNK-SCNYUACCT-STSRYYMD
                                      LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE    WRK-TSUSAN          TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *    特定入院で入院料が変更になるコードの対応
      *
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "2"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   PTNYUINRRK-NYUINYMD   TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   SYOKIKSN-IN-SANTEISTYMD
                                        TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-SHONUM     TO  LNK-TOKNYUIN2-SHONUM
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           IF    LNK-AKYUSEI-RC          =   1
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-FLG(1)
           END-IF
      *    療養病棟（病床）入院のフラグセット
           INITIALIZE                       SYS-5001-REC
           MOVE    "5001"               TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM    TO  SYS-5001-KBNCD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                        TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                 TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD    TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM          TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC         TO  MCPDATA-REC
           PERFORM  900-SYSKANRI-KEY-READ-SEC
           IF    FLG-SYSKANRI       =   ZERO
                 MOVE   MCPDATA-REC     TO  SYS-5001-REC
                 IF    SYS-5001-BTU-SBT    =   "04"  OR  "09"
                     MOVE    "1"        TO
                                  LNK-TOKNYUIN2-TOKUBETSU-FLG(2)
                 END-IF
           END-IF
      *
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                  IF   LNK-TOKNYUIN2-RC   =   1
                       MOVE   SPACE    TO     WRK-TOKUTEI-NYUIN
                                              SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                  END-IF
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *
      *    入院料加算診療コード取り込み
           PERFORM    410191-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      WRK-TOKUTEI-NYUIN   =   SPACE
               MOVE     "0"        TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"        TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            特定入院基本料（一般病棟入院基本料）はマスタが
      *            平成２６年９月３０日までの為、参照時の日付を変更
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 920-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 920-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"      TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"      TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU =   SPACE
               MOVE    ZERO            TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                       TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9  TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE    "099999913"         TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE   SPA-TAIHI-SHOKUJI    TO  LNK-SCNYUACCT-SHOKUJIKBN
           IF     SYS-5000-SKJ-CREATEKBN   =   "1"
               MOVE    "1"             TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   SPA-TAIHI-SHOKUJI-ASA
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   SPA-TAIHI-SHOKUJI-HIRU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   SPA-TAIHI-SHOKUJI-YORU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
           IF     PTNYUINRRK-SKJKBN    =   "B"  OR  "C"
               MOVE    "1"             TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   PTNYUINRRK-SKJKBN-ASA
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   PTNYUINRRK-SKJKBN-HIRU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   PTNYUINRRK-SKJKBN-YU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
      *    食堂加算
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI        =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570"     TO
                                           LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1               TO  LNK-SCNYUACCT-SHOKUDO-KBN
      **             DISPLAY "SPA-SHOKUDOU-OFF = " SPA-SHOKUDOU-OFF
                   IF    SPA-SHOKUDOU-OFF  =   1
                       MOVE    ZERO        TO  LNK-SCNYUACCT-SHOKUDO-KBN
                   END-IF
               END-IF
           END-IF
      *    保険組合せ
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    SPA-TAIHI-HKN   TO  LNK-SCNYUACCT-HKNCOMBINUM
      *     DISPLAY   "LNK-SCNYUACCT-HKNCOMBINUM = "
      *                              LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE   "00"             TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE    SPA-SRYKA       TO  WRK-TAIHI-SRYKA
           MOVE    "00"            TO  SPA-SRYKA
      **   
      *     DISPLAY "NYUINKAIKEI SAKUSEIMAE HYOUJI"
      *     DISPLAY  "SPA-HOSPNUM = " SPA-HOSPNUM
      *     DISPLAY  "SPA-NYUGAIKBN = " SPA-NYUGAIKBN
      *     DISPLAY  "SPA-PTID = " SPA-PTID
      *     DISPLAY  "SPA-SRYKA = " SPA-SRYKA
      *     DISPLAY  "LNK-SCNYUACCT-STSRYYMD = " LNK-SCNYUACCT-STSRYYMD
      *     DISPLAY  "LNK-SCNYUACCT-EDSRYYMD = " LNK-SCNYUACCT-EDSRYYMD
      *     DISPLAY  "LNK-SCNYUACCT-NYUKHN-SRYCD(1) = " 
      *                                   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *     DISPLAY  "LNK-SCNYUACCT-KAGEN-SRYCD(1) = "
      *                                   LNK-SCNYUACCT-KAGEN-SRYCD(1)
      *     DISPLAY  "LNK-SCNYUACCT-KAGEN-STYUKYMD(1) = "
      *                                LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
      *     DISPLAY  "LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) = "
      *                                LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *     DISPLAY  "LNK-SCNYUACCT-NYUKHN-SRYCD(2) = " 
      *                                   LNK-SCNYUACCT-NYUKHN-SRYCD(2)
      *     DISPLAY  "LNK-SCNYUACCT-KAGEN-SRYCD(2) = "
      *                                   LNK-SCNYUACCT-KAGEN-SRYCD(2)
      *     DISPLAY  "LNK-SCNYUACCT-KAGEN-STYUKYMD(2) = "
      *                                LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
      *     DISPLAY  "LNK-SCNYUACCT-KAGEN-EDYUKYMD(2) = "
      *                                LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *     DISPLAY  "LNK-SCNYUACCT-NYUKHN-NISUU = "
      *                                   LNK-SCNYUACCT-NYUKHN-NISUU
      *     DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(1) = "
      *                               LNK-SCNYUACCT-NYUKHNKSN-SRYCD(1)
      *     DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(2) = "
      *                               LNK-SCNYUACCT-NYUKHNKSN-SRYCD(2)
      *     DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(3) = "
      *                               LNK-SCNYUACCT-NYUKHNKSN-SRYCD(3)
      *     DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(4) = "
      *                               LNK-SCNYUACCT-NYUKHNKSN-SRYCD(4)
      *     DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(5) = "
      *                               LNK-SCNYUACCT-NYUKHNKSN-SRYCD(5)
      *     DISPLAY  "ORCSNYUACCT CALL "
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE     PTNYUINRRK-BTUNUM  TO  LNK-SCNYUACCT-BTU-NUM
           CALL    "ORCSNYUACCT"       USING   SPA-AREA
                                               ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA      TO  SPA-SRYKA
      *     IF     LNK-SCNYUACCT-RC   NOT =  ZERO
      *         DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
      *                   LNK-SCNYUACCT-RC
      *     END-IF
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    SPA-PTID            TO  SS010-PTID
            MOVE    SYOKIKSN-IN-SANTEISTYMD (1:6)
                                        TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "0052"          TO  SPA-ERRCD
            END-IF
      *
           .
      *
       410191-NYUACCT-SAKUSEI-EXT.
           EXIT.
      *
      *21118
      *****************************************************************
      *    入院会計テーブル作成処理（特定入院料後の一般入院料）
      *****************************************************************
       410191-IPNNYUACCT-SAKUSEI-SEC   SECTION.
      *
           INITIALIZE      ORCSSYOKIKSNAREA
           MOVE    IPN-SANTEISTYMD(1:6)  TO WRK-NAI-SRYYM-41(1:6)
      *
      *    通算日数を求める（入院履歴を検索）
           MOVE    ZERO                TO  WRK-TSUSAN
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =  ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key62"             TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-SHONUM
                                       TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-REC  TO  WK-PTNYU-REC
      *         DISPLAY  "PTNYUINRRK-RRKNUM = " PTNYUINRRK-RRKNUM
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
      *    通算日数を求める（通算日数取得サブを使用）
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  410191-IPNNYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYU-REC        TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN          TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    IPN-SANTEISTYMD     TO  SYOKIKSN-IN-SANTEISTYMD
      *
           MOVE    SPACE               TO  WRK-TOKUTEI-NYUIN
           IF      WRK-TOKUTEI-NYUIN   NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD   NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM 4801-RJNCD-CHG-SEC
                   PERFORM 4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"          TO  SPA-ERRCD
                       GO  TO  410191-IPNNYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
                   MOVE    "2006"          TO  SPA-ERRCD
                   GO  TO  410191-IPNNYUACCT-SAKUSEI-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE    "9"                 TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
      *     DISPLAY  "PTNYUINRRK-PTID = " PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                       TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE      SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
                                           LNK-SCNYUACCT-EDSRYYMD
      *     DISPLAY "LNK-SCNYUACCT-STSRYYMD = " LNK-SCNYUACCT-STSRYYMD
           PERFORM VARYING     IDX6    FROM    1   BY   1
               UNTIL          (IDX6        >   7   )   OR
                              (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE)
               MOVE    SYOKIKSN-OT-KGNSRYCD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                             (IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                              (IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                              (IDX6)
           END-PERFORM
      *    通算算定済日数
           MOVE    WRK-TSUSAN          TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *    入院料加算診療コード取り込み
           PERFORM 410191-NYUKSN-GET-SEC
      *    特定入院料区分のセット
           MOVE    "0"             TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *    外泊
           MOVE    "099999911"     TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE    "099999912"     TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU =   SPACE
               MOVE    ZERO            TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                       TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9  TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE    "099999913"         TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE    SPA-TAIHI-SHOKUJI   TO  LNK-SCNYUACCT-SHOKUJIKBN
           IF     SYS-5000-SKJ-CREATEKBN   =   "1"
               MOVE    "1"             TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   SPA-TAIHI-SHOKUJI-ASA
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   SPA-TAIHI-SHOKUJI-HIRU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   SPA-TAIHI-SHOKUJI-YORU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
           IF     PTNYUINRRK-SKJKBN    =   "B"  OR  "C"
               MOVE    "1"             TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   PTNYUINRRK-SKJKBN-ASA
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   PTNYUINRRK-SKJKBN-HIRU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   PTNYUINRRK-SKJKBN-YU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
      *    食堂加算
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI           =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570"     TO
                                           LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1               TO  LNK-SCNYUACCT-SHOKUDO-KBN
                   IF    SPA-SHOKUDOU-OFF  =   1
                       MOVE    ZERO        TO  LNK-SCNYUACCT-SHOKUDO-KBN
                   END-IF
               END-IF
           END-IF
      *    保険組合せ
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    SPA-TAIHI-HKN   TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE    "00"            TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE    SPA-SRYKA       TO  WRK-TAIHI-SRYKA
           MOVE    "00"            TO  SPA-SRYKA
      **   
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
           MOVE    PTNYUINRRK-BTUNUM  TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"    USING   SPA-AREA
                                           ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA  TO  SPA-SRYKA
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    SPA-PTID            TO  SS010-PTID
            MOVE    SYOKIKSN-IN-SANTEISTYMD (1:6)
                                        TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "0052"          TO  SPA-ERRCD
            END-IF
      *
           .
      *
       410191-IPNNYUACCT-SAKUSEI-EXT.
           EXIT.
      *021118
      *****************************************************************
      *    通算日数取得処理
      *****************************************************************
       4801-TSUSAN-SANTEI-GET-SEC      SECTION.
      *
           INITIALIZE                  WRK-TSUSAN
                                       WRK-SANTEISTYMD
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    SPA-HOSPNUM     TO  STUSAN-HOSPNUM
           MOVE    PTNYUINRRK-PTID TO  STUSAN-PTID
           MOVE    SPA-NAI-SRYYM   TO  STUSAN-KJNYMD(1:6)
           IF   IPN-NYUKAIKEI-SAKUSEI-FLG  =   "1"
               MOVE   IPN-SANTEISTYMD(7:2) TO  STUSAN-KJNYMD(7:2)
           ELSE
               MOVE    "01"                TO  STUSAN-KJNYMD(7:2)
           END-IF
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF    ( STUSAN-RC       NOT =   ZERO )
               MOVE    "2003"      TO  SPA-ERRCD
               GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
      *    他院（特別な関係にある医療機関）の日数を加算
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU03
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU05
      *
           COMPUTE WRK-TSUSAN      =   STUSAN-NISSU03  -   1
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    STUSAN-KJNYMD   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
      *     DISPLAY "STUSAN-NISSU01 =   "  STUSAN-NISSU01
      *     DISPLAY "STUSAN-NISSU02 =   "  STUSAN-NISSU02
      *     DISPLAY "STUSAN-NISSU03 =   "  STUSAN-NISSU03
      *     DISPLAY "STUSAN-NISSU04 =   "  STUSAN-NISSU04
           COMPUTE WRK-ZOGEN       =   180 - ( STUSAN-NISSU04 )
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
      *     DISPLAY "************************"
      *     DISPLAY "** TSUSAN-SANTEI      **"
      *     DISPLAY "WRK-TSUSAN       = " WRK-TSUSAN
      *     DISPLAY "WRK-SANTEISTYMD  = " WRK-SANTEISTYMD
      *     DISPLAY "WRK-SENTEI-STYMD = " WRK-SENTEI-STYMD
      *     DISPLAY "************************"
      *
           .
       4801-TSUSAN-SANTEI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       5002-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       5002-CALENDAR-EXT.
           EXIT.
      *
      *****************************************************************
      *     選定療養費診療行為コード取得処理
      *****************************************************************
       4801-SENTEI-SET-SEC         SECTION.
      *
           INITIALIZE                  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                       TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      **     MOVE    SENCHG-SENTEISRYCD
      **                               TO  LNK-SCNYUACCT-SENTEI-SRYCD
           IF      SENCHG-SENTEISRYCD  NOT =  SPACE
               MOVE    "099999916"     TO  LNK-SCNYUACCT-SENTEI-SRYCD
               MOVE    WRK-SENTEI-STYMD
                                       TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
           MOVE    WRK-SENTEI-STYMD    TO  LNK-SCNYUACCT-SENTEI-STYMD
      *    １５才未満の患者は選定入院対象外
           MOVE     PTINF-BIRTHDAY     TO  WRK-BIRTHDAY-15
           ADD      15                 TO  WRK-BIRTHDAY-15Y
           IF       WRK-SENTEI-STYMD   <=  WRK-BIRTHDAY-15
               MOVE   WRK-BIRTHDAY-15  TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
      *
      *     DISPLAY "LNK-SCNYUACCT-SENTEI-SRYCD = "
      *              LNK-SCNYUACCT-SENTEI-SRYCD
      *     DISPLAY "LNK-SCNYUACCT-SENTEI-STYMD = "
      *              LNK-SCNYUACCT-SENTEI-STYMD
           .
       4801-SENTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       410191-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING
                                           STS-AREA-DAY
                                           LNK-DAY5-AREA
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *     DISPLAY "410191-KIKAN-CALC-SEC WRK-STYMD = " WRK-STYMD
      *     DISPLAY "410191-KIKAN-CALC-SEC WRK-EDYMD = " WRK-EDYMD
      *     DISPLAY "410191-KIKAN-CALC-SEC WRK-NISSU2 = " WRK-NISSU2
      *
           .
       410191-KIKAN-CALC-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料セット処理
      *****************************************************************
       4801-TOKTEI-NYUIN-SET-SEC   SECTION.
      *
      *    特定入院料コードをサブプロより取得
           INITIALIZE                           ORCSTOKNYUIN-AREA
           MOVE    PTNYUINRRK-TOKUTEI-KBN   TO  LNK-TOKNYUIN-KBN
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN TO  LNK-TOKNYUIN-NYUIN
           MOVE    PTNYUINRRK-BTUNUM        TO  LNK-TOKNYUIN-BTUNUM
           MOVE    PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                        TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
           MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                            TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF      LNK-TOKNYUIN-RC          =   ZERO
               IF    LNK-TOKNYUIN-GAI-SRYCD =  SPACE
                   MOVE   LNK-TOKNYUIN-SRYCD   TO  SYOKIKSN-IN-KHNSRYCD
               ELSE
                   MOVE   LNK-TOKNYUIN-GAI-SRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の取り込み処理
      *****************************************************************
       410191-NYUKSN-GET-SEC         SECTION.
      *
      *    入院料加算取得サブ
           INITIALIZE                       ORCSNYUKSNGET-AREA
           MOVE   SPA-HOSPNUM           TO  LNK-NYUKSNGET-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-NYUKSNGET-PTID
           MOVE   SPA-NAI-SRYYM         TO  LNK-NYUKSNGET-SANTEIYM
           MOVE   PTNYUINRRK-BTUNUM     TO  LNK-NYUKSNGET-BTUNUM
           MOVE   PTNYUINRRK-BRMNUM     TO  LNK-NYUKSNGET-BRMNUM
           IF      LNK-SCNYUACCT-NYUKHN-SRYCD(1)   NOT = SPACE
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           ELSE
               MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           END-IF
      *
      *    特定入院料算定要件外で療養病棟入院基本料Ｉを算定時に、入院料
      *    加算を特別判定する場合の判定区分セット
           IF      LNK-TOKNYUIN-GAI-NYUKSNKBN   =    "1"
               MOVE   "1"               TO  LNK-NYUKSNGET-GAI-NYUKSNKBN
           END-IF
      *
           CALL    "ORCSNYUKSNGET"      USING  ORCSNYUKSNGET-AREA
                                               SPA-AREA
      *    入院料加算取得サブからの返却値セット
           PERFORM    VARYING    IDX8   FROM   1   BY   1
             UNTIL    (LNK-NYUKSNGET-KSNSRYCD(IDX8)  =  SPACE)  OR
                      (IDX8   >     20)
                 MOVE   LNK-NYUKSNGET-NYUKSNKBN(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-KBN(IDX8)
                 MOVE   LNK-NYUKSNGET-KSNSRYCD(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
           END-PERFORM
           MOVE    LNK-NYUKSNGET-TOKUBETSU-NYUIN
                                        TO  WRK-TOKUBETSU-NYUIN
      *
           .
      *
       410191-NYUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
      *     DISPLAY  "4801-BTUSEL-SEC"
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                       TO  SYS-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           ELSE
               INITIALIZE                  SYS-5001-REC
           END-IF
      *
           .
       4801-BTUSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    夜勤時間超過減算コードへの置き換え処理
      *    夜勤時間特別入院基本料コードへの置き換え処理
      *    看護職員数等入院基本料コードへの置き換え処理
      *****************************************************************
       4801-YAKINGEN-CHG-SEC   SECTION.
      *
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20140401"
      *        夜勤時間超過減算入院料コード取得
               INITIALIZE    ORCSYAKINGEN-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINGEN-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINGEN-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINGEN-SRYCD
               CALL   "ORCSYAKINGEN"    USING   ORCSYAKINGEN-AREA
                                                SPA-AREA
               IF   LNK-YAKINGEN-RC     =       ZERO
                    MOVE LNK-YAKINGEN-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20160401"
      *        夜勤時間特別入院基本料コード取得
               INITIALIZE    ORCSYAKINTOKU-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINTOKU-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINTOKU-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINTOKU-SRYCD
               CALL   "ORCSYAKINTOKU"    USING  ORCSYAKINTOKU-AREA
                                                SPA-AREA
               IF   LNK-YAKINTOKU-RC     =       ZERO
                    MOVE LNK-YAKINTOKU-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
      *        看護職員数等入院基本料コード取得
               INITIALIZE    ORCSKANGOCHG-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-KANGOCHG-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-KANGOCHG-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-KANGOCHG-SRYCD
               CALL   "ORCSKANGOCHG"    USING   ORCSKANGOCHG-AREA
                                                SPA-AREA
               IF   LNK-KANGOCHG-RC     =       ZERO
                    MOVE LNK-KANGOCHG-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-YAKINGEN-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料一般コードから老人コードへの置き換え処理
      *****************************************************************
       4801-RJNCD-CHG-SEC   SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
      *    老人対象患者であれば入院診療コードの置き換え（労災除く）
           IF     (AGECHK-HKNKBN       =   1  OR  2 )  OR
                  (AGECHK-ROUJIN2      =   ZERO     )
               CONTINUE
           ELSE
      *        平成１８年４月改定対応
      *        老人コード置き換えは療養病棟、療養病床のみ
               IF   SYOKIKSN-IN-SANTEISTYMD(1:6)  >=  "200604"
                   IF   SYOKIKSN-IN-KHNSRYCD  =  "190080610" OR
                                                 "190080710" OR
                                                 "190081310" OR
                                                 "190081410" OR
                                                 "190098210" OR
                                                 "190098310"
                       INITIALIZE              SRYCDCHG-REC
                       MOVE    SYOKIKSN-IN-KHNSRYCD
                                               TO   CHG-IPNSRYCD
                       MOVE    SPA-HOSPNUM     TO  CHG-HOSPNUM
                       MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                       MOVE    "SRYCDCHG-KEY2" TO   WRK-DBPATH
                       MOVE    "tbl_srycdchg"  TO  MCP-TABLE
                       MOVE    "key2"          TO  MCP-PATHNAME
                       PERFORM 900-TBL-SELECT-SEC 
                       IF    ( FLG-DBRC    =   ZERO )
                           MOVE    MCPDATA-REC
                                               TO   SRYCDCHG-REC
                           IF   CHG-RJNSRYCD   NOT =   SPACE
                                MOVE   CHG-RJNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                           END-IF
                       END-IF
      *
                       MOVE    "tbl_srycdchg"  TO  MCP-TABLE
                       MOVE    "key2"          TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
                   END-IF
               ELSE
                   INITIALIZE              SRYCDCHG-REC
                   MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                   MOVE    SPA-HOSPNUM     TO  CHG-HOSPNUM
                   MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                   MOVE    "SRYCDCHG-KEY2" TO   WRK-DBPATH
                   MOVE    "tbl_srycdchg"  TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 900-TBL-SELECT-SEC 
                   IF    ( FLG-DBRC    =   ZERO )
                       MOVE    MCPDATA-REC
                                           TO   SRYCDCHG-REC
                       IF   CHG-RJNSRYCD   NOT =   SPACE
                            MOVE   CHG-RJNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_srycdchg"  TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-IF
           END-IF
      *
           .
       4801-RJNCD-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料の算定期間置き換え
      *****************************************************************
       4801-KIKAN-CHG-SEC   SECTION.
      *
           MOVE      ZERO              TO   KIKAN-IDX2
           PERFORM   VARYING   KIKAN-IDX1   FROM  1  BY  1
             UNTIL  (KIKAN-IDX1        >    6  )
              IF    LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX1)
                                    <  LNK-SCNYUACCT-STSRYYMD
                IF    LNK-SCNYUACCT-STSRYYMD    <=
                          LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX1)
                    ADD   1       TO   KIKAN-IDX2
                    MOVE  LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX2)
                    MOVE  LNK-SCNYUACCT-STSRYYMD   TO
                          LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX2)
                    MOVE  LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX2)
                END-IF
              ELSE
                    ADD   1       TO   KIKAN-IDX2
                    MOVE  LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX2)
                    MOVE  LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX2)
                    MOVE  LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX2)
              END-IF
           END-PERFORM 
      *
           IF    KIKAN-IDX2    NOT =  ZERO
             ADD       1         TO    KIKAN-IDX2
             PERFORM   VARYING   KIKAN-IDX1   FROM  KIKAN-IDX2  BY  1
               UNTIL  (KIKAN-IDX1        >    6  )
                 MOVE   SPACE   TO  
                             LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX1)
                             LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX1)
                             LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX1)
             END-PERFORM
           END-IF

           .
       4801-KIKAN-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           IF      SPA-NAI-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      470-SIMEIKEN-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      470-SIMEIKEN-EXT
               END-IF
           END-IF.
      *
           MOVE    SPA-GMN-SRYKAMEI-I-41 TO  SPA-I40-SRYKAMEI.
           MOVE    SPA-GMN-SRYKA-I-41    TO  SPA-I40-SRYKA.
      *
           MOVE    SPA-I4KYOUTU          TO  SPA-WORK.
           MOVE    SPA-AREA              TO  SPA-COMMON.
      *
           MOVE    SPACE                 TO  LINKAREA.
           MOVE    SPA-KYOUTU            TO  LNK-COMMON.
           MOVE    SPA-I41               TO  LNK-SCRSPA.
      *
      *    画面移行用のパラメタ変更
           MOVE    "I41"                 TO  SPA-MOTOPG.
           MOVE    "P97"                 TO  SPA-SAKIPG.
      *
      *    氏名をクリアする
           MOVE    SPACE                 TO  SPA-NAME.
           MOVE    SPA-KYOUTU            TO  LNK-KYOUTU.
      *
           MOVE    "CHANGE"              TO  MCP-PUTTYPE.
           MOVE    "P97"                 TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                     TO  FLG-END.
      *
       470-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    剤修正のチェック
           IF      SPA-NAI-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      480-YOYAKU-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      480-YOYAKU-EXT
               END-IF
           END-IF.
      *
           MOVE    SPA-GMN-PTID           TO  SPA-PTID.
           MOVE    SPA-GMN-PTNUM          TO  SPA-PTNUM.
           MOVE    SPA-GMN-NAME           TO  SPA-NAME.
           MOVE    SPA-GMN-SEX            TO  SPA-SEX.
           MOVE    SPA-GMN-BIRTHDAYW      TO  SPA-BIRTHDAYW.
           MOVE    SPA-GMN-SRYKA-I-41     TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41  TO  SPA-I40-SRYKAMEI.
      *
           MOVE    "I41"                  TO  SPA-MOTOPG.
           MOVE    "I41"                  TO  SPA-MOTOPG3.
           MOVE    "Y01"                  TO  SPA-SAKIPG.
      *
           MOVE   "CHANGE"                TO  MCP-PUTTYPE.
           MOVE   "Y01"                   TO  MCP-WINDOW.
      *
           MOVE    SPA-I4KYOUTU           TO  SPA-WORK.
           MOVE    SPA-AREA               TO  SPA-COMMON.
      *
           MOVE    SPACE                  TO  LINKAREA.
           MOVE    SPA-KYOUTU             TO  LNK-COMMON.
           MOVE    SPA-KYOUTU             TO  LNK-KYOUTU.
           MOVE    SPA-I41                 TO  LNK-SCRSPA.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                      TO  FLG-END.
      *
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           IF      SPA-NAI-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      490-UKEICHI-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      490-UKEICHI-EXT
               END-IF
           END-IF.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPA-GMN-SRYKA-I-41     TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41  TO  SPA-I40-SRYKAMEI.
           MOVE    SPA-I4KYOUTU           TO  SPA-WORK.
           MOVE    SPA-AREA               TO  SPA-COMMON.
      *
           MOVE    SPACE                  TO  LINKAREA.
           MOVE    SPA-KYOUTU             TO  LNK-COMMON.
           MOVE    SPA-I41                 TO  LNK-SCRSPA.
      *
      *    画面移行用のパラメタ変更
           MOVE    "I41"                  TO  SPA-MOTOPG.
           MOVE    "U01"                  TO  SPA-SAKIPG.
           MOVE    SPA-KYOUTU             TO  LNK-KYOUTU.
      *
           MOVE   "CHANGE"                TO  MCP-PUTTYPE.
           MOVE   "U01    "               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                      TO  FLG-END.
      *
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       490-TOROKU-SEC              SECTION.
      *
      *    更新前のチェック
           IF      SPA-NAI-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      490-TOROKU-EXT
           END-IF.
      *
      *    入院会計マスター更新
           PERFORM 4901-NYUINACCT-UPD-SEC.
           IF     (SPA-ERRCD       NOT =   SPACE)   OR
                  (SPA-IIDCD       NOT =   SPACE)
               GO      TO      490-TOROKU-EXT
           END-IF.
      *
           PERFORM 490-TOROKU-CRE-SEC.
      *
       490-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録終了　処理
      *****************************************************************
      *
       490-TOROKU-CRE-SEC                SECTION.
      *
      *    患者が選択されてきた時は、元の画面へ
           IF      SPA-I40-MOTOFLG     =   "0"
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *        初期表示
               INITIALIZE                   SPA-I40-SET
               PERFORM 3001-INIT-HENSYU-SEC
           ELSE
      *        更新後、戻るへ
               PERFORM 210-BACK
           END-IF.
      *
       490-TOROKU-CRE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料の回数重複チェック処理
      *****************************************************************
       49011-NYUINACCT-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-NYUDAY-TBL
      *    回数重複チェック
           PERFORM VARYING IDX         FROM  1  BY 1
                   UNTIL  (IDX         >   SPA-GMN-MAX-41 )
                       OR (SPA-ERRCD   NOT =   SPACE )
             IF     (SPA-GMN-TBLREC (IDX)(1:3) =   ALL "-" )  OR
                    (SPA-GMN-TBANGO (IDX)      =   ZERO    )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-TBANGO (IDX) TO  IDX-N
      *
      *            入院基本料
                   IF     (SPA-GMN-TZAISKBKBN-41 (IDX) =   "1"  )  AND
                          (SPA-GMN-TSRYCD (IDX)    NOT =   W-YOYAKU)
                       PERFORM VARYING IDK     FROM    1   BY  1
                               UNTIL  (IDK     >   31   )
                                   OR (SPA-ERRCD   NOT =   SPACE )
                           IF      (SPA-NAI-DAY-41 (IDX-N IDK)
                                                       >   ZERO )
                               IF      WRK-NYUDAY (IDK)  =   ZERO
                                   MOVE    1       TO  WRK-NYUDAY (IDK)
                               ELSE
                                   MOVE    "0051"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
           .
       49011-NYUINACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター更新処理
      *****************************************************************
       4901-NYUINACCT-UPD-SEC            SECTION.
      *
      *    対象がない時、処理なし
           IF      SPA-UPDFLG-41       =   SPACE
               GO      TO      4901-NYUINACCT-UPD-EXT
           END-IF
      *@@
      *    回数重複チェック
           PERFORM 49011-NYUINACCT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4901-NYUINACCT-UPD-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-UPDFLG-41.
      *
      *    入院会計マスターを編集
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-NAI-MAX-41)
                          OR (SPA-ERRCD    NOT =   SPACE )
               INITIALIZE                          NYUINACCT-REC
               MOVE    SPA-HOSPNUM             TO  NACCT-HOSPNUM
               MOVE    SPA-NAI-TNYUGAIKBN(IDX) TO  NACCT-NYUGAIKBN
               MOVE    SPA-GMN-PTID            TO  NACCT-PTID
               MOVE    SPA-NAI-TSRYKA-41 (IDX) TO  NACCT-SRYKA
               MOVE    SPA-NAI-SRYYM           TO  NACCT-SRYYM
               MOVE    SPA-NAI-SRYKBN-41 (IDX) TO  NACCT-SRYKBN
               MOVE    SPA-NAI-ZAINUM-41 (IDX) TO  NACCT-ZAINUM
     *
               MOVE    NYUINACCT-REC           TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF      FLG-NYUINACCT        =   ZERO
      *            入院会計マスターを変更する
                   PERFORM 4901-NYUINACCT-KOUSIN-SEC
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD        TO  NACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  NACCT-UPHMS
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "I41 NYUINACCT UPD ERR:" MCP-RC
                                           ",KEY:" ACCT-KEY
                        MOVE    "2000"         TO  SPA-ERRCD
                   END-IF
               ELSE
                   DISPLAY "I41 NYUINACCT READ ERR:" NACCT-KEY
      **             MOVE    "2000"         TO  SPA-ERRCD
      **********   GO  TO  4901-NYUINACCT-UPD-EXT
               END-IF
      *
      *        一括修正の時、以降の入院診療会計を更新する
               IF      SPA-NAI-TIKKATUFLG (IDX) =   "1"
                   PERFORM 49011-NYUINACCT-ALLUPD-SEC
               END-IF
           END-PERFORM
      *    主科設定
           PERFORM    600-SYUKA-SET-SEC
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    SPA-GMN-PTID        TO  SS010-PTID
            MOVE    SPA-NAI-SRYYM       TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "0052"          TO  SPA-ERRCD
            END-IF
      *
           .
       4901-NYUINACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター更新処理（一括修正で終了日が９９の時）
      *****************************************************************
       49011-NYUINACCT-ALLUPD-SEC       SECTION.
      *
      *    該当開始日の退院日
           MOVE    SPA-NAI-SRYYM           TO  WRK-SRYYMD(1:6)
           MOVE    SPA-NAI-TIKKATUDAY(IDX) TO  WRK-SRYYMD(7:2)
      *    患者入院履歴を検索する
           INITIALIZE                  PTNYUINRRK-REC.
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
           MOVE    WRK-SRYYMD          TO  PTNYUINRRK-TENSTUYMD
           MOVE    WRK-SRYYMD          TO  PTNYUINRRK-TENNYUYMD
      *
           MOVE    "PTNYUINRRK-KEY19"  TO  WRK-DBPATH
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC.
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key19"             TO  MCP-PATHNAME
               PERFORM 970-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           MOVE    SPACE               TO  WRK-TAIINYMD
           PERFORM
                   UNTIL      FLG-PTNYUINRRK    =   1
      *        転入日
               IF      WRK-SRYYMD      <   PTNYUINRRK-TENNYUYMD
                   MOVE    1           TO  FLG-PTNYUINRRK
               ELSE
      *            退院日取得
                   MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-TAIINYMD
               END-IF
      *
               IF      FLG-PTNYUINRRK      =   ZERO
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key19"             TO  MCP-PATHNAME
                   PERFORM 970-PTNYUINRRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-NAI-TBANGO (IDX)    TO  IDG
           MOVE    SPA-GMN-TSRYCD (IDG)    TO  WRK-SRYCD
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM-N
           MOVE    ZERO                TO  FLG-SYORI-END
           PERFORM
                   UNTIL   (WRK-SRYYM-N     >=  WRK-TAIINYMD-YM) OR
                           (FLG-SYORI-END   =   1        )
               COMPUTE WRK-SRYMM-N         =   WRK-SRYMM-N
                                           +   1
               IF      WRK-SRYMM-N         >   12
                   COMPUTE WRK-SRYYY-N         =   WRK-SRYYY-N   +   1
                   MOVE    1                   TO  WRK-SRYMM-N
               END-IF
      *        月末日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-SRYYM-N         TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-MATU
      *        保険組合せの時、期限内のみ
               IF      SPA-NAI-TZAISKBKBN-41 (IDX)
                                           =   "5"
                   PERFORM 490110-HKNCOMBI-CHK-SEC
               END-IF
      *        入院診療行為より対象の剤を更新する
               IF      WRK-MATU            >   ZERO
                   PERFORM  490111-NYUINACCT-UPD3-SEC
               END-IF
           END-PERFORM
      *
           .
       49011-NYUINACCT-ALLUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せの時、期限内チェック処理
      *****************************************************************
       490110-HKNCOMBI-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-HKNCOMBINUM
           MOVE    SPA-NAI-TIKKATUKAI (IDX)
                                       TO  WRK-HKNCOMBINUM(2:3)
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKNCOMBI-MAX
               IF      SPA-I41-THKNCOMBI (IDH) =   WRK-HKNCOMBINUM
                   IF      SPA-I41-TTEKEDYMD (IDH)(1:6)
                                                   =  WRK-SRYYM-N
                       MOVE    SPA-I41-TTEKEDYMD (IDH)(7:2)
                                               TO  WRK-MATU
                   END-IF
      *            期間外の算定
                   IF      SPA-I41-TTEKEDYMD (IDH)(1:6)
                                                   <  WRK-SRYYM-N
                       MOVE    ZERO            TO  WRK-MATU
                       MOVE    1               TO  FLG-SYORI-END
                   END-IF
                   MOVE    SPA-HKNCOMBI-MAX    TO  IDH
               END-IF
           END-PERFORM
      *
           .
       490110-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為より対象の剤を更新処理
      *****************************************************************
       490111-NYUINACCT-UPD3-SEC       SECTION.
      *
      *    入院行為マスター読み込み
           INITIALIZE                      NYUINACT-REC
           MOVE    SPA-HOSPNUM         TO  NSRY-HOSPNUM
           MOVE    "1"                 TO  NSRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  NSRY-PTID
           MOVE    WRK-SRYYM-N         TO  NSRY-SRYYM
           MOVE    WRK-SRYCD           TO  NSRY-SRYCD (1)
           MOVE    WRK-SRYCD           TO  NSRY-SRYCD (2)
           MOVE    WRK-SRYCD           TO  NSRY-SRYCD (3)
           MOVE    WRK-SRYCD           TO  NSRY-SRYCD (4)
           MOVE    WRK-SRYCD           TO  NSRY-SRYCD (5)
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF.
           IF      FLG-NYUINACT        =   1
               MOVE    1               TO  FLG-SYORI-END
           END-IF
      *
           PERFORM UNTIL       FLG-NYUINACT    =   1
                         OR   (SPA-ERRCD   NOT =   SPACE )
               INITIALIZE                      NYUINACCT-REC
               MOVE    NSRY-HOSPNUM        TO  NACCT-HOSPNUM
               MOVE    NSRY-NYUGAIKBN      TO  NACCT-NYUGAIKBN
               MOVE    NSRY-PTID           TO  NACCT-PTID
               MOVE    NSRY-SRYKA          TO  NACCT-SRYKA
               MOVE    NSRY-SRYYM          TO  NACCT-SRYYM
               MOVE    NSRY-SRYKBN         TO  NACCT-SRYKBN
               MOVE    NSRY-ZAINUM         TO  NACCT-ZAINUM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF      FLG-NYUINACCT        =   ZERO
      *            入院会計マスターを変更する
                   IF      NACCT-SRYYM      =   WRK-TAIINYMD-YM
                       MOVE    WRK-TAIINYMD-DD     TO  IDX-D2
                       IF      WRK-MATU            <   IDX-D2
                           MOVE    WRK-MATU            TO  IDX-D2
                       END-IF
                   ELSE
                       MOVE    WRK-MATU            TO  IDX-D2
                   END-IF
                   PERFORM VARYING     IDX-D   FROM    1      BY  1
                           UNTIL       IDX-D   >   IDX-D2
                       MOVE    SPA-NAI-TIKKATUKAI (IDX)
                                              TO  NACCT-DAY (IDX-D)
                   END-PERFORM
      *            剤回数
                   PERFORM 49011-ZAIKAISU-UPD-SEC
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD        TO  NACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  NACCT-UPHMS
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        MOVE    1              TO  FLG-NYUINACT
                        DISPLAY "I41 NYUINACCT UPD ERR02:" MCP-RC
                                               ",KEY:" NACCT-KEY
                        MOVE    "2000"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   DISPLAY "I41 NYUINACCT READ ERR02:" NACCT-KEY
      **              MOVE    "2000"          TO  SPA-ERRCD
      ************ GO  TO  490111-NYUINACCT-UPD3-EXT
               END-IF
      *
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           END-PERFORM.
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC.
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    SPA-GMN-PTID        TO  SS010-PTID
            MOVE    WRK-SRYYM-N         TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "0052"          TO  SPA-ERRCD
            END-IF
      *
            .
      *
       490111-NYUINACCT-UPD3-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター変更処理
      *****************************************************************
       4901-NYUINACCT-KOUSIN-SEC         SECTION.
      *
      *    回数テーブル（１日から３１日）
           MOVE    SPA-NAI-DAY-AREA-41 (IDX) TO  NACCT-DAY-AREA.
      *
      *    月末日を取得
           INITIALIZE                        STS-AREA-DAY
           INITIALIZE                        LNK-DAY7-AREA
           MOVE    "71"                TO    LNK-DAY7-IRAI
           MOVE    NACCT-SRYYM         TO    LNK-DAY7-YM
           CALL    "ORCSDAY"     USING       STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO    WRK-MATSUYMD
      *
           EVALUATE    NACCT-ZAISKBKBN
      *        最終日の食事を退避（次月会計の作成用）
               WHEN    "4"
                   IF    NACCT-ZAITEN   =   ZERO
                       MOVE    ZERO           TO  SPA-TAIHI-SHOKUJI
                       IF    NACCT-SRYCDTOTAL   =   099999913  OR
                                                    099999920
                           MOVE   NACCT-DAY(WRK-MATSUDD)
                                              TO  SPA-TAIHI-SHOKUJI
                       END-IF
                   ELSE
      *                食堂加算
                       IF    NACCT-SRYCDTOTAL   =   197000570
                             MOVE      1          TO   SPA-SHOKUDOU-OFF
                             IF   NACCT-DAY(WRK-MATSUDD)  NOT = ZERO
                                 MOVE   ZERO  TO  SPA-SHOKUDOU-OFF
                             END-IF
                       END-IF
                   END-IF
      *        最終日の保険組合せを退避（次月会計の作成用）
               WHEN    "5"
                   MOVE    ZERO            TO  SPA-TAIHI-HKN
                   PERFORM VARYING IDX-HKN     FROM   1   BY  1
                           UNTIL   IDX-HKN     >   31
                       IF      NACCT-DAY (IDX-HKN)  NOT =   ZERO
                           MOVE   NACCT-DAY(IDX-HKN)
                                               TO  SPA-TAIHI-HKN
                      END-IF
                   END-PERFORM
           END-EVALUATE
      *
      *    剤回数
           PERFORM 49011-ZAIKAISU-UPD-SEC
           .
      *
       4901-NYUINACCT-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤回数更新編集処理
      *****************************************************************
       49011-ZAIKAISU-UPD-SEC              SECTION.
      *
           MOVE    ZERO                TO  NACCT-ZAIKAISU
           PERFORM VARYING     IDX-D   FROM    1      BY  1
                   UNTIL       IDX-D   >   31
               IF      NACCT-DAY (IDX-D)   NOT =   ZERO
                   ADD     1                   TO  NACCT-ZAIKAISU
               END-IF
           END-PERFORM
           .
      *
       49011-ZAIKAISU-UPD-EXT.
           EXIT.
      *
      *ver.4.7
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4010-PREVIEW-SYORI-SEC          SECTION.
      *
      *    剤修正のチェック
           IF      SPA-NAI-NUM-41  NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      4010-PREVIEW-SYORI-EXT
           END-IF
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      4010-PREVIEW-SYORI-EXT
           END-IF
      *
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      4010-PREVIEW-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-XC01-HKNKBN
           MOVE    SPACE               TO  SPA-XC01-RSI-HKNKBN
           MOVE    SPACE               TO  SPA-XC01-CHK
           MOVE    SPACE               TO  WRK-HKNCOMBI-CHK
           MOVE    ZERO                TO  FLG-HKN-OK
           MOVE    ZERO                TO  FLG-RSIHKN-OK
           MOVE    ZERO                TO  FLG-KOGHKN-OK
           MOVE    ZERO                TO  WRK-RSI-CNT
           MOVE    ZERO                TO  WRK-JIB-CNT
           MOVE    ZERO                TO  WRK-KOG-CNT
      *
           MOVE    ZERO                TO  FLG-DAISAN
           MOVE    SPACE               TO  SPA-XC01-DAISANFLG
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-NAI-MAX-41
               IF      SPA-NAI-TZAISKBKBN-41 (IDX) =   "5"
      *            保険判定
                   PERFORM VARYING IDK     FROM    1   BY  1
                           UNTIL   IDK     >   SPA-NAI-SRYMATU
                       IF      SPA-NAI-DAY-41 (IDX IDK)    NOT =   ZERO
                           MOVE    ZERO        TO  WRK-HKNCOMBINUM
                           MOVE    SPA-NAI-DAY-41 (IDX IDK)
                                               TO  WRK-HKNCOMBINUM(2:3)
                           IF      WRK-HKNCOMBINUM
                                               NOT =   WRK-HKNCOMBI-CHK
                               PERFORM 40102-PRV-HKNCOMBI-CHK-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF     (WRK-HKNCOMBI-CHK    =   SPACE )
      *        保険組合せの剤なし
               MOVE    "0071"          TO  SPA-ERRCD
           ELSE
      *        プレビュー対象の全保険内容チェック処理
               PERFORM 40102-PREVIEW-HKNCHK-SEC
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
               IF      SPA-XC01-CHK        =   "1"
      *            プレビュー保険選択画面へ
                   PERFORM 40104-I411-SYORI-SEC
               ELSE
      *!!!
      *          第三者行為あり
                 IF     (SPA-XC01-HKNKBN     =   "0"  )
                   AND  (SPA-XC01-DAISANFLG  =   "1"  )
                   MOVE    "0120"          TO  SPA-IIDCD
                 ELSE
      *            プレビュー処理
                   MOVE    SPACE           TO  SPA-I4ID-FLG
                   PERFORM 4130-PREVIEW-SEC
                 END-IF
               END-IF
           END-IF
           .
       4010-PREVIEW-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー対象の全保険内容チェック処理
      *****************************************************************
       40102-PRV-HKNCOMBI-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKNCOMBI-MAX
               IF      SPA-I41-THKNCOMBI (IDH) =   WRK-HKNCOMBINUM
      *            保険判定
                   EVALUATE    SPA-I41-HKNNUM (IDH)
                       WHEN    "971"
      *                労災
                       WHEN    "973"
      *                自賠責
                           IF      SPA-I41-HKNNUM (IDH)    =   "971"
                               ADD     1               TO  WRK-RSI-CNT
                           ELSE
                               ADD     1               TO  WRK-JIB-CNT
                           END-IF
                           IF      FLG-RSIHKN-OK       =   ZERO
                               MOVE    SPA-I41-RSI-HKNKBN (IDH)
                                                   TO  FLG-RSIHKN-OK
                           END-IF
                       WHEN    "975"
      *                公害
                           ADD     1               TO  WRK-KOG-CNT
                           MOVE    1               TO  FLG-KOGHKN-OK
                       WHEN    OTHER
                           MOVE    1               TO  FLG-HKN-OK
      *                第三者行為
                           IF      SPA-I41-RSI-HKNKBN (IDH)  =   "6"
                               MOVE    1           TO  FLG-DAISAN
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
           MOVE    WRK-HKNCOMBINUM     TO  WRK-HKNCOMBI-CHK
           .
       40102-PRV-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー対象の全保険内容チェック処理
      *****************************************************************
       40102-PREVIEW-HKNCHK-SEC         SECTION.
      *
      *    健保のみ
           IF    ( FLG-HKN-OK      NOT =   ZERO  )
            AND  ( WRK-RSI-CNT         =   ZERO  )
            AND  ( WRK-JIB-CNT         =   ZERO  )
            AND  ( WRK-KOG-CNT         =   ZERO  )
               MOVE    "0"                 TO  SPA-XC01-HKNKBN
               MOVE    SPACE               TO  SPA-XC01-RSI-HKNKBN
           END-IF
      *    労災のみ
           IF    ( FLG-HKN-OK          =   ZERO  )
            AND  ( WRK-RSI-CNT         >   ZERO  )
            AND  ( WRK-JIB-CNT         =   ZERO  )
            AND  ( WRK-KOG-CNT         =   ZERO  )
               MOVE    "1"                 TO  SPA-XC01-HKNKBN
               MOVE    FLG-RSIHKN-OK       TO  SPA-XC01-RSI-HKNKBN
           END-IF
      *    自賠責のみ
           IF    ( FLG-HKN-OK          =   ZERO  )
            AND  ( WRK-RSI-CNT         =   ZERO  )
            AND  ( WRK-JIB-CNT         >   ZERO  )
            AND  ( WRK-KOG-CNT         =   ZERO  )
               MOVE    "2"                 TO  SPA-XC01-HKNKBN
               MOVE    FLG-RSIHKN-OK       TO  SPA-XC01-RSI-HKNKBN
           END-IF
      *    公害のみ
           IF    ( FLG-HKN-OK          =   ZERO  )
            AND  ( WRK-RSI-CNT         =   ZERO  )
            AND  ( WRK-JIB-CNT         =   ZERO  )
            AND  ( WRK-KOG-CNT         >   ZERO  )
               MOVE    "3"             TO  SPA-XC01-HKNKBN
               MOVE    SPACE           TO  SPA-XC01-RSI-HKNKBN
           END-IF
      *
           IF      SPA-XC01-HKNKBN     =   SPACE
               IF     (WRK-RSI-CNT         >   ZERO )
                  OR  (WRK-JIB-CNT         >   ZERO )
                  OR  (WRK-KOG-CNT         >   ZERO )
                   MOVE    "1"             TO  SPA-XC01-CHK
               END-IF
           END-IF
      *
           IF      FLG-DAISAN          =   1
               MOVE    "1"             TO  SPA-XC01-DAISANFLG
           ELSE
               MOVE    SPACE           TO  SPA-XC01-DAISANFLG
           END-IF
           .
       40102-ALL-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4130-PREVIEW-SEC          SECTION.
      *
           PERFORM 41301-PREVIEW-KICK-SEC
      *
           MOVE    SPA-GMN-SRYKA-I-41     TO  SPA-I40-SRYKA
           MOVE    SPA-GMN-SRYKAMEI-I-41  TO  SPA-I40-SRYKAMEI
           MOVE    SPA-I4KYOUTU           TO  SPA-WORK
           MOVE    SPA-AREA               TO  SPA-COMMON
      *
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I41             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "I41"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    SPA-GMN-PTID        TO  LNK-XC01-TBL-GROUP
           MOVE    0                   TO  LNK-XC01-SHORI-RENNUM
           MOVE    1                   TO  LNK-XC01-RENNUM
           MOVE    1                   TO  LNK-XC01-ST-PAGE
           MOVE    99999               TO  LNK-XC01-ED-PAGE
           MOVE    "1"                 TO  LNK-XC01-MODE
           MOVE    SPA-XC01-HKNKBN     TO  LNK-XC01-HKNKBN
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "XC01   "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4130-PREVIEW-EXT.
           EXIT.
      *****************************************************************
      *    プレビューバッチ起動処理
      *****************************************************************
       41301-PREVIEW-KICK-SEC    SECTION.
      *
      *    自賠責
           IF     SPA-XC01-RSI-HKNKBN  =   "4"
               MOVE    "2"                 TO  SPA-XC01-HKNKBN
           END-IF
      *H25.10
      *    第三者行為
           IF     (SPA-XC01-HKNKBN     =   "0"  )  AND
                  (SPA-XC01-RSI-HKNKBN =   SPACE)
               IF      (SPA-I4ID-FLG        =   "OK" )
      *            第三者行為
                   MOVE    "4"                 TO  SPA-XC01-HKNKBN
                   MOVE    "6"                 TO  SPA-XC01-RSI-HKNKBN
               END-IF
           END-IF
      *
      *    レセプトプレビュー区分
           PERFORM 413011-RESPRVFLG-HEN-SEC
      *
           MOVE    SPACE               TO  SPA-XC01-CHK
      *ver.4.7
      *    業務区分追加
           MOVE    "2"                 TO  WRK-GYOUMUKBN
           CALL    "ORCSRECEARGMK"     USING
                                       "0"
                                       SPA-GMN-PTID
                                       SPA-NAI-SRYYM
                                       SPA-NYUGAIKBN
                                       SPA-XC01-HKNKBN
                                       SPA-XC01-RSI-HKNKBN
                                       SPA-XC01-RESPRVFLG
                                       WRK-GYOUMUKBN
                                       SPA-AREA
      *
           .
       41301-PREVIEW-KICK-EXT.
           EXIT.
      *****************************************************************
      *    レセプトプレビュー区分編集処理
      *****************************************************************
       413011-RESPRVFLG-HEN-SEC                  SECTION.
      *
      *    レセプトプレビュー
           MOVE    "1"                 TO  SPA-XC01-RESPRVFLG
      *    会計照会情報より編集とする
           MOVE    SPACE               TO  SYS-1043-REC
           MOVE    "1043"              TO  SYS-1043-KANRICD
           MOVE    "*"                 TO  SYS-1043-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1043-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1043-STYUKYMD(7:2)
           MOVE    SYS-1043-STYUKYMD   TO  SYS-1043-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1043-HOSPNUM
           MOVE    SYS-1043-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1043-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1043-REC
      *        レセプトプレビュー
               IF      SYS-1043-RESPRVFLG  =   "1"  OR  "2"
                   MOVE    SYS-1043-RESPRVFLG  TO  SPA-XC01-RESPRVFLG
               ELSE
                   MOVE    "1"                 TO  SPA-XC01-RESPRVFLG
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       413011-RESPRVFLG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー保険選択画面へ
      *****************************************************************
       40104-I411-SYORI-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-I41             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGI411"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-I41
           MOVE    SPA-WORK            TO  SPA-I4KYOUTU
      *
      *    画面カーソルセット処理
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK
      *
           MOVE    "I411"              TO  SPA-SAKIPG
           MOVE    "I41"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I411"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       40104-I411-SYORI-EXT.
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
           MOVE    SPACE               TO  LINKAREA.
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I41    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG.
           MOVE    SPACE               TO  WRK-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "剤番号入力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0002"
                   MOVE    "剤の変更番号がありません"
                                       TO  WRK-ERRMSG
               WHEN    "0003"
                   MOVE    "診療回数は数字で入力して下さい"
                                       TO  WRK-ERRMSG
               WHEN    "0004"
                   MOVE    "受診履歴がない日です。入力できません"
                                       TO  WRK-ERRMSG
               WHEN    "0005"
                   MOVE    "受診履歴番号がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0006"
                   MOVE    "受診履歴の選択できない処理です。"
                                       TO  WRK-ERRMSG
               WHEN    "0007"
                   MOVE
                   "変更後診療日が暦日エラーです"
                                       TO  WRK-ERRMSG
               WHEN    "0008"
                   MOVE
                   "変更後診療日は診療月内での変更のみでできます"
                                       TO  WRK-ERRMSG
               WHEN    "0009"
                   MOVE
                   "剤が修正中です。変更確定かクリアをして下さい。"
                                       TO  WRK-ERRMSG
               WHEN    "0010"
                   MOVE    "入院科履歴更新エラー" 
                                       TO  WRK-ERRMSG
               WHEN    "0011"
                   MOVE    "この患者に入院の履歴はありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0012"
                   MOVE
               "受診日変更処理で受診歴確定時のみ入力できます。"
                                       TO  WRK-ERRMSG
               WHEN    "0013"
                   MOVE    "剤の変更番号を入力して下さい"
                                       TO  WRK-ERRMSG
               WHEN    "0014"
                   MOVE    "パラメタ出力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0015"
                   MOVE    "誕生日前の診療日は入力できません"
                                       TO  WRK-ERRMSG
               WHEN    "0016"
                   MOVE    "ＩＤ取得サブでエラー" 
                                       TO  WRK-ERRMSG
               WHEN    "0017"
                   MOVE    "該当の患者が存在しません。"
                                       TO  WRK-ERRMSG
               WHEN    "0018"
                   MOVE    "診療年月入力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0019"
                   MOVE    "患者番号を入力して下さい。"
                                       TO  WRK-ERRMSG
               WHEN    "0020"
                   MOVE    "対象の入院年月はありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0021"
                   MOVE    "対象の剤は労災の外来管理加算読み替え分です"
                                       TO  WRK-ERRMSG
                   MOVE    "。剤変更はできません。"
                                       TO  WRK-ERRMSG(43:)
               WHEN    "0022"
                   MOVE    "数字とスラッシュと"
                                       TO  WRK-ERRMSG(1:18)
                   MOVE    "ハイフンで入力して下さい。"
                                       TO  WRK-ERRMSG(19:)
               WHEN    "0122"
                   MOVE    "有効桁数が違います。"
                                       TO  WRK-ERRMSG
                   MOVE    "回数、日の桁数で入力して下さい。"
                                       TO  WRK-ERRMSG(21:)
               WHEN    "0123"
                   MOVE    "日が重複しています。"
                                       TO  WRK-ERRMSG
               WHEN    "0124"
                   MOVE    "Aは最後に入力して下さい。"
                                       TO  WRK-ERRMSG
               WHEN    "0125"
                   MOVE    "日が違います。正しい日を入力して下さい。"
                                       TO  WRK-ERRMSG
               WHEN    "0061"
                   MOVE    "回数の指定がありません。"
                                       TO  WRK-ERRMSG
      *H30.8
               WHEN    "0062"
                   MOVE    "日指定の前に回数の指定をして下さい。"
                                       TO  WRK-ERRMSG
      *
               WHEN    "0023"
                   MOVE    "全科指定では指定できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0024"
                   MOVE    "回数の関連誤りのため変更できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0025"
                   MOVE    "全科指定では変更できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0026"
                   MOVE    "入院基本料の設定がないため変更できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0027"
                   MOVE    "前月データはありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0028"
                   MOVE    "次月データはありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0029"
                   MOVE
                       "剤識別区分が入院基本料以外の指定はできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0030"
                   MOVE    "施設基準ではありません。算定できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0031"
                   MOVE    "指定された科のデータはありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0032"
                   MOVE    "入院期間外のため基本料の設定はできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0033"
                   MOVE    "入院基本料が設定されていない日です。内容の"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "変更はできません。"
                                       TO  WRK-ERRMSG(43:18)
               WHEN    "0034"
                   MOVE    "入力可能な区分は０、１のいずれかです。"
                                       TO  WRK-ERRMSG
               WHEN    "0035"
                 IF   SPA-NAI-SRYYM     <=    "201003"
                     MOVE  "外泊で入力可能な区分は０〜４のいずれかです"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "。"
                                       TO  WRK-ERRMSG(43:2)
                 END-IF
                 IF  (SPA-NAI-SRYYM     >=    "201004")   AND
                     (SPA-NAI-SRYYM     <=    "201203")
                     MOVE  "外泊で入力可能な区分は０〜５のいずれかです"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "。"
                                       TO  WRK-ERRMSG(43:2)
                 END-IF
                 IF  (SPA-NAI-SRYYM     >=    "201204")  AND
                     (SPA-NAI-SRYYM     <=    "201209")
                     MOVE  "外泊で入力可能な区分は０〜７のいずれかです"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "。"
                                       TO  WRK-ERRMSG(43:2)
                 END-IF
                 IF  (SPA-NAI-SRYYM     >=    "201210")  AND
                     (SPA-NAI-SRYYM     <=    "201603")
                     MOVE  "外泊で入力可能な区分は０〜９、１３のいずれ"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "かです。"
                                       TO  WRK-ERRMSG(43:8)
                 END-IF
                 IF  (SPA-NAI-SRYYM     >=    "201604")  AND
                     (SPA-NAI-SRYYM     <=    "201803")
                     MOVE  "外泊で入力可能な区分は０〜６、８、９、１３"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "のいずれかです。"
                                       TO  WRK-ERRMSG(43:16)
                 END-IF
                 IF   SPA-NAI-SRYYM     >=    "201804"
                     MOVE  "外泊で入力可能な区分は０〜６、８、９、１３"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "〜１７のいずれかです。"
                                       TO  WRK-ERRMSG(43:22)
                 END-IF
               WHEN    "0036"
                 IF   SPA-NAI-SRYYM     <=    "201003"
                     MOVE  "選定入院料での入力可能な区分は０、３、４の"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "いずれかです。"
                                       TO  WRK-ERRMSG(43:14)
                 END-IF
                 IF  (SPA-NAI-SRYYM     >=    "201004")   AND
                     (SPA-NAI-SRYYM     <=    "201203")
                     MOVE  "選定入院料での入力可能な区分は０、３、４、"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "５のいずれかです。"
                                       TO  WRK-ERRMSG(43:18)
                 END-IF
                 IF  (SPA-NAI-SRYYM     >=    "201204")  AND
                     (SPA-NAI-SRYYM     <=    "201209")
                     MOVE  "選定入院料で入力可能な区分は０、３〜７のい"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE    "ずれかです。"
                                       TO  WRK-ERRMSG(43:12)
                 END-IF
                 IF  (SPA-NAI-SRYYM     >=    "201210")  AND
                     (SPA-NAI-SRYYM     <=    "201603")
                     MOVE  "選定入院料で入力可能な区分は０、３〜９、１"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE    "３のいずれかです。"
                                       TO  WRK-ERRMSG(43:18)
                 END-IF
                 IF  (SPA-NAI-SRYYM     >=    "201604")  AND
                     (SPA-NAI-SRYYM     <=    "201803")
                     MOVE  "選定入院料で入力可能な区分は０、３〜６、８"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "、９、１３のいずれかです。"
                                       TO  WRK-ERRMSG(43:26)
                 END-IF
                 IF   SPA-NAI-SRYYM     >=    "201804"
                     MOVE  "選定入院料で入力可能な区分は０、３〜６、８"
                                       TO  WRK-ERRMSG(1:42)
                     MOVE  "、９、１３〜１７のいずれかです。"
                                       TO  WRK-ERRMSG(43:32)
                 END-IF
               WHEN    "0037"
                   MOVE    "入力された数字は室料差額番号として設定され"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "ていません。"
                                       TO  WRK-ERRMSG(43:12)
               WHEN    "0038"
                   MOVE    "食事での入力可能な区分は０、１、２、３、４"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "のいずれかです。"
                                       TO  WRK-ERRMSG(43:16)
               WHEN    "0039"
                   MOVE    "この日は外泊されている日ですから食事の設定"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "はできません。"
                                       TO  WRK-ERRMSG(43:14)
               WHEN    "0040"
                   MOVE    "入力された数字は保険組合せ番号として設定さ"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "れていません。"
                                       TO  WRK-ERRMSG(43:14)
               WHEN    "0041"
                   MOVE    "入院料加算での入力可能な区分は０、１のいず"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "れかです。"
                                       TO  WRK-ERRMSG(43:10)
               WHEN    "0042"
                   MOVE    "この日は外泊されている日ですから入院料加算"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "の設定はできません。"
                                       TO  WRK-ERRMSG(43:20)
               WHEN    "0043"
                   MOVE    "終了日に９９の指定はできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0044"
                   MOVE    "他の入院基本料で設定されている日です。内容"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "の変更はできません。"
                                       TO  WRK-ERRMSG(43:20)
               WHEN    "0045"
                   MOVE    "診療内容です。選択できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0046"
                   MOVE    "保険組合せはクリアできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0047"
                   MOVE    "入院期間外のため設定はできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0048"
                   MOVE    "この日は外泊されている日ですから"
                                       TO  WRK-ERRMSG
                   MOVE    "療養担当手当の設定はできません。"
                                       TO  WRK-ERRMSG(33:)
               WHEN    "0049"
                 IF   SPA-NAI-SRYYM     <=    "201603"
                   MOVE    "食事での入力可能な区分は０、１、２"
                                       TO  WRK-ERRMSG(1:34)
                   MOVE    "のいずれかです。"
                                       TO  WRK-ERRMSG(35:16)
                 ELSE
                   MOVE    "食事での入力可能な区分は０、１、２、３"
                                       TO  WRK-ERRMSG(1:38)
                   MOVE    "のいずれかです。"
                                       TO  WRK-ERRMSG(39:16)
                 END-IF
               WHEN    "0050"
                   MOVE    "入院時食事療養２の算定中は２（特食"
                                       TO  WRK-ERRMSG(1:34)
                   MOVE    "）は算定できません。"
                                       TO  WRK-ERRMSG(35:20)
               WHEN    "0051"
                   MOVE    "入院カレンダーに重複があります。"
                                       TO  WRK-ERRMSG
               WHEN    "0052"
                   MOVE    "入院行為照会用テーブルの更新に失敗しました"
                                       TO  WRK-ERRMSG
               WHEN    "0053"
                   MOVE    "保険組合せが設定されていない日です。内容の"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "変更はできません。"
                                       TO  WRK-ERRMSG(43:18)
               WHEN    "0054"
                   MOVE    "医療観察での入院算定時は外泊入力不可です。"
                                       TO  WRK-ERRMSG
               WHEN    "0055"
                   MOVE    "特定曜日入退院減算は土曜、日曜のみ入力可能"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "です。"
                                       TO  WRK-ERRMSG(43:6)
               WHEN    "0056"
                   MOVE    "入院時生活療養（２）の算定中は３（流動食）"
                                       TO  WRK-ERRMSG(1:42)
                   MOVE    "は算定できません。"
                                       TO  WRK-ERRMSG(43:18)
      *ver.4.7
               WHEN    "0071"
                   STRING  "プレビューできる入院登録がありません。"
                           "プレビューはできません。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "1005"
                   MOVE    "受診履歴番号が存在しません。"
                                       TO  WRK-ERRMSG
               WHEN    "2000"
                   MOVE    "ＤＢ更新エラー。"
                                       TO  WRK-ERRMSG
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                       TO  WRK-ERRMSG
                   MOVE    SLOCK-MSG   TO  WRK-ERRMSG(21:)
           END-EVALUATE.
      *
           MOVE    SPACE               TO  I4ERR.
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE.
           MOVE    WRK-ERRMSG          TO  I4ERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    "I4ERR"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
               WHEN    "0102"
                   MOVE    "剤が修正されています。ＯＫで修正分は反映"
                                       TO  WRK-JIDMSG1
                   MOVE    "されずに終了します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0103"
                   MOVE    "剤選択されました。現在選択中の内容はクリ"
                                       TO  WRK-JIDMSG1
                   MOVE    "アされます。"
                                       TO  WRK-JIDMSG2
               WHEN    "0104"
                   MOVE    "現在までの修正分を登録後、受診履歴の選択"
                                       TO  WRK-JIDMSG1
                   MOVE    "で当日の剤内容を追加修正します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0106"
                   MOVE    "診療年月が変更されました。現在までの修正"
                                       TO  WRK-JIDMSG1
                   MOVE    "分を登録します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0107"
                   MOVE    "入院科が変更されました。現在までの修正分"
                                       TO  WRK-JIDMSG1
                   MOVE    "を登録します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0108"
                   MOVE    "診療年月が変更されました。現在までの修正"
                                       TO  WRK-JIDMSG1
                   MOVE    "分を登録します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0110"
                   MOVE    "最新でない入院履歴を更新することになりま"
                                       TO  WRK-JIDMSG1
                   MOVE    "す、よろしいですか。"
                                       TO  WRK-JIDMSG2
      *
               WHEN    "0120"
                   STRING  "第三者行為の保険組合せです。"
                           "ＯＫで第三者をＮＯで健保の処理を行います。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-IIDCD
                                       TO  WRK-JIDMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  SPA-I4ID-FLG.
      *
           MOVE    SPACE               TO  I4ID1.
           MOVE    SPA-IIDCD           TO  I4ID1-ID1CODE.
           MOVE    WRK-JIDMSG          TO  I4ID1-ID1MSG.
           MOVE    "B12"               TO  MCP-WIDGET.
      *
           MOVE    "戻る"              TO  I4ID1-B01
           MOVE    "ＯＫ"              TO  I4ID1-B12
           EVALUATE    SPA-IIDCD
               WHEN    "0120"
                   MOVE    "ＮＯ"              TO  I4ID1-B01
           END-EVALUATE
      *
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
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
      **     DISPLAY "== SYUKA-SET-SEC ==="
      **     DISPLAY "NACCT-SRYYM   = " NACCT-SRYYM
      **     DISPLAY "SPA-HOSPNUM    = " SPA-HOSPNUM
      **     DISPLAY "SPA-PREFNUM   = " SPA-PREFNUM
      **     DISPLAY "SPA-PTID      = " SPA-PTID
      **     DISPLAY "SPA-NYUGAIKBN = " SPA-NYUGAIKBN
      **     DISPLAY "SPA-BIRTHDAY  = " SPA-BIRTHDAY
      **     DISPLAY "NACCT-ZAISKBKBN = " NACCT-ZAISKBKBN
      **     DISPLAY "NACCT-SRYCDTOTAL = " NACCT-SRYCDTOTAL
      *
           INITIALIZE                 ORCSSYUACCAREA
           MOVE    "04"           TO  LNK-SYUACC-I-KBN
           MOVE    NACCT-SRYYM    TO  LNK-SYUACC-I-SRYYM
           CALL    "ORCSSYUKAMAIN"    USING
                                      SPA-AREA
                                      ORCSSYUACCAREA
      *
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
      *    システム管理主キー検索
      *****************************************************************
       900-SYSKANRI-KEY-READ-SEC              SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-KEY-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   IF  WRK-SRYYMD(1:6)      <=  "200803"
                     EVALUATE   TNS-SRYCD
                       WHEN   "190121310"
                           MOVE "療養病棟入院基本料２（Ａ）"
                                               TO  TNS-NAME
                       WHEN   "190121410"
                           MOVE "療養病棟入院基本料２（Ｂ）"
                                               TO  TNS-NAME
                       WHEN   "190121510"
                           MOVE "療養病棟入院基本料２（Ｃ）"
                                               TO  TNS-NAME
                       WHEN   "190121610"
                           MOVE "療養病棟入院基本料２（Ｄ）"
                                               TO  TNS-NAME
                       WHEN   "190121710"
                           MOVE "療養病棟入院基本料２（Ｅ）"
                                               TO  TNS-NAME
                       WHEN   "190121810"
                           MOVE "療養病棟入院基本料２（特別入院基本料）"
                                               TO  TNS-NAME
                       WHEN   "190122010"
                           MOVE "療養病床入院基本料２（Ａ）"
                                               TO  TNS-NAME
                       WHEN   "190122110"
                           MOVE "療養病床入院基本料２（Ｂ）"
                                               TO  TNS-NAME
                       WHEN   "190122210"
                           MOVE "療養病床入院基本料２（Ｃ）"
                                               TO  TNS-NAME
                       WHEN   "190122310"
                           MOVE "療養病床入院基本料２（Ｄ）"
                                               TO  TNS-NAME
                       WHEN   "190122410"
                           MOVE "療養病床入院基本料２（Ｅ）"
                                               TO  TNS-NAME
                       WHEN   "190122510"
                           MOVE
                          "療養病床入院基本料２（特別入院基本料）"
                                               TO  TNS-NAME
                       WHEN   "190123710"
                           MOVE "療養病棟入院基本料２（Ａ生活）"
                                               TO  TNS-NAME
                       WHEN   "190123810"
                           MOVE "療養病棟入院基本料２（Ｂ生活）"
                                               TO  TNS-NAME
                       WHEN   "190123910"
                           MOVE "療養病棟入院基本料２（Ｃ生活）"
                                               TO  TNS-NAME
                       WHEN   "190124010"
                           MOVE "療養病棟入院基本料２（Ｄ生活）"
                                               TO  TNS-NAME
                       WHEN   "190124110"
                           MOVE "療養病棟入院基本料２（Ｅ生活）"
                                               TO  TNS-NAME
                       WHEN   "190124210"
                           MOVE "療養病棟入院基本料２（特別生活）"
                                               TO  TNS-NAME
                       WHEN   "190124310"
                           MOVE "療養病床入院基本料２（Ａ生活）"
                                               TO  TNS-NAME
                       WHEN   "190124410"
                           MOVE "療養病床入院基本料２（Ｂ生活）"
                                               TO  TNS-NAME
                       WHEN   "190124510"
                           MOVE "療養病床入院基本料２（Ｃ生活）"
                                               TO  TNS-NAME
                       WHEN   "190124610"
                           MOVE "療養病床入院基本料２（Ｄ生活）"
                                               TO  TNS-NAME
                       WHEN   "190124710"
                           MOVE "療養病床入院基本料２（Ｅ生活）"
                                               TO  TNS-NAME
                       WHEN   "190124810"
                           MOVE
                          "療養病床入院基本料２（特別生活）"
                                               TO  TNS-NAME
                       WHEN   "197000570"
                           MOVE
                          "食堂加算"           TO  TNS-NAME
                     END-EVALUATE
                   ELSE
                     IF   WRK-SRYYMD(1:6)      <=  "201003"
                       EVALUATE   TNS-SRYCD
                         WHEN   "190121310"
                           MOVE "療養病棟入院基本料（Ａ）"
                                               TO  TNS-NAME
                         WHEN   "190121410"
                           MOVE "療養病棟入院基本料（Ｂ）"
                                               TO  TNS-NAME
                         WHEN   "190121510"
                           MOVE "療養病棟入院基本料（Ｃ）"
                                               TO  TNS-NAME
                         WHEN   "190121610"
                           MOVE "療養病棟入院基本料（Ｄ）"
                                               TO  TNS-NAME
                         WHEN   "190121710"
                           MOVE "療養病棟入院基本料（Ｅ）"
                                               TO  TNS-NAME
                         WHEN   "190121810"
                           MOVE "療養病棟入院基本料（特別入院基本料）"
                                               TO  TNS-NAME
                         WHEN   "190122010"
                           MOVE "療養病床入院基本料（Ａ）"
                                               TO  TNS-NAME
                         WHEN   "190122110"
                           MOVE "療養病床入院基本料（Ｂ）"
                                               TO  TNS-NAME
                         WHEN   "190122210"
                           MOVE "療養病床入院基本料（Ｃ）"
                                               TO  TNS-NAME
                         WHEN   "190122310"
                           MOVE "療養病床入院基本料（Ｄ）"
                                               TO  TNS-NAME
                         WHEN   "190122410"
                           MOVE "療養病床入院基本料（Ｅ）"
                                               TO  TNS-NAME
                         WHEN   "190122510"
                           MOVE
                          "療養病床入院基本料（特別入院基本料）"
                                               TO  TNS-NAME
                         WHEN   "190123710"
                           MOVE "療養病棟入院基本料（Ａ生活）"
                                               TO  TNS-NAME
                         WHEN   "190123810"
                           MOVE "療養病棟入院基本料（Ｂ生活）"
                                               TO  TNS-NAME
                         WHEN   "190123910"
                           MOVE "療養病棟入院基本料（Ｃ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124010"
                           MOVE "療養病棟入院基本料（Ｄ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124110"
                           MOVE "療養病棟入院基本料（Ｅ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124210"
                           MOVE "療養病棟入院基本料（特別生活）"
                                               TO  TNS-NAME
                         WHEN   "190124310"
                           MOVE "療養病床入院基本料（Ａ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124410"
                           MOVE "療養病床入院基本料（Ｂ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124510"
                           MOVE "療養病床入院基本料（Ｃ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124610"
                           MOVE "療養病床入院基本料（Ｄ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124710"
                           MOVE "療養病床入院基本料（Ｅ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124810"
                           MOVE
                          "療養病床入院基本料（特別生活）"
                                               TO  TNS-NAME
                         WHEN   "197000570"
                           MOVE
                          "食堂加算"           TO  TNS-NAME
                       END-EVALUATE
                     ELSE
                       IF   WRK-SRYYMD(1:6)      <=  "201803"
                        EVALUATE   TNS-SRYCD
                         WHEN   "190121310"
                           MOVE "療養病棟入院基本料１（Ａ）"
                                               TO  TNS-NAME
                         WHEN   "190121410"
                           MOVE "療養病棟入院基本料１（Ｂ）"
                                               TO  TNS-NAME
                         WHEN   "190121510"
                           MOVE "療養病棟入院基本料１（Ｃ）"
                                               TO  TNS-NAME
                         WHEN   "190121610"
                           MOVE "療養病棟入院基本料１（Ｄ）"
                                               TO  TNS-NAME
                         WHEN   "190121710"
                           MOVE "療養病棟入院基本料１（Ｅ）"
                                               TO  TNS-NAME
                         WHEN   "190131610"
                           MOVE "療養病棟入院基本料１（Ｆ）"
                                               TO  TNS-NAME
                         WHEN   "190131810"
                           MOVE "療養病棟入院基本料１（Ｇ）"
                                               TO  TNS-NAME
                         WHEN   "190132010"
                           MOVE "療養病棟入院基本料１（Ｈ）"
                                               TO  TNS-NAME
                         WHEN   "190132210"
                           MOVE "療養病棟入院基本料１（Ｉ）"
                                               TO  TNS-NAME
                         WHEN   "190132410"
                           MOVE "療養病棟入院基本料２（Ａ）"
                                               TO  TNS-NAME
                         WHEN   "190132610"
                           MOVE "療養病棟入院基本料２（Ｂ）"
                                               TO  TNS-NAME
                         WHEN   "190132810"
                           MOVE "療養病棟入院基本料２（Ｃ）"
                                               TO  TNS-NAME
                         WHEN   "190133010"
                           MOVE "療養病棟入院基本料２（Ｄ）"
                                               TO  TNS-NAME
                         WHEN   "190133210"
                           MOVE "療養病棟入院基本料２（Ｅ）"
                                               TO  TNS-NAME
                         WHEN   "190133410"
                           MOVE "療養病棟入院基本料２（Ｆ）"
                                               TO  TNS-NAME
                         WHEN   "190133610"
                           MOVE "療養病棟入院基本料２（Ｇ）"
                                               TO  TNS-NAME
                         WHEN   "190133810"
                           MOVE "療養病棟入院基本料２（Ｈ）"
                                               TO  TNS-NAME
                         WHEN   "190134010"
                           MOVE "療養病棟入院基本料２（Ｉ）"
                                               TO  TNS-NAME
                         WHEN   "190121810"
                           MOVE "療養病棟入院基本料（特別入院基本料）"
                                               TO  TNS-NAME
                         WHEN   "190122010"
                           MOVE "療養病床入院基本料（Ａ）"
                                               TO  TNS-NAME
                         WHEN   "190122110"
                           MOVE "療養病床入院基本料（Ｂ）"
                                               TO  TNS-NAME
                         WHEN   "190122210"
                           MOVE "療養病床入院基本料（Ｃ）"
                                               TO  TNS-NAME
                         WHEN   "190122310"
                           MOVE "療養病床入院基本料（Ｄ）"
                                               TO  TNS-NAME
                         WHEN   "190122410"
                           MOVE "療養病床入院基本料（Ｅ）"
                                               TO  TNS-NAME
                         WHEN   "190122510"
                           MOVE
                          "療養病床入院基本料（特別入院基本料）"
                                               TO  TNS-NAME
                         WHEN   "190123710"
                           MOVE "療養病棟入院基本料１（Ａ生活）"
                                               TO  TNS-NAME
                         WHEN   "190123810"
                           MOVE "療養病棟入院基本料１（Ｂ生活）"
                                               TO  TNS-NAME
                         WHEN   "190123910"
                           MOVE "療養病棟入院基本料１（Ｃ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124010"
                           MOVE "療養病棟入院基本料１（Ｄ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124110"
                           MOVE "療養病棟入院基本料１（Ｅ生活）"
                                               TO  TNS-NAME
                         WHEN   "190131710"
                           MOVE "療養病棟入院基本料１（Ｆ生活）"
                                               TO  TNS-NAME
                         WHEN   "190131910"
                           MOVE "療養病棟入院基本料１（Ｇ生活）"
                                               TO  TNS-NAME
                         WHEN   "190132110"
                           MOVE "療養病棟入院基本料１（Ｈ生活）"
                                               TO  TNS-NAME
                         WHEN   "190132310"
                           MOVE "療養病棟入院基本料１（Ｉ生活）"
                                               TO  TNS-NAME
                         WHEN   "190132510"
                           MOVE "療養病棟入院基本料２（Ａ生活）"
                                               TO  TNS-NAME
                         WHEN   "190132710"
                           MOVE "療養病棟入院基本料２（Ｂ生活）"
                                               TO  TNS-NAME
                         WHEN   "190132910"
                           MOVE "療養病棟入院基本料２（Ｃ生活）"
                                               TO  TNS-NAME
                         WHEN   "190133110"
                           MOVE "療養病棟入院基本料２（Ｄ生活）"
                                               TO  TNS-NAME
                         WHEN   "190133310"
                           MOVE "療養病棟入院基本料２（Ｅ生活）"
                                               TO  TNS-NAME
                         WHEN   "190133510"
                           MOVE "療養病棟入院基本料２（Ｆ生活）"
                                               TO  TNS-NAME
                         WHEN   "190133710"
                           MOVE "療養病棟入院基本料２（Ｇ生活）"
                                               TO  TNS-NAME
                         WHEN   "190133910"
                           MOVE "療養病棟入院基本料２（Ｈ生活）"
                                               TO  TNS-NAME
                         WHEN   "190134110"
                           MOVE "療養病棟入院基本料２（Ｉ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124210"
                           MOVE "療養病棟入院基本料（特別生活）"
                                               TO  TNS-NAME
                         WHEN   "190124310"
                           MOVE "療養病床入院基本料（Ａ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124410"
                           MOVE "療養病床入院基本料（Ｂ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124510"
                           MOVE "療養病床入院基本料（Ｃ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124610"
                           MOVE "療養病床入院基本料（Ｄ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124710"
                           MOVE "療養病床入院基本料（Ｅ生活）"
                                               TO  TNS-NAME
                         WHEN   "190124810"
                           MOVE
                          "療養病床入院基本料（特別生活）"
                                               TO  TNS-NAME
                         WHEN   "197000570"
                           MOVE
                          "食堂加算"           TO  TNS-NAME
                        END-EVALUATE
                       ELSE
                        EVALUATE   TNS-SRYCD
                         WHEN   "197000570"
                           MOVE
                          "食堂加算"           TO  TNS-NAME
                        END-EVALUATE
                       END-IF
                     END-IF
                   END-IF
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
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
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC.
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF.
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター次読込
      *****************************************************************
       900-NYUINACCT-READ-SEC       SECTION.
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
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター次読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF.
      *
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       930-NYUINACT-RAED-SEC       SECTION.
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
       930-NYUINACT-RAED-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-RAED-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF.
      *
       930-SRYACT-RAED-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       970-PTNYUINRRK-READ-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
       970-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       980-PTNYUINRRK-SELECT-SEC     SECTION.
      *
           MOVE    WWK-PTNYU-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO             TO  FLG-WWK-PTNYUINRRK
           ELSE
               MOVE    1                TO  FLG-WWK-PTNYUINRRK
               INITIALIZE                   WWK-PTNYU-REC
           END-IF
      *
           .
      *
       980-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       980-PTNYUINRRK-READ-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WWK-PTNYU-REC
               MOVE    ZERO            TO  FLG-WWK-PTNYUINRRK
           ELSE
               INITIALIZE                  WWK-PTNYU-REC
               MOVE    1               TO  FLG-WWK-PTNYUINRRK
           END-IF.
      *
       980-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号検索処理
      *****************************************************************
       980-HKNNUM-SEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM      TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN      TO  HKN-PAYKBN
           MOVE    SPA-NAI-SRYYM   TO  HKN-TEKSTYMD(1:6)
           MOVE    "01"            TO  HKN-TEKSTYMD(7:2)
           MOVE    HKN-TEKSTYMD    TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    "tbl_hknnum"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
               ELSE
                   INITIALIZE              HKNNUM-REC
                   MOVE    1           TO  FLG-HKNNUM
               END-IF
           ELSE
                   INITIALIZE              HKNNUM-REC
                   MOVE    1           TO  FLG-HKNNUM
           END-IF.
      *
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       980-HKNNUM-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別設定テーブル読込処理
      *****************************************************************
       900-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCONF-REC
                   MOVE    ZERO        TO  FLG-PTCONF
               ELSE
                   INITIALIZE              PTCONF-REC
                   MOVE    1           TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTCONF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       920-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
      *
           .
      *
       920-DBSELECT-EXT.
           EXIT.
      *
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
       990-DBCLOSE-SEC                SECTION.
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
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC             SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE.
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID.
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA.
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF.
      *
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC            SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE.
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA.
      *
       990-LOCK-OUT-EXT.
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
       900-PUT-WINDOW-EXT.
           EXIT.
      *
