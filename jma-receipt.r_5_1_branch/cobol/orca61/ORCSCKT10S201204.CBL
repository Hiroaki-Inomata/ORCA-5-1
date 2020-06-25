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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSCKT10S201204.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 外来まとめ
      *  コンポーネント名  : 診察料自動発生編集処理
      *                      平成２４年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/27    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.05.00     NACL-多々納  10/03/XX  Ｈ２２年４月改正対応
      * 04.07.00     NACL-多々納  12/03/XX  Ｈ２４年４月改正対応
      * 04.08.00     NACL-多々納  13/03/XX  Ｈ２５年４月改正対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA            PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-KBN             PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *
           03  FLG-ARI2            PIC 9(01).
           03  FLG-ARI3            PIC 9(01).
      *
           03  FLG-SYOSIN          PIC 9(01).
           03  FLG-SYOSIN2         PIC 9(01).
      *
           03  FLG-ROUMANSEI       PIC 9(01).
      *    小児科外来診療科算定
           03  FLG-SYONIKA         PIC 9(01).
           03  FLG-SYONIKA-ARI     PIC 9(01).
           03  FLG-SYONIKA-TEI     PIC 9(01).
      *    寝たきり老人算定
           03  FLG-TOUG-SANTEI         PIC 9(03).
           03  FLG-JYURR-ARI           PIC 9(01).
      *
      *    特定疾患自動算定チェック用
           03  FLG-KYUJITU         PIC 9(01).
      *    指導チェック用
           03  FLG-SIDOU           PIC 9(01).
      *    診察料
           03  FLG-SINSATU         PIC 9(01).
           03  FLG-SINSATU2        PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
      *    診察料
           03  FLG-SAISIN-CHK      PIC 9(01).
      *    １５歳未満再診料あり
           03  FLG-15MIMAN         PIC 9(01).
      *    ワーク診療行為診察料あり
           03  FLG-SINSATU-ARI     PIC 9(01).
      *    ワーク診療行為診察料あり
           03  FLG-SINSATU-OK      PIC 9(01).
      *    時間外フラグ
           03  FLG-JIKAN           PIC 9(01).
      *    訂正時外来管理加算あり
           03  FLG-GAI             PIC 9(01).
      *    同日再診
           03  FLG-DOUJITU         PIC 9(01).
      *    同日再診有無
           03  FLG-DOUJITU-ARI     PIC 9(01).
      *    ＤＵＭＭＹ診察料
           03  FLG-DUMMY           PIC 9(01).
      *    電話再診あり
           03  FLG-TEL-ARI         PIC 9(01).
      *
           03  FLG-CHK-ALL         PIC 9(01).
      *
           03  FLG-JIKANKBN        PIC 9(01).
      *
           03  FLG-SYONIKA-CHK     PIC 9(01).
      *
           03  FLG-TAIIN           PIC 9(01).
           03  FLG-FUKU            PIC 9(01).
      *
           03  FLG-CHK3             PIC 9(01).
           03  FLG-HKN-ARI          PIC 9(01).
           03  FLG-KA-ARI           PIC 9(01).
           03  FLG-SRYKBN           PIC 9(01).
           03  FLG-TOKUTEI          PIC 9(02).
      *
           03  FLG-FUKU-ARI         PIC 9(01).
      *
           03  FLG-SANTEI-OK        PIC 9(01).
      *
           03  FLG-SYOSIN-ARI       PIC 9(01).
           03  FLG-DOUSYOSIN        PIC 9(01).
      *
           03  FLG-HENKAN           PIC 9(01).
           03  FLG-DOUJI-HEN        PIC 9(01).
           03  FLG-TAKA             PIC 9(01).
           03  FLG-SYOHEN           PIC 9(01).
      *
           03  FLG-SANTEI-KBN       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDX-SIN             PIC 9(04).
           03  IDX-GAI             PIC 9(04).
           03  IDX-WK              PIC 9(04).
      *
           03  IDX-T               PIC 9(04).
      *
           03  IDX-A               PIC 9(04).
           03  IDX-KA              PIC 9(04).
           03  IDX-HKN             PIC 9(04).
           03  IDX-A-HZN           PIC 9(04).
      *
           03  IDX-INT-MAX          PIC 9(04).
      *
           03  IDX-STR              PIC 9(02).
           03  IDX-R                PIC 9(02).
      *
           03  IDX-N2                  PIC 9(02).
           03  IDX-SN                  PIC 9(04).
           03  IDX-SN2                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SRYCD               PIC X(09).
      *
           03  WRK-IDX-IN              PIC 9(04).
           03  WRK-IDX-FT              PIC 9(04).
      *    算定回数
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    年齢判定用
           03  WRK-BIRTHDAY.
               05  WRK-BIRTH-YY    PIC 9(04).
               05  WRK-BIRTH-MM    PIC 9(02).
               05  WRK-BIRTH-DD    PIC 9(02).
           03  WRK-NYUYOJI         PIC 9(04).
           03  WRK-DAYCNT          PIC 9(04).
           03  WRK-DAYCNT-SUM      PIC 9(04).
      *    訂正日の回数
           03  WRK-TEI-KAISU       PIC 9(04).
      *
           03  WRK-SYOYMD          PIC X(08).
           03  WRK-SYOYMD-K        PIC X(08).
           03  WRK-KEIYMD          PIC X(08).
           03  WRK-KEIYMDZEN       PIC X(08).
      *
      *    算定年月日
           03  WRK-SANTEI-YMD          PIC X(08).
      *    初診年月日
           03  WRK-SYOSIN-YMD          PIC X(08).
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-KAISU               PIC 9(01).
           03  WRK-HOSPSBT             PIC 9(01).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-KBN                 PIC X(01).
           03  WRK-KBN2                PIC X(01).
           03  WRK-KBN3                PIC X(01).
      *
           03  WRK-IN-KAISU            PIC 9(03).
      *    算定年月
           03  WRK-SANTEI-YM           PIC X(06).
           03  WRK-SANTEI-ARI          PIC X(06).
           03  WRK-SANTEI-NASI         PIC X(06).
      *
      *    老人慢性疾病診察料回数
           03  WRK-ROUMANSEI           PIC 9(04).
           03  WRK-ROUMANSEI2          PIC 9(04).
           03  WRK-ROUMANSEI-CNT       PIC 9(04).
      **** 03  WRK-GAIRAI-IDX          PIC 9(04).
           03  WRK-SEIKATU-CNT         PIC 9(04).
      *
      *    寝たきり回数
           03  WRK-NETAKIRI-CNT        PIC 9(04).
      *
      *    在宅自己注射
           03  WRK-ZAITAKU             PIC 9(01).
           03  WRK-ZAITAKU-IDX         PIC 9(04).
      *
      *    発生年月日
           03  WRK-HASSYOYMD.
               05  WRK-HAS-YY          PIC 9(04).
               05  WRK-HAS-MM          PIC 9(02).
               05  WRK-HAS-DD          PIC 9(02).
      *
      *    初回算定日
           03  WRK-FSANTEIYMD          PIC X(08).
      *
      *    パス名
           03  WRK-PATH                PIC X(64).
      *
      *    受診回数（外総診  丸め用）
           03  WRK-SINSATU-ROU         PIC 9(04).
      *    受診回数
           03  WRK-SINSATU-CNT         PIC 9(04).
      *
      *    消炎処置回数
           03  WRK-SYOEN-CNT           PIC 9(03).
      *
      *    回数
           03  WRK-SANTEICNT            PIC 9(03).
      *
      *    入力
           03  WRK-INPUTCD             PIC X(22).
      *    当日受診回数
           03  WRK-DAY-SINSATUCNT      PIC 9(04).
      *
      *    小児科外来診察料チェック
           03  WRK-SYONIKA-KBN        PIC X(01).
      *
      *    発生日編集
           03  WRK-HENSYU-AREA.
               05  WRK-KYUSEI-YMD      PIC X(09).
               05  WRK-KYUSEI-90       PIC X(09).
               05  WRK-KYUSEI-180      PIC X(09).
               05  WRK-HENSYU          PIC X(80).
      *
      *    複数科
           03  WRK-SEL-HKNNUM          PIC X(03).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-DRCD.
               05  WRK-DRCD1               PIC X(01).
               05  WRK-DRCD2               PIC X(04).
           03  WRK-DRCD-MEI                PIC X(80).
           03  WRK-HEN-SRYKA               PIC X(02).
      *    在宅療養指導料算定
           03  WRK-ZAITAKU-CNT         PIC 9(04).
      *    リハビリ医学管理料
           03  WRK-RIHA-KBN            PIC X(01).
           03  WRK-RIHA-SYUKBN         PIC X(01).
      *
           03  WRK-RIHA-KAN-G.
               05  WRK-RIHA-KAN        OCCURS   4.
                   07  WRK-RIHA-DD     PIC 9(02).
                   07  WRK-RIHA-STDD   PIC 9(02).
                   07  WRK-RIHA-EDDD   PIC 9(02).
           03  WRK-RDD                 PIC 9(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-SYODD               PIC 9(02).
      *    紹介対応
           03  WRK-KBN4                PIC X(01).
      *
      *    診察料テーブル
       01  TBL-SINSATU-AREA.
      *    初診料
           03  FILLER              PIC X(14)   VALUE   "100F 111000110".
      *    同日初診
           03  FILLER              PIC X(14)   VALUE   "100FD111011810".
      *
      *    再診料
           03  FILLER              PIC X(14)   VALUE   "100  112007410".
      *    電話再診料
           03  FILLER              PIC X(14)   VALUE   "100T 112007950".
      *    同日再診
           03  FILLER              PIC X(14)   VALUE   "100D 112008350".
      *    電話同日再診
           03  FILLER              PIC X(14)   VALUE   "100S 112008850".
      *
      *    再診料（一般 診療所）
      *    03  FILLER              PIC X(14)   VALUE   "102  112009210".
      *    電話再診（一般 診療所）
      *    03  FILLER              PIC X(14)   VALUE   "102T 112009750".
      *    同日再診（一般 診療所）
      *    03  FILLER              PIC X(14)   VALUE   "102D 112010150".
      *    電話同日再診（一般 診療所）
      ***  03  FILLER              PIC X(14)   VALUE   "102S 112010650".
      *
      *    小児科外来診察料(院外処方 初診)
           03  FILLER              PIC X(14)   VALUE   "200FG113003510".
      *    小児科外来診察料(院内処方 初診)
           03  FILLER              PIC X(14)   VALUE   "200FN113003710".
      *    小児科外来診察料(院外処方 再診)
           03  FILLER              PIC X(14)   VALUE   "200 G113003610".
      *    小児科外来診察料(院内処方 再診)
           03  FILLER              PIC X(14)   VALUE   "200 N113003810".
      *
      *
      *    外来診療料
           03  FILLER              PIC X(14)   VALUE   "301  112011310".
      *    同日外来診療料
           03  FILLER              PIC X(14)   VALUE   "301D 112011710".
      *
      *H24.4
      *    再診料（同一日２科目）
           03  FILLER              PIC X(14)   VALUE   "100KD112015810".
      *    電話再診（同一日２科目）
           03  FILLER              PIC X(14)   VALUE   "100JD112015950".
      *    外来診療料（同一日２科目）
           03  FILLER              PIC X(14)   VALUE   "300KD112016210".
      *H24.4
      *
      *H25.4 から
      *    初診料（紹介状なし）
           03  FILLER              PIC X(14)   VALUE   "110F 111012510".
      *    同日初診（紹介状なし）
           03  FILLER              PIC X(14)   VALUE   "110FD111012610".
      *    外来診療料（他医療機関へ紹介）
           03  FILLER              PIC X(14)   VALUE   "311  112016310".
      *    外来診療料（同一日２科目）（他医療機関へ紹介）
           03  FILLER              PIC X(14)   VALUE   "310KD112016410".
      *    同日外来診療料（他医療機関へ紹介）
           03  FILLER              PIC X(14)   VALUE   "311D 112016550".
      *
       01  TBL-SINSATU-AREAR       REDEFINES   TBL-SINSATU-AREA.
      **** 03  TBL-SINSATU-OCC         OCCURS  15    INDEXED  TBL-IDX.
           03  TBL-SINSATU-OCC         OCCURS  20    INDEXED  TBL-IDX.
      *        １：一般、２：小児科外来診察料、３：外来診療料
               05  TBL-KAISU           PIC 9(01).
      *******  老人一般区分（０：一般、１：老人）(０：なし）
      *H25.4　から（１:紹介なし)
               05  TBL-ROUJIN          PIC 9(01).
      *        病院種別（１：病院、２：診療所）
               05  TBL-HOSPSBT         PIC 9(01).
      *        D:同日再診、T：電話再診、S:同日電話再診、F:初診
               05  TBL-KBN             PIC X(01).
      *        G:院外、N:院内（小児科外来診察料）
      *        D:同日初診
               05  TBL-KBN2            PIC X(01).
      *        診療コード
               05  TBL-SRYCD           PIC X(09).
      *
       01  TBL-SIN-MAX                 PIC 9(04)   VALUE   12.
      *
      *    外来管理加算テーブル
       01  TBL-GAIRAI-AREA.
           03  TBLG-SRYCD          PIC X(09)   VALUE   "112011010".
      *
      *    地域医療貢献加算
      *****03  TBL-CHISRYCD        PIC X(09)   VALUE   "112015670".
      *    明細書発行体制等加算
           03  TBL-MEISRYCD        PIC X(09)   VALUE   "112015770".
      *H24.4
      *    時間外対応加算２（地域医療貢献加算２）
           03  TBL-CHISRYCD2       PIC X(09)   VALUE   "112015670".
      *    時間外対応加算１
           03  TBL-CHISRYCD1       PIC X(09)   VALUE   "112016070".
      *    時間外対応加算３
           03  TBL-CHISRYCD3       PIC X(09)   VALUE   "112016170".
      *
      *    労災診察料テーブル
       01  TBL-RSI-SINSATU-AREA.
      *    初診料
           03  FILLER              PIC X(13)   VALUE   "0 F 101110010".
      *    同日初診
           03  FILLER              PIC X(13)   VALUE   "0 FD101110040".
      *    再診料
           03  FILLER              PIC X(13)   VALUE   "0 S 101120040".
      *    外来管理加算
           03  FILLER              PIC X(13)   VALUE   "0 G 101120070".
      *
       01  TBL-RSI-SINSATU-AREAR   REDEFINES   TBL-RSI-SINSATU-AREA.
           03  TBL-RSI-SINSATU-OCC     OCCURS  4    INDEXED  TBL-RSI.
      *        回数
               05  TBLR-KAISU           PIC 9(01).
               05  FILLER               PIC X(01).
      *        F:初診、S:再診、G:外来管理加算
               05  TBLR-KBN             PIC X(01).
      *        D:同日初診
               05  TBLR-KBN2            PIC X(01).
      *        診療コード
               05  TBLR-SRYCD           PIC X(09).
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *    小児科外来診察料テーブル
           COPY   "CMSYONIKATBL.INC".
      *
      *    画面ＳＰＡ領域ワーク
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC2.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK2-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
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
      *    COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
      *    COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
      *    COPY    "CPORCSPTID.INC".
      *
      *    負担金額計算用パラメタ
      *    COPY    "CPORCS01.INC".
      *
      *    診療行為算定用共通パラメタ
      *    COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
      *    COPY    "CPORCSC91.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
      *    COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
      *    COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
      *    COPY    "CPORCSC96.INC".
      *
      *    エラーメッセージ編集パラメタ
      *    COPY    "CPORCSC97.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    パラメタ
           COPY    "CPORCSCKT10S.INC".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    画面スパ領域
           COPY    "KT01SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCKT10SAREA
           SPA-AREA
           SPA-SCR-REC
           SPA-KT01
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           EVALUATE    ORCSCKT10S-SYORI
      *        初診
               WHEN    "1"
                   PERFORM 100-SYOSIN-AUTO-SEC
      *        再診
               WHEN    "2"
                   PERFORM 200-SAISIN-AUTO-SEC
      *        院内院外変更
               WHEN    "4"
                   PERFORM 400-INGAI-HEN-SEC
      *        算定済みの特定算定回数チェック
               WHEN    "5"
                   PERFORM 500-SANTEI-CHKTUKI-SEC
      *        剤分離前の特定算定回数チェック
               WHEN    "6"
                   PERFORM 600-SANTEI-CNT-SEC
      *        剤分離後の特定算定チェック
               WHEN    "7"
                   PERFORM 700-SANTEI-CHK-SEC
           END-EVALUATE
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初診料を算定中かの判定処理
      *****************************************************************
       100-SYOSIN-AUTO-SEC                  SECTION.
      *
      *    最終行に追加
           MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
      *
           MOVE    ZERO                TO  FLG-SYONIKA-ARI
      *    小児科外来診療科の判定
           IF     (SPA-SYONIKA         =   1   ) 
      *        包括まとめ入力対象外
           AND    (SPA-HKNCOMBINUM NOT =   "9999" )
               PERFORM 1001-SYONIKA-CHK-SEC
           END-IF
      *
           IF     (FLG-SYONIKA-ARI     =   1   )
      *      施設入居者以外
             AND  (SPA-PTTOKK-KBN      =   SPACE )
      *        小児科外来診療科
               MOVE    1                   TO  FLG-SINSATU
               PERFORM 1003-SYONIKA-SET-SEC
           ELSE
      *        健保の初診料算定
               PERFORM 1002-KNH-SYOSIN-SET-SEC
           END-IF
           .
       100-SYOSIN-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診療科回数処理
      *****************************************************************
       1001-SYONIKA-CHK-SEC                  SECTION.
      *
      *    ３歳になる年月日を計算
           MOVE    SPA-BIRTHDAY        TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  3
      *    ３歳未満に適用
           IF      WRK-BIRTHDAY(1:6)   <   SPA-SRYYMD (1:6)
               GO      TO      1001-SYONIKA-CHK-EXT
           ELSE
      *        ４歳まではすべて小児科対象
               MOVE    1                   TO  FLG-SYONIKA-ARI
           END-IF
      *
      *    ３歳の当月時、当月算定がなければ対象外
      *    IF     (SPA-NENREI-YY   =   3               )  AND
      *           (WRK-BIRTHDAY(1:6)   =   SPA-SRYYMD (1:6))
      *        IF      WRK-NYUYOJI         =   ZERO
      *            GO      TO      1001-SYONIKA-CHK-EXT
      *        END-IF
      *    END-IF
      *
      *    当日算定があればフラグを立てる
      *    IF      WRK-DAYCNT-SUM      >   ZERO
      *        MOVE    1                   TO  SPA-SYONIKA-ARI2
      *    ELSE
      *        MOVE    ZERO                TO  SPA-SYONIKA-ARI2
      ***  END-IF
      *    小児科対象
      *    MOVE    1                   TO  FLG-SYONIKA-ARI
      **** MOVE    1                   TO  SPA-SYONIKA-ARI3
      *
           .
       1001-SYONIKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診療科の判定処理
      *****************************************************************
       1003-SYONIKA-SET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYONIKA
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *    院外処方
           IF      SPA-INGAIKBN        =   "1"
               IF      FLG-SINSATU         =   1
      *            初診
                   MOVE    "113003510"         TO  WRK-SRYCD
               ELSE
      *            再診
                   MOVE    "113003610"         TO  WRK-SRYCD
               END-IF
           ELSE
      *        院内処方
               IF      FLG-SINSATU          =   1
      *            初診
                   MOVE    "113003710"         TO  WRK-SRYCD
               ELSE
      *            再診
                   MOVE    "113003810"         TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *    小児科外来診療科算定
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      WRK-IDX-IN          =   ZERO
               ADD     1                   TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IDX-IN          TO  SPA-GMN-IDX
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           ADD     1                   TO  SPA-GMN-IDX
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "KT01"              TO  LNK-SC92-PG
      *
           IF      WRK-IDX-IN          =   ZERO
      *        初期算定
               MOVE    "1"                 TO  LNK-SC92-KBN
               MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
               MOVE    ZERO                TO  FLG-JIKAN
           ELSE
      *        院外院内変更
               MOVE    SPACE               TO  LNK-SC92-KBN
               MOVE    WRK-IDX-IN          TO  LNK-SC92-GMN-IDX
               MOVE    SPA-COM-TIMEFLG(WRK-IDX-IN)
                                           TO  FLG-JIKAN
               INITIALIZE                      SPA-COM-TBLREC
                                                        (WRK-IDX-IN)
               MOVE    WRK-SRYCD           TO  SPA-COM-INPUTCD
                                                        (WRK-IDX-IN)
               MOVE    FLG-JIKAN           TO  SPA-COM-TIMEFLG
                                                        (WRK-IDX-IN)
           END-IF
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           PERFORM 5001-KAISU-HEN-SEC
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *    年齢エラーの時、エラーをはずす（当月はＯＫ）
           IF      LNK-SC92-ERRCD5 NOT =   SPACE
      *        当月算定済み
      *        IF     (SPA-SYONIKA-ARI3    =   ZERO )
      *            CONTINUE
      *        ELSE
                   MOVE    SPACE               TO  LNK-SC92-ERRCD5
      *        END-IF
           END-IF
      *
      *    同日再診時、併用算定エラー
      *    IF      LNK-SC92-ERRCD3 NOT =   SPACE
      *        MOVE    SPACE               TO  SPA-ERRCD
      *        MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
      *        MOVE    ZERO                TO  SPA-GMN-IDX
      *        MOVE    1                   TO  FLG-SYONIKA
      *        MOVE    2                   TO  SPA-SYONIKA-ARI
      *        MOVE    1                   TO  SPA-SYONIKA-ARI2
      *        GO      TO      3303-SYONIKA-SET-EXT
      *    END-IF
      *
      *    エラー時、追加しない
      *    IF     (SPA-ERRCD           NOT =   SPACE )  OR
      *           (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        MOVE    SPACE           TO  SPA-ERRCD
      *        MOVE    WRK-SCR-REC     TO  SPA-SCR-REC
      *        MOVE    ZERO            TO  SPA-GMN-IDX
      *        GO      TO      3303-SYONIKA-SET-EXT
      *    END-IF
      *
      *    再編集処理
           MOVE    SPA-GMN-IDX         TO  IDX
           PERFORM 3101-INPUTCD-HEN-SEC
      *
           MOVE    1                   TO  FLG-SYONIKA
           MOVE    1                   TO  SPA-SYONIKA-ARI
      *
           IF      SPA-GMN-IDX         >   ZERO
      *        療養担当手当（北海道）
               PERFORM 330123-RYOYOU-TEATE-SEC
           END-IF
           .
       1003-SYONIKA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数編集処理
      *****************************************************************
       5001-KAISU-HEN-SEC             SECTION.
      *
           INITIALIZE                  LNK-SC92-KT01-AREA
           MOVE    "1"                 TO  LNK-SC92-KT01FLG
      *
           MOVE    ZERO                TO  IDX-SN2
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100 )   OR
                              (SPA-SAN-SRYCD  (IDX-SN) =   SPACE)
               IF     (SPA-SAN-DAY-G (IDX-SN)  =   ZERO )
               AND    (SPA-SANE-DAY-G (IDX-SN) =   ZERO )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-SN2
                   MOVE    SPA-SAN-SRYCD (IDX-SN)
                                           TO  LNK-SC92-KT01-SRYCD
                                                              (IDX-SN2)
                   PERFORM VARYING     IDX-N2  FROM    1   BY  1
                           UNTIL       IDX-N2  >   31
                       ADD     SPA-SAN-DAY (IDX-SN IDX-N2)
                                               TO  LNK-SC92-KT01-DAY
                                                       (IDX-SN2 IDX-N2)
                       ADD     SPA-SANE-DAY (IDX-SN IDX-N2)
                                               TO  LNK-SC92-KT01-DAY
                                                       (IDX-SN2 IDX-N2)
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       5001-KAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料自動算定処理
      *****************************************************************
       1002-KNH-SYOSIN-SET-SEC                  SECTION.
      *
           MOVE    "11"                TO  WRK-SRYKBN
           MOVE    "F"                 TO  WRK-KBN
      *    診察コードをテーブルより決定
           PERFORM 2001-SRYCD-TBL-SEC
      *    施設入居者はダミー
           IF      SPA-PTTOKK-KBN  NOT =   SPACE
               MOVE    "099110001"         TO  WRK-SRYCD
           END-IF
      *    診察料追加
           PERFORM 3301-SYOSIN-HEN-SEC
           IF      SPA-PTTOKK-KBN      =   SPACE
      *        初診の自動発生（育児栄養加算等）
               IF      FLG-ADD             =   1
                   PERFORM 3302-IKUJIEIYO-SET-SEC
      *
      *            療養担当手当発生
                   PERFORM 330123-RYOYOU-TEATE-SEC
               END-IF
           END-IF
           .
       1002-KNH-SYOSIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診料自動発生処理
      *****************************************************************
       200-SAISIN-AUTO-SEC                  SECTION.
      *
      *    最終行に追加
           MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
      *
           MOVE    ZERO                TO  FLG-SYONIKA-ARI
      *    小児科外来診療科の判定
           IF     (SPA-SYONIKA         =   1   ) 
      *        包括まとめ入力対象外
           AND    (SPA-HKNCOMBINUM NOT =   "9999" )
               PERFORM 1001-SYONIKA-CHK-SEC
           END-IF
      *
           IF     (FLG-SYONIKA-ARI     =   1   )
      *      施設入居者以外
             AND  (SPA-PTTOKK-KBN      =   SPACE )
      *        小児科外来診療科
               MOVE    2                   TO  FLG-SINSATU
               PERFORM 1003-SYONIKA-SET-SEC
           ELSE
      *        健保の再診料算定
               PERFORM 2002-KNH-SAISIN-SET-SEC
           END-IF
           .
       200-SAISIN-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診料自動算定処理
      *****************************************************************
       2002-KNH-SAISIN-SET-SEC                  SECTION.
      *
           MOVE    "12"                TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-KBN
      *    診察コードをテーブルより決定
           PERFORM 2001-SRYCD-TBL-SEC
      *    施設入居者はダミー
           IF      SPA-PTTOKK-KBN  NOT =   SPACE
               MOVE    "099120001"         TO  WRK-SRYCD
           END-IF
      *    診察料追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *
           IF      FLG-ADD             =   ZERO
               GO      TO      2002-KNH-SAISIN-SET-EXT
           END-IF
      *    外来診察料の時、以降なし
           IF     (WRK-KAISU           =   3     )
           OR     (SPA-PTTOKK-KBN  NOT =   SPACE )
               MOVE    1               TO  SPA-GAIRAISIN
           END-IF
      *
      *Ｈ２２年４月から
      *    再診料の加算
           IF     (SPA-GAIRAISIN       =   1 )
               CONTINUE
           ELSE
      *        地域医療貢献・明細書発行体制等加算
               PERFORM 33013-SAISINKSN-SEC
           END-IF
      *
      *    外来管理加算コード追加
           IF     (SPA-GAIKANRIKBN-CHK =   3 )
             OR   (SPA-GAIRAISIN       =   1 )
      *        自動発生なし
               CONTINUE
           ELSE
               MOVE    TBLG-SRYCD          TO  WRK-SRYCD
      *        診察料追加
               PERFORM 3301-SYOSIN-HEN-SEC
           END-IF
           IF      FLG-ADD             =   1
      **       指導料の自動発生
      **       PERFORM 33012-SIDOU-SYORI-SEC
      *        療養担当手当発生
               PERFORM 330123-RYOYOU-TEATE-SEC
           END-IF
      *
           .
       2002-KNH-SAISIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料をテーブルより決定処理
      *****************************************************************
       2001-SRYCD-TBL-SEC              SECTION.
      *
           MOVE    1                   TO  WRK-KAISU
           IF      WRK-SRYKBN          =   "12"
      *        一般病床数
               IF     (SPA-BEDSUIPN        >=  200 )  AND
                      (SPA-HOSPSBT         =   1   )
      *            200症以上
                   MOVE    3               TO  WRK-KAISU
               END-IF
           END-IF
      *
           IF      WRK-SRYKBN          =   "11"
               MOVE    ZERO                TO  WRK-HOSPSBT
           ELSE
               MOVE    SPA-HOSPSBT         TO  WRK-HOSPSBT
           END-IF
      *H25.4
           IF      WRK-KBN4        NOT =   "1"
               MOVE    "0"             TO  WRK-KBN4
           END-IF
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYCD
      ***********  WHEN   (SPA-NAI-ROUJIN      =   TBL-ROUJIN(TBL-IDX))
      ************ WHEN   (WRK-HOSPSBT         =   TBL-HOSPSBT(TBL-IDX))
                   WHEN   (WRK-KAISU           =   TBL-KAISU  (TBL-IDX))
                     AND  (WRK-KBN             =   TBL-KBN    (TBL-IDX))
                     AND  (WRK-SRYKBN          =   TBL-SRYCD(TBL-IDX)
                                                                (2:2))
      *H25.4         紹介なし
                     AND  (WRK-KBN4            =   TBL-ROUJIN(TBL-IDX))
                       MOVE    TBL-SRYCD (TBL-IDX)
                                               TO  WRK-SRYCD
           END-SEARCH
      *
           .
       2001-SRYCD-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料追加処理
      *****************************************************************
       3301-SYOSIN-HEN-SEC                SECTION.
      *
           IF     (WRK-SRYCD(1:2)      =   "83"  )   AND
                  (SPA-GMN-IDX         =   ZERO  )
               ADD     1                   TO  SPA-GMN-IDX
      *        診療区分編集
               IF      WRK-SRYCD           =   "830000020"
                   MOVE    ".110"          TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   MOVE    "110"           TO  SPA-COM-SRYSYUKBN
                                                         (SPA-GMN-IDX)
                   MOVE    "11"            TO  SPA-COM-SRYKBN
                                                         (SPA-GMN-IDX)
               ELSE
                   MOVE    ".120"          TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   MOVE    "120"           TO  SPA-COM-SRYSYUKBN
                                                         (SPA-GMN-IDX)
                   MOVE    "12"            TO  SPA-COM-SRYKBN
                                                         (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           ADD     1                   TO  SPA-GMN-IDX
           IF      WRK-SRYCD(1:2)      =   "83"
               MOVE    SPA-TAKA-SRYKA(1)   TO  SPA-COM-ATAI1
                                                         (SPA-GMN-IDX)
               MOVE    1                   TO  SPA-FUKU-SRYKA
           END-IF
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "KT01"              TO  LNK-SC92-PG
      *    警告のチェックなし
           MOVE    SPACE               TO  LNK-SC92-KEIFLG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           PERFORM 5001-KAISU-HEN-SEC
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    初診料の併用エラー
           IF      WRK-SRYKBN          =   "11"
               IF      LNK-SC92-SYOKBN     =   "1"
      ********     前に併用算定がある時、病名がすべて治癒であること
      *            IF      SPA-BYOMEI-STR      =   ZERO
      *                MOVE    1                   TO  SPA-SYOSIN-KEI
      *            ELSE
      *                MOVE    "0033"              TO  LNK-SC92-ERRCD6
      **********   END-IF
                   MOVE    1                   TO  SPA-SYOSIN-KEI
               END-IF
           END-IF
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                       -   1
               GO      TO      3301-SYOSIN-HEN-EXT
           END-IF
      *    診療区分の設定
           IF      SPA-COM-SRYKBN(SPA-GMN-IDX) =   SPACE
               MOVE    SPA-COM-SRYSYUKBN(SPA-GMN-IDX)
                                           TO  SPA-COM-SRYKBN
                                                         (SPA-GMN-IDX)
           END-IF
      *
           IF      WRK-SRYCD(1:2)      =   "83"
      *        再編集処理
               MOVE    SPA-GMN-IDX         TO  IDX
               PERFORM 3101-INPUTCD-HEN-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ADD
           .
       3301-SYOSIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   育児栄養加算自動発生処理
      *****************************************************************
       3302-IKUJIEIYO-SET-SEC                  SECTION.
      *
      *H22.4 廃止
      *    電子化加算
      **** PERFORM 33021-DENSIKA-ADD-SEC
      *
      *    育児栄養加算自動算定
           IF      SPA-IKUJIEIYO-FLG   NOT =   "1"
               GO      TO      3302-IKUJIEIYO-SET-EXT
           END-IF
      *    ３歳になる年月日を計算
           MOVE    SPA-BIRTHDAY        TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  3
      *    ３歳未満に適用
           IF      WRK-BIRTHDAY(1:6)   <   SPA-SRYYMD (1:6)
               GO      TO      3302-IKUJIEIYO-SET-EXT
           END-IF
      *
      *    育児栄養加算自動算定科
           IF     (SPA-IKUJIEIYO-SRYKA =   ZERO      )  OR
                  (SPA-IKUJIEIYO-SRYKA =   SPA-SRYKA )
               CONTINUE
           ELSE
               GO      TO      3302-IKUJIEIYO-SET-EXT
           END-IF
      *
      *    初診（育児栄養指導）加算
           MOVE    "111000470"         TO  WRK-SRYCD
      *    追加共通処理
           MOVE    ZERO                TO  WRK-IDX-IN
           PERFORM 33041-SRYCD-ADD-SEC
      *
           .
       3302-IKUJIEIYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   電子化加算編集処理
      *****************************************************************
      *33021-DENSIKA-ADD-SEC                  SECTION.
      *
      *    IF      SPA-DENSIKA         =   1
      *        MOVE    "111012370"         TO  WRK-SRYCD
      *        追加共通処理
      *        MOVE    ZERO                TO  WRK-IDX-IN
      *        PERFORM 33041-SRYCD-ADD-SEC
      *    END-IF
      *    .
      *33021-DENSIKA-ADD-EXT.
      *    EXIT.
      *****************************************************************
      *   地域医療貢献・明細書発行体制等加算編集処理
      *****************************************************************
       33013-SAISINKSN-SEC                  SECTION.
      *
      *H24.4
      *    時間外対応加算（地域医療貢献加算）
           EVALUATE    SPA-CHIIKKBN
               WHEN    1
      *            時間外対応加算１
                   MOVE    TBL-CHISRYCD1       TO  WRK-SRYCD
                   MOVE    ZERO                TO  WRK-IDX-IN
                   PERFORM 33041-SRYCD-ADD-SEC
               WHEN    2
      *            時間外対応加算２
                   MOVE    TBL-CHISRYCD2       TO  WRK-SRYCD
                   MOVE    ZERO                TO  WRK-IDX-IN
                   PERFORM 33041-SRYCD-ADD-SEC
               WHEN    3
      *            時間外対応加算３
                   MOVE    TBL-CHISRYCD3       TO  WRK-SRYCD
                   MOVE    ZERO                TO  WRK-IDX-IN
                   PERFORM 33041-SRYCD-ADD-SEC
           END-EVALUATE
      *    明細発行体制等加算
           IF      SPA-MEISAKBN        =   1
               MOVE    TBL-MEISRYCD        TO  WRK-SRYCD
      *        追加共通処理
               MOVE    ZERO                TO  WRK-IDX-IN
               PERFORM 33041-SRYCD-ADD-SEC
           END-IF
           .
       33013-SAISINKSN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       3101-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    "K02"               TO  LNK-SC94-PG
           MOVE    IDX                 TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           IF      SPA-GMN-IDX         >   1
               MOVE    SPA-COM-SRYSYUKBN (SPA-GMN-IDX - 1)
                                           TO  SPA-COM-SRYSYUKBN
                                                         (SPA-GMN-IDX)
               MOVE    SPA-COM-SRYKBN (SPA-GMN-IDX - 1)
                                           TO  SPA-COM-SRYKBN
                                                         (SPA-GMN-IDX)
           END-IF
     *
           .
       3101-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    急性発生日決定処理
      *****************************************************************
       10019-KYUSEI-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-KYUSEI-FLG
           MOVE    SPACE               TO  SPA-KYUSEI-180
           MOVE    SPACE               TO  SPA-KYUSEI-90
           MOVE    SPACE               TO  SPA-KYUSEI-YMD
           MOVE    SPACE               TO  SPA-GMN-KYUSEI
      *    急性発生日
           MOVE    "099800101"         TO  WRK-SRYCD
           PERFORM 100191-KYUSEI-SANTEI-SEC
           MOVE    WRK-YMD             TO  SPA-KYUSEI-YMD
      *
      *    急性発生終了日
           MOVE    "099800102"         TO  WRK-SRYCD
           PERFORM 100191-KYUSEI-SANTEI-SEC
      *    訂正の時当日なら、対象外
           IF      SPA-SYORIFLG        =   1
               IF      WRK-YMD         =   SPA-SRYYMD
                   MOVE    SPACE           TO  WRK-YMD
               END-IF
           END-IF
           IF     (WRK-YMD         NOT =   SPACE ) AND
                  (WRK-YMD             <=  SPA-SRYYMD    )  AND
                  (WRK-YMD             >   SPA-KYUSEI-YMD)
               MOVE    WRK-YMD             TO  SPA-KYUSEI-END
           ELSE
               MOVE    SPACE               TO  SPA-KYUSEI-END
           END-IF
      *
           IF     (SPA-KYUSEI-YMD  NOT =   SPACE )  AND
                 ((SPA-KYUSEI-YMD      >   SPA-KYUSEI-END)  OR
                  (SPA-KYUSEI-END      =   SPA-SRYYMD    ))
               PERFORM 100192-KYUSEI-HENSYU-SEC
           END-IF
      *
      *    急性発生日が傷病日より後の場合、急性発生日から６ヶ月後とする
           IF     (SPA-HKNKBN          =   1        )  AND
                  (SPA-RSI-SHOBYOYMD   NOT =   SPACE ) AND
                  (SPA-RSI-SHOBYOYMD   <   SPA-KYUSEI-YMD )
               MOVE    SPA-KYUSEI-YMD      TO  WRK-HASSYOYMD
               COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                           +   6
               IF      WRK-HAS-MM          >   12
                   COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
                   COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
               END-IF
               MOVE    WRK-HASSYOYMD       TO  SPA-RSI-SHOBYOYMD3
           END-IF
      *
           .
       10019-KYUSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    急性発生日算定日処理
      *****************************************************************
       100192-KYUSEI-HENSYU-SEC                  SECTION.
      *
      *    180日後計算
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-KYUSEI-YMD      TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    180                 TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-KYUSEI-180
      *
      *    90日後
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-KYUSEI-YMD      TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    90                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-KYUSEI-90
      *
      *    画面表示用
           MOVE    SPACE               TO  WRK-HENSYU-AREA
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-KYUSEI-YMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-KYUSEI-YMD
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-KYUSEI-90       TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-KYUSEI-90
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-KYUSEI-180      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-KYUSEI-180
      *
           STRING   "急性発症日："     DELIMITED  BY  SIZE
                   WRK-KYUSEI-YMD      DELIMITED  BY  SIZE
                   "　９０超："        DELIMITED  BY  SIZE
                   WRK-KYUSEI-90       DELIMITED  BY  SIZE
                   "　１８０超："      DELIMITED  BY  SIZE
                   WRK-KYUSEI-180      DELIMITED  BY  SIZE
                   INTO                SPA-GMN-KYUSEI
           END-STRING
      *
           .
       100192-KYUSEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    急性発生日算定日処理
      *****************************************************************
       100191-KYUSEI-SANTEI-SEC                  SECTION.
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *
      *    労災毎とする
           IF      SPA-HKNKBN          =   1
                                       OR  2
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    SPACE               TO  WRK-YMD
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6))
      *            最後の発生日、当日以降は対象外
                   IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y3
                   ELSE
                       MOVE    31                  TO  IDX-Y3
                   END-IF
                   MOVE    ZERO                TO  WRK-DD
                   PERFORM VARYING     IDX-Y2  FROM    IDX-Y3  BY  -1
                           UNTIL       IDX-Y2  <   1
                       IF      SANTEI-DAY (IDX-Y2) >   ZERO
                           MOVE    IDX-Y2          TO  WRK-DD
                           MOVE    1               TO  IDX-Y2
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              >   ZERO
                       MOVE    SANTEI-SRYYM    TO  WRK-YMD(1:6)
                       MOVE    1               TO  FLG-SANTEI
                   END-IF
               END-IF
      *
               IF     FLG-SANTEI           =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-YMD(1:6)        =   SPACE
               MOVE    SPACE           TO  WRK-YMD
           END-IF
      *
           .
       100191-KYUSEI-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加共通処理処理
      *****************************************************************
       33041-SRYCD-ADD-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      WRK-IDX-IN          =   ZERO
               ADD     1                   TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IDX-IN          TO  SPA-GMN-IDX
           END-IF
           MOVE    ZERO                TO  WRK-IN-KAISU
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "KT01"              TO  LNK-SC92-PG
           IF      WRK-IDX-IN          =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
               MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           ELSE
               MOVE    SPACE               TO  LNK-SC92-KBN
               MOVE    WRK-IDX-IN          TO  LNK-SC92-GMN-IDX
      *        回数１以上
               IF      (SPA-COM-KAIFLG (WRK-IDX-IN)    =   "1")  AND
                       (SPA-COM-KAISU  (WRK-IDX-IN)    >   1  )
                   MOVE    SPA-COM-KAISU  (WRK-IDX-IN)
                                               TO  WRK-IN-KAISU
               ELSE
                   MOVE    ZERO                TO  WRK-IN-KAISU
               END-IF
               MOVE    SPA-COM-TIMEFLG(WRK-IDX-IN)
                                           TO  FLG-JIKAN
      *        回数１以上
               IF      (SPA-COM-KAIFLG (WRK-IDX-IN)    =   "1")  AND
                       (SPA-COM-KAISU  (WRK-IDX-IN)    >   1  )
                   MOVE    SPA-COM-KAISU  (WRK-IDX-IN)
                                               TO  WRK-IN-KAISU
               ELSE
                   MOVE    ZERO                TO  WRK-IN-KAISU
               END-IF
      *
               INITIALIZE                      SPA-COM-TBLREC
                                                        (WRK-IDX-IN)
               MOVE    WRK-SRYCD           TO  SPA-COM-INPUTCD
                                                        (WRK-IDX-IN)
               IF      FLG-JIKAN           >   ZERO
                   MOVE    FLG-JIKAN           TO  SPA-GMN-INPUTCD
                                                    (WRK-IDX-IN)(1:1)
                   MOVE    WRK-SRYCD           TO  SPA-GMN-INPUTCD
                                                    (WRK-IDX-IN)(3:)
               ELSE
                   MOVE    WRK-SRYCD           TO  SPA-GMN-INPUTCD
                                                        (WRK-IDX-IN)
               END-IF
               MOVE    FLG-JIKAN           TO  SPA-COM-TIMEFLG
                                                        (WRK-IDX-IN)
           END-IF
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           PERFORM 5001-KAISU-HEN-SEC
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX     -   1
           ELSE
      *        回数の再編集
               IF      WRK-IN-KAISU        >   ZERO
                   MOVE    WRK-IN-KAISU        TO  SPA-COM-KAISU
                                                        (SPA-GMN-IDX)
                   MOVE    "1"                 TO  SPA-COM-KAIFLG
                                                        (SPA-GMN-IDX)
      *            再編集処理
                   MOVE    SPA-GMN-IDX         TO  IDX
                   PERFORM 3101-INPUTCD-HEN-SEC
               END-IF
               MOVE    1                  TO  FLG-ADD
           END-IF
      *
           .
       33041-SRYCD-ADD-EXT.
           EXIT.
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    対象の算定履歴判定処理
      *****************************************************************
       200511-SANTEI-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI-OK
      *
           EVALUATE    TRUE
      *        算定履歴区分＝１
               WHEN   (SANTEI-NYUGAIKBN    =   ZERO )  AND
                      (SANTEI-SRYKA        =   ZERO )
                   MOVE    1                   TO  FLG-SANTEI-OK
      *        算定履歴区分＝２
               WHEN   (SANTEI-NYUGAIKBN    =   SPA-NYUGAIKBN) AND
                      (SANTEI-SRYKA        =   ZERO )
                   MOVE    1                   TO  FLG-SANTEI-OK
      *
      *        算定履歴区分＝３
               WHEN   (SANTEI-NYUGAIKBN    =   ZERO )  AND
                      (SANTEI-SRYKA        =   SPA-SRYKA )
                   MOVE    1                   TO  FLG-SANTEI-OK
      *
      *        算定履歴区分＝４
               WHEN   (SANTEI-NYUGAIKBN    =   SPA-NYUGAIKBN) AND
                      (SANTEI-SRYKA        =   SPA-SRYKA )
                   MOVE    1                   TO  FLG-SANTEI-OK
           END-EVALUATE
      *
           .
       200511-SANTEI-CHK-EXT.
           EXIT.
      *
      *!!!!　以下　使用なし  !!!
      *****************************************************************
      *    指導料の自動発生処理
      *****************************************************************
       33012-SIDOU-SYORI-SEC      SECTION.
      *
      *    特定慢性疾患指導料
           IF      SPA-TOKUTEI     NOT =   ZERO
               PERFORM 330121-TOKUTEI-SIDOU-SEC
           END-IF
      *
      *    皮膚科特定疾患
           IF      SPA-TOKUTEI-F   NOT =   ZERO
               MOVE    SPA-TOKUTEI-F       TO  FLG-TOKUTEI
               PERFORM 330122-HIFUKA-SIDOU-SEC
           END-IF
      *
      *    療養担当手当（北海道）
      **** PERFORM 330123-RYOYOU-TEATE-SEC
      *
           .
       33012-SIDOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料の自動発生処理
      *****************************************************************
       330121-TOKUTEI-SIDOU-SEC      SECTION.
      *
      *    指導料の自動発生
      *!!  EVALUATE    SPA-NAI-ROUJIN  ALSO  SPA-HOSPSBT
      *!!      老人　病院
      *        WHEN    1        ALSO         1
      *            IF      SPA-BEDSU           <   100
      *                MOVE    "113700710"         TO  WRK-SRYCD
      *            ELSE
      *                MOVE    "113700810"         TO  WRK-SRYCD
      *            END-IF
      *        老人、診療所
      *        WHEN    1        ALSO         2
      *!!!         MOVE    "113700610"         TO  WRK-SRYCD
           EVALUATE    SPA-HOSPSBT
      *        一般　病院
               WHEN    1
                   IF      SPA-BEDSU           <   100
                       MOVE    "113001910"         TO  WRK-SRYCD
                   ELSE
                       MOVE    "113002010"         TO  WRK-SRYCD
                   END-IF
      *        一般、診療所
               WHEN    2
                   MOVE    "113001810"         TO  WRK-SRYCD
           END-EVALUATE
      *
      *    自動発生区分判定
           IF      SPA-BYOMEIAUTO-FLG  =  "1"
      *        指導料追加、チェック処理
               MOVE    ZERO                TO  FLG-SIDOU
               PERFORM 3301211-SIDOU-ADD-SEC
           END-IF
           .
       330121-TOKUTEI-SIDOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料編集・チェック処理
      *****************************************************************
       3301211-SIDOU-ADD-SEC           SECTION.
      *
      *    EVALUATE    LNK-SC10S-SYORI
      *        中途終了展開
      *        WHEN    "W"
      *        診察料のみの変更処理
      *        WHEN    "S"
      *        診察料のみの変更処理（他保険受診ありで同日再診へ）
      *        WHEN    "J"
      *        診察料算定後、診察料のみの変更処理
      *        WHEN    "K"
      *        WHEN    "E"
      *            PERFORM 33012111-SIDOU-CHUTO-SEC
      *        WHEN    "A"
      *            複数科入力
      *            PERFORM 33012112-SIDOU-FUKU-SEC
      *        WHEN    "D"
      *            中途終了展開時自動算定なし
      *            CONTINUE
      *        WHEN    OTHER
                   PERFORM 33012113-SIDOU-INS-SEC
      *    END-EVALUATE
      *
           .
       3301211-SIDOU-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料編集・チェック処理
      *****************************************************************
       33012113-SIDOU-INS-SEC               SECTION.
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC2
      *
      *    点数マスタ編集・基本チェック
           COMPUTE SPA-GMN-IDX         =   SPA-GMN-IDX   +   1
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
      ***  MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           PERFORM 5001-KAISU-HEN-SEC
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (LNK-SC92-ERRAREA    =   SPACE )
               PERFORM 330122-JYUFUKU-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (LNK-SC92-ERRAREA    =   SPACE )
      *        指導料初診日チェック（ORCSC10 と同様のチェック）
               PERFORM 330121-SIDOU-CHK-SEC
           END-IF
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    WRK-SCR-REC2        TO  SPA-SCR-REC 
           ELSE
      *        再編集処理
               MOVE    SPA-GMN-IDX         TO  IDX
               PERFORM 3101-INPUTCD-HEN-SEC
           END-IF
      *
           .
       33012113-SIDOU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力時指導料編集・チェック処理
      *****************************************************************
       33012112-SIDOU-FUKU-SEC        SECTION.
      *
      *    既に入力済みかどうかの判断
           PERFORM 330122-JYUFUKU-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *        エラーがない時、自動発生する
               PERFORM 33012113-SIDOU-INS-SEC
           ELSE
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
           .
       33012112-SIDOU-FUKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料初診日チェック処理（ORCSC10 と同様のチェック）
      *****************************************************************
       330121-SIDOU-CHK-SEC               SECTION.
      *
           MOVE    SPA-SYOYMD          TO  WRK-SYOYMD
           MOVE    ZERO                TO  FLG-SYOHEN
      *    初診算定日が診療日以前の時
           IF     (SPA-SYOYMD-OLD  NOT =  SPACE     ) AND
                  (WRK-SYOYMD          >  SPA-SRYYMD)
               MOVE    SPA-SYOYMD-OLD     TO  WRK-SYOYMD
           END-IF
      *    同日初診がある時、同日初診
           IF      WRK-SRYKA           =   SPACE
               MOVE    SPA-SRYKA       TO  WRK-SRYKA
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (SPA-DOU-SRYKA (IDX) =   SPACE)
               IF      SPA-DOU-SRYKA (IDX) =   WRK-SRYKA
                   IF      SPA-DOU-SRYYMD(IDX) >   WRK-SYOYMD
                       MOVE    SPA-DOU-SRYYMD(IDX)
                                               TO  WRK-SYOYMD
                       MOVE    1               TO  FLG-SYOHEN
                   END-IF
                   MOVE    10          TO  IDX
               END-IF
           END-PERFORM
      *
      *    最終退院日がある時、退院日
           IF     (SPA-K02N-TAIINYMD   =   SPACE )  OR
                  (SPA-K02N-TAIINYMD   <=  WRK-SYOYMD)
               MOVE    ZERO                TO  FLG-TAIIN
           ELSE
               MOVE    SPA-K02N-TAIINYMD   TO  WRK-SYOYMD
               MOVE    1                   TO  FLG-TAIIN
           END-IF
      *
           MOVE    ZERO                TO  FLG-KYUJITU
           IF      WRK-SYOYMD          =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMD
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMDZEN
           ELSE
      *        初診料算定日＋１月後
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SYOYMD          TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-KEIYMD
               MOVE    LNK-DAY6-KEISANZEN  TO  WRK-KEIYMDZEN
               IF      LNK-DAY6-KEISAN NOT =   LNK-DAY6-KEISANZEN
                   MOVE    1                   TO  FLG-KYUJITU
               END-IF
           END-IF
      *
           EVALUATE    TRUE
               WHEN    WRK-KEIYMD          <   SPA-SRYYMD
                   CONTINUE
               WHEN    WRK-KEIYMD          >   SPA-SRYYMD
      *            １月後の前営業日と同じ時、ＯＫ
                   IF      FLG-SIDOU       =   1
                       IF      FLG-TAIIN       =   1
                           MOVE    "0120"         TO  SPA-ERRCD
                       ELSE
                           MOVE    "0034"          TO  SPA-ERRCD
                       END-IF
                   ELSE
                       IF      FLG-TAIIN       =   1
                           MOVE    "0120"          TO  SPA-ERRCD
                       ELSE
                            MOVE    "0034"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *       初診算定日からの１ヵ月以内のエラーで同日初診日でない時
                   IF     (SPA-ERRCD           =   "0034")  AND
                          (FLG-SYOHEN          =   ZERO  )  AND
                          (SPA-SYOYMD-RSI  NOT =   SPACE )
      ***********   AND   (SPA-SYOYMD-HKN  NOT =   SPACE )
                       PERFORM 2302-ERRCHK-SEC
                   END-IF
               WHEN    WRK-KEIYMD          =   SPA-SRYYMD
                   CONTINUE
           END-EVALUATE
           .
       330121-SIDOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診算定日の警告チェック処理
      *****************************************************************
       2302-ERRCHK-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOYMD-K
      *    保険が労災の時
           IF     (SPA-HKNKBN          =   1   )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *        規準日が労災以外の初診日であれば、労災の初診日でチェック
               IF     (WRK-SYOYMD          =   SPA-SYOYMD-HKN )
                   MOVE    SPA-SYOYMD-RSI      TO  WRK-SYOYMD-K
               END-IF
           ELSE
               IF     (WRK-SYOYMD          =   SPA-SYOYMD-RSI )
      *        規準日が労災の初診日であれば、労災以外の初診日でチェック
                   MOVE    SPA-SYOYMD-HKN      TO  WRK-SYOYMD-K
      *            労災以外の初診がなければ警告とする
                   IF      SPA-SYOYMD-HKN      =   SPACE
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           IF      WRK-SYOYMD-K    NOT =   SPACE
      *        初診料算定日＋１月後
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SYOYMD-K        TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               IF      LNK-DAY6-KEISAN     <=  SPA-SRYYMD
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-IF
           .
       2302-ERRCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾患自動算定処理
      *****************************************************************
       330122-JYUFUKU-CHK-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE          )  OR
                              (SPA-ERRCD       NOT  =   SPACE )  OR
                             ((SPA-COM-INPUTCD(IDX) =   SPACE ) AND
                              (SPA-GMN-INPUTCD(IDX) =   SPACE ))
               IF     (SPA-GMN-IDX         NOT =   IDX   )  AND
                      (SPA-COM-INPUTCD (IDX)   =   WRK-SRYCD)
                   MOVE    "0134"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
           .
       330122-JYUFUKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    皮膚科特定疾患自動算定処理
      *****************************************************************
       330122-HIFUKA-SIDOU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           EVALUATE    FLG-TOKUTEI
      *           （１）
               WHEN    03
                   MOVE    "113000910"     TO  WRK-SRYCD
      *           （２）
               WHEN    04
                   MOVE    "113002310"     TO  WRK-SRYCD
           END-EVALUATE
      *    皮膚科特定疾患管理料（２）でアトピー性皮膚炎のある場合
           IF     (FLG-TOKUTEI         =   04  )  AND
                  (SPA-ATOPII-FLG      =   1   )
      *        16才未満はなし
               IF      SPA-NENREI-YY       <   16
                   MOVE    SPACE           TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *    自動発生区分判定
           IF     (WRK-SRYCD       NOT =   SPACE )  AND
                  (SPA-BYOMEIAUTO-FLG  =   "1"   )
      *        指導料追加、チェック処理
               MOVE    1                   TO  FLG-SIDOU
               PERFORM 3301211-SIDOU-ADD-SEC
           END-IF
      *
           .
       330122-HIFUKA-SIDOU-EXT.
           EXIT.
      *!!!!!　使用なし　ここまで  !!!
      *
      *****************************************************************
      *    療養担当手当（北海道）処理
      *****************************************************************
       330123-RYOYOU-TEATE-SEC         SECTION.
      *
           IF    ( SPA-RYOYOU-FLG      =   "1"     )  AND
                 ((SPA-SRYYMD(5:4)     >=  "1101"  AND
                                       <=  "1231")  OR
                  (SPA-SRYYMD(5:4)     >=  "0101"  AND
                                       <=  "0430") )
               CONTINUE
           ELSE
               GO      TO      330123-RYOYOU-TEATE-EXT
           END-IF
      *
           MOVE    "199000610"         TO  WRK-SRYCD
      *
      *    点数マスタ編集・基本チェック
           COMPUTE SPA-GMN-IDX         =   SPA-GMN-IDX   +   1
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           PERFORM 5001-KAISU-HEN-SEC
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                       -   1
           ELSE
               MOVE    "1"                 TO  SPA-COM-KAIFLG
                                                      (SPA-GMN-IDX)
      *        再編集処理
               MOVE    SPA-GMN-IDX         TO  IDX
               PERFORM 3101-INPUTCD-HEN-SEC
           END-IF
      *
           .
       330123-RYOYOU-TEATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内院外変更処理
      *****************************************************************
       400-INGAI-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX-IN
           MOVE    ZERO                TO  WRK-ZAITAKU
           MOVE    ZERO                TO  WRK-ZAITAKU-IDX
      *****MOVE    ZERO                TO  WRK-GAIRAI-IDX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )  OR
                              (SPA-COM-INPUTCD (IDX)  =   SPACE AND
                               SPA-GMN-INPUTCD (IDX)  =   SPACE )
               EVALUATE    SPA-COM-INPUTCD (IDX)
      *            小児科外来診察料
                   WHEN    "113003510" 
                   WHEN    "113003710" 
                       MOVE    IDX                 TO  WRK-IDX-IN
                       MOVE    1                   TO  FLG-SINSATU
                   WHEN    "113003610" 
                   WHEN    "113003810" 
                       MOVE    IDX                 TO  WRK-IDX-IN
                       MOVE    2                   TO  FLG-SINSATU
      *            寝たきり老人在宅総合診療科
                   WHEN    "114701210"
                   WHEN    "114701710"
                       MOVE    IDX                 TO  WRK-IDX-IN
      *
      *            在宅末期医療総合診察料
                   WHEN    "114007610"
                   WHEN    "114007710"
      *H24.4       在宅がん医総（機能強化）
                   WHEN    "114019510"
                   WHEN    "114019710"
                   WHEN    "114019610"
                   WHEN    "114019810"
                       MOVE    IDX                 TO  WRK-IDX-IN
                       MOVE    IDX                 TO  WRK-IDX-IN
      *            在宅時医学総合管理料
                   WHEN    "114012210"
                   WHEN    "114007510"
                   WHEN    "114012310"
                   WHEN    "114012410"
                       MOVE    IDX                 TO  WRK-IDX-IN
      *H24.4
      *        在医総管（機能強化した在支診等）
                   WHEN    "114018710"
                   WHEN    "114018910"
                   WHEN    "114019110"
                   WHEN    "114019310"
                   WHEN    "114018810"
                   WHEN    "114019010"
                   WHEN    "114019210"
                   WHEN    "114019410"
                       MOVE    IDX                 TO  WRK-IDX-IN
      *
      *            生活習慣病指導管理料
                   WHEN    "113003910"
                   WHEN    "113005810"
                   WHEN    "113005910"
                   WHEN    "113004010"
                   WHEN    "113006010"
                   WHEN    "113006110"
                       MOVE    IDX                 TO  WRK-IDX-IN
      *            特定施設入居時医学総合管理料
                   WHEN    "114013010"
                   WHEN    "114013210"
                   WHEN    "114013110"
                   WHEN    "114013310"
                       MOVE    IDX                 TO  WRK-IDX-IN
               END-EVALUATE
           END-PERFORM
      *
           IF     (WRK-IDX-IN          =   ZERO ) AND
                  (WRK-ZAITAKU-IDX     =   ZERO )
               GO     TO      400-INGAI-HEN-EXT
           END-IF
      *
           IF      SPA-SYONIKA-ARI     NOT =   ZERO
      *        小児科外来診療科自動発生処理
               PERFORM 1003-SYONIKA-SET-SEC
           END-IF
      *
           IF      SPA-SEIKATU-ARI     NOT =   ZERO
      *        生活習慣病指導科振替え処理
               PERFORM 3306-SEIKATU-SET-SEC
           END-IF
      *
           IF      SPA-ZAIMATU-ARI     NOT =   ZERO
      *        在宅末期医療総合診察料
               PERFORM 3307-ZAIMATU-SET-SEC
           END-IF
      *
           IF      SPA-ZAIKANRI-ARI     NOT =   ZERO
      *        在宅時医学総合管理料
               PERFORM 3308-ZAIKANRI-SET-SEC
           END-IF
           IF      SPA-IGAKUKAN-ARI     NOT =   ZERO
      *        特定施設入居時医学総合管理料料
               PERFORM 3308-IGAKUKAN-SET-SEC
           END-IF
      *
           .
       400-INGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    生活習慣病指導科自動処理
      *****************************************************************
       3306-SEIKATU-SET-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           IF      SPA-INGAIKBN        =   "1"
      *        院外へ
               EVALUATE    SPA-COM-INPUTCD (WRK-IDX-IN)
      *            高血圧症（院外処方）
                   WHEN    "113004010"
                       MOVE    "113003910"         TO  WRK-SRYCD
      *            高脂血症（院外処方）
                   WHEN    "113006010"
                       MOVE    "113005810"         TO  WRK-SRYCD
      *            高脂血症（院外処方）
                   WHEN    "113006110"
                       MOVE    "113005910"         TO  WRK-SRYCD
               END-EVALUATE
           ELSE
      *        院内へ
               EVALUATE    SPA-COM-INPUTCD (WRK-IDX-IN)
      *            高血圧症（院外処方）
                   WHEN    "113003910"
                       MOVE    "113004010"         TO  WRK-SRYCD
      *            高脂血症（院外処方）
                   WHEN    "113005810"
                       MOVE    "113006010"         TO  WRK-SRYCD
      *            高脂血症（院外処方）
                   WHEN    "113005910"
                       MOVE    "113006110"         TO  WRK-SRYCD
               END-EVALUATE
           END-IF
           IF      WRK-SRYCD           =   SPACE
               GO      TO      3306-SEIKATU-SET-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-ADD
      *
      *    追加共通処理
           PERFORM 33041-SRYCD-ADD-SEC
      *
           .
       3306-SEIKATU-SET-EXT.
           EXIT.
      *****************************************************************
      *    在宅がん医総合（在宅末期医療総合診察料）自動処理
      *****************************************************************
       3307-ZAIMATU-SET-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *H24.4
           IF      SPA-INGAIKBN        =   "1"
      *        院外処方
               EVALUATE    SPA-COM-INPUTCD (WRK-IDX-IN)
               WHEN    "114007710"
                   MOVE    "114007610"         TO  WRK-SRYCD
               WHEN    "114019610"
                   MOVE    "114019510"         TO  WRK-SRYCD
               WHEN    "114019810"
                   MOVE    "114019710"         TO  WRK-SRYCD
               END-EVALUATE
               MOVE    1                   TO  SPA-ZAIMATU-ARI
           ELSE
      *        院内処方
               EVALUATE    SPA-COM-INPUTCD (WRK-IDX-IN)
               WHEN    "114007610"
                   MOVE    "114007710"         TO  WRK-SRYCD
               WHEN    "114019510"
                   MOVE    "114019610"         TO  WRK-SRYCD
               WHEN    "114019710"
                   MOVE    "114019810"         TO  WRK-SRYCD
               END-EVALUATE
               MOVE    2                   TO  SPA-ZAIMATU-ARI
           END-IF
           MOVE    ZERO                TO  FLG-ADD
      *
      *    追加共通処理
           PERFORM 33041-SRYCD-ADD-SEC
      *
           .
       3307-ZAIMATU-SET-EXT.
           EXIT.
      *****************************************************************
      *    在宅自己注射処理
      *****************************************************************
       4001-ZAITAKU-CHU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           IF      SPA-INGAIKBN        =   "1"
      *        院外 
               IF      WRK-ZAITAKU           =   0
      *            在宅自己注射（院外）へ
                   MOVE    "114009210"         TO  WRK-SRYCD
               END-IF
           ELSE
               IF      WRK-ZAITAKU           =   1
      *            在宅自己注射（院内）へ
                   MOVE    "114003210"         TO  WRK-SRYCD
               END-IF
           END-IF
           IF      WRK-SRYCD           =   SPACE
               GO      TO      4001-ZAITAKU-CHU-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-ADD
      *
      *    追加共通処理
           MOVE    WRK-ZAITAKU-IDX     TO  WRK-IDX-IN
           PERFORM 33041-SRYCD-ADD-SEC
      *
           .
       4001-ZAITAKU-CHU-EXT.
           EXIT.
      *****************************************************************
      *    在宅時医学総合管理料処理
      *****************************************************************
       3308-ZAIKANRI-SET-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           IF      SPA-INGAIKBN        =   "1"
      *        院外へ
               EVALUATE    SPA-COM-INPUTCD (WRK-IDX-IN)
      *            在宅時医学総合管理料１
                   WHEN    "114012310"
                       MOVE    "114012210"         TO  WRK-SRYCD
      *            在宅時医学総合管理料２
                   WHEN    "114012410"
                       MOVE    "114007510"         TO  WRK-SRYCD
      *H24.4
      *            在医総管（機能強化した在支診等）（処方せんなし）
                   WHEN    "114018810"
                       MOVE    "114018710"         TO  WRK-SRYCD
                   WHEN    "114019010"
                       MOVE    "114018910"         TO  WRK-SRYCD
                   WHEN    "114019210"
                       MOVE    "114019110"         TO  WRK-SRYCD
                   WHEN    "114019410"
                       MOVE    "114019310"         TO  WRK-SRYCD
      *H24.4
               END-EVALUATE
           ELSE
      *        院内へ
               EVALUATE    SPA-COM-INPUTCD (WRK-IDX-IN)
      *            在宅時医学総合管理料１
                   WHEN    "114012210"
                       MOVE    "114012310"         TO  WRK-SRYCD
      *            在宅時医学総合管理料２
                   WHEN    "114007510"
                       MOVE    "114012410"         TO  WRK-SRYCD
      *H24.4
      *            在医総管（機能強化した在支診等）（処方せん）
                   WHEN    "114018710"
                       MOVE    "114018810"         TO  WRK-SRYCD
                   WHEN    "114018910"
                       MOVE    "114019010"         TO  WRK-SRYCD
                   WHEN    "114019110"
                       MOVE    "114019210"         TO  WRK-SRYCD
                   WHEN    "114019310"
                       MOVE    "114019410"         TO  WRK-SRYCD
      *H24.4
               END-EVALUATE
           END-IF
           IF      WRK-SRYCD       NOT =   SPACE
               MOVE    ZERO                TO  FLG-ADD
      *        追加共通処理
               PERFORM 33041-SRYCD-ADD-SEC
           END-IF
      *
           .
       3308-ZAIKANRI-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定施設入居時医学総合管理料処理
      *****************************************************************
       3308-IGAKUKAN-SET-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           IF      SPA-INGAIKBN        =   "1"
      *        院外へ
               EVALUATE    SPA-COM-INPUTCD (WRK-IDX-IN)
      *            特定施設入居時医学総合管理料１
                   WHEN    "114013110"
                       MOVE    "114013010"         TO  WRK-SRYCD
      *            特定施設入居時医学総合管理料２
                   WHEN    "114013310"
                       MOVE    "114013210"         TO  WRK-SRYCD
               END-EVALUATE
           ELSE
      *        院内へ
               EVALUATE    SPA-COM-INPUTCD (WRK-IDX-IN)
      *            特定施設入居時医学総合管理料１
                   WHEN    "114013010"
                       MOVE    "114013110"         TO  WRK-SRYCD
      *            特定施設入居時医学総合管理料２
                   WHEN    "114013210"
                       MOVE    "114013310"         TO  WRK-SRYCD
               END-EVALUATE
           END-IF
           IF      WRK-SRYCD       NOT =   SPACE
               MOVE    ZERO                TO  FLG-ADD
      *        追加共通処理
               PERFORM 33041-SRYCD-ADD-SEC
           END-IF
      *
           .
       3308-IGAKUKAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     剤分離前の特定算定回数チェック処理
      *****************************************************************
       500-SANTEI-CHKTUKI-SEC            SECTION.
      *
           INITIALIZE                  SPA-K02FLGSET-AREA
      *    算定履歴から判定
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   100  )
                           OR (SPA-SAN-SRYCD (IDX2)    =   SPACE )
               MOVE    ZERO            TO  WRK-SANTEICNT
               PERFORM VARYING     IDX-N2  FROM    1   BY  1
                       UNTIL       IDX-N2  >   31
                   ADD     SPA-SANE-DAY (IDX2 IDX-N2)
                                               TO  WRK-SANTEICNT
               END-PERFORM
               IF      WRK-SANTEICNT   NOT =   ZERO
                   MOVE    SPA-SAN-SRYCD (IDX2)    TO  WRK-SRYCD
                   MOVE    3                   TO  FLG-SANTEI-KBN
                   PERFORM 61001-SANTEICHK-SEC
               END-IF
           END-PERFORM
           .
       500-SANTEI-CHKTUKI-SEC-EXT.
           EXIT.
      *****************************************************************
      *     剤分離前の特定算定回数チェック処理
      *****************************************************************
       600-SANTEI-CNT-SEC              SECTION.
      *
      *    特定の診療行為の有無
           INITIALIZE                  SPA-K02FLGSET-AREA
      *
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI
           MOVE    ZERO                TO  SPA-ROUMANSEI-ARI
           MOVE    ZERO                TO  SPA-HOUMON-ARI
           MOVE    ZERO                TO  SPA-SYONIKA-ARI
           MOVE    ZERO                TO  SPA-TELSAI-ARI
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI
           MOVE    ZERO                TO  SPA-KAIHOU1-ARI
           MOVE    ZERO                TO  SPA-KAIHOU2-ARI
           MOVE    ZERO                TO  SPA-ZAIKANRI-ARI
      *Ｈ２０．４から
           MOVE    ZERO                TO  SPA-KOURESRY-ARI
           MOVE    ZERO                TO  SPA-IGAKUKAN-ARI
      *
           MOVE    ZERO                TO  SPA-SEIKATU-ARI
           MOVE    ZERO                TO  SPA-KYUSEI-FLG
      *    在宅療養指導料算定
           MOVE    ZERO                TO  WRK-ZAITAKU-CNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE          )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
             IF      SPA-COM-HKNCOMBI (IDX)    NOT =   "9999"
      *
      *        特定の診療行為判定
               EVALUATE    SPA-COM-INPUTCD (IDX)
      *            在宅患者訪問診察料
                   WHEN    "114001110"
      *            在宅患者訪問診察料（住居系施設入居者）
                   WHEN    "114012910"
                       MOVE    1               TO  SPA-HOUMON-ARI
      *
      *            在宅末期医療総合診察料
                   WHEN    "114007610"
      *                院外
                       MOVE    1               TO  SPA-ZAIMATU-ARI
                       MOVE    "1"             TO  SPA-INGAIKBN
                   WHEN    "114007710"
      *                院内
                       MOVE    "0"             TO  SPA-INGAIKBN
                       MOVE    2               TO  SPA-ZAIMATU-ARI
      *H24.4
      *            在宅がん医総（機能強化）
                   WHEN    "114019510"
                   WHEN    "114019710"
      *                院外
                       MOVE    1               TO  SPA-ZAIMATU-ARI
                       MOVE    "1"             TO  SPA-INGAIKBN
                   WHEN    "114019610"
                   WHEN    "114019810"
      *                院内
                       MOVE    "0"             TO  SPA-INGAIKBN
                       MOVE    2               TO  SPA-ZAIMATU-ARI
      *
      *            在宅時医学総合管理料
                   WHEN    "114012210"
                   WHEN    "114007510"
      *                院外
                       MOVE    1               TO  SPA-ZAIKANRI-ARI
                       MOVE    "1"             TO  SPA-INGAIKBN
                   WHEN    "114012310"
                   WHEN    "114012410"
      *                院内
                       MOVE    "0"             TO  SPA-INGAIKBN
                       MOVE    2               TO  SPA-ZAIKANRI-ARI
      *H24.4
      *        在医総管（機能強化した在支診等）
                   WHEN    "114018710"
                   WHEN    "114018910"
                   WHEN    "114019110"
                   WHEN    "114019310"
      *                院外
                       MOVE    1               TO  SPA-ZAIKANRI-ARI
                       MOVE    "1"             TO  SPA-INGAIKBN
                   WHEN    "114018810"
                   WHEN    "114019010"
                   WHEN    "114019210"
                   WHEN    "114019410"
      *                院内
                       MOVE    "0"             TO  SPA-INGAIKBN
                       MOVE    2               TO  SPA-ZAIKANRI-ARI
      *
      *            開放型病院共同指導料（１）
                   WHEN    "180010510"
      *            在宅患者入院共同指導料（１）
                   WHEN    "180015910"
                       MOVE    1               TO  SPA-KAIHOU1-ARI
      *            地域連帯退院時共同指導料（１）
                   WHEN    "113008610"
                   WHEN    "113008710"
                       MOVE    1               TO  SPA-KAIHOU1-ARI
      *            寝たきり老人退院時共同指導料（１）
                   WHEN    "190766710"
                       MOVE    1               TO  SPA-KAIHOU2-ARI
      *
      *            生活習慣病指導管理料
                   WHEN    "113003910"
                   WHEN    "113005810"
                   WHEN    "113005910"
      *                院外
                       MOVE    "1"             TO  SPA-INGAIKBN
                       MOVE    1               TO  SPA-SEIKATU-ARI
                   WHEN    "113004010"
                   WHEN    "113006010"
                   WHEN    "113006110"
      *                院内
                       MOVE    "0"             TO  SPA-INGAIKBN
                       MOVE    1               TO  SPA-SEIKATU-ARI
      *
      *            小児科外来診療科算定
                   WHEN    "113003510"
                   WHEN    "113003610"
      *                院外
                       MOVE    "1"             TO  SPA-INGAIKBN
                   WHEN    "113003710"
                   WHEN    "113003810"
      *                院内
                       MOVE    "0"             TO  SPA-INGAIKBN
      *
      *                同日他科受診
                   WHEN    "830000020"
                   WHEN    "830000021"
                       MOVE    1               TO  SPA-FUKU-SRYKA
      *            慢性疼痛疾患管理料
                   WHEN    "113006510"
                       MOVE    1               TO  SPA-TOKUTEI-KNJ
      *            急性発生
                   WHEN    "099800101"
                       MOVE    1               TO  SPA-KYUSEI-FLG
      *            特定施設入居時医学総合管理料
                   WHEN    "114013010"
                   WHEN    "114013210"
      *                院外
                       MOVE    "1"             TO  SPA-INGAIKBN
                       MOVE    1               TO  SPA-IGAKUKAN-ARI
                   WHEN    "114013110"
                   WHEN    "114013310"
      *                院内
                       MOVE    "2"             TO  SPA-INGAIKBN
                       MOVE    1               TO  SPA-IGAKUKAN-ARI
      *!!
      *H24.4 改正
      *        後期高齢者診療料区分流用
      *        外来リハビリテーション診察料
               WHEN    "113013910"
               WHEN    "113014010"
                   MOVE    2                   TO  SPA-KOURESRY-ARI
      *        外来放射線照射診察料
               WHEN    "113014110"
                   MOVE    3                   TO  SPA-KOURESRY-ARI
      *!!
               END-EVALUATE
      *        在宅療養指導料の判定
               IF     (SPA-COM-INPUTCD (IDX)(1:3)  =   "114") AND
                      (SPA-COM-DATAKBN (IDX)       =   "1"  )
                   IF     (SPA-COM-CDKBN-KBN(IDX)  =   "C"  ) AND
                          (SPA-COM-CDKBN-KBNNUM (IDX)
                                                   >=  "100" AND
                                                   <=  "112")
                       MOVE    1               TO  WRK-ZAITAKU-CNT
                   END-IF
               END-IF
      *
      *        診察料検索
               SET     TBL-IDX             TO  1
               SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                        MOVE    ZERO           TO  FLG-ARI
                    WHEN   SPA-COM-INPUTCD (IDX)
                                              =   TBL-SRYCD (TBL-IDX)
                        MOVE    1                  TO  FLG-ARI
                        MOVE    TBL-KBN (TBL-IDX)  TO  WRK-KBN
               END-SEARCH
               IF      FLG-ARI             =   1
      *            小児科外来診療科算定
                   IF      SPA-COM-INPUTCD (IDX)(1:3)
                                                   =   "113"
                       MOVE    1               TO  SPA-SYONIKA-ARI
                   END-IF
               END-IF
             END-IF
           END-PERFORM
      *
      *    算定履歴から判定
           MOVE    SPACE               TO  SPA-NAI-SYOSIN
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   100  )
                           OR (SPA-SAN-SRYCD (IDX2)    =   SPACE )
               MOVE    ZERO            TO  WRK-SANTEICNT
               PERFORM VARYING     IDX-N2  FROM    1   BY  1
                       UNTIL       IDX-N2  >   31
                   ADD     SPA-SAN-DAY (IDX2 IDX-N2)
                                               TO  WRK-SANTEICNT
                   ADD     SPA-SANE-DAY (IDX2 IDX-N2)
                                               TO  WRK-SANTEICNT
               END-PERFORM
               IF      WRK-SANTEICNT   NOT =   ZERO
                   MOVE    SPA-SAN-SRYCD (IDX2)    TO  WRK-SRYCD
                   MOVE    2                   TO  FLG-SANTEI-KBN
                   PERFORM 61001-SANTEICHK-SEC
               END-IF
      *        初診料判定
               IF      WRK-SANTEICNT   NOT =   ZERO
                   MOVE    SPA-SAN-SRYCD (IDX2)    TO  WRK-SRYCD
                   PERFORM 61002-SYOSINCHK-SEC
               END-IF
           END-PERFORM
      *
           IF      SPA-SYONIKA-ARI     =   ZERO
               MOVE    SPA-SYONIKA-ARI2    TO  SPA-SYONIKA-ARI
           END-IF
           IF      SPA-SYONIKA-ARI     =   ZERO
               IF      WRK-ZAITAKU-CNT     =   ZERO
                   MOVE    SPA-SYONIKA-ARI3    TO  SPA-SYONIKA-ARI
               END-IF
           END-IF
           .
       600-SANTEI-CNT-EXT.
           EXIT.
      *****************************************************************
      *    特定の診療行為の有無処理
      *****************************************************************
       61001-SANTEICHK-SEC                SECTION.
      *
           EVALUATE    WRK-SRYCD
      *        小児科外来診療科
               WHEN    "113003510"
               WHEN    "113003610"
               WHEN    "113003710"
               WHEN    "113003810"
                   MOVE    1                   TO  SPA-SYONIKA-ARI2
                   MOVE    1                   TO  SPA-SYONIKA-ARI3
      *        在宅末期医療総合診察料（院外処方）
               WHEN    "114007610"
                   MOVE    1                   TO  SPA-ZAIMATU-ARI2
      *        在宅末期医療総合診察料（院内処方）
               WHEN    "114007710"
                   MOVE    2                   TO  SPA-ZAIMATU-ARI2
      *H24.4
      *        在宅がん医総（機能強化）
               WHEN    "114019510"
               WHEN    "114019710"
      *            院外
                   MOVE    1                   TO  SPA-ZAIMATU-ARI2
               WHEN    "114019610"
               WHEN    "114019810"
      *            院内
                   MOVE    2                   TO  SPA-ZAIMATU-ARI2
      *        在宅時医学総合管理料算定（院外処方）
               WHEN    "114012210"
               WHEN    "114007510"
                   MOVE    1                   TO  SPA-ZAIKANRI-ARI2
      *        在宅時医学総合管理料算定（院内処方）
               WHEN    "114012310"
               WHEN    "114012410"
                   MOVE    2                   TO  SPA-ZAIKANRI-ARI2
      *H24.4
      *        在医総管（機能強化した在支診等）
               WHEN    "114018710"
               WHEN    "114018910"
               WHEN    "114019110"
               WHEN    "114019310"
      *            院外
                   MOVE    1                   TO  SPA-ZAIKANRI-ARI2
               WHEN    "114018810"
               WHEN    "114019010"
               WHEN    "114019210"
               WHEN    "114019410"
      *            院内
                   MOVE    2                   TO  SPA-ZAIKANRI-ARI2
      *
      *        高血圧症
               WHEN    "113003910"
               WHEN    "113004010"
      *        高脂血症
               WHEN    "113005810"
               WHEN    "113006010"
      *        糖尿病
               WHEN    "113005910"
               WHEN    "113006110"
                   MOVE    1               TO  SPA-SEIKATU-ARI2
      **H22.4 廃止 老人の時、生活習慣病の算定判定をしない
      *            IF      SPA-NAI-ROUJIN      =   ZERO
      *                MOVE    1               TO  SPA-SEIKATU-ARI2
      *            ELSE
      *                MOVE    ZERO            TO  SPA-SEIKATU-ARI2
      *                IF      FLG-SANTEI-KBN  =   3
      *                    MOVE    1               TO  SPA-NAI-SEIKATU
      *                    MOVE    ZERO            TO
      *                                            SPA-NAI-SEIKATUCHK
      *                END-IF
      *******      END-IF
      *        痴呆患者在宅指導管理料
               WHEN    "113700310"
                       MOVE    1               TO  SPA-GAIRAI-NASI
      *        慢性疼痛疾病管理料
               WHEN    "113006510"
                   MOVE    1                   TO  SPA-TOKUTEI-KNJ2
      **           初回算定日より前の算定はなしとする
      *            IF    (SANTEI-FSANTEIYMD(1:6)
      *                                        =   SPA-SRYYMD(1:6))
      *              AND (SANTEI-FSANTEIYMD    >   SPA-SRYYMD )
      *                MOVE    ZERO            TO  SPA-TOKUTEI-KNJ2
      **           END-IF
      *Ｈ２０．４から
      *H22.4   後期高齢者診療料
      *        WHEN    "113702110"
      *******      MOVE    1                   TO  SPA-KOURESRY-ARI2
      *        特定施設入居時医学総合管理料
               WHEN    "114013010"
               WHEN    "114013210"
      *            院外
                   MOVE    1                   TO  SPA-IGAKUKAN-ARI2
               WHEN    "114013110"
               WHEN    "114013310"
      *            院内
                   MOVE    2                   TO  SPA-IGAKUKAN-ARI2
      *H18/04から
      *        介達牽引（矯正固定）
               WHEN    "140030350"
                   ADD     WRK-SANTEICNT       TO  SPA-SYOEN-CNT
      *        介達牽引（変形機械矯正術）
               WHEN    "140048250"
                   ADD     WRK-SANTEICNT       TO  SPA-SYOEN-CNT
      *        腰部固定帯固定
               WHEN    "140048350"
                   ADD     WRK-SANTEICNT       TO  SPA-SYOEN-CNT
      *        胸部固定帯固定
               WHEN    "140048450"
                   ADD     WRK-SANTEICNT       TO  SPA-SYOEN-CNT
      *        低出力レーザ照射
               WHEN    "140048550"
                   ADD     WRK-SANTEICNT       TO  SPA-SYOEN-CNT
      *        肛門処置
               WHEN    "140002450"
                   ADD     WRK-SANTEICNT       TO  SPA-SYOEN-CNT
      *まで
      *H24.4 改正
      *        後期高齢者診療料区分流用
      *        外来リハビリテーション診察料
      *        WHEN    "113013910"
      *        WHEN    "113014010"
      *            MOVE    2                   TO  SPA-KOURESRY-ARI2
      *        外来放射線照射診察料
      *        WHEN    "113014110"
      *            MOVE    3                   TO  SPA-KOURESRY-ARI2
      *!!
      *        在宅療養指導料の判定
               WHEN    OTHER
                   IF      WRK-SRYCD(1:3)      =   "114"
                       MOVE    WRK-SRYCD       TO  TNS-SRYCD
                       PERFORM 9101-TENSU-READ2-SEC
                       IF      (FLG-TENSU          =   ZERO ) AND
                               (TNS-CDKBN-KBN      =   "C"  ) AND
                               (TNS-CDKBN-KBNNUM   >=  "100" AND
                                                   <=  "112")
                           ADD     WRK-SANTEICNT
                                                   TO  WRK-ZAITAKU-CNT
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       61001-SANTEICHK-EXT.
           EXIT.
      *****************************************************************
      *    特定の診療行為の有無処理
      *****************************************************************
       61002-SYOSINCHK-SEC                SECTION.
      *
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-KBN
                   WHEN    WRK-SRYCD           =   TBL-SRYCD(TBL-IDX)
                       MOVE    TBL-KBN (TBL-IDX)
                                               TO  WRK-KBN
           END-SEARCH
      *    初診料算定
           MOVE    ZERO                TO  WRK-SYODD
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           IF     (WRK-KBN             =   "F"         )
      *        ダミー 
             OR   (WRK-SRYCD           =   "099110001" )
               PERFORM VARYING     IDX-N2  FROM    1   BY  1
                       UNTIL       IDX-N2  >   WRK-SRYDD
                   IF      SPA-SAN-DAY (IDX2 IDX-N2)   >   ZERO
                       MOVE    IDX-N2          TO  WRK-SYODD
                   END-IF
               END-PERFORM
               IF      WRK-SYODD           >   ZERO
                   MOVE    SPA-SRYYMD          TO  SPA-NAI-SYOSIN
                   MOVE    WRK-SYODD           TO  SPA-NAI-SYOSIN(7:2)
               END-IF
               IF      SPA-NAI-SYOSIN      <   SPA-SYOSIN
                   MOVE    SPACE               TO  SPA-NAI-SYOSIN
               END-IF
           END-IF
           .
        61002-SYOSINCHK-EXTI.
           EXIT.
      *****************************************************************
      *     剤分離後の特定算定チェック処理
      *****************************************************************
       700-SANTEI-CHK-SEC              SECTION.
      *
      *    小児科外来診療チェック
           PERFORM 5019-SYONIKA-CHK-SEC
      *
      *    生活習慣病指導管理科チェック
           IF     (SPA-SEIKATU-ARI     NOT =   ZERO )  OR
                  (SPA-SEIKATU-ARI2    NOT =   ZERO )
      *        包括診療はチェックなし
               IF    ( SPA-HKTSANTEI-203   =   ZERO  )
               AND   ( SPA-ERRCD           =   SPACE )
                   PERFORM 5022-SEIKATU-CHK-SEC
               END-IF
           END-IF
      *
      *    在宅末期医療総合診察料チェック
           IF     (SPA-ZAIMATU-ARI     NOT =   ZERO )  OR
                  (SPA-ZAIMATU-ARI2    NOT =   ZERO )
      *        包括診療はチェックなし
               IF    ( SPA-HKTSANTEI-206   =   ZERO  )
               AND   ( SPA-ERRCD           =   SPACE )
      *
                   PERFORM 5022-ZAIMATU-CHK-SEC
               END-IF
           END-IF
      *
      *    在宅時医学総合管理料チェック
           IF     (SPA-ZAIKANRI-ARI    NOT =   ZERO )
             OR   (SPA-ZAIKANRI-ARI2   NOT =   ZERO )
      *        包括診療はチェックなし
               IF    ( SPA-HKTSANTEI-204   =   ZERO  )
               AND   ( SPA-ERRCD           =   SPACE )
                   PERFORM 5022-ZAIKANRI-CHK-SEC
               END-IF
           END-IF
      *
      *    後期高齢者診療料チェック
      *    IF     (SPA-KOURESRY-ARI    NOT =   ZERO )
      *      OR   (SPA-KOURESRY-ARI2   NOT =   ZERO )
      *        IF      SPA-ERRCD           =   SPACE
      *            PERFORM 5022-KOURESRY-CHK-SEC
      *        END-IF
      *****END-IF
      *
      *    特定施設入居時等医学管理料
           IF     (SPA-IGAKUKAN-ARI    NOT =   ZERO )
             OR   (SPA-IGAKUKAN-ARI2   NOT =   ZERO )
      *        包括診療はチェックなし
               IF    ( SPA-HKTSANTEI-205   =   ZERO  )
               AND   ( SPA-ERRCD           =   SPACE )
                   PERFORM 5022-IGAKUKAN-CHK-SEC
               END-IF
           END-IF
      *
           .
       700-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診療科チェック処理
      *****************************************************************
       5019-SYONIKA-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *      自費は対象外
             IF     (SPA-COM-SRYKBN(IDX)     =   "95" OR
                                                 "96" OR
                                                 "98" OR
                                                 "99"  )  OR
      *!             複数科・保険行
                    (SPA-GMN-INPUTCD(IDX)(1:1)   =   "#"  OR
                                                     "$" )
      *              コメント
                 OR (SPA-COM-INPUTCD(IDX)(1:1)   =   "8" )
                 OR (SPA-COM-INPUTCD(IDX)(1:3)   =   "008" )
      *          包括保険
                 OR (SPA-COM-HKNCOMBI (IDX)      =   "9999")
      *          治験
                 OR (SPA-COM-CHIKENKBN (IDX)     =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)        =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)        =   "1")
                 CONTINUE
             ELSE
      *        小児科外来診療科の時、小児科外来診療科のみであること
               IF      SPA-SYONIKA-ARI     NOT =   ZERO
                   EVALUATE    SPA-COM-SRYKBN  (IDX)
                       WHEN    "11"
                       WHEN    "12"
                           MOVE    ZERO            TO  FLG-OK
      *                    診察料検索
                           PERFORM 50191-SRYCD-CHK-SEC
      *                    電話再診で、当日診察がないこと
                           IF     (SPA-SYONIKA-ARI NOT =   2 )
                             AND  (SPA-SYONIKA-ARI2    =   ZERO)
                             AND  (WRK-KBN             =   "T"
                                                       OR  "S"  )
                               MOVE    1           TO  FLG-KBN
                               MOVE    1           TO  FLG-OK
                           END-IF
      *                    電話再診の加算はＯＫ
                           IF     (FLG-KBN             =   1  ) AND
                                  (SPA-COM-SRYKBN(IDX) =   "12") AND
                                  (SPA-COM-DATAKBN(IDX)
                                                       =   "2" )
                               MOVE    1           TO  FLG-OK
                           END-IF
                           IF      FLG-OK          =   1
                               CONTINUE
                           ELSE
                               MOVE    "0044"      TO  SPA-ERRCD
                               MOVE    "1"         TO  SPA-COM-CUR
                                                                (IDX)
                           END-IF
      *
                       WHEN    "13"
      *                    種別のみはＯＫ
                           IF      SPA-GMN-INPUTCD(IDX)(1:4) =   ".130"
                               CONTINUE
                           ELSE
      *                        診察料検索
                               PERFORM 50191-SRYCD-CHK-SEC
                               IF      WRK-KAISU       =   2
      *                        同日再診の時、診察料は算定できない
                                   IF      SPA-SYONIKA-ARI     =   2
                                       MOVE    "0046"  TO  SPA-ERRCD
                                       MOVE    "1"     TO  SPA-COM-CUR
                                                              (IDX)
                                   END-IF
                               END-IF
      *                        包括診療以外
                               IF      SPA-HKTSANTEI-202   =   ZERO
      *
      *                        特定の診療行為であること
                               PERFORM 50192-SYONIKASRYCD-CHK-SEC
                               IF      FLG-SYONIKA-CHK     =   ZERO
                                   MOVE    "0044"      TO  SPA-ERRCD
                                   MOVE    "1"         TO  SPA-COM-CUR
                                                                (IDX)
                               END-IF
      *
                               END-IF
      *
                           END-IF
      *
      *                往診料であること
                       WHEN    "14"
                           CONTINUE
      *
                       WHEN    "21"
                       WHEN    "22"
                       WHEN    "23"
      *                包括診療以外
                       IF      SPA-HKTSANTEI-202   =   ZERO
      *                    院内時の診療種別変更
                           PERFORM 50192-SRYSYUKBN-CHK-SEC
      *
      *                    薬剤の時、院外処方であること
                           IF    ((SPA-INGAIKBN        =   "1"  ) AND
                                  (SPA-COM-SRYSYUKBN(IDX)
                                                       =   "210" OR
                                                           "212" OR
                                                           "220" OR
                                                           "222" OR
                                                           "230" OR
                                                           "232" OR
                                                           "290" OR
                                                           "292" OR
                                                           SPACE )) OR
                                 ( SPA-COM-SRYSYUKBN(IDX)
                                                       =   "212" OR
                                                           "222" OR
                                                           "232" OR
                                                           "292" )
                               CONTINUE
                           ELSE
      *                      処方のみはＯＫ
                             IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0044"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                (IDX)
                             END-IF
                           END-IF
                       END-IF
                       WHEN    OTHER
      *                包括診療以外
                       IF      SPA-HKTSANTEI-202   =   ZERO
      *
                            MOVE    "0044"      TO  SPA-ERRCD
                            MOVE    "1"         TO  SPA-COM-CUR
                                                          (IDX)
      *                    開放型病院共同指導料（１）であること
                           PERFORM 50192-SYONIKASRYCD-CHK-SEC
                           IF      FLG-SYONIKA-CHK     =   1
                               MOVE    SPACE       TO  SPA-ERRCD
                               MOVE    SPACE       TO  SPA-COM-CUR
                                                                (IDX)
                           END-IF
      *                    その他の診療種別
                           IF      SPA-GMN-INPUTCD(IDX)(1:4)  =   ".800"
                                                              OR  ".850"
                               MOVE    SPACE       TO  SPA-ERRCD
                               MOVE    SPACE       TO  SPA-COM-CUR
                                                                (IDX)
                           END-IF
                       END-IF
                   END-EVALUATE
               ELSE
      *        小児科外来診療科以外の時、小児科外来診療科でないこと
      *            特定の診療行為であること
                   PERFORM 50192-SYONIKASRYCD-CHK-SEC
                   IF     (FLG-SYONIKA-CHK         =   ZERO)  OR
      *                   開放型病院共同指導料、地域連帯加算
                          (WRK-SYONIKA-KBN         =   "9" )
                       CONTINUE
                   ELSE
                       MOVE    "0045"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-IF
               END-IF
             END-IF
      *
             IF      SPA-COM-ZAIEND (IDX)  =   "1"
                  MOVE    ZERO             TO  FLG-KBN
             END-IF
           END-PERFORM
           .
       5019-SYONIKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬種別変更処理
      *****************************************************************
       50192-SRYSYUKBN-CHK-SEC                SECTION.
      *
      *    自動変換なし
           IF      SPA-CHGSRYSYU-FLG   =   "1"
               GO     TO      50192-SRYSYUKBN-CHK-EXT
           END-IF
      *
           IF      SPA-COM-SRYSYUKBN(IDX)  =   SPACE
      *        院外
               IF      SPA-INGAIKBN        =   "1"
                   CONTINUE
               ELSE
                   MOVE    SPA-COM-SRYKBN(IDX)
                                           TO  SPA-COM-SRYSYUKBN(IDX)
                                                               (1:2)
                   MOVE    "3"             TO  SPA-COM-SRYSYUKBN(IDX)
                                                               (3:1)
               END-IF
           END-IF
           IF      SPA-COM-SRYSYUKBN(IDX)  =   "210"
                                           OR  "220"
                                           OR  "230"
      *        院外
               IF      SPA-INGAIKBN        =   "1"
                   CONTINUE
               ELSE
                   MOVE    "3"             TO  SPA-COM-SRYSYUKBN(IDX)
                                                               (3:1)
                   IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                       MOVE    "3"         TO  SPA-GMN-INPUTCD(IDX)
                                                               (4:1)
                       PERFORM 501921-SRYSYUMEI-HEN-SEC
                   END-IF
               END-IF
           END-IF
      **** 処方のみ
      *    IF      SPA-COM-SRYSYUKBN(IDX)(3:1) =   "3"
      *        院外
      *        IF      SPA-INGAIKBN        =   "1"
      *            MOVE    "0"             TO  SPA-COM-SRYSYUKBN(IDX)
      *                                                        (3:1)
      *            IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
      *                MOVE    "0"         TO  SPA-GMN-INPUTCD(IDX)
      *                                                        (4:1)
      *                PERFORM 501921-SRYSYUMEI-HEN-SEC
      *            END-IF
      *        END-IF
      *****END-IF
           .
       50192-SRYSYUKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別名編集処理
      *****************************************************************
       501921-SRYSYUMEI-HEN-SEC             SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  SPA-GMN-MEISYO (IDX)
                   WHEN    SPA-COM-SRYSYUKBN(IDX)
                                           =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                           TO  SPA-GMN-MEISYO (IDX)
           END-SEARCH 
      *
           .
       501921-SRYSYUMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診察料検索処理
      *****************************************************************
       50191-SRYCD-CHK-SEC                SECTION.
      *    診察料検索
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
               AT   END
                    MOVE    ZERO           TO  FLG-ARI
                    MOVE    SPACE          TO  WRK-KBN
                    MOVE    ZERO           TO  WRK-KAISU
                WHEN   SPA-COM-INPUTCD (IDX)
                                          =   TBL-SRYCD (TBL-IDX)
                    MOVE    1                  TO  FLG-ARI
                    MOVE    TBL-KBN (TBL-IDX)  TO  WRK-KBN
                    MOVE    TBL-KAISU (TBL-IDX)    TO  WRK-KAISU
           END-SEARCH
           .
       50191-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    小児科外来診察料科チェックク処理
      *****************************************************************
       50192-SYONIKASRYCD-CHK-SEC                SECTION.
      *
      *    診察料検索
           SET     TBL-SYO             TO  1
           SEARCH  TBL-SYONIKA-OCC     VARYING   TBL-SYO
               AT   END
                    MOVE    ZERO           TO  FLG-SYONIKA-CHK
                    MOVE   SPACE           TO  WRK-SYONIKA-KBN
                WHEN   SPA-COM-INPUTCD (IDX)
                                           =   TBL-SYONIKA-SRYCD
                                                      (TBL-SYO)
                    MOVE    TBL-SYONIKA-KBN (TBL-SYO)
                                           TO  WRK-SYONIKA-KBN
                    MOVE    1              TO  FLG-SYONIKA-CHK
           END-SEARCH
      *
      *H24.4 追加のコード
           IF      SPA-COM-INPUTCD (IDX)   =   "113013710"
                                           OR  "113013810"
      *        院内トリアージ実施料・夜間休日救急搬送医学管理料
               MOVE    1                   TO  FLG-SYONIKA-CHK
               MOVE    "9"                 TO  WRK-SYONIKA-KBN
           END-IF
           .
       50192-SYONIKASRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人慢性疾病外来総合診療科チェックク処理
      *****************************************************************
       5020-ROUMANSEI-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *      包括保険
             IF     (SPA-COM-HKNCOMBI (IDX)      =   "9999")
                 OR (SPA-GMN-INPUTCD  (IDX)(1:1) =   "#"
                                                 OR  "$"   )
      *          治験
                 OR (SPA-COM-CHIKENKBN (IDX)     =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)        =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)       =   "1")
               CONTINUE
             ELSE
      *        生活指導、検査、注射、在宅料以外であること
               EVALUATE    SPA-COM-SRYKBN(IDX)
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                   WHEN    "60"
                       MOVE    "0052"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
      *                院内時の診療種別変更
                       PERFORM 50192-SRYSYUKBN-CHK-SEC
      *                薬剤の時、院外処方であること
                       IF    ((SPA-INGAIKBN        =   "1"  ) AND
                              (SPA-COM-SRYSYUKBN(IDX)
                                                   =   "210" OR
                                                       "212" OR
                                                       "220" OR
                                                       "222" OR
                                                       "230" OR
                                                       "232" OR
                                                       "290" OR
                                                       "292" OR
                                                       SPACE  ))  OR
                             ( SPA-COM-SRYSYUKBN(IDX)
                                                   =   "212" OR
                                                       "222" OR
                                                       "232" OR
                                                       "292"  )
      *!!                  院外処方箋を交付しない場合の時、エラー
      *                    IF      SPA-ROUMANSEI-ARI   =   2
      *                      処方のみはＯＫ
      *                      IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
      *                                            NOT =   "3" 
      *                        MOVE    "0052"          TO  SPA-ERRCD
      *                        MOVE    "1"             TO  SPA-COM-CUR
      *                                                        (IDX)
      *                      END-IF
      *!!                  END-IF
                           CONTINUE
                       ELSE
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0052"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                               (IDX)
                           END-IF
                       END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
           .
       5020-ROUMANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    生活習慣病指導管理科チェック処理
      *****************************************************************
       5022-SEIKATU-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *      包括保険
             IF     (SPA-COM-HKNCOMBI (IDX)      =   "9999")
                 OR (SPA-GMN-INPUTCD  (IDX)(1:1) =   "#"
                                                 OR  "$"   )
      *          治験
                 OR (SPA-COM-CHIKENKBN (IDX)     =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)        =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)       =   "1")
               CONTINUE
             ELSE
      *        検査、投薬、注射は算定できない
               EVALUATE    SPA-COM-SRYKBN(IDX)
                   WHEN    "60"
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                       MOVE    "0068"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
      *                院内時の診療種別変更
                       PERFORM 50192-SRYSYUKBN-CHK-SEC
      *                薬剤の時、院外処方であること
                       IF    ((SPA-INGAIKBN        =   "1"  ) AND
                              (SPA-COM-SRYSYUKBN(IDX)
                                                   =   "210" OR
                                                       "212" OR
                                                       "220" OR
                                                       "222" OR
                                                       "230" OR
                                                       "232" OR
                                                       "290" OR
                                                       "292" OR
                                                       SPACE  ))  OR
                             ( SPA-COM-SRYSYUKBN(IDX)
                                                   =   "212" OR
                                                       "222" OR
                                                       "232" OR
                                                       "292"  )
      *!!                  院外処方箋を交付しない場合の時、エラー
      *                    IF     (SPA-SEIKATU-ARI     =   2 )  OR
      *                           (SPA-SEIKATU-ARI2    =   2 )
      *                      処方のみはＯＫ
      *                      IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
      *                                            NOT =   "3" 
      *                        MOVE    "0068"          TO  SPA-ERRCD
      *                        MOVE    "1"             TO  SPA-COM-CUR
      *                                                         (IDX)
      *                      END-IF
      *!!                  END-IF
                           CONTINUE
                       ELSE
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0068"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                             (IDX)
                           END-IF
                       END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
           .
       5022-SEIKATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    在宅時医学総合管理料チェック処理
      *****************************************************************
       5022-ZAIKANRI-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE    )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *      包括保険
             IF     (SPA-COM-HKNCOMBI (IDX)      =   "9999")
                 OR (SPA-GMN-INPUTCD  (IDX)(1:1) =   "#"
                                                 OR  "$"   )
      *          治験
                 OR (SPA-COM-CHIKENKBN (IDX)     =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)        =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)       =   "1")
               CONTINUE
             ELSE
      *        投薬は算定できない
               EVALUATE    SPA-COM-SRYKBN(IDX)
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
      *                院内時の診療種別変更
                       PERFORM 50192-SRYSYUKBN-CHK-SEC
      *                薬剤の時、院外処方であること
                       IF    ((SPA-INGAIKBN        =   "1"  ) AND
                              (SPA-COM-SRYSYUKBN(IDX)
                                                   =   "210" OR
                                                       "212" OR
                                                       "220" OR
                                                       "222" OR
                                                       "230" OR
                                                       "232" OR
                                                       "290" OR
                                                       "292" OR
                                                       SPACE  ))  OR
                             ( SPA-COM-SRYSYUKBN(IDX)
                                                   =   "212" OR
                                                       "222" OR
                                                       "232" OR
                                                       "292"  )
      *!!                  院外処方箋を交付しない場合の時、エラー
      *                    IF     (SPA-SEIKATU-ARI     =   2 )  OR
      *                           (SPA-SEIKATU-ARI2    =   2 )
      *                      処方のみはＯＫ
      *                      IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
      *                                            NOT =   "3" 
      *                        MOVE    "1402"          TO  SPA-ERRCD
      *                        MOVE    "1"             TO  SPA-COM-CUR
      *                                                         (IDX)
      *                      END-IF
      *!!                  END-IF
                           CONTINUE
                       ELSE
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "1402"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                             (IDX)
                           END-IF
                       END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
           .
       5022-ZAIKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    在宅末期医療総合診察料チェック処理
      *****************************************************************
       5022-ZAIMATU-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE    )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *      包括保険
             IF     (SPA-COM-HKNCOMBI (IDX)      =   "9999")
                 OR (SPA-GMN-INPUTCD  (IDX)(1:1) =   "#"
                                                 OR  "$"   )
      *          治験
                 OR (SPA-COM-CHIKENKBN (IDX)     =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)        =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)       =   "1")
               CONTINUE
             ELSE
      *        今回入力
               IF      SPA-ZAIMATU-ARI    NOT  =   ZERO
      *            在宅料以外は算定できない
                   EVALUATE    SPA-COM-SRYKBN(IDX)
                   WHEN    "14"
                   WHEN    "95"
                   WHEN    "96"
                   WHEN    "98"
                   WHEN    "99"
                       CONTINUE
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
      *                院内時の診療種別変更
                       PERFORM 50192-SRYSYUKBN-CHK-SEC
      *                薬剤の時、院外処方であること
                       IF    ((SPA-INGAIKBN        =   "1"  ) AND
                              (SPA-COM-SRYSYUKBN(IDX)
                                                   =   "210" OR
                                                       "212" OR
                                                       "220" OR
                                                       "222" OR
                                                       "230" OR
                                                       "232" OR
                                                       "290" OR
                                                       "292" OR
                                                       SPACE  ))  OR
                             ( SPA-COM-SRYSYUKBN(IDX)
                                                   =   "212" OR
                                                       "222" OR
                                                       "232" OR
                                                       "292"  )
                           CONTINUE
                       ELSE
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "1401"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                             (IDX)
                           END-IF
                       END-IF
                       WHEN    "11"
                       WHEN    "12"
                           CONTINUE
                       WHEN    OTHER
      *                    算定エラー
                           MOVE    "1401"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-EVALUATE
               END-IF
            END-IF
           END-PERFORM
           .
       5022-ZAIMATU-CHK-EXT.
           EXIT.
      *
      *H22.4 から廃止
      *****************************************************************
      *    後期高齢者診療料チェック処理
      *****************************************************************
       5022-KOURESRY-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE    )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *        包括保険
               IF     (SPA-COM-HKNCOMBI (IDX)      =   "9999")
                   OR (SPA-GMN-INPUTCD  (IDX)(1:1) =   "#"
                                                   OR  "$"   )
      *          治験
                 OR (SPA-COM-CHIKENKBN (IDX)     =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)        =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)       =   "1")
                   CONTINUE
               ELSE
                   EVALUATE    SPA-COM-SRYKBN(IDX)
      *            画像診断・検査・処置
                   WHEN    "40"
                   WHEN    "60"
                   WHEN    "70"
      *                手技・加算で５５０点以下はエラー
                       IF      SPA-COM-INPUTCD  (IDX)(1:1) =   "1"
                           IF      SPA-COM-TEN (IDX)       <   550
                               IF     (SPA-COM-DATAKBN(IDX)  =   "1" )
                                   OR (FLG-CHK               =   ZERO)
                                   MOVE    "0241"      TO  SPA-ERRCD
                                   MOVE    "1"         TO  SPA-COM-CUR
                                                                (IDX)
                               END-IF
                           ELSE
                               IF      SPA-COM-DATAKBN(IDX)  =   "1"
                                   MOVE    1                 TO  FLG-CHK
                               END-IF
      *                    ５５０点 以上は警告
                               IF     (SPA-COM-KZMFLG (IDX)
                                                       =   SPACE )
                                   MOVE    "K241"      TO  SPA-ERRCD
                                   MOVE    "1"         TO  SPA-COM-CUR
                                                                (IDX)
                                   MOVE    "1"     TO  SPA-COM-KZMFLG
                                                                (IDX)
                               END-IF
                           END-IF
                       END-IF
      *                剤終了
                       IF      SPA-COM-ZAIEND (IDX)    =   "1"
                           MOVE    ZERO               TO  FLG-CHK
                       END-IF
      *                処置医療機器加算は算定可能
                       IF      (SPA-COM-CDKBN-KBN    (IDX) =   "J" )
                           AND (SPA-COM-CDKBN-KBNNUM (IDX) =   "200")
                           AND (SPA-COM-CDKBN-KBNNUM-EDA(IDX)
                                                           =   "00" )
                           MOVE    SPACE           TO  SPA-ERRCD
                           MOVE    SPACE           TO  SPA-COM-CUR
                                                                (IDX)
                       END-IF
                   WHEN    "13"
                       IF      SPA-COM-INPUTCD  (IDX)(1:1) =   "1"
                           IF      (SPA-COM-CDKBN-KBN (IDX) =   "B" )
                              AND  (SPA-COM-CDKBN-KBNNUM(IDX)
                                                            =   "009"
                                                            OR  "010"
                                                            OR  "016"
                                                            OR  "017"
                                                            OR  "018")
                               CONTINUE
                           ELSE
                               MOVE    "0241"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                (IDX)
                           END-IF
                       END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       5022-KOURESRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定施設入居時等医学管理料チェック処理
      *****************************************************************
       5022-IGAKUKAN-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE    )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *      包括保険
             IF     (SPA-COM-HKNCOMBI (IDX)      =   "9999")
                 OR (SPA-GMN-INPUTCD  (IDX)(1:1) =   "#"
                                                 OR  "$"   )
      *          治験
                 OR (SPA-COM-CHIKENKBN (IDX)     =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)        =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)       =   "1")
               CONTINUE
             ELSE
      *        投薬は算定できない
               EVALUATE    SPA-COM-SRYKBN(IDX)
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
      *                院内時の診療種別変更
                       PERFORM 50192-SRYSYUKBN-CHK-SEC
      *                薬剤の時、院外処方であること
                       IF    ((SPA-INGAIKBN        =   "1"  ) AND
                              (SPA-COM-SRYSYUKBN(IDX)
                                                   =   "210" OR
                                                       "212" OR
                                                       "220" OR
                                                       "222" OR
                                                       "230" OR
                                                       "232" OR
                                                       "290" OR
                                                       "292" OR
                                                       SPACE  ))  OR
                             ( SPA-COM-SRYSYUKBN(IDX)
                                                   =   "212" OR
                                                       "222" OR
                                                       "232" OR
                                                       "292"  )
                           CONTINUE
                       ELSE
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "1402"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                             (IDX)
                           END-IF
                       END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
           .
       5022-IGAKUKAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC              SECTION.
      *
      *    算定履歴に関する項目のみ
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key22"             TO  MCP-PATHNAME
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
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       9101-TENSU-READ2-SEC              SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
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
       9101-TENSU-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      ******************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       960-SYSKANRI-READ-SEC         SECTION.
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
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       960-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      *    EXIT.
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
