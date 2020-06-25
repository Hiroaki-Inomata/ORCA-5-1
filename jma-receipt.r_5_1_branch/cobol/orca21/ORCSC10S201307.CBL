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
       PROGRAM-ID.                 ORCSC10S201307.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為診察料自動発生編集処理
      *                      （Ｋ０２のみ共通使用）
      *                      Ｈ２５年７月　労災コードレセ電対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/03/01    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/04/19  生活習慣病指導算定時の追加
      * 01.00.02     MCC-多々納   02/04/22  初診・再診料の判定修正
      * 01.00.03     MCC-多々納   02/04/**  労災用追加
      * 01.00.04     NACL-多々納  02/08/06  アフターケアの時自動算定変更
      * 01.00.05     NACL-多々納  02/11/11  ＤＵＭＭＹ診察料追加
      * 01.00.06     NACL-多々納  02/11/28  中途終了再表示修正
      * 01.00.07     NACL-多々納  02/12/02  生活習慣病のコードエラー修正
      * 01.00.08     NACL-多々納  02/12/06  慢性疼痛の算定チェック修正
      * 01.00.09     NACL-多々納  03/02/19  療養担当手当追加
      * 01.00.10     NACL-多々納  03/03/28  自賠責の逓減を６ヶ月以降とする
      * 01.00.11     NACL-多々納  03/04/07  入院日のチェック追加
      * 02.00.00     NACL-多々納  03/07/01  労災・自賠責以外診察回数をやめる
      * 02.00.01     NACL-多々納  03/08/29  Ｈ１５．９から
      *                                     労災・自賠責逓減制の廃止
      *                                     （レセ電コードの変更なし）
      * 02.00.02     NACL-多々納  03/10/27  育児栄養加算自動算定追加
      * 02.00.02     NACL-多々納  04/03/11  小児科外来診察料チェック修正
      * 02.00.03     NACL-多々納  05/09/28  小児科外来診察料と
      *                                     在宅療養指導管理料の算定修正
      * 02.00.04     NACL-多々納  05/10/24  消炎鎮痛等処置判定修正
      * 02.00.05     NACL-多々納  05/11/18  MONFUNC 対応 他
      * 02.00.06     NACL-多々納  05/11/21  開放型病院共同指導料チェック追加
      * 02.00.07     NACL-多々納  05/12/14  .213 自動変換対応
      * 03.03.00     NACL-多々納  06/10/20  矯正固定等追加
      * 03.04.00     NACL-多々納  06/12/01  自賠責健保準拠追加
      * 03.04.01     NACL-多々納  07/04/XX  H19.4改正
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 03.05.01     NACL-多々納  07/07/XX  同日再診と訂正の選択対応他
      * 04.00.00     NACL-多々納  07/09/XX  テーブル４００対応
      * 04.01.00     NACL-多々納  07/12/XX  .213 自動変換システム設定対応
      *                                     アトピー性皮膚炎の年齢対応
      * 04.02.00     NACL-多々納  08/03/XX  平成２０年４月改正対応
      * 04.03.00     NACL=多々納  08/08/07  労災初診料Ｈ２０年４月改正対応他
      * 04.05.00     NACL=多々納  09/07/28  リハビリ発症日表示対応
      * 04.05.00     NACL-多々納  09/09/XX  小児科外来診察料等包括診療対応
      * 04.05.00     NACL-多々納  09/10/20  初診再診自動発生なし対応
      * 04.05.00     NACL-多々納  10/02/XX  アフターケア対象外対応
      * 04.05.00     NACL-多々納  10/03/XX  平成２２年４月改正対応
      * 04.07.00     NACL-多々納  11/08/XX  リハビリ開始日の終了日対応
      * 04.07.00     NACL-多々納  11/09/08  アトピー性皮膚炎と他病名対応
      * 04.07.00     NACL-多々納  11/11/08  夜間早朝加算の小児科特例対応
      * 04.07.00     NACL-多々納  12/03/XX  平成２４年４月改正対応
      * 04.07.00     NACL-多々納  12/05/01  外来リハビリ診察料等対応
      * 04.07.00     NACL-多々納  12/08/08  診察料なし同日初診料発生対応
      * 04.08.00     NACL-多々納  12/12/XX  電話再診の加算算定対応
      * 04.08.00     NACL-多々納  13/03/XX  平成２５年４月改正対応
      * 04.08.00     NACL-多々納  13/02/XX  労災コードレセ電対応
      * 04.08.00     NACL-多々納  13/09/XX  労災同日再診コード追加対応
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
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
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
           03  FLG-TOKUTEI05        PIC 9(02).
           03  FLG-ATOPII           PIC 9(01).
           03  FLG-ADD2             PIC 9(01).
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
           03  FLG-DOUJI-HEN2       PIC 9(01).
      *
           03  FLG-CHK-END          PIC 9(01).
      *H24.4
      *    ３歳未満再診料あり
           03  FLG-03MIMAN         PIC 9(01).
      *H25.4
           03  FLG-SYOKAINASI      PIC 9(01).
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
           03  IDX-HZN              PIC 9(04).
      *
           03  IDX-STR              PIC 9(02).
           03  IDX-R                PIC 9(02).
      *
           03  IDX-TEL                 PIC 9(04).
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
      *H24.4
      *    年齢判定用
           03  WRK-BIRTHDAY03.
               05  WRK-BIRTH-YY03  PIC 9(04).
               05  WRK-BIRTH-MM03  PIC 9(02).
               05  WRK-BIRTH-DD03  PIC 9(02).
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
               05  WRK-YYMM.
                   07  WRK-YY              PIC 9(04).
                   07  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-KAISU               PIC 9(01).
           03  WRK-HOSPSBT             PIC 9(01).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-KBN                 PIC X(01).
           03  WRK-KBN2                PIC X(01).
           03  WRK-KBN3                PIC X(01).
      *    紹介対応
           03  WRK-KBN4                PIC X(01).
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
      *   リハビリ発症日表示
           03  WRK-RIHABIR-AREA.
               05  WRK-RIHABIR-OCC     OCCURS   8.
                   07  WRK-RIHA-STR        PIC X(08).
                   07  WRK-RIHA-END        PIC X(08).
      *
           03  WRK-RIHABIR-AREA2.
               05  WRK-RIHABIR-OCC2    OCCURS   8.
                   07  WRK-RIHA-STR2       PIC X(08).
           03  WRK-RIHA-YMD                PIC X(08).
           03  WRK-RIHA-GMN                PIC X(09).
           03  WRK-RIHAMEI                 PIC X(60).
           03  WRK-ZOGEN-MEI               PIC X(12).
           03  WRK-ZOGEN                   PIC S9(03).
           03  WRK-ENDYMD                  PIC X(08).
      *    外来時間外区分設定
           03  WRK-HZN-JIKANKBN            PIC 9(01).
      *H24.4
      *    外来リハビリテーション診察料
           03  WRK-LAST-DAY1                PIC X(08).
           03  WRK-LAST-DAY2                PIC X(08).
           03  WRK-LAST-DAY3                PIC X(08).
      *    最終算定日
           03  WRK-LAST-DAY.
               05  WRK-LAST-YM                  PIC X(06).
               05  WRK-LAST-DD                  PIC X(02).
           03  WRK-KIKAN-DAY               PIC X(08).
           03  WRK-KIKAN                   PIC 9(02).
           03  FLG-KIKAN                   PIC 9(01).
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
      ***  再診料（一般 診療所）
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
      *
       01  TBL-SINSATU-AREAR       REDEFINES   TBL-SINSATU-AREA.
      ***  03  TBL-SINSATU-OCC         OCCURS  15    INDEXED  TBL-IDX.
           03  TBL-SINSATU-OCC         OCCURS  20    INDEXED  TBL-IDX.
      *        １：一般、２：小児科外来診察料、３：外来診療料
               05  TBL-KAISU           PIC 9(01).
      *****    老人一般区分（０：一般、１：老人）(０：なし）
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
      *01  TBL-SIN-MAX                 PIC 9(04)   VALUE   12.
      *
      *
      *    再診加算テーブル
       01  TBL-GAIRAI-AREA.
      *    外来管理加算
           03  TBLG-SRYCD          PIC X(09)   VALUE   "112011010".
      *    地域医療貢献加算
      **** 03  TBL-CHISRYCD        PIC X(09)   VALUE   "112015670".
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
      *!!! 03  FILLER              PIC X(13)   VALUE   "0 S 101120040".
           03  FILLER              PIC X(13)   VALUE   "0   101120010".
      *    再診料（同日２科目）
      *!!! 03  FILLER              PIC X(13)   VALUE   "0 SD101120100".
           03  FILLER              PIC X(13)   VALUE   "0 KD101120040".
      *    外来管理加算
      *!!! 03  FILLER              PIC X(13)   VALUE   "0 G 101120070".
      *****03  FILLER              PIC X(13)   VALUE   "0 G 101120020".
      *H25.9 追加
      *    電話再診料
           03  FILLER              PIC X(13)   VALUE   "0 T 101120050".
      *    同日再診料
           03  FILLER              PIC X(13)   VALUE   "0 D 101120060".
      *    同日電話再診料
           03  FILLER              PIC X(13)   VALUE   "0 S 101120070".
      *    電話再診（同一日２科目）
           03  FILLER              PIC X(13)   VALUE   "0 JD101120080".
      *
       01  TBL-RSI-SINSATU-AREAR   REDEFINES   TBL-RSI-SINSATU-AREA.
           03  TBL-RSI-SINSATU-OCC     OCCURS  8    INDEXED  TBL-RSI.
      *        回数
               05  TBLR-KAISU           PIC 9(01).
               05  FILLER               PIC X(01).
      *********F:初診、S:再診、G:外来管理加算
      *        F:初診、 :再診
      *        D:同日再診、T:電話再診,S:同日電話再診,J:電話２科目
               05  TBLR-KBN             PIC X(01).
      *        D:同日初診
               05  TBLR-KBN2            PIC X(01).
      *        診療コード
               05  TBLR-SRYCD           PIC X(09).
      *H25.7
      *    労災外来管理加算等加算テーブル
       01  TBLR-GAIRAI-AREA.
      *    外来管理加算
           03  TBLRG-SRYCD         PIC X(09)   VALUE   "101120020".
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
      *    算定履歴付加
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
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
           COPY    "CPORCSC10S.INC".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSC10SAREA
           SPA-AREA
           SPA-SCR-REC
           SPA-K02
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
           EVALUATE    LNK-SC10S-SYORI
      *        初期自動発生編集
               WHEN    "1"
                   PERFORM 100-SANTEI-CHK-SEC
                   PERFORM 200-SINSATU-SET-SEC
      *        同時再診算定（自動）
               WHEN    "G"
                   PERFORM 200-SYOSIN-SET-SEC
      *        同時再診算定（自動）
               WHEN    "2"
                   PERFORM 300-DOUSAISIN-SET-SEC
      *        算定回数チェック
               WHEN    "3"
                   PERFORM 100-SANTEI-CHK-SEC
      *        院内院外変更
               WHEN    "4"
                   PERFORM 400-INGAI-HEN-SEC
      *        同時再診算定（入力分）
               WHEN    "5"
                   PERFORM 500-INDOUSAISIN-SET-SEC
      *        剤分離前の特定算定回数チェック
               WHEN    "6"
                   PERFORM 600-SANTEI-CNT-SEC
      *        剤分離後の特定算定チェック
               WHEN    "7"
                   PERFORM 700-SANTEI-CHK-SEC
      *        再診料→初診料の振替時チェック処理
               WHEN    "9"
                   PERFORM 900-SYOSIN-CHK-SEC
      *
      *        中途終了選択時チェック処理
               WHEN    "W"
                   PERFORM 1000-WKSRYACT-SYORI-SEC
      *        診察料のみの変更処理
               WHEN    "S"
                   PERFORM 1010-SINSATU-HEN-SEC
      *        診察料のみの変更処理（他保険受診あり）
               WHEN    "H"
                   PERFORM 1012-TAHOKEN-HEN-SEC
      *        診察料のみの変更処理（他保険受診ありで同日再診へ）
               WHEN    "J"
                   PERFORM 1010-SINSATU-HEN-SEC
      *        初診料を算定中かの判定
               WHEN    "C"
                   PERFORM 1011-SYOSIN-CHK-SEC
      *        診察料算定後、診察料のみの変更処理
               WHEN    "K"
                   PERFORM 1014-SINSATU-HENKAN-SEC
      *        複数料（＄）、保険組合せ（＃）対応
               WHEN    "A"
               WHEN    "B"
      *        指導料発生なし
               WHEN    "D"
                   PERFORM 1015-FUKU-SYORI-SEC
      *        初診料を再診料へ変更
               WHEN    "E"
                   PERFORM 1010-SINSATU-HEN-SEC
           END-EVALUATE
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    IDX2                TO  LNK-SC10S-IDX2
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初診料を算定中かの判定処理
      *****************************************************************
       1011-SYOSIN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO               TO  SPA-SYOSIN
      *
      *    訂正の時、初診料を算定中かの判定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
               EVALUATE    SPA-COM-INPUTCD (IDX)
                   WHEN    "111000110"
                   WHEN    "113003510"
                   WHEN    "113003710"
                       MOVE    1              TO  SPA-SYOSIN
      *                労災分
                   WHEN    "101110010"
                       MOVE    1              TO  SPA-SYOSIN
      *H25.4           紹介なし
                   WHEN    "111012510"
                       MOVE    1              TO  SPA-SYOSIN
               END-EVALUATE
           END-PERFORM
      *
           .
       1011-SYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    各診察算定回数処理
      *****************************************************************
       100-SANTEI-CHK-SEC                  SECTION.
      *
           IF      SPA-HKNKBN          =   1
      *        労災の時の処理
               PERFORM 1000-SANTEI-RSI-SEC
               GO      TO      100-SANTEI-CHK-EXT
           END-IF
      *
      *    受診履歴より診察回数を求める
           PERFORM 1000-SINSATU-KAISU-SEC
      *
      *    当月算定回数を求める
           PERFORM 1001-TOUGETU-KAISU-SEC
      *
      *    小児科外来診療科
           IF     (SPA-SYONIKA         =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
             AND  (WRK-ZAITAKU-CNT     =   ZERO)
               PERFORM 1001-SYONIKA-CHK-SEC
           ELSE
               MOVE    ZERO                TO  SPA-SYONIKA-ARI3
               MOVE    ZERO                TO  SPA-SYONIKA-ARI2
               MOVE    ZERO                TO  SPA-SYONIKA-ARI
           END-IF
      *
           .
       100-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴より診察回数を求める処理
      *****************************************************************
       1000-SINSATU-KAISU-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-SINSATU-CNT
           MOVE    ZERO                TO  FLG-15MIMAN
           MOVE    ZERO                TO  SPA-15MIMAN-ARI
      *    当日算定回数
           MOVE    ZERO                TO  WRK-DAY-SINSATUCNT
      *    当日診察料算定有無
           MOVE    ZERO                TO  SPA-DOUJITU
      *    新規の時、受診なし
           IF      SPA-SYOYMD          =   SPACE
           AND    (SPA-SYOSIN          =   1     )
               GO      TO      1000-SINSATU-KAISU-EXT
           END-IF
      *H24.4
      *    １５歳になる年月日を計算
           MOVE    SPA-BIRTHDAY        TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  15
           MOVE    SPA-BIRTHDAY        TO  WRK-BIRTHDAY03
           COMPUTE WRK-BIRTH-YY03      =   WRK-BIRTH-YY03  +  3 
      *
      *    当日に診療がある時のみ
           MOVE    ZERO                TO  FLG-DOUJITU-ARI
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   SPA-GMN-MAX1
      *        １５歳到達月に１５歳までの受診あり
               IF     (SPA-SRYYMD(1:6)     =   WRK-BIRTHDAY(1:6))
                  AND (SPA-NAI-RRKYMD(IDX2)(1:6)
                                           =   WRK-BIRTHDAY(1:6))
                  IF      (SPA-NAI-RRKYMD(IDX2)    <   WRK-BIRTHDAY)
                    AND   (FLG-15MIMAN             =   ZERO    )
                       MOVE    1               TO  SPA-15MIMAN-ARI
                       MOVE    1               TO  FLG-15MIMAN
                  END-IF
               END-IF
      *        ３歳到達月に３歳までの受診あり
               IF     (SPA-SRYYMD(1:6)     =   WRK-BIRTHDAY03(1:6))
                  AND (SPA-NAI-RRKYMD(IDX2)(1:6)
                                           =   WRK-BIRTHDAY03(1:6))
                  IF      (SPA-NAI-RRKYMD(IDX2)    <   WRK-BIRTHDAY03)
                    AND   (FLG-15MIMAN             =   ZERO    )
                       MOVE    2               TO  SPA-15MIMAN-ARI
                       MOVE    1               TO  FLG-15MIMAN
                  END-IF
               END-IF
      *        当日受診あり
               IF      SPA-NAI-RRKYMD(IDX2)    =   SPA-SRYYMD
                   MOVE    1                   TO  FLG-DOUJITU-ARI
               ELSE
                   IF      SPA-NAI-RRKYMD(IDX2)(1:6)
                                               <   SPA-SRYYMD(1:6)
      *                当月分以外は終了
                       MOVE    SPA-GMN-MAX1    TO  IDX2
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-DOUJITU-ARI     =   ZERO
               GO      TO      1000-SINSATU-KAISU-EXT
           END-IF
      *H24.4
      *!
      *
      *    受診履歴より当日の受診回数を求める
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-PTID            TO  JYURRK-PTID
      *    診療日のみ
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    SPA-SRYYMD          TO  JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL    FLG-JYURRK     =   1
               IF      JYURRK-RENNUM       =   1
                   MOVE    ZERO            TO  FLG-SYONIKA
               END-IF
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  FLG-CHK2
      *        保険により対象を決定する
               PERFORM 10000-HKNKBN-CHK-SEC
      *********MOVE    ZERO                TO  FLG-CHK
               IF     (FLG-CHK             =   ZERO  )
      *            当日の時は、全部
                  OR  (SPA-SRYYMD          =   JYURRK-SRYYMD)
      *            受診履歴に同日他科算定がないこと
                   PERFORM 10001-DOUJI-CHK-SEC
               END-IF
      *
               IF     (JYURRK-EDANUM       =   1   )  AND
                      (JYURRK-DOUJI-RENNUM =   1  OR  ZERO )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-CHK
               END-IF
      *        訂正の時、訂正分はカウントしない
               IF      SPA-SYORIFLG        =   1
                   IF      (SPA-SRYYMD         =   JYURRK-SRYYMD) AND
                           (SPA-SRYKA          =   JYURRK-SRYKA ) AND
                           (SPA-RENNUM         =   JYURRK-RENNUM) AND
                           (SPA-DOUJI-RENNUM   =   JYURRK-DOUJI-RENNUM)
                       MOVE    1               TO  FLG-CHK
                       MOVE    1               TO  FLG-CHK2
                   END-IF
      *            訂正の時、訂正分は以降カウントしない
                   IF      (SPA-SRYYMD         =   JYURRK-SRYYMD) AND
                           (SPA-SRYKA          =   JYURRK-SRYKA ) AND
                           (SPA-RENNUM         <   JYURRK-RENNUM)
                       MOVE    1               TO  FLG-CHK
                       MOVE    1               TO  FLG-CHK2
                   END-IF
               END-IF
      *
      *        受診回数
               IF      FLG-CHK         =   ZERO
                   ADD     1               TO  SPA-SINSATU-CNT
               END-IF
      *        当日算定回数
               IF     (FLG-CHK2        =   ZERO )  AND
                      (FLG-SINSATU2    =   1    )
                   ADD     1                   TO  WRK-DAY-SINSATUCNT
      *            当日診察料算定有無
                   ADD     1                   TO  SPA-DOUJITU
               END-IF
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *!!!!!
      *    初診算定日がなく、当日の受診がない時、初診料算定へ
           IF     (SPA-SYORIFLG        =   ZERO  )
           AND    (SPA-SYOYMD          =   SPACE )
           AND    (SPA-SYOSIN          =   ZERO  )
               IF     (SPA-DOUJITU     =   ZERO  )
                   MOVE    1               TO  SPA-SYOSIN
               END-IF
           END-IF
           .
       1000-SINSATU-KAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象保険決定処理
      *****************************************************************
       10000-HKNKBN-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           EVALUATE    SPA-HKNKBN
              WHEN     ZERO
                  IF      JYURRK-HKNKBN        =   ZERO  OR  SPACE
                       CONTINUE
                  ELSE
                       MOVE    1               TO  FLG-CHK
                  END-IF
              WHEN     2
      *           公害
                  IF      JYURRK-HKNKBN        =   "6"
                       CONTINUE
                  ELSE
                       MOVE    1               TO  FLG-CHK
                  END-IF
              WHEN     1
      *           自賠責
                  IF      SPA-RSI-HKNKBN       =   "4"
      *D                                       OR  "5"
                      IF      JYURRK-HKNKBN        =   "2"
                                                   OR  "4"
                           CONTINUE
                      ELSE
                           MOVE    1               TO  FLG-CHK
                      END-IF
                  ELSE
      *           労災
                      IF      JYURRK-HKNKBN        =   "1"
                                                   OR  "5"
                           CONTINUE
                      ELSE
                           MOVE    1               TO  FLG-CHK
                      END-IF
                  END-IF
           END-EVALUATE
      *
           .
       10000-HKNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴に同日他科算定チェック処理
      *****************************************************************
       10001-DOUJI-CHK-SEC                  SECTION.
      *
           IF      JYURRK-SRYKBN (01)      =   "01"
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-CHK
               GO      TO      10001-DOUJI-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-SINSATU
           MOVE    ZERO                TO  FLG-SINSATU2
           MOVE    ZERO                TO  FLG-SYOSIN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )  OR
                              (FLG-SINSATU         =   1     )  OR
                              (JYURRK-ZAINUM (IDX) =   ZERO  )
      *
               MOVE    SPACE               TO  SRYACT-REC
               INITIALIZE                  SRYACT-REC
               MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  SRY-PTID
               MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM (IDX)
                                           TO  SRY-ZAINUM
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACT-READ-SEC
                ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
      *
      *        自費は対象外とする
               IF      SRY-SRYKBN          =   "95"
                                           OR  "96"
                   MOVE    1                   TO  FLG-SRYACT
               END-IF
      *
               PERFORM
                       UNTIL  (FLG-SRYACT      =   1     )  OR
                              (FLG-SINSATU     =   1     )
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL   (IDY        >   5   )  OR
                                   (FLG-SINSATU          =   1     )  OR
                                   (SRY-SRYCD (IDY)      =   SPACE )
      **************** IF      SPA-HKNKBN          =   ZERO
                       IF      JYURRK-HKNKBN       =   ZERO
      *                    健保準拠分
                                                   OR  "4"
                                                   OR  "5"
      *                    公害
                                                   OR  "6"
      *ver.4.7             第三者行為
                                                   OR  "7"
      *                    一般
                           PERFORM 100011-IPN-DOUJI-CHK-SEC
                       ELSE
      *                    労災等
                           PERFORM 100011-RSI-DOUJI-CHK-SEC
                       END-IF
                   END-PERFORM
      *
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           END-PERFORM
      *
           IF    ((FLG-SINSATU         =   1   )  OR
                  (FLG-SYONIKA         =   1   ))  AND
                  (FLG-SYOSIN          =   ZERO )
               MOVE    ZERO            TO  FLG-CHK
           ELSE
               MOVE    1               TO  FLG-CHK
           END-IF
      *
      *    初診料算定時、回数をクリアする
           IF      FLG-SYOSIN          =   1
               MOVE    ZERO            TO  SPA-SINSATU-CNT
           END-IF
      *
           .
       10001-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般同日他科算定チェック処理
      *****************************************************************
       100011-IPN-DOUJI-CHK-SEC        SECTION.
      *
      *    診察料検索
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
               AT   END
                   MOVE    ZERO               TO  FLG-ARI
                   MOVE    SPACE              TO  WRK-KBN
                   MOVE    SPACE              TO  WRK-KBN2
               WHEN   SRY-SRYCD (IDY)     =   TBL-SRYCD (TBL-IDX)
                   MOVE    1                  TO  FLG-ARI
                   MOVE    TBL-KBN  (TBL-IDX) TO  WRK-KBN
                   MOVE    TBL-KBN2 (TBL-IDX) TO  WRK-KBN2
           END-SEARCH
      *
           IF      FLG-ARI         =   1
               MOVE    1               TO  FLG-SINSATU
      *        小児科外来診察料あり
               IF      SRY-SRYCD(IDY)(1:3)     =   "113"
                   MOVE    1           TO  FLG-SYONIKA
               END-IF
           ELSE
               EVALUATE    SRY-SRYCD (IDY)
      *            在宅患者訪問診察料
                   WHEN    "114001110"
      *            在宅患者訪問診察料（住居系施設入居者）
                   WHEN    "114012910"
      *H24.4
      *            在宅患者訪問診察料（同一建物）（特定施設入居）
                   WHEN    "114018010"
      *            在宅末期医療総合診察料
                   WHEN    "114007610"
                   WHEN    "114007710"
      *H24.4
      *            在宅がん医総合
                   WHEN    "114019510"
                   WHEN    "114019710"
                   WHEN    "114019610"
                   WHEN    "114019810"
                       MOVE    1               TO  FLG-SINSATU
      *            ＤＵＭＭＹ診察料
                   WHEN    "099110001"
                   WHEN    "099120001"
                       MOVE    1               TO  FLG-SINSATU
               END-EVALUATE
      *        以外の判定
               IF     (SRY-SRYCD (IDY)(1:1)    =   "1" )  OR
                      (FLG-SINSATU             =   ZERO )
                   PERFORM 1000112-SIDOU-CHK-SEC
               END-IF
           END-IF
      *    初診料は算定外とする
           IF     (SRY-SRYKBN          =   "11" )  OR
                  (SRY-SRYCD(IDY)      =   "113003510"  OR
                                           "113003710" )
               MOVE    1               TO  FLG-SYOSIN
           END-IF
      *
           IF      FLG-SINSATU         =   1
               MOVE    1               TO  FLG-SINSATU2
           END-IF
      *
           .
       100011-IPN-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災等同日他科算定チェック処理
      *****************************************************************
       100011-RSI-DOUJI-CHK-SEC                  SECTION.
      *
      *    診察料検索
           SET     TBL-RSI              TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    ZERO               TO  FLG-ARI
                   MOVE    ZERO               TO  WRK-KAISU
                   MOVE    SPACE              TO  WRK-KBN
               WHEN    SRY-SRYCD (IDY)    =   TBLR-SRYCD (TBL-RSI)
                   MOVE    1                  TO  FLG-ARI
                   MOVE    TBLR-KAISU (TBL-RSI)   TO  WRK-KAISU
                   MOVE    TBLR-KBN   (TBL-RSI)   TO  WRK-KBN
           END-SEARCH
      ***  IF      WRK-KBN             =   "S"  OR  "F"
      *        MOVE    1              TO  FLG-ARI
      *    ELSE
      *        MOVE    ZERO           TO  FLG-ARI
      **** END-IF
      *
           IF      FLG-ARI             =   1
               MOVE    1               TO  FLG-SINSATU
           ELSE
      *
      *        一般の診察料を検索する（電話再診等）
               SET     TBL-IDX             TO  1
               SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    ZERO           TO  FLG-ARI
                   WHEN   SRY-SRYCD (IDY)     =   TBL-SRYCD (TBL-IDX)
      **************   IF      TBL-KBN (TBL-IDX)  =   "T" OR
      *************                                   "S"
                           MOVE    1               TO  FLG-ARI
                           MOVE    1               TO  FLG-SINSATU
      ***************  ELSE
      *                    MOVE    ZERO            TO  FLG-ARI
      ***************  END-IF
               END-SEARCH
      *
               EVALUATE    SRY-SRYCD (IDY)
      *            ＤＵＭＭＹ診察料
                   WHEN    "099110001"
                   WHEN    "099120001"
                       MOVE    1               TO  FLG-SINSATU
               END-EVALUATE
      *        以外の判定
               IF     (SRY-SRYCD (IDY)(1:1)    =   "1" )  OR
                      (FLG-SINSATU             =   ZERO )
                   PERFORM 1000112-SIDOU-CHK-SEC
               END-IF
           END-IF
      *
           IF      FLG-SINSATU         =   1
               MOVE    1               TO  FLG-SINSATU2
           END-IF
      *
      *    初診料は算定外とする
           IF     (SRY-SRYKBN          =   "11" )  AND
                  (FLG-SINSATU         =   1    )
               MOVE    1               TO  FLG-SYOSIN
           END-IF
      *
      *    発症日から３月以内は対象外とする
           IF      JYURRK-SRYYMD       <   WRK-HASSYOYMD
               MOVE    1               TO  FLG-CHK
               MOVE    ZERO            TO  FLG-SINSATU
           END-IF
           .
       100011-RSI-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料包括の判定処理
      *****************************************************************
       1000112-SIDOU-CHK-SEC          SECTION.
      *
           MOVE    SRY-SRYCD (IDY)     TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
      *    実日数、日数・回数判定
               IF     (TNS-JITUDAY         =   2  )  AND
                      (TNS-DAYCNT          =   2  )
                   MOVE    1               TO  FLG-SINSATU
               END-IF
           END-IF
      *
           .
       1000112-SIDOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    当月算定回数を求める処理
      *****************************************************************
       1001-TOUGETU-KAISU-SEC          SECTION.
      *
      *    小児科外来診察料回数チェック
           MOVE    ZERO                TO  WRK-NYUYOJI
           MOVE    ZERO                TO  WRK-DAYCNT-SUM
      *    在宅療養指導料算定
           MOVE    ZERO                TO  WRK-ZAITAKU-CNT
      *    寝たきり老人在宅総合診療科
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI2
      *    在宅末期医療総合診察料
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI2
      *    生活習慣病
           MOVE    ZERO                TO  SPA-SEIKATU-ARI
           MOVE    ZERO                TO  SPA-SEIKATU-ARI2
           MOVE    ZERO                TO  SPA-SEIKATU-CHK
      *    慢性疼痛疾病管理料
           MOVE    ZERO                TO  SPA-TOKUTEI-KNJ2
      *    痴呆患者在宅指導管理料（外来加算付加算定付加）
           MOVE    ZERO                TO  SPA-GAIRAI-NASI
      *    消炎鎮痛等処置
           MOVE    ZERO                TO  SPA-SYOEN-CNT
           MOVE    ZERO                TO  SPA-TOKUTEI-CNT
      *    在宅時医学総合管理料
           MOVE    ZERO                TO  SPA-ZAIKANRI-ARI
           MOVE    ZERO                TO  SPA-ZAIKANRI-ARI2
      *
      *    後期高齢者診療料
      *H24.4 から外来リハビリテーション診察料に流用
      *    １：外来リハビリテーション診察料１
      *    ２：外来リハビリテーション診察料２
      *    ３：外来放射線照射診察料　　（併用算定できないことが前提）
           MOVE    ZERO                TO  SPA-KOURESRY-ARI
           MOVE    ZERO                TO  SPA-KOURESRY-ARI2
           MOVE    SPACE               TO  WRK-LAST-DAY1
           MOVE    SPACE               TO  WRK-LAST-DAY2
           MOVE    SPACE               TO  WRK-LAST-DAY3
      *
           MOVE    ZERO                TO  SPA-IGAKUKAN-ARI
           MOVE    ZERO                TO  SPA-IGAKUKAN-ARI2
      *
      *    包括まとめ入力対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
      *       治験
              OR  (SPA-CHIKENKBN       =   "1"  )
               GO      TO      1001-TOUGETU-KAISU-EXT
           END-IF
      *    訂正で小児科外来診察料を算定済みの場合
           MOVE    ZERO                TO  FLG-SYONIKA-TEI
           IF      SPA-SYORIFLG        =   1
               PERFORM VARYING    IDX-T    FROM    1   BY  1
                       UNTIL     (IDX-T    >   SPA-MAX-LINE )  OR
                                 (SPA-K02-TEI-SRYCD(IDX-T) =   SPACE)
                   EVALUATE    SPA-K02-TEI-SRYCD(IDX-T)
      *            小児科外来診療科
                       WHEN    "113003510"
                       WHEN    "113003610"
                       WHEN    "113003710"
                       WHEN    "113003810"
                           MOVE    1           TO  FLG-SYONIKA-TEI
                           MOVE    SPA-MAX-LINE    TO  IDX-T
                   END-EVALUATE
               END-PERFORM
           END-IF
      *
      *    急性発生日決定
           PERFORM 10019-KYUSEI-CHK-SEC
           INITIALIZE                  WRK-RIHA-KAN-G
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF      SANTEI-MSANTEICNT   >   ZERO
      *            当月算定分
                   PERFORM 10011-TOUGETU-CHK-SEC
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H24.4 改正
      *    外来リハビリ診察料等算定判定
           PERFORM 10019-GAIRAIRIHA-CHK-SEC
           .
       1001-TOUGETU-KAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    当月算定分処理
      *****************************************************************
       10011-TOUGETU-CHK-SEC                  SECTION.
      *
      *    保険毎のコードの判定
           MOVE    SANTEI-SRYCD        TO  WRK-SRYCD
           PERFORM 1000-SANTEI-CHK-SEC
           IF      FLG-HKNSAN          =   1
      *        一般の保険で今回が労災の時対象外
               IF      (SANTEI-HKNCOMBINUM     =   ZERO )  AND
                       (SPA-HKNKBN             =   1
                                               OR  2     )
                   GO      TO      10011-TOUGETU-CHK-EXT
               END-IF
      *        労災の保険で今回が一般
               IF      (SANTEI-HKNCOMBINUM NOT =   ZERO )  AND
                       (SPA-HKNKBN             =   ZERO )
                   GO      TO      10011-TOUGETU-CHK-EXT
               END-IF
      *        労災の保険で今回が労災の時判定
               IF      (SANTEI-HKNCOMBINUM NOT =   ZERO )  AND
                       (SPA-HKNKBN             =   1
                                               OR  2    )  AND
                       (SANTEI-HKNCOMBINUM NOT =   SPA-HKNCOMBINUM)
                   GO      TO      10011-TOUGETU-CHK-EXT
               END-IF
           END-IF
      *!!!!
      *    アフターケアは対象外
           IF      SPA-RSI-HKNKBN      =   "3"
               GO      TO      10011-TOUGETU-CHK-EXT
           END-IF
      *!!!!
      *
      *    上限１日
           MOVE    ZERO                TO  WRK-DAYCNT
           IF     (SPA-SRYYMD(7:2)     NUMERIC   )  AND
                  (SPA-SRYYMD(7:2)     >   ZERO  )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               MOVE    SANTEI-DAY (IDX-Y2) TO  WRK-DAYCNT
           END-IF
      *
      *    訂正の時、訂正分はカウントしない
           IF      SPA-SYORIFLG        =   1
      *        訂正の時、算定していた診療コードかのチェック
               MOVE    ZERO                TO  WRK-TEI-KAISU
               PERFORM VARYING     IDX-T   FROM    1   BY  1
                       UNTIL      (IDX-T       >   SPA-MAX-LINE  )  OR
                                  (SPA-K02-TEI-SRYCD(IDX-T)  =   SPACE)
                   IF      SPA-K02-TEI-SRYCD(IDX-T)    =   SANTEI-SRYCD
                       MOVE    SPA-K02-TEI-KAISU (IDX-T)
                                                   TO  WRK-TEI-KAISU
                       MOVE    SPA-MAX-LINE            TO  IDX-T
                   END-IF
               END-PERFORM
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               IF     (SANTEI-DAY (IDX-Y2) >   ZERO )  AND
                      (SANTEI-MSANTEICNT   >   ZERO )
                   IF      WRK-TEI-KAISU   =   ZERO
                       CONTINUE
                   ELSE
                       COMPUTE SANTEI-MSANTEICNT
                                           =   SANTEI-MSANTEICNT
                                           -   WRK-TEI-KAISU
                       MOVE    ZERO                TO  WRK-DAYCNT
                   END-IF
               END-IF
           END-IF
      *
      *    消炎鎮痛処置の時、訂正日までの回数とする
           MOVE    ZERO                TO  WRK-SYOEN-CNT
      *    急性発生日以前は対象外
           IF     (SPA-KYUSEI-YMD  NOT =   SPACE )  AND
                  (SPA-KYUSEI-YMD(1:6) =   SPA-SRYYMD(1:6))
               MOVE    SPA-KYUSEI-YMD(7:2)     TO  IDX-STR
           ELSE
               MOVE    1                       TO  IDX-STR
           END-IF
      *
           IF      SPA-SYORIFLG        =   1
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           ELSE
               MOVE    32                  TO  IDX-Y2
           END-IF
           PERFORM VARYING     IDX-Y3  FROM  IDX-STR  BY  1
                   UNTIL       IDX-Y3  >=    IDX-Y2
               ADD     SANTEI-DAY (IDX-Y3)
                                           TO  WRK-SYOEN-CNT
           END-PERFORM
      *
      *H24.4 改正
      *    直近の算定日（外来リハビリ診察料）
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           IF      SPA-SYORIFLG        =   1
      *        訂正は前日まで
               COMPUTE IDX-Y2          =   IDX-Y2   -  1
           END-IF
           MOVE    SPACE               TO  WRK-LAST-DAY
           PERFORM VARYING     IDX-Y3  FROM  1   BY  1
                   UNTIL       IDX-Y3  >     IDX-Y2
               IF      SANTEI-DAY (IDX-Y3) >   ZERO
                   MOVE    IDX-Y3          TO  WRK-RDD
                   MOVE    WRK-RDD         TO  WRK-LAST-DD
                   MOVE    SANTEI-SRYYM    TO  WRK-LAST-YM
               END-IF
           END-PERFORM
      *!
      *
           EVALUATE    SANTEI-SRYCD
      *        小児科外来診療科
               WHEN    "113003510"
               WHEN    "113003610"
               WHEN    "113003710"
               WHEN    "113003810"
                   ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
                   ADD     WRK-DAYCNT          TO  WRK-DAYCNT-SUM
      *@Ｈ１８．４から廃止
      *        寝たきり老人在宅総合診療科（院外）
      *        WHEN    "114701210"
      *            IF      SANTEI-MSANTEICNT   >   ZERO
      *                MOVE    1                   TO  SPA-NETAKIRI-ARI2
      *            END-IF
      *        寝たきり老人在宅総合診療科（院内）
      *        WHEN    "114701710"
      *            IF      SANTEI-MSANTEICNT   >   ZERO
      *                MOVE    2                   TO  SPA-NETAKIRI-ARI2
      ***********  END-IF
      *
      *        在宅がん医総合（在宅末期医療総合診察料）（院外処方）
               WHEN    "114007610"
      *H24.4
               WHEN    "114019510"
               WHEN    "114019710"
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    1                   TO  SPA-ZAIMATU-ARI2
                   END-IF
      *        在宅がん医総合（在宅末期医療総合診察料）（院内処方）
               WHEN    "114007710"
      *H24.4
               WHEN    "114019610"
               WHEN    "114019810"
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    2                   TO  SPA-ZAIMATU-ARI2
                   END-IF
      *
      *        在宅時医学総合管理料算定（院外処方）
               WHEN    "114012210"
               WHEN    "114007510"
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    1                   TO  SPA-ZAIKANRI-ARI2
                   END-IF
      *        在宅時医学総合管理料算定（院内処方）
               WHEN    "114012310"
               WHEN    "114012410"
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    2                   TO  SPA-ZAIKANRI-ARI2
                   END-IF
      *H24.4
      *        在医総管（機能強化した在支診等）（処方せん）
               WHEN    "114018710"
               WHEN    "114018910"
               WHEN    "114019110"
               WHEN    "114019310"
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    1                   TO  SPA-ZAIKANRI-ARI2
                   END-IF
      *        在医総管（機能強化した在支診等）（処方せんなし）
               WHEN    "114018810"
               WHEN    "114019010"
               WHEN    "114019210"
               WHEN    "114019410"
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    2                   TO  SPA-ZAIKANRI-ARI2
                   END-IF
      *H24.4
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
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    1                   TO  SPA-SEIKATU-ARI
                       MOVE    1                   TO  SPA-SEIKATU-ARI2
                   END-IF
      ****         老人の時、生活習慣病の算定判定をしない
      *            IF      SPA-NAI-ROUJIN      =   1
      *                MOVE    ZERO                TO  SPA-SEIKATU-ARI
      *                MOVE    ZERO                TO  SPA-SEIKATU-ARI2
      *                MOVE    ZERO                TO  SPA-SEIKATU-FLG
      *                MOVE    1                   TO  SPA-SEIKATU-CHK
      ****         END-IF
      *        痴呆患者在宅指導管理料
               WHEN    "113700310"
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    1               TO  SPA-GAIRAI-NASI
                   END-IF
      *TEST
      *        慢性疼痛疾病管理料
               WHEN    "113006510"
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    1               TO  SPA-TOKUTEI-KNJ2
                   END-IF
      *            初回算定日より前の算定はなしとする
                   IF    (SANTEI-FSANTEIYMD(1:6)
                                               =   SPA-SRYYMD(1:6))
                     AND (SANTEI-FSANTEIYMD    >   SPA-SRYYMD )
                       MOVE    ZERO            TO  SPA-TOKUTEI-KNJ2
                   END-IF
      *Ｈ２２．４から廃止
      ****     後期高齢者診療料
      *        WHEN    "113702110"
      *            IF      SANTEI-MSANTEICNT   >   ZERO
      *                MOVE    1                   TO  SPA-KOURESRY-ARI2
      ******       END-IF
      *        特定施設入居時医学総合管理料
               WHEN    "114013010"
               WHEN    "114013210"
      *            院外
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    1                   TO  SPA-IGAKUKAN-ARI2
                   END-IF
               WHEN    "114013110"
               WHEN    "114013310"
      *                院内
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       MOVE    2                   TO  SPA-IGAKUKAN-ARI2
                   END-IF
      *Ｈ２０．４から廃止
      *        器具等による療法
               WHEN    "140040310"
      *        湿布処置１
               WHEN    "140002210"
      *        湿布処置２
               WHEN    "140002310"
                   ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
      *        介達牽引
               WHEN    "140048010"
                   ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
      *H18/04から
      *        介達牽引（矯正固定）
               WHEN    "140030350"
                   ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
      *        介達牽引（変形機械矯正術）
               WHEN    "140048250"
                   ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
      *        腰部固定帯固定
               WHEN    "140048350"
                   ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
      *        胸部固定帯固定
               WHEN    "140048450"
                   ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
      *        低出力レーザ照射
               WHEN    "140048550"
                   ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
      *        肛門処置
               WHEN    "140002450"
                   ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
      *まで
      *!!
      *        特定疾患療養指導料
               WHEN    "113001910"
               WHEN    "113002010"
               WHEN    "113001810"
                   ADD     WRK-SYOEN-CNT       TO  SPA-TOKUTEI-CNT
      *!!
      *H24.4 改正
      *        外来リハビリテーション診察料
               WHEN    "113013910"
                   MOVE    WRK-LAST-DAY        TO  WRK-LAST-DAY1
               WHEN    "113014010"
                   MOVE    WRK-LAST-DAY        TO  WRK-LAST-DAY2
      *        外来放射線照射診察料
               WHEN    "113014110"
                   MOVE    WRK-LAST-DAY        TO  WRK-LAST-DAY3
      *!!
      *        在宅療養指導料の判定
               WHEN    OTHER
                   IF      SANTEI-SRYCD(1:3)   =   "114"
                       MOVE    SANTEI-SRYCD        TO  TNS-SRYCD
                       PERFORM 9101-TENSU-READ2-SEC
                       IF      (FLG-TENSU          =   ZERO ) AND
                               (TNS-CDKBN-KBN      =   "C"  ) AND
                               (TNS-CDKBN-KBNNUM   >=  "100" AND
                                                   <=  "112")
                           ADD     SANTEI-MSANTEICNT
                                               TO  WRK-ZAITAKU-CNT
                       END-IF
                   END-IF
           END-EVALUATE
           .
       10011-TOUGETU-CHK-EXT.
           EXIT.
      *
      *!!
      *H24.4 改正
      *****************************************************************
      *    外来リハビリ診察料等算定判定処理
      *****************************************************************
       10019-GAIRAIRIHA-CHK-SEC            SECTION.
      *
           IF     (WRK-LAST-DAY1       =   SPACE)  AND
                  (WRK-LAST-DAY2       =   SPACE)  AND
                  (WRK-LAST-DAY3       =   SPACE)
      *        前月履歴の算定
               IF      SPA-SRYYMD(1:6)     >   "201204"
                   PERFORM 100191-GAIRAIRIHA-ZEN-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-KIKAN
           MOVE    ZERO                TO  WRK-KIKAN
           MOVE    SPACE               TO  WRK-KIKAN-DAY
           MOVE    WRK-LAST-DAY1       TO  WRK-KIKAN-DAY
           MOVE    7                   TO  WRK-KIKAN
      *
           IF      WRK-KIKAN-DAY       <   WRK-LAST-DAY2
      *        外来リハビリ診察料２が最終
               MOVE    WRK-LAST-DAY2       TO  WRK-KIKAN-DAY
               MOVE    14                  TO  WRK-KIKAN
           END-IF
           IF      WRK-KIKAN-DAY       <   WRK-LAST-DAY3
      *        外来放射線照射診察料が最終
               MOVE    WRK-LAST-DAY3       TO  WRK-KIKAN-DAY
               MOVE    7                   TO  WRK-KIKAN
               MOVE    1                   TO  FLG-KIKAN
           END-IF
      *
           IF      WRK-KIKAN-DAY   NOT =   SPACE
      *        診察料包括日計算
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-KIKAN-DAY       TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    WRK-KIKAN           TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      *TEST
                 DISPLAY "SC10 LNK-DAY6-KEISAN:"  LNK-DAY6-KEISAN
      *TEST
               IF      LNK-DAY6-KEISAN     >   SPA-SRYYMD
      *            外来リハ診察料等算定中（後期高齢者診療料の区分に）
                   MOVE    2                   TO  SPA-KOURESRY-ARI2
                   IF      FLG-KIKAN           =   1
      *                外来放射線照射診察料算定
                       MOVE    3                   TO  SPA-KOURESRY-ARI2
                   END-IF
               END-IF
           END-IF
           .
       100112-RIHABI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    疾患別リハビリ医学管理料判定処理
      *****************************************************************
       100191-GAIRAIRIHA-ZEN-SEC            SECTION.
      *
      *    前月分検索
           MOVE    SPA-SRYYMD          TO  WRK-YMD
           COMPUTE WRK-MM              =   WRK-MM   -  1
           IF      WRK-MM              =   ZERO
               MOVE    12                  TO  WRK-MM
               COMPUTE WRK-YY              =   WRK-YY  -   1
           END-IF
      *    外来リハビリテーション診察料
           MOVE    "113013910"         TO  WRK-SRYCD
           PERFORM 1001912-SANTEI-KEN-SEC
           IF      WRK-LAST-DAY    NOT =   SPACE
               MOVE    WRK-LAST-DAY        TO  WRK-LAST-DAY1
           END-IF
      *
           MOVE    "113014010"         TO  WRK-SRYCD
           PERFORM 1001912-SANTEI-KEN-SEC
           IF      WRK-LAST-DAY    NOT =   SPACE
               MOVE    WRK-LAST-DAY        TO  WRK-LAST-DAY2
           END-IF
      *    外来放射線照射診察料
           MOVE    "113014110"         TO  WRK-SRYCD
           PERFORM 1001912-SANTEI-KEN-SEC
           IF      WRK-LAST-DAY    NOT =   SPACE
               MOVE    WRK-LAST-DAY        TO  WRK-LAST-DAY3
           END-IF
      *
           .
       100191-GAIRAIRIHA-ZEN-EXT.
           EXIT.
      *****************************************************************
      *    外来リハビリ前月分処理
      *****************************************************************
       1001912-SANTEI-KEN-SEC            SECTION.
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    WRK-YYMM            TO  SANTEI-SRYYM
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    直近の算定日
           MOVE    SPACE               TO  WRK-LAST-DAY
           PERFORM VARYING     IDX-Y3  FROM  31  BY  -1
                   UNTIL      (IDX-Y3  <     1   )
                          OR  (FLG-SANTEI    =   1   )
               IF      SANTEI-DAY (IDX-Y3) >   ZERO
                   MOVE    IDX-Y3          TO  WRK-RDD
                   MOVE    WRK-RDD         TO  WRK-LAST-DD
                   MOVE    SANTEI-SRYYM    TO  WRK-LAST-YM
                   MOVE    1               TO  IDX-Y3
               END-IF
           END-PERFORM
           .
       1001912-SANTEI-KEN-EXT.
           EXIT.
      *H24.4 改正
      *!!
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
      *!!!!
      *    各リハビリ発症日
           PERFORM 100193-RIHABIRI-YMDHEN-SEC
      *!!!!
      *    急性発生日
      *    MOVE    "099800101"         TO  WRK-SRYCD
      *    PERFORM 100191-KYUSEI-SANTEI-SEC
      *    MOVE    WRK-YMD             TO  SPA-KYUSEI-YMD
      *
      *    急性発生終了日
      *    MOVE    "099800102"         TO  WRK-SRYCD
      *    PERFORM 100191-KYUSEI-SANTEI-SEC
      *    訂正の時当日なら、対象外
           IF      SPA-SYORIFLG        =   1
               IF      SPA-KYUSEI-END      =   SPA-SRYYMD
                   MOVE    SPACE               TO  SPA-KYUSEI-END
               END-IF
           END-IF
           IF     (SPA-KYUSEI-YMD          =   SPACE         )  OR
                  (SPA-KYUSEI-END          <=  SPA-KYUSEI-YMD)
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
           MOVE    ZERO                TO  WRK-YMD
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
                       MOVE    SANTEI-SRYYM    TO  WRK-YYMM
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
           IF      WRK-YYMM            =   ZERO
               MOVE    ZERO            TO  WRK-YMD
           END-IF
      *
           .
       100191-KYUSEI-SANTEI-EXT.
           EXIT.
      *!!!!
      *
      *****************************************************************
      *    各リハビリ発症日編集処理
      *****************************************************************
       100193-RIHABIRI-YMDHEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-GMN-RIHAYMD-TBL
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    "099800%"           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
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
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key16"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           INITIALIZE                  WRK-RIHABIR-AREA
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *        最後の発生日、当日以降は対象外
               MOVE    ZERO                TO  WRK-YMD
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
                       PERFORM 1001932-RIHA-SANTEIHEN-SEC
                       IF      FLG-CHK-END     =   1
                           MOVE    1               TO  IDX-Y2
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key16"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  IDX-R
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   8   )
                         OR   (IDX-R   >=  7   )
      *
      *        訂正で終了日が当日なら、対象外
               IF      SPA-SYORIFLG        =   1
                   IF      WRK-RIHA-END (IDX)   =   SPA-SRYYMD
                       MOVE    SPACE            TO  WRK-RIHA-END (IDX) 
                   END-IF
               END-IF
      *
               IF      WRK-RIHA-STR (IDX)  NOT =   SPACE
                   IF     (WRK-RIHA-END (IDX)  =   SPACE)
                   OR     (WRK-RIHA-STR (IDX)  >   WRK-RIHA-END(IDX))
                       MOVE    WRK-RIHA-STR (IDX)
                                               TO  WRK-RIHA-STR2(IDX)
      *                リハビリ発症日編集
                       PERFORM 1001931-RIHA-HENKAN-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       100193-RIHABIRI-YMDHEN-EXT.
           EXIT.
      *****************************************************************
      *    リハビリ発症日編集処理
      *****************************************************************
       1001932-RIHA-SANTEIHEN-SEC                  SECTION.
      *
           MOVE    SANTEI-SRYYM        TO  WRK-YYMM
           MOVE    1                   TO  FLG-CHK-END
           EVALUATE    SANTEI-SRYCD
      *        脳血管疾患急性発生日
               WHEN    "099800101"
                   IF      SPA-KYUSEI-YMD          =   SPACE
                       MOVE    WRK-YMD             TO  SPA-KYUSEI-YMD
                   END-IF
      *        脳血管疾患急性発生終了日
               WHEN    "099800102"
                   IF      SPA-KYUSEI-END          =   SPACE
                       MOVE    WRK-YMD             TO  SPA-KYUSEI-END
                   END-IF
      *        心大血管疾患リハビリテーション
               WHEN    "099800111"
                   IF      WRK-RIHA-STR(1)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-STR(1)
      *                終了日検索
                       PERFORM 1001931-SANTEIPLUS-CHK-SEC
                       IF      WRK-ENDYMD      NOT =   SPACE
                           MOVE    SPACE           TO  WRK-RIHA-STR(1)
                           MOVE    ZERO            TO  FLG-CHK-END
                       END-IF
                   END-IF
               WHEN    "099800112"
                   IF      WRK-RIHA-END(1)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-END(1)
                   END-IF
      *        脳血管疾患リハビリテーション
               WHEN    "099800121"
                   IF      WRK-RIHA-STR(2)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-STR(2)
      *                終了日検索
                       PERFORM 1001931-SANTEIPLUS-CHK-SEC
                       IF      WRK-ENDYMD      NOT =   SPACE
                           MOVE    SPACE           TO  WRK-RIHA-STR(2)
                           MOVE    ZERO            TO  FLG-CHK-END
                       END-IF
                   END-IF
               WHEN    "099800122"
                   IF      WRK-RIHA-END(2)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-END(2)
                   END-IF
      *        運動器リハビリテーション
               WHEN    "099800131"
                   IF      WRK-RIHA-STR(3)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-STR(3)
      *                終了日検索
                       PERFORM 1001931-SANTEIPLUS-CHK-SEC
                       IF      WRK-ENDYMD      NOT =   SPACE
                           MOVE    SPACE           TO  WRK-RIHA-STR(3)
                           MOVE    ZERO            TO  FLG-CHK-END
                       END-IF
                   END-IF
               WHEN    "099800132"
                   IF      WRK-RIHA-END(3)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-END(3)
                   END-IF
      *        呼吸器リハビリテーション
               WHEN    "099800141"
                   IF      WRK-RIHA-STR(4)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-STR(4)
      *                終了日検索
                       PERFORM 1001931-SANTEIPLUS-CHK-SEC
                       IF      WRK-ENDYMD      NOT =   SPACE
                           MOVE    SPACE           TO  WRK-RIHA-STR(4)
                           MOVE    ZERO            TO  FLG-CHK-END
                       END-IF
                   END-IF
               WHEN    "099800142"
                   IF      WRK-RIHA-END(4)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-END(4)
                   END-IF
      *        摂食機能療法終了日
               WHEN    "099800151"
                   IF      WRK-RIHA-STR(5)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-STR(5)
      *                終了日検索
                       PERFORM 1001931-SANTEIPLUS-CHK-SEC
                       IF      WRK-ENDYMD      NOT =   SPACE
                           MOVE    SPACE           TO  WRK-RIHA-STR(5)
                           MOVE    ZERO            TO  FLG-CHK-END
                       END-IF
                   END-IF
               WHEN    "099800152"
                   IF      WRK-RIHA-END(5)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-END(5)
                   END-IF
      *        難病患者リハビリ
               WHEN    "099800161"
                   IF      WRK-RIHA-STR(6)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-STR(6)
      *                終了日検索
                       PERFORM 1001931-SANTEIPLUS-CHK-SEC
                       IF      WRK-ENDYMD      NOT =   SPACE
                           MOVE    SPACE           TO  WRK-RIHA-STR(6)
                           MOVE    ZERO            TO  FLG-CHK-END
                       END-IF
                   END-IF
               WHEN    "099800162"
                   IF      WRK-RIHA-END(6)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-END(6)
                   END-IF
      *        障害者リハビリ
               WHEN    "099800171"
                   IF      WRK-RIHA-STR(7)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-STR(7)
      *                終了日検索
                       PERFORM 1001931-SANTEIPLUS-CHK-SEC
                       IF      WRK-ENDYMD      NOT =   SPACE
                           MOVE    SPACE           TO  WRK-RIHA-STR(7)
                           MOVE    ZERO            TO  FLG-CHK-END
                       END-IF
                   END-IF
               WHEN    "099800172"
                   IF      WRK-RIHA-END(7)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-END(7)
                   END-IF
      *        がん患者リハビリ
               WHEN    "099800181"
                   IF      WRK-RIHA-STR(8)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-STR(8)
      *                終了日検索
                       PERFORM 1001931-SANTEIPLUS-CHK-SEC
                       IF      WRK-ENDYMD      NOT =   SPACE
                           MOVE    SPACE           TO  WRK-RIHA-STR(8)
                           MOVE    ZERO            TO  FLG-CHK-END
                       END-IF
                   END-IF
               WHEN    "099800182"
                   IF      WRK-RIHA-END(8)         =   SPACE
                       MOVE    WRK-YMD             TO  WRK-RIHA-END(8)
                   END-IF
           END-EVALUATE
           .
       1001932-RIHA-SANTEIHEN-EXT.
           EXIT.
      *****************************************************************
      *    リハビリ発症日編集処理
      *****************************************************************
       1001931-RIHA-HENKAN-SEC                  SECTION.
      *
           MOVE    ZERO                TO  WRK-ZOGEN
           EVALUATE    IDX
               WHEN     1
                   MOVE    150                     TO  WRK-ZOGEN
                   MOVE    "心大血管疾患リハ開始日："  TO  WRK-RIHAMEI
                   MOVE    "１５０日超："              TO  WRK-ZOGEN-MEI
               WHEN    2
                   MOVE    180                     TO  WRK-ZOGEN
                   MOVE    "脳血管疾患リハ開始日："    TO  WRK-RIHAMEI
                   MOVE    "１８０日超："              TO  WRK-ZOGEN-MEI
               WHEN    3
                   MOVE    150                     TO  WRK-ZOGEN
                   MOVE    "運動器リハ開始日："    TO  WRK-RIHAMEI
                   MOVE    "１５０日超："          TO  WRK-ZOGEN-MEI
               WHEN    4
                   MOVE    90                      TO  WRK-ZOGEN
                   MOVE    "呼吸器リハ開始日："    TO  WRK-RIHAMEI
                   MOVE    "９０日超："            TO  WRK-ZOGEN-MEI
               WHEN    5
                   MOVE    0                       TO  WRK-ZOGEN
                   MOVE    "摂食機能療法開始日："  TO  WRK-RIHAMEI
               WHEN    6
                   MOVE    0                       TO  WRK-ZOGEN
                   MOVE    "難病患者リハ開始日："  TO  WRK-RIHAMEI
               WHEN    7
                   MOVE    0                       TO  WRK-ZOGEN
                   MOVE    "障害児（者）リハ開始日："  TO  WRK-RIHAMEI
               WHEN    8
                   MOVE    0                       TO  WRK-ZOGEN
                   MOVE    "がん患者リハ開始日："  TO  WRK-RIHAMEI
           END-EVALUATE
      *    Ｎ日後計算
           IF      WRK-ZOGEN           >   ZERO
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-RIHA-STR2(IDX)  TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-RIHA-YMD
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-RIHA-YMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-RIHA-GMN
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-RIHA-STR2(IDX)  TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
      *
           ADD     1                   TO  IDX-R
           IF      WRK-ZOGEN           =   ZERO
               STRING  WRK-RIHAMEI         DELIMITED  BY  SPACE
                       LNK-DAY2-EDTYMD1    DELIMITED  BY  SIZE
                   INTO                SPA-GMN-RIHAYMD (IDX-R)
               END-STRING
           ELSE
               STRING  WRK-RIHAMEI         DELIMITED  BY  SPACE
                       LNK-DAY2-EDTYMD1    DELIMITED  BY  SIZE
                       "　"                DELIMITED  BY  SIZE 
                       WRK-ZOGEN-MEI       DELIMITED  BY  SPACE
                       WRK-RIHA-GMN        DELIMITED  BY  SIZE
                   INTO                SPA-GMN-RIHAYMD (IDX-R)
               END-STRING
           END-IF
      *
           .
       1001931-RIHA-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴付加情報編集処理
      *****************************************************************
       1001931-SANTEIPLUS-CHK-SEC                  SECTION.
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SANTEI-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    SANTEI-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYCD        TO  SANTEIPLUS-SRYCD
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    SANTEI-SRYKA        TO  SANTEIPLUS-SRYKA
           MOVE    SANTEI-NYUGAIKBN    TO  SANTEIPLUS-NYUGAIKBN
           MOVE    SANTEI-HKNCOMBINUM  TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    WRK-DD              TO  SANTEIPLUS-DAYKEY
           MOVE    ZERO                TO  SANTEIPLUS-RENNUM
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    SPACE               TO  WRK-ENDYMD
           IF      FLG-SANTEIPLUS      =   ZERO
               IF      SANTEIPLUS-ENDYMD   <   SPA-SRYYMD
      *            システム日付より前の終了
                   MOVE    SANTEIPLUS-ENDYMD   TO  WRK-ENDYMD
               END-IF
           END-IF
      *
           .
       1001931-SANTEIPLUS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診療科回数処理
      *****************************************************************
       1001-SYONIKA-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-SYONIKA-ARI
           MOVE    ZERO                TO  SPA-SYONIKA-ARI3
      *
      *    包括まとめ入力対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      1001-SYONIKA-CHK-EXT
           END-IF
      *    治験対象外
           IF      SPA-CHIKENKBN       =   "1"
               GO      TO      1001-SYONIKA-CHK-EXT
           END-IF
      *
      *    ３歳になる年月日を計算
           MOVE    SPA-BIRTHDAY        TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  3
      *    3歳未満に適用
           IF      WRK-BIRTHDAY(1:6)   <   SPA-SRYYMD (1:6)
               GO      TO      1001-SYONIKA-CHK-EXT
           END-IF
      *
      *    ３歳の当月時、当月算定がなければ対象外
           IF     (SPA-NENREI-YY   =   3               )  AND
                  (WRK-BIRTHDAY(1:6)   =   SPA-SRYYMD (1:6))
               IF      WRK-NYUYOJI         =   ZERO
                   GO      TO      1001-SYONIKA-CHK-EXT
               END-IF
           END-IF
      *
      *    当日算定があればフラグを立てる
           IF      WRK-DAYCNT-SUM      >   ZERO
               MOVE    1                   TO  SPA-SYONIKA-ARI2
           ELSE
               MOVE    ZERO                TO  SPA-SYONIKA-ARI2
           END-IF
      *    小児科対象
           MOVE    1                   TO  FLG-SYONIKA-ARI
           MOVE    1                   TO  SPA-SYONIKA-ARI3
      *
           .
       1001-SYONIKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時の処理
      *****************************************************************
       1000-SANTEI-RSI-SEC              SECTION.
      *
      *    健保準拠
      *D   IF      SPA-RSI-HKNKBN      =   "5"
           IF      SPA-RSI-JUNKYO      =   "1"
               MOVE    SPA-RSI-SHOBYOYMD   TO  SPA-RSI-HASSYOYMD
           ELSE
      *
      *    発生日を求める　(使用なし　労災コード変更対応なし）
      *    救急医療管理加算のチェック
      *    MOVE    SPACE               TO  WRK-HASSYOYMD
      *    MOVE    "101110030"         TO  WRK-SRYCD
      *    PERFORM 10001-RSI-JYURRK-CHK-SEC
      *    IF      WRK-HASSYOYMD       =   SPACE
               MOVE    SPA-RSI-SHOBYOYMD   TO  SPA-RSI-HASSYOYMD
      *    ELSE
      *        MOVE    WRK-HASSYOYMD       TO  SPA-RSI-HASSYOYMD
      *    END-IF
      *
           END-IF
      *
      *    発生日から３ヶ月の計算（現在使用なし）
           MOVE    SPA-RSI-HASSYOYMD   TO  WRK-HASSYOYMD
           COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                       +   3
           IF      WRK-HAS-MM          >   12
               COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
               COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
           END-IF
           MOVE    WRK-HASSYOYMD       TO  SPA-RSI-HASSYOYMD3
      *
      *    傷病日から６ヶ月の計算
           MOVE    SPA-RSI-SHOBYOYMD   TO  WRK-HASSYOYMD
           COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                       +   6
           IF      WRK-HAS-MM          >   12
               COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
               COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
           END-IF
           MOVE    WRK-HASSYOYMD       TO  SPA-RSI-SHOBYOYMD3
      *
      *    急性発生日が傷病日より後の場合、急性発生日から６ヶ月後とする
           IF     (SPA-HKNKBN          =   1        )  AND
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
      *    受診履歴より診察回数を求める
           PERFORM 1000-SINSATU-KAISU-SEC
      *
      *    当月算定回数を求める
           PERFORM 1001-TOUGETU-KAISU-SEC
      *
      *    慢性疼痛疾病管理料算定チェック
      ***  PERFORM 1004-TOUTUU-CHK-SEC
      *
           MOVE    ZERO                TO  SPA-SYONIKA-ARI3
           MOVE    ZERO                TO  SPA-SYONIKA-ARI2
           MOVE    ZERO                TO  SPA-SYONIKA-ARI
      *
           .
       1000-SANTEI-RSI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RIHA-KBN
                   MOVE    SPACE           TO  WRK-RIHA-SYUKBN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
      *H19.4からリハビリ管理料
                   MOVE    TBL-RSI-KBN (TBL-SAN)
                                           TO  WRK-RIHA-KBN
                   MOVE    TBL-RIHA-SYUKBN (TBL-SAN)
                                           TO  WRK-RIHA-SYUKBN
           END-SEARCH
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理（労災）
      *****************************************************************
       10001-RSI-JYURRK-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUG-SANTEI
           MOVE    SPACE               TO  WRK-SANTEI-YMD
           MOVE    SPACE               TO  WRK-HASSYOYMD
      *
      *    MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *    PERFORM 910-TENSU-READ-SEC
      *    IF      FLG-TENSU           =   1
      *        GO      TO      10001-RSI-JYURRK-CHK-EXT
      *    END-IF
      *
      *    保険毎のコードの判定
      *    PERFORM 1000-SANTEI-CHK-SEC
      *
      *労災コードなので、保険毎の算定履歴とする
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
      **** EVALUATE    TNS-SANTEIRRKKBN
      *        WHEN    1
      *            MOVE    ZERO                TO  SANTEI-NYUGAIKBN
      *            MOVE    ZERO                TO  SANTEI-SRYKA
      *        WHEN    2
      *            MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
      *            MOVE    ZERO                TO  SANTEI-SRYKA
      *        WHEN    3
      *            MOVE    ZERO                TO  SANTEI-NYUGAIKBN
      *            MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
      *        WHEN    4
      *            MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
      *            MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
      *    END-EVALUATE
      *
      *    IF     (FLG-HKNSAN          =   1   )  AND
      *           (SPA-HKNKBN      NOT =   ZERO)
      *        MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
      *    ELSE
      *        MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *****END-IF
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
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF      SANTEI-MSANTEICNT   >   ZERO
      *            初回算定日
                   MOVE    SANTEI-FSANTEIYMD   TO  WRK-HASSYOYMD
                   ADD     1                   TO  WRK-SANTEI-CNT
               END-IF
      *
               IF     FLG-SANTEI           =   ZERO
                  MOVE    "key3"              TO  MCP-PATHNAME
                  PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10001-RSI-JYURRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料初期発生処理
      *****************************************************************
       200-SINSATU-SET-SEC             SECTION.
      *
      ***  MOVE    SPACE               TO  SPA-SCR-REC
      **** INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-GAIRAISIN
           MOVE    ZERO                TO  SPA-SYONIKA-ARI
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    同日再診
           IF     (SPA-HKNCOMBINUM     =   "9999")
      *       受付あり
              OR  (SPA-UKETUKE-ARI     =   1    )
              OR  (SPA-TEISEIFLG       =   "0"  )
      *       治験
              OR  (SPA-CHIKENKBN       =   "1"  )
               CONTINUE
           ELSE
               IF     ( SPA-NYUGAIKBN      =   "2"        ) AND
                      ( SPA-SYORIFLG       =   ZERO       ) AND
      **********      ( SPA-RRK-LSTYMD     =   SPA-SRYYMD ) AND
                      ( SPA-NAI-RRKYMD(1)  =   SPA-SRYYMD ) AND
                      ( SPA-LSTYMD         =   SPA-SRYYMD )
      *            入院中以外
                AND   ( SPA-NAI-NYUIN      =   ZERO       )
                   IF      (SPA-TEISEIFLG       =   "1"  )
                       MOVE    "0100"              TO  SPA-KIDCD
                   ELSE
                       MOVE    "0131"              TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
      *    入院日の時、初診料のみ算定する
           IF    ( SPA-K02N-NYUINYMD(1:6)  =   SPA-NAI-SRYYMD(1:6) )
           AND   ( SPA-NYUGAIKBN           =   "1"    )
               IF      SPA-SYOSIN          =   1
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-IF
           IF     (SPA-HKNCOMBINUM NOT =   "9999" )  AND
                  (SPA-KIDCD           =   SPACE )   AND
                  (FLG-ERR             =   ZERO  )
                PERFORM 200-SYOSIN-SET-SEC
           END-IF
      *
           .
       200-SINSATU-SET-EXT.
           EXIT.
      *****************************************************************
      *    初診料自動発生処理
      *****************************************************************
       200-SYOSIN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-GAIRAISIN
           MOVE    ZERO                TO  SPA-SYONIKA-ARI
           MOVE    ZERO                TO  SPA-SYOSIN-KEI
           MOVE    ZERO                TO  SPA-SYOSIN-KEIFLG
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
           MOVE    ZERO                TO  FLG-ERR
      *    包括まとめ入力対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               MOVE    1                   TO  FLG-ERR
           END-IF
      *    治験対象外
           IF      SPA-CHIKENKBN       =   "1"
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
      *    入院日の時、初診料のみ算定する
           IF    ( SPA-K02N-NYUINYMD(1:6)  =   SPA-NAI-SRYYMD(1:6))
           AND   ( SPA-NYUGAIKBN           =   "1"    )
               IF      SPA-SYOSIN          =   1
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
      *    入院日の時、初診料のみ算定する
           IF    ( SPA-K02N-NYUINYMD       =   SPA-NAI-SRYYMD )
           AND   ( SPA-NYUGAIKBN           =   "2"    )
               IF      SPA-SYOSIN          =   1
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
      *    入院中（アフターケア）
           IF      SPA-NAI-NYUIN       =   1
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
      *    外来で自動発生なし
           IF     (SPA-NYUGAIKBN       =   "2"  )
             AND  (SPA-INITAUTO-FLG    =   "0"  )
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
      *ver.4.7
      *    夜間早朝加算の６歳未満対応
           MOVE    SPA-GAI-JIKANKBN    TO  WRK-HZN-JIKANKBN
           IF      SPA-GAI-JIKANKBN    =   8
      *        ６歳未満は小児科特例夜間へ
               IF     (SPA-NENREI-YY       <   6  )
                  AND (SPA-JIKANTOKU-NYU   =   1  )
                   MOVE    5               TO  SPA-GAI-JIKANKBN
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        労災の初診料算定
               IF     (SPA-HKNKBN          =   1   )
      *           自賠責の健保準拠以外
      *D         AND  (SPA-RSI-HKNKBN  NOT =   "5" )
      *           健保準拠以外
                 AND  (SPA-RSI-JUNKYO  NOT =   "1"  )
                   PERFORM 2001-RSI-SYOSIN-SET-SEC
               ELSE
                   MOVE    ZERO                TO  FLG-SYONIKA
      *            小児科外来診療科
                   IF     (SPA-SYONIKA         =   1   )  AND
                          (SPA-NYUGAIKBN       =   "2" )  AND
                         ((FLG-SYONIKA-ARI     =   1   ) OR
                          (SPA-SYONIKA-ARI3    =   1   ))
      *            施設入居者以外
                     AND  (SPA-PTTOKK-KBN      =   SPACE )
                       PERFORM 3303-SYONIKA-SET-SEC
                   END-IF
                   IF      FLG-SYONIKA       =   1
                       CONTINUE
                   ELSE
      *            健保の診察料算定
                       PERFORM 2002-KNH-SYOSIN-SET-SEC
                   END-IF
               END-IF
           END-IF
      *ver.4.7
      *    外来時間外区分設定を戻す
           MOVE    WRK-HZN-JIKANKBN    TO  SPA-GAI-JIKANKBN
      *
           .
       200-SYOSIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    労災初診料自動発生処理
      *****************************************************************
       2001-RSI-SYOSIN-SET-SEC       SECTION.
      *
      *    ２００床以上で再診の場合、外来診察料とする
           IF     (SPA-SYOSIN      NOT =   1  )  AND
                  (SPA-BEDSUIPN        >=  200 )  AND
                  (SPA-HOSPSBT         =   1   )
      *        健保診察料
               PERFORM 2002-KNH-SYOSIN-SET-SEC
           ELSE
               MOVE    SPACE               TO  WRK-KBN
               PERFORM 2009-RSIHKN-SET-SEC
               IF      SPA-GMN-IDX         >   ZERO
      *            療養担当手当発生
                   PERFORM 330123-RYOYOU-TEATE-SEC
               END-IF
           END-IF
           .
       2001-RSI-SYOSIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    健保診察料自動発生処理
      *****************************************************************
       2002-KNH-SYOSIN-SET-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
      *
      *    初診料算定
           IF      SPA-SYOSIN          =   1
               MOVE    "11"                TO  WRK-SRYKBN
               MOVE    "F"                 TO  WRK-KBN
      *        診察コードをテーブルより決定
               PERFORM 2001-SRYCD-TBL-SEC
           ELSE
      *        同日再診料算定
               MOVE    ZERO                TO  FLG-DOUJITU
               IF     ( SPA-SYORIFLG        =   ZERO     ) AND
                      ( SPA-RRK-LSTYMD      =   SPA-SRYYMD )
                   IF      SPA-DOUJITU     NOT =   ZERO
                       MOVE    1                   TO  FLG-DOUJITU
                   END-IF
               END-IF
               IF      (SPA-SYORIFLG        =   1       )   AND
                       (WRK-DAY-SINSATUCNT  >   ZERO    )
                   MOVE    1                   TO  FLG-DOUJITU
               END-IF
               IF      FLG-DOUJITU         =   1
                   IF     (SPA-TAKA-SRYKA-R    =   SPACE ) OR
                          (LNK-SC10S-SYORI     =   "J"   )
                     OR   (FLG-DOUJI-HEN       =   1     )
                     OR   (FLG-DOUJI-HEN2      =   1     )
                       MOVE    "12"                TO  WRK-SRYKBN
                       MOVE    "D"                 TO  WRK-KBN
      *                診察コードをテーブルより決定
                       PERFORM 2001-SRYCD-TBL-SEC
                   ELSE
                       PERFORM 3301-DOUSAISIN-SEC
                   END-IF
               ELSE
      *        再診料算定
                   MOVE    "12"                TO  WRK-SRYKBN
                   MOVE    SPACE               TO  WRK-KBN
      *            診察コードをテーブルより決定
                   PERFORM 2001-SRYCD-TBL-SEC
               END-IF
           END-IF
      *!!
           IF      SPA-PTTOKK-KBN  NOT =   SPACE
      *        小児科外来診察料は算定なしとする
               MOVE    ZERO                TO  SPA-SYONIKA-ARI2
               MOVE    ZERO                TO  SPA-SYONIKA-ARI3
           END-IF
      *    労災・自賠責、コメント以外
           IF     (SPA-HKNKBN          =   1   )
             OR   (WRK-SRYCD(1:2)      =   "83" )
               CONTINUE
           ELSE
      *    施設入居者はダミー
               IF      SPA-PTTOKK-KBN  NOT =   SPACE
                   IF      SPA-SYOSIN          =   1
                       MOVE    "099110001"         TO  WRK-SRYCD
                   ELSE
                       MOVE    "099120001"         TO  WRK-SRYCD
                   END-IF
               END-IF
           END-IF
      *
      *H24.4
      *    外来リハビリ診察料等算定時、再診ダミー？？
           IF     (WRK-SRYCD(1:2)      =   "83" )
      *        コメント以外
               CONTINUE
           ELSE
               IF     (SPA-KOURESRY-ARI2   =   2   OR  3  )
                   MOVE    "099120001"         TO  WRK-SRYCD
               END-IF
           END-IF
      *H24.4
      *
      *    初診料自動追加処理
           PERFORM 3301-SYOSIN-SYORI-SEC
      *
           IF      SPA-PTTOKK-KBN  NOT =   SPACE
               GO      TO      2002-KNH-SYOSIN-SET-EXT
           END-IF
      *H24.4
      *    外来リハビリ診察料等算定
           IF     (SPA-KOURESRY-ARI2   =   2   OR  3)
               GO      TO      2002-KNH-SYOSIN-SET-EXT
           END-IF
      *H24.4
      *
      *
           IF      WRK-SRYCD(1:2)      =   "83"
               MOVE    "0109"              TO  SPA-KIDCD
           END-IF
      *    入院日の時、初診料のみ算定する
           IF      SPA-K02N-NYUINYMD   =   SPA-NAI-SRYYMD
               MOVE    SPACE               TO  SPA-KIDCD
           END-IF
      *
           IF     (WRK-SRYKBN          =   "11"  )  AND
                  (SPA-GMN-IDX         =   ZERO  )  AND
                  (SPA-NYUGAIKBN       =   "2"   )
      *        初診料でエラーにより対象がない時、エラーの表示
               MOVE    2                   TO  SPA-SYOSIN-KEI
           END-IF
      *
           IF     (SPA-SYOSIN          =   1 )
      *        療養担当手当発生
               PERFORM 330123-RYOYOU-TEATE-SEC
           ELSE
      *        指導料の自動発生
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
               PERFORM 33012-SIDOU-SYORI-SEC
      *        療養担当手当（北海道）
               PERFORM 330123-RYOYOU-TEATE-SEC
           END-IF
      *
           .
       2002-KNH-SYOSIN-SET-EXT.
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
      *            200床以上は外来診察料
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
      *
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
      *    外来管理加算をテーブルより決定処理
      *****************************************************************
       2002-SRYCD-TBLG-SEC                  SECTION.
      *
           MOVE    TBLG-SRYCD          TO  WRK-SRYCD
      *
      *    一般の時、病院・診療所は同じ
      *    IF      SPA-NAI-ROUJIN      =   ZERO
      *        MOVE    ZERO            TO  WRK-HOSPSBT
      *    ELSE
      *        MOVE    SPA-HOSPSBT     TO  WRK-HOSPSBT
      *    END-IF
      *
      *    MOVE    SPACE               TO  WRK-KBN2
      *
      *    診察料セット
      *    SET     TBLG-IDX             TO  1
      *    SEARCH  TBL-GAIRAI-OCC       VARYING   TBLG-IDX
      *            AT   END
      *                MOVE    SPACE           TO  WRK-SRYCD
      *            WHEN   (SPA-NAI-ROUJIN      =   TBLG-ROUJIN
      *                                                      (TBLG-IDX))
      *              AND  (WRK-HOSPSBT         =   TBLG-HOSPSBT
      *                                                      (TBLG-IDX))
      *                MOVE    TBLG-SRYCD (TBLG-IDX)
      *                                        TO  WRK-SRYCD
      **** END-SEARCH
      *
           .
       2002-SRYCD-TBLG-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料自動発生処理
      *****************************************************************
       3301-SYOSIN-SYORI-SEC                  SECTION.
      *
      *    診察料追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *    以降の自動算定なし
           IF      FLG-ADD             =   ZERO
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
           ADD     1                   TO  IDX2
           IF      WRK-SRYCD(1:2)      =   "83"
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
           IF      SPA-PTTOKK-KBN  NOT =   SPACE
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
      *H24.4
      *    外来リハビリ診察料等算定でダミーの時
           IF     (SPA-KOURESRY-ARI2   =   2   OR  3 )
            AND   (WRK-SRYCD (1:3)     =   "099"     )
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
      *H24.4
      *
      *    外来で診察料の時、時間外区分を付ける
           IF     (SPA-SYORIFLG        =   ZERO )  AND
                  (SPA-NYUGAIKBN       =   "2"  )  AND
                  (WRK-SRYCD(1:1)      =   "1"  )  AND
                  (SPA-SRYYMD          =   SPA-SYSYMD) AND
                  (SPA-GAI-JIKANKBN    NOT =   ZERO )  AND
      *           診察料再算定時は付けない
                  (FLG-JIKANKBN        =   ZERO )
      *        小児科特例時間外は６歳未満
               IF     (SPA-GAI-JIKANKBN    =   5
                                           OR  6
                                           OR  7  )  AND
                      (SPA-NENREI-YY       >=  6  )
                   CONTINUE
               ELSE
      *
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                           TO  WRK-INPUTCD
               MOVE    SPACE               TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
               MOVE    SPA-GAI-JIKANKBN    TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)(1:1)
               MOVE    WRK-INPUTCD         TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)(3:)
               MOVE    SPA-GAI-JIKANKBN    TO  SPA-COM-TIMEFLG
                                                     (SPA-GMN-IDX)
           END-IF
      *
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
           MOVE    ZERO                TO  SPA-GAIRAISIN
           IF      SPA-SYOSIN          =   1
      *        初診の自動発生（育児栄養加算等）
               PERFORM 3302-IKUJIEIYO-SET-SEC
           ELSE
      *    以降、再診時のみ
      *        外来診察料の時、以降なし
               IF      WRK-KAISU           =   3
                    MOVE    1               TO  SPA-GAIRAISIN
               END-IF
           END-IF
      *Ｈ２２年４月から
      *    再診料の加算
           IF     (SPA-SYOSIN          =   1 )  OR
                  (SPA-GAIRAISIN       =   1 )  OR
                  (SPA-HOSPSBT         =   1 )
               CONTINUE
           ELSE
      *        地域医療貢献・明細書発行体制等加算
               PERFORM 33013-SAISINKSN-SEC
           END-IF
      *
      *
           IF     (SPA-SYOSIN          =   1 )  OR
                  (SPA-GAIRAISIN       =   1 )
      *        自動発生なし
               OR (SPA-GAIKANRIKBN-CHK =   3 )
               CONTINUE
           ELSE
      *    外来管理加算コード追加
               IF     (SPA-SYORIFLG        =   ZERO  ) OR
                      ((FLG-GAI            =   1   ) AND
                       (SPA-SYORIFLG       =   1   ))
                   PERFORM 2002-SRYCD-TBLG-SEC
      *            診察料追加
                   IF      WRK-SRYCD       NOT =   SPACE
                       PERFORM 3301-SYOSIN-HEN-SEC
      *                自動算定あり
                       IF      FLG-ADD             =   1
                           ADD     1               TO  IDX2
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       3301-SYOSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診料自動発生処理
      *****************************************************************
       3301-DOUSAISIN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
           IF      SPA-SYOYMD          =   SPA-SRYYMD
               MOVE    "830000020"         TO  WRK-SRYCD
           ELSE
               MOVE    "830000021"         TO  WRK-SRYCD
           END-IF
      *
           MOVE    "0109"              TO  SPA-KIDCD
      *    入院日の時、初診料のみ算定する
           IF      SPA-K02N-NYUINYMD   =   SPA-NAI-SRYYMD
               MOVE    SPACE               TO  SPA-KIDCD
           END-IF
      *
           .
       3301-DOUSAISIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料追加処理
      *****************************************************************
       3301-SYOSIN-HEN-SEC                SECTION.
      *
           IF     (WRK-SRYCD(1:2)      =   "83"  )   AND
                  (SPA-GMN-IDX         =   ZERO  )
               ADD     1                   TO  IDX2
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
           MOVE    "K02"               TO  LNK-SC92-PG
      *    警告のチェックなし
           MOVE    SPACE               TO  LNK-SC92-KEIFLG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    初診料の併用エラー
           IF      WRK-SRYKBN          =   "11"
               IF      LNK-SC92-SYOKBN     =   "1"
      *            前に併用算定がある時、病名がすべて治癒であること
                   IF      SPA-BYOMEI-STR      =   ZERO
                       MOVE    1                   TO  SPA-SYOSIN-KEI
                   ELSE
                       MOVE    "0033"              TO  LNK-SC92-ERRCD6
                   END-IF
               END-IF
      *
      *H24.9
      *        初診料の併用エラーは算定する
               IF      LNK-SC92-ERRCD6     =   "0033"
                   MOVE    SPACE               TO  LNK-SC92-ERRCD6
                   MOVE    2                   TO  SPA-SYOSIN-KEI
               END-IF
      *H24.9
      *
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
      ***      MOVE    SPA-COM-SRYSYUKBN(SPA-GMN-IDX)
               MOVE    SPA-COM-TNSSRYKBN(SPA-GMN-IDX)
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
      *平成２２年４月から廃止
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
      *H22.4　廃止
      *    IF      SPA-DENSIKA         =   1
      *        MOVE    "111012370"         TO  WRK-SRYCD
      *        追加共通処理
      *        MOVE    ZERO                TO  WRK-IDX-IN
      *        PERFORM 33041-SRYCD-ADD-SEC
      ***  END-IF
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
      *
      *****************************************************************
      *    小児科外来診療科自動発生処理
      *****************************************************************
       3303-SYONIKA-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYONIKA
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *    院外処方
           IF      SPA-INGAIKBN        =   "1"
               IF      SPA-SYOSIN          =   1
      *            初診
                   MOVE    "113003510"         TO  WRK-SRYCD
               ELSE
      *            再診
                   MOVE    "113003610"         TO  WRK-SRYCD
               END-IF
           ELSE
      *        院内処方
               IF      SPA-SYOSIN          =   1
      *            初診
                   MOVE    "113003710"         TO  WRK-SRYCD
               ELSE
      *            再診
                   MOVE    "113003810"         TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *    小児科外来診療科算定
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      WRK-IDX-IN          =   ZERO
               ADD     1                   TO  IDX2
               ADD     1                   TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IDX-IN          TO  IDX2
               MOVE    WRK-IDX-IN          TO  SPA-GMN-IDX
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
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
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *    年齢エラーの時、エラーをはずす（当月はＯＫ）
           IF      LNK-SC92-ERRCD5 NOT =   SPACE
      *        当月算定済み
               IF     (SPA-SYONIKA-ARI3    =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  LNK-SC92-ERRCD5
               END-IF
           END-IF
      *
      *    同日再診時、併用算定エラー
           IF      LNK-SC92-ERRCD3 NOT =   SPACE
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
               MOVE    ZERO                TO  SPA-GMN-IDX
               MOVE    ZERO                TO  IDX2
               MOVE    1                   TO  FLG-SYONIKA
               MOVE    2                   TO  SPA-SYONIKA-ARI
               MOVE    1                   TO  SPA-SYONIKA-ARI2
               GO      TO      3303-SYONIKA-SET-EXT
           END-IF
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    WRK-SCR-REC     TO  SPA-SCR-REC
               MOVE    ZERO            TO  SPA-GMN-IDX
               MOVE    ZERO            TO  IDX2
               GO      TO      3303-SYONIKA-SET-EXT
           END-IF
      *
      *    算定があり、既に当日他の診察料を算定している時、エラー
           IF     (SPA-SYOSIN          =   ZERO  )  AND
      ********** ((SPA-LSTYMD          =   SPA-SRYYMD) OR
                 ((SPA-LSTYMD          =   SPA-SRYYMD) AND
                  (WRK-DAY-SINSATUCNT  >   ZERO    )) AND
                  (SPA-SYORIFLG        =   ZERO   )
               MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
               MOVE    1                   TO  FLG-SYONIKA
               MOVE    ZERO                TO  SPA-GMN-IDX
               MOVE    ZERO                TO  IDX2
               MOVE    "K005"              TO  SPA-ERRCD
               GO      TO      3303-SYONIKA-SET-EXT
           END-IF
      *
      *    再編集処理
           MOVE    SPA-GMN-IDX         TO  IDX
           PERFORM 3101-INPUTCD-HEN-SEC
      *
      *    外来で診察料の時、時間外区分を付ける
           IF     (SPA-SYORIFLG        =   ZERO )  AND
                  (SPA-NYUGAIKBN       =   "2"  )  AND
                  (SPA-SRYYMD          =   SPA-SYSYMD) AND
                  (SPA-GAI-JIKANKBN    NOT =   ZERO )  AND
      *           診察料再算定時は付けない
                  (FLG-JIKANKBN        =   ZERO )
      *           時間外指定が無い時
             AND  (SPA-COM-TIMEFLG (SPA-GMN-IDX) =   ZERO )
      *            夜間・早朝加算はなし
               IF     (SPA-GAI-JIKANKBN    NOT =   8    ) 
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                               TO  WRK-INPUTCD
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   MOVE    SPA-GAI-JIKANKBN    TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)(1:1)
                   MOVE    WRK-INPUTCD         TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)(3:)
                   MOVE    SPA-GAI-JIKANKBN    TO  SPA-COM-TIMEFLG
                                                     (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-SYONIKA
           MOVE    1                   TO  SPA-SYONIKA-ARI
      *
           IF      SPA-GMN-IDX         >   ZERO
      *        療養担当手当（北海道）
               PERFORM 330123-RYOYOU-TEATE-SEC
           END-IF
           .
       3303-SYONIKA-SET-EXT.
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
               ADD     1                   TO  IDX2
               ADD     1                   TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IDX-IN          TO  IDX2
               MOVE    WRK-IDX-IN          TO  SPA-GMN-IDX
           END-IF
           MOVE    ZERO                TO  WRK-IN-KAISU
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           IF      WRK-IDX-IN          =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
               MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           ELSE
               MOVE    SPACE               TO  LNK-SC92-KBN
               MOVE    WRK-IDX-IN          TO  LNK-SC92-GMN-IDX
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
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
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
               COMPUTE IDX2            =   IDX2            -   1
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
      *****************************************************************
      *    指導料の自動発生処理
      *****************************************************************
       33012-SIDOU-SYORI-SEC           SECTION.
      *
      *    特定疾患科
           MOVE    ZERO                TO  FLG-TOKUTEI
           MOVE    ZERO                TO  FLG-TOKUTEI05
           MOVE    ZERO                TO  FLG-ATOPII
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )  OR
                              (SPA-TOKU-SRYKA(IDX) =   SPACE ) OR
                              (FLG-TOKUTEI     NOT =   ZERO )
               IF      SPA-TOKU-SRYKA (IDX)    =   WRK-SRYKA
                   MOVE    SPA-TOKU-FLG (IDX)      TO  FLG-TOKUTEI
                   MOVE    SPA-TOKU05-FLG (IDX)    TO  FLG-TOKUTEI05
                   MOVE    SPA-ATOPII  (IDX)       TO  FLG-ATOPII
               END-IF
           END-PERFORM
      *
      *    外来難病疾患
           IF      FLG-TOKUTEI         =   09
      *        特定疾患療養指導料算定済みであれば、指導料
               IF      SPA-TOKUTEI-CNT     >   ZERO
                   MOVE    FLG-TOKUTEI05       TO  FLG-TOKUTEI
                END-IF
           END-IF
      *    外来難病疾患
           IF      FLG-TOKUTEI         =   09
               PERFORM 330123-NANBYOU-SIDOU-SEC
           END-IF
      *    特定疾患療養指導料
           IF      FLG-TOKUTEI         =   05
                                       OR  08
               PERFORM 330121-TOKUTEI-SIDOU-SEC
           END-IF
      *
      *    皮膚科特定疾患
           IF      FLG-TOKUTEI         =   03
                                       OR  04
               PERFORM 330122-HIFUKA-SIDOU-SEC
           END-IF
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
      *
               IF      FLG-ADD2            =   1
                   MOVE    1                   TO  SPA-TOKUTEI-CNT
               END-IF
           END-IF
           .
       330121-TOKUTEI-SIDOU-EXT.
           EXIT.
      *****************************************************************
      *    難病外来指導料の自動発生処理
      *****************************************************************
       330123-NANBYOU-SIDOU-SEC      SECTION.
      *
           MOVE    "113002910"         TO  WRK-SRYCD
      *
      *    自動発生区分判定
           IF      SPA-BYOMEIAUTO-FLG  =  "1"
      *        指導料追加、チェック処理
               MOVE    ZERO                TO  FLG-SIDOU
               PERFORM 3301211-SIDOU-ADD-SEC
      *        追加エラーとなった時、特定疾患療養指導料へ
               IF     (FLG-ADD2            =   ZERO )
               AND    (FLG-TOKUTEI05   NOT =   ZERO )
                   MOVE    FLG-TOKUTEI05       TO  FLG-TOKUTEI
               END-IF
           END-IF
           .
       330123-NANBYOU-SIDOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料編集・チェック処理
      *****************************************************************
       3301211-SIDOU-ADD-SEC           SECTION.
      *
          MOVE    ZERO                 TO  FLG-ADD2
          EVALUATE    LNK-SC10S-SYORI
      *        中途終了展開
               WHEN    "W"
      *        診察料のみの変更処理
               WHEN    "S"
      *        診察料のみの変更処理（他保険受診ありで同日再診へ）
               WHEN    "J"
      *        診察料算定後、診察料のみの変更処理
               WHEN    "K"
               WHEN    "E"
                   PERFORM 33012111-SIDOU-CHUTO-SEC
               WHEN    "A"
      *            複数科入力
                   PERFORM 33012112-SIDOU-FUKU-SEC
               WHEN    "D"
      *            中途終了展開時自動算定なし
                   CONTINUE
               WHEN    OTHER
                   PERFORM 33012113-SIDOU-INS-SEC
           END-EVALUATE
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
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
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
               MOVE    1                   TO  FLG-ADD2
           END-IF
           MOVE    SPA-GMN-IDX         TO  IDX2
      *
           .
       33012113-SIDOU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了展開指導料編集・チェック処理
      *****************************************************************
       33012111-SIDOU-CHUTO-SEC        SECTION.
      *
           MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
      *    展開時の内容で判断する
           COMPUTE WRK-GMN-MAX         =   WRK-GMN-MAX   +   1
           IF      WRK-GMN-MAX         >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE            TO  WRK-GMN-MAX
           END-IF
           INITIALIZE                      WRK-GMN-TBL (WRK-GMN-MAX)
           MOVE    WRK-SRYCD           TO  WRK-COM-INPUTCD(WRK-GMN-MAX)
      *
           IF      LNK-SC10S-SYORI     =   "E"
      *        初診料から再診への変更時、初診料を削除
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE )
                   IF     (WRK-COM-INPUTCD (IDX)(1:1)  =   "1" )
                      AND (WRK-COM-SRYKBN (IDX)    =   "11"    )
                       MOVE    SPACE           TO  WRK-COM-INPUTCD (IDX)
                       MOVE    SPA-MAX-LINE        TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *H25.5
           IF     (LNK-SC10S-SYORI     =   "W"  )
      *        中途終了展開で、小児科外来診察料を再診料へ変換した時
      *        小児科外来診察料削除
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE )
                   IF     (WRK-COM-INPUTCD (IDX)(1:1)  =   "1" )
                      AND (WRK-COM-SRYKBN (IDX)    =   "13"    )
                       SET     TBL-IDX             TO  1
                       SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                           AT  END
                               MOVE    ZERO        TO  FLG-ARI
                           WHEN   WRK-COM-INPUTCD (IDX)
                                               =   TBL-SRYCD (TBL-IDX)
                           MOVE    1                   TO  FLG-ARI
                       END-SEARCH
                       IF      FLG-ARI         =   1
                           MOVE    SPACE       TO  WRK-COM-INPUTCD (IDX)
                           MOVE    SPA-MAX-LINE    TO  IDX
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *
           INITIALIZE                  ORCSC92AREA
           MOVE    "3"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    WRK-GMN-MAX         TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE           )  OR
                              (SPA-ERRCD       NOT  =   SPACE )  OR
                             ((HZN-COM-INPUTCD(IDX) =   SPACE ) AND
                              (HZN-GMN-INPUTCD(IDX) =   SPACE ))
               IF      HZN-COM-INPUTCD (IDX)   =   WRK-SRYCD
                   MOVE    "0134"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-SCR-REC
           INITIALIZE                  WRK-SCR-REC
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (LNK-SC92-ERRAREA    =   SPACE )
      *        エラーがない時、自動発生する
               PERFORM 33012113-SIDOU-INS-SEC
           ELSE
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
           .
       33012111-SIDOU-CHUTO-EXT.
           EXIT.
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
      ***         (SPA-ATOPII-FLG      =   1   )
      *ver.4.7   アトピー性皮膚炎のみの時、判定する
                  (FLG-ATOPII          =   1   )
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
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
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
           MOVE    SPA-GMN-IDX         TO  IDX2
      *
           .
       330123-RYOYOU-TEATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の初診料算定算定処理
      *****************************************************************
       2009-RSIHKN-SET-SEC         SECTION.
      *
      *    小児科外来診察料は算定なしとする
           MOVE    ZERO                TO  SPA-SYONIKA-ARI
           MOVE    ZERO                TO  SPA-SYONIKA-ARI2
           MOVE    ZERO                TO  SPA-SYONIKA-ARI3
      *
      *    初診料算定
           IF      SPA-SYOSIN          =   1
               MOVE    ZERO                TO  WRK-KAISU
               MOVE    "F"                 TO  WRK-KBN
               MOVE    SPACE               TO  WRK-KBN2
               PERFORM 20091-RSI-SINSATU-SEC
           ELSE
      *        再診料
      *        IF     (SPA-LSTYMD          =   SPA-SRYYMD  ) AND
      *               (SPA-TAKA-SRYKA-R    NOT =   SPACE   )
      *        AND    (WRK-KBN             =   SPACE   )
      *        AND    (LNK-SC10S-SYORI     NOT =   "J"   )
      *            同日再診・他科受診あり
      *            PERFORM 3301-DOUSAISIN-SEC
      *        ELSE
      *            平成１５年９月１日から逓減廃止とする
      *            MOVE    ZERO                TO  WRK-KAISU
      *            MOVE    "S"                 TO  WRK-KBN
      *            MOVE    SPACE               TO  WRK-KBN2
      *            PERFORM 20091-RSI-SINSATU-SEC
      *******  END-IF
      *H25.9
      *        同日再診料算定
               MOVE    ZERO                TO  FLG-DOUJITU
               IF     ( SPA-SYORIFLG        =   ZERO     ) AND
                      ( SPA-RRK-LSTYMD      =   SPA-SRYYMD )
                   IF      SPA-DOUJITU     NOT =   ZERO
                       MOVE    1                   TO  FLG-DOUJITU
                   END-IF
               END-IF
               IF      (SPA-SYORIFLG        =   1       )   AND
                       (WRK-DAY-SINSATUCNT  >   ZERO    )
                   MOVE    1                   TO  FLG-DOUJITU
               END-IF
               IF      FLG-DOUJITU         =   1
                   IF     (SPA-TAKA-SRYKA-R    =   SPACE ) OR
                          (LNK-SC10S-SYORI     =   "J"   )
                     OR   (FLG-DOUJI-HEN       =   1     )
                     OR   (FLG-DOUJI-HEN2      =   1     )
                       MOVE    "12"                TO  WRK-SRYKBN
                       MOVE    "D"                 TO  WRK-KBN
                       MOVE    SPACE               TO  WRK-KBN2
      *                診察コードをテーブルより決定
                       PERFORM 20091-RSI-SINSATU-SEC
                   ELSE
                       PERFORM 3301-DOUSAISIN-SEC
                   END-IF
               ELSE
      *        再診料算定
                   MOVE    "12"                TO  WRK-SRYKBN
                   MOVE    SPACE               TO  WRK-KBN
                   MOVE    SPACE               TO  WRK-KBN2
      *            診察コードをテーブルより決定
                   PERFORM 20091-RSI-SINSATU-SEC
               END-IF
           END-IF
      *    診察料自動追加処理
           PERFORM 3301-SYOSIN-HEN-SEC
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
           END-IF
      *
      *    入院日の時、初診料のみ算定する
           IF      SPA-NYUGAIKBN       =   "1" 
      ***      電子化加算
      ****     PERFORM 33021-DENSIKA-ADD-SEC
               GO      TO      2009-RSIHKN-SET-EXT
           END-IF
           IF      WRK-SRYCD(1:2)      =   "83"
               MOVE    "0109"              TO  SPA-KIDCD
               GO      TO      2009-RSIHKN-SET-EXT
           END-IF
      *
      *    外来で診察料の時、時間外区分を付ける
           IF     (SPA-SYORIFLG        =   ZERO )  AND
                  (FLG-ADD             =   1    )  AND
                  (SPA-NYUGAIKBN       =   "2"  )  AND
                  (WRK-SRYCD(1:1)      =   "1"  )  AND
                  (SPA-SRYYMD          =   SPA-SYSYMD) AND
                  (SPA-GAI-JIKANKBN    NOT =   ZERO )  AND
      *           診察料再算定時は付けない
                  (FLG-JIKANKBN        =   ZERO )
      *        小児科特例時間外は６歳未満
               IF     (SPA-GAI-JIKANKBN    =   5
                                           OR  6
                                           OR  7  )  AND
                      (SPA-NENREI-YY       >=  6  )
                   CONTINUE
               ELSE
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                           TO  WRK-INPUTCD
               MOVE    SPACE               TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
               MOVE    SPA-GAI-JIKANKBN    TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)(1:1)
               MOVE    WRK-INPUTCD         TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)(3:)
               MOVE    SPA-GAI-JIKANKBN    TO  SPA-COM-TIMEFLG
                                                     (SPA-GMN-IDX)
           END-IF
      *
           END-IF
      *
      *    以降、再診時のみ
           IF      SPA-SYOSIN          =   1
      *
      *****    電子化加算
      *******  PERFORM 33021-DENSIKA-ADD-SEC
               GO      TO      2009-RSIHKN-SET-EXT
           END-IF
      *Ｈ２２年４月から
      *    再診料の加算
           IF     (SPA-SYOSIN          =   1 )  OR
                  (SPA-GAIRAISIN       =   1 )  OR
                  (SPA-HOSPSBT         =   1 )
               CONTINUE
           ELSE
      *        地域医療貢献・明細書発行体制等加算
               PERFORM 33013-SAISINKSN-SEC
           END-IF
      *
      *    アフターケアの時、以下算定なし
           IF      SPA-RSI-HKNKBN      =   "3"
               GO      TO      2009-RSIHKN-SET-EXT
           END-IF
      *
      *    外来管理加算コード追加
           IF     (SPA-SYORIFLG        =   ZERO ) OR
                  (FLG-GAI             =   1   AND
                   SPA-SYORIFLG        =   1    )
               PERFORM 20092-RSIGAIRA-SEC
           END-IF
      *
      *??  継続管理加算追加
      *    　初診料算定月の判定
      *    IF      SPA-SYOYMD(1:6)     =   SPA-SRYYMD(1:6)
      *        IF      SPA-SYOYMD      <=  SPA-SRYYMD
      *            CONTINUE
      *        ELSE
      *            継続管理加算追加処理
      *            PERFORM 3302-KEIZOKU-HEN-SEC
      *        END-IF
      *    ELSE
      *        継続管理加算追加処理
      *        PERFORM 3302-KEIZOKU-HEN-SEC
      *??? END-IF
      *
           .
       2009-RSIHKN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の診察料検索処理
      *****************************************************************
       20091-RSI-SINSATU-SEC         SECTION.
      *
      *    診察料検索
           SET     TBL-RSI              TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    ZERO               TO  FLG-ARI
                   MOVE    SPACE              TO  WRK-SRYCD
               WHEN   (WRK-KAISU       =   TBLR-KAISU (TBL-RSI)) AND
                      (WRK-KBN         =   TBLR-KBN   (TBL-RSI)) AND
                      (WRK-KBN2        =   TBLR-KBN2  (TBL-RSI))
                   MOVE    1                  TO  FLG-ARI
                   MOVE    TBLR-SRYCD (TBL-RSI)
                                              TO  WRK-SRYCD
           END-SEARCH
           .
       20091-RSI-SINSATU-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算コード追加処理（労災）
      *****************************************************************
       20092-RSIGAIRA-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    "G"                 TO  WRK-KBN
      *
      *    テーブル検索
      *****PERFORM 20091-RSI-SINSATU-SEC
      *    労災外来管理加算コード
           MOVE    TBLRG-SRYCD         TO  WRK-SRYCD
           PERFORM 3301-SYOSIN-HEN-SEC
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
           END-IF
      *
           .
       20092-RSIGAIRA-EXT.
           EXIT.
      *
      *****************************************************************
      *    同時再診算定処理（確認後）
      *****************************************************************
       300-DOUSAISIN-SET-SEC         SECTION.
      *
      *    電話再診かのチェック
           PERFORM 3001-TELSAISIN-CHK-SEC
           IF      FLG-TEL-ARI         =   1
               GO      TO      300-DOUSAISIN-SET-EXT
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
      *
           IF     (SPA-HKNKBN          =   1   )
      *        健保準拠以外
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *        労災の同日再診料算定
               MOVE    "D"                 TO  WRK-KBN
               MOVE    SPACE               TO  WRK-KBN2
               MOVE    1                   TO  FLG-DOUJI-HEN
               PERFORM 2009-RSIHKN-SET-SEC
           ELSE
      *        同日再診料へ置換え
               MOVE    "12"                TO  WRK-SRYKBN
               MOVE    "D"                 TO  WRK-KBN
               MOVE    ZERO                TO  FLG-SAISIN-CHK
      *        診察コードをテーブルより決定
               PERFORM 2001-SRYCD-TBL-SEC
      *H24.4
      *       外来リハビリ診察料等算定時、再診ダミー？？
               IF     (SPA-KOURESRY-ARI2   =   2   OR  3  )
                   MOVE    "099120001"         TO  WRK-SRYCD
               END-IF
      *H24.4
      *        初診料自動追加
               PERFORM 3301-SYOSIN-SYORI-SEC
           END-IF
      *    診療行為内容の編集
           PERFORM 10102-SINSATU-CHENGE-SEC
           .
       300-DOUSAISIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    電話再診算定チェック処理（入力分）
      *****************************************************************
       3001-TELSAISIN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TEL-ARI
           MOVE    SPACE               TO  WRK-KBN4
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((SPA-GMN-INPUTCD(IDX)    =   SPACE) AND
                              (SPA-COM-INPUTCD(IDX)    =   SPACE)) OR
                              (FLG-TEL-ARI     =   1   )
      *        電話再診かのチェック
               MOVE    SPA-COM-INPUTCD (IDX)   TO  WRK-SRYCD
               MOVE    IDX                 TO  SPA-GMN-IDX
               PERFORM 30011-TELSAISIN-CHKSYORI-SEC
      *
           END-PERFORM
           .
       3001-TELSAISIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    電話再診コード判定変換処理
      *****************************************************************
       30011-TELSAISIN-CHKSYORI-SEC         SECTION.
      *
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
              AT   END
                   MOVE    SPACE           TO  WRK-KBN
                   MOVE    SPACE           TO  WRK-KBN2
                   MOVE    ZERO            TO  WRK-KAISU
               WHEN   (TBL-SRYCD (TBL-IDX) =   WRK-SRYCD) 
                   MOVE    TBL-KBN   (TBL-IDX)     TO  WRK-KBN
                   MOVE    TBL-KBN2  (TBL-IDX)     TO  WRK-KBN2
                   MOVE    ZERO            TO  WRK-KAISU
      *H25.4       紹介なし対応
                   MOVE    TBL-ROUJIN(TBL-IDX) TO  WRK-KBN4
           END-SEARCH
      *
      *    同日再診料へ置換え
           IF      WRK-KBN             =   "T"
               MOVE    "12"                TO  WRK-SRYKBN
               MOVE    "S"                 TO  WRK-KBN
      *        診察コードをテーブルより決定
               PERFORM 2001-SRYCD-TBL-SEC
      *        電話再診の時、同日電話再診へ振り替え
               MOVE    SPA-GMN-IDX         TO  WRK-IDX-IN
               PERFORM 33041-SRYCD-ADD-SEC
               MOVE    1                   TO  FLG-TEL-ARI
           END-IF
      *    労災判定
           IF     (FLG-TEL-ARI         =   ZERO )
              AND (SPA-HKNKBN          =   1    )
              AND (SPA-RSI-JUNKYO  NOT =   "1" )
               SET     TBL-RSI              TO  1
               SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
                   AT  END
                       MOVE    SPACE              TO  WRK-KBN
                       MOVE    SPACE              TO  WRK-KBN2
                   WHEN    TBLR-SRYCD (TBL-RSI)   =   WRK-SRYCD
                       MOVE    TBLR-KBN   (TBL-RSI)   TO  WRK-KBN
                       MOVE    TBLR-KBN2  (TBL-RSI)   TO  WRK-KBN2
               END-SEARCH
      *
      *        同日電話再診料へ置換え
               IF      WRK-KBN             =   "T"
                   MOVE    "S"                 TO  WRK-KBN
      *            診察コードをテーブルより決定
                   PERFORM 20091-RSI-SINSATU-SEC
                   MOVE    SPA-GMN-IDX         TO  WRK-IDX-IN
                   PERFORM 33041-SRYCD-ADD-SEC
                   MOVE    1                   TO  FLG-TEL-ARI
               END-IF
           END-IF
           .
       30011-TELSAISIN-CHKSYORI-EXT.
           EXIT.
      *
      *H25.9
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
                   WHEN    "113003610" 
                   WHEN    "113003710" 
                   WHEN    "113003810" 
                       MOVE    IDX                 TO  WRK-IDX-IN
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
               PERFORM 3303-SYONIKA-SET-SEC
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
      *****************************************************************
      *    同時再診算定処理（入力分）
      *****************************************************************
       500-INDOUSAISIN-SET-SEC         SECTION.
      *
      *    電話再診かのチェック
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)   TO  WRK-SRYCD
           PERFORM 30011-TELSAISIN-CHKSYORI-SEC
           IF      FLG-TEL-ARI         =   1
               GO      TO      500-TELDOUSAISIN-SET-EXT
           END-IF
      *
      *    SET     TBL-IDX             TO  1
      *    SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
      *            AT   END
      *                MOVE    SPACE           TO  WRK-KBN
      *                MOVE    SPACE           TO  WRK-KBN2
      *                MOVE    ZERO            TO  WRK-KAISU
      *            WHEN   (TBL-SRYCD (TBL-IDX) =   WRK-SRYCD) 
      *                MOVE    TBL-KBN   (TBL-IDX)     TO  WRK-KBN
      *                MOVE    TBL-KBN2  (TBL-IDX)     TO  WRK-KBN2
      *                MOVE    ZERO            TO  WRK-KAISU
      *    END-SEARCH
      *
      *    同日再診料へ置換え
      *    IF      WRK-KBN             =   "T"
      *        MOVE    "12"                TO  WRK-SRYKBN
      *        MOVE    "S"                 TO  WRK-KBN
      *        診察コードをテーブルより決定
      *        PERFORM 2001-SRYCD-TBL-SEC
      *        電話再診の時、同日電話再診へ振り替え
      *        MOVE    SPA-GMN-IDX         TO  WRK-IDX-IN
      *        PERFORM 33041-SRYCD-ADD-SEC
      *        GO      TO      500-TELDOUSAISIN-SET-EXT
      ***  END-IF
      *
      *    電話再診以外の時、再度診察料算定へ
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
           MOVE    ZERO                TO  SPA-SYOSIN
           PERFORM 100-SANTEI-CHK-SEC
           PERFORM 200-SYOSIN-SET-SEC
      *
           .
       500-TELDOUSAISIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     剤分離前の特定算定回数チェック処理
      *****************************************************************
       600-SANTEI-CNT-SEC              SECTION.
      *
      *    特定の診療行為の有無
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
      *    包括まとめ入力対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      600-SANTEI-CNT-EXT
           END-IF
      *
      *    アフターケア対象外
           IF      SPA-RSI-HKNKBN      =   "3"
               GO      TO      600-SANTEI-CNT-EXT
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE          )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
             IF      SPA-COM-HKNCOMBI (IDX)    NOT =   "9999"
      *          治験
                 OR (SPA-COM-CHIKENKBN (IDX)      =   "1")
      *
      *        特定の診療行為判定
               EVALUATE    SPA-COM-INPUTCD (IDX)
      *H14.3.31まで
      *            寝たきり老人在宅総合診療科（院外処方箋あり）
      *            WHEN    "114701210"
      *                MOVE    1               TO  SPA-NETAKIRI-ARI
      *                院外
      *                MOVE    "1"             TO  SPA-INGAIKBN
      *            寝たきり老人在宅総合診療科（院外処方箋なし）
      *            WHEN    "114701710"
      *                MOVE    2               TO  SPA-NETAKIRI-ARI
      *                院内
      *                MOVE    "0"             TO  SPA-INGAIKBN
      *H14.3.31まで
      *            寝たきり老人訪問診察料
      *            WHEN    "114700110"
      *            WHEN    "114701110"
      ***              MOVE    1               TO  SPA-HOUMON-ARI
      *
      *            在宅患者訪問診察料
                   WHEN    "114001110"
      *            在宅患者訪問診察料（住居系施設入居者）
                   WHEN    "114012910"
      *H24.4
      *            在宅患者訪問診察料（同一建物）（特定施設入居）
                   WHEN    "114018010"
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
      *Ｈ２２．４から廃止
      *            後期高齢者診療料
      *            WHEN    "113702110"
      *****            MOVE    1               TO  SPA-KOURESRY-ARI
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
                   EVALUATE    WRK-KBN
      *                電話再診、同日電話再診
                       WHEN    "T"
                       WHEN    "S"
      *H24.4
                       WHEN    "J"
                           MOVE    1               TO  SPA-TELSAI-ARI
      **************** WHEN    SPACE
                       WHEN    OTHER
      *                小児科外来診療科算定
                           IF      SPA-COM-INPUTCD (IDX)(1:3)
                                                       =   "113"
                               MOVE    1           TO  SPA-SYONIKA-ARI
                           END-IF
                   END-EVALUATE
      *H25.9
               ELSE
      *            労災コード判定
                   SET     TBL-RSI              TO  1
                   SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
                   AT  END
                       MOVE    SPACE              TO  WRK-KBN
                    WHEN   SPA-COM-INPUTCD (IDX)
                                              =   TBLR-SRYCD (TBL-RSI)
                       MOVE    1                  TO  FLG-ARI
                       MOVE    TBLR-KBN   (TBL-RSI)   TO  WRK-KBN
                   END-SEARCH
                   EVALUATE    WRK-KBN
      *                電話再診、同日電話再診
                       WHEN    "T"
                       WHEN    "S"
                       WHEN    "J"
                           MOVE    1               TO  SPA-TELSAI-ARI
                   END-EVALUATE
      *
               END-IF
             END-IF
           END-PERFORM
           IF      SPA-SYONIKA-ARI     =   ZERO
               MOVE    SPA-SYONIKA-ARI2    TO  SPA-SYONIKA-ARI
           END-IF
           IF      SPA-SYONIKA-ARI     =   ZERO
               IF      WRK-ZAITAKU-CNT     =   ZERO
                   MOVE    SPA-SYONIKA-ARI3    TO  SPA-SYONIKA-ARI
               END-IF
           END-IF
      **** 寝たきりが削除された時、２４時間加算の自動発生をする
      *    IF      SPA-NETAKIRI-ARI    =   ZERO
      *        MOVE    ZERO                TO  SPA-NETAKIRI-ARI3
      **** END-IF
           .
       600-SANTEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *     剤分離後の特定算定チェック処理
      *****************************************************************
       700-SANTEI-CHK-SEC              SECTION.
      *
      *    アフターケア対象外
           IF      SPA-RSI-HKNKBN      =   "3"
               GO      TO      700-SANTEI-CHK-EXT
           END-IF
      *
      *    小児科外来診療チェック
           PERFORM 5019-SYONIKA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      700-SANTEI-CHK-EXT
           END-IF
      *
      *****寝たきり老人在宅総合診療科チェック
      *    IF     (SPA-NETAKIRI-ARI    NOT =   ZERO )  OR
      *           (SPA-NETAKIRI-ARI2   NOT =   ZERO )
      *        PERFORM 5021-NETAKIRI-CHK-SEC
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      700-SANTEI-CHK-EXT
      *        END-IF
      **** END-IF
      *
      *    生活習慣病指導管理科チェック
           IF     (SPA-SEIKATU-ARI     NOT =   ZERO )  OR
                  (SPA-SEIKATU-ARI2    NOT =   ZERO )
      *        包括診療はチェックなし
               IF      SPA-HKTSANTEI-203   =   ZERO
      *
               PERFORM 5022-SEIKATU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      700-SANTEI-CHK-EXT
               END-IF
      *
               END-IF
           END-IF
      *
      *    在宅末期医療総合診察料チェック
           IF     (SPA-ZAIMATU-ARI     NOT =   ZERO )  OR
                  (SPA-ZAIMATU-ARI2    NOT =   ZERO )
      *        包括診療はチェックなし
               IF      SPA-HKTSANTEI-206   =   ZERO
      *
               PERFORM 5022-ZAIMATU-CHK-SEC
      *
               END-IF
           END-IF
      *
      *    在宅時医学総合管理料チェック
           IF     (SPA-ZAIKANRI-ARI    NOT =   ZERO )
             OR   (SPA-ZAIKANRI-ARI2   NOT =   ZERO )
      *        包括診療はチェックなし
               IF      SPA-HKTSANTEI-204   =   ZERO
      *
               PERFORM 5022-ZAIKANRI-CHK-SEC
      *
               END-IF
           END-IF
      *
      *Ｈ２２．４から廃止
      *    後期高齢者診療料チェック
      *    IF     (SPA-KOURESRY-ARI    NOT =   ZERO )
      *      OR   (SPA-KOURESRY-ARI2   NOT =   ZERO )
      *        PERFORM 5022-KOURESRY-CHK-SEC
      ***  END-IF
      *
      *    特定施設入居時等医学管理料
           IF     (SPA-IGAKUKAN-ARI    NOT =   ZERO )
             OR   (SPA-IGAKUKAN-ARI2   NOT =   ZERO )
      *        包括診療はチェックなし
               IF      SPA-HKTSANTEI-205   =   ZERO
      *
               PERFORM 5022-IGAKUKAN-CHK-SEC
      *
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
                 OR (SPA-COM-CHIKENKBN (IDX)      =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)         =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)         =   "1")
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
      *H24.4
                                                       OR  "J"
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
                         IF      SPA-HKTSANTEI-202   =   ZERO
      *
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
      *
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
                 OR (SPA-COM-CHIKENKBN (IDX)      =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)         =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)         =   "1")
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
                 OR (SPA-COM-CHIKENKBN (IDX)      =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)         =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)         =   "1")
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
                 OR (SPA-COM-CHIKENKBN (IDX)      =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)         =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)         =   "1")
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
                 OR (SPA-COM-CHIKENKBN (IDX)      =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)         =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)         =   "1")
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
                               MOVE    "1421"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                             (IDX)
                           END-IF
                       END-IF
                       WHEN    "11"
                       WHEN    "12"
                           CONTINUE
                       WHEN    OTHER
      *                    算定エラー
                           MOVE    "1421"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-EVALUATE
               END-IF
            END-IF
           END-PERFORM
           .
       5022-ZAIMATU-CHK-EXT.
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
                 OR (SPA-COM-CHIKENKBN (IDX)      =   "1")
      *          治験減点分
                 OR (SPA-COM-GENTEN (IDX)         =   "1")
      *          治験薬評分
                 OR (SPA-COM-YAKUHYO (IDX)         =   "1")
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
      *    再診料→初診料の振替時チェック処理
      *****************************************************************
       900-SYOSIN-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
           IF      SPA-COM-INPUTCD(1)(1:3) =   "113"
      *    小児科外来診察料
      *        院外処方
               IF      SPA-INGAIKBN        =   "1"
                   MOVE    "113003510"         TO  WRK-SRYCD
               ELSE
      *        院内処方
                   MOVE    "113003710"         TO  WRK-SRYCD
               END-IF
           ELSE
               IF      SPA-HKNKBN          =   1
      *        自賠責の健保準拠以外
      ****     AND    (SPA-RSI-HKNKBN  NOT =   "5" )
               AND    (SPA-RSI-JUNKYO  NOT =   "1" )
      *            労災の初診料算定
                   MOVE    ZERO                TO  WRK-KAISU
                   MOVE    "F"                 TO  WRK-KBN
                   PERFORM 20091-RSI-SINSATU-SEC
               ELSE
      *            小児科外来診察料以外
                   MOVE    "11"                TO  WRK-SRYKBN
                   MOVE    "F"                 TO  WRK-KBN
      *            診察コードをテーブルより決定
                   PERFORM 2001-SRYCD-TBL-SEC
               END-IF
           END-IF
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           IF      SPA-COM-INPUTCD(1)(1:3) =   "113"
               MOVE    "2"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    "1"                 TO  LNK-SC92-KBN
           END-IF
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    1                   TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    初診料の併用エラー
           MOVE    ZERO                TO  SPA-SYOSIN-KEI
           MOVE    ZERO                TO  SPA-SYOSIN-KEIFLG
           IF      LNK-SC92-SYOKBN     =   "1"
      *        前に併用算定がある時、病名がすべて治癒であること
               IF      SPA-BYOMEI-STR      =   ZERO
                   MOVE    1                   TO  SPA-SYOSIN-KEI
               ELSE
                   MOVE    "0033"              TO  LNK-SC92-ERRCD6
               END-IF
           END-IF
      *
      *    エラー時、最初のエラーコードをセット
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      900-SYOSIN-CHK-EXT
           END-IF
      *    小児科外来診察料で年齢エラーはＯＫとする
           IF     (SPA-SYONIKA-ARI     NOT =   ZERO )  AND
                  (LNK-SC92-ERRCD5     NOT =   SPACE)
               MOVE    SPACE               TO  LNK-SC92-ERRCD5
           END-IF
      *
           IF      LNK-SC92-ERRAREA    NOT =   SPACE
               IF      LNK-SC92-ERRCD3     NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD3         TO  SPA-ERRCD
               ELSE
                   MOVE    "0001"                  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       900-SYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了展開後のチェック処理
      *****************************************************************
       1000-WKSRYACT-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOSIN2
           MOVE    ZERO                TO  FLG-GAI
           MOVE    ZERO                TO  FLG-DUMMY
           MOVE    ZERO                TO  FLG-SINSATU-ARI
           MOVE    SPACE               TO  WRK-KBN3
      *    時間外なし
           MOVE    1                   TO  FLG-JIKANKBN
      *    画面テーブル内容チェック
           PERFORM 1001-TBLCHEK-SEC
      *    各診察算定回数処理
           PERFORM 100-SANTEI-CHK-SEC
           PERFORM 600-SANTEI-CNT-SEC
      *
           MOVE    1                   TO  FLG-HENKAN
      *    ＤＵＭＭＹ診察料の変更をおこなわない
           IF      FLG-DUMMY           =   1
               MOVE    ZERO                TO  FLG-HENKAN
           END-IF
      *    中途終了で診察料がない時
           IF     (FLG-SINSATU-ARI     =   ZERO )
               MOVE    ZERO                TO  FLG-HENKAN
           END-IF
           MOVE    ZERO                TO  FLG-DOUJI-HEN2
      *    同日再診ですでに診察がある時
           IF     (WRK-KBN3            =   "D"  )  AND
                 ((SPA-LSTYMD          =   SPA-SRYYMD) OR
                  (SPA-RRK-LSTYMD      =   SPA-SRYYMD))
               IF     (SPA-WKSRY-KARTE-FLG =   1   )
                   MOVE    ZERO                TO  FLG-HENKAN
               ELSE
      *            CLAIM　からの時、同日再診も再編集
                   MOVE    1                   TO  FLG-DOUJI-HEN2
               END-IF
           END-IF
      *    電話再診の時
           IF     (WRK-KBN3            =   "T"  OR  "S"
                                       OR  "J"  )
               MOVE    ZERO                TO  FLG-HENKAN
           END-IF
      *H24.4
      *    再診同一日２科目の時
           IF     (WRK-KBN3            =   "K"   )
               MOVE    ZERO                TO  FLG-HENKAN
           END-IF
      *
      *    初診料算定で、既に当日受診がある時は対象
           IF     (FLG-SYOSIN2         =   1    )
      *        初診料以外の時、自動発生あり
               IF     (SPA-SRYYMD      NOT =   SPA-LSTYMD )
      *            当日に診察料算定なし
                 OR  ((SPA-SRYYMD          =   SPA-LSTYMD ) AND
                      (WRK-DAY-SINSATUCNT  =   ZERO        ))
      *
                   MOVE    FLG-SYOSIN2         TO  SPA-SYOSIN
      *            中途終了分
                   IF     (SPA-WKSRY-KARTE-FLG =   1   )
                       MOVE    ZERO                TO  FLG-HENKAN
                   END-IF
      *            ＣＬＡＩＭから
                   IF     (SPA-WKSRY-KARTE-FLG NOT =   1   )
                       IF      (SPA-SINSATU-FLG    =   "0" )
      *                    初診料は自動発生しない
                           MOVE    ZERO                TO  FLG-HENKAN
                       END-IF
                   END-IF
               ELSE
                   MOVE    ZERO                TO  SPA-SYOSIN
               END-IF
           END-IF
      *    中途終了で再算定しない時
           IF     (SPA-CHUTOTENKAI-FLG =   "0"  )  AND
                  (SPA-WKSRY-KARTE-FLG =   1   )
               MOVE    ZERO                TO  FLG-HENKAN
           END-IF
      *    診察料自動発生なし時CLAIMもなし
           IF     (SPA-INITAUTO-FLG    =   "0" )
               MOVE    ZERO                TO  FLG-HENKAN
           END-IF
      *
      *TEST
      *ver.4.8
      *    電話再診処理済み区分設定
           IF     (WRK-KBN3            =   "T"  OR  "S" )
             AND  (IDX-TEL             >   ZERO         )
      *        ＣＬＡＩＭからは、環境設定は反映なし
               IF     (SPA-WKSRY-KARTE-FLG NOT =   1   )
                   MOVE    "2"             TO  SPA-COM-KSNAUTO
                                                   (IDX-TEL)
               END-IF
      *        診察料自動発生なし時CLAIMもなし
               IF     (SPA-INITAUTO-FLG    =   "0" )
      *        中途終了で再算定しない時
                 OR  ((SPA-CHUTOTENKAI-FLG =   "0"  )  AND
                      (SPA-WKSRY-KARTE-FLG =   1    ))
                   MOVE    "1"             TO  SPA-COM-KSNAUTO
                                                   (IDX-TEL)
               END-IF
           END-IF
      *ver.4.8
      *
      *    診察料変換
           IF      FLG-HENKAN          =   1
      *        時間外なし
               MOVE    1                   TO  FLG-JIKANKBN
               PERFORM 10101-SINSATU-HENKAN-SEC
           END-IF
           .
       1000-WKSRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面テーブル内容チェック処理
      *****************************************************************
       1001-TBLCHEK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-TAKA
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-TEL
           MOVE    SPACE               TO  WRK-KBN4
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE    )  OR
                             ((SPA-COM-INPUTCD(IDX) =   SPACE ) AND
                              (SPA-GMN-INPUTCD(IDX) =   SPACE ))
      *        診察料検索
               MOVE    ZERO                TO  FLG-ARI
               IF      FLG-SINSATU-ARI     =   ZERO
                   SET     TBL-IDX             TO  1
                   SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO                TO  FLG-ARI
                       MOVE    SPACE               TO  WRK-KBN
                   WHEN   SPA-COM-INPUTCD (IDX) =
                                             TBL-SRYCD (TBL-IDX)
                       MOVE    1                   TO  FLG-ARI
                       MOVE    TBL-KBN  (TBL-IDX)  TO  WRK-KBN
                       MOVE    1                   TO  FLG-SINSATU-ARI
                       MOVE    TBL-KBN  (TBL-IDX)  TO  WRK-KBN3
      *H25.4           紹介なし対応
                       MOVE    TBL-ROUJIN(TBL-IDX) TO  WRK-KBN4
                   END-SEARCH
               END-IF
      *
               IF      FLG-ARI             =   ZERO
      *            労災コードチェック（初診）
                   SET     TBL-RSI              TO  1
                   SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
                       AT  END
                           MOVE    ZERO               TO  FLG-ARI
                           MOVE    SPACE              TO  WRK-KBN
                       WHEN    SPA-COM-INPUTCD (IDX)
                                              =   TBLR-SRYCD (TBL-RSI)
                           MOVE    1                  TO  FLG-ARI
                           MOVE    TBLR-KBN (TBL-RSI) TO  WRK-KBN
                           MOVE    1           TO  FLG-SINSATU-ARI
                           MOVE    TBLR-KBN (TBL-RSI)
                                                   TO  WRK-KBN2
                           MOVE    TBLR-KBN (TBL-RSI)  TO  WRK-KBN3
                   END-SEARCH
               END-IF
               IF      FLG-ARI             =   1
      *            初診
                   IF      WRK-KBN             =   "F"
                       MOVE    1               TO  FLG-SYOSIN2
                   END-IF
      *            外来管理加算
                   IF      WRK-KBN             =   "G"
                       MOVE    1               TO  FLG-GAI
                   END-IF
      *ver.4.8
      *            電話再診の位置
                   IF     (WRK-KBN3            =   "T"  OR  "S" )
                       MOVE    IDX             TO  IDX-TEL
                   END-IF
      *
               END-IF
               EVALUATE    SPA-COM-INPUTCD (IDX)
      *                ＤＵＭＭＹ診察料
                   WHEN    "099110001"
                   WHEN    "099120001"
                       MOVE    1              TO  FLG-DUMMY
      *                他科受診・他保険
                   WHEN    "830000020"
                   WHEN    "830000021"
                   WHEN    "099999902"
                       MOVE    1               TO  FLG-TAKA
               END-EVALUATE
      *        外来管理加算
               IF      SPA-COM-INPUTCD (IDX)   =   TBLG-SRYCD
                   MOVE    1               TO  FLG-GAI
               END-IF
      *H25.9
      *        労災外来管理加算
               IF      SPA-COM-INPUTCD (IDX)   =   TBLRG-SRYCD
                   MOVE    1               TO  FLG-GAI
               END-IF
      *******  SET     TBLG-IDX              TO  1
      *        SEARCH  TBL-GAIRAI-OCC       VARYING   TBLG-IDX
      *            AT  END
      *                MOVE    ZERO          TO  FLG-ARI
      *            WHEN    SPA-COM-INPUTCD (IDX)
      *                                     =   TBLG-SRYCD (TBLG-IDX)
      *                MOVE    1            TO  FLG-GAI
      ******** END-SEARCH
           END-PERFORM
           .
       1001-TBLCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料のみの変更処理
      *****************************************************************
       1010-SINSATU-HEN-SEC              SECTION.
      *
      *H26.2
      *    保険区分変更の時
           IF     (SPA-HKNKBN      NOT =   SPA-MAE-HKNKBN)
      *        小児科外来診療科判定
               IF     (SPA-SYONIKA         =   1   )
                 AND  (SPA-NYUGAIKBN       =   "2" )
      *            当月算定回数を求める
                   PERFORM 1001-TOUGETU-KAISU-SEC
      *            小児科外来診療科判定
                   IF     (WRK-ZAITAKU-CNT     =   ZERO)
                     AND  (SPA-HKNKBN          =   ZERO )
                       PERFORM 1001-SYONIKA-CHK-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-SYONIKA-ARI3
                       MOVE    ZERO            TO  SPA-SYONIKA-ARI2
                       MOVE    ZERO            TO  SPA-SYONIKA-ARI
                   END-IF
               END-IF
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-SYOSIN2
           MOVE    ZERO                TO  FLG-GAI
           MOVE    ZERO                TO  FLG-DUMMY
           MOVE    ZERO                TO  FLG-SINSATU-ARI
           MOVE    SPACE               TO  WRK-KBN3
           MOVE    SPACE               TO  WRK-KBN4
           MOVE    ZERO                TO  FLG-DOUJI-HEN 
      *    時間外なし
           MOVE    ZERO                TO  FLG-JIKANKBN
      *    診察料を算定中かの判定
           IF     (SPA-SYORIFLG        =   1    )
               OR (LNK-SC10S-SYORI     =   "S"  )
      *H24.5
               OR (LNK-SC10S-SYORI     =   "J"  )
      *        時間外なし
               IF      SPA-SYORIFLG        =   1
                   MOVE    1                   TO  FLG-JIKANKBN
               END-IF
      *        画面テーブル内容チェック
               PERFORM 1001-TBLCHEK-SEC
           END-IF
      *    同時他科受診があった時は、診察料の変更をおこなわない
      *    （中途終了時以外）
           MOVE    1                   TO  FLG-HENKAN
           IF    ( LNK-SC10S-SYORI NOT =   "W"  AND "K"
                                                AND "J" )  AND
                 ((SPA-LSTYMD          =   SPA-SRYYMD) OR
                  (SPA-RRK-LSTYMD      =   SPA-SRYYMD))
               IF      SPA-TAKA-SRYKA-R    NOT =   SPACE
                   MOVE    ZERO                TO  FLG-HENKAN
               END-IF
           END-IF
      *
      *    診察料変換で同日再診以外はそのまま
           IF      (LNK-SC10S-SYORI    =   "S" )  AND
                   (FLG-HENKAN         =   ZERO)
               IF     (SPA-TAKA-SRYKA-R    NOT =   SPACE ) AND
                      (FLG-SINSATU-ARI     =   1         )
                   MOVE    1               TO  FLG-DOUJI-HEN 
                   MOVE    1               TO  FLG-HENKAN
               END-IF
           END-IF
      *
      *H25.7
      *    再診料変換（．１２）で他科受診ありは同日再診料とする
           IF      (LNK-SC10S-SYORI    =   "E" )  AND
                   (FLG-HENKAN         =   ZERO)
               IF     (SPA-TAKA-SRYKA-R    NOT =   SPACE )
                   MOVE    1               TO  FLG-DOUJI-HEN 
                   MOVE    1               TO  FLG-HENKAN
               END-IF
           END-IF
      *
      *    ＤＵＭＭＹ診察料の変更をおこなわない
           IF     (FLG-DUMMY           =   1     )
             AND  (SPA-PTTOKK-KBN      =   SPACE )
               MOVE    ZERO                TO  FLG-HENKAN
           END-IF
      *    診察料自動発生なし時、変更もなし
           IF     (SPA-INITAUTO-FLG    =   "0" )
               MOVE    ZERO                TO  FLG-HENKAN
           END-IF
      *    診察料変換処理
           IF      FLG-HENKAN          =   1
               PERFORM 10101-SINSATU-HENKAN-SEC
           END-IF
      *
           .
       1010-SINSATU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診察料変換処理
      *****************************************************************
       10101-SINSATU-HENKAN-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
      *
           IF      LNK-SC10S-SYORI NOT =   "W"
      *        中途終了展開は既に算定済み
               PERFORM 100-SANTEI-CHK-SEC
           END-IF
      *
      *    診察料編集
           PERFORM 200-SYOSIN-SET-SEC
      *    診療行為内容の編集
           PERFORM 10102-SINSATU-CHENGE-SEC
      *
           .
       10101-SINSATU-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    診察料チェック変換処理
      *****************************************************************
       10102-SINSATU-CHENGE-SEC              SECTION.
      *
      *    診療行為内容の編集
           MOVE    ZERO                TO  FLG-JIKAN
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPACE               TO  WRK-SCR-REC
           INITIALIZE                  WRK-SCR-REC
           MOVE    ZERO                TO  IDX3
           MOVE    ZERO                TO  FLG-CHK-ALL
           MOVE    ZERO                TO  FLG-ARI2
           MOVE    ZERO                TO  FLG-FUKU
           MOVE    IDX2                TO  IDX-INT-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((HZN-COM-INPUTCD (IDX)   =   SPACE ) AND
                              (HZN-GMN-INPUTCD (IDX)   =   SPACE ))
      *
               MOVE    ZERO                    TO  FLG-ARI
               IF      FLG-FUKU                =   ZERO
                 IF     (HZN-COM-SRYKBN(IDX)     =   "11"  OR  "12"
                                                 OR  "13"  OR  "14" ) OR
      *                療養担当手当
                     ( HZN-COM-INPUTCD(IDX)     =   "199000610" )
                   MOVE    HZN-COM-INPUTCD(IDX) TO  WRK-SRYCD
                   IF      HZN-COM-INPUTCD(IDX) NOT =   SPACE
                       PERFORM 10101-SINSATU-CHK-SEC
                   END-IF
      *
                   IF      FLG-ARI             =   1
                       IF     (HZN-GMN-INPUTCD(IDX)(1:1) NUMERIC  )  AND
                              (HZN-GMN-INPUTCD(IDX)(2:1) =   SPACE)
                           MOVE    HZN-GMN-INPUTCD(IDX)(1:1)
                                                  TO  FLG-JIKAN
                       END-IF
                       MOVE    1               TO  FLG-CHK
                   END-IF
                 END-IF
               END-IF
               IF      HZN-GMN-INPUTCD(IDX)(1:1)   =   "$"
                   MOVE    1                   TO  FLG-FUKU
               END-IF
               IF      FLG-ARI                 =   ZERO
                   IF      FLG-CHK             =   ZERO
                       ADD     1                   TO  IDX3
                       MOVE    HZN-GMN-TBL (IDX)   TO  WRK-GMN-TBL(IDX3)
                   END-IF
               END-IF
      *
      *        剤毎に対象を編集する
               IF     (HZN-COM-ZAIEND (IDX)    =   "1" )  OR
                     ((IDX                     <   SPA-MAX-LINE  )  AND
                      (WRK-GMN-INPUTCD(IDX + 1)(1:1)  =  "."))
      *            診療種別だけは削除
                   IF     (IDX3                =   1  )  AND
                          (WRK-GMN-INPUTCD(IDX3)(1:1)  =  "." )
      *H24.11
                       IF     (HZN-GMN-INPUTCD(IDX  + 1)(1:1)
                                                       =  "." )
                       OR     (SPA-COM-KAIFLG (IDX2)   =   "1" )
                           MOVE    ZERO                TO  IDX3
                       ELSE
      *                    診療種別の行削除チェック
                           PERFORM 4201-ZAIDEL-CHK-SEC
                       END-IF
      *
                   END-IF
                   PERFORM VARYING     IDX4    FROM   1   BY  1
                           UNTIL       IDX4    >   IDX3
                       ADD     1                   TO  IDX2
                       ADD     1                   TO  SPA-GMN-IDX
                       MOVE    WRK-GMN-TBL(IDX4)   TO  SPA-GMN-TBL
                                                                (IDX2)
                       MOVE    SPACE               TO  SPA-MAE-INPUTCD
                                                                (IDX2)
                       MOVE    SPACE               TO  SPA-COM-CHKFLG
                                                                (IDX2)
                   END-PERFORM
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX3
                   MOVE    ZERO                TO  FLG-CHK-ALL
                   MOVE    ZERO                TO  FLG-CHK
               END-IF
           END-PERFORM
      *
      *    診察料に変更が無い時、そのまま
      *!** IF      FLG-ARI2            =   1
      *        MOVE    HZN-SCR-REC         TO  SPA-SCR-REC
      *        GO      TO      10102-SINSATU-CHENGE-EXT
      *!   END-IF
      *
      *    剤毎に対象を編集する
           IF      IDX3                >   ZERO
               IF     (IDX3                =   1  )  AND
                      (WRK-GMN-INPUTCD(IDX3)(1:1)  =  "." )
                   MOVE    ZERO                TO  IDX3
               END-IF
               PERFORM VARYING     IDX4    FROM   1   BY  1
                       UNTIL       IDX4    >   IDX3
                   ADD     1                   TO  IDX2
                   ADD     1                   TO  SPA-GMN-IDX
                   MOVE    WRK-GMN-TBL(IDX4)   TO  SPA-GMN-TBL
                                                            (IDX2)
                   MOVE    SPACE               TO  SPA-MAE-INPUTCD(IDX2)
                   MOVE    SPACE               TO  SPA-COM-CHKFLG
                                                                (IDX2)
               END-PERFORM
           END-IF
      *    時間外入力あり
           IF      FLG-JIKAN       NOT =   ZERO
               IF      SPA-COM-INPUTCD(01)(1:1)    =   "1"
                  MOVE    SPA-GMN-INPUTCD(01) TO  WRK-INPUTCD
      *           既に時間外がある場合、前の時間外へ
                  IF     (SPA-GMN-INPUTCD(01)(1:1) NUMERIC  )  AND
                         (SPA-GMN-INPUTCD(01)(2:1) =   SPACE)
                       MOVE    SPA-GMN-INPUTCD(01)(3:)
                                               TO  WRK-INPUTCD
                   END-IF
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD(01)
                   MOVE    FLG-JIKAN           TO  SPA-GMN-INPUTCD(01)
                                                                (1:1)
                   MOVE    WRK-INPUTCD         TO  SPA-GMN-INPUTCD(01)
                                                                (3:)
                   MOVE    FLG-JIKAN           TO  SPA-COM-TIMEFLG(01)
               END-IF
           END-IF
           .
       10102-SINSATU-CHENGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別の行削除チェック処理
      *****************************************************************
       4201-ZAIDEL-CHK-SEC                 SECTION.
      *
      *    次が薬剤で前が剤終了でない時
           IF     (HZN-COM-INPUTCD(IDX  +  1)(1:1) =   "6"   )
           AND    (SPA-COM-KAIFLG (IDX2)       NOT =   "1"   )
               IF     (SPA-COM-SRYKBN (IDX2)
                                               =   "11"
                                               OR  "12"
                                               OR  "13" )
               OR     (SPA-COM-SRYSYUKBN(IDX2)
                                               =   "600" )
                   CONTINUE
               ELSE
                   MOVE    "1"                 TO  SPA-COM-KAIFLG
                                                          (IDX2)
                   INITIALIZE                  ORCSC94AREA
                   MOVE    "1"                 TO  LNK-SC94-KBN
                   MOVE    "K02"               TO  LNK-SC94-PG
                   MOVE    IDX2                TO  LNK-SC94-IDX
                   CALL    "ORCSC94"           USING
                                               ORCSC94AREA
                                               SPA-SCR-REC
                                               SPA-AREA
               END-IF
           END-IF
      *
           IF     (HZN-COM-INPUTCD(IDX + 1)(1:1)   =   "6"   )
           AND    (SPA-COM-SRYSYUKBN(IDX2)     =   "600" )
           AND    (SPA-COM-KAISU    (IDX2)     <=  1     )
      *        検査の時、回数が無効となる為、診療種別を変更する
               MOVE    1                   TO  IDX3
               MOVE    HZN-COM-SRYKBN(IDX + 1)
                                           TO  WRK-GMN-INPUTCD
                                                   (IDX3)(2:2)
               MOVE    "0"                 TO  WRK-GMN-INPUTCD
                                                   (IDX3)(4:1)
           ELSE
               MOVE    ZERO                TO  IDX3
           END-IF
      *
           .
       4201-ZAIDEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料チェック変換処理
      *****************************************************************
       10101-SINSATU-CHK-SEC              SECTION.
      *
      *    診察料検索
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
               AT  END
                   MOVE    ZERO                TO  FLG-ARI
                   MOVE    ZERO                TO  FLG-ARI3
               WHEN   WRK-SRYCD            =   TBL-SRYCD (TBL-IDX)
                   MOVE    1                   TO  FLG-ARI
                   MOVE    1                   TO  FLG-ARI3
           END-SEARCH
      *
           IF      FLG-ARI         =   ZERO
      *    外来管理加算
               IF      WRK-SRYCD           =   TBLG-SRYCD
                   MOVE    1                   TO  FLG-ARI
      *        外来管理加算を自動発生しない時、削除しない
                   IF      SPA-GAIKANRIKBN-CHK     =   3
                       MOVE    ZERO                TO  FLG-ARI
                   END-IF
               END-IF
      *******  SET     TBLG-IDX             TO  1
      *        SEARCH  TBL-GAIRAI-OCC       VARYING   TBLG-IDX
      *            AT  END
      *                MOVE    ZERO         TO  FLG-ARI
      *            WHEN   WRK-SRYCD         =   TBLG-SRYCD (TBLG-IDX)
      *                MOVE    1            TO  FLG-ARI
      ******** END-SEARCH
           END-IF
      *
           IF      FLG-ARI         =   ZERO
      *        労災コードチェック
               SET     TBL-RSI              TO  1
               SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
                   AT  END
                       MOVE    ZERO                TO  FLG-ARI
                   WHEN    WRK-SRYCD       =   TBLR-SRYCD (TBL-RSI)
                       MOVE    1                   TO  FLG-ARI
                       MOVE    1                   TO  FLG-ARI3
      ***              IF      TBLR-KBN (TBL-RSI)  =   "F"   OR "S"
      *                    MOVE    1                   TO  FLG-ARI3
      ***              END-IF
               END-SEARCH
      *        労災外来管理加算
               IF      WRK-SRYCD           =   TBLRG-SRYCD
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-IF
      *    診察料で同じコードがある時
           IF      FLG-ARI3            =   1
               PERFORM VARYING     IDXS    FROM    1   BY  1
                       UNTIL      (IDXS    >   SPA-GMN-IDX )  OR
                                  (FLG-ARI2    =   1           )
                   IF     (WRK-SRYCD(1:1)          =   "1"  )  AND
                          (WRK-SRYCD               =   SPA-COM-INPUTCD
                                                            (IDXS))
                       MOVE    1               TO  FLG-ARI2
                   END-IF
               END-PERFORM
           END-IF
      *
      *    その他の自動発生分
           IF     WRK-SRYCD            =   "830000020"   OR
                                           "830000021"
               MOVE    1               TO  FLG-ARI
           END-IF
      *
      *    ダミー
           IF     WRK-SRYCD            =   "099110001"   OR
                                           "099120001"
               MOVE    1               TO  FLG-ARI
           END-IF
      *    継続管理加算自動発生分
           IF     WRK-SRYCD            =   "112007170"   OR
                                           "112702270"
               MOVE    1               TO  FLG-ARI
           END-IF
      *    育児栄養指導料
           IF     WRK-SRYCD            =   "111000470"
               MOVE    1               TO  FLG-ARI
           END-IF
      *H24.4
      *    時間外対応加算
           IF     WRK-SRYCD            =   TBL-CHISRYCD1
                                       OR  TBL-CHISRYCD2
                                       OR  TBL-CHISRYCD3
               MOVE    1               TO  FLG-ARI
           END-IF
      *    明細書発行体制等加算
           IF     WRK-SRYCD            =   TBL-MEISRYCD
               MOVE    1               TO  FLG-ARI
           END-IF
      *
      *    寝たきり老人在宅総合診療科
           IF     WRK-SRYCD            =   "114701210"   OR
                                           "114701710"
               PERFORM VARYING     IDXS    FROM    1   BY  1
                       UNTIL      (IDXS    >   SPA-GMN-IDX )  OR
                                  (FLG-ARI =   1           )
                   IF      SPA-COM-INPUTCD (IDXS)  =   "114701210" 
                                                   OR  "114701710"
                       MOVE    1               TO  FLG-ARI
                   END-IF
               END-PERFORM
           END-IF
      *    特定慢性疾患指導料
           IF     WRK-SRYCD            =   "113700710"   OR
                                           "113700810"   OR
                                           "113700610"   OR
                                           "113001910"   OR
                                           "113002010"   OR
                                           "113001810"
      *            皮膚科性疾患指導料
                                       OR  "113000910"
                                       OR  "113002310"
               PERFORM VARYING     IDXS    FROM    1   BY  1
                       UNTIL      (IDXS    >   SPA-GMN-IDX )  OR
                                  (FLG-ARI =   1           )
                   IF      SPA-COM-INPUTCD (IDXS)  =   "113700710"
                                                   OR  "113700810"
                                                   OR  "113700610"
                                                   OR  "113001910"
                                                   OR  "113002010"
                                                   OR  "113001810"
      *            皮膚科性疾患指導料
                                                   OR  "113000910"
                                                   OR  "113002310"
                       MOVE    1               TO  FLG-ARI
                   END-IF
               END-PERFORM
           END-IF
      *
      *    その他同じコードがある時
           PERFORM VARYING     IDXS    FROM    1   BY  1
                   UNTIL      (IDXS    >   IDX-INT-MAX )  OR
                              (FLG-ARI =   1           )
               IF     (WRK-SRYCD(1:1)          =   "1"   ) AND
                      (WRK-SRYCD               =   SPA-COM-INPUTCD
                                                            (IDXS))
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
      *
           .
       10101-SINSATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料のみの変更処理（他保険受診あり）
      *****************************************************************
       1012-TAHOKEN-HEN-SEC              SECTION.
      *
      *    算定回数計算編集
           PERFORM 100-SANTEI-CHK-SEC
      *
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           COMPUTE SPA-GMN-IDX         =   SPA-GMN-IDX   +   1
           MOVE    "099999902"         TO  WRK-SRYCD
      *    点数マスタ編集・基本チェック
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
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
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
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                       -   1
               GO      TO      1012-TAHOKEN-HEN-EXT
           END-IF
           MOVE    SPA-GMN-IDX         TO  IDX2
      *    診療行為内容の編集
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (HZN-COM-INPUTCD (IDX)   =   SPACE   AND
                               HZN-GMN-INPUTCD (IDX)   =   SPACE )
               IF     (HZN-COM-SRYKBN(IDX)     =   "11"  OR  "12"
                                               OR  "13"  OR  "14" ) AND
                      (HZN-COM-INPUTCD(IDX)(1:1)   =   "1"   )
                   MOVE    HZN-COM-INPUTCD(IDX) TO  WRK-SRYCD
                   PERFORM 10101-SINSATU-CHK-SEC
                   IF      FLG-ARI             =   1
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
               IF      HZN-COM-INPUTCD(IDX)    =   "099999902"
                   MOVE    1               TO  FLG-CHK
               END-IF
      *
               IF      FLG-CHK         =   ZERO
                   ADD     1                   TO  IDX2
                   ADD     1                   TO  SPA-GMN-IDX
                   MOVE    HZN-GMN-TBL(IDX)    TO  SPA-GMN-TBL (IDX2)
               END-IF
               IF      HZN-COM-ZAIEND (IDX)    =   "1"
                   MOVE     ZERO               TO  FLG-CHK
               END-IF
           END-PERFORM
           .
       1012-TAHOKEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診察料算定後、診察料のみの変更処理
      *****************************************************************
       1014-SINSATU-HENKAN-SEC              SECTION.
      *
      *    複数科がある時、再編集
           MOVE    ZERO                TO  FLG-FUKU-ARI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                             ((SPA-COM-INPUTCD(IDX) =   SPACE ) AND
                              (SPA-GMN-INPUTCD(IDX) =   SPACE ))
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "$"
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX)
                   MOVE    1               TO  FLG-FUKU-ARI
               END-IF
           END-PERFORM
      *
           PERFORM 1010-SINSATU-HEN-SEC
      *
           IF      FLG-FUKU-ARI        =   1
      *        複数料（＄）、保険組合せ（＃）対応
               PERFORM 1015-FUKU-SYORI-SEC
           END-IF
           .
       1014-SINSATU-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    複数料（＄）、保険組合せ（＃）対応処理
      *****************************************************************
       1015-FUKU-SYORI-SEC              SECTION.
      *
           IF      LNK-SC10S-STRIDX    =   ZERO
               MOVE    1               TO  LNK-SC10S-STRIDX
           END-IF
      *
           PERFORM VARYING     IDX-A   FROM    LNK-SC10S-STRIDX   BY  1
                   UNTIL      (IDX-A   >   SPA-MAX-LINE  )  OR
                             ((SPA-COM-INPUTCD (IDX-A) =   SPACE ) AND
                              (SPA-GMN-INPUTCD (IDX-A) =   SPACE )) OR
                              (SPA-ERRCD           NOT =   SPACE )
      *
             IF      SPA-GMN-INPUTCD(IDX-A)  NOT =
                                             SPA-MAE-INPUTCD(IDX-A)
      *        診療科
               IF     (SPA-GMN-INPUTCD(IDX-A)(1:1) =   "$" ) AND
                      (SPA-GMN-INPUTCD(IDX-A)(2:2) NUMERIC ) AND
                      (SPA-GMN-INPUTCD(IDX-A)(4:1) =   SPACE)
                   PERFORM 10151-SRYKA-SYORI-SEC
               END-IF
      *        保険組合せ
               IF     (SPA-GMN-INPUTCD(IDX-A)(1:1) =   "#" ) AND
                      (SPA-GMN-INPUTCD(IDX-A)(2:4) NUMERIC )
                   PERFORM 10152-HKNCOMBI-SYORI-SEC
               END-IF
             END-IF
           END-PERFORM
      *
           .
       1015-FUKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    複数科明細追加処理
      *****************************************************************
       10151-SRYKA-SYORI-SEC              SECTION.
      *
      *    診療科
           MOVE    SPA-GMN-INPUTCD(IDX-A)(2:2)
                                       TO  WRK-SRYKA
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-SRYKA-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF      WRK-SRYKA           =   SPA-GMN-SRYKAL (IDX2)
                   MOVE    IDX2              TO  IDY
                   MOVE    1                 TO  FLG-CHK
               END-IF
           END-PERFORM
           IF       FLG-CHK            =   ZERO
               MOVE    "1053"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX-A)
               MOVE    "1"             TO  SPA-COM-CUR    (IDX-A)
           END-IF
      *!!
      *    受診履歴が９まで登録済みの時、エラー
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 101514-JYURRK-NUM-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX-A)
                   MOVE    "1"             TO  SPA-COM-CUR    (IDX-A)
               END-IF
           END-IF
      *!!
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(IDX-A)(5:4)
                                           TO  WRK-DRCD2
               INITIALIZE                  SPA-GMN-TBLREC (IDX-A)
               INITIALIZE                  SPA-COM-TBLREC (IDX-A)
               STRING  "$"                 DELIMITED   BY  SIZE
                       WRK-SRYKA           DELIMITED   BY  SIZE
                       " "                 DELIMITED   BY  SIZE
                       WRK-DRCD2           DELIMITED   BY  SPACE
                               INTO    SPA-GMN-INPUTCD (IDX-A)
               END-STRING
      *        ドクターの編集
               PERFORM 101513-DRCD-SYORI-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(IDX-A)
                                           TO  SPA-MAE-INPUTCD(IDX-A)
               MOVE    WRK-SRYKA           TO  SPA-COM-SRYKA   (IDX-A)
               MOVE    WRK-DRCD            TO  SPA-COM-DRCD    (IDX-A)
           END-IF
      *
           IF     (FLG-OK              =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE)  AND
                  (LNK-SC10S-SYORI     =   "A"
                                       OR  "D"   )
      *    診察料自動発生ありのみ
           AND    (SPA-INITAUTO-FLG    =   "1"   )
      *        同時初診判定
               PERFORM 101510-DOUJI-CHK-SEC
               IF      FLG-DOUSYOSIN       =   ZERO
                   PERFORM 101511-SRYKA-INS-SEC
               END-IF
           END-IF
      *
           .
       10151-SRYKA-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療履歴連番チェック処理
      *****************************************************************
       101514-JYURRK-NUM-CHK-SEC         SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1  BY  1
                   UNTIL      (IDX2    >   SPA-GMN-MAX1 )
                          OR  (SPA-ERRCD   NOT =   SPACE)
      *        診療日より後の履歴は対象外
               IF      (SPA-SRYYMD         =   SPA-NAI-RRKYMD (IDX2) )
                 AND   (WRK-SRYKA          =   SPA-NAI-RRK-SRYKA(IDX2))
                   IF      SPA-NAI-RENNUM (IDX2)   =   9
                       MOVE    "0220"              TO  SPA-ERRCD
                   END-IF
               END-IF
               IF      (SPA-SRYYMD         >   SPA-NAI-RRKYMD (IDX2))
                   MOVE    SPA-GMN-MAX1        TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       101514-JYURRK-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同時初診判定処理
      *****************************************************************
       101510-DOUJI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-DOUSYOSIN
           PERFORM VARYING     IDX     FROM    IDX-A   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )  OR
                             ((SPA-COM-INPUTCD (IDX) =   SPACE ) AND
                              (SPA-GMN-INPUTCD (IDX) =   SPACE )) OR
                              (FLG-DOUSYOSIN           =   1   )
               IF     (IDX-A                   NOT =   IDX )  AND
                      (SPA-GMN-INPUTCD(IDX)(1:1)   =   "$" )
                   MOVE    SPA-MAX-LINE                TO  IDX
               ELSE
      *            同日初診
                   IF      SPA-COM-INPUTCD (IDX)   =   "111011810"
      *TE 
                                                   OR  "101110040"
                       MOVE    1               TO  FLG-DOUSYOSIN
                   END-IF
      *H24.4
      *            再診料２科目
                   IF      SPA-COM-INPUTCD (IDX)   =   "112015810"
                                                   OR  "112015950"
                                                   OR  "112016210"
                       MOVE    1               TO  FLG-DOUSYOSIN
                   END-IF
      *H25.4
      *            初診料・外来診察料　２科目（紹介なし）
                   IF      SPA-COM-INPUTCD (IDX)   =   "111012610"
                                                   OR  "112016410"
                       MOVE    1               TO  FLG-DOUSYOSIN
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       101510-DOUJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    他科にて算定済み処理
      *****************************************************************
       101511-SRYKA-INS-SEC         SECTION.
      *
      *    他科にて算定済み
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  FLG-SRYKBN
           MOVE    SPACE               TO  WRK-HEN-SRYKA
           MOVE    ZERO                TO  FLG-SYOSIN-ARI
           MOVE    ZERO                TO  FLG-SYOKAINASI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >=  IDX-A    )  OR
                              (FLG-CHK     =   1   )
               IF     (IDX-A                   NOT =   IDX )  AND
                      (SPA-COM-SRYKA    (IDX)  NOT =   SPACE )  AND
                      (SPA-COM-SRYKA    (IDX)  =   WRK-SRYKA )
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF      SPA-SRYKA           =   WRK-SRYKA
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF     (SPA-COM-SRYKBN  (IDX)   =   "11" ) AND
                     ((SPA-COM-INPUTCD (IDX)(1:1)  =   "1" )  OR
                      (SPA-COM-INPUTCD (IDX)(1:3)  =   "099"))
                   IF      SPA-COM-SRYKA(IDX)  =   SPA-SRYKA
                       MOVE    1               TO  FLG-SRYKBN
                   END-IF
      *            初診料
                   IF     (SPA-COM-INPUTCD (IDX)(1:1)  =   "1" )
                     AND  (SPA-COM-DATAKBN (IDX)   =   "1"  )
                       MOVE    1               TO  FLG-SYOSIN-ARI
                   END-IF
      *            同日初診
                   IF      SPA-COM-INPUTCD (IDX)   =   "111011810"
      *TE          労災
                                                   OR  "101110040"
      *H25.4       初診料２科目（紹介なし）
                                                   OR  "111012610"
                       MOVE    1               TO  FLG-DOUSYOSIN
                   END-IF
      *H25.4
      *            初診料（紹介なし）の算定
                   IF      SPA-COM-INPUTCD (IDX)   =   "111012510"
                       MOVE    1               TO  FLG-SYOKAINASI
                   END-IF
      *
               END-IF
               IF     (SPA-COM-SRYKBN  (IDX)   =   "12" ) AND
                     ((SPA-COM-INPUTCD (IDX)(1:1)  =   "1" )  OR
                      (SPA-COM-INPUTCD (IDX)(1:3)  =   "099"))
                   MOVE    2               TO  FLG-SRYKBN
               END-IF
      *
               IF     (SPA-COM-SRYKA   (IDX)   =   SPA-SRYKA ) AND
                      (SPA-COM-SRYKBN  (IDX)   =   "11"      ) AND
                      (SPA-COM-INPUTCD (IDX)   =   "830000020")
                   MOVE    1                   TO  FLG-SRYKBN
                   MOVE    SPA-COM-ATAI1(IDX)  TO  WRK-HEN-SRYKA
               END-IF
               IF     (SPA-COM-SRYKA   (IDX)   =   SPA-SRYKA ) AND
                      (SPA-COM-SRYKBN  (IDX)   =   "12"      ) AND
                      (SPA-COM-INPUTCD (IDX)   =   "830000021")
                   MOVE    2                   TO  FLG-SRYKBN
                   MOVE    SPA-COM-ATAI1(IDX)  TO  WRK-HEN-SRYKA
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               PERFORM 101513-HKNCOMBI-GYOU-SEC
           END-IF
           IF     (FLG-CHK             =   ZERO )  AND
                  (FLG-SRYKBN      NOT =   ZERO )  AND
                  (IDX-A               <   199  )
            AND   (FLG-HKN-ARI     NOT =   9    )
               IF      FLG-HKN-ARI         =   1
                   MOVE    IDX-A           TO  IDX-A-HZN
                   ADD     1               TO  IDX-A
               END-IF
               PERFORM 1015112-TAKA-HEN-SEC
           END-IF
      *    保険組合せ
           IF      FLG-HKN-ARI         =   1
               MOVE    IDX-A-HZN           TO  IDX-A
               ADD     1                   TO  IDX-A
               IF      SPA-GMN-INPUTCD(IDX-A)(1:1)     =   "#"
                   PERFORM 10152-HKNCOMBI-SYORI-SEC
               END-IF
           END-IF
      *
           .
       101511-SRYKA-INS-EXT.
           EXIT.
      *****************************************************************
      *    ドクター名取得処理
      *****************************************************************
       101513-DRCD-SYORI-SEC         SECTION.
      *    ドクターの有無
           IF      WRK-DRCD2           =   SPACE
      *DDD                             OR  ZERO
               MOVE    SPACE               TO  WRK-DRCD-MEI
           ELSE
               MOVE    "1"                 TO  WRK-DRCD1
               MOVE    SPACE               TO  WRK-DRCD-MEI
               PERFORM 1015131-DRCDMEI-HEN-SEC
               IF      WRK-DRCD-MEI       =   SPACE
                   IF      SPA-ERRCD      =   SPACE
                       MOVE    "1054"          TO  SPA-ERRCD
                   END-IF
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX-A)
                   MOVE    "1"             TO  SPA-COM-CUR    (IDX-A)
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
             IF      WRK-DRCD-MEI        =   SPACE
               STRING  "＜＜"              DELIMITED  BY  SIZE
                       SPA-GMN-SRYKAMEIL(IDY)
                                           DELIMITED  BY  SPACE
                        "＞＞"             DELIMITED  BY  SIZE
                   INTO                SPA-GMN-MEISYO-G (IDX-A)
               END-STRING
             ELSE
               STRING  "＜＜"              DELIMITED  BY  SIZE
                       SPA-GMN-SRYKAMEIL(IDY)
                                           DELIMITED  BY  SPACE
                       "　ドクター："      DELIMITED  BY  SIZE
                       WRK-DRCD-MEI        DELIMITED  BY  SPACE
                        "＞＞"             DELIMITED  BY  SIZE
                   INTO                SPA-GMN-MEISYO-G (IDX-A)
               END-STRING
             END-IF
           END-IF
      *
           .
       101513-DRCD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ドクター名取得処理
      *****************************************************************
       1015131-DRCDMEI-HEN-SEC         SECTION.
      *
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 960-SYSKANRI-READ-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-KBNCD      =   WRK-DRCD
                   MOVE    SYS-1010-NAME       TO  WRK-DRCD-MEI
               END-IF
           END-IF
      *
           .
       1015131-DRCDMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    他科にて受診済み自動編集処理
      *****************************************************************
       1015112-TAKA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           IF     (FLG-SRYKBN          =   1 )  AND
                  (FLG-SYOSIN-ARI      =   1 )  AND
                  (FLG-DOUSYOSIN       =   ZERO )
      *        同日初診判定
               PERFORM 10151121-DOJITU-HEN-SEC
               IF      WRK-SRYCD       NOT =   SPACE
                   GO      TO      1015112-TAKA-HEN-EXT
               END-IF
           END-IF
      *
           IF      FLG-SRYKBN          =   1
               MOVE    ".110"              TO  WRK-SRYCD
           ELSE
               MOVE    ".120"              TO  WRK-SRYCD
           END-IF
           IF      IDX-KA              =   ZERO
               ADD     1               TO  IDX-A
               PERFORM 1015119-GYOINSN-SEC
           ELSE
               COMPUTE IDX-A           =   IDX-KA   -  1
           END-IF
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SPA-GMN-TBLREC  (IDX-A)
               INITIALIZE                  SPA-COM-TBLREC  (IDX-A)
               MOVE    WRK-SRYCD       TO  SPA-GMN-INPUTCD (IDX-A)
               MOVE    WRK-SRYCD       TO  SPA-MAE-INPUTCD (IDX-A)
               MOVE    WRK-SRYKA       TO  SPA-COM-SRYKA   (IDX-A)
               MOVE    1               TO  FLG-FUKU
           END-IF
      *
           IF      FLG-SRYKBN          =   1
               MOVE    "830000020"         TO  WRK-SRYCD
           ELSE
               MOVE    "830000021"         TO  WRK-SRYCD
           END-IF
           IF      IDX-A               <   SPA-MAX-LINE
               IF      SPA-COM-INPUTCD(IDX-A + 1)
                                           =   "830000020"  OR
                                               "830000021"
                   ADD     1               TO  IDX-A
               ELSE
                   ADD     1               TO  IDX-A
                   PERFORM 1015119-GYOINSN-SEC
               END-IF
           ELSE
               MOVE    "0007"              TO  SPA-ERRCD
           END-IF
           IF      WRK-HEN-SRYKA       =   SPACE
               MOVE    SPA-SRYKA       TO  WRK-HEN-SRYKA
           END-IF
           IF      SPA-ERRCD       =   SPACE
               INITIALIZE                  SPA-GMN-TBLREC  (IDX-A)
               INITIALIZE                  SPA-COM-TBLREC  (IDX-A)
               MOVE    WRK-SRYCD       TO  SPA-GMN-INPUTCD (IDX-A)
               MOVE    WRK-HEN-SRYKA   TO  SPA-GMN-INPUTCD (IDX-A)
                                                           (11:2)
               MOVE    WRK-SRYCD       TO  SPA-COM-INPUTCD (IDX-A)
               MOVE    WRK-HEN-SRYKA   TO  SPA-COM-ATAI1   (IDX-A)
               PERFORM 10151121-INPUCD-HENSYU-SEC
               MOVE    WRK-SRYKA       TO  SPA-COM-SRYKA   (IDX-A)
           END-IF
           IF      SPA-ERRCD       =   SPACE
      *        特定疾患療養指導料の発生
               PERFORM 10151122-TOKUTEI-HENSYU-SEC
           END-IF
           .
       1015112-TAKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    同日初診編集処理
      *****************************************************************
       10151121-DOJITU-HEN-SEC       SECTION.
      *
           IF      SPA-HKNKBN          =   1
      *        労災・自賠責
               IF      (SPA-RSI-JUNKYO     =   "1" )
      *            健保準拠
                   MOVE   "111011810"          TO  WRK-SRYCD
      *H25.4
      *            紹介なし
                   IF     FLG-SYOKAINASI       =   1
                       MOVE   "111012610"          TO  WRK-SRYCD
                   END-IF
               ELSE
      *            労災
                   MOVE   "101110040"          TO  WRK-SRYCD
               END-IF
           ELSE
               MOVE   "111011810"          TO  WRK-SRYCD
      *H25.4
      *        紹介なし
               IF     FLG-SYOKAINASI       =   1
                   MOVE   "111012610"          TO  WRK-SRYCD
               END-IF
      *
           END-IF
           IF      IDX-A               <   SPA-MAX-LINE
               IF      SPA-COM-INPUTCD(IDX-A + 1)
                                           =   WRK-SRYCD
                   ADD     1               TO  IDX-A
               ELSE
                   ADD     1               TO  IDX-A
                   PERFORM 1015119-GYOINSN-SEC
               END-IF
           ELSE
               MOVE    "0007"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SPA-GMN-TBLREC  (IDX-A)
               INITIALIZE                  SPA-COM-TBLREC  (IDX-A)
               MOVE    WRK-SRYCD       TO  SPA-GMN-INPUTCD (IDX-A)
               MOVE    WRK-SRYCD       TO  SPA-COM-INPUTCD (IDX-A)
               MOVE    "11"            TO  SPA-COM-SRYKBN  (IDX-A)
               PERFORM 10151121-INPUCD-HENSYU-SEC
               MOVE    WRK-SRYKA       TO  SPA-COM-SRYKA   (IDX-A)
               MOVE    1               TO  FLG-DOUSYOSIN
           END-IF
           IF      SPA-ERRCD       =   SPACE
      *        特定疾患療養指導料の発生
               PERFORM 10151122-TOKUTEI-HENSYU-SEC
           ELSE
               IF      SPA-ERRCD       NOT =   SPACE
                   COMPUTE IDX-A           =   IDX-KA   -  1
                   MOVE    SPACE           TO  WRK-SRYCD
               END-IF
           END-IF
           .
       10151121-DOJITU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    複数保険コメント編集処理
      *****************************************************************
       10151121-INPUCD-HENSYU-SEC       SECTION.
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "2"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    IDX-A               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           .
       10151121-INPUCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾患療養指導料の発生処理
      *****************************************************************
       10151122-TOKUTEI-HENSYU-SEC       SECTION.
      *
           IF      IDX-HKN         >   IDX-A
               MOVE    IDX-HKN         TO  SPA-GMN-IDX
           ELSE
               MOVE    IDX-A           TO  SPA-GMN-IDX
           END-IF
      *    指導料の自動発生
           PERFORM 33012-SIDOU-SYORI-SEC
      *    特定疾患科
      *    MOVE    ZERO                TO  FLG-TOKUTEI
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL      (IDX         >   10  )  OR
      *                       (SPA-TOKU-SRYKA(IDX) =   SPACE ) OR
      *                       (FLG-TOKUTEI     NOT =   ZERO )
      *        IF      SPA-TOKU-SRYKA (IDX)    =   WRK-SRYKA
      *            MOVE    SPA-TOKU-FLG (IDX)  TO  FLG-TOKUTEI
      *        END-IF
      *    END-PERFORM
      *    特定慢性疾患指導料
      *    IF      FLG-TOKUTEI         =   05
      *                                OR  08
      *        IF      IDX-HKN         >   IDX-A
      *            MOVE    IDX-HKN         TO  SPA-GMN-IDX
      *        ELSE
      *            MOVE    IDX-A           TO  SPA-GMN-IDX
      *        END-IF
      *        PERFORM 330121-TOKUTEI-SIDOU-SEC
      *    END-IF
      *
      *    皮膚科特定疾患
      *    IF      FLG-TOKUTEI         =   03  OR  04
      *        IF      IDX-HKN         >   IDX-A
      *            MOVE    IDX-HKN         TO  SPA-GMN-IDX
      *        ELSE
      *            MOVE    IDX-A           TO  SPA-GMN-IDX
      *        END-IF
      *        PERFORM 330122-HIFUKA-SIDOU-SEC
      *    END-IF
      *
           .
       10151122-TOKUTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数保険コメント編集処理
      *****************************************************************
       101513-HKNCOMBI-GYOU-SEC       SECTION.
      *
           MOVE    ZERO                TO  IDX-HKN
           MOVE    ZERO                TO  IDX-KA
           MOVE    ZERO                TO  FLG-CHK3
           MOVE    ZERO                TO  FLG-HKN-ARI
           MOVE    ZERO                TO  FLG-KA-ARI
           COMPUTE IDX4                =   IDX-A   +  1
           PERFORM VARYING     IDX     FROM    IDX4    BY  1
                   UNTIL     ( IDX         >   SPA-MAX-LINE     )  OR
                             ((SPA-GMN-INPUTCD(IDX)    =   SPACE ) AND
                              (SPA-COM-INPUTCD(IDX)    =   SPACE )) OR
                             ( FLG-CHK3    =   1       )
               IF      SPA-COM-INPUTCD (IDX)  =   "099409901"
                                              OR  "099999902"
                   MOVE    IDX                TO  IDX-HKN
               END-IF
               IF      SPA-COM-INPUTCD (IDX)  =   "830000020"
                                              OR  "830000021"
                   MOVE    IDX                TO  IDX-KA
               END-IF
               IF     (SPA-COM-INPUTCD (IDX)  =   "099409901"
                                              OR  "099999902"
                                              OR  "830000020"
                                              OR  "830000021") OR
                      (SPA-GMN-INPUTCD (IDX)(1:1)  =   "." 
                                              OR  "#" 
                                              OR  "$"   )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-CHK3
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "#"
                   MOVE    1               TO  FLG-HKN-ARI
      *
                   IF     (SPA-GMN-INPUTCD (IDX)(2:4)  =   "9999")
                       MOVE    9               TO  FLG-HKN-ARI
                   END-IF
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "$"
                   MOVE    1               TO  FLG-KA-ARI
               END-IF
           END-PERFORM
           .
       101513-HKNCOMBI-GYOU-EXT.
           EXIT.
      *****************************************************************
      *    複数保険明細追加処理
      *****************************************************************
       10152-HKNCOMBI-SYORI-SEC              SECTION.
      *
           MOVE    SPA-GMN-INPUTCD(IDX-A)(2:4)
                                       TO  WRK-HKNCOMBI
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPACE               TO  WRK-SEL-HKNNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF      WRK-HKNCOMBI        =   SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   MOVE    SPA-NAI-HKNNUM (IDX)  TO  WRK-SEL-HKNNUM
                   MOVE    IDX               TO  IDY
                   MOVE    1                 TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF       FLG-CHK            =   ZERO
               MOVE    "1051"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX-A)
               MOVE    "1"             TO  SPA-COM-CUR (IDX-A)
           ELSE
      *        保険名称を名称へ
               STRING  "《"                DELIMITED  BY  SIZE
                   SPA-GMN-THKNCOMBIMEI(IDY)
                                       DELIMITED  BY  "  "
                   "》"                DELIMITED  BY  SIZE
                   INTO                SPA-GMN-MEISYO-G (IDX-A)
               END-STRING
               MOVE    SPA-GMN-INPUTCD(IDX-A)
                                       TO  SPA-MAE-INPUTCD(IDX-A)
           END-IF
      *    労災・公害はエラー
           IF     (SPA-ERRCD           =   SPACE  )  AND
                 ((SPA-NAI-RSI-HKNKBN (IDY)   NOT =  SPACE)
              OR  (SPA-NAI-HKNNUM (IDY)    =   "975"  ))
               MOVE    "1050"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX-A)
               MOVE    "1"             TO  SPA-COM-CUR    (IDX-A)
           END-IF
      *
      *    老人・一般判定
      *    ６５歳以上で老人公費のある場合、
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-NAI-ROUJIN2     =   1      )
             AND  (WRK-HKNCOMBI    NOT =   "9999" )
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
               MOVE    SPA-PTID            TO  AGECHK-PTID
               MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
               MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
               MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
               MOVE    WRK-HKNCOMBI        TO  AGECHK-HKNCOMBINUM
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
               IF      SPA-NAI-ROUJIN  NOT =   AGECHK-ROUJIN
                   MOVE    "1052"          TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD
                                                          (IDX-A)
                   MOVE    "1"             TO  SPA-COM-CUR(IDX-A)
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-HKNCOMBI        TO  SPA-COM-HKNCOMBI(IDX-A)
      *        治験
               MOVE    SPA-NAI-CHIKENKBN (IDY)
                                           TO  SPA-COM-CHIKENKBN(IDX-A)
           END-IF
      *
           IF     (FLG-OK              =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE)  AND
                  (LNK-SC10S-SYORI     =   "A"
                                       OR  "D"   )
              AND (WRK-HKNCOMBI    NOT =   "9999" )
      *    診察料自動発生ありのみ
           AND    (SPA-INITAUTO-FLG    =   "1"   )
      *    治験対象外
           AND    (SPA-NAI-CHIKENKBN (IDY) NOT =   "1" )
               PERFORM 101521-HKNCOMBI-HEN-SEC
           END-IF
      *
           .
       10152-HKNCOMBI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    複数保険コメント追加編集処理
      *****************************************************************
       101521-HKNCOMBI-HEN-SEC       SECTION.
      *
      *    他保険にて算定済み
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >=  IDX-A   )  OR
                              (FLG-CHK     =    1  ) 
               IF     (IDX-A                   NOT =   IDX )  AND
                      (SPA-COM-HKNCOMBI (IDX)  NOT =   SPACE )  AND
                      (SPA-COM-HKNCOMBI (IDX)  =   WRK-HKNCOMBI )
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF      SPA-HKNCOMBINUM     =   WRK-HKNCOMBI
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  IDX-HKN
           IF      FLG-CHK             =   ZERO
               PERFORM 101513-HKNCOMBI-GYOU-SEC
           END-IF
           IF     (FLG-CHK             =   ZERO )  AND
                  (IDX-HKN             =   ZERO )
               IF      IDX-KA          NOT =   ZERO
                   MOVE    IDX-KA          TO  IDX-A
               END-IF
               IF      WRK-SEL-HKNNUM      =   SPA-HKNNUM
                   MOVE    "099409901"     TO  WRK-SRYCD
               ELSE
                   MOVE    "099999902"     TO  WRK-SRYCD
               END-IF
               IF      IDX-A           <   SPA-MAX-LINE
                   ADD     1               TO  IDX-A
                   PERFORM 1015119-GYOINSN-SEC
                   IF      SPA-ERRCD       =   SPACE
                       INITIALIZE                  SPA-GMN-TBLREC
                                                        (IDX-A)
                       INITIALIZE                  SPA-COM-TBLREC
                                                        (IDX-A)
                       MOVE    WRK-SRYCD       TO  SPA-GMN-INPUTCD
                                                       (IDX-A)
                       MOVE    WRK-SRYCD       TO  SPA-COM-INPUTCD
                                                       (IDX-A)
                       PERFORM 10151121-INPUCD-HENSYU-SEC
                       MOVE    WRK-HKNCOMBI    TO  SPA-COM-HKNCOMBI
                                                       (IDX-A)
                   END-IF
               END-IF
           END-IF
      *
           .
       101521-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       1015119-GYOINSN-SEC              SECTION.
      *
           IF      IDX-A               >=  SPA-MAX-LINE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      1015119-GYOINSN-EXT
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      IDX             =   IDX-A
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX2
                   MOVE    HZN-COM-TBLREC (IDX2) 
                                           TO  SPA-COM-TBLREC(IDX)
                   MOVE    HZN-GMN-TBLREC (IDX2) 
                                           TO  SPA-GMN-TBLREC(IDX)
               END-IF
               IF     (SPA-GMN-INPUTCD (IDX)   NOT =   SPACE ) OR
                      (SPA-COM-INPUTCD (IDX)   NOT =   SPACE )
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           .
       1015119-GYOINSN-EXT.
           EXIT.
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
      *****************************************************************
      *    算定歴付加マスタ読み込み
      *****************************************************************
       921-SANTEIPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEIPLUS-REC
               MOVE    ZERO                TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                      SANTEIPLUS-REC
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       921-SANTEIPLUS-READ-EXT.
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
