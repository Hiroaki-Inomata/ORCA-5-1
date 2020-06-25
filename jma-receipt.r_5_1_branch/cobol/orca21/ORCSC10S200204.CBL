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
       PROGRAM-ID.                 ORCSC10S200204.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為診察料自動発生編集処理
      *                      （Ｋ０２のみ共通使用）
      *                      Ｈ１４年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/03/01    MCC−多々納   新規作成
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
      * 02.00.05     NACL-多々納  05/11/18  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
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
      *
           03  FLG-KBN             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *
           03  FLG-SYOSIN          PIC 9(01).
      *
           03  FLG-ROUMANSEI       PIC 9(01).
      *    小児科外来診療科算定
           03  FLG-SYONIKA         PIC 9(01).
           03  FLG-SYONIKA-ARI     PIC 9(01).
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
      *    ＤＵＭＭＹ診察料
           03  FLG-DUMMY           PIC 9(01).
      *    電話再診あり
           03  FLG-TEL-ARI         PIC 9(01).
      *
           03  FLG-CHK-ALL         PIC 9(01).
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
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SRYCD               PIC X(09).
      *
           03  WRK-IDX-IN              PIC 9(04).
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
      *
           03  WRK-KEIYMD          PIC X(08).
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
      *    算定年月
           03  WRK-SANTEI-YM           PIC X(06).
           03  WRK-SANTEI-ARI          PIC X(06).
           03  WRK-SANTEI-NASI         PIC X(06).
      *
      *    老人慢性疾病診察料回数
           03  WRK-ROUMANSEI           PIC 9(04).
           03  WRK-ROUMANSEI2          PIC 9(04).
           03  WRK-ROUMANSEI-CNT       PIC 9(04).
           03  WRK-GAIRAI-IDX          PIC 9(04).
           03  WRK-SEIKATU-CNT         PIC 9(04).
      *
      *    寝たきり回数
           03  WRK-NETAKIRI-CNT    PIC 9(04).
      *
      *    在宅自己注射
           03  WRK-ZAITAKU             PIC 9(01).
           03  WRK-ZAITAKU-IDX         PIC 9(04).
      *
      *    発生年月日
           03  WRK-HASSYOYMD.
               05  WRK-HAS-YY         PIC 9(04).
               05  WRK-HAS-MM         PIC 9(02).
               05  WRK-HAS-DD         PIC 9(02).
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
      *    診察料テーブル
       01  TBL-SINSATU-AREA.
      *    初診料（一般 病院）
           03  FILLER              PIC X(14)   VALUE   "101F 111000110".
      *    初診料（老人 病院）
           03  FILLER              PIC X(14)   VALUE   "111F 111700110".
      *    初診料（一般 診療所）
           03  FILLER              PIC X(14)   VALUE   "102F 111003610".
      *    初診料（老人 診療所）
           03  FILLER              PIC X(14)   VALUE   "112F 111700210".
      *
      *    再診料（一般 病院）１回目
           03  FILLER              PIC X(14)   VALUE   "101  112007410".
      *    再診料（一般 病院）２，３回目
           03  FILLER              PIC X(14)   VALUE   "201  112007510".
      *    再診料（一般 病院）４回目以降
           03  FILLER              PIC X(14)   VALUE   "401  112007610".
      *    再診料（一般 病院）１５才未満
           03  FILLER              PIC X(14)   VALUE   "401 M112007710".
      *    再診料（一般 病院）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "401 E112012310".
      *    再診料（一般 病院）人工腎臓
           03  FILLER              PIC X(14)   VALUE   "401 J112007810".
      *
      *    電話再診料（一般 病院）１回目
           03  FILLER              PIC X(14)   VALUE   "101T 112007950".
      *    電話再診料（一般 病院）２，３回目
           03  FILLER              PIC X(14)   VALUE   "201T 112008050".
      *    電話再診料（一般 病院）４回目以降
           03  FILLER              PIC X(14)   VALUE   "401T 112008150".
      *    電話再診料（一般 病院）１５才未満
           03  FILLER              PIC X(14)   VALUE   "401TM112008250".
      *    電話再診料（一般 病院）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "401TE112012450".
      *
      *    同日再診（一般 病院）１回目
           03  FILLER              PIC X(14)   VALUE   "101D 112008350".
      *    同日再診（一般 病院）２，３回目
           03  FILLER              PIC X(14)   VALUE   "201D 112008450".
      *    同日再診（一般 病院）４回目以降
           03  FILLER              PIC X(14)   VALUE   "401D 112008550".
      *    同日再診（一般 病院）１５才未満
           03  FILLER              PIC X(14)   VALUE   "401DM112008650".
      *    同日再診（一般 病院）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "401DE112008750".
      *
      *    電話同日再診（一般 病院）１回目
           03  FILLER              PIC X(14)   VALUE   "101S 112008850".
      *    電話同日再診（一般 病院）２，３回目
           03  FILLER              PIC X(14)   VALUE   "201S 112008950".
      *    電話同日再診（一般 病院）４回目以降
           03  FILLER              PIC X(14)   VALUE   "401S 112009050".
      *    電話同日再診（一般 病院）１５才未満
           03  FILLER              PIC X(14)   VALUE   "401SM112009150".
      *    電話同日再診（一般 病院））厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "401SE112012550".
      *
      *    再診料（一般 診療所）１回目
           03  FILLER              PIC X(14)   VALUE   "102  112009210".
      *    再診料（一般 診療所）２，３回目
           03  FILLER              PIC X(14)   VALUE   "202  112009310".
      *    再診料（一般 診療所）４回目以降
           03  FILLER              PIC X(14)   VALUE   "402  112009410".
      *    再診料（一般 診療所）１５才未満
           03  FILLER              PIC X(14)   VALUE   "402 M112009510".
      *    再診料（一般 診療所）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "402 E112012610".
      *    再診料（一般 診療所）人工腎臓
           03  FILLER              PIC X(14)   VALUE   "402 J112009610".
      *
      *    電話再診（一般 診療所）１回目
           03  FILLER              PIC X(14)   VALUE   "102T 112009750".
      *    電話再診（一般 診療所）２，３回目
           03  FILLER              PIC X(14)   VALUE   "202T 112009850".
      *    電話再診（一般 診療所）４回目以降
           03  FILLER              PIC X(14)   VALUE   "402T 112009950".
      *    電話再診（一般 診療所）１５才未満
           03  FILLER              PIC X(14)   VALUE   "402TM112010050".
      *    電話再診（一般 診療所）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "402TE112012750".
      *
      *    同日再診（一般 診療所）１回目
           03  FILLER              PIC X(14)   VALUE   "102D 112010150".
      *    同日再診（一般 診療所）２，３回目
           03  FILLER              PIC X(14)   VALUE   "202D 112010250".
      *    同日再診（一般 診療所）４回目以降
           03  FILLER              PIC X(14)   VALUE   "402D 112010350".
      *    同日再診（一般 診療所）１５才未満
           03  FILLER              PIC X(14)   VALUE   "402DM112010450".
      *    同日再診（一般 診療所）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "402DE112010550".
      *
      *    電話同日再診（一般 診療所）１回目
           03  FILLER              PIC X(14)   VALUE   "102S 112010650".
      *    電話同日再診（一般 診療所）２，３回目
           03  FILLER              PIC X(14)   VALUE   "202S 112010750".
      *    電話同日再診（一般 診療所）４回目以降
           03  FILLER              PIC X(14)   VALUE   "402S 112010850".
      *    電話同日再診（一般 診療所）１５才未満
           03  FILLER              PIC X(14)   VALUE   "402SM112010950".
      *    電話同日再診（一般 診療所）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "402SE112012850".
      *
      *    再診料（老人 病院）１回目
           03  FILLER              PIC X(14)   VALUE   "111  112702310".
      *    再診料（老人 病院）２，３回目
           03  FILLER              PIC X(14)   VALUE   "211  112702410".
      *    再診料（老人 病院）４回目以降
           03  FILLER              PIC X(14)   VALUE   "411  112702510".
      *    再診料（老人 病院）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "411 E112706310".
      *    再診料（老人 病院）人工腎臓
           03  FILLER              PIC X(14)   VALUE   "411 J112702610".
      *
      *    電話再診（老人 病院）１回目
           03  FILLER              PIC X(14)   VALUE   "111T 112702750".
      *    電話再診（老人 病院）２，３回目
           03  FILLER              PIC X(14)   VALUE   "211T 112702850".
      *    電話再診（老人 病院）４回目以降
           03  FILLER              PIC X(14)   VALUE   "411T 112702950".
      *    電話再診（老人 病院）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "411TE112706450".
      *
      *    同日再診（老人 病院）１回目
           03  FILLER              PIC X(14)   VALUE   "111D 112703050".
      *    同日再診（老人 病院）２，３回目
           03  FILLER              PIC X(14)   VALUE   "211D 112703150".
      *    同日再診（老人 病院）４回目以降
           03  FILLER              PIC X(14)   VALUE   "411D 112703250".
      *    同日再診（老人 病院）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "411DE112703350".
      *
      *    同日電話再診（老人 病院）１回目
           03  FILLER              PIC X(14)   VALUE   "111S 112703450".
      *    同日電話再診（老人 病院）２，３回目
           03  FILLER              PIC X(14)   VALUE   "211S 112703550".
      *    同日電話再診（老人 病院）４回目以降
           03  FILLER              PIC X(14)   VALUE   "411S 112703650".
      *    同日電話再診（老人 病院）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "411SE112706550".
      *
      *    再診料（老人 診療所）１回目
           03  FILLER              PIC X(14)   VALUE   "112  112703710".
      *    再診料（老人 診療所）２，３回目
           03  FILLER              PIC X(14)   VALUE   "212  112703810".
      *    再診料（老人 診療所）４回目以降
           03  FILLER              PIC X(14)   VALUE   "412  112703910".
      *    再診料（老人 診療所）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "412 E112706610".
      *    再診料（老人 診療所）人工腎臓
           03  FILLER              PIC X(14)   VALUE   "412 J112704010".
      *
      *    電話再診（老人 診療所）１回目
           03  FILLER              PIC X(14)   VALUE   "112T 112704150".
      *    電話再診（老人 診療所）２，３回目
           03  FILLER              PIC X(14)   VALUE   "212T 112704250".
      *    電話再診（老人 診療所）４回目以降
           03  FILLER              PIC X(14)   VALUE   "412T 112704350".
      *    電話再診（老人 診療所）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "412TE112706750".
      *
      *    同日再診（老人 診療所）１回目
           03  FILLER              PIC X(14)   VALUE   "112D 112704450".
      *    同日再診（老人 診療所）２，３回目
           03  FILLER              PIC X(14)   VALUE   "212D 112704550".
      *    同日再診（老人 診療所）４回目以降
           03  FILLER              PIC X(14)   VALUE   "412D 112704650".
      *    同日再診（老人 診療所）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "412DE112704750".
      *
      *    同日電話再診（老人 診療所）１回目
           03  FILLER              PIC X(14)   VALUE   "112S 112704850".
      *    同日電話再診（老人 診療所）２，３回目
           03  FILLER              PIC X(14)   VALUE   "212S 112704950".
      *    同日電話再診（老人 診療所）４回目以降
           03  FILLER              PIC X(14)   VALUE   "412S 112705050".
      *    同日電話再診（老人 診療所）厚生省が定める患者
           03  FILLER              PIC X(14)   VALUE   "412SE112706850".
      *
      *
      *    小児科外来診察料(院外処方 初診)
           03  FILLER              PIC X(14)   VALUE   "102FG113003510".
      *    小児科外来診察料(院内処方 初診)
           03  FILLER              PIC X(14)   VALUE   "102FN113003710".
      *    小児科外来診察料(院外処方 再診)
           03  FILLER              PIC X(14)   VALUE   "202 G113003610".
      *    小児科外来診察料(院内処方 再診)
           03  FILLER              PIC X(14)   VALUE   "202 N113003810".
      *
       01  TBL-SINSATU-AREAR       REDEFINES   TBL-SINSATU-AREA.
           03  TBL-SINSATU-OCC         OCCURS  84    INDEXED  TBL-IDX.
      *        回数
               05  TBL-KAISU           PIC 9(01).
      *        老人一般区分（０：一般、１：老人）
               05  TBL-ROUJIN          PIC 9(01).
      *        病院種別（１：病院、２：診療所）
               05  TBL-HOSPSBT         PIC 9(01).
      *        D:同日再診、T：電話再診、S:同日電話再診、F:初診
               05  TBL-KBN             PIC X(01).
      *        M:１５才未満、E：厚生省が定める患者、J:人工腎臓
      *        G:院外、N:院内（小児科外来診察料）
               05  TBL-KBN2            PIC X(01).
      *        診療コード
               05  TBL-SRYCD           PIC X(09).
      *
       01  TBL-SIN-MAX                 PIC 9(04)   VALUE   84.
      *
      *
      *    外来管理加算テーブル
       01  TBL-GAIRAI-AREA.
      *    一般（３回目まで）
           03  FILLER              PIC X(13)   VALUE    "100 112011010".
      *    一般（４回目以降）
           03  FILLER              PIC X(13)   VALUE    "200 112011110".
      *    一般（１５才未満）
           03  FILLER              PIC X(13)   VALUE    "200M112011210".
      *    一般（厚生省が定める患者）
           03  FILLER              PIC X(13)   VALUE    "200E112012910".
      *
      *    老人 病院（３回目まで）
           03  FILLER              PIC X(13)   VALUE    "111 112705110".
      *    老人 病院（４回目以降）
           03  FILLER              PIC X(13)   VALUE    "211 112705210".
      *    老人 病院（厚生省が定める患者）
           03  FILLER              PIC X(13)   VALUE    "211E112706910".
      *
      *    老人 診療所（３回目まで）
           03  FILLER              PIC X(13)   VALUE    "112 112705310".
      *    老人 診療所（４回目以降）
           03  FILLER              PIC X(13)   VALUE    "212 112705410".
      *    老人 診療所（厚生省が定める患者）
           03  FILLER              PIC X(13)   VALUE    "212E112707010".
      *
      *
       01  TBL-GAIRAI-AREAR       REDEFINES   TBL-GAIRAI-AREA.
           03  TBL-GAIRAI-OCC          OCCURS  10   INDEXED  TBLG-IDX.
      *        回数
               05  TBLG-KAISU           PIC 9(01).
      *        老人一般区分（０：一般、１：老人）
               05  TBLG-ROUJIN          PIC 9(01).
      *        病院種別（１：病院、２：診療所）
               05  TBLG-HOSPSBT         PIC 9(01).
      *        M:１５才未満、E：厚生省が定める患者
               05  TBLG-KBN2            PIC X(01).
      *        診療コード
               05  TBLG-SRYCD           PIC X(09).
      *
      *    労災診察料テーブル
       01  TBL-RSI-SINSATU-AREA.
      *    初診料
           03  FILLER              PIC X(13)   VALUE   "0 F 101110010".
      *    再診料（３月以内）
           03  FILLER              PIC X(13)   VALUE   "0 S 101120040".
      *    再診料（３月超）５回以内
           03  FILLER              PIC X(13)   VALUE   "1 S 101120050".
      *    再診料（３月超）５回以内
           03  FILLER              PIC X(13)   VALUE   "5 S 101120060".
      *
      *    外来管理加算（３月以内）
           03  FILLER              PIC X(13)   VALUE   "0 G 101120070".
      *    外来管理加算（３月超）５回以内
           03  FILLER              PIC X(13)   VALUE   "1 G 101120080".
      *    外来管理加算（３月超）５回以内
           03  FILLER              PIC X(13)   VALUE   "5 G 101120090".
      *
       01  TBL-RSI-SINSATU-AREAR   REDEFINES   TBL-RSI-SINSATU-AREA.
           03  TBL-RSI-SINSATU-OCC     OCCURS  7    INDEXED  TBL-RSI.
      *        回数
               05  TBLR-KAISU           PIC 9(01).
               05  FILLER               PIC X(01).
      *        F:初診、S:再診、G:外来管理加算
               05  TBLR-KBN             PIC X(01).
               05  FILLER               PIC X(01).
      *        診療コード
               05  TBLR-SRYCD           PIC X(09).
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
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
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
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
      *   患者番号変換サブ
      *    COPY    "CPORCSPTID.INC".
      *
      *    負担金額計算用パラメタ
      *D   COPY    "CPORCS01.INC".
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
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
      *        再診料の４回目以降の振替え処理
               WHEN    "8"
                   PERFORM 800-SAISIN-HEN-SEC
      *        再診料→初診料の振替時チェック処理
               WHEN    "9"
                   PERFORM 900-SYOSIN-CHK-SEC
      *        中途終了選択時チェック処理
               WHEN    "W"
      *******      PERFORM 1000-WKSRYACT-HEN-SEC
                   PERFORM 1010-SINSATU-HEN-SEC
      *        診察料のみの変更処理
               WHEN    "S"
                   PERFORM 1010-SINSATU-HEN-SEC
      *        診察料のみの変更処理（他保険受診あり）
               WHEN    "H"
                   PERFORM 1012-TAHOKEN-HEN-SEC
      *        初診料を算定中かの判定
               WHEN    "C"
                   PERFORM 1011-SYOSIN-CHK-SEC
      *        診察料算定後、診察料のみの変更処理
               WHEN    "K"
                   PERFORM 1014-SINSATU-HENKAN-SEC
           END-EVALUATE
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    IDX2                TO  LNK-SC10S-IDX2
      *
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
                   UNTIL      (IDX     >   SPA-MAX-LINE    )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
               EVALUATE    SPA-COM-INPUTCD (IDX)
                   WHEN    "111700110"
                   WHEN    "111700210"
                   WHEN    "111000110"
                   WHEN    "111003610"
                   WHEN    "113003510"
                   WHEN    "113003710"
                       MOVE    1              TO  SPA-SYOSIN
      *                労災分
                   WHEN    "101110010"
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
      *    小児科外来診療科
           IF     (SPA-SYONIKA         =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               PERFORM 1001-SYONIKA-CHK-SEC
           END-IF
      *
      *    寝たきり老人在宅総合診療科算定処理
           IF     (SPA-NAI-ROUJIN      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               PERFORM 1002-NETAKIRI-CHK-SEC
           END-IF
      *
      *    老人慢性疾患外来総合診療科算定
           IF     (SPA-ROUMANSEI       =   1   )  AND
                  (SPA-SYOSIN      NOT =   1   )  AND
                  (SPA-NAI-ROUJIN      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               PERFORM 1003-ROUMANSEI-CHK-SEC
           END-IF
      *
      *    在宅末期医療総合診察料チェック
           PERFORM 1008-ZAIMATU-CHK-SEC
      *
      *    生活習慣病指導管理料算定チェック
           PERFORM 1007-SEIKATU-CHK-SEC
      *
      *    慢性疼痛疾病管理料算定チェック
           PERFORM 1004-TOUTUU-CHK-SEC
      *
      *    消炎鎮痛処置定チェック
           PERFORM 1005-SYOUEN-CHK-SEC
      *
      *    外来管理加算算定付加チェック
           PERFORM 1006-GAIRAI-CHK-SEC
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
      *    新規の時、受診なし
           IF      SPA-SYOYMD          =   SPACE
               GO      TO      1000-SINSATU-KAISU-EXT
           END-IF
      *
      *    １５歳になる年月日を計算
           MOVE    SPA-BIRTHDAY        TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  15
      *
      *    受診履歴より受診回数を求める
      *    受診歴を検索する（診療月１日から診療日までの範囲）
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-SRYYMD          TO  JYURRK-UPSRYYMD
      *
           MOVE    "JYURRK-KEY14"      TO  WRK-PATH
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
      *        保険により対象を決定する
               PERFORM 10000-HKNKBN-CHK-SEC
      *********MOVE    ZERO                TO  FLG-CHK
               IF      FLG-CHK             =   ZERO
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
                   END-IF
      *            訂正の時、訂正分は以降カウントしない
                   IF      (SPA-SRYYMD         =   JYURRK-SRYYMD) AND
                           (SPA-SRYKA          =   JYURRK-SRYKA ) AND
                           (SPA-RENNUM         <   JYURRK-RENNUM)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
      *
      *        受診回数
               IF      FLG-CHK         =   ZERO
                   ADD     1               TO  SPA-SINSATU-CNT
               END-IF
      *        当日算定回数
               IF     (FLG-CHK         =   ZERO )  AND
                      (SPA-SRYYMD      =   JYURRK-SRYYMD)
                   ADD     1                  TO  WRK-DAY-SINSATUCNT
               END-IF
      *
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    １５歳未満再診科算定有り
           IF     (SPA-NENREI-YY       =   15              )  AND
                  (WRK-BIRTHDAY(1:6)   =   SPA-SRYYMD (1:6))
               IF      FLG-15MIMAN         =   1
                   MOVE    1               TO  SPA-15MIMAN-ARI
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
              WHEN     1
      *           自賠責
                  IF      SPA-RSI-HKNKBN       =   "4"
                      IF      JYURRK-HKNKBN    NOT =   2
                           MOVE    1               TO  FLG-CHK
                      END-IF
                  ELSE
      *           労災
                      IF      JYURRK-HKNKBN    NOT =   1
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
               PERFORM
                       UNTIL  (FLG-SRYACT      =   1     )  OR
                              (FLG-SINSATU     =   1     )
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL   (IDY        >   5   )  OR
                                   (FLG-SINSATU          =   1     )  OR
                                   (SRY-SRYCD (IDY)      =   SPACE )
                       IF      SPA-HKNKBN          =   ZERO
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
      *        １５歳未満の受診あり
               IF      JYURRK-SRYYMD       <   WRK-BIRTHDAY
                   MOVE    1               TO  FLG-15MIMAN
               END-IF
           ELSE
               EVALUATE    SRY-SRYCD (IDY)
      *
      *        寝たきり老人訪問診察料
      ************ WHEN    "114700110"
      *************WHEN    "114701110"
      *            在宅患者訪問診察料
                   WHEN    "114001110"
      *            在宅末期医療総合診察料
                   WHEN    "114007610"
                   WHEN    "114007710"
                       MOVE    1               TO  FLG-SINSATU
      *            ＤＵＭＭＹ診察料
                   WHEN    "099110001"
                   WHEN    "099120001"
                       MOVE    1               TO  FLG-SINSATU
               END-EVALUATE
           END-IF
      *    初診料は算定外とする
           IF     (SRY-SRYKBN          =   "11" )  OR
                  (SRY-SRYCD(IDY)      =   "113003510"  OR
                                           "113003710" )
               MOVE    1               TO  FLG-SYOSIN
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
           IF      WRK-KBN             =   "S"  OR  "F"
               MOVE    1              TO  FLG-ARI
           ELSE
               MOVE    ZERO           TO  FLG-ARI
           END-IF
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
           END-IF
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
      *    小児科外来診療科回数処理
      *****************************************************************
       1001-SYONIKA-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-SYONIKA-ARI
           MOVE    ZERO                TO  SPA-SYONIKA-ARI3
      *
      *    ３歳になる年月日を計算
           MOVE    SPA-BIRTHDAY        TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  3
      *    3歳未満に適用
           IF      WRK-BIRTHDAY(1:6)   <   SPA-SRYYMD (1:6)
               GO      TO      1001-SYONIKA-CHK-EXT
           END-IF
      *
      *    当月算定チェック
           MOVE    ZERO                TO  WRK-NYUYOJI
           MOVE    ZERO                TO  WRK-DAYCNT-SUM
      *    院外処方   初診
           MOVE    "113003510"         TO  WRK-SRYCD
           PERFORM 33031-JYURRK-CHK-SEC
           ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
           ADD     WRK-DAYCNT          TO  WRK-DAYCNT-SUM
      *    院外処方   再診
           MOVE    "113003610"         TO  WRK-SRYCD
           PERFORM 33031-JYURRK-CHK-SEC
           ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
           ADD     WRK-DAYCNT          TO  WRK-DAYCNT-SUM
      *    院内処方   初診
           MOVE    "113003710"         TO  WRK-SRYCD
           PERFORM 33031-JYURRK-CHK-SEC
           ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
           ADD     WRK-DAYCNT          TO  WRK-DAYCNT-SUM
      *    院内処方   再診
           MOVE    "113003810"         TO  WRK-SRYCD
           PERFORM 33031-JYURRK-CHK-SEC
           ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
           ADD     WRK-DAYCNT          TO  WRK-DAYCNT-SUM
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
      *****************************************************************
      *    寝たきり老人在宅総合診療科自動算定処理
      *****************************************************************
       1002-NETAKIRI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUG-SANTEI
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI2
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
      *    院外処方あり
           MOVE    "114701210"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >=  1
               MOVE    1                   TO  SPA-NETAKIRI-ARI2
           ELSE
               MOVE    WRK-SANTEI-YM       TO  WRK-SANTEI-ARI
      *        院外処方なし
               MOVE    "114701710"         TO  WRK-SRYCD
               PERFORM 33051-JYURRK-MAECHK-SEC
      *        当月算定あり
               IF      FLG-TOUG-SANTEI     >=  1
                   MOVE    2                   TO  SPA-NETAKIRI-ARI2
               END-IF
               MOVE    WRK-SANTEI-YM       TO  WRK-SANTEI-NASI
           END-IF
      *
      *    当月算定回数
           MOVE    WRK-SANTEI-CNT      TO  WRK-NETAKIRI-CNT
      *
           .
       1002-NETAKIRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    老人慢性疾患外来総合診療科算定回数処理
      *****************************************************************
       1003-ROUMANSEI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-ROUMANSEI
           MOVE    ZERO                TO  SPA-ROUMANSEI-ARI
           MOVE    ZERO                TO  SPA-ROUMANSEI-ARI2
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           MOVE    ZERO                TO  WRK-ROUMANSEI-CNT
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *    老人慢性疾病外来診療科の算定チェック
           MOVE    ZERO                TO  WRK-ROUMANSEI
           MOVE    ZERO                TO  WRK-ROUMANSEI2
      *
      *    院外処方
           MOVE    "113701010"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-ROUMANSEI
           END-IF
      *    院内処方１回目
           MOVE    "113701110"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-ROUMANSEI
               ADD     1                   TO  WRK-ROUMANSEI2
           END-IF
      *    院内処方２回目
           MOVE    "113701510"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-ROUMANSEI
           END-IF
      *
      *    算定回数
           MOVE    WRK-SANTEI-CNT      TO  WRK-ROUMANSEI-CNT
      *
      *    当月に算定時、フラグをセットする
           IF      WRK-ROUMANSEI       >   ZERO
               MOVE    1                   TO  SPA-ROUMANSEI-ARI
               IF      WRK-ROUMANSEI2      >   ZERO
                   MOVE    2                   TO  SPA-ROUMANSEI-ARI
               END-IF
           END-IF
      *
      *    算定回数セット
           MOVE    WRK-ROUMANSEI       TO  SPA-ROUMANSEI-ARI2
           .
       1003-ROUMANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅末期医療総合診察料チェック処理
      *****************************************************************
       1008-ZAIMATU-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI2
      *
      *    在宅末期医療総合診察料（院外処方）
           MOVE    "114007610"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               MOVE    1                   TO  SPA-ZAIMATU-ARI2
           END-IF
      *    在宅末期医療総合診察料（院内処方）
           MOVE    "114007710"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               MOVE    1                   TO  SPA-ZAIMATU-ARI2
           END-IF
           .
       1008-ZAIMATU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    生活習慣病指導管理料算定チェック処理
      *****************************************************************
       1007-SEIKATU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-SEIKATU-ARI
           MOVE    ZERO                TO  SPA-SEIKATU-ARI2
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           MOVE    ZERO                TO  WRK-SEIKATU-CNT
      *
      *    高血圧症（院外処方）
           MOVE    "113003910"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-SEIKATU-CNT
           END-IF
      *    高血圧症（院内処方）
           MOVE    "113004010"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-SEIKATU-CNT
           END-IF
      *
      *    高脂血症（院外処方）
           MOVE    "113005810"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-SEIKATU-CNT
           END-IF
      *    高脂血症（院内処方）
           MOVE    "113006010"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-SEIKATU-CNT
           END-IF
      *
      *    糖尿病（院外処方）
           MOVE    "113005910"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-SEIKATU-CNT
           END-IF
      *    糖尿病（院内処方）
           MOVE    "113006110"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-SEIKATU-CNT
           END-IF
      *
           IF      WRK-SEIKATU-CNT      >   ZERO
               MOVE    1               TO  SPA-SEIKATU-ARI
               MOVE    1               TO  SPA-SEIKATU-ARI2
           END-IF
           .
       1007-SEIKATU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性疼痛疾病管理料算定チェック処理
      *****************************************************************
       1004-TOUTUU-CHK-SEC              SECTION.
      *
      *    慢性疼痛疾病管理料
           MOVE    "113006510"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
      *    厚生省が定める患者である
           IF      FLG-TOUG-SANTEI     >   ZERO
               MOVE    1               TO  SPA-TOKUTEI-KNJ2
           ELSE
               MOVE    ZERO            TO  SPA-TOKUTEI-KNJ2
           END-IF
      *
           .
       1004-TOUTUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    消炎鎮痛処置算定チェック処理
      *****************************************************************
       1005-SYOUEN-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-SYOEN-CNT
      *
      *    器具等による療法
           MOVE    "140040310"         TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-SYOEN-CNT
           PERFORM 33031-JYURRK-CHK-SEC
           IF      WRK-SYOEN-CNT       >   ZERO
               ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
           END-IF
      *
      *    湿布処置１
           MOVE    "140002210"         TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-SYOEN-CNT
           PERFORM 33031-JYURRK-CHK-SEC
           IF      WRK-SYOEN-CNT       >   ZERO
               ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
           END-IF
      *
      *    湿布処置２
           MOVE    "140002310"         TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-SYOEN-CNT
           PERFORM 33031-JYURRK-CHK-SEC
           IF      WRK-SYOEN-CNT       >   ZERO
               ADD     WRK-SYOEN-CNT       TO  SPA-SYOEN-CNT
           END-IF
      *
           .
       1005-SYOUEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算算定付加チェック処理
      *****************************************************************
       1006-GAIRAI-CHK-SEC              SECTION.
      *
      *    痴呆患者在宅指導管理料
           MOVE    "113700310"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               MOVE    1               TO  SPA-GAIRAI-NASI
           ELSE
               MOVE    ZERO            TO  SPA-GAIRAI-NASI
           END-IF
      *
      *
           .
       1006-GAIRAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時の処理
      *****************************************************************
       1000-SANTEI-RSI-SEC              SECTION.
      *
      *    発生日を求める
      *    救急医療管理加算のチェック
           MOVE    SPACE               TO  WRK-HASSYOYMD
           MOVE    "101110030"         TO  WRK-SRYCD
           PERFORM 10001-RSI-JYURRK-CHK-SEC
           IF      WRK-HASSYOYMD       =   SPACE
               MOVE    SPA-RSI-SHOBYOYMD   TO  SPA-RSI-HASSYOYMD
      *********MOVE    ZERO                TO  SPA-RSI-KYUKASN
           ELSE
               MOVE    WRK-HASSYOYMD       TO  SPA-RSI-HASSYOYMD
      ******** MOVE    1                   TO  SPA-RSI-KYUKASN
      *        IF      SPA-SYORIFLG        =   1
      *            IF      WRK-HASSYOYMD       =   SPA-SRYYMD
      *                MOVE    ZERO                TO  SPA-RSI-KYUKASN
      *            END-IF
      *********END-IF
           END-IF
      *
      *    発生日から３ヶ月の計算
           MOVE    SPA-RSI-HASSYOYMD   TO  WRK-HASSYOYMD
           COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                       +   3
           IF      WRK-HAS-MM          >   12
               COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
               COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
           END-IF
           MOVE    WRK-HASSYOYMD       TO  SPA-RSI-HASSYOYMD3
      *
      *    傷病日から３ヶ月の計算
           MOVE    SPA-RSI-SHOBYOYMD   TO  WRK-HASSYOYMD
           COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                       +   3
           IF      WRK-HAS-MM          >   12
               COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
               COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
           END-IF
           MOVE    WRK-HASSYOYMD       TO  SPA-RSI-SHOBYOYMD3
      *
      *    平成１５年２月より
      *    自賠責時、６ヶ月とする（診療日がＨ１５．２以降）
           IF      (SPA-HKNKBN         =   1    )  AND
                   (SPA-RSI-HKNKBN     =   "4"  )  AND
                   (SPA-SRYYMD         >=  "20030201")
      *         傷病日判定
               IF      SPA-RSI-SHOBYOYMD  <   "20030201"
                   MOVE    "20030201"          TO  WRK-HASSYOYMD
               ELSE
                   MOVE    SPA-RSI-SHOBYOYMD   TO  WRK-HASSYOYMD
               END-IF
      *        傷病日から６ヶ月の計算
               COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                           +   6
               IF      WRK-HAS-MM          >   12
                   COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
                   COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
               END-IF
               MOVE    WRK-HASSYOYMD       TO  SPA-RSI-SHOBYOYMD3
      *
      *        発生日判定
               IF      SPA-RSI-HASSYOYMD   <   "20030201"
                   MOVE    "20030201"          TO  WRK-HASSYOYMD
               ELSE
                   MOVE    SPA-RSI-HASSYOYMD   TO  WRK-HASSYOYMD
               END-IF
      *        発生日から６ヶ月の計算
               COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                           +   6
               IF      WRK-HAS-MM          >   12
                   COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
                   COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
               END-IF
               MOVE    WRK-HASSYOYMD       TO  SPA-RSI-HASSYOYMD3
           END-IF
      *
      *    療養の給付請求書取扱料のチェック
      *****MOVE    SPACE               TO  WRK-HASSYOYMD
      *    MOVE    "101130020"         TO  WRK-SRYCD
      *    PERFORM 10001-RSI-JYURRK-CHK-SEC
      *    IF      WRK-HASSYOYMD       =   SPACE
      *        MOVE    ZERO                TO  SPA-RSI-KYUFU
      *    ELSE
      *        MOVE    1                   TO  SPA-RSI-KYUFU
      *        IF      SPA-SYORIFLG        =   1
      *            IF      WRK-HASSYOYMD       =   SPA-SRYYMD
      *                MOVE    ZERO                TO  SPA-RSI-KYUFU
      *            END-IF
      *        END-IF
      *    END-IF
      *
      *    手指の創傷に係る機能回復始動加算
      *    MOVE    SPACE               TO  WRK-HASSYOYMD
      *    MOVE    "101500080"         TO  WRK-SRYCD
      *    PERFORM 10001-RSI-JYURRK-CHK-SEC
      *    IF      WRK-HASSYOYMD       =   SPACE
      *        MOVE    ZERO                TO  SPA-RSI-KAIFUKU
      *    ELSE
      *        MOVE    1                   TO  SPA-RSI-KAIFUKU
      *        IF      SPA-SYORIFLG        =   1
      *            IF      WRK-HASSYOYMD       =   SPA-SRYYMD
      *                MOVE    ZERO                TO  SPA-RSI-KAIFUKU
      *            END-IF
      *        END-IF
      *****END-IF
      *
      *    受診履歴より診察回数を求める
           PERFORM 1000-SINSATU-KAISU-SEC
      *
      *    慢性疼痛疾病管理料算定チェック
           PERFORM 1004-TOUTUU-CHK-SEC
      *
      *    消炎鎮痛処置定チェック
           PERFORM 1005-SYOUEN-CHK-SEC
      *
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
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
           END-SEARCH
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *
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
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      10001-RSI-JYURRK-CHK-EXT
           END-IF
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
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
      *    初診料自動発生処理
      *****************************************************************
       200-SYOSIN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *    MOVE    ZERO                TO  SPA-FRST-LINE
      *    MOVE    SPACE               TO  SPA-FRST-SRYCD
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    入院日の時、初診料のみ算定する
           IF      SPA-K02N-NYUINYMD   =   SPA-NAI-SRYYMD
               IF      SPA-SYOSIN          =   1
                   CONTINUE
               ELSE
                   GO      TO      200-SYOSIN-SET-EXT
               END-IF
           END-IF
      *
      *    労災の初診料算定
           IF      SPA-HKNKBN          =   1
               MOVE    SPACE               TO  WRK-KBN
               PERFORM 2009-RSIHKN-SET-SEC
               IF      SPA-GMN-IDX         >   ZERO
      *            療養担当手当発生
                   PERFORM 330123-RYOYOU-TEATE-SEC
      *
      *            MOVE    SPA-GMN-IDX         TO  SPA-FRST-LINE
      *            MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
      *                                        TO  SPA-FRST-SRYCD
               END-IF
               GO      TO      200-SYOSIN-SET-EXT
           END-IF
      *
      *    小児科外来診療科
           IF     (SPA-SYONIKA         =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )  AND
                  (FLG-SYONIKA-ARI     =   1   )
               PERFORM 3303-SYONIKA-SET-SEC
               IF      FLG-SYONIKA         =   1
      *************IF      SPA-GMN-IDX         >   ZERO
      *                MOVE    SPA-GMN-IDX         TO  SPA-FRST-LINE
      *                MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
      *                                            TO  SPA-FRST-SRYCD
      ************ END-IF
                   GO      TO      200-SYOSIN-SET-EXT
               END-IF
           END-IF
      *
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
                       (SPA-RRK-LSTYMD      =   SPA-SRYYMD )
      *??             ((SPA-LSTYMD          =   SPA-SRYYMD ) OR
      *??              (SPA-RRK-LSTYMD      =   SPA-SRYYMD ))
                   MOVE    1                   TO  FLG-DOUJITU
               END-IF
               IF      (SPA-SYORIFLG        =   1       )   AND
                       (WRK-DAY-SINSATUCNT  >   ZERO    )
                   MOVE    1                   TO  FLG-DOUJITU
               END-IF
               IF      FLG-DOUJITU         =   1
                   IF      SPA-TAKA-SRYKA-R    =   SPACE
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
      *
      *    初診料自動追加処理
           PERFORM 3301-SYOSIN-SYORI-SEC
      *
           IF      WRK-SRYCD(1:2)      =   "83"
               MOVE    "0109"              TO  SPA-KIDCD
           END-IF
      *    入院日の時、初診料のみ算定する
           IF      SPA-K02N-NYUINYMD   =   SPA-NAI-SRYYMD
               MOVE    SPACE               TO  SPA-KIDCD
           END-IF
      *
      *    指導料の自動発生
           PERFORM 33012-SIDOU-SYORI-SEC
      *
      *    IF      SPA-GMN-IDX         >   ZERO
      *        MOVE    SPA-GMN-IDX         TO  SPA-FRST-LINE
      *        MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
      *                                    TO  SPA-FRST-SRYCD
      **** END-IF
           .
       200-SYOSIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料をテーブルより決定処理
      *****************************************************************
       2001-SRYCD-TBL-SEC              SECTION.
      *
           IF      WRK-SRYKBN          =   "11"
               MOVE    1               TO  WRK-KAISU
           ELSE
               EVALUATE    SPA-SINSATU-CNT
                   WHEN    0
                       MOVE    1               TO  WRK-KAISU
                   WHEN    1
                   WHEN    2
                       MOVE    2               TO  WRK-KAISU
                   WHEN    OTHER
                       MOVE    4               TO  WRK-KAISU
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  WRK-KBN2
      *
      *    再診料振替え時のみ、判定
           IF      FLG-SAISIN-CHK      =   1
      *
      *    特定の患者（診療行為入力後に変更）
               IF     (SPA-TOKUTEI-KNJ     =   1  OR
                       SPA-TOKUTEI-KNJ2    =   1  )  AND
                      (WRK-KAISU           =   4  )
                   MOVE    4                   TO  WRK-KAISU
                   MOVE    "E"                 TO  WRK-KBN2
               END-IF
      *    人工腎臓の患者（診療行為入力後に変更）
               IF     (SPA-TOKUTEI-JNZ     =   1  )  AND
                      (WRK-KAISU           =   4  )
                   MOVE    4                   TO  WRK-KAISU
                   MOVE    "J"                 TO  WRK-KBN2
      *        同日再診の時は、コードがないので、特定の患者とする
                  IF      WRK-KBN             =   "D"
                       MOVE    "E"             TO  WRK-KBN2
                   END-IF
               END-IF
      *
           END-IF
      *    １５才未満
           IF    ((SPA-NENREI-YY       <   15 ) OR
                  (SPA-15MIMAN-ARI     =   1  )) AND
                 ( WRK-KAISU           =   4  )
               MOVE    4                   TO  WRK-KAISU
               MOVE    "M"                 TO  WRK-KBN2
           END-IF
      *
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN   (WRK-KAISU           =   TBL-KAISU (TBL-IDX))
                     AND  (SPA-NAI-ROUJIN      =   TBL-ROUJIN(TBL-IDX))
                     AND  (SPA-HOSPSBT         =   TBL-HOSPSBT(TBL-IDX))
                     AND  (WRK-KBN             =   TBL-KBN    (TBL-IDX))
                     AND  (WRK-KBN2            =   TBL-KBN2   (TBL-IDX))
                     AND  (WRK-SRYKBN          =   TBL-SRYCD(TBL-IDX)
                                                                (2:2))
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
           IF      SPA-SINSATU-CNT     <=  2
      *        ３回目まで
               MOVE    1               TO  WRK-KAISU
           ELSE
               MOVE    2               TO  WRK-KAISU
           END-IF
      *
      *    一般の時、病院・診療所は同じ
           IF      SPA-NAI-ROUJIN      =   ZERO
               MOVE    ZERO            TO  WRK-HOSPSBT
           ELSE
               MOVE    SPA-HOSPSBT     TO  WRK-HOSPSBT
           END-IF
      *
           MOVE    SPACE               TO  WRK-KBN2
      *
      *    再診料振替え時のみ、判定
           IF      FLG-SAISIN-CHK      =   1
      *        特定の患者
               IF     (SPA-TOKUTEIG-KNJ     =   1  OR
                       SPA-TOKUTEIG-KNJ2    =   1  )  AND
                      (WRK-KAISU           =   2  )
                   MOVE    2                   TO  WRK-KAISU
                   MOVE    "E"                 TO  WRK-KBN2
               END-IF
           END-IF
      *    １５才未満
           IF    ((SPA-NENREI-YY       <   15  ) OR
                  (SPA-15MIMAN-ARI     =   1  )) AND
                  (WRK-KAISU           =   2  )
               MOVE    2                   TO  WRK-KAISU
               MOVE    "M"                 TO  WRK-KBN2
           END-IF
      *
      *    診察料セット
           SET     TBLG-IDX             TO  1
           SEARCH  TBL-GAIRAI-OCC       VARYING   TBLG-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN   (WRK-KAISU           =   TBLG-KAISU 
                                                             (TBLG-IDX))
                     AND  (SPA-NAI-ROUJIN      =   TBLG-ROUJIN
                                                             (TBLG-IDX))
                     AND  (WRK-HOSPSBT         =   TBLG-HOSPSBT
                                                             (TBLG-IDX))
                     AND  (WRK-KBN2            =   TBLG-KBN2
                                                             (TBLG-IDX))
                       MOVE    TBLG-SRYCD (TBLG-IDX)
                                               TO  WRK-SRYCD
           END-SEARCH
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
      *    初診料自動追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *    以降の自動算定なし
           IF      FLG-ADD             =   ZERO
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
           ADD     1                   TO  IDX2
           IF      WRK-SRYCD(1:2)      =   "83"
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    以降、再診時のみ
           IF      SPA-SYOSIN          =   1
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
      *
      *    再診料算定（乳幼児加算）
           MOVE    "112000970"         TO  WRK-SRYCD
      *
      *    初診料自動追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
      *        自動追加分
               MOVE    "1"             TO  SPA-COM-AUTOKBN(SPA-GMN-IDX)
           END-IF
      *
      *    再診料算定（幼児加算）
           MOVE    "112001070"         TO  WRK-SRYCD
      *
      *    初診料自動追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
      *        自動追加分
               MOVE    "1"             TO  SPA-COM-AUTOKBN(SPA-GMN-IDX)
           END-IF
      *
      *    外来管理加算コード追加
           IF     (SPA-SYORIFLG        =   ZERO ) OR
                  (FLG-GAI             =   1   AND
                   SPA-SYORIFLG        =   1    )
               PERFORM 2002-SRYCD-TBLG-SEC
           END-IF
      *
           PERFORM 3301-SYOSIN-HEN-SEC
           MOVE    ZERO                TO  WRK-GAIRAI-IDX
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
      *        自動追加分
      *********MOVE    "1"             TO  SPA-COM-AUTOKBN(SPA-GMN-IDX)
               MOVE    SPA-GMN-IDX     TO  WRK-GAIRAI-IDX
           END-IF
      *
      *
      *    継続管理加算追加
      *    　初診料算定月の判定
           IF      SPA-SYOYMD(1:6)     =   SPA-SRYYMD(1:6)
               IF      SPA-SYOYMD      <=  SPA-SRYYMD
                   CONTINUE
               ELSE
      *            継続管理加算追加処理
                   PERFORM 3302-KEIZOKU-HEN-SEC
               END-IF
           ELSE
      *        継続管理加算追加処理
               PERFORM 3302-KEIZOKU-HEN-SEC
           END-IF
      *
      *    寝たきり老人在宅総合診療科算定処理
           IF     (SPA-NAI-ROUJIN      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               MOVE    ZERO            TO  WRK-IDX-IN
               PERFORM 3305-NETAKIRI-SET-SEC
               IF     (SPA-NETAKIRI-ARI    NOT =   ZERO )  OR
                      (SPA-NETAKIRI-ARI2   NOT =   ZERO )
      *        寝たきり老人が当月算定済みの時、老人慢性疾患を算定しない
                   GO      TO      3301-SYOSIN-SYORI-EXT
               END-IF
           END-IF
      *
      *    老人慢性疾患外来総合診療科算定
           IF     (SPA-ROUMANSEI       =   1   )  AND
                  (SPA-SYOSIN      NOT =   1   )  AND
                  (SPA-NAI-ROUJIN      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               MOVE    ZERO                TO  WRK-IDX-IN
               PERFORM 3304-ROUMANSEI-SET-SEC
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *    老人慢性疾患外来総合診療科が当月算定時で３回目の再診料算算定
      *           以外の時、外来管理加算は削除
           IF     (SPA-ROUMANSEI-ARI   =   ZERO )  AND
                  (SPA-ROUMANSEI-ARI2  NOT =   ZERO )  AND
                  (SPA-SINSATU-CNT     >=  3    )  AND
                  (WRK-GAIRAI-IDX      >   ZERO )
               MOVE    1                   TO  SPA-NAI-GAIFLG
               INITIALIZE              SPA-GMN-TBLREC (WRK-GAIRAI-IDX)
               INITIALIZE              SPA-COM-TBLREC (WRK-GAIRAI-IDX)
           END-IF
      *
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
      *    初診料自動算定処理
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
               GO      TO      3301-SYOSIN-HEN-EXT
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
      *    継続管理加算追加処理
      *****************************************************************
       3302-KEIZOKU-HEN-SEC                SECTION.
      *
           IF      SPA-NAI-ROUJIN      =   ZERO
      *        一般
               MOVE    "112007170"         TO  WRK-SRYCD
           ELSE
      *        老人
               MOVE    "112702270"         TO  WRK-SRYCD
           END-IF
      *
      *    初診料自動追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
      *        自動追加分
      *********MOVE    "1"             TO  SPA-COM-AUTOKBN(SPA-GMN-IDX)
           END-IF
      *
           .
       3302-KEIZOKU-HEN-EXT.
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
      *     院外処方
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
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           IF      WRK-IDX-IN          =   ZERO
      *        初期算定
               MOVE    "1"                 TO  LNK-SC92-KBN
               MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           ELSE
      *        院外院内変更
               MOVE    SPACE               TO  LNK-SC92-KBN
               MOVE    WRK-IDX-IN          TO  LNK-SC92-GMN-IDX
               INITIALIZE                      SPA-COM-TBLREC
                                                        (WRK-IDX-IN)
               MOVE    WRK-SRYCD           TO  SPA-COM-INPUTCD
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
               MOVE    SPACE               TO  LNK-SC92-ERRCD5
           END-IF
      *
      *    同日再診時、併用算定エラー
           IF      LNK-SC92-ERRCD3 NOT =   SPACE
               MOVE    SPACE               TO  SPA-ERRCD
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
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
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    ZERO            TO  SPA-GMN-IDX
               MOVE    ZERO            TO  IDX2
               GO      TO      3303-SYONIKA-SET-EXT
           END-IF
      *
      *    算定があり、既に当日他の診察料を算定している時、エラー
           IF     (SPA-SYOSIN          =   ZERO  )  AND
                  (SPA-LSTYMD          =   SPA-SRYYMD)  AND
                  (SPA-SYORIFLG        =   ZERO   )
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    1               TO  FLG-SYONIKA
               MOVE    ZERO            TO  SPA-GMN-IDX
               MOVE    ZERO            TO  IDX2
               MOVE    "K005"          TO  SPA-ERRCD
               GO      TO      3303-SYONIKA-SET-EXT
           END-IF
      *
      *    入力項目編集処理
      *    INITIALIZE                  WRK-MOJI-AREA
      *    PERFORM 800-KOMOKU-HENSYU-SEC
      *
      *    再編集処理
           MOVE    SPA-GMN-IDX         TO  IDX
           PERFORM 3101-INPUTCD-HEN-SEC
      *
           MOVE    1                   TO  FLG-SYONIKA
           MOVE    1                   TO  SPA-SYONIKA-ARI
           .
       3303-SYONIKA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       33031-JYURRK-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURR-ARI
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      33031-JYURRK-CHK-EXT
           END-IF
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
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
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SANTEI          =   1
               MOVE    SPACE               TO  SANTEI-REC
               INITIALIZE              SANTEI-REC
           END-IF
      *
      *    算定歴の回数をチェックする
      *    初回算定日＜診療日であること
           IF     (SANTEI-MSANTEID     >   ZERO           ) AND
                  (SANTEI-MSANTEID     <   SPA-SRYYMD(7:2))
               MOVE    1                   TO  FLG-JYURR-ARI
           END-IF
      *    上限１日
           MOVE    ZERO                TO  WRK-DAYCNT
           IF     (TNS-JGNCNT1D        >   ZERO  )  AND
                  (SPA-SRYYMD(7:2)     NUMERIC   )  AND
                  (SPA-SRYYMD(7:2)     >   ZERO  )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               MOVE    SANTEI-DAY (IDX-Y2) TO  WRK-DAYCNT
           END-IF
      *
      *    消炎鎮痛処置の時、訂正日までの回数とする
           MOVE    ZERO                TO  WRK-SYOEN-CNT
           IF     (SPA-SYORIFLG        =   1     )  AND
                  (WRK-SRYCD(1:3)      =   "140" )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               PERFORM VARYING     IDX-Y3  FROM  1  BY  1
                       UNTIL       IDX-Y3  >=   IDX-Y2
                   ADD     SANTEI-DAY (IDX-Y3)
                                               TO  WRK-SYOEN-CNT
               END-PERFORM
           ELSE
               MOVE    SANTEI-MSANTEICNT   TO  WRK-SYOEN-CNT
           END-IF
      *
           .
       33031-JYURRK-CHK-EXT.
           EXIT.
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *****************************************************************
      *    老人慢性疾患外来総合診療科自動発生処理
      *****************************************************************
       3304-ROUMANSEI-SET-SEC          SECTION.
      *
      *    過去に算定が無い時算定なし
           IF     (WRK-ROUMANSEI-CNT   =   ZERO )  AND
                  (WRK-IDX-IN          =   ZERO )
               GO      TO      3304-ROUMANSEI-SET-EXT
           END-IF
      *    ２回以上は算定なし
           IF     (SPA-ROUMANSEI-ARI2  >=  2    )  AND
                  (WRK-IDX-IN          =   ZERO )
      *********OR (SPA-ROUMANSEI-ARI       =   ZERO)
               MOVE    ZERO            TO  SPA-ROUMANSEI-ARI
               GO      TO      3304-ROUMANSEI-SET-EXT
           END-IF
      *
      *    院外処方
           IF      SPA-INGAIKBN        =   "1"
               MOVE    "113701010"         TO  WRK-SRYCD
           ELSE
      *        院内処方
               IF      WRK-ROUMANSEI       =   ZERO
      *            １回目
                   MOVE    "113701110"         TO  WRK-SRYCD
               ELSE
      *            ２回目
                   MOVE    "113701510"         TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *    追加共通処理
           PERFORM 33041-SRYCD-ADD-SEC
      *    追加なし
           IF      FLG-ADD             =   ZERO
               GO      TO      3304-ROUMANSEI-SET-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROUMANSEI
      *
           IF      WRK-SRYCD           =   "113701110"  OR
                                           "113701510"
               MOVE    2                   TO  SPA-ROUMANSEI-ARI
           ELSE
               IF      SPA-ROUMANSEI-ARI   =   ZERO
                   MOVE    1                   TO  SPA-ROUMANSEI-ARI
               END-IF
           END-IF
      *
      *    外来管理加算のチェックを行う
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      TNS-GAIKANRIKBN     =   ZERO
               MOVE    ZERO                TO  WRK-GAIRAI-IDX
           END-IF
      *
      *    外来加算料を削除する
           IF      WRK-GAIRAI-IDX      >   ZERO
               INITIALIZE              SPA-GMN-TBLREC (WRK-GAIRAI-IDX)
               INITIALIZE              SPA-COM-TBLREC (WRK-GAIRAI-IDX)
           END-IF
           .
       3304-ROUMANSEI-SET-EXT.
           EXIT.
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
               INITIALIZE                      SPA-COM-TBLREC
                                                        (WRK-IDX-IN)
               MOVE    WRK-SRYCD           TO  SPA-COM-INPUTCD
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
               GO      TO      33041-SRYCD-ADD-EXT
           END-IF
      *
           MOVE    1                  TO  FLG-ADD
      *
           .
       33041-SRYCD-ADD-EXT.
           EXIT.
      *****************************************************************
      *    寝たきり老人在宅総合診療科自動算定処理
      *****************************************************************
       3305-NETAKIRI-SET-SEC          SECTION.
      *
      *    当月算定済み 自動作成なし
           IF    ((SPA-NETAKIRI-ARI2   NOT =   ZERO )  OR
                  (WRK-NETAKIRI-CNT        =   ZERO ))  AND
                  (WRK-IDX-IN              =   ZERO )
               GO      TO      3305-NETAKIRI-SET-EXT
           END-IF
      *
      *    自動算定
           IF    ( WRK-SANTEI-ARI      >   WRK-SANTEI-NASI )  OR
                 ( SPA-INGAIKBN        =   "1"             )
      *        院外処方あり
               MOVE    "114701210"         TO  WRK-SRYCD
               MOVE    1                   TO  SPA-NETAKIRI-ARI
           ELSE
      *        院外処方なし
               MOVE    "114701710"         TO  WRK-SRYCD
               MOVE    2                   TO  SPA-NETAKIRI-ARI
           END-IF
      *
      *    追加共通処理
           PERFORM 33041-SRYCD-ADD-SEC
      *    追加なし
           IF      FLG-ADD             =   ZERO
               GO      TO      3305-NETAKIRI-SET-EXT
           END-IF
      *
           .
       3305-NETAKIRI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理（前履歴）
      *****************************************************************
       33051-JYURRK-MAECHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUG-SANTEI
           MOVE    SPACE               TO  WRK-SANTEI-YM
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      33051-JYURRK-MAECHK-EXT
           END-IF
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
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
           MOVE    SPACE               TO  WRK-FSANTEIYMD
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6))
                   ADD     1                   TO  WRK-SANTEI-CNT
      *            当月算定済み
                   IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                       MOVE    SANTEI-SRYYM        TO  WRK-SANTEI-YM
                       MOVE    SANTEI-MSANTEICNT   TO  FLG-TOUG-SANTEI
      *                慢性疼痛の時、診療日より後の時なしとする
                       IF     (WRK-SRYCD       =   "113006510" ) AND
                              (SANTEI-FSANTEIYMD(1:6)
                                               =   SANTEI-SRYYM ) AND
                              (SPA-SRYYMD(7:2) <   SANTEI-MSANTEID) AND
                              (WRK-FSANTEIYMD  NOT =
                                                   SANTEI-FSANTEIYMD)
                           MOVE    ZERO        TO  FLG-TOUG-SANTEI
                       END-IF
                   END-IF
                   IF      SANTEI-SRYYM        <   SPA-SRYYMD(1:6)
                       MOVE    SANTEI-SRYYM        TO  WRK-SANTEI-YM
                       MOVE    1                   TO  FLG-SANTEI
                   END-IF
                   MOVE    SANTEI-FSANTEIYMD   TO  WRK-FSANTEIYMD
      *
      *            訂正の時、訂正分はカウントしない
                   IF     (SPA-SYORIFLG        =   1     )  AND
                          (SANTEI-SRYYM        =   SPA-SRYYMD(1:6))
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       IF     (SANTEI-DAY (IDX-Y2) >   ZERO )  AND
                              (FLG-TOUG-SANTEI     >   ZERO )
                           COMPUTE FLG-TOUG-SANTEI
                                           =   FLG-TOUG-SANTEI
                                           -   SANTEI-DAY (IDX-Y2)
                       END-IF
                   END-IF
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
       33051-JYURRK-MAECHK-EXT.
           EXIT.
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *
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
               PERFORM 330122-HIFUKA-SIDOU-SEC
           END-IF
      *
      *    療養担当手当（北海道）
           PERFORM 330123-RYOYOU-TEATE-SEC
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
           EVALUATE    SPA-NAI-ROUJIN  ALSO  SPA-HOSPSBT
      *        老人　病院
               WHEN    1        ALSO         1
                   IF      SPA-BEDSU           <   100
                       MOVE    "113700710"         TO  WRK-SRYCD
                   ELSE
                       MOVE    "113700810"         TO  WRK-SRYCD
                   END-IF
      *        老人、診療所
               WHEN    1        ALSO         2
                   MOVE    "113700610"         TO  WRK-SRYCD
      *        一般　病院
               WHEN    ZERO     ALSO         1
                   IF      SPA-BEDSU           <   100
                       MOVE    "113001910"         TO  WRK-SRYCD
                   ELSE
                       MOVE    "113002010"         TO  WRK-SRYCD
                   END-IF
      *        一般、診療所
               WHEN    ZERO     ALSO         2
                   MOVE    "113001810"         TO  WRK-SRYCD
           END-EVALUATE
      *
      *    指導料追加、チェック処理
           MOVE    ZERO                TO  FLG-SIDOU
           PERFORM 3301211-SIDOU-ADD-SEC
           .
       330121-TOKUTEI-SIDOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料編集・チェック処理
      *****************************************************************
       3301211-SIDOU-ADD-SEC           SECTION.
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
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                       -   1
           END-IF
           MOVE    SPA-GMN-IDX         TO  IDX2
      *
           .
       330121-TOKUTEI-SIDOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料初診日チェック処理（ORCSC10 と同様のチェック）
      *****************************************************************
       330121-SIDOU-CHK-SEC               SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-KYUJITU
           IF      SPA-SYOYMD          =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMD
           ELSE
      *        初診料算定日＋１月後
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-SYOYMD          TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-KEIYMD
               IF      LNK-DAY6-KOYOMI NOT =   LNK-DAY6-KOYOMIZEN
                   MOVE    1                   TO  FLG-KYUJITU
               END-IF
           END-IF
      *
           EVALUATE    TRUE
               WHEN    WRK-KEIYMD          <   SPA-SRYYMD
                   CONTINUE
               WHEN    WRK-KEIYMD          >   SPA-SRYYMD
                   MOVE    "0034"          TO  SPA-ERRCD
               WHEN    WRK-KEIYMD          =   SPA-SRYYMD
      *            皮膚科の時、１月後
                   IF      FLG-SIDOU       =   1
                       MOVE    "0034"          TO  SPA-ERRCD
                   ELSE
      *            特定疾患の時、１月後が休日の時、ＯＫ
                       IF      FLG-KYUJITU     =   ZERO
                           MOVE    "0034"          TO  SPA-ERRCD
                       END-IF
                   END-IF
           END-EVALUATE
           .
       330121-SIDOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    皮膚科特定疾患自動算定処理
      *****************************************************************
       330122-HIFUKA-SIDOU-SEC         SECTION.
      *
           EVALUATE    SPA-TOKUTEI-F
      *           （１）
               WHEN    03
                   MOVE    "113000910"     TO  WRK-SRYCD
      *           （２）
               WHEN    04
                   MOVE    "113002310"     TO  WRK-SRYCD
           END-EVALUATE
      *
      *    指導料追加、チェック処理
           MOVE    1                   TO  FLG-SIDOU
           PERFORM 3301211-SIDOU-ADD-SEC
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
      *    初診料算定
           IF      SPA-SYOSIN          =   1
               MOVE    ZERO                TO  WRK-KAISU
               MOVE    "F"                 TO  WRK-KBN
               PERFORM 20091-RSI-SINSATU-SEC
           ELSE
      *        再診料
               IF     (SPA-LSTYMD          =   SPA-SRYYMD  ) AND
                      (SPA-TAKA-SRYKA-R    NOT =   SPACE   )
               AND    (WRK-KBN             =   SPACE   )
      *            同日再診・他科受診あり
                   PERFORM 3301-DOUSAISIN-SEC
               ELSE
                   IF      SPA-SRYYMD      <   SPA-RSI-HASSYOYMD3
      *                ３月以内
                       MOVE    ZERO                TO  WRK-KAISU
                       MOVE    "S"                 TO  WRK-KBN
                   ELSE
                       IF      SPA-SINSATU-CNT     >=  5
      *                ３月以降の同月５回目以降
                           MOVE    5                   TO  WRK-KAISU
                           MOVE    "S"                 TO  WRK-KBN
                       ELSE
                           MOVE    1                   TO  WRK-KAISU
                           MOVE    "S"                 TO  WRK-KBN
                       END-IF
                   END-IF
                   PERFORM 20091-RSI-SINSATU-SEC
               END-IF
           END-IF
      *    初診料自動追加処理
           PERFORM 3301-SYOSIN-HEN-SEC
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
           END-IF
      *
      *    入院日の時、初診料のみ算定する
           IF      SPA-K02N-NYUINYMD   =   SPA-NAI-SRYYMD
               GO      TO      2009-RSIHKN-SET-EXT
           END-IF
          IF      WRK-SRYCD(1:2)      =   "83"
               MOVE    "0109"              TO  SPA-KIDCD
               GO      TO      2009-RSIHKN-SET-EXT
           END-IF
      *
      *    以降、再診時のみ
           IF      SPA-SYOSIN          =   1
               GO      TO      2009-RSIHKN-SET-EXT
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
      *    継続管理加算追加
      *    　初診料算定月の判定
           IF      SPA-SYOYMD(1:6)     =   SPA-SRYYMD(1:6)
               IF      SPA-SYOYMD      <=  SPA-SRYYMD
                   CONTINUE
               ELSE
      *            継続管理加算追加処理
                   PERFORM 3302-KEIZOKU-HEN-SEC
               END-IF
           ELSE
      *        継続管理加算追加処理
               PERFORM 3302-KEIZOKU-HEN-SEC
           END-IF
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
                      (WRK-KBN         =   TBLR-KBN   (TBL-RSI))
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
      *    外来管理加算コード追加
           IF      SPA-SRYYMD      <   SPA-RSI-HASSYOYMD3
      *        ３月以内
               MOVE    ZERO                TO  WRK-KAISU
               MOVE    "G"                 TO  WRK-KBN
           ELSE
               IF      SPA-SINSATU-CNT     >=  5
      *        ３月以降の同月５回目以降
                   MOVE    5                   TO  WRK-KAISU
                   MOVE    "G"                 TO  WRK-KBN
               ELSE
                   MOVE    1                   TO  WRK-KAISU
                   MOVE    "G"                 TO  WRK-KBN
               END-IF
           END-IF
      *    テーブル検索
           PERFORM 20091-RSI-SINSATU-SEC
           PERFORM 3301-SYOSIN-HEN-SEC
           MOVE    ZERO                TO  WRK-GAIRAI-IDX
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
               MOVE    SPA-GMN-IDX     TO  WRK-GAIRAI-IDX
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
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
      *
           IF      SPA-HKNKBN          =   1
      *        労災の同日再診料算定
               MOVE    "D"                 TO  WRK-KBN
               PERFORM 2009-RSIHKN-SET-SEC
           ELSE
      *        同日再診料へ置換え
               MOVE    "12"                TO  WRK-SRYKBN
               MOVE    "D"                 TO  WRK-KBN
               MOVE    ZERO                TO  FLG-SAISIN-CHK
      *        診察コードをテーブルより決定
               PERFORM 2001-SRYCD-TBL-SEC
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
      *    院内院外変更処理
      *****************************************************************
       400-INGAI-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX-IN
           MOVE    ZERO                TO  WRK-ZAITAKU
           MOVE    ZERO                TO  WRK-ZAITAKU-IDX
           MOVE    ZERO                TO  WRK-GAIRAI-IDX
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
      *            老人慢性疾病外来総合診療科
                   WHEN    "113701010"
                   WHEN    "113701110"
                   WHEN    "113701510"
                       MOVE    IDX                 TO  WRK-IDX-IN
      *            在宅自己注射（院外）
      ***********  WHEN    "114009210"
      *                MOVE    IDX                 TO  WRK-ZAITAKU-IDX
      *                MOVE    1                   TO  WRK-ZAITAKU
      *            在宅自己注射（院ない）
      *            WHEN    "114003210"
      *                MOVE    IDX                 TO  WRK-ZAITAKU-IDX
      ***********      MOVE    0                   TO  WRK-ZAITAKU
      *
      *            生活習慣病指導管理料
                   WHEN    "113003910"
                   WHEN    "113005810"
                   WHEN    "113005910"
                   WHEN    "113004010"
                   WHEN    "113006010"
                   WHEN    "113006110"
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
      *    老人慢性疾患外来総合診療科自動発生処理
           IF      SPA-ROUMANSEI-ARI   NOT =   ZERO
               MOVE    1                   TO  WRK-ROUMANSEI-CNT
               PERFORM 3304-ROUMANSEI-SET-SEC
           END-IF
      *    寝たきり老人在宅総合診療科自動算定処理
           IF      SPA-NETAKIRI-ARI    NOT =   ZERO
      *        当月算定済み 自動作成なし
               MOVE    1                   TO  WRK-NETAKIRI-CNT
               MOVE    SPACE               TO  WRK-SANTEI-ARI
               MOVE    SPACE               TO  WRK-SANTEI-NASI
               PERFORM 3305-NETAKIRI-SET-SEC
           END-IF
      *
           IF      SPA-SEIKATU-ARI     NOT =   ZERO
      *        生活習慣病指導科振替え処理
               PERFORM 3306-SEIKATU-SET-SEC
           END-IF
      *
      *    在宅自己注射
      ***  IF      WRK-ZAITAKU-IDX   NOT =   ZERO
      *        PERFORM 4001-ZAITAKU-CHU-SEC
      **** END-IF
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
      *
      *****************************************************************
      *    同時再診算定処理（入力分）
      *****************************************************************
       500-INDOUSAISIN-SET-SEC         SECTION.
      *
      *
      *    電話再診かのチェック
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)   TO  WRK-SRYCD
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-KBN
                       MOVE    SPACE           TO  WRK-KBN2
                       MOVE    ZERO            TO  WRK-KAISU
                   WHEN   (TBL-SRYCD (TBL-IDX) =   WRK-SRYCD) 
                       MOVE    TBL-KBN   (TBL-IDX)     TO  WRK-KBN
                       MOVE    TBL-KBN2  (TBL-IDX)     TO  WRK-KBN2
                       MOVE    TBL-KAISU (TBL-IDX)     TO  WRK-KAISU
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
               GO      TO      500-TELDOUSAISIN-SET-EXT
           END-IF
      *
      *    電話再診以外の時、再度診察料算定へ
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
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
      *
           MOVE    ZERO                TO  SPA-SEIKATU-ARI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE     )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
      *
      *        特定の診療行為判定
               EVALUATE    SPA-COM-INPUTCD (IDX)
      *            寝たきり老人在宅総合診療科（院外処方箋あり）
                   WHEN    "114701210"
                       MOVE    1               TO  SPA-NETAKIRI-ARI
      *            寝たきり老人在宅総合診療科（院外処方箋なし）
                   WHEN    "114701710"
                       MOVE    2               TO  SPA-NETAKIRI-ARI
      *
      *            老人慢性疾病外来総合診療科（院外処方あり）
                   WHEN    "113701010"
                       MOVE    1               TO  SPA-ROUMANSEI-ARI
      *            老人慢性疾病外来総合診療科（院外処方なし）
                   WHEN    "113701110"
                   WHEN    "113701510"
                       MOVE    2               TO  SPA-ROUMANSEI-ARI
      *
      *            寝たきり老人訪問診察料
                   WHEN    "114700110"
                   WHEN    "114701110"
                       MOVE    1               TO  SPA-HOUMON-ARI
      *
      *            在宅患者訪問診察料
                   WHEN    "114001110"
                       MOVE    1               TO  SPA-HOUMON-ARI
      *
      *            在宅末期医療総合診察料
                   WHEN    "114007610"
                   WHEN    "114007710"
                       MOVE    1               TO  SPA-HOUMON-ARI
                       MOVE    1               TO  SPA-ZAIMATU-ARI
      *
      *            生活習慣病指導管理料
                   WHEN    "113003910"
                   WHEN    "113005810"
                   WHEN    "113005910"
                   WHEN    "113004010"
                   WHEN    "113006010"
                   WHEN    "113006110"
                       MOVE    1               TO  SPA-SEIKATU-ARI
      *
      *            小児科外来診療科算定
      *************WHEN    "113003510"
      *            WHEN    "113003610"
      *            WHEN    "113003710"
      *            WHEN    "113003810"
      *                MOVE    1               TO  SPA-SYONIKA-ARI
      *
      *            電話再診算定
      *            WHEN    "112002850"
      *            WHEN    "112002950"
      *            WHEN    "112700450"
      *            WHEN    "112700750"
      *                同日電話再診
      *            WHEN    "112003050"
      *            WHEN    "112003150"
      *            WHEN    "112700650"
      *            WHEN    "112700950"
      ******************OVE    1               TO  SPA-TELSAI-ARI
      *                同日他科受診
                   WHEN    "830000020"
                   WHEN    "830000021"
                       MOVE    1               TO  SPA-FUKU-SRYKA
      *            慢性疼痛疾患管理料
                   WHEN    "113006510"
                       MOVE    1               TO  SPA-TOKUTEI-KNJ
               END-EVALUATE
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
                           MOVE    1               TO  SPA-TELSAI-ARI
      **************** WHEN    SPACE
                       WHEN    OTHER
      *                小児科外来診療科算定
                           IF      SPA-COM-INPUTCD (IDX)(1:3)
                                                       =   "113"
                               MOVE    1           TO  SPA-SYONIKA-ARI
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
           IF      SPA-SYONIKA-ARI     =   ZERO
               MOVE    SPA-SYONIKA-ARI2    TO  SPA-SYONIKA-ARI
           END-IF
           IF      SPA-SYONIKA-ARI     =   ZERO
               MOVE    SPA-SYONIKA-ARI3    TO  SPA-SYONIKA-ARI
           END-IF
           .
       600-SANTEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *     剤分離後の特定算定チェック処理
      *****************************************************************
       700-SANTEI-CHK-SEC              SECTION.
      *
      *    小児科外来診療チェック
           PERFORM 5019-SYONIKA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      700-SANTEI-CHK-EXT
           END-IF
      *
      *    老人慢性疾病外来総合診療科チェック
           IF     (SPA-ROUMANSEI-ARI   NOT =   ZERO )  OR
                  (SPA-ROUMANSEI-ARI2  NOT =   ZERO )
               PERFORM 5020-ROUMANSEI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      700-SANTEI-CHK-EXT
               END-IF
           END-IF
      *
      *    寝たきり老人在宅総合診療科チェック
           IF     (SPA-NETAKIRI-ARI    NOT =   ZERO )  OR
                  (SPA-NETAKIRI-ARI2   NOT =   ZERO )
               PERFORM 5021-NETAKIRI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      700-SANTEI-CHK-EXT
               END-IF
           END-IF
      *
      *    生活習慣病指導管理科チェック
           IF     (SPA-SEIKATU-ARI     NOT =   ZERO )  OR
                  (SPA-SEIKATU-ARI2    NOT =   ZERO )
               PERFORM 5022-SEIKATU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      700-SANTEI-CHK-EXT
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
                   UNTIL      (IDX         >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *      自費は対象外
             IF      SPA-COM-SRYKBN(IDX)     =   "95" OR
                                                 "96" OR
                                                 "99"
                 CONTINUE
             ELSE
      *        小児科外来診療科の時、小児科外来診療科のみであること
               IF      SPA-SYONIKA-ARI     NOT =   ZERO
      *************EVALUATE    SPA-COM-INPUTCD (IDX)(1:1)
                   EVALUATE    SPA-COM-SRYKBN  (IDX)
                       WHEN    "11"
                       WHEN    "12"
                       WHEN    "13"
      *                    特定の診療行為であること
                           EVALUATE    SPA-COM-INPUTCD (IDX)
                               WHEN    "113003610"
                               WHEN    "113003510"
                               WHEN    "113003810"
                               WHEN    "113003710"
      *                            同日再診の時、診察料は算定できない
                                   IF      SPA-SYONIKA-ARI     =   2
                                       MOVE    "0046"  TO  SPA-ERRCD
                                       MOVE    "1"     TO  SPA-COM-CUR
                                                                 (IDX)
                                   END-IF
                               WHEN    "111011170"
                               WHEN    "111010370"
                               WHEN    "112005770"
                               WHEN    "113005270"
                               WHEN    "111011070"
                               WHEN    "111010470"
                               WHEN    "111010970"
                               WHEN    "112006070"
                               WHEN    "113005570"
                               WHEN    "113005370"
                               WHEN    "112005870"
                               WHEN    "111010770"
                               WHEN    "111010870"
                               WHEN    "111010570"
                               WHEN    "113005170"
                               WHEN    "113005070"
                               WHEN    "113005470"
                               WHEN    "112005970"
                               WHEN    "111010670"
      *                        開放型病院共同指導料（１）
                               WHEN    "180010510"
                                   CONTINUE
                               WHEN    OTHER
                                   MOVE    ZERO            TO  FLG-OK
      *                            診察料検索
                                   PERFORM 50191-SRYCD-CHK-SEC
      *                            電話再診で、当日診察がないこと
                                   IF     (SPA-SYONIKA-ARI NOT =   2 )
                                     AND  (SPA-SYONIKA-ARI2    =   ZERO)
                                     AND  (WRK-KBN         =   "T" OR
                                                               "S"  )
                                       MOVE    1           TO  FLG-KBN
                                       MOVE    1           TO  FLG-OK
                                   END-IF
      *                            種別のみはＯＫ
                                   IF    ( SPA-GMN-INPUTCD(IDX)(1:4)
                                                         =   ".130"  )
                                       MOVE    1           TO  FLG-OK
                                   END-IF
      *                            電話再診の加算はＯＫ
                                   IF     (FLG-KBN         =   1  ) AND
                                          (SPA-COM-SRYKBN(IDX)
                                                           =   "12") AND
                                          (SPA-COM-DATAKBN(IDX)
                                                           =   "2" )
                                       MOVE    1           TO  FLG-OK
                                   END-IF
                                   IF      FLG-OK          =   1
                                       CONTINUE
                                   ELSE
                                       MOVE    "0044"  TO  SPA-ERRCD
                                       MOVE    "1"     TO  SPA-COM-CUR
                                                                (IDX)
                                   END-IF
      *****************************往診料であること
      *                            IF      SPA-COM-SRYKBN (IDX)
      *                                                    =  "14"
      *                                CONTINUE
      *****************************ELSE
      *                            種別のみはＯＫ
      *                            IF      SPA-GMN-INPUTCD(IDX)(1:4)
      *                                                  =   ".130"
      *                                CONTINUE
      *                            ELSE
      *                                MOVE    "0044"  TO  SPA-ERRCD
      *                                MOVE    "1"     TO  SPA-COM-CUR
      *                                                         (IDX)
      **************************** END-IF
                           END-EVALUATE
      *
      *                往診料であること
                       WHEN    "14"
                           CONTINUE
      *
      *****************WHEN    "6"
                       WHEN    "21"
                       WHEN    "22"
                       WHEN    "23"
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
                                                           "232" )
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
                       WHEN    OTHER
      *********************院外薬剤と往診料であること
      *                    IF    ((SPA-INGAIKBN          =   "1" ) AND
      *                           (SPA-COM-SRYSYUKBN(IDX)
      *                                                  =   "210" OR
      *                                                      "212" OR
      *                                                      "220" OR
      *                                                      "222" OR
      *                                                      "230" OR
      *                                                      "232" OR
      *                                                      "290" OR
      *                                                      "292"
      *                                                  OR  SPACE ) ))
      *                       OR  ( SPA-COM-SRYSYUKBN(IDX)
      *                                                  =   "212" OR
      *                                                      "222" OR
      *                                                      "232" OR
      *                                                      "130" OR
      *                                                      "140" )
      *                        CONTINUE
      *                    ELSE
      *                      処方のみはＯＫ
      *                      IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
      *******                                      NOT =   "3" 
      *                        MOVE    "0044"          TO  SPA-ERRCD
      *                        MOVE    "1"             TO  SPA-COM-CUR
      *                                                         (IDX)
      *                      END-IF
      *********************END-IF
      *                    種別のみはＯＫ
                           IF      SPA-GMN-INPUTCD(IDX)(1:4) =   ".130"
                               CONTINUE
                           ELSE
                               MOVE    "0044"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                (IDX)
                           END-IF
      *                    特定の診療行為であること
                           EVALUATE    SPA-COM-INPUTCD (IDX)
      *                        開放型病院共同指導料（１）
                               WHEN    "180010510"
                                   MOVE    SPACE       TO  SPA-ERRCD
                                   MOVE    SPACE       TO  SPA-COM-CUR
                                                                (IDX)
                           END-EVALUATE
                   END-EVALUATE
               ELSE
      *        小児科外来診療科以外の時、小児科外来診療科でないこと
                   EVALUATE    SPA-COM-INPUTCD (IDX)
                               WHEN    "113003610"
                               WHEN    "113003510"
                               WHEN    "113003810"
                               WHEN    "113003710"
                               WHEN    "111011170"
                               WHEN    "111010370"
                               WHEN    "112005770"
                               WHEN    "113005270"
                               WHEN    "111011070"
                               WHEN    "111010470"
                               WHEN    "111010970"
                               WHEN    "112006070"
                               WHEN    "113005570"
                               WHEN    "113005370"
                               WHEN    "112005870"
                               WHEN    "111010770"
                               WHEN    "111010870"
                               WHEN    "111010570"
                               WHEN    "113005170"
                               WHEN    "113005070"
                               WHEN    "113005470"
                               WHEN    "112005970"
                               WHEN    "111010670"
                                   MOVE    "0045"      TO  SPA-ERRCD
                                   MOVE    "1"         TO  SPA-COM-CUR
                                                                (IDX)
                   END-EVALUATE 
               END-IF
             END-IF
      *
             IF      SPA-COM-ZAIEND (IDX)  =   "1"
                  MOVE    ZERO                TO  FLG-KBN
             END-IF
           END-PERFORM
           .
       5019-SYONIKA-CHK-EXT.
           EXIT.
      *
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
                WHEN   SPA-COM-INPUTCD (IDX)
                                          =   TBL-SRYCD (TBL-IDX)
                    MOVE    1                  TO  FLG-ARI
                    MOVE    TBL-KBN (TBL-IDX)  TO  WRK-KBN
           END-SEARCH
           .
       50191-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人慢性疾病外来総合診療科チェックク処理
      *****************************************************************
       5020-ROUMANSEI-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
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
                                                       "232"  )
      *                    院外処方箋を交付しない場合の時、エラー
                           IF      SPA-ROUMANSEI-ARI   =   2
      *                      処方のみはＯＫ
                             IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0052"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                               (IDX)
                             END-IF
                           END-IF
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
           END-PERFORM
           .
       5020-ROUMANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    寝たきり老人在宅総合診療科チェック処理
      *****************************************************************
       5021-NETAKIRI-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE    )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *        検査、投薬は算定できない
               EVALUATE    SPA-COM-SRYKBN(IDX)
                   WHEN    "60"
                       MOVE    "0054"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
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
                                                       "232"  )
      *                    院外処方箋を交付しない場合の時、エラー
                           IF     (SPA-NETAKIRI-ARI   =   2 )  OR
                                  (SPA-NETAKIRI-ARI2  =   2 )
      *                      処方のみはＯＫ
                             IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0054"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                (IDX)
                             END-IF
                           END-IF
                       ELSE
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0054"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                             (IDX)
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM
           .
       5021-NETAKIRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    生活習慣病指導管理科チェック処理
      *****************************************************************
       5022-SEIKATU-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
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
                                                       "232"  )
      *                    院外処方箋を交付しない場合の時、エラー
                           IF     (SPA-SEIKATU-ARI     =   2 )  OR
                                  (SPA-SEIKATU-ARI2    =   2 )
      *                      処方のみはＯＫ
                             IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0068"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                (IDX)
                             END-IF
                           END-IF
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
           END-PERFORM
           .
       5022-SEIKATU-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    再診料の４回目以降の振替え処理
      *****************************************************************
       800-SAISIN-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-SIN
           MOVE    ZERO                TO  IDX-GAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE       )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
      *
      *        診察料検索
               IF      IDX-SIN             =   ZERO
                   SET     TBL-IDX             TO  1
                   SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                       AT  END
                           MOVE    SPACE           TO  WRK-KBN
                           MOVE    SPACE           TO  WRK-KBN2
                           MOVE    ZERO            TO  WRK-KAISU
                       WHEN   SPA-COM-INPUTCD (IDX)
                                              =   TBL-SRYCD (TBL-IDX)
                           MOVE    IDX             TO  IDX-SIN
                           MOVE    TBL-KAISU(TBL-IDX) TO  WRK-KAISU
                           MOVE    TBL-KBN (TBL-IDX)  TO  WRK-KBN
                           MOVE    TBL-KBN2(TBL-IDX)  TO  WRK-KBN2
                   END-SEARCH
               END-IF
      *
      *        外来管理加算
               IF      IDX-GAI             =   ZERO
                   SET     TBLG-IDX             TO  1
                   SEARCH  TBL-GAIRAI-OCC       VARYING   TBLG-IDX
                       AT  END
                           MOVE    ZERO         TO  IDX-GAI
                       WHEN   SPA-COM-INPUTCD (IDX)
                                               =   TBLG-SRYCD (TBLG-IDX)
                           MOVE    IDX             TO  IDX-GAI
                   END-SEARCH
               END-IF
      *        労災の時、労災のコードを振り替える
               IF     (SPA-HKNKBN          =   1   )  AND
                      (IDX-SIN             =   ZERO)
                   SET     TBL-RSI             TO  1
                   SEARCH  TBL-RSI-SINSATU-OCC     VARYING   TBL-RSI
                       AT  END
                           MOVE    ZERO            TO  IDX-SIN
                       WHEN   SPA-COM-INPUTCD (IDX)
                                                =  TBLR-SRYCD (TBL-RSI)
                           MOVE    IDX             TO  IDX-SIN
                   END-SEARCH
                END-IF
           END-PERFORM
      *
      *    労災の時、労災のコードを振り替える
           IF     (SPA-HKNKBN          =   1   )  AND
                  (IDX-SIN             >   ZERO)
               MOVE    TBLR-SRYCD (3)      TO  WRK-SRYCD
               MOVE    IDX-SIN             TO  WRK-IDX-IN
      *        追加共通処理
               PERFORM 33041-SRYCD-ADD-SEC
               GO      TO      800-SAISIN-HEN-EXT
            END-IF
      *
           IF      IDX-SIN             >   ZERO
      *        再診料変更
               MOVE    "12"                TO  WRK-SRYKBN
               MOVE    1                   TO  FLG-SAISIN-CHK
      *        診察コードをテーブルより決定
               PERFORM 2001-SRYCD-TBL-SEC
               IF     (WRK-SRYCD       NOT =   SPACE )  AND
                      (WRK-SRYCD       NOT =   SPA-COM-INPUTCD(IDX-SIN))
                   MOVE    IDX-SIN         TO  WRK-IDX-IN
      *            追加共通処理
                   PERFORM 33041-SRYCD-ADD-SEC
                END-IF
            END-IF
      *
      *
           IF      IDX-GAI             >   ZERO
      *        外来管理加算コード変更加
               MOVE    1                   TO  FLG-SAISIN-CHK
               PERFORM 2002-SRYCD-TBLG-SEC
               IF     (WRK-SRYCD       NOT =   SPACE )  AND
                      (WRK-SRYCD       NOT =   SPA-COM-INPUTCD(IDX-GAI))
                   MOVE    IDX-GAI         TO  WRK-IDX-IN
      *            追加共通処理
                   PERFORM 33041-SRYCD-ADD-SEC
               END-IF
           END-IF
      *
           .
       800-SAISIN-HEN-EXT.
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
      *    小児科外来診察料以外
               MOVE    "11"                TO  WRK-SRYKBN
               MOVE    "F"                 TO  WRK-KBN
      *        診察コードをテーブルより決定
               PERFORM 2001-SRYCD-TBL-SEC
           END-IF
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
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
      *
      *    エラー時、最初のエラーコードをセット
           IF      SPA-ERRCD           NOT =   SPACE
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
      *    中途終了選択時編集処理
      *****************************************************************
      *1000-WKSRYACT-HEN-SEC           SECTION.
      *
      *    MOVE    ZERO                TO  FLG-JIKAN
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    OPEN    INPUT              PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-PARA 
      *    END-IF
      *    MOVE    ZERO                TO  FLG-SINSATU-OK
      *    MOVE    ZERO                TO  LNK-SC10S-ZAINUM-AREA
      *    MOVE    ZERO                TO  IDX-WK
      *    PERFORM
      *        UNTIL   FLG-PARA            =   1
      *        IF      PARA-FILEMEI        =   "WKSRYACT"
      *            MOVE    PARA-DATA-REC       TO  WKSRYACT-REC
      *            PERFORM 10001-SRYCD-CHK-SEC
      *        END-IF
      *
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *
      *    CLOSE                       PARA-FILE
      *
      *    PERFORM 100-SANTEI-CHK-SEC
      *    診察料自動発生あり
      *    IF      FLG-SINSATU-OK      =   1
      *        PERFORM 200-SYOSIN-SET-SEC
      *        IF      FLG-JIKAN       NOT =   ZERO
      *            MOVE    SPA-GMN-INPUTCD(01) TO  WRK-INPUTCD
      *            MOVE    SPACE               TO  SPA-GMN-INPUTCD(01)
      *            MOVE    FLG-JIKAN           TO  SPA-GMN-INPUTCD(01)
      *                                                         (1:1)
      *            MOVE    WRK-INPUTCD         TO  SPA-GMN-INPUTCD(01)
      *                                                         (3:1)
      *        END-IF
      *        診察料算定済み
      *        MOVE    1               TO  LNK-SC10S-SINSATU
      *    END-IF
      *
      *    .
      *1000-WKSRYACT-HEN-EXT.
      *    EXIT.
      *****************************************************************
      *    診療コードチェック処理
      *****************************************************************
      *10001-SRYCD-CHK-SEC             SECTION.
      *
      *    MOVE    ZERO                TO  FLG-CHK
      *    MOVE    ZERO                TO  FLG-ARI
      *    MOVE    ZERO                TO  FLG-DUMMY
      *    診療区分
      *    IF      WKSRY-SRYKBN        =   "11"  OR  "12"  OR
      *                                    "13"  OR  "14"
      *        CONTINUE
      *    ELSE
      *        GO      TO      10001-SRYCD-CHK-EXT
      *    END-IF
      *
      *    PERFORM VARYING     IDY     FROM    1   BY  1
      *            UNTIL      (IDY         >   5   )   OR
      *                       (FLG-ARI     =   1   )   OR
      *                       (WKSRY-SRYCD (IDY)   =   SPACE )
      *
      *        診察料検索
      *        SET     TBL-IDX             TO  1
      *        SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
      *            AT   END
      *                MOVE    ZERO                TO  FLG-ARI
      *                MOVE    SPACE               TO  WRK-KBN
      *                MOVE    SPACE               TO  WRK-KBN2
      *            WHEN   WKSRY-SRYCD (IDY)    =   TBL-SRYCD (TBL-IDX)
      *                MOVE    1                   TO  FLG-ARI
      *                MOVE    TBL-KBN  (TBL-IDX)  TO  WRK-KBN
      *                MOVE    TBL-KBN2 (TBL-IDX)  TO  WRK-KBN2
      *        END-SEARCH
      *
      *        IF      FLG-ARI         =   ZERO
      *            労災コードチェック（初診）
      *            SET     TBL-RSI              TO  1
      *            SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
      *                AT  END
      *                    MOVE    ZERO               TO  FLG-ARI
      *                    MOVE    SPACE              TO  WRK-KBN
      *                WHEN    WKSRY-SRYCD (IDY)
      *                                       =   TBLR-SRYCD (TBL-RSI)
      *                    MOVE    1                  TO  FLG-ARI
      *                    MOVE    TBLR-KBN (TBL-RSI) TO  WRK-KBN
      *            END-SEARCH
      *            IF      WRK-KBN             =   "S"  OR  "F"
      *                MOVE    1               TO  FLG-ARI
      *            ELSE
      *                MOVE    ZERO            TO  FLG-ARI
      *            END-IF
      *        END-IF
      *        その他の自動発生分
      *        IF      FLG-ARI         =   ZERO
      *            IF     WKSRY-SRYCD (IDY)    =   "830000020"   OR
      *                                            "830000021"   OR
      *                                            "112007170"   OR
      *                                            "112702270"   OR
      *                                            "113701010"   OR
      *                                            "113701110"   OR
      *                                            "113701510"   OR
      *                                            "114701210"   OR
      *                                            "114701710"
      *                MOVE    1               TO  FLG-ARI
      *                MOVE    SPACE           TO  WRK-KBN
      *            END-IF
      *
      *        END-IF
      *        特定の診療行為判定
      *        EVALUATE    WKSRY-SRYCD (IDY)
      *            寝たきり老人在宅総合診療科（院外処方箋あり）
      *            WHEN    "114701210"
      *                MOVE    "1"             TO  SPA-INGAIKBN
      *            寝たきり老人在宅総合診療科（院外処方箋なし）
      *            WHEN    "114701710"
      *                MOVE    "0"             TO  SPA-INGAIKBN
      *            老人慢性疾病外来総合診療科（院外処方あり）
      *            WHEN    "113701010"
      *                MOVE    "1"             TO  SPA-INGAIKBN
      *            老人慢性疾病外来総合診療科（院外処方なし）
      *            WHEN    "113701110"
      *            WHEN    "113701510"
      *                MOVE    "0"             TO  SPA-INGAIKBN
      *
      *            小児科外来診療科算定
      *            WHEN    "113003510"
      *            WHEN    "113003610"
      *                MOVE    "1"             TO  SPA-INGAIKBN
      *            WHEN    "113003710"
      *            WHEN    "113003810"
      *                MOVE    "0"             TO  SPA-INGAIKBN
      *            ＤＵＭＭＹ診療科
      *            WHEN    "099110001"
      *            WHEN    "099120001"
      *                MOVE    1               TO  FLG-DUMMY
      *        END-EVALUATE
      *
      *        IF      FLG-ARI         =   1
      *             初診料以外の時、自動発生あり
      *             初診料算定で、既に当日受診がある時は対象
      *            IF    ((WRK-KBN             =   "F"   )  AND
      *                   (SPA-SRYYMD          =   SPA-LSTYMD))  OR
      *                  ( WRK-KBN         NOT =   "F"  )
      *                MOVE    1           TO  FLG-CHK
      *                MOVE    1           TO  FLG-SINSATU-OK
      *                IF     (WKSRY-INPUTCD(IDY)(1:1) NUMERIC  )  AND
      *                       (WKSRY-INPUTCD(IDY)(2:1) =   SPACE)
      *                    MOVE    WKSRY-INPUTCD(IDY)(1:1) TO  FLG-JIKAN
      *                END-IF
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *
      *    IF      FLG-SINSATU-OK      =   1
      *        IF     (WKSRY-SRYKBN    NOT =   "11" )  OR
      *               (FLG-CHK             =   1    )
      *            IF       (WKSRY-SRYKBN        =   "14" OR "13")  AND
      *                     (FLG-ARI             =   ZERO )
      *                CONTINUE
      *            ELSE
      *                ADD     1               TO  IDX-WK
      *                MOVE    WKSRY-ZAINUM    TO  LNK-SC10S-ZAINUM
      *                                                   (IDX-WK)
      *            END-IF
      *        END-IF
      *    END-IF
      *
      *    DUMMY診察料がある時自動発生はしない
      *    IF      FLG-DUMMY           =   1
      *        MOVE    ZERO            TO  FLG-SINSATU-OK
      *    END-IF
      *
      *    .
      *10001-SRYCD-CHK-EXT.
      *    EXIT.
      *****************************************************************
      *    診察料のみの変更処理
      *****************************************************************
       1010-SINSATU-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-GAI
           MOVE    ZERO                TO  FLG-DUMMY
           MOVE    ZERO                TO  FLG-SINSATU-ARI
           MOVE    SPACE               TO  WRK-KBN3
      *    訂正の時、初診料を算定中かの判定
           IF     (SPA-SYORIFLG        =   1    )  OR
                  (LNK-SC10S-SYORI     =   "W"  )
               PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE    )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
      *            診察料検索
                   SET     TBL-IDX             TO  1
                   SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                       AT   END
                           MOVE    ZERO                TO  FLG-ARI
                           MOVE    SPACE               TO  WRK-KBN
                       WHEN   SPA-COM-INPUTCD (IDX) =
                                             TBL-SRYCD (TBL-IDX)
                           MOVE    1                   TO  FLG-ARI
                           MOVE    TBL-KBN  (TBL-IDX)  TO  WRK-KBN
                           MOVE    1               TO  FLG-SINSATU-ARI
                           MOVE    TBL-KBN  (TBL-IDX)  TO  WRK-KBN3
                   END-SEARCH
      *
                   IF      FLG-ARI         =   ZERO
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
                               IF      WRK-KBN     NOT =   "G"
                                   MOVE    1           TO
                                                       FLG-SINSATU-ARI
                                   MOVE    TBLR-KBN (TBL-RSI)
                                                       TO  WRK-KBN2
                               END-IF
                       END-SEARCH
                   END-IF
                   IF      FLG-ARI             =   1
      *                初診
                       IF      WRK-KBN             =   "F"
                           MOVE    1               TO  SPA-SYOSIN
                       END-IF
      *                外来管理加算
                       IF      WRK-KBN             =   "G"
                           MOVE    1            TO  FLG-GAI
                       END-IF
                   END-IF
                   EVALUATE    SPA-COM-INPUTCD (IDX)
      *                    ＤＵＭＭＹ診察料
                       WHEN    "099110001"
                       WHEN    "099120001"
                           MOVE    1              TO  FLG-DUMMY
                   END-EVALUATE
      *            外来管理加算
                   SET     TBLG-IDX             TO  1
                   SEARCH  TBL-GAIRAI-OCC       VARYING   TBLG-IDX
                       AT  END
                           MOVE    ZERO          TO  FLG-ARI
                       WHEN   SPA-COM-INPUTCD (IDX)
                                            =   TBLG-SRYCD (TBLG-IDX)
                           MOVE    1            TO  FLG-GAI
                   END-SEARCH
               END-PERFORM
           END-IF
      *    中途終了で診察料がない時
           IF      LNK-SC10S-SYORI     =   "W"
               PERFORM 100-SANTEI-CHK-SEC
               PERFORM 600-SANTEI-CNT-SEC
           END-IF
      *    同時他科受診があった時は、診察料の変更をおこなわない
      *    （中途終了時以外）
           IF    ( LNK-SC10S-SYORI NOT =   "W"  AND "K")  AND
                 ((SPA-LSTYMD          =   SPA-SRYYMD) OR
                  (SPA-RRK-LSTYMD      =   SPA-SRYYMD))
               IF      SPA-TAKA-SRYKA-R    NOT =   SPACE
                   GO      TO      1010-SINSATU-HEN-EXT
               END-IF
           END-IF
      *    ＤＵＭＭＹ診察料の変更をおこなわない
           IF      FLG-DUMMY           =   1
               GO      TO      1010-SINSATU-HEN-EXT
           END-IF
      *    中途終了で診察料がない時
           IF     (LNK-SC10S-SYORI     =   "W"  )  AND
                  (FLG-SINSATU-ARI     =   ZERO )
               GO      TO      1010-SINSATU-HEN-EXT
           END-IF
      *    初診料算定で、既に当日受診がある時は対象
           IF     (LNK-SC10S-SYORI     =   "W"  )  AND
                  (SPA-SYOSIN          =   1    )
      *             初診料以外の時、自動発生あり
               IF      SPA-SRYYMD      NOT =   SPA-LSTYMD
                   GO      TO      1010-SINSATU-HEN-EXT
               ELSE
                   MOVE    ZERO            TO  SPA-SYOSIN
               END-IF
           END-IF
      *    中途終了で電話再診の時
           IF     (LNK-SC10S-SYORI     =   "W"  )  AND
                  (WRK-KBN3            =   "T"  OR  "S" )
               GO      TO      1010-SINSATU-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
           PERFORM 100-SANTEI-CHK-SEC
      *    診察料編集
           PERFORM 200-SYOSIN-SET-SEC
      *    診療行為内容の編集
           PERFORM 10102-SINSATU-CHENGE-SEC
           .
       1010-SINSATU-HEN-EXT.
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((HZN-COM-INPUTCD (IDX)   =   SPACE ) AND
                              (HZN-GMN-INPUTCD (IDX)   =   SPACE ))
      *
               MOVE    ZERO                    TO  FLG-ARI
               IF     (HZN-COM-SRYKBN(IDX)     =   "11"  OR  "12"
                                               OR  "13"  OR  "14" ) OR
      *                療養担当手当
                     ( HZN-COM-INPUTCD(IDX)     =   "199000610" )
                   MOVE    HZN-COM-INPUTCD(IDX) TO  WRK-SRYCD
                   IF      HZN-COM-INPUTCD(IDX) NOT =   SPACE
                       PERFORM 10101-SINSATU-CHK-SEC
                   END-IF
                   IF      FLG-ARI             =   1
                       IF     (HZN-GMN-INPUTCD(IDX)(1:1) NUMERIC  )  AND
                              (HZN-GMN-INPUTCD(IDX)(2:1) =   SPACE)
                           MOVE    HZN-GMN-INPUTCD(IDX)(1:1)
                                                  TO  FLG-JIKAN
                       END-IF
                       MOVE    1               TO  FLG-CHK
                   END-IF
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
                      (WRK-GMN-INPUTCD(IDX + 1)(1:1)  =  ".")
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
                   END-PERFORM
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX3
                   MOVE    ZERO                TO  FLG-CHK-ALL
                   MOVE    ZERO                TO  FLG-CHK
               END-IF
           END-PERFORM
      *    剤毎に対象を編集する
           IF      IDX3                >   ZERO
               PERFORM VARYING     IDX4    FROM   1   BY  1
                       UNTIL       IDX4    >   IDX3
                   ADD     1                   TO  IDX2
                   ADD     1                   TO  SPA-GMN-IDX
                   MOVE    WRK-GMN-TBL(IDX4)   TO  SPA-GMN-TBL
                                                            (IDX2)
               END-PERFORM
           END-IF
      *    時間外入力あり
           IF      FLG-JIKAN       NOT =   ZERO
               IF      SPA-COM-INPUTCD(01)(1:1)    =   "1"
                   MOVE    SPA-GMN-INPUTCD(01) TO  WRK-INPUTCD
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
               WHEN   WRK-SRYCD            =   TBL-SRYCD (TBL-IDX)
                   MOVE    1                   TO  FLG-ARI
           END-SEARCH
      *
           IF      FLG-ARI         =   ZERO
      *    外来管理加算
               SET     TBLG-IDX             TO  1
               SEARCH  TBL-GAIRAI-OCC       VARYING   TBLG-IDX
                   AT  END
                       MOVE    ZERO         TO  FLG-ARI
                   WHEN   WRK-SRYCD         =   TBLG-SRYCD (TBLG-IDX)
                       MOVE    1            TO  FLG-ARI
               END-SEARCH
           END-IF
      *
           IF      FLG-ARI         =   ZERO
      *        労災コードチェック
               SET     TBL-RSI              TO  1
               SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
                   AT  END
                       MOVE    ZERO               TO  FLG-ARI
                   WHEN    WRK-SRYCD       =   TBLR-SRYCD (TBL-RSI)
                       MOVE    1                  TO  FLG-ARI
               END-SEARCH
           END-IF
      *
           IF      FLG-ARI             =   1
               GO      TO      10101-SINSATU-CHK-EXT
           END-IF
      *    その他の自動発生分
           IF     WRK-SRYCD            =   "830000020"   OR
                                           "830000021"
               MOVE    1               TO  FLG-ARI
               GO      TO      10101-SINSATU-CHK-EXT
           END-IF
      *    その他の自動発生分
           IF     WRK-SRYCD            =   "112007170"   OR
                                           "112702270"
               MOVE    1               TO  FLG-ARI
               GO      TO      10101-SINSATU-CHK-EXT
           END-IF
      *
      *           老人慢性疾病外来総合診療科
           IF     WRK-SRYCD            =   "113701010"   OR
                                           "113701110"   OR
                                           "113701510"   OR
      *           寝たきり老人在宅総合診療科
                                           "114701210"   OR
                                           "114701710"   OR
      *            特定慢性疾患指導料
                                           "113700710"   OR
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
                   IF      WRK-SRYCD       =   SPA-COM-INPUTCD (IDXS)
                       MOVE    1               TO  FLG-ARI
                   END-IF
               END-PERFORM
      *********労災の時、削除
      *        IF      SPA-HKNKBN      NOT =   ZERO
      *            MOVE    1               TO  FLG-ARI
      *********END-IF
           END-IF
      *    その他同じコードがある時
           PERFORM VARYING     IDXS    FROM    1   BY  1
                   UNTIL      (IDXS    >   SPA-GMN-IDX )  OR
                              (FLG-ARI =   1           )
               IF      WRK-SRYCD       =   SPA-COM-INPUTCD (IDXS)
                   MOVE    1               TO  FLG-ARI
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
      *
           PERFORM 1010-SINSATU-HEN-SEC
      *
           .
       1014-SINSATU-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC              SECTION.
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
       910-TENSU-READ-EXT.
           EXIT.
      *
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
