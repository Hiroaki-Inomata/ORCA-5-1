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
       PROGRAM-ID.                 ORCSC10200204.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（診療）
      *                      Ｈ１４年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC−多々納  01/04/19  時間外加算の乳幼児加算の修正等
      * 02.00.00     MCC−多々納  01/08/09  全体の修正
      * 02.00.01     MCC−多々納  02/03/01  在宅薬剤、在宅器材の回数を１
      *                                     回のみへ
      * 02.00.02     MCC−多々納  02/04/02  慢性疼痛疾病管理料が有る時
      *                                     外来管理加算を削除する
      * 02.00.03     MCC-多々納   02/04/10  労災用追加
      * 02.00.04     NACL-多々納  02/08/09  労災チェック追加
      * 02.00.05     NACL-多々納  02/10/02  注加算チェックの追加
      *                                     （すべての診区のチェック）
      * 02.00.06     NACL−多々納 02/10/10  外来管理加算チェックの有無
      * 02.00.07     NACL−多々納 02/10/30  特定薬剤治療管理料の重複なし
      * 02.00.08     NACL−多々納 02/10/31  外来管理加算自動算定なしの追加
      * 02.00.09     NACL−多々納 02/11/11  ＤＵＭＭＹ診察料追加
      * 02.00.10     NACL−多々納 02/11/25  ＤＵＭＭＹ診察料のチェックを
      *                                     Ｋ０２のみとする
      * 02.00.11     NACL−多々納 02/12/06  初診の時労災の置換えをしない
      * 02.00.12     NACL-多々納  02/12/17  在宅指導料の追加（143)
      * 02.00.13     NACL-多々納  03/03/14  特定薬剤治療管理算定有無追加
      * 02.00.13     NACL-多々納  03/04/04  在宅加算料の算定チェック修正
      * 02.00.14     NACL-多々納  03/05/06  １月後の判定修正
      * 02.00.16     NACL-多々納  03/06/17  診療情報提供料重複ＯＫ
      * 02.00.17     NACL-多々納  05/11/24  MONFUNC 対応
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-SP              PIC 9(01).
      *
           03  FLG-KYUJITU         PIC 9(01).
           03  FLG-GAIKAN          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *    電話再診有無
           03  FLG-TELSAI          PIC 9(01).
      *    特定薬剤治療管理初回
           03  FLG-SYOKAI          PIC 9(01).
           03  FLG-SYOKAI2         PIC 9(01).
      *    指導料
           03  FLG-SIDOU           PIC 9(01).
      *
      *    外来管理加算
           03  FLG-GAIRAI          PIC 9(01).
      *    継続加算
           03  FLG-KEIZOK          PIC 9(01).
      *    労災加算
           03  FLG-RSIFLG          PIC 9(01).
      *
           03  FLG-ADD             PIC 9(01).
      *    他保険算定済み
           03  FLG-TAHOKEN         PIC 9(01).
      *    ＤＵＭＭＹ診察料用
           03  FLG-KENSIN          PIC 9(01).
           03  FLG-DUMMY           PIC 9(01).
           03  FLG-DUMMY-ARI       PIC 9(01).
           03  FLG-DELETE          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z2              PIC 9(04).
           03  IDX-Z3              PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(04).
           03  IDX-ZAI1            PIC 9(04).
           03  IDX-ZAI2            PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
           03  IDX-GAIKAN          PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-2               PIC 9(02).
      *    診察料の位置
           03  IDX-SINSATU          PIC 9(04).
      *    注加算位置
           03  IDX-CHU              PIC 9(04).
      *
           03  IDY                  PIC 9(04).
      *    DUMMY診察料チェック
           03  IDX-MAE-IDX          PIC 9(04).
           03  IDX-DUMMY            PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *    パス名
           03  WRK-DBPATH          PIC X(64).
      *
           03  WRK-IDX             PIC 9(03).
      *
           03  WRK-MOJI            PIC X(01). 
      *    加算エラー分
           03  WRK-KASAN-NASI      PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
      *    労災救急医療管理加算有無
           03  WRK-RSISYOSIN       PIC 9(04).
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
      *    往診時、所定点数加算分
           03  WRK-TENSU-O1        PIC 9(08).
      *    往診時、所定点数加算外
           03  WRK-TENSU-O2        PIC 9(08).
      *
           03  WRK-TENSU1          PIC 9(08).
      *
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-KEIYMD.
               05  WRK-KEI-YY          PIC 9(04).
               05  WRK-KEI-MM          PIC 9(02).
               05  WRK-KEI-DD          PIC 9(02).
           03  WRK-KEIYMDZEN           PIC X(08).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    乳幼児の自動加算コード
           03  WRK-NYU-SRYCD           PIC X(09).
      *    入力時表示用
           03  IDX-INCNT               PIC 9(04).
           03  WRK-HEN-INPUTCD         PIC X(22).
      *    労災用、外来管理読み替え算定
           03  WRK-GAI-CNT             PIC 9(04).
           03  WRK-GAI-IDX             PIC 9(04).
           03  WRK-GAI-TEN             PIC 9(08).
      *
           03  WRK-SYOKAI-YMD3.
               05  WRK-SYO-YY3         PIC 9(04).
               05  WRK-SYO-MM3         PIC 9(02).
               05  WRK-SYO-DD3         PIC 9(02).
      *
           03  WRK-SYOKAI-YMD          PIC X(08).
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    特定薬剤初回算定日
           03  WRK-SANTEI-YMD          PIC X(08).
           03  WRK-WYM.
               05  WRK-WYY             PIC X(02).
               05  WRK-WMM             PIC X(02).
           03  WRK-SYOKAI              PIC 9(04).
      *
      *    診察料算定用
           03  WRK-KBN                 PIC X(01).
           03  WRK-KBN2                PIC X(01).
           03  WRK-KAISU               PIC 9(04).
           03  WRK-SAN-KAISU           PIC 9(04).
           03  WRK-HOSPSBT             PIC 9(01).
           03  WRK-SINSATU-KBN         PIC X(01).
           03  WRK-SINSATU-KBN2        PIC X(01).
           03  WRK-SINSATU-KAISU       PIC 9(01).
      *    診察料チェック用
           03  WRK-KEICD               PIC X(04).
           03  WRK-KEICD-IDX           PIC 9(04).
      *厚生大臣が定める患者
           03  WRK-TOKUTEI-KNJ       PIC  9(01).
      *人工腎臓
           03  WRK-TOKUTEI-JNZ       PIC  9(01).
      *厚生大臣が定める患者（外来管理）
           03  WRK-TOKUTEIG-KNJ      PIC  9(01).
      *
      *    初診算定日
           03  WRK-SYOYMD              PIC X(08).
      *
      *    重複警句エラー用
           03  IDX-KEI                 PIC 9(04).
           03  FLG-KEI                 PIC 9(01).
      *
      *    保存用ＳＰＡ
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *    保存用ＳＰＡ
       01  HZN2-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN2-//.
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    特定薬剤管理加算料テーブル
       01  TBL-TOKUYAKU-AREA.
           03  FILLER              PIC X(09)   VALUE   "113000410".
           03  FILLER              PIC X(09)   VALUE   "180000110".
           03  FILLER              PIC X(09)   VALUE   "180000210".
           03  FILLER              PIC X(09)   VALUE   "113000510".
       01  TBL-TOKUYAKU-AREA-R     REDEFINES   TBL-TOKUYAKU-AREA.
           03  TBL-TOKUYAKU-OCC    OCCURS   4  INDEXED  TBL-TOK.
               05  TBL-TOKSRYCD    PIC X(09).
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
           03  FILLER              PIC X(14)   VALUE   "100  113003510".
      *    小児科外来診察料(院内処方 初診)
           03  FILLER              PIC X(14)   VALUE   "100  113003710".
      *    小児科外来診察料(院外処方 再診)
           03  FILLER              PIC X(14)   VALUE   "200  113003610".
      *    小児科外来診察料(院内処方 再診)
           03  FILLER              PIC X(14)   VALUE   "200  113003810".
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
               05  TBL-KBN2            PIC X(01).
      *        診療コード
               05  TBL-SRYCD           PIC X(09).
      *
       01  TBL-SIN-MAX                 PIC 9(04)   VALUE   84.
      *
      *
      *    外来管理加算・継続管理加算テーブル
       01  TBL-GAIRAI-AREA.
      *    一般（３回目まで）
           03  FILLER              PIC X(14)   VALUE   "100G 112011010".
      *    一般（４回目以降）
           03  FILLER              PIC X(14)   VALUE   "200G 112011110".
      *    一般（１５才未満）
           03  FILLER              PIC X(14)   VALUE   "200GM112011210".
      *    一般（厚生省が定める患者）
           03  FILLER              PIC X(14)   VALUE   "200GE112012910".
      *
      *    老人 病院（３回目まで）
           03  FILLER              PIC X(14)   VALUE   "111G 112705110".
      *    老人 病院（４回目以降）
           03  FILLER              PIC X(14)   VALUE   "211G 112705210".
      *    老人 病院（厚生省が定める患者）
           03  FILLER              PIC X(14)   VALUE   "211GE112706910".
      *
      *    老人 診療所（３回目まで）
           03  FILLER              PIC X(14)   VALUE   "112G 112705310".
      *    老人 診療所（４回目以降）
           03  FILLER              PIC X(14)   VALUE   "212G 112705410".
      *    老人 診療所（厚生省が定める患者）
           03  FILLER              PIC X(14)   VALUE   "212GE112707010".
      *
      *    継続管理加算（一般）
           03  FILLER              PIC X(14)   VALUE   "000K 112007170".
      *    継続管理加算（老人）
           03  FILLER              PIC X(14)   VALUE   "010K 112702270".
      *
      *
       01  TBL-GAIRAI-AREAR       REDEFINES   TBL-GAIRAI-AREA.
           03  TBL-GAIRAI-OCC          OCCURS  12   INDEXED  TBLG-IDX.
      *        回数
               05  TBLG-KAISU           PIC 9(01).
      *        老人一般区分（０：一般、１：老人）
               05  TBLG-ROUJIN          PIC 9(01).
      *        病院種別（１：病院、２：診療所）
               05  TBLG-HOSPSBT         PIC 9(01).
      *         :外来管理加算、K:継続管理加算
               05  TBLG-KBN             PIC X(01).
      *        M:１５才未満、E：厚生省が定める患者
               05  TBLG-KBN2            PIC X(01).
      *        診療コード
               05  TBLG-SRYCD           PIC X(09).
      *
      *
      *    労災診察料テーブル
       01  TBL-RSI-SINSATU-AREA.
      *    初診料
           03  FILLER              PIC X(13)   VALUE   "0 F 101110010".
      *    再診料（３月以内）
           03  FILLER              PIC X(13)   VALUE   "0 S 101120040".
      *    再診料（３月超）５回以内
           03  FILLER              PIC X(13)   VALUE   "1 S 101120050".
      *    再診料（３月超）５回超
           03  FILLER              PIC X(13)   VALUE   "5 S 101120060".
      *
      *    外来管理加算（３月以内）
           03  FILLER              PIC X(13)   VALUE   "0 G 101120070".
      *    外来管理加算（３月超）５回以内
           03  FILLER              PIC X(13)   VALUE   "1 G 101120080".
      *    外来管理加算（３月超）５回超
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
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    画面編集
           COPY    "CPORCSC94.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           EVALUATE    LNK-ORCSC-KBN
      *        剤別　チェック、編集、自動算定
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        全体のチェック
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        自動算定
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
      *
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (WRK-GMN-INPUTCD(IDX-MAX)
                                       =   SPACE )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
      *                ＤＵＭＭＹ診察料判定
                       PERFORM 10012-DUMMYSYORI-SEC
      ************ WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    "1"
      *                手技料
                       PERFORM 10011-SYUGISYORI-SEC
      *
                   WHEN    "8"
      *                コメントのチェック
                       PERFORM 10018-COMMENT-SEC
      *
                   WHEN    "7"
                       IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                    フィルムの時エラー
                           MOVE    "0047"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
      *
      *            酸素・窒素はエラー
                       IF      WRK-COM-TOKUKIZAISBT1 (IDX-Z) =   2
                           MOVE    "0039"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR  (IDX-Z)
                       END-IF
               END-EVALUATE
      *
      *        初診、再診、指導、在宅の時、回数は１のみ
               IF     (WRK-COM-SRYKBN (IDX-Z)  =   "11"  OR
                                                   "12"  OR
                                                   "13"  OR
                                                   "14" )
                  AND (WRK-ERRCD               =   SPACE )
      *?????          (WRK-COM-SRYKBN (IDX-Z)  =   "14" )
      *??????     AND  WRK-COM-SRYSYUKBN(IDX-Z)  = "140" )
                  IF      (WRK-COM-KAIFLG (IDX-Z)  =   1  )  AND
                          (WRK-COM-KAISU  (IDX-Z)  >   1  )
                       PERFORM 10012-KAISU-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数チェック処理
      *****************************************************************
       10012-KAISU-CHK-SEC            SECTION.
      *
      *    診療情報提供料（Ａ），（Ｂ）（Ｄ）は重複ＯＫとする
           IF     (IDX-SYU             >   ZERO )  AND
                  (WRK-COM-INPUTCD (IDX-SYU)   =   "180016110"
                                               OR  "180016210"
                                               OR  "113004410" )
               CONTINUE
           ELSE
               MOVE    "0008"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       10012-KAISU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-SYUGISYORI-SEC                  SECTION.
      *
      *    在宅診察料が有る時、再診料を削除する
           IF      LNK-ORCSC-HOUMON-ARI    =   1
               IF      WRK-COM-SRYKBN (IDX-Z)  =   "12"
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *    在宅診察料が有る時、初診料が算定済みであること
           IF      LNK-ORCSC-HOUMON-ARI    =   1
      *********IF      LNK-ORCSC-LSTYMD    =   SPACE
               IF      LNK-ORCSC-SYOYMD    =   SPACE
                   MOVE    "1020"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *
      *    診察料の判定
           PERFORM 2001-SINSATU-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      10011-SYUGISYORI-EXT
           END-IF
      *    診察料のチェック、電話再診時の振替え
           PERFORM 10012-SINSATU-SYORI-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      10011-SYUGISYORI-EXT
           END-IF
      *    労災の診察料の判定
           IF      SPA-HKNKBN          =   1
               PERFORM 10013-RSI-SINSATU-CHK-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   GO      TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *
      *    外来管理加算、継続管理加算の判定
           PERFORM 100111-KEIZOKU-CHK-SEC
           IF      FLG-GAIRAI          =   1
      *        労災読み替え加算分は外来管理加算削除としない
               IF      WRK-COM-RSIFLG2 (IDX-Z)     =   "1"
                   MOVE    ZERO            TO  FLG-GAIRAI
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      10011-SYUGISYORI-EXT
            END-IF
      *
      *    老人慢性疾患外来診療料が有る時、外来管理加算を削除する 
      *        受診回数が３回目以外の時
           IF     (LNK-ORCSC-ROUMANSEI-ARI     NOT =   ZERO )  OR
                  (LNK-ORCSC-ROUMANSEI-ARI2    NOT =   ZERO  AND
                   LNK-ORCSC-SINSATU-CNT       NOT =   2    )
               IF      FLG-GAIRAI          =   1
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *
      *    電話再診が有る時、外来管理加算・継続管理加算を削除する
           IF      LNK-ORCSC-TELSAI-ARI    =   1
               IF     (FLG-GAIRAI          =   1  )  OR
                      (FLG-KEIZOK          =   1  )
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *
      *    慢性疼痛疾病管理料が有る時、外来管理加算を削除する
           IF     (LNK-ORCSC-TOKUTEI-KNJ2  =   1  )  OR
      ************(LNK-ORCSC-TOKUTEI-KNJ   =   1  )  OR
                  (LNK-ORCSC-GAIRAI-NASI   =   1  )
               IF      FLG-GAIRAI          =   1
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *
      *    外来管理加算・継続管理加算の剤は、１明細のみとする
           IF      WRK-KASAN-NASI      =   "1"
               IF      WRK-COM-INPUTCD(IDX-Z)(1:1) =   "1"
                   MOVE    "0059"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
           IF     (FLG-GAIRAI          =   1  )  OR
                  (FLG-KEIZOK          =   1  )
               MOVE    "1"                 TO  WRK-KASAN-NASI
           END-IF
      *
      *    剤内に手技料は１つのみ
           IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
               IF      IDX-SYU             >   ZERO
                   MOVE    "0019"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      10011-SYUGISYORI-EXT
               ELSE
                   MOVE    IDX-Z           TO  IDX-SYU
               END-IF
           END-IF
      *
      *    加算チェック
           IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
               PERFORM 1005-KASAN-CHK-SEC
           END-IF
      *
      *    年齢加算算定チェック
           MOVE    SPACE               TO  WRK-NYU-SRYCD
           PERFORM 1007-NENREI-SYORI-SEC
      *
      *    時間加算チェック
           IF     WRK-COM-TIMEFLG (IDX-Z)  NOT =   ZERO
               PERFORM 1002-KASAN-SET-SEC
           END-IF
      *
      *    時間加算を入力された時、自動振替をする
           IF     (WRK-COM-DATAKBN (IDX-Z)     =   "2"  )  AND
                  (WRK-COM-TIMEKSNKBN(IDX-Z)   =   1  OR
                                                   2  OR
                                                   3  OR
                                                   4  OR
                                                   5     ) AND
                  (WRK-COM-AUTOKBN (IDX-Z)     =   SPACE )
      *        年齢加算自動振り替え処理
               IF      WRK-NYU-SRYCD   NOT =   SPACE
                   PERFORM 1008-NENREI-CHENGE-SEC
               END-IF
           END-IF
      *
      *    時間外特例のチェック
           IF      WRK-COM-INPUTCD (IDX-Z)   =   "111000870"  OR
                                                 "111700870"  OR
                                                 "112001410"  OR
                                                 "112701310"  OR
                                                 "111010770"  OR
                                                 "112006070"
               IF      LNK-ORCSC-JIKANTOKU     NOT =   1
                   MOVE    "0069"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *    寝たきり老人在宅総合診療科を算定時、加算を追加
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "114701210"  OR
                                                   "114701710"
               PERFORM 1009-NETAKIRI-HEN-SEC
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *----(01.00.03) LINE ADD START ----------------------------------
      *    診療情報提供料を入力時、算定日のコメントを自動加算
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "180016110"  OR
                                                   "180016210"  OR
                                                   "180016310"  OR
                                                   "113004410"
               PERFORM 10010-COMMT-SEC
           END-IF
      *
      *    特定薬剤治療加算料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113000410"  OR
                                                   "180000110"  OR
                                                   "180000210"  OR
                                                   "113000510"  OR
                                                   "113000770"  OR
                                                   "113000670"
               PERFORM 10011-TOKUYAKU-SEC
           END-IF
      *
      *    悪性腫瘍特異物資治療管理料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113001210"  OR
                                                   "113001310"  OR
                                                   "113002110"
               PERFORM 10012-AKUSEI-SEC
           END-IF
      *
      *    アフターケアの時算定できないコードのチェック
           IF      SPA-RSI-HKNKBN      =   "3"
               PERFORM 100113-RSI-AFTCHK-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   GO      TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
           .
       10011-SYUGISYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料のチェック、電話再診時の振替え処理
      *****************************************************************
       10012-SINSATU-SYORI-SEC                  SECTION.
      *
      *    診察料のチェック
           IF     (WRK-COM-SRYKBN(IDX-Z)   =   "12")  AND
                  (WRK-KAISU       NOT =   ZERO    )
      *       １５才未満のチェック
      ********IF     ((LNK-ORCSC-NENREI-YY <   15   )  OR
      *               (LNK-ORCSC-15MIMAN-ARI   =   1  )) AND
      *               (WRK-KAISU           =   4    )
      *            IF      WRK-KBN2        NOT =   "M"
      *                MOVE    "0066"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      *                GO      TO      10012-SINSATU-SYORI-EXT
      *             END-IF
      *********END-IF
      *        厚生省が定める患者チェック
               IF      WRK-KBN2            =   "E"
                   IF     (LNK-ORCSC-TOKUTEI-KNJ   =   1 OR
                           LNK-ORCSC-TOKUTEI-KNJ2  =   1  )  AND
                          (WRK-KAISU           =   4    )
                       CONTINUE
                   ELSE
                       MOVE    "0067"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      10012-SINSATU-SYORI-EXT
                    END-IF
               END-IF
           END-IF
      *
      *    電話再診の時、回数による振替え
           IF      WRK-KBN             =   "T"  OR  "S"
               CONTINUE
           ELSE
               GO      TO      10012-SINSATU-SYORI-EXT
           END-IF
      *
           EVALUATE    LNK-ORCSC-SINSATU-CNT
               WHEN    0
                   MOVE    1               TO  WRK-SAN-KAISU
               WHEN    1
               WHEN    2
                   MOVE    2               TO  WRK-SAN-KAISU
               WHEN    OTHER
                   MOVE    4               TO  WRK-SAN-KAISU
           END-EVALUATE
      *
           IF      WRK-KAISU       NOT =   WRK-SAN-KAISU
      *        診察料の決定
               PERFORM 100121-SINSATU-CD-SEC
               IF      WRK-SRYCD       NOT =   SPACE
      *            自動追加
                   MOVE    IDX-Z               TO  IDX-T
                   MOVE    1                   TO  FLG-ARI
                   PERFORM 1100-KASAN-SET-SEC
                   MOVE    SPACE               TO  WRK-COM-AUTOKBN
                                                                (IDX-Z)
              END-IF
           END-IF
      *
           .
       10012-SINSATU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料の決定処理
      *****************************************************************
       100121-SINSATU-CD-SEC           SECTION.
      *
           MOVE    WRK-SAN-KAISU       TO  WRK-KAISU
      *
           MOVE    SPACE               TO  WRK-KBN2
      *    特定の患者
           IF     (LNK-ORCSC-TOKUTEI-KNJ   =   1  OR
                   LNK-ORCSC-TOKUTEI-KNJ2  =   1  )  AND
                  (WRK-KAISU           =   4  )
               MOVE    4                   TO  WRK-KAISU
               MOVE    "E"                 TO  WRK-KBN2
           END-IF
      *    人工腎臓の患者
           IF     (LNK-ORCSC-TOKUTEI-JNZ   =   1  )  AND
                  (WRK-KAISU           =   4  )
               MOVE    4                   TO  WRK-KAISU
               MOVE    "J"                 TO  WRK-KBN2
      *        同日再診の時は、コードがないので、特定の患者とする
               IF      WRK-KBN             =   "D"
                   MOVE    "E"             TO  WRK-KBN2
               END-IF
           END-IF
      *    １５才未満
           IF    ((LNK-ORCSC-NENREI-YY     <   15 )  OR
                  (LNK-ORCSC-15MIMAN-ARI   =   1  )) AND
                  (WRK-KAISU               =   4  )
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
                     AND  (LNK-ORCSC-ROUJIN    =   TBL-ROUJIN(TBL-IDX))
                     AND  (SPA-HOSPSBT         =   TBL-HOSPSBT(TBL-IDX))
                     AND  (WRK-KBN             =   TBL-KBN    (TBL-IDX))
                     AND  (WRK-KBN2            =   TBL-KBN2   (TBL-IDX))
                       MOVE    TBL-SRYCD (TBL-IDX)
                                               TO  WRK-SRYCD
           END-SEARCH
      *
           .
       100121-SINSATU-CD-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算,継続管理加算の判定処理
      *****************************************************************
       100111-KEIZOKU-CHK-SEC                  SECTION.
      *
      *    外来管理加算、継続管理加算の判定
           MOVE    ZERO                TO  FLG-GAIRAI
           MOVE    ZERO                TO  FLG-KEIZOK
           SET     TBLG-IDX            TO  1
           SEARCH  TBL-GAIRAI-OCC      VARYING   TBLG-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-KBN
                   WHEN    WRK-COM-INPUTCD(IDX-Z)  =   TBLG-SRYCD
                                                             (TBLG-IDX)
                       MOVE    TBLG-KBN   (TBLG-IDX)
                                               TO  WRK-KBN
           END-SEARCH
           EVALUATE    WRK-KBN
      *        外来管理加算
               WHEN    "G"
                   MOVE    1           TO  FLG-GAIRAI
      *        継続管理加算
               WHEN    "K"
                   MOVE    1           TO  FLG-KEIZOK
           END-EVALUATE
      *
      *    健保で、継続管理の時
      **** IF     (SPA-HKNKBN      NOT =   1   ) OR
      *           (FLG-KEIZOK          =   1   )
      *        GO      TO      100111-KEIZOKU-CHK-EXT
      **** END-IF
      *
      *    健保の時
           IF      SPA-HKNKBN      NOT =   1
               GO      TO      100111-KEIZOKU-CHK-EXT
           END-IF
      *    労災で継続管理の時
           IF      FLG-KEIZOK          =   1
               IF      SPA-RSI-HKNKBN      =   "3"
      *            アフターケアの時エラー
                   MOVE    "8006"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               END-IF
               GO      TO      100111-KEIZOKU-CHK-EXT
           END-IF
      *
      *    労災保険の時、外来管理加算はエラーとする
           IF      WRK-KBN         =   "G"
               MOVE    "8001"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               GO      TO      100111-KEIZOKU-CHK-EXT
           END-IF
      *
      *    労災保険の外来管理加算の判定
           MOVE    ZERO                TO  FLG-GAIRAI
           SET     TBL-RSI             TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    ZERO                TO  FLG-ARI
                   MOVE    SPACE               TO  WRK-KBN
               WHEN    WRK-COM-INPUTCD(IDX-Z)  =   TBLR-SRYCD (TBL-RSI)
                   MOVE    1                   TO  FLG-ARI
                   MOVE    TBLR-KBN(TBL-RSI)   TO  WRK-KBN
           END-SEARCH
      *    労災の外来管理加算点数セット
           IF      WRK-KBN             =   "G"
               IF      SPA-RSI-HKNKBN      =   "3"
      *            アフターケアの時エラー
                   MOVE    "8007"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               ELSE
                   MOVE    1               TO  FLG-GAIRAI
                   MOVE    WRK-COM-KISOTEN(IDX-Z)
                                           TO  LNK-ORCSC-GAIRAI-TEN
               END-IF
           END-IF
      *
           .
       100111-KEIZOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の診察料の判定チェック処理
      *****************************************************************
       10013-RSI-SINSATU-CHK-SEC       SECTION.
      *    労災の診察料の判定
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "101110010"
      *        初診料の時、過去に受診がないこと
      *        受診歴を検索する  医療機関、患者ＩＤ
               INITIALIZE                  JYURRK-REC
               MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
               MOVE    SPA-PTID            TO  JYURRK-PTID
               MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
               MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key15"             TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               ELSE
                   INITIALIZE                  JYURRK-REC
                   MOVE    1               TO  FLG-JYURRK
               END-IF
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-JYURRK          =   ZERO
                   IF      JYURRK-SRYYMD       <   SPA-SRYYMD
                       MOVE    "8002"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                         (IDX-Z)
                       GO      TO      10013-RSI-SINSATU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    救急医療管理加算
      **** IF      WRK-COM-INPUTCD(IDX-Z)  =   "101110030"
      *        同一傷病に付き１回
      *        IF      LNK-ORCSC-RSI-KYUKASN   =   1
      *            MOVE    "8003"          TO  WRK-ERRCD
      *            MOVE    "1"             TO  WRK-COM-CUR
      *                                                  (IDX-Z)
      *            GO      TO      10013-RSI-SINSATU-CHK-EXT
      *        END-IF
      **** END-IF
      *
      *    療養の給付請求書取扱料
      **** IF      WRK-COM-INPUTCD(IDX-Z)  =   "101130020"
      *        同一傷病に付き１回
      *        IF      LNK-ORCSC-RSI-KYUFU     =   1
      *            MOVE    "8011"          TO  WRK-ERRCD
      *            MOVE    "1"             TO  WRK-COM-CUR
      *                                                  (IDX-Z)
      *            GO      TO      10013-RSI-SINSATU-CHK-EXT
      *        END-IF
      **** END-IF
      *
      *    再診時療養指導管理料
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "101120030"
      *        初診料算定時、エラー
               IF      LNK-ORCSC-SYOSIN2       =   1
                   MOVE    "8013"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR
                                                         (IDX-Z)
                   GO      TO      10013-RSI-SINSATU-CHK-EXT
               END-IF
           END-IF
      *
      *    救急医療管理加算（入院）入院日のみ
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "101110020"
               IF      SPA-SRYYMD          =   LNK-ORCSC-NYUINYMD
                   CONTINUE
               ELSE
                   MOVE    "8022"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR
                                                         (IDX-Z)
                   GO      TO      10013-RSI-SINSATU-CHK-EXT
               END-IF
           END-IF
      *
           .
       10013-RSI-SINSATU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    アフターケアの時算定できないコードのチェック処理
      *****************************************************************
       100113-RSI-AFTCHK-SEC           SECTION.
      *
      *    在宅自己導尿指導管理料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "114004410"
               MOVE    "8008"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO      TO      100113-RSI-AFTCHK-EXT
           END-IF
      *
      *    てんかん指導料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113002850"
               MOVE    "8009"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO      TO      100113-RSI-AFTCHK-EXT
           END-IF
      *    再診時療養指導管理料
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "101120030"
               MOVE    "8012"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO      TO      100113-RSI-AFTCHK-EXT
           END-IF
      *
      *    救急医療管理加算
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "101110020"  OR
                                               "101110030"
               MOVE    "8015"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO      TO      100113-RSI-AFTCHK-EXT
           END-IF
      *    労災の指導料加算等
           IF      WRK-COM-INPUTCD(IDX-Z)  >=  "101130010"  AND
                                           <=  "101130100"
               MOVE    "8016"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO      TO      100113-RSI-AFTCHK-EXT
           END-IF
      *    在宅料
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "14"
               MOVE    "8017"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO      TO      100113-RSI-AFTCHK-EXT
           END-IF
      *
           .
       100113-RSI-AFTCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料チェック処理
      *****************************************************************
       1005-KASAN-CHK-SEC                  SECTION.
      *
      *    加算データの時、前に手技料があること
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
      *
           END-PERFORM
      *
      *    当日小児科外来診察料を算定済みの時、時間外のみはＯＫとする
           IF      LNK-ORCSC-SYONIKA-ARI2  =   1
               EVALUATE    WRK-COM-INPUTCD (IDX-Z) 
      *            再診時時間外加算
                   WHEN    "112005770"
      *            再診時休日加算
                   WHEN    "112005870"
      *            再診時深夜加算
                   WHEN    "112005970"
      *            再診時時間外特例
                   WHEN    "112006070"
                       GO      TO      1005-KASAN-CHK-EXT
               END-EVALUATE
           END-IF
      *    寝たきり在宅があり、在宅指導料はＯＫとする
           IF     (LNK-ORCSC-NETAKIRI-ARI  NOT =   ZERO  )  OR
                  (LNK-ORCSC-NETAKIRI-ARI2 NOT =   ZERO  )
               IF      WRK-COM-SRYSYUKBN (IDX-Z)   =   "143"
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-IF
      *
      *    在宅加算料
           IF      WRK-COM-SRYSYUKBN (IDX-Z)  =   "143"
      *        在宅で、往診区分がないものは単独ＯＫとする
               IF     (WRK-COM-TNSSRYSYUKBN (IDX-Z)  =   "140" ) AND
                      (WRK-COM-OSINKBN (IDX-Z)       =   ZERO  )
                   MOVE    1                    TO  FLG-ARI
               END-IF
           END-IF
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間加算追加処理
      *****************************************************************
       1002-KASAN-SET-SEC              SECTION.
      *
      *    診察の時、他の時間加算もする
           MOVE    WRK-COM-TIMEFLG(IDX-Z)
                                       TO  LNK-ORCSC-TIMEFLG
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    時間加算
           EVALUATE    WRK-COM-SRYKBN(IDX-Z)
              WHEN     "11"
      *----(01.00.01) LINE UPD START ----------------------------------
      *           初診料加算
                  EVALUATE    WRK-COM-TIMEFLG(IDX-Z)  ALSO
                                                   LNK-ORCSC-ROUJIN
      *            /* 時間外 */
                       WHEN    1       ALSO     0
                           MOVE    "111000570"     TO  WRK-SRYCD
      *            /* 休日 */
                       WHEN    2       ALSO     0
                           MOVE    "111000670"     TO  WRK-SRYCD
      *            /* 深夜 */
                       WHEN    3       ALSO     0
                           MOVE    "111000770"     TO  WRK-SRYCD
      *            /* 時間外特例 */
                       WHEN    4       ALSO     0
                           MOVE    "111000870"     TO  WRK-SRYCD
      *            /* 時間外 　老人
                       WHEN    1       ALSO     1
                           MOVE    "111700570"     TO  WRK-SRYCD
      *            /* 休日 　老人*/
                       WHEN    2       ALSO     1
                           MOVE    "111700670"     TO  WRK-SRYCD
      *            /* 深夜 　老人*/
                       WHEN    3       ALSO     1
                           MOVE    "111700770"     TO  WRK-SRYCD
      *            /* 時間外特例 　老人*/
                       WHEN    4       ALSO     1
                           MOVE    "111700870"     TO  WRK-SRYCD
                   END-EVALUATE
      *----(01.00.01) LINE UPD END   ----------------------------------
              WHEN     "12"
      *           再診料加算
                  EVALUATE    WRK-COM-TIMEFLG(IDX-Z)  ALSO
                                                      LNK-ORCSC-ROUJIN
      *            /* 時間外 */
                       WHEN    1       ALSO     0
                           MOVE    "112001110"     TO  WRK-SRYCD
      *            /* 休日 */
                       WHEN    2       ALSO     0
                           MOVE    "112001210"     TO  WRK-SRYCD
      *            /* 深夜 */
                       WHEN    3       ALSO     0
                           MOVE    "112001310"     TO  WRK-SRYCD
      *            /* 時間外特例 */
                       WHEN    4       ALSO     0
                           MOVE    "112001410"     TO  WRK-SRYCD
      *            /* 時間外 　老人
                       WHEN    1       ALSO     1
                           MOVE    "112701010"     TO  WRK-SRYCD
      *            /* 休日 　老人*/
                       WHEN    2       ALSO     1
                           MOVE    "112701110"     TO  WRK-SRYCD
      *            /* 深夜 　老人*/
                       WHEN    3       ALSO     1
                           MOVE    "112701210"     TO  WRK-SRYCD
      *            /* 時間外特例　老人*/
                       WHEN    4       ALSO     1
                           MOVE    "112701310"     TO  WRK-SRYCD
                   END-EVALUATE
           END-EVALUATE
      *
      *    小児科外来診療科の加算
           IF      WRK-COM-TIMEFLG(IDX-Z)  NOT =   ZERO
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            初診
                   WHEN    "113003510"
                   WHEN    "113003710"
                       EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                           WHEN    1
      *                        初診時時間外加算
                               MOVE    "111010470"     TO  WRK-SRYCD
                           WHEN    2
      *                        初診時休日加算
                               MOVE    "111010570"     TO  WRK-SRYCD
                           WHEN    3
      *                        初診時深夜加算
                               MOVE    "111010670"     TO  WRK-SRYCD
                           WHEN    4
      *                        初診時  時間外特例
                               MOVE    "111010770"     TO  WRK-SRYCD
                       END-EVALUATE
      *            再診
                   WHEN    "113003610"
                   WHEN    "113003810"
                       EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                           WHEN    1
      *                        再診時時間外加算
                               MOVE    "112005770"     TO  WRK-SRYCD
                           WHEN    2
      *                        再診時休日加算
                               MOVE    "112005870"     TO  WRK-SRYCD
                           WHEN    3
      *                        再診時深夜加算
                               MOVE    "112005970"     TO  WRK-SRYCD
                           WHEN    4
      *                        再診時  時間外特例
                               MOVE    "112006070"     TO  WRK-SRYCD
                       END-EVALUATE
               END-EVALUATE
           END-IF
      *
           IF      WRK-SRYCD            =    SPACE
               GO      TO      1002-KASAN-SET-EXT
           END-IF
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF     (WRK-COM-DATAKBN    (IDX1)       =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX1)   NOT =   ZERO )
                    MOVE    1              TO  FLG-ARI
                    MOVE    IDX1           TO  IDX-T
               END-IF
           END-PERFORM
      *    自動追加
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
      *
           IF      FLG-ADD             =   1
      *        年齢加算自動振り替え処理
               IF      WRK-NYU-SRYCD   NOT =   SPACE
                   PERFORM 1008-NENREI-CHENGE-SEC
               END-IF
           END-IF
      *
           .
       1002-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算　存在チェック加処理
      *****************************************************************
       1110-KASAN-CHK-SEC        SECTION.
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE  )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
           .
       1110-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算１行追加処理
      *****************************************************************
       1100-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    点数マスタ編集・基本チェック
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
               MOVE    SPACE               TO  WRK-COM-GMNFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               MOVE    SPACE               TO  SPA-ERRCD
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1100-KASAN-SET-EXT
           END-IF
      *
           IF      IDX-S               >   1
               MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYSYUKBN(IDX-S)
           END-IF
      *
      *    自動追加分（入力時に追加）
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           MOVE    1                   TO  FLG-ADD
      *
           .
       1100-KASAN-SET-EXT.
           EXIT.
           .
      *****************************************************************
      *    年齢加算追加処理
      *****************************************************************
       1007-NENREI-SYORI-SEC           SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                          =   SPACE  OR  ZERO )
      *
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
      *        加算チェック
               PERFORM 1110-KASAN-CHK-SEC
               MOVE    IDX-Z               TO  IDX-S
      *        加算追加
               PERFORM 1100-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
                   MOVE    WRK-SRYCD           TO  WRK-NYU-SRYCD
               END-IF
           END-PERFORM
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算時間外自動振替処理
      *****************************************************************
       1008-NENREI-CHENGE-SEC           SECTION.
      *
      *
           MOVE    WRK-NYU-SRYCD       TO  WRK-SRYCD
      *    加算存在チェック
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   ZERO
               GO      TO      1008-NENREI-CHENGE-EXT
           END-IF
      *
           MOVE    IDX-SYU               TO  IDX-S
           EVALUATE    WRK-COM-SRYKBN(IDX-SYU)
               WHEN    "11"
      *            初診
                   MOVE    "111011470"         TO  WRK-SRYCD
                   PERFORM 1100-KASAN-SET-SEC
               WHEN    "12"
      *            再診（乳幼児）（時間加算等）
                   MOVE    "112006970"         TO  WRK-SRYCD
                   PERFORM 1100-KASAN-SET-SEC
                   IF      FLG-ADD             =   ZERO
      *                再診（幼児）（時間加算等）
                       MOVE    "112007070"         TO  WRK-SRYCD
                       PERFORM 1100-KASAN-SET-SEC
                   END-IF
           END-EVALUATE
      *
           .
       1008-NENREI-CHENGE-EXT.
           EXIT.
      *****************************************************************
      *    寝たきり老人在宅総合診療科に加算追加処理
      *****************************************************************
       1009-NETAKIRI-HEN-SEC           SECTION.
      *
      *    既に入力のある時、自動加算しない
      *     ２４時間連携体制加算（１)
           MOVE    "114701870"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *     ２４時間連携体制加算（２)
           MOVE    "114703970"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *     ２４時間連携体制加算（３)
           MOVE    "114703770"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *
      *     ２４時間連携体制加算（１)
           MOVE    "114701870"         TO  WRK-SRYCD
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *
      *     ２４時間連携体制加算（２)
           MOVE    "114703970"         TO  WRK-SRYCD
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *
      *     ２４時間連携体制加算（３)
           MOVE    "114703770"         TO  WRK-SRYCD
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *
           .
       1009-NETAKIRI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療情報提供算定日追加処理
      *****************************************************************
       10010-COMMT-SEC           SECTION.
      *    診療情報提供料算定日
           MOVE    "840000052"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      10010-COMMT-EXT
           END-IF
      *
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   ZERO
               GO      TO      10010-COMMT-EXT
           END-IF
      *    診療日を編集
           MOVE    SPA-SRYYMD(7:2)     TO  WRK-COM-ATAI1 (IDX-S)
      *
      *    画面再編集
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    LNK-ORCSC-PG        TO  LNK-SC94-PG
           MOVE    IDX-S               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    診療日を編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
           .
       10010-COMMT-EXT.
           EXIT.
      *****************************************************************
      *    悪性腫瘍特異物資治療管理料のコメント加算処理
      *****************************************************************
       10012-AKUSEI-SEC           SECTION.
      *
      *    腫瘍マーカー検査名
           MOVE    "830000015"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      10012-AKUSEI-EXT
           END-IF
      *
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-S)
           END-IF
      *
           .
       10012-AKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療加算料チェック処理
      *****************************************************************
       10011-TOKUYAKU-SEC              SECTION.
      *
      *    特定薬剤治療管理料算定なしの時、チェックしない
           IF      LNK-ORCSC-TOKUTEI-FLG   =   "1"
               CONTINUE
           ELSE
               GO      TO      10011-TOKUYAKU-EXT
           END-IF
      *
           MOVE    ZERO                 TO  IDX-S
      *    初回算定日の編集
           MOVE    SPACE               TO  WRK-SYOKAI-YMD
           MOVE    ZERO                TO  WRK-SYOKAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        算定履歴検索（診療日降順）
               SET     TBL-TOK             TO  IDX
               MOVE    TBL-TOKSRYCD  (TBL-TOK) TO  WRK-SRYCD
               MOVE    "key3"              TO  WRK-DBPATH
               PERFORM 100111-SANTEI-KEN-SEC
               PERFORM
                   UNTIL   FLG-SANTEI          =   1
                   IF     (SANTEI-MSANTEICNT   >   ZERO      ) AND
                          (SANTEI-FSANTEIYMD   <=  SPA-SRYYMD)
                       IF      SANTEI-FSANTEIYMD   >=  WRK-SYOKAI-YMD
                           MOVE    SANTEI-FSANTEIYMD
                                                  TO  WRK-SYOKAI-YMD
                       END-IF
      *                当月算定あり
                       IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                           MOVE    SANTEI-MSANTEICNT   TO  WRK-SYOKAI
                           IF     (LNK-ORCSC-SYORIFLG  =   1  ) AND
                                  (SANTEI-DAY (IDX-Y2) >   ZERO)
                               COMPUTE WRK-SYOKAI      =   WRK-SYOKAI
                                                       -   SANTEI-DAY
                                                              (IDX-Y2)
                           END-IF
                       END-IF
                   END-IF
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    WRK-DBPATH          TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
      *    訂正の時、初回算定日と診療日が同じ時、初回分とする
           IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                  (WRK-SYOKAI-YMD       =   SPA-SRYYMD)
               MOVE    SPACE            TO  WRK-SYOKAI-YMD
           END-IF
      *    すでに初回算定日がある時、初回算定日に入れる
           IF      LNK-ORCSC-TOKTEI-YMD    NOT =   SPACE
               MOVE    LNK-ORCSC-TOKTEI-YMD    TO  WRK-SYOKAI-YMD
           END-IF
      *
      *    特定薬剤治療管理加算がある時、初回とする
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE      ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE ) OR
                              (WRK-ERRCD       NOT =   SPACE )
               EVALUATE    WRK-COM-INPUTCD (IDX1)
                   WHEN    "113000770"
      *            特定薬剤治療管理料が当月存在しないこと
                       MOVE    "113000410"         TO  WRK-SRYCD
                       MOVE    "key"               TO  WRK-DBPATH
                       PERFORM 100111-SANTEI-KEN-SEC
                       MOVE    "tbl_santei"        TO  MCP-TABLE
                       MOVE    WRK-DBPATH          TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
                       IF     (FLG-SANTEI          =   1  )  OR
                              (LNK-ORCSC-SYORIFLG  =   1  AND
                               SPA-SRYYMD(7:2)     =   SANTEI-MSANTEID)
                           MOVE    SPACE           TO  WRK-SYOKAI-YMD
                       ELSE
                           MOVE    "0132"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
               END-EVALUATE
           END-PERFORM
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      10011-TOKUYAKU-EXT
           END-IF
      *
      *    初回時
           IF      WRK-SYOKAI-YMD      =   SPACE
      *********MOVE    SPA-SYSYMD          TO  WRK-SANTEI-YMD
               MOVE    SPA-SRYYMD          TO  WRK-SANTEI-YMD
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
      *                特定薬剤治療管理加算１回目自動発生
                       MOVE    "113000770"     TO  WRK-SRYCD
                       PERFORM 1110-KASAN-CHK-SEC
                       IF      FLG-ARI         =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                           PERFORM 1100-KASAN-SET-SEC
                       ELSE
                           MOVE    IDX-T               TO  IDX-S
                       END-IF
      *                特定薬剤治療管理料初回算定追加
                       MOVE    "840000038"         TO  WRK-SRYCD
                       PERFORM 100112-COMMT1-SEC
      *            特定薬剤治療管理料（ジギタリス製剤等）
                   WHEN    "180000110"
                   WHEN    "180000210"
      *                特定薬剤治療管理加算有無
                       MOVE    "113000770"     TO  WRK-SRYCD
                       PERFORM 1110-KASAN-CHK-SEC
                       IF      FLG-ARI         =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                       ELSE
                           MOVE    IDX-T               TO  IDX-S
                       END-IF
      *                特定薬剤治療管理料初回算定追加
                       MOVE    "840000038"         TO  WRK-SRYCD
                       PERFORM 100112-COMMT1-SEC
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
                       MOVE    "0130"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      10011-TOKUYAKU-EXT
               END-EVALUATE
      *
           END-IF
      *    ２回目以降
      *    Ｋ０２のみ
           IF     (WRK-SYOKAI-YMD  NOT =   SPACE  )  AND
                  (LNK-ORCSC-PG        =   "K02"  )
      *        算定回数計算
               PERFORM 10013-SANTEI-CNT-SEC
      *        初回算定日＋３月
               MOVE    WRK-SYOKAI-YMD      TO  WRK-SYOKAI-YMD3
               COMPUTE WRK-SYO-MM3         =   WRK-SYO-MM3   +   3
               IF      WRK-SYO-MM3         >   12
                   COMPUTE WRK-SYO-YY3     =   WRK-SYO-YY3   +   1
                   COMPUTE WRK-SYO-MM3     =   WRK-SYO-MM3   -   12
               END-IF
      *
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
      ***************  IF      WRK-SANTEI-CNT      >=  3
                       IF      SPA-SRYYMD(1:6)     >=  WRK-SYOKAI-YMD3
                                                               (1:6)
      ***H14.03.08 現在 振替えなしで、警告表示
      *                   ４月目以降へ振り替え
      *                   MOVE    "113000510"          TO  WRK-SRYCD
      *                   MOVE    IDX-Z                TO  IDX-T
      *                   MOVE    1                    TO  FLG-ARI
      *                   PERFORM 1100-KASAN-SET-SEC
      *                   MOVE    SPACE            TO  WRK-COM-AUTOKBN
      *****                                                (IDX-Z)
                          IF      WRK-COM-KZMFLG (IDX-Z)   =   SPACE
                              MOVE    "K007"           TO  WRK-ERRCD
                              MOVE    "1"              TO  WRK-COM-CUR
                                                          (IDX-Z)
                              MOVE    "1"          TO  WRK-COM-KZMFLG
                                                          (IDX-Z)
                           END-IF
                       END-IF
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
      *****************IF      WRK-SANTEI-CNT      >=  3
                       IF      SPA-SRYYMD(1:6)     >=  WRK-SYOKAI-YMD3
                                                               (1:6)
                           CONTINUE
                       ELSE
                           MOVE    "0130"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                           GO      TO      10011-TOKUYAKU-EXT
                       END-IF
               END-EVALUATE
      *
      *        特定薬剤治療管理料初回算定追加
               MOVE    WRK-SYOKAI-YMD      TO  WRK-SANTEI-YMD
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
      *            特定薬剤治療管理料（ジギタリス製剤等）
                   WHEN    "180000110"
                   WHEN    "180000210"
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
                       IF      WRK-SYOKAI          =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                           MOVE    "840000038"         TO  WRK-SRYCD
                           PERFORM 100112-COMMT1-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    測定薬剤名コメント自動発生
           IF     (WRK-COM-INPUTCD (IDX-Z + 1) =   SPACE )  OR
                  (IDX-S                       >    ZERO  AND
                   WRK-COM-INPUTCD (IDX-S + 1) =   SPACE )
               MOVE    "830000014"         TO  WRK-SRYCD
               IF      IDX-S               =   ZERO
                   MOVE    IDX-Z           TO  IDX-S
               END-IF
               PERFORM 100112-COMMT1-SEC
           END-IF
      *    初回算定日をセットする
           MOVE    WRK-SYOKAI-YMD      TO  LNK-ORCSC-TOKTEI-YMD
           .
       10011-TOKUYAKU-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療管理料のコメント追加処理
      *****************************************************************
       100112-COMMT1-SEC           SECTION.
      *
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      100112-COMMT1-EXT
           END-IF
      *
      *****MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   ZERO
               GO      TO      100112-COMMT1-EXT
           END-IF
      *    測定薬剤名コメントの追加
           IF      WRK-SRYCD           =   "830000014"
      *        自動追加分を止める
               MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-S)
               MOVE    "1"                 TO  WRK-COM-CUR     (IDX-S)
               GO      TO      100112-COMMT1-EXT
           END-IF
      *
      *    特定薬剤治療管理料初回算定コメント
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SANTEI-YMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           MOVE    LNK-DAY1-WYMD(2:2)  TO  WRK-COM-ATAI1 (IDX-S)
           MOVE    LNK-DAY1-WYMD(4:2)  TO  WRK-COM-ATAI2 (IDX-S)
      *
      *    画面再編集
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    LNK-ORCSC-PG        TO  LNK-SC94-PG
           MOVE    IDX-S               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    診療日を編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
           .
       100112-COMMT1-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療管理料算定回数計算処理
      *****************************************************************
       10013-SANTEI-CNT-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               SET     TBL-TOK             TO  IDX
      *        算定履歴検索
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    TBL-TOKSRYCD (TBL-TOK)
                                           TO  SANTEI-SRYCD
               MOVE    WRK-SYOKAI-YMD      TO  SANTEI-FSANTEIYMD
      *        算定履歴区分は’１’であることとする
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
               END-IF
               PERFORM
                       UNTIL       FLG-SANTEI      =   1
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       ADD     SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
                   END-IF
      *            訂正の時、訂正分は対象外
                   IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                          (SANTEI-SRYYM         =   SPA-SRYYMD(1:6))
                     MOVE  ZERO                 TO  IDX-Y2
                     IF    TBL-TOKSRYCD (TBL-TOK)
                                                =   WRK-COM-INPUTCD
                                                         (IDX-Z)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       IF      SANTEI-DAY (IDX-Y2) >   ZERO
                           COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                                   -   SANTEI-DAY
                                                             (IDX-Y2)
                       END-IF
                     END-IF
                     ADD     1                 TO  IDX-Y2
                     PERFORM VARYING     IDX-2 FROM   IDX-Y2 BY  1
                             UNTIL       IDX-2 >   31
                           COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                                   -   SANTEI-DAY
                                                             (IDX-2)
                     END-PERFORM
                   END-IF
      *            訂正の時、訂正日以降は対象外
                   IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                          (SANTEI-SRYYM        >   SPA-SRYYMD(1:6))
                       COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                               -   SANTEI-MSANTEICNT
                   END-IF
      *
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
           .
       10013-SANTEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴検索処理
      *****************************************************************
       100111-SANTEI-KEN-SEC                  SECTION.
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           IF      WRK-DBPATH          =   "key"
               MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
           END-IF
      *    算定履歴区分は’１’であることとする
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       100111-SANTEI-KEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   コメントのチェック処理
      *****************************************************************
       10018-COMMENT-SEC                  SECTION.
      *
      *    特定薬剤治療管理初回算定コメントがあれば初回とする
           IF     (WRK-COM-INPUTCD (IDX-Z) =   "840000038" ) AND
                  (LNK-ORCSC-TOKTEI-YMD    NOT =   SPACE   )
      *        特定開始日和暦変換
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                    TO  LNK-DAY1-IRAI
               MOVE    LNK-ORCSC-TOKTEI-YMD    TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
      *
               MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
               MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
      *        初回算定年月が前回初回と違う時システム日を初回算定とする
               IF      WRK-WYM             =   LNK-DAY1-WYMD(2:4)
                   CONTINUE
               ELSE
      *            診療年月和暦変換
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "11"                    TO  LNK-DAY1-IRAI
                   MOVE    SPA-SRYYMD              TO  LNK-DAY1-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      WRK-WYM         =   LNK-DAY1-WYMD(2:4)
                       MOVE    SPA-SRYYMD      TO  LNK-ORCSC-TOKTEI-YMD
                   ELSE
                       MOVE    "0133"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
      *
      *    特定薬剤治療管理初回算定コメントがあれば初回とする
           IF     (WRK-COM-INPUTCD (IDX-Z) =   "840000038" ) AND
                  (LNK-ORCSC-TOKTEI-YMD    =   SPACE   )  AND
                  (WRK-COM-ATAI1 (IDX-Z)(1:2)  NOT =   SPACE) AND
                  (WRK-COM-ATAI2 (IDX-Z)(1:2)  NOT =   SPACE )
      *
               MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
               MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
      *        入力年月を西暦変換
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "12"                TO  LNK-DAY1-IRAI
               IF      WRK-WYY                 >   "60"
                   MOVE    "3"                 TO  LNK-DAY1-WYMD(1:1)
               ELSE
                   MOVE    "4"                 TO  LNK-DAY1-WYMD(1:1)
               END-IF
               MOVE    WRK-WYM             TO  LNK-DAY1-WYMD(2:4)
               MOVE    "01"                TO  LNK-DAY1-WYMD(6:2)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               IF      SPA-SRYYMD(1:6)     >   WRK-SYMD(1:6)
                   MOVE    WRK-SYMD            TO  LNK-ORCSC-TOKTEI-YMD
               ELSE
                   IF      SPA-SRYYMD(1:6)     <   WRK-SYMD(1:6)
                       MOVE    "0135"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
      *    ＤＵＭＭＹの診察料判定
      *    Ｋ０２のみ
           IF      LNK-ORCSC-PG        =   "K02"
               CONTINUE
           ELSE
               GO      TO      10018-COMMENT-EXT
           END-IF
           IF      WRK-COM-INPUTCD (IDX-Z) =   "820000003"  OR
                                               "820000004"  OR
                                               "820000005"  OR
                                               "820000006"
               IF      WRK-COM-SRYKBN(IDX-Z)   =   "11"  OR  "12"
      *            同じ剤内にＤＵＭＭＹがない時自動発生する
                   PERFORM 100181-DUMMY-SYORI-SEC
               ELSE
                   MOVE    "0136"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
           .
       10018-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＵＭＭＹ診察料自動発生処理
      *****************************************************************
       100181-DUMMY-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE  AND
                               WRK-COM-INPUTCD (IDX)   =   SPACE )
               IF      WRK-GMN-INPUTCD(IDX)    =   ".110"  OR
                                                   ".120"
                   MOVE    1                   TO  FLG-OK
               END-IF
               IF      WRK-COM-INPUTCD (IDX)   =   "099110001"  OR
                                                   "099120001"
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0136"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (01)
               GO      TO      100181-DUMMY-SYORI-EXT
           END-IF
      *
      *
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "11"
      *        初診  同日診察でないこと
               IF     (LNK-ORCSC-SYORIFLG  =   ZERO       )  AND
                      (LNK-ORCSC-LSTYMD    =   SPA-SRYYMD )  AND
                      (LNK-ORCSC-SYOYMD    NOT =   LNK-ORCSC-LSTYMD)
                   MOVE    "0138"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (01)
               END-IF
           ELSE
      *        再診  初診料が算定済みであること
               IF     (LNK-ORCSC-SYORIFLG  =   ZERO       )  AND
                      (LNK-ORCSC-LSTYMD    =   SPACE      )
                   MOVE    "0139"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (01)
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      100181-DUMMY-SYORI-EXT
           END-IF
      *    診察料自動算定
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "11"
               MOVE    "099110001"         TO  WRK-SRYCD
           ELSE
               MOVE    "099120001"         TO  WRK-SRYCD
           END-IF
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      100181-DUMMY-SYORI-EXT
           END-IF
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
      *        自動追加分を止める
               MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-S)
           END-IF
      *
           .
       100181-DUMMY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＵＭＭＹ診察料判定処理
      *****************************************************************
       10012-DUMMYSYORI-SEC          SECTION.
      *
      *    Ｋ０２のみ
           IF      LNK-ORCSC-PG        =   "K02"
               CONTINUE
           ELSE
               GO      TO      10012-DUMMYSYORI-EXT
           END-IF
      *
      *    ＤＵＭＭＹの診察料判定
           EVALUATE    WRK-COM-INPUTCD (IDX-Z)
               WHEN    "099110001"
      *            初診  同日診察でないこと
                   IF     (LNK-ORCSC-SYORIFLG  =   ZERO       )  AND
                          (LNK-ORCSC-LSTYMD    =   SPA-SRYYMD )  AND
                          (LNK-ORCSC-SYOYMD    NOT =   LNK-ORCSC-LSTYMD)
                       MOVE    "0138"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               WHEN    "099110001"
      *            再診  初診料が算定済みであること
                   IF     (LNK-ORCSC-SYORIFLG  =   ZERO       )  AND
                          (LNK-ORCSC-LSTYMD    =   SPACE      )
                       MOVE    "0139"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       10012-DUMMYSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    最終行
      *    薬剤点数計算
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-MAX             TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    往診料編集
           IF      WRK-COM-SRYKBN (IDX-MAX)    =   "14"
               MOVE    WRK-TENSU-O1        TO  LNK-SC90-TENSUKEI
           END-IF
      *    割合加算
           PERFORM 1006-KASAN-SET-SEC
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   LNK-SC90-TENGAKEI
                                       +   WRK-TENSU-O2
      *    各点数編集
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
      *    器材点数
           MOVE    LNK-SC90-KIZTEN     TO  WRK-COM-KIZAITEN (IDX-MAX)
      *
      *    点数計　計算
           COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-ZAITEN
                                           +   LNK-SC90-GAITEN
                                           +   LNK-SC90-KIZTEN
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
      *    院外分の時、ゼロとする
           IF      WRK-COM-SRYSYUKBN(IDX-MAX)  =   "148"  OR
                                                   "149"
               MOVE    ZERO            TO  WRK-COM-TENSU  (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-SYUTEN1 (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-KIZAITEN(IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-YKZTEN (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-KEI    (IDX-MAX)
           END-IF
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    往診料の時、往診区分により、点数集計を変更する
           IF     (WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1"  ) AND
                  (WRK-COM-SRYKBN      (IDX-Z)     =   "14" )  AND
                  (WRK-COM-TENSIKIBETU (IDX-Z) NOT =   5    )  AND
                  (WRK-COM-TUSOKUGAIKBN(IDX-Z) NOT =   "1"  )
               EVALUATE    WRK-COM-OSINKBN (IDX-Z)
                   WHEN    1
                   WHEN    2
                   WHEN    6
                       ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-O1
                   WHEN    OTHER
                       ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-O2
               END-EVALUATE
           END-IF
      *
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )  OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC               SECTION.
      *
      *    重複チェック
           PERFORM 2000-JYUFUKU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDX-ZAI
           MOVE    ZERO                TO  IDX-ZAI1
           MOVE    ZERO                TO  IDX-ZAI2
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-SYU2
           MOVE    ZERO                TO  FLG-GAIKAN
           MOVE    ZERO                TO  IDX-GAIKAN
           MOVE    ZERO                TO  LNK-ORCSC-CHK-IDX
      *
           MOVE    ZERO                TO  FLG-SYOKAI
           MOVE    ZERO                TO  FLG-SYOKAI2
           MOVE    ZERO                TO  FLG-TAHOKEN
      *
           MOVE    ZERO                TO  FLG-KENSIN
           MOVE    ZERO                TO  FLG-DUMMY-ARI
      *
           MOVE    SPACE               TO  WRK-KEICD
           MOVE    ZERO                TO  WRK-KEICD-IDX
           MOVE    ZERO                TO  WRK-RSISYOSIN
           MOVE    ZERO                TO  IDX-SINSATU
           MOVE    LNK-ORCSC-TOKUTEI-JNZ
                                       TO  WRK-TOKUTEI-JNZ
           MOVE    LNK-ORCSC-TOKUTEI-KNJ
                                       TO  WRK-TOKUTEI-KNJ
           MOVE    LNK-ORCSC-TOKUTEIG-KNJ
                                       TO  WRK-TOKUTEIG-KNJ
           MOVE    ZERO                TO  LNK-ORCSC-TOKUTEI-JNZ
           MOVE    ZERO                TO  LNK-ORCSC-TOKUTEI-KNJ
           MOVE    ZERO                TO  LNK-ORCSC-TOKUTEIG-KNJ
           MOVE    ZERO                TO  LNK-ORCSC-GAIRAI-TEN
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE    )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      ******** IF     (WRK-COM-ZAIEND(IDX-Z)   =    "1"  ) AND
      *               (WRK-COM-SRYKBN(IDX-Z)   =    "11"  OR  "12")
      *            ADD     1               TO  IDX-ZAI
      ******** END-IF
      *        診察料の判定
               IF      IDX-SINSATU         =   ZERO
                   PERFORM 2001-SINSATU-CHK-SEC
      *            回数のチェック
                   IF     (WRK-COM-SRYKBN(IDX-Z)   =   "12")  AND
                          (WRK-KAISU       NOT =   ZERO  )
      *            １回目
                       IF      LNK-ORCSC-SINSATU-CNT   =   ZERO
                          IF      WRK-KAISU           =   1
                              CONTINUE
                           ELSE
                              MOVE    "K020"          TO  WRK-KEICD
                              MOVE    IDX-Z           TO  WRK-KEICD-IDX
                           END-IF
                       END-IF
      *                ２、３回目
                       IF      LNK-ORCSC-SINSATU-CNT   =   1  OR  2
                           IF      WRK-KAISU       NOT =   2
                               MOVE    "K020"          TO  WRK-KEICD
                               MOVE    IDX-Z           TO  WRK-KEICD-IDX
                           END-IF
                       END-IF
                       IF      LNK-ORCSC-SINSATU-CNT   >   2
                           IF      WRK-KAISU       NOT =   4
                              MOVE    "K020"          TO  WRK-KEICD
                              MOVE    IDX-Z           TO  WRK-KEICD-IDX
                           END-IF
                       END-IF
                   END-IF
                   MOVE    WRK-KBN         TO  WRK-SINSATU-KBN
                   MOVE    WRK-KAISU       TO  WRK-SINSATU-KAISU
                   MOVE    WRK-KBN2        TO  WRK-SINSATU-KBN2
                   MOVE    IDX-Z           TO  IDX-SINSATU
      *            労災の時、労災コードで判定する
                   IF      SPA-HKNKBN      =   1
                       PERFORM 20012-RSISINSATU-CHK-SEC
                   END-IF
               END-IF
      *
      *        外来管理加算、継続管理加算の判定
               PERFORM 100111-KEIZOKU-CHK-SEC
      *        労災コードの判定
               PERFORM 200112-RSI-KEI-CHK-SEC
      *
      *********IF    ( WRK-COM-ZAIEND(IDX-Z)   NOT =    "1"   )  OR
      *              外来管理加算、継続加算以外のチェック
               IF    ( FLG-GAIRAI              =    1   OR
                       FLG-KEIZOK              =    1   OR
      *                労災分以外
                       FLG-RSIFLG              =    1         )
                   CONTINUE
               ELSE
                   IF      WRK-COM-DATAKBN(IDX-Z)  =    "1"
                       IF      WRK-COM-SRYKBN(IDX-Z)   =    "11"
                           ADD     1               TO  IDX-ZAI1
                           MOVE   IDX-Z            TO  IDX-SYU2
                       END-IF
                       IF      WRK-COM-SRYKBN(IDX-Z)   =    "12"
                           ADD     1               TO  IDX-ZAI2
                           MOVE   IDX-Z            TO  IDX-SYU2
                       END-IF
                   END-IF
               END-IF
      *
               IF     (WRK-COM-SRYKBN (IDX-Z)  =   "11"  OR "12" ) AND
                      (WRK-COM-DATAKBN(IDX-Z)  =   "1"           )
      *            外来管理加算、継続加算、同日再診、同日電話再診
                   IF      (FLG-GAIRAI         =   1  )  OR
                           (FLG-KEIZOK         =   1  )  OR
                           (FLG-RSIFLG         =   1  )  OR
                           (WRK-SINSATU-KBN    =   "D"   OR
                                                   "S" )
                       CONTINUE
                   ELSE
                       IF      LNK-ORCSC-CHK-IDX   >   ZERO
                           MOVE    "0017"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                       ELSE
                           MOVE    IDX-Z       TO  LNK-ORCSC-CHK-IDX
                       END-IF
                   END-IF
               END-IF
      *        併用算定チェック
      *?0608   IF      WRK-COM-SDOKNRYO (IDX-Z)    NOT =   ZERO
      *************PERFORM 220-SDOKNRYO-CHK-SEC
      *???     END-IF
      *
      *        外来管理加算算定チェック
               IF     (WRK-COM-SRYKBN (IDX-Z)  =    "12" )  AND
                      (WRK-COM-DATAKBN(IDX-Z)  =    "1" )
                   MOVE    IDX-Z               TO  IDX-SYU
               END-IF
      *
      *        外来管理加算算定
               IF     FLG-GAIRAI          =   1
      *            外来管理加算は１つのみ
                   IF      IDX-GAIKAN          >   ZERO
                       MOVE    "0058"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       MOVE    IDX-Z           TO  IDX-GAIKAN
                    END-IF
                END-IF
                IF     (WRK-COM-SRYKBN     (IDX-Z)
                                           NOT =   "95" AND "96") AND
                       (WRK-COM-GAIKANRIKBN(IDX-Z)
                                               =   1   )
                   MOVE    1               TO  FLG-GAIKAN
                END-IF
      * 
      *         指導料・初診日チェック
                IF      WRK-COM-INPUTCD(IDX-Z) =   "113001810"  OR
                                                   "113001910"  OR
                                                   "113002010"  OR
                                                   "113700610"  OR
                                                   "113700710"  OR
                                                   "113700810" 
                   MOVE    ZERO                TO  FLG-SIDOU
                   PERFORM 230-SIDOU-SYOSIN-SEC
               END-IF
      *         皮膚科特定疾患
               IF      WRK-COM-INPUTCD(IDX-Z) =    "113002310"
                                               OR  "113000910"
                   MOVE    1                   TO  FLG-SIDOU
                   PERFORM 230-SIDOU-SYOSIN-SEC
               END-IF
      * 
      *         老人慢性疾病外来総合診療科チェック
                IF      WRK-COM-INPUTCD(IDX-Z) =   "113701010"  OR
                                                   "113701110"  OR
                                                   "113701510" 
                   PERFORM 230-ROUMANSEI-SYOSIN-SEC
               END-IF
      *
      *        小児科外来診療科チェック
      *            初診
               IF      WRK-COM-INPUTCD(IDX-Z) =   "113003510"  OR
                                                  "113003710"
                   ADD     1               TO  IDX-ZAI1
               END-IF
      *            再診
               IF      WRK-COM-INPUTCD(IDX-Z) =   "113003610"  OR
                                                  "113003810"
                   ADD     1               TO  IDX-ZAI2
               END-IF
      *
      *        再診の４回目以降の判定
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            人工腎臓１
                   WHEN    "140036710"
      *            人工腎臓１（４時間以上）
                   WHEN    "140036810"
      *            人工腎臓１（５時間以上）
                   WHEN    "140037910"
      *            人工腎臓２
                   WHEN    "140007710"
      *            人工腎臓２（４時間以上）
                   WHEN    "140078100"
      *            人工腎臓２（５時間以上）
                   WHEN    "140038010"
                       MOVE    1           TO  LNK-ORCSC-TOKUTEI-JNZ
      *
      *        厚生大臣が定める患者判定
      *            慢性疼痛疾患管理料
                   WHEN    "113006510"
                       MOVE    1           TO  LNK-ORCSC-TOKUTEI-KNJ
      *        厚生大臣が定める患者判定（外来管理加算）
      *            ？？？？
      *            WHEN    "113006510"
      **************   MOVE    1           TO  LNK-ORCSC-TOKUTEIG-KNJ
               END-EVALUATE
      *
      *        特定薬剤治療管理初回算定コメントチェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "840000038"
      *            特定開始日和暦変換
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "11"                TO  LNK-DAY1-IRAI
                   MOVE    SPA-SRYYMD          TO  LNK-DAY1-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
                   MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
                   IF      WRK-WYM             =   LNK-DAY1-WYMD(2:4)
                       MOVE    1                   TO  FLG-SYOKAI
                   END-IF
               END-IF
      *
      *        他保険算定済み
               IF      WRK-COM-INPUTCD(IDX-Z) =   "099999902"
                   MOVE    1               TO  FLG-TAHOKEN
               END-IF
      *
      *        検診からのコメント
               IF      WRK-COM-SRYKBN (IDX-Z) =   "11"  OR  "12"
                   IF      WRK-COM-INPUTCD(IDX-Z)  =  "820000003"  OR
                                                      "820000004"  OR
                                                      "820000005"  OR
                                                      "820000006"
                       MOVE    1                   TO  FLG-KENSIN
                   END-IF
                   IF      WRK-COM-INPUTCD(IDX-Z)  =  "099110001"  OR
                                                      "099120001"
                       IF      FLG-DUMMY-ARI       =   1
                           MOVE    "0137"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                       ELSE
                           MOVE    1                   TO  FLG-DUMMY-ARI
                       END-IF
                   END-IF
               END-IF
      *
      *        注加算コードのチェック（すべての診区で行う）
               IF      (WRK-COM-SRYKBN     (IDX-Z)
                                           =   "95" OR  "96"
                                                    OR  "99")   OR
                       (WRK-COM-CHUKSNCD   (IDX-Z)
                                           =   SPACE  OR  ZERO )  OR
                       (WRK-COM-CHUKSNTUBAN(IDX-Z)
                                           =   SPACE  OR  ZERO)
                   CONTINUE
               ELSE
                   PERFORM 290-CHUKSNCHK-SEC
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
      *    各検診からのダミーがある時、診察料を削除する
           IF    ((FLG-KENSIN          =   1  )  OR
                  (FLG-DUMMY-ARI       =   1  ))  AND
                 ((IDX-ZAI1            >   ZERO) OR
                  (IDX-ZAI2            >   ZERO))
               PERFORM 291-DUMMY-DELETE-SEC
               IF      FLG-DUMMY           =   1
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *    初診料・再診料の重複チェック
           IF     (IDX-ZAI1            >   ZERO) AND
                  (IDX-ZAI2            >   ZERO)
               MOVE    "0017"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
               GO      TO      200-ALL-SYORI-EXT
           END-IF
           IF     (IDX-ZAI1            >   1   ) OR
                  (IDX-ZAI2            >   1   )
               MOVE    "0017"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *    新規患者入力の時、必ず初診料であること
           IF     (LNK-ORCSC-PG        =   "K02"  )  AND
                  (LNK-ORCSC-SYOSIN    =   1      )
               IF     (IDX-ZAI1            =   ZERO) AND
                      (IDX-ZAI2            =   1   )
                   MOVE    "1017"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *    他保険算定済みのコメントがある時、診察料がないこと
           IF      FLG-TAHOKEN         =   1
               IF      (IDX-ZAI1       >   ZERO )  OR
                       (IDX-ZAI2       >   ZERO )
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *
      *    初診・再診料の算定チェック
      *    訂正・入院の時、チェックなし
           IF     (LNK-ORCSC-SYORIFLG  =   ZERO )  AND
                  (SPA-NYUGAIKBN       =   "2"  )
              IF     (LNK-ORCSC-CHK-IDX   >   ZERO       )  AND
                      (LNK-ORCSC-LSTYMD    =   SPA-SRYYMD ) AND
      *               同日再診有無
                      (LNK-ORCSC-RRK-LSTYMD    NOT =   SPACE )
      *            労災には同日再診はないのでチェックなし
                   IF      SPA-HKNKBN          =   1
                       CONTINUE
                   ELSE
                       MOVE    "0101"              TO  LNK-ORCSC-KIDCD
                       GO      TO      200-ALL-SYORI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    以下外来のみとする
           IF      SPA-NYUGAIKBN       =   "1"
                GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
           IF      SPA-HKNKBN          =   1
      *        労災の時、外来管理加算の読み替えチェック
               IF      IDX-ZAI1            =   ZERO
      *            再診の時のみ
                   PERFORM 20011-RSIGAIKAN-SYORI-SEC
               END-IF
           ELSE
      *        労災以外の外来管理加算の算定チェック
               PERFORM 20012-GAIKAN-SYORI-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (WRK-KEICD       NOT =   SPACE )
               IF      WRK-COM-KZMFLG (WRK-KEICD-IDX)  =   SPACE
                   MOVE    "1"             TO  WRK-COM-KZMFLG
                                                       (WRK-KEICD-IDX)
                   MOVE    "1"             TO  WRK-COM-CUR
                                                       (WRK-KEICD-IDX)
                   MOVE    WRK-KEICD       TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    労災で救急医療管理加算算定は初診料算定時のみとする
           IF      WRK-RSISYOSIN       >   ZERO
               IF     (IDX-ZAI1        >   ZERO  )  AND
                      (IDX-ZAI2        =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "8010"         TO  SPA-ERRCD
                   MOVE    "1"            TO  WRK-COM-CUR(WRK-RSISYOSIN)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *
      *    人工腎臓・厚生大臣による再診料振替え処理
           IF      SPA-HKNKBN          =   ZERO
               PERFORM 20013-SAISIN-SYORI-SEC
           ELSE
      *        労災
               PERFORM 20014-RSISAISIN-SYORI-SEC
           END-IF
           IF      LNK-ORCSC-KIDCD NOT =   SPACE
               GO      TO       200-ALL-SYORI-EXT
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        特定薬剤治療管理料の算定チェック
               PERFORM 20010-TOKUTEI-ALLCHK-SEC
           END-IF
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    重複チェック処理
      *****************************************************************
       2000-JYUFUKU-CHK-SEC               SECTION.
      *
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  FLG-KEI
      *        診察で手技の時、重複チェックを行う
               IF     (WRK-COM-SRYKBN(IDX-Z)(1:1)  =   "1"   )  AND
                      (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "1"  )   AND
                      (WRK-COM-DATAKBN(IDX-Z)      =   "1"  )
      *            特定薬剤治療管理料
                   IF    WRK-COM-INPUTCD(IDX-Z)    =   "113000410" OR
                                                       "113000510"
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
      *            診療情報提供料（Ａ），(Ｂ),(Ｄ)は重複時警告とする
                   IF    WRK-COM-INPUTCD (IDX-Z) =   "180016110"
                                                 OR  "180016210"
                                                 OR  "113004410"
                      MOVE    1                  TO  FLG-KEI
                   END-IF
               END-IF
      *
               IF      FLG-CHK             =   1
                   MOVE    ZERO                TO  IDX-KEI
                   COMPUTE IDX-Z3          =   IDX-Z   +   1
                   PERFORM VARYING     IDX-Z2  FROM    IDX-Z3 BY  1
                           UNTIL      (IDX-Z2      >   SPA-MAX-LINE)  OR
                                      (SPA-ERRCD   NOT =   SPACE )  OR
                                      (WRK-GMN-INPUTCD (IDX-Z2)
                                                       =   SPACE )
                       IF     (WRK-COM-SRYKBN  (IDX-Z)
                                           =   WRK-COM-SRYKBN(IDX-Z2))
                         AND  (WRK-COM-INPUTCD (IDX-Z)
                                           =   WRK-COM-INPUTCD(IDX-Z2))
      *                    警告
                           IF      FLG-KEI         =   1
                               MOVE    IDX-Z2          TO  IDX-KEI
                           ELSE
      *                    エラー
                               MOVE    "0134"          TO  SPA-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z2)
                           END-IF
                       END-IF
                   END-PERFORM
      *
      *            警告
                   IF      FLG-KEI         =   1
                       IF     (IDX-KEI         =   ZERO )  AND
                              (WRK-COM-KAISU(IDX-Z)    >   1  )
                           MOVE    IDX-Z           TO  IDX-KEI
                       END-IF
                       IF      IDX-KEI             >   ZERO
                           IF      WRK-COM-KZMFLG (IDX-KEI)
                                                   =   SPACE
                               MOVE    "K016"      TO  SPA-ERRCD
                               MOVE    "1"         TO  WRK-COM-CUR
                                                             (IDX-KEI)
                               MOVE    "1"         TO  WRK-COM-KZMFLG
                                                             (IDX-KEI)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2000-JYUFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料チェック処理
      *****************************************************************
       2001-SINSATU-CHK-SEC                  SECTION.
      *
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-KBN
                       MOVE    SPACE           TO  WRK-KBN2
                       MOVE    ZERO            TO  WRK-KAISU
                   WHEN    WRK-COM-INPUTCD(IDX-Z)
                                               =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-KAISU (TBL-IDX) TO  WRK-KAISU
                       MOVE    TBL-KBN (TBL-IDX)   TO  WRK-KBN
                       MOVE    TBL-KBN2(TBL-IDX)   TO  WRK-KBN2
           END-SEARCH
      *
      *    労災保険のチェック
           IF      SPA-HKNKBN          =   1
      *        労災保険の時、電話再診以外はエラーとする
               IF     (WRK-KAISU       =   ZERO    )  AND
                      (WRK-KBN         =   SPACE   )  AND
                      (WRK-KBN2        =   SPACE   )
                   CONTINUE
               ELSE
      *            電話再診もエラー
                   IF      WRK-KBN         =   "T" OR  "S"
                       MOVE    "8021"          TO  WRK-ERRCD
                   ELSE
                       MOVE    "8001"          TO  WRK-ERRCD
                   END-IF
                   MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               END-IF
           END-IF
      *
      *    初診料の判定
           IF      WRK-KBN         NOT =   SPACE
               IF      WRK-KBN             =   "F"
                   MOVE    1               TO  LNK-ORCSC-SYOSIN2
               ELSE
                   MOVE    ZERO            TO  LNK-ORCSC-SYOSIN2
               END-IF
           END-IF
      *
           .
       2001-SINSATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    労災診察料の判定処理
      *****************************************************************
       20012-RSISINSATU-CHK-SEC          SECTION.
      *
      *    診察料検索
           SET     TBL-RSI              TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    SPACE               TO  WRK-KBN
                   MOVE    SPACE               TO  WRK-KBN2
                   MOVE    ZERO                TO  WRK-KAISU
               WHEN    WRK-COM-INPUTCD(IDX-Z)  =   TBLR-SRYCD (TBL-RSI)
                   MOVE    TBLR-KBN  (TBL-RSI) TO  WRK-KBN
                   MOVE    TBLR-KAISU(TBL-RSI) TO  WRK-KAISU
           END-SEARCH
      *
           IF      WRK-KBN             =   "F"  OR  "S"
               MOVE    WRK-KBN         TO  WRK-SINSATU-KBN
               MOVE    WRK-KAISU       TO  WRK-SINSATU-KAISU
               MOVE    IDX-Z           TO  IDX-SINSATU
      *
      *        発生日３月以降の回数チェック
               IF      SPA-SRYYMD      >=  SPA-RSI-HASSYOYMD3
                   IF      LNK-ORCSC-SINSATU-CNT   >=  5
                       IF      WRK-KAISU       NOT =   5
                           MOVE    "K020"          TO  WRK-KEICD
                           MOVE    IDX-Z           TO  WRK-KEICD-IDX
                       END-IF
                   ELSE
                       IF      WRK-KAISU           =   5
                           MOVE    "K020"          TO  WRK-KEICD
                           MOVE    IDX-Z           TO  WRK-KEICD-IDX
                       END-IF
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       20012-RSISINSATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    労災コードの判定処理
      *****************************************************************
       200112-RSI-KEI-CHK-SEC          SECTION.
      *
      *    労災の救急医療管理加算、再診料療養指導料は対象外とする
           IF       WRK-COM-INPUTCD(IDX-Z) =    "101110030"  OR
                                                "101120030"
                                           OR   "101110020"
               MOVE    1                   TO  FLG-RSIFLG
           ELSE
               MOVE    ZERO                TO  FLG-RSIFLG
           END-IF
      *
      *    労災の救急医療管理加算算定有無
           IF       WRK-COM-INPUTCD(IDX-Z) =   "101110030"
               MOVE    IDX-Z               TO  WRK-RSISYOSIN
           END-IF
      *
      *
           .
       200112-RSI-KEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来管理加算コード追加処理
      *****************************************************************
       20011-GAIKASAN-SET-SEC                  SECTION.
      *    労災の外来管理加算
           IF      SPA-HKNKBN          =   1
               PERFORM 200111-RSI-GAIKASAN-SEC
               GO      TO      20011-GAIKASAN-SET-EXT
           END-IF
      *
           IF      LNK-ORCSC-SINSATU-CNT     <=  2
      *        ３回目まで
               MOVE    1               TO  WRK-KAISU
           ELSE
               MOVE    2               TO  WRK-KAISU
           END-IF
      *
      *    一般の時、病院・診療所は同じ
           IF      LNK-ORCSC-ROUJIN    =   ZERO
               MOVE    ZERO            TO  WRK-HOSPSBT
           ELSE
               MOVE    SPA-HOSPSBT     TO  WRK-HOSPSBT
           END-IF
      *
      *
           MOVE    "G"                 TO  WRK-KBN
           MOVE    SPACE               TO  WRK-KBN2
           IF      FLG-ARI             =   ZERO
      *
      *        特定の患者
               IF     (LNK-ORCSC-TOKUTEIG-KNJ  =   1   OR
                       LNK-ORCSC-TOKUTEIG-KNJ2 =   1  )  AND
                      (WRK-KAISU           =   2  )
                   MOVE    2                   TO  WRK-KAISU
                   MOVE    "E"                 TO  WRK-KBN2
               END-IF
           END-IF
      *
      *    １５才未満
           IF    ((LNK-ORCSC-NENREI-YY     <   15 )  OR
                  (LNK-ORCSC-15MIMAN-ARI   =   1  )) AND
                  (WRK-KAISU               =   2  )
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
                     AND  (LNK-ORCSC-ROUJIN    =   TBLG-ROUJIN
                                                             (TBLG-IDX))
                     AND  (WRK-HOSPSBT         =   TBLG-HOSPSBT
                                                             (TBLG-IDX))
                     AND  (WRK-KBN             =   TBLG-KBN 
                                                             (TBLG-IDX))
                     AND  (WRK-KBN2            =   TBLG-KBN2
                                                             (TBLG-IDX))
                       MOVE    TBLG-SRYCD (TBLG-IDX)
                                               TO  WRK-SRYCD
           END-SEARCH
      *
           .
       20011-GAIKASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算コード追加処理（労災）
      *****************************************************************
       200111-RSI-GAIKASAN-SEC            SECTION.
      *
      *    外来管理加算コード追加
           IF      SPA-SRYYMD          <   SPA-RSI-HASSYOYMD3
      *        ３月以内
               MOVE    ZERO                TO  WRK-KAISU
               MOVE    "G"                 TO  WRK-KBN
           ELSE
               IF      LNK-ORCSC-SINSATU-CNT     >=  5
      *            ３月以降の同月５回目以降
                   MOVE    5                   TO  WRK-KAISU
                   MOVE    "G"                 TO  WRK-KBN
               ELSE
                   MOVE    1                   TO  WRK-KAISU
                   MOVE    "G"                 TO  WRK-KBN
               END-IF
           END-IF
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
      *
           .
       200111-RSI-GAIKASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算コード追加処理
      *****************************************************************
       2001-GAIKASAN-SEC            SECTION.
      *
      *    外来管理加算コードセット
           MOVE    ZERO                TO  FLG-ARI
           PERFORM 20011-GAIKASAN-SET-SEC
      *
           COMPUTE IDX-S               =   IDX-Z   +  1
      *
           MOVE    ZERO                TO  FLG-ADD
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              WRK-GMN-TBLREC (IDX-S)
               INITIALIZE              WRK-COM-TBLREC (IDX-S)
               COMPUTE IDX-S           =   IDX-S
                                       -   1
               GO      TO      2001-GAIKASAN-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
      *    自動追加分（入力時に追加）
      **** MOVE    "1"                 TO  WRK-COM-AUTOKBN (IDX-S)
           MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-S)
      *
           MOVE    1                   TO  FLG-ADD
           ADD     1                   TO  WRK-GMN-MAX
           .
       2001-GAIKASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料初診日チェック処理
      *****************************************************************
       230-SIDOU-SYOSIN-SEC            SECTION.
      *
      *    電話再診の時、エラーとする
           IF      LNK-ORCSC-TELSAI-ARI    =   1
               MOVE    "0077"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               GO      TO      230-SIDOU-SYOSIN-EXT
           END-IF
      *
           IF      LNK-ORCSC-SYORIFLG      =   1
      *        訂正時点の初診算定日
               MOVE    LNK-ORCSC-SYOYMD-OLD    TO  WRK-SYOYMD
           ELSE
               MOVE    LNK-ORCSC-SYOYMD        TO  WRK-SYOYMD
           END-IF
      *    最終退院日がある時、退院日
           IF     (LNK-ORCSC-TAIINYMD  =   SPACE )  OR
                  (LNK-ORCSC-TAIINYMD  <=  WRK-SYOYMD)
               CONTINUE
           ELSE
               MOVE    LNK-ORCSC-TAIINYMD  TO  WRK-SYOYMD
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
      *********MOVE    LNK-ORCSC-SYOYMD    TO  LNK-DAY6-KIJUN
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
      *!           MOVE    "0034"          TO  SPA-ERRCD
      *            １月後の前営業日と同じ時、ＯＫ
                   IF      FLG-SIDOU       =   1
                       MOVE    "0034"          TO  SPA-ERRCD
                   ELSE
                       IF      WRK-KEIYMDZEN   =   SPA-SRYYMD
                           IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
                             AND  (LNK-ORCSC-KEI-IDX       =   ZERO )
                               MOVE    IDX-Z       TO  LNK-ORCSC-KEI-IDX
                               MOVE    "1"         TO  WRK-COM-KZMFLG
                                                                (IDX-Z)
                               MOVE    "K002"          TO  SPA-ERRCD
                           END-IF
                       ELSE
                           MOVE    "0034"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               WHEN    WRK-KEIYMD          =   SPA-SRYYMD
                   CONTINUE
      *********    IF      FLG-SIDOU       =   1
      *                MOVE    "0034"          TO  SPA-ERRCD
      *            ELSE
      *                IF      FLG-KYUJITU     =   ZERO
      *                    MOVE    "0034"          TO  SPA-ERRCD
      *                ELSE
      *                    IF      WRK-COM-KZMFLG (IDX-Z)  =   SPACE
      *                        MOVE    "1"         TO  WRK-COM-KZMFLG
      *                                                         (IDX-Z)
      *                        MOVE    "K002"          TO  SPA-ERRCD
      *                    END-IF
      *                END-IF
      ***********  END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
           END-IF
           .
       230-SIDOU-SYOSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人慢性疾患外来総合診察料との初診日チェック
      *****************************************************************
       230-ROUMANSEI-SYOSIN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUJITU
           IF      LNK-ORCSC-SYOYMD    =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMD
               MOVE    LNK-ORCSC-SYOYMD        TO  WRK-SYOYMD
           ELSE
               IF      LNK-ORCSC-SYORIFLG      =   1
      *            訂正時点の初診算定日
                   MOVE    LNK-ORCSC-SYOYMD-OLD    TO  WRK-SYOYMD
               ELSE
                   MOVE    LNK-ORCSC-SYOYMD        TO  WRK-SYOYMD
               END-IF
               MOVE    WRK-SYOYMD          TO  WRK-KEIYMD
               COMPUTE WRK-KEI-MM          =   WRK-KEI-MM
                                           +   1
               IF      WRK-KEI-MM          >   12
                   MOVE    1                   TO  WRK-KEI-MM
                   COMPUTE WRK-KEI-YY          =   WRK-KEI-YY
                                               +   1
               END-IF
           END-IF
           IF      WRK-SYOYMD(7:2)     =   "01"
      *        初診料算定日が１日の時翌月から
               IF      WRK-KEIYMD(1:6)     <=  SPA-SRYYMD(1:6)
                   CONTINUE
               ELSE
                   MOVE    "0034"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        初診料算定日が１日以外の時、翌々月から
               IF      WRK-KEIYMD(1:6)     <   SPA-SRYYMD(1:6)
                   CONTINUE
               ELSE
                   MOVE    "0065"            TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
           END-IF
           .
       230-ROUMANSEI-SYOSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定薬剤治療管理料の算定チェック処理
      *****************************************************************
       20010-TOKUTEI-ALLCHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )  OR
                              (SPA-ERRCD   NOT =   SPACE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z)  =   SPACE  AND
                               WRK-COM-INPUTCD(IDX-Z)  =   SPACE  )
      *        特定薬剤治療加算料チェック
               IF      WRK-COM-INPUTCD (IDX-Z)     =   "180000110"  OR
                                                       "180000210"
                   MOVE    ZERO                TO  FLG-SYOKAI2
                   IF      LNK-ORCSC-TOKTEI-YMD    NOT  =  SPACE
      *                算定履歴検索
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       MOVE    "key3"              TO  WRK-DBPATH
                       PERFORM 100111-SANTEI-KEN-SEC
                       MOVE    "tbl_santei"        TO  MCP-TABLE
                       MOVE    WRK-DBPATH          TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
                       IF      FLG-SANTEI          =   ZERO
      *                    訂正の時、当日分はチェックしない
                           IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                                  (SANTEI-SRYYM         =   SPA-SRYYMD
                                                            (1:6))
                               MOVE    SPA-SRYYMD(7:2) TO  IDX-Y2
                               IF      SANTEI-DAY (IDX-Y2) >   ZERO
                                   MOVE    1           TO  FLG-SYOKAI2
                               END-IF
                           END-IF
      *                    今回の初回算定日より前の算定日の時はＯＫ
                           IF     (LNK-ORCSC-TOKTEI-YMD
                                                 >   SANTEI-FSANTEIYMD)
                               OR (FLG-SYOKAI    =   1     )
                               OR (FLG-SYOKAI2   =   1     )
                               CONTINUE
                           ELSE
                               MOVE    "0131"          TO  SPA-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                            (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20010-TOKUTEI-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算の算定チェック処理
      *****************************************************************
       20012-GAIKAN-SYORI-SEC       SECTION.
      *
           IF      FLG-GAIKAN          =   1
               IF      IDX-GAIKAN          >   ZERO
                   MOVE    "0102"              TO  LNK-ORCSC-KIDCD
                   MOVE    IDX-GAIKAN          TO  LNK-ORCSC-CHK-IDX
               END-IF
           ELSE
               IF     (IDX-SYU             >   ZERO )  AND
                      (IDX-GAIKAN          =   ZERO )  AND
                      (LNK-ORCSC-GAIFLG    =   ZERO )  AND
      *                自動算定なしの時、追加しない
                      (LNK-ORCSC-GAIKANRIKBN-CHK   NOT =   "2" )
                   IF     (LNK-ORCSC-ROUMANSEI-ARI     NOT =   ZERO ) OR
                          (LNK-ORCSC-ROUMANSEI-ARI2    NOT =   ZERO  AND
                           LNK-ORCSC-SINSATU-CNT       NOT =   3    ) OR
                          (LNK-ORCSC-TOKUTEI-KNJ2  =   1  )  OR
                          (LNK-ORCSC-TOKUTEI-KNJ   =   1  )  OR
                          (LNK-ORCSC-GAIRAI-NASI   =   1  )
                       CONTINUE
                   ELSE
                       MOVE    IDX-SYU         TO  IDX-Z
                       PERFORM
                           UNTIL  WRK-COM-DATAKBN (IDX-Z + 1) NOT =  "2"
                               ADD     1               TO  IDX-Z
                       END-PERFORM
      *                外来管理加算コード追加処理
                       PERFORM 2001-GAIKASAN-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       20012-GAIKAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災外来管理加算の読み替え処理
      *****************************************************************
       20011-RSIGAIKAN-SYORI-SEC       SECTION.
      *
      *    アフターケアの時、外来管理加算を算定しない
           IF      SPA-RSI-HKNKBN      =   "3"
               GO    TO      20011-RSIGAIKAN-SYORI-EXT
           END-IF
      *
      *    外来管理加算がないとき、労災のコードで点数マスタを検索
           IF      LNK-ORCSC-GAIRAI-TEN    =   ZERO
               PERFORM 200111-RSI-GAIKASAN-SEC
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-TEN         TO  LNK-ORCSC-GAIRAI-TEN
               END-IF
           END-IF
      *
           MOVE   ZERO                 TO  WRK-GAI-CNT
           MOVE   ZERO                 TO  WRK-GAI-IDX
           MOVE   99999999             TO  WRK-GAI-TEN
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
                MOVE    ZERO                TO  WRK-COM-RSIGIRI
                                                            (IDX-Z)
      *
                IF     (WRK-COM-SRYKBN     (IDX-Z)
                                           NOT =   "95" AND "96") AND
                       (WRK-COM-INPUTCD(IDX-Z)(1:3)
                                           NOT =   "101"  )   AND
                       (WRK-COM-GAIKANRIKBN(IDX-Z)
                                               =   1   )
                   IF     (WRK-COM-SYUTEN2 (IDX-Z) >  ZERO )  AND
                          (WRK-COM-SYUTEN2 (IDX-Z)
                                               <   LNK-ORCSC-GAIRAI-TEN)
                       ADD     1               TO  WRK-GAI-CNT
                       IF      WRK-COM-SYUTEN2 (IDX-Z)
                                               <   WRK-GAI-TEN
                           MOVE    WRK-COM-SYUTEN2 (IDX-Z)
                                               TO  WRK-GAI-TEN
                           MOVE    IDX-Z       TO  WRK-GAI-IDX
                       END-IF
                   END-IF
                END-IF
           END-PERFORM
      *
           IF      WRK-GAI-CNT         >   1
      *        外来管理加算へ読み替える
               MOVE    ZERO                TO  WRK-GAI-CNT
               PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
                    IF     (WRK-COM-SRYKBN     (IDX-Z)
                                          NOT =   "95" AND "96") AND
                           (WRK-COM-INPUTCD(IDX-Z)(1:3)
                                           NOT =   "101"  )   AND
                           (WRK-COM-GAIKANRIKBN(IDX-Z)
                                               =   1   )
                       IF     (WRK-COM-SYUTEN2 (IDX-Z)
                                               >   ZERO   )  AND
                              (WRK-COM-SYUTEN2 (IDX-Z)
                                               <   LNK-ORCSC-GAIRAI-TEN)
                           ADD     1               TO  WRK-GAI-CNT
                           IF      IDX-Z       NOT =   WRK-GAI-IDX
                               MOVE    1           TO  WRK-COM-RSIGIRI
                                                            (IDX-Z)
      *                        労災の時、読み替えあり
                               MOVE    "1"         TO  LNK-ORCSC-RSIOK
                           END-IF
                       END-IF
                    END-IF
               END-PERFORM
           END-IF
      *
      *    外来管理加算不可のコードがあり、５２点以下の手技が存在しない
           IF     (FLG-GAIKAN              =   1   OR
                   LNK-ORCSC-TOKUTEI-KNJ2  =   1  )  AND
                  (WRK-GAI-CNT             =   ZERO)
      *        外来管理加算削除
               IF      IDX-GAIKAN          >   ZERO
                   MOVE    "0102"              TO  LNK-ORCSC-KIDCD
                   MOVE    IDX-GAIKAN          TO  LNK-ORCSC-CHK-IDX
               END-IF
           ELSE
               IF     (IDX-GAIKAN          =   ZERO )  AND
                      (LNK-ORCSC-GAIFLG    =   ZERO )  AND
                      (WRK-GAI-CNT         >   ZERO)
                   IF      IDX-SYU2        =   ZERO
                       MOVE    ZERO            TO  IDX-Z
                   ELSE
                       MOVE    IDX-SYU2        TO  IDX-Z
                       PERFORM
                           UNTIL   WRK-COM-SRYKBN (IDX-Z) NOT =  "12"
                            OR     WRK-COM-ZAIEND (IDX-Z)     =  "1"
                           ADD     1               TO  IDX-Z
                       END-PERFORM
                   END-IF
      *            外来管理加算コード追加処理
                   PERFORM 2001-GAIKASAN-SEC
      *            労災読み替え加算分
                   IF      FLG-ADD         =   1
                       MOVE    "1"             TO  WRK-COM-RSIFLG2
                                                               (IDX-S)
                   END-IF
               ELSE
      *            外来管理加算が算定できる時
                   IF     (IDX-SYU2            >   ZERO )  AND
                          (FLG-GAIKAN          =   ZERO )  AND
                          (IDX-GAIKAN          =   ZERO )  AND
                          (LNK-ORCSC-SYOSIN    =   ZERO )  AND
                          (LNK-ORCSC-SYOSIN2   =   ZERO )
                       MOVE    IDX-SYU2        TO  IDX-Z
                       PERFORM
                           UNTIL   WRK-COM-SRYKBN (IDX-Z)
                                                         NOT =  "12"
                            OR     WRK-COM-ZAIEND (IDX-Z)    =  "1"
                           ADD     1               TO  IDX-Z
                       END-PERFORM
      *                外来管理加算コード追加処理
                       PERFORM 2001-GAIKASAN-SEC
                   END-IF
      *            外来管理加算の削除
      *            診察料が存在せず、外来管理加算がある時
                   IF     (IDX-SYU2            =   ZERO )  AND
                          (FLG-GAIKAN          =   ZERO )  AND
                          (IDX-GAIKAN          >   ZERO )  AND
                          (WRK-GAI-CNT         =   ZERO )  AND
                          (LNK-ORCSC-SYOSIN    =   ZERO )  AND
                          (LNK-ORCSC-SYOSIN2   =   ZERO )
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                        (IDX-GAIKAN)
                   END-IF
               END-IF
           END-IF
           .
       20011-RSIGAIKAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   人工腎臓・厚生大臣による再診料振替え処理
      *****************************************************************
       20013-SAISIN-SYORI-SEC                SECTION.
      *
      *    再診の４回目以降の時、人工腎臓があれば変更する
           IF      WRK-SINSATU-KAISU   =   4
             IF    ( LNK-ORCSC-SAISIN-CHK    =   ZERO  )  OR
                   ((LNK-ORCSC-TOKUTEI-JNZ
                                           NOT =  WRK-TOKUTEI-JNZ ) OR
                    (LNK-ORCSC-TOKUTEI-KNJ
                                           NOT =  WRK-TOKUTEI-KNJ ))
      *        診察料の決定
               MOVE    WRK-SINSATU-KAISU   TO  WRK-SAN-KAISU
               MOVE    WRK-SINSATU-KBN     TO  WRK-KBN
               PERFORM 100121-SINSATU-CD-SEC
               IF     (WRK-SRYCD       NOT =   SPACE )  AND
                      (WRK-COM-INPUTCD (IDX-SINSATU)
                                       NOT =   WRK-SRYCD)
      *            人工腎臓
                   IF      LNK-ORCSC-TOKUTEI-JNZ   =   1
                       IF      WRK-SINSATU-KBN2    NOT =   "J"
                           MOVE    "0115"          TO  LNK-ORCSC-KIDCD
                           GO      TO      20013-SAISIN-SYORI-EXT
                       END-IF
                   END-IF
      *            厚生大臣の
                   IF     (LNK-ORCSC-TOKUTEI-KNJ   =   1 )  OR
                          (LNK-ORCSC-TOKUTEI-KNJ2  =   1 )
                       IF      WRK-SINSATU-KBN2    NOT =   "E"
                           MOVE    "0114"          TO  LNK-ORCSC-KIDCD
                           GO      TO      20013-SAISIN-SYORI-EXT
                       END-IF
                   END-IF
      *
                   MOVE    ZERO            TO  LNK-ORCSC-SAISIN-CHK
      *            自動変更
                   MOVE    IDX-SINSATU         TO  IDX-T
                   MOVE    1                   TO  FLG-ARI
                   PERFORM 1100-KASAN-SET-SEC
                   MOVE    SPACE               TO  WRK-COM-AUTOKBN
                                                          (IDX-SINSATU)
               END-IF
             END-IF
           END-IF
      *
      *    外来管理加算の４回目以降の変更
           IF     (WRK-SINSATU-KAISU       =   4   )  AND
                  (LNK-ORCSC-TOKUTEIG-KNJ  NOT =  WRK-TOKUTEIG-KNJ)
      *        外来管理加算
               IF      IDX-GAIKAN          >   ZERO
      *            外来管理加算コード変更処理
                   MOVE    1               TO  FLG-ARI
                   PERFORM 20011-GAIKASAN-SET-SEC
                   MOVE    IDX-GAIKAN      TO  IDX-T
                   MOVE    1               TO  FLG-ARI
                   PERFORM 1100-KASAN-SET-SEC
                   MOVE    SPACE           TO  WRK-COM-AUTOKBN
                                                          (IDX-GAIKAN)
               END-IF
           END-IF
      *
           .
       20013-SAISIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災人工腎臓・厚生大臣による再診料振替え処理
      *****************************************************************
       20014-RSISAISIN-SYORI-SEC                SECTION.
      *
      *    ３ヶ月超で５回目以降の時、人工腎臓があれば変更する
           IF      WRK-SINSATU-KAISU       =   5
             IF  ( LNK-ORCSC-SAISIN-CHK    =   ZERO  )  OR
                 ((LNK-ORCSC-TOKUTEI-JNZ   NOT =  WRK-TOKUTEI-JNZ ) OR
                  (LNK-ORCSC-TOKUTEI-KNJ   NOT =  WRK-TOKUTEI-KNJ ) )
      *        人工腎臓・慢性疼痛
               IF     (LNK-ORCSC-TOKUTEI-JNZ   =   1   )  OR
                      (LNK-ORCSC-TOKUTEI-KNJ   =   1   )  OR
                      (LNK-ORCSC-TOKUTEI-KNJ2  =   1   )
                   MOVE    "0120"          TO  LNK-ORCSC-KIDCD
                   GO      TO      20014-RSISAISIN-SYORI-EXT
               END-IF
      *
               MOVE    ZERO            TO  LNK-ORCSC-SAISIN-CHK
             END-IF
           END-IF
      *
           .
       20014-RSISAISIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    注加算コードのチェック処理
      *****************************************************************
       290-CHUKSNCHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX-CHU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD(IDX)    =   SPACE )
               IF      IDX             NOT =   IDX-Z
      *                    注加算コード、通番が同じ時エラー
                   IF      (WRK-COM-SRYKBN (IDX)
                                           =   WRK-COM-SRYKBN
                                                           (IDX-Z)) AND
                           (WRK-COM-CHUKSNCD   (IDX)
                                           =   WRK-COM-CHUKSNCD
                                                           (IDX-Z)) AND
                           (WRK-COM-CHUKSNTUBAN(IDX)
                                           =   WRK-COM-CHUKSNTUBAN
                                                             (IDX-Z))
                       MOVE    "0042"          TO  SPA-ERRCD
                       MOVE    IDX             TO  IDX-CHU
                   END-IF
               END-IF
      *        診察料以外は、剤内のチェックとする
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   IF      WRK-COM-SRYKBN (IDX-Z)  =   "11"  OR  "12"
                       CONTINUE
                   ELSE
                       IF      IDX                 <   IDX-Z
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                       IF      IDX                 >=  IDX-Z
                           MOVE    SPA-MAX-LINE    TO  IDX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF      (SPA-ERRCD      NOT =   SPACE )  AND
                   (IDX-CHU        NOT =   ZERO  )
                MOVE    "1"             TO  WRK-COM-CUR(IDX-CHU)
           END-IF
      *
           .
       290-CHUKSNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ダミー診察料削除処理
      *****************************************************************
       291-DUMMY-DELETE-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-DUMMY
           MOVE    ZERO                TO  IDX-MAE-IDX
           MOVE    ZERO                TO  IDX-DUMMY
      *    診察料
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD(IDX)    =   SPACE AND
                               WRK-COM-INPUTCD(IDX)    =   SPACE )
      *        診察料のチェック
               IF     (WRK-GMN-INPUTCD (IDX)   NOT =   SPACE )  AND
                      (WRK-COM-SRYKBN  (IDX)       =   "11" OR
                                                       "12"  )  AND
      *        労災の振替えは削除しない
                      (WRK-COM-RSIFLG2 (IDX)       =   SPACE )
                   PERFORM 2911-SINSATU-CHK-SYORI-SEC
                   IF      FLG-ARI             =   1
      *                剤を削除する
                       MOVE    1                   TO  FLG-DUMMY
                       PERFORM 2912-ZAIDEL-SYORI-SEC
                   END-IF
               END-IF
      *        ＤＵＭＭＹセット
               IF     (IDX-DUMMY               =   ZERO )  AND
                      (IDX                     >   1    )  AND
                      (WRK-COM-ZAIEND(IDX - 1) =   "1"  ) AND
                      (WRK-COM-RSIFLG2 (IDX - 1 )
                                           NOT =   "1"   )
                   MOVE    IDX                 TO  IDX-MAE-IDX
               END-IF
               IF      WRK-COM-INPUTCD(IDX)    =   "099110001" OR
                                                   "099120001"
                   MOVE    IDX                 TO  IDX-DUMMY
               END-IF
           END-PERFORM
      *
           IF     (IDX-MAE-IDX         =   ZERO )  OR
                  (WRK-COM-RSIFLG2 (IDX-MAE-IDX)
                                       =   "1"   )
               GO      TO      291-DUMMY-DELETE-EXT
           END-IF
      *
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                      WRK-SCR-REC
           INITIALIZE                      HZN2-SCR-REC
           MOVE    ZERO                TO  FLG-DELETE
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-MAX-LINE
               IF     (HZN-GMN-INPUTCD(IDX)    =   SPACE ) AND
                      (HZN-COM-INPUTCD(IDX)    =   SPACE )
                   CONTINUE
               ELSE
                   IF     (IDX-MAE-IDX     <=  IDX       )  AND
                          (FLG-DELETE      =   ZERO      )
                       IF      HZN-COM-ZAIEND (IDX)    =   "1"
                           MOVE    1                   TO  FLG-DELETE
                       END-IF
                   ELSE
                       ADD     1                   TO  IDX2
                       MOVE    HZN-GMN-TBL (IDX)   TO  HZN2-GMN-TBL
                                                                  (IDX2)
                       MOVE    SPACE               TO  HZN-GMN-TBL (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *    ＤＵＭＭＹ診察料を最初へ
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-MAX-LINE
               IF     (HZN-GMN-INPUTCD(IDX)    =   SPACE ) AND
                      (HZN-COM-INPUTCD(IDX)    =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    HZN-GMN-TBL (IDX)   TO  WRK-GMN-TBL (IDX2)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-MAX-LINE
               IF     (HZN2-GMN-INPUTCD(IDX)   =   SPACE ) AND
                      (HZN2-COM-INPUTCD(IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    HZN2-GMN-TBL (IDX)  TO  WRK-GMN-TBL (IDX2)
               END-IF
           END-PERFORM
      *
           .
       291-DUMMY-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料削除処理
      *****************************************************************
       2912-ZAIDEL-SYORI-SEC                SECTION.
      *
           PERFORM VARYING     IDY     FROM    IDX BY  1
                   UNTIL      (IDY         >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD(IDY)    =   SPACE  AND
                               WRK-COM-INPUTCD(IDY)    =   SPACE  )
               IF      WRK-COM-INPUTCD (IDY)   NOT =   "099110001"  AND
                                                       "099120001"
                   MOVE    SPACE               TO  WRK-GMN-INPUTCD(IDY)
                   IF      WRK-COM-ZAIEND (IDY)    =   "1"
                       MOVE    SPA-MAX-LINE        TO  IDY
                   END-IF
               END-IF
           END-PERFORM
           .
       2912-ZAIDEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料チェッ処理
      *****************************************************************
       2911-SINSATU-CHK-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    ZERO            TO  FLG-ARI
                   WHEN    WRK-COM-INPUTCD(IDX)
                                               =   TBL-SRYCD (TBL-IDX)
                       MOVE    1               TO  FLG-ARI
           END-SEARCH
           IF      FLG-ARI             =   1
               GO      TO      2911-SINSATU-CHK-SYORI-EXT
           END-IF
      *
      *    外来管理加算、継続管理加算の判定
           SET     TBLG-IDX            TO  1
           SEARCH  TBL-GAIRAI-OCC      VARYING   TBLG-IDX
                   AT   END
                       MOVE    ZERO            TO  FLG-ARI
                   WHEN    WRK-COM-INPUTCD(IDX)    =   TBLG-SRYCD
                                                             (TBLG-IDX)
                       MOVE    1               TO  FLG-ARI
           END-SEARCH
           IF      FLG-ARI             =   1
               GO      TO      2911-SINSATU-CHK-SYORI-EXT
           END-IF
      *
      *    労災保険の外来管理加算の判定
           SET     TBL-RSI             TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    ZERO                TO  FLG-ARI
               WHEN    WRK-COM-INPUTCD(IDX)    =   TBLR-SRYCD (TBL-RSI)
                   MOVE    1                   TO  FLG-ARI
           END-SEARCH
           IF      FLG-ARI             =   1
               GO      TO      2911-SINSATU-CHK-SYORI-EXT
           END-IF
           .
       2911-SINSATU-CHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
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
      *****************************************************************
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
      *
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
