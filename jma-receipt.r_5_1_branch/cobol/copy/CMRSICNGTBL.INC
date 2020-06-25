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
      *----------------------------------------------------------------*
      *    労災コード置換えテーブル COPY (CMRSICNGTBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  13/11/01  労災電子化加算追加
      *****************************************************************
      *
       01  TBL-RSICHG-AREA.
      *    労災の救急医療管理加算
           03  FILLER      PIC X(20)   VALUE   "R101110020 101800880".
           03  FILLER      PIC X(20)   VALUE   "R101110030 101800890".
      *    労災の療養の給付請求書取扱料
           03  FILLER      PIC X(20)   VALUE   "R101130020 101800870".
      *    労災の初診時ブラッシング料
           03  FILLER      PIC X(20)   VALUE   "R101400030 101400030".
      *    労災の初診時ブラッシング料（手術）
           03  FILLER      PIC X(20)   VALUE   "R101400030 101500150".
      *    労災の手指の創傷に係る機能回復指導加算
           03  FILLER      PIC X(20)   VALUE   "R101500080 101500080".
      *
      *    労災の再就労療養指導管理料
           03  FILLER      PIC X(20)   VALUE   "A101130010 101800920".
           03  FILLER      PIC X(20)   VALUE   "A101130005 101800910".
      *
      *    再診料
           03  FILLER      PIC X(20)   VALUE   " 101120040 101120010".
      *    外来管理加算（読み替え）
           03  FILLER      PIC X(20)   VALUE   " 101120070 101120020".
      *    再診料（同一日２科目）
           03  FILLER      PIC X(20)   VALUE   " 101120100 101120040".
      *
      *    再診時療養指導管理料
           03  FILLER      PIC X(20)   VALUE   " 101120030 101130190".
      *    休業証明
           03  FILLER      PIC X(20)   VALUE   " 101130030 101800470".
      *    看護証明
           03  FILLER      PIC X(20)   VALUE   " 101130040 101800480".
      *    診断書料
           03  FILLER      PIC X(20)   VALUE   " 101130050 101800490".
      *    診断書料（はり・きゅう）
           03  FILLER      PIC X(20)   VALUE   " 101130060 101800500".
      *    診断書料（一般療養とはり・きゅう）
           03  FILLER      PIC X(20)   VALUE   " 101130070 101800510".
      *    診断書料（評価表添付）
           03  FILLER      PIC X(20)   VALUE   " 101130080 101800520".
      *    診断書料（マッサージ）
           03  FILLER      PIC X(20)   VALUE   " 101130090 101800530".
      *    介護（補償）給付診断書料
           03  FILLER      PIC X(20)   VALUE   " 101130100 101800540".
      *    リハビリテーション情報提供加算
           03  FILLER      PIC X(20)   VALUE   " 101130110 101130200".
      *    心大血管疾患リハビリテーション料（１）
           03  FILLER      PIC X(20)   VALUE   " 101800080 101800230".
      *    心大血管疾患リハビリテーション料（２）
           03  FILLER      PIC X(20)   VALUE   " 101800090 101800240".
      *    脳血管疾患等リハビリテーション料（１）
           03  FILLER      PIC X(20)   VALUE   " 101800100 101800250".
      *    脳血管疾患等リハビリテーション料（２）
           03  FILLER      PIC X(20)   VALUE   " 101800110 101800260".
      *    脳血管疾患等リハビリテーション料（３）
           03  FILLER      PIC X(20)   VALUE   " 101800120 101800270".
      *    運動器リハビリテーション料（１）
           03  FILLER      PIC X(20)   VALUE   " 101800125 101800280".
      *    運動器リハビリテーション料（２）
           03  FILLER      PIC X(20)   VALUE   " 101800130 101800290".
      *    運動器リハビリテーション料（３）
           03  FILLER      PIC X(20)   VALUE   " 101800140 101800330".
      *    呼吸器リハビリテーション料（１）
           03  FILLER      PIC X(20)   VALUE   " 101800150 101800340".
      *    呼吸器リハビリテーション料（２）
           03  FILLER      PIC X(20)   VALUE   " 101800160 101800350".
      *    ＡＤＬ加算
           03  FILLER      PIC X(20)   VALUE   " 101800170 101800360".
      *    職業復帰訪問指導料
           03  FILLER      PIC X(20)   VALUE   " 101800175 101800930".
      *    職業復帰訪問指導料（その他の疾患の場合）
           03  FILLER      PIC X(20)   VALUE   " 101800180 101800940".
      *    職業復帰訪問指導料（異なる職種２人以上）加算
           03  FILLER      PIC X(20)   VALUE   " 101800190 101800950".
      *    労災リハ（１．２倍）
           03  FILLER      PIC X(20)   VALUE   " 101800200 101800080".
      *    言語テスト
           03  FILLER      PIC X(20)   VALUE   " 101800210 101800090".
      *    心理相談指導管理料
           03  FILLER      PIC X(20)   VALUE   " 101800220 101800100".
      *    医療社会復帰指導管理料
           03  FILLER      PIC X(20)   VALUE   " 101800230 101800110".
      *    徒手筋力テスト
           03  FILLER      PIC X(20)   VALUE   " 101800240 101800120".
      *    関節可動域検査
           03  FILLER      PIC X(20)   VALUE   " 101800250 101800130".
      *    心理テスト
           03  FILLER      PIC X(20)   VALUE   " 101800260 101800140".
      *    心理テスト
           03  FILLER      PIC X(20)   VALUE   " 101800270 101800150".
      *    心理テスト（操作と処理が極めて複雑なもの）
           03  FILLER      PIC X(20)   VALUE   " 101800280 101800120".
      *    日常生活動作検査
           03  FILLER      PIC X(20)   VALUE   " 101800290 101800170".
      *    復職検査
           03  FILLER      PIC X(20)   VALUE   " 101800300 101800180".
      *    一般就労検査
           03  FILLER      PIC X(20)   VALUE   " 101800310 101800190".
      *    カウンセリング
           03  FILLER      PIC X(20)   VALUE   " 101800320 101800200".
      *    自助具の制作費
           03  FILLER      PIC X(20)   VALUE   " 101800330 101800210".
      *    補装具の処方料
           03  FILLER      PIC X(20)   VALUE   " 101800340 101800220".
      *    精神科職場復帰支援加算
           03  FILLER      PIC X(20)   VALUE   " 101800350 101800370".
      *    石綿疾患療養管理料
           03  FILLER      PIC X(20)   VALUE   " 101800360 101130160".
      *    石綿疾患労災請求指導料
           03  FILLER      PIC X(20)   VALUE   " 101800370 101800900".
      *    固定用伸縮性包帯
           03  FILLER      PIC X(20)   VALUE   " 788888001 788888003".
      *    腰部固定帯
           03  FILLER      PIC X(20)   VALUE   " 788888002 788888004".
      *    頸椎固定用シーネ
           03  FILLER      PIC X(20)   VALUE   " 788888003 788888007".
      *    鎖骨固定帯
           03  FILLER      PIC X(20)   VALUE   " 788888004 788888008".
      *    膝・足関節の創部固定帯
           03  FILLER      PIC X(20)   VALUE   " 788888005 788888009".
      *H25.10
      *    労災電子化加算（保険毎）
           03  FILLER      PIC X(20)   VALUE   "A101801000 101801000".
      *
       01  TBL-RSICHG-AREA-R       REDEFINES   TBL-RSICHG-AREA.
           03  TBL-RSICHG-OCC      OCCURS   59  INDEXED   TBL-RCN.
      *        (R:労災毎１回、A:保険毎）
               05  TBL-RSICHG-KBN             PIC X(01).
      *        前コード
               05  TBL-RSICHG-MAE-SRYCD       PIC X(09).
      *
               05  TBL-RSICHG-YOBI1           PIC X(01).
      *        後コード
               05  TBL-RSICHG-ATO-SRYCD       PIC X(09).
      *
