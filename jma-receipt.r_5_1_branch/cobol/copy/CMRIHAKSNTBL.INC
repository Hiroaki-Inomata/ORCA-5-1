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
      *    リハビリの早期リハ加算テーブル COPY (CMRIHAKSNTBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  04/03/17  Ｈ１６．４改正対応
      *****************************************************************
      *
       01  TBL-RIHABIRI-AREA.
      *    理学早期リハ加算
           03  FILLER              PIC X(11)   VALUE   "180022970 1".
           03  FILLER              PIC X(11)   VALUE   "180023070 2".
           03  FILLER              PIC X(11)   VALUE   "180023170 3".
           03  FILLER              PIC X(11)   VALUE   "180023270 1".
           03  FILLER              PIC X(11)   VALUE   "180023370 2".
           03  FILLER              PIC X(11)   VALUE   "180023470 3".
           03  FILLER              PIC X(11)   VALUE   "180023570  ".
      *    作業早期リハ加算
           03  FILLER              PIC X(11)   VALUE   "180024070 1".
           03  FILLER              PIC X(11)   VALUE   "180024170 2".
           03  FILLER              PIC X(11)   VALUE   "180024270 3".
           03  FILLER              PIC X(11)   VALUE   "180024370 1".
           03  FILLER              PIC X(11)   VALUE   "180024470 2".
           03  FILLER              PIC X(11)   VALUE   "180024570 3".
           03  FILLER              PIC X(11)   VALUE   "180024670  ".
      *    理学早期リハ加算(老人）
           03  FILLER              PIC X(11)   VALUE   "180707670 1".
           03  FILLER              PIC X(11)   VALUE   "180707770 2".
           03  FILLER              PIC X(11)   VALUE   "180707870 3".
           03  FILLER              PIC X(11)   VALUE   "180707970  ".
           03  FILLER              PIC X(11)   VALUE   "180708570 1".
           03  FILLER              PIC X(11)   VALUE   "180708670 2".
           03  FILLER              PIC X(11)   VALUE   "180708770 3".
           03  FILLER              PIC X(11)   VALUE   "180708870  ".
      *    早期リハビリ加算（労災）
           03  FILLER              PIC X(11)   VALUE   "101800040 1".
           03  FILLER              PIC X(11)   VALUE   "101800050 2".
           03  FILLER              PIC X(11)   VALUE   "101800060 3".
           03  FILLER              PIC X(11)   VALUE   "101800070  ".
      *    理学療法外来移行加算(老人）
           03  FILLER              PIC X(11)   VALUE   "180708070".
      *    作業療法外来移行加算(老人）
           03  FILLER              PIC X(11)   VALUE   "180708970".
      *
      *    言語早期リハ加算
           03  FILLER              PIC X(11)   VALUE   "180025670 1".
           03  FILLER              PIC X(11)   VALUE   "180025770 2".
           03  FILLER              PIC X(11)   VALUE   "180025870 3".
      *    言語早期リハ加算（１５歳未満）
           03  FILLER              PIC X(11)   VALUE   "180025970 1".
           03  FILLER              PIC X(11)   VALUE   "180026070 2".
           03  FILLER              PIC X(11)   VALUE   "180026170 3".
      *
       01  TBL-RIHABIRI-AREA-R       REDEFINES   TBL-RIHABIRI-AREA.
      *****03  TBL-RIHABIRI-OCC      OCCURS   28  INDEXED   TBL-RIDX.
           03  TBL-RIHABIRI-OCC      OCCURS   34  INDEXED   TBL-RIDX.
               05  TBL-RIHA-SRYCD      PIC X(09).
               05  TBL-RIHA-F1         PIC X(01).
      *        1:１４日まで（イ）、2:１５日から３０日（ロ）
      *        3:３０日から９０日（ハ）
               05  TBL-RIHA-KBN        PIC X(01).
      *
      *平成１６年４月から
       01  TBL-RIHABIRI-AREA02.
      *    理学療法外来移行加算(老人）
           03  FILLER              PIC X(09)   VALUE   "180708070".
      *    作業療法外来移行加算(老人）
           03  FILLER              PIC X(09)   VALUE   "180708970".
      *
       01  TBL-RIHABIRI-AREA02-R     REDEFINES   TBL-RIHABIRI-AREA02.
           03  TBL-RIHABIRI2-OCC     OCCURS   2  INDEXED   TBL-RIDX2.
               05  TBL-RIHA2-SRYCD     PIC X(09).
      *
      *
      *    早期リハビリあり
       01  FLG-SOUKI                   PIC 9(01).
      *
