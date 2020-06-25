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
      *    酸素コードテーブル   COPY (CMSANSOTBL.INC)                  *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *    酸素コードテーブル
       01  TBL-SANSO-AREA.
      *    液体酸素・定置式
           03  FILLER              PIC X(09)   VALUE   "739200000".
      *    液体酸素・可般式
           03  FILLER              PIC X(09)   VALUE   "739210000".
      *    酸素ボンベ大型
           03  FILLER              PIC X(09)   VALUE   "739220000".
      *    酸素ボンベ小型
           03  FILLER              PIC X(09)   VALUE   "739230000".
      *    液体酸素・定置式（離島）
           03  FILLER              PIC X(09)   VALUE   "739240000".
      *    液体酸素・可般式（離島）
           03  FILLER              PIC X(09)   VALUE   "739250000".
      *    酸素ボンベ大型（離島）
           03  FILLER              PIC X(09)   VALUE   "739260000".
      *    酸素ボンベ小型（離島）
           03  FILLER              PIC X(09)   VALUE   "739270000".
      *
       01  TBL-SANSO-AREA-R        REDEFINES   TBL-SANSO-AREA.
           03  TBL-SANSO-OCC       OCCURS   8  INDEXED   TBL-SANSO.
               05  TBL-SANSO-SRYCD     PIC X(09).
      *    酸素フラグ
       01  FLG-SANSO-OK                PIC 9(01).
      *
      *
