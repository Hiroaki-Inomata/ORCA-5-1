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
      ************************************************************
      *    一般名記載ログ作成サブ   COPY (CPORCHC02GELOG.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCHC02GELOGAREA.
      *
      *    処理区分
           03  ORCHC02GELOG-SYORIKBN       PIC  X(01).
      *    内容テーブル
           03  ORCHC02GELOG-TBL-AREA.
               05  ORCHC02GELOG-TBL        OCCURS  100.
      *            伝票番号
                   07  ORCHC02GELOG-DENPNUM        PIC  9(07).
      *            剤番号
                   07  ORCHC02GELOG-ZAINUM         PIC  9(08).
      *            診療コード
                   07  ORCHC02GELOG-SRYCD          PIC  X(09).
      *            薬価基準コード（一般名コード）
                   07  ORCHC02GELOG-YAKKAKJNCD     PIC  X(12).
      *            一般名記載内容
                   07  ORCHC02GELOG-GENERICNAME    PIC  X(200).
      *
           03  ORCHC02GELOG-RCD            PIC 9(01).
      *
