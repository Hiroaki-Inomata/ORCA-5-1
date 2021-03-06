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
      *    夜勤時間特別入院基本料コード取得サブ
      *                                COPY (CPORCSYAKINTOKU.INC)
      *                                使用プログラム (ORCSYAKINTOKU)
      *    設定項目
      *      入院料コード
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSYAKINTOKU-AREA.
      *
      *算定年月
           03  LNK-YAKINTOKU-SANTEIYM              PIC  X(06).
      *病棟番号
           03  LNK-YAKINTOKU-BTU-NUM               PIC  X(02).
      *入院料コード
           03  LNK-YAKINTOKU-SRYCD                 PIC  X(09).
      *返却項目
           03  LNK-YAKINTOKU-HENKYAKU-AREA.
      *経過措置入院料コード
              05  LNK-YAKINTOKU-CHGSRYCD           PIC  X(09).
      *復帰コード（０：夜勤特別入院料返却あり　１：夜勤特別入院料返却なし）
           03  LNK-YAKINTOKU-RC                    PIC S9(9) BINARY.
      *
