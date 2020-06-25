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
      *    亜急性期入院医療管理料判定パラメタ　　　COPY (CPORCSAKYUSEI.INC)
      *                                使用プログラム (ORCSAKYUSEI)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定年月
      *      保険区分
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSAKYUSEI-AREA.
      *
      *医療機関ＩＤ
           03  LNK-AKYUSEI-HOSPNUM                PIC  9(02).
      *患者ＩＤ
           03  LNK-AKYUSEI-PTID                   PIC  9(10).
      *入院履歴番号
           03  LNK-AKYUSEI-RRKNUM                 PIC  9(03).
      *
      *復帰コード
      *     ０：平成２４年３月３１日に亜急性期入院医療管理料の算定無し
      *     １：平成２４年３月３１日に亜急性期入院医療管理料１を算定
      *     ２：平成２４年３月３１日に亜急性期入院医療管理料２を算定
      *     ９：その他エラー
           03  LNK-AKYUSEI-RC                     PIC S9(9) BINARY.
      *
