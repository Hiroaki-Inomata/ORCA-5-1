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
      *    夜間看護体制特定日減算取得（平成３０年度改正）
      *                             パラメタ　COPY (CPORCSKANGOGEN.INC)
      *                             使用プログラム (ORORCSKANGOGEN)
      *    設定項目
      *      医療機関番号
      *      患者ＩＤ
      *      算定年月
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSKANGOGEN-AREA.
      *
      *医療機関番号
           03  LNK-KANGOGEN-HOSPNUM           PIC 9(2).
      *患者ＩＤ
           03  LNK-KANGOGEN-PTID              PIC  9(10).
      *算定年月
           03  LNK-KANGOGEN-SANTEIYM          PIC  X(06).
      *返却項目
           03  LNK-KANGOGEN-HENKYAKU-AREA.
             05  LNK-KANGOGEN-TBL.
      *        夜間看護体制特定日減算コード
               07  LNK-KANGOGEN-OCC  OCCURS  31.
                   09  LNK-KANGOGEN-SRYCD         PIC  X(09).
                   09  LNK-KANGOGEN-TEN           PIC  9(05).
      *
      *復帰コード（１：病院以外　２：算定年月エラー　
      *            ３：患者検索エラー　４：入院会計なし
      *            ５：算定月に減額該当日の入力無し）
           03  LNK-KANGOGEN-RC                PIC S9(9) BINARY.
      *
