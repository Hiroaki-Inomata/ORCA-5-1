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
      *    栄養管理経過措置入院料コード取得サブパラメタ　　COPY (CPORCSEIYOUKKS.INC)
      *                                使用プログラム (ORCSEIYOUKKS)
      *    設定項目
      *      入院料コード
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSEIYOUKKS-AREA.
      *
      *算定年月
           03  LNK-EIYOUKKS-SANTEIYM               PIC  X(06).
      *入院料コード
           03  LNK-EIYOUKKS-SRYCD                  PIC  X(09).
      *返却項目
           03  LNK-EIYOUKKS-HENKYAKU-AREA.
      *経過措置入院料コード（平成２６年３月まで返却）
              05  LNK-EIYOUKKS-CHGSRYCD            PIC  X(09).
      *経過措置入院料コード（平成２６年４月以降のレセプト略称記載用に返却）
              05  LNK-EIYOUKKS-RECE-CHGSRYCD       PIC  X(09).
      *復帰コード（０：経過措置入院料返却あり　１：経過措置入院料返却なし）
           03  LNK-EIYOUKKS-RC                     PIC S9(9) BINARY.
      *
