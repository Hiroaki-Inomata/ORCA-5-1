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
      *    医療観察入院料コード取得パラメタ　　COPY (CPORCSIRYOKNS.INC)
      *                                使用プログラム (ORCSIRYOKNS)
      *    設定項目
      *      医療機関識別番号
      *      特定入院料区分
      *      特定入院料
      *      病棟番号
      *      算定年月
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSIRYOKNS-AREA.
      *
      *医療機関識別番号
           03  LNK-IRYOKNS-HOSPNUM                    PIC  9(02).
      *特定入院料区分（１：医療機関／２：病棟／３：病室／４：医療観察）
           03  LNK-IRYOKNS-KBN                        PIC  X(01).
      *特定入院料
           03  LNK-IRYOKNS-NYUIN                      PIC  X(02).
      *病棟番号
           03  LNK-IRYOKNS-BTUNUM                     PIC  X(02).
      *算定年月
           03  LNK-IRYOKNS-SANTEIYM                   PIC  X(06).
      *返却項目
           03  LNK-IRYOKNS-HENKYAKU-AREA.
      *医療観察入院料コード
               05  LNK-IRYOKNS-SRYCD                  PIC  X(09)
                                                      OCCURS  10.
      *復帰コード（０：医療観察入院料返却あり　１：医療観察入院料返却なし）
           03  LNK-IRYOKNS-RC                         PIC S9(9) BINARY.
      *
