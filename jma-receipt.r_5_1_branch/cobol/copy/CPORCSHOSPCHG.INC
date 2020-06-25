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
      *    医療機関コード切替え判定パラメタ　　　COPY (CPORCSHOSPCHG.INC)
      *                                使用プログラム (ORCSHOSPCHG)
      *    設定項目
      *      医療機関ＩＤ
      *      判定年月
      *      患者ＩＤ
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSHOSPCHG-AREA.
      *
      *医療機関ＩＤ（必須）
           03  LNK-HOSPCHG-HOSPNUM                   PIC  9(02).
      *判定年月（必須）
           03  LNK-HOSPCHG-HANTEIYM                  PIC  X(06).
      *患者ＩＤ（任意）
           03  LNK-HOSPCHG-PTID                      PIC  9(10).
      *返却項目
           03  LNK-HOSPCHG-HENKYAKU-AREA.
      *医療機関コード切替え区分（０：医療機関コード切替えなし
      *                          １：医療機関コード切替えあり）
             05  LNK-HOSPCHG-CHGKBN                  PIC  9(01).
      *医療機関コード返却エリア（添字１：旧医療機関　添字２：新医療機関）
             05  LNK-HOSPCHG-HENKYAKU-TABLE          OCCURS  2.
      *医療機関コード
               07  LNK-HOSPCHG-HOSPCD                PIC  9(07).
      *医療機関コード（漢字）
               07  LNK-HOSPCHG-HOSPCDN               PIC  X(40).
      *医療機関種別
               07  LNK-HOSPCHG-HOSPSBT               PIC  9(01).
      *有効開始年月日
               07  LNK-HOSPCHG-STYUKYMD              PIC  X(08).
      *有効終了年月日
               07  LNK-HOSPCHG-EDYUKYMD              PIC  X(08).
      *
      *復帰コード（１：判定年月エラー　２：患者検索エラー）
           03  LNK-HOSPCHG-RC                        PIC S9(9) BINARY.
      *
