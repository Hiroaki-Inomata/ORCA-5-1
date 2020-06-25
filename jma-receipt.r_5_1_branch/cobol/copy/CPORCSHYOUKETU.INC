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
      *    標欠情報取得パラメタ　COPY (CPORCSHYOUKETU.INC)
      *                                使用プログラム (ORCSHYOUKETU.CBL)
      *    設定項目
      *      算定年月
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSHYOUKETU-AREA.
      *
      *算定年月
           03  LNK-HYOUKETU-SANTEIYM                 PIC  X(06).
      *返却項目
           03  LNK-HYOUKETU-HENKYAKU-AREA.
      *標欠返却テーブル
             04  LNK-HYOUKETU-HENKYAKU-TBL           OCCURS 2.
      *標欠コード
               05  LNK-HYOUKETU-SRYCD                PIC  X(09).
      *標欠開始年月
               05  LNK-HYOUKETU-STARTYM              PIC  X(06).
      *標欠終了年月
               05  LNK-HYOUKETU-ENDYM                PIC  X(06).
      *復帰コード（１：算定年月エラー　２：システム管理情報無し
      *            ３：標欠対象外）
           03  LNK-HYOUKETU-RC                    PIC S9(9) BINARY.
      *
