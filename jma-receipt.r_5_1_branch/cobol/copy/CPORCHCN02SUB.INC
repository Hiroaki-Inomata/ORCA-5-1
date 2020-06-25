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
      *    退院証明書用入院取得パラメタ　　COPY (CPORCHCN02.INC)
      *                              使用プログラム (ORCHCN02.CBL)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      入院日
      *      退院日
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCHCN02SUB-AREA.
      *
      *医療機関ＩＤ
           03  LNK-ORCHCN02SUB-HOSPNUM                PIC 9(2).
      *患者ＩＤ
           03  LNK-ORCHCN02SUB-PTID                   PIC  9(10).
      *入院日
           03  LNK-ORCHCN02SUB-NYUINYMD               PIC  X(08).
      *退院日
           03  LNK-ORCHCN02SUB-TAIINYMD               PIC  X(08).
      *返却項目
           03  LNK-ORCHCN02SUB-HENKYAKU-AREA.
      *入院返却テーブル
             04  LNK-ORCHCN02SUB-HENKYAKU-TABLE       OCCURS  20.
      *入院料コード
               05  LNK-ORCHCN02SUB-SYUBETUCD          PIC  X(09).
      *入院料名称
               05  LNK-ORCHCN02SUB-SYUBETUMEI         PIC  X(50).
      *入院日数
               05  LNK-ORCHCN02SUB-NISSU              PIC  9(05).
      *入院期間
             04  LNK-ORCHCN02SUB-KIKAN                PIC  9(05).
      *入院期間開始日
             04  LNK-ORCHCN02SUB-KIKANSTRYMD          PIC  X(08).
      *入院期間終了日
             04  LNK-ORCHCN02SUB-KIKANENDYMD          PIC  X(08).
      *選定入院期間
             04  LNK-ORCHCN02SUB-TKIKAN               PIC  9(05).
      *
      *復帰コード（１：パラメタエラー　２：入院会計なし
      *            ３：通算取得エラー）
           03  LNK-ORCHCN02SUB-RC                     PIC S9(9) BINARY.
      *
