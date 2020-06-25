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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川      2010/10/01   特別療養費国保対応
      *  01.00.02    井上      2018/11/12   修正履歴コメント削除
      ********************************************************************
       01  SKT1910.
      *特別療養費
           03  SKT1910-TOKUBETU            PIC  X(10).
      *請求テーブル
           03  SKT1910-SEIKYU  OCCURS  7.
      *保険者名
               05  SKT1910-HKNJANAME       PIC  X(20).
      *管掌テーブル
               05  SKT1910-KANSYO-TBL  OCCURS  5.
      *入外・食事テーブル
                   07  SKT1910-NYUGAI-SKJ-TBL OCCURS 3.
      *件数
                       09  SKT1910-KENSU        PIC  X(05).
      *日数
                       09  SKT1910-NISSU        PIC  X(05).
      *点数・金額
                       09  SKT1910-TENSU        PIC  X(09).
      *合計テーブル
           03  SKT1910-GOKEI-TBL. 
      *管掌テーブル
               05  SKT1910-G-KANSYO-TBL  OCCURS  5.
      *入外・食事テーブル
                   07  SKT1910-GNYUGAI-SKJ-TBL OCCURS 3.
      *合計件数
                       09  SKT1910-GKENSU        PIC  X(06).
      *合計日数
                       09  SKT1910-GNISSU        PIC  X(06).
      *合計点数・金額
                       09  SKT1910-GTENSU        PIC  X(10).
