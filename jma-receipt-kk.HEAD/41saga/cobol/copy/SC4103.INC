      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         11/04/15  帳票様式の変更に伴う修正
      *  01.00.02    萩野         19/02/07  修正履歴コメント削除
      ********************************************************************
       01  SC03.
      *印刷日付
           03  SC03-PRTYM             PIC  X(16).
      *医療機関所在地
           03  SC03-ADRS              PIC  X(100).
      *医療機関名称
           03  SC03-HOSPNAME          PIC  X(100).
      *電話番号
           03  SC03-TEL               PIC  X(15).
      *開設者名
           03  SC03-KAISETUNAME       PIC  X(30).
      *医療機関コード
           03  SC03-HOSPCDN           PIC  X(20).
      *請求テーブル
           03  SC03-SEIKYU-TBL   OCCURS  16.
      *保険者名
               05  SC03-HKNJANAME1         PIC  X(20).
      *入外テーブル（国保）
               05  SC03-NYUGAI-TBL1      OCCURS   2.
      *件数
                   07  SC03-KENSU1         PIC  X(05).
      *日数
                   07  SC03-NISSU1         PIC  X(05).
      *点数
                   07  SC03-TENSU1         PIC  X(10).
      *患者負担額
                   07  SC03-ITBFTN1        PIC  X(09).
      *市町村名
               05  SC03-HKNJANAME2         PIC  X(20).
      *入外テーブル（社保）
               05  SC03-NYUGAI-TBL2      OCCURS   2.
      *件数
                   07  SC03-KENSU2         PIC  X(05).
      *日数
                   07  SC03-NISSU2         PIC  X(05).
      *点数
                   07  SC03-TENSU2         PIC  X(10).
      *患者負担額
                   07  SC03-ITBFTN2        PIC  X(09).
      *合計テーブル
           03  SC03-GOKEI-TBL   OCCURS  2.
      *合計−件数（国保）
               05  SC03-GKENSU1        PIC  X(06).
      *合計−日数（国保）
               05  SC03-GNISSU1        PIC  X(06).
      *合計−点数（国保）
               05  SC03-GTENSU1        PIC  X(11).
      *合計−患者負担額（国保）
               05  SC03-GITBFTN1       PIC  X(11).
      *合計−件数（社保）
               05  SC03-GKENSU2        PIC  X(06).
      *合計−日数（社保）
               05  SC03-GNISSU2        PIC  X(06).
      *合計−点数（社保）
               05  SC03-GTENSU2        PIC  X(11).
      *合計−患者負担額（社保）
               05  SC03-GITBFTN2       PIC  X(11).
