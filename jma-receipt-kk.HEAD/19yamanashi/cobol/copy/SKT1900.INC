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
      *  プログラム修正履歴
      * Maj/Min/Rev 修正者  日付        内容
      *  01.00.01   吉川    2008/05/27  帳票様式の変更に伴う修正
      *  01.00.02   吉川    2008/06/06  ページ番号追加
      *  01.00.03   吉川    2010/10/01  特別療養費国保対応
      *  01.00.04   井上    2018/11/12  修正履歴コメント削除
      ********************************************************************
       01  SKT1900.
      *診療年月
           03  SKT1900-SRYYM               PIC  X(16).
      *医療機関コード
           03  SKT1900-HOSPCDN             PIC  X(20).
      *住所
           03  SKT1900-ADRS                PIC  X(100).
      *医療機関名
           03  SKT1900-HOSPNAME            PIC  X(100).
      *開設者氏名
           03  SKT1900-KAISETUNAME         PIC  X(40).
      *ページ番号
           03  SKT1900-NO                  PIC  X(04).
      *特別療養費
           03  SKT1900-TOKUBETU            PIC  X(10).
      *請求テーブル
           03  SKT1900-SEIKYU  OCCURS  10.
      *保険者番号
               05  SKT1900-HKNJANUM        PIC  X(16).
      *保険者名
               05  SKT1900-HKNJANAME       PIC  X(20).
      *給付割合(国保組合用)
               05  SKT1900-KYURATE         PIC  X(10).
      *管掌テーブル
               05  SKT1900-KANSYO-TBL  OCCURS  9.
      *入外・食事テーブル
                   07  SKT1900-NYUGAI-SKJ-TBL OCCURS 3.
      *件数
                       09 SKT1900-KENSU        PIC  X(05).
      *日数
                       09 SKT1900-NISSU        PIC  X(05).
      *点数・金額
                       09 SKT1900-TENSU        PIC  X(09).
      *合計テーブル
           03  SKT1900-GOKEI-TBL. 
      *管掌テーブル
               05  SKT1900-G-KANSYO-TBL  OCCURS  9.
      *入外・食事テーブル
                   07  SKT1900-GNYUGAI-SKJ-TBL OCCURS 3.
      *合計件数
                       09 SKT1900-GKENSU        PIC  X(06).
      *合計日数
                       09 SKT1900-GNISSU        PIC  X(06).
      *合計点数・金額
                       09 SKT1900-GTENSU        PIC  X(10).
