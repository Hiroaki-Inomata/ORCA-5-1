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
       01  SC21.
      *診療年月
              03  SC21-SRYYM		PIC  X(16).
      *医療機関住所
              03  SC21-ADRS             PIC  X(100).
      *医療機関名称
              03  SC21-HOSPNAME         PIC  X(100).
      *医療機関電話番号
              03  SC21-TEL              PIC  X(15).
      *医療機関コード
              03  SC21-HOSPCD		PIC  X(14).
      *様式コード
              03  SC21-KOHKBN-TBL.
                  05  SC21-KOHKBN OCCURS 3  PIC X(02).
      *市町村数
              03  SC21-CNT-CITY         PIC  X(03).
      *市町村データテーブル
              03  SC21-CITY-TBL  OCCURS 11.
      *公費負担者番号
                  05  SC21-FTNJANUM     PIC  X(16).
      *市町村名
                  05  SC21-CITYNAME     PIC  X(40).
      *
                  05  SC21-HKN-TBL      OCCURS 2.
      *件数
                      07  SC21-KENSU    PIC  X(06).
      *請求金額
                      07  SC21-SEIKYUGK PIC  X(13).
      *合計テーブル
              03  SC21-GOKEI-TBL.
      *
                  05  SC21-GHKN-TBL     OCCURS 2.
      *合計件数
                  07  SC21-GKENSU       PIC  X(07).
      *合計請求金額
                  07  SC21-GSEIKYUGK    PIC  X(14).
