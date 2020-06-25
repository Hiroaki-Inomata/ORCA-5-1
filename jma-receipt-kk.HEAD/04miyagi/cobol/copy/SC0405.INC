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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    門間    2019/05/21  改元対応
      ********************************************************************
        01  SC05.
      *     診療年月
            03  SC05-SRYYM          PIC X(16).
      *     請求年月日
            03  SC05-SEIYMD         PIC X(22).
      *     医療機関コード
            03  SC05-HOSPCD         PIC X(14).
      *     医療機関所在地
            03  SC05-ADRS           PIC X(100).
      *     医療機関名称
            03  SC05-HOSPNAME       PIC X(100).
      *     医療機関電話番号
            03  SC05-TEL            PIC X(15).
      *     医療機関開設者氏名
            03  SC05-KAISETU        PIC X(40).
      *     市町村名
            03  SC05-CITYNAME       PIC X(26).
      *     ページ数
            03  SC05-PAGE           PIC X(03).
            03  SC05-SEIKYU-TBL         OCCURS  10 TIMES.
      *         入外区分
                05  SC05-NYUGAIKBN  PIC X(01).
      *         負担者番号
                05  SC05-FTNJANUM   PIC X(08).
      *         受給者番号
                05  SC05-JKYSNUM    PIC X(14).
      *         保険者番号
                05  SC05-HKNJANUM   PIC X(16).
      *         誕生年月日
                05  SC05-TANJOGG    PIC X(02).
                05  SC05-TANJOYY    PIC X(02).
                05  SC05-TANJOMM    PIC X(02).
                05  SC05-TANJODD    PIC X(02).
      *         氏名
                05  SC05-NAME       PIC X(26).
      *         性別
                05  SC05-SEX        PIC X(01).
      *         診療日数
                05  SC05-NISSU      PIC X(02).
      *         請求点数
                05  SC05-TENSU      PIC X(07).
      *         乳幼児医療費請求額
                05  SC05-SEIKYUGK   PIC X(07).
      *         食事日数
                05  SC05-SNISSU     PIC X(02).
      *         食事保険請求額
                05  SC05-SKINGK     PIC X(07).
      *         食事標準負担額
                05  SC05-SFTNGK     PIC X(07).
      *         公費番号
                05  SC05-KOHNUM     PIC X(02).
      *         マル長
                05  SC05-TYOKI      PIC X(02).
      *
            03  SC05-GOKEI-TBL.
      *         合計件数
                05  SC05-GKENSU     PIC X(02).
      *         合計点数
                05  SC05-GTENSU     PIC X(09).
      *         合計乳幼児医療費請求額
                05  SC05-GSEIKYUGK  PIC X(09).
      *         合計食事保険請求額
                05  SC05-GSKINGK    PIC X(09).
      *         合計食事標準負担額
                05  SC05-GSFTNGK    PIC X(09).
