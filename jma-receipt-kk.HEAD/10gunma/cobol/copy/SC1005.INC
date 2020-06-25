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
        01  SC05.
      *診療年月日
            03  SC05-SRYYM                          PIC X(16).
      *医療機関点数表区分
            03  SC05-TENHYOKBN                      PIC X(02).
      *医療機関コード
            03  SC05-HOSPCD                         PIC X(40).
      *医療機関住所
            03  SC05-ADRS                           PIC X(100).
      *医療機関名称
            03  SC05-HOSPNAME                       PIC X(100).
      *電話番号
            03  SC05-TEL                            PIC X(15).
      *開設者名
            03  SC05-KANRINAME                      PIC X(40).
      *請求テーブル
            03  SC05-TBL                            OCCURS  20 TIMES.
      *保険者番号
              05  SC05-HKNJANUM                     PIC X(12).
      *給付割合
              05  SC05-KYUFURATE-TBL                OCCURS  5 TIMES.
                07  SC05-KYURATE                    PIC X(02).
      *入外テーブル
              05  SC05-NYUGAIKBN-TBL                OCCURS  2 TIMES.
                07  SC05-NYUGAIKBN                  PIC X(02).
      *件数
              05  SC05-KENSU                        PIC X(03).
      *点数
              05  SC05-TENSU                        PIC X(09).
      *福祉医療費請求額
              05  SC05-FKSGAKU                      PIC X(10).
