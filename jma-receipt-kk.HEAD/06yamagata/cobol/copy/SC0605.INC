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
        01  SC05.
      *請求年月日
            03  SC05-SRYYM             PIC X(16).
      *診療年月
            03  SC05-SEIYMD            PIC X(22).
      *保険者名
            03  SC05-HKNJANAME         PIC X(26).
      *保険者番号
            03  SC05-HKNJANUM          PIC X(6).
      *点数表
            03  SC05-TENHYOKBN         PIC X(2)    OCCURS  4 TIMES.
      *医療機関コード
            03  SC05-HOSPCD            PIC X(7).
      *医療機関住所
            03  SC05-ADRS              PIC X(100).
      *医療機関名
            03  SC05-HOSPNAME          PIC X(100).
      *開設者氏名
            03  SC05-KAISETU           PIC X(40).
      *電話番号
            03  SC05-TEL               PIC X(15).
      *ＦＡＸ
            03  SC05-FAX               PIC X(15).
      *頁数
            03  SC05-NO                PIC X(4).
      *請求テーブル
            03  SC05-SEIKYU81-TBL       OCCURS  7 TIMES.
                05  SC05-KENSU81       PIC X(5).
                05  SC05-TENSU81       PIC X(9).
            03  SC05-SEIKYU82-TBL       OCCURS  7 TIMES.
                05  SC05-KENSU82       PIC X(5).
                05  SC05-TENSU82       PIC X(9).
                05  SC05-ITBFTN82      PIC X(9).
            03  SC05-SEIKYU83-TBL       OCCURS  2 TIMES.
                05  SC05-KENSU83       PIC X(5).
                05  SC05-TENSU83       PIC X(9).
            03  SC05-SEIKYU84-TBL       OCCURS  2 TIMES.
                05  SC05-KENSU84       PIC X(5).
                05  SC05-TENSU84       PIC X(9).
                05  SC05-ITBFTN84      PIC X(9).
            03  SC05-SEIKYU85-TBL       OCCURS  7 TIMES.
                05  SC05-KENSU85       PIC X(5).
                05  SC05-TENSU85       PIC X(9).
