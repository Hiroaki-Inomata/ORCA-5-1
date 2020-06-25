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
       01  SC13.
      *診療年月
           03  SC13-PRTYM                     PIC  X(16).
      *請求年月日
           03  SC13-SEIYMD                    PIC  X(22).
      *証明年月日
           03  SC13-SHOMEIYMD                 PIC  X(22).
      *市町村名
           03  SC13-CITYNAME                  PIC  X(22).
      *申請者氏名
           03  SC13-SEINAME                   PIC  X(20).
      *医療機関住所
           03  SC13-HOSPADRS                  PIC  X(100).
      *医療機関名称
           03  SC13-HOSPNAME                  PIC  X(100).
      *開設者氏名
           03  SC13-KAISETU                   PIC  X(40).
      *受給者資格証番号
           03  SC13-JKYSNUM                   PIC  X(14).
      *受給者住所
           03  SC13-ADRS                      PIC  X(80).
      *受給者氏名
           03  SC13-NAME                      PIC  X(30).
      *生年月日
           03  SC13-TANJOYMD                  PIC  X(22).
      *被保険者氏名
           03  SC13-HI-NAME                   PIC  X(30).
      *公費区分
           03  SC13-KOHKBN                    PIC  X(02) OCCURS 3.
      *請求テーブル
           03  SC13-SEIKYU-TBL.
      *保険種別
               05  SC13-HKNSYUBETU    OCCURS 6 PIC X(02).
      *保険区分(一般・退職)
               05  SC13-HKNKBN        OCCURS 2 PIC X(02).
      *入院データ
               05  SC13-NYUIN-TBL.
      *入院−診療報酬点数
                   07  SC13-N-TENSU.
                       09  SC13-N-TEN           OCCURS 5 PIC X(02).
      *入院−他法負担額
                   07  SC13-N-THFTNGK.
                       09  SC13-N-THFTN         OCCURS 5 PIC X(02).
      *入院−保険者負担点数
                   07  SC13-N-HKNJAFTNGK.
                       09  SC13-N-HKNJAFTN      OCCURS 5 PIC X(02).
      *入院−一部負担額(本人負担額)
                   07  SC13-N-ITBFTN       PIC X(18).
      *入院標準負担
                   07  SC13-N-HYJFTN       PIC X(18).
      *入院開始年月日
                   07  SC13-N-NYUINYMD     PIC X(22).
      *入院−継続
                   07  SC13-KEIZOKU        PIC X(02).
      *入院−通院日数
                   07  SC13-N-NISSU        PIC X(04).
      *外来データ
               05  SC13-GAIRAI-TBL.
      *外来−診療報酬点数
                   07  SC13-G-TENSU.
                       09  SC13-G-TEN           OCCURS 5 PIC X(02).
      *外来−他法負担額
                   07  SC13-G-THFTNGK.
                       09  SC13-G-THFTN         OCCURS 5 PIC X(02).
      *外来−保険者負担点数
                   07  SC13-G-HKNJAFTNGK.
                       09  SC13-G-HKNJAFTN      OCCURS 5 PIC X(02).
      *外来−一部負担額(本人負担額)
                   07  SC13-G-ITBFTN       PIC  X(18).
      *外来−薬剤一部負担額
                   07  SC13-G-YKZFTN       PIC  X(18).
