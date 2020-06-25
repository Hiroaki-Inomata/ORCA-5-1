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
       01  SC17.
      *医療機関CD
           03  SC17-HOSPCD                 PIC  X(14).
      *医療機関所在地
           03  SC17-ADRS                   PIC  X(100).
      *医療機関名称
           03  SC17-HOSPNAME               PIC  X(100).
      *電話番号
           03  SC17-TEL                    PIC  X(15).
      *診療年月
           03  SC17-PRTYM                  PIC  X(12).
      *市町村名
           03  SC17-CITYNAME               PIC  X(16).
      *制度種別
           03  SC17-KOHKBN                 PIC  X(02) OCCURS 6.
      *市町村番号
           03  SC17-CITYNUM.
               05  SC17-CITYNUM-OCC        PIC  X(01) OCCURS 4.
      *受給者番号
           03  SC17-JKYSNUM.
               05  SC17-JKYSNUM-OCC        PIC  X(01) OCCURS 7.
      *氏名
           03  SC17-NAME                   PIC  X(24).
      *カナ氏名
           03  SC17-KANANAME               PIC  X(30).
      *性別
           03  SC17-SEX                    PIC  X(02) OCCURS 2.
      *元号
           03  SC17-GENGO                  PIC  X(02) OCCURS 4.
      *生年月日
           03  SC17-TANJOYMD               PIC  X(18).
      *給付割合
           03  SC17-KYURATE                PIC  X(02) OCCURS 4.
      *保険者番号
           03  SC17-HKNJANUM.
               05  SC17-HKNJANUM-OCC       PIC  X(01) OCCURS 8.
      *老人受給者番号
           03  SC17-RJNJKYSNUM.
               05  SC17-RJNJKYSNUM-OCC     PIC  X(01) OCCURS 7.
      *点数
           03  SC17-TENSU                  PIC  X(07) OCCURS 2.
      *保険負担額
           03  SC17-HKNGAKU                PIC  X(09) OCCURS 2.
      *日数
           03  SC17-NISSU                  PIC  X(02) OCCURS 2.
      *一部負担金
           03  SC17-ITBFTN                 PIC  X(07) OCCURS 2.
      *食事日数
           03  SC17-SNISSU                 PIC  X(02) OCCURS 4.
      *標準負担額
           03  SC17-SFTNGK                 PIC  X(06) OCCURS 5.
      *備考
           03  SC17-BIKO                   PIC  X(02) OCCURS 3.
      *公費点数
           03  SC17-TAHOTEN                PIC  X(07).
      *公費患者負担額
           03  SC17-TAHOFTN                PIC  X(07).
      *他法
           03  SC17-TAHO                   PIC  X(36).
      *限度額
           03  SC17-GENDO                  PIC  X(02) OCCURS 5.
