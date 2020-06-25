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
       01  SC11.
      *印刷日付
           03  SC11-SRYYM             PIC  X(16).
      *請求日付
           03  SC11-SEIYMD            PIC  X(22).
      *医療機関コード
           03  SC11-HOSPCDN           PIC  X(20).
      *医療機関所在地
           03  SC11-ADRS              PIC  X(100).
      *医療機関名称
           03  SC11-HOSPNAME          PIC  X(100).
      *管理者名
           03  SC11-KAISETU           PIC  X(80).
      *保険者番号
           03  SC11-HKNJANUM          PIC  X(12).
           03  SC11-HKNJANUM2         PIC  X(12).
      *保険区分
           03  SC11-HKNKBN            PIC  X(02) OCCURS 4.
      *明細テーブル
           03  SC11-MEISAI-TBL OCCURS 3.
      *給付割合
               05  SC11-KYURATE       PIC  X(06).
      *請求テーブル
               05  SC11-SEIKYU-TBL OCCURS 3.
      *件数
                   07  SC11-KENSU     PIC  X(05).
      *診療実日数
                   07  SC11-NISSU     PIC  X(05).
      *総点数
                   07  SC11-TENSU     PIC  X(09).
      *乳・障・母医療給付外点数
                   07  SC11-KYUFUGAI  PIC  X(07).
      *薬剤分
                   07  SC11-YKZFTN    PIC  X(07).
      *一部負担分
                   07  SC11-ITBFTN    PIC  X(07).
      *合計
                   07  SC11-GOKEI     PIC  X(09).
