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
       01  SC25.
      *医療機関コード
           03  SC25-HOSPCD              PIC  X(14).
      *請求日付
           03  SC25-SEIYMD              PIC  X(22).
      *医療機関所在地
           03  SC25-ADRS                PIC  X(100).
      *医療機関名称
           03  SC25-HOSPNAME            PIC  X(100).
      *開設者名
           03  SC25-KAISETU             PIC  X(40).
      *合計請求金額
           03  SC25-GSEIKYUGK           PIC  X(09).
      *
           03  SC25-MEISAI-TBL OCCURS 9.
      *受給者番号
               05  SC25-JKYSNUM         PIC  X(14).
      *診療年月
               05  SC25-SRYYY           PIC  X(02).
               05  SC25-SRYMM           PIC  X(02).
      *入院外来区分
               05  SC25-NYUGAIKBN-TBL   OCCURS 2.
                   07  SC25-NYUGAIKBN   PIC  X(02).
      *診療日数
               05  SC25-NISSU           PIC  X(02).
      *保険総点数
               05  SC25-TENSU           PIC  X(10).
      *他法点数(結核・精神)
               05  SC25-TAHOU           PIC  X(10).
      *一部負担金(公費負担額)
               05  SC25-ITBFTN          PIC  X(07).
      *薬剤一部負担金
               05  SC25-YKZFTN          PIC  X(06).
      *入院日数
               05  SC25-SNISSU          PIC  X(02).
      *食事療養標準負担額
               05  SC25-FTNGK           PIC  X(06).
      *受給者氏名
               05  SC25-NAME            PIC  X(20).
      *本人家族区分
               05  SC25-HONKZKKBN-TBL OCCURS 2.
                   07  SC25-HONKZKKBN   PIC  X(02).
      *診療科名
               05  SC25-SRYKANAME       PIC  X(10).
      *合計
           03  SC23-GOKEI-TBL.
      *小計−一部負担金件数
               05  SC25-SKENSU          PIC  X(01).
      *小計−一部負担金
               05  SC25-SITBFTN         PIC  X(07).
      *小計−入院標準負担額
               05  SC25-SFTNGK          PIC  X(07).
      *小計−薬剤一部負担金件数
               05  SC25-SYKZKENSU       PIC  X(01).
      *小計−薬剤一部負担金額
               05  SC25-SYKZFTN         PIC  X(07).
      *合計−件数
               05  SC25-GKENSU          PIC  X(03).
      *合計−金額
               05  SC25-GITBFTN         PIC  X(09).
      *合計−入院標準負担額
               05  SC25-GFTNGK          PIC  X(07).
