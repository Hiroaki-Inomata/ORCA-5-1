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
       01  SC20.
      *医療機関コード
           03  SC20-HOSPCD              PIC  X(14).
      *請求日付
           03  SC20-SEIYMD              PIC  X(22).
      *医療機関所在地
           03  SC20-ADRS                PIC  X(100).
      *医療機関名称
           03  SC20-HOSPNAME            PIC  X(100).
      *管理者名
           03  SC20-KAISETU             PIC  X(40).
      *合計請求金額
           03  SC20-GSEIKYUGK           PIC  X(09).
      *
           03  SC20-MEISAI-TBL OCCURS 10.
      *受給者番号
               05  SC20-JKYSNUM         PIC  X(14).
      *保険者番号
               05  SC20-HKNJANUM        PIC  X(16).
      *診療年月
               05  SC20-SRYYY           PIC  X(02).
               05  SC20-SRYMM           PIC  X(02).
      *入院外来区分
               05  SC20-NYUGAIKBN-TBL OCCURS 2.
                   07  SC20-NYUGAIKBN   PIC  X(02).
      *診療日数
               05  SC20-NISSU           PIC  X(02).
      *保険総点数
               05  SC20-TENSU           PIC  X(12).
      *他法点数
               05  SC20-TAHOU           PIC  X(12).
      *一部負担金
               05  SC20-ITBFTN          PIC  X(07).
      *入院日数
               05  SC20-SNISSU          PIC  X(02).
      *食事療養標準負担額
               05  SC20-FTNGK           PIC  X(07).
      *受給者氏名
               05  SC20-NAME            PIC  X(20).
      *診療科名
               05  SC20-SRYKANAME       PIC  X(10).
      *小計エリア
           03  SC20-SYOKEI-AREA.
               05  SC20-SKENSU         PIC  X(02).
               05  SC20-SITBFTN        PIC  X(09).
               05  SC20-SFTNGK         PIC  X(07).
      *合計エリア
           03  SC20-GOKEI-AREA.
               05  SC20-GKENSU           PIC  X(03).
               05  SC20-GITBFTN          PIC  X(09).
               05  SC20-GFTNGK           PIC  X(09).
