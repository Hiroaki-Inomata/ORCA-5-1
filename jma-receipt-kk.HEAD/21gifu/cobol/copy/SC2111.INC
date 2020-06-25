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
      *表示年月
              03  SC11-PRTYM            PIC  X(16).
      *請求年月日
              03  SC11-SEIYMD           PIC  X(22).
      *医療機関住所
              03  SC11-ADRS             PIC  X(100).
      *医療機関名称
              03  SC11-HOSPNAME         PIC  X(100).
      *開設者名
              03  SC11-KAISETU          PIC  X(80).
      *医療機関コード
              03  SC11-HOSPCDN          PIC  X(20).
      *保険区分
              03  SC11-HKNKBN-TBL OCCURS 2.
                  05  SC11-HKNKBN       PIC  X(02).
      *患者テーブル
              03  SC11-DATA-TBL         OCCURS  10.
      *受給者番号
                  05  SC11-JKYSNUM      PIC  X(24).
      *氏名
                  05  SC11-NAME         PIC  X(20).
      *入院・外来区分
                  05  SC11-NYUGAIKBN1   PIC  X(02).
                  05  SC11-NYUGAIKBN2   PIC  X(02).
      *総点数
                  05  SC11-TOTALTEN     PIC  X(18).
      *結予・精神、公費負担点数
                  05  SC11-KOHFTNTEN    PIC  X(18).
      *一部負担額
                  05  SC11-ITBFTNGAKU   PIC  X(18).
      *合計請求
              03  SC11-GOKEI-TBL.
      *入院 件数
                  05  SC11-N-GKENSU     PIC  X(08).
      *入院 総点数
                  05  SC11-N-GTOTALTEN  PIC  X(18).
      *入院 結予、精神公費負担点数
                  05  SC11-N-GFTNTEN    PIC  X(18).
      *入院 一部負担額
                  05  SC11-N-GFTNGAKU   PIC  X(18).
      *外来 件数
                  05  SC11-G-GKENSU     PIC  X(08).
      *外来 総点数
                  05  SC11-G-GTOTALTEN  PIC  X(18).
      *外来 結予、精神公費負担点数
                  05  SC11-G-GFTNTEN    PIC  X(18).
      *外来 一部負担額
                  05  SC11-G-GFTNGAKU   PIC  X(18).
      *備考
              03  SC11-BIKO-TBL.
                  05  SC11-BIKO         PIC  X(16) OCCURS 10.
