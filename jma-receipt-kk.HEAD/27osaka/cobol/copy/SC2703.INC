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
       01  SC03.
      *表示年月
              03  SC03-PRTYM            PIC  X(16).
      *請求年月日
              03  SC03-SEIYMD           PIC  X(22).
      *医療機関コード
              03  SC03-HOSPCDN          PIC  X(20).
      *ページ番号
              03  SC03-PAGE             PIC  X(02).
      *住所
              03  SC03-ADRS             PIC  X(100).
      *医療機関名
              03  SC03-HOSPNAME	        PIC  X(100).
      *管理者名
              03  SC03-KAISETU          PIC  X(40).
      *電話番号
              03  SC03-TEL              PIC  X(15).
      *テーブル
              03  SC03-SRY-TBL          OCCURS  10.
      *負担者番号(上２桁)
                  05  SC03-FTNJANUM1    PIC  X(04).
      *負担者番号(下４桁)
                  05  SC03-FTNJANUM2    PIC  X(08).
      *受給者番号
                  05  SC03-JKYSNUM      PIC  X(14).
      *受給者氏名
                  05  SC03-NAME         PIC  X(20).
      *被保険者証：記号・番号
                  05  SC03-KIGO	        PIC  X(20).
                  05  SC03-NUM          PIC  X(20).
      *入院外来/年齢
                  05  SC03-NYUGAI-TBL   OCCURS  2.
                      07  SC03-AGE-TBL  OCCURS  5.
                          09  SC03-KBN  PIC  X(02).
      *診療年月
                  05  SC03-SRYYM        PIC  X(08).
      *日数
                  05  SC03-NISSU        PIC  X(02).
      *請求点数
                  05  SC03-TENSU        PIC  X(07).
      *公費分点数（精神・結核等）
                  05  SC03-KOHTENSU     PIC  X(07).
      *一部負担金
                  05  SC03-ITBFTN       PIC  X(06).
      *食事日数
                  05  SC03-SNISSU       PIC  X(02).
      *食事療養費
                  05  SC03-SKINGK       PIC  X(06).
      *食事療養費標準負担額
                  05  SC03-SFTNGK       PIC  X(06).
      *長期療養
                  05  SC03-TYOKI        PIC  X(02).
      *備考
                  05  SC03-BIKO1        PIC  X(02).
                  05  SC03-BIKO2        PIC  X(02).
                  05  SC03-BIKO3        PIC  X(02).
                  05  SC03-BIKO4        PIC  X(02).
                  05  SC03-BIKO5        PIC  X(06).
