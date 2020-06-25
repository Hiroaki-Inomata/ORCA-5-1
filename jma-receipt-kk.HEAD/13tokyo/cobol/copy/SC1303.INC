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
      *医療機関コード
              03  SC03-HOSPCDN         PIC  X(20).
      *診療年月
              03  SC03-PRTYM           PIC  X(16).
      *請求年月日
              03  SC03-SEIYMD          PIC  X(22).
      *点数区分表：１医科
              03  SC03-TENHYOKBN1      PIC  X(02).
      *点数区分表：３歯科
              03  SC03-TENHYOKBN2      PIC  X(02).
      *点数区分表：４調剤
              03  SC03-TENHYOKBN3      PIC  X(02).
      *点数区分表：６訪問
              03  SC03-TENHYOKBN4      PIC  X(02).
      *住所
              03  SC03-ADRS            PIC  X(100).
      *医療機関名
              03  SC03-HOSPNAME        PIC  X(100).
      *電話番号
              03  SC03-TEL             PIC  X(15).
      *管理者名
              03  SC03-KAISETUNAME     PIC  X(40).
      *請求テーブル
              03  SC03-SEIKYU-TBL   OCCURS  10.
      *負担者番号
                  05  SC03-FTNJANUM    PIC  X(08).
      *受給者番号
                  05  SC03-JKYSNUM     PIC  X(12).
      *保険者番号
                  05  SC03-CITYNUM     PIC  X(06).
                  05  SC03-HKNJANUM    PIC  X(16).
      *受給者氏名
                  05  SC03-KANANAME    PIC  X(20).
                  05  SC03-NAME        PIC  X(20).
      *負担割合
                  05  SC03-KYURATE1    PIC  X(02).
                  05  SC03-KYURATE2    PIC  X(02).
      *入外区分
                  05  SC03-NYUGAIKBN1  PIC  X(02).
                  05  SC03-NYUGAIKBN2  PIC  X(02).
      *診療日数
                  05  SC03-NISSU       PIC  X(02).
      *請求額
                  05  SC03-SEIKYU      PIC  X(07).
      *食事療養費負担分
                  05  SC03-SYKFTN1     PIC  X(06).
      *総点数
                  05  SC03-TENSU       PIC  X(07).
      *一部負担金
                  05  SC03-ITBFTN      PIC  X(07).
      *備考:上段
                  05  SC03-BIKO1-1     PIC  X(02).
                  05  SC03-BIKO1-2     PIC  X(02).
      *備考：下段
                  05  SC03-BIKO2-1     PIC  X(02).
                  05  SC03-BIKO2-2     PIC  X(02).
      *合計件数
              03  SC03-SKENSU	       PIC  X(02).
      *合計請求額
              03  SC03-SSEIKYU	       PIC  X(07).
      *食事記号
              03  SC03-SYOKU-TBL OCCURS 10.
                  05  SC03-SYOKU       PIC  X(02).

