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
      *医療機関コード
              03  SC05-HOSPCDN         PIC  X(20).
      *診療年月
              03  SC05-PRTYM           PIC  X(16).
      *請求年月日
              03  SC05-SEIYMD          PIC  X(22).
      *点数区分表：１医科
              03  SC05-TENHYOKBN1      PIC  X(02).
      *点数区分表：３歯科
              03  SC05-TENHYOKBN2      PIC  X(02).
      *点数区分表：４調剤
              03  SC05-TENHYOKBN3      PIC  X(02).
      *点数区分表：６訪問
              03  SC05-TENHYOKBN4      PIC  X(02).
      *住所
              03  SC05-ADRS            PIC  X(100).
      *医療機関名
              03  SC05-HOSPNAME        PIC  X(100).
      *電話番号
              03  SC05-TEL             PIC  X(15).
      *管理者名
              03  SC05-KAISETUNAME     PIC  X(40).
      *請求テーブル
              03  SC05-SEIKYU-TBL   OCCURS  10.
      *負担者番号
                  05  SC05-FTNJANUM    PIC  X(08).
      *受給者番号
                  05  SC05-JKYSNUM     PIC  X(12).
      *保険者番号
                  05  SC05-HKNJANUM    PIC  X(16).
      *受給者氏名
                  05  SC05-KANANAME    PIC  X(20).
                  05  SC05-NAME        PIC  X(20).
      *本人家族区分
                  05  SC05-KYURATE1    PIC  X(02).
                  05  SC05-KYURATE2    PIC  X(02).
      *入外区分
                  05  SC05-NYUGAIKBN1  PIC  X(02).
                  05  SC05-NYUGAIKBN2  PIC  X(02).
      *診療日数
                  05  SC05-NISSU       PIC  X(02).
      *請求額
                  05  SC05-SEIKYU      PIC  X(07).
      *食事療養費薬剤負担分
                  05  SC05-SYKYKZFTN   PIC  X(06).
      *総点数
                  05  SC05-TENSU       PIC  X(07).
      *一部負担金
                  05  SC05-ITBFTN      PIC  X(07).
      *合計件数
              03  SC05-SKENSU		PIC  X(02).
      *合計請求額
              03  SC05-SSEIKYU		PIC  X(07).
      *食事療養・薬剤判別記号
              03  SC05-SYKYKZ-TBL  OCCURS 10.
                  05  SC05-SYKYKZ       PIC  X(02).
      *備考欄
              03  SC05-BIKO-TBL OCCURS 10.
      *上段文字
                  05  SC05-BIKO1        PIC  X(04).
      *下段文字
                  05  SC05-BIKO2        PIC  X(04).
      *上段◯
                  05  SC05-BIKO3        PIC  X(02).
      *下段◯
                  05  SC05-BIKO4        PIC  X(02).
