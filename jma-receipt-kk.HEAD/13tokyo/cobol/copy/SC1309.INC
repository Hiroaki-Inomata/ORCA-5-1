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
       01  SC09.
      *医療機関コード
              03  SC09-HOSPCDN         PIC  X(20).
      *診療年月
              03  SC09-PRTYM           PIC  X(16).
      *請求年月日
              03  SC09-SEIYMD          PIC  X(22).
      *点数区分表：１医科
              03  SC09-TENHYOKBN1      PIC  X(02).
      *住所
              03  SC09-ADRS            PIC  X(100).
      *医療機関名
              03  SC09-HOSPNAME        PIC  X(100).
      *電話番号
              03  SC09-TEL             PIC  X(15).
      *管理者名
              03  SC09-KAISETUNAME     PIC  X(40).
      *
      *負担者番号
              03  SC09-FTNJA-TBL.
                  05  SC09-FTNJANUM    PIC  X(08) OCCURS 10.
      *受給社番号
              03  SC09-JKYSNUM-TBL.
                  05  SC09-JKYSNUM     PIC  X(14) OCCURS 10.
      *保険者番号
              03  SC09-HKNJANUM-TBL.
                  05  SC09-HKNJANUM    PIC  X(16) OCCURS 10.
      *患者氏名
              03  SC09-KANANAME-TBL.
                  05  SC09-KANANAME    PIC  X(20) OCCURS 10.
              03  SC09-NAME-TBL.
                  05  SC09-NAME        PIC  X(20) OCCURS 10.
      *負担割合
              03  SC09-FTNRATE-TBL.
                  05  SC09-FTNRATE     PIC  X(02) OCCURS 10.
      *入院・外来
              03  SC09-NYUGAI-TBL.
                  05  SC09-NYUGAI      PIC  X(02) OCCURS 10.
      *診療日数
              03  SC09-NISSU-TBL.
                  05  SC09-NISSU       PIC  X(02) OCCURS 10.
      *請求額
              03  SC09-SEIKYUGK-TBL.
                  05  SC09-SEIKYUGK    PIC  X(07) OCCURS 10.
      *公費分点数
              03  SC09-TENSU-TBL.
                  05  SC09-TENSU       PIC  X(07) OCCURS 10.
      *備考欄
              03  SC09-BIKO-TBL OCCURS 10.
                  05  SC09-BIKO1       PIC  X(02) OCCURS 2.
                  05  SC09-BIKO2       PIC  X(02) OCCURS 2.
      *合計
              03  SC09-GOKEI-TBL.
      *合計件数
                  05  SC09-GKENSU      PIC  X(02).
      *合計請求額
                  05  SC09-GSEIKYUGK   PIC  X(07).
