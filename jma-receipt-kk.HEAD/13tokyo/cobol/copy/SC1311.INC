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
      *医療機関コード
              03  SC11-HOSPCD          PIC  X(14).
      *診療年月
              03  SC11-PRTYM           PIC  X(16).
      *請求年月日
              03  SC11-SEIYMD          PIC  X(22).
      *点数区分表：１医科 ４調剤
              03  SC11-TENHYOKBN       PIC  X(02) OCCURS 2.
      *住所
              03  SC11-ADRS            PIC  X(100).
      *医療機関名
              03  SC11-HOSPNAME        PIC  X(100).
      *電話番号
              03  SC11-TEL             PIC  X(15).
      *管理者名
              03  SC11-KAISETUNAME     PIC  X(40).
      *NO
              03  SC11-NO              PIC  X(03).
      *
      *負担者番号
              03  SC11-FTNJA-TBL.
                  05  SC11-FTNJANUM2   PIC  X(04) OCCURS 10.
                  05  SC11-FTNJANUM4   PIC  X(08) OCCURS 10.
      *受給社番号
              03  SC11-JKYSNUM-TBL.
                  05  SC11-JKYSNUM     PIC  X(14) OCCURS 10.
      *保険者番号
              03  SC11-HKNJANUM-TBL.
                  05  SC11-HKNJANUM    PIC  X(16) OCCURS 10.
      *患者氏名
              03  SC11-KANANAME-TBL.
                  05  SC11-KANANAME    PIC  X(20) OCCURS 10.
              03  SC11-NAME-TBL.
                  05  SC11-NAME        PIC  X(20) OCCURS 10.
      *負担割合
              03  SC11-FTNRATE-TBL.
                  05  SC11-FTNRATE-G OCCURS 10.
                      07  SC11-FTNRATE PIC X(02) OCCURS 3.
      *入院・外来
              03  SC11-NYUGAI-TBL.
                  05  SC11-NYUGAI-G OCCURS 10.
                      07  SC11-NYUGAIKBN  PIC X(02) OCCURS 2.
      *診療日数
              03  SC11-NISSU-TBL.
                  05  SC11-NISSU       PIC  X(02) OCCURS 10.
      *請求額
              03  SC11-SEIKYUGK-TBL.
                  05  SC11-SEIKYUGK    PIC  X(07) OCCURS 10.
      *食事標準負担額
              03  SC11-SFTNGK-TBL.
                  05  SC11-SFTNGK      PIC  X(07) OCCURS 10.
      *公費分点数
              03  SC11-TENSU-TBL.
                  05  SC11-TENSU       PIC  X(07) OCCURS 10.
      *備考欄
              03  SC11-BIKO-TBL OCCURS 10.
                  05  SC11-BIKO1       PIC  X(02) OCCURS 2.
                  05  SC11-BIKO2       PIC  X(02) OCCURS 2.
      *合計
              03  SC11-GOKEI-TBL.
      *合計件数
                  05  SC11-GKENSU      PIC  X(02).
      *合計請求額
                  05  SC11-GSEIKYUGK   PIC  X(09).
