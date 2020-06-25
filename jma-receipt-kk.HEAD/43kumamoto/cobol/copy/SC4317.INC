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
      *診療年月
           03  SC17-PRTYM              PIC  X(16).
      *請求年月日
           03  SC17-SEIYMD             PIC  X(22).
      *医療機関所在地
           03  SC17-ADRS               PIC  X(100).
      *医療機関名称
           03  SC17-HOSPNAME           PIC  X(100).
      *電話番号
           03  SC17-TEL                PIC  X(15).
      *開設者名
           03  SC17-KAISETU            PIC  X(40).
      *郵便番号
           03  SC17-POST               PIC  X(08).
      *医療機関コード
           03  SC17-HOSPCD             PIC  X(14).
      *請求テーブル
           03  SC17-NYUGAI-TBL  OCCURS 2.
               05  SC17-SEIKYU-TBL  OCCURS 7.
      *件数
                   07  SC17-KENSU      PIC  X(05).
      *日数 
                   07  SC17-NISSU      PIC  X(05).
      *点数
                   07  SC17-TENSU      PIC  X(09).
      *療養費請求額
                   07  SC17-SEIKYUGK   PIC  X(09).
      *請求額合計
                   07  SC17-GSEIKYUGK  PIC  X(09).
      *公費テーブル
           03  SC17-KOH-NYUGAI-TBL  OCCURS 2.
               05  SC17-KOH-TBL  OCCURS 2.
      *法別番号
                   07  SC17-HBTNUM     PIC  X(02).
      *公費名
                   07  SC17-KOHNAME    PIC  X(08).
      *件数
                   07  SC17-KKENSU      PIC  X(05).
      *日数 
                   07  SC17-KNISSU      PIC  X(05).
      *点数
                   07  SC17-KTENSU      PIC  X(09).
      *療養費請求額
                   07  SC17-KSEIKYUGK   PIC  X(09).
      *請求額合計
                   07  SC17-KGSEIKYUGK  PIC  X(09).
      *小計
           03  SC17-SYOKEI-TBL.
      *件数
               05  SC17-S-KENSU          PIC  X(05).
      *日数 
               05  SC17-S-NISSU          PIC  X(05).
      *点数
               05  SC17-S-TENSU          PIC  X(09).
      *療養費請求額
               05  SC17-S-SEIKYUGK       PIC  X(09).
      *請求額合計
               05  SC17-S-GSEIKYUGK      PIC  X(09).
      *発行手数料件数
           03  SC17-TKENSU             PIC  X(05).
      *発行手数料
           03  SC17-TSEIKYUGK          PIC  X(09).
      *合計請求額
           03  SC17-G-GSEIKYUGK        PIC  X(09).
