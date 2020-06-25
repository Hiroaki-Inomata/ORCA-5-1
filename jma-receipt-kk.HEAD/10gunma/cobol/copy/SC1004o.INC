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
       01  SC04o.
      *    医療機関点数表区分
           03  SC04o-TENSUHYO            PIC  X(02) OCCURS 4.
      *    医療機関コード
           03  SC04o-HOSPCDN             PIC  X(40).
      *    印刷日付
           03  SC04o-PRTYM               PIC  X(16).
      *    市町村名
           03  SC04o-CITYNAME            PIC  X(30).
      *    請求日付
           03  SC04o-SEIYMD              PIC  X(22).
      *    所在地
           03  SC04o-ADRS                PIC  X(100).
      *    医療機関名称
           03  SC04o-HOSPNAME            PIC  X(100).
      *    電話番号
           03  SC04o-TEL                 PIC  X(15).
      *    給付割合
           03  SC04o-KYURATE-TBL        OCCURS  8.
               05  SC04o-KYURATE         PIC  X(02).
      *    その他公費番号
           03  SC04o-ETCKOHNUM           PIC  X(04).
      *    受給者資格証記号
           03  SC04o-JKYSKIGO            PIC  X(02).
      *
           03  SC04o-MEISAI-TBL         OCCURS  12.
      *    受給者番号
               05  SC04o-JKYSNUM         PIC  X(14).
      *    保険種別
               05  SC04o-HKNKBN-TBL     OCCURS  5.
                   07  SC04o-HKNKBN      PIC  X(02).
      *    氏名
               05  SC04o-NAME            PIC  X(30).
      *    性別
               05  SC04o-SEX-TBL        OCCURS  2.
                   07  SC04o-SEX         PIC  X(02).
      *    生年月日
               05  SC04o-BIRTHDAY-TBL.
                   07  SC04o-NENGO-TBL  OCCURS  4.
                       09  SC04o-NENGO   PIC  X(02).
                   07  SC04o-BIRTHDAY    PIC  X(14).
      *    点数
               05  SC04o-TENSU-TBL      OCCURS 2.
                   07  SC04o-TENSU       PIC  X(07).
      *    食事標準負担額
               05  SC04o-SFTNGK          PIC  X(06).
      *    福祉医療費請求額
               05  SC04o-SEIKYUGK        PIC  X(07).
      *    食事日数
               05  SC04o-SNISSU          PIC  X(02).
      *    備考
               05  SC04o-BIKO            PIC  X(06).
      *    備考２
               05  SC04o-BIKO2           PIC  X(10).
      *    備考３
               05  SC04o-BIKO3           PIC  X(06).
      *    備考（税、多）
               05  SC04o-TEI-TBL        OCCURS 2.
                   07  SC04o-TEI         PIC  X(02).
                   07  SC04o-MARU        PIC  X(02).
      *    合計
           03  SC04o-GOKEI-TBL.
      *    合計−点数
               05  SC04o-GTENSU-TBL     OCCURS 2.
                   07  SC04o-GTENSU      PIC  X(09).
      *    合計−薬剤一部負担金
               05  SC04o-GSFTNGK         PIC  X(07).
      *    合計−福祉医療費請求額
               05  SC04o-GSEIKYUGK       PIC  X(09).
      *    合計−食事日数
               05  SC04o-GSNISSU         PIC  X(03).
