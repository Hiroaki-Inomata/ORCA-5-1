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
       01  SKT3330.
      *    診療年月
           03       SKT3330-SRYYM                 PIC  X(16).
      *    医療機関住所
           03       SKT3330-ADRS                  PIC  X(100).
      *    医療機関名
           03       SKT3330-HOSPNAME              PIC  X(100).
      *    電話番号
           03       SKT3330-TEL                   PIC  X(15).
      *    開設者名
           03       SKT3330-KAISETUNAME           PIC  X(40).
      *    保険者名
           03       SKT3330-HKNJANAME             PIC  X(40).
      *    請求年月日
           03       SKT3330-SEIYMD                PIC  X(22).
      *    保険者番号
           03       SKT3330-HKNJANUM              PIC  X(12).
      *    医療機関コード
           03       SKT3330-HOSPCD                PIC  X(14).
      *    ページ番号
           03       SKT3330-NO                    PIC  X(04).
      *
           03  SKT3330-HKN-AREA.
               05  SKT3330-HKN-TBL  OCCURS  9. 
                   07  SKT3330-NYUGAI-TBL  OCCURS  2.
                       09  SKT3330-KENSU          PIC  X(05).
                       09  SKT3330-NISSU          PIC  X(05).
                       09  SKT3330-TENSU          PIC  X(09).
                       09  SKT3330-YKZFTN         PIC  X(07).
                       09  SKT3330-ITBFTN         PIC  X(09).
                   07  SKT3330-SKENSU             PIC  X(05).
                   07  SKT3330-SNISSU             PIC  X(05).
                   07  SKT3330-SKINGK             PIC  X(09).
                   07  SKT3330-SFTNGK             PIC  X(09).
               05  SKT3330-ZAITAKU-TBL OCCURS 6.
                   07  SKT3330-ZKENSU             PIC  X(03).
                   07  SKT3330-ZNISSU             PIC  X(03).
                   07  SKT3330-ZTENSU             PIC  X(07).
                   07  SKT3330-ZITBFTN            PIC  X(07).
               05  SKT3330-KOH-TBL  OCCURS 4.
                   07  SKT3330-HBTNUM                  PIC  X(03).
                   07  SKT3330-TANSEIDONAME            PIC  X(12).
                   07  SKT3330-NYUGAI-TBL  OCCURS  2.
                       09  SKT3330-K-KENSU             PIC  X(05).
                       09  SKT3330-K-NISSU             PIC  X(05).
                       09  SKT3330-K-TENSU             PIC  X(09).
                       09  SKT3330-K-YKZFTN            PIC  X(07).
                       09  SKT3330-K-ITBFTN            PIC  X(09).
                   07  SKT3330-K-SKENSU                PIC  X(05).
                   07  SKT3330-K-SNISSU                PIC  X(05).
                   07  SKT3330-K-SKINGK                PIC  X(09).
                   07  SKT3330-K-SFTNGK                PIC  X(09).
               05  SKT3330-BIKO.
                   07  SKT3330-B-TBL OCCURS 4.
                       09  SKT3330-BKOHNUM           PIC  X(03).
                       09  SKT3330-BNYUGAI           PIC  X(02).
                       09  SKT3330-BKENSU            PIC  X(03).
                       09  SKT3330-BKEN              PIC  X(02).
                       09  SKT3330-BYKZFTN           PIC  X(07).
                       09  SKT3330-BFILLER1          PIC  X(02).
                       09  SKT3330-BITBFTN           PIC  X(07).
                       09  SKT3330-BFILLER2          PIC  X(02).
               05  SKT3330-BIKO2.
                   07  SKT3330-B-TBL2 OCCURS 2.
                       09  SKT3330-BSKOHNUM          PIC  X(03).
                       09  SKT3330-BSKENSU           PIC  X(03).
                       09  SKT3330-BSKEN             PIC  X(02).
                       09  SKT3330-BSKINGK           PIC  X(07).
                       09  SKT3330-BSFILLER1         PIC  X(02).
                       09  SKT3330-BSFTNGK           PIC  X(07).
                       09  SKT3330-BSFILLER2         PIC  X(02).
