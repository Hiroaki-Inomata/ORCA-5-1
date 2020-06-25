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
            01  HC06.
              03  HC06-TEISYUTUSAKI    PIC  X(6).
              03  HC06-PTNUM           PIC  X(20).
              03  HC06-SRYY            PIC  X(4).
              03  HC06-SRYM            PIC  X(4).
              03  HC06-PREFNUM         PIC  X(4).
              03  HC06-HOSPCDN         PIC  X(20).
              03  HC06-KHNTEN2         PIC  X(7)   OCCURS  2.
              03  HC06-KHNTEN1         PIC  X(7).
              03  HC06-SRYKBN          PIC  X(2)   OCCURS  35.
              03  HC06-TEKIYO          PIC  X(52)  OCCURS  35.
              03  HC06-KOUTEN          PIC  X(7)   OCCURS  35.
              03  HC06-TOTALTEN        PIC  X(11)  OCCURS  3.
              03  HC06-YKZFTN          PIC  X(11)  OCCURS  3.
              03  HC06-FTNMONEY        PIC  X(11)  OCCURS  3.
              03  HC06-OCR1            PIC  X(67).
              03  HC06-OCR2            PIC  X(67).
              03  HC06-OCR3            PIC  X(67).
              03  HC06-RJNFTNJANUM     PIC  X(16).
              03  HC06-KOHFTNJANUM1    PIC  X(16).
              03  HC06-KOHFTNJANUM2    PIC  X(16).
              03  HC06-HKNJANUM        PIC  X(16).
              03  HC06-MSG             PIC  X(8).
              03  HC06-KYURATE         PIC  X(8).
              03  HC06-RJNJKYSNUM      PIC  X(14).
              03  HC06-KOHJKYSNUM1     PIC  X(14).
              03  HC06-KOHJKYSNUM2     PIC  X(14).
              03  HC06-HKNSBT1         PIC  X(6).
              03  HC06-HKNSBT2         PIC  X(6).
              03  HC06-HONKZK          PIC  X(6).
              03  HC06-KIGO            PIC  X(34).
              03  HC06-NUM             PIC  X(34).
              03  HC06-KANANAME        PIC  X(30).
              03  HC06-SEXNUM          PIC  X(2).
              03  HC06-BIRTHDAY        PIC  X(22).
              03  HC06-JIYU            PIC  X(20).
              03  HC06-SEXN            PIC  X(2).
              03  HC06-TOKKI           PIC  X(12)  OCCURS  4.
              03  HC06-HOSPTEL         PIC  X(15).
              03  HC06-SRYKA           PIC  X(6).
              03  HC06-BEDSU           PIC  X(6).
              03  HC06-BYOMEI          PIC  X(54)  OCCURS  5.
              03  HC06-SRYYMD          PIC  X(24)  OCCURS  5.
              03  HC06-TENKI           PIC  X(10)  OCCURS  5.
              03  HC06-JINISSU         PIC  X(2).
              03  HC06-JINISSU1        PIC  X(2).
              03  HC06-JINISSU2        PIC  X(2).
              03  HC06-TENSU           PIC  X(7)   OCCURS  35.
              03  HC06-KAISU           PIC  X(3)   OCCURS  35.
              03  HC06-KHNTEN          PIC  X(7)   OCCURS  5.
              03  HC06-HOSPNAME        PIC  X(80).
              03  HC06-HOSPADRS        PIC  X(50).
              03  HC06-NAME            PIC  X(50).
              03  HC06-HEAD1-G.
                  05  HC06-HEAD1       PIC  X(46)  OCCURS  2.
              03  HC06-HEAD2-G.
                  05  HC06-HEAD2       PIC  X(02)  OCCURS  7.
              03  HC06-HEAD3-G.
                  05  HC06-HEAD3       PIC  X(50)  OCCURS  2.
              03  HC06-BODY            PIC  X(52).
              03  HC06-FOOT-G.    
                  05  HC06-FOOT        PIC  X(46)  OCCURS  4.
              03  HC06-SHOSHINTIME     PIC  X(10).
              03  HC06-TENKIYMD        PIC  X(06)  OCCURS  5.
