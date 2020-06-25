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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付        内容
      *  01.00.01    吉川     11/03/15   東北地方太平洋沖地震対応（対象者別集計）
      *  01.00.02    萩野     18/12/12   修正履歴コメント削除
      ************************************************************
       01  SKT2030TMP-REC.
           03  SKT2030TMP-KEY.
               05  SKT2030TMP-SYUTURYOKU       PIC  9(01).
               05  SKT2030TMP-HKNJANUM         PIC  X(06).
               05  SKT2030TMP-SRYYM            PIC  9(06).
               05  SKT2030TMP-ZAITAKU          PIC  X(01).
               05  SKT2030TMP-GENMEN           PIC  X(05).
           03  SKT2030TMP-IPAN-TBL    OCCURS  7.
               05  SKT2030TMP-KENSU            PIC  9(06).
               05  SKT2030TMP-NISSU            PIC  9(06).
               05  SKT2030TMP-TENSU            PIC  9(10).
               05  SKT2030TMP-YKZFTN           PIC  9(10).
               05  SKT2030TMP-ITBFTN           PIC  9(10).
           03  SKT2030TMP-KOH-AREA.
             05  SKT2030TMP-KOH-TBL    OCCURS  20.
                 07  SKT2030TMP-KOHNUM         PIC  X(03).
                 07  SKT2030TMP-KOHNAME        PIC  X(12).
                 07  SKT2030TMP-K-KENSU        PIC  9(06).
                 07  SKT2030TMP-K-NISSU        PIC  9(06).
                 07  SKT2030TMP-K-TENSU        PIC  9(10).
                 07  SKT2030TMP-K-YKZFTN       PIC  9(10).
                 07  SKT2030TMP-K-ITBFTN       PIC  9(10).
