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
      ************************************************************
      *    レセ電用国保総括表     COPY (CPSKT1320TMP.INC)             
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT1320TMP-REC.
           03  SKT1320TMP-KEY.
               05  SKT1320TMP-SYUTURYOKU           PIC 9(01).
               05  SKT1320TMP-NYUGAIKBN            PIC X(01).
               05  SKT1320TMP-ZAITAKU              PIC X(01).
           03  SKT1320TMP-KEN-TBL OCCURS 2.
               05  SKT1320TMP-KKENSU               PIC 9(05).
               05  SKT1320TMP-HKN-TBL OCCURS 4.
                   07  SKT1320TMP-KENSU            PIC 9(05).
                   07  SKT1320TMP-TENSU            PIC 9(11).
                   07  SKT1320TMP-SKENSU           PIC 9(05).
                   07  SKT1320TMP-SKINGK           PIC 9(11).
           03  SKT1320TMP-KOUKI-TBL OCCURS 2.
               05  SKT1320TMP-KO-KENSU             PIC 9(05).
               05  SKT1320TMP-KO-TENSU             PIC 9(11).
               05  SKT1320TMP-KO-KKENSU            PIC 9(05).
               05  SKT1320TMP-KO-SKENSU            PIC 9(05).
               05  SKT1320TMP-KO-SKINGK            PIC 9(11).

