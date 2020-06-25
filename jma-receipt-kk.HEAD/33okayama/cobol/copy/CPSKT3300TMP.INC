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
      *    国保総括票     COPY (CPSKT3300TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************   
       01  SKT3300TMP-REC.
           03    SKT3300TMP-KEY.
             05    SKT3300TMP-DUMMY              PIC  X(01).
           03    SKT3300TMP-PREF-TBL     OCCURS 3.
             05    SKT3300TMP-NYUGAI-TBL   OCCURS 2.
               07    SKT3300TMP-HKN-TBL      OCCURS 3.
                  09   SKT3300TMP-KENSU           PIC  9(05).
                  09   SKT3300TMP-TENSU           PIC  9(09).
             05    SKT3300TMP-KINGK               PIC  9(09).
             05    SKT3300TMP-RKINGK              PIC  9(09).
      *
