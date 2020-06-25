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
      *    診療報酬請求書     COPY (CPSKT0600TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT0600TMP-REC.
           03      SKT0600TMP-KEY.
             05    SKT0600TMP-KBN                   PIC  9(01).
             05    SKT0600TMP-HKNJANUM              PIC  X(06).
             05    SKT0600TMP-HENREIYM              PIC  X(06).
           03      SKT0600TMP-DATA.
             05    SKT0600TMP-NYUIN.
               07  SKT0600TMP-N-KENSU               PIC  9(06).
               07  SKT0600TMP-N-NISSU               PIC  9(06).
               07  SKT0600TMP-N-TENSU               PIC  9(10).
             05    SKT0600TMP-GAIRAI.
               07  SKT0600TMP-G-KENSU               PIC  9(06).
               07  SKT0600TMP-G-NISSU               PIC  9(06).
               07  SKT0600TMP-G-TENSU               PIC  9(10).
