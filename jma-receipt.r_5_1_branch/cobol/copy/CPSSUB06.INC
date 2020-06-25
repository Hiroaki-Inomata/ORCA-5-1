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
      ************************************************************
      * 請求情報更新サブ
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SSUB06-AREA.
      *    09:再発行/10:外来月別
           03  SSUB06-REQMODE              PIC X(02).
           03  SSUB06-NYUGAIKBN            PIC X(01).
           03  SSUB06-DENPNUM              PIC X(07).
           03  SSUB06-DENPEDANUM           PIC X(02).
           03  SSUB06-SRYYM                PIC X(07).
           03  SSUB06-STDAY                PIC X(02).
           03  SSUB06-EDDAY                PIC X(02).
           03  SSUB06-PRINT-AREA.
               05  SSUB06-SKYPRT           PIC X(01).
               05  SSUB06-SRYPRT           PIC X(01).
           03  SSUB06-PRINT-ORDER-AREA.
               05  SSUB06-SKYGRP           PIC X(01).
           03  SSUB06-OUT-AREA.
               05  SSUB06-OUT-DENPNUM      PIC 9(07).
               05  SSUB06-OUT-DENPEDANUM   PIC 9(02).
               05  SSUB06-OUT-SRYYM        PIC X(06).
               05  SSUB06-OUT-STYMD.
                   07  SSUB06-OUT-STYM     PIC X(06).
                   07  SSUB06-OUT-STDAY    PIC 9(02).
               05  SSUB06-OUT-EDYMD.
                   07  SSUB06-OUT-EDYM     PIC X(06).
                   07  SSUB06-OUT-EDDAY    PIC 9(02).
               05  SSUB06-OUT-KIKANKBN     PIC X(01).
               05  SSUB06-OUT-SKYPRT       PIC X(01).
               05  SSUB06-OUT-SKYGRP       PIC X(01).
               05  SSUB06-OUT-SRYPRT       PIC X(01).
