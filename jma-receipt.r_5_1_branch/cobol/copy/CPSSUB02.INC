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
      * 収納業務収納明細編集サブ
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SSUB02-AREA.
           03  SSUB02-DENPNUM                 PIC  9(07).
           03  SSUB02-OT.
               05  SSUB02-MAX                 PIC  9(03).
               05  SSUB02-OCC                 OCCURS 99.
                   07  SSUB02-JOUTAI          PIC  X(20).
                   07  SSUB02-MINUS           PIC  X(1).
                   07  SSUB02-CHOSEI1         PIC  S9(10).
                   07  SSUB02-CHOSEI2         PIC  S9(10).
                   07  SSUB02-PROC-FLG        PIC  9(01).
                   07  SSUB02-VISIBLE         PIC  9(01).
