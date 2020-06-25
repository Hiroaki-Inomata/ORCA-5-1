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
      *****************************************************************
      *    オーダーサブ引数    COPY (CPORCSODR11S02.INC)              *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  SODR11S02-AREA.
           03  SODR11S02-SRYYMD            PIC X(08).
           03  SODR11S02-HKNJANUM          PIC X(08).
           03  SODR11S02-HONKZKKBN         PIC X(01).
           03  SODR11S02-KIGO              PIC X(40).
           03  SODR11S02-NUM               PIC X(40).
           03  SODR11S02-KANREN            PIC X(01).
           03  SODR11S02-ERRCD             PIC X(04).
           03  SODR11S02-HKNJANAME         PIC X(100).
           03  SODR11S02-HKNNUM            PIC X(03).
           03  SODR11S02-KANYUKBN          PIC X(01).
