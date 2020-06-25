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
      *    請求確認情報編集サブ　COPY (CPORCSS016.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SS016-AREA.
           03  SS016-DENPNUM             PIC 9(07).
           03  SS016-DENPPRTYMD          PIC X(08).
           03  SS016-GRP-DENPNUM         PIC 9(07).
           03  SS016-NYUGAIKBN           PIC X(01).
           03  SS016-PTID                PIC 9(10).
           03  SS016-SRYKA               PIC X(02).
           03  SS016-REQMODE             PIC X(02).
           03  SS016-CHOSEI1             PIC S9(10).
           03  SS016-CHOSEI2             PIC S9(10).
           03  SS016-SKYMONEY            PIC S9(10).
           03  SS016-NYUKIN              PIC S9(10).
           03  SS016-NYKN-HOHO           PIC X(02).
           03  SS016-CHOSEI1-MOTO        PIC S9(10).
           03  SS016-CHOSEI2-MOTO        PIC S9(10).
           03  SS016-SKYMONEY-MOTO       PIC S9(10).
           03  SS016-NYUKIN-MOTO         PIC S9(10).
           03  SS016-NYUKINYMD           PIC X(08).
           03  SS016-NYUKINHMS           PIC X(06).
           03  SS016-PROC-FLG            PIC 9(01).
           03  SS016-DENPEDANUM          PIC 9(02).
           03  SS016-SYURRKUPDKBN        PIC X(1).
           03  SS016-SYURRKNUM           PIC 9(09).
           03  SS016-SYUEDANUM           PIC 9(09).
