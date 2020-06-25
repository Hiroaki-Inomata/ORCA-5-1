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
       01  SS002-AREA.
           03  SS002-IN.
      *        空白:請求・訂正/1:保険変更/2:調整金/3:最新番号取得
               05  SS002-UPDKBN            PIC X(01).
               05  SS002-IN-SYURRKNUM      PIC S9(09).
               05  SS002-IN-SYUEDANUM      PIC S9(09).
               05  SS002-IN-SYURRKUPDKBN   PIC X(01).
           03  SS002-OT.
               05  SS002-SYURRKNUM         PIC S9(09).
               05  SS002-SYUEDANUM         PIC S9(09).
               05  SS002-SYURRKUPDKBN      PIC X(01).
               05  SS002-RC                PIC S9(09)  BINARY.
