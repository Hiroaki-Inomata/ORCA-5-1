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
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        02  X06.
      *    02  X06-FIXED6.
              03  X06-B06-STATE PIC S9(9)      BINARY.
              03  X06-B07-STATE PIC S9(9)      BINARY.
              03  X06-SRYCD     PIC X(20).
              03  X06-NAME      PIC X(100).
              03  X06-LBLTITLE-STYLE PIC X(20).
              03  X06-NO        PIC S9(3)      OCCURS     20.
              03  X06-TEKCD     PIC X(20)      OCCURS     20.
              03  X06-TEKNAME   PIC X(100)     OCCURS     20.
