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
        02  W29.
      *      02  W29-FIXED6.
              03  W29-B03.
                04  W29-B03-STATE   PIC S9(9)   BINARY.
              03  W29-STYUKYMD.
                04  W29-VALUE   PIC X(10).
              03  W29-EDYUKYMD.
                04  W29-VALUE   PIC X(10).
              03  W29-TAIIN-KBNCD.
                04  W29-VALUE   PIC X(2).
              03  W29-TAIIN-NAME.
                04  W29-VALUE   PIC X(20).
              03  W29-TAIIN-TANNAME.
                04  W29-VALUE   PIC X(10).
