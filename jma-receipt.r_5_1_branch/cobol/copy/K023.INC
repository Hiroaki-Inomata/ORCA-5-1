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
        02  K023.
      ****  02  K021-FIXED10.
              03  K023-SRYCD        OCCURS   20.
                04  K023-VALUE  PIC X(09).
              03  K023-SRYCDMEI     OCCURS   20.
                04  K023-VALUE  PIC X(60).
              03  K023-INPUTCD1     OCCURS   20.
                04  K023-VALUE  PIC X(20).
              03  K023-INPUTCD2     OCCURS   20.
                04  K023-VALUE  PIC X(20).
              03  K023-MIDASI.
                04  K023-MIDASI-STYLE  PIC X(20).
                04  K023-MIDASI-VALUE  PIC X(12).
              03  K023-INPUTCD3     OCCURS  20.
                04  K023-VALUE  PIC X(20).
