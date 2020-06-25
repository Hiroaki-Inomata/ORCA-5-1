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
        02  I44.
              03  I44-SRYKBN    OCCURS 20.
                04  I44-VALUE   PIC X(2).
              03  I44-INPUTCD   OCCURS  20.
                04  I44-VALUE   PIC X(22).
              03  I44-MEISYO    OCCURS  20.
                04  I44-VALUE   PIC X(62).
              03  I44-SURYO     OCCURS  20.
                04  I44-VALUE   PIC X(40).
              03  I44-PTNUM.
                04  I44-VALUE   PIC X(20).
              03  I44-SRYYM.
                04  I44-VALUE   PIC X(8).
              03  I44-KANANAME.
                04  I44-VALUE   PIC X(50).
              03  I44-NAME.
                04  I44-VALUE   PIC X(50).
              03  I44-BIRTHDAY.
                04  I44-VALUE   PIC X(9).
              03  I44-SEX.
                04  I44-VALUE   PIC X(2).
              03  I44-SRYKA.
                04  I44-VALUE   PIC X(10).
              03  I44-DAY       OCCURS  31.
                04  I44-VALUE   PIC X(2).
