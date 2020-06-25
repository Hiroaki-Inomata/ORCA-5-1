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
        02  SID4.
            03  SID4-FIXED1.
              04  SID4-B06-STATE    PIC S9(9) BINARY.
              04  SID4-B06-VISIBLE  PIC X(1).
              04  SID4-B07-STATE    PIC S9(9) BINARY.
              04  SID4-B07-VISIBLE  PIC X(1).
              04  SID4-LBLID1SKYPRT.
                05  SID4-VALUE  PIC X(70).
              04  SID4-LBLSRYYMD.
                05  SID4-VALUE  PIC X(8).
              04  SID4-LBLKENSU.
                05  SID4-VALUE  PIC X(20).
              04  SID4-SRYYMD   PIC X(20).
              04  SID4-SRYKA    PIC X(50).
              04  SID4-HOKEN    PIC X(20).
              04  SID4-KINGAKU  PIC X(10) OCCURS 5.

