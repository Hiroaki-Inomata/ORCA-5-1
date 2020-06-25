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
        02  W05.
            03  W05-FIXED1.
              04  W05-YUKOSTYMD.
                05  W05-VALUE   PIC X(9).
              04  W05-YUKOEDYMD.
                05  W05-VALUE   PIC X(9).
              04  W05-B03.
                05  W05-B03-STATE   PIC S9(9)   BINARY.
              04  W05-SRYKANAME1.
                05  W05-VALUE   PIC X(10).
              04  W05-SRYKANAME.
                05  W05-VALUE   PIC X(40).
              04  W05-SRYCD.
                05  W05-VALUE   PIC X(2).
              04  W05-SRYKANAME2.
                05  W05-VALUE   PIC X(6).
              04  W05-SRYKANAME3.
                05  W05-VALUE   PIC X(2).
              04  W05-CMB-RECESRYKA.
                05  W05-RECESRYKA          PIC X(43).
                05  W05-RECESRYKA-COUNT    PIC S9(9) BINARY.
                05  W05-RECESRYKA-ITEM     PIC X(43) OCCURS 50.
              04  W05-JIKANKSN1-40.
                05  W05-VALUE   PIC X.
              04  W05-JIKANKSN1-50.
                05  W05-VALUE   PIC X.
