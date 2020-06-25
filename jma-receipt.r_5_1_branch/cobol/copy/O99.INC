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
        02  O99.
      *     02  O99-FIXED6.
              03  O99-MSG.
                04  O99-VALUE   PIC X(100).
              03  O99-SCROLLEDWINDOW28.
                04  O99-JOBKANRILST.
                  05  O99-COUNT   PIC S9(9)   BINARY.
                  05  O99-ROW     PIC S9(9)   BINARY.
                  05  O99-ROWATTR PIC S9(9)   BINARY.
                  05  O99-ITEM        OCCURS  5 TIMES.
                    06  O99-RENNUM    PIC X(7).
                    06  O99-SHORINM   PIC X(40).
                    06  O99-STARTTIME PIC X(8).
                    06  O99-ENDTIME   PIC X(8).
                    06  O99-ERRMSG    PIC X(100).
                  05  O99-SELECT  PIC X       OCCURS  5 TIMES.
              03  O99-B12-STATE   PIC S9(9)   BINARY.
              03  O99-B12-LABEL   PIC X(14).
              03  O99-PROGRESSBAR-VALUE       PIC S9(9)   BINARY.
