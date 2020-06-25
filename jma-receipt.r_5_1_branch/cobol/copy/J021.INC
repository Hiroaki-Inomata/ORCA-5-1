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
        02  J021.
   ********** 02  J021-KNMINO.
              03  J021-MAE-HKNCOMBI.
                04  J021-VALUE  PIC X(4).
              03  J021-MAE-HKNCOMBIMEI.
                04  J021-VALUE  PIC X(60).
              03  J021-ATO-HKNCOMBI.
                04  J021-VALUE  PIC X(4).
              03  J021-ATO-HKNCOMBIMEI.
                04  J021-VALUE  PIC X(60).
              03  J021-MAE-FTNRATE.
                04  J021-VALUE  PIC X(8).
              03  J021-ATO-FTNRATE.
                04  J021-VALUE  PIC X(8).
              03  J021-SCROLLEDWINDOW1.
                04  J021-HKNLIST.
                  05  J021-HKNLIST-COUNT  PIC S9(9)   BINARY.
                  05  J021-HKNLIST-ROW   PIC S9(9)   BINARY.
                  05  J021-HKNLIST-ROWATTR   PIC S9(9)   BINARY.
                  05  J021-ITEM       OCCURS  40 TIMES.
                    06  J021-NUM PIC X(4).
                    06  J021-HKNKUMI PIC X(60).
                    06  J021-TEKSTYMD PIC X(10).
                    06  J021-TEKEDYMD PIC X(10).
                    06  J021-FTNRATE  PIC X(8).
                  05  J021-HKNLIST-SELECT PIC X       OCCURS  40 TIMES.
              03  J021-DAY1.
                04  J021-VALUE  PIC S9(2).
              03  J021-DAY2.
                04  J021-VALUE  PIC S9(2).
