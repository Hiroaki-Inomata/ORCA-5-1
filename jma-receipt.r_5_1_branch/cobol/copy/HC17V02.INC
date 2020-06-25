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
       01  HC17V02.
              03  HC17V02-SYSYMD    PIC  X(22).
              03  HC17V02-SRYYMD    PIC  X(22).
              03  HC17V02-PAGE    PIC  X(3).
              03  HC17V02-MIDASI    PIC  X(10).
              03  HC17V02-GMONEY-G    OCCURS  10.
                  05  HC17V02-GMONEY  PIC  X(8).
              03  HC17V02-GHKNTEN-G   OCCURS  10.
                  05  HC17V02-GHKNTEN PIC  X(8).
              03  HC17V02-GKENSU-G    OCCURS  10.
                  05  HC17V02-GKENSU  PIC  X(8).
              03  HC17V02-RENNUM-G    OCCURS  30.
                  05  HC17V02-RENNUM  PIC  X(3).
              03  HC17V02-PTNUM-G     OCCURS  30.
                  05  HC17V02-PTNUM   PIC  X(20).
              03  HC17V02-NAME-G      OCCURS  30.
                  05  HC17V02-NAME    PIC  X(26).
              03  HC17V02-HKNTEN-G    OCCURS  30.
                  05  HC17V02-HKNTEN  PIC  X(8).
              03  HC17V02-MONEY-G     OCCURS  9.
                  05  HC17V02-MONEY-OCC    OCCURS  30.
                      07  HC17V02-MONEY    PIC  X(8).
