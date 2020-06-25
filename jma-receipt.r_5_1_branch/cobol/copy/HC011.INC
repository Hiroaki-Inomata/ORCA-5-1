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
       01  HC011.
              03  HC011-PTNUM    PIC  X(20).
              03  HC011-NAME    PIC  X(24).
              03  HC011-TENKI-TBL     OCCURS  22.
                  05  HC011-TENKIG.
                      07 HC011-TENKI      OCCURS  3  TIMES
                                          PIC  X(02).

              03  HC011-STRYMD-TBL    OCCURS  22.
                  05  HC011-STRYMDG.
                      07 HC011-STRYMD     OCCURS  3  TIMES
                                          PIC  X(02).
              03  HC011-ENDYMD-TBL    OCCURS  22.
                  05  HC011-ENDYMDG.
                      07 HC011-ENDYMD     OCCURS  3  TIMES
                                          PIC  X(02).
              03  HC011-BYOMEI-TBL    OCCURS  22.
                  05  HC011-BYOMEIG.
                      07 HC011-BYOMEI     PIC  X(112).
