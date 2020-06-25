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
       01  HC07.
              03  HC07-SRYKA    PIC  X(40).
              03  HC07-SRYYMD    PIC  X(22).
              03  HC07-PTNUM-G.
                  05  HC07-PTNUM-TBL    OCCURS  28.
                      07  HC07-PTNUM    PIC  X(40).
              03  HC07-NAME-G.
                  05  HC07-NAME-TBL     OCCURS  28.
                      07  HC07-NAME     PIC  X(26).
              03  HC07-SEX-G.
                  05  HC07-SEX-TBL      OCCURS  28.
                      07  HC07-SEX      PIC  X(2).
              03  HC07-BIRTHDAY-G.
                  05  HC07-BIRTHDAY-TBL OCCURS  28.
                      07  HC07-BIRTHDAY PIC  X(22).
              03  HC07-SEIKYU-G.
                  05  HC07-SEIKYU-TBL   OCCURS  28.
                      07  HC07-SEIKYU   PIC  X(9).
              03  HC07-RYOSYU-G.
                  05  HC07-RYOSYU-TBL   OCCURS  28.
                      07  HC07-RYOSYU   PIC  X(9).
              03  HC07-MISYU-G.
                  05  HC07-MISYU-TBL    OCCURS  28.
                      07  HC07-MISYU    PIC  X(9).
              03  HC07-SEIKYUTTLMSG1    PIC  X(20).
              03  HC07-RYOSYUTTLMSG1    PIC  X(20).
              03  HC07-MISYUTTLMSG1     PIC  X(20).
              03  HC07-TTL-G.
                  05  HC07-TTL-TBL      OCCURS  3.
                      07  HC07-TTLMSG    PIC  X(10).
                      07  HC07-SEIKYUTTL PIC  X(10).
                      07  HC07-RYOSYUTTL PIC  X(10).
                      07  HC07-MISYUTTL  PIC  X(10).
              03  HC07-PAGE             PIC X(3).
