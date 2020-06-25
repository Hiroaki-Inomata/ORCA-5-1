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
       01  HC14.
              03  HC14-SRYYMD    PIC  X(22).
              03  HC14-TITLE    PIC  X(16).
              03  HC14-PAGE    PIC  X(3).
              03  HC14-NUM     PIC  X(3)   OCCURS  34.
              03  HC14-YYKTIME    PIC  X(5)  OCCURS  34.
              03  HC14-SRYKA      PIC  X(20) OCCURS  34.
              03  HC14-DRNAME     PIC  X(16) OCCURS  34.
              03  HC14-NAME       PIC  X(20) OCCURS  34.
              03  HC14-PTNUM      PIC  X(20) OCCURS  34.
              03  HC14-RENRAKU    PIC  X(20) OCCURS  34.
              03  HC14-YOYAKUNAIY      PIC  X(16) OCCURS  34.
              03  HC14-RAIIN     PIC  X(2) OCCURS  34.
              03  HC14-SRYNAIYO     PIC  X(10)  OCCURS  34.
