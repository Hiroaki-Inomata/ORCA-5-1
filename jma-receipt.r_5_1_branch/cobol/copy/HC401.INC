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
       01  HC401.
               03  HC401-SINRYOU-G    PIC  X(4).
               03  HC401-SINRYOU-Y    PIC  X(2).
               03  HC401-SINRYOU-M    PIC  X(2).
               03  HC401-NYUGAIKBN    PIC  X(8).
               03  HC401-PAGE         PIC  X(3).
               03  HC401-RENBAN       PIC  X(5)  OCCURS  20  TIMES.
               03  HC401-PTID         PIC  X(20) OCCURS  20  TIMES.
               03  HC401-NAME         PIC  X(30) OCCURS  20  TIMES.
               03  HC401-NENREI       PIC  X(3)  OCCURS  20  TIMES.
               03  HC401-MESG         PIC  X(90) OCCURS  20  TIMES.
