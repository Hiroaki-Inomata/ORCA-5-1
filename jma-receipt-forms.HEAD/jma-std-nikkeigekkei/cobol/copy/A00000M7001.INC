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
      *  プログラム修正履歴
      *              @@@@-高嶋    06/11/06  基本診療項目追加対応
      ********************************************************************
       01  M7001.
               03  M7001-JOBMSG       PIC X(60).
               03  M7001-PAGE         PIC X(3).
               03  M7001-SYSYMD       PIC X(22).
               03  M7001-PRTYM        PIC X(16).
               03  M7001-NYUGAIKBN    PIC X(4).
               03  M7001-SRYKA        PIC X(20).
               03  M7001-HKNFTN-G.
                   05  M7001-HKNFTN   PIC X(10)   OCCURS  32 TIMES.
               03  M7001-JIHIFTN-G.
                   05  M7001-JIHIFTN  PIC X(10)   OCCURS  32 TIMES.
               03  M7001-GSEIKYU-G.
                   05  M7001-GSEIKYU  PIC X(10)   OCCURS  32 TIMES.
               03  M7001-GRYOSYU-G.
                   05  M7001-GRYOSYU  PIC X(10)   OCCURS  32 TIMES.
