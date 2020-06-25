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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/15  コメント履歴削除
      ********************************************************************
       01  D1241.
              02  D1241-TITLE.
                03  D1241-SUBTITLE  PIC X(30).
                03  D1241-JOBMSG    PIC X(60).
                03  D1241-PAGE      PIC X(3).
                03  D1241-SYSYMD    PIC X(22).
                03  D1241-SRYYM     PIC X(10).
                03  D1241-SRYKA     PIC X(20).
                03  D1241-DRNAME    PIC X(30).
                03  D1241-TJIHI-G           OCCURS  10 TIMES.
                  04  D1241-TJIHI   PIC X(12).
              02  D1241-DATA.
                03  D1241-TDAY-G            OCCURS  13 TIMES.
                  04  D1241-TDAY    PIC X(6).
                03  D1241-YOSO-G            OCCURS  13 TIMES.
                  04  D1241-YOSO-OCC        OCCURS  11 TIMES.
                    05  D1241-YOSO  PIC X(11).
