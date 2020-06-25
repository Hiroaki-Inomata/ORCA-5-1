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
      * 01.00.01     @@@@-門間    10/12/13  コメント履歴削除
      * 01.00.02     @@@@-井上    19/01/24  明細部の桁数修正
      ********************************************************************
       01  D122.
              03  D122-TITLE.
                05  D122-SUBTITLE   PIC X(30).
                05  D122-JOBMSG     PIC X(60).
                05  D122-PAGE       PIC X(3).
                05  D122-SYSYMD     PIC X(22).
                05  D122-SRYYM      PIC X(16).
                05  D122-SRYKA      PIC X(20).
                05  D122-DRNAME     PIC X(30).
                05  D122-TENKI-G            OCCURS  3 TIMES.
                  07  D122-TENKI    PIC X(22).
              03  D122-DATA.
                05  D122-TDAY-G             OCCURS  17 TIMES.
                  07  D122-TDAY     PIC X(6).
                05  D122-YOSO-G             OCCURS  17 TIMES.
                  07  D122-YOSO-OCC         OCCURS  38 TIMES.
                    09  D122-YOSO   PIC X(11).
