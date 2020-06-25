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
       01  D1281.
              03  D1281-TITLE.
                05  D1281-SUBTITLE    PIC X(30).
                05  D1281-JOBMSG      PIC X(60).
                05  D1281-PAGE        PIC X(3).
                05  D1281-SYSYMD      PIC X(22).
                05  D1281-FRYMD       PIC X(22).
                05  D1281-TOYMD       PIC X(22).
                05  D1281-SRYKA       PIC X(20).
                05  D1281-TJIHI-G           OCCURS  10 TIMES.
                  07  D1281-TJIHI     PIC X(12).
              03  D1281-DATA.
                05  D1281-TDRNAME-G         OCCURS  8 TIMES.
                  07  D1281-TDRNAME   PIC X(18).
                05  D1281-YOSO-G            OCCURS  8 TIMES.
                  07  D1281-YOSO-OCC        OCCURS  11 TIMES.
                    09  D1281-YOSO    PIC X(11).
