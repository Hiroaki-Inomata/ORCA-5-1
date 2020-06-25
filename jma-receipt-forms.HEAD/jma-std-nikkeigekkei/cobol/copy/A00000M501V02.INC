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
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    @@@@-門間    15/01/27  新規作成
      ********************************************************************
       01  HC17BV02.
              03  HC17BV02-SYSYMD       PIC X(22).
              03  HC17BV02-SRYYM        PIC X(16).
              03  HC17BV02-PAGE         PIC X(3).
              03  HC17BV02-MIDASI       PIC X(10).
              03  HC17BV02-JOBMSG       PIC X(60).
              03  HC17BV02-NYUGAIKBN    PIC X(4).
              03  HC17BV02-SRYKA        PIC X(20).
              03  HC17BV02-TJIHI-G                  OCCURS  10 TIMES.
                  05  HC17BV02-TJIHI    PIC X(8).
              03  HC17BV02-KENSU1       PIC X(8)    OCCURS  2 TIMES.
              03  HC17BV02-KENSU2       PIC X(8)    OCCURS  2 TIMES.
              03  HC17BV02-KENSU3       PIC X(8)    OCCURS  2 TIMES.
              03  HC17BV02-KENSU4       PIC X(10)   OCCURS  2 TIMES.
              03  HC17BV02-TJYUSHIN     PIC X(8).
              03  HC17BV02-GHKNTEN      PIC X(10).
              03  HC17BV02-OHKNTEN      PIC X(10).
              03  HC17BV02-RHKNKEN      PIC X(10).
              03  HC17BV02-SHKNKEN      PIC X(10).
              03  HC17BV02-SAHKNKEN     PIC X(10).
              03  HC17BV02-GMONEY-G                 OCCURS  20 TIMES.
                  05  HC17BV02-GMONEY   PIC X(10).
              03  HC17BV02-OKENSU-G                 OCCURS  20 TIMES.
                  05  HC17BV02-OKENSU   PIC X(10).
              03  HC17BV02-RKENSU-G                 OCCURS  20 TIMES.
                  05  HC17BV02-RKENSU   PIC X(10).
              03  HC17BV02-SKENSU-G                 OCCURS  20 TIMES.
                  05  HC17BV02-SKENSU   PIC X(10).
              03  HC17BV02-SAKENSU-G                OCCURS  20 TIMES.
                  05  HC17BV02-SAKENSU  PIC X(10).
              03  HC17BV02-GSEIKYU      PIC X(10).
              03  HC17BV02-GRYOSYU      PIC X(10).
              03  HC17BV02-GSYUEKI      PIC X(10).
              03  HC17BV02-OSEIKYUKEN   PIC X(10).
              03  HC17BV02-ORYOSYUKEN   PIC X(10).
              03  HC17BV02-OSYUEKIKEN   PIC X(10).
              03  HC17BV02-RSEIKYUKEN   PIC X(10).
              03  HC17BV02-RRYOSYUKEN   PIC X(10).
              03  HC17BV02-RSYUEKIKEN   PIC X(10).
              03  HC17BV02-SSEIKYUKEN   PIC X(10).
              03  HC17BV02-SRYOSYUKEN   PIC X(10).
              03  HC17BV02-SSYUEKIKEN   PIC X(10).
              03  HC17BV02-SASEIKYUKEN  PIC X(10).
              03  HC17BV02-SARYOSYUKEN  PIC X(10).
              03  HC17BV02-SASYUEKIKEN  PIC X(10).
              03  HC17BV02-JYUSHINCNT   PIC X(10).
              03  HC17BV02-JIHI-GMONEY  PIC X(10).
              03  HC17BV02-JIHI-OKENSU  PIC X(10).
              03  HC17BV02-JIHI-RKENSU  PIC X(10).
              03  HC17BV02-JIHI-SKENSU  PIC X(10).
              03  HC17BV02-JIHI-SAKENSU  PIC X(10).
