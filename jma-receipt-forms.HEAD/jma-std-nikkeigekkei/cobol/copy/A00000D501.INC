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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      *                                     open-cobol1.0対応
      ********************************************************************
       01  HC17B.
            03  HC17B-SYSYMD      PIC X(22).
            03  HC17B-SRYYMD      PIC X(22).
            03  HC17B-PAGE        PIC  X(3).
            03  HC17B-MIDASI      PIC X(10).
            03  HC17B-JOBMSG      PIC X(60).
            03  HC17B-NYUGAIKBN   PIC X(4).
            03  HC17B-SRYKA       PIC X(20).
            03  HC17B-TJIHI-G     OCCURS  10 TIMES.
                05  HC17B-TJIHI   PIC X(8).
            03  HC17B-KENSU1
                    PIC X(8)      OCCURS  2 TIMES.
            03  HC17B-KENSU2
                    PIC X(8)      OCCURS  2 TIMES.
            03  HC17B-KENSU3
                    PIC X(8)      OCCURS  2 TIMES.
            03  HC17B-KENSU4
                    PIC X(10)     OCCURS  2 TIMES.
            03  HC17B-TJYUSHIN    PIC X(8).
            03  HC17B-GHKNKEN     PIC X(9).
            03  HC17B-OHKNKEN     PIC X(9).
            03  HC17B-RHKNKEN     PIC X(9).
            03  HC17B-SHKNKEN     PIC X(9).
            03  HC17B-SAHKNKEN    PIC X(9).
            03  HC17B-GMONEY-G    OCCURS  20 TIMES.
              05  HC17B-GMONEY    PIC X(9).
            03  HC17B-OKENSU-G    OCCURS  20 TIMES.
              05  HC17B-OKENSU    PIC X(9).
            03  HC17B-RKENSU-G    OCCURS  20 TIMES.
              05  HC17B-RKENSU    PIC X(9).
            03  HC17B-SKENSU-G    OCCURS  20 TIMES.
              05  HC17B-SKENSU    PIC X(9).
            03  HC17B-SAKENSU-G   OCCURS  20 TIMES.
              05  HC17B-SAKENSU   PIC X(9).
            03  HC17B-GSEIKYU     PIC X(9).
            03  HC17B-GRYOSYU     PIC X(9).
            03  HC17B-GSYUEKI     PIC X(9).
            03  HC17B-OSEIKYUKEN  PIC X(9).
            03  HC17B-ORYOSYUKEN  PIC X(9).
            03  HC17B-OSYUEKIKEN  PIC X(9).
            03  HC17B-RSEIKYUKEN  PIC X(9).
            03  HC17B-RRYOSYUKEN  PIC X(9).
            03  HC17B-RSYUEKIKEN  PIC X(9).
      *--- 2010/12/07 CHANGE START -------------------------------------
      *      02  HC17B-SSEIKYUKEN  PIC X(9).
            03  HC17B-SSEIKYUKEN  PIC X(9).
      *--- 2010/12/07 CHANGE END   -------------------------------------
            03  HC17B-SRYOSYUKEN  PIC X(9).
            03  HC17B-SSYUEKIKEN  PIC X(9).
            03  HC17B-SASEIKYUKEN PIC X(9).
            03  HC17B-SARYOSYUKEN PIC X(9).
            03  HC17B-SASYUEKIKEN PIC X(9).
            03  HC17B-JYUSHINCNT  PIC X(9).
