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
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      ********************************************************************
       01  M700.
              03  M700-JOBMSG          PIC X(60).
              03  M700-PAGE            PIC X(3).
              03  M700-SYSYMD          PIC X(22).
              03  M700-PRTYM           PIC X(16).
              03  M700-NYUGAIKBN       PIC X(4).
              03  M700-SRYKA           PIC X(20).
              03  M700-TJYUSHIN        PIC X(8).
              03  M700-KENSU1	       PIC X(8).
              03  M700-KENSU2          PIC X(8).
              03  M700-KENSU3          PIC X(10).
              03  M700-JYUSHINCNT-G.
                  05  M700-JYUSHINCNT  PIC X(5)    OCCURS  32 TIMES.
              03  M700-SHKNKEN-G.
                  05  M700-SHKNKEN     PIC X(5)    OCCURS  32 TIMES.
              03  M700-RHKNKEN-G.
                  05  M700-RHKNKEN     PIC X(5)    OCCURS  32 TIMES.
              03  M700-OHKNKEN-G.
                  05  M700-OHKNKEN     PIC X(5)    OCCURS  32 TIMES.
              03  M700-GHKNTEN-G.
                  05  M700-GHKNTEN     PIC X(9)    OCCURS  32 TIMES.
              03  M700-TEN-G.
                  05  M700-TEN-TBL                 OCCURS  16 TIMES.
                      07  M700-TEN     PIC X(9)    OCCURS  32 TIMES.
