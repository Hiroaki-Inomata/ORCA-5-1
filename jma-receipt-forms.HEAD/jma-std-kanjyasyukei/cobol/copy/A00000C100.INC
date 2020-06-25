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
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/15  コメント履歴削除
      *                                     open-cobol1.0対応
      *****************************************************************
       01  C100.
          03        C100-MIDASHI        PIC  X(60).
          03        C100-JOBMSG         PIC  X(60).
          03        C100-PAGE           PIC  X(03).
          03        C100-PRTYMD         PIC  X(22).
          03        C100-SRYMD-FR       PIC  X(22).
          03        C100-SRMD-TO        PIC  X(12).
          03        C100-TANI           PIC  X(06).
          03        C100-MD-G.
           05       C100-MD-TBL         OCCURS 32.
            07      C100-MD             PIC  X(05).
          03        C100-SRYKA-G.
           05       C100-SRYKA-TBL      OCCURS  7.
            07      C100-SRYKA          PIC  X(20).
            07      C100-KENSU-1        PIC  X(06).
            07      C100-KENSU-2        PIC  X(06).
            07      C100-KENSU-3        PIC  X(06).
            07      C100-11-G.
             08     C100-11-TBL         OCCURS 32.
              09    C100-11-CNT         PIC  X(06).
            07      C100-12-G.
             08     C100-12-TBL         OCCURS 32.
              09    C100-12-CNT         PIC  X(06).
            07      C100-99-G.
             08     C100-99-TBL         OCCURS 32.
              09    C100-99-CNT         PIC  X(06).
