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
      *  01.00.01    吉川         10/10/12  帳票様式の変更に伴う修正
      *  01.00.02    井上         19/02/05  修正履歴コメント削除
      ********************************************************************
        01  SC01.
            03  SC01-PRTYM          PIC X(16).
            03  SC01-SEIYMD         PIC X(22).
            03  SC01-HOSPCD         PIC X(14).
            03  SC01-ADRS           PIC X(100).
            03  SC01-HOSPNAME       PIC X(100).
            03  SC01-KAISETU        PIC X(60).
            03  SC01-CITYNAME       PIC X(30).
            03  SC01-FTNJANUM       PIC X(16).
            03  SC01-SEIKYU-TBL         OCCURS  15 TIMES.
                05  SC01-JKYSNUM    PIC X(14).
                05  SC01-NAME       PIC X(32).
                05  SC01-TANJOYMD   PIC X(12).
                05  SC01-SEX        PIC X(02)    OCCURS  2 TIMES.
                05  SC01-HKN        PIC X(02)    OCCURS  2 TIMES.
                05  SC01-NYUGAIKBN  PIC X(02)    OCCURS  2 TIMES.
                05  SC01-FTNRATE    PIC X(01).
                05  SC01-NISSU      PIC X(02).
                05  SC01-TENSU      PIC X(07).
                05  SC01-KOHFTNGK   PIC X(07).
                05  SC01-ITBFTN     PIC X(07).
                05  SC01-NNISSU     PIC X(02).
                05  SC01-NYUFTNGK   PIC X(06).
                05  SC01-SEIKYUGK   PIC X(07).
                05  SC01-SRYYM      PIC X(4).
                05  SC01-HKNJANAME  PIC X(32).
                05  SC01-KOGAKU     PIC X(02).
            03  SC01-GOKEI-TBL          OCCURS  2 TIMES.
                05  SC01-GNISSU     PIC X(04).
                05  SC01-GTENSU     PIC X(09).
                05  SC01-GKOHFTNGK  PIC X(09).
                05  SC01-GITBFTN    PIC X(09).
                05  SC01-GNNISSU    PIC X(04).
                05  SC01-GNYUFTNGK  PIC X(09).
                05  SC01-GSEIKYUGK  PIC X(09).
