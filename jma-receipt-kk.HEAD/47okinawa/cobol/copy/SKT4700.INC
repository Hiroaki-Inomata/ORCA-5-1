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
      *  01.00.01    吉川         11/03/04  特別療養費国保対応
      *  01.00.02    井上         18/11/01  修正履歴コメント削除
      ********************************************************************
        01  SKT4700.
            03  SKT4700-SRYYM               PIC X(16).
            03  SKT4700-SEIYMD              PIC X(22).
            03  SKT4700-HOSPCD              PIC X(14).
            03  SKT4700-HOSPADRS            PIC X(100).
            03  SKT4700-HOSPNAME            PIC X(100).
            03  SKT4700-KAISETU             PIC X(40).
            03  SKT4700-TEL                 PIC X(15).
            03  SKT4700-RECEKA              PIC X(20).
            03  SKT4700-TOKUBETU            PIC X(10).
            03  SKT4700-SEIKYU-TBL          OCCURS  17 TIMES.
                05  SKT4700-SEINYUGAI-TBL   OCCURS  2 TIMES.
                    07  SKT4700-KENSU       PIC X(05).
            03  SKT4700-GKENSU              PIC X(06).
            03  SKT4700-KOH-TBL             OCCURS  6 TIMES.
                05  SKT4700-HBTNUM          PIC X(02).
                05  SKT4700-KOHNAME         PIC X(08).
                05  SKT4700-KOHNYUGAI-TBL   OCCURS  2 TIMES.
                    07  SKT4700-KKENSU      PIC X(05).
            03  SKT4700-GKKENSU             PIC X(06).
            03  SKT4700-SKKENSU             PIC X(13).
