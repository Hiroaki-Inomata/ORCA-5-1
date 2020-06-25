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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/11/05  特別療養費国保対応
      *  01.00.02    吉川    2012/05/10  帳票様式の変更
      *  01.00.03    井上    2018/10/12  修正履歴コメント削除
      ********************************************************************
        01  SKT0610.
            03  SKT0610-SRYYM           PIC X(16).
            03  SKT0610-SEIYMD          PIC X(22).
            03  SKT0610-HKNJANAME       PIC X(26).
            03  SKT0610-HKNJANUM        PIC X(06).
            03  SKT0610-HOSPCD          PIC X(07).
            03  SKT0610-ADRS            PIC X(100).
            03  SKT0610-HOSPNAME        PIC X(100).
            03  SKT0610-KAISETU         PIC X(40).
            03  SKT0610-TEL             PIC X(15).
            03  SKT0610-FAX             PIC X(15).
            03  SKT0610-NO              PIC X(04).
            03  SKT0610-TOKUBETU        PIC X(10).
            03  SKT0610-SEIKYU-TBL      OCCURS  13 TIMES.
                05  SKT0610-KENSU       PIC X(05).
                05  SKT0610-NISSU       PIC X(05).
                05  SKT0610-TENSU       PIC X(09).
                05  SKT0610-ITBFTN      PIC X(09).
            03  SKT0610-KOHI-TBL      OCCURS  12 TIMES.
                05  SKT0610-HBTNUM      PIC X(02).
                05  SKT0610-KKENSU      PIC X(05).
                05  SKT0610-KNISSU      PIC X(05).
                05  SKT0610-KTENSU      PIC X(09).
                05  SKT0610-KITBFTN     PIC X(09).
