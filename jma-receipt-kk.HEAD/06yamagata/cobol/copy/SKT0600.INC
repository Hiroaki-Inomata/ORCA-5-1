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
      *  01.00.02    井上    2018/10/12  修正履歴コメント削除
      ********************************************************************
       01  SKT0600.
           03         SKT0600-SEIYM            PIC  X(16).
           03         SKT0600-HOSPCD           PIC  X(07).
           03         SKT0600-ADRS             PIC  X(100).
           03         SKT0600-HOSPNAME         PIC  X(100).
           03         SKT0600-KAISETUNAME      PIC  X(40).
           03         SKT0600-TOKUBETU         PIC  X(10).
           03         SKT0600-DATA.
             05         SKT0600-DATA-TBL         OCCURS  27.
               07       SKT0600-HKNJANAME        PIC  X(20).
               07       SKT0600-KAMIBUN          PIC  X(12).
               07       SKT0600-NYUIN.
                 09     SKT0600-N-KENSU          PIC  X(07).
                 09     SKT0600-N-NISSU          PIC  X(07).
                 09     SKT0600-N-TENSU          PIC  X(14).
               07       SKT0600-GAIRAI.
                 09     SKT0600-G-KENSU          PIC  X(07).
                 09     SKT0600-G-NISSU          PIC  X(07).
                 09     SKT0600-G-TENSU          PIC  X(14).
