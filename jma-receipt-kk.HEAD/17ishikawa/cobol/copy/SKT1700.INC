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
      *  01.00.01    吉川    2010/08/19  特別療養費国保対応
      *  01.00.02    門間    2019/04/23  修正履歴コメント削除
      ********************************************************************
        01  SKT1700.
            03  SKT1700-SRYYM   PIC X(16).
            03  SKT1700-SEIYMD  PIC X(22).
            03  SKT1700-HOSPCD  PIC X(14).
            03  SKT1700-RECEKA  PIC X(10).
            03  SKT1700-ADRS    PIC X(100).
            03  SKT1700-HOSPNAME    PIC X(100).
            03  SKT1700-TOKUBETU    PIC X(10).
            03  SKT1700-SEIKYU-TBL      OCCURS  3 TIMES.
              05  SKT1700-NYUGAI-TBL      OCCURS  2 TIMES.
                07  SKT1700-KENSU   PIC X(5).
                07  SKT1700-NISSU   PIC X(5).
                07  SKT1700-TENSU   PIC X(9).
                07  SKT1700-ITBFTN  PIC X(9).
              05  SKT1700-SKENSU  PIC X(5).
              05  SKT1700-SKAISU  PIC X(5).
              05  SKT1700-SKINGK  PIC X(9).
              05  SKT1700-SFTNGK  PIC X(9).
            03  SKT1700-KOU-TBL.
              05  SKT1700-NYUGAI-KOU-TBL          OCCURS  2 TIMES.
                07  SKT1700-KOUKENSU    PIC X(5).
                07  SKT1700-KOUNISSU    PIC X(5).
                07  SKT1700-KOUTENSU    PIC X(9).
                07  SKT1700-KOUITBFTN   PIC X(9).
              05  SKT1700-KOUSKENSU   PIC X(5).
              05  SKT1700-KOUSKAISU   PIC X(5).
              05  SKT1700-KOUSKINGK   PIC X(9).
              05  SKT1700-KOUSFTNGK   PIC X(9).
            03  SKT1700-ROU-TBL.
              05  SKT1700-NYUGAI-ROU-TBL          OCCURS  2 TIMES.
                07  SKT1700-ROUKENSU    PIC X(5).
                07  SKT1700-ROUNISSU    PIC X(5).
                07  SKT1700-ROUTENSU    PIC X(9).
                07  SKT1700-ROUITBFTN   PIC X(9).
              05  SKT1700-ROUSKENSU   PIC X(5).
              05  SKT1700-ROUSKAISU   PIC X(5).
              05  SKT1700-ROUSKINGK   PIC X(9).
              05  SKT1700-ROUSFTNGK   PIC X(9).
            03  SKT1700-KOH-TBL.
              05  SKT1700-NYUGAI-KOH-TBL          OCCURS  2 TIMES.
                07  SKT1700-KKENSU  PIC X(5).
                07  SKT1700-KNISSU  PIC X(5).
                07  SKT1700-KTENSU  PIC X(9).
                07  SKT1700-KITBFTN PIC X(9).
              05  SKT1700-KSKENSU PIC X(5).
              05  SKT1700-KSKAISU PIC X(5).
              05  SKT1700-KSKINGK PIC X(9).
              05  SKT1700-KSFTNGK PIC X(9).
