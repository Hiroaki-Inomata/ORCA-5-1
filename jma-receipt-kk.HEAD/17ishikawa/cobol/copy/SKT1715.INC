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
      *    後期高齢者請求書（石川県）     COPY (SKT1715.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/29    田中          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT1715.
            02  SKT1715-SRYYM   PIC X(16).
            02  SKT1715-SEIYMD  PIC X(22).
            02  SKT1715-HOSPCDN  PIC X(20).
            02  SKT1715-HKNJANUM    PIC X(12).
            02  SKT1715-ADRS    PIC X(100).
            02  SKT1715-HOSPNAME    PIC X(100).
            02  SKT1715-KAISETUNAME PIC X(40).
            02  SKT1715-ZAI1    PIC X(2).
            02  SKT1715-ZAI2    PIC X(2).
            02  SKT1715-NO  PIC X(4).
            02  SKT1715-SEIKYU-TBL      OCCURS  2 TIMES.
              03  SKT1715-NYUGAI-TBL      OCCURS  3 TIMES.
                04  SKT1715-KENSU   PIC X(5).
                04  SKT1715-NISSU   PIC X(5).
                04  SKT1715-TENSU   PIC X(9).
                04  SKT1715-ITBFTN  PIC X(9).
                04  SKT1715-TYOKI   PIC X(3).
              03  SKT1715-S-KENSU PIC X(5).
              03  SKT1715-S-NISSU PIC X(5).
              03  SKT1715-S-KINGK PIC X(9).
              03  SKT1715-S-FTNGK PIC X(9).
            02  SKT1715-KOUHI-TBL       OCCURS  7 TIMES.
              03  SKT1715-HBTNUM  PIC X(2).
              03  SKT1715-K-NYUGAI-TBL            OCCURS  2 TIMES.
                04  SKT1715-KKENSU  PIC X(5).
                04  SKT1715-KNISSU  PIC X(5).
                04  SKT1715-KTENSU  PIC X(9).
                04  SKT1715-KITBFTN PIC X(9).
              03  SKT1715-S-KKENSU    PIC X(5).
              03  SKT1715-S-KNISSU    PIC X(5).
              03  SKT1715-S-KKINGK    PIC X(9).
              03  SKT1715-S-KFTNGK    PIC X(9).
            02  SKT1715-KEKKAU-TBL.
              03  SKT1715-KK-NYUGAI       OCCURS  2 TIMES.
                04  SKT1715-KKKENSU PIC X(5).
                04  SKT1715-KKNISSU PIC X(5).
                04  SKT1715-KKTENSU PIC X(9).
                04  SKT1715-KKITBFTN    PIC X(9).
              03  SKT1715-S-KKKENSU   PIC X(5).
              03  SKT1715-S-KKNISSU   PIC X(5).
              03  SKT1715-S-KKKINGK   PIC X(9).
              03  SKT1715-S-KKFTNGK   PIC X(10).
            02  SKT1715-KOUSEI-TBL.
              03  SKT1715-KOU-NYUGAI      OCCURS  2 TIMES.
                04  SKT1715-KOUKENSU    PIC X(5).
                04  SKT1715-KOUNISSU    PIC X(5).
                04  SKT1715-KOUTENSU    PIC X(9).
                04  SKT1715-KOUITBFTN   PIC X(9).
              03  SKT1715-S-KOUKENSU  PIC X(5).
              03  SKT1715-S-KOUNISSU  PIC X(5).
              03  SKT1715-S-KOUKINGK  PIC X(9).
              03  SKT1715-S-KOUFTNGK  PIC X(9).
            02  SKT1715-TOKUTEI-TBL.
              03  SKT1715-TOK-NYUGAI      OCCURS  2 TIMES.
                04  SKT1715-TOKKENSU    PIC X(5).
                04  SKT1715-TOKNISSU    PIC X(5).
                04  SKT1715-TOKTENSU    PIC X(9).
                04  SKT1715-TOKITBFTN   PIC X(9).
              03  SKT1715-S-TOKKENSU  PIC X(5).
              03  SKT1715-S-TOKNISSU  PIC X(5).
              03  SKT1715-S-TOKKINGK  PIC X(9).
              03  SKT1715-S-TOKFTNGK  PIC X(9).
