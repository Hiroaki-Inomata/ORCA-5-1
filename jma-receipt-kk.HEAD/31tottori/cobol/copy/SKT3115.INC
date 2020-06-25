      ******************************************************************
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
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    田中      2008/04/27   新規作成
      ******************************************************************
        01  SKT3115.
            03  SKT3115-SRYYM   PIC X(16).
            03  SKT3115-SEIYMD  PIC X(22).
            03  SKT3115-HOSPCD  PIC X(14).
            03  SKT3115-HKNJANUM    PIC X(12).
            03  SKT3115-ADRS    PIC X(100).
            03  SKT3115-HOSPNAME    PIC X(100).
            03  SKT3115-TEL PIC X(20).
            03  SKT3115-KAISETUNAME PIC X(40).
            03  SKT3115-NO  PIC X(4).
            03  SKT3115-SEIKYU-TBL      OCCURS  2 TIMES.
              05  SKT3115-NYUGAI-TBL      OCCURS  2 TIMES.
                07  SKT3115-KENSU   PIC X(5).
                07  SKT3115-NISSU   PIC X(5).
                07  SKT3115-TENSU   PIC X(9).
                07  SKT3115-ITBFTN  PIC X(9).
                07  SKT3115-TYOKI   PIC X(3).
              05  SKT3115-S-KENSU PIC X(5).
              05  SKT3115-S-NISSU PIC X(5).
              05  SKT3115-S-KINGK PIC X(9).
              05  SKT3115-S-FTNGK PIC X(9).
            03  SKT3115-ZAI-TBL         OCCURS  2 TIMES.
              05  SKT3115-ZKENSU    PIC X(5).
              05  SKT3115-ZNISSU    PIC X(5).
              05  SKT3115-ZTENSU    PIC X(9).
              05  SKT3115-ZITBFTN   PIC X(9).
              05  SKT3115-ZTYOKI    PIC X(3).
            03  SKT3115-KOUHI-TBL       OCCURS  5 TIMES.
              05  SKT3115-HBTNUM  PIC X(2).
              05  SKT3115-KOHNAME   PIC X(10).
              05  SKT3115-KNYUGAI-TBL            OCCURS  2 TIMES.
                07  SKT3115-KKENSU  PIC X(5).
                07  SKT3115-KNISSU  PIC X(5).
                07  SKT3115-KTENSU  PIC X(9).
                07  SKT3115-KITBFTN PIC X(9).
              05  SKT3115-S-KKENSU    PIC X(5).
              05  SKT3115-S-KNISSU    PIC X(5).
              05  SKT3115-S-KKINGK    PIC X(9).
              05  SKT3115-S-KFTNGK    PIC X(9).
