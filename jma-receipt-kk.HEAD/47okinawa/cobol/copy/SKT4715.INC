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
      * Maj/Min/Rev  修正者  日付         内容
      *  01.00.00    田中    2008/05/01   新規作成
      *  01.00.01    門間    2017/10/05   診療科別で帳票を分けるように修正
      *  01.00.02    井上    2018/11/01   修正履歴コメント削除
      ******************************************************************
        01  SKT4715.
            03  SKT4715-SRYYM   PIC X(16).
            03  SKT4715-SEIYMD  PIC X(22).
            03  SKT4715-HOSPCD  PIC X(14).
            03  SKT4715-HKNJANUM    PIC X(16).
            03  SKT4715-ADRS    PIC X(100).
            03  SKT4715-HOSPNAME    PIC X(100).
            03  SKT4715-TEL PIC X(20).
            03  SKT4715-KAISETUNAME PIC X(40).
            03  SKT4715-ZAI1    PIC X(2).
            03  SKT4715-ZAI2    PIC X(2).
            03  SKT4715-NO  PIC X(4).
            03  SKT4715-RECEKA          PIC X(10).
            03  SKT4715-SEIKYU-TBL      OCCURS  2 TIMES.
              05  SKT4715-NYUGAI-TBL      OCCURS  2 TIMES.
                07  SKT4715-KENSU   PIC X(5).
                07  SKT4715-NISSU   PIC X(5).
                07  SKT4715-TENSU   PIC X(9).
                07  SKT4715-ITBFTN  PIC X(9).
              05  SKT4715-S-KENSU PIC X(5).
              05  SKT4715-S-NISSU PIC X(5).
              05  SKT4715-S-KINGK PIC X(9).
              05  SKT4715-S-FTNGK PIC X(9).
            03  SKT4715-KOUHI-TBL       OCCURS  6 TIMES.
              05  SKT4715-HBTNUM  PIC X(2).
              05  SKT4715-KOHNAME   PIC X(10).
              05  SKT4715-KNYUGAI-TBL            OCCURS  2 TIMES.
                07  SKT4715-KKENSU  PIC X(5).
                07  SKT4715-KNISSU  PIC X(5).
                07  SKT4715-KTENSU  PIC X(9).
                07  SKT4715-KITBFTN PIC X(9).
              05  SKT4715-S-KKENSU    PIC X(5).
              05  SKT4715-S-KNISSU    PIC X(5).
              05  SKT4715-S-KKINGK    PIC X(9).
              05  SKT4715-S-KFTNGK    PIC X(9).
