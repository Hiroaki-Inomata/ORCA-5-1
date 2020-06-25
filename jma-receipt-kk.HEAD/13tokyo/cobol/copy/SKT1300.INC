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
      *  01.00.01    吉川    2010/08/03  特別療養費国保対応
      *  01.00.02    門間    2016/10/04  帳票様式の変更
      *  01.00.03    門間    2018/11/02  修正履歴コメント削除
      *  01.00.04    門間    2019/09/02  開設者氏名の印字文字数の増加
      ********************************************************************
        01  SKT1300.
      *    診療年月
            02  SKT1300-SEIYM   PIC X(16).
      *    住所
            02  SKT1300-ADRS    PIC X(100).
      *    医療機関名
            02  SKT1300-HOSPNAME    PIC X(100).
      *    開設者名
            02  SKT1300-KAISETUNAME PIC X(40).
      *    電話番号
            02  SKT1300-TEL PIC X(15).
            02  SKT1300-HOSPCD  PIC X(20).
            02  SKT1300-ZIP-TBL.
              03  SKT1300-ZIP1    PIC X(6).
              03  SKT1300-ZIP2    PIC X(2).
              03  SKT1300-ZIP3    PIC X(2).
              03  SKT1300-ZIP4    PIC X(2).
              03  SKT1300-ZIP5    PIC X(2).
      *    在宅
            02  SKT1300-ZAI1    PIC X(2).
            02  SKT1300-ZAI2    PIC X(2).
      *    特別療養費
            02  SKT1300-TOKUBETU  PIC X(10).
      *    集計テーブル
            02  SKT1300-SYUKEI-TBL      OCCURS  2 TIMES.
              03  SKT1300-HKN-TBL         OCCURS  4 TIMES.
                04  SKT1300-KENSU   PIC X(5).
                04  SKT1300-TENSU   PIC X(14).
                04  SKT1300-SKENSU  PIC X(5).
                04  SKT1300-SKINGK  PIC X(14).
                04  SKT1300-HEIYOKEN  PIC X(5).
      *    後期高齢者集計テーブル
            02  SKT1300-KOUKI-TRL       OCCURS  2 TIMES.
              03  SKT1300-KKENSU  PIC X(5).
              03  SKT1300-KTENSU  PIC X(14).
              03  SKT1300-KHEIYOKEN   PIC X(5).
              03  SKT1300-KSKENSU PIC X(5).
              03  SKT1300-KSKINGK PIC X(14).
      *    開設者名２
            02  SKT1300-KAISETUNAME2 PIC X(60).
