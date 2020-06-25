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
      *  後期高齢者請求書（宮崎県）     COPY (SKT4525.INC)               
      *
      *  作成日付    作業者        記述
      *  08/05/01    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT4525.
      *    保険者番号
            03  SKT4525-HKNJANUM          PIC X(12).
      *    医療機関コード
            03  SKT4525-HOSPCD            PIC X(14).
      *    ページ番号
            03  SKT4525-NO                PIC X(04).
      *    診療年月
            03  SKT4525-SRYYM             PIC X(16).
      *    請求年月日
            03  SKT4525-SEIYMD            PIC X(22).
      *    住所 
            03  SKT4525-ADRS              PIC X(100).
      *    医療機関名
            03  SKT4525-HOSPNAME          PIC X(100).
      *    開設者名
            03  SKT4525-KAISETUNAME       PIC X(40).
      *    電話番号
            03  SKT4525-TEL               PIC X(15).
      *    在宅
            03  SKT4525-ZAI1              PIC X(02).
            03  SKT4525-ZAI2              PIC X(02).
      *    請求テーブル
            03  SKT4525-HKN-TBL.
              05  SKT4525-SEIKYU-TBL  OCCURS  2 TIMES.
                07  SKT4525-NYUGAI-TBL  OCCURS  2 TIMES.
                  09  SKT4525-KENSU       PIC X(05).
                  09  SKT4525-NISSU       PIC X(05).
                  09  SKT4525-TENSU       PIC X(09).
                  09  SKT4525-ITBFTN      PIC X(09).
                07  SKT4525-S-KENSU       PIC X(05).
                07  SKT4525-S-NISSU       PIC X(05).
                07  SKT4525-S-KINGK       PIC X(09).
                07  SKT4525-S-FTNGK       PIC X(09).
              05  SKT4525-KOUHI-TBL  OCCURS  4 TIMES.
                07  SKT4525-HBTNUM        PIC X(02).
                07  SKT4525-KNYUGAI-TBL  OCCURS  2 TIMES.
                  09  SKT4525-KKENSU      PIC X(05).
                  09  SKT4525-KNISSU      PIC X(05).
                  09  SKT4525-KTENSU      PIC X(09).
                  09  SKT4525-KITBFTN     PIC X(09).
                07  SKT4525-S-KKENSU      PIC X(05).
                07  SKT4525-S-KNISSU      PIC X(05).
                07  SKT4525-S-KKINGK      PIC X(09).
                07  SKT4525-S-KFTNGK      PIC X(09).
