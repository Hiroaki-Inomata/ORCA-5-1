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
      *  後期高齢者請求書（宮崎県）     COPY (SKT4535.INC)               
      *
      *  作成日付    作業者        記述
      *  08/05/01    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT4535.
      *    保険者番号
            02  SKT4535-HKNJANUM           PIC X(12).
      *    医療機関コード
            02  SKT4535-HOSPCD             PIC X(14).
      *    ページ番号
            02  SKT4535-NO                 PIC X(04).
      *    診療年月
            02  SKT4535-SRYYM              PIC X(16).
      *    在宅
            02  SKT4535-ZAI1               PIC X(02).
            02  SKT4535-ZAI2               PIC X(02).
      *    請求テーブル
            02  SKT4535-HKN-TBL.
              03  SKT4535-KOUHI-TBL  OCCURS  7 TIMES.
                04  SKT4535-HBTNUM         PIC X(02).
                04  SKT4535-NYUGAI-TBL  OCCURS  2 TIMES.
                  05  SKT4535-KKENSU       PIC X(05).
                  05  SKT4535-KNISSU       PIC X(05).
                  05  SKT4535-KTENSU       PIC X(09).
                  05  SKT4535-KITBFTN      PIC X(09).
                04  SKT4535-S-KKENSU       PIC X(05).
                04  SKT4535-S-KNISSU       PIC X(05).
                04  SKT4535-S-KKINGK       PIC X(09).
                04  SKT4535-S-KFTNGK       PIC X(09).
