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
      ********************************************************************
      *    後期高齢者請求書 （山梨県）     COPY (SKT1915.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/22    安藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT1915.
      *     印刷年月
            02  SKT1915-SRYYM   PIC X(16).
      *     医療機関住所
            02  SKT1915-ADRS    PIC X(100).
      *     医療機関電話番号
            02  SKT1915-TEL PIC X(28).
      *     医療機関名称
            02  SKT1915-HOSPNAME    PIC X(100).
      *     開設者名
            02  SKT1915-KAISETUNAME PIC X(40).
      *     医療機関コード
            02  SKT1915-HOSPCD  PIC X(40).
      *     保険者番号
            02  SKT1915-HKNJANAME   PIC X(100).
      *     請求年月日
            02  SKT1915-SEIYMD  PIC X(22).
      *     在宅
            02  SKT1915-ZAI1    PIC X(2).
            02  SKT1915-ZAI2    PIC X(2).
      *     ページ番号
            02  SKT1915-NO  PIC X(4).
      *     請求テーブル
            02  SKT1915-SEIKYU-TBL      OCCURS  2 TIMES.
      *     入外テーブル
              03  SKT1915-NYUGAI-TBL      OCCURS  2 TIMES.
      *     件数
                04  SKT1915-KENSU   PIC X(5).
      *     日数
                04  SKT1915-NISSU   PIC X(5).
      *     点数
                04  SKT1915-TENSU   PIC X(9).
      *     一部負担金
                04  SKT1915-ITBFTN  PIC X(9).
      *     食事件数
              03  SKT1915-S-KENSU PIC X(5).
      *     食事日数
              03  SKT1915-S-NISSU PIC X(5).
      *     金額
              03  SKT1915-S-KINGK PIC X(9).
      *     標準負担金
              03  SKT1915-S-FTNGK PIC X(9).
      *     公費請求テーブル
            02  SKT1915-KOUHI-TBL       OCCURS  3 TIMES.
      *     公費名
              03  SKT1915-KOHNAME PIC X(10).
      *     法別番号
              03  SKT1915-HBTNUM  PIC X(2).
      *     公費入外テーブル
              03  SKT1915-KNYUGAI-TBL      OCCURS  2 TIMES.
      *     公費件数
                04  SKT1915-KKENSU  PIC X(5).
      *     公費日数
                04  SKT1915-KNISSU  PIC X(5).
      *     公費点数
                04  SKT1915-KTENSU  PIC X(9).
      *     公費一部負担金
                04  SKT1915-KITBFTN PIC X(9).
      *     公費食事件数
              03  SKT1915-S-KKENSU    PIC X(5).
      *     公費食事日数
              03  SKT1915-S-KNISSU    PIC X(5).
      *     公費食事金額
              03  SKT1915-S-KKINGK    PIC X(9).
      *     公費標準負担金
              03  SKT1915-S-KFTNGK    PIC X(9).
      *     地方公費テーブル
            02  SKT1915-TKOUHI-TBL      OCCURS  2 TIMES.
      *     入外テーブル
              03  SKT1915-TNYUGAI-TBL      OCCURS  2 TIMES.
      *     地方公費件数
                04  SKT1915-TKENSU PIC X(5).
      *     地方公費日数
                04  SKT1915-TNISSU PIC X(5).
      *     地方公費点数
                04  SKT1915-TTENSU PIC X(9).
      *     地方公費一部負担金
                 04  SKT1915-TITBFTN   PIC X(9).
