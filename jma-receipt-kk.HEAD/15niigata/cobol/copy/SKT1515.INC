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
      *    後期高齢者請求書 （新潟県）     COPY (SKT1515.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/29    田中          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT1515.
      *    印刷年月
            03  SKT1515-SRYYM   PIC X(16).
      *    医療機関住所
            03  SKT1515-ADRS    PIC X(100).
      *    医療機関名称
            03  SKT1515-HOSPNAME    PIC X(100).
      *    開設者名
            03  SKT1515-KAISETUNAME PIC X(40).
      *    医療機関TEL
            03  SKT1515-TEL PIC X(15).
      *    保険者番号
            03  SKT1515-HKNJANUM    PIC X(12).
      *    医療機関コード
            03  SKT1515-HOSPCD  PIC X(14).
      *    保険者名
            03  SKT1515-HKNJANAME   PIC X(100).
      *    請求年月日
            03  SKT1515-SEIYMD  PIC X(22).
      *    在宅
            03  SKT1515-ZAI1    PIC X(2).
            03  SKT1515-ZAI2    PIC X(2).
      *    ページ番号
            03  SKT1515-NO  PIC X(4).
      *    請求テーブル
            03  SKT1515-SEIKYU-TBL      OCCURS  2 TIMES.
      *    入外テーブル
              05  SKT1515-NYUGAI-TBL      OCCURS  2 TIMES.
      *    件数
                07  SKT1515-KENSU   PIC X(5).
      *    日数
                07  SKT1515-NISSU   PIC X(5).
      *    点数
                07  SKT1515-TENSU   PIC X(9).
      *    一部負担金
                07  SKT1515-ITBFTN  PIC X(9).
      *    食事件数
              05  SKT1515-S-KENSU PIC X(5).
      *    食事日数
              05  SKT1515-S-NISSU PIC X(5).
      *    金額
              05  SKT1515-S-KINGK PIC X(9).
      *    標準負担金
              05  SKT1515-S-FTNGK PIC X(9).
      *    地方公費テーブル
            03  SKT1515-FTN-TBL         OCCURS  2 TIMES.
      *    件数
              05  SKT1515-FKENSU  PIC X(5).
      *    点数
              05  SKT1515-FTENSU  PIC X(9).
      *    食事件数
              05  SKT1515-S-FKENSU    PIC X(5).
      *    金額
              05  SKT1515-S-FKINGK    PIC X(9).
      *    標準負担金
              05  SKT1515-S-FFTNGK    PIC X(9).
      *    公費請求テーブル
            03  SKT1515-KOUHI-TBL       OCCURS  5 TIMES.
      *    法別番号
              05  SKT1515-HBTNUM  PIC X(2).
      *    公費名
              05  SKT1515-KOHNAME PIC X(10).
      *    公費入外テーブル
              05  SKT1515-KNYUGAI-TBL     OCCURS  2 TIMES.
      *    公費件数
                07  SKT1515-KKENSU  PIC X(5).
      *    公費日数
                07  SKT1515-KNISSU  PIC X(5).
      *    公費点数
                07  SKT1515-KTENSU  PIC X(9).
      *    公費一部負担金
                07  SKT1515-KITBFTN PIC X(9).
      *    公費食事件数
              05  SKT1515-S-KKENSU    PIC X(5).
      *    公費食事日数
              05  SKT1515-S-KNISSU    PIC X(5).
      *    公費食事金額
              05  SKT1515-S-KKINGK    PIC X(9).
      *    公費標準負担金
              05  SKT1515-S-KFTNGK    PIC X(9).
