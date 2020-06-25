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
      *    後期高齢者請求書 （北海道）     COPY (SKT0115.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/22    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2008/06/27  マル長欄追加対応
      *  01.00.02    吉川    2008/07/23  備考欄に「超過」「標欠」印字対応
      *                                  マル長欄削除。元に戻す
      *  01.00.03    萩野    2018/09/10  修正履歴コメント削除
      ********************************************************************
       01  SKT0115.
      *    印刷年月
           03  SKT0115-SRYYM               PIC X(16).
      *    医療機関住所
           03  SKT0115-ADRS                PIC X(100).
      *    医療機関名称
           03  SKT0115-HOSPNAME            PIC X(100).
      *    開設者名
           03  SKT0115-KAISETUNAME         PIC X(40).
      *    保険者番号
           03  SKT0115-HKNJANUM            PIC X(12).
      *    医療機関コード
           03  SKT0115-HOSPCD              PIC X(14).
      *    保険者名
           03  SKT0115-HKNJANAME           PIC X(20).
      *    請求年月日
           03  SKT0115-SEIYMD              PIC X(22).
      *    在宅
           03  SKT0115-ZAI1                PIC X(02).
           03  SKT0115-ZAI2                PIC X(02).
      *    ページ番号
           03  SKT0115-NO                  PIC X(04).
      *    請求テーブル
           03  SKT0115-SEIKYU-TBL   OCCURS  2.
      *    入外テーブル
               05  SKT0115-NYUGAI-TBL   OCCURS 2.
      *    件数
                   07  SKT0115-KENSU       PIC X(05).
      *    日数
                   07  SKT0115-NISSU       PIC X(05).
      *    点数
                   07  SKT0115-TENSU       PIC X(09).
      *    一部負担金
                   07  SKT0115-ITBFTN      PIC X(09).
      *    食事件数
               05  SKT0115-S-KENSU         PIC X(05).
      *    食事日数
               05  SKT0115-S-NISSU         PIC X(05).
      *    金額
               05  SKT0115-S-KINGK         PIC X(09).
      *    標準負担金
               05  SKT0115-S-FTNGK         PIC X(09).
      *    公費請求テーブル
           03   SKT0115-KOUHI-TBL   OCCURS  4.
      *    法別番号
               05  SKT0115-HBTNUM1         PIC X(02).
               05  SKT0115-HBTNUM2         PIC X(02).
      *    公費入外テーブル
               05  SKT0115-KNYUGAI-TBL  OCCURS  2.
      *    公費件数
                   07  SKT0115-KKENSU      PIC X(05).
      *    公費日数
                   07  SKT0115-KNISSU      PIC X(05).
      *    公費点数
                   07  SKT0115-KTENSU      PIC X(09).
      *    公費一部負担金
                   07  SKT0115-KITBFTN     PIC X(09).
      *    公費食事件数
               05  SKT0115-S-KKENSU        PIC X(05).
      *    公費食事日数
               05  SKT0115-S-KNISSU        PIC X(05).
      *    公費食事金額
               05  SKT0115-S-KKINGK        PIC X(09).
      *    公費標準負担金
               05  SKT0115-S-KFTNGK        PIC X(09).
      *    備考テーブル
           03  SKT0115-BIKO-TBL.
      *    超過
               05  SKT0115-TYOKA           PIC X(04).
               05  SKT0115-TYOKA-MARU      PIC X(02).
      *    標欠
               05  SKT0115-HYOKETU         PIC X(04).
               05  SKT0115-HYOKETU-MARU    PIC X(02).
