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
      *    後期高齢者請求書 （兵庫県）     COPY (SKT2815.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/15    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2008/05/21  足りない公費は備考欄に記載
      *  01.00.02    萩野    2019/03/08  修正履歴コメント削除
      ********************************************************************
       01  SKT2815.
      *    印刷年月
           03  SKT2815-SRYYM               PIC X(16).
      *    医療機関住所
           03  SKT2815-ADRS                PIC X(100).
      *    医療機関名称
           03  SKT2815-HOSPNAME            PIC X(100).
      *    開設者名
           03  SKT2815-KAISETUNAME         PIC X(40).
      *    保険者番号
           03  SKT2815-HKNJANUM            PIC X(12).
      *    医療機関コード
           03  SKT2815-HOSPCD              PIC X(14).
      *    保険者名
           03  SKT2815-HKNJANAME           PIC X(100).
      *    請求年月日
           03  SKT2815-SEIYMD              PIC X(22).
      *    在宅
           03  SKT2815-ZAI1                PIC X(02).
           03  SKT2815-ZAI2                PIC X(02).
      *    ページ番号
           03  SKT2815-NO                  PIC X(04).
      *    請求テーブル
           03  SKT2815-SEIKYU-TBL   OCCURS 2.
      *    入外テーブル
               05  SKT2815-NYUGAI-TBL   OCCURS 2.
      *    件数
                   07  SKT2815-KENSU       PIC X(05).
      *    日数
                   07  SKT2815-NISSU       PIC X(05).
      *    点数
                   07  SKT2815-TENSU       PIC X(09).
      *    一部負担金
                   07  SKT2815-ITBFTN      PIC X(09).
      *    食事件数
               05  SKT2815-S-KENSU         PIC X(05).
      *    食事日数
               05  SKT2815-S-NISSU         PIC X(05).
      *    金額
               05  SKT2815-S-KINGK         PIC X(09).
      *    標準負担金
               05  SKT2815-S-FTNGK         PIC X(09).
      *    公費請求テーブル
           03   SKT2815-KOUHI-TBL  OCCURS 7.
      *    法別番号
               05  SKT2815-HBTNUM          PIC  X(02).
      *    公費名
               05  SKT2815-KOHNAME         PIC  X(12).
      *    備考欄記載用入院
               05  SKT2815-NYUIN           PIC  X(06).
      *    備考欄記載用入院外
               05  SKT2815-NYUINGAI        PIC  X(06).
      *    公費入外テーブル
               05  SKT2815-KNYUGAI-TBL  OCCURS  2.
      *    公費件数
                   07  SKT2815-KKENSU      PIC X(05).
      *    公費日数
                   07  SKT2815-KNISSU      PIC X(05).
      *    公費点数
                   07  SKT2815-KTENSU      PIC X(09).
      *    公費一部負担金
                   07  SKT2815-KITBFTN     PIC X(09).
      *    公費食事件数
               05  SKT2815-S-KKENSU        PIC X(05).
      *    公費食事日数
               05  SKT2815-S-KNISSU        PIC X(05).
      *    公費食事金額
               05  SKT2815-S-KKINGK        PIC X(09).
      *    公費標準負担金
               05  SKT2815-S-KFTNGK        PIC X(09).
