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
      *    後期高齢者請求書 （静岡県）     COPY (SKT2215.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/25    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2008/11/19  保険者番号のバイト数増加
      *  01.00.02    吉川    2009/05/13  食事回数と金額のバイト数増加
      *  01.00.03    萩野    2018/09/10  修正履歴コメント削除
      ********************************************************************
       01  SKT2215.
      *    診療年月
           03  SKT2215-SRYYM            PIC X(16).
      *    保険者番号
           03  SKT2215-HKNJANUM         PIC X(16).
      *    保険者名
           03  SKT2215-HKNJANAME        PIC X(20).
      *    在宅
           03  SKT2215-ZAI1             PIC X(02).
           03  SKT2215-ZAI2             PIC X(02).
      *    請求年月日
           03  SKT2215-SEIYMD           PIC X(22).
      *    医療機関コード
           03  SKT2215-HOSPCD           PIC X(07).
      *    医療機関住所
           03  SKT2215-ADRS             PIC X(100).
      *    医療機関名称
           03  SKT2215-HOSPNAME         PIC X(100).
      *    開設者氏名
           03  SKT2215-KAISETUNAME      PIC X(40).
      *    ページ番号
           03  SKT2215-NO               PIC X(04).
      *    請求テーブル
           03  SKT2215-SEIKYU-TBL       OCCURS  2.
      *    入外テーブル
               05  SKT2215-NYUGAI-TBL   OCCURS  2.
      *    件数
                   07  SKT2215-KENSU    PIC X(05).
      *    診療実日数
                   07  SKT2215-NISSU    PIC X(05).
      *    点数
                   07  SKT2215-TENSU    PIC X(09).
      *    一部負担金
                   07  SKT2215-ITBFTN   PIC X(09).
      *    件数
               05  SKT2215-S-KENSU      PIC X(05).
      *    診療実日数
               05  SKT2215-S-NISSU      PIC X(06).
      *    金額
               05  SKT2215-S-KINGK      PIC X(11).
      *    標準負担額
               05  SKT2215-S-FTNGK      PIC X(09).
      *    公費テーブル
           03  SKT2215-KOUHI-TBL        OCCURS  4.
      *    公費名
               05  SKT2215-KOHNAME      PIC X(08).
      *    法別番号
               05  SKT2215-HBTNUM       PIC X(02).
      *    公費入外テーブル
               05  SKT2215-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT2215-KKENSU   PIC X(05).
      *    診療実日数
                   07  SKT2215-KNISSU   PIC X(05).
      *    点数
                   07  SKT2215-KTENSU   PIC X(09).
      *    一部負担金
                   07  SKT2215-KITBFTN  PIC X(09).
      *    件数
               05  SKT2215-S-KKENSU     PIC X(05).
      *    診療実日数
               05  SKT2215-S-KNISSU     PIC X(06).
      *    金額
               05  SKT2215-S-KKINGK     PIC X(11).
      *    標準負担額
               05  SKT2215-S-KFTNGK     PIC X(09).
      *    長期
           03  SKT2215-TYOKI            PIC X(07).
      *    原爆
           03  SKT2215-GENBAKU          PIC X(07).
