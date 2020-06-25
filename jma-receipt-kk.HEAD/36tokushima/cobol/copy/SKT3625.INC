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
      *    後期高齢者請求書（徳島県）     COPY (SKT3625.INC)               
      *
      *  作成日付    作業者        記述
      *  08/04/17    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/06/08  点数、金額、食事回数のバイト数増加
      *  01.00.02    吉川    2015/02/12  開設者氏名のバイト数増加
      ********************************************************************
        01  SKT3625.
      *    診療年月
            03  SKT3625-SRYYM   PIC X(16).
      *    医療機関コード
            03  SKT3625-HOSPCDN PIC X(20).
      *    保険者番号
            03  SKT3625-HKNJANUM    PIC X(16).
      *    住所
            03  SKT3625-ADRS    PIC X(100).
      *    医療機関名
            03  SKT3625-HOSPNAME    PIC X(100).
      *    電話番号
            03  SKT3625-TEL PIC X(15).
      *    開設者名
            03  SKT3625-KAISETUNAME PIC X(60).
      *    在総診・在医総
            03  SKT3625-ZAI1    PIC X(2).
            03  SKT3625-ZAI2    PIC X(2).
      *    ページ番号
            03  SKT3625-NO  PIC X(4).
      *    請求年月日
            03  SKT3625-SEIYMD    PIC X(22).
      *    請求テーブル
            03  SKT3625-SEIKYU-TBL      OCCURS  2 TIMES.
      *    入外テーブル
              05  SKT3625-NYUGAI-TBL      OCCURS  2 TIMES.
      *    件数
                07  SKT3625-KENSU   PIC X(5).
      *    診療実日数
                07  SKT3625-NISSU   PIC X(5).
      *    点数
                07  SKT3625-TENSU   PIC X(10).
      *    一部負担金
                07  SKT3625-ITBFTN  PIC X(9).
      *    食事療養：件数
              05  SKT3625-S-KENSU PIC X(5).
      *    食事療養：日数
              05  SKT3625-S-NISSU PIC X(6).
      *    食事療養：金額
              05  SKT3625-S-KINGK PIC X(10).
      *    食事療養：標準負担額
              05  SKT3625-S-FTNGK PIC X(9).
      *    公費請求テーブル
            03  SKT3625-KOUHI-TBL       OCCURS  4 TIMES.
      *    入外テーブル
              05  SKT3625-KNYUGAI-TBL     OCCURS  2 TIMES.
      *    件数
                07  SKT3625-KKENSU  PIC X(5).
      *    診療実日数
                07  SKT3625-KNISSU  PIC X(5).
      *    点数
                07  SKT3625-KTENSU  PIC X(10).
      *    一部負担金
                07  SKT3625-KITBFTN PIC X(9).
      *    公費名
              05  SKT3625-KOHNAME PIC X(10).
              05  SKT3625-HBTNUM  PIC X(4).
      *    食事療養：件数
              05  SKT3625-S-KKENSU    PIC X(5).
      *    食事療養：日数
              05  SKT3625-S-KNISSU    PIC X(6).
      *    食事療養：金額
              05  SKT3625-S-KKINGK    PIC X(10).
      *    食事療養：標準負担額
              05  SKT3625-S-KFTNGK    PIC X(9).
