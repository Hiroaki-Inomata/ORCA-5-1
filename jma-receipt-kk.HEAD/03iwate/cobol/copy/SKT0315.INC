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
      *    後期高齢者請求書 （岩手県）     COPY (SKT0315.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/23    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付         内容
      * 01.00.01     吉川     2008/05/03   医療機関コードバイト数増加
      * 01.00.02     吉川     2012/04/19   帳票様式の変更
      * 01.00.03     吉川     2015/04/01   帳票様式の変更
      ********************************************************************
       01  SKT0315.
      *    印刷年月
           03  SKT0315-SRYYM               PIC X(16).
      *    医療機関住所
           03  SKT0315-ADRS                PIC X(100).
      *    医療機関TEL
           03  SKT0315-TEL                 PIC X(15).
      *    医療機関名称
           03  SKT0315-HOSPNAME            PIC X(100).
      *    開設者名
           03  SKT0315-KAISETUNAME         PIC X(40).
      *    保険者番号
           03  SKT0315-HKNJANUM            PIC X(12).
      *    医療機関コード
           03  SKT0315-HOSPCD              PIC X(40).
      *    保険者名
           03  SKT0315-HKNJANAME           PIC X(100).
      *    請求年月日
           03  SKT0315-SEIYMD              PIC X(22).
      *    ページ番号
           03  SKT0315-NO                  PIC X(04).
      *    請求テーブル
           03  SKT0315-SEIKYU-TBL   OCCURS 2.
      *    入外テーブル
               05  SKT0315-NYUGAI-TBL   OCCURS 2.
      *    件数
                   07  SKT0315-KENSU       PIC X(05).
      *    日数
                   07  SKT0315-NISSU       PIC X(05).
      *    点数
                   07  SKT0315-TENSU       PIC X(09).
      *    一部負担金
                   07  SKT0315-ITBFTN      PIC X(09).
      *    長期件数
                   07  SKT0315-TYOKI       PIC X(02).
      *    食事件数
               05  SKT0315-S-KENSU         PIC X(05).
      *    食事日数
               05  SKT0315-S-NISSU         PIC X(05).
      *    金額
               05  SKT0315-S-KINGK         PIC X(09).
      *    標準負担金
               05  SKT0315-S-FTNGK         PIC X(09).
      *    在宅テーブル
           03  SKT0315-ZAI-TBL         OCCURS  2 TIMES.
      *    件数
               05  SKT0315-ZKENSU  PIC X(5).
      *    日数
               05  SKT0315-ZNISSU  PIC X(5).
      *    点数
               05  SKT0315-ZTENSU  PIC X(9).
      *    一部負担金
               05  SKT0315-ZITBFTN PIC X(9).
      *    長期件数
               05  SKT0315-ZTYOKI  PIC X(2).
      *    既定公費請求テーブル
           03  SKT0315-KITEI-TBL  OCCURS  5.
      *    既定公費入外テーブル
               05  SKT0315-KINYUGAI-TBL OCCURS  2.
      *    既定公費件数
                   07  SKT0315-KIKENSU     PIC X(05).
      *    既定公費日数
                   07  SKT0315-KINISSU     PIC X(05).
      *    既定公費点数
                   07  SKT0315-KITENSU     PIC X(09).
      *    既定公費一部負担金
                   07  SKT0315-KIITBFTN    PIC X(09).
      *    既定公費食事件数
               05  SKT0315-S-KIKENSU       PIC X(05).
      *    既定公費食事日数
               05  SKT0315-S-KINISSU       PIC X(05).
      *    既定公費食事金額
               05  SKT0315-S-KIKINGK       PIC X(09).
      *    既定公費標準負担金
               05  SKT0315-S-KIFTNGK       PIC X(09).
      *    公費請求テーブル
           03   SKT0315-KOUHI-TBL  OCCURS  3.
      *    法別番号
               05  SKT0315-HBTNUM          PIC X(02).
      *    公費名
               05  SKT0315-KOHNAME         PIC X(10).
      *    公費入外テーブル
               05  SKT0315-KNYUGAI-TBL  OCCURS  2.
      *    公費件数
                   07  SKT0315-KKENSU      PIC X(05).
      *    公費日数
                   07  SKT0315-KNISSU      PIC X(05).
      *    公費点数
                   07  SKT0315-KTENSU      PIC X(09).
      *    公費一部負担金
                   07  SKT0315-KITBFTN     PIC X(09).
      *    公費食事件数
               05  SKT0315-S-KKENSU        PIC X(05).
      *    公費食事日数
               05  SKT0315-S-KNISSU        PIC X(05).
      *    公費食事金額
               05  SKT0315-S-KKINGK        PIC X(09).
      *    公費標準負担金
               05  SKT0315-S-KFTNGK        PIC X(09).
