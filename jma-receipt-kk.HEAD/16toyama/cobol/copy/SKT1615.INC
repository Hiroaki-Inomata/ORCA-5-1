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
      *    後期高齢者請求書 （富山県）     COPY (SKT1615.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/23    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/05/13  開設者名のバイト数増加
      *  01.00.02    門間    2020/01/07  修正履歴コメント削除
      ********************************************************************
       01  SKT1615.
      *    印刷年月
           03  SKT1615-SRYYM               PIC X(16).
      *    医療機関住所
           03  SKT1615-ADRS                PIC X(100).
      *    医療機関名称
           03  SKT1615-HOSPNAME            PIC X(100).
      *    開設者名
           03  SKT1615-KAISETUNAME         PIC X(80).
      *    保険者番号
           03  SKT1615-HKNJANUM            PIC X(12).
      *    医療機関コード
           03  SKT1615-HOSPCD              PIC X(40).
      *    保険者名
           03  SKT1615-HKNJANAME           PIC X(100).
      *    請求年月日
           03  SKT1615-SEIYMD              PIC X(22).
      *    在宅
           03  SKT1615-ZAI1                PIC X(02).
           03  SKT1615-ZAI2                PIC X(02).
      *    ページ番号
           03  SKT1615-NO                  PIC X(04).
      *    請求テーブル
           03  SKT1615-SEIKYU-TBL   OCCURS 2.
      *    入外テーブル
               05  SKT1615-NYUGAI-TBL   OCCURS 2.
      *    件数
                   07  SKT1615-KENSU       PIC X(04).
      *    日数
                   07  SKT1615-NISSU       PIC X(04).
      *    点数
                   07  SKT1615-TENSU       PIC X(07).
      *    一部負担金
                   07  SKT1615-ITBFTN      PIC X(07).
      *    長期件数
                   07  SKT1615-TYOKI       PIC X(04).
      *    食事件数
               05  SKT1615-S-KENSU         PIC X(04).
      *    食事日数
               05  SKT1615-S-NISSU         PIC X(04).
      *    金額
               05  SKT1615-S-KINGK         PIC X(07).
      *    標準負担金
               05  SKT1615-S-FTNGK         PIC X(07).
