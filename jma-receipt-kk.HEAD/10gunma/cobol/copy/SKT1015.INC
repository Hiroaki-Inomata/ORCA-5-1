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
      *  後期高齢者請求書（群馬県）     COPY (SKT1015.INC)               
      *
      *  作成日付    作業者        記述
      *  08/04/16    田中          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2008/05/22  公費名は印字しない
      *  01.00.02    吉川    2013/07/09  食事回数の桁数増加
      ********************************************************************
       01  SKT1015.
      *    保険者番号
           03       SKT1015-HKNJANUM              PIC  X(04).
      *    保険者名
           03       SKT1015-HKNJANAME             PIC  X(20).
      *    医療機関コード
           03       SKT1015-HOSPCD                PIC  X(14).
      *    ページ番号
           03       SKT1015-NO                    PIC  X(04).
      *    診療年月
           03       SKT1015-SRYYM                 PIC  X(16).
      *    請求年月日
           03       SKT1015-SEIYMD                PIC  X(22).
      *    住所 
           03       SKT1015-ADRS                  PIC  X(100).
      *    医療機関名
           03       SKT1015-HOSPNAME              PIC  X(100).
      *    開設者名
           03       SKT1015-KAISETUNAME           PIC  X(40).
      *    電話番号
           03       SKT1015-TEL                   PIC  X(15).
      *    在総診
           03       SKT1015-ZAI1                  PIC  X(02).
           03       SKT1015-ZAI2                  PIC  X(02).
      *    請求テーブル
           03       SKT1015-HKN-TBL.
               05  SKT1015-ROUHO-TBL  OCCURS  2.      
                   07  SKT1015-NYUGAI-TBL  OCCURS  2.
                       09  SKT1015-KENSU                PIC  X(05).
                       09  SKT1015-NISSU                PIC  X(05).
                       09  SKT1015-TENSU                PIC  X(09).
                       09  SKT1015-ITBFTN               PIC  X(09).
                   07  SKT1015-S-KENSU                  PIC  X(05).
                   07  SKT1015-S-NISSU                  PIC  X(06).
                   07  SKT1015-S-KINGK                  PIC  X(09).
                   07  SKT1015-S-FTNGK                  PIC  X(09).
               05  SKT1015-KOH-TBL  OCCURS 4.
                   07  SKT1015-HBTNUM                  PIC  X(02).
                   07  SKT1015-NYUGAI-TBL  OCCURS  2.
                       09  SKT1015-KKENSU             PIC  X(05).
                       09  SKT1015-KNISSU             PIC  X(05).
                       09  SKT1015-KTENSU             PIC  X(09).
                       09  SKT1015-KITBFTN            PIC  X(09).
                   07  SKT1015-S-KKENSU                PIC  X(05).
                   07  SKT1015-S-KNISSU                PIC  X(06).
                   07  SKT1015-S-KKINGK                PIC  X(09).
                   07  SKT1015-S-KFTNGK                PIC  X(09).
