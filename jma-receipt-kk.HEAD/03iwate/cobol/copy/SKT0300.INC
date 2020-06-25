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
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01     中川     2006/08/29   請求先名(県内)追加63→64
      *  01.00.02     吉川     2010/03/11   請求先名(県外)追加8→9
      *  01.00.03     吉川     2011/06/15   特別療養費国保対応
      *  01.00.04     吉川     2012/04/19   帳票様式の変更
      *  01.00.05     井上     2018/10/10   修正履歴コメント削除
      ************************************************************
       01  SKT0300.
      *    診療年月
           03  SKT0300-SRYYM                PIC  X(16).
      *    医療機関住所
           03  SKT0300-ADRS                 PIC  X(100).
      *    医療機関名
           03  SKT0300-HOSPNAME             PIC  X(100).
      *    開設者名
           03  SKT0300-KAISETUNAME          PIC  X(40).
      *    電話番号
           03  SKT0300-TEL                  PIC  X(15).
      *    医療機関コード
           03  SKT0300-HOSPCDN              PIC  X(40).
      *    割合（一般保険者）
           03  SKT0300-KYURATE              PIC  X(02).
      *    特別療養費
           03  SKT0300-TOKUBETU             PIC  X(10).
      *    請求テーブル
           03  SKT0300-SEIKYU-TBL     OCCURS  8.
      *    入外テーブル
               05  SKT0300-NYUGAI-TBL     OCCURS  2.
      *    県内県外テーブル
                   07  SKT0300-PREF-TBL     OCCURS  2.
      *    件数
                       09  SKT0300-KENSU    PIC  X(05).
      *    実日数
                       09  SKT0300-NISSU    PIC  X(05).
      *    点数
                       09  SKT0300-TENSU    PIC  X(09).
      *    在総診テーブル
           03  SKT0300-ZAI-TBL     OCCURS  4.
      *    県内県外テーブル
               05  SKT0300-ZPREF-TBL    OCCURS  2.
      *    件数
                   07  SKT0300-ZKENSU       PIC  X(05).
      *    実日数
                   07  SKT0300-ZNISSU       PIC  X(05).
      *    点数
                   07  SKT0300-ZTENSU       PIC  X(09).
      *    合計テーブル
           03  SKT0300-GOKEI-TBL     OCCURS  3.
      *    県内県外テーブル
               05  SKT0300-GPREF-TBL    OCCURS  2.
      *    件数
                   07  SKT0300-GKENSU       PIC  X(05).
      *    実日数
                   07  SKT0300-GNISSU       PIC  X(05).
      *    点数
                   07  SKT0300-GTENSU       PIC  X(09).
      *    県内保険者数
           03  SKT0300-NHKNJA-TBL  OCCURS  40.
               05  SKT0300-NHKNJA           PIC  X(02).
      *    県名
           03  SKT0300-KENMEI-TBL  OCCURS  5.
               05  SKT0300-KENMEI           PIC  X(06).
      *    県外保険者数
           03  SKT0300-GHKNJA-TBL  OCCURS  9.
               05  SKT0300-GHKNJA           PIC  X(02).
           03  SKT0300-GHKNJA-TBL2  OCCURS  5.
               05  SKT0300-GHKNJA2           PIC  X(02).
      *    保険者合計数
           03  SKT0300-GOKEIHKNJA-TBL  OCCURS  2.
               05  SKT0300-GOKEIHKNJA       PIC  X(03).
