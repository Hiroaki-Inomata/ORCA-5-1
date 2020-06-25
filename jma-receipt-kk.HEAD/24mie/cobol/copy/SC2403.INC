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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         10/08/19  受給者番号の桁数増加
      *  01.00.02    吉川         14/03/05  一部負担額、市町コードの追加
      *                                     処方せん医療機関情報の位置変更
      *  01.00.03    萩野         19/05/23  修正履歴コメント削除
      *  01.00.04    萩野         19/05/27  年号の配列増加
      ********************************************************************
       01  SC03.
      *    医療機関コード
           03  SC03-HOSPCD               PIC X(20).
      *    請求年月日
           03  SC03-SEIYMD               PIC X(22).
      *    郵便番号
           03  SC03-POST                 PIC X(10).
      *    所在地
           03  SC03-ADRS                 PIC X(100).
      *    医療機関名称
           03  SC03-HOSPNAME             PIC X(100).
      *    開設者氏名
           03  SC03-KAISETU              PIC X(40).
      *    電話番号
           03  SC03-TEL                  PIC X(15).
      *    市町コード
           03  SC03-CITYCD               PIC X(06).
      *    市町村名
           03  SC03-CITYNAME             PIC X(26).
      *    公費区分
           03  SC03-KOHKBN               PIC X(02)  OCCURS  5.
      *    受給資格証番号
           03  SC03-JKYSNUM              PIC X(20).
      *    患者氏名
           03  SC03-NAME                 PIC X(26).
      *    性別
           03  SC03-SEX                  PIC X(02)  OCCURS  2.
      *    年号
           03  SC03-NENGO                PIC X(02)  OCCURS  5.
      *    生年月日
           03  SC03-TANJOYMD             PIC X(18).
      *    ページ
           03  SC03-PAGE                 PIC X(03).
      *    請求テーブル
           03  SC03-SEIKYU-TBL  OCCURS  4.
      *    診療年月
               05  SC03-SRYYM            PIC X(16).
      *    一部負担割合
               05  SC03-KYURATE          PIC X(02)  OCCURS  4.
      *    診療科名
               05  SC03-SRYKANAME        PIC X(20).
      *    診療科コード
               05  SC03-SRYKA            PIC X(02).
      *    入院外来区分
               05  SC03-NYUGAIKBN        PIC X(02)  OCCURS  2.
      *    入院診療実日数
               05  SC03-NISSU            PIC X(02).
      *    保険請求額
               05  SC03-SEIKYUGK         PIC X(09).
      *    保険請求点数
               05  SC03-TENSU            PIC X(09).
      *    一部負担額
               05  SC03-ITBFTN           PIC X(09).
      *    公費・長期区分
               05  SC03-TAHO             PIC X(10).
      *    公費請求点数
               05  SC03-EKOHTENSU        PIC X(09).
      *    公費・長期一部負担額
               05  SC03-EKOHFTNGK        PIC X(09).
      *    食事−保険請求分
               05  SC03-SKINGK           PIC X(09).
      *    食事−標準負担分
               05  SC03-SFTNGK           PIC X(09).
      *    食事−公費請求分
               05  SC03-EKOHSKINGK       PIC X(09).
      *    食事−公費標準負担分
               05  SC03-EKOHSFTNGK       PIC X(09).
      *    処方せん発行区分
               05  SC03-SYOHOKBN         PIC X(02)  OCCURS  2.
      *    処方せん発行医療機関番号
               05  SC03-SYOHONUM         PIC X(10).
      *    処方せん発行医療機関等の名称
               05  SC03-SYOHONAME        PIC X(40).
