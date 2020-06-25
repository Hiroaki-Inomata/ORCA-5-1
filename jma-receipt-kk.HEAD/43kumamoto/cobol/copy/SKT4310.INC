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
      *  01.00.01    吉川         11/02/10  特別療養費国保対応
      *  01.00.02    門間         18/09/05  不要コメントを削除
      ********************************************************************
       01  SKT4310.
      *診療年月
           03  SKT4310-SRYYM               PIC  X(16).
      *郵便番号
           03  SKT4310-POST.
               05  SKT4310-POST-OCC        PIC  X(02) OCCURS 7.
      *標榜科名
           03  SKT4310-SRYKANAME           PIC  X(16) OCCURS 2.
      *NO
           03  SKT4310-NO                  PIC  X(03).
      *医療機関所在地
           03  SKT4310-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT4310-HOSPNAME            PIC  X(100).
      *電話番号
           03  SKT4310-TEL                 PIC  X(15).
      *開設者氏名
           03  SKT4310-KAISETU             PIC  X(30).
      *医療機関コード
           03  SKT4310-HOSPCDN             PIC  X(20).
      *特別療養費
           03  SKT4310-TOKUBETU            PIC  X(10).
      *請求一覧
           03  SKT4310-SEIKYU  OCCURS 8.
      *保険者名
               05  SKT4310-HKNJANAME       PIC  X(20).
      *給付割合
               05  SKT4310-KYURATE         PIC  X(06).
      *保険テーブル
               05  SKT4310-HKN-TBL   OCCURS  5.
      *入外テーブル
                   07  SKT4310-NYUGAIKBN OCCURS 2.
      *件数
                       09  SKT4310-KENSU   PIC  X(04).
      *点数
                       09  SKT4310-TENSU   PIC  X(07).
      *食事療養費
                   07  SKT4310-SKINGK      PIC  X(07).
      *標準負担額
                   07  SKT4310-SFTNGK      PIC  X(07).
      *合計エリア
           03  SKT4310-GOKEI-AREA.
               05  SKT4310-G-HKN-TBL   OCCURS  5.
      *入外テーブル
                   07  SKT4310-G-NYUGAIKBN OCCURS 2.
      *件数
                       09  SKT4310-G-KENSU   PIC  X(04).
      *点数
                       09  SKT4310-G-TENSU   PIC  X(07).
      *食事療養費
                   07  SKT4310-G-SKINGK      PIC  X(07).
      *標準負担額
                   07  SKT4310-G-SFTNGK      PIC  X(07).
      *公費エリア
           03  SKT4310-KOHI-TBL  OCCURS 6.
      *公費：法別番号
               05  SKT4310-HBTNUM            PIC X(02).
      *
               05  SKT4310-KNYUGAIKBN  OCCURS 2.
      *公費：件数
                   07  SKT4310-K-KENSU       PIC X(04).
      *公費：点数
                   07  SKT4310-K-TENSU       PIC X(07).
      *公費：食事療養費
               05  SKT4310-K-SKINGK          PIC X(07).
      *公費：標準負担額
               05  SKT4310-K-SFTNGK          PIC X(07).
