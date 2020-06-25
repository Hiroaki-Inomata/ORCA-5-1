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
      *  後期高齢者総括表（熊本県）     COPY (SKT4305.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/18    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         12/03/14  点数と食事療養費の桁数増加
      *  01.00.02    門間         18/09/05  不要コメントを削除
      ********************************************************************
       01  SKT4305.
      *    診療年月
           03  SKT4305-SRYYM               PIC  X(16).
      *    郵便番号
           03  SKT4305-POST.
               05  SKT4305-POST-OCC        PIC  X(02) OCCURS 7.
      *    標榜科名
           03  SKT4305-SRYKANAME           PIC  X(16) OCCURS 2.
      *    NO
           03  SKT4305-NO                  PIC  X(04).
      *    医療機関所在地
           03  SKT4305-ADRS                PIC  X(100).
      *    医療機関名称
           03  SKT4305-HOSPNAME            PIC  X(100).
      *    電話番号
           03  SKT4305-TEL                 PIC  X(15).
      *    開設者氏名
           03  SKT4305-KAISETUNAME         PIC  X(40).
      *    医療機関コード
           03  SKT4305-HOSPCD              PIC  X(40).
      *    請求一覧
           03  SKT4305-SEIKYU-TBL  OCCURS 8.
      *    市町村名
               05  SKT4305-HKNJANAME       PIC  X(20).
      *    保険テーブル
               05  SKT4305-NYUGAI-TBL   OCCURS  2.
      *    入外テーブル
                   07  SKT4305-KYURATE-TBL  OCCURS 2.
      *    件数
                       09  SKT4305-KENSU   PIC  X(04).
      *    点数
                       09  SKT4305-TENSU   PIC  X(08).
      *    食事件数
               05  SKT4305-S-KENSU         PIC  X(04).
      *    食事療養費
               05  SKT4305-S-KINGK         PIC  X(08).
      *    標準負担額
               05  SKT4305-S-FTNGK         PIC  X(07).
      *    合計エリア
           03  SKT4305-GOKEI-TBL.
               05  SKT4305-GNYUGAI-TBL   OCCURS  2.
      *    入外テーブル
                   07  SKT4305-GKYURATE-TBL OCCURS 2.
      *    件数
                       09  SKT4305-GKENSU  PIC  X(04).
      *    点数
                       09  SKT4305-GTENSU  PIC  X(08).
      *    食事件数
               05  SKT4305-S-GKENSU        PIC  X(04).
      *    食事療養費
               05  SKT4305-S-GKINGK        PIC  X(08).
      *    標準負担額
               05  SKT4305-S-GFTNGK        PIC  X(07).
      *    公費エリア
           03  SKT4305-KOUHI-TBL  OCCURS 6.
      *    公費：法別番号
               05  SKT4305-HBTNUM          PIC X(02).
      *
               05  SKT4305-KNYUGAI-TBL  OCCURS 2.
      *    公費：件数
                   07  SKT4305-KKENSU      PIC X(04).
      *    公費：点数
                   07  SKT4305-KTENSU      PIC X(08).
      *    公費：食事件数
               05  SKT4305-S-KKENSU        PIC X(04).
      *    公費：食事療養費
               05  SKT4305-S-KKINGK        PIC X(08).
      *    公費：標準負担額
               05  SKT4305-S-KFTNGK        PIC X(07).
      *    総件数
           03  SKT4305-SOUKENSU            PIC X(07).
