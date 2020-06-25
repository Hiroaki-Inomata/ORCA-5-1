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
      *    後期高齢者請求総括表 （山形県）     COPY (SKT0605.INC) 
      *
      *  作成日付    作業者        記述
      *  08/05/08    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/03/12  点数表区分追加
      *  01.00.02    吉川    2012/05/10  帳票様式の変更
      *  01.00.03    井上    2018/10/12  修正履歴コメント削除
      ********************************************************************
       01  SKT0605.
      *    請求年月
           03  SKT0605-SRYYM               PIC X(16).
      *    医療機関住所
           03  SKT0605-ADRS                PIC X(100).
      *    医療機関名称
           03  SKT0605-HOSPNAME            PIC X(100).
      *    開設者名
           03  SKT0605-KAISETUNAME         PIC X(40).
      *    医療機関コード
           03  SKT0605-HOSPCD              PIC X(14).
      *    点数表区分
           03  SKT0605-TENHYOKBN           PIC X(02) OCCURS 4.
      *    データテーブル
           03  SKT0605-DATA-TBL   OCCURS 27.
      *    保険者名
               05  SKT0605-HKNJANAME       PIC X(20).
      *    請求区分
               05  SKT0605-KAMIBUN         PIC X(12).
      *    入外テーブル
               05  SKT0605-NYUGAI-TBL   OCCURS 2.
      *    件数
                   07  SKT0605-KENSU       PIC X(07).
      *    日数
                   07  SKT0605-NISSU       PIC X(07).
      *    点数
                   07  SKT0605-TENSU       PIC X(14).
