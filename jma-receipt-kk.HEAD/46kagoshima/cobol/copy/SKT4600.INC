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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    11/03/04    特別療養費国保対応
      *  01.00.02    吉川    12/01/16    点数欄のバイト数増加
      *  01.00.02    門間    19/02/04    修正履歴コメント削除
      ********************************************************************
       01  SKT4600.
      *診療年月
           03  SKT4600-SRYYM               PIC  X(16).
      *県内・県外
           03  SKT4600-KENNNAI-GAI         PIC  X(02) OCCURS  2.
      *標榜科
           03  SKT4600-SRYKANAME           PIC  X(30) OCCURS 4.
      *医療機関コード
           03  SKT4600-HOSPCD              PIC  X(07).
      *郵便番号
           03  SKT4600-POST                PIC  X(18).
      *医療機関住所
           03  SKT4600-HOSPADRS            PIC  X(100).
      *医療機関名称
           03  SKT4600-HOSPNAME            PIC  X(100).
      *開設者名
           03  SKT4600-KAISETU             PIC  X(40).
      *電話番号
           03  SKT4600-TEL                 PIC  X(15).
      *請求保険者数（総合計）
           03  SKT4600-HKNSUUG             PIC  X(05).
      *請求書枚数（総合計）
           03  SKT4600-SKYSUUG             PIC  X(05).
      *明細書枚数（総合計）
           03  SKT4600-MEISAISUUG          PIC  X(05).
      *請求保険者数
           03  SKT4600-HKNSUU              PIC  X(05).
      *請求書枚数
           03  SKT4600-SKYSUU              PIC  X(05).
      *明細書枚数
           03  SKT4600-MEISAISUU           PIC  X(05).
      *請求保険者数（後期高齢）
           03  SKT4600-HKNSUUK             PIC  X(05).
      *請求書枚数（後期高齢）
           03  SKT4600-SKYSUUK             PIC  X(05).
      *明細書枚数（後期高齢）
           03  SKT4600-MEISAISUUK          PIC  X(05).
      *特別療養費
           03  SKT4600-TOKUBETU            PIC  X(10).
      *療養の給付テーブル
           03  SKT4600-SEIKYU  OCCURS  13.
               05  SKT4600-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT4600-KENSU        PIC  X(05).
      *点数
                   07 SKT4600-TENSU        PIC  X(10).
      *薬剤一部負担金
      *             07 SKT4600-YKZFTN       PIC  X(07).
      *一部負担金
                   07 SKT4600-ITBFTN       PIC  X(09).
      *食事療養費テーブル
           03  SKT4600-SEIKYU-S  OCCURS  11.
      *食事件数
               05  SKT4600-SKENSU          PIC  X(05).
      *食事療養費
               05  SKT4600-SKINGK          PIC  X(09).
      *標準負担額
               05  SKT4600-SFTNGK          PIC  X(09).
