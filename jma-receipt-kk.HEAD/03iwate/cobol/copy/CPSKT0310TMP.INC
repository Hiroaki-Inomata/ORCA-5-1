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
      *    診療報酬請求書     COPY (CPSKT0310TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    門間         08/10/09  公費名称追加
      *  01.00.02    井上         18/10/11  修正履歴コメント削除
      ************************************************************
       01  SKT0310TMP-REC.
      *  KEY
           03  SKT0310TMP-KEY.
      *    出力順
               05  SKT0310TMP-SYUTURYOKU           PIC  9(01).
      *    保険者番号
               05  SKT0310TMP-HKNJANUM             PIC  X(06).
      *    岩手宮城内陸地震減免
               05  SKT0310TMP-GENMEN               PIC  X(05).
      *  請求テーブル
           03  SKT0310TMP-SEIKYU-TBL     OCCURS  13.
      *      入外テーブル
               05  SKT0310TMP-NYUGAI-TBL  OCCURS  2.
      *         件数
                   07  SKT0310TMP-KENSU            PIC  9(06).
      *         日数
                   07  SKT0310TMP-NISSU            PIC  9(06).
      *         点数
                   07  SKT0310TMP-TENSU            PIC  9(10).
      *         薬剤一部負担金
                   07  SKT0310TMP-YKZFTN           PIC  9(10).
      *         一部負担金額
                   07  SKT0310TMP-ITBFTN           PIC  9(10).
      *         長期
                   07  SKT0310TMP-TYOKI            PIC  9(02).
      *      食事ー件数
               05  SKT0310TMP-S-KENSU              PIC  9(06).
      *      食事ー日数
               05  SKT0310TMP-S-NISSU              PIC  9(06).
      *      食事ー金額
               05  SKT0310TMP-S-KINGAKU            PIC  9(10).
      *      食事ー標準負担額
               05  SKT0310TMP-S-FTNGAKU            PIC  9(10).
      *  在宅テーブル
           03  SKT0310TMP-ZAI-TBL     OCCURS  6.
      *      在宅ー件数
               05  SKT0310TMP-ZKENSU               PIC  9(06).
      *      在宅ー日数
               05  SKT0310TMP-ZNISSU               PIC  9(06).
      *      在宅ー点数
               05  SKT0310TMP-ZTENSU               PIC  9(10).
      *      在宅ー薬剤一部負担金
               05  SKT0310TMP-ZYKZFTN              PIC  9(10).
      *      在宅ー一部負担金額
               05  SKT0310TMP-ZITBFTN              PIC  9(10).
      *      在宅ー長期
               05  SKT0310TMP-ZTYOKI               PIC  9(02).
      *  公費テーブル
           03  SKT0310TMP-KOH-TBL      OCCURS  20.
      *      公費番号
               05  SKT0310TMP-KOHNUM               PIC  X(03).
               05  SKT0310TMP-KOHNAME              PIC  X(10).
      *      入外テーブル
               05  SKT0310TMP-R-NYUGAI-TBL  OCCURS  2.
      *          公費ー件数
                   07  SKT0310TMP-KKENSU           PIC  9(06).
      *          公費ー日数
                   07  SKT0310TMP-KNISSU           PIC  9(06).
      *          公費ー点数
                   07  SKT0310TMP-KTENSU           PIC  9(10).
      *          公費ー薬剤一部負担金
                   07  SKT0310TMP-KYKZFTN          PIC  9(10).
      *          公費ー一部負担金額
                   07  SKT0310TMP-KITBFTN          PIC  9(10).
      *      公費ー食事件数
               05  SKT0310TMP-S-KKENSU             PIC  9(06).
      *      公費ー食事日数
               05  SKT0310TMP-S-KNISSU             PIC  9(06).
      *      公費ー食事金額
               05  SKT0310TMP-S-KKINGAKU           PIC  9(10).
      *      公費ー食事標準負担額
               05  SKT0310TMP-S-KFTNGAKU           PIC  9(10).
      *  給付割合
           03  SKT0310TMP-KYURATE1                 PIC  9(02).
           03  SKT0310TMP-KYURATE2                 PIC  9(02).
