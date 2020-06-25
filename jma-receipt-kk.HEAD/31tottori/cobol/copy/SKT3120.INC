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
      *  01.00.01    長屋      2008/05/03   帳票様式に伴う修正
      *  01.00.01    吉川      2010/12/08   特別療養費国保対応
      ********************************************************************
        01  SKT3120.
      *医療機関コード
            03  SKT3120-HOSPCD                        PIC X(14).
      *保険者番号
            03  SKT3120-HKNJANUM                      PIC X(12).
      *医療機関住所
            03  SKT3120-ADRS                          PIC X(100).
      *医療機関名
            03  SKT3120-HOSPNAME                      PIC X(100).
      *電話番号
            03  SKT3120-TEL                           PIC X(15).
      *開設者名
            03  SKT3120-KAISETU                       PIC X(60).
      *診療年月
            03  SKT3120-SRYYM                         PIC X(16).
      *請求年月日
            03  SKT3120-SEIYMD                        PIC X(22).
      *特別療養費
            03  SKT3120-TOKUBETU                      PIC X(10).
      *ページ番号
            03  SKT3120-NO                            PIC X(04).
      *請求テーブル
            03  SKT3120-SEIKYU-TBL                    OCCURS  4 TIMES.
      *入外区分
              05  SKT3120-NYUGAI-TBL                  OCCURS  2 TIMES.
      *件数
                07  SKT3120-KENSU                     PIC X(05).
      *実日数
                07  SKT3120-NISSU                     PIC X(05).
      *点数
                07  SKT3120-TENSU                     PIC X(09).
      *薬剤一部負担金
                07  SKT3120-YKZFTN                    PIC X(07).
      *一部負担金
                07  SKT3120-ITBFTN                    PIC X(09).
      *食事ー件数
              05  SKT3120-SKENSU                      PIC X(05).
      *食事ー日数
              05  SKT3120-SNISSU                      PIC X(05).
      *食事ー金額
              05  SKT3120-SKINGK                      PIC X(09).
      *食事ー標準負担額
              05  SKT3120-SFTNGK                      PIC X(09).
      *原爆件数
            03  SKT3120-GENBKKEN                      PIC X(05).
      *特別公費テーブル
            03  SKT3120-KOUHI-TBL                     OCCURS  6 TIMES.
      *公費番号
              05  SKT3120-KOHNUM                      PIC X(02).
      *公費名
              05  SKT3120-KOHNAME                     PIC X(08).
      *特別公費ー入外区分
              05  SKT3120-KNYUGAI-TBL                 OCCURS  2 TIMES.
      *特別公費ー件数
                07  SKT3120-KKENSU                    PIC X(05).
      *特別公費ー日数
                07  SKT3120-KNISSU                    PIC X(05).
      *特別公費ー件数
                07  SKT3120-KTENSU                    PIC X(09).
      *特別公費ー薬剤一部負担金
                07  SKT3120-KYKZFTN                   PIC X(07).
      *特別公費ー一部負担金
                07  SKT3120-KITBFTN                   PIC X(09).
      *特別公費ー食事件数
              05  SKT3120-K-SKENSU                    PIC X(05).
      *特別公費ー食事日数
              05  SKT3120-K-SNISSU                    PIC X(05).
      *特別公費ー食事金額
              05  SKT3120-K-SKINGK                    PIC X(09).
      *特別公費ー食事標準負担額
              05  SKT3120-K-SFTNGK                    PIC X(09).
      *在宅テーブル
            03  SKT3120-ZAITAKU-TBL                   OCCURS  2 TIMES.
      *在宅ー件数
              05  SKT3120-ZKENSU                      PIC X(05).
      *在宅ー日数
              05  SKT3120-ZNISSU                      PIC X(05).
      *在宅ー点数
              05  SKT3120-ZTENSU                      PIC X(09).
      *在宅ー一部負担金
              05  SKT3120-ZITBFTN                     PIC X(09).
