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
      *  01.00.01    吉川      2010/12/08   特別療養費国保対応
      ********************************************************************
        01  SKT3110.
      *医療機関コード
            03  SKT3110-HOSPCD                        PIC X(14).
      *保険者番号
            03  SKT3110-HKNJANUM                      PIC X(12).
      *医療機関住所
            03  SKT3110-ADRS                          PIC X(100).
      *医療機関名
            03  SKT3110-HOSPNAME                      PIC X(100).
      *電話番号
            03  SKT3110-TEL                           PIC X(15).
      *開設者名
            03  SKT3110-KAISETU                       PIC X(60).
      *診療年月
            03  SKT3110-SRYYM                         PIC X(16).
      *請求年月日
            03  SKT3110-SEIYMD                        PIC X(22).
      *特別療養費
            03  SKT3110-TOKUBETU                      PIC X(10).
      *ページ番号
            03  SKT3110-NO                            PIC X(04).
      *請求テーブル
            03  SKT3110-SEIKYU-TBL                    OCCURS  7 TIMES.
      *入外区分
              05  SKT3110-NYUGAI-TBL                  OCCURS  2 TIMES.
      *件数
                07  SKT3110-KENSU                     PIC X(05).
      *実日数
                07  SKT3110-NISSU                     PIC X(05).
      *点数
                07  SKT3110-TENSU                     PIC X(09).
      *薬剤一部負担金
                07  SKT3110-YKZFTN                    PIC X(07).
      *一部負担金
                07  SKT3110-ITBFTN                    PIC X(09).
      *食事ー件数
              05  SKT3110-SKENSU                      PIC X(05).
      *食事ー日数
              05  SKT3110-SNISSU                      PIC X(05).
      *食事ー金額
              05  SKT3110-SKINGK                      PIC X(09).
      *食事ー標準負担額
              05  SKT3110-SFTNGK                      PIC X(09).
      *在宅テーブル
            03  SKT3110-ZAITAKU-TBL                   OCCURS  4 TIMES.
      *在宅ー件数
              05  SKT3110-ZKENSU                      PIC X(05).
      *在宅ー日数
              05  SKT3110-ZNISSU                      PIC X(05).
      *在宅ー点数
              05  SKT3110-ZTENSU                      PIC X(09).
      *在宅ー一部負担金
              05  SKT3110-ZITBFTN                     PIC X(09).
