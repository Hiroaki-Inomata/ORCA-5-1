      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        01  SC04.
      *印刷日付
            03  SC04-PRTYM                  PIC X(16).
      *医療機関コード
            03  SC04-HOSPCDN                PIC X(20).
      *保険区分
            03  SC04-HKNKBN-TBL         OCCURS  4 TIMES.
                05  SC04-HKNKBN             PIC X(02).
      *単独併用区分
            03  SC04-HEIKBN-TBL         OCCURS  3 TIMES.
                05  SC04-HEIKBN             PIC X(02).
      *本人家族区分
            03  SC04-HONKZKKBN-TBL      OCCURS  3 TIMES.
                05  SC04-HONKZKKBN          PIC X(02).
      *高齢区分
            03  SC04-KOREIKBN-TBL       OCCURS  2 TIMES.
                05  SC04-KOREIKBN           PIC X(02).
      *老人市町村番号
            03  SC04-RJNFTNJANUM            PIC X(16).
      *公費負担者番号
            03  SC04-FTNJANUM
                                            PIC X(16) OCCURS  2 TIMES.
      *老人受給者番号
            03  SC04-RJNJKYSNUM             PIC X(14).
      *公費受給者番号
            03  SC04-JKYSNUM
                                            PIC X(14) OCCURS  2 TIMES.
      *保険者番号
            03  SC04-HKNJANUM               PIC X(16).
      *被保険者証記号番号
            03  SC04-KIGO                   PIC X(34).
            03  SC04-BANGO                  PIC X(34).
      *患者番号
            03  SC04-PTNUM                  PIC X(20).
      *受給者氏名
            03  SC04-NAME                   PIC X(30).
      *生年月日
            03  SC04-BIRTHDAY               PIC X(08).
      *性別
            03  SC04-SEX-TBL            OCCURS  2 TIMES.
                05  SC04-SEX                PIC X(02).
      *生年月日：年号
            03  SC04-NENGO-TBL          OCCURS  4 TIMES.
                05  SC04-NENGO              PIC X(02).
      *特記事項
            03  SC04-TOKKI                  PIC X(08).
            03  SC04-ZAISOU                 PIC X(04).
            03  SC04-ZAIMARU                PIC X(02).
      *給付割合
            03  SC04-KYURATE
                                            PIC X(02) OCCURS  4 TIMES.
      *医療機関所在地
            03  SC04-ADRS                   PIC X(100).
      *医療機関名称
            03  SC04-HOSPNAME               PIC X(100).
      *開設者名
            03  SC04-KAISETUNAME            PIC X(80).
      *診療実日数
            03  SC04-NISSU
                                            PIC X(02) OCCURS  3 TIMES.
      *請求点数
            03  SC04-TENSU
                                            PIC X(09) OCCURS  3 TIMES.
      *一部負担金
            03  SC04-FTNMONEY
                                            PIC X(07) OCCURS  3 TIMES.
      *食事−日数
            03  SC04-SNISSU
                                            PIC X(02) OCCURS  3 TIMES.
      *食事−請求金額
            03  SC04-SKINGK
                                            PIC X(09) OCCURS  3 TIMES.
      *食事−標準負担額
            03  SC04-SFTNGK
                                            PIC X(09) OCCURS  3 TIMES.
