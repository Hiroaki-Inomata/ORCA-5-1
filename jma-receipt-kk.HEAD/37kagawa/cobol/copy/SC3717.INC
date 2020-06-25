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
        01  SC17.
      *請求分の年月
            03  SC17-SRYYM                      PIC X(16).
      *請求書作成日
            03  SC17-SEIYMD                     PIC X(22).
      *医療機関コード
            03  SC17-HOSPCD                     PIC X(20).
      *医療機関名称
            03  SC17-HOSPNAME                   PIC X(60).
      *医療機関所在地
            03  SC17-ADRS                       PIC X(60).
      *医療機関電話番号
            03  SC17-TEL                        PIC X(15).
      *代表者名
            03  SC17-KAISETU                    PIC X(60).
      *銀行名
            03  SC17-GINKOU                     PIC X(20).
      *支店名
            03  SC17-SITEN                      PIC X(20).
      *口座区分
            03  SC17-KOUZAKBN                   PIC X(02)
                                                OCCURS 2 TIMES.
      *口座番号
            03  SC17-KOUZANUM                   PIC X(10).
      *公費区分
            03  SC17-KOHKBN
                                                PIC X(02)
                                                OCCURS  4 TIMES.
      *内訳書テーブル
            03  SC17-SEIKYU-TBL         OCCURS  10 TIMES.
      *受給者番号
                05  SC17-JKYSNUM                PIC X(16).
      *受給者氏名
                05  SC17-NAME                   PIC X(32).
      *生年月日
                05  SC17-TANJOYMD.
      *生年
                    07  SC17-TANJOYY            PIC X(03).
      *生月
                    07  SC17-TANJOMM            PIC X(02).
      *生日
                    07  SC17-TANJODD            PIC X(02).
      *診療科
                05  SC17-SRYKA                  PIC X(08).
      *保険種別
                05  SC17-HKNSYUBETU
                                                PIC X(02)
                                                OCCURS  4 TIMES.
      *入院年月日
                05  SC17-NYUINYMD               PIC X(09).
      *入院継続
                05  SC17-KEIZOKU                PIC X(02).
      *入院日数
                05  SC17-NISSU                  PIC X(02).
      *点数
                05  SC17-TENSU                  PIC X(09).
      *他法
                05  SC17-TAHO                   PIC X(07).
      *医療費請求額
                05  SC17-SEIKYUGK               PIC X(09).
      *小計
            03  SC17-SKENSU                     PIC X(03).
      *合計
            03  SC17-GKENSU                     PIC X(03).
      *点数小計
            03  SC17-STENSU                     PIC X(09).
      *点数合計
            03  SC17-GTENSU                     PIC X(09).
      *他法小計
            03  SC17-STAHO                      PIC X(09).
      *他法合計
            03  SC17-GTAHO                      PIC X(09).
      *請求金額小計
            03  SC17-SSEIKYUGK                  PIC X(09).
      *請求金額合計
            03  SC17-GSEIKYUGK                  PIC X(09).
      *総枚数
            03  SC17-GPAGE                      PIC X(03).
      *内枚数
            03  SC17-PAGE                       PIC X(03).
      *合計請求額
            03  SC17-GKSKGK                     PIC X(09).
