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
        01  SC09.
      *公費区分
            03  SC09-KOHKBN
                                        PIC X(02)    OCCURS  7 TIMES.
      *有効期間
            03  SC09-YUKOSTYY                        PIC X(04).
            03  SC09-YUKOSTMM                        PIC X(04).
            03  SC09-YUKOSTDD                        PIC X(04).
            03  SC09-YUKOEDYY                        PIC X(04).
            03  SC09-YUKOEDMM                        PIC X(04).
            03  SC09-YUKOEDDD                        PIC X(04).
      *市町村コード
            03  SC09-CITYNUM                         PIC X(12).
      *市町村名
            03  SC09-CITYNAME                        PIC X(18).
      *印刷日付
            03  SC09-SEIYMDG.
              05  SC09-SEIYY                         PIC X(04).
              05  SC09-SEIMM                         PIC X(04).
              05  SC09-SEIDD                         PIC X(04).
      *医療機関コード
            03  SC09-HOSPCD                          PIC X(14).
      *医療機関住所
            03  SC09-ADRS                            PIC X(100).
      *医療機関名
            03  SC09-HOSPNAME                        PIC X(100).
      *開設者名
            03  SC09-KAISETU                         PIC X(30).
      *診療年月
            03  SC09-SRYYMG.
              05  SC09-SRYYY                         PIC X(04).
              05  SC09-SRYMM                         PIC X(04).
      *入外区分
            03  SC09-NYUGAIKBN
                                        PIC X(02)    OCCURS  2 TIMES.
      *受給者番号
            03  SC09-JKYSKIGO                        PIC X(04).
            03  SC09-JKYSNUM                         PIC X(14).
      *記号・番号
            03  SC09-KIGO                            PIC X(12).
            03  SC09-NUM                             PIC X(20).
      *氏名
            03  SC09-NAME                            PIC X(24).
      *性別
            03  SC09-SEX
                                        PIC X(02)    OCCURS  2 TIMES.
      *元号
            03  SC09-GENGO
                                        PIC X(02)    OCCURS  4 TIMES.
      *誕生年
            03  SC09-TANJOYY                         PIC X(02).
      *保険種別
            03  SC09-HKNSYUBETU
                                        PIC X(02)    OCCURS  4 TIMES.
      *保険者番号
            03  SC09-HKNJANUM                        PIC X(16).
      *保険者名
            03  SC09-HKNJANAME                       PIC X(30).
      *給付割合
            03  SC09-FTNRATE
                                        PIC X(02)    OCCURS  4 TIMES.
      *入院日数
            03  SC09-NNISSU                          PIC X(02).
      *入院日
            03  SC09-NYUINYY                         PIC X(04).
            03  SC09-NYUINMM                         PIC X(04).
            03  SC09-NYUINDD                         PIC X(04).
      *退院日
            03  SC09-TAIINYY                         PIC X(04).
            03  SC09-TAIINMM                         PIC X(04).
            03  SC09-TAIINDD                         PIC X(04).
      *外来日数
            03  SC09-GNISSU                          PIC X(02).
      *総点数
            03  SC09-TENSU                           PIC X(16).
      *結予点数
            03  SC09-KESETEN                         PIC X(14).
      *薬剤一部負担金
            03  SC09-YKZFTN                          PIC X(07).
      *受給者支払い額
            03  SC09-SIHARAIGK                       PIC X(06).
      *受給者支払い回数
            03  SC09-SIHARAIKAISU                    PIC X(02).
      *受給者支払い日数
            03  SC09-SIHARAINISSU1                   PIC X(02).
            03  SC09-SIHARAINISSU2                   PIC X(02).
      *食事日数
            03  SC09-SHOKUJINISSU                    PIC X(02).
      *標準負担額
            03  SC09-SFTNGK                          PIC X(06).
      *請求額
            03  SC09-SEIKYUGK                        PIC X(09).
      *区分
            03  SC09-ETC-KBN
                                        PIC X(02)    OCCURS  10 TIMES.
