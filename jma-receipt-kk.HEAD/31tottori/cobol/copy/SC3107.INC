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
        01  SC07.
      *公費判別
            03  SC07-KOHKBN
                                       PIC X(02)    OCCURS  7 TIMES.
      *有効期間
            03  SC07-YUKOSTYY                       PIC X(04).
            03  SC07-YUKOSTMM                       PIC X(04).
            03  SC07-YUKOSTDD                       PIC X(04).
            03  SC07-YUKOEDYY                       PIC X(04).
            03  SC07-YUKOEDMM                       PIC X(04).
            03  SC07-YUKOEDDD                       PIC X(04).
      *市町村コード
            03  SC07-CITYNUM                        PIC X(12).
      *市町村名
            03  SC07-CITYNAME                       PIC X(18).
      *印刷日付
            03  SC07-SEIYMDG.
              05  SC07-SEIYY                        PIC X(04).
              05  SC07-SEIMM                        PIC X(04).
              05  SC07-SEIDD                        PIC X(04).
      *医療機関コード
            03  SC07-HOSPCD                         PIC X(14).
      *医療機関住所
            03  SC07-ADRS                           PIC X(100).
      *医療機関名
            03  SC07-HOSPNAME                       PIC X(100).
      *開設者名
            03  SC07-KAISETU                        PIC X(30).
      *診療年月
            03  SC07-SRYYMG.
              05  SC07-SRYYY                        PIC X(04).
              05  SC07-SRYMM                        PIC X(04).
      *入外区分
            03  SC07-NYUGAIKBN
                                       PIC X(02)    OCCURS  2 TIMES.
      *受給者番号
            03  SC07-JKYSKIGO                       PIC X(04).
            03  SC07-JKYSNUM                        PIC X(14).
      *記号・番号
            03  SC07-KIGO                           PIC X(18).
            03  SC07-NUM                            PIC X(20).
      *氏名
            03  SC07-NAME                           PIC X(24).
      *性別
            03  SC07-SEX
                                       PIC X(02)    OCCURS  2 TIMES.
      *元号
            03  SC07-GENGO
                                       PIC X(02)    OCCURS  4 TIMES.
      *誕生年
            03  SC07-TANJOYY                        PIC X(02).
      *保険種別
            03  SC07-HKNSYUBETU
                                       PIC X(02)    OCCURS  3 TIMES.
      *保険者番号
            03  SC07-HKNJANUM                       PIC X(16).
      *保険者名
            03  SC07-HKNJANAME                      PIC X(30).
      *給付割合
            03  SC07-FTNRATE
                                       PIC X(02)    OCCURS  4 TIMES.
      *入院日数
            03  SC07-NNISSU                         PIC X(02).
      *入院日
            03  SC07-NYUINYY                        PIC X(04).
            03  SC07-NYUINMM                        PIC X(04).
            03  SC07-NYUINDD                        PIC X(04).
      *退院日
            03  SC07-TAIINYY                        PIC X(04).
            03  SC07-TAIINMM                        PIC X(04).
            03  SC07-TAIINDD                        PIC X(04).
      *外来日数
            03  SC07-GNISSU                         PIC X(02).
      *総点数
            03  SC07-TENSU                          PIC X(16).
      *結予点数
            03  SC07-KESETEN                        PIC X(14).
      *薬剤一部負担金
            03  SC07-YKZFTN                         PIC X(07).
      *受給者支払い額
            03  SC07-SIHARAIGK                      PIC X(06).
      *受給者支払い回数
            03  SC07-SIHARAIKAISU                   PIC X(02).
      *受給者支払い日数
            03  SC07-SIHARAINISSU1                  PIC X(02).
            03  SC07-SIHARAINISSU2                  PIC X(02).
      *食事日数
            03  SC07-SHOKUJINISSU                   PIC X(02).
      *標準負担額
            03  SC07-SFTNGK                         PIC X(06).
      *請求額
            03  SC07-SEIKYUGK                       PIC X(09).
      *区分
            03  SC07-ETC-KBN
                                       PIC X(02)    OCCURS  10 TIMES.
