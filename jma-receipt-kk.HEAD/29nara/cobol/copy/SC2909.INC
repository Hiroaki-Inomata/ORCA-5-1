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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    萩野         19/04/25  修正履歴コメント削除
      *                                     診療年月に元号を追加
      ************************************************************
        01  SC09.
      *対象年月
            03  SC09-PRTYM                           PIC X(16).
      *請求年月日
            03  SC09-SEIYMD                          PIC X(22).
      *医療機関コード
            03  SC09-HOSPCD                          PIC X(07).
      *医療機関住所
            03  SC09-ADRS                            PIC X(100).
      *医療機関名
            03  SC09-HOSPNAME                        PIC X(100).
      *開設者名
            03  SC09-KAISETUNAME                     PIC X(40).
      *電話番号
            03  SC09-TEL                             PIC X(15).
      *保険区分
            03  SC09-HKNKBN
                                        PIC X(02)    OCCURS  2 TIMES.
      *点数表区分
            03  SC09-TENSUHYO
                                        PIC X(02)    OCCURS  5 TIMES.
      *明細テーブル
            03  SC09-MEISAI-TBL                      OCCURS  12 TIMES.
      *負担者番号
                05  SC09-FTNJANUM1                   PIC X(04).
                05  SC09-FTNJANUM2                   PIC X(08).
      *受給者番号
                05  SC09-JKYSNUM                     PIC X(14).
      *保険者番号
                05  SC09-HKNJANUM                    PIC X(16).
      *氏名
                05  SC09-NAME                        PIC X(24).
      *生年月日
                05  SC09-TANJOYMD.
                    07  SC09-TANJOGG                 PIC X(02).
                    07  SC09-TANJOYY                 PIC X(04).
                    07  SC09-TANJOMM                 PIC X(04).
                    07  SC09-TANJODD                 PIC X(04).
      *入外区分
                05  SC09-NYUGAIKBN
                                        PIC X(02)    OCCURS  2 TIMES.
      *給付割合
                05  SC09-KYURATE                     PIC X(02).
      *日数
                05  SC09-NISSU                       PIC X(02).
      *点数
                05  SC09-TENSU                       PIC X(07).
      *一部負担金
                05  SC09-ITBFTN                      PIC X(07).
      *長期
                05  SC09-TYOKI                       PIC X(02).
      *診療年月
                05  SC09-SRYYM.
                    07  SC09-SRYG                    PIC X(02).
                    07  SC09-SRYY                    PIC X(04).
                    07  SC09-SRYM                    PIC X(04).
      *備考
                05  SC09-BIKO                        PIC X(10).
      *合計テーブル
            03  SC09-GOKEI-AREA.
      *合計ー点数
                05  SC09-GTENSU                      PIC X(09).
      *合計ー一部負担金
                05  SC09-GITBFTN                     PIC X(09).
      *ページ数
            03  SC09-PAGE                            PIC X(03).
      *総ページ数
            03  SC09-GPAGE                           PIC X(03).
