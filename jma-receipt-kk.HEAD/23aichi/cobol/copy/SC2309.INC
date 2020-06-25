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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者    日付         内容
      *  01.00.01    萩野      2020/02/12   修正履歴コメント削除
      ********************************************************************
       01  SC09.
      *請求年月日
              03  SC09-SEIYMD           PIC  X(22).
      *表示年月
              03  SC09-PRTYM            PIC  X(16).
      *住所
              03  SC09-ADRS             PIC  X(100).
      *医療機関名
              03  SC09-HOSPNAME         PIC  X(100).
      *管理者名
              03  SC09-KAISETUNAME      PIC  X(40).
      *電話番号
              03  SC09-TEL              PIC  X(15).
      *医療機関コード
              03  SC09-HOSPCD           PIC  X(14).
      *合計金額
              03  SC09-GKINGAKU         PIC  X(22).
      *総件数
              03  SC09-GKENSU           PIC  X(10).
      *総ページ数
              03  SC09-GPAGE            PIC  X(06).
      *ページ数
              03  SC09-PAGE             PIC  X(06).
      *市町村番号
              03  SC09-CITYNAME         PIC  X(20).
      *診療テーブル
              03  SC09-SRYTBL           OCCURS  25.
      *受給者番号
                  05  SC09-JKYSNUM      PIC  X(20).
      *受診者氏名
                  05  SC09-NAME         PIC  X(20).
      *総点数
                  05  SC09-TENSU        PIC  X(07).
      *精神・結核点数
                  05  SC09-KESE         PIC  X(07).
      *請求割合
                  05  SC09-KYURATE1     PIC  X(02).
                  05  SC09-KYURATE2     PIC  X(02).
      *市町村負担額
                  05  SC09-KOHFTN       PIC  X(09).
      *合計テーブル
              03  SC09-SUM-TBL.
      *合計：件数
                  05  SC09-SKENSU       PIC  X(02).
      *合計：点数
                  05  SC09-GTENSU       PIC  X(09).
      *合計：精神・結核点数
                  05  SC09-GKESE        PIC  X(09).
      *合計：市町村負担額
                  05  SC09-GKOHFTN      PIC  X(09).
      *備考欄
              03  SC09-BIKO-TBL OCCURS 25.
                  05  SC09-BIKO1        PIC  X(18).
                  05  SC09-BIKO2        PIC  X(18).
                  05  SC09-BIKO3        PIC  X(18).
