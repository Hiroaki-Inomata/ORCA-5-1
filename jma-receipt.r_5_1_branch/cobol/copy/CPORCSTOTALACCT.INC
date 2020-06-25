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
      *    診療会計請求点取得サブ　　　パラメタ　COPY (CPORCSTOTALACCT.INC)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    11/09/16  公費単独対応
      *                                     公費を最大５０に変更に併せて
      *                                     保険組合せも変更
      ************************************************************
       01  ORCSTOTALACCT-AREA.
      *
      *医療機関ＩＤ
           03  LNK-ORCSTOTALACCT-HOSPNUM               PIC 9(2).
      *入外区分
           03  LNK-ORCSTOTALACCT-NYUGAIKBN             PIC  X(01).
      *患者ＩＤ
           03  LNK-ORCSTOTALACCT-PTID                  PIC  9(10).
      *算定年月
           03  LNK-ORCSTOTALACCT-SANTEIYM              PIC  X(06).
      *処理区分（空白：点数・受診回数返却　１：点数返却　２：受診回数返却）
      *        ＜入院外の時のみ処理区分有効＞
           03  LNK-ORCSTOTALACCT-SHORIKBN              PIC  X(01).
      *
      *返却項目
      *
           03  LNK-ORCSTOTALACCT-OUT-AREA.
      *
               05  LNK-ORCSTOTALACCT-OUT-TBL           OCCURS  50.
      *保険組合せ番号
                   07  LNK-ORCSTOTALACCT-HKNCOMBINUM   PIC 9(04).
      *診療科返却テーブル
                   07  LNK-ORCSTOTALACCT-OUT-TBL2      OCCURS  50.
      *診療科
                       09  LNK-ORCSTOTALACCT-SRYKA     PIC  X(02).
      *点数
                       09  LNK-ORCSTOTALACCT-TENSU     PIC S9(08).
      *受診回数
                       09  LNK-ORCSTOTALACCT-KAISU     PIC  9(03).
      *
      *復帰コード
      *
           03  LNK-ORCSTOTALACCT-RC                    PIC S9(9) BINARY.
      *
