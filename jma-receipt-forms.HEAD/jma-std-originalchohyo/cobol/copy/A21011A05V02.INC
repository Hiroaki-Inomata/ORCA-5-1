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
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      ********************************************************************
       01  A05.
      *    ページ数
              03  A05-PAGE          PIC  X(03).
      *    対象年月
              03  A05-SKYYM         PIC  X(18).
      *    作成年月日
              03  A05-SYSYMD        PIC  X(22).
      *    エラーメッセージ
              03  A05-JOBMSG        PIC  X(60).
      *    出力区分
              03  A05-KBN           PIC  X(20).
      *    患者番号
              03  A05-PTNUM-TBL.
                  05  A05-PTNUM     PIC  X(10) OCCURS 20.
                  05  A05-PTNUM2    PIC  X(20) OCCURS 20.
      *    患者氏名
              03  A05-NAME-TBL.
                  05  A05-NAME      PIC  X(30) OCCURS 20.
      *    保険者番号
              03  A05-HKNJANUM-TBL.
                  05  A05-HKNJANUM  PIC  X(16) OCCURS 20.
      *    入外区分
              03  A05-NYUGAI-TBL.
                  05  A05-NYUGAI    PIC  X(04) OCCURS 20.
      *    本人・家族区分
              03  A05-HONKZKKBN-TBL.
                  05  A05-HONKZKKBN PIC  X(02) OCCURS 20.
      *    保険番号・老人保険番号
              03  A05-HKNNUM-TBL.
                  05  A05-HKN-RJN-TBL OCCURS 20.
                      07  A05-HKNNUM        PIC  X(06).
                      07  A05-RJNHKNNUM     PIC  X(06).
      *    給付割合
              03  A05-KYURATE-TBL.
                  05  A05-KYURATE   PIC  X(06) OCCURS 20.
      *    公費番号１
              03  A05-KOHNUM1-TBL.
                  05  A05-KOHNUM1   PIC  X(06) OCCURS 20.
      *    公費番号２
              03  A05-KOHNUM2-TBL.
                  05  A05-KOHNUM2   PIC  X(06) OCCURS 20.
      *    診療実日数
              03  A05-NISSU-TBL.
                  05  A05-NISSU     PIC  X(08) OCCURS 20.
      *    点数
              03  A05-TENSU-TBL.
                  05  A05-TENSU     PIC  X(18) OCCURS 20.
      *    薬剤一部負担金
              03  A05-YKZFTN-TBL.
                  05  A05-YKZFTN    PIC  X(14) OCCURS 20.
      *    一部負担金
              03  A05-ITBFTN-TBL.
                  05  A05-ITBFTN    PIC  X(14) OCCURS 20.
      *    食事日数
              03  A05-SNISSU-TBL.
                  05  A05-SNISSU    PIC  X(08) OCCURS 20.
      *    食事金額
              03  A05-SKINGK-TBL.
                  05  A05-SKINGK    PIC  X(18) OCCURS 20.
      *    食事標準負担額
              03  A05-SFTNGK-TBL.
                  05  A05-SFTNGK    PIC  X(14) OCCURS 20.
      *    診療年月
              03  A05-SRYYM-TBL.
                  05  A05-SRYYM     PIC  X(16) OCCURS 20.
