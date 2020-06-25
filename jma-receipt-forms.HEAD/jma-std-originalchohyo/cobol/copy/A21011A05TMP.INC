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
      *                           11/01/13  月途中での本家区分変更へ対応
      * 01.00.02     @@@@-門間    11/10/18  月途中での負担者番号変更へ対応
      * 01.00.03     @@@@-門間    16/03/11  一時ファイルに特記事項を追加（07老併、08老健）
      * 01.00.04     @@@@-門間    16/05/16  一時ファイルに適用開始年月日を追加
      ********************************************************************
      * 01  A05TMP-REC.
           03  A05TMP-KEY.
      *出力区分
               05  A05TMP-SYUTURYOKU      PIC  X(01).
      *患者番号
               05  A05TMP-PTNUM           PIC  X(20).
      *入外区分
               05  A05TMP-NYUGAIKBN       PIC  X(01).
      *給付割合
               05  A05TMP-KYURATE         PIC  9(03).
      *診療年月
               05  A05TMP-SRYYM           PIC  X(06).
      *保険者番号
               05  A05TMP-HKNJANUM        PIC  X(08).
      *本人・家族区分
               05  A05TMP-HONKZKKBN       PIC  X(01).
      *負担者番号
               05  A05TMP-KOHFTNNUM1      PIC  X(08).
               05  A05TMP-KOHFTNNUM2      PIC  X(08).
      *特記事項（07老併、08老健）
               05  A05TMP-TOKKI           PIC  X(02).
      *適用開始年月日
               05  A05TMP-TEKSTYMD        PIC  X(08).
      *患者氏名
           03  A05TMP-PTNAME              PIC  X(30).
      *主保険番号
           03  A05TMP-HKNNUM              PIC  X(03).
      *老人保険番号
           03  A05TMP-RJNHKNNUM           PIC  X(03).
      *公費番号１
           03  A05TMP-KOHNUM1             PIC  X(03).
      *公費番号２
           03  A05TMP-KOHNUM2             PIC  X(03).
      *日数
           03  A05TMP-NISSU               PIC  9(02).
      *点数
           03  A05TMP-TENSU               PIC  9(08).
      *薬剤一部負担金
           03  A05TMP-YKZFTN              PIC  9(05).
      *一部負担金
           03  A05TMP-ITBFTN              PIC  9(09).
      *食事日数
           03  A05TMP-SNISSU              PIC  9(02).
      *食事療養費
           03  A05TMP-SKINGK              PIC  9(09).
      *食事負担額
           03  A05TMP-SFTNGK              PIC  9(09).
