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
      *          医療費請求書 COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者     日付      内容
      * 01.00.01     門間       19/01/10  修正履歴コメント削除
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *公費−負担者番号
               05  MF100-FTNJANUM         PIC  X(05).
               05  MF100-FTNJANUM2        PIC  X(05).
      *国保・社保区分
               05  MF100-KBN              PIC  X(01).
      *県外国保の２枚目を表すフラグ
               05  MF100-KENGAI           PIC  9(01).
      *請求テーブル
           03  MF100-SEIKYU-TBL OCCURS 3.
      *給付割合
               05  MF100-KYURATE          PIC  9(02).
      *公費明細テーブル
               05  MF100-KOUOHI-TBL OCCURS 3.
      *件数
                   07  MF100-KENSU            PIC  9(04).
      *日数
                   07  MF100-NISSU            PIC  9(04).
      *福祉医療費点数
                   07  MF100-TENSU            PIC  9(08).
      *乳・障・母給付外点数
                   07  MF100-KYUFUGAI         PIC  9(08).
      *薬剤分
                   07  MF100-YKZFTN           PIC  9(08).
      *一部負担分
                   07  MF100-ITBFTN           PIC  9(07).
      *合計
                   07  MF100-GOKEI            PIC  9(07).
