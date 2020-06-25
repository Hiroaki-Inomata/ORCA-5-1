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
      *    お薬手帳データ連携      COPY (CPOKUSURIKANRI.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  OKUSURI-KANRI-REC.
      *キー
           03  OKUKAN-KEY.
      *医療機関ＩＤ
               05  OKUKAN-HOSPNUM           PIC  9(02).
      *患者ＩＤ
               05  OKUKAN-PTID              PIC  9(10).
      *調剤日
               05  OKUKAN-SRYYMD            PIC  X(08).
      *医療機関コード
               05  OKUKAN-HOSPCD            PIC  X(07).
      *連番
               05  OKUKAN-RENNUM            PIC  9(03).
      *患者氏名
           03  OKUKAN-NAME                  PIC  X(100).
      *性別
           03  OKUKAN-SEX                   PIC  X(1).
      *生年月日
           03  OKUKAN-BIRTHDAY              PIC  X(8).
      *医療機関名
           03  OKUKAN-HOSPNAME              PIC  X(200).
      *データキー
           03  OKUKAN-DATA-KEY              PIC  9(16).
      *端末ＩＤ
           03  OKUKAN-TERMID                PIC  X(16).
      *オペーレータＩＤ
           03  OKUKAN-OPID                  PIC  X(16).
      *作成年月日
           03  OKUKAN-CREYMD                PIC  X(08).
      *更新年月日
           03  OKUKAN-UPYMD                 PIC  X(08).
      *更新時間
           03  OKUKAN-UPHMS                 PIC  X(06).
