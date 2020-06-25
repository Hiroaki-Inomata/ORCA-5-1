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
      *****************************************************************
      *  受診内容データ      CPJYUSIN.INC 
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  JYU-JYUSIN-REC.
           03  JYU-KEY.
      *医療機関コード
               05  JYU-HOSPID          PIC X(24).
      *患者ＩＤ
               05  JYU-PTID            PIC 9(10).
      *受付年月日
               05  JYU-UKEYMD          PIC X(08).
      *受付時間
               05  JYU-UKETIME         PIC X(06).
      *患者氏名
           03  JYU-KNJNAME             PIC X(50).
      *診療科
           03  JYU-SRYKA               PIC X(02).
      *ドクターコード
           03  JYU-TANTDR              PIC X(05).
      *診療内容
           03  JYU-SRYNAIYO            PIC X(02).
      *会計時間
           03  JYU-KAIKEITIME          PIC 9(04).
      *キー予約時間
           03  JYU-KYYKTIME            PIC 9(04).
      *予約時間
           03  JYU-YYKTIME             PIC 9(04).
      *予約ＩＤ
           03  JYU-YYKID               PIC 9(02).
      *表示順
           03  JYU-SORT                PIC 9(04).
      *端末ＩＤ
           03  JYU-ADDYMD              PIC X(08).
      *オペレータＩＤ
           03  JYU-UPDYMD              PIC X(08).
      *作成年月日
           03  JYU-UPDYMD              PIC X(08).
      *更新年月日
           03  JYU-UPDYMD              PIC X(08).
      *更新時間
           03  JYU-UPDYMD              PIC X(08).
