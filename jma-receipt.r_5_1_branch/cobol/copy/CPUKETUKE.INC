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
      *  受付               (CPUKETUKE.INC) 
      *****************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  UKETUKE-REC.
      *
      *キー
           03  UKE-KEY.
      *医療機関ＩＤ
               05  UKE-HOSPNUM          PIC 9(2).
      *受付年月日
               05  UKE-UKEYMD          PIC  X(08).
      *受付ＩＤ
               05  UKE-UKEID           PIC  9(05).
      *受付時間
           03  UKE-UKETIME             PIC  X(06).
      *患者ＩＤ
           03  UKE-PTID                PIC  9(10).
      *患者氏名
           03  UKE-NAME                PIC X(100).
      *診療科
           03  UKE-SRYKA               PIC X(02).
      *ドクターコード
           03  UKE-DRCD                PIC X(05).
      *診療内容
           03  UKE-SRYNAIYO            PIC X(02).
      *保険組合せ
           03  UKE-HKNCOMBI            PIC 9(04).
      *会計時間
           03  UKE-KAIKEITIME          PIC 9(04).
      *キー予約時間
           03  UKE-KEYYYKTIME          PIC 9(04).
      *予約時間
           03  UKE-YYKTIME             PIC 9(04).
      *予約ＩＤ
           03  UKE-YYKID               PIC 9(02).
      *表示順
           03  UKE-HYOJIJUN            PIC 9(04).
      *確認連絡
           03  UKE-KAKREN              PIC X(02).
      *送受信フラグ
           03  UKE-SRFLG               PIC X(02).
      *端末ＩＤ
           03  UKE-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  UKE-OPID                PIC  X(16).
      *作成年月日
           03  UKE-CREYMD              PIC  X(08).
      *更新年月日
           03  UKE-UPYMD               PIC  X(08).
      *更新時間
           03  UKE-UPHMS               PIC  X(06).
