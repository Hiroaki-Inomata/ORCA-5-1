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
      *    診療行為照会テーブル      COPY (CPSRYSRH.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SRYSRH-REC.
      *医療機関番号
           03  SRYSRH-HOSPNUM      PIC 9(2).
      *端末ＩＤ
           03  SRYSRH-TERMID       PIC X(64).
      *患者ＩＤ
           03  SRYSRH-PTID         PIC 9(10).
      *剤番号
           03  SRYSRH-ZAINUM       PIC 9(09).
      *入外区分
           03  SRYSRH-NYUGAIKBN    PIC X(1).
      *診療科
           03  SRYSRH-SRYKA        PIC X(2).
      *保険組合せ
           03  SRYSRH-HKNCOMBINUM  PIC 9(4).
      *診療区分
           03  SRYSRH-SRYKBN       PIC X(2).
      *診療種別区分
           03  SRYSRH-SRYSYUKBN    PIC X(3).
      *診療日
           03  SRYSRH-SRYYMD       PIC X(8).
      *剤回数
           03  SRYSRH-ZAIKAISU     PIC 9(5).
      *数量トータル
           03  SRYSRH-SURYOTOTAL   PIC 9(7)V9(05).
      *連番
           03  SRYSRH-RENNUM       PIC 9(02).
      *診療コード
           03  SRYSRH-SRYCD-G.
               05  SRYSRH-SRYCD-OCC    OCCURS 5.
                   07  SRYSRH-SRYCD    PIC X(9).
                   07  SRYSRH-SRYSURYO PIC 9(05)V9(05).
      *包括区分
           03  SRYSRH-HOKATSUKBN   PIC X(01).
      *会計区分（1:診療会計剤 2:入院会計剤）
           03  SRYSRH-ACCTKBN      PIC X(01).
      *
      *作成日
           03  SRYSRH-CREYMD       PIC X(8).
      *更新日
           03  SRYSRH-UPYMD        PIC X(8).
      *更新時間
           03  SRYSRH-UPHMS        PIC X(6).
      *
      *    以下virtual項目
      *患者番号
           03  SRYSRH-PTNUM        PIC X(20).
      *カナ氏名
           03  SRYSRH-KANANAME     PIC X(100).
      *性別
           03  SRYSRH-SEX          PIC X(1).
      *誕生日
           03  SRYSRH-BIRTHDAY     PIC X(8).
      *開始診療日
           03  SRYSRH-STSRYYMD     PIC X(8).
      *終了診療日
           03  SRYSRH-EDSRYYMD     PIC X(8).
      *開始診療月
           03  SRYSRH-STSRYYM      PIC X(6).
      *終了診療月
           03  SRYSRH-EDSRYYM      PIC X(6).
      *ドクター
           03  SRYSRH-DRCD         PIC X(5).
      *SQL
           03  SRYSRH-SQL          PIC X(15000).
      *日付＋時刻
           03  SRYSRH-YMDHMS       PIC X(14).

