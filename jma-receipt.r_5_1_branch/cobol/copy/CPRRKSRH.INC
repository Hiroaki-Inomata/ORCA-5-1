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
      *    受診履歴照会テーブル    COPY (CPRRKSRH.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  RRKSRH-REC.
      *医療機関番号
            03  RRKSRH-HOSPNUM         PIC 9(2).
      *端末ＩＤ
            03  RRKSRH-TERMID          PIC X(64).
      *患者ＩＤ
            03  RRKSRH-PTID            PIC 9(10).
      *入外区分
            03  RRKSRH-NYUGAIKBN       PIC X(1).
      *診療科
            03  RRKSRH-SRYKA           PIC X(2).
      *ドクター
            03  RRKSRH-DRCD            PIC X(5).
      *保険組合せ
            03  RRKSRH-HKNCOMBINUM     PIC 9(4).
      *診療日
            03  RRKSRH-SRYYMD          PIC X(8).
      *作成日
            03  RRKSRH-CREYMD          PIC X(8).
      *更新日
            03  RRKSRH-UPYMD           PIC X(8).
      *更新時間
            03  RRKSRH-UPHMS           PIC X(6).
      *以下検索用項目
      *開始診療日
            03  RRKSRH-STSRYYMD        PIC X(8).
      *終了診療日
            03  RRKSRH-EDSRYYMD        PIC X(8).
      *公費の種類
            03  RRKSRH-KOHNUM-G.
                05 RRKSRH-KOHNUM       PIC X(3) OCCURS 5.
      *日付＋時刻
            03  RRKSRH-YMDHMS          PIC X(14).

