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
      *         ＣＲＯＮ情報テーブル   (CPCRON.INC)              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CRON-REC.
           03  CRON-KEY.
               05  CRON-CRONID         PIC 9(04).
      *実行区分
           03  CRON-RUN                PIC X(01).
      *処理名
           03  CRON-NAME               PIC X(30).
      *分
           03  CRON-CRNMIN             PIC X(02).
      *時
           03  CRON-CRNHOUR            PIC X(02).
      *日
           03  CRON-CRNDOM             PIC X(02).
      *月
           03  CRON-CRNMON             PIC X(02).
      *曜日
           03  CRON-CRNDOW             PIC X(01).
      *スクリプト名
           03  CRON-SCRIPT             PIC X(100).
      *端末ＩＤ
           03  CRON-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  CRON-OPID               PIC  X(16).
      *作成年月日
           03  CRON-CREYMD             PIC  X(08).
      *更新年月日
           03  CRON-UPYMD              PIC  X(08).
      *更新時間
           03  CRON-UPHMS              PIC  X(06).
      *--- 以下virtual項目
           03  CRON-STCRONID           PIC  9(04).
           03  CRON-EDCRONID           PIC  9(04).
