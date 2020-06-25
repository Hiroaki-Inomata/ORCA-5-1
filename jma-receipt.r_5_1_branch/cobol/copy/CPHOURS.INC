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
      *  締め時刻管理                 (CPHOURS.INC) 
      *****************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  HOURS-REC.
      *
      *キー
           03  HOURS-KEY.
               05  HOURS-HOSPNUM       PIC 9(2).
      *診療日
               05  HOURS-SRYYMD        PIC  X(08).
      *締め時間
               05  HOURS-TIME          PIC  X(04).
      *ＩＤ
           03  HOURS-ID                PIC  X(12).
      *締め時刻区分
           03  HOURS-HOURSKBN          PIC  X(1).
      *端末ＩＤ
           03  HOURS-TERMID            PIC  X(32).
      *オペーレータＩＤ
           03  HOURS-OPID              PIC  X(16).
      *作成年月日
           03  HOURS-CREYMD            PIC  X(08).
      *更新年月日
           03  HOURS-UPYMD             PIC  X(08).
      *更新時間
           03  HOURS-UPHMS             PIC  X(06).
      *締め日開始
           03  HOURS-STSRYYMD          PIC  X(08).
      *締め日終了
           03  HOURS-EDSRYYMD          PIC  X(08).
      *LIMIT
           03  HOURS-LIMIT             PIC 9(5).
