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
      *         システム基本  COPY (CPSYSUSER.INC)               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SYSUSER-REC.
      *    ユーザＩＤ
           03  SYSUSER-USERID          PIC  X(16).
      *    医療機関識別番号
           03  SYSUSER-HOSPNUM         PIC  9(02).
      *    スーパーバイザー
           03  SYSUSER-SUPERVISOR      PIC  X(01).
      *
           03  SYSUSER-UPYMD           PIC  X(08).
