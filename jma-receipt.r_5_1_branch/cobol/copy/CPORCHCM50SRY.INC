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
      *    自費内訳明細書診療行為ファイル  COPY (CPORCHCM50SRY.INC)   *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  HCM50SRY-REC.
           03  HCM50SRY-ISM-KEY.
               05  HCM50SRY-NYUGAIKBN      PIC X(01).
               05  HCM50SRY-SKYSTYMD       PIC X(08).
               05  HCM50SRY-SKYEDYMD       PIC X(08).
               05  HCM50SRY-SRYKBN         PIC X(02).
               05  HCM50SRY-SRYKBN2        PIC X(02).
               05  HCM50SRY-TENTTLKBN      PIC 9(03).
               05  HCM50SRY-KANANAME       PIC X(80).
               05  HCM50SRY-SURYO          PIC S9(11).
               05  HCM50SRY-KINGAKU        PIC S9(11).
               05  HCM50SRY-SRYCD          PIC X(09).
           03  HCM50SRY-ZAIKAISU           PIC S9(11).
           03  HCM50SRY-NAME               PIC X(80).
