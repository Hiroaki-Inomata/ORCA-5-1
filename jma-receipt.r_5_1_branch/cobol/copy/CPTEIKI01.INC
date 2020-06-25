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
      *    患者定期請求収納データ
      *　  COPY  (CPTEIKI01.INC)                                 *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  TEIKI01-REC.
           03  TEIKI01-KEY.
               05  TEIKI01-MAIN-KEY.
                   07  TEIKI01-HOSPNUM     PIC 9(02).
                   07  TEIKI01-PTID        PIC 9(10).
                   07  TEIKI01-HKNCOMBINUM PIC 9(04).
               05  TEIKI01-SKYSTYMD        PIC X(08).
               05  TEIKI01-SKYEDYMD        PIC X(08).
           03  TEIKI01-SAKKBN              PIC X(01).
           03  TEIKI01-KBT                 PIC X(01).
           03  TEIKI01-SYU-DAT             PIC X(5000).
           03  TEIKI01-TSI-SKYTEN          PIC S9(09).
           03  TEIKI01-TSI-SKYMONEY-TAX-SAI
                                           PIC S9(07).
           03  TEIKI01-TSI-SKYMONEY        PIC S9(09).
           03  TEIKI01-SYUDAY.
               05  TEIKI01-SYUDAY-EXIST-FLG
                                           PIC 9(01).
               05  TEIKI01-SYUDAY-DAT      PIC X(5000).
