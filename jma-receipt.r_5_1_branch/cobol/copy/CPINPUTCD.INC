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
      *    入力コード       COPY (CPINPUTCD.INC)
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   01/05/18  表示用ＳＥＱの追加
      ************************************************************
      *01  INPUTCD-REC.
           03  ICD-KEY.
      *医療機関ＩＤ
               05  ICD-HOSPNUM          PIC 9(2).
      *コード種別
               05  ICD-CDSYU           PIC  X(01).
      *入力コード
               05  ICD-INPUTCD         PIC  X(20).
               05  ICD-INPUTCD-R1      REDEFINES   ICD-INPUTCD.
                   07  ICD-CD4         PIC  X(04).
                   07  ICD-FIL1        PIC  X(16).
               05  ICD-INPUTCD-R2      REDEFINES   ICD-INPUTCD.
                   07  ICD-CD5         PIC  X(05).
                   07  ICD-FIL2        PIC  X(15).
               05  ICD-INPUTCD-R3      REDEFINES   ICD-INPUTCD.
                   07  ICD-CD6         PIC  X(06).
                   07  ICD-CD6-R1      REDEFINES   ICD-CD6.
                       09  ICD-BURCD   PIC  X(03).
                       09  ICD-MEICD   PIC  X(03).
                   07  ICD-FIL3        PIC  X(14).
      *診療区分
           03  ICD-SRYKBN              PIC  X(02).
      *診療コード
           03  ICD-SRYCD               PIC  X(09).
      *----(01.00.01) LINE ADD START ----------------------------------
      *表示連番（１からの連番　１が主表示項目）
           03  ICD-DSPSEQ              PIC  9(02).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *表示名
           03  ICD-DSPNAME             PIC  X(200).
      *端末ＩＤ
           03  ICD-TERMID              PIC  X(16).
      *オペレータＩＤ
           03  ICD-OPID                PIC  X(16).
      *作成年月日
           03  ICD-CREYMD              PIC  X(08).
      *更新年月日
           03  ICD-UPYMD               PIC  X(08).
      *更新時間
           03  ICD-UPHMS               PIC  X(06).
