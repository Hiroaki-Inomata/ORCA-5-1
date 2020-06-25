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
      *-------------------------------------------------*
      *    選定療養費・一般コード振替    COPY (CPSENTEICDCHG.INC)
      *
      *-------------------------------------------------*
      *01  SENTEICDCHG-REC.
           03  SENCHG-KEY.
      *一般診療コード
               05  SENCHG-IPNSRYCD     PIC  X(09).
      *選定療養費診療コード
               05  SENCHG-SENTEISRYCD  PIC  X(09).
      *端末ＩＤ
           03  SENCHG-TERMID           PIC  X(16).
      *オペレータＩＤ
           03  SENCHG-OPID             PIC  X(16).
      *作成年月日
           03  SENCHG-CREYMD           PIC  X(08).
      *更新年月日
           03  SENCHG-UPYMD            PIC  X(08).
      *更新時間
           03  SENCHG-UPHMS            PIC  X(06).
