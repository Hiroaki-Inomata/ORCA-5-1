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
      *    老人・一般コード振替変更用ワーク    COPY (CPSRUCDCHGWK.INC)
      *
      *-------------------------------------------------*
      *01  SRYCDCHG-REC.
      *処理区分
           03  CHGWK-SYORIKBN            PIC X(01).
      *
           03  CHGWK-KEY.
      *一般診療コード
               05  CHGWK-IPNSRYCD        PIC  X(09).
      *老人診療コード
               05  CHGWK-RJNSRYCD        PIC  X(09).
      *端末ＩＤ
           03  CHGWK-TERMID              PIC  X(16).
      *オペレータＩＤ
           03  CHGWK-OPID                PIC  X(16).
      *作成年月日
           03  CHGWK-CREYMD              PIC  X(08).
      *更新年月日
           03  CHGWK-UPYMD               PIC  X(08).
      *更新時間
           03  CHGWK-UPHMS               PIC  X(06).
