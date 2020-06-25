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
      *         点数マスタ 薬剤附加情報   COPY (CPTENSUPLUS2.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  TENSUPLUS2-REC.
           03  TNSP2-KEY.
               05  TNSP2-HOSPNUM            PIC 9(2).
               05  TNSP2-SRYCD             PIC  X(09).
      *    長期投与日数
           03  TNSP2-CHOKI-NISSU           PIC  9(03).
      *    長期投与期限
           03  TNSP2-CHOKI-KIGEN           PIC  X(08).
      *    長期投与エラー処理
           03  TNSP2-CHOKIERR              PIC  9(01).
      *    投与量
           03  TNSP2-TOUYORYO-G.
               05  TNSP2-TOUYORYO-T        OCCURS   10.
      *            下限年齢
                   07  TNSP2-TOUYOKGNAGE           PIC  X(02).
      *            上限年齢
                   07  TNSP2-TOUYOJGNAGE           PIC  X(02).
      *            投与量
                   07  TNSP2-TOUYORYO1             PIC  9(05)V9(05).
      *            投与量（頓服）
                   07  TNSP2-TOUYORYO2             PIC  9(05)V9(05).
      *    投与量エラー処理
           03  TNSP2-TOUYOERR              PIC  9(01).
      *    内服算定区分
           03  TNSP2-NAIFUKUKBN            PIC  9(01).
      *    月上限回数
           03  TNSP2-TUKIJGNKAISU          PIC  9(3).
      *    月総投与量
           03  TNSP2-TUKIJGNTOUYORYO       PIC  9(5)V9(5).
           03  TNSP2-TERMID                PIC  X(16).
           03  TNSP2-OPID                  PIC  X(16).
           03  TNSP2-CREYMD                PIC  X(08).
           03  TNSP2-UPYMD                 PIC  X(08).
           03  TNSP2-UPHMS                 PIC  X(06).
