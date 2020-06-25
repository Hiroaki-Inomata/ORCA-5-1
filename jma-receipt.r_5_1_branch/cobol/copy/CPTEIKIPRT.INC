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
      *    定期請求印刷情報                   (CPTEIKIPRT.INC)   *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  TEIKIPRT-REC.
           03  TEIKIPRT-KEY.
      *医療機関番号
               05  TEIKIPRT-HOSPNUM    PIC  9(02).
      *病棟番号
               05  TEIKIPRT-BTUNUM     PIC  X(02).
      *伝票番号
               05  TEIKIPRT-DENPNUM    PIC  9(07).
      *患者ＩＤ
           03  TEIKIPRT-PTID           PIC  9(10).
      *患者番号
           03  TEIKIPRT-PTNUM          PIC  X(20).
      *患者カナ氏名
           03  TEIKIPRT-NAME           PIC  X(100).
