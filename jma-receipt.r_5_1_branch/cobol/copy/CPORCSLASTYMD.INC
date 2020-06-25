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
      *    患者最終来院日編集パラメタ                            *
      *                         COPY  (CPORCSLASTYMD.INC)        *
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSLASTYMDAREA.
      *
      *    患者ＩＤ
           03  ORCSLASTYMD-PTID                PIC 9(10).
      *    診療科名称設定（１：設定する）
           03  ORCSLASTYMD-SRYKAFLG            PIC X(01).
      *    出力項目
           03  ORCSLASTYMD-OUT.
      *        最終来院日（西暦）
               05  ORCSLASTYMD-LASTYMD         PIC X(08).
      *        最終来院日（和暦）
               05  ORCSLASTYMD-LASTYMDW        PIC X(09).
      *        前回診療科
               05  ORCSLASTYMD-SRYKA           PIC X(02).
      *        前回診療科名
               05  ORCSLASTYMD-SRYKA-MEI       PIC X(10).
      *        保険組合せ
               05  ORCSLASTYMD-HKNCOMBI        PIC X(04).
      *        保険（公費）
               05  ORCSLASTYMD-HKNNUM          PIC X(03).
      *        保険（公費）名称
               05  ORCSLASTYMD-HKNCOMBI-MEI    PIC X(20).
      *        本人家族名
               05  ORCSLASTYMD-HONKZKKBN       PIC X(02).
      *        前回ドクター
               05  ORCSLASTYMD-DRCD            PIC X(05).
      *        病棟（システム日で入院中）
               05  ORCSLASTYMD-BRMNUM          PIC X(09).
      *        最終入院日
               05  ORCSLASTYMD-NYUINYMD        PIC X(08).
      *        最終退院日
               05  ORCSLASTYMD-TAIINYMD        PIC X(08).
      *
