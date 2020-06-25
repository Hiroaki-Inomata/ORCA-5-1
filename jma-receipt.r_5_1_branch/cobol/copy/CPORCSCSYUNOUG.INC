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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ中途終了データから収納編集サブパラメタ（一体化）
      *                                         (CPORCSCSYUNOUG.INC)
      ******************************************************************
       01  ORCSCSYUNOUGAREA.
      *    処理区分
           03  ORCSCSYU-SYORIKBN       PIC X(01).
      *    処理区分
           03  ORCSCSYU-SYORIPG        PIC X(04).
      *    中途データ件数
           03  ORCSCSYU-WKSRYACT-MAX   PIC 9(04).
      *    収納データ件数
           03  ORCSCSYU-SYUNOU-MAX     PIC 9(04).
      *    保険組合せ・診療科情報
           03  ORCSCSYU-HKNCOMBI-AREA.
      *        保険組合せ番号
               05  ORCSCSYU-HKNCOMBI-TBL     OCCURS  15.
                   07  ORCSCSYU-HKNCOMBI     PIC X(04).
                   07  ORCSCSYU-SRYKA        PIC X(02)
                                             OCCURS   10.
      *
      *    エラー区分
           03  ORCSCSYU-ERRCD        PIC 9(02).
      *
