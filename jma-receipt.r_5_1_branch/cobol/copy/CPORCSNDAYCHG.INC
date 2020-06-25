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
      *----------------------------------------------------------*
      *    診療行為算定日一括入力パラメタ  COPY (CPORCSNDAYCHG.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSNDAYCHGAREA.
      *    入力内容
           03  ORCSNDAYCHG-INPUTCD          PIC X(100).
      *    診療年月日
           03  ORCSNDAYCHG-SRYYMD           PIC X(08).
      *    入力桁数
           03  ORCSNDAYCHG-KETA             PIC 9(03).
      *    処理区分（１：入院期間チェックあり）
           03  ORCSNDAYCHG-KBN              PIC X(01).
      *    入院期間
           03  ORCSNDAYCHG-NYUINYMD         PIC X(08).
           03  ORCSNDAYCHG-TAIINYMD         PIC X(08).
      *
      *    出力領域
           03  ORCSNDAYCHG-OUTAREA.
      *        回数
               05  ORCSNDAYCHG-KAISU        PIC 9(03).
      *        回数桁数
               05  ORCSNDAYCHG-KAISUCNT     PIC 9(03).
      *        入院回数
               05  ORCSNDAYCHG-NYUIN-KAISU.
                   07  ORCSNDAYCHG-NYU-DAY      PIC 9(02)
                                                OCCURS   31.
      *        エラー区分
               05  ORCSNDAYCHG-ERRCD        PIC X(04).
      *        エラー内容
               05  ORCSNDAYCHG-ERRMSG       PIC X(100).
      *
