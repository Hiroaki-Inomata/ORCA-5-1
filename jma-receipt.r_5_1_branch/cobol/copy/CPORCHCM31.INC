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
      *    出力パラメタ              COPY (CPORCHCM31.INC)
      *                                   (ORCHCM31、ORCHCM32で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHCM31AREA.
      *
      *    開始診療日
           03  ORCHCM31-STRSRYYMD       PIC  X(08).
      *    終了診療日
           03  ORCHCM31-ENDSRYYMD       PIC  X(08).
      *    期間指定区分
           03  ORCHCM31-KIKANKBN        PIC X(01).
      *    期間終了日前の支払い区分
           03  ORCHCM31-SUMKBN          PIC X(01).
      *    期間終了日後の支払い区分
           03  ORCHCM31-SYORIKBN        PIC X(01).
      *    請求内容の印字区分
           03  ORCHCM31-DSPKBN          PIC X(01).
      *    患者番号
           03  ORCHCM31-PTID            PIC  9(10).
      *    入外
           03  ORCHCM31-NYUGAI          PIC  X(01).
      *
           03  ORCHCM31-OT-AREA.
      *        出力件数
               05  ORCHCM31-OT-CNT      PIC  9(05).
      *
      *        リターンコード
               05  ORCHCM31-RC          PIC S9(03).
      *
