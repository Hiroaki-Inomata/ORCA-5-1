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
      *    出力パラメタ              COPY (CPORCHCM50.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHCM50AREA.
           03  ORCHCM50-STRSRYYMD       PIC  X(08).
           03  ORCHCM50-ENDSRYYMD       PIC  X(08).
      *    集計区分 0:日別/1:月別
           03  ORCHCM50-SUMKBN          PIC  X(01).
           03  ORCHCM50-PTID            PIC  9(10).
           03  ORCHCM50-NYUGAI          PIC  X(01).
           03  ORCHCM50-OT-AREA.
      *        出力件数
               05  ORCHCM50-OT-CNT      PIC  9(05).
      *
      *        リターンコード
               05  ORCHCM50-RC          PIC S9(03).
      *
