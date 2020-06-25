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
      *    プログラムオプションパラメタ     COPY (CPORCSPRGOPTION.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSPRGOPTIONAREA.
      *設定
      *    プログラム名（必須）
           03  ORCSPRGOPTION-PRGID      PIC X(25).
      *    パラメタ文字（必須）
           03  ORCSPRGOPTION-IN-DATA    PIC X(30).
      *返却
           03  ORCSPRGOPTION-OUT-G.
      *        パラメタ値
               05  ORCSPRGOPTION-OT-PARA    PIC X(100).
      *        最大桁数
               05  ORCSPRGOPTION-OT-SIZE    PIC 9(03).
      *        文字タイプ（１：半角、２：全角、３：混在）
               05  ORCSPRGOPTION-OT-TYPE    PIC 9(01).
      *    復帰コード
           03  ORCSPRGOPTION-RC            PIC 9(02).
      *
