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
      *    入院請求期間編集　　COPY (CPORCSS006.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      ************************************************************
       01  SS006-AREA.
           03  SS006-IN.
      *請求期間開始開始日
               05  SS006-SKYSTYMD          PIC X(08).
      *請求期間開始終了日
               05  SS006-SKYEDYMD          PIC X(08).
      *計算区分(1:期間終了日＝退院日/2:期間終了日≠退院日)
               05  SS006-SKYKBN            PIC X(01).
           03  SS006-OT.
      *請求開始日同日再入院フラグ
               05  SS006-DOJITUFLG-ST      PIC  X(01).
      *開始日入院食事区分（０：食なし、１：朝から、２：昼から、３：夕から）
               05  SS006-TIMEKBN-ST        PIC  X(01).
      *開始日食道加算算定可否
               05  SS006-SYOKUDO-OK        PIC  9(01).
      *請求終了日同日再入院フラグ
               05  SS006-DOJITUFLG-ED      PIC  X(01).
      *終了日入院食事区分（０：食なし、１：朝まで、２：昼まで、３：夕まで）
               05  SS006-TIMEKBN-ED        PIC  X(01).
