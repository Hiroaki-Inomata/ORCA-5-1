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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      *****************************************************************
       01  L106T.
          03  L106T-KEY.
            05  L106T-DRCD            PIC  X(05).
            05  L106T-SUBKEY1         PIC  X(40).
            05  L106T-SRYKA           PIC  X(10).
            05  L106T-PTNUM           PIC  X(10).
          03  L106T-KANANAME          PIC  X(40).
          03  L106T-NAME              PIC  X(26).
          03  L106T-SEX               PIC  X(01).
          03  L106T-BIRTHDAY          PIC  X(08).
          03  L106T-AGE               PIC  X(03).
          03  L106T-HKNJANUM          PIC  X(08).
          03  L106T-HONKAZKKBN        PIC  X(01).
          03  L106T-KIGONUM           PIC  X(52).
          03  L106T-KIGO              PIC  X(52).
          03  L106T-BANGO             PIC  X(52).
          03  L106T-KOH1FTNNUM        PIC  X(30).
          03  L106T-KOH2FTNNUM        PIC  X(30).
          03  L106T-KOH3FTNNUM        PIC  X(30).
          03  L106T-ATAMAGAKI         PIC  X(10).
          03  L106T-MISYU             PIC S9(09).
          03  L106T-LASTYMD           PIC  X(10).
          03  L106T-REMISYU           PIC S9(09).
