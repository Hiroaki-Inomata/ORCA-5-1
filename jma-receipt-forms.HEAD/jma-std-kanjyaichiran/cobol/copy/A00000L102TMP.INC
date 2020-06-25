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
       01  L102T.
          03  L102T-KEY.
            05  L102T-SRYKA           PIC  X(02).
            05  L102T-DRCD            PIC  X(05).
            05  L102T-SUBKEY1         PIC  X(40).
            05  L102T-PTNUM           PIC  X(10).
          03  L102T-KANANAME          PIC  X(40).
          03  L102T-NAME              PIC  X(26).
          03  L102T-SEX               PIC  X(01).
          03  L102T-BIRTHDAY          PIC  X(08).
          03  L102T-AGE               PIC  X(03).
          03  L102T-HKNJANUM          PIC  X(08).
          03  L102T-HONKAZKKBN        PIC  X(01).
          03  L102T-KIGONUM           PIC  X(52).
          03  L102T-KIGO              PIC  X(52).
          03  L102T-BANGO             PIC  X(52).
          03  L102T-ATAMAGAKI         PIC  X(08).
          03  L102T-KOH1FTNNUM        PIC  X(30).
          03  L102T-KOH2FTNNUM        PIC  X(30).
          03  L102T-KOH3FTNNUM        PIC  X(30).
