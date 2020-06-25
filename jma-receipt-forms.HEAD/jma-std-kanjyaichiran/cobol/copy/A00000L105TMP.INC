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
       01  L105T.
          03  L105T-KEY.
            05  L105T-SRYKA           PIC  X(02).
            05  L105T-DRCD            PIC  X(05).
            05  L105T-SUBKEY1         PIC  X(40).
            05  L105T-PTNUM           PIC  X(10).
            05  L105T-NYUINYMD        PIC  X(10).
          03  L105T-KANANAME          PIC  X(40).
          03  L105T-NAME              PIC  X(26).
          03  L105T-SEX               PIC  X(01).
          03  L105T-BIRTHDAY          PIC  X(08).
          03  L105T-AGE               PIC  X(03).
          03  L105T-HKNJANUM          PIC  X(08).
          03  L105T-HONKAZKKBN        PIC  X(01).
          03  L105T-KIGONUM           PIC  X(52).
          03  L105T-KIGO              PIC  X(52).
          03  L105T-BANGO             PIC  X(52).
          03  L105T-ATAMAGAKI         PIC  X(08).
          03  L105T-KOH1FTNNUM        PIC  X(30).
          03  L105T-KOH2FTNNUM        PIC  X(30).
          03  L105T-KOH3FTNNUM        PIC  X(30).
          03  L105T-TAIINYMD          PIC  X(10).
          03  L105T-PTID              PIC  X(10).
