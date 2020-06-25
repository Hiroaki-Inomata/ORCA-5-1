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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SBD010S02-AREA.
           03  SBD010S02-STDAILYKEY       PIC  X(12).
           03  SBD010S02-EDDAILYKEY       PIC  X(12).
           03  SBD010S02-MIN-DENPEDANUM   PIC  9(01).
           03  SBD010S02-MAX              PIC  9(05).
           03  SBD010S02-OCC              OCCURS   200.
               05  SBD010S02-KEYSRYKA     PIC  X(02).
               05  SBD010S02-KEYHOKEN     PIC  9(02).
               05  SBD010S02-SRYKA        PIC  X(02).
               05  SBD010S02-HOKEN        PIC  9(02).
               05  SBD010S02-HKNCOMBI     PIC  X(04).
               05  SBD010S02-HKNNUM       PIC  X(03).
               05  SBD010S02-HONKZKKBN    PIC  X(01).
               05  SBD010S02-KOHNUM       PIC  X(03).
               05  SBD010S02-HKNTEN       PIC  S9(10).
               05  SBD010S02-SEIKYU       PIC  S9(10).
               05  SBD010S02-HKNSEIKYU    PIC  S9(10).
               05  SBD010S02-JIHSEIKYU    PIC  S9(10).
               05  SBD010S02-GENMEN       PIC  S9(10).
               05  SBD010S02-HKNGENMEN    PIC  S9(10).
               05  SBD010S02-JIHGENMEN    PIC  S9(10).
               05  SBD010S02-CHOSEI       PIC  S9(10).
               05  SBD010S02-CHOSEI1      PIC  S9(10).
               05  SBD010S02-CHOSEI2      PIC  S9(10).
               05  SBD010S02-HKNCHOSEI    PIC  S9(10).
               05  SBD010S02-HKNCHOSEI1   PIC  S9(10).
               05  SBD010S02-HKNCHOSEI2   PIC  S9(10).
               05  SBD010S02-JIHCHOSEI    PIC  S9(10).
               05  SBD010S02-JIHCHOSEI1   PIC  S9(10).
               05  SBD010S02-JIHCHOSEI2   PIC  S9(10).
               05  SBD010S02-NYUKIN       PIC  S9(10).
               05  SBD010S02-HKNNYUKIN    PIC  S9(10).
               05  SBD010S02-JIHNYUKIN    PIC  S9(10).
               05  SBD010S02-KONNYUKIN    PIC  S9(10).
               05  SBD010S02-KONHKNNYUKIN PIC  S9(10).
               05  SBD010S02-KONJIHNYUKIN PIC  S9(10).
               05  SBD010S02-ZENNYUKIN    PIC  S9(10).
               05  SBD010S02-ZENHKNNYUKIN PIC  S9(10).
               05  SBD010S02-ZENJIHNYUKIN PIC  S9(10).
               05  SBD010S02-MISYU        PIC  S9(10).
               05  SBD010S02-HKNMISYU     PIC  S9(10).
               05  SBD010S02-JIHMISYU     PIC  S9(10).
               05  SBD010S02-KONMISYU     PIC  S9(10).
               05  SBD010S02-KONHKNMISYU  PIC  S9(10).
               05  SBD010S02-KONJIHMISYU  PIC  S9(10).
               05  SBD010S02-ZENMISYU     PIC  S9(10).
               05  SBD010S02-ZENHKNMISYU  PIC  S9(10).
               05  SBD010S02-ZENJIHMISYU  PIC  S9(10).
               05  SBD010S02-ETCJIHI-OCC  OCCURS   10.
                   07  SBD010S02-ETCJIHI  PIC  S9(10).
               05  SBD010S02-HOHO-G.
                   07  SBD010S02-HOHO-MAX PIC  9(05).
                   07  SBD010S02-HOHO-OCC OCCURS   100.
                       09  SBD010S02-HOHO PIC  X(02).
                       09  SBD010S02-HOHOMONEY
                                          PIC  S9(10).
               05  SBD010S02-DAILYKEY     PIC  X(12).
               05  SBD010S02-DENPEDANUM-G.
                   07  SBD010S02-DENPEDANUM    PIC  9(02).
                   07  SBD010S02-SYURRKNUM     PIC  9(02).
                   07  SBD010S02-SYUEDANUM     PIC  9(02).
               05  SBD010S02-RENNUM       PIC  9(09).
               05  SBD010S02-LAST         PIC  9(01).
