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
       01  SBD010S03-AREA.
           03  SBD010S03-MAX              PIC 9(05).
           03  SBD010S03-OCC              OCCURS   200.
               05  SBD010S03-DENPEDANUM   PIC  9(02).
               05  SBD010S03-JOUTAIKBN    PIC  X(01).
               05  SBD010S03-SRYKA        PIC  X(02).
               05  SBD010S03-HKNCOMBI     PIC  X(04).
               05  SBD010S03-HKNNUM       PIC  X(03).
               05  SBD010S03-HONKZKKBN    PIC  X(01).
               05  SBD010S03-KOHNUM       PIC  X(03).
               05  SBD010S03-HKNTEN       PIC  S9(10).
               05  SBD010S03-SEIKYU       PIC  S9(10).
               05  SBD010S03-NYUKIN       PIC  S9(10).
               05  SBD010S03-CHOSEI       PIC  S9(10).
               05  SBD010S03-CHOSEI1      PIC  S9(10).
               05  SBD010S03-CHOSEI2      PIC  S9(10).
               05  SBD010S03-GENMEN       PIC  S9(10).
               05  SBD010S03-HKNSEIKYU    PIC  S9(10).
               05  SBD010S03-JIHSEIKYU    PIC  S9(10).
               05  SBD010S03-HKNNYUKIN    PIC  S9(10).
               05  SBD010S03-JIHNYUKIN    PIC  S9(10).
               05  SBD010S03-HKNCHOSEI    PIC  S9(10).
               05  SBD010S03-HKNCHOSEI1   PIC  S9(10).
               05  SBD010S03-HKNCHOSEI2   PIC  S9(10).
               05  SBD010S03-JIHCHOSEI    PIC  S9(10).
               05  SBD010S03-JIHCHOSEI1   PIC  S9(10).
               05  SBD010S03-JIHCHOSEI2   PIC  S9(10).
               05  SBD010S03-HKNGENMEN    PIC  S9(10).
               05  SBD010S03-JIHGENMEN    PIC  S9(10).
               05  SBD010S03-ETCJIHI-OCC  OCCURS   10.
                   07  SBD010S03-ETCJIHI  PIC  S9(10).
               05  SBD010S03-HOHO-G.
                   07  SBD010S03-HOHO-MAX PIC  9(05).
                   07  SBD010S03-HOHO-OCC OCCURS   100.
                       09  SBD010S03-HOHO PIC  X(02).
                       09  SBD010S03-HOHOMONEY
                                          PIC  S9(10).
               05  SBD010S03-DAILYKEY     PIC  X(12).
               05  SBD010S03-SYURRKNUM    PIC  9(02).
               05  SBD010S03-SYUEDANUM    PIC  9(02).
