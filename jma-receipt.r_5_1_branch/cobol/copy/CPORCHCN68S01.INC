      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
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
      * 113-8681, Japan.
      ********************************************************************
      *-------------------------------------------------------------*
      *    ∞ÂŒ≈∂Ë ¨°¶£¡£ƒ£Ã∂Ë ¨…æ≤¡…º ‘Ω∏•µ•÷•—•È•·•ø
      *    COPY (CPORCHCN68S01.INC)
      *-------------------------------------------------------------*
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      ************************************************************
      *
       01  HCN68S01-AREA.
           03  HCN68S01-RANGE                      PIC X(01).
           03  HCN68S01-PTID                       PIC 9(10).
           03  HCN68S01-SRYYM                      PIC X(06).
           03  HCN68S01-SRYYMD                     PIC X(08).
           03  HCN68S01-NYUIN-NISSU                PIC 9(03).
           03  HCN68S01-COMB-DAY-G.
               05  HCN68S01-COMB-DAY               PIC 9(04)
                                                   OCCURS  31.
           03  HCN68S01-NYUIN-DAY-G.
               05  HCN68S01-NYUIN-DAY              PIC 9(01)
                                                   OCCURS  31.
           03  HCN68S01-RYOYO-FLG                  PIC 9(01).
           03  HCN68S01-RYOYO-DAY-G.
               05  HCN68S01-RYOYO-DAY-OCC          OCCURS  31.
                   07  HCN68S01-RYOYO-DAY          PIC S9(02).
                   07  HCN68S01-RYOYO-CD           PIC X(09).
                   07  HCN68S01-RYOYO-KHN          PIC 9(02).
           03  HCN68S01-RYOLIST-AREA.
               05  HCN68S01-RYOLIST-ADL-STIDX      PIC 9(03).
               05  HCN68S01-RYOLIST-DESIGNR-STIDX  PIC 9(03).
               05  HCN68S01-RYOLIST-MAX            PIC 9(05).
               05  HCN68S01-RYOLIST-OCC            OCCURS  100.
                   07  HCN68S01-TID                PIC X(02).
                   07  HCN68S01-TNAME              PIC X(100).
                   07  HCN68S01-TZAISKBKBN         PIC X(01).
                   07  HCN68S01-TMEDKBN            PIC X(01).
                   07  HCN68S01-TLIMIT             PIC 9(03).
                   07  HCN68S01-TALLKBN            PIC 9(01).
                   07  HCN68S01-TZAINUM            PIC 9(04).
                   07  HCN68S01-TSRYCD             PIC X(09).
                   07  HCN68S01-TDAYALL            PIC 9(02).
                   07  HCN68S01-TDAY-G.
                       09  HCN68S01-TDAY           PIC 9(02)
                                                   OCCURS  31.
           03  HCN68S01-PTSTS-G.
               05  HCN68S01-PTSTS-OCC              OCCURS  31.
                   07  HCN68S01-PTSTS-DAY          PIC 9(02).
                   07  HCN68S01-PTSTS-DAY2         PIC 9(02).
                   07  HCN68S01-PTSTS-DAY3         PIC 9(02).
                   07  HCN68S01-PTSTS-SRYCD        PIC X(09).
      *
           03  HCN68S01-MEDKBN-DAY-AREA.
               05  HCN68S01-MEDKBN-OCC             OCCURS 2.
                   07  HCN68S01-MEDKBN-DAYALL      PIC 9(01).
                   07  HCN68S01-MEDKBN-DAY-G.
                       09  HCN68S01-MEDKBN-DAY     PIC 9(01)
                                                   OCCURS  31.
           03  HCN68S01-ADLTEN-DAY-AREA.
               05  HCN68S01-ADLTEN-DAY-G.
                   07  HCN68S01-ADLTEN-DAY         PIC 9(02)
                                                   OCCURS  31.
           03  HCN68S01-DESIGNR-AREA.
               05  HCN68S01-DESIGNR-EVAL-DAY-G.
                   07  HCN68S01-DESIGNR-EVAL-DAY   PIC 9(02)
                                                   OCCURS  31.
