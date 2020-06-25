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
      *    請求確認情報編集サブ　COPY (CPORCSS015.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SS015-AREA.
           03  SS015-SRYYMD                    PIC X(08).
           03  SS015-JIHINAME-G.
               05  SS015-JIHINAME-OCC          OCCURS 2.
                   07  SS015-JIHINAME-OCC2     OCCURS 10.
                       09  SS015-JIHINAME      PIC X(20).
           03  SS015-NYKN-HOHOLST-G.
               05  SS015-NYKN-HOHOLST-MAX      PIC 9(03).
               05  SS015-NYKN-HOHOLST          OCCURS 50.
                   07  SS015-NYKN-HOHOL        PIC X(02).
                   07  SS015-NYKN-HOHOFL       PIC X(01).
                   07  SS015-NYKN-HOHOMEIL     PIC X(20).
           03  SS015-SMEIJOTAILST-G.
               05  SS015-SMEIJOTAILST-MAX      PIC 9(03).
               05  SS015-SMEIJOTAILST          OCCURS 10.
                   07  SS015-SMEIJOTAIL        PIC X(01).
                   07  SS015-SMEIJOTAIFL       PIC X(01).
                   07  SS015-SMEIJOTAIMEIL     PIC X(20).
           03  SS015-SRYITEMLST-G.
               05  SS015-SRYITEMLST-MAX        PIC 9(03).
               05  SS015-SRYITEMLST-OCC        OCCURS   16.
                   07  SS015-SRYITEML          PIC X(03).
                   07  SS015-SRYITEMFL         PIC X(01).
                   07  SS015-SRYITEMMEIL       PIC X(30).
