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
      ******************************************************************
      *    HAORIレスポンスマスタ取得
      *    COPY (MEV3RES.INC)
      ******************************************************************
        01 MERES-AREA.
           03  MERES-CONTEXT                   PIC S9(9)   BINARY.
           03  MERES-OBJECT                    PIC X(36).
           03  MERES-MODE                      PIC S9(9)   BINARY.
           03  MERES-RECORDNAME                PIC X(128).
           03  MERES-DATA-AREA.
               05  MERES-REQNUM                PIC X(2).
               05  MERES-RESNUM                PIC X(2).
               05  MERES-KARTE-UID             PIC X(36).
               05  MERES-ORCA-UID              PIC X(36).
               05  MERES-INFORMATION-DATE      PIC X(10).
               05  MERES-INFORMATION-TIME      PIC X(08).
               05  MERES-API-RESULT            PIC X(03).
               05  MERES-API-RESULT-MSG        PIC X(100).
               05  MERES-MASTER-AREA.
                   07  MERES-MASTER-OCC        OCCURS  100.
                       09  MERES-MASTER-ID     PIC X(50).
                       09  MERES-MASTER-NAME   PIC X(50).
                       09  MERES-EXISTS-UPYMD  PIC X(01).
               05  MERES-DATA-ID-AREA.
                   07  MERES-DATA-ID           PIC X(37).
               05  MERES-RECORD-COUNT          PIC X(10).
