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
       01  XSUB01-LNK.
           03  XSUB01-IN-AREA.
               05  XSUB01-SINGLE                   PIC 9(01).
               05  XSUB01-BYOCD-G.
                   07  XSUB01-BYOCD-OCC            OCCURS  6.
                       09  XSUB01-BYOCD            PIC X(20).
           03  XSUB01-OT-AREA.
               05  XSUB01-O-BYOMEI-CNT             PIC 9(05).
               05  XSUB01-O-BYOCD-OCC              OCCURS  6.
                   07  XSUB01-O-TOP                PIC 9(01).
                   07  XSUB01-O-OVER               PIC 9(01).
                   07  XSUB01-O-HENSYU             PIC 9(01).
                   07  XSUB01-O-BYOCD              PIC X(20).
                   07  XSUB01-O-BYOMEI             PIC X(100).
                   07  XSUB01-O-BYOSTATUS          PIC X(01).
                   07  XSUB01-O-RECECD-G.
                       09  XSUB01-O-RECECD-MAX     PIC 9(05).
                       09  XSUB01-O-RECECD-OCC     OCCURS  21.
                           11  XSUB01-O-RECECD     PIC X(07).
                           11  XSUB01-O-RECECDMEI  PIC X(100).
               05  XSUB01-O-BYOSTATUSALL           PIC X(01).
               05  XSUB01-O-BYOMEIALL              PIC X(400).
      *        リターンコード
      *        ０：正常
      *        １：未コード化傷病名コードの直接入力
      *        ２：レセ電コードが２１件を超過
      *        ３：該当コードなし（病名マスタ）
      *        ４：該当コードなし（自院病名マスタ）
               05  XSUB01-O-RC                     PIC 9(03).
               05  XSUB01-O-ERRIDX                 PIC 9(03).
