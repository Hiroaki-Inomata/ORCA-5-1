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
       01  PUSHP001-REC.
           03  PUSHP001-EVENT                  PIC X(50).
           03  PUSHP001-OPID                   PIC X(16).
           03  PUSHP001-BODY.
               05  PUSHP001-OCC                OCCURS 10.
      *            レポートＩＤ
                   07  PUSHP001-FORMID         PIC X(20).
      *            カスタムＩＤ
                   07  PUSHP001-CUSTOMID       PIC X(20).
      *            レポート名称
                   07  PUSHP001-FORMNAME       PIC X(100).
      *            データＩＤ
                   07  PUSHP001-DATAID         PIC X(80).

