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
      *****************************************************************
      *
      *    ＤＢサブ   ワーク領域      CPMCPDBWRK.INC
      *          
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/04/09  WRK-BYOMEI 追加     
      *****************************************************************
       WORKING-STORAGE         SECTION.
       01  WORK.
      **     03  I           PIC S9(9)   BINARY.
      **     03  J           PIC S9(9)   BINARY.
           03  WRK-OPID            PIC X(16).
           03  WRK-INPUTCD         PIC X(20).
           03  WRK-NAME            PIC X(100).
           03  WRK-FILEMEI         PIC X(20).
           03  IDX                 PIC S9(9)   BINARY.
           03  WRK-MOJISU          PIC 9(04).
           03  WRK-BYOMEI          PIC X(200). 
      *
      *
           03  WRK-MOJISU2         PIC 9(04).
           03  WRK-ICHI1           PIC 9(04).
           03  WRK-ICHI2           PIC 9(04).
      *     
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-TABLE           PIC X(64).
      *    03  WRK-PATH.
      *        04  WRK-MCP-BLOCKS       PIC S9(9)   BINARY.
      *        04  WRK-MCP-RNAME        PIC S9(9)   BINARY.
      *        04  WRK-MCP-PNAME        PIC S9(9)   BINARY.
