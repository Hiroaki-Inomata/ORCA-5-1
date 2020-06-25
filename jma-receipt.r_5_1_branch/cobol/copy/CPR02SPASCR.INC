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
      *    レセプト作成−印刷指示画面ＳＰＡ
      *          
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  SPA-SCR-REC.
           03  SPA-R02-AREA-1.
               05  SPA-NAI-TBL.
                   07  SPA-NAI-TBL-OCC         OCCURS   19.
                       09  SPA-NAI-THKNJANUM-SYAHO PIC X(08).
                       09  SPA-NAI-TBL-G       OCCURS   36.
                           11  SPA-NAI-PRTJYUNI        PIC X(02).
                           11  SPA-NAI-TEISYUTUSAKI    PIC X(01).
                           11  SPA-NAI-TDRCD           PIC X(05).
                           11  SPA-NAI-TBTUNUM         PIC X(02).
                           11  SPA-NAI-THKNJANUM
                                                   PIC X(08).
                           11  SPA-NAI-THKNJANUM-ERR
                                                   PIC X(01).
                           11  SPA-NAI-TPREFKBN-SRT
                                                   PIC X(01).
                           11  SPA-NAI-TPAGE-G.
                               13  SPA-NAI-TPAGE-CHOUKI
                                                   PIC 9(04).
                               13  SPA-NAI-TPAGE-ZAITAKU
                                                   PIC 9(04).
                               13  SPA-NAI-TPAGE-KOUGAKU
                                                   PIC 9(04).
                               13  SPA-NAI-TPAGE-SYUKA
                                                   PIC 9(04)
                                                   OCCURS 101.
                               13  SPA-NAI-TPAGE-CHOUKI-SYUKA
                                                   PIC 9(01)
                                                   OCCURS 101.
                               13  SPA-NAI-TPAGE-ZAITAKU-SYUKA
                                                   PIC 9(01)
                                                   OCCURS 101.
                               13  SPA-NAI-TPAGE-KOUGAKU-SYUKA
                                                   PIC 9(01)
                                                   OCCURS 101.
                           11  SPA-NAI-SYUBETU-G.
                               13  SPA-NAI-SYUBETU     PIC X(04)
                                                   OCCURS 5.
                           11  SPA-NAI-TSYUBETU-SRT1   PIC X(04).
                           11  SPA-NAI-TPREFKBN        PIC X(01).
 