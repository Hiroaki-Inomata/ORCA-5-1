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
      *    印刷用コンスタント領域 COPY (CPPRTCONST.INC)          *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  PRTCONST-AREA.
           03  PRTCONST-T.
      *       
               05  PRTCONST-PRTID-T01  PIC  X(20)
                                       VALUE   "HC401     " .
               05  PRTCONST-PRTID-T02  PIC  X(20)
      *        収納一覧                     
                                       VALUE   "HC07      " .
               05  PRTCONST-PRTID-T03  PIC  X(20)
      *        日計表                     
                                       VALUE   "HC17      " .
               05  PRTCONST-PRTID-T04  PIC  X(20)
                                       VALUE   "HC17V01   " .
      *        社保総括表                     
               05  PRTCONST-PRTID-T05  PIC  X(20)
                                       VALUE   "HC08      " .
               05  PRTCONST-PRTID-T06  PIC  X(20)
                                       VALUE   "HC08V01   " .
      *        国保総括表                     
               05  PRTCONST-PRTID-T07  PIC  X(20)
                                       VALUE   "HC04      " .
               05  PRTCONST-PRTID-T08  PIC  X(20)
                                       VALUE   "HC04V01   " .
               05  PRTCONST-PRTID-T09  PIC  X(20)
                                       VALUE   "HC04V02   " .
               05  PRTCONST-PRTID-T10  PIC  X(20)
                                       VALUE   "HC04V03   " .
      *        国保総括表（負担割合あり）                     
               05  PRTCONST-PRTID-T11  PIC  X(20)
                                       VALUE   "HC041     " .
               05  PRTCONST-PRTID-T12  PIC  X(20)
                                       VALUE   "HC041V01  " .
               05  PRTCONST-PRTID-T13  PIC  X(20)
                                       VALUE   "HC041V02  " .
               05  PRTCONST-PRTID-T14  PIC  X(20)
                                       VALUE   "HC041V03  " .
      *        点数マスタ一覧                     
               05  PRTCONST-PRTID-T15  PIC  X(20)
                                       VALUE   "HCML01    " .
               05  PRTCONST-PRTID-T16  PIC  X(20)
                                       VALUE   "HCML02    " .
               05  PRTCONST-PRTID-T17  PIC  X(20)
                                       VALUE   "HCML03    " .
               05  PRTCONST-PRTID-T18  PIC  X(20)
                                       VALUE   "HCML06    " .
      *        国保総括表（２００２年１０月改正対応）                     
               05  PRTCONST-PRTID-T19  PIC  X(20)
                                       VALUE   "HCM2C.red " .
               05  PRTCONST-PRTID-T20  PIC  X(20)
                                       VALUE   "HCM2D.red " .
               05  PRTCONST-PRTID-T21  PIC  X(20)
                                       VALUE   "HCM2E.red " .
               05  PRTCONST-PRTID-T22  PIC  X(20)
                                       VALUE   "HCM2F.red " .
      *                                 
           03  PRTCONST-R  REDEFINES   PRTCONST-T.
               05  PRTCONST-TX         OCCURS    22
                                       INDEXED   PRTCONST-IDX.
                   07  PRTCONST-PRTID  PIC  X(20).
