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
      *    病名登録 画面ＳＰＡ 
      *            画面　Ｃ０５で使用 
      *          
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/11/13  病名コードを７桁に変更     
      *
      *  01.01.01    NACL-藤原    02/08/20  新傷病名マスタ対応
      *  01.01.02    NACL-藤原    03/12/17  新患者病名マスタ対応
      *  01.01.03    NACL-藤原    04/07/12  検索用病名の追加
      *****************************************************************
      *
      *01  SPA-SCR-REC.
           03  SPA-C05.
               05  SPA-C05-AREA.
                   07  SPA-GMN-PAGE        PIC 9(02).
                   07  SPA-GMN-LINE        PIC 9(03).
                   07  SPA-GMN-MAX         PIC 9(04).
                   07  SPA-GMN-CUR         PIC 9(02).
                   07  SPA-GMN-NEXT        PIC 9(01).
                   07  SPA-GMN-AREA.
                       09  SPA-GMN-HENBYO      PIC X(100).
                       09  SPA-GMN-BYOKEN      PIC X(30).
                       09  SPA-GMN-BYOMEI-TBL.
                           11  SPA-GMN-BYOMEI-G        OCCURS  100.
                               13  SPA-GMN-TBANGO      PIC 9(05).
                               13  SPA-GMN-TSYUBET     PIC X(06).
                               13  SPA-GMN-TBYOMEI     PIC X(200).
                               13  SPA-GMN-TBYOCD      PIC X(07).
                               13  SPA-GMN-TIKOSAKICD  PIC X(04).
                               13  SPA-GMN-THAISIYMD   PIC X(10).
                       09  SPA-GMN-BYOMEI-MAX          PIC 9(03).
                       09  SPA-GMN-BYOMEICD-TBL.
                           11  SPA-GMN-BYOMEICD-G      OCCURS  21.
                               13  SPA-GMN-TBYOMEICD1  PIC X(07).
                               13  SPA-GMN-TBYOMEI1    PIC X(200).
                       09  SPA-GMN-BYOMEICD-MAX        PIC 9(02).
      *
                       09  SPA-GMN-SELNUM1             PIC 9(05).
                       09  SPA-GMN-SELNUM2             PIC 9(02).
      *
                   07  SPA-NAIB-AREA.
                       09  SPA-NAI-KOUMOKU.
                           11  SPA-NAI-SYOBYO          PIC 9(01).
                           11  SPA-MAX-BYOCD2          PIC 9(02).
                       09  SPA-NAI-BYOMEI-TBL.
                           11  SPA-NAI-BYOMEI-G        OCCURS  100.
                               13  SPA-NAI-THAISIYMDH  PIC X(22).
                               13  SPA-NAI-THAISIYMD   PIC X(08).
                               13  SPA-NAI-TIKOSAKICD  PIC X(07).
                       09  SPA-NAI-BYOKEN              PIC X(40).
      *                病名コード移行用領域
                       09  SPA-C05-IKOSAKI-BYOMEICD
                                                       PIC X(07).
                       09  SPA-C05-IKOSAKI-BYOMEI      PIC X(200).
                       09  SPA-C05-IKOSAKI-INDEX       PIC 9(02).  
                       09  SPA-C05-IKOMOTO-BYOMEICD
                                                       PIC X(07).
                       09  SPA-C05-IKOMOTO-BYOMEI      PIC X(200).
      *
                   07  SPA-NAI-SELNO1                  PIC 9(04).                             
                   07  SPA-NAI-SELNO2                  PIC 9(02).                             
