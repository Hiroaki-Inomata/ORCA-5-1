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
      *            画面　Ｃ０３で使用 
      *          
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/08/20  病名コード移行用項目の追加
      *  01.00.02    NACL-藤原    03/05/16  コラムリストの分類名を３０に変更
      *
      *  01.01.01    NACL-藤原    03/08/28  登録数の制限をなくす
      *                                     コード別編集病名フラグを追加
      *
      *  03.01.01    NACL-藤原    06/06/01  病名入力コードのアルファベッ
      *                                     ト７文字対応
      *****************************************************************
      *
      *01  SPA-SCR-REC.
      *
           03  SPA-C03-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(04).
               05  SPA-GMN-CUR         PIC 9(02).
               05  SPA-GMN-NEXT        PIC 9(01).
               05  SPA-GMN-AREA.
                   07  SPA-GMN.
                       09  SPA-GMN-BUNRUICD-TBL.
                           11  SPA-GMN-BUNRUICD-TBLO   OCCURS   100.
                               13  SPA-GMN-BBUNRUICD   PIC 9(03).
                               13  SPA-GMN-BBUNRUIMEI  PIC X(30).
                           11  SPA-GMN-BUNRUI-MAX      PIC 9(04).
      *
                       09  SPA-GMN-BYOMEI-TBL.
                           11  SPA-GMN-TBLO            OCCURS   100.
                               13  SPA-GMN-TBANGO      PIC 9(05).
                               13  SPA-GMN-TBUNRUIMEI  PIC X(30).
                               13  SPA-GMN-TBYOMEIINPUTCD
                                                       PIC X(10).
                               13  SPA-GMN-TBYOMEISBT  PIC X(06).
                               13  SPA-GMN-TBYOMEI     PIC X(80).
                               13  SPA-GMN-TCHARTBYOMEI
                                                       PIC X(80).
                               13  SPA-GMN-TUTAGAIFLG  PIC X(14).
                               13  SPA-GMN-TMANSEIKBN  PIC X(42).
                               13  SPA-GMN-THKNBYOMEIFLG
                                                       PIC X(08).
                               13  SPA-GMN-TDSPSEQ     PIC 9(02).
      *                 
                       09  SPA-GMN-KOUMOKU.
                         10  SPA-GMN-BUNRUICD          PIC 9(03).
                         10  SPA-GMN-BUNRUIMEI         PIC X(30).
                         10  SPA-GMN-KOUMOKU1.
                           11  SPA-GMN-BANGO           PIC 9(05).
                           11  SPA-GMN-BYOMEIINPUTCD   PIC X(10).
                           11  SPA-GMN-BYOMEICD-G.
                               13  SPA-GMN-BYOMEICD    OCCURS  6
                                                       PIC X(08).
                           11  SPA-GMN-UTAGAIFLG-G.
                               13  SPA-GMN-UTAGAIFLG   PIC X(01).
                               13  SPA-GMN-F1          PIC X(01).
                               13  SPA-GMN-UTAGAIFLGMEI
                                                       PIC X(12).
                           11  SPA-GMN-MANSEIKBN-G.
                               13  SPA-GMN-MANSEIKBN   PIC X(02).
                               13  SPA-GMN-F1          PIC X(01).
                               13  SPA-GMN-MANSEIKBNMEI
                                                       PIC X(42).
                           11  SPA-GMN-BYOMEI          PIC X(80).
                           11  SPA-GMN-CHARTBYOMEI     PIC X(80).
                           11  SPA-GMN-BYOMEIHENFLG    PIC X(09).
                           11  SPA-GMN-DSPSEQ          PIC 9(02).
                           11  SPA-GMN-HKNBYOMEIFLG-G.
                               13  SPA-GMN-HKNBYOMEIFLG    PIC X(01).
                               13  SPA-GMN-F1              PIC X(01).
                               13  SPA-GMN-HKNBYOMEIFLGMEI PIC X(08).
                           11  SPA-GMN-BYOMEISBT-G.
                               13  SPA-GMN-BYOMEISBT       PIC X(01).
                               13  SPA-GMN-F1              PIC X(01).
                               13  SPA-GMN-BYOMEISBTMEI    PIC X(06).
      *
                       09  SPA-GMN-UTAGAIFLGLST        OCCURS  5.
                           11  SPA-GMN-UTAGAIFLGL      PIC X(01).
                           11  SPA-GMN-F1L             PIC X(01).
                           11  SPA-GMN-UTAGAIFLGMEIL   PIC X(12).
                       09  SPA-GMN-UTAGAIFLG-MAX       PIC 9(02).        
                       09  SPA-GMN-MANSEIKBNLST        OCCURS  5.
                           11  SPA-GMN-MANSEIKBNL      PIC X(02).
                           11  SPA-GMN-F1L             PIC X(01).
                           11  SPA-GMN-MANSEIKBNMEIL   PIC X(42).
                       09  SPA-GMN-MANSEITANLST        OCCURS  5.
                           11  SPA-GMN-MANSEIKBNTANL   PIC X(10).    
                       09  SPA-GMN-MANSEIKBN-MAX       PIC 9(02).        
                       09  SPA-GMN-HKNBYOMEIFLGLST     OCCURS  5.
                           11  SPA-GMN-HKNBYOMEIFLGL   PIC X(01).
                           11  SPA-GMN-F1L             PIC X(01).
                           11  SPA-GMN-HKNBYOMEIFLGMEIL
                                                       PIC X(08).
                       09  SPA-GMN-HKNBYOMEIFLG-MAX    PIC 9(02).                                    
                       09  SPA-GMN-BYOMEISBTLST        OCCURS  4.
                           11  SPA-GMN-BYOMEISBTL      PIC X(01).
                           11  SPA-GMN-F1L             PIC X(01).
                           11  SPA-GMN-BYOMEISBTMEIL   PIC X(06).
                       09  SPA-GMN-BYOMEISBT-MAX       PIC 9(02).   
      *
                   07  SPA-NAIB-AREA.
                       09  SPA-NAI-KOUMOKU.
                           11  SPA-NAI-BYOMEIINPUTCD   PIC X(10).
                           11  SPA-NAI-CDSYU           PIC X(01).
                           11  SPA-NAI-BYOMEIMOJI      PIC 9(02).
                           11  SPA-NAI-BYOMEIHENFLG    PIC X(01).
                           11  SPA-NAI-CHARTBYOMEIMOJI PIC 9(02).
                           11  SPA-NAI-BYOMEI-TBL.
                               13  SPA-NAI-BYOMEI-OCC  OCCURS  21.
                                   15  SPA-NAI-BYOMEI-G.
                                       17  SPA-NAI-BYOMEICD
                                                       PIC X(07).
                                       17  SPA-NAI-BYOMEI  
                                                       PIC X(80).
                                       17  SPA-NAI-UTAGAIFLG
                                                       PIC X(01).
                                       17  SPA-NAI-TOKSKNCD
                                                       PIC 9(02).
                                       17  SPA-NAI-BYOMEIHENFLG-O
                                                       PIC X(01).
                           11  SPA-NAI-UPDFLG          PIC 9(01). 
      *
                       09  SPA-NAI-TBL.
                           11  SPA-NAI-TBLO         OCCURS   100.
                               13  SPA-NAI-TCDSYU      PIC X(01).    
      *                         
                       09  SPA-NAI-SELNO                PIC 9(03).
                       09  SPA-NAI-BUNRUI-SELNO         PIC 9(03).
      *            病名コード移行用領域
                       09  SPA-C03-IKOSAKI-BYOMEICD
                                                   PIC X(07).
                       09  SPA-C03-IKOSAKI-BYOMEI  PIC X(200).
                       09  SPA-C03-IKOSAKI-INDEX   PIC 9(02).  
                       09  SPA-C03-IKOMOTO-BYOMEICD
                                                   PIC X(07).
                       09  SPA-C03-IKOMOTO-BYOMEI  PIC X(200).
