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
      *    レセプト管理                       (CPRCF007.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02.02.12  人数の追加
      *  01.00.02    MCC-藤原     02.02.25  索引ファイルからＤＢへ変更
      *
      *  01.01.01    NACL-藤原    02.09.26  入外区分・処理区分の追加
      *  01.01.02    NACL-藤原    05.01.13  処理種別・日付指定の追加
      *
      *  02.07.02    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *                                     県内県外印刷区分
      *                                     主科コード
      *                                     保険者印刷区分
      *                                     レセプト種別印刷区分
      *                                     保険者番号
      *                                     在総診・長期（長期高額療養費）
      *                                     在総診・長期のページ
      *
      *  03.04.01    NACL-藤原    07/03/01  国保レセプト編綴順詳細設定対応
      *                                     在総診印刷設定区分
      *                                     長期（長期高額療養費）印刷設定区分
      *                                     高額点数フラグ
      *                                     高額点数頁
      *
      *  04.03.01    NACL-藤原    08/07/14  仮レセ作成（院外処方含む）対応
      ***********************************************************
      *01  RECE07-REC.
      *ＫＥＹ
           03  RECE07-KEY.
      *医療機関ＩＤ
               05  RECE07-HOSPNUM          PIC 9(2).
      *帳票ＩＤ
               05  RECE07-PRTID            PIC  X(04).
      *処理区分（１：一括、２：個別）
               05  RECE07-SYOKBN           PIC  X(01).
      *作成日
               05  RECE07-CREYMD           PIC  X(08).
      *作成時間
               05  RECE07-CREHMS           PIC  X(06).
      *審査支払機関
               05  RECE07-TEISYUTUSAKI     PIC  X(01).
      *主科コード（社保のみ使用）
               05  RECE07-SRYKA            PIC  X(02).
      *県内県外印刷区分（国保のみ使用）
               05  RECE07-PREFKBN-SRT      PIC  X(02).
      *保険者印刷区分（国保のみ使用）
               05  RECE07-HKNJANUM-SRT     PIC  X(01).
      *保険者番号（国保のみ使用、99999999は編綴順指定無しで作成）        
               05  RECE07-HKNJANUM         PIC  X(08).
      *レセプト種別
               05  RECE07-RECESYUBETU      PIC  X(04).
      *レセプト種別印刷区分（国保のみ使用）
               05  RECE07-RECESYUBETU-SRT  PIC  X(03).
      *在総診（未使用）
               05  RECE07-ZAITAKU          PIC  X(01).
      *長期（長期高額療養費）（未使用）
               05  RECE07-CHOUKI           PIC  X(01).
      *高額点数
           03  RECE07-KOUGAKU              PIC  X(01).
      *入外区分
           03  RECE07-NYUGAIKBN            PIC  X(01).
      *診療年月（一括：診療年月、個別：先頭の診療年月）
           03  RECE07-SRYYM                PIC  X(06).
      *ページ
           03  RECE07-PAGE                 PIC  9(05).  
      *ページ
           03  RECE07-PAGE-ZAITAKU         PIC  9(05).  
      *ページ
           03  RECE07-PAGE-CHOUKI          PIC  9(05).  
      *ページ
           03  RECE07-PAGE-KOUGAKU         PIC  9(05).  
      *ページ
           03  RECE07-PAGE-DUP             PIC  9(05).  
      *件数
           03  RECE07-KENSU                PIC  9(05).
      *データ区分（０:全体 １:社保 ２:国保）
           03  RECE07-RECEKBN              PIC  X(01).
      *処理種別
           03  RECE07-SYORISBT             PIC  X(01).
      *日付指定（開始）
           03  RECE07-STYMD                PIC  X(08).
      *日付指定（終了）
           03  RECE07-EDYMD                PIC  X(08).
      *編綴順情報
           03  RECE07-HSEQNUM              PIC  9(08).
      *先頭種別（詳細設定時）
           03  RECE07-SYUBETU-SRT1         PIC  X(04).
      *編綴順詳細設定
           03  RECE07-RECE-PUTH.
      *県内県外区分
               05  RECE07-PREFKBN          PIC  X(01).
      *在総診
               05  RECE07-ZAITAKU-KBN      PIC  X(01).
      *長期（長期高額療養費）
               05  RECE07-CHOUKI-KBN       PIC  X(01).
      *最終出力順
               05  RECE07-SRT-LAST         PIC  X(01).
      *高額点数
               05  RECE07-KOUGAKU-TEN      PIC  9(07).
      *レセプト作成区分（１：点検用）
               05  RECE07-PRTKBN           PIC  X(01).
      *端末ＩＤ
           03  RECE07-TERMID               PIC  X(64).
      *オペレータＩＤ
           03  RECE07-OPID                 PIC  X(16).
