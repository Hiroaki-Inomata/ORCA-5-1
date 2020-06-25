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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 SEIKYU4705.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 沖縄・複写式レセ
      *                      (121)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/06/15    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/08/10  日レセver4.2でインストールすると
      *                                  エラーが起きるのを修正
      *  01.00.02    吉川    2009/09/28  帳票様式の変更に伴う修正
      *  01.00.03    吉川    2009/10/27  差引請求額欄は、社保・国保の場合
      *                                  1の位を四捨五入、後期高齢は切捨て
      *  01.00.04    吉川    2010/06/11  差引請求額欄は後期高齢も四捨五入する
      *  01.00.05    吉川    2010/11/22  差引請求額欄が０円の場合は
      *                                  ０と記載するように修正
      *  01.00.06    吉川    2010/11/25  open-cobol1.0対応
      *  01.00.07    吉川    2011/07/12  所得者情報は診療月時点での情報を
      *                                  参照するように修正
      *  01.00.08    吉川    2013/01/17  テーブルのprimary key変更対応
      *  01.00.09    吉川    2013/01/22  スタックオーバーフロー対応
      *                                  医療機関情報編集対応
      *  01.00.10    吉川    2013/02/12  プラグイン対応
      *  01.00.11    吉川    2013/10/09  プログラムオプション対応
      *  01.00.12    吉川    2014/10/28  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.13    萩野    2016/04/14  医療機関編集情報への開設者名追加対応
      *  01.00.14    吉川    2017/01/17  対象公費はPGオプションから取得する
      *                                  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.15    吉川    2017/04/13  医療機関情報の取得の修正
      *  01.00.16    吉川    2017/09/14  レセプトを複数の条件で
      *                                  作成した場合の不具合対応
      *  01.00.17    門間    2017/10/05  SITEKBN変更（動作に変更なし）
      *  01.00.18    門間    2018/02/05  保険番号063、072〜075は組合健保に「〇」を印字
      *                                  保険番号063の場合、「特退」を印字するよう修正
      *  01.00.19    井上    2018/11/12  修正履歴コメント削除
      *  01.00.20    門間    2019/05/08  改元対応
      *                      2019/05/10  摘要欄の印字ずれを修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL. 
      *    編集ファイル
           SELECT  MF100-FILE      ASSIGN    MF100PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  MF100-KEY
                                   FILE    STATUS  IS  STS-MF100.
      *
      *    レセプト０３１データ
           SELECT  RECE031-FILE     ASSIGN  RECE031PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE.
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    編集ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI4705.INC".
      *
      *    レセプト０３１データ
       FD  RECE031-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //MF100//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE031//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    レセプト印刷データ用領域
           COPY    "HCM11V07.INC"  REPLACING  //HCM11V07//
                                   BY         //HC063//.
      *
      *    帳票領域
      *    外来用
           COPY    "SC4705.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-MF100           PIC X(02).
           03  STS-SYUTURYOKU      PIC X(02).
           03  STS-RECE            PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END         PIC 9(03).
           03  FLG-RECE08      PIC 9(01).
           03  FLG-RECE031     PIC 9(01).
           03  FLG-KANRIMST    PIC 9(01).
           03  FLG-KUGIRI      PIC 9(01).
           03  FLG-RECE07      PIC 9(01).
           03  FLG-RECECOM     PIC 9(01).
           03  FLG-ERR         PIC 9(01).
           03  FLG-PTINF       PIC 9(01).
           03  FLG-SYSKANRI    PIC 9(01).
           03  FLG-SYSKANRI3   PIC 9(01).
           03  FLG-SYUNOU      PIC 9(01).
           03  FLG-HKNCOMBI    PIC 9(01).
           03  FLG-TENKI       PIC 9(01).
           03  FLG-SYORI       PIC 9(01).
           03  FLG-BTPARA      PIC 9(01).
           03  FLG-SEIKYU      PIC 9(01).
           03  FLG-PTKOHINF    PIC 9(01).
           03  FLG-PTKOHFTN    PIC 9(01).
           03  FLG-HEIYOU      PIC 9(01).
           03  FLG-TYOKI       PIC 9(01).
           03  FLG-PRGOPTION   PIC 9(01).
           03  FLG-OK          PIC 9(01).
           03  FLG-TAISYO      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR         PIC 9(06).
           03  CNT-RECE02      PIC 9(06).
           03  CNT-OUT         PIC 9(06).
           03  CNT-ZAIMEISAISU PIC 9(03).
           03  CNT-PAGE        PIC 9(03).
           03  CNT-KENSU       PIC 9(03).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW             PIC 9(04).
           03  IDX             PIC 9(05).
           03  IDXX            PIC 9(04).
           03  IDY             PIC 9(04).
           03  IDZ             PIC 9(04).
           03  IDA             PIC 9(04).
           03  IDB             PIC 9(04).
           03  IDX1            PIC 9(02).
           03  IDX2            PIC 9(02).
           03  IDX3            PIC 9(02).
           03  IDX4            PIC 9(03).
           03  IDX5            PIC 9(03).
           03  IDXP            PIC 9(02).
           03  IDY1            PIC 9(03).
           03  IDY2            PIC 9(03).
           03  IDY3            PIC 9(03).
      *
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-TERMID     PIC X(16).
           03  WRK-PARA-SYORIFLG   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *    患者病名編集用パラメタ領域
       01  WRK-PARA-LINK.
           05  WRK-PARA-LINK-SRYYM   PIC  X(06).
           05  WRK-PARA-LINK-TERMID  PIC  X(16).
           05  WRK-PARA-LINK-SYSYMD  PIC  X(08).
           05  WRK-PARA-LINK-JOBID   PIC  9(07).
           05  WRK-PARA-LINK-SHELLID PIC  X(08).
           05  WRK-PARA-LINK-HOSPNUM PIC  9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-RECENUM         PIC 9(06).
           03  WRK-KOHID           PIC 9(10).
      *
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-BEDSU           PIC ZZ9.
           03  WRK-CSTTENSUX       PIC ZZZZZ9.
           03  WRK-PAGE            PIC 9(03).
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-FTNJANUM        PIC X(20).
           03  WRK-JKYSNUM         PIC X(20).
           03  WRK-SIDOU           PIC 9(09).
           03  WRK-YM              PIC 9(06).
           03  WRK-HOSPNUM         PIC 9(02).
           03  WRK-PTID            PIC 9(10).
           03  WRK-HKNCOMBI        PIC 9(04).
      *    ファイル名称用エリア
           03 WRK-CREYMD           PIC X(08).
           03 WRK-CREHMS           PIC X(06).
      *
           03  WRK-PATH              PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
           03  WRK-STUNUM          PIC X(02).
      *
      *    医療機関情報
           03  WRK-HOSPNAME          PIC X(100).
           03  WRK-HOSPNAME-R        REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1     PIC X(50).
               05  WRK-HOSPNAME2     PIC X(50).
           03  WRK-HOSPADRS          PIC X(100).
           03  WRK-HOSPADRS-R        REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1     PIC X(50).
               05  WRK-HOSPADRS2     PIC X(50).
           03  WRK-HOSPTEL           PIC X(15).
           03  WRK-KAISETUNAME       PIC X(80).
      *
           03  WRK-KASAN-TBL.
               05  WRK-KEISAN        PIC 9(09).
               05  WRK-AMARI         PIC 9(07).
               05  WRK-SEIKYUGK      PIC 9(09)V9(01).
               05  WRK-SEIKYUGK-G    PIC 9(09)V9(01).
               05  WRK-SEIKYUGK-G2   PIC 9(09).
           03  WRK-FTNMONEY-G        PIC 9(08).
      *
      *    患者病名情報退避エリア
           03  WRK-BYOMEI-TAIHI-TBL.
               05  WRK-TAIHI-BYOMEI   PIC X(54) OCCURS 5.
               05  WRK-TAIHI-SRYYMD   PIC X(26) OCCURS 5.
               05  WRK-TAIHI-TENKI    PIC X(10) OCCURS 5.
               05  WRK-TAIHI-TENKIYMD PIC X(06) OCCURS 5.
               05  WRK-TAIHI-TEKIYO   PIC X(52) OCCURS 35.
      *
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM-TBL.
               05  WRK-TAISYO-KOHNUM PIC X(03)  OCCURS 100.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-BYO-OLD.
               05  WRK-BO-SYUBYOFLG            PIC X(01).
               05  WRK-BO-SRYYMD               PIC X(08).
               05  WRK-BO-TENKIKBN             PIC X(01).
               05  WRK-BO-TENKIYMD             PIC X(08).
      *
           03  WRK-EDIT-AREA.
      *        傷病名連番     
               05  WRK-LINE                    PIC 9(02).
      *        編集傷病名文字数 
               05  WRK-CLM                     PIC 9(03).
      *        ワーク編集傷病名文字数 
               05  WRK-WCLM                    PIC 9(03).
      *        病名文字数         
               05  WRK-MOJISU                  PIC 9(03).
      *        編集開始位置         
               05  WRK-STCLM                   PIC 9(03).
      *        傷病名欄編集行         
               05  WRK-LINEBYO                 PIC 9(02).
      *        適用欄編集行         
               05  WRK-LINETEK                 PIC 9(02).
      *        前行の続きのとき（傷病名）         
               05  WRK-CONTINUE                PIC 9(01).
      *    傷病名編集
           03  WRK-BYOMEI                      PIC X(150).
           03  WRK-BYOMEIW                     PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1                     PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2                     PIC X(01). 
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1                     PIC X(54).
           03  WRK-BYOMEI1-W   REDEFINES       WRK-BYOMEI1.
               05  WRK-BYOMEI1-NO              PIC X(06).
               05  WRK-BYOMEI1-NAME            PIC X(48).
               
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2                     PIC X(52).
           03  WRK-BYOMEI2-W   REDEFINES       WRK-BYOMEI2.
               05  WRK-BYOMEI2-NO              PIC X(08).
               05  WRK-BYOMEI2-NAME            PIC X(44).
               05  WRK-BYOMEI2-NAMEW   REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(18).
                   07  WRK-BYOMEI2-ELSE.
                       09  FILLER              PIC X(02).
                       09  WRK-BYOMEI2-YMD-G   PIC X(02).
                       09  WRK-BYOMEI2-YMD     PIC X(18).
                       09  WRK-BYOMEI2-TENKI   PIC X(04).
               05  WRK-BYOMEI2R-NAMEW  REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(12).
                   07  WRK-BYOMEI2R-ELSE.
                       09  FILLER              PIC X(02).
                       09  WRK-BYOMEI2R-YMD-G  PIC X(02).
                       09  WRK-BYOMEI2R-YMD    PIC X(18).
                       09  WRK-BYOMEI2R-TENKI  PIC X(04).
                       09  WRK-BYOMEI2R-TENKIYMD
                                               PIC X(06).
      *                 
           03  WRK-2003-TBL OCCURS 200.
               05  WRK-2003-TEISYUTUSAKI       PIC X(01).
               05  WRK-2003-NYUGAIKBN          PIC X(01).
               05  WRK-2003-DSPSEQ             PIC 9(03).
               05  WRK-2003-SYUBETU-G.
                   07  WRK-2003-SYUBETU        PIC X(04) OCCURS 10.
      *
           03  WRK-SRYYMD                      PIC X(08).
           03  WRK-Z9                          PIC Z,ZZZ,ZZZ.
           03  WRK-Z10                         PIC ZZ,ZZZ,ZZZ.
           03  WRK-KGKZ                        PIC ZZZ,ZZZ,ZZZ.
           03  WRK-KGK9                        PIC ZZZ,ZZZ,ZZ9.
           03  WRK-KGK2                        PIC Z9.
           03  WRK-KGK6                        PIC ZZZ,ZZ9.
           03  WRK-KGK7                        PIC ZZZZ,ZZ9.
           03  WRK-KGK10                       PIC ZZ,ZZZ,ZZ9.
      *
           03  WRK-KYUFUGAI                    PIC 9(08).
           03  WRK-KOHNUM                      PIC X(03).
      *
      *
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   48.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(02)   VALUE   44.
      *    適用欄最大行数
           03  WRK-TEKIYOU-LINE    PIC 9(02)   VALUE   35.
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   5.
      *
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.       
      *
      *    区切り線
       01  WRK-LINE-AREA.
           03  WRK-KUGIRI1         PIC X(52)   VALUE
               ALL "−".
           03  WRK-KUGIRI2.
               05  FILLER          PIC X(22)   VALUE
                                   "＝＝＝＝＝＝＝　以下　".
               05  WRK-KUGIRI2-KOHNAME         PIC X(10).
               05  FILLER          PIC X(20)   VALUE
                                   "　適用分　＝＝＝＝＝".
           03  WRK-KUGIRI3         PIC X(52)   VALUE
               ALL "−".
      *
      *    改行コード
       01  WRK-CONS-RETURN          PIC X(01)   VALUE   X"0A".
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTEN.
               05  WRK-KHNTENZ     PIC  ZZZZZZZ.
           03  WRK-KAISU.
               05  WRK-KAISUZ      PIC  ZZZ.
           03  WRK-KOUTEN.
               05  WRK-KOUTENZ     PIC  ZZZZZZZ.
           03  WRK-TENSU.
               05  WRK-TENSUZ      PIC  ZZZZZZZ.
           03  WRK-JINISSU.
               05  WRK-JINISSUZ    PIC  ZZ.
           03  WRK-Z2              PIC  ZZ.
           03  WRK-Z5              PIC  Z,ZZZ.
      *
       01  WRK-CNT                 PIC  9(5).
      *
      *    固定項目エリア
        01  WRK-CONS-AREA.
      *    疑い
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".   
      *    急性
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".   
      *    レセプトフォームID
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK2005.INC".
      *
      *    レセプト作成指示(印刷順)情報
           COPY    "CPSK2003.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
           COPY    "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者公費負担
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    レセプト明細
           COPY    "CPRCF008.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  RECEERR
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID    
                                               LNK-PRTKANRI-PRTNM    
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SYORIFLG
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    地方公費対象患者用病名ファイル作成
           PERFORM 1001-BYOMEI-HEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU4705"    TO  JOB-PGID
           MOVE    "複写式レセ"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    ファイルＯＰＥＮ処理
           PERFORM 110-FILE-OPEN-SEC
      *
      *    システム管理マスタ（医療機関情報）
           PERFORM 120-HOSPINF-GET-SEC
      *
           MOVE    ZERO                TO  WRK-RECENUM
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
       
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU4705"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    レセプト出力順
           MOVE    "RECESYUTURYOKU"          TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(22:2)        TO  WRK-STUNUM
      *    正しいデータでは無い時は０１に置き換える
           IF  WRK-STUNUM  NOT =  "01" AND "02" AND "03" AND "04"
             MOVE    "01"                    TO  WRK-STUNUM
           END-IF
           DISPLAY "出力順変換="  WRK-STUNUM
      *
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  WRK-TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費番号="  WRK-TAISYO-KOHNUM(IDX)
           END-PERFORM
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタセット処理
      *****************************************************************
       1001-OPTION-SET-SEC              SECTION.
      *
           INITIALIZE                            WRK-OPTION-TBL
                                                 FLG-OK
      *    プログラムオプション情報読込
           PERFORM  900-PRGOPTION-READ-SEC
      *
      *    オプション数の初期設定
           MOVE    1                         TO  IDX1
           IF  FLG-PRGOPTION  =  ZERO
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL   (IDX     >   44000 )
                         OR   (IDX1    >  100)
                         OR   (FLG-OK  NOT =   ZERO)
      *
      *        オプションの1桁目が空白であれば処理終了
               IF   PRGOPTION-OPTION(IDX:1)  =  SPACE
               AND  IDY                      =  ZERO
                   MOVE    1                 TO  FLG-OK
               END-IF
      *
               IF  FLG-OK  =  ZERO
      *          改行地点か
                 IF  PRGOPTION-OPTION(IDX:1)  =   X"0A"
                 OR  IDY  >=  100
                   ADD    1                  TO  IDX1
                   MOVE    ZERO              TO  IDY
                 ELSE
                   ADD    1                  TO  IDY
      *            PGオプション内容を１文字ずつセット
                   MOVE    PRGOPTION-OPTION(IDX:1)
                                         TO  WRK-OPTION(IDX1)(IDY:1)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       1001-OPTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費対象患者用病名ファイル作成
      *****************************************************************
       1001-BYOMEI-HEN-SEC         SECTION.
      *
           INITIALIZE  WRK-PARA-LINK
           MOVE  LNK-PRTKANRI-SRYYM   TO   WRK-PARA-LINK-SRYYM
           MOVE  LNK-PRTKANRI-TERMID  TO   WRK-PARA-LINK-TERMID
           MOVE  LNK-PRTKANRI-SKYYMD  TO   WRK-PARA-LINK-SYSYMD
           MOVE  WRK-PARA-JOBID       TO   WRK-PARA-LINK-JOBID
           MOVE  WRK-PARA-SHELLID     TO   WRK-PARA-LINK-SHELLID
           MOVE  WRK-PARA-HOSPNUM     TO   WRK-PARA-LINK-HOSPNUM
      * 
           DISPLAY "編集１開始"
           CALL  "SEIKYU47BYOMEI1"    USING  WRK-PARA-LINK
                                             WRK-TAISYO-KOHNUM-TBL
           DISPLAY "編集２開始"
           CALL  "SEIKYU47BYOMEI2"    USING  WRK-PARA-LINK
           DISPLAY "病名ファイル作成終了"
      *
           .
       1001-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF100PARA-TERMID
      *
           MOVE    "RECE031"           TO  RECE031PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID        TO  RECE031PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM      TO  RECE031PARA-HOSPNUM
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECE031PARA-BASENAME TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF100PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECE031PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
           OPEN    INPUT               RECE031-FILE
           IF  STS-RECE            NOT =   ZERO
               DISPLAY "*** SEIKYU4705 RECE031 OPEN ERR  ***"
           END-IF
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報検索
      *****************************************************************
       120-HOSPINF-GET-SEC            SECTION.
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"        TO  SYS-1001-KANRICD
           MOVE  "*"           TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1001-STYUKYMD(7:2)
                                       SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
      *
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF
           ELSE
             DISPLAY "*** SEIKYU4705 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"        TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM   TO  SYS-1002-HOSPNUM
      *
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU4705 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *    レセプト編集区分情報
           INITIALIZE                   SYS-2005-REC
           MOVE    "2005"           TO  SYS-2005-KANRICD
           MOVE    "*"              TO  SYS-2005-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                   TO  SYS-2005-STYUKYMD(1:6)
                                       SYS-2005-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-2005-STYUKYMD(7:2)
                                       SYS-2005-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-2005-HOSPNUM
      *
           MOVE  SYS-2005-REC       TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN    =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN
           END-IF
           IF      SYS-2005-TENKIHENKBN     =   SPACE
               MOVE    ZERO             TO  SYS-2005-TENKIHENKBN
           END-IF
      *
      *    システム管理マスタよりレセプト出力順を取得
           MOVE    "2003"                 TO  SYS-2003-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                   TO  SYS-2003-STYUKYMD(1:6)
                                       SYS-2003-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-2003-STYUKYMD(7:2)
                                       SYS-2003-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-2003-HOSPNUM
      *
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE    1       TO      IDX
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
                    OR     IDX    > 200
             IF  FLG-SYSKANRI3 = ZERO
                 MOVE SYS-2003-TEISYUTUSAKI  
                               TO WRK-2003-TEISYUTUSAKI(IDX)
                 MOVE SYS-2003-SYUBETU-G
                               TO WRK-2003-SYUBETU-G(IDX)
                 MOVE SYS-2003-NYUGAIKBN
                               TO WRK-2003-NYUGAIKBN(IDX)
                 MOVE SYS-2003-DSPSEQ
                               TO WRK-2003-DSPSEQ(IDX)
                 ADD  1        TO     IDX 
             END-IF  
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           DISPLAY "2003 IDX=" IDX
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       1001-PARA-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDXX
                                           FLG-SYORI
           INITIALIZE                      TABLE-AREA
      *
           IF      WRK-PARA-SYORIFLG   =   "1" 
               INITIALIZE                      BTPARA-REC
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               ELSE
                   INITIALIZE                  BTPARA-REC
                   MOVE    1               TO  FLG-BTPARA
               END-IF
      *
               PERFORM     UNTIL   FLG-BTPARA  =   1
                   ADD     1                   TO  IDXX
                   IF      BTPARA-INFO-KBN     =   "1"
                     MOVE    1                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PARA
                                       TO TBL-PARA-HKNJANUM(IDXX)
                   ELSE
                     MOVE    2                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PTID
                                       TO TBL-PARA-PTID(IDXX)
                   END-IF
                   DISPLAY "パラメタ区分=" BTPARA-INFO-KBN
                           "パラメタ=" BTPARA-INFO-PARA
                           "保険者番号="   TBL-PARA-HKNJANUM(IDXX)
                           "患者番号="     TBL-PARA-PTID(IDXX)
      *
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      IDXX            =   ZERO
                   MOVE    ZERO            TO  WRK-PARA-SYORIFLG
               END-IF
           END-IF 
           .
       1001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT  MF100-FILE
           CLOSE           MF100-FILE
           OPEN    I-O     MF100-FILE
      *
           DISPLAY "2001-START"
           PERFORM 2001-SYUKEI-SEC
                   UNTIL   FLG-END     =   1
           DISPLAY "2001-END"
      *
           CLOSE   MF100-FILE
      *
           IF  CNT-RECE02  >       0
               PERFORM   2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ抽出処理
      *****************************************************************
       2001-SYUKEI-SEC         SECTION.
      *
      *
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
               INITIALIZE                          WRK-CHK-KOHNUM-TBL
               MOVE    RECE02-KOHNUM(IDZ)      TO  WRK-CHK-KOHNUM(1)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
               PERFORM 20011-SYUKEI-SEC
             END-IF
           END-PERFORM
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
      *    TMP-FILE キーセット
      *
           INITIALIZE  MF100-REC
           MOVE  ZERO                     TO  FLG-ERR
                                              WRK-SEIKYUGK-G
      *
           MOVE  RECE02-HOSPNUM           TO  MF100-HOSPNUM
           MOVE  RECE02-SRYYM             TO  MF100-SRYYM
           MOVE  RECE02-TEISYUTUSAKI      TO  MF100-TEISYUTUSAKI
           MOVE  RECE02-NYUGAIKBN         TO  MF100-NYUGAIKBN
           MOVE  RECE02-HKNJANUM          TO  MF100-HKNJANUM2
           MOVE  RECE02-PTID              TO  MF100-PTID
           MOVE  RECE02-PTNUM             TO  MF100-PTNUM
           MOVE  RECE02-RECEKA            TO  MF100-RECEKA
           MOVE  RECE02-RECESYUBETU       TO  MF100-RECESYUBETU
           MOVE  RECE02-HOJOKBN-KEY       TO  MF100-HOJOKBN-KEY
           MOVE  RECE02-TEKSTYMD          TO  MF100-TEKSTYMD
           MOVE  RECE02-PTNUM             TO  MF100-PTNUM
           MOVE  RECE02-KANANAME          TO  MF100-KANANAME
           MOVE  RECE02-NAME              TO  MF100-NAME
           MOVE  RECE02-SEX               TO  MF100-SEX
           MOVE  RECE02-BIRTHDAY          TO  MF100-BIRTHDAY
           MOVE  RECE02-AGE               TO  MF100-AGE
           MOVE  RECE02-HKN               TO  MF100-HKN
           MOVE  RECE02-RJNHKN            TO  MF100-RJNHKN
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     10
             MOVE  RECE02-HKNCOMBIINF(IDX1)  TO
                                           MF100-HKNCOMBIINF(IDX1)
           END-PERFORM
      *
           MOVE  RECE02-KOHINF(IDZ)       TO  MF100-KOHINF
           MOVE  MF100-KOHFTNJANUM        TO  MF100-FTNJANUM-KEY
           MOVE  MF100-KOHJKYSNUM         TO  MF100-JKYSNUM-KEY
           MOVE  MF100-KOHID              TO  MF100-KOHID-KEY
           COMPUTE IDY     =   IDZ +   1
           MOVE  RECE02-JNISSU(IDY)       TO  MF100-JNISSU
           MOVE  RECE02-TOTALTEN(IDY)     TO  MF100-TOTALTEN
           MOVE  RECE02-SHOKUJIINF(IDY)   TO  MF100-SHOKUJIINF
      *
           MOVE  RECE02-SHOSHINTIME       TO  MF100-SHOSHINTIME
           MOVE  RECE02-ZAITAKU           TO  MF100-ZAITAKU
           MOVE  RECE02-ZAITAKU-SAIKEI    TO  MF100-ZAITAKU-SAIKEI
      *    給付外の額
           MOVE    RECE02-FTNMONEY (IDY)  TO  MF100-KYUFUGAITEN
      *    請求マスタ読込
           PERFORM  900-SEIKYU-SET-SEC
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     45
             MOVE  RECE10-SRYKBNINF(IDX1) TO  MF100-SRYKBNINF(IDX1)
           END-PERFORM
      *
      *    給付外点数集計
           MOVE    ZERO                   TO  WRK-KYUFUGAI
                                              WRK-FTNMONEY-G
           MOVE    MF100-KOHNUM           TO  WRK-KOHNUM
           MOVE    MF100-KOHID            TO  WRK-KOHID
           DISPLAY "給付外点数集計開始  患者番号=" MF100-PTNUM
                   "保険番号=" MF100-HKNNUM
                   "  公費番号=" WRK-KOHNUM
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"     TO  WRK-PATH
           ELSE
               MOVE    "key25"     TO  WRK-PATH
           END-IF
      *
           PERFORM 920-SYUNOU-SET-SEC
           DISPLAY "給付外点数集計終了=" WRK-KYUFUGAI
           IF      RECE02-FTNMONEY(1)  >   ZERO
               IF      RECE02-FTNMONEY(1)  <   WRK-FTNMONEY-G
                   MOVE    RECE02-FTNMONEY(1)  TO  WRK-FTNMONEY-G
               END-IF
               MOVE    WRK-FTNMONEY-G          TO  MF100-FTNMONEY
           END-IF
      *
      *    ＜請求額計算＞
      *    老人＋在宅等で保険に限度額が有る場合の対応
           IF      RECE02-FTNMONEY(1)  >   ZERO
           AND     RECE02-FTNMONEY(1)  <   WRK-SEIKYUGK-G
               MOVE    RECE02-FTNMONEY(1)  TO  WRK-SEIKYUGK-G
           END-IF
      *
      *    長期
           IF      FLG-TYOKI       =   1
           AND     WRK-SEIKYUGK-G  >   10000
               MOVE    10000       TO  WRK-SEIKYUGK-G
           END-IF
      *    長期上位
           IF      FLG-TYOKI       =   2
           AND     WRK-SEIKYUGK-G  >   20000
               MOVE    20000       TO  WRK-SEIKYUGK-G
           END-IF
      *
           COMPUTE WRK-SEIKYUGK-G2 =   WRK-SEIKYUGK-G  +   0.5 
      *
               MOVE    WRK-SEIKYUGK-G  TO  WRK-SEIKYUGK-G2
               DIVIDE  WRK-SEIKYUGK-G2 BY  10
                       GIVING              WRK-KEISAN
                       REMAINDER           WRK-AMARI
               COMPUTE WRK-SEIKYUGK-G2 =   WRK-SEIKYUGK-G2
                                       -   WRK-AMARI
               IF      WRK-AMARI       >=  5
                   ADD     10          TO  WRK-SEIKYUGK-G2
               END-IF
               MOVE    WRK-SEIKYUGK-G2 TO  MF100-SEIKYUGK
      *
           DISPLAY "集計結果=" MF100-SEIKYUGK
      *
      *    総請求額
           COMPUTE  MF100-SOUSEIKYUGK  =  MF100-TOTALTEN  *  10
      *    他法負担額（保険負担）
           COMPUTE  MF100-TAHOFTNGK  =  MF100-SOUSEIKYUGK  * 
                                        (100 - RECE02-KYURATE)  /  100
      *    公費負担額
           COMPUTE  MF100-KOHFTNGK  =  MF100-SOUSEIKYUGK  - 
                                      (MF100-TAHOFTNGK + MF100-SEIKYUGK)
      *
      *    公費適用開始〜終了日取得
           PERFORM  910-PTKOHINF-SET-SEC
      *
      *    公費上限額取得
           PERFORM  920-PTKOHFTN-READ-SEC
      *
      *    ＜出力順設定＞
      *    保険者番号順
           IF    WRK-STUNUM  =  "01" OR "02"
             EVALUATE  MF100-HKNNUM
      *      ４桁
             WHEN "001"
               MOVE  MF100-HKNJANUM(1:4)  TO  MF100-HKNJANUM-KEY(5:4)
      *      ６桁
             WHEN "060"
               MOVE  MF100-HKNJANUM(1:6)  TO  MF100-HKNJANUM-KEY(3:6)
      *      ８桁
             WHEN OTHER
               MOVE  MF100-HKNJANUM       TO  MF100-HKNJANUM-KEY
             END-EVALUATE
      *      システム管理マスタの出力順を設定 
             PERFORM  VARYING  IDX   FROM  1  BY  1
                      UNTIL    IDX   >     200
               IF    (WRK-2003-TEISYUTUSAKI(IDX) 
                                              = MF100-TEISYUTUSAKI)
                 AND (WRK-2003-NYUGAIKBN(IDX) = MF100-NYUGAIKBN)
                 IF  MF100-RECESYUBETU = WRK-2003-SYUBETU(IDX 1)
                 OR                      WRK-2003-SYUBETU(IDX 2)
                 OR                      WRK-2003-SYUBETU(IDX 3)
                 OR                      WRK-2003-SYUBETU(IDX 4)
                 OR                      WRK-2003-SYUBETU(IDX 5)
                 OR                      WRK-2003-SYUBETU(IDX 6)
                 OR                      WRK-2003-SYUBETU(IDX 7)
                 OR                      WRK-2003-SYUBETU(IDX 8)
                 OR                      WRK-2003-SYUBETU(IDX 9)
                 OR                      WRK-2003-SYUBETU(IDX 10)
                   MOVE  WRK-2003-DSPSEQ(IDX)
                                          TO  MF100-DSPSEQ
                   DISPLAY "出力番号=" MF100-DSPSEQ
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
      *      カナ氏名順・患者番号順
           IF  WRK-STUNUM  =  "01" OR "03"
             MOVE  MF100-KANANAME  TO  MF100-KANANAME-KEY
           ELSE
             MOVE  MF100-PTNUM     TO  MF100-PTNUM-KEY
           END-IF
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF      FLG-ERR            =     ZERO
               WRITE    MF100-REC
           ELSE
               DISPLAY "助成対象外="        MF100-PTNUM
                       "負担者番号="        MF100-KOHFTNJANUM
                       "老人負担者番号="    MF100-RJNFTNJANUM
                       "公費番号="          MF100-KOHNUM
           END-IF
      *
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "470"                     TO  WRK-HKNJANUM(1:3)
           MOVE    MF100-FTNJANUM-KEY(6:2)   TO  WRK-HKNJANUM(4:2)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
                   IF      WRK-HKNJANUM(1:5)
                                   =   TBL-PARA-HKNJANUM(IDXX)(1:5)
                       MOVE    ZERO                TO  FLG-ERR
                       MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                   END-IF
               END-PERFORM 
           END-IF
      *
           .
       200111-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT       MF100-FILE
      *    一時ファイル読込
           MOVE     ZERO        TO  FLG-END
           INITIALIZE  MF100-REC
           PERFORM  900-TMP-READ-SEC

           PERFORM         UNTIL   FLG-END        =   1
               DISPLAY "対象者=" MF100-PTNUM
               MOVE    1         TO  CNT-PAGE
      *
               IF      MF100-NYUGAIKBN =   "2"
      *
      *            帳票編集<ヘッダー部>処理
                   PERFORM 310-HC101-HEAD-HEN-SEC
      *
      *            帳票編集<点数>処理
                   PERFORM 330-HC101-TENSU-HEN-SEC
      *
      *            帳票編集<請求>処理
                   PERFORM 340-HC101-MEISAI-HEN-SEC
      *
      *            帳票編集<傷病名>処理
                   PERFORM 320-HC101-BYOMEI-HEN-SEC
      *
      *            帳票編集<摘要欄>処理
                   PERFORM 350-HC101-TEKIYO-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 390-PRINT-OUT-SEC
      *
               END-IF
      *
      *        一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE   MF100-FILE
           MOVE    1           TO  FLG-END
      *
           .
       2002-PRINT-EXT.
           EXIT.
      ******************************************************************
      *    帳票編集<ヘッダー部>処理(外来)
      *****************************************************************
       310-HC101-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      SC05
           MOVE    SPACE               TO  SC05
      *
      *    診療年月
           MOVE    MF100-SRYYM         TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  SC05-SRYYM
      *
      *    上限額
           MOVE    MF100-JGNGAK        TO  WRK-Z9
           MOVE    WRK-Z9              TO  SC05-JGNGAK
      *
      *    証明書番号（負担者番号）
           MOVE    MF100-KOHFTNJANUM   TO  SC05-FTNJANUM
      *
      *    患者票番号（受給者番号）
           MOVE    MF100-KOHJKYSNUM    TO  SC05-JKYSNUM
      *
      *    適用開始日
           MOVE    MF100-KOHTEKSTYMD   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SC05-TEKSTYMD
      *
      *    適用終了日
           MOVE    MF100-KOHTEKEDYMD   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SC05-TEKEDYMD
      *
      *    カナ氏名
           IF  MF100-KANANAME(41:)  NOT =  SPACE
             MOVE  MF100-KANANAME      TO  SC05-KANANAME2
           ELSE
             MOVE  MF100-KANANAME      TO  SC05-KANANAME
           END-IF
      *
      *    氏名
           MOVE    MF100-NAME          TO  SC05-NAME
      *
      *    性別
           EVALUATE  MF100-SEX
           WHEN  "1"
             MOVE    "○"              TO  SC05-SEX(1)
           WHEN  "2"
             MOVE    "○"              TO  SC05-SEX(2)
           END-EVALUATE
      *
      *    年号・誕生年
           MOVE    MF100-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           INITIALIZE              ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI 
           MOVE    LNK-DAY1-GENGO      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  SC05-BIRTHDAY(1:)
           END-IF    
           MOVE    MF100-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3(1:10)  TO  SC05-BIRTHDAY2
      *
      *    医療機関所在地
           MOVE  WRK-HOSPADRS          TO  SC05-HOSPADRS
           INSPECT SC05-HOSPADRS       REPLACING ALL "  " BY "　"
      *
      *    医療機関名称
           MOVE  WRK-HOSPNAME          TO  SC05-HOSPNAME
           INSPECT SC05-HOSPNAME       REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE  WRK-KAISETUNAME       TO  SC05-KAISETU
      *
      *    本人・家族
           IF  MF100-HONKZKKBN  =  "1"
             MOVE  "◯"                TO  SC05-HONKZKKBN(1)
           ELSE
             MOVE  "◯"                TO  SC05-HONKZKKBN(2)
           END-IF

      *    保険区分
           IF  MF100-RJNHKNNUM  NOT =  SPACE
           OR  MF100-HKNNUM  =  "039"  OR  "040"
      *        後期高齢
               MOVE    "○"            TO  SC05-HKNKBN(9)
           ELSE
             EVALUATE  MF100-TEISYUTUSAKI
      *        社保の場合
               WHEN  "1"
                 EVALUATE  MF100-HKNNUM
                   WHEN  "001"
                   WHEN  "009"
      *              政府管掌
                     MOVE    "○"      TO  SC05-HKNKBN(1)
                   WHEN  "002"
      *              船舶
                     MOVE    "○"      TO  SC05-HKNKBN(3)
                   WHEN  "031"  THRU  "034"
      *              共済
                     MOVE    "○"      TO  SC05-HKNKBN(4)
      *              組合健保
                   WHEN  "006"
                   WHEN  "072"  THRU  "075"
                     MOVE    "○"      TO  SC05-HKNKBN(2)
                   WHEN  "063"
                     MOVE    "○"      TO  SC05-HKNKBN(2)
                     MOVE    "特退"    TO  SC05-HKNKBN2
                 END-EVALUATE
      *        国保の場合
               WHEN  "2"
                 EVALUATE  MF100-HKNNUM
                   WHEN  "060"
                   WHEN  "068"
      *              一般国保
                     MOVE    "○"      TO  SC05-HKNKBN(6)
                     MOVE    "○"      TO  SC05-HKNKBN(7)
                   WHEN  "067"
                   WHEN  "069"
      *              退職国保
                     MOVE    "○"      TO  SC05-HKNKBN(6)
                     MOVE    "○"      TO  SC05-HKNKBN(8)
                 END-EVALUATE
      *        その他
               WHEN  OTHER
                 EVALUATE  MF100-HKNNUM
                   WHEN  "971"
      *              労災
                     MOVE    "○"      TO  SC05-HKNKBN(5)
                   WHEN  "980"
      *              自費
                     MOVE    "○"      TO  SC05-HKNKBN(11)
                 END-EVALUATE
             END-EVALUATE
           END-IF
      *
      *    診療実日数
           IF  CNT-PAGE  =  1
               MOVE    MF100-JNISSU    TO  WRK-JINISSUZ
               MOVE    WRK-JINISSU     TO  SC05-JINISSU
           END-IF
      *
           .
       310-HC101-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       320-HC101-BYOMEI-HEN-SEC      SECTION.
      *
      *    病名退避エリア初期化
           INITIALIZE  WRK-BYOMEI-TAIHI-TBL
      *
      *    パラメータ０３ファイルより編集
           MOVE    LOW-VALUE           TO  WRK-BYO-OLD
      *     
           INITIALIZE                  WRK-EDIT-AREA
           MOVE    SPACE               TO  WRK-BYOMEI1
                                           WRK-BYOMEI2
           MOVE    ZERO                TO  FLG-TENKI
      *     
           INITIALIZE                      RECE031-REC
           MOVE    MF100-HOSPNUM      TO  RECE031-HOSPNUM
           MOVE    MF100-SRYYM        TO  RECE031-SRYYM
           MOVE    MF100-NYUGAIKBN    TO  RECE031-NYUGAIKBN
           MOVE    MF100-PTID         TO  RECE031-PTID
           MOVE    MF100-RECEKA       TO  RECE031-RECEKA
           MOVE    MF100-TEISYUTUSAKI TO  RECE031-TEISYUTUSAKI
           MOVE    MF100-RECESYUBETU  TO  RECE031-RECESYUBETU
           MOVE    MF100-HKNJANUM-KEY TO  RECE031-HKNJANUM
           MOVE    MF100-HOJOKBN-KEY  TO  RECE031-HOJOKBN
           MOVE    MF100-TEKSTYMD     TO  RECE031-TEKSTYMD
      *
           START   RECE031-FILE
               KEY IS    >=   RECE031-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE031
                   DISPLAY "患者病名ファイル終了"
               NOT INVALID
                   PERFORM 990-RECE031-NEXT-SEC
           END-START
           DISPLAY "フラグ="      FLG-RECE031
           DISPLAY "患者番号1="    MF100-PTNUM
           DISPLAY "診療年月1="   MF100-SRYYM
           DISPLAY "入外区分1="   MF100-NYUGAIKBN
           DISPLAY "患者ID1="     MF100-PTID
           DISPLAY "保険者番号KEY1=" MF100-HKNJANUM-KEY
           DISPLAY "保険者番号1=" MF100-HKNJANUM
           DISPLAY "レセプト科1=" MF100-RECEKA
           DISPLAY "摘要開始日1=" MF100-TEKSTYMD
           DISPLAY "提出先1="     MF100-TEISYUTUSAKI
           DISPLAY "レセ種別1="   MF100-RECESYUBETU
           DISPLAY "補助区分1="   MF100-HOJOKBN-KEY
           DISPLAY "---------------------------"
           DISPLAY "患者番号2="    MF100-PTNUM
           DISPLAY "診療年月2="   RECE031-SRYYM
           DISPLAY "入外区分2="   RECE031-NYUGAIKBN
           DISPLAY "患者ID2="     RECE031-PTID
           DISPLAY "保険者番号2=" RECE031-HKNJANUM
           DISPLAY "レセプト科2=" RECE031-RECEKA
           DISPLAY "摘要開始日2=" RECE031-TEKSTYMD
           DISPLAY "提出先2="     RECE031-TEISYUTUSAKI
           DISPLAY "レセ種別2="   RECE031-RECESYUBETU
           DISPLAY "補助区分2="   RECE031-HOJOKBN
           DISPLAY "---------------------------"
      *
           PERFORM
               UNTIL  ( FLG-RECE031          =   1                    )
               OR     ( MF100-HOSPNUM    NOT =   RECE031-HOSPNUM      )
               OR     ( MF100-SRYYM      NOT =   RECE031-SRYYM        )
               OR     ( MF100-NYUGAIKBN  NOT =   RECE031-NYUGAIKBN    )
               OR     ( MF100-PTID       NOT =   RECE031-PTID         )
               OR     ( MF100-RECEKA     NOT =   RECE031-RECEKA       )
               OR     ( MF100-TEISYUTUSAKI
                                         NOT =   RECE031-TEISYUTUSAKI )
               OR     ( MF100-RECESYUBETU
                                         NOT =   RECE031-RECESYUBETU  )
               OR     ( MF100-HKNJANUM
                                         NOT =   RECE031-HKNJANUM     )
               OR     ( MF100-HOJOKBN-KEY
                                         NOT =   RECE031-HOJOKBN      )
               OR     ( MF100-TEKSTYMD   NOT =   RECE031-TEKSTYMD     )
      *    
                   DISPLAY "病名編集開始"
      *        病名編集処理
               PERFORM 3210-BYOMEI-SET-SEC
      *
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE031     
      *
           IF  WRK-LINEBYO      <  WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 3221-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF  WRK-BYOMEI2-NAME  NOT =   SPACE
                   MOVE    1             TO  FLG-TENKI
                   PERFORM 3222-BYOMEI2-OUT-SEC
                   MOVE    ZERO          TO  FLG-TENKI
               END-IF
           END-IF
      *
      *    主病名との区切り線編集処理     
           IF    ( SYS-2005-SYUBYOHENFLG   =   "3" )
           AND   ( WRK-BO-SYUBYOFLG        =   "1" )
               IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   ADD     1           TO  WRK-LINEBYO
                   MOVE    WRK-KUGIRI3
                           TO WRK-TAIHI-BYOMEI(WRK-LINEBYO)(2:52)
               ELSE
                   IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF 
                   MOVE    1           TO  FLG-KUGIRI
                   PERFORM 34110-KUGIRI-LINE-SEC
                   MOVE    ZERO        TO  FLG-KUGIRI   
               END-IF
           END-IF
      *
      *    ワークエリアに退避した病名情報を設定
           PERFORM 3201-TAIHI-SET-SEC
      *
           .
       320-HC101-BYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    退避病名設定
      *****************************************************************
       3201-TAIHI-SET-SEC       SECTION.
      *
           IF      MF100-NYUGAIKBN     =   "1"
               CONTINUE
           ELSE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >      35
                   IF      IDX1        <=     5
                       MOVE    WRK-TAIHI-SRYYMD(IDX1)
                                           TO  SC05-SRYYMD(IDX1)
                       MOVE    WRK-TAIHI-TENKI(IDX1)
                                           TO  SC05-TENKI(IDX1)
                       MOVE    WRK-TAIHI-TENKIYMD(IDX1)
                                           TO  SC05-TENKIYMD(IDX1)
                       MOVE    WRK-TAIHI-BYOMEI(IDX1)
                                           TO  SC05-BYOMEI(IDX1)
                   END-IF
                   MOVE    WRK-TAIHI-TEKIYO(IDX1)
                                           TO  SC05-TEKIYO(IDX1)
               END-PERFORM
           END-IF
      *
           .
       3201-TAIHI-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *     病名編集処理
      ****************************************************************
       3210-BYOMEI-SET-SEC           SECTION.
      * 
      *    連結表示のとき
           IF      SYS-2005-BYOMEIHENKBN    =   "0"
      *        前のデータと同一主病名フラグ・診療開始日・転帰区分
               IF  ( RECE031-SYUBYOFLG    =   WRK-BO-SYUBYOFLG )  AND
                   ( RECE031-SRYYMD       =   WRK-BO-SRYYMD    )  AND
                   ( RECE031-TENKIKBN     =   WRK-BO-TENKIKBN  )  AND 
                   ( RECE031-TENKIYMD     =   WRK-BO-TENKIYMD  )   
      *
      *            病名をつなげると何桁になるかを計算（，＋病名）
                   IF  WRK-CLM           =   ZERO
                       MOVE    WRK-CLM   TO  WRK-WCLM
                   ELSE
                       COMPUTE WRK-WCLM  =   WRK-CLM   +   2
                   END-IF
                   COMPUTE     WRK-WCLM  =
                              ( RECE031-BYOMEIMOJI  *   2 )
                                                    +   WRK-WCLM
      *
                   EVALUATE    TRUE
      *                傷病名欄に編集
                       WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI1-MAX
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                            前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
      *                適用欄に編集
                       WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI2-MAX
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                        前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI2-NAME
                                                       (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
                   END-EVALUATE
               ELSE
      *            主病名フラグ・診療開始日・転帰区分 が異なる場合
                   IF      WRK-BYO-OLD        =   LOW-VALUE
                       CONTINUE
                   ELSE
                       IF  WRK-LINEBYO       <  WRK-BYOMEI-LINE
                           IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           END-IF
                       ELSE
                           IF  WRK-BYOMEI2   NOT =   SPACE
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI 
                           END-IF
                       END-IF
                   END-IF    
               END-IF
           ELSE
                IF  WRK-LINEBYO       <   WRK-BYOMEI-LINE
                    IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                        PERFORM 3221-BYOMEI1-OUT-SEC
                    END-IF
                ELSE
                    IF  WRK-BYOMEI2   NOT =   SPACE
                        MOVE    1             TO  FLG-TENKI
                        PERFORM 3222-BYOMEI2-OUT-SEC
                        MOVE    ZERO          TO  FLG-TENKI
                    END-IF
                END-IF
           END-IF
      *
      *    主病名との区切り線編集処理     
           PERFORM 3211-SYUBYOFLG-HENSYU-SEC
      *
      *    対象病名を編集
           COMPUTE     WRK-STCLM         =   WRK-CLM   +   1
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE031-BYOMEIMOJI   *   2
           MOVE    RECE031-BYOMEI      TO  WRK-BYOMEI
      *
      *
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI1-MAX
                       MOVE    RECE031-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE031-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE031-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE
                       ELSE
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE    
                       END-IF                                   
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI2-MAX 
                       MOVE    RECE031-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE031-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE031-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM  3222-BYOMEI2-OUT-SEC
                       MOVE    1               TO  WRK-CONTINUE
                       MOVE    WRK-BYOMEI(45:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   END-IF                                       
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *     
           MOVE    RECE031-SYUBYOFLG     TO  WRK-BO-SYUBYOFLG
           MOVE    RECE031-SRYYMD        TO  WRK-BO-SRYYMD
           MOVE    RECE031-TENKIKBN      TO  WRK-BO-TENKIKBN
           MOVE    RECE031-TENKIYMD      TO  WRK-BO-TENKIYMD
      *
           PERFORM 990-RECE031-NEXT-SEC
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      ****************************************************************
      *      主病名と副病名の区切り線の編集
      ****************************************************************
       3211-SYUBYOFLG-HENSYU-SEC         SECTION.
      *
      *    主病名編集フラグが区切り有のときのみ処理を行う
           IF      SYS-2005-SYUBYOHENFLG   =   "1" OR  "3"
      *        主病名との区切り線（副傷病名があるときのみ）     
               IF      WRK-BYO-OLD             =   LOW-VALUE
                   CONTINUE
               ELSE
                   IF    ( FLG-RECE031             =   ZERO            )
                   AND   ( RECE031-SYUBYOFLG   NOT =   WRK-BO-SYUBYOFLG)
                   AND   ( WRK-BO-SYUBYOFLG        =   "1"             )
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           ADD     1           TO  WRK-LINEBYO
                           MOVE    WRK-KUGIRI3 TO
                                   WRK-TAIHI-BYOMEI(WRK-LINEBYO)(2:52)
                       ELSE
                           IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           END-IF    
                           MOVE    1           TO  FLG-KUGIRI
                           PERFORM 34110-KUGIRI-LINE-SEC
                           MOVE    ZERO        TO  FLG-KUGIRI   
                       END-IF
                   END-IF
               END-IF
           END-IF        
      *
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      ****************************************************************
      *     傷病名１出力処理
      *      WRK-CONTINUE = 1 の場合、２行編集の２行目
      ****************************************************************
       3221-BYOMEI1-OUT-SEC        SECTION.
      *
      *    傷病名欄の行カウント
           ADD     1               TO  WRK-LINEBYO
      *
           IF  WRK-CONTINUE        =   ZERO
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
               MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
               MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-BYOMEI1-NO 
               END-IF    
      *        診療開始日
               MOVE    WRK-BYOMEI1-NO      TO  WRK-TAIHI-SRYYMD
                                                    (WRK-LINEBYO)
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3-G (1:2)
                                           TO  WRK-TAIHI-SRYYMD
                                                    (WRK-LINEBYO)(7:2)
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-TAIHI-SRYYMD
                                                    (WRK-LINEBYO)(9:)
      *        転帰
               MOVE    WRK-BYOMEI1-NO    TO 
                                         WRK-TAIHI-TENKI(WRK-LINEBYO)
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"   
                                 TO  WRK-TAIHI-TENKI(WRK-LINEBYO)(7:)
               WHEN    "2"
                   MOVE    "死亡"   
                                 TO  WRK-TAIHI-TENKI(WRK-LINEBYO)(7:)
               WHEN    "3"
                   MOVE    "中止"   
                                 TO  WRK-TAIHI-TENKI(WRK-LINEBYO)(7:)
               WHEN    OTHER
                   MOVE    SPACE   
                                 TO  WRK-TAIHI-TENKI(WRK-LINEBYO)
               END-EVALUATE
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3 (17:)
                                           TO  WRK-TAIHI-TENKIYMD
                                                    (WRK-LINEBYO)
               END-IF                                     
           END-IF
      *    病名
           MOVE    WRK-BYOMEI1     TO  WRK-TAIHI-BYOMEI(WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3221-BYOMEI1-OUT-EXT.
           EXIT.
      ****************************************************************
      *     傷病名２出力処理
      *      WRK-CONTINUE = 0 の場合、２行編集の１行目
      ****************************************************************
       3222-BYOMEI2-OUT-SEC        SECTION.
      *
      *    適用欄の行カウント
           IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF    
           ADD     1               TO  WRK-LINETEK
      
      *
           IF  WRK-CONTINUE        =   ZERO
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               IF  WRK-LINE            <   10
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
               ELSE    
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (1:2)  TO  KANACHK-MAE-INPUT (2:2)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (4:1)
               END-IF
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-BYOMEI2-NO 
               END-IF    
           END-IF    
      *
      *    診療開始日・転帰を印字するスペースが有る場合
           IF      SYS-2005-TENKIHENKBN    =   ZERO
               IF  WRK-BYOMEI2-ELSE    =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           ELSE
               IF  WRK-BYOMEI2R-ELSE   =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           END-IF    
      *
           INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
           MOVE    WRK-BYOMEI2 TO  WRK-TAIHI-TEKIYO (WRK-LINETEK)
      *
      *    最後の病名に印字するスペースがなかったとき 
           IF       FLG-TENKI              =   1     
      *        診療年月日・転帰　編集処理
               MOVE    SPACE           TO  WRK-BYOMEI2
               PERFORM 32221-SRYYMD-TENKI-HEN-SEC
      *        適用欄の行カウント
               IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK
               INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
               MOVE    WRK-BYOMEI2 TO  WRK-TAIHI-TEKIYO
                                                 (WRK-LINETEK)

           END-IF
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3222-BYOMEI2-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療年月日・転帰　編集処理
      *****************************************************************
       32221-SRYYMD-TENKI-HEN-SEC    SECTION.
      *
      *    転帰日を編集しないとき
           IF      SYS-2005-TENKIHENKBN    =   ZERO
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3-G (1:2)
                                           TO  WRK-BYOMEI2-YMD-G
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2-YMD
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2-TENKI
               END-EVALUATE
           ELSE
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3-G (1:2)
                                           TO  WRK-BYOMEI2R-YMD-G
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2R-YMD
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2R-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2R-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2R-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2R-TENKI
               END-EVALUATE
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3 (17:)
                                               TO  WRK-BYOMEI2R-TENKIYMD
               END-IF                                
           END-IF                                     
      *
           .
       32221-SRYYMD-TENKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<点数>処理(外来)
      *****************************************************************
       330-HC101-TENSU-HEN-SEC      SECTION.
      *
           EVALUATE  MF100-SHOSHINTIME
               WHEN  "1"
                     MOVE  "（時間外）"   TO  SC05-SHOSHINTIME
               WHEN  "2"
                     MOVE  "（深夜）"     TO  SC05-SHOSHINTIME
               WHEN  "3"
                     MOVE  "（休日）"      TO  SC05-SHOSHINTIME
               WHEN  "4"
                     MOVE  "時外・深夜"    TO  SC05-SHOSHINTIME
               WHEN  "5"
                     MOVE  "時外・休日"    TO  SC05-SHOSHINTIME
               WHEN  "6"
                     MOVE  "休日・深夜"    TO  SC05-SHOSHINTIME
               WHEN  "7"
                     MOVE  "外・休・深"    TO  SC05-SHOSHINTIME
           END-EVALUATE
      *
      *    その他
           MOVE  "その他"                  TO  SC05-SONOTA
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     35
      *＜回数記載＞
      *        回数
               MOVE  MF100-KAISU(IDX)      TO  WRK-KAISUZ
               MOVE  WRK-KAISUZ            TO  SC05-KAISU(IDX)
      *
      *＜基本点数記載＞
               EVALUATE  IDX
                 WHEN  2
                 WHEN  3
                 WHEN  4
                 WHEN  5
                 WHEN  6
      *            再診−再診〜深夜
                   MOVE  MF100-KHNTEN(IDX)  TO  WRK-KHNTENZ
                   MOVE  WRK-KHNTENZ        TO  SC05-KHNTEN(IDX - 1)
                 WHEN  14
      *            投薬−内服薬剤
                   MOVE  MF100-KHNTEN(IDX)  TO  WRK-KHNTENZ
                   MOVE  WRK-KHNTENZ        TO  SC05-KHNTEN1(1)
                 WHEN  15
      *            投薬−内服調剤
                   MOVE  MF100-KHNTEN(IDX)  TO  WRK-KHNTENZ
                   MOVE  WRK-KHNTENZ        TO  SC05-KHNTEN1(2)
                 WHEN  18
      *            投薬−外用調剤
                   MOVE  MF100-KHNTEN(IDX)  TO  WRK-KHNTENZ
                   MOVE  WRK-KHNTENZ        TO  SC05-KHNTEN2(1)
                 WHEN  19
      *            投薬−処方
                   MOVE  MF100-KHNTEN(IDX)  TO  WRK-KHNTENZ
                   MOVE  WRK-KHNTENZ        TO  SC05-KHNTEN2(2)
                 WHEN  20
      *            投薬−麻毒
                   MOVE  MF100-KHNTEN(IDX)  TO  WRK-KHNTENZ
                   MOVE  WRK-KHNTENZ        TO  SC05-KHNTEN2(3)
                 WHEN  21
      *            投薬−調基
                   MOVE  MF100-KHNTEN(IDX)  TO  WRK-KHNTENZ
                   MOVE  WRK-KHNTENZ        TO  SC05-KHNTEN2(4)
                 WHEN  33
      *            その他−処方せん
                   MOVE  MF100-KHNTEN(IDX)  TO  WRK-KHNTENZ
                   MOVE  WRK-KHNTENZ        TO  SC05-KHNTEN3
               END-EVALUATE
      *
      *＜点数記載＞
      *        点数
               MOVE  MF100-TENSU(IDX 1)     TO  WRK-TENSUZ
               MOVE  WRK-TENSUZ             TO  SC05-TENSU(IDX)
      *
      *＜公費点数記載＞
      *        公費点数
               MOVE  MF100-TENSU(IDX 2)     TO  WRK-TENSUZ
               MOVE  WRK-TENSUZ             TO  SC05-KOUTEN(IDX)
           END-PERFORM
      *
           .
       330-HC101-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　編集処理
      *****************************************************************
       34110-KUGIRI-LINE-SEC       SECTION.
      *
           ADD     1               TO  WRK-LINETEK
      *
           EVALUATE    FLG-KUGIRI
           WHEN    1
               MOVE    WRK-KUGIRI1 TO  WRK-TAIHI-TEKIYO  (WRK-LINETEK)
           END-EVALUATE
      *
           .
       34110-KUGIRI-LINE-EXT.
           EXIT.
      *
      ******************************************************************
      **    改ページ処理
      ******************************************************************
       34120-PAGE-SEC       SECTION.
      *
      *    ワークエリアに退避した病名情報を設定
           PERFORM 3201-TAIHI-SET-SEC
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *    帳票編集<ヘッダー部>処理
           ADD     1               TO  CNT-PAGE
           IF      MF100-NYUGAIKBN =   "2"
               PERFORM 310-HC101-HEAD-HEN-SEC
           END-IF
           INITIALIZE  WRK-BYOMEI-TAIHI-TBL
           MOVE    ZERO          TO  WRK-LINETEK
      *
           .
       34120-PAGE-EXT.
           EXIT.
      **
      *****************************************************************
      *    帳票編集<摘要欄>処理(外来)
      *****************************************************************
       350-HC101-TEKIYO-HEN-SEC      SECTION.
      *
           INITIALIZE                     HC063
      *
      *    レセプト明細読み込み
           PERFORM  900-RECEPRT-READ-SEC
      *
           MOVE    RECE08-CREYMD          TO  WRK-CREYMD
           MOVE    RECE08-CREHMS          TO  WRK-CREHMS
      *
           PERFORM  UNTIL  FLG-RECE08  =  1
      *
             IF    (MF100-HKNJANUM      =  RECE08-HKNJANUM
               AND  MF100-TEISYUTUSAKI  =  RECE08-TEISYUTUSAKI
               AND  MF100-RECESYUBETU   =  RECE08-RECESYUBETU)
               OR   FLG-RECE08          =  1
      *
      *        連番が2以降は改ページしてから印刷
               IF  RECE08-RENNUM  NOT =  1
      *          帳票印刷処理
                 PERFORM 390-PRINT-OUT-SEC
      *          帳票編集<ヘッダー部>処理
                 ADD     1               TO  CNT-PAGE
                 IF      MF100-NYUGAIKBN =   "2"
                     PERFORM 310-HC101-HEAD-HEN-SEC
                 END-IF
                 INITIALIZE                  HC063
               END-IF
      *
               MOVE   RECE08-PRTDATA      TO  HC063
      *        摘要欄編集
               PERFORM  VARYING  IDX  FROM  1  BY  1
                        UNTIL    IDX  >  WRK-TEKIYOU-LINE
                 MOVE  HC063-SRYKBN(IDX)  TO  SC05-SRYKBN(IDX)
                 MOVE  HC063-TEKIYO(IDX)  TO  SC05-TEKIYO(IDX)
               END-PERFORM
      *
             END-IF
      *
             PERFORM  900-RECEPRT-FETCH-SEC
      *
      *      作成年月日が異なる同患者レセプトがあったら処理終了
             IF  (RECE08-CREYMD  NOT =  WRK-CREYMD)
             OR  (RECE08-CREHMS  NOT =  WRK-CREHMS)
                 MOVE    1               TO  FLG-RECE08
             END-IF
      *
           END-PERFORM
      *
      *    レセプト明細クローズ
           PERFORM  900-RECEPRT-CLOSE-SEC
           .
       350-HC101-TEKIYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細>処理(外来)
      *****************************************************************
       340-HC101-MEISAI-HEN-SEC      SECTION.
      *
      *    合計点数
           MOVE  MF100-TOTALTEN        TO  WRK-TENSUZ
           MOVE  WRK-TENSUZ            TO  SC05-TOTALTEN
      *    請求額
           MOVE  MF100-SOUSEIKYUGK     TO  WRK-Z10
           MOVE  WRK-Z10               TO  SC05-SEIKYUGK(1)
      *    公費負担額
           MOVE  MF100-KOHFTNGK        TO  WRK-Z10
           MOVE  WRK-Z10               TO  SC05-SEIKYUGK(2)
      *    他法負担額
           MOVE  MF100-TAHOFTNGK       TO  WRK-Z10
           MOVE  WRK-Z10               TO  SC05-SEIKYUGK(3)
      *    差引請求額（窓口負担金）
           MOVE  MF100-SEIKYUGK        TO  WRK-KGK10
           MOVE  WRK-KGK10             TO  SC05-SEIKYUGK(4)
      *
           .
       340-HC101-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                    TO  WRK-PAGE
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "複写式レセ"        TO  SPRT-TITLE
           IF      MF100-NYUGAIKBN     =   "2"
               MOVE   "SC4705.red"     TO  SPRT-PRTID
               MOVE    SC05            TO  SPRT-PRTDATA
           END-IF
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    MF100-PTID          TO  SPRT-PTID
           MOVE    "2"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "帳票ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦日付編集
      *****************************************************************
       31012-SE-WA-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"   USING   STS-AREA-DAY  LNK-DAY1-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
           ELSE
               MOVE    SPACE               TO  WRK-SYMD
           END-IF
           .
       31012-SE-WA-YMDEDIT-EXT.
           EXIT.
057400*
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE031-FILE
      *
           MOVE  WRK-PAGE  TO  WRK-PARA-PAGE
           DISPLAY "*** SEIKYU4705 IN   "  CNT-RECE02
           DISPLAY "*** SEIKYU4705 OUT  "  CNT-OUT
           DISPLAY "*** SEIKYU4705 END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       WRK-TAISYO-KOHNUM(IDXX)  =  SPACE
               IF  WRK-CHK-KOHNUM(1)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(2)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(3)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(4)  =  WRK-TAISYO-KOHNUM(IDXX)
                   MOVE     1                 TO  FLG-TAISYO
                   MOVE     100               TO  IDXX
               END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込 SELECT
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費請求書マスタ FETCH
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE02-REC
      *
               INITIALIZE                          WRK-CHK-KOHNUM-TBL
               MOVE    RECE02-KOHNUM(1)        TO  WRK-CHK-KOHNUM(1)
               MOVE    RECE02-KOHNUM(2)        TO  WRK-CHK-KOHNUM(2)
               MOVE    RECE02-KOHNUM(3)        TO  WRK-CHK-KOHNUM(3)
               MOVE    RECE02-KOHNUM(4)        TO  WRK-CHK-KOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *
      *            入院は対象外
                   IF  RECE02-NYUGAIKBN  =  "1"
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
      *            個別発行で患者番号指定の場合、対象判定を行う
                   MOVE  ZERO                TO  FLG-ERR
                   IF      WRK-PARA-SYORIFLG   =   "1"
                   AND     FLG-SYORI           =    2
                     MOVE  1                   TO  FLG-ERR
                     PERFORM VARYING IDXX    FROM    1   BY  1
                             UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                             OR      TBL-PARA-PTID(IDXX) =   ZERO
                         IF      RECE02-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO                TO  FLG-ERR
                           MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                         END-IF
                     END-PERFORM 
                   END-IF
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
                   ADD  1  TO  CNT-RECE02
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
      *
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ取得処理
      *****************************************************************
       900-SEIKYU-SET-SEC         SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                     RECE10-REC
      *
           MOVE  RECE02-KEY           TO  RECE10-KEY
           MOVE  RECE10-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC             =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC             =   ZERO
               MOVE    MCPDATA-REC    TO  RECE10-REC
      *
           ELSE
               MOVE    1              TO  FLG-SEIKYU
           END-IF
      *
      *    クローズ
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ CLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE031-NEXT-SEC           SECTION.
      *
           READ    RECE031-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE031
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE031
      *    保険者が同一かどうか
                   IF  RECE031-HKNJANUM  NOT =  MF100-HKNJANUM
                       GO  TO  990-RECE031-NEXT-SEC
                   END-IF
      *
                   IF      RECE031-TENKIKBN     =   "8"
                       GO  TO  990-RECE031-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE031-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                         HKNCOMBI-REC
           MOVE    WRK-HOSPNUM                TO  COMB-HOSPNUM
           MOVE    WRK-PTID                   TO  COMB-PTID
           MOVE    WRK-HKNCOMBI               TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
             MOVE  MF100-NYUGAIKBN    TO  SYU-NYUGAIKBN
             MOVE  MF100-PTID         TO  SYU-PTID
             MOVE  MF100-SRYYM(1:6)   TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)
             MOVE  SYUNOU-REC          TO  MCPDATA-REC
             MOVE  "DBSELECT"          TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE02-HKNCOMBI(1) OR
                                             RECE02-HKNCOMBI(2) OR
                                             RECE02-HKNCOMBI(3) OR
                                             RECE02-HKNCOMBI(4) OR
                                             RECE02-HKNCOMBI(5) OR
                                             RECE02-HKNCOMBI(6) OR
                                             RECE02-HKNCOMBI(7) OR
                                             RECE02-HKNCOMBI(8) OR
                                             RECE02-HKNCOMBI(9) OR
                                             RECE02-HKNCOMBI(10) 
                     MOVE    SYU-HOSPNUM     TO  WRK-HOSPNUM
                     MOVE    SYU-PTID        TO  WRK-PTID
                     MOVE    SYU-HKNCOMBINUM TO  WRK-HKNCOMBI
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF  COMB-KOHID(1)    =  WRK-KOHID   OR
                         COMB-KOHID(2)    =  WRK-KOHID   OR
                         COMB-KOHID(3)    =  WRK-KOHID   OR
                         COMB-KOHID(4)    =  WRK-KOHID
      *                  請求額計算
                         PERFORM  800-SEIKYU-SEC
                         ADD    SYU-KYUFUGAI-TEN(6)    TO
                                WRK-KYUFUGAI
                         ADD    SYU-KYUFUGAI-SIDOU-TEN TO
                                WRK-SIDOU
                         DISPLAY "給付外点数加算=" SYU-KYUFUGAI-TEN(6)
                                 "診療日=" SYU-SRYYMD
                         IF      RECE02-RJNHKNNUM    NOT = SPACE
                             ADD SYU-KOH-COMPFTN(1)  TO  WRK-FTNMONEY-G
                         ELSE
                             ADD SYU-SYUCOMPFTN      TO  WRK-FTNMONEY-G
                         END-IF
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE  "tbl_syunou"       TO  MCP-TABLE
                 MOVE  WRK-PATH           TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SYUNOU-REC
             MOVE  ZERO         TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1  TO  FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO    TO      WRK-SEIKYUGK
                                   FLG-HEIYOU
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
               IF  SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                   SYU-KOH-HKNNUM(IDX)     NOT =   "027"
                   MOVE    1       TO      FLG-HEIYOU
      *
                   IF  RECE02-FTNMONEY(1)  =   ZERO
                       MOVE    SYU-KOH-COMPFTN-ENTANI(IDX)
                                           TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-KOH-COMPFTN(IDX)
                                           TO  WRK-SEIKYUGK
                   END-IF
               END-IF
      *
      *        長期
               IF  SYU-KOH-HKNNUM(IDX)         =   "972"
                   MOVE    1       TO      FLG-TYOKI
               END-IF
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX)         =   "974"
                   MOVE    2       TO      FLG-TYOKI
               END-IF
      *
           END-PERFORM
      *
           IF  FLG-HEIYOU      =   ZERO
               IF  RECE02-FTNMONEY(1)  =   ZERO
                   IF  RECE02-RJNHKNNUM    NOT =   SPACE
                       MOVE    SYU-KOH-COMPFTN-ENTANI(1)
                                                   TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-SYUCOMPFTN-ENTANI
                                                   TO  WRK-SEIKYUGK
                   END-IF
               ELSE
                   IF  RECE02-RJNHKNNUM    NOT =   SPACE
                       MOVE    SYU-KOH-COMPFTN(1)  TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-SYUCOMPFTN      TO  WRK-SEIKYUGK
                   END-IF
               END-IF
           END-IF
      *
           ADD    WRK-SEIKYUGK        TO  WRK-SEIKYUGK-G
           DISPLAY "診療日=" SYU-SRYYMD
                   "点数="   SYU-TOTAL-HKNTEN
                   "加算請求額=" WRK-SEIKYUGK
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"               TO  MCP-FUNC
             MOVE  "tbl_syskanri"          TO  MCP-TABLE
             MOVE  "key10"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  ZERO  TO  FLG-SYSKANRI
             ELSE
             DISPLAY "システム管理フェッチ失敗"
               MOVE  1  TO  FLG-SYSKANRI
             END-IF
           ELSE
             DISPLAY "システム管理セレクト失敗"
             MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ検索(レセプト出力順情報取得)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-2003-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key2"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
          IF      MCP-RC           =   ZERO
                PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
               PERFORM 820-SYSKANRI-DBCLOSE-SEC
           END-IF        
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(レセプト出力順情報取得)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key2"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-2003-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
               PERFORM 820-SYSKANRI-DBCLOSE-SEC
           END-IF  
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key2"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    患者公費情報マスタ取得
      *****************************************************************
       910-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE  SPA-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE  MF100-PTID         TO  PTKOH-PTID
           MOVE  WRK-KOHNUM         TO  PTKOH-KOHNUM
      *
           MOVE  PTKOHINF-REC              TO  MCPDATA-REC
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE  "tbl_ptkohinf"     TO  MCP-TABLE
           MOVE  "key4"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "tbl_ptkohinf"   TO  MCP-TABLE
             MOVE  "key4"           TO  MCP-PATHNAME
             PERFORM  910-PTKOHINF-READ-SEC
      *
             PERFORM  UNTIL  FLG-PTKOHINF = 1
               IF  PTKOH-SAKJOKBN  NOT = "1"
                 IF  PTKOH-TEKSTYMD(1:6) <= MF100-SRYYM  AND 
                     PTKOH-TEKEDYMD(1:6) >= MF100-SRYYM
                     MOVE  PTKOH-TEKSTYMD  TO  MF100-KOHTEKSTYMD
                     MOVE  PTKOH-TEKEDYMD  TO  MF100-KOHTEKEDYMD
                 END-IF
               END-IF
               MOVE  "tbl_ptkohinf"   TO  MCP-TABLE
               MOVE  "key4"           TO  MCP-PATHNAME
               PERFORM  910-PTKOHINF-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_ptkohinf"     TO  MCP-TABLE
           MOVE  "key4"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       910-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスタ読込処理
      *****************************************************************
       910-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  PTKOHINF-REC
             MOVE  ZERO         TO  FLG-PTKOHINF
           ELSE
             INITIALIZE  PTKOHINF-REC
             MOVE  1  TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担情報読込処理
      *****************************************************************
       920-PTKOHFTN-READ-SEC         SECTION.
      *
           INITIALIZE                          PTKOHFTN-REC
      *
           MOVE    SPA-HOSPNUM             TO  PTKOHFTN-HOSPNUM
           MOVE    MF100-PTID              TO  PTKOHFTN-PTID
           MOVE    PTKOHFTN-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptkohftn"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM  920-PTKOHFTN-FETCH-SEC
      *
               PERFORM  UNTIL  FLG-PTKOHFTN  =  1
                 IF  PTKOHFTN-TEKSTYMD(1:6) <= RECE10-SRYYM(1:6)  AND 
                     PTKOHFTN-TEKEDYMD(1:6) >= RECE10-SRYYM(1:6)
                   IF  MF100-NYUGAIKBN  =  "1"
                     MOVE  PTKOHFTN-NYUJGNGAK    TO  MF100-JGNGAK
                   ELSE
                     MOVE  PTKOHFTN-GAIJGNGAK    TO  MF100-JGNGAK
                   END-IF
                 END-IF
                 DISPLAY "MF100-JGNGAK ="  MF100-JGNGAK
                 PERFORM  920-PTKOHFTN-FETCH-SEC
               END-PERFORM
      *
           ELSE
               MOVE    1                   TO  FLG-PTKOHFTN
               INITIALIZE                  PTKOHFTN-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohftn"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-PTKOHFTN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担情報ＦＥＴＣＨ処理
      *****************************************************************
       920-PTKOHFTN-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHFTN-REC
               MOVE    ZERO                TO  FLG-PTKOHFTN
           ELSE
               MOVE    1                   TO  FLG-PTKOHFTN
               INITIALIZE                  PTKOHFTN-REC
           END-IF
      *
           .
       920-PTKOHFTN-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細読み込み
      *****************************************************************
       900-RECEPRT-READ-SEC         SECTION.
      *
           INITIALIZE                      RECE08-REC
      *
           MOVE  MF100-HOSPNUM        TO  RECE08-HOSPNUM
           MOVE  "HC06"                TO  RECE08-PRTID
           MOVE  MF100-SRYYM           TO  RECE08-RECEYM
      *    患者番号編集
           MOVE  MF100-PTNUM           TO  RECE08-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
             IF  RECE08-PTNUM(IDX:1)   NOT = SPACE
               MOVE    1               TO  IDX 
             ELSE  
               MOVE    "Z"             TO  RECE08-PTNUM(IDX:1)
             END-IF
           END-PERFORM 
      *
           MOVE  RECE08-REC            TO  MCPDATA-REC
           MOVE  "DBSELECT"            TO  MCP-FUNC
           MOVE  "tbl_receprt"         TO  MCP-TABLE
           MOVE  "key53"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"               USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM  900-RECEPRT-FETCH-SEC
           ELSE
               INITIALIZE                  RECE08-REC
               MOVE    1               TO  FLG-RECE08
           END-IF
      *
           .
       900-RECEPRT-READ-EXT.
           EXIT.
      *****************************************************************
      *    レセプト明細フェッチ処理
      *****************************************************************
       900-RECEPRT-FETCH-SEC         SECTION.
      *
           MOVE  "DBFETCH"         TO  MCP-FUNC
           MOVE  "tbl_receprt"     TO  MCP-TABLE
           MOVE  "key53"           TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC TO  RECE08-REC
               MOVE    ZERO        TO  FLG-RECE08
           ELSE
               INITIALIZE              RECE08-REC
               MOVE    1           TO  FLG-RECE08
           END-IF
      *
           .
       900-RECEPRT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    レセプト明細クローズ処理
      *****************************************************************
       900-RECEPRT-CLOSE-SEC         SECTION.
      *
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE  "tbl_receprt"         TO  MCP-TABLE
           MOVE  "key53"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"               USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-RECEPRT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-END
               NOT AT  END
                   ADD   1  TO  CNT-OUT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE                   TO  SYS-1900-REC
           INITIALIZE                          SYS-1900-REC
           MOVE    "1900"                  TO  SYS-1900-KANRICD
           MOVE    "*"                     TO  SYS-1900-KBNCD
           MOVE    SPA-HOSPNUM             TO  SYS-1900-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *            医療機関住所編集情報（２）
                   PERFORM 900-1903-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1901-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
                                           SYS-1901-EDYUKYMD(7:2)
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1902-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
                                           SYS-1902-EDYUKYMD(7:2)
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1903-REC
           INITIALIZE                      SYS-1903-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
           MOVE    "1903"              TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD(7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                  SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション情報読込
      *****************************************************************
       900-PRGOPTION-READ-SEC  SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    WRK-PRGID           TO  PRGOPTION-PRGID
           MOVE    WRK-KBNCD           TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRGOPTION-REC
                   MOVE    ZERO                TO  FLG-PRGOPTION
               ELSE
                   INITIALIZE                      PRGOPTION-REC
                   MOVE    1                   TO  FLG-PRGOPTION
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PRGOPTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
