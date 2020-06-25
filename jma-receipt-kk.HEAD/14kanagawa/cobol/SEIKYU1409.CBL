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
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU1409.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 神奈川・複写式レセプト
      *                      (180,280,181,281,381,185,285,188,189,289)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/27    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者	日付		内容
      *  01.00.01    有村     05/05/11    老人負担者番号・受給者番号が
      *                                   印字されないのを修正
      *  01.00.02    呉秀凡   06/01/13    188と189の追加
      *  01.00.03    内田     07/03/28    特記事項に「１６長２」追加
      *                                   プレビュー時患者番号を表示
      *  01.00.04    内田     07/05/02    対象保険番号の追加（289）
      *  01.00.05    内田     07/06/22    一部負担金の印字追加
      *  01.00.06    内田     07/10/10    日レセver4.0対応
      *                                   システム管理マスタ「1901」の
      *                                   医療機関情報を参照
      *  01.00.07    内田     08/09/17    SPA-AREA初期化
      *  01.00.08    吉川     09/05/01    病名数が2頁以降までまたがると
      *                                   1頁目が印字されない不具合を修正
      *  01.00.09    吉川     10/09/21    実日数がゼロの場合は0を印字
      *  01.00.10    吉川     11/01/25    特記事項に高額療養費を印字する
      *  01.00.11    吉川     12/02/16    主病名が一つのみの場合
      *                                   区切り線の次の番号を正しくする
      *  01.00.12    吉川     12/12/25    医療機関情報編集対応
      *  01.00.13    吉川     14/05/21    保険＋全国公費と保険＋福祉を
      *                                   使用した場合の一部負担金欄の修正
      *  01.00.14    吉川     14/10/09    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.15    吉川     14/12/04    対象公費はプログラムオプション
      *                                   から取得するようにする
      *  01.00.16    吉川     15/01/30    平成２７年１月改正対応
      *  01.00.17    萩野     15/12/10    医療機関編集情報への開設者名追加対応
      *                                   特記事項欄の記載数の追加
      *  01.00.18    門間     17/10/17    医療機関コード変更時、
      *                                   医療機関コード別の作成処理対応
      *                                   SITEKBN変更（動作に変更なし）
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF101-FILE   ASSIGN    MF101PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF101-KEY
                               FILE    STATUS  IS  STS-MF101.
      *
      *    レセプト０３１データ
           SELECT  RECE031-FILE     ASSIGN  RECE031PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *
      *    集計ファイル
       FD  MF101-FILE.
       01  MF101-REC.
           COPY  "SEI1409.INC".
      *
      *    レセプト０３１データ
       FD  RECE031-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF101//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE031//.
      *
      *    エラーファイル 名称領域
      *---(01.00.06) LINE ADD START ------------------------------------
      *     COPY  "CPCOMMONDAT2.INC"
      *             REPLACING  //RECE01PARA//
      *             BY         //RECEERR//.
      *     03      PIC X(04)   VALUE   ".dat".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *     COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
      *                              BY         //RECEERR//.
           COPY    "CPRECEERR.INC".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    MONPE用コピー句
           COPY  "SC1409.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF101       PIC X(02).
           03  STS-RECE02      PIC X(02).
           03  STS-RECEERR     PIC X(02).
           03  STS-RECE        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF       PIC 9(01).
           03  FLG-END         PIC 9(01).
           03  FLG-MF101       PIC 9(01).
           03  FLG-PRINT       PIC 9(01).
           03  FLG-SYSKANRI    PIC 9(01).
           03  FLG-SYSKANRI3   PIC 9(01).
           03  FLG-ERR         PIC 9(01).
           03  FLG-SYUNOU      PIC 9(01).
           03  FLG-RECE10      PIC 9(01).
           03  FLG-PTKOHINF    PIC 9(01).
           03  FLG-RECE031     PIC 9(01).
           03  FLG-HKNCOMBI    PIC 9(01).
           03  FLG-TENKI       PIC 9(01).
           03  FLG-KUGIRI      PIC 9(01).
           03  FLG-BTPARA      PIC 9(01).
           03  FLG-SYORI       PIC 9(01).
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
           03  FLG-PRGOPTION   PIC 9(01).
           03  FLG-OK          PIC 9(01).
           03  FLG-TAISYOKOHI  PIC 9(01).
           03  FLG-TAISYO      PIC 9(01).
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(05).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
      *---(01.00.06) LINE ADD START ------------------------------------
      *     03  WRK-PARA-HOSPID       PIC X(24).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
      *---(01.00.06) LINE ADD END   ------------------------------------
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
           03  WRK-PARA-STSRYYM      PIC 9(06).
           03  WRK-PARA-EDSRYYM      PIC 9(06).
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *
      *    患者病名編集用パラメタ領域
       01  WRK-PARA-LINK.
           05  WRK-PARA-LINK-SRYYM   PIC  X(06).
           05  WRK-PARA-LINK-TERMID  PIC  X(16).
           05  WRK-PARA-LINK-SYSYMD  PIC  X(08).
           05  WRK-PARA-LINK-JOBID   PIC  9(07).
           05  WRK-PARA-LINK-SHELLID PIC  X(08).
      *---(01.00.06) LINE ADD START ------------------------------------
      *     05  WRK-PARA-LINK-HOSPID  PIC  X(24). 
           05  WRK-PARA-LINK-HOSPNUM PIC  9(02). 
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z1                PIC Z.
      *=== 実日数がゼロの場合は0を印字 by Yoshikawa(2010/09/21) start
           03  WRK-Z1-9              PIC Z9.
      *=== 実日数がゼロの場合は0を印字 by Yoshikawa(2010/09/21) end
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-LASTYMD           PIC 9(08).
           03  WRK-NYMD.
               05  WRK-NYY           PIC 9(04).
               05  WRK-NMM           PIC 9(02).
               05  WRK-NDD           PIC 9(02).
           03  WRK-TYMD.
               05  WRK-TYY           PIC 9(04).
               05  WRK-TMM           PIC 9(02).
               05  WRK-TDD           PIC 9(02).
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-PRTID             PIC X(20).
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-ETC-TBL.
               05  WRK-PATH              PIC X(64).
               05  WRK-KOHNUM            PIC X(03).
               05  WRK-KOHID             PIC 9(10).
               05  WRK-ETC-KOHNUM        PIC X(03).
               05  WRK-ETC-KOHID         PIC 9(10).
               05  WRK-FTNMONEY          PIC 9(08).
      *
      *---(01.00.06) LINE ADD START ------------------------------------
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
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
           03  WRK-KAISETUNAME       PIC X(80).
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    病名退避テーブル
           03  WRK-TAIHI-TBL.
                05  WRK-TAIHI-BYOMEI   PIC X(54)   OCCURS  5 TIMES.
                05  WRK-TAIHI-SRYYMD   PIC X(24)   OCCURS  5 TIMES.
                05  WRK-TAIHI-TEKIYO   PIC X(52)   OCCURS  35 TIMES.
      *
           03  WRK-BYO-OLD.
               05  WRK-BO-SYUBYOFLG            PIC X(01).
               05  WRK-BO-SRYYMD               PIC X(08).
               05  WRK-BO-TENKIKBN             PIC X(01).
               05  WRK-BO-TENKIYMD             PIC X(08).
      *
           03  WRK-EDIT-AREA.
      *        転記用カウント
               05  WRK-CNT-TENKI1              PIC 9(01).
               05  WRK-CNT-TENKI2              PIC 9(01).
               05  WRK-CNT-TENKI3              PIC 9(01).
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
                   07  FILLER                  PIC X(20).
                   07  WRK-BYOMEI2-ELSE.
                       09  WRK-BYOMEI2-YMD     PIC X(20).
                       09  WRK-BYOMEI2-TENKI   PIC X(04).
               05  WRK-BYOMEI2R-NAMEW  REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(14).
                   07  WRK-BYOMEI2R-ELSE.
                       09  WRK-BYOMEI2R-YMD    PIC X(20).
                       09  WRK-BYOMEI2R-TENKI  PIC X(04).
                       09  WRK-BYOMEI2R-TENKIYMD
                                               PIC X(06).
      *
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
      *    プログラムオプション
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
      *
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
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
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF101   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-TENKI1  PIC 9(02).
           03  CNT-TENKI2  PIC 9(02).
           03  CNT-TENKI3  PIC 9(02).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM              PIC X(03)  OCCURS  4.
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) start
      *    固定領域
       01  CONS-AREA.
           03  WRK-CONS-SRYYM-201501   PIC X(06)  VALUE  "201501".
      *=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) end
      *
      *=== 特記事項欄の記載数の追加 by hagino(2015/12/10) start
      *    特記事項
       01  TBL-TOKKI-AREA.
           03  FILLER          PIC X(02)   VALUE   "01".
           03  FILLER          PIC X(10)   VALUE   "０１公".
           03  FILLER          PIC X(02)   VALUE   "02".
           03  FILLER          PIC X(10)   VALUE   "０２長".
           03  FILLER          PIC X(02)   VALUE   "03".
           03  FILLER          PIC X(10)   VALUE   "０３長処".
           03  FILLER          PIC X(02)   VALUE   "04".
           03  FILLER          PIC X(10)   VALUE   "０４後保".
           03  FILLER          PIC X(02)   VALUE   "07".
           03  FILLER          PIC X(10)   VALUE   "０７老併".
           03  FILLER          PIC X(02)   VALUE   "08".
           03  FILLER          PIC X(10)   VALUE   "０８老健".
           03  FILLER          PIC X(02)   VALUE   "09".
           03  FILLER          PIC X(10)   VALUE   "０９施".
           03  FILLER          PIC X(02)   VALUE   "10".
           03  FILLER          PIC X(10)   VALUE   "１０第三".
           03  FILLER          PIC X(02)   VALUE   "11".
           03  FILLER          PIC X(10)   VALUE   "１１薬治".
           03  FILLER          PIC X(02)   VALUE   "12".
           03  FILLER          PIC X(10)   VALUE   "１２器治".
           03  FILLER          PIC X(02)   VALUE   "13".
           03  FILLER          PIC X(10)   VALUE   "１３先進".
           03  FILLER          PIC X(02)   VALUE   "14".
           03  FILLER          PIC X(10)   VALUE   "１４制超".
           03  FILLER          PIC X(02)   VALUE   "15".
           03  FILLER          PIC X(10)   VALUE   "１５経過".
           03  FILLER          PIC X(02)   VALUE   "16".
           03  FILLER          PIC X(10)   VALUE   "１６長２".
           03  FILLER          PIC X(02)   VALUE   "17".
           03  FILLER          PIC X(10)   VALUE   "１７上位".
           03  FILLER          PIC X(02)   VALUE   "18".
           03  FILLER          PIC X(10)   VALUE   "１８一般".
           03  FILLER          PIC X(02)   VALUE   "19".
           03  FILLER          PIC X(10)   VALUE   "１９低所".
           03  FILLER          PIC X(02)   VALUE   "20".
           03  FILLER          PIC X(10)   VALUE   "２０二割".
           03  FILLER          PIC X(02)   VALUE   "21".
           03  FILLER          PIC X(10)   VALUE   "２１高半".
           03  FILLER          PIC X(02)   VALUE   "22".
           03  FILLER          PIC X(10)   VALUE   "２２多上".
           03  FILLER          PIC X(02)   VALUE   "23".
           03  FILLER          PIC X(10)   VALUE   "２３多一".
           03  FILLER          PIC X(02)   VALUE   "24".
           03  FILLER          PIC X(10)   VALUE   "２４多低".
           03  FILLER          PIC X(02)   VALUE   "25".
           03  FILLER          PIC X(10)   VALUE   "２５出産".
           03  FILLER          PIC X(02)   VALUE   "26".
           03  FILLER          PIC X(10)   VALUE   "２６区ア".
           03  FILLER          PIC X(02)   VALUE   "27".
           03  FILLER          PIC X(10)   VALUE   "２７区イ".
           03  FILLER          PIC X(02)   VALUE   "28".
           03  FILLER          PIC X(10)   VALUE   "２８区ウ".
           03  FILLER          PIC X(02)   VALUE   "29".
           03  FILLER          PIC X(10)   VALUE   "２９区エ".
           03  FILLER          PIC X(02)   VALUE   "30".
           03  FILLER          PIC X(10)   VALUE   "３０区オ".
           03  FILLER          PIC X(02)   VALUE   "31".
           03  FILLER          PIC X(10)   VALUE   "３１多ア".
           03  FILLER          PIC X(02)   VALUE   "32".
           03  FILLER          PIC X(10)   VALUE   "３２多イ".
           03  FILLER          PIC X(02)   VALUE   "33".
           03  FILLER          PIC X(10)   VALUE   "３３多ウ".
           03  FILLER          PIC X(02)   VALUE   "34".
           03  FILLER          PIC X(10)   VALUE   "３４多エ".
           03  FILLER          PIC X(02)   VALUE   "35".
           03  FILLER          PIC X(10)   VALUE   "３５多オ".
           03  FILLER          PIC X(02)   VALUE   "36".
           03  FILLER          PIC X(10)   VALUE   "３６加治".
      *
       01  TBL-TOKKI-R         REDEFINES   TBL-TOKKI-AREA.
           03  TBL-TOKKI       OCCURS   34.
               05  TBL-TOKKICD             PIC X(02).
               05  TBL-TOKKINAME           PIC X(10).
      *
       01  TBL-TOKKI-MAX                   PIC 9(02)   VALUE   34.
      *=== 特記事項欄の記載数の追加 by hagino(2015/12/10) end
      *
      *    DB参照用
      *---(01.00.06) LINE ADD START ------------------------------------
      *     COPY    "ORCA-DBPATH".
           COPY    "COMMON-SPA".
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *
           COPY    "CPSK2005.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者公費情報マスタ
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY  "MCPDATA.INC".
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
      ****************************************************************
       LINKAGE  SECTION.
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *****************************************************************
      *
       PROCEDURE  DIVISION
               USING COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END = 1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC  SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
      *---(01.00.07) LINE ADD START ------------------------------------
           INITIALIZE  SPA-AREA
      *---(01.00.07) LINE ADD END   ------------------------------------
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
           INITIALIZE  RECEERR
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
           DISPLAY "START"
           PERFORM 100-DBOPEN-SEC
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
                                               WRK-PARA-SYORIFLG
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *---(01.00.06) LINE ADD START ------------------------------------
                                               RECEERR
      *---(01.00.06) LINE ADD END   ------------------------------------
           END-UNSTRING
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    地方公費対象患者用病名ファイル作成
           PERFORM 1001-BYOMEI-HEN-SEC
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU1409"    TO  JOB-PGID
           MOVE    "複写式レセプト" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD         TO    WRK-LASTYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO 
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *     MOVE    "MF101ZZ"                   TO  MF101PARA-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID(1:16)   TO  MF101PARA-TERMID
      **---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  WRK-PARA-HOSPNUM              TO  MF101PARA-HOSPNUM
      **---(01.00.06) LINE ADD END   ------------------------------------
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF101PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF101PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF101PARA-TERMID
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
           MOVE    "RECE031"                   TO  RECE031PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID         TO  RECE031PARA-TERMID
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE  WRK-PARA-HOSPNUM              TO  RECE031PARA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    "RECEERR"                   TO  RECEERR-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID(1:16)   TO  RECEERR-TERMID
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
           MOVE  MF101PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECE031PARA-BASENAME TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF101PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECE031PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM(1:6)
      *                             TO  SYS-1001-STYUKYMD(1:6)
      *                                 SYS-1001-EDYUKYMD(1:6)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    "01"            TO  SYS-1001-STYUKYMD(7:2)
                                       SYS-1001-EDYUKYMD(7:2)
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =  ZERO
               MOVE    MCPDATA-REC TO  SYS-1001-REC
           ELSE
               DISPLAY "*** SEIKYU1409 SYS 1001 ERR ***"
               MOVE  1             TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM(1:6)
      *                             TO  SYS-1002-STYUKYMD(1:6)
      *                                 SYS-1002-EDYUKYMD(1:6)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    "01"            TO  SYS-1002-STYUKYMD(7:2)
                                       SYS-1002-EDYUKYMD(7:2)
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE  SPA-HOSPNUM       TO  SYS-1002-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC TO  SYS-1002-REC
           ELSE
               DISPLAY "*** SEIKYU1409 SYS 1001 ERR ***"
               MOVE    1           TO  FLG-END
           END-IF
      *
      *    レセプト編集区分情報
           INITIALIZE                   SYS-2005-REC
           MOVE    "2005"           TO  SYS-2005-KANRICD
           MOVE    "*"              TO  SYS-2005-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                   TO  SYS-2005-STYUKYMD(1:6)
                                       SYS-2005-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-2005-STYUKYMD(7:2)
                                       SYS-2005-EDYUKYMD(7:2)
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE  SPA-HOSPNUM       TO  SYS-2005-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
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
      *---(01.00.06) LINE ADD START ------------------------------------
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-BTPARA-KEY5
      *                                     TO  MCP-PATH
      *         MOVE    "DBSELECT"          TO  MCP-FUNC
      *         CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
               IF      MCP-RC              =   ZERO
      *---(01.00.06) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-BTPARA-KEY5
      *                                     TO  MCP-PATH
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  WRK-PARA-HOSPID      TO   WRK-PARA-LINK-HOSPID
           MOVE  WRK-PARA-HOSPNUM     TO   WRK-PARA-LINK-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           DISPLAY "編集１開始"
           CALL  "SEIKYU14BYOMEI1"    USING  WRK-PARA-LINK
           DISPLAY "編集２開始"
           CALL  "SEIKYU14BYOMEI2"    USING  WRK-PARA-LINK
           DISPLAY "病名ファイル作成終了"
      *
           .
       1001-BYOMEI-HEN-EXT.
           EXIT.
      *
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU1409"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
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
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
           OPEN     OUTPUT  MF101-FILE
           CLOSE            MF101-FILE
           OPEN     I-O     MF101-FILE
           OPEN    INPUT    RECE031-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF101-FILE
      *
           IF       CNT-RECE02   >       0
                    PERFORM   2002-PRINT-SEC
           END-IF
           MOVE   1   TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
           DISPLAY "2001 START"
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
      *         IF      RECE02-KOHNUM (IDZ) = "180" OR "280" OR "181"
      *                           OR "281" OR "381" OR "185" OR "285"
      **---(01.00.04) LINE ADD START ------------------------------------
      *****2006/01/13 @@@@-BRAND  START
      **                           OR "188" OR "189"
      *****2006/01/13 @@@@-BRAND  END
      *                           OR "188" OR "189" OR "289"
      **---(01.00.04) LINE ADD END   ------------------------------------
               INITIALIZE                            TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(IDZ)        TO  TBL-HKOHNUM(1)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
                   PERFORM 20011-SYUKEI-SEC
               END-IF
           END-PERFORM
      *
      *
           PERFORM  900-KOHSKY-READ-SEC
           DISPLAY "2001 END"
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF101-REC
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    RECE02-HOSPID       TO  MF101-HOSPID
           MOVE    RECE02-HOSPNUM      TO  MF101-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    RECE02-PTNUM        TO  MF101-PTNUM
           MOVE    RECE02-RECEKA       TO  MF101-SRYKA
           MOVE    RECE02-HKNJANUM-KEY TO  MF101-HKNJANUM-KEY
           MOVE    RECE02-TEKSTYMD     TO  MF101-TEKSTYMD
           MOVE    RECE02-NYUGAIKBN    TO  MF101-NYUGAIKBN
           MOVE    RECE02-SRYYM        TO  MF101-SRYYM
           MOVE    RECE02-TEISYUTUSAKI TO  MF101-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  MF101-RECESYUBETU
           MOVE    RECE02-HOJOKBN-KEY  TO  MF101-HOJOKBN-KEY
      *
           MOVE    RECE02-PTID         TO  MF101-PTID
           MOVE    RECE02-NAME         TO  MF101-NAME
           MOVE    RECE02-KANANAME     TO  MF101-KANANAME
           MOVE    RECE02-BIRTHDAY     TO  MF101-BIRTHDAY
           MOVE    RECE02-SEX          TO  MF101-SEX
           MOVE    RECE02-AGE          TO  MF101-AGE
           MOVE    RECE02-HKN          TO  MF101-HKN
           MOVE    RECE02-RJNHKN       TO  MF101-RJNHKN
           MOVE    RECE02-ZAITAKU      TO  MF101-ZAITAKU
      *
           MOVE    RECE02-KOHFTNJANUM (IDZ)  TO  MF101-FTNJANUM
           MOVE    RECE02-KOHJKYSNUM(IDZ)    TO  MF101-JKYSNUM
           MOVE    RECE02-KOHINF(IDZ)        TO  MF101-KOHINF
           COMPUTE IDY =   IDZ +   1
           MOVE    RECE02-JNISSU(IDY)        TO  MF101-JNISSU
           MOVE    RECE02-TOTALTEN(IDY)      TO  MF101-TOTALTEN
           MOVE    RECE02-SHOKUJIINF(IDY)    TO  MF101-SHOKUJIINF
      *---(01.00.05) LINE ADD START ------------------------------------
           MOVE    RECE02-FTNMONEY (IDY)     TO  MF101-KOHITBFTN
      *---(01.00.05) LINE ADD END   ------------------------------------
      *
      *    収納情報より他法公費情報と一部負担金を取得
           INITIALIZE  WRK-ETC-TBL
           MOVE    MF101-KOHNUM    TO  WRK-KOHNUM
           MOVE    MF101-KOHID     TO  WRK-KOHID
           IF      RECE02-NYUGAIKBN    =   "1"
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYUNOU-KEY33   TO  WRK-PATH
               MOVE    "key33"     TO  WRK-PATH
      *---(01.00.06) LINE ADD END   ------------------------------------
           ELSE
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYUNOU-KEY25   TO  WRK-PATH
               MOVE    "key25"     TO  WRK-PATH
      *---(01.00.06) LINE ADD END   ------------------------------------
           END-IF
      *
           PERFORM  920-SYUNOU-SET-SEC
      *
           IF    WRK-FTNMONEY          >    RECE02-FTNMONEY(1)
               MOVE    RECE02-FTNMONEY(1)   TO  WRK-FTNMONEY
           END-IF
      *
      *=== 一部負担金欄の修正 by Yoshikawa(2014/05/21) start
      *    高額療養費がある場合、高額給付分を負担金に設定する
           IF  (RECE02-ZAITAKU  NOT =  SPACE)
           AND (RECE02-ZAITAKU  NOT =  "1")
               IF  RECE02-FTNMONEY(1)  NOT  =  ZERO
                   MOVE    RECE02-FTNMONEY(1)   TO  WRK-FTNMONEY
               END-IF
           END-IF
      *=== 一部負担金欄の修正 by Yoshikawa(2014/05/21) end
      *
           MOVE    WRK-FTNMONEY      TO  MF101-FTNMONEY
      *
           IF      WRK-ETC-KOHNUM  NOT =   SPACE
               PERFORM 930-PTKOHINF-SET-SEC
               MOVE    WRK-ETC-KOHNUM  TO  MF101-EKOHNUM
               MOVE    WRK-ETC-KOHID   TO  MF101-EKOHID
               MOVE    PTKOH-FTNJANUM  TO  MF101-EKOHFTNJANUM
               MOVE    PTKOH-JKYSNUM   TO  MF101-EKOHJKYSNUM
           END-IF
      *
           MOVE    ZERO    TO  FLG-ERR
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
      *=== 特記事項欄の記載数の追加 by hagino(2015/12/10) start
           IF       IDZ           =     1
           AND      RECE02-KOHNUM(IDZ + 1)  =  SPACE
             PERFORM  900-SEIKYU-READ-SEC
      *      特記事項
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     3
                 IF   RECE10-TOKKI(IDX)  NOT =  SPACE
                   MOVE  RECE10-TOKKI(IDX)  TO  MF101-TOKKI(IDX)
                 END-IF
             END-PERFORM
           END-IF
      *=== 特記事項欄の記載数の追加 by hagino(2015/12/10) end
      *
           IF      FLG-ERR     =   ZERO
               WRITE   MF101-REC
               ADD     1   TO  CNT-MF101
           ELSE
               DISPLAY "対象外患者=" RECE02-PTNUM
                       "負担者番号=" MF101-FTNJANUM
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
           MOVE    RECE02-KOHFTNJANUM(IDZ)(3:5)  TO  WRK-HKNJANUM(1:5)
           DISPLAY "保険者番号=" WRK-HKNJANUM
      *
           IF      WRK-PARA-SYORIFLG   =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
      *
      *    １４４００、１４５００の代表番号が指定されている場合、
      *    横浜市内、川崎市内の全ての区を対象とする
                   IF      (WRK-HKNJANUM(1:5)  =
                                       TBL-PARA-HKNJANUM(IDXX)(1:5))
                   OR      ((WRK-HKNJANUM(1:3) =   "144")
                       AND  (TBL-PARA-HKNJANUM(IDXX)(1:5) = "14400"))
                   OR      ((WRK-HKNJANUM(1:3) =   "145")
                       AND  (TBL-PARA-HKNJANUM(IDXX)(1:5) = "14500"))
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
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           DISPLAY "2002-PRINT START"
      *
           OPEN    INPUT       MF101-FILE
      *
           MOVE    ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
      *
           PERFORM     UNTIL   FLG-PRINT   =  1
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
      *=== 病名による改頁不具合の修正 by Yoshikawa(2009/05/01) start
      *         PERFORM  320-BODY-HEN-SEC
      *=== 病名による改頁不具合の修正 by Yoshikawa(2009/05/01) end
      *
               IF      MF101-NYUGAIKBN =   "1"
                   MOVE    "SC1410.red"    TO  WRK-PRTID
               ELSE
                   MOVE    "SC1409.red"    TO  WRK-PRTID
               END-IF
      *=== 病名による改頁不具合の修正 by Yoshikawa(2009/05/01) start
               PERFORM  320-BODY-HEN-SEC
      *=== 病名による改頁不具合の修正 by Yoshikawa(2009/05/01) end
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    MF101-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理(入院用)
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SC09
      *
      *    診療年月
           MOVE    MF101-SRYYM             TO  WRK-SYMD (1:6)
           MOVE    "01"                    TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)       TO  SC09-PRTYM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN        TO  SC09-HOSPCDN
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *    住所
      *     MOVE  SYS-1002-ADRS             TO  SC09-ADRS
      *
      *    医療機関名
      *     MOVE  SYS-1001-HOSPNAME         TO  SC09-HOSPNAME
      *    住所
           MOVE    WRK-HOSPADRS            TO  SC09-ADRS
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) start
           INSPECT SC09-ADRS               REPLACING ALL "  " BY "　"
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) end
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SC09-HOSPNAME
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) start
           INSPECT SC09-HOSPNAME           REPLACING ALL "  " BY "　"
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) end
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    管理者名
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
      *     MOVE  SYS-1001-KAISETUNAME      TO  SC09-KAISETUNAME
           MOVE  WRK-KAISETUNAME           TO  SC09-KAISETUNAME
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
      *
      *    保険区分
           EVALUATE    MF101-RECESYUBETU(2:1)
               WHEN    "1"
                   MOVE   "◯"            TO  SC09-HKNKBN(1)
               WHEN    "2"
                   MOVE   "◯"            TO  SC09-HKNKBN(2)
               WHEN    "3"
                   MOVE   "◯"            TO  SC09-HKNKBN(3)
               WHEN    "4"
                   MOVE   "◯"            TO  SC09-HKNKBN(4)
           END-EVALUATE
      *
      *    単独併用区分
      ****2006/01/13 @@@@-BRAND  START
      *     IF    MF101-EKOHNUM        =  SPACE
      *         MOVE  "○"  TO  SC09-HEIKBN (1)
      *     ELSE
      *         MOVE  "○"  TO  SC09-HEIKBN (2)
      *     END-IF
           IF    MF101-EKOHNUM        =  SPACE
               MOVE  "○"  TO  SC09-HEIKBN (2)
           ELSE
               MOVE  "○"  TO  SC09-HEIKBN (3)
           END-IF
      ****2006/01/13 @@@@-BRAND  END
      *
      *    本人家族区分
           EVALUATE    MF101-RECESYUBETU(4:1)
               WHEN    "1"
               WHEN    "2"
                   MOVE    "◯"    TO  SC09-HONKZKKBN (1)
               WHEN    "3"
               WHEN    "4"
                   MOVE    "◯"    TO  SC09-HONKZKKBN (2)
               WHEN    "5"
               WHEN    "6"
                   MOVE    "◯"    TO  SC09-HONKZKKBN (3)
               WHEN    "7"
               WHEN    "8"
                   MOVE    "◯"    TO  SC09-HONKZKKBN (4)
               WHEN    "9"
               WHEN    "0"
                   MOVE    "◯"    TO  SC09-HONKZKKBN (5)
           END-EVALUATE
      *
      *    記号
           MOVE  MF101-KIGO          TO  SC09-KIGO
      *    番号
           MOVE  MF101-NUM           TO  SC09-BANGO
      *    患者番号
           MOVE  MF101-PTNUM         TO  SC09-PTNUM
      *
      *    性別
           IF      MF101-SEX       =   "1"
               MOVE    "◯"        TO    SC09-SEX(1)
           ELSE
               MOVE    "◯"        TO    SC09-SEX(2)
           END-IF
      *
      *    生年月日
           MOVE  MF101-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           EVALUATE  LNK-DAY2-EDTYMD1(1:1)
               WHEN  "M"
                   MOVE  "○"  TO  SC09-NENGO (1)
               WHEN  "T"
                   MOVE  "○"  TO  SC09-NENGO (2)
               WHEN  "S"
                   MOVE  "○"  TO  SC09-NENGO (3)
               WHEN  "H"
                   MOVE  "○"  TO  SC09-NENGO (4)
           END-EVALUATE
           MOVE    LNK-DAY2-EDTYMD1(2:8)   TO  SC09-BIRTHDAY 
      *
      *    特記事項
      *=== 特記事項欄の記載数の追加 by hagino(2015/12/10) start
      *     IF      MF101-TYOKI     =  1
      *         MOVE    "０２長"    TO  SC09-TOKKI
      *     END-IF
      **---(01.00.03) LINE ADD START ------------------------------------
      *     IF      MF101-TYOKI     =  2
      *         MOVE    "１６長２"  TO  SC09-TOKKI
      *     END-IF
      **---(01.00.03) LINE ADD END   ------------------------------------
      **
      **=== 特記事項に高額療養費を印字する by Yoshikawa(2011/01/25) start
      **    高額療養費
      **=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) start
      *     IF  MF101-SRYYM  >=  WRK-CONS-SRYYM-201501
      *       EVALUATE  MF101-ZAITAKU 
      *         WHEN  "2"
      *           MOVE    "２６区ア"  TO  SC09-TOKKI
      *         WHEN  "3"
      *           MOVE    "２７区イ"  TO  SC09-TOKKI
      *         WHEN  "4"
      *           MOVE    "２８区ウ"  TO  SC09-TOKKI
      *         WHEN  "5"
      *           MOVE    "２９区エ"  TO  SC09-TOKKI
      *         WHEN  "6"
      *           MOVE    "３０区オ"  TO  SC09-TOKKI
      *       END-EVALUATE
      *     ELSE
      **=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) end
      *       EVALUATE  MF101-ZAITAKU 
      *         WHEN  "2"
      *           MOVE    "１７上位"  TO  SC09-TOKKI
      *         WHEN  "3"
      *           MOVE    "１８一般"  TO  SC09-TOKKI
      *         WHEN  "4"
      *           MOVE    "１９低所"  TO  SC09-TOKKI
      *       END-EVALUATE
      **=== 特記事項に高額療養費を印字する by Yoshikawa(2011/01/25) end
      **=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) start
      *     END-IF
      **=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) end
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL  IDY  >  TBL-TOKKI-MAX
               IF  MF101-TOKKI(IDX)  =  TBL-TOKKICD(IDY)
                   MOVE  TBL-TOKKINAME(IDY)   TO  SC09-TOKKI(IDX)
                   MOVE  TBL-TOKKI-MAX  TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
      *=== 特記事項欄の記載数の追加 by hagino(2015/12/10) end
      *
           IF      MF101-ZAITAKU   =  "1"
               MOVE    "７０"      TO  SC09-ZAISOU
               MOVE    "○"        TO  SC09-ZAIMARU
           END-IF
      *
      *    氏名
           MOVE    MF101-NAME          TO  SC09-NAME
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理(入院用)
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    保険者番号
           MOVE  MF101-HKNJANUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           IF     MF101-HKNNUM  =  "001"
             MOVE  WRK-OUT-INPUT(1:8) TO  SC09-HKNJANUM(9:8)
           ELSE
             MOVE  WRK-OUT-INPUT      TO  SC09-HKNJANUM
           END-IF
      *
      ***** 05/05/11 INS
      *    老人市町村番号
           MOVE    MF101-RJNFTNJANUM       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC09-RJNFTNJANUM
      *    老人受給者番号
           MOVE    MF101-RJNJKYSNUM        TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC09-RJNJKYSNUM
      *****
           IF      MF101-EKOHFTNJANUM  NOT =   SPACE
               MOVE    2       TO  IDX1
               MOVE    1       TO  IDX2
           ELSE
               MOVE    1       TO  IDX1
               MOVE    2       TO  IDX2
           END-IF 
      *    負担者番号
             MOVE    MF101-KOHFTNJANUM   TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT       TO  SC09-FTNJANUM(IDX1)
      *
           MOVE    MF101-EKOHFTNJANUM  TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  SC09-FTNJANUM(IDX2)
      *
      *    受給者番号
             MOVE    MF101-KOHJKYSNUM    TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT       TO  SC09-JKYSNUM(IDX1)
      *
           MOVE    MF101-EKOHJKYSNUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  SC09-JKYSNUM(IDX2)
      *
      *    日数
      *=== 実日数がゼロの場合は0を印字 by Yoshikawa(2010/09/21) start
      *     MOVE    MF101-JNISSU        TO  WRK-Z2
      *     MOVE    WRK-Z2              TO  SC09-NISSU  
           MOVE    MF101-JNISSU        TO  WRK-Z1-9
           MOVE    WRK-Z1-9            TO  SC09-NISSU  
      *=== 実日数がゼロの場合は0を印字 by Yoshikawa(2010/09/21) end
      *
      *    点数
           MOVE    MF101-TOTALTEN      TO  WRK-Z7
           MOVE    WRK-Z7              TO  SC09-TENSU
      *
      *    一部負担金
           MOVE    MF101-FTNMONEY      TO  WRK-Z7
           MOVE    WRK-Z7              TO  SC09-ITBFTN
      *
      *---(01.00.05) LINE ADD START ------------------------------------
      *    公費一部負担金
           MOVE    MF101-KOHITBFTN     TO  WRK-Z7
           MOVE    WRK-Z7              TO  SC09-ITBFTN2
      *---(01.00.05) LINE ADD END   ------------------------------------
      *
      *    食事日数
           MOVE    MF101-SHOKUJINISSU  TO  WRK-Z2
           MOVE    WRK-Z2              TO  SC09-SNISSU
      *
      *    食事療養費
           MOVE    MF101-SHOKUJIRYOYOHI    TO  WRK-Z7
           MOVE    WRK-Z7              TO  SC09-SKINGK
      *
      *    標準負担額
           MOVE    MF101-SHOKUJIFTN    TO  WRK-Z7
           MOVE    WRK-Z7              TO  SC09-SFTNGK
      *
      *    傷病名編集処理
           INITIALIZE  WRK-TAIHI-TBL
           PERFORM 3201-BYOMEI-HEN-SEC
           MOVE    WRK-TAIHI-TBL            TO  SC09-BYOMEI-TBL
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理(入院用)
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
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
           MOVE    WRK-PRTID           TO  SPRT-PRTID
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    MF101-PTID          TO  SPRT-PTID
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    "複写式レセプト"
                                    TO  SPRT-TITLE
           MOVE    SC09             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      *=== SITEKBN変更 by Monma(2017/10/17) start
           MOVE    "2"                 TO  SPRT-SITEKBN
      *=== SITEKBN変更 by Monma(2017/10/17) end
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *---(01.00.06) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    傷病名編集処理
      *****************************************************************
       3201-BYOMEI-HEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  WRK-BYO-OLD
      *     
           INITIALIZE                  WRK-EDIT-AREA
           MOVE    ZERO                TO  CNT-TENKI1
                                           CNT-TENKI2
                                           CNT-TENKI3
           MOVE    SPACE               TO  WRK-BYOMEI1       
      *
           INITIALIZE                     RECE031-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    MF101-HOSPID       TO  RECE031-HOSPID
           MOVE    MF101-HOSPNUM      TO  RECE031-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    MF101-SRYYM        TO  RECE031-SRYYM
           MOVE    MF101-NYUGAIKBN    TO  RECE031-NYUGAIKBN
           MOVE    MF101-PTID         TO  RECE031-PTID
           MOVE    MF101-SRYKA        TO  RECE031-RECEKA
           MOVE    MF101-TEISYUTUSAKI TO  RECE031-TEISYUTUSAKI
           MOVE    MF101-RECESYUBETU  TO  RECE031-RECESYUBETU
           MOVE    MF101-HKNJANUM-KEY TO  RECE031-HKNJANUM
           MOVE    MF101-HOJOKBN-KEY  TO  RECE031-HOJOKBN
           MOVE    MF101-TEKSTYMD     TO  RECE031-TEKSTYMD
      *
           START   RECE031-FILE
               KEY IS    >=   RECE031-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE031
               NOT INVALID
                   PERFORM 990-RECE031-NEXT-SEC
           END-START
           DISPLAY "フラグ="      FLG-RECE031
           DISPLAY "診療年月1="   MF101-SRYYM
           DISPLAY "入外区分1="   MF101-NYUGAIKBN
           DISPLAY "患者ID1="     MF101-PTID
           DISPLAY "保険者番号1=" MF101-HKNJANUM-KEY
           DISPLAY "レセプト科1=" MF101-SRYKA
           DISPLAY "摘要開始日1=" MF101-TEKSTYMD
           DISPLAY "提出先1="     MF101-TEISYUTUSAKI
           DISPLAY "レセ種別1="   MF101-RECESYUBETU
           DISPLAY "補助区分1="   MF101-HOJOKBN-KEY
           DISPLAY "---------------------------"
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
      *
           PERFORM
               UNTIL  ( FLG-RECE031          =   1                    )
      *---(01.00.06) LINE ADD START ------------------------------------
      *         OR     ( MF101-HOSPID     NOT =   RECE031-HOSPID       )
               OR     ( MF101-HOSPNUM    NOT =   RECE031-HOSPNUM      )
      *---(01.00.06) LINE ADD END   ------------------------------------
               OR     ( MF101-SRYYM      NOT =   RECE031-SRYYM        )
               OR     ( MF101-NYUGAIKBN  NOT =   RECE031-NYUGAIKBN    )
               OR     ( MF101-PTID       NOT =   RECE031-PTID         )
               OR     ( MF101-SRYKA      NOT =   RECE031-RECEKA       )
               OR     ( MF101-TEISYUTUSAKI
                                         NOT =   RECE031-TEISYUTUSAKI )
               OR     ( MF101-RECESYUBETU
                                         NOT =   RECE031-RECESYUBETU  )
               OR     ( MF101-HKNJANUM-KEY
                                         NOT =   RECE031-HKNJANUM     )
               OR     ( MF101-HOJOKBN-KEY
                                         NOT =   RECE031-HOJOKBN      )
               OR     ( MF101-TEKSTYMD   NOT =   RECE031-TEKSTYMD     )
      *    
                   DISPLAY "病名編集開始"
      *        病名編集処理
               PERFORM 3210-BYOMEI-SET-SEC
      *
           END-PERFORM
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
           DISPLAY "SYS2005-SYUBYOHENFLG =" SYS-2005-SYUBYOHENFLG
                   "主病名フラグ=" WRK-BO-SYUBYOFLG
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
      *    ３番までは番号、年月日を印字する
      *=== 主病名のみの場合の番号修正 by Yoshikawa(2012/02/16) start
      *     MOVE  WRK-LINEBYO    TO    IDX2
           MOVE  WRK-LINE       TO    IDX2
      *=== 主病名のみの場合の番号修正 by Yoshikawa(2012/02/16) end
           ADD   1              TO    IDX2
           PERFORM  VARYING  IDX1  FROM  IDX2  BY  1
                    UNTIL    IDX1  >    3
      *=== 主病名のみの場合の番号修正 by Yoshikawa(2012/02/16) start
             ADD     1               TO  WRK-LINEBYO
      *=== 主病名のみの場合の番号修正 by Yoshikawa(2012/02/16) end
             MOVE  IDX1              TO  WRK-Z1
             INITIALIZE              ORCSKANACHKAREA
             MOVE    "2"             TO  KANACHK-SYORI 
             MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
             MOVE    WRK-Z1          TO  KANACHK-MAE-INPUT (2:1)
             MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
             CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
      *=== 主病名のみの場合の番号修正 by Yoshikawa(2012/02/16) start
      *           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
      *                                   TO  WRK-TAIHI-BYOMEI(IDX1)
      *                                       WRK-TAIHI-SRYYMD(IDX1)
                 MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-TAIHI-BYOMEI(WRK-LINEBYO)
                                           WRK-TAIHI-SRYYMD(WRK-LINEBYO)
      *=== 主病名のみの場合の番号修正 by Yoshikawa(2012/02/16) end
             END-IF
      *=== 主病名のみの場合の番号修正 by Yoshikawa(2012/02/16) start
      *       MOVE  "　　年　　月　　日"  TO  WRK-TAIHI-SRYYMD(IDX1)(7:)
             MOVE  "　　年　　月　　日"  
                                   TO  WRK-TAIHI-SRYYMD(WRK-LINEBYO)(7:)
      *=== 主病名のみの場合の番号修正 by Yoshikawa(2012/02/16) end
           END-PERFORM
      *
      *
           .
       3201-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       3210-BYOMEI-SET-SEC  SECTION.
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
      *
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      *
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
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-TAIHI-SRYYMD
                                                    (WRK-LINEBYO)(7:)
      **        転帰日
      *         IF      WRK-BO-TENKIYMD     =   SPACE
      *             CONTINUE 
      *         ELSE
      *             MOVE    WRK-BO-TENKIYMD TO  WRK-SYMD
      *             MOVE    WRK-SDD         TO  WRK-Z2
      *             EVALUATE    WRK-BO-TENKIKBN
      *             WHEN    "1"
      *                 MOVE    WRK-Z2  TO  WRK-TAIHI-TENKI1(WRK-LINEBYO)
      *             WHEN    "2"
      *                 MOVE    WRK-Z2  TO  WRK-TAIHI-TENKI2(WRK-LINEBYO)
      *             WHEN    "3"
      *                 MOVE    WRK-Z2  TO  WRK-TAIHI-TENKI3(WRK-LINEBYO)
      *             END-EVALUATE
      *         END-IF                                     
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
               MOVE    WRK-BYOMEI2 TO  WRK-TAIHI-TEKIYO  (WRK-LINETEK)

           END-IF
      *
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
      *
      ******************************************************************
      **    改ページ処理
      ******************************************************************
       34120-PAGE-SEC       SECTION.
      *
      *
      *    帳票印刷処理
           MOVE    WRK-TAIHI-TBL            TO  SC09-BYOMEI-TBL
           PERFORM 330-PRINT-OUT-SEC
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HEAD-HEN-SEC
           INITIALIZE  WRK-TAIHI-TBL
           MOVE    ZERO          TO  WRK-LINETEK
      *
           .
       34120-PAGE-EXT.
           EXIT.
      **
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF  STS-RECEERR  =  ZERO
             CONTINUE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE  WRK-RECEERR  TO  RECEERR-REC
             WRITE RECEERR-REC
           END-IF
      *
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
           CLOSE                       RECE031-FILE
      *
      *
           CLOSE  MF101-FILE
           MOVE   CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU1409 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1409 OUT "  CNT-MF101
           DISPLAY "*** SEIKYU1409 END ***"
      *
           IF  CNT-MF101  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
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
             IF  TBL-HKOHNUM(1)  =  WRK-TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(2)  =  WRK-TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(3)  =  WRK-TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(4)  =  WRK-TAISYO-KOHNUM(IDXX)
               MOVE     1                   TO  FLG-TAISYO
               MOVE     100                 TO  IDXX
             END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF101-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
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
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    WRK-PARA-HOSPID     TO  RECE02-HOSPID
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *    公費請求書マスタＲＥＡＤ
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
      ****2006/01/13 @@@@-BRAND  START
      *---(01.00.04) LINE ADD START ------------------------------------
      *         IF      RECE02-KOHNUM(1) = "180" OR "280" OR "181"
      *                        OR "281" OR "381" OR "185" OR "285"
      *                        OR "188" OR "189"
      *         OR      RECE02-KOHNUM(2) = "180" OR "280" OR "181"
      *                        OR "281" OR "381" OR "185" OR "285"
      *                        OR "188" OR "189"
      *         OR      RECE02-KOHNUM(3) = "180" OR "280" OR "181"
      *                        OR "281" OR "381" OR "185" OR "285"
      *                        OR "188" OR "189"
      *         OR      RECE02-KOHNUM(4) = "180" OR "280" OR "181"
      *                        OR "281" OR "381" OR "185" OR "285"
      *                        OR "188" OR "189"
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
      *         IF      RECE02-KOHNUM(1) = "180" OR "280" OR "181"
      *                        OR "281" OR "381" OR "185" OR "285"
      *                        OR "188" OR "189" OR "289"
      *         OR      RECE02-KOHNUM(2) = "180" OR "280" OR "181"
      *                        OR "281" OR "381" OR "185" OR "285"
      *                        OR "188" OR "189" OR "289"
      *         OR      RECE02-KOHNUM(3) = "180" OR "280" OR "181"
      *                        OR "281" OR "381" OR "185" OR "285"
      *                        OR "188" OR "189" OR "289"
      *         OR      RECE02-KOHNUM(4) = "180" OR "280" OR "181"
      *                        OR "281" OR "381" OR "185" OR "285"
      *                        OR "188" OR "189" OR "289"
               INITIALIZE                            TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(1)          TO  TBL-HKOHNUM(1)
               MOVE    RECE02-KOHNUM(2)          TO  TBL-HKOHNUM(2)
               MOVE    RECE02-KOHNUM(3)          TO  TBL-HKOHNUM(3)
               MOVE    RECE02-KOHNUM(4)          TO  TBL-HKOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
      *---(01.00.04) LINE ADD END   ------------------------------------
      ****2006/01/13 @@@@-BRAND  END
      *        テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *             MOVE    RECE02-HOSPID       TO  PTINF-HOSPID
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF   
      *
      *        個別発行で患者番号指定の場合、対象判定を行う
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
               MOVE    1               TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-PTINF-KEY      TO  MCP-PATH
      *         CALL    "MCPSUB"                USING
      *                                             MCPAREA
      *                                             MCPDATA-REC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    SYU-HOSPID              TO  COMB-HOSPID
           MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    SYU-PTID                TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM         TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNCOMBI-KEY   TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-HKNCOMBI-KEY   TO  MCP-PATH
      *         CALL    "MCPSUB"                USING
      *                                             MCPAREA
      *                                             MCPDATA-REC
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNCOMBI-KEY   TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =  SPACE
      *
               IF      SYU-KOH-HKNNUM(IDX)(1:1) =  "0" AND
                       SYU-KOH-HKNNUM(IDX) NOT  =  "027"
      *
      *    全国公費点数の公費IDを取得する
      *    (一部負担金はレセプトに記載する場合のみ集計する)
                   MOVE    SYU-KOH-HKNNUM(IDX)  TO  WRK-ETC-KOHNUM
                   MOVE    COMB-KOHID(IDX)      TO  WRK-ETC-KOHID
               END-IF
      *
               IF      SYU-KOH-HKNNUM(IDX) =   "972"
                   MOVE    1       TO  MF101-TYOKI
               END-IF
      *---(01.00.03) LINE ADD START ------------------------------------
               IF      SYU-KOH-HKNNUM(IDX) =   "974"
                   MOVE    2       TO  MF101-TYOKI
               END-IF
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           END-PERFORM
      *
      *    レセプト記載する場合のみ主保険一部負担金を取得
           IF      RECE02-FTNMONEY(1)  >   ZERO
               IF    RECE02-RJNHKNNUM  NOT =   SPACE
                   ADD     SYU-KOH-COMPFTN(1)  TO  WRK-FTNMONEY
               ELSE
                   ADD     SYU-SYUCOMPFTN      TO  WRK-FTNMONEY
               END-IF
           END-IF
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報取得
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
           INITIALIZE  SYUNOU-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    RECE02-HOSPID       TO  SYU-HOSPID
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    RECE02-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    RECE02-PTID         TO  SYU-PTID
           MOVE    RECE02-SRYYM(1:6)   TO  SYU-SRYYMD(1:6)
           MOVE    "__"                TO  SYU-SRYYMD(7:2)
           MOVE    WRK-KOHNUM          TO  SYU-KOH-HKNNUM(1)
                                           SYU-KOH-HKNNUM(2)
                                           SYU-KOH-HKNNUM(3)
                                           SYU-KOH-HKNNUM(4)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    WRK-PATH            TO  MCP-PATH
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           MOVE  "tbl_syunou"       TO  MCP-TABLE
           MOVE  WRK-PATH           TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    WRK-PATH        TO  MCP-PATH
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
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
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF  COMB-KOHID(1)    =  WRK-KOHID   OR
                         COMB-KOHID(2)    =  WRK-KOHID   OR
                         COMB-KOHID(3)    =  WRK-KOHID   OR
                         COMB-KOHID(4)    =  WRK-KOHID
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *           MOVE  WRK-PATH            TO  MCP-PATH
                 MOVE  "tbl_syunou"       TO  MCP-TABLE
                 MOVE  WRK-PATH           TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY  "SYUNOU SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    WRK-PATH                TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE  "tbl_syunou"       TO  MCP-TABLE
           MOVE  WRK-PATH           TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
           MOVE    "DBFETCH"               TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC      =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE      SYUNOU-REC
               MOVE    1   TO  FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *=== 特記事項欄の記載数の追加 by hagino(2015/12/10) start
      *****************************************************************
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY                  TO  RECE10-KEY
           MOVE    RECE10-REC                  TO  MCPDATA-REC
           MOVE    "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                 TO  MCP-FUNC
               MOVE    "tbl_seikyu"            TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE10
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE10
                   INITIALIZE                  RECE10-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
               INITIALIZE                  RECE10-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *=== 特記事項欄の記載数の追加 by hagino(2015/12/10) end
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE                      PTKOHINF-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    RECE02-HOSPID       TO  PTKOH-HOSPID
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    RECE02-PTID         TO  PTKOH-PTID
           MOVE    WRK-ETC-KOHID       TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY   TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE  "tbl_ptkohinf"     TO  MCP-TABLE
           MOVE  "key"              TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-PTKOHINF-KEY   TO  MCP-PATH
      *         CALL    "MCPSUB"                USING
      *                                             MCPAREA
      *                                             MCPDATA-REC
               MOVE  "tbl_ptkohinf"     TO  MCP-TABLE
               MOVE  "key"              TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY   TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE  "tbl_ptkohinf"     TO  MCP-TABLE
           MOVE  "key"              TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           .
       930-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       935-PTKOHINF-READ-SEC  SECTION.
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY   TO  MCP-PATH
      *     MOVE    "DBFETCH"               TO  MCP-FUNC
      *     CALL    "MCPSUB"                USING
      *                                             MCPAREA
      *                                             MCPDATA-REC
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  PTKOHINF-REC
             MOVE  ZERO         TO  FLG-PTKOHINF
           ELSE
             INITIALIZE  PTKOHINF-REC
             MOVE  1  TO  FLG-PTKOHINF
           END-IF
      *
           .
       935-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SYSKANRI-KEY10 TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYSKANRI-KEY10   TO  MCP-PATH
      *         CALL    "MCPSUB"                USING
      *                                             MCPAREA
      *                                             MCPDATA-REC
               MOVE  "tbl_syskanri"          TO  MCP-TABLE
               MOVE  "key10"                 TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"             USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
               IF  MCP-RC  =  ZERO
                   MOVE  ZERO  TO  FLG-SYSKANRI
               ELSE
                   MOVE  1  TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SYSKANRI-KEY10   TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *---(01.00.06) LINE ADD START ------------------------------------
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
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-STYUKYMD(1:6)
      *     MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
      *     MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-EDYUKYMD(1:6)
      *     MOVE    "01"                    TO  SYS-1900-EDYUKYMD(7:2)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
      *            医療機関住所編集情報（２）
                   PERFORM 900-1903-READ-SEC
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
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
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-EDYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1901-EDYUKYMD(7:2)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
                                           SYS-1901-EDYUKYMD(7:2)
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
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
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-EDYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1902-EDYUKYMD(7:2)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
                                           SYS-1902-EDYUKYMD(7:2)
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
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
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
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
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD (1:6)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
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
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
      *
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) start
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
      *=== 対象公費をPGオプションから取得 by Yoshikawa(2014/12/04) end
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                 MCPAREA
      *                                 MCPDATA-REC
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    "DBOPEN"        TO  MCP-FUNC.
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"        USING
      *                                 MCPAREA
      *                                 MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    "DBSTART"       TO  MCP-FUNC.
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"        USING
      *                                 MCPAREA
      *                                 MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC  SECTION.
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"        USING
      *                                 MCPAREA
      *                                 MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"        USING
      *                                MCPAREA
      *                                MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
