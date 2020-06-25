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
       PROGRAM-ID.     SEIKYU0307.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 岩手・複写式レセプト
      *                      (150,110,130,140,120,199)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/17    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            (個別発行対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者	日付		内容
      *  01.00.01     内田    2007/05/08  プレビュー時の患者番号を印字
      *  01.00.02     内田    2007/10/06  日レセver4.0対応
      *                                   システム管理マスタ「1901」の
      *                                   医療機関情報を参照
      *                                   長期上位（974）は長期と同様の
      *                                   処理をする
      *  01.00.03     内田    2008/05/16  ４月改正対応
      *  01.00.04     門間    2008/10/09  SPA-AREAの初期化
      *  01.00.05     内田    2009/11/13  帳票上部の公費種別は市町村公費の場合
      *                                   受給者番号の事業コードで判別する
      *  01.00.06     吉川    2011/01/17  帳票様式の変更に伴う修正
      *  01.00.07     吉川    2012/12/19  医療機関情報編集修正
      *  01.00.08     吉川    2014/10/03  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.09     吉川    2016/04/14  レセプト印刷順をPGオプションで
      *                                   設定できるように修正
      *                                   医療機関編集情報への開設者名追加対応
      *  01.00.10     萩野    2016/06/08  対象公費はPGオプションから取得する
      *  01.00.11     吉川    2016/08/29  公費の助成がない場合は印字しない
      *                                   受給者番号順処理修正
      *  01.00.12     萩野    2016/10/07  特記事項欄の記載数を追加
      *  01.00.13     門間    2017/01/11  医療機関コード変更時、
      *                                   医療機関コード別の作成処理対応
      *  01.00.14     萩野    2017/03/08  医療機関情報の取得の修正
      *                                   受給者番号順１１桁対応
      *  01.00.15     萩野    2017/05/12  開設者名を記載しないようにする
      *  01.00.16     門間    2018/04/09  SITEKBN変更（動作に変更なし）
      *  01.00.17     萩野    2018/05/08  特記事項に印字内容追加
      *                       2018/05/09  公費欄−一部負担金金額置換対応
      *                       2018/05/10  保険欄−一部負担金金額置換編集
      *  01.00.18     井上    2018/10/10  修正履歴コメント削除
      *  01.00.19     吉川    2019/04/25  元号改正対応
      *  01.00.20     井上    2020/01/15  現物給付の場合、市町村番号欄へ
      *                                   受給者番号を印字するよう修正。
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
           COPY  "SEI0307.INC".
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
           COPY  "CPRECEERR.INC".
      *
      *    MONPE用コピー句
           COPY  "SC0307.INC".
           COPY  "SC0308.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF101      PIC X(02).
           03  STS-RECE02     PIC X(02).
           03  STS-RECEERR    PIC X(02).
           03  STS-RECE       PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-MF101      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-SYSKANRI3  PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-RECE10     PIC 9(01).
           03  FLG-PTKOHINF   PIC 9(01).
           03  FLG-RECE031    PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-TENKI      PIC 9(01).
           03  FLG-KUGIRI     PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
           03  FLG-SRYACT            PIC 9(01).
           03  FLG-PTCOM             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(05).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDX4           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
           03  IDW1                  PIC 9(04).
           03  IDW2                  PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG     PIC X(01).
           03  WRK-PARA-STSRYYM      PIC 9(06).
           03  WRK-PARA-EDSRYYM      PIC 9(06).
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
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z1                PIC Z.
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
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOBETUCITYNUM     PIC X(02).
           03  WRK-RECESYUTURYOKU    PIC X(02).
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-KOHID               PIC 9(10).
           03  WRK-FTNMONEY            PIC 9(08).
           03  WRK-FTNMONEY-G          PIC 9(08).
      *
           03  WRK-PATH              PIC  X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
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
           03  WRK-ETC-TBL.
               05  WRK-ETC-KOHNUM          PIC X(03) OCCURS 2.
               05  WRK-ETC-TENSU           PIC 9(07) OCCURS 2.
               05  WRK-ETC-ITBFTN          PIC 9(07) OCCURS 2.
               05  WRK-ETC-SHOKUJIRYOYOHI  PIC 9(07) OCCURS 2.
               05  WRK-ETC-SHOKUJIFTN      PIC 9(07) OCCURS 2.
               05  WRK-ETC-FTNJANUM        PIC X(20).
               05  WRK-ETC-JKYSNUM         PIC X(20).
      *
      *    病名退避テーブル
           03  WRK-TAIHI-TBL.
                05  WRK-TAIHI-BYOMEI   PIC X(54)   OCCURS  5 TIMES.
                05  WRK-TAIHI-SRYYMD   PIC X(26)   OCCURS  5 TIMES.
                05  WRK-TAIHI-TENKI1   PIC X(02)   OCCURS  5 TIMES.
                05  WRK-TAIHI-TENKI2   PIC X(02)   OCCURS  5 TIMES.
                05  WRK-TAIHI-TENKI3   PIC X(02)   OCCURS  5 TIMES.
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
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
           03  WRK-SRYCD             PIC X(09).
           03  WRK-INPUTCOMENT       PIC X(80).
           03  WRK-INPUTCOMENT2      PIC X(14).
           03  WRK-INPUTCOMENT-KIN-T.
               05  WRK-INPUTCOMENT-KIN     PIC 9(07).
           03  WRK-INPUTCOMENT-KIN2-T.
               05  WRK-INPUTCOMENT-KIN2    PIC X(07).
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
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *    保険欄一部負担金置換コード
           03  WRK-CONS-KIN-OKIKAE-HKN  PIC X(09)  VALUE   "099999910".
      *    公費一部負担金置換コード
           03  WRK-CONS-KIN-OKIKAE     PIC X(09)   VALUE   "099999921".
      *
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
           03  FILLER          PIC X(02)   VALUE   "37".
           03  FILLER          PIC X(10)   VALUE   "３７申出".
           03  FILLER          PIC X(02)   VALUE   "38".
           03  FILLER          PIC X(10)   VALUE   "３８医併".
           03  FILLER          PIC X(02)   VALUE   "39".
           03  FILLER          PIC X(10)   VALUE   "３９医療".
      *
       01  TBL-TOKKI-R         REDEFINES   TBL-TOKKI-AREA.
           03  TBL-TOKKI       OCCURS   37.
               05  TBL-TOKKICD             PIC X(02).
               05  TBL-TOKKINAME           PIC X(10).
      *
       01  TBL-TOKKI-MAX                   PIC 9(02)   VALUE   37.
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
      *    レセプト作成指示(印刷順)情報
           COPY    "CPSK2003.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
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
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
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
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
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
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY  "MCPDATA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
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
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SYORIFLG
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    地方公費対象患者用病名ファイル作成
           PERFORM 1001-BYOMEI-HEN-SEC
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU0307"    TO  JOB-PGID
           MOVE    "複写式レセプト" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
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
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF101PARA-HOSPNUM
           MOVE  "MF101ZZ"            TO  MF101PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF101PARA-TERMID
      *
           MOVE    "RECE031"                   TO  RECE031PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID         TO  RECE031PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM              TO  RECE031PARA-HOSPNUM
      *
           MOVE  MF101PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECE031PARA-BASENAME TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF101PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECE031PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
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
           MOVE  "01"          TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU0307 SYS 1001 ERR ***"
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
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU0307 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    レセプト編集区分情報
           INITIALIZE                   SYS-2005-REC
           MOVE    "2005"           TO  SYS-2005-KANRICD
           MOVE    "*"              TO  SYS-2005-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                                    TO  SYS-2005-STYUKYMD(1:6)
                                        SYS-2005-EDYUKYMD(1:6)
           MOVE  "01"               TO  SYS-2005-STYUKYMD(7:2)
                                        SYS-2005-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM      TO  SYS-2005-HOSPNUM
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
           MOVE    "2003"          TO  SYS-2003-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                   TO  SYS-2003-STYUKYMD(1:6)
                                       SYS-2003-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-2003-STYUKYMD(7:2)
                                       SYS-2003-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-2003-HOSPNUM
      *
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE    1               TO      IDX
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
                    OR     IDX    > 200
             IF  FLG-SYSKANRI3 = ZERO
                 MOVE SYS-2003-TEISYUTUSAKI  
                                   TO  WRK-2003-TEISYUTUSAKI(IDX)
                 MOVE SYS-2003-SYUBETU-G
                                   TO  WRK-2003-SYUBETU-G(IDX)
                 MOVE SYS-2003-NYUGAIKBN
                                   TO  WRK-2003-NYUGAIKBN(IDX)
                 MOVE SYS-2003-DSPSEQ
                                   TO  WRK-2003-DSPSEQ(IDX)
                 ADD  1            TO  IDX 
             END-IF  
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
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
           MOVE    "SEIKYU0307"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    レセプト出力順
           MOVE    "RECESYUTURYOKU"          TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(1:2)        TO  WRK-RECESYUTURYOKU
           DISPLAY "レセプト出力順="  WRK-RECESYUTURYOKU
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
                         OR   (IDX1  >  100)
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
           MOVE  SPA-HOSPNUM          TO   WRK-PARA-LINK-HOSPNUM
      *
           DISPLAY "編集１開始"
           CALL  "SEIKYU03BYOMEI1"    USING  WRK-PARA-LINK
           DISPLAY "編集２開始"
           CALL  "SEIKYU03BYOMEI2"    USING  WRK-PARA-LINK
           DISPLAY "病名ファイル作成終了"
      *
           .
       1001-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       1001-PARA-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                    TO  IDXX
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
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
             INITIALIZE                            WRK-CHK-KOHNUM-TBL
             MOVE    RECE02-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  800-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
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
           MOVE  SPA-HOSPNUM         TO  MF101-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF101-PTNUM
           MOVE  RECE02-RECEKA       TO  MF101-SRYKA
           MOVE  RECE02-TEKSTYMD     TO  MF101-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF101-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF101-SRYYM
           MOVE  RECE02-TEISYUTUSAKI TO  MF101-TEISYUTUSAKI
           MOVE  RECE02-RECESYUBETU  TO  MF101-RECESYUBETU
           MOVE  RECE02-HOJOKBN-KEY  TO  MF101-HOJOKBN-KEY
      *
           MOVE  RECE02-PTID         TO  MF101-PTID
           MOVE  RECE02-NAME         TO  MF101-NAME
           MOVE  RECE02-KANANAME     TO  MF101-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF101-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF101-SEX
           MOVE  RECE02-AGE          TO  MF101-AGE
           MOVE  RECE02-HKN          TO  MF101-HKN
           MOVE  RECE02-RJNHKN       TO  MF101-RJNHKN
           MOVE  RECE02-ZAITAKU      TO  MF101-ZAITAKU
      *
           MOVE  RECE02-KOHJKYSNUM  (IDZ)  TO  MF101-JKYSNUM
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF101-KOHINF
           COMPUTE  IDY  =  IDZ  +  1
           MOVE  RECE02-JNISSU      (IDY)  TO  MF101-JNISSU
           MOVE  RECE02-TOTALTEN    (IDY)  TO  MF101-TOTALTEN
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF101-SHOKUJIINF
      *
      *    個別発行用市町村番号
           MOVE  MF101-JKYSNUM(1:2)        TO  WRK-KOBETUCITYNUM
      *
      *
      *    長期の限度額を反映させる為、収納情報より一部負担金を集計
           MOVE    MF101-KOHNUM            TO  WRK-KOHNUM
           MOVE    MF101-KOHID             TO  WRK-KOHID
           MOVE    ZERO                    TO  WRK-FTNMONEY-G
           MOVE     1                      TO   FLG-ERR
           INITIALIZE  WRK-ETC-TBL
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"             TO  WRK-PATH
           ELSE
               MOVE    "key25"             TO  WRK-PATH
           END-IF
           PERFORM 920-SYUNOU-SET-SEC
      *    
           IF  IDZ         =   1
               AND     RECE02-KOHNUM(IDZ + 1)  =  SPACE
               PERFORM 200121-ETC-KOH-HEN-SEC
           ELSE
               PERFORM 200122-ETC-KOH-HEN-SEC
           END-IF
      *
      *    フラグ（未就学児、特例措置）
           MOVE    RECE02-FLG (1)     TO  MF101-FLG (1)
           MOVE    RECE02-FLG (2)     TO  MF101-FLG (2)
      *      出力順設定
      *      保険者番号順
           IF    WRK-RECESYUTURYOKU  =  "01" OR "02"
             EVALUATE  MF101-HKNNUM
      *      ６桁
             WHEN "060" 
             WHEN "068"
               MOVE  MF101-HKNJANUM(1:6)  TO  MF101-HKNJANUM-KEY(3:6)
      *      ８桁
             WHEN OTHER
               MOVE  MF101-HKNJANUM       TO  MF101-HKNJANUM-KEY
             END-EVALUATE
      *      システム管理マスタの出力順を設定 
             PERFORM  VARYING  IDX   FROM  1  BY  1
                      UNTIL    IDX   >     200
               IF    (WRK-2003-TEISYUTUSAKI(IDX) 
                                              = MF101-TEISYUTUSAKI)
                 AND (WRK-2003-NYUGAIKBN(IDX) = MF101-NYUGAIKBN)
                 IF  MF101-RECESYUBETU = WRK-2003-SYUBETU(IDX 1)
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
                                          TO  MF101-DSPSEQ
                   DISPLAY "出力番号=" MF101-DSPSEQ
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           EVALUATE  WRK-RECESYUTURYOKU
             WHEN  "01"
             WHEN  "03"
      *        カナ氏名順
               MOVE  MF101-KANANAME     TO  MF101-KANANAME-KEY
             WHEN  "04"
      *        患者番号順
               MOVE  MF101-PTNUM        TO  MF101-PTNUM-KEY
             WHEN  "05"
      *        受給者番号順（優先順位の関係で保険者番号KEYにセット）
               MOVE  MF101-KOHJKYSNUM
                                        TO  MF101-KOHJKYSNUM-KEY
           END-EVALUATE
      *
      *    償還払いの公費は助成チェックをしない
           IF  WRK-KOHNUM  =  "110" OR "130" OR "140" OR "150" OR "199"
               MOVE    ZERO                    TO  FLG-ERR
           END-IF
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
      *    診療行為情報取得
           MOVE    WRK-CONS-KIN-OKIKAE-HKN TO  WRK-SRYCD
           PERFORM 940-SRYACT-SET-SEC
           IF  FLG-SRYACT  =  ZERO
      *      保険欄−一部負担金置換編集
             PERFORM  200112-HKN-ICHIBUFTN-HENSYU-SEC
           END-IF
      *
      *    診療行為情報取得
           MOVE    WRK-CONS-KIN-OKIKAE     TO  WRK-SRYCD
           PERFORM 940-SRYACT-SET-SEC
           IF  FLG-SRYACT  =  ZERO
      *      公費−一部負担金置換編集
             PERFORM  200112-ICHIBUFTN-HENSYU-SEC
           END-IF
      *
           IF  FLG-ERR    =    ZERO
             WRITE   MF101-REC
             ADD     1   TO  CNT-MF101
           ELSE
               DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                       "公費番号="            MF101-KOHNUM
                       "診療年月="            MF101-SRYYM
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
           MOVE    "030"                    TO  WRK-HKNJANUM(1:3)
           MOVE    WRK-KOBETUCITYNUM        TO  WRK-HKNJANUM(4:2)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  MF101-PTNUM
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
      *
                   DISPLAY  "TBL-PARA-HKNJANUM = " 
                                               TBL-PARA-HKNJANUM(IDXX)
      *
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
      *    保険欄−一部負担金金額編集処理
      *****************************************************************
       200112-HKN-ICHIBUFTN-HENSYU-SEC             SECTION.
      *
           INITIALIZE                             WRK-INPUTCOMENT
                                                  WRK-INPUTCOMENT2
                                                  WRK-INPUTCOMENT-KIN
                                                  WRK-INPUTCOMENT-KIN2
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >  5
             IF  ((SRY-SRYCD(IDX4)(1:1)  =  "8")  OR
                  (SRY-SRYCD(IDX4)(1:3)  =  "008"))
             AND ( SRY-INPUTNUM(IDX4)  NOT =  ZERO )
      *
      *        患者コメント情報読込
               PERFORM  950-PTCOM-READ-SEC
               MOVE  PTCOM-INPUTCOMENT           TO  WRK-INPUTCOMENT
      *
               IF  WRK-INPUTCOMENT(1:12)  =  "（再掲上）："
               OR  WRK-INPUTCOMENT(1:12)  =  "（再掲下）："
               OR  WRK-INPUTCOMENT(1:12)  =  "（再掲３）："
               OR  WRK-INPUTCOMENT(1:12)  =  "（再掲４）："
               OR  WRK-INPUTCOMENT(1:12)  =  "保険負担金："
                 MOVE  WRK-INPUTCOMENT(13:14)    TO  WRK-INPUTCOMENT2
      *
      *          置換金額編集
                 IF  WRK-INPUTCOMENT2  =  "　　　　　　　"  OR  SPACE
                   CONTINUE
                 ELSE
      *            全角数字を半角に置き換える
                   PERFORM  VARYING  IDW1  FROM  1  BY  2
                            UNTIL    IDW1  >  14
                     COMPUTE  IDW2  =  (IDW1  +  1)  /  2
                     EVALUATE  WRK-INPUTCOMENT2(IDW1:2)
                       WHEN  "０"
                         MOVE  "0"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "１"
                         MOVE  "1"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "２"
                         MOVE  "2"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "３"
                         MOVE  "3"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "４"
                         MOVE  "4"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "５"
                         MOVE  "5"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "６"
                         MOVE  "6"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "７"
                         MOVE  "7"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "８"
                         MOVE  "8"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "９"
                         MOVE  "9"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  OTHER
                         MOVE  14   TO  IDW1
                     END-EVALUATE
                   END-PERFORM
      *
                   IF  WRK-INPUTCOMENT-KIN2-T  NOT =  SPACE
      *              右詰めの数字項目に変換する
                     MOVE    8      TO  IDW2
                     PERFORM  VARYING  IDW1  FROM  7  BY  -1
                              UNTIL    IDW1  <  1
                       IF  WRK-INPUTCOMENT-KIN2-T(IDW1:1)  =  SPACE
                         CONTINUE
                       ELSE
                         COMPUTE  IDW2  =  IDW2  -  1
                         MOVE  WRK-INPUTCOMENT-KIN2-T(IDW1:1)
                                    TO  WRK-INPUTCOMENT-KIN-T(IDW2:1)
                       END-IF
                     END-PERFORM
      *
      *              集計用ファイルに格納
                     IF  WRK-INPUTCOMENT(1:12)  =  "（再掲上）："
                       MOVE  ZERO        TO  MF101-OKIKAE-SAIKEI1
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-SAIKEI1
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "（再掲下）："
                       MOVE  ZERO        TO  MF101-OKIKAE-SAIKEI2
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-SAIKEI2
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "（再掲３）："
                       MOVE  ZERO        TO  MF101-OKIKAE-SAIKEI3
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-SAIKEI3
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "（再掲４）："
                       MOVE  ZERO        TO  MF101-OKIKAE-SAIKEI4
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-SAIKEI4
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "保険負担金："
                       MOVE  ZERO        TO  MF101-OKIKAE-HKN
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-HKN
                     END-IF
                   END-IF
      *
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       200112-HKN-ICHIBUFTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費欄−一部負担金金額編集処理
      *****************************************************************
       200112-ICHIBUFTN-HENSYU-SEC                SECTION.
      *
           INITIALIZE                             WRK-INPUTCOMENT
                                                  WRK-INPUTCOMENT2
                                                  WRK-INPUTCOMENT-KIN
                                                  WRK-INPUTCOMENT-KIN2
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >  5
             IF  ((SRY-SRYCD(IDX4)(1:1)  =  "8")  OR
                  (SRY-SRYCD(IDX4)(1:3)  =  "008"))
             AND ( SRY-INPUTNUM(IDX4)  NOT =  ZERO )
      *
      *        患者コメント情報読込
               PERFORM  950-PTCOM-READ-SEC
               MOVE  PTCOM-INPUTCOMENT           TO  WRK-INPUTCOMENT
      *
               IF  WRK-INPUTCOMENT(1:12)  =  "公１負担金："
               OR  WRK-INPUTCOMENT(1:12)  =  "公２負担金："
               OR  WRK-INPUTCOMENT(1:12)  =  "公３負担金："
               OR  WRK-INPUTCOMENT(1:12)  =  "公４負担金："
                 MOVE  WRK-INPUTCOMENT(13:14)    TO  WRK-INPUTCOMENT2
      *
      *          置換金額編集
                 IF  WRK-INPUTCOMENT2  =  "　　　　　　　"  OR  SPACE
                   CONTINUE
                 ELSE
      *            全角数字を半角に置き換える
                   PERFORM  VARYING  IDW1  FROM  1  BY  2
                            UNTIL    IDW1  >  14
                     COMPUTE  IDW2  =  (IDW1  +  1)  /  2
                     EVALUATE  WRK-INPUTCOMENT2(IDW1:2)
                       WHEN  "０"
                         MOVE  "0"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "１"
                         MOVE  "1"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "２"
                         MOVE  "2"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "３"
                         MOVE  "3"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "４"
                         MOVE  "4"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "５"
                         MOVE  "5"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "６"
                         MOVE  "6"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "７"
                         MOVE  "7"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "８"
                         MOVE  "8"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "９"
                         MOVE  "9"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  OTHER
                         MOVE  14   TO  IDW1
                     END-EVALUATE
                   END-PERFORM
      *
                   IF  WRK-INPUTCOMENT-KIN2-T  NOT =  SPACE
      *              右詰めの数字項目に変換する
                     MOVE    8      TO  IDW2
                     PERFORM  VARYING  IDW1  FROM  7  BY  -1
                              UNTIL    IDW1  <  1
                       IF  WRK-INPUTCOMENT-KIN2-T(IDW1:1)  =  SPACE
                         CONTINUE
                       ELSE
                         COMPUTE  IDW2  =  IDW2  -  1
                         MOVE  WRK-INPUTCOMENT-KIN2-T(IDW1:1)
                                    TO  WRK-INPUTCOMENT-KIN-T(IDW2:1)
                       END-IF
                     END-PERFORM
      *
      *              集計用ファイルに格納
                     IF  WRK-INPUTCOMENT(1:12)  =  "公１負担金："
                       MOVE  WRK-INPUTCOMENT-KIN  
                                    TO  MF101-OKIKAE-FTN(1)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公２負担金："
                       MOVE  WRK-INPUTCOMENT-KIN
                                    TO  MF101-OKIKAE-FTN(2)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公３負担金："
                       MOVE  WRK-INPUTCOMENT-KIN 
                                    TO  MF101-OKIKAE-FTN(3)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公４負担金："
                       MOVE  WRK-INPUTCOMENT-KIN 
                                    TO  MF101-OKIKAE-FTN(4)
                     END-IF
                   END-IF
      *
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       200112-ICHIBUFTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全国公費設定１
      *****************************************************************
       200121-ETC-KOH-HEN-SEC                SECTION.
      *
      *    月途中で公費を切替えていない場合、請求管理情報より取得
           PERFORM  900-SEIKYU-READ-SEC
           MOVE     RECE10-FTNMONEY(1)     TO  MF101-FTNMONEY
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     2  OR
                    RECE10-KOHNUM(IDX)  =  SPACE
             MOVE    RECE10-JNISSU(IDX + 1)     TO  MF101-ENISSU(IDX)
             MOVE    RECE10-TOTALTEN(IDX + 1)   TO  MF101-ETENSU(IDX)
             MOVE    RECE10-FTNMONEY(IDX + 1)   TO  MF101-EITBFTN(IDX)
             MOVE    RECE10-SHOKUJINISSU(IDX + 1)
                                    TO  MF101-ESHOKUJINISSU(IDX)
             MOVE    RECE10-SHOKUJIRYOYOHI(IDX + 1)
                                    TO  MF101-ESHOKUJIRYOYOHI(IDX)
             MOVE    RECE10-SHOKUJIFTN(IDX + 1) 
                                    TO  MF101-ESHOKUJIFTN(IDX)
             MOVE    RECE10-KOHNUM(IDX)         TO  WRK-KOHNUM
                                                    MF101-EKOHNUM(IDX)
             PERFORM 930-PTKOHINF-SET-SEC
             MOVE    WRK-ETC-FTNJANUM    TO  MF101-EKOHFTNJANUM(IDX)
             MOVE    WRK-ETC-JKYSNUM     TO  MF101-EKOHJKYSNUM(IDX)
      *    点数と日数は食事療養は地方公費と同じ値の時は記載を省略する
             IF      MF101-ENISSU(IDX)           =  MF101-JNISSU
               MOVE  ZERO                       TO  MF101-ENISSU(IDX)
             END-IF
      *
             IF      MF101-ETENSU(IDX)           =  MF101-TOTALTEN
               MOVE  ZERO                       TO  MF101-ETENSU(IDX)
             END-IF
      *
             IF      MF101-ESHOKUJINISSU(IDX)    =  MF101-SHOKUJINISSU
               MOVE  ZERO               TO  MF101-ESHOKUJINISSU(IDX)
             END-IF
      *
             IF      MF101-ESHOKUJIRYOYOHI(IDX)  
                                         =  MF101-SHOKUJIRYOYOHI
               MOVE  ZERO               TO  MF101-ESHOKUJIRYOYOHI(IDX)
             END-IF
      *
             IF      MF101-ESHOKUJIFTN(IDX)
                                         =  MF101-SHOKUJIFTN
               MOVE  ZERO               TO  MF101-ESHOKUJIFTN(IDX)
             END-IF
             
           END-PERFORM
      *
      *    特記事項
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     3
               IF   RECE10-TOKKI(IDX)  NOT =  SPACE
                 MOVE  RECE10-TOKKI(IDX)  TO  MF101-TOKKI(IDX)
               END-IF
           END-PERFORM
      *
           .
       200121-ETC-KOH-HEN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    全国公費設定２
      *****************************************************************
       200122-ETC-KOH-HEN-SEC                SECTION.
      *
      *    月途中で公費を切替えている場合、収納情報より取得(実日数、食事日数除く)
           DISPLAY  "患者番号=" RECE02-PTNUM
                    "その他公費1=" WRK-ETC-KOHNUM(1)
                    "その他公費2=" WRK-ETC-KOHNUM(2)
           IF  WRK-ETC-KOHNUM(1)     NOT  =  SPACE
             DISPLAY "その他公費番号=" WRK-ETC-KOHNUM(1)
                     ","               WRK-ETC-KOHNUM(2)
             IF      WRK-ETC-KOHNUM(2)    =  SPACE  OR
                     WRK-ETC-KOHNUM(2)    >  WRK-ETC-KOHNUM(1)
               MOVE  1                   TO  IDX
               MOVE  2                   TO  IDY
             ELSE
               MOVE  2                   TO  IDX
               MOVE  1                   TO  IDY
             END-IF
             MOVE    WRK-ETC-KOHNUM(1)   TO  WRK-KOHNUM
                                             MF101-EKOHNUM(IDX)
             PERFORM 930-PTKOHINF-SET-SEC
             MOVE    WRK-ETC-TENSU(1)    TO  MF101-ETENSU(IDX)
             MOVE    WRK-ETC-ITBFTN(1)   TO  MF101-EITBFTN(IDX)
             MOVE    WRK-ETC-SHOKUJIRYOYOHI(1)
                                         TO  MF101-ESHOKUJIRYOYOHI(IDX)
             MOVE    WRK-ETC-SHOKUJIFTN(1)
                                         TO  MF101-ESHOKUJIFTN(IDX)
             MOVE    WRK-ETC-FTNJANUM    TO  MF101-EKOHFTNJANUM(IDX)
             MOVE    WRK-ETC-JKYSNUM     TO  MF101-EKOHJKYSNUM(IDX)
             IF      WRK-ETC-KOHNUM(2)  NOT  =  SPACE
               MOVE    WRK-ETC-KOHNUM(2) TO  WRK-KOHNUM
                                             MF101-EKOHNUM(IDY)
               PERFORM 930-PTKOHINF-SET-SEC
               MOVE    WRK-ETC-TENSU(2)  TO  MF101-ETENSU(IDY)
               MOVE    WRK-ETC-ITBFTN(2) TO  MF101-EITBFTN(IDY)
               MOVE    WRK-ETC-SHOKUJIRYOYOHI(2)
                                         TO  MF101-ESHOKUJIRYOYOHI(IDX)
               MOVE    WRK-ETC-SHOKUJIFTN(2)
                                         TO  MF101-ESHOKUJIFTN(IDX)
               MOVE    WRK-ETC-FTNJANUM  TO  MF101-EKOHFTNJANUM(IDY)
               MOVE    WRK-ETC-JKYSNUM   TO  MF101-EKOHJKYSNUM(IDY)
             END-IF
      *    点数は地方公費と同じ値の時は記載を省略する
             IF      MF101-ETENSU(1)      =  MF101-TOTALTEN
               MOVE  ZERO                TO  MF101-ETENSU(1)
             END-IF
             IF      MF101-ETENSU(2)      =  MF101-TOTALTEN
               MOVE  ZERO                TO  MF101-ETENSU(2)
             END-IF
           END-IF
      *
           .
       200122-ETC-KOH-HEN-EXT.
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
      *
               IF      MF101-NYUGAIKBN     =  "1"
      *    帳票編集<見出し>処理
                   PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
                   PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
                   PERFORM  330-PRINT-OUT-SEC
               ELSE
      *    帳票編集<見出し>処理
                   PERFORM  311-HEAD-HEN-SEC
      *    帳票編集<明細>処理
                   PERFORM  321-BODY-HEN-SEC
      *    帳票印刷処理
                   PERFORM  331-PRINT-OUT-SEC
               END-IF
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
           INITIALIZE  SC07
      *
      *    診療年月
           MOVE    MF101-SRYYM             TO  WRK-SYMD (1:6)
           MOVE    "01"                    TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)       TO  SC07-SRYYM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN        TO  SC07-HOSPCDN
      
      *
      *    保険区分
           IF      MF101-RJNHKNNUM     NOT =   SPACE
           OR      MF101-HKNNUM  =  "039"  OR  "040"
               MOVE    "○"                TO  SC07-HKNKBN(3)
           ELSE
               IF      MF101-HKNNUM        =   "067"
                   MOVE    "○"            TO  SC07-HKNKBN(4)
               ELSE
                   MOVE    "○"            TO  SC07-HKNKBN(1)
               END-IF
           END-IF
      *
      *    併用区分
           IF      MF101-RECESYUBETU(3:1)  =   "1"
               MOVE    "○"                TO  SC07-HEIYOKBN(1)
           ELSE
               MOVE    "○"                TO  SC07-HEIYOKBN(2)
           END-IF
      *
      *    本人・家族区分
           EVALUATE   MF101-RECESYUBETU(4:1)
               WHEN "1"
                   MOVE    "○"            TO  SC07-HONKZKKBN(1)
               WHEN "3"
                   MOVE    "○"            TO  SC07-HONKZKKBN(2)
               WHEN "5"
                   MOVE    "○"            TO  SC07-HONKZKKBN(3)
               WHEN "7"
                   MOVE    "○"            TO  SC07-HONKZKKBN(4)
               WHEN "9"
                   MOVE    "○"            TO  SC07-HONKZKKBN(5)
           END-EVALUATE
      *
      *    現物給付公費市町村番号
           IF  MF101-KOHNUM   NOT =  "110" AND "130" AND "140" AND "150"
                                 AND "199"
               MOVE   MF101-KOHJKYSNUM     TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:16)      TO  SC07-FTNJANUM(1)
               MOVE    WRK-OUT-INPUT(17:6)      TO  SC07-JKYSNUM(1)
           ELSE
      *    老人市町村番号
               MOVE    MF101-RJNFTNJANUM       TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT           TO  SC07-FTNJANUM(1)
      *    老人受給者番号
               MOVE    MF101-RJNJKYSNUM        TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT           TO  SC07-JKYSNUM(1)
           END-IF
      *
      *    保険者番号
           MOVE    MF101-HKNJANUM          TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           EVALUATE    MF101-HKNNUM
               WHEN "001"
                   MOVE    WRK-OUT-INPUT(1:8)
                                           TO  SC07-HKNJANUM(9:8)
               WHEN "060"
                   MOVE    WRK-OUT-INPUT(1:12)
                                           TO  SC07-HKNJANUM(5:12)
               WHEN OTHER
                   MOVE    WRK-OUT-INPUT   TO  SC07-HKNJANUM
           END-EVALUATE
      * 
      *    記号・番号
           MOVE    MF101-KIGO              TO  SC07-KIGO
           MOVE    MF101-NUM               TO  SC07-NUM
      *
      *    医療機関住所
           MOVE  WRK-HOSPADRS              TO  SC07-ADRS
           INSPECT SC07-ADRS               REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE  WRK-HOSPNAME              TO  SC07-HOSPNAME
           INSPECT SC07-HOSPNAME           REPLACING ALL "  " BY "　"
      *
      *    給付割合
           IF   MF101-SRYYM   >=  WRK-CONS-SRYYM-200804
           AND  MF101-FLG (2)  =  1
               MOVE    "○"                TO  SC07-KYURATE(3)
           ELSE
             EVALUATE  MF101-KYURATE
               WHEN 10
                   MOVE    "○"            TO  SC07-KYURATE(2)
               WHEN 20
                   MOVE    "○"            TO  SC07-KYURATE(3)
               WHEN 30
                   MOVE    "○"            TO  SC07-KYURATE(4)
               WHEN OTHER
                   MOVE    "○"            TO  SC07-KYURATE(1)
             END-EVALUATE
           END-IF
      *
      *    患者氏名
           MOVE    MF101-KANANAME          TO  SC07-KANANAME
           MOVE    MF101-NAME              TO  SC07-NAME
      *
      *    患者番号
           MOVE    MF101-PTNUM             TO  SC07-PTNUM
      *
      *    誕生日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                          LNK-DAY2-AREA
           MOVE    "21"                    TO  LNK-DAY2-IRAI
           MOVE    MF101-BIRTHDAY          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING       STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE  LNK-DAY2-EDTYMD3          TO  SC07-BIRTHDAY2
      *
      *    性別
           IF      MF101-SEX               =   "1"
               MOVE    "○"                TO  SC07-SEX(1)
           ELSE
               MOVE    "○"                TO  SC07-SEX(2)
           END-IF
      *
      *    特記事項
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL  IDY  >  TBL-TOKKI-MAX
               IF  MF101-TOKKI(IDX)  =  TBL-TOKKICD(IDY)
                   MOVE  TBL-TOKKINAME(IDY)   TO  SC07-TOKKI(IDX)
                   MOVE  TBL-TOKKI-MAX  TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
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
      *    日数
           MOVE    MF101-JNISSU             TO  WRK-Z2
           MOVE    WRK-Z2                   TO  SC07-NISSU(1)
      *
      *    点数
           MOVE    MF101-TOTALTEN           TO  WRK-Z7
           MOVE    WRK-Z7                   TO  SC07-TENSU(1)
      *
      *    負担金額
               IF  MF101-OKIKAE-HKN       NOT =  ZERO
                 MOVE    MF101-OKIKAE-HKN         TO  WRK-Z7
                 MOVE    WRK-Z7                   TO  SC07-ITBFTN(1)
               ELSE
                 MOVE    MF101-FTNMONEY           TO  WRK-Z7
                 MOVE    WRK-Z7                   TO  SC07-ITBFTN(1)
               END-IF
      *
      *    食事日数
           MOVE    MF101-SHOKUJINISSU       TO  WRK-Z2
           MOVE    WRK-Z2                   TO  SC07-SNISSU(1)
      *
      *    食事療養費
           MOVE    MF101-SHOKUJIRYOYOHI     TO  WRK-Z7
           MOVE    WRK-Z7                   TO  SC07-RYOYOHI(1)
      *
      *    標準負担額
           MOVE    MF101-SHOKUJIFTN         TO  WRK-Z7
           MOVE    WRK-Z7                   TO  SC07-FTNGK(1)
      *
      *    全国公費データ設定
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2   OR
                   MF101-EKOHNUM(IDX)  =   SPACE
      *
      *    負担者番号
               MOVE    MF101-EKOHFTNJANUM(IDX) TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC07-FTNJANUM(IDX + 1)
      *
      *    受給者番号
               MOVE    MF101-EKOHJKYSNUM(IDX)  TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC07-JKYSNUM(IDX + 1)
      *
      *    点数
               MOVE    MF101-ETENSU(IDX)       TO  WRK-Z7
               MOVE    WRK-Z7                  TO  SC07-TENSU(IDX + 1)
      *
      *    日数
               MOVE    MF101-ENISSU(IDX)       TO  WRK-Z2
               MOVE    WRK-Z2                  TO  SC07-NISSU(IDX + 1)
      *
      *    一部負担金
               IF  MF101-OKIKAE-FTN(IDX)  NOT =  ZERO
                 MOVE    MF101-OKIKAE-FTN(IDX) TO  WRK-Z7
                 MOVE    WRK-Z7                TO  SC07-ITBFTN(IDX + 1)
               ELSE
                 MOVE    MF101-EITBFTN(IDX)    TO  WRK-Z7
                 MOVE    WRK-Z7                TO  SC07-ITBFTN(IDX + 1)
               END-IF
      *
      *    食事日数
               MOVE    MF101-ESHOKUJINISSU(IDX)    TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC07-SNISSU(IDX + 1)
      *
      *    食事療養費
               MOVE    MF101-ESHOKUJIRYOYOHI(IDX)  TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC07-RYOYOHI(IDX + 1)
      *
      *    食事標準負担額
               MOVE    MF101-ESHOKUJIFTN(IDX)      TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC07-FTNGK(IDX + 1)
           
           END-PERFORM 
      *
      *    傷病名編集処理
           INITIALIZE  WRK-TAIHI-TBL
           PERFORM 3201-BYOMEI-HEN-SEC
           MOVE    WRK-TAIHI-TBL            TO  SC07-BYOMEI-TBL
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
           MOVE    "SC0307.red"        TO  SPRT-PRTID
           MOVE    "複写式レセプト"
                                    TO  SPRT-TITLE
           MOVE    SC07             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           MOVE    MF101-PTID          TO  SPRT-PTID
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
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
      *    帳票編集<ヘッダー部>処理(外来用)
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SC08
      *
      *    診療年月
           MOVE    MF101-SRYYM             TO  WRK-SYMD (1:6)
           MOVE    "01"                    TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)       TO  SC08-SRYYM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN        TO  SC08-HOSPCDN
      *
      *    保険区分
           IF      MF101-RJNHKNNUM     NOT =   SPACE
           OR      MF101-HKNNUM  =  "039"  OR  "040"
               MOVE    "○"                TO  SC08-HKNKBN(3)
           ELSE
               IF      MF101-HKNNUM        =   "067"
                   MOVE    "○"            TO  SC08-HKNKBN(4)
               ELSE
                   MOVE    "○"            TO  SC08-HKNKBN(1)
               END-IF
           END-IF
      *
      *    併用区分
           IF      MF101-RECESYUBETU(3:1)  =   "1"
               MOVE    "○"                TO  SC08-HEIYOKBN(1)
           ELSE
               MOVE    "○"                TO  SC08-HEIYOKBN(2)
           END-IF
      *
      *    本人・家族区分
           EVALUATE   MF101-RECESYUBETU(4:1)
               WHEN "2"
                   MOVE    "○"            TO  SC08-HONKZKKBN(1)
               WHEN "4"
                   MOVE    "○"            TO  SC08-HONKZKKBN(2)
               WHEN "6"
                   MOVE    "○"            TO  SC08-HONKZKKBN(3)
               WHEN "8"
                   MOVE    "○"            TO  SC08-HONKZKKBN(4)
               WHEN "0"
                   MOVE    "○"            TO  SC08-HONKZKKBN(5)
           END-EVALUATE
      *
      *    現物給付公費市町村番号
           IF  MF101-KOHNUM   NOT =  "110" AND "130" AND "140" AND "150"
                                 AND "199"
               MOVE   MF101-KOHJKYSNUM     TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:16)      TO  SC08-FTNJANUM(1)
               MOVE    WRK-OUT-INPUT(17:6)      TO  SC08-JKYSNUM(1)
           ELSE
      *    老人市町村番号
               MOVE    MF101-RJNFTNJANUM       TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT           TO  SC08-FTNJANUM(1)
      *    老人受給者番号
               MOVE    MF101-RJNJKYSNUM        TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT           TO  SC08-JKYSNUM(1)
           END-IF
      * 
      *    保険者番号
           MOVE    MF101-HKNJANUM          TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           EVALUATE    MF101-HKNNUM
               WHEN "001"
                   MOVE    WRK-OUT-INPUT(1:8)
                                           TO  SC08-HKNJANUM(9:8)
               WHEN "060"
                   MOVE    WRK-OUT-INPUT(1:12)
                                           TO  SC08-HKNJANUM(5:12)
               WHEN OTHER
                   MOVE    WRK-OUT-INPUT   TO  SC08-HKNJANUM
           END-EVALUATE
      * 
      *    記号・番号
           MOVE    MF101-KIGO              TO  SC08-KIGO
           MOVE    MF101-NUM               TO  SC08-NUM
      *
      *    医療機関住所
           MOVE  WRK-HOSPADRS              TO  SC08-ADRS
           INSPECT SC08-ADRS               REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE  WRK-HOSPNAME              TO  SC08-HOSPNAME
           INSPECT SC08-HOSPNAME           REPLACING ALL "  " BY "　"
      *
      *    給付割合
           IF   MF101-SRYYM   >=  WRK-CONS-SRYYM-200804
           AND  MF101-FLG (2)  =  1
               MOVE    "○"                TO  SC08-KYURATE(3)
           ELSE
             EVALUATE  MF101-KYURATE
               WHEN 10
                   MOVE    "○"            TO  SC08-KYURATE(2)
               WHEN 20
                   MOVE    "○"            TO  SC08-KYURATE(3)
               WHEN 30
                   MOVE    "○"            TO  SC08-KYURATE(4)
               WHEN OTHER
                   MOVE    "○"            TO  SC08-KYURATE(1)
             END-EVALUATE
           END-IF
      *
      *    患者氏名
           MOVE    MF101-KANANAME          TO  SC08-KANANAME
           MOVE    MF101-NAME              TO  SC08-NAME
      *
      *    患者番号
           MOVE    MF101-PTNUM             TO  SC08-PTNUM
      *
      *    誕生日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                          LNK-DAY2-AREA
           MOVE    "21"                    TO  LNK-DAY2-IRAI
           MOVE    MF101-BIRTHDAY          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING       STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE  LNK-DAY2-EDTYMD3          TO  SC08-BIRTHDAY2
      *
      *    性別
           IF      MF101-SEX               =   "1"
               MOVE    "○"                TO  SC08-SEX(1)
           ELSE
               MOVE    "○"                TO  SC08-SEX(2)
           END-IF
      *
      *    特記事項
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL  IDY  >  TBL-TOKKI-MAX
               IF  MF101-TOKKI(IDX)  =  TBL-TOKKICD(IDY)
                   MOVE  TBL-TOKKINAME(IDY)   TO  SC08-TOKKI(IDX)
                   MOVE  TBL-TOKKI-MAX  TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理(外来用)
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    日数
           MOVE    MF101-JNISSU             TO  WRK-Z2
           MOVE    WRK-Z2                   TO  SC08-NISSU(1)
      *
      *    点数
           MOVE    MF101-TOTALTEN           TO  WRK-Z7
           MOVE    WRK-Z7                   TO  SC08-TENSU(1)
      *
      *    負担金額
               IF  MF101-OKIKAE-HKN       NOT =  ZERO
                 MOVE    MF101-OKIKAE-HKN         TO  WRK-Z7
                 MOVE    WRK-Z7                   TO  SC08-ITBFTN(1)
               ELSE
                 MOVE    MF101-FTNMONEY           TO  WRK-Z7
                 MOVE    WRK-Z7                   TO  SC08-ITBFTN(1)
               END-IF
      *
      *    全国公費データ設定
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2   OR
                   MF101-EKOHNUM(IDX)  =   SPACE
      *
      *    負担者番号
               MOVE    MF101-EKOHFTNJANUM(IDX) TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC08-FTNJANUM(IDX + 1)
      *
      *    受給者番号
               MOVE    MF101-EKOHJKYSNUM(IDX)  TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC08-JKYSNUM(IDX + 1)
      *
      *    点数
               MOVE    MF101-ETENSU(IDX)       TO  WRK-Z7
               MOVE    WRK-Z7                  TO  SC08-TENSU(IDX + 1)
      *
      *    日数
               MOVE    MF101-ENISSU(IDX)       TO  WRK-Z2
               MOVE    WRK-Z2                  TO  SC08-NISSU(IDX + 1)
      *
      *    一部負担金
               IF  MF101-OKIKAE-FTN(IDX)  NOT =  ZERO
                 MOVE    MF101-OKIKAE-FTN(IDX) TO  WRK-Z7
                 MOVE    WRK-Z7                TO  SC08-ITBFTN(IDX + 1)
               ELSE
                 MOVE    MF101-EITBFTN(IDX)    TO  WRK-Z7
                 MOVE    WRK-Z7                TO  SC08-ITBFTN(IDX + 1)
               END-IF
      *
           END-PERFORM 
      *
      *    傷病名編集処理
           INITIALIZE  WRK-TAIHI-TBL
           PERFORM 3201-BYOMEI-HEN-SEC
           MOVE    WRK-TAIHI-TBL            TO  SC08-BYOMEI-TBL
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理(外来用)
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
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
           MOVE    "SC0308.red"        TO  SPRT-PRTID
           MOVE    "複写式レセプト"
                                    TO  SPRT-TITLE
           MOVE    SC08             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           MOVE    MF101-PTID          TO  SPRT-PTID
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       331-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    傷病名編集処理
      *****************************************************************
       3201-BYOMEI-HEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  WRK-BYO-OLD
      *     
           INITIALIZE                  WRK-EDIT-AREA
           MOVE    SPACE               TO  WRK-BYOMEI1
      *
           INITIALIZE                     RECE031-REC
           MOVE    MF101-HOSPNUM      TO  RECE031-HOSPNUM
           MOVE    MF101-SRYYM        TO  RECE031-SRYYM
           MOVE    MF101-NYUGAIKBN    TO  RECE031-NYUGAIKBN
           MOVE    MF101-PTID         TO  RECE031-PTID
           MOVE    MF101-SRYKA        TO  RECE031-RECEKA
           MOVE    MF101-TEISYUTUSAKI TO  RECE031-TEISYUTUSAKI
           MOVE    MF101-RECESYUBETU  TO  RECE031-RECESYUBETU
           MOVE    MF101-HKNJANUM     TO  RECE031-HKNJANUM
           MOVE    MF101-HOJOKBN-KEY  TO  RECE031-HOJOKBN
           MOVE    MF101-TEKSTYMD     TO  RECE031-TEKSTYMD
      *
         DISPLAY  "KEY1  :  "  RECE031-KEY
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
           DISPLAY "保険者番号1=" MF101-HKNJANUM
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
               OR     ( MF101-HOSPNUM    NOT =   RECE031-HOSPNUM      )
               OR     ( MF101-SRYYM      NOT =   RECE031-SRYYM        )
               OR     ( MF101-NYUGAIKBN  NOT =   RECE031-NYUGAIKBN    )
               OR     ( MF101-PTID       NOT =   RECE031-PTID         )
               OR     ( MF101-SRYKA      NOT =   RECE031-RECEKA       )
               OR     ( MF101-TEISYUTUSAKI
                                         NOT =   RECE031-TEISYUTUSAKI )
               OR     ( MF101-RECESYUBETU
                                         NOT =   RECE031-RECESYUBETU  )
               OR     ( MF101-HKNJANUM   NOT =   RECE031-HKNJANUM     )
               OR     ( MF101-HOJOKBN-KEY
                                         NOT =   RECE031-HOJOKBN      )
               OR     ( MF101-TEKSTYMD   NOT =   RECE031-TEKSTYMD     )
      *    
                   DISPLAY "病名編集開始"
      *        病名編集処理
      *************************************************
             DISPLAY  "             "  RECE031PARA
             DISPLAY  "HOSPNUM1  :  "  MF101-HOSPNUM
             DISPLAY  "HOSPNUM2  :  "  RECE031-HOSPNUM
             DISPLAY  "SRYYM     :  "  RECE031-SRYYM
             DISPLAY  "KEY2  :  "  RECE031-KEY
               PERFORM 3210-BYOMEI-SET-SEC
      *===============================================
             DISPLAY  "HOSPNUM1  :  "  MF101-HOSPNUM
             DISPLAY  "HOSPNUM2  :  "  RECE031-HOSPNUM
             DISPLAY  "FLG       :  "  FLG-RECE031
             DISPLAY  "KEY3  :  "  RECE031-KEY
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
           MOVE  WRK-LINE       TO    IDX2
           ADD   1              TO    IDX2
           PERFORM  VARYING  IDX1  FROM  IDX2  BY  1
                    UNTIL    IDX1  >    3
             ADD   1                 TO  WRK-LINEBYO
             MOVE  IDX1              TO  WRK-Z1
             INITIALIZE              ORCSKANACHKAREA
             MOVE    "2"             TO  KANACHK-SYORI 
             MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
             MOVE    WRK-Z1          TO  KANACHK-MAE-INPUT (2:1)
             MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
             CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                     TO  WRK-TAIHI-BYOMEI(WRK-LINEBYO)
                                         WRK-TAIHI-SRYYMD(WRK-LINEBYO)
             END-IF
             MOVE  "　　　年　　月　　日"
                                 TO  WRK-TAIHI-SRYYMD(WRK-LINEBYO)(7:)
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
               MOVE    LNK-DAY2-EDTYMD3-G (1:2)
                                           TO  WRK-TAIHI-SRYYMD
                                                    (WRK-LINEBYO)(7:2)
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-TAIHI-SRYYMD
                                                    (WRK-LINEBYO)(9:)
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD TO  WRK-SYMD
                   MOVE    WRK-SDD         TO  WRK-Z2
                   EVALUATE    WRK-BO-TENKIKBN
                   WHEN    "1"
                       MOVE    WRK-Z2  TO  WRK-TAIHI-TENKI1(WRK-LINEBYO)
                   WHEN    "2"
                       MOVE    WRK-Z2  TO  WRK-TAIHI-TENKI2(WRK-LINEBYO)
                   WHEN    "3"
                       MOVE    WRK-Z2  TO  WRK-TAIHI-TENKI3(WRK-LINEBYO)
                   END-EVALUATE
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
           IF      MF101-NYUGAIKBN    =    "1"
      *    帳票印刷処理
               MOVE    WRK-TAIHI-TBL            TO  SC07-BYOMEI-TBL
               PERFORM 330-PRINT-OUT-SEC
      *    帳票編集<ヘッダー部>処理
               PERFORM 310-HEAD-HEN-SEC
               INITIALIZE  WRK-TAIHI-TBL
               MOVE    ZERO          TO  WRK-LINETEK
           ELSE
      *    帳票印刷処理
               MOVE    WRK-TAIHI-TBL            TO  SC08-BYOMEI-TBL
               PERFORM 331-PRINT-OUT-SEC
      *    帳票編集<ヘッダー部>処理
               PERFORM 311-HEAD-HEN-SEC
               INITIALIZE  WRK-TAIHI-TBL
               MOVE    ZERO          TO  WRK-LINETEK
           END-IF
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
           DISPLAY "*** SEIKYU0307 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU0307 OUT "  CNT-MF101
           DISPLAY "*** SEIKYU0307 END ***"
      *
           IF  CNT-MF101  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
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
      *    診療情報セット
      *****************************************************************
       940-SRYACT-SET-SEC  SECTION.
      *
             INITIALIZE                      SRYACT-REC
      *
             MOVE  SPA-HOSPNUM           TO  SRY-HOSPNUM
             MOVE  RECE02-NYUGAIKBN      TO  SRY-NYUGAIKBN
             MOVE  RECE02-PTID           TO  SRY-PTID
             MOVE  RECE02-SRYYM          TO  SRY-SRYYM
             MOVE  WRK-SRYCD             TO  SRY-SRYCD(1)
                                             SRY-SRYCD(2)
                                             SRY-SRYCD(3)
                                             SRY-SRYCD(4)
                                             SRY-SRYCD(5)
             MOVE  SRYACT-REC            TO  MCPDATA-REC
             MOVE  "DBSELECT"            TO  MCP-FUNC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key7"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_sryact"        TO  MCP-TABLE
               MOVE  "key7"              TO  MCP-PATHNAME
               PERFORM  945-SRYACT-READ-SEC
             ELSE
               DISPLAY  "SRYACT SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key7"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       940-SRYACT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報読込
      *****************************************************************
       945-SRYACT-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SRYACT-REC
             MOVE  ZERO         TO  FLG-SRYACT
           ELSE
             INITIALIZE   SRYACT-REC
             MOVE  1  TO  FLG-SRYACT
           END-IF
      *
           .
       945-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント情報セット
      *****************************************************************
       950-PTCOM-READ-SEC  SECTION.
      *
             INITIALIZE                      PTCOM-REC
      *
             MOVE  SPA-HOSPNUM           TO  PTCOM-HOSPNUM
             MOVE  SRY-PTID              TO  PTCOM-PTID
             MOVE  SRY-ZAINUM            TO  PTCOM-ZAINUM
             MOVE  SRY-SRYCD(IDX4)       TO  PTCOM-SRYCD
             MOVE  SRY-INPUTNUM(IDX4)    TO  PTCOM-RENNUM
             MOVE  PTCOM-REC             TO  MCPDATA-REC
             MOVE  "DBSELECT"            TO  MCP-FUNC
             MOVE  "tbl_ptcom"           TO  MCP-TABLE
             MOVE  "key"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_ptcom"         TO  MCP-TABLE
               MOVE  "key"               TO  MCP-PATHNAME
               MOVE  "DBFETCH"           TO  MCP-FUNC
               CALL  "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF  MCP-RC  =  ZERO
                 MOVE  MCPDATA-REC       TO  PTCOM-REC
                 MOVE  ZERO              TO  FLG-PTCOM
               ELSE
                 INITIALIZE                  PTCOM-REC
                 MOVE    1               TO  FLG-PTCOM
               END-IF
             ELSE
               DISPLAY  "PTCOM SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
             MOVE  "tbl_ptcom"           TO  MCP-TABLE
             MOVE  "key"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       950-PTCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込
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
               PERFORM 900-KOHSKY-CLOSE-SEC
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
           MOVE    "DBFETCH"                TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
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
      *        テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
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
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
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
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                 TO  MCP-FUNC
               MOVE    "tbl_ptinf"       TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
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
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
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
           MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
           MOVE    SYU-PTID                TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM         TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                 TO  MCP-FUNC
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
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO     TO     WRK-FTNMONEY
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
      *
               IF   SYU-KOH-HKNNUM(IDX)  NOT =  "027"  AND
                    SYU-KOH-HKNNUM(IDX)(1:1) =  "0"
                 IF  WRK-ETC-KOHNUM(1)       =  SPACE
                 OR                             SYU-KOH-HKNNUM(IDX)
                   MOVE  1                    TO  IDY
                 ELSE
                   MOVE  2                    TO  IDY
                 END-IF
                 DISPLAY "IDY=" IDY
                 MOVE  SYU-KOH-HKNNUM(IDX)  TO  WRK-ETC-KOHNUM(IDY)
                 ADD   SYU-TOTAL-HKNTEN     TO  WRK-ETC-TENSU(IDY)
                 ADD   SYU-SKJ-RYOYOHI(IDX) 
                                   TO  WRK-ETC-SHOKUJIRYOYOHI(IDY)
                 ADD   SYU-SKJ-FTN-RECE(IDX) 
                                   TO  WRK-ETC-SHOKUJIFTN(IDY)
                 IF   (RECE02-FTNMONEY(1)    >  ZERO)
                 OR   (SYU-KOH-HKNNUM(IDX)  NOT =  "010" AND "011"
                                            AND    "021" AND "020")
                   ADD     SYU-KOH-COMPFTN(IDX)
                                            TO  WRK-ETC-ITBFTN(IDY)
                 END-IF
               END-IF
      *
             IF    SYU-KOH-HKNNUM(IDX)   =  "972"
                 MOVE    1                TO    MF101-TYOKI
             END-IF
             IF    SYU-KOH-HKNNUM(IDX)   =  "974"
                 MOVE    2                TO    MF101-TYOKI
             END-IF
      *
      *    計算負担金に差がある場合、地方公費の助成があったと判断する
             IF  (FLG-ERR                  =  1)
             AND (SYU-KOH-HKNNUM(IDX)      =  WRK-KOHNUM)
               IF     IDX                 =  1
                 IF  SYU-SYUCOMPFTN-ENTANI
                                      >  SYU-KOH-COMPFTN-ENTANI(IDX)
                   MOVE     ZERO          TO  FLG-ERR
                 END-IF
               ELSE
                 IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                      >  SYU-KOH-COMPFTN-ENTANI(IDX)
                   MOVE     ZERO          TO  FLG-ERR
                 END-IF
               END-IF
             END-IF
      *
           END-PERFORM
      *
           ADD    WRK-FTNMONEY            TO    WRK-FTNMONEY-G
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
             MOVE  RECE02-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE02-PTID        TO  SYU-PTID
             MOVE  RECE02-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"     TO  MCP-TABLE
               MOVE  WRK-PATH         TO  MCP-PATHNAME
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
                 MOVE  "tbl_syunou"      TO  MCP-TABLE
                 MOVE  WRK-PATH          TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
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
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY              TO  RECE10-KEY
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE  SPA-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE  RECE02-PTID        TO  PTKOH-PTID
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
             PERFORM  935-PTKOHINF-READ-SEC
             PERFORM  UNTIL  FLG-PTKOHINF = 1
               IF  PTKOH-SAKJOKBN  NOT = "1"
                 IF  PTKOH-TEKSTYMD(1:6) <= RECE02-SRYYM(1:6)  AND 
                     PTKOH-TEKEDYMD(1:6) >= RECE02-SRYYM(1:6)
                     MOVE  PTKOH-FTNJANUM  TO  WRK-ETC-FTNJANUM
                     MOVE  PTKOH-JKYSNUM   TO  WRK-ETC-JKYSNUM
                 END-IF
               END-IF
               MOVE  "tbl_ptkohinf"   TO  MCP-TABLE
               MOVE  "key4"           TO  MCP-PATHNAME
                 PERFORM  935-PTKOHINF-READ-SEC
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
       930-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       935-PTKOHINF-READ-SEC  SECTION.
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
       935-PTKOHINF-READ-EXT.
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
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"  TO  MCP-FUNC
             MOVE  "tbl_syskanri"        TO  MCP-TABLE
             MOVE  "key10"               TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  ZERO  TO  FLG-SYSKANRI
             ELSE
               MOVE  1  TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(レセプト出力順情報取得)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-2003-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
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
           MOVE    "DBFETCH"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO               TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-2003-REC
           ELSE
               MOVE    1                  TO  FLG-SYSKANRI3
               PERFORM 820-SYSKANRI-DBCLOSE-SEC
           END-IF  
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理(レセプト出力順情報取得)
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
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
      *
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
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD (1:6)
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
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"  TO  MCP-FUNC.
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
