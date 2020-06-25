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
       PROGRAM-ID.     SEIKYU0609.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 山形・複写式レセプト
      *                      (182,282,183,283,184)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/31    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            (個別発行対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    山口    05/12/19    様式変更による修正
      *  01.00.01    山口    06/02/01    保険番号マスタ変更による修正
      *  01.00.02    内田    07/06/15    返戻分も診療年月を印字
      *  01.00.03    内田    07/07/04    プレビュー時、患者番号の印字
      *  01.00.04    内田    07/09/17    入院データも印字する
      *  01.00.05    内田    07/10/05    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.06    内田    08/03/07    入院の保険種別の印字の修正
      *  01.00.07    内田    08/06/04    帳票様式の変更に伴う修正
      *  01.00.08    吉川    08/09/09    SPA-AREA初期化
      *  01.00.09    吉川    10/03/08    保険点数欄は公費合計点数を印字する
      *  01.00.10    吉川    11/03/07    open-cobol1.0対応
      *  01.00.11    吉川    12/02/17    シス管「2005」の主病名編集対応
      *  01.00.12    萩野    12/09/19    特記事項に印字内容追加
      *  01.00.13    吉川    12/12/21    医療機関情報編集対応
      *  01.00.14    吉川    13/04/03    公費欄−一部負担金金額置換対応
      *                                  摘要欄にコメントを印字
      *                                  プラグイン対応
      *  01.00.15    吉川    13/11/14    プログラムオプション対応
      *  01.00.16    萩野    14/09/09    入院での医療機関の開設者名が印字されない
      *                                  不具合を修正
      *  01.00.17    吉川    14/10/03    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.18    吉川    15/01/19    平成２７年１月改正対応
      *  01.00.19    吉川    15/02/10    分点時は公費２欄に日数、点数を記載
      *  01.00.20    吉川    15/03/03    保険点数が印字されないのを修正
      *  01.00.21    萩野    15/10/08    医療機関編集情報への開設者名追加対応
      *  01.00.22    門間    16/05/17    特記事項欄に「37申出」を追加
      *  01.00.23    門間    16/06/03    対象公費はPGオプションから取得する
      *  01.00.24    吉川    16/09/12    長期上限を超えたら印字対象外
      *  01.00.25    吉川    17/07/14    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.26    萩野    18/05/02    SITEKBN変更（動作に変更なし）
      *                                  特記事項に印字内容追加
      *  01.00.27    井上    18/10/11    修正履歴コメント削除
      *  01.00.28    井上    19/04/26    改元対応
      *                                  傷病名欄開始日の元号改正対応
      *  01.00.29    井上    20/02/14    生年月日欄を元号の数字も印字するように修正
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
           COPY  "SEI0609.INC".
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
           COPY    "CPRECEERR.INC".
      *
           COPY  "SC0609.INC".
           COPY  "SC0610.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF101      PIC X(02).
           03  STS-RECE02     PIC X(02).
           03  STS-RECEERR    PIC X(02).
           03  STS-RECE       PIC X(02).
           03  STS-KENSIN     PIC X(02).
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
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-SRYACT     PIC 9(01).
           03  FLG-PTCOM      PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(04).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDX4           PIC 9(04).
           03  IDW1           PIC 9(04).
           03  IDW2           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
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
      *
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
           03  WRK-HKNJANUM2         PIC X(08).
           03  WRK-PRTID             PIC X(20).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOBETUCITYNUM     PIC X(03).
      *    傷病名編集
           03  WRK-BYOMEI                      PIC X(150).
           03  WRK-BYOMEIW                     PIC X(150).
      *
           03  WRK-BYO-OLD.
               05  WRK-BO-SYUBYOFLG            PIC X(01).
               05  WRK-BO-SRYYMD               PIC X(08).
               05  WRK-BO-TENKIKBN             PIC X(01).
               05  WRK-BO-TENKIYMD             PIC X(08).
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1                     PIC X(54).
           03  WRK-BYOMEI1-W   REDEFINES       WRK-BYOMEI1.
               05  WRK-BYOMEI1-NO              PIC X(06).
               05  WRK-BYOMEI1-NAME            PIC X(48).
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
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-KOHID               PIC 9(10).
           03  WRK-FTNMONEY-G          PIC 9(08).
           03  WRK-ETC-TBL.
               05  WRK-ETC-KOHNUM          PIC X(03) OCCURS 2.
               05  WRK-ETC-TENSU           PIC 9(07) OCCURS 2.
               05  WRK-ETC-ITBFTN          PIC 9(07) OCCURS 2.
               05  WRK-ETC-FTNJANUM        PIC X(20).
               05  WRK-ETC-JKYSNUM         PIC X(20).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-INPUTCOMENT         PIC X(80).
           03  WRK-INPUTCOMENT2        PIC X(14).
           03  WRK-INPUTCOMENT-KIN-T.
               05  WRK-INPUTCOMENT-KIN     PIC 9(07).
           03  WRK-INPUTCOMENT-KIN2-T.
               05  WRK-INPUTCOMENT-KIN2    PIC X(07).
           03  WRK-KENSINCD            PIC X(09).
      *
           03  WRK-PATH              PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
      *
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
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
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   48.
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   5.
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
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
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
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
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
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
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
           MOVE    "SEIKYU0609"    TO  JOB-PGID
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
           MOVE    "RECE031"           TO  RECE031PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID        TO  RECE031PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM             TO  RECE031PARA-HOSPNUM
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
           MOVE  SPA-HOSPNUM   TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU0609 SYS 1001 ERR ***"
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
           MOVE  SPA-HOSPNUM   TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU0609 SYS 1002 ERR ***"
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
           MOVE  SPA-HOSPNUM        TO  SYS-2005-HOSPNUM
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
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
           .
       100-INIT-EXT.
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
           CALL  "SEIKYU06BYOMEI1"    USING  WRK-PARA-LINK
           DISPLAY "編集２開始"
           CALL  "SEIKYU06BYOMEI2"    USING  WRK-PARA-LINK
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
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU0609"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    検診コメント設定
           MOVE    "KENSIN"                  TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           IF  WRK-OPTION(1)(1:2)  =  "※"
             MOVE    SPACE                   TO  WRK-KENSINCD
           ELSE
             MOVE    WRK-OPTION(1)(1:9)      TO  WRK-KENSINCD
           END-IF
           DISPLAY "検診コメントコード =" WRK-KENSINCD
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
      *    月途中の公費切替え対応
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
             INITIALIZE                            WRK-CHK-KOHNUM-TBL
             MOVE    RECE02-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  800-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
      *
               PERFORM 20011-SYUKEI-SEC
             END-IF
           END-PERFORM
      *
           PERFORM  900-KOHSKY-READ-SEC
           DISPLAY "2001 END"
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
           INITIALIZE       MF101-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF101-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF101-PTNUM
           MOVE  RECE02-RECEKA       TO  MF101-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF101-HKNJANUM-KEY
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
           MOVE  RECE02-ZAITAKU      TO  MF101-ZAI
      *
           MOVE  RECE02-KOHFTNJANUM (IDZ)  TO  MF101-FTNJANUM
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF101-KOHINF
           COMPUTE  IDY  =  IDZ  +  1
           MOVE  RECE02-JNISSU      (IDY)  TO  MF101-JNISSU
           MOVE  RECE02-FTNMONEY    (IDY)  TO  MF101-FTNMONEY2
           MOVE  RECE02-TOTALTEN    (IDY)  TO  MF101-TOTALTEN
           MOVE  RECE02-SHOKUJIINF(1)      TO  MF101-SHOKUJIINF-G
      *
      *    個別発行用市町村番号
           MOVE  RECE02-KOHFTNJANUM(IDZ)(5:3)    TO  WRK-KOBETUCITYNUM
      *
      *    請求管理情報より、他公費情報取得
           IF       IDZ           =     1
           AND      RECE02-KOHNUM(IDZ + 1)  =  SPACE
             PERFORM  900-SEIKYU-READ-SEC
              IF  RECE10-KOHNUM(1)  NOT =  SPACE
              OR  RECE10-KOHNUM(2)  NOT =  SPACE
                MOVE   RECE10-TOTALTEN(1)         TO  MF101-TOTALTEN-G
              END-IF
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     2  OR
                      RECE10-KOHNUM(IDX)  =  SPACE
               MOVE    RECE10-JNISSU(IDX + 1)     TO  MF101-ENISSU(IDX)
               MOVE    RECE10-TOTALTEN(IDX + 1)   TO  MF101-ETENSU(IDX)
               MOVE    RECE10-FTNMONEY(IDX + 1)   TO  MF101-EITBFTN(IDX)
               MOVE    RECE10-KOHNUM(IDX)         TO  WRK-KOHNUM
               MOVE    RECE10-SHOKUJIINF(IDX + 1)
                                           TO  MF101-ESHOKUJIINF(IDX)
               PERFORM 930-PTKOHINF-SET-SEC
               MOVE    WRK-ETC-FTNJANUM    TO  MF101-EKOHFTNJANUM(IDX)
               MOVE    WRK-ETC-JKYSNUM     TO  MF101-EKOHJKYSNUM(IDX)
             END-PERFORM
      *      特記事項
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     3
                 IF   RECE10-TOKKI(IDX)  NOT =  SPACE
                   MOVE  RECE10-TOKKI(IDX)  TO  MF101-TOKKI(IDX)
                 END-IF
             END-PERFORM
           END-IF
      *
           MOVE     MF101-KOHNUM           TO  WRK-KOHNUM
           MOVE     MF101-KOHID            TO  WRK-KOHID
           MOVE     1                      TO  FLG-ERR
           MOVE     ZERO                   TO  WRK-FTNMONEY-G
           INITIALIZE                          WRK-ETC-TBL
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"        TO  WRK-PATH
           ELSE
               MOVE    "key25"        TO  WRK-PATH
           END-IF
           DISPLAY  "収納読み込み開始"
           PERFORM  920-SYUNOU-SET-SEC
           IF       IDZ           =     1
           AND      RECE02-KOHNUM(IDZ + 1)  =  SPACE
             DISPLAY "請求管理から公費情報取得済み" RECE02-PTNUM
           ELSE
             DISPLAY  "患者番号=" RECE02-PTNUM
                      "その他公費=" WRK-ETC-KOHNUM(1)
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
               PERFORM 930-PTKOHINF-SET-SEC
               MOVE    WRK-ETC-TENSU(1)    TO  MF101-ETENSU(IDX)
               MOVE    WRK-ETC-ITBFTN(1)   TO  MF101-EITBFTN(IDX)
               MOVE    WRK-ETC-FTNJANUM    TO  MF101-EKOHFTNJANUM(IDX)
               MOVE    WRK-ETC-JKYSNUM     TO  MF101-EKOHJKYSNUM(IDX)
               IF      WRK-ETC-KOHNUM(2)  NOT  =  SPACE
                 MOVE    WRK-ETC-KOHNUM(2)   TO  WRK-KOHNUM
                 PERFORM 930-PTKOHINF-SET-SEC
                 MOVE    WRK-ETC-TENSU(2)  TO  MF101-ETENSU(IDY)
                 MOVE    WRK-ETC-ITBFTN(2) TO  MF101-EITBFTN(IDY)
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
           END-IF
      *
      *
           IF  RECE02-FTNMONEY(1)          >   ZERO  AND
               RECE02-FTNMONEY(1)          <   WRK-FTNMONEY-G
             MOVE  RECE02-FTNMONEY(1)      TO  WRK-FTNMONEY-G
           END-IF
           MOVE  WRK-FTNMONEY-G            TO  MF101-FTNMONEY
      *
      *    フラグ（未就学児、特例措置）
           MOVE    RECE02-FLG (1)     TO  MF101-FLG (1)
           MOVE    RECE02-FLG (2)     TO  MF101-FLG (2)
      *
      *    診療行為情報取得
           MOVE    WRK-CONS-KIN-OKIKAE     TO  WRK-SRYCD
           PERFORM 940-SRYACT-SET-SEC
           IF  FLG-SRYACT  =  ZERO
      *      公費−一部負担金置換編集
             PERFORM  200112-ICHIBUFTN-HENSYU-SEC
      *
             MOVE  WRK-KENSINCD            TO  WRK-SRYCD
             PERFORM 940-SRYACT-SET-SEC
             IF  FLG-SRYACT  =  ZERO
      *        検診コメント取得
               PERFORM  200113-COMMENT-HENSYU-SEC
             END-IF
           END-IF
      *
      *    182＋長期で10000円を超えたら印字対象外
           IF  (MF101-KOHNUM     = "182")
           AND (MF101-TYOKI      =  1)
           AND (MF101-FTNMONEY2  >=  10000)
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF     FLG-ERR         =     1
             DISPLAY "助成対象外="  RECE02-PTNUM
           ELSE
             WRITE  MF101-REC
             ADD  1  TO  CNT-MF101
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
           MOVE    "06"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  RECE02-PTNUM
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
      *    コメント編集処理
      *****************************************************************
       200113-COMMENT-HENSYU-SEC                SECTION.
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >  5
             IF  ((SRY-SRYCD(IDX4)(1:1)  =  "8")  OR
                  (SRY-SRYCD(IDX4)(1:3)  =  "008"))
             AND ( SRY-INPUTNUM(IDX4)  NOT =  ZERO )
      *        患者コメント情報読込
               PERFORM  950-PTCOM-READ-SEC
               STRING  "＊"                 DELIMITED BY SIZE
                       PTCOM-INPUTCOMENT    DELIMITED BY SIZE
                       INTO                 MF101-KENSIN
               END-STRING
             END-IF
      *
           END-PERFORM
      *
           .
       200113-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           DISPLAY "2002-PRINT START"
      *
           OPEN     INPUT       MF101-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
      *    帳票印刷処理
             IF  MF101-NYUGAIKBN  =  "2"
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
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC06
      *
      *    印刷日付
           MOVE    MF101-SRYYM         TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  SC06-PRTYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN       TO  SC06-HOSPCDN
      *
      *    住所
           MOVE  WRK-HOSPADRS           TO   SC06-ADRS
           INSPECT SC06-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME           TO   SC06-HOSPNAME
           INSPECT SC06-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    管理者名
           MOVE  WRK-KAISETUNAME        TO  SC06-KAISETUNAME
      *
      *    保険種別区分
           EVALUATE    MF101-RECESYUBETU(2:1)
               WHEN    "1"
                   MOVE   "◯"            TO  SC06-HKNKBN(1)
               WHEN    "2"
                   MOVE   "◯"            TO  SC06-HKNKBN(2)
               WHEN    "3"
                   MOVE   "◯"            TO  SC06-HKNKBN(3)
               WHEN    "4"
                   MOVE   "◯"            TO  SC06-HKNKBN(4)
           END-EVALUATE
      *
      *    単独併用区分
           IF  MF101-EKOHFTNJANUM (1)  = SPACE
               MOVE  "○"                TO  SC06-HEIKBN (1)
           ELSE
               MOVE  "○"                TO  SC06-HEIKBN (2)
           END-IF
      *
      *    本人家族区分
           EVALUATE    MF101-RECESYUBETU (4:1)
               WHEN    "1"
               WHEN    "2"
                   MOVE    "◯"    TO  SC06-HONKZKKBN (1)
               WHEN    "3"
               WHEN    "4"
                   MOVE    "◯"    TO  SC06-HONKZKKBN (2)
               WHEN    "5"
               WHEN    "6"
                   MOVE    "◯"    TO  SC06-HONKZKKBN (3)
               WHEN    "7"
               WHEN    "8"
                   MOVE    "◯"    TO  SC06-KOREIKBN (1)
               WHEN    "9"
               WHEN    "0"
                   MOVE    "◯"    TO  SC06-KOREIKBN (2)
           END-EVALUATE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    老人市町村番号
           MOVE     MF101-RJNFTNJANUM       TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT           TO  SC06-RJNFTNJANUM
      *
      *    老人受給者番号
           MOVE     MF101-RJNJKYSNUM        TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT           TO  SC06-RJNJKYSNUM
      *
      *    負担者番号・受給者番号１
      *      全国公費
           IF  MF101-EKOHFTNJANUM (1) NOT = SPACE
             MOVE     MF101-EKOHFTNJANUM (1)  TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT           TO  SC06-FTNJANUM1
      *
             MOVE     MF101-EKOHJKYSNUM (1)   TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT           TO  SC06-JKYSNUM1
      *      地方公費
             MOVE     MF101-KOHFTNJANUM       TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT           TO  SC06-FTNJANUM2
      *
             MOVE  7  TO  IDX
             MOVE  7  TO  IDY
             PERFORM   UNTIL  IDX  <  1
                IF  MF101-KOHJKYSNUM(IDX:1) NOT = SPACE
                  MOVE  MF101-KOHJKYSNUM(IDX:1)  
                                              TO  SC06-JKYSNUM2(IDY:1)
                  COMPUTE  IDY  =  IDY  -  1
                END-IF
                COMPUTE  IDX  =  IDX  -  1
             END-PERFORM
      *
             MOVE     SC06-JKYSNUM2              TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT              TO  SC06-JKYSNUM2
           ELSE
             MOVE     MF101-KOHFTNJANUM       TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT           TO  SC06-FTNJANUM1
      *
             MOVE  7  TO  IDX
             MOVE  7  TO  IDY
             PERFORM   UNTIL  IDX  <  1
                IF  MF101-KOHJKYSNUM(IDX:1) NOT = SPACE
                  MOVE  MF101-KOHJKYSNUM(IDX:1)
                                              TO  SC06-JKYSNUM1(IDY:1)
                  COMPUTE  IDY  =  IDY  -  1
                END-IF
                COMPUTE  IDX  =  IDX  -  1
             END-PERFORM
      *
             MOVE     SC06-JKYSNUM1              TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT              TO  SC06-JKYSNUM1
           END-IF
      *
      *    保険者番号
           MOVE  8  TO  IDX
           MOVE  8  TO  IDY
           PERFORM   UNTIL  IDX  <  1
              IF  MF101-HKNJANUM(IDX:1) NOT = SPACE
                MOVE  MF101-HKNJANUM(IDX:1)  TO  SC06-HKNJANUM(IDY:1)
                COMPUTE  IDY  =  IDY  -  1
              END-IF
              COMPUTE  IDX  =  IDX  -  1
           END-PERFORM
      *
           MOVE     SC06-HKNJANUM             TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT             TO  SC06-HKNJANUM
      *
      *    給付割合
           IF   MF101-SRYYM   >=  WRK-CONS-SRYYM-200804
           AND  MF101-FLG (2)  =  1
               MOVE  "○"    TO  SC06-KYURATE8
           ELSE
             EVALUATE  MF101-KYURATE
               WHEN   0
                 MOVE  "○"  TO  SC06-KYURATE10 
               WHEN  10
                 MOVE  "○"  TO  SC06-KYURATE9
               WHEN  20
                 MOVE  "○"  TO  SC06-KYURATE8
               WHEN  30
                 MOVE  "○"  TO  SC06-KYURATE7
             END-EVALUATE
           END-IF
      *
      *    被保険者証記号・番号
           MOVE    MF101-KIGO      TO    SC06-KIGO
           MOVE    MF101-NUM       TO    SC06-BANGO
      *
      *    患者番号
           MOVE  MF101-PTNUM         TO  SC06-PTNUM
      *
      *    氏名
           MOVE  MF101-NAME          TO  SC06-NAME
      *
      *    カナ氏名
           MOVE  MF101-KANANAME      TO  SC06-KANANAME
      *
      *    性別
           MOVE  "○"                TO  SC06-SEX (MF101-SEX)
      *
      *    生年月日
           MOVE  MF101-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31011-SEIWA-HEN-SEC
           PERFORM 31012-SEIWA-HEN-SEC
           STRING  LNK-DAY1-GENGO            DELIMITED BY SIZE
                   LNK-DAY1-GENMEI(1:2)      DELIMITED BY SIZE
                   " "                       DELIMITED BY SIZE
                   LNK-DAY2-EDTYMD3(5:18)    DELIMITED BY SIZE
                   INTO                SC06-BIRTHDAY2
           END-STRING
      *
      *    特記事項
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL  IDY  >  TBL-TOKKI-MAX
               IF  MF101-TOKKI(IDX)  =  TBL-TOKKICD(IDY)
                   MOVE  TBL-TOKKINAME(IDY)   TO  SC06-TOKKI(IDX)
                   MOVE  TBL-TOKKI-MAX  TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
      *
      *    診療実日数(保険)
           MOVE     MF101-JNISSU     TO  WRK-Z2
           MOVE     WRK-Z2           TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT    TO  SC06-SNISSU1
      *
           IF  MF101-EKOHFTNJANUM (1) NOT = SPACE
      *    診療実日数(公費１)
             MOVE     MF101-ENISSU(1)  TO  WRK-Z2
             MOVE     WRK-Z2           TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT    TO  SC06-SNISSU2
      *    診療実日数(公費２)
             MOVE     MF101-JNISSU     TO  WRK-Z2
             MOVE     WRK-Z2           TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT    TO  SC06-SNISSU3
           END-IF 
      *
      *    点数
           IF  MF101-TOTALTEN-G  NOT =  ZERO
             MOVE  MF101-TOTALTEN-G   TO  WRK-Z7
           ELSE
             MOVE  MF101-TOTALTEN     TO  WRK-Z7
           END-IF
           MOVE  WRK-Z7              TO  SC06-STENSU1
           IF  MF101-EKOHFTNJANUM (1) NOT = SPACE
      *    点数（公費１）
             MOVE  MF101-ETENSU(1)     TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC06-STENSU2
      *    点数（公費２）
             MOVE  MF101-TOTALTEN      TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC06-STENSU3
           END-IF
      *    一部負担金額
           MOVE  MF101-FTNMONEY      TO  WRK-Z7
           MOVE  WRK-Z7              TO  SC06-ITBFTN1
      *    一部負担金額
           IF  MF101-EKOHFTNJANUM (1) NOT = SPACE
             IF  MF101-OKIKAE-FTN(1)  NOT =  ZERO
               MOVE  MF101-OKIKAE-FTN(1)   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC06-ITBFTN2
             ELSE
               MOVE  MF101-EITBFTN(1)  TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC06-ITBFTN2
             END-IF
      *
             IF  MF101-OKIKAE-FTN(2)  NOT =  ZERO
               MOVE  MF101-OKIKAE-FTN(2)   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC06-ITBFTN3
             ELSE
               MOVE  MF101-FTNMONEY2   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC06-ITBFTN3
             END-IF
           ELSE
             IF  MF101-OKIKAE-FTN(1)  NOT =  ZERO
               MOVE  MF101-OKIKAE-FTN(1)   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC06-ITBFTN2
             ELSE
               MOVE  MF101-FTNMONEY2   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC06-ITBFTN2
             END-IF
           END-IF
      *    置換コメント
           MOVE  MF101-KENSIN          TO  SC06-KENSIN
      *
      *    傷病名編集処理
           PERFORM 3201-BYOMEI-HEN-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
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
               OR     ( MF101-HOSPNUM    NOT =   RECE031-HOSPNUM      )
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
                           TO SC06-BYOMEI(WRK-LINEBYO)(2:52) 
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
                                     TO  SC06-BYOMEI(WRK-LINEBYO)
                                         SC06-SRYYMD(WRK-LINEBYO)
             END-IF
             MOVE  "　　　年　　月　　日"
                                     TO  SC06-SRYYMD(WRK-LINEBYO)(7:)
           END-PERFORM
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
                       END-IF
                   END-IF    
               END-IF
           ELSE
                IF  WRK-LINEBYO       <   WRK-BYOMEI-LINE
                    IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                        PERFORM 3221-BYOMEI1-OUT-SEC
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
                       END-IF                                   
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
           IF  WRK-LINEBYO    >=    5
              MOVE    1         TO  FLG-RECE031
           END-IF
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
                                  SC06-BYOMEI(WRK-LINEBYO)(2:52)
                       END-IF
                   END-IF
               END-IF
           END-IF        
      *
           .
       3211-SYUBYOFLG-HENSYU-EXT.
           EXIT.
      *
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
               MOVE    WRK-BYOMEI1-NO      TO  SC06-SRYYMD
                                                    (WRK-LINEBYO)
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (1:2)
                                           TO  SC06-SRYYMD
                                                    (WRK-LINEBYO)(7:2)
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  SC06-SRYYMD
                                                    (WRK-LINEBYO)(9:)
           END-IF
      *    病名
           MOVE    WRK-BYOMEI1     TO  SC06-BYOMEI(WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3221-BYOMEI1-OUT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
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
           MOVE    "SC0609.red"        TO  SPRT-PRTID
           MOVE    MF101-PTID          TO  SPRT-PTID
           MOVE    "複写式レセプト"    TO  SPRT-TITLE
           MOVE    SC06                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
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
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC06
           INITIALIZE  SC10
      *
      *    患者番号
           MOVE    MF101-PTNUM          TO  SC10-PTNUM
      *
      *    提出先
           IF      MF101-RECESYUBETU  =    ZERO    OR  8888
               MOVE    SPACE            TO  SC10-TEISYUTUSAKI
           ELSE    
               EVALUATE    MF101-TEISYUTUSAKI
                   WHEN    1
                       MOVE    "１社"   TO  SC10-TEISYUTUSAKI
                   WHEN    2
                       MOVE    "２国"   TO  SC10-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE    TO  SC10-TEISYUTUSAKI
               END-EVALUATE
           END-IF
      *
      *    診療年月（診療年月日として退避）
           MOVE    MF101-SRYYM          TO  WRK-SYMD
           MOVE    1                    TO  WRK-SDD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3(1:4)TO  SC10-SRYG
           MOVE    LNK-DAY2-EDTYMD1-Y   TO  SC10-SRYY
           MOVE    LNK-DAY2-EDTYMD1-M   TO  SC10-SRYM
      *
      *    県番
           MOVE    SYS-1001-PREFNUM     TO  SC10-PREFNUM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN     TO  SC10-HOSPCDN
      *
      *    <保険種別１>
           EVALUATE    MF101-RECESYUBETU (2:1)
               WHEN    "1"
                   EVALUATE    MF101-TEISYUTUSAKI
                       WHEN    1
                           MOVE    "１社"  TO  SC10-HKNSBT1
                       WHEN    2
                           MOVE    "１国"  TO  SC10-HKNSBT1
                   END-EVALUATE
               WHEN    "2"
                   MOVE    "２公費"        TO  SC10-HKNSBT1
               WHEN    "3"
                   MOVE    "３高齢"        TO  SC10-HKNSBT1
               WHEN    "4"
                   MOVE    "４退職"        TO  SC10-HKNSBT1
           END-EVALUATE
      *
      *    <保険種別２>
           IF  MF101-EKOHFTNJANUM (1) = SPACE
               MOVE    "２２併"     TO  SC10-HKNSBT2
           ELSE
               MOVE    "３３併"     TO  SC10-HKNSBT2
           END-IF
      *
      *    <本人・家族>
      *    不明分・自費は編集しない
           EVALUATE    MF101-RECESYUBETU (4:1)
               WHEN    "9"
                   MOVE    "９高入７"   TO  SC10-HKNKZK
               WHEN    "1"
                   MOVE    "１本入"     TO  SC10-HKNKZK
               WHEN    "3"
                   MOVE    "３六入"     TO  SC10-HKNKZK
               WHEN    "5"
                   MOVE    "５家入"     TO  SC10-HKNKZK
               WHEN    "7"
                   MOVE    "７高入一"   TO  SC10-HKNKZK
           END-EVALUATE
      *
      *    老人市町村番号
           MOVE     MF101-RJNFTNJANUM       TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT           TO  SC10-RJNFTNJANUM
      *
      *    老人受給者番号
           MOVE     MF101-RJNJKYSNUM        TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT           TO  SC10-RJNJKYSNUM
      *    負担者番号・受給者番号１
      *      全国公費
           IF  MF101-EKOHFTNJANUM (1) NOT = SPACE
             MOVE     MF101-EKOHFTNJANUM (1)  TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT           TO  SC10-FTNJANUM1
      *
             MOVE     MF101-EKOHJKYSNUM (1)   TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT           TO  SC10-JKYSNUM1
      *      地方公費
             MOVE     MF101-KOHFTNJANUM       TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT           TO  SC10-FTNJANUM2
      *
             MOVE  7  TO  IDX
             MOVE  7  TO  IDY
             PERFORM   UNTIL  IDX  <  1
                IF  MF101-KOHJKYSNUM(IDX:1) NOT = SPACE
                  MOVE  MF101-KOHJKYSNUM(IDX:1)  
                                              TO  SC10-JKYSNUM2(IDY:1)
                  COMPUTE  IDY  =  IDY  -  1
                END-IF
                COMPUTE  IDX  =  IDX  -  1
             END-PERFORM
      *
             MOVE     SC10-JKYSNUM2              TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT              TO  SC10-JKYSNUM2
           ELSE
             MOVE     MF101-KOHFTNJANUM       TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT           TO  SC10-FTNJANUM1
      *
             MOVE  7  TO  IDX
             MOVE  7  TO  IDY
             PERFORM   UNTIL  IDX  <  1
                IF  MF101-KOHJKYSNUM(IDX:1) NOT = SPACE
                  MOVE  MF101-KOHJKYSNUM(IDX:1)
                                              TO  SC10-JKYSNUM1(IDY:1)
                  COMPUTE  IDY  =  IDY  -  1
                END-IF
                COMPUTE  IDX  =  IDX  -  1
             END-PERFORM
      *
             MOVE     SC10-JKYSNUM1              TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT              TO  SC10-JKYSNUM1
           END-IF
      *
      *    保険者番号
           MOVE  8  TO  IDX
           MOVE  8  TO  IDY
           PERFORM   UNTIL  IDX  <  1
              IF  MF101-HKNJANUM(IDX:1) NOT = SPACE
                MOVE  MF101-HKNJANUM(IDX:1)  TO  SC10-HKNJANUM(IDY:1)
                COMPUTE  IDY  =  IDY  -  1
              END-IF
              COMPUTE  IDX  =  IDX  -  1
           END-PERFORM
      *
           MOVE     SC10-HKNJANUM             TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT             TO  SC10-HKNJANUM
      *
      *    記号・番号
           MOVE    MF101-KIGO          TO  SC10-KIGO
           MOVE    MF101-NUM           TO  SC10-NUM
      *
      *    給付割合
           IF   MF101-SRYYM   >=  WRK-CONS-SRYYM-200804
           AND  MF101-FLG (2)  =  1
               MOVE  "　８割"    TO  SC10-KYURATE
           ELSE
             EVALUATE  MF101-KYURATE
               WHEN   0
                 MOVE  "１０割"  TO  SC10-KYURATE
               WHEN  10
                 MOVE  "　９割"  TO  SC10-KYURATE
               WHEN  20
                 MOVE  "　８割"  TO  SC10-KYURATE
               WHEN  30
                 MOVE  "　７割"  TO  SC10-KYURATE
             END-EVALUATE
           END-IF
      *
      *    カナ氏名
           MOVE  MF101-KANANAME   TO  SC10-KANANAME
      *
      *    氏名
           MOVE    MF101-NAME          TO  SC10-NAME
      *
      *    性別
           EVALUATE    MF101-SEX
           WHEN    "1"
               MOVE    "１"            TO  SC10-SEXNUM
               MOVE    "男"            TO  SC10-SEXN
           WHEN    "2"
               MOVE    "２"            TO  SC10-SEXNUM
               MOVE    "女"            TO  SC10-SEXN
           END-EVALUATE
      *
      *    生年月日
           MOVE  MF101-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31011-SEIWA-HEN-SEC
           PERFORM 31012-SEIWA-HEN-SEC
           STRING  LNK-DAY1-GENGO            DELIMITED BY SIZE
                   LNK-DAY1-GENMEI(1:2)      DELIMITED BY SIZE
                   " "                       DELIMITED BY SIZE
                   LNK-DAY2-EDTYMD3(5:18)    DELIMITED BY SIZE
                   INTO                SC10-BIRTHDAY2
           END-STRING
      *
      *    特記事項
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL  IDY  >  TBL-TOKKI-MAX
               IF  MF101-TOKKI(IDX)  =  TBL-TOKKICD(IDY)
                   MOVE  TBL-TOKKINAME(IDY)   TO  SC10-TOKKI(IDX)
                   MOVE  TBL-TOKKI-MAX  TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
      *
      *    医療機関所在地
           MOVE    WRK-HOSPADRS        TO   SC10-ADRS
           INSPECT SC10-ADRS           REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO   SC10-HOSPNAME
           INSPECT SC10-HOSPNAME       REPLACING ALL "  " BY "　"
           MOVE  WRK-KAISETUNAME        TO  SC10-KAISETUNAME
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *
      *    診療実日数(保険)
           MOVE     MF101-JNISSU     TO  WRK-Z2
           MOVE     WRK-Z2           TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT    TO  SC10-SNISSU1
      *
           IF  MF101-EKOHFTNJANUM (1) NOT = SPACE
      *    診療実日数(公費１)
             MOVE     MF101-ENISSU(1)  TO  WRK-Z2
             MOVE     WRK-Z2           TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT    TO  SC10-SNISSU2
      *    診療実日数(公費２)
             MOVE     MF101-JNISSU     TO  WRK-Z2
             MOVE     WRK-Z2           TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT    TO  SC10-SNISSU3
           END-IF
      *
      *    点数
           IF  MF101-TOTALTEN-G  NOT =  ZERO
             MOVE  MF101-TOTALTEN-G    TO  WRK-Z7
           ELSE
             MOVE  MF101-TOTALTEN      TO  WRK-Z7
           END-IF
           MOVE  WRK-Z7              TO  SC10-STENSU1
           IF  MF101-EKOHFTNJANUM (1) NOT = SPACE
      *    点数（公費１）
             MOVE  MF101-ETENSU(1)     TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC10-STENSU2
      *    点数（公費２）
             MOVE  MF101-TOTALTEN      TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC10-STENSU3
           END-IF
      *    一部負担金額
           MOVE  MF101-FTNMONEY      TO  WRK-Z7
           MOVE  WRK-Z7              TO  SC10-ITBFTN1
      *    一部負担金額
           IF  MF101-EKOHFTNJANUM (1) NOT = SPACE
             IF  MF101-OKIKAE-FTN(1)  NOT =  ZERO
               MOVE  MF101-OKIKAE-FTN(1)   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC10-ITBFTN2
             ELSE
               MOVE  MF101-EITBFTN(1)  TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC10-ITBFTN2
             END-IF
      *
             IF  MF101-OKIKAE-FTN(2)  NOT =  ZERO
               MOVE  MF101-OKIKAE-FTN(2)   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC10-ITBFTN3
             ELSE
               MOVE  MF101-FTNMONEY2   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC10-ITBFTN3
             END-IF
           ELSE
             IF  MF101-OKIKAE-FTN(1)  NOT =  ZERO
               MOVE  MF101-OKIKAE-FTN(1)   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC10-ITBFTN2
             ELSE
               MOVE  MF101-FTNMONEY2   TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC10-ITBFTN2
             END-IF
           END-IF
      *    食事日数
           MOVE    MF101-SHOKUJINISSU-G    TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC10-SNISSU(1)
      *    食事療養費
           MOVE    MF101-SHOKUJIRYOYOHI-G  TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC10-SKJRYOHOHI(1)
      *    標準負担額
           MOVE    MF101-SHOKUJIFTN-G      TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC10-SKJFTN(1)
      *
           IF  MF101-ESHOKUJINISSU (1)  NOT =  ZERO
             MOVE    MF101-ESHOKUJINISSU(1) TO  WRK-Z2
             MOVE    WRK-Z2                 TO  SC10-SNISSU(2)
             MOVE    MF101-ESHOKUJIRYOYOHI(1)  TO  WRK-Z9
             MOVE    WRK-Z9                 TO  SC10-SKJRYOHOHI(2)
             MOVE    MF101-ESHOKUJIFTN(1)   TO  WRK-Z9
             MOVE    WRK-Z9                 TO  SC10-SKJFTN(2)
           END-IF
           IF  MF101-ESHOKUJINISSU (2)  NOT =  ZERO
             MOVE    MF101-ESHOKUJINISSU(2) TO  WRK-Z2
             MOVE    WRK-Z2                 TO  SC10-SNISSU(3)
             MOVE    MF101-ESHOKUJIRYOYOHI(2)  TO  WRK-Z9
             MOVE    WRK-Z9                 TO  SC10-SKJRYOHOHI(3)
             MOVE    MF101-ESHOKUJIFTN(2)   TO  WRK-Z9
             MOVE    WRK-Z9                 TO  SC10-SKJFTN(3)
           END-IF
      *
      *    置換コメント
           MOVE  MF101-KENSIN               TO  SC10-KENSIN
      *
      *    傷病名編集処理
           PERFORM 3201-BYOMEI-HEN-SEC
      *
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >     5
             MOVE  SC06-BYOMEI-TBL(IDX2)  TO  SC10-BYOMEI-TBL(IDX2)
             MOVE  SC06-SRY-TBL (IDX2)    TO  SC10-SRY-TBL (IDX2)
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
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
           MOVE    "SC0610.red"        TO  SPRT-PRTID
           MOVE    MF101-PTID          TO  SPRT-PTID
           MOVE    "複写式レセプト"    TO  SPRT-TITLE
           MOVE    SC10                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
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
      *
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
      *    西暦・和暦変換処理
      *****************************************************************
       31011-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           .
       31011-SEIWA-HEN-EXT.
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
           DISPLAY "*** SEIKYU0609 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU0609 OUT "  CNT-MF101
           DISPLAY "*** SEIKYU0609 END ***"
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
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF101-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF101
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF101
           END-READ
           .
       800-SKTTMP-READ-EXT.
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
      *    公費請求書マスタＲＥＡＤ
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
      *    入院データは対象外にする
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
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
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM                TO  COMB-HOSPNUM
           MOVE    SYU-PTID                   TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM            TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     4  OR
                    SYU-KOH-HKNNUM(IDX) =  SPACE
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
                 IF   (RECE02-FTNMONEY(1)    >  ZERO)
                 OR   (SYU-KOH-HKNNUM(IDX)  NOT =  "010" AND "011"
                                            AND    "021" AND "020")
                   ADD     SYU-KOH-COMPFTN(IDX)
                                            TO  WRK-ETC-ITBFTN(IDY)
                 END-IF
               END-IF
      *
      *    計算負担金に差が有る場合、地方公費の助成があったと判定する
               IF   SYU-KOH-HKNNUM(IDX) = WRK-KOHNUM
                 IF     IDX             =  1
                   IF  SYU-SYUCOMPFTN-ENTANI   
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO         TO  FLG-ERR
                   END-IF
                 ELSE
                   IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO         TO  FLG-ERR
                   END-IF
                 END-IF
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX) = "972"
                 MOVE    1     TO      MF101-TYOKI
               END-IF
      *      長期上位
               IF  SYU-KOH-HKNNUM(IDX) = "974"
                 MOVE    2     TO      MF101-TYOKI
               END-IF
      *
           END-PERFORM
      *
           IF  RECE02-FTNMONEY(1)       >  ZERO
             IF  RECE02-RJNHKNNUM  NOT  =  SPACE
               ADD  SYU-KOH-COMPFTN(1) TO WRK-FTNMONEY-G
             ELSE
               ADD  SYU-SYUCOMPFTN     TO WRK-FTNMONEY-G
             END-IF
           END-IF
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
                 MOVE  "tbl_syunou"       TO  MCP-TABLE
                 MOVE  WRK-PATH           TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"   TO  MCP-FUNC
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
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
             MOVE    "key4"                  TO  MCP-PATHNAME
             PERFORM  935-PTKOHINF-READ-SEC
             PERFORM  UNTIL  FLG-PTKOHINF = 1
               IF  PTKOH-SAKJOKBN  NOT = "1"
                 IF  PTKOH-TEKSTYMD(1:6) <= RECE02-SRYYM(1:6)  AND 
                     PTKOH-TEKEDYMD(1:6) >= RECE02-SRYYM(1:6)
                     MOVE  PTKOH-FTNJANUM  TO  WRK-ETC-FTNJANUM
                     MOVE  PTKOH-JKYSNUM   TO  WRK-ETC-JKYSNUM
                 END-IF
               END-IF
                 MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
                 MOVE    "key4"                  TO  MCP-PATHNAME
                 PERFORM  935-PTKOHINF-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
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
           MOVE  "DBFETCH"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
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
       950-PTCOM-READ-EXT.
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
             MOVE  "DBFETCH"                TO  MCP-FUNC
             MOVE  "tbl_syskanri"          TO  MCP-TABLE
             MOVE  "key10"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
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
      *
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
      *
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
      *
