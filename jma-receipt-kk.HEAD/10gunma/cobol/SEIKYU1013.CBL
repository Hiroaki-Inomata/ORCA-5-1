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
       PROGRAM-ID.     SEIKYU1013.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 群馬・福祉医療費（連記式）電子レセプト
      *                      (143,243,145,245,146,246)
      *  管理者            :
      *  作成日付    作業者        記述
      *  09/08/01    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/09/16  ファイル名称の請求年月が
      *                                  診療年月になっているのを修正
      *                                  請求年月に診療年月が
      *                                  印字されている不具合を修正
      *                                  受給者番号は頭ゼロ付きで表示
      *                                  末尾のカンマは不要
      *  01.00.02    吉川    2009/10/23  科別と医療機関番号は頭ゼロ付きで表示
      *                                  70歳以上入院の福祉医療費請求額を印字
      *                                  負担者番号の空白をゼロで埋める
      *  01.00.03    吉川    2010/07/26  「特」欄は限度額に達してない場合は
      *                                  記載しないようにする
      *                                  福祉請求額の計算方法変更
      *                                  福祉、第一請求点数の計算方法変更
      *  01.00.04    吉川    2010/08/25  「特」欄の該当公費の上限額に
      *                                  達していない場合でも「特」とする
      *  01.00.05    吉川    2010/09/29  福祉請求点数に記載する条件を変更
      *  01.00.06    吉川    2010/10/04  請求額の計算方法変更
      *  01.00.07    吉川    2011/03/10  open-cobol1.0対応
      *  01.00.08    吉川    2011/08/16  点数欄は福祉点数を印字する
      *  01.00.09    吉川    2012/03/19  限度額認定証の有無に関わらず
      *                                  高額療養費対象は「特」欄に記載
      *  01.00.10    吉川    2012/09/12  多数該当と非課税の両方の場合
      *                                  備考欄コードは「03」とする
      *  01.00.11    吉川    2012/10/18  特定疾患と957等を持っている場合
      *                                  「特」欄に記載されないのを修正
      *                                  社保前期高齢者１割＋146障害で
      *                                  在宅の場合は、「特」とせずに
      *                                  70歳2割に含めるように修正
      *  01.00.12    吉川    2013/02/13  福祉請求点数欄には福祉分を
      *                                  第一公費請求点数欄には公費分を
      *                                  記載するように修正
      *  01.00.13    吉川    2013/02/26  福祉請求点数の計算方法修正
      *  01.00.14    吉川    2013/03/19  福祉請求点数欄と第一公費請求点数欄は
      *                                  併用全国公費に関係なく記載する
      *  01.00.15    吉川    2013/04/08  「特」欄対象に024,038,079追加
      *  01.00.16    吉川    2013/07/09  全国公費と高額療養費の場合
      *                                  全国公費を「特」とするように修正
      *  01.00.17    吉川    2013/09/11  公費と福祉が同点数の場合
      *                                  第一請求点数は公費の点数を記録
      *                                  月途中で全国公費を追加して、
      *                                  保険＋全国公費(新)を組合せた場合
      *                                  全国公費(新)の分が記録されないように修正
      *  01.00.18    吉川    2014/10/06  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.19    吉川    2015/01/06  平成２７年１月改正対応
      *  01.00.20    吉川    2015/01/29  難病＋高額の場合、備考欄に
      *                                  高額区分が印字されないのを修正
      *  01.00.21    吉川    2015/03/16  標準負担額欄は福祉に係る金額を記載
      *  01.00.22    門間    2015/07/07  長期及び長期上位の判別処理を修正
      *  01.00.23    吉川    2016/03/10  限度額認定証なしで低所得の場合
      *                                  請求額を35400円で止めるように修正
      *  01.00.24    萩野    2016/06/08  対象公費はPGオプションから取得する
      *  01.00.25    吉川    2016/07/13  難病の場合、生活療養費負担額は
      *                                  標準負担額欄の集計対象外とする
      *                                  低所得の設定が地方公費のみ対象の場合
      *                                  低所の限度額とならないのを修正
      *  01.00.26    吉川    2016/08/12  多数と高額の両方に該当する場合
      *                                  備考が「01」で記録されるのを修正
      *                                  福祉医療費請求額の端数処理修正
      *  01.00.27    吉川    2016/09/05  福祉医療費請求額の修正
      *  01.00.28    萩野    2016/09/09  公費の助成がない場合は印字対象外とする
      *  01.00.29    吉川    2016/09/21  分点時の福祉医療費請求額修正
      *                                  WRK-SEIKYUGK2の初期化漏れ
      *  01.00.30    吉川    2016/10/13  保険＋全国公費＋福祉と保険＋福祉の
      *                                  場合10円単位で記載されるのを修正
      *  01.00.31    門間    2017/02/06  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.32    吉川    2017/03/10  高額４回目で月途中の市町村変更の場合
      *                                  福祉医療費請求額はそれぞれの市町村で
      *                                  案分計算をするように修正
      *  01.00.33    吉川    2018/02/09  上限額の取得処理の修正
      *  01.00.34    吉川    2018/08/07  平成３０年８月改正対応
      *  01.00.35    門間    2018/10/02  不要コメントを削除
      *                                  福祉医療費請求額の修正
      *  01.00.36    吉川    2019/01/11  全国公費＋福祉＋高額の場合
      *                                  福祉医療費請求額が印字されないのを修正
      *  01.00.37    萩野    2019/10/09  返戻、月遅れの場合、37番目項目の
      *                                  福祉認定日が記録されないのを修正
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *    群馬保険番号データ
           SELECT  TOWN-FILE     ASSIGN FILE-NAME
                                 ORGANIZATION   IS  LINE SEQUENTIAL
                                 ACCESS MODE    IS  SEQUENTIAL
                                 FILE   STATUS  IS  STS-TOWN.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI1013.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
      *    群馬保険番号データ
       FD  TOWN-FILE.
           COPY  "TOWN1002.INC".
      *
       WORKING-STORAGE  SECTION.
      *    シェル用領域
           COPY  "CPSHELLTBL.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    群馬保険番号データ
           COPY  "TOWN1001.INC".
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100                  PIC X(02).
           03  STS-RECEERR                PIC X(02).
           03  STS-RECE                   PIC X(02).
           03  STS-TOWN                   PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                    PIC 9(01).
           03  FLG-MF100                  PIC 9(01).
           03  FLG-PRINT                  PIC 9(01).
           03  FLG-ERR                    PIC 9(01).
           03  FLG-PTINF                  PIC 9(01).
           03  FLG-SYSKANRI               PIC 9(01).
           03  FLG-SYUNOU                 PIC 9(01).
           03  FLG-RECE10                 PIC 9(01).
           03  FLG-PTKOHINF               PIC 9(01).
           03  FLG-HKNCOMBI               PIC 9(01).
           03  FLG-TSYRRK                 PIC 9(01).
           03  FLG-TNKRRK                 PIC 9(01).
           03  FLG-HKNJAINF               PIC 9(01).
           03  FLG-BTPARA                 PIC 9(01).
           03  FLG-SYORI                  PIC 9(01).
           03  FLG-MOJI                   PIC 9(01).
           03  FLG-TOWN                   PIC 9(01).
           03  FLG-HEIYOU                 PIC 9(01).
           03  FLG-TYOKI                  PIC 9(01).
           03  FLG-PTKOHFTN               PIC 9(01).
           03  FLG-KOH091                 PIC 9(01).
           03  FLG-KOGAKU                 PIC 9(01).
           03  FLG-KOHOK                  PIC 9(01).
           03  FLG-NINTEI                 PIC 9(01).
           03  FLG-KOHSKJ                 PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
           03  FLG-ZENKOHI    PIC 9(01).
           03  FLG-SYUNOU2    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                        PIC 9(04).
           03  IDXX                       PIC 9(04).
           03  IDX1                       PIC 9(04).
           03  IDX2                       PIC 9(04).
           03  IDX3                       PIC 9(04).
           03  IDY                        PIC 9(04).
           03  IDZ                        PIC 9(04).
           03  IDW                        PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID             PIC 9(07).
           03  WRK-PARA-SHELLID           PIC X(08).
           03  WRK-PARA-HOSPNUM           PIC 9(02).
           03  WRK-PARA-PAGE              PIC 9(10).
           03  WRK-PARA-SYORIFLG          PIC X(01).
           03  WRK-PARA-STSRYYM           PIC 9(06).
           03  WRK-PARA-EDSRYYM           PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR                PIC  X(200).
           03  WRK-Z1                     PIC Z.
           03  WRK-Z2                     PIC ZZ.
           03  WRK-Z3                     PIC ZZZ.
           03  WRK-Z4                     PIC ZZZ9.
           03  WRK-Z5                     PIC Z,ZZZ.
           03  WRK-Z6                     PIC ZZ,ZZZ.
           03  WRK-Z7                     PIC ZZZ,ZZZ.
           03  WRK-Z8                     PIC ZZZZZZZZ.
           03  WRK-Z9                     PIC Z,ZZZ,ZZZ.
           03  WRK-9                      PIC 9(10).
           03  WRK-SRYYMWH                PIC X(16).
           03  WRK-SEIYMDWH               PIC X(22).
           03  WRK-HENYMDG                PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY                PIC 9(04).
               05  WRK-SMM                PIC 9(02).
               05  WRK-SDD                PIC 9(02).
           03  WRK-LASTYMD                PIC 9(08).
           03  WRK-NYMD.
               05  WRK-NYY                PIC 9(04).
               05  WRK-NMM                PIC 9(02).
               05  WRK-NDD                PIC 9(02).
           03  WRK-TYMD.
               05  WRK-TYY                PIC 9(04).
               05  WRK-TMM                PIC 9(02).
               05  WRK-TDD                PIC 9(02).
           03  WRK-SKYYM                  PIC 9(05).
           03  WRK-KOHTEKSTYMD            PIC 9(08).
           03  WRK-KASAN-TBL.
               05   WRK-SEIKYUGK-G        PIC 9(08)V9(01).
               05   WRK-SEIKYUGK-G2       PIC 9(08).
               05   WRK-SEIKYUGK          PIC 9(08)V9(01).
               05   WRK-SEIKYUGK2-G       PIC 9(08)V9(01).
               05   WRK-SEIKYUGK2-G2      PIC 9(08).
               05   WRK-SEIKYUGK2         PIC 9(08)V9(01).
               05   WRK-KOH-SKJFTN        PIC 9(08).
               05   WRK-KOH-SKJFTN-G      PIC 9(08).
           03  WRK-HKNJANUM               PIC X(06).
           03  WRK-HKNJANAME              PIC X(20).
           03  WRK-PRTID                  PIC X(20).
           03  WRK-KOHNUM                 PIC X(03).
           03  WRK-KOBETUCITYNUM          PIC X(03).
           03  WRK-SYUBETU                PIC 9(01).
           03  WRK-TOKURAN                PIC X(02).
           03  WRK-BIKO                   PIC X(02).
           03  WRK-KOHINF  OCCURS 4.
               05  WRK-H-KOHNUM           PIC X(03).
           03  WRK-JGNGAK                 PIC 9(08).
           03  WRK-FUKUSHI                PIC 9(08).
           03  WRK-KOHFTNMONEY            PIC 9(08).
           03  WRK-GAIJGNGAK              PIC 9(08).
           03  WRK-NYUJGNGAK              PIC 9(08).
      *
      *    負担者番号ゼロ埋め変換
           03  WRK-FTNJAHENKAN.
               05  WRK-FTNJANUM-MAE       PIC X(08).
               05  WRK-FTNJANUM-ATO       PIC X(08).
               05  WRK-HANI               PIC 9(03).
               05  WRK-ICHI               PIC 9(03).
               05  WRK-HENKANFLG          PIC 9(01).
      *
      *    文字変換
           03  WRK-MAE-INPUT              PIC X(200).
           03  WRK-OUT-INPUT              PIC X(200).
      *
           03  WRK-PATH                   PIC  X(50).
           03  WRK-PATH2                  PIC  X(50).
           03  WRK-FILENAME               PIC  X(40).
      *    データ編集
           03  WRK-HENSHU.
               05  WRK-SOUGOKEIKENSU      PIC 9(07).
      *
               05  WRK-RECENO             PIC 9(05).
               05  WRK-RECEHKNSHU         PIC 9(01).
               05  WRK-TOKKI              PIC X(02).
               05  WRK-TOWN               PIC 9(03).
               05  WRK-CHK-SRYYM          PIC 9(06).
               05  WRK-HKNJACVS           PIC 9(08).
               05  WRK-MOJIST             PIC 9(03).
               05  WRK-MOJIED             PIC 9(03).
               05  WRK-RATE               PIC 9(02).
               05  WRK-MOJIHEN            PIC 9(10).
      *
               05  WRK-JNISSU             PIC X(03).
               05  WRK-EJNISSU1           PIC X(03).
               05  WRK-EJNISSU2           PIC X(03).
               05  WRK-TOTALTEN           PIC X(08).
               05  WRK-ETOTALTEN1         PIC X(08).
               05  WRK-ETOTALTEN2         PIC X(08).
               05  WRK-FTNMONEY           PIC X(08).
               05  WRK-EITBFTN1           PIC X(08).
               05  WRK-EITBFTN2           PIC X(08).
               05  WRK-SHOKUJINISSU       PIC X(03).
               05  WRK-SHOKUJIRYOYOHI     PIC X(08).
               05  WRK-SHOKUJIFTN         PIC X(08).
      *
      *    全国公費用データワーク
           03  WRK-KOHID                  PIC 9(10).
           03  WRK-FTNMONEY-G             PIC 9(08).
           03  WRK-ETC-TBL.
               05  WRK-ETC-KOHNUM         PIC X(03) OCCURS 2.
               05  WRK-ETC-TENSU          PIC 9(07) OCCURS 2.
               05  WRK-ETC-ITBFTN         PIC 9(07) OCCURS 2.
               05  WRK-ETC-FTNJANUM       PIC X(20).
               05  WRK-ETC-JKYSNUM        PIC X(20).
               05  WRK-ETC-SKJ-NISSU      PIC 9(02) OCCURS 2.
               05  WRK-ETC-SKJ-RYOYOHI    PIC 9(07) OCCURS 2.
               05  WRK-ETC-SKJ-FTN        PIC 9(07) OCCURS 2.
      *
           03  WRK-REC                    PIC X(500).
           03  WRK-REC-LENGTH             PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA               PIC X(100).
               05  WRK-NUM                PIC ZZZZZZZZZZ.999.
               05  WRK-NUM9               PIC ZZZZZZZZZ9.999.
               05  WRK-NAGASA             PIC 9(04).
               05  WRK-ST                 PIC 9(02).
               05  WRK-SYOSUU             PIC 9(01).
               05  WRK-END                PIC 9(01).
      *
           03  WRK-SCSVINFO-SEQ-NO        PIC 9(06).
           03  WRK-SCSVINFO-PTID          PIC 9(10).
           03  WRK-SCSVINFO-NYUGAIKBN     PIC X(01).
      *
      *    パラメータファイル名
           03  FILE-NAME                  PIC X(512).
      *
      *    医療機関情報
           03  WRK-HOSPNAME               PIC X(100).
           03  WRK-HOSPNAME-R        REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1          PIC X(50).
               05  WRK-HOSPNAME2          PIC X(50).
           03  WRK-HOSPADRS               PIC X(100).
           03  WRK-HOSPADRS-R        REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1          PIC X(50).
               05  WRK-HOSPADRS2          PIC X(50).
           03  WRK-HOSPTEL                PIC X(15).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
      *
      *    ファイル名
       01  DATA-FILE.
           03  FILLER       PIC X(05)  VALUE  "data/".
           03  FILLER       PIC X(12)  VALUE  "TOWN1001.TXT".
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02                 PIC 9(06).
           03  CNT-MF100                  PIC 9(06).
           03  CNT-PAGE                   PIC 9(03).
           03  CNT-OUT                    PIC 9(06).
      *
      *    集計用エリア
       01  SUM-AREA  VALUE   ZERO.
      *    合計
           03  SUM-GOKEI-TBL.
             05  SUM-GKENSU               PIC  9(06).
             05  SUM-GTENSU               PIC  9(09).
             05  SUM-GSEIKYUGK            PIC  9(09).
             05  SUM-GSKENSU              PIC  9(09).
             05  SUM-GSFTNGK              PIC  9(09).
      *
      *    テーブルエリア
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
      *    平成２０年４月改正
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
      *
      *    レセプト印刷情報
           COPY    "CPRCF008.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
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
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    低所得情報
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    低年金情報
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納情報
       01  SYUNOU2-REC.
           COPY  "CPSYUNOU.INC"
                 REPLACING  //SYU//
                 BY         //SYU2//.
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    患者公費負担
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
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
      *    チェックディジット（モジュラス１１用）算出サブ
           COPY    "CPORCCHKDGT11.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＣＳＶ管理ＤＢ制御サブ
           COPY    "CPORCSCSVINFO.INC".
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
           INITIALIZE  IDX-AREA
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
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU1013"    TO  JOB-PGID
           MOVE    "福祉医療費連記式ＣＳＶ処理"
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
           MOVE  WRK-PARA-HOSPNUM     TO  MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF100PARA-TERMID
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF100PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"         TO  SYS-1001-KANRICD
           MOVE  "*"            TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"           TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC   TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU1013 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"         TO  SYS-1002-KANRICD
           MOVE  "*"            TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"           TO  SYS-1002-STYUKYMD(7:2)
                                    SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM    TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC   TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU1013 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    市町村情報
           PERFORM 110-TOWNINF-GET-SEC
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
           MOVE    "SEIKYU1013"              TO  WRK-PRGID
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
      *    公費保険番号データ読込
      *****************************************************************
       110-TOWNINF-GET-SEC  SECTION.
      *
      *    ファイルディレクトリ位置指定
           INITIALIZE           ORCSMKPASSAREA
           MOVE  DATA-FILE     TO  MKPASS-IN-01
           CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
           MOVE  MKPASS-OT-01  TO  FILE-NAME
      *
           OPEN  INPUT  TOWN-FILE
      *
           MOVE  ZERO   TO  FLG-TOWN
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL FLG-TOWN = 1
             READ TOWN-FILE NEXT
               AT END
                 MOVE  1         TO  FLG-TOWN
               NOT AT END
                 MOVE  TOWN-REC  TO  TOWN-TBL(IDX)
             END-READ
           END-PERFORM
      *
           CLOSE TOWN-FILE
           .
       110-TOWNINF-GET-EXT.
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
           OPEN     OUTPUT  MF100-FILE
           CLOSE            MF100-FILE
           OPEN     I-O     MF100-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF100-FILE
      *
           IF       CNT-RECE02   >       0
      *       ＣＳＶファイル出力
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
           PERFORM  900-KOHSKY-READ-SEC
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
           INITIALIZE       MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-SRYKA(1)     TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
           MOVE  RECE02-TEISYUTUSAKI TO  MF100-TEISYUTUSAKI
           MOVE  RECE02-RECESYUBETU  TO  MF100-RECESYUBETU
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
      *
      *    公費情報
           MOVE  RECE02-KOHFTNJANUM (IDZ)  TO  MF100-FTNJANUM
           MOVE  RECE02-KOHJKYSNUM  (IDZ)  TO  MF100-JKYSNUM
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF

           COMPUTE IDY  =  IDZ  +  1
           MOVE  RECE02-JNISSU      (IDY)  TO  MF100-JNISSU
           MOVE  RECE02-TOTALTEN    (IDY)  TO  MF100-TOTALTEN
           MOVE  RECE02-YKZFTN      (IDY)  TO  MF100-YKZFTN
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
           MOVE  RECE02-FTNMONEY    (IDY)  TO  MF100-FTNMONEY
      *    公費適用開始日取得
           MOVE  RECE02-KOHNUM(IDZ)        TO  WRK-KOHNUM
           PERFORM 930-PTKOHINF-SET-SEC
           MOVE  WRK-KOHTEKSTYMD           TO  MF100-KOHTEKSTYMD
           MOVE  SPACE                     TO  WRK-ETC-FTNJANUM
           MOVE  SPACE                     TO  WRK-ETC-JKYSNUM
      *
           DISPLAY "PTNUM =" MF100-PTNUM
      *    特欄初期化
           MOVE  SPACE                     TO  WRK-TOKURAN
      *
      *    高額療養費
      *    特疾＋957等も高額認定されてしまうので、FLG4を判断して回避
           IF  RECE02-FLG(4)  =  ZERO
             IF  RECE02-ZAITAKU  =  "2"  OR  "3"  OR "4"
                                OR  "5"  OR  "6"
               MOVE  "99"                TO  WRK-TOKURAN
      *        ４回目以降
               IF  RECE02-FTNMONEY (1)  =  24600
               OR  RECE02-FTNMONEY (1)  =  44400
               OR  RECE02-FTNMONEY (1)  =  93000
               OR  RECE02-FTNMONEY (1)  =  140100
                 MOVE  1                 TO  MF100-KOGAKU4
               ELSE
                 EVALUATE  RECE02-ZAITAKU
                   WHEN  "2"
                     IF  RECE02-FTNMONEY(1)  <  140100
                       MOVE  SPACE       TO  WRK-TOKURAN
                     END-IF
                   WHEN  "3"
                     IF  RECE02-FTNMONEY(1)  <  93000
                       MOVE  SPACE       TO  WRK-TOKURAN
                     END-IF
                   WHEN  "4"
                   WHEN  "5"
                     IF  RECE02-FTNMONEY(1)  <  44400
                       MOVE  SPACE       TO  WRK-TOKURAN
                     END-IF
                   WHEN  "6"
                     IF  RECE02-FTNMONEY(1)  <  24600
                       MOVE  SPACE       TO  WRK-TOKURAN
                     END-IF
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
           IF  RECE02-ZAITAKU  =  "1"
      *      前期高齢者1割で障害146の場合は在宅としない
             IF  (MF100-KOHNUM  =  "146")
             AND (RECE02-AGE  >=  70)
             AND (RECE02-KYURATE  NOT =  30)
               CONTINUE
             ELSE
               MOVE  "40"                  TO  WRK-TOKURAN
             END-IF
           END-IF
      *
      *    請求管理より、保険使用時情報取得
           PERFORM  900-SEIKYU-READ-SEC
           MOVE  RECE10-JNISSU(1)          TO  MF100-H-JNISSU
           MOVE  RECE10-FTNMONEY(1)        TO  MF100-H-FTNMONEY
           MOVE  RECE10-TOTALTEN(1)        TO  MF100-H-TOTALTEN
           MOVE  RECE10-SHOKUJIINF(1)      TO  MF100-H-SHOKUJIINF
      *    請求管理より、保険使用時情報取得
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  4
             MOVE  RECE10-KOHINF(IDX)      TO  WRK-KOHINF(IDX)
           END-PERFORM
      *
      *    請求管理情報より、他公費情報取得
           IF   IDZ  =  1
           AND  RECE02-KOHNUM(IDZ + 1)  =  SPACE
      *
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     2  OR
                      RECE10-KOHNUM(IDX)  =  SPACE
      *
      *       月途中に追加した公費と福祉と絡んでいるかどうか判定
              PERFORM 200117-KOHI-CHK-SEC
      *
              IF  FLG-KOHOK  =  1
               MOVE  RECE10-KOHNUM(IDX)         TO  MF100-EKOHNUM(IDX)
               MOVE  RECE10-JNISSU(IDX + 1)     TO  MF100-ENISSU(IDX)
               MOVE  RECE10-TOTALTEN(IDX + 1)   TO  MF100-ETENSU(IDX)
               MOVE  RECE10-FTNMONEY(IDX + 1)   TO  MF100-EITBFTN(IDX)
      *        食事日数
               MOVE  RECE10-SHOKUJINISSU(IDX + 1)
                                   TO  MF100-E-SHOKUJINISSU(IDX)
      *        食事療養日
               MOVE  RECE10-SHOKUJIRYOYOHI(IDX + 1)
                                   TO  MF100-E-SHOKUJIRYOYOHI(IDX)
      *        食事標準負担額
               MOVE  RECE10-SHOKUJIFTN(IDX + 1)
                                   TO  MF100-E-SHOKUJIFTN(IDX)
      *
      *        他公費の負担者番号・受給者番号取得
               MOVE    RECE10-KOHNUM(IDX)  TO  WRK-KOHNUM
               PERFORM 930-PTKOHINF-SET-SEC
               MOVE    WRK-ETC-FTNJANUM    TO  MF100-EKOHFTNJANUM(IDX)
               MOVE    WRK-ETC-JKYSNUM     TO  MF100-EKOHJKYSNUM(IDX)
              END-IF
             END-PERFORM
           END-IF
      *
      *    助成判定及び、月途中で公費を切替えた時の全国公費分のデータ集計
           MOVE  MF100-KOHNUM              TO  WRK-KOHNUM
           MOVE  MF100-KOHID               TO  WRK-KOHID
           MOVE  ZERO                      TO  WRK-SEIKYUGK-G
                                               FLG-TYOKI
                                               FLG-KOH091
                                               FLG-KOGAKU
                                               FLG-KOHSKJ
                                               WRK-KOH-SKJFTN-G
                                               FLG-ZENKOHI
                                               WRK-JGNGAK
                                               WRK-SEIKYUGK2-G
      *
           IF  RECE02-NYUGAIKBN  =  "1"
             MOVE  "key33"                 TO  WRK-PATH
           ELSE
             MOVE  "key25"                 TO  WRK-PATH
           END-IF
      *    収納情報取得
           MOVE    1                  TO  FLG-ERR
           PERFORM  920-SYUNOU-SET-SEC
      *
           IF   IDZ  =  1
           AND  RECE02-KOHNUM(IDZ + 1)  =  SPACE
             DISPLAY "請求管理から公費情報取得済み" RECE02-PTNUM
           ELSE
             DISPLAY  "患者番号=" RECE02-PTNUM
                      "その他公費=" WRK-ETC-KOHNUM(1)
             IF  WRK-ETC-KOHNUM(1)  NOT =  SPACE
               DISPLAY "その他公費番号=" WRK-ETC-KOHNUM(1)
                       ","               WRK-ETC-KOHNUM(2)
               IF  WRK-ETC-KOHNUM(2)  =  SPACE
               OR  WRK-ETC-KOHNUM(2)  >  WRK-ETC-KOHNUM(1)
                 MOVE  1                   TO  IDX
                 MOVE  2                   TO  IDY
               ELSE
                 MOVE  2                   TO  IDX
                 MOVE  1                   TO  IDY
               END-IF
      *        第一公費処理
               MOVE  WRK-ETC-KOHNUM(1)     TO  WRK-KOHNUM
               PERFORM 930-PTKOHINF-SET-SEC
               MOVE  WRK-ETC-TENSU(1)      TO  MF100-ETENSU(IDX)
               MOVE  WRK-ETC-ITBFTN(1)     TO  MF100-EITBFTN(IDX)
               MOVE  WRK-ETC-FTNJANUM      TO  MF100-EKOHFTNJANUM(IDX)
               MOVE  WRK-ETC-JKYSNUM       TO  MF100-EKOHJKYSNUM(IDX)
      *        食事
               MOVE  WRK-ETC-SKJ-NISSU(1)
                                       TO  MF100-E-SHOKUJINISSU(IDX)
               MOVE  WRK-ETC-SKJ-RYOYOHI(1)
                                       TO  MF100-E-SHOKUJIRYOYOHI(IDX)
               MOVE  WRK-ETC-SKJ-FTN(1)
                                       TO  MF100-E-SHOKUJIFTN(IDX)
      *
      *        第二公費
               IF  WRK-ETC-KOHNUM(2)  NOT =  SPACE
                 MOVE  WRK-ETC-KOHNUM(2)   TO  WRK-KOHNUM
                 PERFORM 930-PTKOHINF-SET-SEC
                 MOVE  WRK-ETC-TENSU(2)    TO  MF100-ETENSU(IDY)
                 MOVE  WRK-ETC-ITBFTN(2)   TO  MF100-EITBFTN(IDY)
                 MOVE  WRK-ETC-FTNJANUM    TO  MF100-EKOHFTNJANUM(IDY)
                 MOVE  WRK-ETC-JKYSNUM     TO  MF100-EKOHJKYSNUM(IDY)
      *          食事
                 MOVE  WRK-ETC-SKJ-NISSU(2)
                                       TO  MF100-E-SHOKUJINISSU(IDY)
                 MOVE  WRK-ETC-SKJ-RYOYOHI(2)
                                       TO  MF100-E-SHOKUJIRYOYOHI(IDY)
                 MOVE  WRK-ETC-SKJ-FTN(2)
                                       TO  MF100-E-SHOKUJIFTN(IDY)
               END-IF
             END-IF
           END-IF
      *
      *    収納で取得できなかった全国公費を請求管理から取得
           IF    WRK-TOKURAN       =     SPACE
              PERFORM  VARYING  IDX1  FROM  1  BY  1
                       UNTIL    IDX1  >     4
                IF  WRK-H-KOHNUM(IDX1)  =  "010" OR "011" OR "020" OR
                                           "021" OR "015" OR "016" OR
                                           "051" OR "091" OR "052"
                                        OR "024" OR "038" OR "079"
                                        OR "054"
      *
                  MOVE  1                        TO  FLG-ZENKOHI
                  MOVE  WRK-H-KOHNUM(IDX1)(2:2)  TO  WRK-TOKURAN
      *
                  IF  WRK-H-KOHNUM(IDX1)  =  "091"
                    MOVE  "51"                   TO  WRK-TOKURAN
                    MOVE   1                     TO  FLG-KOH091
                  END-IF
      *
                END-IF
              END-PERFORM
           END-IF
      *
           DISPLAY "福祉MF100-TOTALTEN = " MF100-TOTALTEN
           DISPLAY "公費MF100-TOTALTEN2 = " MF100-TOTALTEN2
           DISPLAY "WRK-TOKURAN =" WRK-TOKURAN
      *    総点数と公費点数が同じ場合は公費点数は印字しない
           IF  MF100-TOTALTEN         =     MF100-TOTALTEN2
           OR  MF100-TOTALTEN2        =     ZERO
             CONTINUE
           ELSE
             DISPLAY "第一公費点数 = " MF100-ETENSU(1)
           END-IF
      *
      *    老人＋在宅等で保険に限度額が有る場合の対応
           IF     RECE02-FTNMONEY(1)  >   ZERO     AND
                  RECE02-FTNMONEY(1)  <   WRK-SEIKYUGK-G
             MOVE    RECE02-FTNMONEY(1)   TO  WRK-SEIKYUGK-G
           END-IF
      *
      *    長期
           IF    FLG-TYOKI    =    1
             IF  WRK-SEIKYUGK-G    >=  10000
               MOVE    10000          TO    WRK-SEIKYUGK-G
             END-IF
             MOVE    1                TO    MF100-TYOKI
           END-IF
      *    長期上位
           IF    FLG-TYOKI    =    2
             IF  WRK-SEIKYUGK-G    >=  20000
               MOVE    20000          TO    WRK-SEIKYUGK-G
             END-IF
             MOVE    2                TO    MF100-TYOKI
           END-IF
      *
      *    四捨五入
           COMPUTE  WRK-SEIKYUGK-G2 ROUNDED  =  WRK-SEIKYUGK-G
           MOVE     WRK-SEIKYUGK-G2    TO    MF100-SEIKYUGK
      *
      *    社保上位所得者、４回目以降の負担金額計算
           IF  RECE02-TEISYUTUSAKI  =  1
           AND ( RECE02-ZAITAKU  =  "2" OR "3" )
               IF  MF100-SEIKYUGK  =  140100 OR 93000
      *
                   COMPUTE  MF100-SEIKYUGK  ROUNDED =  80100 +
                            ( RECE10-TOTALTEN(1) * 10 - 267000 ) / 100
               END-IF
           END-IF
      *
      *    全国公費を持っている場合、
      *    請求額が上限額を超えているかどうかの確認
           IF  FLG-ZENKOHI  =  1
      *        上限額の取得
               PERFORM  930-PTKOHFTN-READ-SEC
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >  4
                        OR       WRK-KOHINF(IDX2)  =  SPACE
                   EVALUATE  WRK-KOHINF(IDX2)
                     WHEN  "015"
                     WHEN  "016"
                     WHEN  "021"
                     WHEN  "038"
                     WHEN  "052"
                     WHEN  "054"
                       COMPUTE  WRK-JGNGAK  =  WRK-GAIJGNGAK
                                            -  WRK-NYUJGNGAK
                     WHEN  "024"
                     WHEN  "051"
                     WHEN  "079"
                       MOVE    WRK-GAIJGNGAK   TO  WRK-JGNGAK
                     WHEN  OTHER
                       MOVE    ZERO            TO  WRK-JGNGAK
                   END-EVALUATE
               END-PERFORM
      *
      *        収納から全国公費の負担金額を取得
               IF  RECE02-NYUGAIKBN  =  "1"
                   MOVE    "key17"             TO  WRK-PATH2
               ELSE
                   MOVE    "key14"             TO  WRK-PATH2
               END-IF
               PERFORM  940-SYUNOU2-READ-SEC
      *
      *        公費負担金額が全国公費の上限額を超えたら１０円単位
               DISPLAY "WRK-JGNGAK  =" WRK-JGNGAK
               DISPLAY "WRK-KOHFTNMONEY  =" WRK-KOHFTNMONEY
               IF  (WRK-KOHFTNMONEY  >=  WRK-JGNGAK)
               AND (WRK-JGNGAK  NOT =  ZERO)
                   MOVE    WRK-SEIKYUGK2-G     TO  MF100-SEIKYUGK
               END-IF
           END-IF
      *
      *    市町村番号セット処理
           IF  MF100-FTNJANUM(3:1)  NOT =  SPACE
      *      負担者番号が8桁の場合（新）
             MOVE  MF100-FTNJANUM(3:5)    TO  MF100-CITYNUM2
           ELSE
      *      負担者番号が2桁の場合（旧）
             MOVE  MF100-FTNJANUM         TO  MF100-CITYNUM
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX   >    100
               IF  MF100-CITYNUM   =  TOWN-NUM(IDX)
                 MOVE  TOWN-CITYNUM(IDX)(1:5)  TO  MF100-CITYNUM2
                 MOVE  100                TO  IDX
               END-IF
             END-PERFORM
           END-IF
      *
      *    特欄コード
           MOVE    WRK-TOKURAN        TO  MF100-TOKURAN
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
      *    経過措置チェック
           PERFORM 200112-KEIKA-CHK-SEC
      *
      *    低所得チェック
           PERFORM 200113-TEISHO-CHK-SEC
      *
      *    低年金チェック
           IF  MF100-TEISHO  =  ZERO
               PERFORM 200114-TEINEN-CHK-SEC
           END-IF
      *
      *    割合区分設定
           PERFORM 200115-WARIAI-SET-SEC
      *
      *    高額該当者で限度額認定証無しの請求額計算
           IF  FLG-KOGAKU  =  1
             IF  MF100-SEIKYUGK  =  140100
             OR  MF100-SEIKYUGK  =  93000
             OR  MF100-SEIKYUGK  =  44400
             OR  MF100-SEIKYUGK  =  24600
               MOVE    1               TO  MF100-KOGAKU4
             ELSE
               IF  MF100-TEISHO  =  ZERO
                   IF  MF100-SEIKYUGK  >  57600
                       MOVE    57600       TO  MF100-SEIKYUGK
                   END-IF
               ELSE
                   IF  MF100-SEIKYUGK  >  35400
                       MOVE    35400       TO  MF100-SEIKYUGK
                   END-IF
               END-IF
             END-IF
           END-IF
      *
      *    ７０歳２割で低所の高額療養費現物給付該当者
           IF  (RECE02-AGE  >=  70)
           AND (RECE02-KYURATE  =  20)
           AND (RECE02-FTNMONEY(1)  NOT =  ZERO)
               EVALUATE  RECE02-SHOKUJIFTNKBN
                 WHEN  "1"
                 WHEN  "2"
      *            低所得２
                   MOVE    2           TO  MF100-GENBUTU
                 WHEN  "3"
      *            低所得１
                   MOVE    1           TO  MF100-GENBUTU
               END-EVALUATE
           END-IF
      *
      *    高額療養費
           IF  ((RECE02-ZAITAKU  NOT =  SPACE)
                AND (RECE02-ZAITAKU  NOT =  "1"))
           OR  (RECE02-AGE  >=  70)
      *        限度額認定証所持チェック
               PERFORM 200118-KOGAKU-CHK-SEC
               IF  FLG-NINTEI  =  1
                    MOVE    RECE02-ZAITAKU  TO  MF100-KOGAKU
               END-IF
           END-IF
      *
      *    備考欄コード設定
           PERFORM 200116-BIKO-SET-SEC
      *
      *    高半チェック
           IF  RECE10-TOKKI(1)  =  "21"
           OR  RECE10-TOKKI(2)  =  "21"
           OR  RECE10-TOKKI(3)  =  "21"
           OR  RECE10-TOKKI(4)  =  "21"
           OR  RECE10-TOKKI(5)  =  "21"
               MOVE  1                 TO  MF100-KOUHAN
           END-IF
      *
      *    食事負担
           IF  FLG-KOHSKJ  =  1
               MOVE    WRK-KOH-SKJFTN-G     TO  MF100-SHOKUJIFTN
           END-IF
      *
      *    低所の設定が「地方公費のみ」になっている場合
      *    収納の計算は普段通りになっているので限度額を低所にする
           IF  (RECE02-ZAITAKU = "2" OR "3" OR "4" OR "5" OR "6")
           AND (MF100-TEISHO  NOT =  ZERO)
               IF  MF100-KOGAKU4  NOT =  ZERO
                   IF  MF100-SEIKYUGK  >  24600
                       MOVE    24600         TO  MF100-SEIKYUGK
                   END-IF
               ELSE
                   IF  MF100-SEIKYUGK  >  35400
                       MOVE    35400         TO  MF100-SEIKYUGK
                   END-IF
               END-IF
           END-IF
      *
      *    高額4回目で月途中に市町村を変更した場合は案分計算
      *    （本体で自動で案分計算されるようになったらコメントアウトする）
           IF  (RECE02-KOHNUM(1)           =  RECE02-KOHNUM(2))
           AND (RECE02-KOHFTNJANUM(1)  NOT =  RECE02-KOHFTNJANUM(2))
           AND (WRK-TOKURAN                =  "99")
      *        44400円で入っているところは、
      *        FTNMONEY(1)から他市町村分を割り出してそれと差し引く
               IF  MF100-SEIKYUGK  =  44400
                   IF  RECE02-FTNMONEY(1)  NOT =  88800
                       COMPUTE  MF100-SEIKYUGK  =
                                 44400 - (RECE02-FTNMONEY(1) - 44400)
                   ELSE
                       IF  IDZ  =  1
                           MOVE    ZERO     TO  MF100-SEIKYUGK
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF  FLG-ERR    =    ZERO
               WRITE  MF100-REC
           ELSE
               DISPLAY "助成対象外患者番号="  RECE02-PTNUM
           END-IF
      *
           ADD   1   TO   WRK-SOUGOKEIKENSU
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
           INITIALIZE                                 WRK-HKNJANUM
           MOVE    MF100-CITYNUM2                 TO  WRK-HKNJANUM
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  MF100-PTNUM
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
      *    経過措置チェック処理
      *****************************************************************
       200112-KEIKA-CHK-SEC                SECTION.
      *
           INITIALIZE                      PTKOHINF-REC
                                           FLG-PTKOHINF
           MOVE    SPA-HOSPNUM             TO  PTKOH-HOSPNUM
           MOVE    RECE02-PTID             TO  PTKOH-PTID
           MOVE    PTKOHINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC   =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
      *
               PERFORM  UNTIL  FLG-PTKOHINF  =  1
                 IF  RECE02-SRYYM  >=  PTKOH-TEKSTYMD(1:6)  AND
                     RECE02-SRYYM  <=  PTKOH-TEKEDYMD(1:6)
                   IF  PTKOH-KOHNUM  =  "978"  OR  "979"
                     MOVE  1  TO  MF100-KEIKA
                     MOVE  1  TO  FLG-PTKOHINF
                   END-IF
                 END-IF
      *
                 MOVE    "DBFETCH"               TO  MCP-FUNC
                 MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
                 IF      MCP-RC   =   ZERO
                     MOVE    ZERO                TO  FLG-PTKOHINF
                     MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                 ELSE
                     MOVE    1                   TO  FLG-PTKOHINF
                     INITIALIZE                  PTKOHINF-REC
                 END-IF
      *
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       200112-KEIKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得チェック処理
      *****************************************************************
       200113-TEISHO-CHK-SEC                SECTION.
      *
           INITIALIZE                      TSYRRK-REC
                                           FLG-TSYRRK
           MOVE    SPA-HOSPNUM             TO  TSYRRK-HOSPNUM
           MOVE    RECE02-PTID             TO  TSYRRK-PTID
           MOVE    TSYRRK-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      FLG-TSYRRK          =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC   =   ZERO
                   MOVE    ZERO                TO  FLG-TSYRRK
                   MOVE    MCPDATA-REC         TO  TSYRRK-REC
               ELSE
                   MOVE    1                   TO  FLG-TSYRRK
                   INITIALIZE                  TSYRRK-REC
               END-IF
      *
               PERFORM  UNTIL  FLG-TSYRRK  =  1
                 IF  RECE02-SRYYM  >=  TSYRRK-NINSTYMD(1:6)  AND
                     RECE02-SRYYM  <=  TSYRRK-NINEDYMD(1:6)
                   MOVE  1    TO  MF100-TEISHO
                   MOVE  1    TO  FLG-TSYRRK
                 END-IF
      *
                 MOVE    "DBFETCH"               TO  MCP-FUNC
                 MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
                 IF      MCP-RC   =   ZERO
                     MOVE    ZERO                TO  FLG-TSYRRK
                     MOVE    MCPDATA-REC         TO  TSYRRK-REC
                 ELSE
                     MOVE    1                   TO  FLG-TSYRRK
                     INITIALIZE                  TSYRRK-REC
                 END-IF
      *
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       200113-TEISHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低年金チェック処理
      *****************************************************************
       200114-TEINEN-CHK-SEC                SECTION.
      *
           INITIALIZE                      TNKRRK-REC
                                           FLG-TNKRRK
           MOVE    SPA-HOSPNUM             TO  TNKRRK-HOSPNUM
           MOVE    RECE02-PTID             TO  TNKRRK-PTID
           MOVE    TNKRRK-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      FLG-TNKRRK          =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC   =   ZERO
                   MOVE    ZERO                TO  FLG-TNKRRK
                   MOVE    MCPDATA-REC         TO  TNKRRK-REC
               ELSE
                   MOVE    1                   TO  FLG-TNKRRK
                   INITIALIZE                  TNKRRK-REC
               END-IF
      *
               PERFORM  UNTIL  FLG-TNKRRK  =  1
                 IF  RECE02-SRYYM  >=  TNKRRK-STYMD(1:6)  AND
                     RECE02-SRYYM  <=  TNKRRK-EDYMD(1:6)
                   MOVE  1    TO  MF100-TEISHO
                   MOVE  1    TO  FLG-TNKRRK
                 END-IF
      *
                 MOVE    "DBFETCH"               TO  MCP-FUNC
                 MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
                 IF      MCP-RC   =   ZERO
                     MOVE    ZERO                TO  FLG-TNKRRK
                     MOVE    MCPDATA-REC         TO  TNKRRK-REC
                 ELSE
                     MOVE    1                   TO  FLG-TNKRRK
                     INITIALIZE                  TNKRRK-REC
                 END-IF
      *
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       200114-TEINEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    割合区分設定処理
      *****************************************************************
       200115-WARIAI-SET-SEC  SECTION.
      *
           IF  RECE02-RJNHKNNUM  =  "027"
      *      旧２７老人
             EVALUATE  RECE02-KYURATE
               WHEN  "010"
               WHEN  "020"
                 MOVE  5               TO  MF100-WARIAIKBN
               WHEN  "030"
                 MOVE  9               TO  MF100-WARIAIKBN
             END-EVALUATE
           ELSE
      *      ７０歳以上
             IF  RECE02-AGE  >=  70
               IF   RECE02-SRYYM   >=  WRK-CONS-SRYYM-200804
               AND  RECE02-FLG (2)  =  1
                 MOVE  5               TO  MF100-WARIAIKBN
               ELSE
                 IF  RECE02-KYURATE  =  "030"
                   MOVE  9             TO  MF100-WARIAIKBN
                 ELSE
                   MOVE  5             TO  MF100-WARIAIKBN
                 END-IF
               END-IF
      *      一般・未就学
             ELSE
               IF   RECE02-SRYYM   >=  WRK-CONS-SRYYM-200804
               AND  RECE02-FLG (1)  =  1
                 MOVE  6               TO  MF100-WARIAIKBN
               ELSE
                 IF  RECE02-AGE  <  3
                   MOVE  6             TO  MF100-WARIAIKBN
                 ELSE
                   IF  RECE02-KYURATE  =  "030"
                     MOVE  8           TO  MF100-WARIAIKBN
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       200115-WARIAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    備考欄コード設定処理
      *****************************************************************
       200116-BIKO-SET-SEC  SECTION.
      *
           IF  MF100-KOGAKU4  =  1
             EVALUATE  MF100-KOGAKU
               WHEN  "2"
                 MOVE    "20"          TO  MF100-BIKO
               WHEN  "3"
                 MOVE    "21"          TO  MF100-BIKO
               WHEN  "4"
                 MOVE    "22"          TO  MF100-BIKO
             END-EVALUATE
           ELSE
             EVALUATE  MF100-KOGAKU
               WHEN  "2"
                 MOVE    "10"          TO  MF100-BIKO
               WHEN  "3"
                 MOVE    "11"          TO  MF100-BIKO
               WHEN  "4"
                 MOVE    "12"          TO  MF100-BIKO
             END-EVALUATE
           END-IF
           IF  MF100-TYOKI  =  2
             MOVE  "07"                TO  MF100-BIKO
           END-IF
           IF  RECE02-SHOKUJIFTNKBN  =  "1" OR "2"
             MOVE  "06"                TO  MF100-BIKO
           END-IF
           IF   MF100-KOGAKU4  =  1
             IF  MF100-TEISHO  =  1
               MOVE  "03"              TO  MF100-BIKO
             END-IF
           END-IF
           IF  RECE02-SHOKUJIFTNKBN  =  "3"
             MOVE  "05"                TO  MF100-BIKO
           END-IF
           IF  MF100-TEISHO  =  1
             MOVE  "02"                TO  MF100-BIKO
           END-IF
      *     IF   MF100-KOGAKU4  =  1
           IF   (MF100-KOGAKU4  =  1)
           AND  (MF100-KOGAKU  NOT =  "2" AND "3" AND "4")
             IF  MF100-TEISHO  =  1
               MOVE  "03"              TO  MF100-BIKO
             ELSE
               MOVE  "01"              TO  MF100-BIKO
             END-IF
           END-IF
      *
           .
       200116-BIKO-SET-EXT.
           EXIT.
      *****************************************************************
      *    公費と福祉の組合せチェック処理
      *****************************************************************
       200117-KOHI-CHK-SEC  SECTION.
      *
           INITIALIZE                      FLG-KOHOK
      *
      *    保険組合せ号マスタ読込
           PERFORM 910-HKNCOMBI2-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-HKNCOMBI  =  1
      *      公費＋福祉の組合せ探索
             IF  MF100-KOHNUM  =  COMB-KOHHKNNUM(1)
                               OR COMB-KOHHKNNUM(2)
                               OR COMB-KOHHKNNUM(3)
                               OR COMB-KOHHKNNUM(4)
      *
      *        保険組合せが公費請求テーブルに存在していれば
      *        公費＋福祉の組合せを使用していることになる
               IF  COMB-HKNCOMBINUM  =  RECE02-HKNCOMBI(1)
                                    OR  RECE02-HKNCOMBI(2)
                                    OR  RECE02-HKNCOMBI(3)
                                    OR  RECE02-HKNCOMBI(4)
                                    OR  RECE02-HKNCOMBI(5)
                                    OR  RECE02-HKNCOMBI(6)
                                    OR  RECE02-HKNCOMBI(7)
                                    OR  RECE02-HKNCOMBI(8)
                                    OR  RECE02-HKNCOMBI(9)
                                    OR  RECE02-HKNCOMBI(10)
      *
                 MOVE    1             TO  FLG-KOHOK
               END-IF
             END-IF
      *
             PERFORM 910-HKNCOMBI2-FETCH-SEC
           END-PERFORM
      *
           PERFORM 910-HKNCOMBI2-CLOSE-SEC
      *
           .
       200117-KOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    限度額認定証所持チェック処理
      *****************************************************************
       200118-KOGAKU-CHK-SEC                SECTION.
      *
           INITIALIZE                      PTKOHINF-REC
                                           FLG-PTKOHINF
                                           FLG-NINTEI
      *
           MOVE    SPA-HOSPNUM             TO  PTKOH-HOSPNUM
           MOVE    RECE02-PTID             TO  PTKOH-PTID
           MOVE    PTKOHINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      FLG-PTKOHINF        =   ZERO
               PERFORM  800-PTKOHINF-FETCH-SEC
      *
               PERFORM  UNTIL  FLG-PTKOHINF  =  1
                 IF  (PTKOH-KOHNUM  =  "966" OR "967" OR "946")
                 AND (RECE02-SRYYM  >=  PTKOH-TEKSTYMD(1:6))
                 AND (RECE02-SRYYM  <=  PTKOH-TEKEDYMD(1:6))
                 AND (PTKOH-SAKJOKBN  =  SPACE)
                     MOVE    1             TO  FLG-NINTEI
                     MOVE    1             TO  FLG-PTKOHINF
                 END-IF
                 PERFORM  800-PTKOHINF-FETCH-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       200118-KOGAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           DISPLAY "2002-PRINT START"
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           INITIALIZE                    SUM-GOKEI-TBL
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
      *    ＣＳＶ<ヘッダーレコード>処理
           PERFORM 310-HEAD-HEN-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1

      *    ＣＳＶ<データレコード>処理
             PERFORM  330-CSV-HEN-SEC

           END-PERFORM
      *
      *    ＣＳＶ<エンドレコード>処理
           PERFORM 350-END-HEN-SEC
      *
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ<ヘッダーレコード>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                          WRK-REC
                                               WRK-REC-LENGTH
      *
      *    レコード区分
           MOVE    "H"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１
           MOVE    "請求年月"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２
           MOVE    "医療機関Ｃ"            TO  WRK-DATA
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３
           MOVE    "科別"                  TO  WRK-DATA
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し４
           MOVE    "入院区分"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し５
           MOVE    "請求先市町村"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し６
           MOVE    "割合区分"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し７
           MOVE    "特欄設定Ｃ"            TO  WRK-DATA
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し８
           MOVE    "診療年月"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し９
           MOVE    "福祉負担番号"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１０
           MOVE    "第一公費番号"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１１
           MOVE    "第二公費番号"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１２
           MOVE    "受給資格番号"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１３
           MOVE    "氏名"                  TO  WRK-DATA
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１４
           MOVE    "性別"                  TO  WRK-DATA
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１５
           MOVE    "生年月日"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１６
           MOVE    "被保記号"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１７
           MOVE    "被保番号"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１８
           MOVE    "医療保険番号"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し１９
           MOVE    "保険請求点数"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２０
           MOVE    "福祉請求点数"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２１
           MOVE    "第一請求点数"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２２
           MOVE    "第二請求点数"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２３
           MOVE    "福祉請求額"            TO  WRK-DATA
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２４
           MOVE    "第一患者負担"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２５
           MOVE    "第二患者負担"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２６
           MOVE    "食事回数"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２７
           MOVE    "福祉食事回数"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２８
           MOVE    "第一食事回数"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し２９
           MOVE    "第二食事回数"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３０
           MOVE    "食事標準負担"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３１
           MOVE    "福祉食事標準"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３２
           MOVE    "第一食事標準"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３３
           MOVE    "第二食事標準"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３４
           MOVE    "備考欄設定Ｃ"          TO  WRK-DATA
           MOVE    12                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３５
           MOVE    "診療科目"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３６
           MOVE    "福祉認定日"            TO  WRK-DATA
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３７
           MOVE    "高半該当"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３８
           MOVE    "経過該当"              TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    見出し３９
           MOVE    "処方箋発行"            TO  WRK-DATA
           MOVE    10                      TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           MOVE    ZERO                    TO  WRK-SCSVINFO-PTID
           MOVE    SPACE                   TO  WRK-SCSVINFO-NYUGAIKBN
           PERFORM 500-CSVINFO-SEC
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<データレコード>処理
      *****************************************************************
       330-CSV-HEN-SEC      SECTION.
      *
           INITIALIZE  WRK-REC
                       WRK-REC-LENGTH
           INITIALIZE  FLG-MOJI
      *
      *    １．レコード区分
           MOVE    "D"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．請求年月
           MOVE    LNK-PRTKANRI-SKYYMD(1:6) TO  WRK-DATA
           MOVE    6                        TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-DATA
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４．科別
           MOVE    "01"                    TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ５．入院区分
           IF  MF100-NYUGAIKBN  =  "1"
             MOVE  1                       TO  WRK-NUM
           ELSE
             MOVE  2                       TO  WRK-NUM
           END-IF
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ６．請求先市町村コード
           MOVE    MF100-CITYNUM2          TO  WRK-HKNJANUM
           PERFORM  920-HKNJAINF-SET-SEC
           MOVE    WRK-HKNJANUM            TO  WRK-DATA
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ７．割合区分
           EVALUATE  MF100-WARIAIKBN
             WHEN  5
               MOVE  "05"                  TO  WRK-DATA
             WHEN  6
               MOVE  "06"                  TO  WRK-DATA
             WHEN  8
               MOVE  "08"                  TO  WRK-DATA
             WHEN  9
               MOVE  "09"                  TO  WRK-DATA
           END-EVALUATE
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ８．特欄設定コード
           MOVE    MF100-TOKURAN           TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ９．診療年月
           MOVE    MF100-SRYYM             TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １０．福祉負担者番号
           INITIALIZE                          WRK-FTNJAHENKAN
           MOVE    MF100-FTNJANUM          TO  WRK-FTNJANUM-MAE
           PERFORM 331-FTNJA-HENKAN-SEC
           MOVE    WRK-FTNJANUM-ATO        TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １１．第一公費負担者番号
           INITIALIZE                          WRK-FTNJAHENKAN
           MOVE    MF100-EKOHFTNJANUM(1)   TO  WRK-FTNJANUM-MAE
           PERFORM 331-FTNJA-HENKAN-SEC
           MOVE    WRK-FTNJANUM-ATO        TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １２．第二公費負担者番号
           INITIALIZE                          WRK-FTNJAHENKAN
           MOVE    MF100-EKOHFTNJANUM(2)   TO  WRK-FTNJANUM-MAE
           PERFORM 331-FTNJA-HENKAN-SEC
           MOVE    WRK-FTNJANUM-ATO        TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １３．受給資格者証記号
           MOVE    MF100-JKYSNUM           TO  WRK-DATA
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １４．受給者氏名
           MOVE    MF100-NAME              TO  WRK-DATA
           MOVE    40                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １５．受給者性別
           MOVE    MF100-SEX               TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １６．受給者生年月日
           MOVE    MF100-BIRTHDAY          TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １７．被保険者証記号
           MOVE    MF100-KIGO              TO  WRK-DATA
           MOVE    20                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １８．被保険者証番号
           MOVE    MF100-NUM               TO  WRK-DATA
           MOVE    20                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １９．医療保険者番号
           MOVE    MF100-HKNJANUM          TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２０．保険請求点数
           MOVE    MF100-H-TOTALTEN        TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２１．福祉請求点数
           IF  MF100-TOTALTEN  =  MF100-TOTALTEN2
             MOVE    MF100-TOTALTEN        TO  WRK-FUKUSHI
           ELSE
             COMPUTE  WRK-FUKUSHI  =  MF100-TOTALTEN  -  MF100-TOTALTEN2
           END-IF
           MOVE    WRK-FUKUSHI             TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２２．第一公費請求点数
           IF  MF100-EKOHFTNJANUM(1)  NOT =  SPACE
           IF  MF100-TOTALTEN  =  MF100-TOTALTEN2
             MOVE    MF100-ETENSU(1)       TO  WRK-NUM9
           ELSE
             MOVE    MF100-TOTALTEN2       TO  WRK-NUM9
           END-IF
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
           ELSE
             MOVE  ZERO                    TO  WRK-NUM
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ２３．第二公費請求点数
           IF  MF100-EKOHFTNJANUM(2)  NOT =  SPACE
             MOVE  MF100-ETENSU(2)         TO  WRK-NUM9
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
           ELSE
             MOVE  ZERO                    TO  WRK-NUM
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ２４．福祉医療費請求額
           IF  MF100-TOKURAN  NOT =  SPACE
           OR  MF100-SEIKYU-FLG   =  1
             MOVE  MF100-SEIKYUGK          TO  WRK-NUM
           ELSE
             MOVE  ZERO                    TO  WRK-NUM
           END-IF
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２５．第一公費患者負担額
           IF  MF100-EKOHFTNJANUM(1)  NOT =  SPACE
             MOVE  MF100-EITBFTN(1)        TO  WRK-NUM9
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
           ELSE
             MOVE  ZERO                    TO  WRK-NUM
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ２６．第二公費患者負担額
           IF  MF100-EKOHFTNJANUM(2)  NOT =  SPACE
             MOVE  MF100-EITBFTN(2)        TO  WRK-NUM9
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
           ELSE
             MOVE  ZERO                    TO  WRK-NUM
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ２７．食事回数
           MOVE    MF100-H-SHOKUJINISSU    TO  WRK-NUM
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２８．福祉食事回数
           IF  MF100-NYUGAIKBN  =  "1"
             MOVE  MF100-SHOKUJINISSU      TO  WRK-NUM9
             MOVE  4                       TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
           ELSE
             MOVE  ZERO                    TO  WRK-NUM
             MOVE  4                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ２９．第一公費食事回数
           MOVE    MF100-E-SHOKUJINISSU(1) TO  WRK-NUM
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３０．第二公費食事回数
           MOVE    MF100-E-SHOKUJINISSU(2) TO  WRK-NUM
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３１．食事標準負担額
           MOVE    MF100-H-SHOKUJIFTN      TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３２．福祉食事標準負担額
           IF  MF100-NYUGAIKBN  =  "1"
             MOVE  MF100-SHOKUJIFTN        TO  WRK-NUM9
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
           ELSE
             MOVE  ZERO                    TO  WRK-NUM
             MOVE  8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ３３．第一公費食事標準負担額
           MOVE    MF100-E-SHOKUJIFTN(1)   TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３４．第二公費食事標準負担額
           MOVE    MF100-E-SHOKUJIFTN(2)   TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３５．備考欄設定コード
           MOVE    MF100-BIKO              TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３６．診療科目コード
           IF  SYS-1001-HOSPSBT1  =  1
             MOVE  MF100-SRYKA             TO  WRK-DATA
           ELSE
             MOVE  SPACE                   TO  WRK-DATA
           END-IF
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３７．福祉認定日
           IF  MF100-TOKURAN  NOT =  SPACE
             MOVE  LNK-PRTKANRI-SRYYM      TO  WRK-SYMD(1:6)
             MOVE  "01"                    TO  WRK-SYMD(7:2)
             IF  MF100-KOHTEKSTYMD  >  MF100-SRYYM
               MOVE  MF100-KOHTEKSTYMD     TO  WRK-NUM
             END-IF
           ELSE
             MOVE  ZERO                    TO  WRK-NUM
           END-IF
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３８．高半該当区分
           MOVE    MF100-KOUHAN            TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３９．経過該当区分
           MOVE    MF100-KEIKA             TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４０．処方箋発行期間有無
           MOVE    "0"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
      *    小計
      *    突合用件数
           ADD     1                       TO  SUM-GKENSU
      *    突合用点数
           ADD     MF100-H-TOTALTEN        TO  SUM-GTENSU
      *    突合用福祉請求額
           IF  MF100-TOKURAN  NOT =  SPACE
           OR  MF100-SEIKYU-FLG   =  1
             ADD   MF100-SEIKYUGK          TO  SUM-GSEIKYUGK
           END-IF
      *    突合用食事回数
           ADD     MF100-SHOKUJINISSU      TO  SUM-GSKENSU
      *    突合用食事標準負担額
           ADD     MF100-SHOKUJIFTN        TO  SUM-GSFTNGK
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
           .
       330-CSV-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号変換処理（前ゼロ埋め）
      *****************************************************************
       331-FTNJA-HENKAN-SEC     SECTION.
      *
      *    旧負担者番号の場合、右詰めしてから空白をゼロで埋める
           IF  WRK-FTNJANUM-MAE(3:1)  =  SPACE
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1     >  8
               IF  WRK-FTNJANUM-MAE(IDX1:1)  =  SPACE
                   COMPUTE  WRK-HANI  =  IDX1 - 1
                   COMPUTE  WRK-ICHI  =  (8 - WRK-HANI) + 1
                   MOVE     8            TO  IDX1
                   MOVE     1            TO  WRK-HENKANFLG
               END-IF
             END-PERFORM
      *
             IF  WRK-HENKANFLG  =  ZERO
               MOVE  WRK-FTNJANUM-MAE    TO  WRK-FTNJANUM-ATO
             ELSE
               IF  WRK-FTNJANUM-MAE  NOT =  SPACE
                 MOVE  WRK-FTNJANUM-MAE(1:WRK-HANI)
                                 TO  WRK-FTNJANUM-ATO(WRK-ICHI:WRK-HANI)
                 INSPECT  WRK-FTNJANUM-ATO  REPLACING  ALL " "  BY  "0"
               END-IF
             END-IF
      *
           ELSE
             MOVE  WRK-FTNJANUM-MAE      TO  WRK-FTNJANUM-ATO
           END-IF
      *
           .
       331-FTNJA-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ<エンドレコード>処理
      *****************************************************************
       350-END-HEN-SEC          SECTION.
      *
           INITIALIZE                          WRK-REC
                                               WRK-REC-LENGTH
      *
      *    １．レコード区分
           MOVE    "E"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．突合用件数
           MOVE    SUM-GKENSU              TO  WRK-NUM
           MOVE    9                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３．突合用点数
           MOVE    SUM-GTENSU              TO  WRK-NUM
           MOVE    9                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ４．突合用福祉請求額
           MOVE    SUM-GSEIKYUGK           TO  WRK-NUM
           MOVE    9                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ５．突合用食事回数
           MOVE    SUM-GSKENSU             TO  WRK-NUM
           MOVE    9                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ６．突合用食事標準負担額
           MOVE    SUM-GSFTNGK             TO  WRK-NUM
           MOVE    9                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           MOVE    ZERO                    TO  WRK-SCSVINFO-PTID
           MOVE    SPACE                   TO  WRK-SCSVINFO-NYUGAIKBN
           PERFORM 500-CSVINFO-SEC
      *
           .
       350-END-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ＿ＩＮＦＯ出力処理
      *****************************************************************
       500-CSVINFO-SEC                SECTION.
      *
           ADD     1                   TO  WRK-SCSVINFO-SEQ-NO
      *
           INITIALIZE                  ORCSCSVINFOAREA
           MOVE    "INS"               TO  SCSVINFO-MODE
           MOVE    WRK-PARA-SHELLID    TO  SCSVINFO-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SCSVINFO-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SCSVINFO-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-RENNUM TO  SCSVINFO-RENNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  SCSVINFO-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SCSVINFO-SKYYMD
           MOVE    WRK-SCSVINFO-SEQ-NO TO  SCSVINFO-SEQ-NO
           MOVE    WRK-SCSVINFO-NYUGAIKBN
                                       TO  SCSVINFO-NYUGAIKBN
           MOVE    WRK-SCSVINFO-PTID   TO  SCSVINFO-PTID
           MOVE    WRK-REC             TO  SCSVINFO-CSVDATA
           CALL    "ORCSCSVINFO"       USING
                                       ORCSCSVINFOAREA
                                       SPA-AREA
           IF      SCSVINFO-RETURN     =   ZERO
               ADD     1                   TO  CNT-OUT
           ELSE
               MOVE    "ＣＳＶ用ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           .
       500-CSVINFO-EXT.
           EXIT.
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
      *      ジョブ終了処理
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
           MOVE   1                  TO  FLG-END
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
           CLOSE  MF100-FILE
           MOVE   WRK-SOUGOKEIKENSU  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU1013 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1013 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU1013 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ファイル保存情報更新
           PERFORM 3001-FILE-INFO-INSERT-SEC
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
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SFILESV-SHORI-RENNUM
                                                        (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
      *    ファイル名
           STRING  "FWMA001_"              DELIMITED  BY  SIZE
                   LNK-PRTKANRI-SKYYMD(1:6) DELIMITED BY  SIZE
                   "_"                     DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN      DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD         DELIMITED  BY  SIZE
                   "_"                     DELIMITED  BY  SIZE
                   "2"                     DELIMITED  BY  SIZE
                   ".CSV"                  DELIMITED  BY  SIZE
                   INTO                    WRK-FILENAME
           END-STRING
           MOVE    WRK-FILENAME    TO  SFILESV-TO-DATA  (1)
           MOVE    WRK-SOUGOKEIKENSU   TO  SFILESV-CNT-ALL  (1)
           MOVE    "福祉医療費連記式ＣＳＶ"    TO  SFILESV-TITLE    (1)
           IF      WRK-SOUGOKEIKENSU   >   ZERO
               MOVE    "1"         TO  SFILESV-DATA-TYPE(1)
           END-IF
           MOVE    WRK-RECEERR     TO  SFILESV-ERR-MSG  (1)
           IF      WRK-RECEERR NOT =   SPACE
               MOVE    "1"         TO  SFILESV-ERR-CODE (1)
           END-IF
           MOVE    "1"             TO  SFILESV-CODE-TYPE (1)
      *
           CALL    "ORCSFILESV"        USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
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
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
             NOT AT  END
               ADD  1                 TO  CNT-MF100
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
      *
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
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
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
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得マスタ読込
      *****************************************************************
       800-TSYRRK-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC   =   ZERO
               MOVE    ZERO                TO  FLG-TSYRRK
               MOVE    MCPDATA-REC         TO  TSYRRK-REC
           ELSE
               MOVE    1                   TO  FLG-TSYRRK
               INITIALIZE                  TSYRRK-REC
           END-IF
      *
           .
       800-TSYRRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    低年金マスタ読込
      *****************************************************************
       800-TNKRRK-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC   =   ZERO
               MOVE    ZERO                TO  FLG-TNKRRK
               MOVE    MCPDATA-REC         TO  TNKRRK-REC
           ELSE
               MOVE    1                   TO  FLG-TNKRRK
               INITIALIZE                  TNKRRK-REC
           END-IF
      *
           .
       800-TNKRRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM                 TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
           MOVE  "tbl_hknjainf"              TO  MCP-TABLE
           MOVE  "key2"                      TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"                 USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"          TO  MCP-TABLE
             MOVE    "key2"                  TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
             PERFORM  UNTIL  FLG-HKNJAINF  =  1
               IF  HKNJA-HKNNUM  =  "060"
                   MOVE  HKNJA-HKNJANUM      TO  WRK-HKNJANUM
                   MOVE  HKNJA-HKNJANAME     TO  WRK-HKNJANAME
                   MOVE  1                   TO  FLG-HKNJAINF
               END-IF
               PERFORM  925-HKNJAINF-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE  "tbl_hknjainf"              TO  MCP-TABLE
           MOVE  "key2"                      TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"                 USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1            TO  FLG-HKNJAINF
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
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
      *
      *=== 福祉と絡まない公費の掲載防止 by Yoshikawa(2013/09/11) start
      *****************************************************************
      *    保険組合せマスター読込処理２
      *****************************************************************
       910-HKNCOMBI2-SELECT-SEC  SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
           MOVE    RECE10-PTID             TO  COMB-PTID
           MOVE    RECE10-KOHID(IDX)       TO  COMB-KOHID(1)
                                               COMB-KOHID(2)
                                               COMB-KOHID(3)
                                               COMB-KOHID(4)
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               PERFORM 910-HKNCOMBI2-FETCH-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI2-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスターＦＥＴＣＨ
      *****************************************************************
       910-HKNCOMBI2-FETCH-SEC  SECTION.
      *
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key4"                TO  MCP-PATHNAME
           MOVE    "DBFETCH"             TO  MCP-FUNC
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC       TO  HKNCOMBI-REC
               MOVE    ZERO              TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                    HKNCOMBI-REC
               MOVE    1                 TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI2-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタークローズ処理
      *****************************************************************
       910-HKNCOMBI2-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key4"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       910-HKNCOMBI2-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担情報読込処理
      *****************************************************************
       930-PTKOHFTN-READ-SEC         SECTION.
      *
           INITIALIZE                          PTKOHFTN-REC
                                               FLG-PTKOHFTN
                                               WRK-GAIJGNGAK
                                               WRK-NYUJGNGAK
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
               PERFORM  930-PTKOHFTN-FETCH-SEC
               PERFORM  UNTIL  FLG-PTKOHFTN  =  1
                   IF  (PTKOHFTN-TEKSTYMD(1:6) <= RECE02-SRYYM(1:6))
                   AND (PTKOHFTN-TEKEDYMD(1:6) >= RECE02-SRYYM(1:6))
                       MOVE    PTKOHFTN-GAIJGNGAK
                                           TO  WRK-GAIJGNGAK
                       MOVE    PTKOHFTN-NYUJGNGAK
                                           TO  WRK-NYUJGNGAK
                       MOVE    1           TO  FLG-PTKOHFTN
                   ELSE
                       PERFORM  930-PTKOHFTN-FETCH-SEC
                   END-IF
               END-PERFORM
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
       930-PTKOHFTN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担情報ＦＥＴＣＨ処理
      *****************************************************************
       930-PTKOHFTN-FETCH-SEC         SECTION.
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
       930-PTKOHFTN-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報読み込み
      *****************************************************************
       940-SYUNOU2-READ-SEC  SECTION.
      *
           INITIALIZE                      SYUNOU2-REC
                                           FLG-SYUNOU2
                                           WRK-KOHFTNMONEY
      *
           MOVE  SPA-HOSPNUM               TO  SYU2-HOSPNUM
           MOVE  RECE02-NYUGAIKBN          TO  SYU2-NYUGAIKBN
           MOVE  RECE02-PTID               TO  SYU2-PTID
           MOVE  RECE02-SRYYM(1:6)         TO  SYU2-SRYYMD(1:6)
           MOVE  "__"                      TO  SYU2-SRYYMD(7:2)
           MOVE  SYUNOU2-REC               TO  MCPDATA-REC
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE  "tbl_syunou"              TO  MCP-TABLE
           MOVE  WRK-PATH2                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"               USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_syunou"          TO  MCP-TABLE
             MOVE    WRK-PATH2             TO  MCP-PATHNAME
             PERFORM  945-SYUNOU2-FETCH-SEC
      *
             PERFORM  UNTIL  FLG-SYUNOU2  =  1
      *          全国公費の保険負担金額集計
                 PERFORM  VARYING  IDX  FROM  1  BY  1
                          UNTIL    IDX  >    4
                          OR       SYU2-KOH-HKNNUM(IDX)  =  SPACE
                     IF  SYU2-KOH-HKNNUM(IDX)(1:1) =  "0"
                         ADD    SYU2-KOH-HKNFTNMONEY(IDX)  TO
                                                WRK-KOHFTNMONEY
                     END-IF
                 END-PERFORM
      *
                 MOVE    "tbl_syunou"      TO  MCP-TABLE
                 MOVE    WRK-PATH2         TO  MCP-PATHNAME
                 PERFORM   945-SYUNOU2-FETCH-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_syunou"            TO  MCP-TABLE
           MOVE    WRK-PATH2               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       940-SYUNOU2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       945-SYUNOU2-FETCH-SEC  SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC         TO  SYUNOU2-REC
               MOVE    ZERO                TO  FLG-SYUNOU2
           ELSE
               INITIALIZE  SYUNOU2-REC
               MOVE    1                   TO  FLG-SYUNOU2
           END-IF
      *
           .
       945-SYUNOU2-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO     TO     FLG-HEIYOU
                                   WRK-SEIKYUGK
                                   WRK-KOH-SKJFTN
                                   WRK-SEIKYUGK2
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     4  OR
                    SYU-KOH-HKNNUM(IDX) =  SPACE
      *
               IF   SYU-KOH-HKNNUM(IDX)  NOT =  "027"  AND
                    SYU-KOH-HKNNUM(IDX)(1:1) =  "0"
      *
                 MOVE   1                TO    FLG-HEIYOU
                 IF    RECE02-FTNMONEY(1) >  ZERO
                   MOVE   SYU-KOH-COMPFTN(IDX) TO  WRK-SEIKYUGK
                                                   WRK-SEIKYUGK2
                 ELSE
                   MOVE   SYU-KOH-COMPFTN-ENTANI(IDX)
                                               TO  WRK-SEIKYUGK
                                                   WRK-SEIKYUGK2
                 END-IF
      *
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
      * 主保険の食事療養費情報から食事療養費、標準負担額を取得
      * ※全国公費分がある場合に限り取得しているため、主保険で代用
                 ADD  SYU-SYUSKJ-NISSU
                            TO  WRK-ETC-SKJ-NISSU(IDY)
                 ADD  SYU-SYUSKJ-RYOYOHI
                            TO  WRK-ETC-SKJ-RYOYOHI(IDY)
                 ADD  SYU-SYUSKJ-FTN
                            TO  WRK-ETC-SKJ-FTN(IDY)
                 IF  SYU-KOH-HKNNUM(IDX)  =  "054"
                     COMPUTE WRK-KOH-SKJFTN  =  SYU-SKJ-FTN(IDX)  -
                                                SYU-SKYMONEY-LIFE-KEI
                 ELSE
                     MOVE    SYU-SKJ-FTN(IDX)  TO  WRK-KOH-SKJFTN
                 END-IF
                 MOVE    1                    TO  FLG-KOHSKJ
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX) = "972"
                 MOVE    1     TO      FLG-TYOKI
               END-IF
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX) = "974"
                 MOVE    2     TO      FLG-TYOKI
               END-IF
      *
      *        特欄コード設定
               EVALUATE  SYU-KOH-HKNNUM(IDX)
                 WHEN  "052"
                   MOVE  "52"              TO  WRK-TOKURAN
                 WHEN  "054"
                   MOVE  "54"              TO  WRK-TOKURAN
                 WHEN  "038"
                   MOVE  "38"              TO  WRK-TOKURAN
                 WHEN  "051"
                 WHEN  "091"
                   MOVE  "51"              TO  WRK-TOKURAN
                 WHEN  "079"
                   MOVE  "79"              TO  WRK-TOKURAN
                 WHEN  "024"
                   MOVE  "24"              TO  WRK-TOKURAN
                 WHEN  "016"
                   MOVE  "16"              TO  WRK-TOKURAN
                 WHEN  "015"
                   MOVE  "15"              TO  WRK-TOKURAN
                 WHEN  "021"
                   MOVE  "21"              TO  WRK-TOKURAN
                 WHEN  "011"
                   MOVE  "11"              TO  WRK-TOKURAN
                 WHEN  "010"
                   MOVE  "10"              TO  WRK-TOKURAN
                 WHEN  "972"
                 WHEN  "974"
                   MOVE  "02"              TO  WRK-TOKURAN
               END-EVALUATE
      *
               MOVE  FLG-HEIYOU            TO  FLG-ZENKOHI
      *
      *        特疾負無は「51」でまとめる
               IF  SYU-KOH-HKNNUM(IDX)  =  "091"
                 MOVE   1                  TO  FLG-KOH091
               END-IF
      *
      *        限度額認定証を持っていない場合でも、
      *        高額療養費計算されていれば高額該当とする
               IF  SYU-KOH-FTN-ENTANI  NOT =  ZERO
                 MOVE   "99"               TO  WRK-TOKURAN
                 MOVE    1                 TO  FLG-KOGAKU
               END-IF
      *
             IF   SYU-KOH-HKNNUM(IDX)     =  WRK-KOHNUM
               IF     IDX                 =  1
                 IF  SYU-SYUCOMPFTN-ENTANI
                                       > SYU-KOH-COMPFTN-ENTANI(IDX)
                   MOVE  ZERO          TO  FLG-ERR
                 END-IF
               ELSE
                 IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                      >  SYU-KOH-COMPFTN-ENTANI(IDX)
                   MOVE  ZERO          TO  FLG-ERR
                 END-IF
               END-IF
             END-IF
      *
           END-PERFORM
      *
           IF    FLG-HEIYOU    =    ZERO
      *    上限が有る場合は10円単位、ない場合は1円単位で加算
             IF    RECE02-FTNMONEY(1) >  ZERO
               IF    RECE02-RJNHKNNUM NOT = SPACE
                 MOVE     SYU-KOH-COMPFTN(1) TO  WRK-SEIKYUGK
                                                 WRK-SEIKYUGK2
               ELSE
                 MOVE     SYU-SYUCOMPFTN     TO  WRK-SEIKYUGK
               END-IF
               MOVE       1                  TO  MF100-SEIKYU-FLG
             ELSE
               IF    RECE02-RJNHKNNUM NOT = SPACE
                 MOVE     SYU-KOH-COMPFTN-ENTANI(1) TO  WRK-SEIKYUGK
               ELSE
                 MOVE     SYU-SYUCOMPFTN-ENTANI     TO  WRK-SEIKYUGK
                                                        WRK-SEIKYUGK2
               END-IF
             END-IF
           ELSE
             ADD   SYU-TOTAL-HKNTEN     TO  MF100-TOTALTEN2
           END-IF
      *
           ADD    WRK-SEIKYUGK               TO    WRK-SEIKYUGK-G
           ADD    WRK-SEIKYUGK2              TO    WRK-SEIKYUGK2-G
           ADD    WRK-KOH-SKJFTN             TO    WRK-KOH-SKJFTN-G
           DISPLAY "加算請求額="  WRK-SEIKYUGK
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
             MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
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
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
                     MOVE  PTKOH-TEKSTYMD  TO  WRK-KOHTEKSTYMD
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
      *    患者公費情報マスタ読込
      *****************************************************************
       800-PTKOHINF-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC   =   ZERO
               MOVE    ZERO                TO  FLG-PTKOHINF
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           .
       800-PTKOHINF-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW    <       1
                   IF      WRK-DATA (IDW:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW)           DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZZ.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF
      *
               PERFORM VARYING IDW    FROM    WRK-ST    BY  1
                       UNTIL   IDW    >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                        END-STRING
                        COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                      IDZ
                        MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5006-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF
      *
               PERFORM VARYING IDW    FROM    WRK-ST    BY  1
                       UNTIL   IDW    >       10
                   IF      WRK-NUM9 (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                  DELIMITED  BY  SIZE
                               WRK-NUM9(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                        END-STRING
                        COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                      IDZ
                        MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5006-NUM-HENSYU-EXT.
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
               MOVE  1  TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
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
      *
