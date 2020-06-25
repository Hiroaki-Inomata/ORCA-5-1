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
       PROGRAM-ID.     SEIKYU2309.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 愛知・乳障母戦医療費請求書(外来用)
      *                      (対象公費はTAISYOKOHI.INI参照
      *                      (デフォルト値：181,281,381,481,182,282,183)
      *                      (              283,184,284,185,285)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/28    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            (個別発行対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    06/07/19    自立支援に伴う備考欄の修正
      *                                  結精点数の修正
      *  01.00.02    内田    07/03/08    長期上位対応
      *  01.00.03    内田    07/04/24    高額療養費対応（市町村負担額、備考欄）
      *  01.00.04    内田    07/06/05  　備考欄の文字（１７、１９）修正
      *  01.00.05    内田    07/08/22    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.06    内田    08/05/06    印字対象番号の追加（185,285）
      *  01.00.07    内田    09/01/16    SPA-AREA初期化
      *  01.00.08    内田    09/08/14    保険番号185,285は、受給者番号2桁目
      *                                  から2桁が「00」の場合、豊橋市とする
      *  01.00.09    吉川    10/04/08    市町村合併対応（あま市）
      *  01.00.10    吉川    10/09/15    受給者番号の2桁目が「9」の場合、
      *                                  岡崎市とするように修正
      *  01.00.11    吉川    12/04/18    対象公費番号をINIファイルから
      *                                  取得するようにする
      *                                  市町村負担額の計算修正
      *  01.00.12    吉川    12/05/17    一宮市で患者負担が無い場合
      *                                  市町村負担額欄は記載しない
      *  01.00.13    吉川    12/08/22    一宮市は市町村負担額欄に記載しない
      *                                  場合でも上部の合計金額欄は集計する
      *  01.00.14    吉川    13/01/10    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.15    吉川    13/03/11    総点数欄は福祉分の点数ではなく
      *                                  保険の総点数を記載するように修正
      *  01.00.16    吉川    13/07/08    備考の診療年月は「yyyy年mm月」
      *                                  のように印字する
      *                                  プログラムオプション対応
      *  01.00.17    吉川    14/02/06    月途中で市町村が変わった場合
      *                                  備考にそれぞれの市の点数を記載
      *  01.00.18    吉川    14/02/13    長期の判定不具合修正
      *  01.00.19    萩野    14/05/14    長期の判定不具合修正
      *  01.00.20    萩野    14/06/16    長期は上限額を超えた時に記載する
      *  01.00.21    吉川    14/08/12    北名古屋市こども医療対応
      *                                  福祉併用分と単独がある場合
      *                                  市町村請求額は記載する
      *  01.00.22    吉川    14/10/02    一宮市、北名古屋市は市町村負担額が
      *                                  空白の分も合計に集計する
      *  01.00.23    吉川    14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.24    萩野    15/01/16    公費併用とそうではない診療があった場合、
      *                                  備考欄へ該当公費と対象点数を記載する
      *                                  平成２７年１月より追加された特記事項欄への
      *                                  記載「２６区ア」等を備考欄へ記載する
      *  01.00.25    萩野    15/01/28    全国公費があった場合、備考欄へ
      *                                  公費と対象点数を記載する
      *                                  特記事項欄の多数回該当「３１多ア」等を
      *                                  備考欄へ記載する
      *  01.00.26    吉川    15/02/16    結精点数欄の対象公費修正
      *  01.00.27    吉川    15/03/05    公費の助成がない場合は印字しない
      *  01.00.28    吉川    15/03/17    備考３が記載されなくなる現象修正
      *  01.00.29    吉川    15/04/13    江南市、犬山市等子ども医療対応
      *  01.00.30    吉川    15/06/08    他市町村変更時、一方の請求点数が
      *                                  ゼロの場合、備考欄の市町村名は
      *                                  記載しないように修正
      *  01.00.31    門間    15/09/11    長期上位の場合、備考欄に長２と記載
      *  01.00.32    門間    15/10/13    医療機関編集情報への開設者名追加対応
      *  01.00.33    吉川    16/05/13    月途中に主保険が変更されても
      *                                  １行にまとめて印字する
      *                                  市町村負担額が空白のパターンでも
      *                                  全国公費を併用している場合は印字
      *  01.00.34    吉川    16/06/06    請求総件数欄が印字されない不具合修正
      *  01.00.35    吉川    16/09/12    分点時の福祉点数の表示不具合修正
      *                                  分点時の高額療養費は併用分ではなく
      *                                  保険総点数で計算をする
      *  01.00.36    吉川    16/11/15    分点かつ高額の判定ミス修正
      *  01.00.37    萩野    17/01/13    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.38    吉川    17/04/13    保険単独分が高額上限に達した場合
      *                                  印字対象外とするように修正
      *                                  医療機関情報の取得の修正
      *  01.00.39    吉川    17/05/15    難病＋９５７かつ分点の場合の
      *                                  市町村負担額の計算修正
      *  01.00.40    吉川    17/07/14    不要なDISPLAY文削除
      *  01.00.41    門間    17/08/02    備考の診療年月は和暦で記載
      *  01.00.42    吉川    17/09/12    月途中で保険が変わった場合
      *                                  行を分けて印字し、備考欄に記載
      *  01.00.43    吉川    17/11/15    備考欄の初期化不具合修正
      *                                  SITEKBN変更
      *  01.00.44    吉川    17/12/08    高額かつ限度額未満で分点の場合の
      *                                  市町村負担額の修正
      *  01.00.45    萩野    18/01/11    市町村負担額欄の桁数の増加
      *  01.00.46    萩野    20/02/12    修正履歴コメント削除
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
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
      *    市町村名用ファイル
           SELECT  CITY-FILE      ASSIGN TXT-NAME
                                  ORGANIZATION   IS  LINE SEQUENTIAL
                                  ACCESS MODE    IS  SEQUENTIAL
                                  FILE   STATUS  IS  STS-CITY.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI2307.INC".
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
      *    市町村名用データ
       FD  CITY-FILE.
           COPY  "TOWN2301.INC".
      *
       WORKING-STORAGE  SECTION.
      *
      *    対象公費番号データ
           COPY  "TAISYO2301.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
           COPY  "SC2309.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100    PIC X(02).
           03  STS-RECE02   PIC X(02).
           03  STS-RECEERR  PIC X(02).
           03  STS-CITY     PIC X(02).
           03  STS-TAISYOKOHI  PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF        PIC 9(01).
           03  FLG-END          PIC 9(01).
           03  FLG-SYUNOU       PIC 9(01).
           03  FLG-HEIYOU       PIC 9(01).
           03  FLG-MF100        PIC 9(01).
           03  FLG-TYOKI        PIC 9(01).
           03  FLG-PRINT        PIC 9(01).
           03  FLG-SYSKANRI     PIC 9(01).
           03  FLG-PTNYUINRRK   PIC 9(01).
           03  FLG-HKNJAINF     PIC 9(01).
           03  FLG-TOWN         PIC 9(01).
           03  FLG-PASS         PIC 9(01).
           03  FLG-HKNCOMBI     PIC 9(01).
           03  FLG-ERR          PIC 9(01).
           03  FLG-SYORI        PIC 9(01).
           03  FLG-BTPARA       PIC 9(01).
           03  FLG-TAISYOKOHI   PIC 9(01).
           03  FLG-TAISYO       PIC 9(01).
           03  FLG-RECE10       PIC 9(01).
           03  FLG-PRGOPTION    PIC 9(01).
           03  FLG-OK           PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-SYUNOU2      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX         PIC 9(04).
           03  IDXX        PIC 9(04).
           03  IDX1        PIC 9(04).
           03  IDX2        PIC 9(04).
           03  IDX3        PIC 9(04).
           03  IDY         PIC 9(04).
           03  IDZ         PIC 9(04).
           03  IDS         PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR                PIC  X(200).
           03  WRK-Z1                     PIC Z.
           03  WRK-Z2                     PIC ZZ.
           03  WRK-Z3                     PIC ZZZ.
           03  WRK-Z5                     PIC Z,ZZZ.
           03  WRK-Z6                     PIC ZZ,ZZZ.
           03  WRK-Z6-2                   PIC ZZZZZZ.
           03  WRK-Z7                     PIC ZZZ,ZZZ.
           03  WRK-Z8                PIC ZZZZZZZZ.
           03  WRK-Z9                     PIC Z,ZZZ,ZZZ.
           03  WRK-Z11                    PIC ZZZ,ZZZ,ZZZ.
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
           03  WRK-KOBETUCITYNUM          PIC X(03).
           03  WRK-KASAN-TBL.
               05   WRK-FTNJANUM          PIC X(08).
               05   WRK-KOHNUM            PIC X(03).
               05   WRK-KOHID             PIC 9(10).
                05   WRK-SEIKYUGK-G       PIC 9(08)V9(01).
                05   WRK-SEIKYUGK-G2      PIC 9(08).
                05   WRK-SEIKYUGK         PIC 9(08)V9(01).
               05  WRK-TYOKIFTNGK   PIC 9(08).
               05  WRK-TYOKIFTNGK-G PIC 9(08).
           03  WRK-HKNJANUM               PIC X(05).
           03  WRK-HKNJANAME              PIC X(20).
           03  WRK-KYURATE                PIC 9(02).
           03  WRK-KEISAN                 PIC 9(08).
           03  WRK-AMARI                  PIC 9(02).
           03  WRK-KOHFTN                 PIC 9(08).
      *    市町村用ファイル名エリア
           03  TXT-NAME                   PIC X(512).
           03  WRK-KEY-CITYHKNJANUM       PIC X(05).
           03  WRK-SYOKEI-AREA.
               05  WRK-S-KENSU            PIC 9(02).
               05  WRK-S-TENSU            PIC 9(09).
               05  WRK-S-FTNGK            PIC 9(09).
               05  WRK-S-KESE             PIC 9(09).
               05  WRK-S-SEIKYUGK         PIC 9(09).
               05  WRK-S-KOHFTN           PIC 9(09).
           03  WRK-GKENSU                 PIC 9(04).
           03  WRK-GPAGE                  PIC 9(03).
           03  WRK-PAGE                   PIC 9(03).
           03  WRK-BIKO                   PIC X(18).
           03  WRK-BIKO2                  PIC X(10).
           03  WRK-BIKO3                  PIC X(20).
           03  WRK-BIKO4                  PIC X(18).
           03  WRK-JTENSU                 PIC ZZZZZZ.
           03  WRK-KOHJKYSNUM             PIC X(07).
           03  WRK-CITYHKNJANUM           PIC X(05).
           03  WRK-TENSU                  PIC X(06).
           03  WRK-TAISYONAME             PIC X(06).
           03  WRK-TAISYOTENSU2           PIC X(09).
           03  WRK-TANSEIDO               PIC X(06).
           03  WRK-KESE                   PIC 9(07).
           03  WRK-HKNTANDOKU             PIC 9(04).
           03  WRK-HKNTANDOKU-KIN         PIC 9(08).
           03  WRK-PATH2                  PIC X(50).
      *
           03  WRK-MAE-INPUT              PIC X(200).
           03  WRK-OUT-INPUT              PIC X(200).
      *
           03  WRK-PATH              PIC  X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
      *
           03  WRK-SAV-PTNUM               PIC X(20).
           03  WRK-SAV-HKNJANUM            PIC X(08).
           03  WRK-SAV-TEKSTYMD            PIC X(08).
           03  WRK-HKNCHG-AREA.
               05  WRK-HKNCHG-TBL  OCCURS  100.
                   07  WRK-HKNCHG-PTNUM    PIC X(20).
                   07  WRK-HKNCHG-TEKSTYMD PIC X(08).
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
      *    市町村名退避エリア
       01  CITY-AREA.
           03  CITY-TBL  OCCURS  200.
               05  CITY-NUM     PIC  X(02).
               05  CITY-SP1     PIC  X(01).
               05  CITY-HKNJA   PIC  X(05).
               05  CITY-KAIGYO  PIC  X(01).
      *    キー用エリア
       01  KEY-AREA  VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-CITYNUM   PIC X(05).
           03  KEY-OLD.
               05  KEY-O-CITYNUM   PIC X(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-PAGE2   PIC 9(03).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM              PIC X(03)  OCCURS  4.
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-ICHINOMIYA1    PIC X(03)  VALUE  "304".
           03  WRK-CONS-ICHINOMIYA2    PIC X(03)  VALUE  "404".
           03  WRK-CONS-KITANAGOYA1    PIC X(03)  VALUE  "337".
           03  WRK-CONS-KITANAGOYA2    PIC X(03)  VALUE  "437".
           03  WRK-CONS-KOUNAN1        PIC X(03)  VALUE  "318".
           03  WRK-CONS-KOUNAN2        PIC X(03)  VALUE  "418".
           03  WRK-CONS-INUYAMA1       PIC X(03)  VALUE  "316".
           03  WRK-CONS-INUYAMA2       PIC X(03)  VALUE  "416".
           03  WRK-CONS-OOGUCHI1       PIC X(03)  VALUE  "341".
           03  WRK-CONS-OOGUCHI2       PIC X(03)  VALUE  "441".
           03  WRK-CONS-FUSOU1         PIC X(03)  VALUE  "342".
           03  WRK-CONS-FUSOU2         PIC X(03)  VALUE  "442".
      *
      *    平成２７年１月改正対応
           03  WRK-CONS-SRYYM-201501   PIC X(06) VALUE "201501".
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
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
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
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
      *    収納情報２
       01  SYUNOU2-REC.
           COPY  "CPSYUNOU.INC"
                   REPLACING  //SYU//
                   BY         //SYU2//.
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY  "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           INITIALIZE  CITY-AREA
           INITIALIZE  KEY-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  TAISYO-DAT
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
           MOVE    "SEIKYU2309"    TO  JOB-PGID
           MOVE    "愛知・外来用・乳障母戦医療費請求書"
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
             DISPLAY "*** SEIKYU2307 SYS 1001 ERR ***"
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
             DISPLAY "*** SEIKYU2307 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
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
           PERFORM  1001-CITY-SEC
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
      *    市町村名用テキスト読み込み処理 
      *****************************************************************
       1001-CITY-SEC                SECTION.
      *
      *    ファイルディレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE  "data/TOWN2307.TXT"     TO  MKPASS-IN-01
           CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
           MOVE  MKPASS-OT-01  TO  TXT-NAME
      *
           OPEN  INPUT  CITY-FILE
      *
           MOVE  ZERO   TO  FLG-TOWN
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL FLG-TOWN = 1
             READ CITY-FILE NEXT
               AT END
                 MOVE  1         TO  FLG-TOWN
               NOT AT END
                 MOVE  TOWN-TBL  TO  CITY-TBL(IDX)
             END-READ
           END-PERFORM
      *
           CLOSE CITY-FILE
      *
      *
           .
       1001-CITY-EXT.
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
           MOVE    "SEIKYU2309"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費番号="  TAISYO-KOHNUM(IDX)
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
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
             INITIALIZE                            TBL-HIKAKU-KOHI
             MOVE    RECE02-KOHNUM(IDZ)        TO  TBL-HKOHNUM(1)
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
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
           MOVE  1                   TO  MF100-SKBFLG
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-ZAITAKU      TO  MF100-ZAITAKU
      *
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           MOVE  MF100-KOHJKYSNUM          TO  MF100-JKYSNUM
           COMPUTE IDY     =       IDZ     +   1
           MOVE  RECE02-JNISSUINF   (IDY)  TO  MF100-JNISSUINF
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
      *
      *    市町村番号設定
           MOVE  MF100-KOHJKYSNUM          TO  WRK-KOHJKYSNUM
           MOVE  MF100-KOHNUM              TO  WRK-KOHNUM
           PERFORM  2001-CITYNUM-SEC
           MOVE  WRK-CITYHKNJANUM          TO  MF100-CITYHKNJANUM
      *
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    請求管理から総点数取得
           PERFORM  900-SEIKYU-READ-SEC
           ADD     RECE10-TOTALTEN(1)      TO  MF100-TOTALTEN
           ADD     RECE02-FTNMONEY(IDY)    TO  MF100-FTNMONEY
           ADD     RECE02-TOTALTEN(IDY)    TO  MF100-FUKUSITEN
      *
           IF  (RECE02-KOHNUM(2)  NOT =  SPACE)
           AND (RECE02-KOHJKYSNUM(1)(2:2)  NOT =
                RECE02-KOHJKYSNUM(2)(2:2))
      *      月途中で変更になった公費の市町村と点数保存
             IF  IDZ  =  1
               MOVE  RECE02-KOHJKYSNUM(2)  TO  WRK-KOHJKYSNUM
               MOVE  RECE02-KOHNUM(2)      TO  WRK-KOHNUM
               MOVE  RECE02-TOTALTEN(3)    TO  MF100-TAKOHTEN
             ELSE
               MOVE  RECE02-KOHJKYSNUM(1)  TO  WRK-KOHJKYSNUM
               MOVE  RECE02-KOHNUM(1)      TO  WRK-KOHNUM
               MOVE  RECE02-TOTALTEN(2)    TO  MF100-TAKOHTEN
             END-IF
             PERFORM  2001-CITYNUM-SEC
             MOVE  WRK-CITYHKNJANUM        TO  MF100-TACITYHKNJANUM
           END-IF
      *
      *    高額療養費チェック
           IF   RECE02-ZAITAKU  NOT =  SPACE
           AND  RECE02-ZAITAKU  NOT =  "1"
             MOVE  RECE02-ZAITAKU     TO  MF100-KOGAKU
           END-IF
      *
      *    多数回該当チェック
           IF  RECE02-FLG(4)      =  1
               PERFORM  VARYING  IDX  FROM  1  BY  1
                        UNTIL    IDX  >     5
                   IF  RECE10-TOKKI(IDX)  =  "31" OR "32" OR "33" OR
                                             "34" OR "35"
                       MOVE    RECE10-TOKKI(IDX)      TO  MF100-TOKKI
                   END-IF
               END-PERFORM
           END-IF
      *
           INITIALIZE       WRK-KASAN-TBL
      *    請求金額集計
           MOVE    MF100-KOHNUM       TO    WRK-KOHNUM
           MOVE    MF100-KOHID        TO    WRK-KOHID
           MOVE    ZERO               TO    WRK-SEIKYUGK-G
                                            FLG-TYOKI
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"        TO  WRK-PATH
           ELSE
               MOVE    "key25"        TO  WRK-PATH
           END-IF
           MOVE    1                  TO  FLG-ERR
           PERFORM  920-SYUNOU-SET-SEC
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           COMPUTE  WRK-SEIKYUGK-G2  ROUNDED = WRK-SEIKYUGK-G
      *
      *    保険限度額設定
           IF  RECE02-FTNMONEY(1)    >   ZERO  AND
               RECE02-FTNMONEY(1)    <   WRK-SEIKYUGK-G2
             MOVE  RECE02-FTNMONEY(1)    TO  WRK-SEIKYUGK-G2
           END-IF  
      *
      *    長期
           IF    FLG-TYOKI    =    1
               IF    WRK-TYOKIFTNGK-G  >=  10000
                   MOVE    1              TO    MF100-TYOKI
               END-IF
               IF  WRK-SEIKYUGK-G2    >=  10000
                   MOVE    10000          TO    WRK-SEIKYUGK-G2
               END-IF
           END-IF
      *
      *    長期上位
           IF    FLG-TYOKI    =    2
               IF    WRK-TYOKIFTNGK-G  >=  20000
                   MOVE    2              TO    MF100-TYOKI
               END-IF
               IF  WRK-SEIKYUGK-G2   >=  20000
                   MOVE    20000          TO    WRK-SEIKYUGK-G2
               END-IF
           END-IF
      *
      *    高額療養費を算定かつ分点時、市町村負担額は総点数に対しての
      *    高額療養費から保険単独時の窓口負担額を減算した値を求める
           IF  (MF100-KOGAKU    NOT =  ZERO)
           AND (MF100-TOTALTEN  NOT =  MF100-FUKUSITEN)
           AND (RECE02-FLG(4)  =  ZERO)
           AND (RECE02-FTNMONEY(1)  NOT =  ZERO)
               COMPUTE WRK-KEISAN = (MF100-TOTALTEN - MF100-FUKUSITEN)
                                    * RECE02-KYURATE / 10
               COMPUTE WRK-SEIKYUGK-G2  =  RECE10-FTNMONEY(1)
                                        -  WRK-KEISAN
           END-IF
      *
           ADD     WRK-SEIKYUGK-G2   TO    MF100-SEIKYUGK
      *
      *    保険単独分の高額療養費の上限額達成判定
           IF  MF100-KOGAKU  NOT =  ZERO
      *        保険単独の保険組合せ番号を取得
               PERFORM  950-HKNCOMBI-KEY6-READ-SEC
      *        収納から保険単独分の計算負担金集計
               IF  RECE02-NYUGAIKBN  =  "1"
                   MOVE    "key19"             TO  WRK-PATH2
               ELSE
                   MOVE    "key41"             TO  WRK-PATH2
               END-IF
               PERFORM  960-SYUNOU2-READ-SEC
      *
      *        保険単独が限度額に達した場合は印字対象外
               IF  WRK-HKNTANDOKU-KIN  =  140100
               OR  WRK-HKNTANDOKU-KIN  =  93000
               OR  WRK-HKNTANDOKU-KIN  =  44400
               OR  WRK-HKNTANDOKU-KIN  =  57600
               OR  WRK-HKNTANDOKU-KIN  =  35400
               OR  WRK-HKNTANDOKU-KIN  =  24600
                    MOVE    1                  TO  FLG-ERR
               END-IF
           END-IF
      *
      *    １件前の患者と同番号かつ異なる保険者番号の場合、
      *    月途中の保険変更患者とみなす
           IF  (MF100-PTNUM         =  WRK-SAV-PTNUM)
           AND (MF100-HKNJANUM  NOT =  WRK-SAV-HKNJANUM)
               ADD     1               TO  IDS
               MOVE    MF100-PTNUM     TO  WRK-HKNCHG-PTNUM(IDS)
      *        適用開始日が後の方を保険変更日とする
               IF  MF100-TEKSTYMD  >  WRK-SAV-TEKSTYMD
                   MOVE    MF100-TEKSTYMD
                                       TO  WRK-HKNCHG-TEKSTYMD(IDS)
               ELSE
                   MOVE    WRK-SAV-TEKSTYMD
                                       TO  WRK-HKNCHG-TEKSTYMD(IDS)
               END-IF
           END-IF
      *    患者番号と保険者番号保存
           MOVE    MF100-PTNUM         TO  WRK-SAV-PTNUM
           MOVE    MF100-HKNJANUM      TO  WRK-SAV-HKNJANUM
           MOVE    MF100-TEKSTYMD      TO  WRK-SAV-TEKSTYMD
      *
           MOVE     MF100-CITYHKNJANUM TO    WRK-KEY-CITYHKNJANUM
      *
           IF    FLG-ERR              =    ZERO
            IF      FLG-MF100  =  1
              WRITE    MF100-REC
              ADD      1        TO    CNT-MF100
      *
      *    合計用ファイル書き出し
             INITIALIZE  MF100-REC
             MOVE    SPA-HOSPNUM           TO  MF100-HOSPNUM
             MOVE    WRK-KEY-CITYHKNJANUM  TO  MF100-CITYHKNJANUM
      *    TMPファイル読み込み
             PERFORM  800-SKTTMP-READ-SEC
             ADD     1                     TO  MF100-GKENSU
      *
             IF      FLG-MF100  =  1
                     WRITE    MF100-REC
             ELSE
                     REWRITE  MF100-REC
             END-IF
            ELSE
                REWRITE  MF100-REC
            END-IF
           ELSE
             DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                     "点数="                RECE02-TOTALTEN(1)
                     "一部負担金"           MF100-FTNMONEY
                     "診療年月="            MF100-SRYYM
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    市町村保険者番号設定処理
      *****************************************************************
       2001-CITYNUM-SEC   SECTION.
      *
           MOVE    ZERO    TO    FLG-PASS
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >   100  OR
                    FLG-PASS      =   1
             IF WRK-KOHJKYSNUM(2:2)   =  CITY-NUM(IDX)
               MOVE    1    TO    FLG-PASS
               MOVE    CITY-HKNJA(IDX)    TO  WRK-CITYHKNJANUM
             END-IF
           END-PERFORM
      *
      *    市長村合併対応(赤羽町は田原市に統合する。)
           IF  WRK-CITYHKNJANUM  =  "23107"
               MOVE  "23106"    TO  WRK-CITYHKNJANUM
           END-IF
      *
      *    七宝町、美和町、甚目寺町はあま市に統合する
           IF  WRK-CITYHKNJANUM  =  "23066" OR "23067" OR "23068"
               MOVE  "23034"    TO  WRK-CITYHKNJANUM
           END-IF
      *
      *    保険番号185,285で「00」の場合は豊橋市
           IF  (WRK-KOHNUM  =  "185" OR "285")
           AND (WRK-KOHJKYSNUM(2:2)  =  "00")
               MOVE  "23002"    TO  WRK-CITYHKNJANUM
           END-IF
      *
      *    受給者番号の2桁目が「9」の場合は岡崎市
           IF  WRK-KOHJKYSNUM(2:1)  =  "9"
             MOVE    "23003"    TO  WRK-CITYHKNJANUM
           END-IF
      *
      *    名古屋用保険者番号設定
           IF  WRK-KOHJKYSNUM(1:1)  =  "0" OR "1" OR "6" OR "7"
             MOVE    "23400"    TO    WRK-CITYHKNJANUM
           END-IF 
      *
           MOVE  WRK-CITYHKNJANUM(3:3)      TO  WRK-KOBETUCITYNUM
      *
            .
       2001-CITYNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "23"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
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
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-MF100-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
               MOVE  KEY-NEW  TO  KEY-OLD
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
      *
           END-PERFORM
      *
           CLOSE    MF100-FILE
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
           ADD      1         TO        CNT-PAGE
                                        CNT-PAGE2
           INITIALIZE  SC09
      *
      *    印刷日付
           MOVE  WRK-SRYYMWH            TO  SC09-PRTYM
      *
      *    請求日付
           MOVE  WRK-SEIYMDWH           TO  SC09-SEIYMD
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCD     TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT       TO  SC09-HOSPCD
      *
      *    住所
           MOVE  WRK-HOSPADRS           TO  SC09-ADRS
           INSPECT SC09-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME           TO  SC09-HOSPNAME
           INSPECT SC09-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    管理者名
           MOVE    WRK-KAISETUNAME      TO  SC09-KAISETUNAME
      *
      *    電話番号
           MOVE  WRK-HOSPTEL            TO  SC09-TEL
      *
      *    市町村名
           MOVE  MF100-CITYHKNJANUM   TO  WRK-HKNJANUM
           PERFORM 920-HKNJAINF-SET-SEC
           MOVE  HKNJA-HKNJANAME        TO  SC09-CITYNAME
      *
      *    合計件数・合計ページ数
           IF    MF100-SKBFLG  =  ZERO
             MOVE    MF100-GKENSU     TO  WRK-GKENSU
             DIVIDE  MF100-GKENSU     BY  25
                     GIVING               WRK-GPAGE
                     REMAINDER            WRK-AMARI
             IF  WRK-AMARI  NOT  =  ZERO
               ADD    1               TO  WRK-GPAGE
             END-IF
      *    レセプト明細読込
             PERFORM 900-MF100-READ-SEC
             MOVE     1              TO  CNT-PAGE2
           END-IF
      *
           MOVE    WRK-GKENSU        TO  WRK-Z5
           MOVE    WRK-Z5            TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT    TO  SC09-GKENSU
      *
           MOVE    WRK-GPAGE         TO  WRK-Z3
           MOVE    WRK-Z3            TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT    TO  SC09-GPAGE
      *
      *    ページ数
           MOVE    CNT-PAGE2         TO  WRK-Z3
           MOVE    WRK-Z3            TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT    TO  SC09-PAGE
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
           INITIALIZE  WRK-SYOKEI-AREA
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     25 OR
                    FLG-PRINT     =     1  OR
                    KEY-NEW  NOT  =  KEY-OLD
      *    受給資格証記号番号（受給者番号）
             MOVE  MF100-KOHJKYSNUM        TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT        TO  SC09-JKYSNUM (IDX)
      *    氏名
             MOVE  MF100-NAME        TO  SC09-NAME (IDX)
      *    総点数
             MOVE  MF100-TOTALTEN    TO  WRK-Z7
             MOVE  WRK-Z7            TO  SC09-TENSU (IDX)
      *    結核精神点数
             IF  MF100-ZENKOHNUM          =   "010"
               MOVE  MF100-ZENKOHTENSU TO  WRK-KESE
             ELSE
               MOVE  0                 TO  WRK-KESE
             END-IF
             MOVE  WRK-KESE            TO  WRK-Z7
             MOVE  WRK-Z7            TO  SC09-KESE(IDX)
      *    給付割合
             EVALUATE  MF100-KYURATE
             WHEN 10
               MOVE  "○"            TO  SC09-KYURATE2 (IDX)
             WHEN 20
               MOVE  "○"            TO  SC09-KYURATE1 (IDX)
             END-EVALUATE
      *    市町村負担額
           DISPLAY "PTNUM =" MF100-PTNUM
           DISPLAY "市町村負担額 =" MF100-SEIKYUGK
           DISPLAY "総点数 =" MF100-TOTALTEN
           DISPLAY "福祉点数 =" MF100-FUKUSITEN
           IF  (MF100-JKYSNUM(1:3)  =  WRK-CONS-ICHINOMIYA1
                                   OR  WRK-CONS-ICHINOMIYA2
                                   OR  WRK-CONS-KITANAGOYA1
                                   OR  WRK-CONS-KITANAGOYA2
                                   OR  WRK-CONS-INUYAMA1
                                   OR  WRK-CONS-INUYAMA2
                                   OR  WRK-CONS-KOUNAN1
                                   OR  WRK-CONS-KOUNAN2
                                   OR  WRK-CONS-OOGUCHI1
                                   OR  WRK-CONS-OOGUCHI2
                                   OR  WRK-CONS-FUSOU1
                                   OR  WRK-CONS-FUSOU2)
           AND (MF100-FTNMONEY  =  ZERO)
           AND (MF100-TOTALTEN  =  MF100-FUKUSITEN)
           AND (MF100-ZENKOHNUM  =  SPACE)
             MOVE  ZERO              TO  WRK-KOHFTN
             MOVE  SPACE             TO  SC09-KOHFTN(IDX)
           ELSE
             MOVE  MF100-SEIKYUGK    TO  WRK-KOHFTN
                                         WRK-Z9
             MOVE  WRK-Z9            TO  SC09-KOHFTN(IDX)
           END-IF
      *
      *    備考
             IF    MF100-TYOKI      =   1
               MOVE    "長"       TO  SC09-BIKO1(IDX)
             END-IF
      *
             IF    MF100-TYOKI      =   2
               MOVE    "長２"     TO  SC09-BIKO1(IDX)
             END-IF
      *
             IF    MF100-ZENKOHNUM    NOT =  SPACE
               IF  SC09-BIKO1(IDX)  NOT =  SPACE
                 MOVE    SC09-BIKO1(IDX) TO  WRK-BIKO
                 STRING    WRK-BIKO     DELIMITED BY SPACE
                           ","          DELIMITED BY SIZE
                           INTO         SC09-BIKO1(IDX)
                 END-STRING
               END-IF
               IF    MF100-ZENKOHNUM  =   "015"
                                      OR  "016"
                                      OR  "021"
                 MOVE    MF100-ZENKOHTENSU   TO  WRK-JTENSU
                 MOVE    SC09-BIKO1(IDX)     TO  WRK-BIKO
                 STRING    WRK-BIKO     DELIMITED BY SPACE
                           "自立"       DELIMITED BY SIZE
                           WRK-JTENSU   DELIMITED BY SIZE
                           "点"         DELIMITED BY SIZE
                           INTO         SC09-BIKO1(IDX)
                 END-STRING
               ELSE
      *          保険番号マスター検索
                 PERFORM 930-HKNNUM-READ-SEC
                 IF      FLG-HKNNUM      NOT =   ZERO
                     INITIALIZE              HKNNUM-REC
                 END-IF
                 MOVE    HKN-TANSEIDONAME(1:6)  TO  WRK-TANSEIDO
                 IF    MF100-ZENKOHNUM    NOT =   "051"
                   MOVE    MF100-ZENKOHTENSU   TO  WRK-JTENSU
                   MOVE    SC09-BIKO1(IDX)     TO  WRK-BIKO
                   STRING    WRK-BIKO      DELIMITED BY SPACE
                             WRK-TANSEIDO  DELIMITED BY SPACE
                             " "           DELIMITED BY SIZE
                             WRK-JTENSU    DELIMITED BY SIZE
                             "点"          DELIMITED BY SIZE
                             INTO          SC09-BIKO1(IDX)
                   END-STRING
                 ELSE
                   MOVE    MF100-ZENKOHTENSU   TO  WRK-JTENSU
                   MOVE    SC09-BIKO1(IDX)     TO  WRK-BIKO
                   STRING    WRK-BIKO      DELIMITED BY SPACE
                             "特疾"        DELIMITED BY SPACE
                             " "           DELIMITED BY SIZE
                             WRK-JTENSU    DELIMITED BY SIZE
                             "点"          DELIMITED BY SIZE
                             INTO          SC09-BIKO1(IDX)
                   END-STRING
                 END-IF
               END-IF
             END-IF
      *
      *
             IF    LNK-PRTKANRI-SRYYM  NOT  =  MF100-SRYYM
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY1-AREA
               MOVE    "11"            TO  LNK-DAY1-IRAI
               MOVE    MF100-SRYYM     TO  LNK-DAY1-YMD(1:6)
               MOVE    "01"            TO  LNK-DAY1-YMD(7:2)
               CALL    "ORCSDAY"       USING  STS-AREA-DAY
                                              LNK-DAY1-AREA
      *
               STRING  LNK-DAY1-GENKIGO    DELIMITED BY SIZE
                       LNK-DAY1-YMD(3:2)   DELIMITED BY SIZE
                       "."                 DELIMITED BY SIZE
                       LNK-DAY1-YMD(5:2)   DELIMITED BY SIZE
                       INTO  WRK-HENYMDG
               END-STRING
               IF  SC09-BIKO2(IDX)  =  SPACE
                 MOVE  WRK-HENYMDG(1:6)  TO  SC09-BIKO2(IDX)
               ELSE
                 MOVE    SC09-BIKO2(IDX) TO  WRK-BIKO
                 STRING    WRK-BIKO     DELIMITED BY SPACE
                           ","          DELIMITED BY SIZE
                           WRK-HENYMDG(1:6)
                                        DELIMITED BY SIZE
                           INTO         SC09-BIKO2(IDX)
                 END-STRING
               END-IF
             END-IF
      *
             IF    MF100-HKNNUM     =  "063" OR "072" OR "073" OR
                                       "074" OR "075"
               IF  SC09-BIKO2(IDX)  =  SPACE
                 MOVE  "特退"           TO  SC09-BIKO2(IDX)
               ELSE
                 MOVE    SC09-BIKO2(IDX) TO  WRK-BIKO
                 STRING    WRK-BIKO     DELIMITED BY SPACE
                           ","          DELIMITED BY SIZE
                           "特退"       DELIMITED BY SIZE
                           INTO         SC09-BIKO2(IDX)
                 END-STRING
               END-IF
             ELSE
               PERFORM  3201-KOKUHO-HEN-SEC
             END-IF
      *
             IF  MF100-KOGAKU  NOT =  ZERO
               INITIALIZE  WRK-BIKO2
               IF  MF100-SRYYM  >=  WRK-CONS-SRYYM-201501
                 EVALUATE  MF100-KOGAKU
                   WHEN  2
                     MOVE  "２６区ア"    TO  WRK-BIKO2
                   WHEN  3
                     MOVE  "２７区イ"    TO  WRK-BIKO2
                   WHEN  4
                     MOVE  "２８区ウ"    TO  WRK-BIKO2
                   WHEN  5
                     MOVE  "２９区エ"    TO  WRK-BIKO2
                   WHEN  6
                     MOVE  "３０区オ"    TO  WRK-BIKO2
               END-EVALUATE
                 EVALUATE  MF100-TOKKI
                   WHEN  "31"
                     MOVE  "３１多ア"    TO  WRK-BIKO2
                   WHEN  "32"
                     MOVE  "３２多イ"    TO  WRK-BIKO2
                   WHEN  "33"
                     MOVE  "３３多ウ"    TO  WRK-BIKO2
                   WHEN  "34"
                     MOVE  "３４多エ"    TO  WRK-BIKO2
                   WHEN  "35"
                     MOVE  "３５多オ"    TO  WRK-BIKO2
                 END-EVALUATE
      *
               ELSE
                   EVALUATE  MF100-KOGAKU
                     WHEN  2
                       MOVE  "１７・上位"    TO  WRK-BIKO2
                     WHEN  3
                       MOVE  "１８・一般"    TO  WRK-BIKO2
                     WHEN  4
                       MOVE  "１９・低所"    TO  WRK-BIKO2
                   END-EVALUATE
               END-IF
      *
               IF  SC09-BIKO1(IDX)  =  SPACE
                 MOVE  WRK-BIKO2        TO  SC09-BIKO1(IDX)
               ELSE
                 IF  SC09-BIKO2(IDX)  =  SPACE
                   MOVE  WRK-BIKO2      TO  SC09-BIKO2(IDX)
                 ELSE
                   IF  SC09-BIKO3(IDX)  =  SPACE
                     MOVE  WRK-BIKO2      TO  SC09-BIKO3(IDX)
                   END-IF
                 END-IF
               END-IF
      *
             END-IF
      *
      *      他市町村にも点数がある場合、備考にその市町村と点数を記載
             IF  MF100-TACITYHKNJANUM  NOT =  SPACE
             AND MF100-TAKOHTEN        NOT =  ZERO
               INITIALIZE                           WRK-BIKO
                                                    WRK-HKNJANAME
               MOVE    MF100-TACITYHKNJANUM     TO  WRK-HKNJANUM
               MOVE    MF100-TAKOHTEN           TO  WRK-Z6-2
               MOVE    WRK-Z6-2                 TO  WRK-TENSU
               PERFORM 920-HKNJAINF-SET-SEC
      *        "（）"が付く市町村名は括弧を除去
               STRING    HKNJA-HKNJANAME  DELIMITED BY "（"
                         INTO             WRK-HKNJANAME
               END-STRING
      *        文字結合
               STRING    WRK-HKNJANAME    DELIMITED BY SPACE
                         WRK-TENSU        DELIMITED BY SIZE
                         "点"             DELIMITED BY SIZE
                         INTO             WRK-BIKO
               END-STRING
      *
               IF  SC09-BIKO3(IDX)  =  SPACE
                 MOVE  WRK-BIKO        TO  SC09-BIKO3(IDX)
               ELSE
                 IF  SC09-BIKO2(IDX)  =  SPACE
                   MOVE  WRK-BIKO      TO  SC09-BIKO2(IDX)
                 ELSE
                   MOVE  WRK-BIKO      TO  SC09-BIKO1(IDX)
                 END-IF
               END-IF
             END-IF
      *
             IF (MF100-TOTALTEN  >  MF100-FUKUSITEN)
               INITIALIZE                           WRK-BIKO3
               MOVE    MF100-FUKUSITEN   TO  WRK-Z8
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2    >    9
                   IF  WRK-Z8(IDX2:1)  NOT =  SPACE
                       MOVE    WRK-Z8(IDX2: )      TO  WRK-TAISYOTENSU2
                       MOVE    9                   TO  IDX2
                   END-IF
               END-PERFORM
      *
               EVALUATE  MF100-KOHNUM(2:2)
                   WHEN  "81"
                         MOVE    "子ども"   TO  WRK-TAISYONAME
                   WHEN  "82"
                         MOVE    "障害"     TO  WRK-TAISYONAME
                   WHEN  "83"
                         MOVE    "母子"     TO  WRK-TAISYONAME
                   WHEN  "85"
                         MOVE    "精障"     TO  WRK-TAISYONAME
               END-EVALUATE
               STRING    WRK-TAISYONAME    DELIMITED BY SPACE
                         " "               DELIMITED BY SIZE
                         WRK-TAISYOTENSU2  DELIMITED BY SPACE
                         "点"              DELIMITED BY SIZE
                         INTO              WRK-BIKO3
               END-STRING
      *
               IF  SC09-BIKO1(IDX)  =  SPACE
                 MOVE  WRK-BIKO3        TO  SC09-BIKO1(IDX)
               ELSE
                 IF  SC09-BIKO2(IDX)  =  SPACE
                   MOVE  WRK-BIKO3      TO  SC09-BIKO2(IDX)
                 ELSE
                   IF  SC09-BIKO3(IDX)  =  SPACE
                     MOVE  WRK-BIKO3      TO  SC09-BIKO3(IDX)
                   END-IF
                 END-IF
               END-IF
      *
             END-IF
      *
             INITIALIZE                    WRK-BIKO4
             PERFORM  VARYING  IDX3  FROM  1  BY  1
                      UNTIL    IDX3    >    100
                      OR       WRK-HKNCHG-PTNUM(IDX3)  =  SPACE
                IF  MF100-PTNUM  =  WRK-HKNCHG-PTNUM(IDX3)
                    INITIALIZE                  STS-AREA-DAY
                                                LNK-DAY1-AREA
                    MOVE    "11"            TO  LNK-DAY1-IRAI
                    MOVE    WRK-HKNCHG-TEKSTYMD(IDX3)
                                            TO  LNK-DAY1-YMD
                    CALL    "ORCSDAY"       USING  STS-AREA-DAY
                                                   LNK-DAY1-AREA
      *
                    STRING  LNK-DAY1-GENKIGO    DELIMITED BY SIZE
                            LNK-DAY1-YMD(3:2)   DELIMITED BY SIZE
                            "."                 DELIMITED BY SIZE
                            LNK-DAY1-YMD(5:2)   DELIMITED BY SIZE
                            "."                 DELIMITED BY SIZE
                            LNK-DAY1-YMD(7:2)   DELIMITED BY SIZE
                            " 保険変更"         DELIMITED BY SIZE
                            INTO  WRK-BIKO4
                    END-STRING
                    MOVE    100             TO  IDX3
                END-IF
             END-PERFORM
      *
             IF  SC09-BIKO1(IDX)  =  SPACE
               MOVE  WRK-BIKO4             TO  SC09-BIKO1(IDX)
             ELSE
               IF  SC09-BIKO2(IDX)  =  SPACE
                 MOVE  WRK-BIKO4           TO  SC09-BIKO2(IDX)
               ELSE
                 IF  SC09-BIKO3(IDX)  =  SPACE
                   MOVE  WRK-BIKO4         TO  SC09-BIKO3(IDX)
                 END-IF
               END-IF
             END-IF
      *
      *    小計加算
             ADD  1                  TO  WRK-S-KENSU
             ADD  MF100-TOTALTEN     TO  WRK-S-TENSU
             ADD  WRK-KESE           TO  WRK-S-KESE
             ADD  MF100-SEIKYUGK     TO  WRK-S-SEIKYUGK
             ADD  WRK-KOHFTN         TO  WRK-S-KOHFTN
      *
      *    レセプト明細読込
             PERFORM 900-MF100-READ-SEC
           END-PERFORM
      *
      *    小計印字
           PERFORM  3201-BODY-HEN-SEC
      *
           IF    KEY-NEW  NOT  =  KEY-OLD
             MOVE    ZERO    TO  WRK-GPAGE
                                 WRK-PAGE
                                 WRK-GKENSU
           END-IF
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保編集処理
      *****************************************************************
       3201-KOKUHO-HEN-SEC      SECTION.
      *
           EVALUATE  MF100-HKNNUM
           WHEN  "060"
               IF    MF100-HKNJANUM(3:1)  =  "3"
                 MOVE  MF100-HKNJANUM(1:5)  TO  WRK-HKNJANUM
                 PERFORM 920-HKNJAINF-SET-SEC
                 MOVE  HKNJA-HKNJANAME  TO  SC09-BIKO3(IDX)
               ELSE
                 IF    MF100-HKNJANUM(1:5) NOT = 
                                            MF100-CITYHKNJANUM
                   IF    SC09-BIKO2(IDX)  =  SPACE
                         MOVE    "特例"     TO  SC09-BIKO2(IDX)
                   ELSE
                         MOVE    SC09-BIKO2(IDX) TO  WRK-BIKO
                         STRING    WRK-BIKO     DELIMITED BY SPACE
                                   ","          DELIMITED BY SIZE
                                   "特例"       DELIMITED BY SIZE
                                   INTO         SC09-BIKO2(IDX)
                         END-STRING
                   END-IF
                   MOVE  MF100-HKNJANUM(1:5)  TO  WRK-HKNJANUM
                   PERFORM 920-HKNJAINF-SET-SEC
                   MOVE  HKNJA-HKNJANAME  TO  SC09-BIKO3(IDX)
                 END-IF
               END-IF
           WHEN  "067"
               IF    MF100-HKNJANUM(3:5) NOT = 
                                          MF100-CITYHKNJANUM
                 IF    SC09-BIKO2(IDX)  =  SPACE
                       MOVE    "特例"     TO  SC09-BIKO2(IDX)
                 ELSE
                       MOVE    SC09-BIKO2(IDX) TO  WRK-BIKO
                       STRING    WRK-BIKO     DELIMITED BY SPACE
                                 ","          DELIMITED BY SIZE
                                 "特例"       DELIMITED BY SIZE
                                 INTO         SC09-BIKO2(IDX)
                       END-STRING
                 MOVE  MF100-HKNJANUM(3:5)  TO  WRK-HKNJANUM
                 PERFORM 920-HKNJAINF-SET-SEC
                 MOVE  HKNJA-HKNJANAME      TO  SC09-BIKO3(IDX)
                 END-IF
               END-IF
           END-EVALUATE
      *
           .
       3201-KOKUHO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ページ計>処理
      *****************************************************************
       3201-BODY-HEN-SEC      SECTION.
      *
      *    件数
           MOVE    WRK-S-KENSU        TO  WRK-Z2
           MOVE    WRK-Z2             TO  SC09-SKENSU
      *    点数
           MOVE    WRK-S-TENSU        TO  WRK-Z9
           MOVE    WRK-Z9             TO  SC09-GTENSU
      *    結核・精神点数
           MOVE    WRK-S-KESE         TO  WRK-Z9
           MOVE    WRK-Z9             TO  SC09-GKESE
      *    市町村負担額
           MOVE    WRK-S-SEIKYUGK     TO  WRK-Z9
           MOVE    WRK-Z9             TO  SC09-GKOHFTN
      *    合計金額
           MOVE    WRK-S-SEIKYUGK     TO  WRK-Z11
           MOVE    WRK-Z11            TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT     TO  SC09-GKINGAKU
      *
           .
       3201-BODY-HEN-EXT.
           EXIT.
      *
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
           MOVE   "SC2309.red"            TO  SPRT-PRTID
           MOVE    "外来用・乳障母戦医療費請求書"
                                    TO  SPRT-TITLE
           MOVE    SC09             TO  SPRT-PRTDATA
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
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       TAISYO-KOHNUM(IDXX)  =  SPACE
             IF  TBL-HKOHNUM(1)  =  TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(2)  =  TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(3)  =  TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(4)  =  TAISYO-KOHNUM(IDXX)
               MOVE     1                   TO  FLG-TAISYO
               MOVE     100                 TO  IDXX
             END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO     TO     FLG-HEIYOU
                                   WRK-SEIKYUGK
                                   WRK-TYOKIFTNGK
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF    SYU-KOH-HKNNUM(IDX)(1:1) =  "0"  AND
                   SYU-KOH-HKNNUM(IDX) NOT  =  "027"
               MOVE   1                TO    FLG-HEIYOU
      *
               MOVE    SYU-KOH-HKNNUM(IDX) TO    MF100-ZENKOHNUM
               ADD     SYU-TOTAL-HKNTEN    TO    MF100-ZENKOHTENSU
             END-IF
      *
             IF    SYU-KOH-HKNNUM(IDX)   =   "972"
               MOVE    1     TO    FLG-TYOKI
               MOVE    SYU-KOH-COMPFTN(IDX)  TO  WRK-TYOKIFTNGK
             END-IF
      *
             IF    SYU-KOH-HKNNUM(IDX)   =   "974"
               MOVE    2     TO    FLG-TYOKI
               MOVE    SYU-KOH-COMPFTN(IDX)  TO  WRK-TYOKIFTNGK
             END-IF
      *
             IF  SYU-KOH-HKNNUM(IDX)  =  WRK-KOHNUM
               IF  RECE02-FTNMONEY(1)  =  ZERO
                 IF  IDX  =  1
                   COMPUTE  WRK-SEIKYUGK  =  
                            SYU-SYUCOMPFTN-ENTANI  -
                            SYU-KOH-COMPFTN-ENTANI(1)
                 ELSE
                   COMPUTE  WRK-SEIKYUGK  =  
                            SYU-KOH-COMPFTN-ENTANI(IDX - 1)  -
                            SYU-KOH-COMPFTN-ENTANI(IDX)
                 END-IF
               ELSE
                 IF  IDX  =  1
                   COMPUTE  WRK-SEIKYUGK  =  
                            SYU-SYUCOMPFTN  -  SYU-KOH-COMPFTN(1)
                 ELSE
                   COMPUTE  WRK-SEIKYUGK  =  
                            SYU-KOH-COMPFTN(IDX - 1)  -
                            SYU-KOH-COMPFTN(IDX)
                 END-IF
               END-IF
             END-IF
      *
      *    計算負担金に差が有る場合、地方公費の助成があったと判定する
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
           END-PERFORM
      *
           ADD    WRK-SEIKYUGK                  TO    WRK-SEIKYUGK-G
           ADD    WRK-TYOKIFTNGK             TO  WRK-TYOKIFTNGK-G
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
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM (1)
                                          SYU-KOH-HKNNUM (2)
                                          SYU-KOH-HKNNUM (3)
                                          SYU-KOH-HKNNUM (4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE    "tbl_syunou"     TO  MCP-TABLE
             MOVE    WRK-PATH         TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    WRK-PATH       TO  MCP-PATHNAME
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
                     IF  COMB-KOHID (1)  =  WRK-KOHID   OR
                         COMB-KOHID (2)  =  WRK-KOHID   OR
                         COMB-KOHID (3)  =  WRK-KOHID   OR
                         COMB-KOHID (4)  =  WRK-KOHID
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
                 MOVE    "tbl_syunou"   TO  MCP-TABLE
                 MOVE    WRK-PATH       TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
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
           CALL  "ORCDBMAIN"    USING
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU2307 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU2307 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU2307 END ***"
      *
           IF  CNT-MF100  =  0
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
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-MF100-READ-SEC  SECTION.
      *
           READ  MF100-FILE  NEXT
             AT  END
               MOVE  HIGH-VALUE          TO  KEY-NEW
               MOVE  1                   TO  FLG-PRINT
             NOT AT  END
               ADD   1                   TO  CNT-MF100
               MOVE  MF100-CITYHKNJANUM  TO  KEY-N-CITYNUM
           END-READ
           .
       900-MF100-READ-EXT.
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
               INITIALIZE                            TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(1)          TO  TBL-HKOHNUM(1)
               MOVE    RECE02-KOHNUM(2)          TO  TBL-HKOHNUM(2)
               MOVE    RECE02-KOHNUM(3)          TO  TBL-HKOHNUM(3)
               MOVE    RECE02-KOHNUM(4)          TO  TBL-HKOHNUM(4)
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
      *        外来データ以外は読み飛ばす
                   IF    RECE02-NYUGAIKBN  NOT = "2"
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
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC  SECTION.
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
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
           MOVE    "tbl_hkncombi"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
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
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM                  TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                 TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                 TO  MCPDATA-REC
           MOVE  "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"       TO  MCP-TABLE
             MOVE    "key2"               TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
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
           MOVE  "DBFETCH"                   TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
      *
      *    国保でない時は、読み直す
             IF    HKNJA-HKNNUM  NOT  =  "060" AND "067"
               GO               TO    925-HKNJAINF-READ-SEC
             END-IF
      *
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1            TO  FLG-HKNJAINF
             DISPLAY "フェッチ失敗"  WRK-HKNJANUM
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY          TO  RECE10-KEY
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
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
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
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
           MOVE  "DBCLOSECURSOR"                 TO  MCP-FUNC
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
      *    保険番号マスター読み込み
      *****************************************************************
       930-HKNNUM-READ-SEC         SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    MF100-HOSPNUM       TO  HKN-HOSPNUM
           MOVE    MF100-ZENKOHNUM     TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  HKN-TEKSTYMD
           MOVE    "01"                TO  HKN-TEKSTYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  HKN-TEKEDYMD
           MOVE    "01"                TO  HKN-TEKEDYMD(7:2)
           MOVE    "00"                TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       930-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み（key6）
      *****************************************************************
       950-HKNCOMBI-KEY6-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
                                           WRK-HKNTANDOKU
      *
           MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
           MOVE    MF100-PTID              TO  COMB-PTID
           MOVE    LNK-PRTKANRI-SRYYM      TO  COMB-TEKSTYMD
                                               COMB-TEKEDYMD
           MOVE    "01"                    TO  COMB-TEKSTYMD(7:2)
                                               COMB-TEKEDYMD(7:2)
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key6"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM  955-HKNCOMBI-KEY6-FETCH-SEC
               PERFORM  UNTIL  FLG-HKNCOMBI  =  1
      *            保険単独の組み合わせを探す
                   IF  COMB-KOHID (1)  =  ZERO
                   AND COMB-KOHID (2)  =  ZERO
                   AND COMB-KOHID (3)  =  ZERO
                   AND COMB-KOHID (4)  =  ZERO
                       MOVE    COMB-HKNCOMBINUM  TO  WRK-HKNTANDOKU
                       MOVE    1                 TO  FLG-HKNCOMBI
                   END-IF
                   PERFORM  955-HKNCOMBI-KEY6-FETCH-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key6"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       950-HKNCOMBI-KEY6-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスターFETCH（key6）
      *****************************************************************
       955-HKNCOMBI-KEY6-FETCH-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key6"                  TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       955-HKNCOMBI-KEY6-FETCH-EXT.
           EXIT.
      *****************************************************************
      *  収納情報読み込み
      *****************************************************************
       960-SYUNOU2-READ-SEC  SECTION.
      *
           INITIALIZE                      SYUNOU2-REC
                                           FLG-SYUNOU2
                                           WRK-HKNTANDOKU-KIN
      *
           MOVE  SPA-HOSPNUM               TO  SYU2-HOSPNUM
           MOVE  RECE02-NYUGAIKBN          TO  SYU2-NYUGAIKBN
           MOVE  RECE02-PTID               TO  SYU2-PTID
           MOVE  RECE02-SRYYM(1:6)         TO  SYU2-SRYYMD(1:6)
           MOVE  "__"                      TO  SYU2-SRYYMD(7:2)
           MOVE  WRK-HKNTANDOKU            TO  SYU2-HKNCOMBINUM
           MOVE  SYUNOU2-REC               TO  MCPDATA-REC
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE  "tbl_syunou"              TO  MCP-TABLE
           MOVE  WRK-PATH2                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"               USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  965-SYUNOU2-FETCH-SEC
             PERFORM  UNTIL  FLG-SYUNOU2  =  1
      *          主保険の計算負担金集計
                 ADD  SYU2-SYUCOMPFTN-ENTANI  TO  WRK-HKNTANDOKU-KIN
      *
                 PERFORM  965-SYUNOU2-FETCH-SEC
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
       960-SYUNOU2-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       965-SYUNOU2-FETCH-SEC  SECTION.
      *
           MOVE    "tbl_syunou"            TO  MCP-TABLE
           MOVE    WRK-PATH2               TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC         TO  SYUNOU2-REC
               MOVE    ZERO                TO  FLG-SYUNOU2
           ELSE
               INITIALIZE                  SYUNOU2-REC
               MOVE    1                   TO  FLG-SYUNOU2
           END-IF
      *
           .
       965-SYUNOU2-FETCH-EXT.
           EXIT.
      *
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
       900-DBCLOSE-EXT.
           EXIT.
      *
