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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             SOKATU1410.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/26    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            (返戻対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01    有村    05/05/27    負担者番号を持たない障害公費が
      *                                  請求管理情報の記載外公費に
      *                                  設定される場合の対応追加
      *  01.00.02    大貫    06/01/06    老人２割の保険番号１４２が集計
      *                                  されないバグを修正
      *  01.00.03    呉秀凡  06/01/13    １８８と１８９併用患者の再掲(特記事項)
      *  01.00.04    内田    06/10/31    老人の集計を修正
      *                                  長期９７４の追加
      *  01.00.05    内田    06/11/14    帳票様式の変更
      *  01.00.06    内田    07/03/28    帳票様式の変更にともなう修正
      *                                  経過措置は上限に達したら印字
      *  01.00.07    内田    07/04/22    対象保険番号の追加（289）
      *  01.00.08    内田    07/06/22    帳票様式の変更にともなう修正
      *  01.00.09    内田    07/08/19    「障施」を正しく印字するよう修正
      *  01.00.10    内田    07/10/08    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.11    内田    08/04/27    ４月改正に伴う修正
      *                                  SPA-AREA初期化
      *  01.00.12    内田    08/05/20    帳票様式の変更に伴う修正
      *  01.00.13    内田    08/06/10    公費再掲欄「肝炎」の印字修正
      *                                  公費再掲欄「二割」の印字
      *  01.00.14    吉川    09/07/13    対象保険番号の追加（381,385）
      *                                  シス管「2007」の公費まとめ対応
      *  01.00.15    吉川    09/11/18    シス管「2007」のまとめ公費で
      *                                  6つ目以降がまとまらない不具合を修正
      *  01.00.16    吉川    10/08/04    オンライン返戻対応
      *                                  特別療養費国保対応
      *                                  相模原市の代表番号追加
      *  01.00.17    吉川    11/01/25    open-cobol1.0対応
      *  01.00.18    吉川    11/06/23    東北地方太平洋沖地震対応
      *  01.00.19    吉川    11/08/17    障害分は公費再掲欄に記載する
      *  01.00.20    吉川    12/05/16    在宅はまとめて集計するように修正
      *  01.00.21    吉川    12/09/18    対象公費番号をINIファイルから
      *                                  取得するようにする
      *  01.00.22    吉川    12/12/25    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.23    吉川    13/08/06    プログラムオプション対応
      *                                  MCPDATA-RECへの移行漏れ
      *  01.00.24    吉川    14/05/21    前期高齢者２割対応
      *  01.00.25    吉川    14/10/09    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.26    吉川    14/12/04    返戻時、再印刷画面のタイトル修正
      *  01.00.27    吉川    15/01/30    平成２７年１月改正対応
      *  01.00.28    萩野    15/12/10    医療機関編集情報への開設者名追加対応
      *  01.00.29    門間    17/10/17    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT1410TMP-FILE ASSIGN    SKT1410TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT1410TMP-KEY
                                   FILE    STATUS  IS  STS-SKT1410.
      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      *     SELECT  YUSEN-FILE    ASSIGN FILE-NAME
      *                           ORGANIZATION   IS  LINE SEQUENTIAL
      *                           ACCESS MODE    IS  SEQUENTIAL
      *                           FILE   STATUS  IS  STS-YUSEN.
      **
      **=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
      **    印字対象公費番号ファイル
      **=== プラグイン対応 by Yoshikawa(2012/12/25) start
      **     SELECT  TAISYOKOHI-FILE ASSIGN FILE-NAME
      *     SELECT  TAISYOKOHI-FILE ASSIGN FILE-NAME2
      **=== プラグイン対応 by Yoshikawa(2012/12/25) end
      *                             ORGANIZATION   IS  LINE SEQUENTIAL
      *                             ACCESS MODE    IS  SEQUENTIAL
      *                             FILE   STATUS  IS  STS-TAISYOKOHI.
      **=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求書一時ファイル  
       FD  SKT1410TMP-FILE.
           COPY    "CPSKT1410TMP.INC".
      *
006900*    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      **    優先保険者番号ファイル
      * FD  YUSEN-FILE.
      * 01  YUSEN-HKNJANUM             PIC X(06). 
      **
      **=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
      **    印字対象公費番号ファイル
      * FD  TAISYOKOHI-FILE.
      * 01  TAISYOKOHI-REC.
      *     03  TAISYO-DAT-KBN      PIC X(01).
      *     03  FILLER              PIC X(01).
      *     03  TAISYO-DAT-KOHNUM   PIC X(03). 
      *     03  TAISYO-DAT-KAIGYO   PIC X(01). 
      **=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求書一時ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT1410TP//.
      *
006900*    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
      *    エラーファイル 名称領域 
      *---(01.00.10) LINE ADD START ------------------------------------
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *     COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
      *                              BY         //RECEERR//.
           COPY    "CPRECEERR.INC".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *---(01.00.10) LINE ADD END   ------------------------------------
      *     
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
      *    対象公費番号データ
           COPY  "TAISYO1401.INC".
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "SKT1410.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT1410         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
           03  STS-TAISYOKOHI      PIC X(02).
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT1410TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-TYOKI           PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-KEIKA           PIC 9(01).
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
           03  FLG-TAISYOKOHI      PIC 9(01).
           03  FLG-TAISYO          PIC 9(01).
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) start
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) end
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
           03  IDX                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
           03  IDY                 PIC 9(04).
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *##  パラメタ領域（地方公費仕様）
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *---(01.00.10) LINE ADD START ------------------------------------
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *---(01.00.10) LINE ADD END   ------------------------------------
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
           03  WRK-PARA-TOKUBETU   PIC X(01).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      **=== プラグイン対応 by Yoshikawa(2012/12/25) start
      **    ファイル名称用エリア
      * 01  FILE-NAME.
      *     03  FILLER              PIC X(05)  VALUE "/tmp/".
      *     03  FILE-HOSPNUM        PIC 9(02).
      *     03  FILLER              PIC X(10)  VALUE "SOKATU1410".
      *     03  FILLER              PIC X(09)  VALUE "YUSEN.INI".
      **
      * 01  FILE-NAME2.
      *     03  FILLER              PIC X(05)  VALUE "/tmp/".
      *     03  FILE-HOSPNUM2       PIC 9(02).
      *     03  FILLER              PIC X(10)  VALUE "SOKATU1410".
      *     03  FILLER              PIC X(14)  VALUE "TAISYOKOHI.INI".
      **=== プラグイン対応 by Yoshikawa(2012/12/25) end
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *    ファイル名称用エリア
      *=== プラグイン対応 by Yoshikawa(2012/12/25) start
      *     03  FILE-NAME        PIC X(512).
      *=== プラグイン対応 by Yoshikawa(2012/12/25) end
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-PAYKBN          PIC 9(02).
           03  WRK-RATE            PIC 9(03).
           03  WRK-CNT             PIC 9(02).
           03  WRK-HKNJANUM        PIC X(06).
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) start
           03  WRK-KOHNAME         PIC X(12).
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) end
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
           03  WRK-HBTKBN          PIC X(01).
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13             PIC Z,ZZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
      *---(01.00.10) LINE ADD START ------------------------------------
      *     03  WRK-PATH.
      *         05  FILLER          PIC S9(09)  BINARY.
      *         05  FILLER          PIC S9(09)  BINARY.
      *         05  FILLER          PIC S9(09)  BINARY.
           03  WRK-PATH            PIC X(50).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
           03  WRK-PATH2           PIC X(50).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
           03  WRK-YUSEN           PIC X(06).
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *    医療機関情報
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
           03  WRK-KAISETUNAME       PIC X(80).
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) start
      *    公費番号まとめエリア
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  10.
           03  WRK-2007-MAX        PIC 9(03).
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) end
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
           03  CNT-KOH             PIC 9(02).
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *---(01.00.10) LINE ADD START ------------------------------------
      *     COPY    "ORCA-DBPATH".
           COPY    "COMMON-SPA".
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM              PIC X(03)  OCCURS  4.
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *
      *---(01.00.11) LINE ADD START ------------------------------------
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *---(01.00.11) LINE ADD END   ------------------------------------
      *
      *=== 災害対応 by Yoshikawa(2011/06/23) start
      *    災害用都道府県テーブル
       01  TBL-PREF-AREA.
           03  FILLER          PIC X(02)   VALUE   "03".
           03  FILLER          PIC X(08)   VALUE   "岩手県".
           03  FILLER          PIC X(02)   VALUE   "04".
           03  FILLER          PIC X(08)   VALUE   "宮城県".
           03  FILLER          PIC X(02)   VALUE   "07".
           03  FILLER          PIC X(08)   VALUE   "福島県".
           03  FILLER          PIC X(02)   VALUE   "08".
           03  FILLER          PIC X(08)   VALUE   "茨城県".
           03  FILLER          PIC X(02)   VALUE   "09".
           03  FILLER          PIC X(08)   VALUE   "栃木県".
           03  FILLER          PIC X(02)   VALUE   "12".
           03  FILLER          PIC X(08)   VALUE   "千葉県".
           03  FILLER          PIC X(02)   VALUE   "20".
           03  FILLER          PIC X(08)   VALUE   "長野県".
       01  TBL-PREF-R         REDEFINES   TBL-PREF-AREA.
           03  TBL-PREF       OCCURS   7.
               05  TBL-PREFNUM             PIC X(02).
               05  TBL-PREFNAME            PIC X(08).
       01  TBL-PREF-MAX                   PIC 9(02)   VALUE   7.
      *=== 災害対応 by Yoshikawa(2011/06/23) end
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
      *---(01.00.10) LINE ADD START ------------------------------------
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
      *
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) start
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) end
      *
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      * 
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      ******************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
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
           INITIALIZE                  CNT-AREA
      *---(01.00.11) LINE ADD START ------------------------------------
           INITIALIZE                  SPA-AREA
      *---(01.00.11) LINE ADD END   ------------------------------------
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
           INITIALIZE                  RECEERR
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
           DISPLAY  "START"
      *
           PERFORM 100-DBOPEN-SEC
      *
      *###（地方公費仕様）
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
      *
      *---(01.00.10) LINE ADD START ------------------------------------
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
                                               WRK-PARA-TOKUBETU
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *---(01.00.10) LINE ADD START ------------------------------------
                                               RECEERR
      *---(01.00.10) LINE ADD END   ------------------------------------
           END-UNSTRING
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU1410"    TO  JOB-PGID
           MOVE    "国保請求書"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *---(01.00.10) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *##（地方公費仕様：LNK-PRTKANRI-TERMID)
           MOVE    "SKT1410"           TO  SKT1410TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT1410TP-TERMID
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SKT1410TP-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
           MOVE  SKT1410TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT1410TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
      *###（地方公費仕様)
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
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
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM(1:6)
      *                                 TO  SYS-1001-STYUKYMD(1:6)
      *                                     SYS-1001-EDYUKYMD(1:6)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM(1:6)
      *                                 TO  SYS-1002-STYUKYMD(1:6)
      *                                     SYS-1002-EDYUKYMD(1:6)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1002-REC
               IF      SYS-1002-ADRS       =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1002-ADRS
               END-IF    
           ELSE
               MOVE    "医療機関情報（所在地）が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
      *
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) start
      *    公費番号まとめ情報
           INITIALIZE                      WRK-2007-AREA
      *
           INITIALIZE                      SYS-2007-REC
           MOVE    "2007"              TO  SYS-2007-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-2007-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2007-STYUKYMD (7:2)
           MOVE    SYS-2007-STYUKYMD   TO  SYS-2007-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2007-HOSPNUM
           MOVE    SYS-2007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX1
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX1           >=  100
               MOVE    MCPDATA-REC         TO  SYS-2007-REC
      *
               ADD     1                   TO  IDX1
               MOVE    SYS-2007-MEISYO(1) TO  WRK-2007-MEISYO(IDX1)
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       10
                       OR      SYS-2007-KOHNUM (IDX2)   =   SPACE
                   MOVE    SYS-2007-KOHNUM (IDX2)
                                           TO WRK-2007-KOHNUM(IDX1 IDX2)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    IDX1                    TO  WRK-2007-MAX
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) end
      *
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      *     PERFORM  110-INI-FILE-SEC
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
           OPEN    OUTPUT  UPD-FILE
      *  
           DISPLAY     "WRK-PARA-SYOKBN=" WRK-PARA-SYOKBN
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
      *     EVALUATE    WRK-PARA-SYOKBN 
      *         WHEN    "2"
      **---(01.00.10) LINE ADD START ------------------------------------
      **             MOVE    PATH-TBL-SEIKYU-KEY37    TO  WRK-PATH
      *             MOVE    "key37"                  TO  WRK-PATH
      **---(01.00.10) LINE ADD END   ------------------------------------
      *         WHEN    "1"
      **---(01.00.10) LINE ADD START ------------------------------------
      **             MOVE    PATH-TBL-SEIKYU-KEY38    TO  WRK-PATH
      *             MOVE    "key38"                  TO  WRK-PATH
      **---(01.00.10) LINE ADD END   ------------------------------------
      *     END-EVALUATE
           EVALUATE    WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
               WHEN   "2"               ALSO  "1"
                   MOVE    "key37"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
           END-EVALUATE
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *  
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      ******************************************************************
      **    優先保険者番号INIファイル読み込み処理 
      ******************************************************************
      * 110-INI-FILE-SEC                SECTION.
      **
      **    ファイルディレクトリ位置指定
      **=== プラグイン対応 by Yoshikawa(2012/12/25) start
      **     INITIALIZE           ORCSMKPASSAREA
      **     MOVE  "data/YUSEN.INI"     TO  MKPASS-IN-01
      **     CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
      **     MOVE  MKPASS-OT-01  TO  FILE-NAME
      *     MOVE  SPA-HOSPNUM   TO  FILE-HOSPNUM
      **=== プラグイン対応 by Yoshikawa(2012/12/25) end
      **
      *     DISPLAY "ファイル名="  FILE-NAME
      **
      *     INITIALIZE   YUSEN-HKNJANUM
      **
      *     OPEN  INPUT  YUSEN-FILE
      **
      *     READ YUSEN-FILE
      *       AT END
      *         INITIALIZE        YUSEN-HKNJANUM
      *       DISPLAY "読み込み失敗"  
      *     END-READ
      **
      *     DISPLAY "優先保険者番号="  YUSEN-HKNJANUM
      **
      *     CLOSE YUSEN-FILE
      **
      **=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
      **    印字対象公費番号ファイル読み込み
      **    ファイルディレクトリ位置指定
      **=== プラグイン対応 by Yoshikawa(2012/12/25) start
      **     INITIALIZE           ORCSMKPASSAREA
      **     MOVE  "data/TAISYOKOHI.INI"     TO  MKPASS-IN-01
      **     CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
      **     MOVE  MKPASS-OT-01  TO  FILE-NAME
      *     MOVE  SPA-HOSPNUM   TO  FILE-HOSPNUM2
      **=== プラグイン対応 by Yoshikawa(2012/12/25) end
      **
      *     DISPLAY "ファイル名2="  FILE-NAME2
      **
      *     INITIALIZE   TAISYOKOHI-REC
      **
      *     OPEN  INPUT  TAISYOKOHI-FILE
      **
      *     MOVE  ZERO  TO  IDX
      *                     FLG-TAISYOKOHI
      **
      *     PERFORM UNTIL    FLG-TAISYOKOHI = 1
      *       READ TAISYOKOHI-FILE NEXT
      *         AT END
      *           MOVE  1                 TO  FLG-TAISYOKOHI
      *         NOT AT END
      *           IF  TAISYO-DAT-KBN = "*" OR SPACE
      *             CONTINUE
      *           ELSE
      *             ADD   1               TO  IDX
      *             MOVE  TAISYOKOHI-REC  TO  TAISYO-TBL(IDX)
      *             DISPLAY "読み込み対象公費 = " TAISYOKOHI-REC
      *           END-IF
      *       END-READ
      *     END-PERFORM
      **
      *     CLOSE TAISYOKOHI-FILE
      **
      **=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *
      *     .
      * 110-INI-FILE-EXT.
      *     EXIT.
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SOKATU1410"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:1)    TO  TAISYO-KBN(IDX)
             MOVE    WRK-OPTION(IDX)(3:3)    TO  TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費区分="  TAISYO-KBN(IDX)
             DISPLAY "対象公費番号="  TAISYO-KOHNUM(IDX)
           END-PERFORM
      *    
      *    優先保険者番号
           MOVE    "YUSEN"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(1:6)        TO  WRK-YUSEN
           DISPLAY "優先保険者番号="  WRK-YUSEN
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
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN     OUTPUT  SKT1410TMP-FILE
           CLOSE            SKT1410TMP-FILE
           OPEN     I-O     SKT1410TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT1410TMP-FILE
      *
           IF       FLG-UPD   =       1
                    PERFORM   2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了    処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      CNT-RECE10          =   ZERO 
                   CONTINUE
           ELSE
                   IF      FLG-UPD     =   1
                           PERFORM     310-SEIKYU-UPD-SEC
                   END-IF
           END-IF
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU1410 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU1410 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *###（地方公費仕様）
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *---(01.00.10) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------

           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号別集計処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC              SECTION.
      * 
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT1410TMP-REC
      *
           IF  RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  WRK-HKNJANUM(1:6)
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  WRK-HKNJANUM(1:6)
           END-IF
      *
      *    区の場合代表番号セット
           PERFORM  2002-HKNJANUM-SET-SEC
           MOVE  WRK-HKNJANUM    TO  SKT1410TMP-HKNJANUM
      *
      *    在宅フラグ
      *=== 在宅はまとめて集計 by Yoshikawa(2012/05/16) start
      *     MOVE  RECE10-ZAITAKU  TO  SKT1410TMP-ZAITAKU
      *=== 在宅はまとめて集計 by Yoshikawa(2012/05/16) end
      *
      *    印字順設定
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      *     IF    YUSEN-HKNJANUM   =  SKT1410TMP-HKNJANUM
           IF    WRK-YUSEN        =  SKT1410TMP-HKNJANUM
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
             MOVE          1   TO    SKT1410TMP-SYUTURYOKU
           ELSE
             IF    RECE10-PREFKBN    =    "1"
                 IF    SKT1410TMP-HKNJANUM(1:2)  =  "14"
                   MOVE    2   TO    SKT1410TMP-SYUTURYOKU
                 ELSE
                   MOVE    3   TO    SKT1410TMP-SYUTURYOKU
                 END-IF
             ELSE
      *=== 災害対応 by Yoshikawa(2011/06/23) start
      *         MOVE        9   TO    SKT1410TMP-SYUTURYOKU
               MOVE        8   TO    SKT1410TMP-SYUTURYOKU
      *=== 災害対応 by Yoshikawa(2011/06/23) end
             END-IF
           END-IF
      *
      *=== 災害対応 by Yoshikawa(2011/06/23) start
      *    災１、災２、不詳・災１は別帳票
           IF  RECE10-ETC-YKZFTN (1)  =  2 OR 4 OR 5
           OR  RECE10-HKNJANUM  =  ZERO
             MOVE    "00001"     TO  SKT1410TMP-GENMEN
             MOVE    9           TO  SKT1410TMP-SYUTURYOKU
      *
      *      保険者番号が不明の場合、住所から県を取得
             IF  RECE10-HKNJANUM  =  ZERO
               PERFORM  20011-CITYGET-SEC
             END-IF
           END-IF
      *=== 災害対応 by Yoshikawa(2011/06/23) end
      *
           PERFORM  900-SKTTMP-READ-SEC
      *
           PERFORM  210-HKN-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "集計対象外　患者番号＝"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " 診療年月＝"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " レセ種別＝"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *=== 災害対応 by Yoshikawa(2011/06/23) start
      *****************************************************************
      *    住所から市町村名取得処理
      *****************************************************************
       20011-CITYGET-SEC          SECTION.
      *
      *    住所から都道府県名を取得
           PERFORM  VARYING  IDX5  FROM  1  BY  1
                    UNTIL  IDX5  >  TBL-PREF-MAX
             IF  PTINF-HOME-ADRS(1:6)  =  TBL-PREFNAME(IDX5)(1:6)
               DISPLAY  "都道府県番号 ="        TBL-PREFNUM(IDX5)
               DISPLAY  "都道府県名 ="          TBL-PREFNAME(IDX5)
               MOVE  TBL-PREFNUM(IDX5)      TO  SKT1410TMP-HKNJANUM
               MOVE  TBL-PREF-MAX           TO  IDX5
             END-IF
           END-PERFORM
      *
           .
       20011-CITYGET-EXT.
           EXIT.
      *
      *=== 災害対応 by Yoshikawa(2011/06/23) end
      *****************************************************************
      *    代表番号セット
      *****************************************************************
       2002-HKNJANUM-SET-SEC                SECTION.
      *    横浜市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "144"
             MOVE  "144006"  TO  WRK-HKNJANUM
           END-IF
      *    川崎市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "145"
             MOVE  "145003"  TO  WRK-HKNJANUM
           END-IF
      *=== 相模原市の代表番号追加 by Yoshikawa(2010/08/04) start
      *    相模原市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "146"
             MOVE  "146001"  TO  WRK-HKNJANUM
           END-IF
      *=== 相模原市の代表番号追加 by Yoshikawa(2010/08/04) end
      *
           .
       2002-HKNJANUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    入院／外来
           IF  RECE10-NYUGAIKBN  =  1
             MOVE  1  TO  IDX2
           ELSE
             MOVE  2  TO  IDX2
           END-IF
      *
      *    一般被保険者
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
      *     IF  RECE10-HKNNUM  =  "060"
           IF  RECE10-HKNNUM  =  "060" OR "068"
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *    年齢チェック
      *---(01.00.11) LINE ADD START ------------------------------------
      *       EVALUATE  TRUE
      *         WHEN  RECE10-AGE  >  69
      **---(01.00.04) LINE ADD START ------------------------------------
      **           IF  RECE10-KYURATE  =  8
      **             MOVE  6  TO  IDX1
      **           ELSE
      **             MOVE  7  TO  IDX1
      **          END-IF
      *           IF  RECE10-KYURATE  =  9
      *             MOVE  5  TO  IDX1
      *           ELSE
      *             MOVE  4  TO  IDX1
      *           END-IF
      **---(01.00.04) LINE ADD END   ------------------------------------
      *         WHEN  RECE10-AGE  >  2
      **---(01.00.05) LINE ADD START ------------------------------------
      **             EVALUATE  RECE10-KYURATE
      **               WHEN   7      MOVE  1  TO  IDX1
      **               WHEN   8      MOVE  2  TO  IDX1
      **               WHEN   9      MOVE  3  TO  IDX1
      **               WHEN  OTHER   MOVE  4  TO  IDX1
      **             END-EVALUATE
      *             EVALUATE  RECE10-KYURATE
      *               WHEN   7      MOVE  1  TO  IDX1
      *               WHEN  OTHER   MOVE  2  TO  IDX1
      *             END-EVALUATE
      **---(01.00.05) LINE ADD END   ------------------------------------
      *         WHEN OTHER
      **---(01.00.05) LINE ADD START ------------------------------------
      **           MOVE  8  TO  IDX1
      *           MOVE  6  TO  IDX1
      **---(01.00.05) LINE ADD END   ------------------------------------
      *       END-EVALUATE
             IF  RECE10-AGE  >=  70
      *=== 前期高齢者２割対応 by Yoshikawa(2014/05/21) start
      *           IF  RECE10-KYURATE  =  9
      *               MOVE  5  TO  IDX1
      *           ELSE
      *               MOVE  4  TO  IDX1
      *           END-IF
                 EVALUATE  RECE10-KYURATE
                   WHEN  9
                     MOVE    5     TO    IDX1
                   WHEN  8
                     IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                         MOVE    5    TO    IDX1
                     ELSE
                         MOVE    4    TO    IDX1
                     END-IF
                   WHEN  7
                     MOVE    4     TO    IDX1
                 END-EVALUATE
      *=== 前期高齢者２割対応 by Yoshikawa(2014/05/21) end
             ELSE
                 IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (1)   =  1)
                     MOVE  6     TO  IDX1
                 ELSE
                     IF  RECE10-AGE  >=  3
      *---(01.00.12) LINE ADD START ------------------------------------
      *                   IF  RECE10-KYURATE  =  7
      *                       MOVE  1     TO  IDX1
      *                   ELSE
      *                       MOVE  2     TO  IDX1
      *                   END-IF
                         MOVE  1     TO  IDX1
      *---(01.00.12) LINE ADD END   ------------------------------------
                     ELSE
                         MOVE  6     TO  IDX1
                     END-IF
                 END-IF
             END-IF
      *---(01.00.11) LINE ADD END   ------------------------------------
      *     障害を所持している患者は年齢を問わず「障害」欄に集計する
      *     但し、全国土木、全国建設工事業は除く
      *=== 障害は公費再掲欄に記載する by Yoshikawa(2011/08/17) start
      *       IF     SKT1410TMP-HKNJANUM NOT = "133033" AND "133298"
      *         IF   RECE10-KOHNUM(1) = "180" OR "280"
      *           OR RECE10-KOHNUM(2) = "180" OR "280"
      *           OR RECE10-KOHNUM(3) = "180" OR "280"
      *           OR RECE10-KOHNUM(4) = "180" OR "280"
      ****** 05/05/27 INS
      *           OR RECE10-ETC-KOHNUM(1) = "180" OR "280"
      *           OR RECE10-ETC-KOHNUM(2) = "180" OR "280"
      *           OR RECE10-ETC-KOHNUM(3) = "180" OR "280"
      *           OR RECE10-ETC-KOHNUM(4) = "180" OR "280"
      ******
      **---(01.00.05) LINE ADD START ------------------------------------
      **           MOVE     5  TO  IDX1
      *           MOVE     3  TO  IDX1
      **---(01.00.05) LINE ADD END   ------------------------------------
      *         END-IF
      *       END-IF
      *=== 障害は公費再掲欄に記載する by Yoshikawa(2011/08/17) end
      *
      *    退職者
           ELSE
      *      本人
             IF  RECE10-HONKZKKBN  =  "1"
      *    年齢チェック
               EVALUATE  TRUE
                 WHEN  RECE10-AGE  >  69
      *---(01.00.04) LINE ADD START ------------------------------------
      *             IF  RECE10-KYURATE  =  8
      *               MOVE  13  TO  IDX1
      *             ELSE
      *               MOVE  14  TO  IDX1
      *             END-IF
      *=== 前期高齢者２割対応 by Yoshikawa(2014/05/21) start
      *             IF  RECE10-KYURATE  =  9
      *               MOVE  10  TO  IDX1
      *             ELSE
      *               MOVE  9   TO  IDX1
      *             END-IF
                   EVALUATE  RECE10-KYURATE
                     WHEN  9
                       MOVE    10    TO    IDX1
                     WHEN  8
                       IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                           MOVE    10   TO    IDX1
                       ELSE
                           MOVE    9    TO    IDX1
                       END-IF
                     WHEN  7
                       MOVE    9     TO    IDX1
                   END-EVALUATE
      *=== 前期高齢者２割対応 by Yoshikawa(2014/05/21) end
      *---(01.00.04) LINE ADD END   ------------------------------------
                 WHEN  OTHER
      *---(01.00.05) LINE ADD START ------------------------------------
      *               EVALUATE  RECE10-KYURATE
      *                   WHEN  7
      *                         MOVE  9    TO  IDX1
      *                   WHEN  8
      *                         MOVE  10   TO  IDX1
      *                  WHEN  OTHER
      *                         MOVE  11   TO  IDX1
      *               END-EVALUATE
                     EVALUATE  RECE10-KYURATE
                         WHEN  7
                               MOVE  7    TO  IDX1
                         WHEN  OTHER
                               MOVE  8    TO  IDX1
                     END-EVALUATE
      *---(01.00.05) LINE ADD END   ------------------------------------
               END-EVALUATE
      *        障害を所持している患者は年齢を問わず「本人障害」欄に集計する
      *        但し、全国土木、全国建設工事業は除く
      *=== 障害は公費再掲欄に記載する by Yoshikawa(2011/08/17) start
      *         IF     SKT1410TMP-HKNJANUM NOT = "133033" AND "133298"
      *           IF   RECE10-KOHNUM(1) = "180" OR "280"
      *             OR RECE10-KOHNUM(2) = "180" OR "280"
      *             OR RECE10-KOHNUM(3) = "180" OR "280"
      *             OR RECE10-KOHNUM(4) = "180" OR "280"
      ****** 05/05/27 INS
      *             OR RECE10-ETC-KOHNUM(1) = "180" OR "280"
      *             OR RECE10-ETC-KOHNUM(2) = "180" OR "280"
      *             OR RECE10-ETC-KOHNUM(3) = "180" OR "280"
      *             OR RECE10-ETC-KOHNUM(4) = "180" OR "280"
      ******
      **---(01.00.05) LINE ADD START ------------------------------------
      **             MOVE      12  TO  IDX1
      *             MOVE      8  TO  IDX1
      **---(01.00.05) LINE ADD END   ------------------------------------
      *           END-IF
      *         END-IF
      *=== 障害は公費再掲欄に記載する by Yoshikawa(2011/08/17) end
      *      家族
             ELSE
               IF  RECE10-AGE  >  2
      *---(01.00.11) LINE ADD START ------------------------------------
                 IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (1)   =  1)
                     MOVE  13     TO  IDX1
                 ELSE
      *---(01.00.11) LINE ADD END   ------------------------------------
      *          前期高齢者判定
                   IF    RECE10-AGE   >= 70
      *---(01.00.04) LINE ADD START ------------------------------------
      *             IF  RECE10-KYURATE  =  8
      *               MOVE  13  TO  IDX1
      *             ELSE
      *               MOVE  14  TO  IDX1
      *             END-IF
      *=== 前期高齢者２割対応 by Yoshikawa(2014/05/21) start
      *               IF  RECE10-KYURATE  =  9
      *                 MOVE  10  TO  IDX1
      *               ELSE
      *                 MOVE  9   TO  IDX1
      *               END-IF
                     EVALUATE  RECE10-KYURATE
                       WHEN  9
                         MOVE    10    TO    IDX1
                       WHEN  8
                         IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                             MOVE    10   TO    IDX1
                         ELSE
                             MOVE    9    TO    IDX1
                         END-IF
                       WHEN  7
                         MOVE    9     TO    IDX1
                     END-EVALUATE
      *=== 前期高齢者２割対応 by Yoshikawa(2014/05/21) end
      *---(01.00.04) LINE ADD END   ------------------------------------
                   ELSE
      *---(01.00.05) LINE ADD START ------------------------------------
      *               EVALUATE  RECE10-KYURATE
      *                 WHEN   7      MOVE  15  TO  IDX1
      *                 WHEN   8      MOVE  16  TO  IDX1
      *                 WHEN  OTHER   MOVE  17  TO  IDX1
      *               END-EVALUATE
                       EVALUATE  RECE10-KYURATE
                         WHEN   7      MOVE  11  TO  IDX1
                         WHEN  OTHER   MOVE  12  TO  IDX1
                       END-EVALUATE
      *---(01.00.05) LINE ADD END   ------------------------------------
                   END-IF
      *---(01.00.11) LINE ADD START ------------------------------------
                 END-IF
      *---(01.00.11) LINE ADD END   ------------------------------------
               ELSE
      *---(01.00.05) LINE ADD START ------------------------------------
      *           MOVE  19  TO   IDX1
                 MOVE  13  TO   IDX1
      *---(01.00.05) LINE ADD END   ------------------------------------
               END-IF
      *       障害を所持している患者は年齢を問わず「家族障害」欄に集計する
      *       但し、全国土木、全国建設工事業は除く
      *=== 障害は公費再掲欄に記載する by Yoshikawa(2011/08/17) start
      *         IF     SKT1410TMP-HKNJANUM NOT = "133033" AND "133298"
      *           IF   RECE10-KOHNUM(1) = "180" OR "280"
      *             OR RECE10-KOHNUM(2) = "180" OR "280"
      *             OR RECE10-KOHNUM(3) = "180" OR "280"
      *             OR RECE10-KOHNUM(4) = "180" OR "280"
      ****** 05/05/27 INS
      *             OR RECE10-ETC-KOHNUM(1) = "180" OR "280"
      *             OR RECE10-ETC-KOHNUM(2) = "180" OR "280"
      *             OR RECE10-ETC-KOHNUM(3) = "180" OR "280"
      *             OR RECE10-ETC-KOHNUM(4) = "180" OR "280"
      ******
      **            ３歳未満、前期高齢者は家族でも本人欄に集計する
      **---(01.00.11) LINE ADD START ------------------------------------
      *             IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
      *             AND  (RECE10-FLG (1)   =  1)
      *                 MOVE  8       TO  IDX1
      *             ELSE
      **---(01.00.11) LINE ADD ENF   ------------------------------------
      *               IF  RECE10-AGE     <   3  OR
      *                   RECE10-AGE     >   69
      **---(01.00.05) LINE ADD START ------------------------------------
      **                 MOVE     12     TO   IDX1
      *                 MOVE     8      TO   IDX1
      **---(01.00.05) LINE ADD END   ------------------------------------
      *               ELSE
      **---(01.00.05) LINE ADD START ------------------------------------
      **                 MOVE     18     TO   IDX1
      *                 MOVE     12     TO   IDX1
      **---(01.00.05) LINE ADD END   ------------------------------------
      *               END-IF
      **---(01.00.11) LINE ADD START ------------------------------------
      *             END-IF
      **---(01.00.11) LINE ADD END   ------------------------------------
      *           END-IF
      *         END-IF
      *=== 障害は公費再掲欄に記載する by Yoshikawa(2011/08/17) end
             END-IF
           END-IF
      *
      *---(01.00.05) LINE ADD START ------------------------------------
      *    １０割患者
      *     IF    SKT1410TMP-HKNJANUM  =  "143016"
      *                               OR  "144006"
      *       IF    RECE10-AGE     =  ZERO
      *         IF    RECE10-KOHNUM(1)  =  "381"  OR
      *               RECE10-KOHNUM(2)  =  "381"  OR
      *               RECE10-KOHNUM(3)  =  "381"  OR
      *               RECE10-KOHNUM(4)  =  "381"
      *           IF    RECE10-HKNNUM   =  "060"
      *             MOVE    4          TO  IDX1
      *           ELSE
      *             IF    RECE10-HONKZKKBN  =  "1"
      *               MOVE    11       TO  IDX1
      *             ELSE
      *               MOVE    17       TO  IDX1
      *             END-IF
      *           END-IF
      *         END-IF
      *       END-IF
      *     END-IF
      *---(01.00.05) LINE ADD END   ------------------------------------
      *
      *    老人保健
           IF  RECE10-RJNHKNNUM  NOT  =  SPACE
      *---(01.00.05) LINE ADD START ------------------------------------
      *       EVALUATE  RECE10-KYURATE
      *         WHEN  9     MOVE  21  TO  IDX1
      *         WHEN  8     MOVE  20  TO  IDX1
      *---(01.00.04) LINE ADD START ------------------------------------
      *         WHEN  7     MOVE  20  TO  IDX1
      *---(01.00.04) LINE ADD END   ------------------------------------
      *         WHEN OTHER  MOVE  22  TO  IDX1
      *       END-EVALUATE
             EVALUATE  RECE10-KYURATE
               WHEN  9     MOVE  15  TO  IDX1
               WHEN OTHER  MOVE  14  TO  IDX1
             END-EVALUATE
      *---(01.00.05) LINE ADD END   ------------------------------------
           END-IF
      *
           PERFORM  2101-SYUKEI-SEC
      *
      *    公費負担分
           IF  RECE10-KOHNUM (1)   NOT =   SPACE  OR
               RECE10-KOHNUM (2)   NOT =   SPACE  OR
               RECE10-KOHNUM (3)   NOT =   SPACE  OR
               RECE10-KOHNUM (4)   NOT =   SPACE
      *    記載外公費判定
            OR RECE10-ETC-KOHNUM(1) NOT = SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-IF
      *
           IF  FLG-SKT1410TMP  =  1
               WRITE    SKT1410TMP-REC
           ELSE
               REWRITE  SKT1410TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・一般国保分集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD   1                   TO  SKT1410TMP-KENSU (IDX1 IDX2)
      *    療養の給付:診療実日数
           ADD   RECE10-JNISSU (1)   TO  SKT1410TMP-NISSU (IDX1 IDX2)
      *    療養の給付：点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT1410TMP-TENSU (IDX1 IDX2)
      *---(01.00.05) LINE ADD START ------------------------------------
      *    療養の給付：薬剤一部負担金
      *     ADD   RECE10-YKZFTN (1)   TO  SKT1410TMP-YKZFTN(IDX1 IDX2)
      *---(01.00.05) LINE ADD END   ------------------------------------
      *---(01.00.08) LINE ADD START ------------------------------------
      *    療養の給付：一部負担金(障害欄集計時は記載しない)
      *     IF      IDX1    NOT    =      5  AND  12  AND  18
      *       ADD RECE10-FTNMONEY(1)  TO  SKT1410TMP-ITBFTN(IDX1 IDX2)
      *     END-IF
           ADD RECE10-FTNMONEY(1)  TO  SKT1410TMP-ITBFTN(IDX1 IDX2)
      *---(01.00.08) LINE ADD END   ------------------------------------
      *    食事療養:件数
           IF  RECE10-SHOKUJINISSU(1)  >  0
             ADD   1  TO  SKT1410TMP-S-KENSU(IDX1)
           END-IF
      *
           IF  RECE10-NYUGAIKBN  =  1
      *        食事療養:日数
               COMPUTE SKT1410TMP-S-NISSU(IDX1)  =
                       SKT1410TMP-S-NISSU(IDX1)  +
                       RECE10-SHOKUJINISSU(1)
      *        食事療養：金額
               COMPUTE SKT1410TMP-S-KINGAKU(IDX1)  =
                       SKT1410TMP-S-KINGAKU(IDX1)  +
                       RECE10-SHOKUJIRYOYOHI(1)
      *        食事療養：標準負担額
               COMPUTE SKT1410TMP-S-FTNGAKU(IDX1)  =
                       SKT1410TMP-S-FTNGAKU(IDX1)  +
                       RECE10-SHOKUJIFTN(1)
           END-IF
      *
      *---(01.00.05) LINE ADD START ------------------------------------
      *   退職者、被扶養者、８割、入院外の場合は集計を行わない。
      *     IF    IDX1    =  16
      *      AND  RECE10-NYUGAIKBN  =   2
      *           MOVE  ZERO        TO  SKT1410TMP-KENSU (16 2)
      *           MOVE  ZERO        TO  SKT1410TMP-NISSU (16 2)
      *           MOVE  ZERO        TO  SKT1410TMP-TENSU (16 2)
      *           MOVE  ZERO        TO  SKT1410TMP-YKZFTN(16 2)
      *           MOVE  ZERO        TO  SKT1410TMP-ITBFTN(16 2)
      *           MOVE  ZERO        TO  SKT1410TMP-S-KENSU(16)
      *           MOVE  ZERO        TO  SKT1410TMP-S-NISSU(16)
      *           MOVE  ZERO        TO  SKT1410TMP-S-KINGAKU(16)
      *           MOVE  ZERO        TO  SKT1410TMP-S-FTNGAKU(16)
      *     ELSE
      *           MOVE  1           TO  FLG-UPD
      *     END-IF
      *---(01.00.05) LINE ADD END   ------------------------------------
      *
      *---(01.00.05) LINE ADD START ------------------------------------
      *     経過措置チェック
            INITIALIZE          FLG-KEIKA
            PERFORM  900-PTKOHINF-READ-SEC
      *---(01.00.06) LINE ADD START ------------------------------------
      *      IF  FLG-KEIKA  =  1
            IF  FLG-KEIKA  =  1  AND  RECE10-FTNMONEY(1)  >=  12000
      *---(01.00.06) LINE ADD END   ------------------------------------
      *---(01.00.13) LINE ADD START ------------------------------------
      *        ADD   1  TO  SKT1410TMP-K-KENSU (30)
              ADD   1  TO  SKT1410TMP-K-KENSU (31)
      *---(01.00.13) LINE ADD END   ------------------------------------
            END-IF
      *---(01.00.05) LINE ADD END   ------------------------------------
      *---(01.00.13) LINE ADD START ------------------------------------
      *    二割 チェック
           IF  RECE10-TOKKI(1)  =  "20"
           OR  RECE10-TOKKI(2)  =  "20"
           OR  RECE10-TOKKI(3)  =  "20"
           OR  RECE10-TOKKI(4)  =  "20"
           OR  RECE10-TOKKI(5)  =  "20"
               ADD   1  TO  SKT1410TMP-K-KENSU (33)
           END-IF
      *---(01.00.13) LINE ADD END   ------------------------------------
      *
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
      ***** 05/05/27 INS
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-ETC-KOHNUM (IDX3) =   SPACE
               IF      RECE10-ETC-KOHNUM(IDX3)  =   "180" OR "280"
                 IF      RECE10-RJNHKNNUM  NOT  =   SPACE
      *    老人、又は２つの国保組合の場合障老健に分類
                   OR  RECE10-HKNJANUM  =  "133033"
                   OR  RECE10-HKNJANUM  =  "133298"
      *---(01.00.05) LINE ADD START ------------------------------------
      *              療養の給付：件数
      *               ADD   1  TO  SKT1410TMP-K-KENSU (19)
                     ADD   1  TO  SKT1410TMP-K-KENSU (20)
      *
      *              療養の給付：点数
      *               ADD   RECE10-ETC-TOTALTEN(IDX3)
      *                                  TO  SKT1410TMP-K-TENSU(19)
      *
      *              療養の給付：薬剤一部負担金
      *               ADD   RECE10-ETC-YKZFTN(IDX3)
      *                                  TO  SKT1410TMP-K-YKZFTN(19)
      *
      *              療養の給付：一部負担金
      *               ADD   RECE10-ETC-FTNMONEY(IDX3)
      *                                  TO  SKT1410TMP-K-ITBFTN(19)
      *---(01.00.05) LINE ADD END   ------------------------------------
                 END-IF
               END-IF
           END-PERFORM
      *****
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      *
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) start
      *        公費番号まとめ情報があるとき
             IF  WRK-2007-MAX  >  ZERO
               MOVE  SPACE     TO  WRK-KOHNAME
               PERFORM VARYING IDZ1    FROM    1   BY  1
                       UNTIL   IDZ1    >       WRK-2007-MAX
                 PERFORM VARYING IDZ2  FROM    1   BY  1
      *=== 公費番号まとめの編集方法修正 by Yoshikawa(2009/11/18) start
      *                   UNTIL   IDZ2  >       5
                         UNTIL   IDZ2  >       10
      *=== 公費番号まとめの編集方法修正 by Yoshikawa(2009/11/18) end
                         OR WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                   IF  RECE10-KOHNUM(IDX3)  
                                 =   WRK-2007-KOHNUM(IDZ1 IDZ2)
                     MOVE  WRK-2007-KOHNUM(IDZ1 1)
                                 TO  RECE10-KOHNUM (IDX3)
                     MOVE  WRK-2007-MEISYO (IDZ1)
                                 TO  WRK-KOHNAME
      *=== 公費番号まとめの編集方法修正 by Yoshikawa(2009/11/18) start
      *               MOVE  5             TO  IDZ2
                     MOVE  10            TO  IDZ2
      *=== 公費番号まとめの編集方法修正 by Yoshikawa(2009/11/18) end
                     MOVE  WRK-2007-MAX  TO  IDZ1
                   END-IF
                 END-PERFORM
               END-PERFORM
             END-IF
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) end
      *
             EVALUATE  RECE10-KOHNUM (IDX3)
               WHEN  010   MOVE  1   TO  IDX4
               WHEN  011   MOVE  2   TO  IDX4
               WHEN  015   MOVE  3   TO  IDX4
               WHEN  016   MOVE  4   TO  IDX4
               WHEN  017   MOVE  5   TO  IDX4
               WHEN  019   MOVE  6   TO  IDX4
               WHEN  020   MOVE  7   TO  IDX4
               WHEN  021   MOVE  8   TO  IDX4
               WHEN  022   MOVE  9   TO  IDX4
               WHEN  023   MOVE  10  TO  IDX4
      *---(01.00.05) LINE ADD START ------------------------------------
      *         WHEN  028   MOVE  11  TO  IDX4
      *         WHEN  029   MOVE  12  TO  IDX4
      ***** 06/01/06 保険番号合計の修正
      *         WHEN  241
      *         WHEN  142
      *         WHEN  141   MOVE  13  TO  IDX4
      *         WHEN  051   MOVE  14  TO  IDX4
      *         WHEN  091   MOVE  14  TO  IDX4
      *         WHEN  052   MOVE  15  TO  IDX4
      *         WHEN  053   MOVE  16  TO  IDX4
      *         WHEN  281
      *         WHEN  181   MOVE  17  TO  IDX4
      *         WHEN  285                                                
      *         WHEN  185   MOVE  18  TO  IDX4
      *         WHEN  180
      *         WHEN  280                                                
      *          IF  RECE10-RJNHKNNUM  NOT =  SPACE
      *    老人、又は２つの国保組合の場合障老健に分類
      *             OR  RECE10-HKNJANUM  =  "133033"
      *             OR  RECE10-HKNJANUM  =  "133298"
      *            MOVE  19  TO  IDX4
      *          ELSE
      *            MOVE  99  TO  IDX4
      *          END-IF
      ****2006/01/13 @@@@-BRAND  START
      *         WHEN  188   MOVE  25  TO IDX4
      *         WHEN  189   MOVE  26  TO IDX4
      ****2006/01/13 @@@@-BRAND  END
      *         WHEN  OTHER MOVE  99  TO  IDX4
               WHEN  024   MOVE  11  TO  IDX4
               WHEN  028   MOVE  12  TO  IDX4
               WHEN  029   MOVE  13  TO  IDX4
      *---(01.00.12) LINE ADD START ------------------------------------
      *         WHEN  142
      *         WHEN  141   MOVE  14  TO  IDX4
      *         WHEN  051   MOVE  15  TO  IDX4
      *         WHEN  091   MOVE  15  TO  IDX4
      *         WHEN  052   MOVE  16  TO  IDX4
      *         WHEN  053   MOVE  17  TO  IDX4
      *         WHEN  066   MOVE  18  TO  IDX4
      *---(01.00.09) LINE ADD START ------------------------------------
      **         WHEN  066   MOVE  19  TO  IDX4
      *         WHEN  079   MOVE  19  TO  IDX4
      *---(01.00.09) LINE ADD END   ------------------------------------
      *         WHEN  281
      *         WHEN  181   MOVE  21  TO  IDX4
      *         WHEN  285
      *         WHEN  185   MOVE  22  TO  IDX4
      *         WHEN  180
      *         WHEN  280
      *           IF  RECE10-RJNHKNNUM  NOT =  SPACE
      *    老人、又は２つの国保組合の場合障老健に分類
      *             OR  RECE10-HKNJANUM  =  "133033"
      *             OR  RECE10-HKNJANUM  =  "133298"
      *             MOVE  20  TO  IDX4
      *           ELSE
      *             MOVE  99  TO  IDX4
      *           END-IF
      *         WHEN  188   MOVE  23  TO  IDX4
      *---(01.00.07) LINE ADD START ------------------------------------
      *         WHEN  289
      *---(01.00.07) LINE ADD END   ------------------------------------
      *         WHEN  189   MOVE  24  TO  IDX4*
      *         WHEN  960
      *         WHEN  961
      *         WHEN  962
      *       WHEN  963   MOVE  28  TO  IDX4
      *---(01.00.11) LINE ADD START ------------------------------------
      *         WHEN  038   MOVE  32  TO  IDX4
      *---(01.00.11) LINE ADD END   ------------------------------------
      *         WHEN  OTHER MOVE  99  TO  IDX4
      *---(01.00.05) LINE ADD END   ------------------------------------
               WHEN  038   MOVE  14  TO  IDX4
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
      *         WHEN  142
      *         WHEN  141   MOVE  15  TO  IDX4
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
               WHEN  051   MOVE  16  TO  IDX4
               WHEN  091   MOVE  16  TO  IDX4
               WHEN  052   MOVE  17  TO  IDX4
               WHEN  053   MOVE  18  TO  IDX4
      *=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) start
               WHEN  054   MOVE  34  TO  IDX4
      *=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) end
               WHEN  066   MOVE  19  TO  IDX4
               WHEN  079   MOVE  20  TO  IDX4
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
      *         WHEN  180
      *         WHEN  280
      **=== 障害は公費再掲欄に記載する by Yoshikawa(2011/08/17) start
      **           IF  RECE10-RJNHKNNUM  NOT =  SPACE
      ***    老人、又は２つの国保組合の場合障老健に分類
      **             OR  RECE10-HKNJANUM  =  "133033"
      **             OR  RECE10-HKNJANUM  =  "133298"
      **             MOVE  21  TO  IDX4
      **           ELSE
      **             MOVE  99  TO  IDX4
      **           END-IF
      *           MOVE  21  TO  IDX4
      **=== 障害は公費再掲欄に記載する by Yoshikawa(2011/08/17) end
      **=== 印字対象番号追加 by yoshikawa(2009/07/13) start
      *         WHEN  381
      **=== 印字対象番号追加 by yoshikawa(2009/07/13) end
      *         WHEN  281
      *         WHEN  181   MOVE  22  TO  IDX4
      **=== 印字対象番号追加 by yoshikawa(2009/07/13) start
      *         WHEN  385
      **=== 印字対象番号追加 by yoshikawa(2009/07/13) end
      *         WHEN  285
      *         WHEN  185   MOVE  23  TO  IDX4
      *         WHEN  188   MOVE  24  TO  IDX4
      *         WHEN  289
      *         WHEN  189   MOVE  25  TO  IDX4
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
               WHEN  960
               WHEN  961
               WHEN  962
               WHEN  963   MOVE  29  TO  IDX4
               WHEN  OTHER MOVE  99  TO  IDX4
      *---(01.00.12) LINE ADD END   ------------------------------------
             END-EVALUATE
      *
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
             IF  RECE10-KOHNUM(IDX3)(1:1)  NOT =  "0" AND "9"
               INITIALIZE                            TBL-HIKAKU-KOHI
               MOVE    RECE10-KOHNUM(IDX3)       TO  TBL-HKOHNUM(1)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
                 EVALUATE  WRK-HBTKBN
      *            老人
                   WHEN  "1"
                     MOVE  15        TO  IDX4
      *            重障
                   WHEN  "2"
                     MOVE  21        TO  IDX4
      *            乳
                   WHEN  "3"
                     MOVE  22        TO  IDX4
      *            親
                   WHEN  "4"
                     MOVE  23        TO  IDX4
      *            ぜん息
                   WHEN  "5"
                     MOVE  24        TO  IDX4
      *            成ぜ
                   WHEN  "6"
                     MOVE  25        TO  IDX4
                 END-EVALUATE
               ELSE
                 MOVE    99          TO  IDX4
               END-IF
             END-IF
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *
             IF  IDX4  NOT =  99
      *    療養の給付：件数
                ADD   1  TO  SKT1410TMP-K-KENSU (IDX4)
      *---(01.00.05) LINE ADD START ------------------------------------
      *    療養の給付：点数
      *         COMPUTE SKT1410TMP-K-TENSU(IDX4)  =
      *                 SKT1410TMP-K-TENSU(IDX4)  +
      *                 RECE10-TOTALTEN(IDX3 + 1)
      *    療養の給付：薬剤一部負担金
      *         COMPUTE SKT1410TMP-K-YKZFTN(IDX4)  =
      *                 SKT1410TMP-K-YKZFTN(IDX4)  +
      *                 RECE10-YKZFTN(IDX3 + 1)
      *    療養の給付：一部負担金
      *         COMPUTE SKT1410TMP-K-ITBFTN(IDX4)  =
      *                 SKT1410TMP-K-ITBFTN(IDX4)  +
      *                 RECE10-FTNMONEY(IDX3 + 1)
      *---(01.00.05) LINE ADD END   ------------------------------------
             END-IF
           END-PERFORM
      *    ９７２ チェック
           IF  RECE10-ETC-KOHNUM(1)  =  "972" OR
               RECE10-ETC-KOHNUM(2)  =  "972" OR
               RECE10-ETC-KOHNUM(3)  =  "972" OR
               RECE10-ETC-KOHNUM(4)  =  "972"
      *---(01.00.05) LINE ADD START ------------------------------------
      *         ADD   1  TO  SKT1410TMP-K-KENSU (21)
      *---(01.00.12) LINE ADD START ------------------------------------
      *         ADD   1  TO  SKT1410TMP-K-KENSU (26)
               ADD   1  TO  SKT1410TMP-K-KENSU (27)
      *---(01.00.12) LINE ADD END   ------------------------------------
      *    療養の給付：点数
      *         COMPUTE SKT1410TMP-K-TENSU(21)  =
      *                 SKT1410TMP-K-TENSU(21)  +
      *                 RECE10-TOTALTEN(1)
      *    療養の給付：薬剤一部負担金
      *         COMPUTE SKT1410TMP-K-YKZFTN(21)  =
      *                 SKT1410TMP-K-YKZFTN(21)  +
      *                 RECE10-YKZFTN(1)
      *    療養の給付：一部負担金
      *          ADD    10000    TO    SKT1410TMP-K-ITBFTN(21)
      *---(01.00.05) LINE ADD END   ------------------------------------
           END-IF
      *---(01.00.06) LINE ADD START ------------------------------------
      *    ９７４ チェック
           IF  RECE10-ETC-KOHNUM(1)  =  "974" OR
               RECE10-ETC-KOHNUM(2)  =  "974" OR
               RECE10-ETC-KOHNUM(3)  =  "974" OR
               RECE10-ETC-KOHNUM(4)  =  "974"
      *---(01.00.12) LINE ADD START ------------------------------------
      *         ADD   1  TO  SKT1410TMP-K-KENSU (31)
               ADD   1  TO  SKT1410TMP-K-KENSU (32)
      *---(01.00.12) LINE ADD END   ------------------------------------
           END-IF
      *---(01.00.06) LINE ADD END   ------------------------------------
      *    老人＋原爆
            IF  RECE10-ETC-KOHNUM(1)  =  "019" OR
                RECE10-ETC-KOHNUM(2)  =  "019" OR
                RECE10-ETC-KOHNUM(3)  =  "019" OR
                RECE10-ETC-KOHNUM(4)  =  "019" 
      *---(01.00.05) LINE ADD START ------------------------------------
      *         ADD   1  TO  SKT1410TMP-K-KENSU (24)
      *---(01.00.12) LINE ADD START ------------------------------------
      *         ADD   1  TO  SKT1410TMP-K-KENSU (29)
               ADD   1  TO  SKT1410TMP-K-KENSU (30)
      *---(01.00.12) LINE ADD END   ------------------------------------
      *    療養の給付：点数
      *         COMPUTE SKT1410TMP-K-TENSU(24)  =
      *                 SKT1410TMP-K-TENSU(24)  +
      *                 RECE10-TOTALTEN(1)
      *---(01.00.05) LINE ADD END   ------------------------------------
            END-IF
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
006900*    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       230-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
           ADD     1                   TO  CNT-UPDFILE
           .
       230-UPD-FILE-HENSYU-EXT.
           EXIT.
057400*
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT       SKT1410TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *    帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM 330-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE    SKT1410TMP-FILE
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
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT1410
      *
      *    ページNO
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT1410-NO
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT1410-SRYYM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT1410-HOSPCDN
      *    保険者番号
           MOVE    SKT1410TMP-HKNJANUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT1410-HKNJANUM
      *    在総診
      *=== 在宅はまとめて集計 by Yoshikawa(2012/05/16) start
      *     IF  SKT1410TMP-ZAITAKU  =  1
      *       MOVE  "在"                  TO  SKT1410-ZAI
      *       MOVE  "○"                  TO  SKT1410-ZAIMARU
      *     END-IF
      *=== 在宅はまとめて集計 by Yoshikawa(2012/05/16) end
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT1410-SEIYMD
      *---(01.00.10) LINE ADD START ------------------------------------
      *    医療機関住所
      *     MOVE    SYS-1002-ADRS         TO  SKT1410-ADRS
      *    医療機関名
      *     MOVE    SYS-1001-HOSPNAME     TO  SKT1410-HOSPNAME
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT1410-ADRS
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) start
           INSPECT SKT1410-ADRS          REPLACING ALL "  " BY "　"
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) end
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT1410-HOSPNAME
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) start
           INSPECT SKT1410-HOSPNAME      REPLACING ALL "  " BY "　"
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) end
      *---(01.00.10) LINE ADD END   ------------------------------------
      *    開設者名
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) start
      *     MOVE    SYS-1001-KAISETUNAME  TO  SKT1410-KAISETUNAME
           MOVE    WRK-KAISETUNAME       TO  SKT1410-KAISETUNAME
      *=== 医療機関編集情報へ開設者名追加 by hagino(2015/12/10) end
      *
      *---(01.00.10) LINE ADD START ------------------------------------
      *    電話番号
      *     MOVE    SYS-1002-TEL          TO  SKT1410-TEL
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT1410-TEL
      *---(01.00.10) LINE ADD END   ------------------------------------
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT1410-TOKUBETU
           END-IF
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
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
      *    編集処理
           MOVE  1  TO  IDX4
           PERFORM  VARYING  IDX1  FROM  1  BY  1
      *---(01.00.05) LINE ADD START ------------------------------------
      *              UNTIL    IDX1  >     22
                    UNTIL    IDX1  >     15
      *---(01.00.05) LINE ADD END   ------------------------------------
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT1410TMP-KENSU(IDX1 IDX2)    TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT1410-KENSU(IDX1 IDX2)
      *    療養の給付：日数
               MOVE  SKT1410TMP-NISSU(IDX1 IDX2)    TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT1410-NISSU(IDX1 IDX2)
      *    療養の給付：点数
               MOVE  SKT1410TMP-TENSU(IDX1 IDX2)    TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT1410-TENSU(IDX1 IDX2)
      *---(01.00.05) LINE ADD START ------------------------------------
      *    療養の給付：薬剤一部負担金
      *         MOVE  SKT1410TMP-YKZFTN (IDX1 IDX2)  TO  WRK-Z7
      *         MOVE  WRK-Z7   TO  SKT1410-YKZFTN(IDX1 IDX2)
      *---(01.00.05) LINE ADD END   ------------------------------------
      *    療養の給付：一部負担金
               MOVE  SKT1410TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT1410-ITBFTN(IDX1 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT1410TMP-S-KENSU (IDX1)    TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT1410-S-KENSU(IDX1)
      *    食事療養：日数
             MOVE  SKT1410TMP-S-NISSU (IDX1)    TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT1410-S-NISSU(IDX1)
      *    食事療養：金額
             MOVE  SKT1410TMP-S-KINGAKU (IDX1)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT1410-S-KINGAKU(IDX1)
      *    食事療養：標準負担額
             MOVE  SKT1410TMP-S-FTNGAKU (IDX1)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT1410-S-FTNGAKU(IDX1)
           END-PERFORM
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
      *=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) start
      *---(01.00.13) LINE ADD START ------------------------------------
      *---(01.00.06) LINE ADD START ------------------------------------
      *---(01.00.05) LINE ADD START ------------------------------------
      ****2006/01/13 @@@@-BRAND  START
      *              UNTIL   IDX3   >    26
      *              UNTIL   IDX3   >    30
      *              UNTIL   IDX3   >    31
      ****2006/01/13 @@@@-BRABD  END
      *---(01.00.05) LINE ADD END   ------------------------------------
      *---(01.00.06) LINE ADD END   ------------------------------------
      *              UNTIL   IDX3   >    33
      *---(01.00.13) LINE ADD END   ------------------------------------
                    UNTIL   IDX3   >    34
      *=== 平成２７年１月改正対応 by Yoshikawa(2015/01/30) end
      *    療養の給付：件数
             MOVE  SKT1410TMP-K-KENSU (IDX3)  TO  WRK-Z3
             MOVE  WRK-Z3   TO  SKT1410-K-KENSU (IDX3)
      *---(01.00.05) LINE ADD START ------------------------------------
      *    療養の給付：点数
      *       MOVE  SKT1410TMP-K-TENSU (IDX3)  TO  WRK-Z7
      *       MOVE  WRK-Z7   TO  SKT1410-K-TENSU (IDX3)
      *    療養の給付：薬剤一部負担金
      *       MOVE  SKT1410TMP-K-YKZFTN (IDX3)  TO  WRK-Z7
      *       MOVE  WRK-Z7   TO  SKT1410-K-YKZFTN (IDX3)
      *    療養の給付：一部負担金
      *       MOVE  SKT1410TMP-K-ITBFTN (IDX3)  TO  WRK-Z7
      *       MOVE  WRK-Z7   TO  SKT1410-K-ITBFTN (IDX3)
      *---(01.00.05) LINE ADD END   ------------------------------------
      *    集計
           END-PERFORM
      *---(01.00.13) LINE ADD START ------------------------------------
      *---(01.00.11) LINE ADD START ------------------------------------
      **    肝炎
      *     IF  SKT1410TMP-K-KENSU (32)  NOT =  ZERO
      *         MOVE  "肝炎"        TO  SKT1410-ETC-HBT
      *         MOVE  "38"          TO  SKT1410-ETC-CODE
      *         MOVE  SKT1410TMP-K-KENSU (32)   TO  WRK-Z3
      *         MOVE  WRK-Z3                    TO  SKT1410-ETC-KENSU
      *     END-IF
      **---(01.00.11) LINE ADD END   ------------------------------------
      *---(01.00.13) LINE ADD END   ------------------------------------
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
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
           MOVE   "SKT1410.red"        TO  SPRT-PRTID
      *=== 返戻時、再印刷画面のタイトル修正 by Yoshikawa(2014/12/04) start
      *     MOVE    "国保請求書"        TO  SPRT-TITLE
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］" 
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
      *=== 返戻時、再印刷画面のタイトル修正 by Yoshikawa(2014/12/04) end
           MOVE    SKT1410             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      *=== SITEKBN変更 by Monma(2017/10/17) start
           MOVE    "2"                 TO  SPRT-SITEKBN
      *=== SITEKBN変更 by Monma(2017/10/17) end
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *---(01.00.10) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
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
      *---(01.00.10) LINE ADD START ------------------------------------
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *---(01.00.10) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   請求管理ＤＢ更新処理
      *****************************************************************
       310-SEIKYU-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               MOVE    RECE10X-KEY         TO  RECE10-KEY
               MOVE    RECE10-REC          TO  MCPDATA-REC
               PERFORM 910-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "2"             TO  RECE10-SKYKBN
      *
      *###（地方公費仕様）
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"             TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-SEIKYU-KEY    TO  MCP-PATH
      *             CALL    "MCPSUB"             USING
      *                                          MCPAREA
      *                                          MCPDATA-REC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY 
                       MOVE  "*** SOKATU1410 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU1410 SEIKYU INVALID ***"
                                           TO  WRK-RECEERR
                   MOVE    RECE10-KEY      TO  WRK-RECEERR(33:)
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
               END-IF
           END-PERFORM  
      *
           CLOSE   UPD-FILE
           .
       310-SEIKYU-UPD-EXT.
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
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) start
      *****************************************************************
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
                          WRK-HBTKBN
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       TAISYO-KOHNUM(IDXX)  =  SPACE
             IF  TAISYO-KOHNUM(IDXX)  =  TBL-HKOHNUM(1)
                                     OR  TBL-HKOHNUM(2)
                                     OR  TBL-HKOHNUM(3)
                                     OR  TBL-HKOHNUM(4)
               MOVE     1                   TO  FLG-TAISYO
               MOVE     TAISYO-KBN(IDXX)    TO  WRK-HBTKBN
               MOVE     100                 TO  IDXX
             END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *=== 対象公費をINIファイルから取得 by Yoshikawa(2012/09/18) end
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
      *###（地方公費仕様）
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE   SYS-1001-HOSPID       TO    RECE10-HOSPID.
           MOVE    SPA-HOSPNUM          TO  RECE10-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
             EVALUATE  WRK-PARA-HKNJANUM(1:3)
             WHEN "144"
               MOVE  "144000"           TO  RECE10-XXSTHKNJANUM
               MOVE  "144999"           TO  RECE10-XXEDHKNJANUM
             WHEN "145"
               MOVE  "145000"           TO  RECE10-XXSTHKNJANUM
               MOVE  "145999"           TO  RECE10-XXEDHKNJANUM
      *=== 相模原市の代表番号追加 by Yoshikawa(2010/08/04) start
             WHEN "146"
               MOVE  "146000"           TO  RECE10-XXSTHKNJANUM
               MOVE  "146999"           TO  RECE10-XXEDHKNJANUM
      *=== 相模原市の代表番号追加 by Yoshikawa(2010/08/04) end
             WHEN OTHER
               MOVE  WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                            RECE10-XXEDHKNJANUM
             END-EVALUATE
           ELSE
               MOVE    SPACE            TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"       TO  RECE10-XXEDHKNJANUM
           END-IF
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
           IF  WRK-PARA-TOKUBETU  =  "2"
      *=== open-cobol1.0対応 by Yoshikawa(2011/01/25) start
      *       MOVE  "000000"             TO  RECE10-SRYYM
      *       MOVE  "999999"             TO  RECE10-XXSRYYM
             MOVE  000000               TO  RECE10-SRYYM
             MOVE  999999               TO  RECE10-XXSRYYM
      *=== open-cobol1.0対応 by Yoshikawa(2011/01/25) end
           END-IF
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    WRK-PATH            TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    WRK-PATH            TO  MCP-PATH
      *     MOVE    "DBFETCH"           TO  MCP-FUNC
      *     CALL    "MCPSUB"                USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH                TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
      *=== オンライン返戻対応 by Yoshikawa(2010/08/04) start
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *=== オンライン返戻対応 by Yoshikawa(2010/08/04) end
      *
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
      *---(01.00.10) LINE ADD START ------------------------------------
      *         MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
      *         
               ADD     1                   TO  CNT-RECE10   
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
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
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報SELECT
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           INITIALIZE                             PTKOHINF-REC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    SYS-1001-HOSPID            TO  PTKOH-HOSPID
           MOVE    SPA-HOSPNUM                TO  PTKOH-HOSPNUM
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    RECE10-PTID                TO  PTKOH-PTID
      *=== MCPDATA-RECへの移行漏れ by Yoshikawa(2013/08/06) start
           MOVE    PTKOHINF-REC               TO  MCPDATA-REC
      *=== MCPDATA-RECへの移行漏れ by Yoshikawa(2013/08/06) end
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY2     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptkohinf"           TO  MCP-TABLE
           MOVE    "key2"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               PERFORM  900-PTKOHINF-FETCH-SEC
      *
               PERFORM  UNTIL  FLG-PTKOHINF  =  1
                 IF  PTKOH-KOHNUM  =  "978"  OR  "979"
                   IF   PTKOH-TEKSTYMD(1:6)  <=  RECE10-SRYYM
                   AND  PTKOH-TEKEDYMD(1:6)  >=  RECE10-SRYYM
                     MOVE  1     TO  FLG-KEIKA
                     MOVE  1     TO  FLG-PTKOHINF
                   END-IF
                 END-IF
                 PERFORM  900-PTKOHINF-FETCH-SEC
               END-PERFORM
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY2     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptkohinf"           TO  MCP-TABLE
           MOVE    "key2"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報FETCH
      *****************************************************************
       900-PTKOHINF-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY2     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptkohinf"           TO  MCP-TABLE
           MOVE    "key2"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTKOHINF
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           .
       900-PTKOHINF-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT1410TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT1410TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT1410TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ更新用ファイル読込
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
           END-READ
           .
       900-UPD-FILE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
      *###（地方公費仕様）
           MOVE    "DBSELECT"                   TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"              TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SEIKYU-KEY        TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SEIKYU-KEY        TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_seikyu"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
      *---(01.00.10) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SEIKYU-KEY        TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *---(01.00.10) LINE ADD START ------------------------------------
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
      * 900-HKNJAINF-INV-SEC         SECTION.
      *
      *     MOVE    "DBSELECT"                 TO  MCP-FUNC
      *     MOVE    PATH-TBL-HKNJAINF-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
      *     IF      MCP-RC              =   ZERO
      *         MOVE    "DBFETCH"                  TO  MCP-FUNC
      *         MOVE    PATH-TBL-HKNJAINF-KEY      TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
      *         IF      MCP-RC              =   ZERO
      *             MOVE    ZERO                TO  FLG-HKNJAINF
      *             MOVE    MCPDATA-REC         TO  HKNJAINF-REC
      *         ELSE
      *             MOVE    1                   TO  FLG-HKNJAINF
      *         END-IF
      *     ELSE
      *         MOVE    1                   TO  FLG-HKNJAINF
      *     END-IF
      *
      *     MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
      *     MOVE    PATH-TBL-HKNJAINF-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
      *
      *     .
      * 900-HKNJAINF-INV-EXT.
      *     EXIT.
      *---(01.00.10) LINE ADD END   ------------------------------------
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT1410TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) start
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *=== シス管「2007」対応 by Yoshikawa(2009/07/13) end
      *---(01.00.10) LINE ADD START ------------------------------------
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
           PERFORM 910-SYSKANRI-INV-SEC
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
           PERFORM 910-SYSKANRI-INV-SEC
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
           PERFORM 910-SYSKANRI-INV-SEC
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
      *---(01.00.10) LINE ADD END   ------------------------------------
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
           PERFORM 910-SYSKANRI-INV-SEC
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
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
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
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      *---(01.00.10) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    "DBSTART"           TO  MCP-FUNC.
      *---(01.00.10) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
      *---(01.00.10) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
      *
      *---(01.00.10) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.10) LINE ADD END   ------------------------------------
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      *---(01.00.10) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.10) LINE ADD END   ------------------------------------
           .                             
       900-DBDISCONNECT-EXT.
           EXIT.
