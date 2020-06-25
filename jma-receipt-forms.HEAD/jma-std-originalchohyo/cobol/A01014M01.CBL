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
       PROGRAM-ID.             A01014M01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 診療行為別月計表
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/09/12    @@@@-有村     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    11/01/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-田中    11/01/20  点数単価取得条件を修正
      *  01.00.03    @@@@-田中    12/02/02  公務災害（健保準拠）時の点数単価不具合を修正
      *  01.00.04    @@@@-門間    12/05/25  CSVファイル作成機能追加
      *  01.00.05    @@@@-門間    13/06/26  診療区分「97」を集計処理から除外
      *  01.00.06    @@@@-門間    14/01/07  療養担当手当の自動加算分を集計するように修正
      *  01.00.07    @@@@-門間    14/10/07  労災・自賠責保険時の医療収益集計方法の修正
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    15/02/05  剤点数の正負（点数区分）判定を追加
      *  02.00.03    @@@@-門間    15/06/12  初診及び再診の振分け処理を修正
      *  02.00.04    @@@@-門間    17/04/27  75歳以上が行項目の初診（後期高齢者）及び
      *                                     再診（後期高齢者）へ集計されるように変更
      *  02.00.05    @@@@-門間    18/08/20  在宅・往診及び検査の集計処理を修正
      *****************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    患者一時ファイル
           SELECT  M01TMP-FILE    ASSIGN    AM01TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  M01TMP-KEY
                                   FILE    STATUS  IS  STS-M01TMP.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求書一時ファイル
       FD  M01TMP-FILE.
           COPY    "A01014M01TMP.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //AM01TP//.
      *
           COPY    "A01014M01V02.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-RECE            PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-SYUBETU         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-M01TMP         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-SYSKANRI3            PIC 9(01).
           03  FLG-JYURRK               PIC 9(01).
           03  FLG-PASS                 PIC 9(01).
           03  FLG-TOKUTEI              PIC 9(01).
           03  FLG-TYPE                 PIC 9(01).
           03  FLG-SRYACT               PIC 9(01).
           03  FLG-SRYACCT              PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
           03  FLG-SYUNOU               PIC 9(01).
           03  FLG-SYUNOU1              PIC 9(01).
           03  FLG-NYUIN                PIC 9(01).
           03  FLG-TENSU                PIC 9(01).
           03  FLG-M01TMP               PIC 9(01).
           03  FLG-PRINT                PIC 9(01).
           03  FLG-KENSASORT            PIC 9(01).
           03  INP-STS                  PIC 9(04).
           03  FLG-NYUINACCT            PIC 9(01).
           03  FLG-PTNYUINRRK           PIC 9(01).
           03  FLG-ROOM                 PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
           03  FLG-HKNNUM               PIC 9(01).
           03  FLG-SRYACCTP             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-ACCT            PIC 9(06).
           03  CNT-ACT             PIC 9(06).
           03  CNT-NINZU           PIC 9(06).
           03  CNT-JYURRK          PIC 9(06).
           03  CNT-DATA            PIC 9(06).
           03  CNT-READ            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IIII               PIC 9(01).
           03  IDX                PIC 9(01).
           03  IDX1               PIC 9(04).
           03  IDX2               PIC 9(04).
           03  IDX3               PIC 9(04).
           03  IDX4               PIC 9(04).
           03  IDX5               PIC 9(04).
           03  IDX6               PIC 9(03).
           03  IDA-G.
               05  IDA            PIC 9(02).
           03  IDB-G.
               05  IDB            PIC 9(02).
           03  IDC                PIC 9(04).
           03  IDY                PIC 9(04).
           03  IDZ                PIC 9(04).
           03  IDM                PIC 9(04).
           03  IDX-MAX            PIC 9(04).
      *
           03  IDX8               PIC 9(04).
           03  IDX8X              PIC 9(04).
           03  IDX10              PIC 9(04).
           03  IDX-ST             PIC 9(03).
           03  IDX-ED             PIC 9(03).
           03  IDXP               PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
      *    対象診療年月
           03  WRK-PARA-SRYYM        PIC X(06).
      *    診療科
           03  WRK-PARA-SRYKA        PIC X(02).
      *    ドクターコード
           03  WRK-PARA-DRCD         PIC X(05).
      *    対象年月最終日・初日
           03  WRK-PARA-PRTLYMD      PIC X(08).
           03  WRK-PARA-PRTSYMD      PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY         PIC 9(04).
               05  WRK-SRYMM         PIC 9(02).
               05  WRK-SRYDD         PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-SRYYM             PIC X(22).
           03  WRK-SEIYMD            PIC X(22).
           03  WRK-RECEERR           PIC X(200).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-DRCD              PIC X(05).
           03  WRK-CITYNUM-G.
               05  WRK-CITYNUM       PIC X(02).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-PTID              PIC 9(10).
           03  WRK-NINZU             PIC 9(01).
           03  WRK-1-31YMD-G.
               05  WRK-1             PIC X(01).
               05  WRK-2             PIC X(02).
               05  WRK-3             PIC X(02).
               05  WRK-4             PIC X(04).
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHIKBN           PIC X(02).
           03  WRK-CHK-PTID          PIC 9(10).
           03  WRK-CHK-PTID2         PIC 9(10).
           03  WRK-CHK-SRYKA         PIC X(02).
           03  WRK-CHK-DRCD          PIC X(05).
           03  WRK-CHK-KBN1          PIC X(01).
           03  WRK-CHK-KBN2          PIC X(02).
           03  WRK-CHK-KBN3          PIC X(03).
           03  WRK-BUNRUI-1          PIC X(01).
           03  WRK-BUNRUI-2          PIC X(02).
           03  WRK-BUNRUI-3          PIC X(03).
           03  WRK-WARIAI            PIC 9(03).
           03  WRK-HENKAN-G.
               05  WRK-HENKAN-1      PIC X(03).
               05  WRK-HENKAN-2      PIC X(02).
           03  WRK-KAISU             PIC 9(09).
           03  WRK-NISSU             PIC 9(03).
           03  WRK-TENSU             PIC S9(09).
           03  WRK-TENSU2            PIC 9(09).
           03  WRK-SYUEKI            PIC S9(09).
           03  WRK-SYUEKI2           PIC S9(09).
           03  WRK-SYUEKI3           PIC 9(09).
           03  WRK-HKNNUM            PIC X(03).
           03  WRK-TENTNK            PIC 9(03)V9(02).
           03  WRK-NENREI            PIC 9(03).
           03  WRK-KNSBUNRUI         PIC 9(02).
           03  WRK-YY1-G.
               05  WRK-YY1               PIC 9(04).
           03  WRK-YY2-G.
               05  WRK-YY2               PIC 9(04).
      *
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z3                PIC ZZ9.
           03  WRK-Z4                PIC ZZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZ9.
           03  WRK-Z62               PIC ZZZZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z92               PIC ZZZZZZZZZ.
           03  WRK-Z10               PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z11               PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13               PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-Z14               PIC ZZ,ZZZ,ZZZ,ZZZ.
           03  WRK-DENPJTIKBN        PIC 9(01).
           03  WRK-ZAINUM            PIC 9(08).
           03  WRK-ZENPTID           PIC 9(10).
           03  WRK-TENSU3            PIC 9(09).
           03  WRK-4001-TENTANKA     PIC 9(02)V9(01).
           03  WRK-4001-KOMUFLG      PIC X(01).
           03  WRK-4001-KOMUTENTANKA PIC 9(02).
      *
      *    診療科名退避エリア
       01  SUM-AREA.
           03  SUM-SRYKAMAX                         PIC 9(03).
           03  SUM-SRYDR-TBL       OCCURS 100.
               05  SUM-SRYKA                        PIC X(02).
               05  SUM-SRYKANAME                    PIC X(24).
      *    入院履歴情報退避エリア
       01  WRK-NYUIN-AREA.
           03  WRK-NYUIN-TBL.
               05  WRK-NYUIN-DAY    PIC  9(01) OCCURS 31.
      *
      *    室料差額退避エリア
       01  WRK-ROOM-SAGAKU-TBL.
          03  WRK-ROOM-MAX           PIC 9(02).
          03  WRK-ROOM-DATA     OCCURS 50.
             05  WRK-ROOM-CD         PIC 9(03).
             05  WRK-ROOM-SGK        PIC 9(05).
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(100)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-KKBN       PIC X(50) VALUE "行為区分".
               07  CSV-HEAD-KENSU      PIC X(50) VALUE "件数".
               07  CSV-HEAD-NISSU      PIC X(50) VALUE "日数".
               07  CSV-HEAD-TENSU      PIC X(50) VALUE "点数".
               07  CSV-HEAD-SYUEKI     PIC X(50) VALUE "医療収益".
      *
             05  CSV-HEAD-R            REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM       PIC X(50) OCCURS 5.
             05  CSV-HEAD-MAX          PIC 9(05) VALUE  5.
      *
       01  CSV-HEAD-AREA2.
           03  CSV-HEAD-G2.
             05  CSV-HEAD-VAL2.
               07  CSV-HEAD-SS         PIC X(50)
                                       VALUE "診察（計）".
               07  CSV-HEAD-SS-SHO1    PIC X(50) VALUE "初診（６歳）".
               07  CSV-HEAD-SS-SHO2    PIC X(50) VALUE "初診（一般）".
               07  CSV-HEAD-SS-SHO3    PIC X(50) VALUE "初診（前期）".
               07  CSV-HEAD-SS-SHO4    PIC X(50) VALUE "初診（後期）".
               07  CSV-HEAD-SS-SAI1    PIC X(50) VALUE "再診（６歳）".
               07  CSV-HEAD-SS-SAI2    PIC X(50) VALUE "再診（一般）".
               07  CSV-HEAD-SS-SAI3    PIC X(50) VALUE "再診（前期）".
               07  CSV-HEAD-SS-SAI4    PIC X(50) VALUE "再診（後期）".
               07  CSV-HEAD-SS-SHIDO   PIC X(50) VALUE "指導".
               07  CSV-HEAD-SS-ETC     PIC X(50) VALUE "その他".
      *
               07  CSV-HEAD-ZO         PIC X(50)
                                       VALUE "在宅・往診（計）".
               07  CSV-HEAD-ZO-ZAITAKU PIC X(50) VALUE "在宅".
               07  CSV-HEAD-ZO-OOSHIN  PIC X(50) VALUE "往診".
      *
               07  CSV-HEAD-TY         PIC X(50)
                                       VALUE "投薬（計）".
               07  CSV-HEAD-TY-NAI     PIC X(50) VALUE "内服".
               07  CSV-HEAD-TY-TON     PIC X(50) VALUE "頓服".
               07  CSV-HEAD-TY-GAI     PIC X(50) VALUE "外用".
               07  CSV-HEAD-TY-TYOZ    PIC X(50) VALUE "調剤".
               07  CSV-HEAD-TY-SYO     PIC X(50) VALUE "処方".
               07  CSV-HEAD-TY-MAD     PIC X(50) VALUE "麻毒".
               07  CSV-HEAD-TY-TYOK    PIC X(50) VALUE "調基".
      *
               07  CSV-HEAD-TS         PIC X(50)
                                       VALUE "注射（計）".
               07  CSV-HEAD-TS-HIK     PIC X(50) VALUE "皮下筋肉内".
               07  CSV-HEAD-TS-JYO     PIC X(50) VALUE "静脈".
               07  CSV-HEAD-TS-TEN     PIC X(50) VALUE "点滴・その他".
      *
               07  CSV-HEAD-CT         PIC X(50)
                                       VALUE "処置（計）".
      *
               07  CSV-HEAD-SM         PIC X(50)
                                       VALUE "手術・麻酔（計）".
               07  CSV-HEAD-SM-SYU     PIC X(50) VALUE "手術".
               07  CSV-HEAD-SM-YUK     PIC X(50) VALUE "輸血".
               07  CSV-HEAD-SM-MAS     PIC X(50) VALUE "麻酔".
      *
               07  CSV-HEAD-KS         PIC X(50)
                                       VALUE "検査（計）".
               07  CSV-HEAD-KS-KEN     PIC X(50) VALUE "検査".
               07  CSV-HEAD-KS-SAI     PIC X(50) VALUE "採血".
               07  CSV-HEAD-KS-HAN     PIC X(50) VALUE "判断料".
               07  CSV-HEAD-KS-BYO     PIC X(50) VALUE "病理診断".
      *
               07  CSV-HEAD-GS         PIC X(50)
                                       VALUE "画像診断（計）".
      *
               07  CSV-HEAD-ETC        PIC X(50)
                                       VALUE "その他（計）".
               07  CSV-HEAD-ETC-ING    PIC X(50) VALUE "院外処方箋".
               07  CSV-HEAD-ETC-RIH    PIC X(50) VALUE "リハビリ".
               07  CSV-HEAD-ETC-ETC    PIC X(50) VALUE "その他".
      *
               07  CSV-HEAD-NYU        PIC X(50)
                                       VALUE "入院（計）".
               07  CSV-HEAD-NYU-NYU    PIC X(50) VALUE "入院基本料".
               07  CSV-HEAD-NYU-TOK    PIC X(50) VALUE "特定入院料".
               07  CSV-HEAD-NYU-ETC    PIC X(50)
                                       VALUE "その他（室料差額・食事）".
      *
               07  CSV-HEAD-JIHI       PIC X(50)
                            VALUE "自費（保険外自費・その他自費含む）".
      *
               07  CSV-HEAD-KEI        PIC X(50) VALUE "合計".
      *
             05  CSV-HEAD-R2           REDEFINES CSV-HEAD-VAL2.
               07  CSV-HEAD-ITEM2      PIC X(50) OCCURS 47.
             05  CSV-HEAD-MAX2         PIC 9(05) VALUE  47.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    職員情報情報
           COPY    "CPSK1010.INC".
      *
      *    請求自費名称情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療会計付加マスタ
       01  SRYACCTP-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院会計マスタ２
       01  NYUINACCT2-REC.
           COPY    "CPNYUINACCT.INC"   REPLACING  //NACCT-//
                                       BY         //NACCT2-//.
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    室料差額情報
           COPY  "CPSK5113.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    検査ソートマスタ
       01  KENSASORT-REC.
           COPY    "CPKENSASORT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
      *    労災保険番号マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
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
           IF      INP-STS                 =   ZERO
               PERFORM 200-MAIN-SEC
           ELSE
               PERFORM 210-ERR-SEC
           END-IF
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-PARA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-ROOM-SAGAKU-TBL
           INITIALIZE                  WRK-NYUIN-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
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
                                               RECEERR
                                               WRK-PARA-SRYYM
                                               WRK-PARA-SRYKA
                                               WRK-PARA-DRCD
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A01014M01"      TO  JOB-PGID
           MOVE    "診療行為別月計表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "A0M01TP"                TO  AM01TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  AM01TP-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  AM01TP-HOSPNUM
      *
           MOVE    AM01TP-BASENAME          TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  AM01TP-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *
           MOVE    WRK-PARA-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYM
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD               TO    WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-SRYYM     TO    WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"               TO    WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集
           INITIALIZE                     SYS-1001-REC
           MOVE    "1001"         TO      SYS-1001-KANRICD
           MOVE    "*"            TO      SYS-1001-KBNCD
           MOVE  WRK-PARA-PRTSYMD  TO     SYS-1001-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD  TO     SYS-1001-STYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC   TO      MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF     FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO    SYS-1001-REC
               IF      SYS-1001-HOSPNAME  =  LOW-VALUE
                   MOVE    SPACE   TO    SYS-1001-HOSPNAME
               END-IF
               IF      SYS-1001-KAISETUNAME  =   LOW-VALUE
                   MOVE    SPACE   TO    SYS-1001-KAISETUNAME
               END-IF
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                       TO    WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE     SPACE         TO        SYS-1031-REC
           INITIALIZE                       SYS-1031-REC
           MOVE     "1031"        TO        SYS-1031-KANRICD
           MOVE     "*"           TO        SYS-1031-KBNCD
           MOVE  WRK-PARA-PRTSYMD  TO     SYS-1031-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD  TO     SYS-1031-STYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1031-HOSPNUM
           MOVE     SYS-1031-REC  TO        MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC TO  SYS-1031-REC
           ELSE
               INITIALIZE              SYS-1031-REC
           END-IF
      *
      *    自賠責点数単価取得
           MOVE    SPACE             TO  SYS-4001-REC
           INITIALIZE                    SYS-4001-REC
           MOVE    "4001"            TO  SYS-4001-KANRICD
           MOVE    "*"               TO  SYS-4001-KBNCD
           MOVE    WRK-PARA-PRTSYMD  TO  SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD  TO  SYS-4001-STYUKYMD
           MOVE    SPA-HOSPNUM       TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC      TO  MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC     TO  SYS-4001-REC
           ELSE
             INITIALIZE              SYS-4001-REC
           END-IF
           IF    SYS-4001-TENKBN  =  "1"
             MOVE    11.5            TO  WRK-4001-TENTANKA
           ELSE
             MOVE    12              TO  WRK-4001-TENTANKA
           END-IF
           IF  SYS-4001-KOMUKBN  =  "2"
      *    公務災害(健保基準)時の点数単価設定
             MOVE    SYS-4001-KOMUTENTANKA-X TO  WRK-4001-KOMUTENTANKA
      *    公務災害区分が健保基準の場合、１を格納
             MOVE    "1"        TO  WRK-4001-KOMUFLG
           END-IF
      *
      *    診療科別印刷
           MOVE    "1005"                    TO   SYS-1005-KANRICD
           MOVE    WRK-PARA-PRTLYMD  TO    WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD  TO     SYS-1005-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD  TO     SYS-1005-STYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE   1        TO     IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
               IF   FLG-SYSKANRI3 = ZERO
                   MOVE SYS-1005-KBNCD      TO SUM-SRYKA(IDX1)
                   MOVE SYS-1005-SRYKANAME1 TO SUM-SRYKANAME(IDX1)
                   ADD  1        TO     IDX1
               END-IF
               PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           SUBTRACT  1        FROM IDX1
           MOVE  IDX1         TO   SUM-SRYKAMAX
           PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *
      *    全室料差額取得
           INITIALIZE                  SYS-5113-REC
           MOVE    "5113"              TO  SYS-5113-KANRICD
           MOVE  WRK-PARA-PRTSYMD  TO     SYS-5113-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD  TO     SYS-5113-STYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5113-HOSPNUM
           PERFORM  900-SYS-SELECT-SEC
           PERFORM  900-SYS-FETCH-SEC
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >     50
                    OR       FLG-ROOM  =  1
               IF  FLG-SYSKANRI  =  ZERO
                   MOVE  SYS-5113-BRM-SAGAKU-NM  TO WRK-ROOM-SGK(IDX4)
                   MOVE  IDX4                    TO WRK-ROOM-CD(IDX4)
               ELSE
                   MOVE  1  TO  FLG-ROOM
               END-IF
               PERFORM 900-SYS-FETCH-SEC
           END-PERFORM
           PERFORM   900-SYS-CLOSE-SEC
           IF    FLG-ROOM     =   ZERO
               MOVE  IDX4    TO    WRK-ROOM-MAX
           ELSE
               COMPUTE    WRK-ROOM-MAX  =    IDX4  -  1
           END-IF
      *
      *    システム日付セット
           MOVE  LNK-PRTKANRI-SKYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG  TO  WRK-SEIYMD
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN     OUTPUT  M01TMP-FILE
           CLOSE            M01TMP-FILE
           OPEN     I-O     M01TMP-FILE
      *
      *    データ集計処理
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  2
               MOVE    IDX    TO    WRK-NYUGAIKBN
               PERFORM  20011-SYUKEI-SEC
               PERFORM  2001-SYUKEI-SEC
           END-PERFORM
      *
           CLOSE   M01TMP-FILE
      **
           IF    CNT-DATA    >    ZERO
               PERFORM  2002-PRINT-SEC
           ELSE
               MOVE    3    TO   INP-STS
               PERFORM     210-ERR-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料データ抽出処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
           PERFORM  900-SRYACCT-SELECT-SEC
      *
           PERFORM  UNTIL    FLG-SRYACCT   =   1
      *
      *    入院料取得処理
             IF    (  ACCT-SRYKBN  NOT =  "97"  )
               PERFORM  20012-SRY-SET
             END-IF
             PERFORM  900-SRYACCT-FETCH-SEC
      *
           END-PERFORM
           PERFORM  900-SRYACCT-CLOSE-SEC
             .
       2001-SYUKEI-EXT.
            EXIT.
      ***************************************************************
      *     入院料データ抽出処理
      ****************************************************************
       20011-SYUKEI-SEC              SECTION.
      *
           PERFORM  900-NYUINACCT-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-NYUINACCT = 1
               IF  WRK-NYUGAIKBN = "1"
                   IF  WRK-CHK-PTID  NOT = NACCT-PTID
                       MOVE  NACCT-PTID   TO  WRK-CHK-PTID
                       PERFORM 20011-NYUIN-SEC
                   END-IF
      *    日数・件数加算
                   PERFORM   20000-NYUINKENSU-SEC
               END-IF
               PERFORM  900-NYUINACCT-FETCH-SEC
           END-PERFORM
      	   PERFORM  900-NYUINACCT-CLOSE-SEC
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料設定
      *****************************************************************
       20011-NYUIN-SEC                SECTION.
      *
           INITIALIZE     WRK-NYUIN-AREA
      *    入院履歴情報取得
           PERFORM    930-PTNYUINRRK-SELECT-SEC
      *
           PERFORM    UNTIL    FLG-PTNYUINRRK    =    1
      *
      *    日数・件数加算
               PERFORM  20012-NYUIN-SET-SEC
      *
               PERFORM  930-PTNYUINRRK-FETCH-SEC
           END-PERFORM
      *
           PERFORM    930-PTNYUINRRK-CLOSE-SEC
      *
      *    収納情報より、入院料・室料差額・食事取得
           PERFORM  940-SYUNOU-SELECT-SEC
           PERFORM  UNTIL   FLG-SYUNOU  =  1
      *
               PERFORM  20013-NYUIN-SET2-SEC
      *
               PERFORM  940-SYUNOU-FETCH-SEC
           END-PERFORM
      *
           PERFORM  940-SYUNOU-CLOSE-SEC
      *
           .
       20011-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院日数・件数集計処理
      *****************************************************************
       20012-NYUIN-SET-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE                      M01TMP-REC
      *
           MOVE    "1"                     TO    M01TMP-NYUGAIKBN
           MOVE    PTNYUINRRK-NYUINKA      TO    M01TMP-SRYKA
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    開始日設定
           IF  PTNYUINRRK-TENNYUYMD         <    WRK-PARA-PRTSYMD
               MOVE  1                         TO   IDA
           ELSE
               MOVE  PTNYUINRRK-TENNYUYMD(7:2) TO   IDA-G
           END-IF
      *
      *    終了日設定
           IF  PTNYUINRRK-TENSTUYMD         >    WRK-PARA-PRTLYMD
               MOVE  WRK-PARA-PRTLYMD(7:2)  TO   IDB-G
           ELSE
               MOVE  PTNYUINRRK-TENSTUYMD(7:2) TO IDB-G
           END-IF
      *
      *    期間設定
           IF  PTNYUINRRK-TENSTUYMD      =   PTNYUINRRK-TAIINYMD
             OR  PTNYUINRRK-TENSTUYMD      >   WRK-PARA-PRTLYMD
               COMPUTE   IDC             =    IDB  -  IDA  +  1
           ELSE
               COMPUTE IDC         =    IDB  -  IDA
               SUBTRACT  1         FROM IDB
           END-IF
      *    特定入院料区分で加算場所を判定
           IF  PTNYUINRRK-TOKUTEI-KBN    =   SPACE
               MOVE   ZERO                 TO  FLG-TOKUTEI
           ELSE
               MOVE   1                    TO  FLG-TOKUTEI
           END-IF
      *
      *    日数加算
           PERFORM    UNTIL    IDA  >     IDB
      **    特定区分設定
               MOVE    FLG-TOKUTEI          TO    WRK-NYUIN-DAY(IDA)
               ADD     1                    TO    IDA
           END-PERFORM
      *
           IF  FLG-M01TMP   =  1
               WRITE    M01TMP-REC
           ELSE
               REWRITE  M01TMP-REC
           END-IF
      *
           .
       20012-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院点数集計処理
      *****************************************************************
       20013-NYUIN-SET2-SEC                SECTION.
      *
      *    点数単価取得
           MOVE     SYU-SYUHKNNUM   TO    WRK-HKNNUM
           PERFORM  930-HKNNUM-SET-SEC
           IF    FLG-HKNNUM    =    1
             MOVE    10             TO   WRK-TENTNK
           ELSE
             MOVE    HKN-TENTNK     TO   WRK-TENTNK
           END-IF
      *
      *    キー設定
           INITIALIZE               M01TMP-REC
      *
           MOVE    SYU-NYUGAIKBN    TO    M01TMP-NYUGAIKBN
           MOVE    SYU-SRYKA        TO    M01TMP-SRYKA
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    入院料加算
           MOVE    ZERO             TO    WRK-SYUEKI
                                          WRK-SYUEKI2
           EVALUATE  WRK-HKNNUM
             WHEN "971"
             WHEN "973"
      *    患者労災保険情報取得
               PERFORM  900-PTRSIINF-READ-SEC
      *    点数単価を設定
               IF  PTRSI-HKNKBN  =  "5"
                   AND  WRK-4001-KOMUFLG  =  "1"
      *    公務災害（健保基準）の場合
                 MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
               ELSE
                 MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
               END-IF
      *
               COMPUTE  WRK-SYUEKI  ROUNDED  =  SYU-NYN-HKNTEN
                                             *  WRK-TENTNK
      *
               COMPUTE  WRK-SYUEKI2 ROUNDED  =  SYU-RYO-HKNTEN
                                             *  WRK-TENTNK
             WHEN  "980"  THRU  "989"
               IF   SYS-1001-JIHIPOSITION  =  "2"
                 IF   SYU-NYN-TGMONEY-TAX  NOT =  SPACE
                   COMPUTE  WRK-SYUEKI  =  SYU-NYN-TGMONEY-TAX
                   COMPUTE  WRK-SYUEKI2 =  SYU-RYO-TGMONEY-TAX
                 ELSE
                   COMPUTE  WRK-SYUEKI  =  SYU-NYN-TGMONEY
                   COMPUTE  WRK-SYUEKI2 =  SYU-RYO-TGMONEY
                 END-IF
               END-IF
             WHEN OTHER
               COMPUTE  WRK-SYUEKI  =  SYU-NYN-HKNTEN  *  WRK-TENTNK
               COMPUTE  WRK-SYUEKI2 =  SYU-RYO-HKNTEN  *  WRK-TENTNK
           END-EVALUATE
      *
           MOVE    SYU-SKYEDYMD(7:2)     TO   IDA-G
           IF    WRK-NYUIN-DAY(IDA)  =  1
             ADD    SYU-NYN-HKNTEN       TO   M01TMP-TENSU(42)
             ADD    WRK-SYUEKI           TO   M01TMP-SYUEKI(42)
           ELSE
             ADD    SYU-NYN-HKNTEN       TO   M01TMP-TENSU(41)
             ADD    WRK-SYUEKI           TO   M01TMP-SYUEKI(41)
           END-IF
      *
      *    診療区分・合計加算
           ADD    SYU-NYN-HKNTEN         TO   M01TMP-TENSU(40)
           ADD    WRK-SYUEKI             TO   M01TMP-SYUEKI(40)
           ADD    SYU-NYN-HKNTEN         TO   M01TMP-TENSU(45)
           ADD    WRK-SYUEKI             TO   M01TMP-SYUEKI(45)
      *
      *    室料差額加算
           ADD    SYU-RMSAGAKU           TO   M01TMP-SYUEKI(43)
                                              M01TMP-SYUEKI(40)
                                              M01TMP-SYUEKI(45)
      *
      *    食事加算
           ADD    SYU-RYOYOHI-SKJ        TO   M01TMP-SYUEKI(43)
                                              M01TMP-SYUEKI(40)
                                              M01TMP-SYUEKI(45)
      *
      *    療養担当手当
           ADD    SYU-RYO-HKNTEN         TO   M01TMP-TENSU(36)
                                              M01TMP-TENSU(39)
                                              M01TMP-TENSU(45)
           ADD    WRK-SYUEKI2            TO   M01TMP-SYUEKI(36)
                                              M01TMP-SYUEKI(39)
                                              M01TMP-SYUEKI(45)
      *
           IF  FLG-M01TMP  =  1
             WRITE    M01TMP-REC
           ELSE
             REWRITE  M01TMP-REC
           END-IF
      *
           .
       20013-NYUIN-SET2-EXT.
           EXIT.
      *****************************************************************
      *    入院料・件数集計処理
      *****************************************************************
       20000-NYUINKENSU-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE                    M01TMP-REC
      *
           MOVE    "1"                   TO   M01TMP-NYUGAIKBN
           MOVE    PTNYUINRRK-NYUINKA    TO   M01TMP-SRYKA
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
           PERFORM  VARYING  IDA  FROM  1  BY 1
                    UNTIL  IDA  >  31
             PERFORM   920-NYUINACCT-READ-SEC
             IF  NACCT-DAY(IDA)  >  0
               IF  NACCT-ZAISKBKBN  =  "1"
      *    入院料件数・日数加算
                 IF  WRK-NYUIN-DAY(IDA)  =   1
                   ADD     1         TO   M01TMP-KENSU(42)
                   ADD     1         TO   M01TMP-KENSU(40)
                   ADD     1         TO   M01TMP-KENSU(45)
                   IF    M01TMP-DAY(42 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(42 IDA)
                     ADD     1       TO   M01TMP-NISSU(42)
                   END-IF
                   IF    M01TMP-DAY(40 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(40 IDA)
                     ADD     1       TO   M01TMP-NISSU(40)
                   END-IF
                   IF    M01TMP-DAY(45 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(45 IDA)
                     ADD     1       TO   M01TMP-NISSU(45)
                   END-IF
                 ELSE
                   ADD     1         TO   M01TMP-KENSU(41)
                   ADD     1         TO   M01TMP-KENSU(40)
                   ADD     1         TO   M01TMP-KENSU(45)
                   IF    M01TMP-DAY(41 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(41 IDA)
                     ADD     1       TO   M01TMP-NISSU(41)
                   END-IF
                   IF    M01TMP-DAY(40 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(40 IDA)
                     ADD     1       TO   M01TMP-NISSU(40)
                   END-IF
                   IF    M01TMP-DAY(45 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(45 IDA)
                     ADD     1       TO   M01TMP-NISSU(45)
                   END-IF
                 END-IF
               END-IF
      *    室料差額、食事件数日数加算
               IF  NACCT-ZAISKBKBN  =  "3"  OR  "4"
                 IF  NACCT-SRYCDTOTAL  NOT =  "00000197000570"
                   ADD     1         TO   M01TMP-KENSU(43)
                   ADD     1         TO   M01TMP-KENSU(40)
                   ADD     1         TO   M01TMP-KENSU(45)
                   IF    M01TMP-DAY(43 IDA)   =  ZERO
                     MOVE    1       TO   M01TMP-DAY(43 IDA)
                     ADD     1       TO   M01TMP-NISSU(43)
                   END-IF
                   IF    M01TMP-DAY(45 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(45 IDA)
                     ADD     1       TO   M01TMP-NISSU(45)
                   END-IF
                   IF    M01TMP-DAY(40 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(40 IDA)
                     ADD     1       TO   M01TMP-NISSU(40)
                  END-IF
                 END-IF
               END-IF
      *    療養担当手当
               IF  NACCT-ZAISKBKBN  =  "5"
                 IF  NACCT-SRYCDTOTAL  =  "00000099999914"
                   ADD     1         TO   M01TMP-KENSU(36)
                   ADD     1         TO   M01TMP-KENSU(39)
                   ADD     1         TO   M01TMP-KENSU(45)
                   IF    M01TMP-DAY(36 IDA)   =  ZERO
                     MOVE    1       TO   M01TMP-DAY(36 IDA)
                     ADD     1       TO   M01TMP-NISSU(36)
                   END-IF
                   IF    M01TMP-DAY(39 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(39 IDA)
                     ADD     1       TO   M01TMP-NISSU(39)
                   END-IF
                   IF    M01TMP-DAY(45 IDA)  =  ZERO
                     MOVE    1       TO   M01TMP-DAY(45 IDA)
                     ADD     1       TO   M01TMP-NISSU(45)
                  END-IF
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           IF  FLG-M01TMP  =  1
             WRITE    M01TMP-REC
           ELSE
             REWRITE  M01TMP-REC
           END-IF
           .
       20000-NYUINKENSU-EXT.
           EXIT.
      *****************************************************************
      *    入院診療会計情報検索
      *****************************************************************
       920-NYUINACCT-READ-SEC           SECTION.
      *
           INITIALIZE                   NYUINACCT2-REC
           MOVE    NACCT-HOSPNUM        TO   NACCT2-HOSPNUM
           MOVE    NACCT-PTID           TO   NACCT2-PTID
           MOVE    NACCT-SRYYM          TO   NACCT2-SRYYM
           MOVE    "2"                  TO   NACCT2-ZAISKBKBN
           MOVE    NYUINACCT2-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO   MCP-TABLE
           MOVE    "key17"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM   920-NYUINACCT-FETCH-SEC
             EVALUATE  NACCT2-DAY(IDA)
               WHEN  1
               WHEN  2
               WHEN  3
                 IF   NACCT-ZAISKBKBN  =  "4"  OR  "6"
                   MOVE    0            TO   NACCT-DAY(IDA)
                 END-IF
             END-EVALUATE
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO   MCP-TABLE
           MOVE    "key17"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院診療会計情報検索
      *****************************************************************
       920-NYUINACCT-FETCH-SEC           SECTION.
      *
           MOVE    "DBFETCH"          TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
           MOVE    "key17"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC      TO   NYUINACCT2-REC
           ELSE
             INITIALIZE                    NYUINACCT2-REC
           END-IF
           .
       920-NYUINACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報取得設定
      *****************************************************************
       20012-SRY-SET                SECTION.
      *
      *    患者の主保険の点数単価取得
           INITIALIZE  HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    ACCT-PTID         TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI     TO  COMB-HKNCOMBINUM
           PERFORM  930-HKNCOMBI-SET-SEC
           MOVE    COMB-HKNNUM       TO  WRK-HKNNUM
           PERFORM  930-HKNNUM-SET-SEC
           IF    FLG-HKNNUM    =    1
             MOVE    10              TO  WRK-TENTNK
           ELSE
             MOVE    HKN-TENTNK      TO  WRK-TENTNK
           END-IF
      *
           IF  WRK-HKNNUM  =  "971"  OR  "973"
             IF    SYS-4001-TENKBN  =  "1"
               MOVE    11.5          TO  WRK-TENTNK
             ELSE
               MOVE    12            TO  WRK-TENTNK
             END-IF
      *    患者労災保険情報取得
             PERFORM  910-PTRSIINF-READ-SEC
      *    点数単価を設定
             IF  PTRSI-HKNKBN  =  "5"
                 AND  WRK-4001-KOMUFLG  =  "1"
      *    公務災害（健保基準）の場合
               MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
             ELSE
               MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
             END-IF
           END-IF
      *
      *    キー設定
           INITIALIZE                M01TMP-REC
      *
           MOVE    ACCT-NYUGAIKBN    TO  M01TMP-NYUGAIKBN
           MOVE    ACCT-SRYKA        TO  M01TMP-SRYKA
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    診療行為情報読み込み
      *    検査在宅は診療コード毎に分類する
           IF    ACCT-SRYKBN  =  "60" OR "14"
                                 OR  "64"
               PERFORM  200121-KENZAI-SEC
           ELSE
               PERFORM  200122-ETC-SEC
           END-IF
      *
           IF  FLG-M01TMP   =  1
               WRITE    M01TMP-REC
           ELSE
               REWRITE  M01TMP-REC
           END-IF
      *
           .
       20012-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査在宅点数設定処理１
      ****************************************************************
       200121-KENZAI-SEC                SECTION.
      *
           INITIALIZE  WRK-ZAINUM
      *
           PERFORM  900-SRYACT-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-SRYACT   =   1
      *
               PERFORM  VARYING  IDZ  FROM  1  BY  1
                        UNTIL    IDZ  >     5  OR
                        SRY-SRYCD(IDZ)    =   SPACE
                   IF  SRY-SRYKBN    =    "14"
                       PERFORM  200121-ZAITAKU-SET-SEC
                   ELSE
                       PERFORM  200121-KENSA-SET-SEC
                   END-IF
               END-PERFORM
      *
               PERFORM  900-SRYACT-FETCH-SEC
           END-PERFORM
      *
           PERFORM  900-SRYACT-CLOSE-SEC
      *
      *    合計に加算
      *    １レコードに付き、１回だけ診療会計の剤点数を加算
           COMPUTE  WRK-TENSU   =   ACCT-ZAITEN  *  ACCT-ZAIKAISU
           COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-TENSU  *  WRK-TENTNK
           MOVE     ZERO        TO  WRK-NISSU
                                    ACCT-ZAIKAISU
           IF  SRY-SRYKBN    =    "14"
             MOVE    46           TO    IDX2
           ELSE
             MOVE    31           TO    IDX2
           END-IF

           PERFORM   20014-GOKEI-SET-SEC
      *
           .
       200121-KENZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅点数設定処理
      *****************************************************************
       200121-ZAITAKU-SET-SEC                SECTION.
      *
           PERFORM  900-TENSU-READ-SEC
      *
           IF    TNS-CDKBN-KBN   =  "C"
             AND   TNS-CDKBN-KBNNUM  =  ZERO
               MOVE    11     TO      IDX1
           ELSE
               MOVE    12     TO      IDX1
           END-IF
      *
           MOVE    ZERO     TO   WRK-TENSU
                                 WRK-SYUEKI
           IF  ACCT-ZAINUM NOT =  WRK-ZAINUM
             COMPUTE  WRK-TENSU  =
                      ACCT-ZAITEN  * ACCT-ZAIKAISU
             COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-TENSU  *  WRK-TENTNK
      *
      *    点数加算
             ADD  WRK-TENSU        TO  M01TMP-TENSU(IDX1)
      *
      *    収益加算
             ADD  WRK-SYUEKI       TO  M01TMP-SYUEKI(IDX1)
      *
      *    件数加算
             ADD  ACCT-ZAIKAISU    TO  M01TMP-KENSU(IDX1)
      *
      *    日数
             MOVE    ZERO       TO  WRK-NISSU
             PERFORM  VARYING  IDX3  FROM  1  BY  1
                      UNTIL  IDX3  >  31
               IF  M01TMP-DAY(IDX1 IDX3)  =  ZERO
                 AND  ACCT-DAY(1 IDX3)  >  ZERO
                 MOVE    1      TO  M01TMP-DAY(IDX1 IDX3)
                 ADD  1         TO  WRK-NISSU
               END-IF
             END-PERFORM
             ADD  WRK-NISSU     TO  M01TMP-NISSU(IDX1)
      *
      *    合計に加算
             MOVE    ZERO       TO  WRK-TENSU
                                    WRK-SYUEKI
             MOVE    46         TO  IDX2
             PERFORM   20014-GOKEI-SET-SEC
      *
           END-IF
           MOVE   ACCT-ZAINUM TO  WRK-ZAINUM
           .
       200121-ZAITAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査点数設定処理
      *****************************************************************
       200121-KENSA-SET-SEC                SECTION.
      *
           IF   SRY-SRYKBN  =  "60"
      *    採血チェック
             MOVE    2               TO     WRK-KNSBUNRUI
             PERFORM  900-KENSA-READ-SEC
             IF  FLG-KENSASORT       =      ZERO
               MOVE    33          TO     IDX1
             ELSE
      *    判断料チェック
               MOVE    99              TO     WRK-KNSBUNRUI
               PERFORM  900-KENSA-READ-SEC
               IF    FLG-KENSASORT   =      ZERO
                   MOVE    34          TO     IDX1
               ELSE
                   MOVE    32          TO     IDX1
               END-IF
             END-IF
           ELSE
             MOVE    47          TO   IDX1
           END-IF
      *
      *    点数取得
           PERFORM  900-TENSU-READ-SEC
      *
           MOVE    ZERO     TO   WRK-TENSU
                                 WRK-SYUEKI
           IF  ACCT-ZAINUM NOT =  WRK-ZAINUM
             COMPUTE  WRK-TENSU  =  ACCT-ZAITEN  *  ACCT-ZAIKAISU
             COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-TENSU  *  WRK-TENTNK
      *
      *    点数加算
             ADD    WRK-TENSU      TO     M01TMP-TENSU(IDX1)
      *
      *    収益加算
             ADD    WRK-SYUEKI     TO     M01TMP-SYUEKI(IDX1)
      *
      *    件数加算
             ADD    ACCT-ZAIKAISU  TO     M01TMP-KENSU(IDX1)
      *
      *    日数
             MOVE     ZERO     TO    WRK-NISSU
             PERFORM  VARYING  IDX3  FROM   1  BY  1
                      UNTIL    IDX3  >    31
                 IF  M01TMP-DAY(IDX1 IDX3)  =  ZERO   AND
                     ACCT-DAY(1 IDX3)       >  ZERO
                     MOVE    1    TO    M01TMP-DAY(IDX1 IDX3)
                     ADD     1    TO    WRK-NISSU
                 END-IF
             END-PERFORM
             ADD     WRK-NISSU       TO    M01TMP-NISSU(IDX1)
      *
      *    合計に加算
             MOVE      ZERO          TO    WRK-TENSU
                                          WRK-SYUEKI
             MOVE      31            TO    IDX2
             PERFORM   20014-GOKEI-SET-SEC
      *
           END-IF
           MOVE   ACCT-ZAINUM TO  WRK-ZAINUM
           .
       200121-KENSA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査・在宅以外設定処理
      *****************************************************************
       200122-ETC-SEC                SECTION.
      *
      *    診療行為情報より診療種別区分取得
           PERFORM  900-SRYACT-SELECT-SEC
           PERFORM  900-SRYACT-CLOSE-SEC
      *
      *    集計インデックス設定
           PERFORM  20013-IDXSET-SEC
      *
           MOVE    ZERO     TO   WRK-TENSU
                                 WRK-SYUEKI
           IF  SRY-SRYKBN  =  "95" OR "96" OR "97"
             COMPUTE  WRK-SYUEKI  =   ACCT-ZAITEN  *  ACCT-ZAIKAISU
           ELSE
             COMPUTE  WRK-TENSU   =   ACCT-ZAITEN  *  ACCT-ZAIKAISU
             PERFORM  900-SRYACCTPLUS-READ-SEC
      *
             IF  ACCTP-PLUSKBN  =  "1"
               COMPUTE  WRK-TENSU   =   WRK-TENSU  *  -1
             END-IF
      *
             IF  ( (      WRK-HKNNUM             =  "971" OR "973" )
                     AND  FLG-SRYACCTP           =  ZERO
                     AND  ACCTP-RSIKINGAKU   NOT =  ZERO              )
               COMPUTE  WRK-SYUEKI  ROUNDED  =  ACCTP-RSIKINGAKU
                                             *  ACCT-ZAIKAISU
             ELSE
               COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-TENSU
                                             *  WRK-TENTNK
             END-IF
           END-IF
      *
           IF  IDX1  =  41  OR  42
             CONTINUE
           ELSE
      *    点数加算
               ADD    WRK-TENSU      TO     M01TMP-TENSU(IDX1)
      *
      *    収益加算
               ADD    WRK-SYUEKI     TO     M01TMP-SYUEKI(IDX1)
      *
      *    件数加算
               ADD    ACCT-ZAIKAISU  TO     M01TMP-KENSU(IDX1)
      *
      *    日数
               MOVE     ZERO     TO    WRK-NISSU
               PERFORM  VARYING  IDX3  FROM   1  BY  1
                        UNTIL    IDX3  >    31
                   IF  M01TMP-DAY(IDX1 IDX3)  =  ZERO   AND
                       ACCT-DAY(1 IDX3)       >  ZERO
                       MOVE    1    TO    M01TMP-DAY(IDX1 IDX3)
                       ADD     1    TO    WRK-NISSU
                   END-IF
               END-PERFORM
               ADD     WRK-NISSU       TO    M01TMP-NISSU(IDX1)
      *
      *    合計・診療区分加算
               PERFORM   20014-GOKEI-SET-SEC
           END-IF
      *
           .
       200122-ETC-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算場所設定処理
      *****************************************************************
       20013-IDXSET-SEC                SECTION.
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    ACCT-HOSPNUM        TO  AGECHK-HOSPNUM
           MOVE    ACCT-PTID            TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"       TO  AGECHK-SRYYMD(7:2)
           MOVE    COMB-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
           MOVE    ZERO    TO    IDX1
           EVALUATE  SRY-SRYKBN
               WHEN "11" THRU "14"
                   MOVE    1     TO    IDX2
                   EVALUATE  SRY-SRYSYUKBN
                       WHEN "110"
                           MOVE    PTINF-BIRTHDAY(1:4)  TO  WRK-YY1-G
                           MOVE    WRK-PARA-SRYYM(1:4)  TO  WRK-YY2-G
                           COMPUTE  WRK-NENREI  =  WRK-YY2  -  WRK-YY1
                           IF PTINF-BIRTHDAY(5:2) > WRK-PARA-SRYYM(5:2)
                             SUBTRACT  1        FROM WRK-NENREI
                           END-IF
                           IF  COMB-KOH1HKNNUM    =  "027"
                             OR   COMB-HKNNUM  =  "039"
                             OR   COMB-HKNNUM  =  "040"
                             OR   WRK-NENREI   >= 75
                               MOVE      5      TO    IDX1
                           ELSE
                               IF  WRK-NENREI   >= 70
                                   MOVE    4      TO    IDX1
                               ELSE
                                   IF  AGECHK-SYUGAKUKBN   =  1
                                       MOVE    2    TO    IDX1
                                   ELSE
                                       MOVE    3    TO    IDX1
                                   END-IF
                               END-IF
                           END-IF
                       WHEN "120"
                           MOVE    PTINF-BIRTHDAY(1:4)  TO  WRK-YY1-G
                           MOVE    WRK-PARA-SRYYM(1:4)  TO  WRK-YY2-G
                           COMPUTE  WRK-NENREI  =  WRK-YY2  -  WRK-YY1
                           IF PTINF-BIRTHDAY(5:2) > WRK-PARA-SRYYM(5:2)
                               SUBTRACT  1        FROM WRK-NENREI
                           END-IF
                           IF  COMB-KOH1HKNNUM    =  "027"
                             OR   COMB-HKNNUM  =  "039"
                             OR   COMB-HKNNUM  =  "040"
                             OR   WRK-NENREI   >= 75
                               MOVE      9      TO    IDX1
                           ELSE
                               IF  WRK-NENREI   >= 70
                                   MOVE    8      TO    IDX1
                               ELSE
                                   IF  AGECHK-SYUGAKUKBN   =  1
                                       MOVE    6    TO    IDX1
                                   ELSE
                                       MOVE    7    TO    IDX1
                                   END-IF
                               END-IF
                           END-IF
                       WHEN "130"
                           MOVE    10    TO    IDX1
                       WHEN OTHER
                           MOVE    13    TO    IDX1
                   END-EVALUATE
               WHEN "21" THRU "27"
                   MOVE    14      TO    IDX2
                   EVALUATE  SRY-SRYSYUKBN
                       WHEN "210" THRU "213"
                       WHEN "290" THRU "292"
                           MOVE    15     TO    IDX1
                       WHEN "220" THRU "223"
                           MOVE    16     TO    IDX1
                       WHEN "230" THRU "233"
                          MOVE    17     TO    IDX1
                       WHEN "240" THRU "242"
                           MOVE    18     TO    IDX1
                       WHEN "250"
                           MOVE    19     TO    IDX1
                       WHEN "260"
                           MOVE    20     TO    IDX1
                       WHEN "270"
                           MOVE    21     TO    IDX1
                   END-EVALUATE
               WHEN "31" THRU "34"
                   MOVE    22       TO    IDX2
                   EVALUATE  SRY-SRYSYUKBN
                       WHEN "310" THRU "311"
                           MOVE   23      TO    IDX1
                       WHEN "320" THRU "321"
                           MOVE   24      TO    IDX1
                       WHEN "330" THRU "331"
                       WHEN "340"
                           MOVE   25      TO    IDX1
                   END-EVALUATE
               WHEN "40"
                   MOVE     26      TO    IDX1
               WHEN "50" THRU "54"
                   MOVE     27      TO    IDX2
                   EVALUATE  SRY-SRYSYUKBN
                       WHEN "500" THRU "502"
                           MOVE   28      TO    IDX1
                       WHEN "510"
                           MOVE   29      TO    IDX1
                       WHEN "540" THRU "542"
                           MOVE   30      TO    IDX1
                   END-EVALUATE
               WHEN "70"
                   MOVE     35      TO    IDX1
               WHEN "80"
                   MOVE     36      TO    IDX2
                   EVALUATE  SRY-SRYSYUKBN
                       WHEN "820"
                           MOVE   37      TO    IDX1
                       WHEN  "800"
                       WHEN  "810"
                           MOVE   38      TO    IDX1
                       WHEN  "830"
                       WHEN  "840"
                       WHEN  "850"
                       WHEN  "890"
                           MOVE   39      TO    IDX1
                   END-EVALUATE
               WHEN "90"
               WHEN "92"
               WHEN "97"
                   MOVE     40      TO    IDX2
                   EVALUATE  SRY-SRYSYUKBN
                       WHEN "900"
                           MOVE   41      TO    IDX1
                       WHEN "920"
                           MOVE   42      TO    IDX1
                       WHEN "970"
                       WHEN "971"
                           MOVE   43      TO    IDX1
                   END-EVALUATE
               WHEN "95" THRU "96"
                   MOVE     44      TO    IDX1
           END-EVALUATE
      *
      *    診療種別がスペースだったら診療区分で分類する
           IF  IDX1                           =     ZERO
               EVALUATE  SRY-SRYKBN
                   WHEN "11"
                       MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                       MOVE    WRK-PARA-SRYYM(1:4)    TO    WRK-YY2-G
                       COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                       IF  PTINF-BIRTHDAY(5:2)  >  WRK-PARA-SRYYM(5:2)
                           SUBTRACT  1        FROM WRK-NENREI
                       END-IF
                       IF  COMB-KOH1HKNNUM    =  "027"
                         OR   COMB-HKNNUM  =  "039"
                         OR   COMB-HKNNUM  =  "040"
                         OR   WRK-NENREI   >= 75
                           MOVE      5      TO    IDX1
                       ELSE
                           IF  WRK-NENREI   >= 70
                               MOVE    4      TO    IDX1
                           ELSE
                               IF  WRK-NENREI  <  3
                                   MOVE    2    TO    IDX1
                               ELSE
                                   MOVE    3    TO    IDX1
                               END-IF
                           END-IF
                       END-IF
                   WHEN "12"
                       MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                       MOVE    WRK-PARA-SRYYM(1:4)    TO    WRK-YY2-G
                       COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                       IF  PTINF-BIRTHDAY(5:2)  >  WRK-PARA-SRYYM(5:2)
                           SUBTRACT  1        FROM WRK-NENREI
                       END-IF
                       IF  COMB-KOH1HKNNUM    =  "027"
                         OR   COMB-HKNNUM  =  "039"
                         OR   COMB-HKNNUM  =  "040"
                         OR   WRK-NENREI   >= 75
                           MOVE      9      TO    IDX1
                       ELSE
                           IF  WRK-NENREI   >= 70
                               MOVE    8      TO    IDX1
                           ELSE
                               IF  WRK-NENREI  <  3
                                   MOVE    6    TO    IDX1
                               ELSE
                                   MOVE    7    TO    IDX1
                               END-IF
                           END-IF
                       END-IF
                   WHEN "13"
                       MOVE     10        TO    IDX1
                   WHEN "14"
                       MOVE     11        TO    IDX1
                   WHEN "21"
                       MOVE     15        TO    IDX1
                   WHEN "22"
                       MOVE     16        TO    IDX1
                   WHEN "23"
                       MOVE     17        TO    IDX1
                   WHEN "24"
                       MOVE     18        TO    IDX1
                   WHEN "25"
                       MOVE     19        TO    IDX1
                   WHEN "26"
                       MOVE     20        TO    IDX1
                   WHEN "27"
                       MOVE     21        TO    IDX1
                   WHEN "31"
                       MOVE     23        TO    IDX1
                   WHEN "32"
                       MOVE     24        TO    IDX1
                   WHEN "33"
                       MOVE     25        TO    IDX1
                   WHEN "40"
                       MOVE     26        TO    IDX1
                   WHEN "50"
                       MOVE     28        TO    IDX1
                   WHEN "54"
                       MOVE     29        TO    IDX1
                   WHEN "70"
                       MOVE     35        TO    IDX1
                   WHEN "80"
                   WHEN "93"
                       MOVE     37        TO    IDX1
                   WHEN "95"
                   WHEN "96"
                       MOVE     44        TO    IDX1
                   WHEN "97"
                       MOVE     43        TO    IDX1
                   WHEN "90"
                       MOVE     41        TO    IDX1
                   WHEN "92"
                       MOVE     42        TO    IDX1
                   WHEN OTHER
                       MOVE     39        TO    IDX1
               END-EVALUATE
           END-IF
      *
           .
       20013-IDXSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療区分・合計加算処理
      *****************************************************************
       20014-GOKEI-SET-SEC                SECTION.
      *
           IF    SRY-SRYKBN  NOT  =   "40" AND "70" AND "95" AND "96"
               ADD    WRK-TENSU      TO     M01TMP-TENSU(IDX2)
               ADD    WRK-SYUEKI     TO     M01TMP-SYUEKI(IDX2)
               ADD    ACCT-ZAIKAISU  TO     M01TMP-KENSU(IDX2)
               MOVE     ZERO     TO    WRK-NISSU
               PERFORM  VARYING  IDX3  FROM   1  BY  1
                        UNTIL    IDX3  >    31
                   IF  M01TMP-DAY(IDX2 IDX3)  =  ZERO
                     AND    ACCT-DAY(1 IDX3)       >  ZERO
                       MOVE    1    TO    M01TMP-DAY(IDX2 IDX3)
                       ADD     1    TO    WRK-NISSU
                   END-IF
               END-PERFORM
               ADD     WRK-NISSU       TO    M01TMP-NISSU(IDX2)
           END-IF
      *    合計に加算
           ADD    WRK-TENSU        TO     M01TMP-TENSU(45)
           ADD    WRK-SYUEKI       TO     M01TMP-SYUEKI(45)
           ADD    ACCT-ZAIKAISU    TO     M01TMP-KENSU(45)
           MOVE     ZERO     TO    WRK-NISSU
           PERFORM  VARYING  IDX3  FROM   1  BY  1
                      UNTIL    IDX3  >    31
               IF  M01TMP-DAY(45 IDX3)  =  ZERO   AND
                   ACCT-DAY(1 IDX3)       >  ZERO
                   MOVE    1    TO    M01TMP-DAY(45 IDX3)
                   ADD     1    TO    WRK-NISSU
               END-IF
           END-PERFORM
           ADD     WRK-NISSU       TO    M01TMP-NISSU(45)
      *
           .
       20014-GOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE     M01
           MOVE    "対象データ無し"  TO   M01-SRYKANAME
      *
           PERFORM 330-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT      M01TMP-FILE
           MOVE    ZERO    TO    FLG-PRINT
           INITIALIZE          M01TMP-REC
      *
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT   =    1
      *
      *    見出し編集
               PERFORM  310-HEAD-HEN-SEC
      *
      *    明細編集
               PERFORM  320-BODY-HEN-SEC
      *
      *    印刷処理
               PERFORM  330-PRINT-OUT-SEC
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    M01TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC                 SECTION.
      *
           INITIALIZE                      M01
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    M01-PAGE
      *
           MOVE     1               TO    IDX4
           MOVE     SUM-SRYKAMAX    TO    IDX-MAX
           PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF  M01TMP-SRYKA       =     SUM-SRYKA(IDX4)
                   MOVE    ZERO                   TO    IDX-MAX
                   MOVE    SUM-SRYKANAME(IDX4)    TO    M01-SRYKANAME
               ELSE
                   ADD     1                      TO    IDX4
               END-IF
           END-PERFORM
      *
           IF    M01TMP-NYUGAIKBN      =    "1"
               MOVE     "入院"        TO   M01-NYUGAI
           ELSE
               MOVE     "外来"        TO   M01-NYUGAI
           END-IF
      *
           IF WRK-PARA-SRYYM  <  "200804"
               MOVE     "(３歳未満)"  TO   M01-NENREIKBN(1)
               MOVE     "(老人)"      TO   M01-NENREIKBN2(1)
               MOVE     "(３歳未満)"  TO   M01-NENREIKBN(2)
               MOVE     "(老人)"      TO   M01-NENREIKBN2(2)
           ELSE
               MOVE     "(義務教育就学前)"  TO   M01-NENREIKBN(1)
               MOVE     "(後期高齢者)"      TO   M01-NENREIKBN2(1)
               MOVE     "(義務教育就学前)"  TO   M01-NENREIKBN(2)
               MOVE     "(後期高齢者)"      TO   M01-NENREIKBN2(2)
           END-IF
      *
           MOVE    WRK-SRYYM        TO   M01-SRYYM
           MOVE    WRK-SEIYMD       TO   M01-SYSYMD
      *
      *    CSV ヘッダー処理
           MOVE    1                   TO  IDXP
           PERFORM  800-CSV-HEAD-SEC
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細>処理
      *****************************************************************
       320-BODY-HEN-SEC               SECTION.
           PERFORM    VARYING    IDX1    FROM  1  BY  1
                    UNTIL  IDX1  >  47
      *
               MOVE    M01TMP-KENSU(IDX1)   TO    WRK-Z62
               MOVE    WRK-Z62              TO    M01-KENSU(IDX1)
      *
               MOVE    M01TMP-NISSU(IDX1)   TO    WRK-Z4
               MOVE    WRK-Z4               TO    M01-NISSU(IDX1)
      *
               MOVE    M01TMP-TENSU(IDX1)   TO    WRK-Z92
               MOVE    WRK-Z92              TO    M01-TENSU(IDX1)
      *
               MOVE    M01TMP-SYUEKI(IDX1)  TO    WRK-Z92
               MOVE    WRK-Z92              TO    M01-SYUEKI(IDX1)
           END-PERFORM
      *
           PERFORM  400-CSV-DATA-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
      *
           INITIALIZE                       ORCSPRTAREA
           MOVE    "INS"                    TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM      TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY     TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP   TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM       TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD      TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID     TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY    TO   SPRT-PRIORITY
           MOVE    "A01014M01V02.red"        TO   SPRT-PRTID
           MOVE    "診療行為別月計表"       TO   SPRT-TITLE
           MOVE    M01                      TO   SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID      TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID        TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM       TO   SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           END-IF
      *
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           IF  CNT-PAGE  NOT =  1
             INITIALIZE  WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
           END-IF
      *
      *    対象年月
           INITIALIZE  WRK-ITEM
           STRING  "対象年月："              DELIMITED  BY  SIZE
                   M01-SRYYM                 DELIMITED  BY  SPACE
                                             INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療科
           INITIALIZE  WRK-ITEM
           STRING  "診療科："                DELIMITED  BY  SIZE
                   M01-SRYKANAME             DELIMITED  BY  SPACE
                                             INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療科
           INITIALIZE  WRK-ITEM
           STRING  "入外区分："              DELIMITED  BY  SIZE
                   M01-NYUGAI                DELIMITED  BY  SPACE
                                             INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    作成年月日
           INITIALIZE  WRK-ITEM
           MOVE    M01-SYSYMD                TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    列項目
           PERFORM  VARYING  IDX8  FROM  1  BY  1
                    UNTIL    IDX8  >  CSV-HEAD-MAX
      *
             INITIALIZE  WRK-ITEM
             MOVE    CSV-HEAD-ITEM(IDX8)     TO  WRK-ITEM
      *
             IF  IDX8  =  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
           END-PERFORM
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV明細編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
      *    行項目、明細情報
           PERFORM  VARYING  IDX8  FROM  1  BY  1
                    UNTIL    IDX8  >  CSV-HEAD-MAX2
      *
             EVALUATE  IDX8
               WHEN   1 THRU 10
                 COMPUTE  IDX8X  =  IDX8
               WHEN  11
                 COMPUTE  IDX8X  =  IDX8  +   2
               WHEN  12
                 COMPUTE  IDX8X  =  IDX8  +  34
               WHEN  13 THRU 14
                 COMPUTE  IDX8X  =  IDX8  -   2
               WHEN  15 THRU 35
                 COMPUTE  IDX8X  =  IDX8  -   1
               WHEN  36
                 COMPUTE  IDX8X  =  IDX8  +  11
               WHEN  37 THRU 47
                 COMPUTE  IDX8X  =  IDX8  -   2
             END-EVALUATE
      *
      *      行項目
             INITIALIZE  WRK-ITEM
             MOVE    CSV-HEAD-ITEM2(IDX8)     TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      件数
             INITIALIZE  WRK-ITEM
             MOVE    M01TMP-KENSU(IDX8X)      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      日数
             INITIALIZE  WRK-ITEM
             MOVE    M01TMP-NISSU(IDX8X)      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      点数
             INITIALIZE  WRK-ITEM
             MOVE    M01TMP-TENSU(IDX8X)      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      医療収益
             INITIALIZE  WRK-ITEM
             MOVE    M01TMP-SYUEKI(IDX8X)     TO  WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
      *
           END-PERFORM
      *
      *
           .
      *
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDXP
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","       DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"     DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX10  FROM  1  BY  1
                    UNTIL  IDX10  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX10:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX10:1)  NOT =  SPACE
                 MOVE    IDX10      TO  IDX-ST
                 MOVE    1          TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    CSV出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                        ORCSTOUKEICSVAREA
           MOVE    "INS"                     TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY      TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP    TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID      TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM        TO  STOUKEICSV-SRYYM
           MOVE    ZERO                      TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"               TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM               DELIMITED  BY  SIZE
                   "shinryoukouigekkei_"     DELIMITED  BY  SIZE
                   WRK-PARA-SRYYM            DELIMITED  BY  SPACE
                   ".csv"                    DELIMITED  BY  SIZE
                                             INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA               TO  STOUKEICSV-CSVDATA
           MOVE    "診療行為別月計表"        TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"           USING
                                             ORCSTOUKEICSVAREA
                                             SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE  WRK-CSVDATA
           MOVE    1   TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"        TO  MCP-TABLE
               MOVE    "key10"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-1005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key2"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF      MCP-RC           =   ZERO
               PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1005-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
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
           MOVE   "DBCLOSECURSOR"         TO  MCP-FUNC
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
      *    管理マスタ検索(職員情報)
      *****************************************************************
       821-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-1010-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC           =   ZERO
               PERFORM 821-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
           END-IF
           .
       821-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(職員情報)
      *****************************************************************
       821-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1010-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
           END-IF
           .
       821-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       821-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       821-SYSKANRI-DBCLOSE-EXT.
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
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
           .
       31012-SEIWA-HEN-EXT.
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
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
      *
      *        ジョブ終了処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
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
           CLOSE   RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    M01TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-M01TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-M01TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  M01TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT AT END
                   ADD   1   TO    CNT-READ
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"             TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC   SECTION.
      *
           INITIALIZE                    TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM      TO  TNS-HOSPNUM
           MOVE    SRY-SRYCD(IDZ)        TO  TNS-SRYCD
           MOVE    ALL "9"               TO  TNS-YUKOSTYMD
                                             TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_tensu"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_tensu"         TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC       TO  TNS-TENSU-REC
               MOVE    ZERO              TO  FLG-TENSU
             ELSE
               MOVE    1                 TO  FLG-TENSU
               INITIALIZE                TNS-TENSU-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-TENSU
             INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_tensu"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査ソートマスタ読込処理
      *****************************************************************
       900-KENSA-READ-SEC   SECTION.
      *
           INITIALIZE                  KENSASORT-REC
           MOVE    SRY-SRYCD(IDZ)      TO  KENSASORT-SRYCD
           MOVE    WRK-KNSBUNRUI       TO  KENSASORT-KNSBUNRUI
           MOVE    KENSASORT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_kensasort"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_kensasort"   TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  KENSASORT-REC
               MOVE    ZERO            TO  FLG-KENSASORT
             ELSE
               MOVE    1               TO  FLG-KENSASORT
               INITIALIZE              KENSASORT-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-KENSASORT
             INITIALIZE                KENSASORT-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_kensasort"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-KENSA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報SELECT(抽出)
      *****************************************************************
       900-SRYACT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO  SRY-PTID
           MOVE    ACCT-SRYKA           TO  SRY-SRYKA
           MOVE    ACCT-SRYYM           TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO  SRY-ZAINUM
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO               TO  FLG-SRYACT
             PERFORM   900-SRYACT-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報１件読み込み(抽出)
      *****************************************************************
       900-SRYACT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    MCPDATA-REC        TO  SRYACT-REC
             MOVE    ZERO               TO  FLG-SRYACT
           ELSE
             MOVE    1                  TO  FLG-SRYACT
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報クローズ(抽出)
      *****************************************************************
       900-SRYACT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC
           MOVE    WRK-PARA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM       TO  ACCT-SRYYM
           MOVE    WRK-NYUGAIKBN        TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC          =   ZERO
               MOVE    ZERO         TO  FLG-SRYACCT
               PERFORM    900-SRYACCT-FETCH-SEC
           ELSE
               MOVE   1             TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報１件読み込み
      *****************************************************************
       900-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
      *    テスト患者は対象外
               MOVE    ACCT-PTID    TO  WRK-PTID
               PERFORM  900-PTINF-READ-SEC
               IF      FLG-PTINF             =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SRYACCT-FETCH-SEC
               END-IF
      *    コメントは対象外
               IF    ACCT-SRYKBN    =    "99"  OR  "98"
                   GO    TO    900-SRYACCT-FETCH-SEC
               END-IF
               ADD    1               TO  CNT-DATA
           ELSE
               MOVE   1               TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計付加
      *****************************************************************
       900-SRYACCTPLUS-READ-SEC         SECTION.
      *
           INITIALIZE                   SRYACCTP-REC
           MOVE    ACCT-HOSPNUM         TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID            TO  ACCTP-PTID
           MOVE    ACCT-SRYKA           TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM           TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM          TO  ACCTP-ZAINUM
           MOVE    SRYACCTP-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"    TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_sryacctplus"    TO  MCP-TABLE
             MOVE    "key2"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC        TO  SRYACCTP-REC
               MOVE    ZERO               TO  FLG-SRYACCTP
             ELSE
               MOVE    1                  TO  FLG-SRYACCTP
             END-IF
           ELSE
             MOVE    1                  TO  FLG-SRYACCTP
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"    TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *******************************************************************:
      *   入院 診療会計情報SELECT
      *****************************************************************
       900-NYUINACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   NYUINACCT-REC
           MOVE    WRK-PARA-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM       TO  NACCT-SRYYM
           MOVE    WRK-NYUGAIKBN        TO  NACCT-NYUGAIKBN
           MOVE    NYUINACCT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC               =   ZERO
               MOVE    ZERO         TO  FLG-NYUINACCT
               PERFORM    900-NYUINACCT-FETCH-SEC
           ELSE
               MOVE   1             TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *   入院 診療会計情報１件読み込み
      *****************************************************************
       900-NYUINACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
      *    テスト患者は対象外
               MOVE    NACCT-PTID    TO  WRK-PTID
               PERFORM  900-PTINF-READ-SEC
               IF      FLG-PTINF             =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-NYUINACCT-FETCH-SEC
               END-IF
      *    コメントは対象外
               IF    NACCT-SRYKBN    =    "99"
                 GO    TO    900-NYUINACCT-FETCH-SEC
               END-IF
               ADD    1               TO  CNT-DATA
           ELSE
               MOVE   1               TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *  入院  診療会計情報クローズ
      *****************************************************************
       900-NYUINACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-NYUINACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       900-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    収納情報SELECT
      *****************************************************************
       940-SYUNOU-SELECT-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    WRK-PARA-HOSPNUM    TO   SYU-HOSPNUM
           MOVE    WRK-PTID            TO   SYU-PTID
           MOVE    WRK-NYUGAIKBN       TO   SYU-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD     TO  SYU-SKYSTYMD
           MOVE    WRK-PARA-PRTLYMD     TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               PERFORM 940-SYUNOU-FETCH-SEC
           ELSE
               MOVE     1               TO  FLG-SYUNOU
           END-IF
           .
       940-SYUNOU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    収納情報１件読み込み
      *****************************************************************
       940-SYUNOU-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC        =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
               IF    SYU-DENPJTIKBN    =   "3"
                 GO    TO    940-SYUNOU-FETCH-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           .
       940-SYUNOU-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    収納情報クローズ
      *****************************************************************
       940-SYUNOU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       940-SYUNOU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    収納情報SELECT
      *****************************************************************
       900-SYUNOU-SELECT-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    WRK-PARA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN        TO  SYU-NYUGAIKBN
           MOVE    WRK-DENPJTIKBN       TO  SYU-DENPJTIKBN
           MOVE    WRK-PARA-PRTSYMD     TO  SYU-SKYSTYMD
           MOVE    WRK-PARA-PRTLYMD     TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC           =   ZERO
               PERFORM 900-SYUNOU-FETCH-SEC
           ELSE
               MOVE   1               TO  FLG-SYUNOU1
           END-IF
           .
       900-SYUNOU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    収納情報１件読み込み
      *****************************************************************
       900-SYUNOU-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU1
               MOVE    SYU-PTID   TO   WRK-PTID
               IF    SYU-DENPJTIKBN    =   "3"
                   GO    TO    900-SYUNOU-FETCH-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU1
           END-IF
           .
       900-SYUNOU-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    収納情報クローズ
      *****************************************************************
       900-SYUNOU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SYUNOU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ情報セット
      *****************************************************************
       930-HKNCOMBI-SET-SEC  SECTION.
      *
           MOVE    HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE     "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_hkncombi"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  HKNCOMBI-REC
               MOVE    ZERO             TO  FLG-HKNCOMBI
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE    1                TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       930-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険番号情報セット
      *****************************************************************
       930-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE    WRK-PARA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM           TO  HKN-HKNNUM
           MOVE    "00"                 TO  HKN-PAYKBN
           MOVE    ALL "9"              TO  HKN-TEKSTYMD
           MOVE    ZERO                 TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE  "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_hknnum"     TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
               IF  MCP-RC  =  ZERO
                   MOVE  MCPDATA-REC  TO  HKNNUM-REC
                   MOVE  ZERO         TO  FLG-HKNNUM
               ELSE
                   INITIALIZE  HKNCOMBI-REC
                   MOVE  1            TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1            TO  FLG-HKNNUM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       930-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報SELECT
      *****************************************************************
       930-PTNYUINRRK-SELECT-SEC         SECTION.
      *
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PTID             TO  PTNYUINRRK-PTID
           MOVE    WRK-PARA-PRTLYMD     TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-PARA-PRTSYMD     TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               PERFORM 930-PTNYUINRRK-FETCH-SEC
           ELSE
               MOVE   1               TO  FLG-PTNYUINRRK
           END-IF
           .
       930-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報１件読み込み
      *****************************************************************
       930-PTNYUINRRK-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               IF    PTNYUINRRK-TENSTUYMD  < WRK-PARA-PRTSYMD
                   GO    TO    930-PTNYUINRRK-FETCH-SEC
               END-IF
               IF    PTNYUINRRK-TENNYUYMD  > WRK-PARA-PRTLYMD
                   GO    TO    930-PTNYUINRRK-FETCH-SEC
               END-IF
      *
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           .
       930-PTNYUINRRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報クローズ
      *****************************************************************
       930-PTNYUINRRK-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       930-PTNYUINRRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理SELECT(室料差額用)
      *****************************************************************
       900-SYS-SELECT-SEC         SECTION.
      *
           MOVE    SYS-5113-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC     NOT  =   ZERO
               MOVE    1                TO  FLG-ROOM
           END-IF
           .
       900-SYS-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理１件読み込み(室料差額用)
      *****************************************************************
       900-SYS-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-5113-REC
               MOVE    ZERO             TO  FLG-ROOM
           ELSE
               MOVE    1                TO  FLG-ROOM
           END-IF
           .
       900-SYS-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    システム管理クローズ(室料差額用)
      *****************************************************************
       900-SYS-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SYS-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       910-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC
           MOVE    WRK-PARA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM       TO  ACCT-SRYYM
           MOVE    SYU-NYUGAIKBN        TO  ACCT-NYUGAIKBN
           MOVE    SYU-PTID             TO  ACCT-PTID
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC          =   ZERO
               MOVE    ZERO         TO  FLG-SRYACCT
               PERFORM    910-SRYACCT-FETCH-SEC
           ELSE
               MOVE   1             TO  FLG-SRYACCT
           END-IF
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報１件読み込み
      *****************************************************************
       910-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
      *    テスト患者は対象外
               MOVE    ACCT-PTID    TO  WRK-PTID
               PERFORM  900-PTINF-READ-SEC
               IF      FLG-PTINF             =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  910-SRYACCT-FETCH-SEC
               END-IF
      *    コメントは対象外
               IF    ACCT-SRYKBN    =    "99"  OR  "98"
                   GO    TO    910-SRYACCT-FETCH-SEC
               END-IF
               ADD    1               TO  CNT-DATA
           ELSE
               MOVE   1               TO  FLG-SRYACCT
           END-IF
           .
       910-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       910-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
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
      *    保険組合せマスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
      *    保険組合せ読み出し
           INITIALIZE                    HKNCOMBI-REC
           MOVE    SYU-HOSPNUM           TO  COMB-HOSPNUM
           MOVE    SYU-PTID              TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM       TO  COMB-HKNCOMBINUM
           PERFORM  930-HKNCOMBI-SET-SEC
           IF   FLG-HKNCOMBI  =  ZERO
      *    患者保険情報読み込み
             INITIALIZE                  PTRSIINF-REC
             MOVE    SYU-HOSPNUM         TO  PTRSI-HOSPNUM
             MOVE    SYU-PTID            TO  PTRSI-PTID
             MOVE    COMB-HKNID          TO  PTRSI-HKNID
             MOVE    PTRSIINF-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    "DBFETCH"         TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF   MCP-RC  =  ZERO
                 MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               ELSE
                 INITIALIZE              PTRSIINF-REC
               END-IF
             END-IF
      *
             MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           END-IF
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       910-PTRSIINF-READ-SEC         SECTION.
      *
      *    患者保険情報読み込み
           INITIALIZE                  PTRSIINF-REC
           MOVE    ACCT-HOSPNUM        TO  PTRSI-HOSPNUM
           MOVE    ACCT-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
             ELSE
               INITIALIZE              PTRSIINF-REC
             END-IF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
