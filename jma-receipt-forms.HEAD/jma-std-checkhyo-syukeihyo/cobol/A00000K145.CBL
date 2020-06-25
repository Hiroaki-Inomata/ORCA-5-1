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
      *******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000K145.
      *******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : (病棟別)診療行為区分別集計表
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/03/26    @@@@-半田     新規作成
      *******************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者      日付      内容
      *   01.00.01    @@@@-田中   11/01/05  open-cobol1.0対応
      *                                     コメント履歴削除
      *   01.00.02    @@@@-門間   11/12/13  ヘッダ項目に数列が表示される不具合を修正
      *   01.00.03    @@@@-門間   12/05/22  CSVファイル作成機能追加
      *   01.00.04    @@@@-門間   13/02/21  CSVデータの処理順を変更
      *   01.00.05    @@@@-門間   13/06/26  診療区分「97」を集計処理から除外
      *   01.00.06    @@@@-吉川   13/08/26  在宅の集計不具合修正
      *   01.00.07    @@@@-門間   14/01/07  療養担当手当の自動加算分を集計するように修正
      *   01.00.08    @@@@-門間   14/04/14  「その他」回数欄の不具合を修正
      *   01.00.09    @@@@-門間   14/07/03  自・他院歴の情報は読み込まないように修正
      *                                     入院履歴が存在しない場合は「入院履歴なし」へ集計するよう変更
      *   02.00.01    @@@@-門間   14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *   02.00.02    @@@@-門間   15/08/25  剤点数の正負（点数区分）判定を変更
      *   02.00.03    @@@@-門間   17/04/27  75歳以上が行項目の初診（後期高齢者）及び
      *                                     再診（後期高齢者）へ集計されるように変更
      *   02.00.04    @@@@-門間   18/02/01  金額表示機能の追加
      *******************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療行為受診対象患者一時ファイル
           SELECT  K145T-FILE      ASSIGN              AK145T
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  K145TMP-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為受診対象患者一時ファイル
       FD  K145T-FILE.
           COPY    "A00000K145TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    診療行為受診対象患者一時ファイル(外部ファイル名称)
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //AK145T//.
      *
           COPY    "A00000K100V02.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-RECE            PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-SYUBETU         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-M01TMP          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-SYSKANRI4       PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-TOKUTEI         PIC 9(01).
           03  FLG-TYPE            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACCT2        PIC 9(01).
           03  FLG-NSRYACCT        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-NYUIN           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-K145NPT         PIC 9(01).
           03  FLG-K145JPT         PIC 9(01).
           03  FLG-M01TMP          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KENSASORT       PIC 9(01).
           03  INP-STS             PIC 9(04).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-ROOM            PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNTG           PIC 9(01).
           03  GET-OFF             PIC 9(01).
           03  GET-TENSU           PIC 9(01).
           03  FLG-NKENSU          PIC 9(01).
           03  FLG-SRYACCTP        PIC 9(01).
           03  FLG-JBI             PIC 9(01).
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
           03  CNT-K145TMP         PIC 9(06).
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
           03  IDX                PIC 9(01).
           03  IDX1               PIC 9(04).
           03  IDX2               PIC 9(04).
           03  IDX3               PIC 9(04).
           03  IDX4               PIC 9(04).
           03  IDX5G.
               05  IDX5           PIC 9(04).
           03  IDX6               PIC 9(03).
           03  IDX7G.
               05  IDX7           PIC 9(04).
           03  IDA-G.
               05  IDA            PIC 9(02).
           03  IDB-G.
               05  IDB            PIC 9(02).
           03  IDC                PIC 9(04).
           03  IDY                PIC 9(04).
           03  IDZ                PIC 9(04).
           03  IDM                PIC 9(04).
           03  IDX-MAX            PIC 9(04).
           03  IDX-TEN            PIC 9(02).
      *
           03  IDX8                PIC 9(04).
           03  IDX9                PIC 9(04).
           03  IDX9X               PIC 9(04).
           03  IDX10               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-ST              PIC 9(03).
           03  IDX-ED              PIC 9(03).
           03  IDXP                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
      *    対象年月最終日・初日
           03  WRK-PARA-PRTLYMD      PIC X(08).
           03  WRK-PARA-PRTSYMD      PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-DENPJTIKBN        PIC 9(01).
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
           03  WRK-PTID2             PIC 9(10).
           03  WRK-ZENJYUYMD         PIC X(08).
           03  WRK-TENSU2            PIC S9(09).
           03  WRK-SYUEKI            PIC 9(09).
           03  WRK-SYUEKI2           PIC 9(09).
           03  WRK-SYUEKI3           PIC 9(09).
           03  WRK-HKNNUM            PIC X(03).
           03  WRK-TENTNK            PIC 9(02)V9(01).
           03  WRK-TENTNK-JG         PIC 9(02)V9(01).
           03  WRK-TENTNK-JY         PIC 9(02)V9(01).
           03  WRK-NENREI            PIC 9(03).
           03  WRK-KNSBUNRUI         PIC 9(02).
           03  WRK-TMP-NYUGAIKBN     PIC X(01).
           03  WRK-CNV-TEN-V         PIC S9(11)V9(02).
           03  WRK-CNV-TEN           PIC S9(11).
           03  WRK-YY1-G.
               05  WRK-YY1           PIC 9(04).
           03  WRK-YY2-G.
               05  WRK-YY2           PIC 9(04).
           03  WRK-TJIHI-G           OCCURS  10.
               05  WRK-TJIHI         PIC X(20).
               05  WRK-TJIHI-FLG     PIC 9(01).
           03  WRK-TJIHIN-G          OCCURS  10.
               05  WRK-TJIHIN        PIC X(20).
               05  WRK-TJIHIN-FLG    PIC 9(01).
           03  WRK-SKYSTYMD.
               05  WRK-SKYSTY        PIC 9(04).
               05  WRK-SKYSTM        PIC 9(02).
               05  WRK-SKYSTD        PIC 9(02).
           03  WRK-SKYEDYMD.
               05  WRK-SKYEDY        PIC 9(04).
               05  WRK-SKYEDM        PIC 9(02).
               05  WRK-SKYEDD        PIC 9(02).
           03  WRK-N-NISSU           PIC 9(02).
           03  WRK-CTRL-ZAINUM.
               05  WRK-CZ-SRYYMD     PIC X(08).
               05  WRK-CZ-PTID       PIC 9(10).
               05  WRK-CZ-SRYKBN     PIC X(02).
               05  WRK-CZ-ZAINUM     PIC 9(08).
           03  WRK-DD                PIC X(02).
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
           03  WRK-TENSU-HEN         PIC ZZZZZZZZZZZ.
           03  WRK-KAISU-HEN         PIC ZZZZZZZZZZZ.
           03  WRK-BTU-SRYYMD.
               05 WRK-BTU-SRYY       PIC 9(04).
               05 WRK-BTU-SRYM       PIC 9(02).
               05 WRK-BTU-SRYD       PIC 9(02).
           03  WRK-BTU-INDEX         PIC X(02).
           03  WRK-ZAINUM            PIC 9(08).
           03  WRK-ZENPTID           PIC 9(10).
           03  WRK-ZENPTID2          PIC 9(10).
           03  WRK-TENSU3            PIC 9(09).
           03  WRK-TENYMD            PIC X(08).
           03  WRK-T2NAME            PIC X(04).
           03  WRK-R-TTNK            PIC 9(02)V9(01).
           03  WRK-J-TTNK            PIC 9(02)V9(01).
           03  WRK-JG-TTNK           PIC 9(02)V9(01).
           03  WRK-JY-TTNK           PIC 9(02)V9(01).
           03  WRK-K-TTNK            PIC 9(02)V9(01).
           03  WRK-ZAITEN            PIC 9(08).
           03  WRK-ZAITEN1           PIC 9(08).
           03  WRK-ZAITEN2           PIC 9(08).
           03  WRK-HKNCOMBI          PIC X(04).
      *
      *    病棟名退避エリア
       01  WRK-BTU-AREA.
           03  WRK-BTUMAX             PIC 9(03).
           03  WRK-BTU-TBL            OCCURS 500.
               05  WRK-BTUNUM         PIC X(02).
               05  WRK-BTUNAME        PIC X(20).
      *
      *    入院履歴情報退避エリア
       01  WRK-NYUIN-AREA.
           03  WRK-NYUIN-TBL.
               05  WRK-NYUIN-DAY      PIC  9(01) OCCURS 31.
      *
      *    室料差額退避エリア
       01  WRK-ROOM-SAGAKU-AREA.
           03  WRK-ROOM-MAX           PIC 9(02).
           03  WRK-ROOM-DATA          OCCURS 50.
               05  WRK-ROOM-CD        PIC 9(03).
               05  WRK-ROOM-SGK       PIC 9(05).
      *
      *   入外別全医師加算エリア
       01 WRK-K145ALL-AREA.
           03  WRK-K145ALL-SRY-TBL OCCURS 56.
               05  WRK-K145ALL-TENSU  PIC  9(11).
               05  WRK-K145ALL-KAISU  PIC  9(11).
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  CSV-BODY-AREA.
           03  CSV-BODY-G              OCCURS 30.
             05  WRK-BODY-NAME           PIC X(20).
             05  WRK-BODY-TBL            OCCURS 56.
               07  WRK-BODY-KAISU          PIC X(11).
               07  WRK-BODY-TENSU          PIC X(11).
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(100)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-SS-SHOSHIN1   PIC X(50) VALUE "初診(６歳)".
               07  CSV-HEAD-SS-SHOSHIN2   PIC X(50) VALUE "初診(一般)".
               07  CSV-HEAD-SS-SHOSHIN3   PIC X(50) VALUE "初診(前期)".
               07  CSV-HEAD-SS-SHOSHIN4   PIC X(50) VALUE "初診(後期)".
               07  CSV-HEAD-SS-SAISHIN1   PIC X(50) VALUE "再診(６歳)".
               07  CSV-HEAD-SS-SAISHIN2   PIC X(50) VALUE "再診(一般)".
               07  CSV-HEAD-SS-SAISHIN3   PIC X(50) VALUE "再診(前期)".
               07  CSV-HEAD-SS-SAISHIN4   PIC X(50) VALUE "再診(後期)".
               07  CSV-HEAD-SS-SHIDO      PIC X(50) VALUE "指導".
               07  CSV-HEAD-SS-ETC        PIC X(50) VALUE "その他".
      *
               07  CSV-HEAD-SS-R          PIC X(50) VALUE "診察料(計)".
      *
               07  CSV-HEAD-OZ-OUSHIN     PIC X(50) VALUE "往診".
               07  CSV-HEAD-OZ-ZAITAKU    PIC X(50) VALUE "在宅".
      *
               07  CSV-HEAD-OZ-R          PIC X(50)
                                          VALUE "往診・在宅料(計)".
      *
               07  CSV-HEAD-TY-NAIFUKU    PIC X(50) VALUE "内服".
               07  CSV-HEAD-TY-TONPUKU    PIC X(50) VALUE "頓服".
               07  CSV-HEAD-TY-GAIYOU     PIC X(50) VALUE "外用".
               07  CSV-HEAD-TY-TYOZAI     PIC X(50) VALUE "調剤".
               07  CSV-HEAD-TY-SHOHO      PIC X(50) VALUE "処方".
               07  CSV-HEAD-TY-MADOKU     PIC X(50) VALUE "麻毒".
               07  CSV-HEAD-TY-TYOKI      PIC X(50) VALUE "調基".
      *
               07  CSV-HEAD-TY-R          PIC X(50) VALUE "投薬料(計)".
      *
               07  CSV-HEAD-TS-HIKA       PIC X(50) VALUE "皮下筋肉".
               07  CSV-HEAD-TS-JYOMYAKU   PIC X(50) VALUE "静脈".
               07  CSV-HEAD-TS-TENTEKI-ETC    PIC X(50)
                                          VALUE "点滴・その他".
      *
               07  CSV-HEAD-TS-R          PIC X(50) VALUE "注射料(計)".
      *
               07  CSV-HEAD-ST-R          PIC X(50) VALUE "処置料(計)".
      *
               07  CSV-HEAD-SM-SYUJYUTU   PIC X(50) VALUE "手術".
               07  CSV-HEAD-SM-YUKETU     PIC X(50) VALUE "輸血".
               07  CSV-HEAD-SM-MASUI      PIC X(50) VALUE "麻酔".
      *
               07  CSV-HEAD-SM-R          PIC X(50)
                                          VALUE "手術・麻酔料(計)".
      *
               07  CSV-HEAD-K-KENSA       PIC X(50) VALUE "検査".
               07  CSV-HEAD-K-SAIKETU     PIC X(50) VALUE "採血".
               07  CSV-HEAD-K-HANDAN      PIC X(50) VALUE "判断料".
               07  CSV-HEAD-K-BYORI       PIC X(50) VALUE "病理診断".
      *
               07  CSV-HEAD-K-R           PIC X(50)
                                          VALUE "検査料(計)".
      *
               07  CSV-HEAD-GS-R          PIC X(50)
                                          VALUE "画像診断料(計)".
      *
               07  CSV-HEAD-E-INGAISHOHO  PIC X(50)
                                          VALUE "院外処方せん".
               07  CSV-HEAD-E-RIHABIRI    PIC X(50) VALUE "リハビリ".
               07  CSV-HEAD-E-ETC         PIC X(50) VALUE "その他(注)".
      *
               07  CSV-HEAD-E-R           PIC X(50)
                                          VALUE "その他料(計)".
      *
               07  CSV-HEAD-RSSS          PIC X(50) VALUE
                                       "室料差額・食事／生活療養費(円)".
      *
               07  CSV-HEAD-NYUIN-R       PIC X(50)
                                          VALUE "入院料(計)".
      *
               07  CSV-HEAD-ETC-R         PIC X(50)
                                          VALUE "その他料(計)".
      *
               07  CSV-HEAD-JIHI          PIC X(50) OCCURS 10.
               07  CSV-HEAD-JIHI-K        PIC X(50)
                                          VALUE "その他自費計(円)".
      *
               07  CSV-HEAD-KEI           PIC X(50) VALUE "計".
      *
             05  CSV-HEAD-R               REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM          PIC X(50) OCCURS 56.
             05  CSV-HEAD-MAX             PIC 9(05) VALUE 56.
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
      *    職員情報情報
           COPY    "CPSK1010.INC".
      *
      *    請求自費名称情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    受診履歴マスタ
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
       01  SRYACCT-REC2.
           COPY    "CPSRYACCT.INC"   REPLACING  //ACCT-//
                                     BY         //ACCT2-//.
      *
      *    入院会計マスタ
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院会計マスタ２
       01  NYUINACCT2-REC.
           COPY    "CPNYUINACCT.INC"   REPLACING  //NACCT-//
                                       BY         //NACCT2-//.
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
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
           COPY    "CPSK5113.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療会計付加マスタ
       01  SRYACCTP-REC.
           COPY    "CPSRYACCTPLUS.INC".
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
      *    労災保険番号マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブGET-OFF
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
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
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           COPY    "A00000K100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000K100-LNK.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF  INP-STS  =  ZERO
      *    正常主処理
             PERFORM 200-MAIN-SEC
           ELSE
      *    エラー処理
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
           INITIALIZE                  WRK-K145ALL-AREA
           INITIALIZE                  WRK-BTU-AREA
           INITIALIZE                  WRK-ROOM-SAGAKU-AREA
           INITIALIZE                  WRK-NYUIN-AREA
           INITIALIZE                  SPA-AREA
                                       CSV-BODY-AREA
           INITIALIZE                  RECEERR
      *
           MOVE    K100LNK-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    K100LNK-JOBID   TO  JOB-JOBID
           MOVE    K100LNK-SHELLID TO  JOB-SHELLID
           MOVE    "A00000K145"    TO  JOB-PGID
           MOVE    "(病棟別)診療区分別集計表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "AK145TP"                TO  AK145T-FILE-ID
           MOVE    K100LNK-PRTKANRI-TERMID  TO  AK145T-TERMID
           MOVE    K100LNK-HOSPNUM          TO  AK145T-HOSPNUM
      *
           MOVE    AK145T-BASENAME          TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  AK145T-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
           MOVE    K100LNK-SRYYM       TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    K100LNK-SRYYM       TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYM
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    K100LNK-SRYYM       TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD            TO    WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    K100LNK-SRYYM       TO    WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                TO    WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集
           INITIALIZE                     SYS-1001-REC
           MOVE    "1001"         TO      SYS-1001-KANRICD
           MOVE    "*"            TO      SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM    TO      SYS-1001-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO  SYS-1001-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC   TO      MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF    FLG-SYSKANRI        =   ZERO
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
           MOVE    SPA-HOSPNUM        TO    SYS-1031-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO    SYS-1031-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO    SYS-1031-EDYUKYMD
           MOVE     SYS-1031-REC  TO        MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF    FLG-SYSKANRI        =   ZERO
             MOVE    MCPDATA-REC TO  SYS-1031-REC
           ELSE
             INITIALIZE              SYS-1031-REC
             MOVE    01    TO        INP-STS
           END-IF
      *
      *    入外区分のチェック
           IF   K100LNK-NYUGAIKBN       =   "2"
             MOVE    30      TO  INP-STS
             GO              TO  100-INIT-EXT
           END-IF
      *
      *    入外区分のチェック
           IF   K100LNK-NYUGAIKBN   NOT =   SPACE
               AND K100LNK-NYUGAIKBN   NOT =   "1"
               AND K100LNK-NYUGAIKBN   NOT =   "2"
             MOVE    10      TO  INP-STS
             GO              TO  100-INIT-EXT
           END-IF
      *
      *    保険外区分のチェック
           IF   K100LNK-HKNTG   NOT =   SPACE
               AND K100LNK-HKNTG   NOT =   "0"
             MOVE    20      TO  INP-STS
             GO              TO  100-INIT-EXT
           END-IF
      *
      *    病棟別印刷K145NPT
           PERFORM  110-BTU-PARA-HENSYU-SEC
      *
      *    入院自費情報取得
      *    区分11
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "11"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(1)
             MOVE  1                 TO  WRK-TJIHIN-FLG(1)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(1)
           END-IF
      *
      *    区分12
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "12"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(2)
             MOVE  1                 TO  WRK-TJIHIN-FLG(2)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(2)
           END-IF
      *
      *    区分13
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "13"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(3)
             MOVE  1                 TO  WRK-TJIHIN-FLG(3)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(3)
           END-IF
      *
      *    区分14
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "14"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(4)
             MOVE  1                 TO  WRK-TJIHIN-FLG(4)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(4)
           END-IF
      *
      *    区分15
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "15"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(5)
             MOVE  1                 TO  WRK-TJIHIN-FLG(5)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(5)
           END-IF
      *
      *    区分16
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "16"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(6)
               MOVE  1                 TO  WRK-TJIHIN-FLG(6)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "17"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(7)
             MOVE  1                 TO  WRK-TJIHIN-FLG(7)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "18"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(8)
             MOVE  1                 TO  WRK-TJIHIN-FLG(8)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "19"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(9)
             MOVE  1                 TO  WRK-TJIHIN-FLG(9)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "10"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(10)
             MOVE  1                 TO  WRK-TJIHIN-FLG(10)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(10)
           END-IF
      *
      *    外来自費情報取得
      *    区分21
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "21"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(1)
             MOVE  1                 TO  WRK-TJIHI-FLG(1)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(1)
           END-IF
      *
      *    区分22
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "22"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(2)
             MOVE  1                 TO  WRK-TJIHI-FLG(2)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(2)
           END-IF
      *
      *    区分23
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "23"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(3)
             MOVE  1                 TO  WRK-TJIHI-FLG(3)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(3)
           END-IF
      *
      *    区分24
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "24"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(4)
             MOVE  1                 TO  WRK-TJIHI-FLG(4)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "25"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(5)
             MOVE  1                 TO  WRK-TJIHI-FLG(5)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(5)
           END-IF
      *
      *    区分26
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "26"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(6)
             MOVE  1                 TO  WRK-TJIHI-FLG(6)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(6)
           END-IF
      *
      *    区分27
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "27"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(7)
             MOVE  1                 TO  WRK-TJIHI-FLG(7)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(7)
           END-IF
      *
      *    区分28
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "28"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(8)
             MOVE  1                 TO  WRK-TJIHI-FLG(8)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(8)
           END-IF
      *
      *    区分29
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "29"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(9)
             MOVE  1                 TO  WRK-TJIHI-FLG(9)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(9)
           END-IF
      *
      *    区分20
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "20"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(10)
             MOVE  1                 TO  WRK-TJIHI-FLG(10)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(10)
           END-IF
      *
      *    全室料差額取得
           INITIALIZE                      SYS-5113-REC
           MOVE    "5113"              TO  SYS-5113-KANRICD
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
             COMPUTE  WRK-ROOM-MAX  =    IDX4  -  1
           END-IF
      *
      *    システム日付セット
           MOVE  K100LNK-PRTKANRI-SKYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG  TO  WRK-SEIYMD
      *
      *    労災医療機関情報
           MOVE    SPACE                 TO  SYS-4001-REC
           INITIALIZE                    SYS-4001-REC
           MOVE    "4001"                TO  SYS-4001-KANRICD
           MOVE    "*"                   TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-4001-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD      TO  SYS-4001-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD      TO  SYS-4001-EDYUKYMD
           MOVE    SYS-4001-REC          TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
      *
      *    点数単価
           IF  FLG-SYSKANRI2  =  ZERO
             MOVE    MCPDATA-REC           TO  SYS-4001-REC
      *      労災
      *      点数単価  1:11.5円  2:12円
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5                TO  WRK-R-TTNK
               WHEN  "2"
                 MOVE    12                  TO  WRK-R-TTNK
               WHEN  OTHER
                 MOVE    10                  TO  WRK-R-TTNK
             END-EVALUATE
      *      自賠責
      *      自賠責区分  1:労災準拠  2:健保準拠
             EVALUATE  SYS-4001-JIBAIKBN
      *        1:労災準拠
               WHEN  "1"
                 MOVE    WRK-R-TTNK          TO  WRK-J-TTNK
      *        2:健保準拠
               WHEN  "2"
                 IF  K100LNK-NYUGAIKBN  =  "1"
      *            入院:技術点単価
                   MOVE    SYS-4001-SYUTENTANKA2   TO  WRK-JG-TTNK
      *            入院:薬剤等点単価
                   MOVE    SYS-4001-YKZTENTANKA2   TO  WRK-JY-TTNK
                 ELSE
      *            外来:技術点単価
                   MOVE    SYS-4001-SYUTENTANKA    TO  WRK-JG-TTNK
      *            外来:薬剤等点単価
                   MOVE    SYS-4001-YKZTENTANKA    TO  WRK-JY-TTNK
                 END-IF
      *          自賠責  健保準拠
                 MOVE    1                       TO  FLG-JBI
             END-EVALUATE
      *      公務災害
      *      公務災害区分  1:労災準拠  2:健保準拠
             EVALUATE  SYS-4001-KOMUKBN
      *        1:労災準拠
               WHEN  "1"
      *          公務災害労災準拠点数単価区分
                 EVALUATE  SYS-4001-KOMURSITANKAKBN
                   WHEN  "1"
                     MOVE    WRK-R-TTNK              TO  WRK-K-TTNK
                   WHEN  "2"
                     MOVE    SYS-4001-KOMURSITANKA   TO  WRK-K-TTNK
                 END-EVALUATE
      *        2:健保準拠
               WHEN  "2"
                 MOVE    SYS-4001-KOMUTENTANKA     TO  WRK-K-TTNK
             END-EVALUATE
           ELSE
             MOVE    10                  TO  WRK-R-TTNK
                                             WRK-J-TTNK
                                             WRK-JG-TTNK
                                             WRK-JY-TTNK
                                             WRK-K-TTNK
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟編集処理
      *****************************************************************
       110-BTU-PARA-HENSYU-SEC              SECTION.
      *
      * 病棟別カラム作成
      *
           MOVE    "5001"            TO   SYS-5001-KANRICD
           MOVE    SPA-HOSPNUM       TO   SYS-5001-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD  TO   SYS-5001-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD  TO   SYS-5001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD  TO   WRK-SYSYMD
           PERFORM 830-SYSKANRI-DBSELECT-SEC
           PERFORM  UNTIL  FLG-SYSKANRI4 = 1
             ADD    1                    TO   IDX1
             MOVE    SYS-5001-BTU-NUM    TO   WRK-BTUNUM(IDX1)
             MOVE    SYS-5001-BTU-NAME   TO   WRK-BTUNAME(IDX1)
      *
             PERFORM 830-SYSKANRI-DBFETCH-SEC
           END-PERFORM
      *
           ADD     1                   TO   IDX1
           MOVE    "ZZ"                TO   WRK-BTUNUM(IDX1)
           MOVE    "入院履歴なし"      TO   WRK-BTUNAME(IDX1)
           MOVE      IDX1     TO   WRK-BTUMAX
           PERFORM 830-SYSKANRI-DBCLOSE-SEC
      *
           .
       110-BTU-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           OPEN     OUTPUT  K145T-FILE
           CLOSE            K145T-FILE
           OPEN     I-O     K145T-FILE
      *
           MOVE     ZERO    TO        GET-OFF
           EVALUATE  K100LNK-NYUGAIKBN
             WHEN  SPACE
             WHEN  "1"
               MOVE    "1"       TO     WRK-NYUGAIKBN
             WHEN  "2"
               MOVE    "2"       TO     WRK-NYUGAIKBN
           END-EVALUATE
      *
      *    データ集計処理
           PERFORM  UNTIL   GET-OFF   =      1
      *    集計処理
             PERFORM  2001-SYUKEI-SEC
             EVALUATE  K100LNK-NYUGAIKBN
               WHEN  SPACE
                 IF  WRK-NYUGAIKBN  =   "1"
                   MOVE    "2"    TO   WRK-NYUGAIKBN
                 ELSE
                   MOVE 1         TO   GET-OFF
                 END-IF
               WHEN  "1"
               WHEN  "2"
                 MOVE     1       TO   GET-OFF
             END-EVALUATE
           END-PERFORM
      *
           CLOSE   K145T-FILE
      *
      *    帳票印刷処理
           PERFORM  2002-PRINT-SEC
      *
      *    CSV印字処理
      *    CSV ヘッダー処理
           PERFORM  800-CSV-HEAD-SEC
      *    CSV 出力処理
           PERFORM  230-TOUKEICSV-SEC
      *    CSV 明細処理
           PERFORM  400-CSV-DATA-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE               K100
      *
           MOVE   "(病棟別)"        TO    K100-SUBTITLE
      *
           EVALUATE INP-STS
             WHEN  01
               MOVE   "プリンタ情報が取得できませんでした"
                                      TO    K100-JOBMSG
             WHEN  10
               MOVE   "入力された入外区分の値に誤りがあります"
                                      TO    K100-JOBMSG
             WHEN  20
               MOVE   "入力された保険外区分の値に誤りがあります"
                                      TO    K100-JOBMSG
             WHEN  30
               MOVE
                 "病棟別診療行為区分集計表は外来では集計できません"
                                    TO    K100-JOBMSG
           END-EVALUATE
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    K100-PAGE
      *
           MOVE    WRK-SRYYM        TO    K100-SRYYM
           MOVE    WRK-SEIYMD       TO    K100-SYSYMD

           PERFORM 330-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      * 病棟インデックス判別処理    20040413  Y.Takahashi
      *****************************************************************
       220-BTU-GROUPING-SEC      SECTION.
      *
      * 集計日
           MOVE     "0"           TO   WRK-BTU-INDEX
           PERFORM  930-PTNYUINRRK-SELECT-SEC
      *
           MOVE   PTNYUINRRK-BTUNUM    TO WRK-BTU-INDEX
           PERFORM  930-PTNYUINRRK-CLOSE-SEC
      *
           IF  FLG-PTNYUINRRK  NOT =  ZERO
             MOVE   "ZZ"    TO WRK-BTU-INDEX
           END-IF
      *
           .
       220-BTU-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為データ集計処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
      *    入院診療行為情報集計
           PERFORM  900-NYUINACCT-SELECT-SEC
           PERFORM  UNTIL    FLG-NSRYACCT   =   1
             IF  WRK-NYUGAIKBN  =   "1"
               IF   NACCT-PTID  NOT =  WRK-ZENPTID2
                 MOVE   ZERO         TO    FLG-NKENSU
                 PERFORM  20011-NYUIN-SEC
                 MOVE   NACCT-PTID   TO    WRK-ZENPTID2
               END-IF
      *        入院件数カウント処理
               IF  FLG-NKENSU  = 1
                 PERFORM   20000-NYUINKENSU-SEC
               END-IF
             END-IF
             PERFORM  900-NYUINACCT-FETCH-SEC
           END-PERFORM
           PERFORM  900-NYUINACCT-CLOSE-SEC
      *
           PERFORM  900-JYURRK-SELECT-SEC
      *
           PERFORM  UNTIL    FLG-JYURRK   =  1
      *
             INITIALIZE     WRK-CTRL-ZAINUM
             PERFORM  900-SRYACCT-SELECT-SEC
             PERFORM  UNTIL  FLG-SRYACCT   =   1
               IF  JYURRK-ZAINUM1  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM2  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM3  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM4  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM5  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM6  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM7  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM8  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM9  =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM10 =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM11 =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM12 =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM13 =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM14 =  ACCT-ZAINUM
                   OR JYURRK-ZAINUM15 =  ACCT-ZAINUM
      *
                 INITIALIZE   WRK-TENYMD
                 PERFORM   VARYING   IDX-TEN   FROM   1   BY   1
                           UNTIL     IDX-TEN   >   31
                           OR        WRK-TENYMD   NOT =   SPACE
                   IF   ACCT-DAY(1 IDX-TEN)   >   ZERO
                     MOVE   ACCT-SRYYM   TO   WRK-TENYMD(1:6)
                     MOVE   IDX-TEN      TO   WRK-TENYMD(7:2)
                   END-IF
                 END-PERFORM
      *
                 IF    (  ACCT-SRYKBN  NOT =  "97"  )
      *
      *    麻毒、調剤の場合、対象外にする
                   IF  ACCT-SRYKBN NOT = "24"
                       AND  ACCT-SRYKBN NOT = "26"
      *    年齢インデックス
                     MOVE  JYURRK-SRYYMD  TO  WRK-BTU-SRYYMD
                     PERFORM  220-BTU-GROUPING-SEC
                     IF  WRK-BTU-INDEX    NOT =   "0"
                       PERFORM  20012-SRY-SET
                     END-IF
                   END-IF
      *
                 END-IF
      *
               END-IF
               PERFORM  900-SRYACCT-FETCH-SEC
             END-PERFORM
             PERFORM  900-SRYACCT-CLOSE-SEC
      *
           PERFORM  900-JYURRK-FETCH-SEC
      *
           END-PERFORM
      *
           PERFORM  900-JYURRK-CLOSE-SEC
      *
      *    麻毒、調剤の集計
           PERFORM  910-SRYACCT-SELECT-SEC
           PERFORM  UNTIL  FLG-SRYACCT   =   1
             INITIALIZE   WRK-TENYMD
             PERFORM   VARYING   IDX-TEN   FROM   1   BY   1
                       UNTIL     IDX-TEN   >   31
                       OR        WRK-TENYMD   NOT =   SPACE
               IF   ACCT-DAY(1 IDX-TEN)   >   ZERO
                 MOVE   ACCT-SRYYM   TO   WRK-TENYMD(1:6)
                 MOVE   IDX-TEN      TO   WRK-TENYMD(7:2)
               END-IF
             END-PERFORM
             IF  ACCT-SRYKBN  =  "24" OR "26"
               IF  ACCT-DAY-1(1) NOT =  0
                 MOVE   "01"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-2(1) NOT =  0
                 MOVE   "02"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-3(1) NOT =  0
                 MOVE   "03"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-4(1) NOT =  0
                 MOVE   "04"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-5(1) NOT =  0
                 MOVE   "05"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-6(1) NOT =  0
                 MOVE   "06"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-7(1) NOT =  0
                 MOVE   "07"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-8(1) NOT =  0
                 MOVE   "08"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-9(1) NOT =  0
                 MOVE   "09"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-10(1) NOT =  0
                 MOVE   "10"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-11(1) NOT =  0
                 MOVE   "11"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-12(1) NOT =  0
                 MOVE   "12"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-13(1) NOT =  0
                 MOVE   "13"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-14(1) NOT =  0
                 MOVE   "14"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-15(1) NOT =  0
                 MOVE   "15"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-16(1) NOT =  0
                 MOVE   "16"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-17(1) NOT =  0
                 MOVE   "17"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-18(1) NOT =  0
                 MOVE   "18"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-19(1) NOT =  0
                 MOVE   "19"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-20(1) NOT =  0
                 MOVE   "20"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-21(1) NOT =  0
                 MOVE   "21"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-22(1) NOT =  0
                 MOVE   "22"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-23(1) NOT =  0
                 MOVE   "23"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-24(1) NOT =  0
                 MOVE   "24"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-25(1) NOT =  0
                 MOVE   "25"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-26(1) NOT =  0
                 MOVE   "26"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-27(1) NOT =  0
                 MOVE   "27"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-28(1) NOT =  0
                 MOVE   "28"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-29(1) NOT =  0
                 MOVE   "29"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-30(1) NOT =  0
                 MOVE   "30"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
               IF  ACCT-DAY-31(1) NOT =  0
                 MOVE   "31"       TO    WRK-DD
                 PERFORM  999-BEDSET-SEC
                 PERFORM  20012-SRY-SET
               END-IF
      *
             END-IF
             PERFORM  910-SRYACCT-FETCH-SEC
           END-PERFORM
           PERFORM  910-SRYACCT-CLOSE-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟CD設定
      *****************************************************************
       999-BEDSET-SEC                SECTION.
      *
           MOVE  ACCT-HOSPNUM TO    PTINF-HOSPNUM
           MOVE  ACCT-PTID    TO    PTINF-PTID
           STRING  ACCT-SRYYM   DELIMITED  BY  SPACE
                   WRK-DD       DELIMITED  BY  SPACE
                   INTO         WRK-BTU-SRYYMD
           END-STRING
      *
           PERFORM  930-PTNYUINRRK-SELECT-SEC
      *
           IF  FLG-PTNYUINRRK  =   ZERO
             IF  PTNYUINRRK-BTUNUM   NOT  =   SPACE
      *    病棟CDをセット
               MOVE    PTNYUINRRK-BTUNUM   TO   WRK-BTU-INDEX
             ELSE
               MOVE   "ZZ"    TO WRK-BTU-INDEX
             END-IF
           ELSE
             MOVE   "ZZ"    TO WRK-BTU-INDEX
           END-IF
           MOVE    WRK-NYUGAIKBN  TO   JYURRK-NYUGAIKBN
           MOVE    ZERO           TO   JYURRK-RENNUM
           MOVE    WRK-BTU-SRYYMD TO   JYURRK-SRYYMD
      *
           PERFORM  930-PTNYUINRRK-CLOSE-SEC
      *
           .
       999-BEDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料設定
      *****************************************************************
       20011-NYUIN-SEC                SECTION.
      *
      *    収納情報より、入院料・室料差額・食事取得
      *    収納情報検索
           PERFORM  940-SYUNOU-SELECT-SEC
      *
           PERFORM UNTIL   FLG-SYUNOU  =  1
             IF     SYU-NYUGAIKBN       =  "1"
      *    テスト患者は対象外
               MOVE    SYU-PTID     TO  WRK-PTID
               PERFORM  900-PTINF-READ-SEC
               IF   FLG-PTINF             =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "0"
                 MOVE  SYU-SRYYMD   TO   WRK-BTU-SRYYMD
                 PERFORM  220-BTU-GROUPING-SEC
                 IF   WRK-BTU-INDEX    NOT =   "0"
                   PERFORM  20013-NYUIN-SET-SEC
                   MOVE   1             TO    FLG-NKENSU
                 END-IF
               END-IF
             END-IF
             PERFORM  940-SYUNOU-FETCH-SEC
           END-PERFORM
           PERFORM  940-SYUNOU-CLOSE-SEC
      *
           .
       20011-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院点数集計処理
      *****************************************************************
       20013-NYUIN-SET-SEC                 SECTION.
      *
      *    キー設定
           INITIALIZE                            K145TMP-REC
      *
           MOVE    SYU-NYUGAIKBN           TO    K145TMP-NYUGAIKBN
           MOVE    WRK-BTU-INDEX           TO    K145TMP-BTUINDEX
      *
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    入院料加算
           IF  K100LNK-TKKBN  =  2
      *      金額
             MOVE    SYU-NYN-HKNTEN        TO    WRK-ZAITEN1
             PERFORM  20014-NMHEN-SEC
             ADD    WRK-TENSU              TO    K145TMP-TENSU(42)
           ELSE
      *      点数
             ADD    SYU-NYN-HKNTEN         TO    K145TMP-TENSU(42)
           END-IF
      *
      *    室料差額加算
           IF    SYU-RMSAGAKU  NOT = ZERO
             ADD    SYU-RMSAGAKU           TO    K145TMP-TENSU(41)
           END-IF
      *
      *    食事加算
           ADD      SYU-RYOYOHI-SKJ        TO    K145TMP-TENSU(41)
           ADD      SYU-RYOYOHI-LIFE       TO    K145TMP-TENSU(41)
      *
      *    療養担当手当
           IF  K100LNK-TKKBN  =  2
      *      金額
             MOVE    SYU-RYO-HKNTEN        TO    WRK-ZAITEN1
             PERFORM  20014-NMHEN-SEC
             ADD    WRK-TENSU              TO    K145TMP-TENSU(39)
                                                 K145TMP-TENSU(40)
           ELSE
      *      点数
             ADD    SYU-RYO-HKNTEN         TO    K145TMP-TENSU(39)
                                                 K145TMP-TENSU(40)
           END-IF
      *
           IF  FLG-M01TMP   =  1
             WRITE    K145TMP-REC
           ELSE
             REWRITE  K145TMP-REC
           END-IF
      *
           .
       20013-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院金額編集処理
      *****************************************************************
       20014-NMHEN-SEC      SECTION.
      *
           MOVE    SYU-HKNCOMBINUM       TO    WRK-HKNCOMBI
           MOVE    SYU-SYUHKNNUM         TO    WRK-HKNNUM
      *
           EVALUATE  WRK-HKNNUM
      *    労災
           WHEN  "971"
             INITIALIZE                          HKNCOMBI-REC
             MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
             MOVE    WRK-PTID                TO  COMB-PTID
             MOVE    WRK-HKNCOMBI            TO  COMB-HKNCOMBINUM
             PERFORM  900-HKNCOMBI-READ-SEC
             PERFORM  900-PTRSIINF-READ-SEC
             IF  PTRSI-HKNKBN  NOT =  "5"
      *        労災
               MOVE    WRK-R-TTNK            TO  WRK-TENTNK
             ELSE
      *        公務
               MOVE    WRK-K-TTNK            TO  WRK-TENTNK
             END-IF
      *      金額計算
             COMPUTE    WRK-TENSU  =  WRK-ZAITEN1  *  WRK-TENTNK
      *    自賠責
           WHEN  "973"
             IF  FLG-JBI  NOT =  "1"
      *        労災準拠
               MOVE    WRK-J-TTNK            TO  WRK-TENTNK
      *        金額計算
               COMPUTE    WRK-TENSU  =  WRK-ZAITEN1  *  WRK-TENTNK
             ELSE
      *        健保準拠
               MOVE    WRK-JG-TTNK           TO  WRK-TENTNK-JG
               MOVE    WRK-JY-TTNK           TO  WRK-TENTNK-JY
      *        金額計算
               COMPUTE    WRK-TENSU  =  WRK-ZAITEN1  *  WRK-TENTNK-JG
             END-IF
      *    上記以外
           WHEN  OTHER
             MOVE    10                    TO  WRK-TENTNK
      *      金額計算
             COMPUTE    WRK-TENSU  =  WRK-ZAITEN1  *  WRK-TENTNK
           END-EVALUATE
      *
           .
       20014-NMHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院件数集計処理
      *****************************************************************
       20000-NYUINKENSU-SEC                SECTION.
      *
      *    キー設定一時ファイル
           INITIALIZE                 K145TMP-REC
      *
           MOVE    SYU-NYUGAIKBN           TO    K145TMP-NYUGAIKBN
           MOVE    WRK-BTU-INDEX           TO    K145TMP-BTUINDEX
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
           PERFORM  VARYING  IDA  FROM 1  BY 1
                    UNTIL  IDA  >  31
             PERFORM   920-NYUINACCT-READ-SEC
             IF  NACCT-DAY(IDA)  >  0
      *
               EVALUATE  NACCT-ZAISKBKBN
                 WHEN  "1"
      *    入院料件数加算
                   ADD     1          TO   K145TMP-KAISU(42)
                 WHEN  "3"
                 WHEN  "4"
      *    室料差額件数、食事件数加算
                   IF  NACCT-SRYCDTOTAL  NOT =  "00000197000570"
                     ADD    1         TO   K145TMP-KAISU(41)
                   END-IF
                 WHEN  "7"
      *    療養担当手当
                   IF  NACCT-SRYCDTOTAL  =  "00000099999917"
                     ADD    1         TO   K145TMP-KAISU(39)
                                           K145TMP-KAISU(40)
                   END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
      *
           IF  FLG-M01TMP   =  1
               WRITE    K145TMP-REC
           ELSE
               REWRITE  K145TMP-REC
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
      *    キー設定
           INITIALIZE                      K145TMP-REC
      *
           MOVE    JYURRK-NYUGAIKBN  TO    K145TMP-NYUGAIKBN
           MOVE    WRK-BTU-INDEX     TO    K145TMP-BTUINDEX
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
           PERFORM 930-HKNCOMBI-SET-SEC
      *
      *    診療会計付加情報  読込
           PERFORM  900-SRYACCTPLUS-READ-SEC
      *
           IF   K100LNK-HOUKATU  NOT =  "0"
             IF   ACCT-HKNCOMBI  NOT =  "9999"
                 AND  ACCT-JIHOKBN  NOT =  "1"
      *
      *    診療行為情報読み込み
               PERFORM  200121-SRYCD-CHK-SEC
      *
             END-IF
           ELSE
             PERFORM  200121-SRYCD-CHK-SEC
           END-IF
           IF  FLG-M01TMP   =  1
             WRITE  K145TMP-REC
           ELSE
             REWRITE  K145TMP-REC
           END-IF
      *
           .
       20012-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードチェック処理
      ****************************************************************
       200121-SRYCD-CHK-SEC                SECTION.
      *
           PERFORM  900-SRYACT-SELECT-SEC
      *
           PERFORM  UNTIL    FLG-SRYACT   =   1
      *
             PERFORM  VARYING  IDZ  FROM  1  BY  1
                      UNTIL  IDZ  >     5
                      OR   SRY-SRYCD(IDZ)    =   SPACE
               EVALUATE  SRY-SRYKBN
                 WHEN  "14"
                   PERFORM  200121-ZAITAKU-SET-SEC
                 WHEN  "60"
                 WHEN  "64"
                   PERFORM  200121-KENSA-SET-SEC
                 WHEN  "95"
                 WHEN  "96"
                   PERFORM  200121-JIHI-SET-SEC
                 WHEN  OTHER
                   PERFORM  200122-ETC-SEC
               END-EVALUATE
             END-PERFORM
      *
             PERFORM  900-SRYACT-FETCH-SEC
           END-PERFORM
      *
           PERFORM  900-SRYACT-CLOSE-SEC
      *
           .
       200121-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅点数設定処理
      *****************************************************************
       200121-ZAITAKU-SET-SEC                SECTION.
      *
           PERFORM  900-TENSU-READ-SEC
      *
           IF    TNS-CDKBN-KBN     =  "C"
               AND   TNS-CDKBN-KBNNUM  =  ZERO
             MOVE    12    TO   IDX1
           ELSE
             MOVE    13    TO   IDX1
           END-IF
      *
      *    回数加算
           COMPUTE  IDX6  =  JYURRK-RENNUM   +   1
           INITIALIZE          WRK-KAISU
           EVALUATE  JYURRK-SRYYMD(7:2)
             WHEN  "01"
               MOVE      ACCT-DAY-1(IDX6)    TO   WRK-KAISU
             WHEN  "02"
               MOVE      ACCT-DAY-2(IDX6)    TO   WRK-KAISU
             WHEN  "03"
               MOVE      ACCT-DAY-3(IDX6)    TO   WRK-KAISU
             WHEN  "04"
               MOVE      ACCT-DAY-4(IDX6)    TO   WRK-KAISU
             WHEN  "05"
               MOVE      ACCT-DAY-5(IDX6)    TO   WRK-KAISU
             WHEN  "06"
               MOVE      ACCT-DAY-6(IDX6)    TO   WRK-KAISU
             WHEN  "07"
               MOVE      ACCT-DAY-7(IDX6)    TO   WRK-KAISU
             WHEN  "08"
               MOVE      ACCT-DAY-8(IDX6)    TO   WRK-KAISU
             WHEN  "09"
               MOVE      ACCT-DAY-9(IDX6)    TO   WRK-KAISU
             WHEN  "10"
               MOVE      ACCT-DAY-10(IDX6)    TO   WRK-KAISU
             WHEN  "11"
               MOVE      ACCT-DAY-11(IDX6)    TO   WRK-KAISU
             WHEN  "12"
               MOVE      ACCT-DAY-12(IDX6)    TO   WRK-KAISU
             WHEN  "13"
               MOVE      ACCT-DAY-13(IDX6)    TO   WRK-KAISU
             WHEN  "14"
               MOVE      ACCT-DAY-14(IDX6)    TO   WRK-KAISU
             WHEN  "15"
               MOVE      ACCT-DAY-15(IDX6)    TO   WRK-KAISU
             WHEN  "16"
               MOVE      ACCT-DAY-16(IDX6)    TO   WRK-KAISU
             WHEN  "17"
               MOVE      ACCT-DAY-17(IDX6)    TO   WRK-KAISU
             WHEN  "18"
               MOVE      ACCT-DAY-18(IDX6)    TO   WRK-KAISU
             WHEN  "19"
               MOVE      ACCT-DAY-19(IDX6)    TO   WRK-KAISU
             WHEN  "20"
               MOVE      ACCT-DAY-20(IDX6)    TO   WRK-KAISU
             WHEN  "21"
               MOVE      ACCT-DAY-21(IDX6)    TO   WRK-KAISU
             WHEN  "22"
               MOVE      ACCT-DAY-22(IDX6)    TO   WRK-KAISU
             WHEN  "23"
               MOVE      ACCT-DAY-23(IDX6)    TO   WRK-KAISU
             WHEN  "24"
               MOVE      ACCT-DAY-24(IDX6)    TO   WRK-KAISU
             WHEN  "25"
               MOVE      ACCT-DAY-25(IDX6)    TO   WRK-KAISU
             WHEN  "26"
               MOVE      ACCT-DAY-26(IDX6)    TO   WRK-KAISU
             WHEN  "27"
               MOVE      ACCT-DAY-27(IDX6)    TO   WRK-KAISU
             WHEN  "28"
               MOVE      ACCT-DAY-28(IDX6)    TO   WRK-KAISU
             WHEN  "29"
               MOVE      ACCT-DAY-29(IDX6)    TO   WRK-KAISU
             WHEN  "30"
               MOVE      ACCT-DAY-30(IDX6)    TO   WRK-KAISU
             WHEN  "31"
               MOVE      ACCT-DAY-31(IDX6)    TO   WRK-KAISU
           END-EVALUATE
      *
      *    点数加算
           IF   ACCT-ZAINUM  NOT =  WRK-ZAINUM
               OR  ( ACCT-ZAINUM  =  WRK-ZAINUM
               AND   ACCT-PTID  NOT =  WRK-PTID2 )
               OR  (JYURRK-SRYYMD  NOT =  WRK-ZENJYUYMD)
      *
             IF  K100LNK-TKKBN  =  2
      *        金額
               COMPUTE  WRK-ZAITEN1  =  ACCT-SYUTEN1  +  ACCT-SYUTEN2
               COMPUTE  WRK-ZAITEN2  =  ACCT-YKZTEN   +  ACCT-KIZAITEN
               PERFORM  200122-MHEN-SEC
               COMPUTE  WRK-TENSU  =  WRK-ZAITEN  *  WRK-KAISU
             ELSE
      *        点数
               COMPUTE  WRK-TENSU  =  ACCT-ZAITEN  *  WRK-KAISU
             END-IF
      *
             IF  ACCTP-PLUSKBN  =  "1"
               COMPUTE  WRK-TENSU   =   WRK-TENSU  *  -1
             END-IF
      *
             ADD    WRK-TENSU        TO   K145TMP-TENSU(IDX1)
                                          K145TMP-TENSU(14)
      *
      *    回数加算
             ADD    WRK-KAISU        TO   K145TMP-KAISU(IDX1)
                                          K145TMP-KAISU(14)
           END-IF
           MOVE    ACCT-ZAINUM      TO   WRK-ZAINUM
           MOVE    ACCT-PTID        TO   WRK-PTID2
           MOVE    JYURRK-SRYYMD    TO   WRK-ZENJYUYMD
      *
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
             MOVE    2             TO   WRK-KNSBUNRUI
             PERFORM  900-KENSA-READ-SEC
             IF   FLG-KENSASORT  =  ZERO
               MOVE    33          TO   IDX1
             ELSE
      *    判断料チェック
               MOVE      99        TO   WRK-KNSBUNRUI
               PERFORM  900-KENSA-READ-SEC
               IF    FLG-KENSASORT  =  ZERO
                 MOVE    34        TO   IDX1
               ELSE
                 MOVE    32        TO   IDX1
               END-IF
             END-IF
           ELSE
             MOVE    54            TO   IDX1
           END-IF
      *
      *    点数取得
           PERFORM  900-TENSU-READ-SEC
      *
      *    回数加算
           COMPUTE IDX6  =     JYURRK-RENNUM   +   1
           INITIALIZE          WRK-KAISU
           EVALUATE  JYURRK-SRYYMD(7:2)
               WHEN  "01"
                 MOVE      ACCT-DAY-1(IDX6)    TO   WRK-KAISU
               WHEN  "02"
                 MOVE      ACCT-DAY-2(IDX6)    TO   WRK-KAISU
               WHEN  "03"
                 MOVE      ACCT-DAY-3(IDX6)    TO   WRK-KAISU
               WHEN  "04"
                 MOVE      ACCT-DAY-4(IDX6)    TO   WRK-KAISU
               WHEN  "05"
                 MOVE      ACCT-DAY-5(IDX6)    TO   WRK-KAISU
               WHEN  "06"
                 MOVE      ACCT-DAY-6(IDX6)    TO   WRK-KAISU
               WHEN  "07"
                 MOVE      ACCT-DAY-7(IDX6)    TO   WRK-KAISU
               WHEN  "08"
                 MOVE      ACCT-DAY-8(IDX6)    TO   WRK-KAISU
               WHEN  "09"
                 MOVE      ACCT-DAY-9(IDX6)    TO   WRK-KAISU
               WHEN  "10"
                 MOVE      ACCT-DAY-10(IDX6)    TO   WRK-KAISU
               WHEN  "11"
                 MOVE      ACCT-DAY-11(IDX6)    TO   WRK-KAISU
               WHEN  "12"
                 MOVE      ACCT-DAY-12(IDX6)    TO   WRK-KAISU
               WHEN  "13"
                 MOVE      ACCT-DAY-13(IDX6)    TO   WRK-KAISU
               WHEN  "14"
                 MOVE      ACCT-DAY-14(IDX6)    TO   WRK-KAISU
               WHEN  "15"
                 MOVE      ACCT-DAY-15(IDX6)    TO   WRK-KAISU
               WHEN  "16"
                 MOVE      ACCT-DAY-16(IDX6)    TO   WRK-KAISU
               WHEN  "17"
                 MOVE      ACCT-DAY-17(IDX6)    TO   WRK-KAISU
               WHEN  "18"
                 MOVE      ACCT-DAY-18(IDX6)    TO   WRK-KAISU
               WHEN  "19"
                 MOVE      ACCT-DAY-19(IDX6)    TO   WRK-KAISU
               WHEN  "20"
                 MOVE      ACCT-DAY-20(IDX6)    TO   WRK-KAISU
               WHEN  "21"
                 MOVE      ACCT-DAY-21(IDX6)    TO   WRK-KAISU
               WHEN  "22"
                 MOVE      ACCT-DAY-22(IDX6)    TO   WRK-KAISU
               WHEN  "23"
                 MOVE      ACCT-DAY-23(IDX6)    TO   WRK-KAISU
               WHEN  "24"
                 MOVE      ACCT-DAY-24(IDX6)    TO   WRK-KAISU
               WHEN  "25"
                 MOVE      ACCT-DAY-25(IDX6)    TO   WRK-KAISU
               WHEN  "26"
                 MOVE      ACCT-DAY-26(IDX6)    TO   WRK-KAISU
               WHEN  "27"
                 MOVE      ACCT-DAY-27(IDX6)    TO   WRK-KAISU
               WHEN  "28"
                 MOVE      ACCT-DAY-28(IDX6)    TO   WRK-KAISU
               WHEN  "29"
                 MOVE      ACCT-DAY-29(IDX6)    TO   WRK-KAISU
               WHEN  "30"
                 MOVE      ACCT-DAY-30(IDX6)    TO   WRK-KAISU
               WHEN  "31"
                 MOVE      ACCT-DAY-31(IDX6)    TO   WRK-KAISU
           END-EVALUATE
      *
           IF   ACCT-ZAINUM  NOT =  WRK-ZAINUM
               OR  ( ACCT-ZAINUM  =  WRK-ZAINUM
               AND  ACCT-PTID  NOT =  WRK-PTID2 )
               OR  (JYURRK-SRYYMD  NOT =  WRK-ZENJYUYMD)
      *
             IF  K100LNK-TKKBN  =  2
      *        金額
               COMPUTE  WRK-ZAITEN1  =  ACCT-SYUTEN1  +  ACCT-SYUTEN2
               COMPUTE  WRK-ZAITEN2  =  ACCT-YKZTEN   +  ACCT-KIZAITEN
               PERFORM  200122-MHEN-SEC
               COMPUTE  WRK-TENSU  =  WRK-ZAITEN  *  WRK-KAISU
             ELSE
      *        点数
               COMPUTE  WRK-TENSU  =  ACCT-ZAITEN  *  WRK-KAISU
             END-IF
      *
             IF  ACCTP-PLUSKBN  =  "1"
               COMPUTE  WRK-TENSU   =   WRK-TENSU  *  -1
             END-IF
      *
             ADD    WRK-TENSU        TO   K145TMP-TENSU(IDX1)
                                          K145TMP-TENSU(35)
      *
      *    回数加算
             ADD    WRK-KAISU        TO   K145TMP-KAISU(IDX1)
                                          K145TMP-KAISU(35)
           END-IF
           MOVE    ACCT-ZAINUM       TO   WRK-ZAINUM
           MOVE    ACCT-PTID         TO   WRK-PTID2
           MOVE    JYURRK-SRYYMD     TO   WRK-ZENJYUYMD
      *
           .
       200121-KENSA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外点数、自費項目金額設定処理
      *****************************************************************
       200121-JIHI-SET-SEC                SECTION.
      *
           PERFORM  900-TENSU-READ-SEC
           IF       SRY-JIHIMONEY(IDZ)     =     ZERO
      *    点数、金額がゼロの場合は加算処理は行わない。
             GO          TO    200121-JIHI-SET-EXT
           END-IF
      *
           INITIALIZE   FLG-HKNTG
           INITIALIZE   IDX1
           INITIALIZE   IDX2
           IF    TNS-SRYCD(1:3)    =     "095"
               OR   TNS-SRYCD(1:3)    =     "096"
             IF  WRK-NYUGAIKBN      =     "1"
               EVALUATE  TNS-NYUTENTTLKBN
                 WHEN  1
                   MOVE    43     TO   IDX1
                 WHEN  2
                   MOVE    44     TO   IDX1
                 WHEN  3
                   MOVE    45     TO   IDX1
                 WHEN  4
                   MOVE    46     TO   IDX1
                 WHEN  5
                   MOVE    47     TO   IDX1
                 WHEN  6
                   MOVE    48     TO   IDX1
                 WHEN  7
                   MOVE    49     TO   IDX1
                 WHEN  8
                   MOVE    50     TO   IDX1
                 WHEN  9
                   MOVE    51     TO   IDX1
                 WHEN  10
                   MOVE    52     TO   IDX1
                 WHEN  OTHER
                   MOVE    53     TO   IDX1
      *
               END-EVALUATE
             ELSE
               EVALUATE  TNS-GAITENTTLKBN
                 WHEN  1
                   MOVE    43     TO   IDX1
                 WHEN  2
                   MOVE    44     TO   IDX1
                 WHEN  3
                   MOVE    45     TO   IDX1
                 WHEN  4
                   MOVE    46     TO   IDX1
                 WHEN  5
                   MOVE    47     TO   IDX1
                 WHEN  6
                   MOVE    48     TO   IDX1
                 WHEN  7
                   MOVE    49     TO   IDX1
                 WHEN  8
                   MOVE    50     TO   IDX1
                 WHEN  9
                   MOVE    51     TO   IDX1
                 WHEN  10
                   MOVE    52     TO   IDX1
                 WHEN  OTHER
                   MOVE    53     TO   IDX1
      *
               END-EVALUATE
             END-IF
           ELSE
      *
      *    保険適用外フラグON
             MOVE  1     TO   FLG-HKNTG
             IF    K100LNK-HKNTG   =   SPACE
      *    保険適用外取扱区分がSPACEの場合は加算処理は行わない。
               GO      TO     200121-JIHI-SET-EXT
             END-IF
             IF    TNS-DATAKBN  =  "1"
               PERFORM   20013-IDXSET2-SEC
             ELSE
               EVALUATE  TNS-SRYCD
                 WHEN  "610000000" THRU "619999999"
                   PERFORM   20013-IDXSET2-SEC
                 WHEN  "660000000" THRU "669999999"
                   PERFORM   20013-IDXSET2-SEC
                 WHEN  OTHER
                   GO    TO  200121-JIHI-SET-EXT
               END-EVALUATE
             END-IF
           END-IF
      *
      *    点数加算
           IF   FLG-HKNTG  =  1
      *    保険適用外の場合
      *
             EVALUATE  TNS-TENSIKIBETU
               WHEN  1
      *    金額項目の場合は点数に変換してデータ化する。
                 IF  K100LNK-TKKBN  =  2
      *            点数
                   COMPUTE   WRK-CNV-TEN    =  TNS-TEN
                                            *  SRY-SRYSURYO(IDZ)
                 ELSE
      *            金額
                   COMPUTE   WRK-CNV-TEN-V  = (TNS-TEN
                                            *  SRY-SRYSURYO(IDZ))
                                            /  10
                   COMPUTE   WRK-CNV-TEN  ROUNDED  =  WRK-CNV-TEN-V
                                                   *  1
                 END-IF
               WHEN  3
               WHEN  8
      *    点数項目の場合は回数をデータ化する。
                 MOVE   TNS-TEN       TO     WRK-CNV-TEN
               WHEN  OTHER
      *    点数、金額以外の場合
                 GO           TO     200121-JIHI-SET-EXT
             END-EVALUATE
           ELSE
      *
      *    自費項目の場合
             MOVE    SRY-JIHIMONEY(IDZ)    TO     WRK-CNV-TEN
           END-IF
      *
      *    回数加算
           COMPUTE IDX6  =     JYURRK-RENNUM   +   1
           INITIALIZE          WRK-KAISU
           EVALUATE  JYURRK-SRYYMD(7:2)
             WHEN  "01"
               MOVE    ACCT-DAY-1(IDX6)     TO   WRK-KAISU
             WHEN  "02"
               MOVE    ACCT-DAY-2(IDX6)     TO   WRK-KAISU
             WHEN  "03"
               MOVE    ACCT-DAY-3(IDX6)     TO   WRK-KAISU
             WHEN  "04"
               MOVE    ACCT-DAY-4(IDX6)     TO   WRK-KAISU
             WHEN  "05"
               MOVE    ACCT-DAY-5(IDX6)     TO   WRK-KAISU
             WHEN  "06"
               MOVE    ACCT-DAY-6(IDX6)     TO   WRK-KAISU
             WHEN  "07"
               MOVE    ACCT-DAY-7(IDX6)     TO   WRK-KAISU
             WHEN  "08"
               MOVE    ACCT-DAY-8(IDX6)     TO   WRK-KAISU
             WHEN  "09"
               MOVE    ACCT-DAY-9(IDX6)     TO   WRK-KAISU
             WHEN  "10"
               MOVE    ACCT-DAY-10(IDX6)    TO   WRK-KAISU
             WHEN  "11"
               MOVE    ACCT-DAY-11(IDX6)    TO   WRK-KAISU
             WHEN  "12"
               MOVE    ACCT-DAY-12(IDX6)    TO   WRK-KAISU
             WHEN  "13"
               MOVE    ACCT-DAY-13(IDX6)    TO   WRK-KAISU
             WHEN  "14"
               MOVE    ACCT-DAY-14(IDX6)    TO   WRK-KAISU
             WHEN  "15"
               MOVE    ACCT-DAY-15(IDX6)    TO   WRK-KAISU
             WHEN  "16"
               MOVE    ACCT-DAY-16(IDX6)    TO   WRK-KAISU
             WHEN  "17"
               MOVE    ACCT-DAY-17(IDX6)    TO   WRK-KAISU
             WHEN  "18"
               MOVE    ACCT-DAY-18(IDX6)    TO   WRK-KAISU
             WHEN  "19"
               MOVE    ACCT-DAY-19(IDX6)    TO   WRK-KAISU
             WHEN  "20"
               MOVE    ACCT-DAY-20(IDX6)    TO   WRK-KAISU
             WHEN  "21"
               MOVE    ACCT-DAY-21(IDX6)    TO   WRK-KAISU
             WHEN  "22"
               MOVE    ACCT-DAY-22(IDX6)    TO   WRK-KAISU
             WHEN  "23"
               MOVE    ACCT-DAY-23(IDX6)    TO   WRK-KAISU
             WHEN  "24"
               MOVE    ACCT-DAY-24(IDX6)    TO   WRK-KAISU
             WHEN  "25"
               MOVE    ACCT-DAY-25(IDX6)    TO   WRK-KAISU
             WHEN  "26"
               MOVE    ACCT-DAY-26(IDX6)    TO   WRK-KAISU
             WHEN  "27"
               MOVE    ACCT-DAY-27(IDX6)    TO   WRK-KAISU
             WHEN  "28"
               MOVE    ACCT-DAY-28(IDX6)    TO   WRK-KAISU
             WHEN  "29"
               MOVE    ACCT-DAY-29(IDX6)    TO   WRK-KAISU
             WHEN  "30"
               MOVE    ACCT-DAY-30(IDX6)    TO   WRK-KAISU
             WHEN  "31"
               MOVE    ACCT-DAY-31(IDX6)    TO   WRK-KAISU
           END-EVALUATE
           ADD   WRK-KAISU      TO  K145TMP-KAISU(IDX1)
      *
      *    点数加算
           COMPUTE   WRK-TENSU  =  WRK-CNV-TEN  *  WRK-KAISU
           ADD     WRK-TENSU  TO     K145TMP-TENSU(IDX1)
      *
           .
       200121-JIHI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他診療区分別設定処理
      *****************************************************************
       200122-ETC-SEC                SECTION.
      *
           IF    ACCT-ZAITEN  =        ZERO
      *    点数、金額がゼロの場合は加算設定処理は行わない。
             GO           TO       200122-ETC-EXT
           END-IF
      *
           PERFORM  900-TENSU-READ-SEC
      *
      *    集計インデックス設定
           PERFORM  20013-IDXSET-SEC
      *
           IF   WRK-CZ-SRYYMD    NOT =   JYURRK-SRYYMD
               OR  WRK-CZ-PTID      NOT =   ACCT-PTID
               OR  WRK-CZ-SRYKBN    NOT =   ACCT-SRYKBN
               OR  WRK-CZ-ZAINUM    NOT =   ACCT-ZAINUM
               OR  ( WRK-CZ-ZAINUM  =  ACCT-ZAINUM
               AND  ACCT-PTID  NOT =  WRK-CZ-PTID )
      *
      *    回数加算
             COMPUTE  IDX6  =   JYURRK-RENNUM      +     1
             INITIALIZE      WRK-KAISU
             EVALUATE  JYURRK-SRYYMD(7:2)
               WHEN  "01"
                 MOVE    ACCT-DAY-1(IDX6)    TO   WRK-KAISU
               WHEN  "02"
                 MOVE    ACCT-DAY-2(IDX6)    TO   WRK-KAISU
               WHEN  "03"
                 MOVE    ACCT-DAY-3(IDX6)    TO   WRK-KAISU
               WHEN  "04"
                 MOVE    ACCT-DAY-4(IDX6)    TO   WRK-KAISU
               WHEN  "05"
                 MOVE    ACCT-DAY-5(IDX6)    TO   WRK-KAISU
               WHEN  "06"
                 MOVE    ACCT-DAY-6(IDX6)    TO   WRK-KAISU
               WHEN  "07"
                 MOVE    ACCT-DAY-7(IDX6)    TO   WRK-KAISU
               WHEN  "08"
                 MOVE    ACCT-DAY-8(IDX6)    TO   WRK-KAISU
               WHEN  "09"
                 MOVE    ACCT-DAY-9(IDX6)    TO   WRK-KAISU
               WHEN  "10"
                 MOVE    ACCT-DAY-10(IDX6)    TO   WRK-KAISU
               WHEN  "11"
                 MOVE    ACCT-DAY-11(IDX6)    TO   WRK-KAISU
               WHEN  "12"
                 MOVE    ACCT-DAY-12(IDX6)    TO   WRK-KAISU
               WHEN  "13"
                 MOVE    ACCT-DAY-13(IDX6)    TO   WRK-KAISU
               WHEN  "14"
                 MOVE    ACCT-DAY-14(IDX6)    TO   WRK-KAISU
               WHEN  "15"
                 MOVE    ACCT-DAY-15(IDX6)    TO   WRK-KAISU
               WHEN  "16"
                 MOVE    ACCT-DAY-16(IDX6)    TO   WRK-KAISU
               WHEN  "17"
                 MOVE    ACCT-DAY-17(IDX6)    TO   WRK-KAISU
               WHEN  "18"
                 MOVE    ACCT-DAY-18(IDX6)    TO   WRK-KAISU
               WHEN  "19"
                 MOVE    ACCT-DAY-19(IDX6)    TO   WRK-KAISU
               WHEN  "20"
                 MOVE    ACCT-DAY-20(IDX6)    TO   WRK-KAISU
               WHEN  "21"
                 MOVE    ACCT-DAY-21(IDX6)    TO   WRK-KAISU
               WHEN  "22"
                 MOVE    ACCT-DAY-22(IDX6)    TO   WRK-KAISU
               WHEN  "23"
                 MOVE    ACCT-DAY-23(IDX6)    TO   WRK-KAISU
               WHEN  "24"
                 MOVE    ACCT-DAY-24(IDX6)    TO   WRK-KAISU
               WHEN  "25"
                 MOVE    ACCT-DAY-25(IDX6)    TO   WRK-KAISU
               WHEN  "26"
                 MOVE    ACCT-DAY-26(IDX6)    TO   WRK-KAISU
               WHEN  "27"
                 MOVE    ACCT-DAY-27(IDX6)    TO   WRK-KAISU
               WHEN  "28"
                 MOVE    ACCT-DAY-28(IDX6)    TO   WRK-KAISU
               WHEN  "29"
                 MOVE    ACCT-DAY-29(IDX6)    TO   WRK-KAISU
               WHEN  "30"
                 MOVE    ACCT-DAY-30(IDX6)    TO   WRK-KAISU
               WHEN  "31"
                 MOVE    ACCT-DAY-31(IDX6)    TO   WRK-KAISU
             END-EVALUATE
             IF  IDX1  NOT =  42
               ADD   WRK-KAISU          TO  K145TMP-KAISU(IDX1)
      *
      *    点数加算
             IF  K100LNK-TKKBN  =  2
      *        金額
      *        労災及び自賠責の場合、労災金額があればそちらを集計
               IF  ( (      COMB-HKNNUM           =  "971" OR "973" )
                       AND  FLG-SRYACCTP          =  ZERO
                       AND  ACCTP-RSIKINGAKU  NOT =  ZERO             )
                 COMPUTE  WRK-TENSU  ROUNDED  =  ACCTP-RSIKINGAKU
                                              *  WRK-KAISU
               ELSE
                 COMPUTE  WRK-ZAITEN1  =  ACCT-SYUTEN1  +  ACCT-SYUTEN2
                 COMPUTE  WRK-ZAITEN2  =  ACCT-YKZTEN   +  ACCT-KIZAITEN
                 PERFORM  200122-MHEN-SEC
                 COMPUTE  WRK-TENSU  =  WRK-ZAITEN  *  WRK-KAISU
               END-IF
             ELSE
      *        点数
               COMPUTE  WRK-TENSU  =  ACCT-ZAITEN  *  WRK-KAISU
             END-IF
      *
               IF  ACCTP-PLUSKBN  =  "1"
                 COMPUTE  WRK-TENSU   =   WRK-TENSU  *  -1
               END-IF
      *
               ADD       WRK-TENSU      TO  K145TMP-TENSU(IDX1)
             END-IF
      *
      *    剤番号ブレイクキーセット
             MOVE    JYURRK-SRYYMD    TO   WRK-CZ-SRYYMD
             MOVE    ACCT-PTID        TO   WRK-CZ-PTID
             MOVE    ACCT-SRYKBN      TO   WRK-CZ-SRYKBN
             MOVE    ACCT-ZAINUM      TO   WRK-CZ-ZAINUM
           END-IF
      *
           .
       200122-ETC-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額編集処理
      *****************************************************************
       200122-MHEN-SEC      SECTION.
      *
           MOVE    ACCT-PTID             TO  WRK-PTID
      *
           EVALUATE  COMB-HKNNUM
      *    労災
           WHEN  "971"
             PERFORM  900-PTRSIINF-READ-SEC
      *      公務災害
             IF  PTRSI-HKNKBN  NOT =  "5"
      *        労災準拠
               MOVE    WRK-R-TTNK            TO  WRK-TENTNK
             ELSE
      *        健保準拠
               MOVE    WRK-K-TTNK            TO  WRK-TENTNK
             END-IF
      *      金額計算
             COMPUTE  WRK-ZAITEN  =  ( WRK-ZAITEN1  +  WRK-ZAITEN2 )
                                  *  WRK-TENTNK
      *    自賠責
           WHEN  "973"
             IF  FLG-JBI  NOT =  "1"
      *        労災準拠
               MOVE    WRK-J-TTNK            TO  WRK-TENTNK
      *        金額計算
               COMPUTE  WRK-ZAITEN  =  ( WRK-ZAITEN1  +  WRK-ZAITEN2 )
                                   *  WRK-TENTNK
             ELSE
      *        健保準拠
               MOVE    WRK-JG-TTNK           TO  WRK-TENTNK-JG
               MOVE    WRK-JY-TTNK           TO  WRK-TENTNK-JY
      *        金額計算
               COMPUTE  WRK-ZAITEN  =  ( WRK-ZAITEN1  *  WRK-TENTNK-JG )
                                    +  ( WRK-ZAITEN2  *  WRK-TENTNK-JY )
             END-IF
      *    上記以外
           WHEN  OTHER
             MOVE    10                    TO  WRK-TENTNK
      *      金額計算
             COMPUTE  WRK-ZAITEN  =  ( WRK-ZAITEN1  +  WRK-ZAITEN2 )
                                  *  WRK-TENTNK
           END-EVALUATE
      *
           .
       200122-MHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算インデックス設定処理
      *****************************************************************
       20013-IDXSET-SEC                SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    ACCT-HOSPNUM        TO  AGECHK-HOSPNUM
           MOVE    ACCT-PTID           TO  AGECHK-PTID
           MOVE    K100LNK-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"       TO  AGECHK-SRYYMD(7:2)
           MOVE    COMB-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
           MOVE    ZERO    TO    IDX1
           EVALUATE  SRY-SRYKBN
             WHEN  "11" THRU "14"
               EVALUATE  SRY-SRYSYUKBN
               WHEN  "110"
                 MOVE     PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE     K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                   SUBTRACT  1     FROM WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM      =     "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                     OR   WRK-NENREI   >= 75
                   MOVE      4    TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >= 70
                     MOVE    3    TO    IDX1
                   ELSE
                     IF   K100LNK-SRYYM  <  "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "120"
                 MOVE    PTINF-BIRTHDAY(1:4)   TO    WRK-YY1-G
                 MOVE    K100LNK-SRYYM(1:4)    TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                   SUBTRACT  1    FROM  WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM      =  "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                     OR   WRK-NENREI   >= 75
                   MOVE      8    TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >= 70
                     MOVE    7    TO    IDX1
                   ELSE
                     IF  K100LNK-SRYYM < "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "130"
                 MOVE    9     TO    IDX1
               WHEN  OTHER
                 MOVE    10    TO    IDX1
             END-EVALUATE
             WHEN  "21" THRU "27"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN  "210" THRU "213"
                 WHEN  "290" THRU "292"
                   MOVE    15     TO    IDX1
                 WHEN  "220" THRU "223"
                   MOVE    16     TO    IDX1
                 WHEN  "230" THRU "233"
                   MOVE    17     TO    IDX1
                 WHEN  "240" THRU "242"
                   MOVE    18     TO    IDX1
                 WHEN  "250"
                   MOVE    19     TO    IDX1
                 WHEN  "260"
                   MOVE    20     TO    IDX1
                 WHEN  "270"
                   MOVE    21     TO    IDX1
               END-EVALUATE
             WHEN  "31" THRU "34"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN  "310" THRU "311"
                   MOVE   23      TO    IDX1
                 WHEN  "320" THRU "321"
                   MOVE   24      TO    IDX1
                 WHEN  "330" THRU "331"
                 WHEN  "340"
                   MOVE   25      TO    IDX1
               END-EVALUATE
             WHEN  "40"
               MOVE     27      TO    IDX1
             WHEN  "50" THRU "54"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN  "500" THRU "502"
                   MOVE   28      TO    IDX1
                 WHEN  "510"
                   MOVE   29      TO    IDX1
                 WHEN  "540" THRU "542"
                   MOVE   30      TO    IDX1
               END-EVALUATE
             WHEN  "70"
                 MOVE     36      TO    IDX1
             WHEN  "80"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN "820"
                   MOVE   37      TO    IDX1
                 WHEN  "800"
                 WHEN  "810"
                   MOVE   38      TO    IDX1
                 WHEN  "840"
                 WHEN  "830"
                 WHEN  "850"
                   MOVE   39      TO    IDX1
               END-EVALUATE
      *
             WHEN  "90"
             WHEN  "92"
             WHEN  "97"
               EVALUATE SRY-SRYSYUKBN
                 WHEN  "900"
                 WHEN  "920"
                   MOVE   42      TO    IDX1
                 WHEN  "970"
                 WHEN  "971"
                   MOVE   41      TO    IDX1
               END-EVALUATE
             WHEN  "95" THRU "96"
                 MOVE     53        TO    IDX1
      *
           END-EVALUATE
      *
      *    診療種別がスペースだったら診療区分で分類する
           IF   IDX1   =      ZERO
             EVALUATE  SRY-SRYKBN
               WHEN  "11"
                 MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                     SUBTRACT  1       FROM  WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM    =  "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                     OR   WRK-NENREI   >= 75
                   MOVE    4          TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >=    70
                     MOVE    3      TO    IDX1
                   ELSE
                     IF K100LNK-SRYYM < "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "12"
                 MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                   SUBTRACT       1     FROM  WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM    = "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                     OR   WRK-NENREI   >= 75
                   MOVE    8      TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >= 70
                       MOVE    7      TO    IDX1
                   ELSE
                     IF   K100LNK-SRYYM  <  "200804"
                       IF   WRK-NENREI  <  3
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "13"
                 MOVE     9         TO    IDX1
               WHEN  "14"
                 MOVE     13        TO    IDX1
               WHEN  "21"
                 MOVE     15        TO    IDX1
               WHEN  "22"
                 MOVE     16        TO    IDX1
               WHEN  "23"
                 MOVE     17        TO    IDX1
               WHEN  "24"
                 MOVE     18        TO    IDX1
               WHEN  "25"
                 MOVE     19        TO    IDX1
               WHEN  "26"
                 MOVE     20        TO    IDX1
               WHEN  "27"
                 MOVE     21        TO    IDX1
               WHEN  "31"
                 MOVE     23        TO    IDX1
               WHEN  "32"
                 MOVE     24        TO    IDX1
               WHEN  "33"
                 MOVE     25        TO    IDX1
               WHEN  "40"
                 MOVE     27        TO    IDX1
               WHEN  "50"
                 MOVE     28        TO    IDX1
               WHEN  "51"
                 MOVE     29        TO    IDX1
               WHEN  "54"
                 MOVE     30        TO    IDX1
               WHEN  "70"
                 MOVE     36        TO    IDX1
               WHEN  "80"
               WHEN  "93"
                 MOVE     37        TO    IDX1
               WHEN  "95"
               WHEN  "96"
                 MOVE     53        TO    IDX1
               WHEN  "97"
                 MOVE     41        TO    IDX1
               WHEN  "90"
                 MOVE     42        TO    IDX1
               WHEN  "92"
                 MOVE     42        TO    IDX1
               WHEN  OTHER
                 MOVE     39        TO    IDX1
      *
             END-EVALUATE
           END-IF
      *
           .
       20013-IDXSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算インデックス設定処理(点数マスタの診療区分と診療種別により判定)
      *****************************************************************
       20013-IDXSET2-SEC                SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    ACCT-HOSPNUM        TO  AGECHK-HOSPNUM
           MOVE    ACCT-PTID            TO  AGECHK-PTID
           MOVE    K100LNK-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"       TO  AGECHK-SRYYMD(7:2)
           MOVE    COMB-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
           MOVE      ZERO    TO    IDX1
           EVALUATE  TNS-SRYKBN
               WHEN "11" THRU "14"
                   EVALUATE  TNS-SRYSYUKBN
                       WHEN "110"
                           MOVE     PTINF-BIRTHDAY(1:4)  TO   WRK-YY1-G
                           MOVE     K100LNK-SRYYM(1:4)   TO   WRK-YY2-G
                           COMPUTE  WRK-NENREI   =  WRK-YY2  -  WRK-YY1
                           IF  PTINF-BIRTHDAY(5:2) > K100LNK-SRYYM(5:2)
                               SUBTRACT  1     FROM WRK-NENREI
                           END-IF
                           IF  COMB-KOH1HKNNUM      =     "027"
                             OR   COMB-HKNNUM  =  "039"
                             OR   COMB-HKNNUM  =  "040"
                             OR   WRK-NENREI   >= 75
                               MOVE      4    TO    IDX1
                           ELSE
                               IF  WRK-NENREI   >= 70
                                   MOVE    3    TO    IDX1
                               ELSE
                                   IF K100LNK-SRYYM < "200804"
                                       IF  WRK-NENREI  <  3
                                           MOVE    1    TO    IDX1
                                       ELSE
                                           MOVE    2    TO    IDX1
                                       END-IF
                                   ELSE
                                       IF  AGECHK-SYUGAKUKBN   =  1
                                           MOVE    1    TO    IDX1
                                       ELSE
                                           MOVE    2    TO    IDX1
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       WHEN "120"
                           MOVE    PTINF-BIRTHDAY(1:4)  TO  WRK-YY1-G
                           MOVE    K100LNK-SRYYM(1:4)  TO  WRK-YY2-G
                           COMPUTE  WRK-NENREI   =  WRK-YY2  -  WRK-YY1
                           IF  PTINF-BIRTHDAY(5:2) > K100LNK-SRYYM(5:2)
                               SUBTRACT  1    FROM  WRK-NENREI
                           END-IF
                           IF  COMB-KOH1HKNNUM      =  "027"
                             OR   COMB-HKNNUM  =  "039"
                             OR   COMB-HKNNUM  =  "040"
                             OR   WRK-NENREI   >= 75
                               MOVE      8    TO    IDX1
                           ELSE
                               IF  WRK-NENREI   >= 70
                                   MOVE    7    TO    IDX1
                               ELSE
                                   IF K100LNK-SRYYM < "200804"
                                       IF  WRK-NENREI  <  3
                                           MOVE    5    TO    IDX1
                                       ELSE
                                           MOVE    6    TO    IDX1
                                       END-IF
                                   ELSE
                                       IF  AGECHK-SYUGAKUKBN   =  1
                                           MOVE    5    TO    IDX1
                                       ELSE
                                           MOVE    6    TO    IDX1
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       WHEN "130"
                           MOVE    9     TO    IDX1
                       WHEN OTHER
                           MOVE    10    TO    IDX1
                   END-EVALUATE
               WHEN "21" THRU "27"
                   EVALUATE  TNS-SRYSYUKBN
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
                   EVALUATE  TNS-SRYSYUKBN
                       WHEN "310" THRU "311"
                           MOVE   23      TO    IDX1
                       WHEN "320" THRU "321"
                           MOVE   24      TO    IDX1
                       WHEN "330" THRU "331"
                       WHEN "340"
                           MOVE   25      TO    IDX1
                   END-EVALUATE
               WHEN "40"
                   MOVE     27      TO    IDX1
               WHEN "50" THRU "54"
                   EVALUATE  TNS-SRYSYUKBN
                       WHEN "500" THRU "502"
                           MOVE   28      TO    IDX1
                       WHEN "510"
                           MOVE   29      TO    IDX1
                       WHEN "540" THRU "542"
                           MOVE   30      TO    IDX1
                   END-EVALUATE
               WHEN "70"
                   MOVE     36      TO    IDX1
               WHEN "80"
                   EVALUATE  TNS-SRYSYUKBN
                       WHEN "820"
                           MOVE   37      TO    IDX1
                       WHEN "800"
                       WHEN "810"
                           MOVE   38      TO    IDX1
                       WHEN "840"
                       WHEN "830"
                       WHEN "850"
                           MOVE   39      TO    IDX1
                   END-EVALUATE
      *
               WHEN "90"
               WHEN "92"
               WHEN "97"
                   EVALUATE TNS-SRYSYUKBN
                       WHEN "900"
                       WHEN "920"
                           MOVE   42      TO    IDX1
                       WHEN "970"
                       WHEN "971"
                           MOVE   41      TO    IDX1
                   END-EVALUATE
      *
           END-EVALUATE
      *
      *    診療種別がスペースだったら診療区分で分類する
           IF   IDX1   =      ZERO
             EVALUATE  TNS-SRYKBN
               WHEN "11"
                 MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                     SUBTRACT  1       FROM  WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM    =  "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                     OR   WRK-NENREI   >= 75
                   MOVE      4          TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >=    70
                     MOVE    3      TO    IDX1
                   ELSE
                     IF  K100LNK-SRYYM < "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN "12"
                 MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                   SUBTRACT       1     FROM  WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM    = "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                     OR   WRK-NENREI   >= 75
                   MOVE    8      TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >= 70
                     MOVE    7      TO    IDX1
                   ELSE
                     IF K100LNK-SRYYM < "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "13"
                 MOVE     9         TO    IDX1
               WHEN  "14"
                 MOVE     13        TO    IDX1
               WHEN  "21"
                 MOVE     15        TO    IDX1
               WHEN  "22"
                 MOVE     16        TO    IDX1
               WHEN  "23"
                 MOVE     17        TO    IDX1
               WHEN  "24"
                 MOVE     18        TO    IDX1
               WHEN  "25"
                 MOVE     19        TO    IDX1
               WHEN  "26"
                 MOVE     20        TO    IDX1
               WHEN  "27"
                 MOVE     21        TO    IDX1
               WHEN  "31"
                 MOVE     23        TO    IDX1
               WHEN  "32"
                 MOVE     24        TO    IDX1
               WHEN  "33"
                 MOVE     25        TO    IDX1
               WHEN  "40"
                 MOVE     27        TO    IDX1
               WHEN  "50"
                 MOVE     28        TO    IDX1
               WHEN  "51"
                 MOVE     29        TO    IDX1
               WHEN  "54"
                 MOVE     30        TO    IDX1
               WHEN  "70"
                 MOVE     36        TO    IDX1
               WHEN  "80"
               WHEN  "93"
                 MOVE     37        TO    IDX1
               WHEN  "97"
                 MOVE     41        TO    IDX1
               WHEN  "90"
                 MOVE     42        TO    IDX1
               WHEN  "92"
                 MOVE     42        TO    IDX1
      *
               WHEN  OTHER
                 EVALUATE TNS-SRYCD
                   WHEN  "610000000"  THRU  "619999999"
      *    内服薬
                     MOVE     15   TO    IDX1
                   WHEN  "660000000"  THRU  "669999999"
      *    外用薬
                     MOVE     17   TO    IDX1
                   WHEN  OTHER
                     MOVE     39   TO    IDX1
                 END-EVALUATE
             END-EVALUATE
           END-IF
      *
           .
       20013-IDXSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN    INPUT       K145T-FILE
           MOVE    ZERO    TO  FLG-PRINT
           INITIALIZE          K145TMP-REC
      *
           INITIALIZE  IDX-S
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
           INITIALIZE                       WRK-K145ALL-AREA
           MOVE    ZERO                TO   IDX2
           MOVE    K145TMP-NYUGAIKBN   TO   WRK-TMP-NYUGAIKBN
      *    見出し編集
           PERFORM  310-HEAD-HEN-SEC
      *
           PERFORM  UNTIL  FLG-PRINT   =    1
      *
             IF  K145TMP-BTUINDEX  NOT =  ZERO
      *
               IF   K145TMP-NYUGAIKBN   NOT =   WRK-TMP-NYUGAIKBN
      *          全医師合計明細の印字
      *          印刷処理
                 IF   IDX2              >=   6
      *            印刷処理
                   PERFORM  330-PRINT-OUT-SEC
                   MOVE  ZERO         TO  IDX2
                 END-IF
      *          全医師明細編集
                 ADD      1              TO  IDX2
                 PERFORM  320-BODY-HEN2-SEC
      *          印刷処理
                 PERFORM  330-PRINT-OUT-SEC
                 INITIALIZE                  WRK-K145ALL-AREA
                 MOVE  ZERO              TO  IDX2
                 MOVE  K145TMP-NYUGAIKBN TO  WRK-TMP-NYUGAIKBN
      *          見出し編集
                 PERFORM  310-HEAD-HEN-SEC
               END-IF
      *
      *        １頁内で医師のMAXに達している場合
               IF   IDX2  >=     6
      *          印刷処理
                 PERFORM  330-PRINT-OUT-SEC
                 MOVE  ZERO              TO  IDX2
      *          見出し編集
                 PERFORM  310-HEAD-HEN-SEC
               END-IF
      *
      *        病棟別明細編集
               ADD      1   TO   IDX2
               PERFORM  320-BODY-HEN-SEC
      *
             END-IF
      *
      *      一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
      *    全医師合計明細の印字
           IF   IDX2    >=   6
      *    印刷処理
             PERFORM  330-PRINT-OUT-SEC
             MOVE  ZERO         TO  IDX2
      *    見出し編集
             PERFORM  310-HEAD-HEN-SEC
           END-IF
      *    全診療科明細編集
           ADD      1              TO  IDX2
           PERFORM  320-BODY-HEN2-SEC
      *
           IF   CNT-K145TMP   =    ZERO
             MOVE   "対象データが存在しません"    TO  K100-JOBMSG
             MOVE   SPACE          TO  K100-NYUGAIKBN
           END-IF
      *
      *    印刷処理
           PERFORM  330-PRINT-OUT-SEC
      *
           CLOSE    K145T-FILE
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
           INITIALIZE                     K100
      *
           MOVE   "(病棟別)"        TO    K100-SUBTITLE
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    K100-PAGE
      *
           IF    K145TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   K100-NYUGAIKBN
             PERFORM VARYING  IDX3  FROM 1  BY  1
                     UNTIL    IDX3  >    10
               MOVE  WRK-TJIHIN(IDX3) TO  K100-TJIHI(IDX3)
             END-PERFORM
           ELSE
             MOVE     "外来"        TO   K100-NYUGAIKBN
             PERFORM VARYING  IDX3  FROM 1  BY  1
                     UNTIL    IDX3  >    5
               MOVE  WRK-TJIHI(IDX3)  TO  K100-TJIHI(IDX3)
             END-PERFORM
           END-IF
      *
           MOVE    WRK-SRYYM        TO   K100-SRYYM
           MOVE    WRK-SEIYMD       TO   K100-SYSYMD
      *
           IF   K100LNK-SRYYM  <   "200804"
             MOVE   "(３歳未満)"   TO  K100-NENREIKBN(1)
             MOVE   "(老人)"       TO  K100-NENREIKBN2(1)
             MOVE   "(３歳未満)"   TO  K100-NENREIKBN(2)
             MOVE   "(老人)"       TO  K100-NENREIKBN2(2)
           ELSE
             MOVE   "(６歳)"   TO  K100-NENREIKBN(1)
             MOVE   "(後期高齢者)"       TO  K100-NENREIKBN2(1)
             MOVE   "(６歳)"   TO  K100-NENREIKBN(2)
             MOVE   "(後期高齢者)"       TO  K100-NENREIKBN2(2)
           END-IF
      *
      *    表示区分
           IF  K100LNK-TKKBN  =  2
             MOVE    "金額"           TO  WRK-T2NAME
           ELSE
             MOVE    "点数"           TO  WRK-T2NAME
           END-IF
      *
           PERFORM  VARYING  IDX3  FROM 1  BY  1
                    UNTIL    IDX3  >    6
             MOVE    WRK-T2NAME       TO  K100-T2NAME(IDX3)
           END-PERFORM
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<診療科別明細>処理
      *****************************************************************
       320-BODY-HEN-SEC               SECTION.
      *
           MOVE     1               TO    IDX4
           MOVE     WRK-BTUMAX      TO    IDX-MAX
           PERFORM  UNTIL    IDX4   >     IDX-MAX
             IF  K145TMP-BTUINDEX   =     WRK-BTUNUM(IDX4)
               MOVE    ZERO                   TO  IDX-MAX
               MOVE    WRK-BTUNAME(IDX4)      TO  K100-TNAME(IDX2)
               ADD     1                      TO  IDX-S
               MOVE    WRK-BTUNAME(IDX4)      TO  WRK-BODY-NAME(IDX-S)
             ELSE
               ADD     1                      TO    IDX4
             END-IF
           END-PERFORM
      *
           PERFORM    VARYING     IDX1    FROM  1  BY  1
                      UNTIL       IDX1    >     56
      *
             EVALUATE IDX1
               WHEN 11
                 COMPUTE     K145TMP-TENSU(11)    =
                             K145TMP-TENSU(01)
                       +     K145TMP-TENSU(02)
                       +     K145TMP-TENSU(03)
                       +     K145TMP-TENSU(04)
                       +     K145TMP-TENSU(05)
                       +     K145TMP-TENSU(06)
                       +     K145TMP-TENSU(07)
                       +     K145TMP-TENSU(08)
                       +     K145TMP-TENSU(09)
                       +     K145TMP-TENSU(10)
                 COMPUTE     K145TMP-KAISU(11)    =
                             K145TMP-KAISU(01)
                       +     K145TMP-KAISU(02)
                       +     K145TMP-KAISU(03)
                       +     K145TMP-KAISU(04)
                       +     K145TMP-KAISU(05)
                       +     K145TMP-KAISU(06)
                       +     K145TMP-KAISU(07)
                       +     K145TMP-KAISU(08)
                       +     K145TMP-KAISU(09)
                       +     K145TMP-KAISU(10)
               WHEN 22
                 COMPUTE     K145TMP-TENSU(22)    =
                             K145TMP-TENSU(15)
                       +     K145TMP-TENSU(16)
                       +     K145TMP-TENSU(17)
                       +     K145TMP-TENSU(18)
                       +     K145TMP-TENSU(19)
                       +     K145TMP-TENSU(20)
                       +     K145TMP-TENSU(21)
                 COMPUTE     K145TMP-KAISU(22)    =
                             K145TMP-KAISU(15)
                       +     K145TMP-KAISU(16)
                       +     K145TMP-KAISU(17)
                       +     K145TMP-KAISU(18)
                       +     K145TMP-KAISU(19)
                       +     K145TMP-KAISU(20)
                       +     K145TMP-KAISU(21)
               WHEN 26
                 COMPUTE     K145TMP-TENSU(26)    =
                             K145TMP-TENSU(23)
                       +     K145TMP-TENSU(24)
                       +     K145TMP-TENSU(25)
                 COMPUTE     K145TMP-KAISU(26)    =
                             K145TMP-KAISU(23)
                       +     K145TMP-KAISU(24)
                       +     K145TMP-KAISU(25)
               WHEN 31
                 COMPUTE     K145TMP-TENSU(31)    =
                             K145TMP-TENSU(28)
                       +     K145TMP-TENSU(29)
                       +     K145TMP-TENSU(30)
                 COMPUTE     K145TMP-KAISU(31)    =
                             K145TMP-KAISU(28)
                       +     K145TMP-KAISU(29)
                       +     K145TMP-KAISU(30)
               WHEN 40
                 COMPUTE     K145TMP-TENSU(40)    =
                             K145TMP-TENSU(37)
                       +     K145TMP-TENSU(38)
                       +     K145TMP-TENSU(39)
                 COMPUTE     K145TMP-KAISU(40)    =
                             K145TMP-KAISU(37)
                       +     K145TMP-KAISU(38)
                       +     K145TMP-KAISU(39)
               WHEN 55
                 COMPUTE     K145TMP-TENSU(55)    =
                             K145TMP-TENSU(43)
                       +     K145TMP-TENSU(44)
                       +     K145TMP-TENSU(45)
                       +     K145TMP-TENSU(46)
                       +     K145TMP-TENSU(47)
                       +     K145TMP-TENSU(48)
                       +     K145TMP-TENSU(49)
                       +     K145TMP-TENSU(50)
                       +     K145TMP-TENSU(51)
                       +     K145TMP-TENSU(52)
                       +     K145TMP-TENSU(53)
                 COMPUTE     K145TMP-KAISU(55)    =
                             K145TMP-KAISU(43)
                       +     K145TMP-KAISU(44)
                       +     K145TMP-KAISU(45)
                       +     K145TMP-KAISU(46)
                       +     K145TMP-KAISU(47)
                       +     K145TMP-KAISU(48)
                       +     K145TMP-KAISU(49)
                       +     K145TMP-KAISU(50)
                       +     K145TMP-KAISU(51)
                       +     K145TMP-KAISU(52)
                       +     K145TMP-KAISU(53)
               WHEN 56
                 COMPUTE     K145TMP-TENSU(56)    =
                             K145TMP-TENSU(11)
                       +     K145TMP-TENSU(14)
                       +     K145TMP-TENSU(22)
                       +     K145TMP-TENSU(26)
                       +     K145TMP-TENSU(27)
                       +     K145TMP-TENSU(31)
                       +     K145TMP-TENSU(35)
                       +     K145TMP-TENSU(36)
                       +     K145TMP-TENSU(40)
                       +     K145TMP-TENSU(42)
                 COMPUTE     K145TMP-KAISU(56)    =
                             K145TMP-KAISU(11)
                       +     K145TMP-KAISU(14)
                       +     K145TMP-KAISU(22)
                       +     K145TMP-KAISU(26)
                       +     K145TMP-KAISU(27)
                       +     K145TMP-KAISU(31)
                       +     K145TMP-KAISU(35)
                       +     K145TMP-KAISU(36)
                       +     K145TMP-KAISU(40)
                       +     K145TMP-KAISU(42)
             END-EVALUATE
             MOVE     K145TMP-TENSU(IDX1) TO  WRK-TENSU-HEN
             MOVE     WRK-TENSU-HEN       TO  K100-TENSU(IDX2 IDX1)
                                              WRK-BODY-TENSU(IDX-S IDX1)
      *
             MOVE     K145TMP-KAISU(IDX1) TO  WRK-KAISU-HEN
             MOVE     WRK-KAISU-HEN       TO  K100-KAISU(IDX2 IDX1)
                                              WRK-BODY-KAISU(IDX-S IDX1)
      *
      *    全診療科加算エリアに加算する。
      *
             COMPUTE  WRK-K145ALL-TENSU(IDX1) =
                            WRK-K145ALL-TENSU(IDX1)
                        +   K145TMP-TENSU(IDX1)
             COMPUTE  WRK-K145ALL-KAISU(IDX1) =
                            WRK-K145ALL-KAISU(IDX1)
                        +   K145TMP-KAISU(IDX1)
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<全診療科明細>処理
      *****************************************************************
       320-BODY-HEN2-SEC               SECTION.
      *
           MOVE    "全病棟"             TO  K100-TNAME(IDX2)
           ADD     1                    TO  IDX-S
           MOVE    "全病棟"             TO  WRK-BODY-NAME(IDX-S)
      *
           PERFORM    VARYING  IDX1   FROM  1  BY  1
                      UNTIL    IDX1   >     56
      *
             MOVE     WRK-K145ALL-TENSU(IDX1)
                                          TO  WRK-TENSU-HEN
             MOVE     WRK-TENSU-HEN       TO  K100-TENSU(IDX2 IDX1)
                                              WRK-BODY-TENSU(IDX-S IDX1)
      *
             MOVE     WRK-K145ALL-KAISU(IDX1)
                                          TO  WRK-KAISU-HEN
             MOVE     WRK-KAISU-HEN       TO  K100-KAISU(IDX2 IDX1)
                                              WRK-BODY-KAISU(IDX-S IDX1)
      *
           END-PERFORM
      *
           .
       320-BODY-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO  SPRT-MODE
           MOVE    K100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    K100LNK-PRTKANRI-TBL-KEY
                                           TO  SPRT-TBL-KEY
           MOVE    K100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    K100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    K100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    K100LNK-PRTKANRI-SHELLID
                                           TO  SPRT-SHELLID
           MOVE    K100LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SPRT-SHORI-RENNUM
           MOVE    K100LNK-PRTKANRI-PRIORITY
                                           TO  SPRT-PRIORITY
           MOVE   "A00000K100V03.red"      TO  SPRT-PRTID
           MOVE    "(病棟別)診療行為区分別集計表"
                                           TO  SPRT-TITLE
           MOVE    K100                    TO  SPRT-PRTDATA
           MOVE    K100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    K100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    K100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
      *
           IF      SPRT-RETURN         =   ZERO
             CONTINUE
           END-IF
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1               TO  IDXP
      *
           INITIALIZE  WRK-ITEM
           STRING  "対象年月："          DELIMITED  BY  SIZE
                   WRK-SRYYM             DELIMITED  BY  SPACE
                                         INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
           IF    K145TMP-NYUGAIKBN   =   "1"
             MOVE    "入外区分：入院"    TO  WRK-ITEM
           ELSE
             MOVE    "入外区分：外来"    TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
           INITIALIZE  WRK-ITEM
           STRING  WRK-SEIYMD            DELIMITED  BY  SPACE
                   "  発行"              DELIMITED  BY  SIZE
                                         INTO  WRK-ITEM
           END-STRING
           MOVE    WRK-SEIYMD          TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    病棟
           INITIALIZE  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX8  FROM  1  BY  1
                    UNTIL    IDX8  >  IDX-S
      *
             MOVE    WRK-BODY-NAME(IDX8)      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
             INITIALIZE  WRK-ITEM
             IF  IDX8  =  IDX-S
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
           END-PERFORM
      *
      *    表示区分
           IF  K100LNK-TKKBN  =  2
             MOVE    "金額"           TO  WRK-T2NAME
           ELSE
             MOVE    "点数"           TO  WRK-T2NAME
           END-IF
      *
      *    回数／点数
           INITIALIZE  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX8  FROM  1  BY  1
                    UNTIL    IDX8  >  IDX-S
      *
             MOVE    WRK-T2NAME               TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
             MOVE    "回数"                   TO  WRK-ITEM
             IF  IDX8  =  IDX-S
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
      *    項目名称・明細情報セット
           INITIALIZE  IDX10
           PERFORM  VARYING  IDX9  FROM  1  BY  1
                    UNTIL    IDX9  >  CSV-HEAD-MAX
      *
      *      項目名称セット
             EVALUATE  IDX9
      *
               WHEN  44 THRU 53
      *          自費名称
                 ADD   1                         TO  IDX10
                 INITIALIZE  WRK-ITEM
                 IF  K145TMP-NYUGAIKBN   =   "1"
                   STRING  WRK-TJIHIN(IDX10)     DELIMITED  BY  SPACE
                           "(円)"                DELIMITED  BY  SIZE
                                                 INTO  WRK-ITEM
                   END-STRING
                 ELSE
                   STRING  WRK-TJIHI(IDX10)      DELIMITED  BY  SPACE
                           "(円)"                DELIMITED  BY  SIZE
                                                 INTO  WRK-ITEM
                   END-STRING
                 END-IF
      *
               WHEN  54
      *          自費名称
                 INITIALIZE  WRK-ITEM
                 MOVE    "(円)"                  TO  WRK-ITEM
      *
               WHEN  OTHER
      *          その他の項目名称
                 INITIALIZE  WRK-ITEM
                 MOVE    CSV-HEAD-ITEM(IDX9)     TO  WRK-ITEM
             END-EVALUATE
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      明細情報  インデックスセット
             EVALUATE  IDX9
               WHEN  35
                 COMPUTE  IDX9X  =  IDX9 + 19
               WHEN  36 THRU 53
                 COMPUTE  IDX9X  =  IDX9 - 1
               WHEN  OTHER
                 COMPUTE  IDX9X  =  IDX9
             END-EVALUATE
      *
      *      明細情報セット
             PERFORM  VARYING  IDX8  FROM  1  BY  1
                      UNTIL    IDX8  >  IDX-S
      *
               IF  IDX9  NOT =  54
                 MOVE    WRK-BODY-TENSU(IDX8 IDX9X)  TO  WRK-ITEM
                 PERFORM  800-CSVDATA-EDIT-SEC
                 MOVE    WRK-BODY-KAISU(IDX8 IDX9X)  TO  WRK-ITEM
               ELSE
                 INITIALIZE  WRK-ITEM
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
      *
               IF  IDX8  =  IDX-S
                 PERFORM  810-CSVDATA-EDIT-SEC
               ELSE
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
      *
             END-PERFORM
      *
             PERFORM  230-TOUKEICSV-SEC
      *
           END-PERFORM
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
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX1:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX1:1)  NOT =  SPACE
                 MOVE    IDX1      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
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
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                         ORCSTOUKEICSVAREA
           MOVE    "INS"                      TO  STOUKEICSV-MODE
           MOVE    K100LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
           MOVE    K100LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
           MOVE    K100LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
           MOVE    K100LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
           MOVE    K100LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
           MOVE    K100LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
           MOVE    ZERO                       TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"                TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM                DELIMITED  BY  SIZE
                   "shinryoukubun_byoutou_"   DELIMITED  BY  SIZE
                   K100LNK-SRYYM              DELIMITED  BY  SPACE
                   ".csv"                     DELIMITED  BY  SIZE
                                              INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                        TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA                TO  STOUKEICSV-CSVDATA
           MOVE    "(病棟別)診療行為区分別集計表"
                                              TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"            USING
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
           INITIALIZE                         WRK-CSVDATA
           MOVE    1                          TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    K100LNK-PRTKANRI-SKYYMD
                                       TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_syskanri"     TO  MCP-TABLE
             MOVE    "key10"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
             ELSE
               MOVE    1                   TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み(自費情報)
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF    MCP-RC   =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI2
             ELSE
               MOVE    1               TO  FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(医師情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYSYMD           TO  ORC-DBYMD
           MOVE    SYS-1010-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1               TO   FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(医師情報)
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
           IF   MCP-RC   =   ZERO
             MOVE    ZERO             TO  FLG-SYSKANRI3
             MOVE    MCPDATA-REC      TO  SYS-1010-REC
           ELSE
             MOVE    1                TO  FLG-SYSKANRI3
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
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"   TO  MCP-TABLE
           MOVE    "key2"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
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
           MOVE    WRK-SYSYMD            TO  ORC-DBYMD
           MOVE    SYS-1010-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           IF   MCP-RC   =   ZERO
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
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           INITIALIZE                    STS-AREA-DAY
           INITIALIZE                    LNK-DAY7-AREA
           MOVE    "71"          TO      LNK-DAY7-IRAI
           MOVE    WRK-SYMD      TO      LNK-DAY7-YM
           CALL    "ORCSDAY"     USING   STS-AREA-DAY
                                         LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO      WRK-SYMD
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
      *    ジョブ終了処理
             MOVE    "JBE"             TO  SJOBKANRI-MODE
             INITIALIZE                    JOBKANRI-REC
             MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
             MOVE    K100LNK-JOBID     TO  JOB-JOBID
             MOVE    K100LNK-SHELLID   TO  JOB-SHELLID
             MOVE    WRK-RECEERR       TO  JOB-YOBI
             MOVE    "9999"            TO  JOB-ERRCD
             CALL    "ORCSJOB"         USING
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
      *    診療行為受診対象患者一時ファイル読込(KEY READ)
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    K145T-FILE
             INVALID    KEY
               MOVE    1           TO  FLG-M01TMP
             NOT  INVALID  KEY
               MOVE    ZERO        TO  FLG-M01TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  K145T-FILE   NEXT
             AT  END
               MOVE  1   TO  FLG-PRINT
             NOT  AT  END
               ADD   1   TO  CNT-K145TMP
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(病棟情報)
      *****************************************************************
       830-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYSYMD           TO  ORC-DBYMD
           MOVE    SYS-5001-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM 830-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1               TO   FLG-SYSKANRI4
           END-IF
           .
       830-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(病棟情報)
      *****************************************************************
       830-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    ZERO             TO  FLG-SYSKANRI4
             MOVE    MCPDATA-REC      TO  SYS-5001-REC
           ELSE
             MOVE    1                TO  FLG-SYSKANRI4
           END-IF
           .
       830-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       830-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       830-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    K100LNK-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE   "tbl_ptinf"         TO  MCP-TABLE
             MOVE   "key"               TO  MCP-PATHNAME
             CALL   "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO    TO  FLG-PTINF
             ELSE
               MOVE    1       TO  FLG-PTINF
               INITIALIZE      PTINF-REC
             END-IF
           ELSE
             MOVE    1           TO  FLG-PTINF
             INITIALIZE          PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC   SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SRY-SRYCD(IDZ)      TO  TNS-SRYCD
           MOVE    ZERO                TO  TNS-YUKOSTYMD
      *
           MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key21"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             PERFORM 900-TENSU-READ-CHK-SEC
           ELSE
             MOVE    1                   TO  FLG-TENSU
             INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-TENSU-READ-EXT.
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
      *****************************************************************
      *    点数マスタ対象期間チェック(複数レコードの読込み)
      *****************************************************************
       900-TENSU-READ-CHK-SEC   SECTION.
      *
           INITIALIZE GET-TENSU
           PERFORM    UNTIL    GET-TENSU   =   1
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE   "tbl_tensu"         TO  MCP-TABLE
             MOVE   "key21"             TO  MCP-PATHNAME
             CALL   "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO  TNS-TENSU-REC
               MOVE    ZERO             TO  FLG-TENSU
             ELSE
               MOVE    1                TO  FLG-TENSU
               INITIALIZE               TNS-TENSU-REC
               MOVE    1                TO  GET-TENSU
               PERFORM 900-TENSU-READ2-SEC
             END-IF
             IF    TNS-YUKOSTYMD  <=      WRK-PARA-PRTLYMD
                 AND  TNS-YUKOEDYMD  >=      WRK-PARA-PRTLYMD
               MOVE  1                TO  GET-TENSU
             END-IF
           END-PERFORM
      *
           .
       900-TENSU-READ-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込処理２(単一レコードの読込み)
      *****************************************************************
       900-TENSU-READ2-SEC   SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SRY-SRYCD(IDZ)      TO  TNS-SRYCD
           MOVE    WRK-TENYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-TENYMD          TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE   "tbl_tensu"         TO  MCP-TABLE
             MOVE   "key"               TO  MCP-PATHNAME
             CALL   "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF    MCP-RC    =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
             ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-TENSU
             INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-TENSU-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査ソートマスタ読込処理
      *****************************************************************
       900-KENSA-READ-SEC   SECTION.
      *
           INITIALIZE                      KENSASORT-REC
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
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE   "tbl_kensasort"     TO  MCP-TABLE
             MOVE   "key"               TO  MCP-PATHNAME
             CALL   "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  KENSASORT-REC
               MOVE    ZERO                TO  FLG-KENSASORT
             ELSE
               MOVE    1                   TO  FLG-KENSASORT
               INITIALIZE                  KENSASORT-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-KENSASORT
             INITIALIZE                  KENSASORT-REC
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
           INITIALIZE                       SRYACT-REC
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
      *
           IF    MCP-RC  =  ZERO
             MOVE  ZERO               TO FLG-SRYACT
             PERFORM   900-SRYACT-FETCH-SEC
           ELSE
             MOVE   1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報１件読み込み(抽出)
      *****************************************************************
       900-SRYACT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC              =   ZERO
             MOVE    MCPDATA-REC     TO  SRYACT-REC
             MOVE    ZERO            TO  FLG-SRYACT
           ELSE
             MOVE   1                TO  FLG-SRYACT
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報クローズ(抽出)
      *****************************************************************
       900-SRYACT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       910-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC
           MOVE    K100LNK-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    K100LNK-SRYYM        TO  ACCT-SRYYM
           MOVE    WRK-NYUGAIKBN        TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC   =   ZERO
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
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
             IF    ACCT-SRYKBN    =    "99"
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
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴情報検索 SELECT
      *****************************************************************
       900-JYURRK-SELECT-SEC               SECTION.
      *
           INITIALIZE                      JYURRK-REC
           MOVE    K100LNK-HOSPNUM     TO  JYURRK-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD    TO  JYURRK-SRYYMD
           MOVE    WRK-PARA-PRTLYMD    TO  JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
             MOVE    ZERO        TO  FLG-JYURRK
           ELSE
             MOVE    1           TO  FLG-JYURRK
           END-IF
           .
       900-JYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報検索 FETCH
      *****************************************************************
       900-JYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key21"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC           =   ZERO
             MOVE    ZERO         TO  FLG-JYURRK
             MOVE    MCPDATA-REC  TO  JYURRK-REC
           ELSE
             MOVE    1            TO  FLG-JYURRK
             INITIALIZE               JYURRK-REC
           END-IF
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-JYURRK-CLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC
           MOVE    JYURRK-HOSPNUM       TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN     TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID          TO  ACCT-PTID
           MOVE    JYURRK-SRYKA         TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)   TO  ACCT-SRYYM
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC               =   ZERO
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
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC              =   ZERO
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
             IF    ACCT-SRYKBN    =    "99"
               GO    TO    900-SRYACCT-FETCH-SEC
             END-IF
             ADD    1               TO  CNT-DATA
           ELSE
             MOVE   1               TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       900-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       900-NYUINACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   NYUINACCT-REC
           MOVE    K100LNK-HOSPNUM      TO  NACCT-HOSPNUM
           MOVE    K100LNK-SRYYM        TO  NACCT-SRYYM
           MOVE    WRK-NYUGAIKBN        TO  NACCT-NYUGAIKBN
           MOVE    NYUINACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    ZERO         TO  FLG-NSRYACCT
             PERFORM    900-NYUINACCT-FETCH-SEC
           ELSE
             MOVE   1             TO  FLG-NSRYACCT
           END-IF
           .
       900-NYUINACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報１件読み込み
      *****************************************************************
       900-NYUINACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    MCPDATA-REC     TO  NYUINACCT-REC
             MOVE    ZERO            TO  FLG-NSRYACCT
      *    テスト患者は対象外
             MOVE    NACCT-PTID    TO  WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF   FLG-PTINF  =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-NYUINACCT-FETCH-SEC
             END-IF
      *    コメントは対象外
             IF    ACCT-SRYKBN   =    "99"
               GO    TO    900-NYUINACCT-FETCH-SEC
             END-IF
             ADD    1               TO  CNT-DATA
           ELSE
             MOVE   1               TO  FLG-NSRYACCT
           END-IF
           .
       900-NYUINACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       900-NYUINACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE    K100LNK-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    "1"                  TO  SYU-NYUGAIKBN
           MOVE    NACCT-PTID            TO  SYU-PTID
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
           IF    MCP-RC    =   ZERO
             PERFORM 940-SYUNOU-FETCH-SEC
           ELSE
             MOVE   1               TO  FLG-SYUNOU
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
           IF  MCP-RC    =   ZERO
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
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
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
      *   保険組合せ情報セット
      *****************************************************************
       930-HKNCOMBI-SET-SEC  SECTION.
      *
           INITIALIZE  HKNCOMBI-REC
           MOVE  ACCT-HOSPNUM         TO  COMB-HOSPNUM
           MOVE  ACCT-PTID            TO  COMB-PTID
           MOVE  ACCT-HKNCOMBI        TO  COMB-HKNCOMBINUM
           MOVE  HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE  "DBFETCH"  TO  MCP-FUNC
             MOVE  "tbl_hkncombi"   TO  MCP-TABLE
             MOVE  "key"            TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"      USING
                                  MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE  MCPDATA-REC  TO  HKNCOMBI-REC
               MOVE  ZERO         TO  FLG-HKNCOMBI
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE  1            TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1            TO  FLG-HKNCOMBI
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_hkncombi" TO  MCP-TABLE
           MOVE    "key"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"    USING
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
           MOVE    ACCT-HOSPNUM       TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM         TO  HKN-HKNNUM
           MOVE    "00"               TO  HKN-PAYKBN
           MOVE    ALL "9"            TO  HKN-TEKSTYMD
           MOVE    ZERO               TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"          TO  MCP-FUNC
             MOVE  "tbl_hknnum"       TO  MCP-TABLE
             MOVE  "key"              TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC    TO  HKNNUM-REC
               MOVE  ZERO           TO  FLG-HKNNUM
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE  1            TO  FLG-HKNNUM
             END-IF
      *
           ELSE
             MOVE    1            TO  FLG-HKNNUM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
           MOVE    "tbl_hknnum"      TO  MCP-TABLE
           MOVE    "key"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
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
           MOVE    PTINF-HOSPNUM        TO  PTNYUINRRK-HOSPNUM
           MOVE    PTINF-PTID           TO  PTNYUINRRK-PTID
           MOVE    WRK-BTU-SRYYMD       TO  PTNYUINRRK-TENNYUYMD
           MOVE    WRK-BTU-SRYYMD       TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key42"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             PERFORM 930-PTNYUINRRK-FETCH-SEC
           ELSE
             MOVE   1             TO  FLG-PTNYUINRRK
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
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
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
           MOVE    "key42"              TO  MCP-PATHNAME
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
           MOVE    SYS-5113-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC         NOT  =   ZERO
             MOVE    1               TO  FLG-ROOM
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
           IF      MCP-RC    =   ZERO
             MOVE    MCPDATA-REC     TO  SYS-5113-REC
             MOVE    ZERO            TO  FLG-ROOM
           ELSE
             MOVE    1               TO  FLG-ROOM
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
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SYS-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
             IF    MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
             ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-HKNCOMBI
             INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       920-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC2
           MOVE    SPA-HOSPNUM         TO  ACCT2-HOSPNUM
           MOVE    K100LNK-SRYYM       TO  ACCT2-SRYYM
           MOVE    SYU-NYUGAIKBN       TO  ACCT2-NYUGAIKBN
           MOVE    SYU-PTID            TO  ACCT2-PTID
           MOVE    SRYACCT-REC2        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    ZERO         TO  FLG-SRYACCT2
             PERFORM    920-SRYACCT-FETCH-SEC
           ELSE
             MOVE   1             TO  FLG-SRYACCT2
           END-IF
           .
       920-SRYACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報１件読み込み
      *****************************************************************
       920-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    MCPDATA-REC     TO  SRYACCT-REC2
             MOVE    ZERO            TO  FLG-SRYACCT2
      *    テスト患者は対象外
             MOVE    ACCT2-PTID    TO  WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF    FLG-PTINF  =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  920-SRYACCT-FETCH-SEC
             END-IF
      *    コメントは対象外
             IF    ACCT2-SRYKBN    =    "99"  OR  "98"
               GO    TO    920-SRYACCT-FETCH-SEC
             END-IF
             ADD    1               TO  CNT-DATA
           ELSE
             MOVE   1               TO  FLG-SRYACCT2
           END-IF
           .
       920-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       920-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       920-SRYACCT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
      *    患者保険情報読み込み
           INITIALIZE                  PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    WRK-PTID            TO  PTRSI-PTID
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
      *
           .
       900-PTRSIINF-READ-EXT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    K100LNK-JOBID       TO  JOB-JOBID
           MOVE    K100LNK-SHELLID     TO  JOB-SHELLID
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
