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
       PROGRAM-ID.             ORCBNYUALL.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次処理
      *  コンポーネント名  : 入院診療行為一括登録
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/11/29    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.05.00     NACL-多々納  10/01/XX  治験対応
      * 04. 05.00    NACL-太田    10/01/29  拡張漢字対応
      * 04.07.00     NACL-多々納  11/10/31  同日再入院対応
      * 04.07.00     NACL-多々納  14/01/17  数量ゼロエラー対応
      * 04.08.00     NACL-多々納  14/07/17  一時ファイル対応
      * 04.08.00     NACL-多々納  17/01/17  翌月入院料チェック対応
      * 04.08.00     NACL-多々納  18/09/XX  選択式コメント対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *    データ
           SELECT  DATA-FILE       ASSIGN  FILE-NAME1
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-DATA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *    ワーク診療行為データ
       FD  DATA-FILE.
       01  DATA-REC                   PIC X(6000).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名 (X(1024))
       01  FILE-NAME1.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(08)   VALUE   "WKSRYACT".
           03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
           03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
           03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
           03  FILLER              PIC X(935)  VALUE   SPACE.
      *
           COPY    "CPRECEERR.INC".
      *
      *
      *    確認一覧（縦）
           COPY    "HCMSL80.INC".
      *    エラー一覧（横）
           COPY    "HCMSL55.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
           03  STS-PARA                PIC X(02).
           03  STS-DATA                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-WKSRYACT            PIC 9(01).
           03  FLG-LOCK                PIC 9(01).
           03  FLG-PARA                PIC 9(01).
      *
           03  FLG-SYORI-ERR           PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-TOROKU              PIC 9(01).
           03  FLG-END-SYORI           PIC 9(01).
           03  FLG-END-SYORI2          PIC 9(01).
           03  FLG-LOCKERR             PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE1               PIC 9(03).
           03  CNT-PAGE1               PIC 9(05).
           03  CNT-LINE2               PIC 9(03).
           03  CNT-PAGE2               PIC 9(05).
      *
           03  CNT-IN                  PIC 9(05).
           03  CNT-OK                  PIC 9(05).
           03  CNT-ERR                 PIC 9(05).
           03  CNT-KEI                 PIC 9(05).
           03  CNT-OUT                 PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX-ERR                 PIC 9(04).
           03  IDXN                    PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX4                    PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH             PIC 9(02).
               05  SYS-TMM             PIC 9(02).
               05  SYS-TSS             PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
    ****** 03  RECEERR                 PIC X(100).
           03  WRK-PARA-SRYYMD         PIC X(08).
           03  WRK-PARA-SYORIKBN       PIC X(01).
           03  WRK-PARA-PRTKBN1        PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYSYMDG             PIC X(22).
      *
           03  WRK-EDABAN              PIC 9(03).
      *
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(80).
           03  WRK-ERRMSG2             PIC X(80).
      *    入院日
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SRYDD               PIC 9(02).
      *
           03  WRK-ZAIJUNKBN-FLG       PIC X(01).
      *    診療行為ＳＯＲＴ
           03  WRK-SORT-KEY.
               05  WRK-ZAINUM-X.
                   07  WRK-ZAINUM          PIC  9(08).
               05  WRK-RENNUM-X.
                   07  WRK-RENNUM          PIC  9(02).
               05  WRK-SRYKBN          PIC  X(02).
           03  WRK-MAX                 PIC  9(04).
      *
           03  LINE-MAX                PIC 9(04).
      *H29.1
      *    翌月入院料チェック
           03  FLG-NEXTTUKI            PIC 9(01).
           03  IDN1                    PIC 9(04).
           03  IDN3                    PIC 9(04).
           03  IDN4                    PIC 9(04).
           03  WRK-KAISU               PIC 9(03).
      *
      *
           03  WRK-ERR-MEI             PIC X(80).
           03  WRK-ERR-MEI-R           REDEFINES  WRK-ERR-MEI.
               05  WRK-ERR-MEI1            PIC X(56).
               05  WRK-ERR-MEI2            PIC X(24).
      *
       01  TBL-KEIMSG-AREA.
           03  TBL-KEIMSG-G            OCCURS   50.
               05  TBL-KEIERRCD            PIC X(04).
               05  TBL-KEIMSG              PIC X(80).
           03  IDX-KEI                     PIC 9(03).
      *
      *    リスト領域
       01  LST-ERR-AREA.
           03  LST-ERR-HEAD1.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(40)   VALUE   
                   "＃＃入院診療行為一括処理エラーリスト＃＃".
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE
                   "診療日：".
               05  ERR-SRYYMD          PIC X(22).
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  ERR-SYSYMD          PIC X(10)   VALUE   SPACE.
               05  ERR-TIME            PIC X(07)   VALUE   SPACE.
               05  ERR-PAGE            PIC ZZZ9.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  LST-ERR-HEAD2.
               05  FILLER              PIC X(20)   VALUE
                   "患者番号".
      *        05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(28)   VALUE
                   "氏名".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE
                   "科".
               05  FILLER              PIC X(12)   VALUE
                   "保険組合せ".
               05  FILLER              PIC X(80)   VALUE
                   "エラー内容".
           03  LST-ERR-BODY-G.
               05  LST-ERR-BODY        OCCURS   50.
                   07  ERR-PTNUM           PIC X(20).
                   07  ERR-NAME            PIC X(30).
                   07  FILLER              PIC X(01).
                   07  ERR-SRYKA           PIC X(02).
                   07  FILLER              PIC X(01).
                   07  ERR-HKNCOMBI        PIC X(04).
                   07  FILLER              PIC X(06).
                   07  ERR-ERRMSG          PIC X(80).
           03  LST-ERR-TAIL1.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "エラー件数：".
               05  ERR-CNT             PIC ZZ,ZZ9.
               05  FILLER              PIC X(04)   VALUE
                   "　件".
      * 確認リスト
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  PRT-MIDASI          PIC X(40).
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE
                   "診療日：".
               05  PRT-SRYYMD          PIC X(22).
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  PRT-SYSYMD          PIC X(10)   VALUE   SPACE.
               05  PRT-TIME            PIC X(07)   VALUE   SPACE.
               05  PRT-PAGE            PIC ZZZ9.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  LST-PRT-HEAD2.
               05  FILLER              PIC X(20)   VALUE
                   "患者番号".
      *        05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(28)   VALUE
                   "氏名".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE
                   "科".
               05  FILLER              PIC X(12)   VALUE
                   "保険組合せ".
               05  FILLER              PIC X(80)   VALUE
                   "警告内容".
           03  LST-PRT-BODY-G.
               05  LST-PRT-BODY        OCCURS      50.
                   07  PRT-PTNUM           PIC X(20). 
                   07  PRT-NAME            PIC X(30).
                   07  FILLER              PIC X(01).
                   07  PRT-SRYKA           PIC X(02).
                   07  FILLER              PIC X(01).
                   07  PRT-HKNCOMBI        PIC X(04).
                   07  FILLER              PIC X(06).
                   07  PRT-MSG             PIC X(80).
           03  LST-PRT-TAIL1.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(10)   VALUE
                   "更新件数：".
               05  PRT-OKCNT           PIC ZZ,ZZ9.
               05  FILLER              PIC X(04)   VALUE
                   "　件".
               05  FILLER              PIC X(12)   VALUE
                   "（警告件数：".
               05  PRT-KEICNT          PIC ZZ,ZZ9.
               05  FILLER              PIC X(06)   VALUE
                   "　件）".
      *
       01  CONS-HEAD.
           03  CONS-HAED1                  PIC X(40)   VALUE   
                   "＝入院診療行為一括処理確認・警告リスト＝".
           03  CONS-HAED2                  PIC X(40)   VALUE   
                   "＝＝入院診療行為一括処理警告リスト＝＝　".
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  FILLER          PIC X(01)   VALUE   "(".
               05  WRK-THH         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-TMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ")".
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK1009.INC".
      *    チェック機能制御情報 
           COPY    "CPSK1008.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *    患者番号変換
      *01  PTNUM-REC.
      *    COPY    "CPPTNUM.INC".
      *    診療行為マスタ−
      *01  SRYACT-REC.
      *    COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
      *01  HKNCOMBI-REC.
      *    COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
      *01  PTCOM-REC.
      *    COPY    "CPPTCOM.INC".
      *
      *    受診履歴
      *01  JYURRK-REC.
      *    COPY    "CPJYURRK.INC".
      *
      *    診療会計マスタ−
      *01  SRYACCT-REC.
      *    COPY    "CPSRYACCT.INC".
      *
      *    診療科履歴
      *01  SRYKARRK-REC.
      *    COPY    "CPSRYKARRK.INC".
      *
       01  HZN-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //HZN-WKSRY-//.
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    ＳＯＲＴワーク診療行為
       01  SRT-WKSRYACT-AREA.
           02  SRT-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //SRT-WKSRY-//.
      *    附加情報を追加
           03  SRT-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //SRT-WKSRYP-//.
      *
       01  SRT2-WKSRYACT-AREA.
           02  SRT2-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //SRT2-WKSRY-//.
      *    附加情報を追加
           03  SRT2-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //SRT2-WKSRYP-//.
       01  SRT2-WKSRYACT-MAX           PIC 9(04).
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
      *01  PRTKANRI-REC.
      *    COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
      *01  PRTDATA-REC.
      *    COPY    "CPPRTDATA.INC".
      *
      *    排他管理情報（詳細）
           COPY  "CPTBLLOCK.INC".
      *
      *ver.4.8
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    点数編集チェックパラメタ
           COPY    "CPORCSC92.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCWKSRY.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCNYUIN.INC".
      *
      *    診療行為内容編集・計算
           COPY    "CPORCSC100.INC".
      *
      *    入院履歴編集サブパラメタ
           COPY    "CPORCSCNYUHEN.INC".
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *    全角変換
           COPY    "CPORCSKANACHK.INC".
           COPY    "CPORCSKANACHK2.INC".
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
      *    オンライン帳票名・出力先プリンタ名取得パラ
      *    COPY    "CPORCSPRTNM.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
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
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  MCPAREA
           MOVE    SPACE           TO  LST-ERR-BODY-G
           MOVE    SPACE           TO  LST-PRT-BODY-G
           INITIALIZE                  LST-ERR-BODY-G
           INITIALIZE                  LST-PRT-BODY-G
           MOVE    ZERO            TO  CNT-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-SRYYMD
                                       WRK-PARA-SYORIKBN
                                       WRK-PARA-PRTKBN1
           END-UNSTRING
      *
           IF      WRK-PARA-SYORIKBN   =   SPACE
               MOVE    "0"             TO  WRK-PARA-SYORIKBN
           END-IF
           IF      WRK-PARA-PRTKBN1    =   SPACE
               MOVE    "0"             TO  WRK-PARA-PRTKBN1
           END-IF
      *
      *    ＳＰＡ設定
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    一時ファイル名取得
           MOVE    LNK-PRTKANRI-TERMID TO  FILE-TERMID1
           MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    FILE-NAME1          TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  FILE-NAME1
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBNYUALL"        TO  JOB-PGID
           MOVE    "入院一括登録処理"
                                       TO  JOB-SHELLMSG
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    3               TO  JOB-STOPFLG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
      *
      *    初期値セット
           PERFORM 110-SYOKI-SET-SEC
      *
           IF      SPA-SRYYMD          <   "20080401"
      *        Ｈ２０．４．１以前はエラー
               MOVE    "診療日はＨ２０年４月１日以降として下さい"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *    入院診療行為の処理中判定
           PERFORM 120-LOCK-SYORI-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期値セット
      *****************************************************************
       110-SYOKI-SET-SEC           SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    処理日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  ERR-SYSYMD
           MOVE    WRK-HENYMDG         TO  PRT-SYSYMD
      *
      *    処理時間
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SYS-THH             TO  WRK-THH
           MOVE    SYS-TMM             TO  WRK-TMM
           MOVE    WRK-HENTIME         TO  ERR-TIME
           MOVE    WRK-HENTIME         TO  PRT-TIME
      *
      *ＳＰＡ設定
      *    診療日
           MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
      *    患者番号構成管理情報編集
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN
                                       TO  SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  SPA-1009-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
           END-IF
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
      *        病床数
               MOVE    SYS-1001-BEDSU      TO  SPA-BEDSU
      *        病床数（一般）
               IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                      (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
                   MOVE    ZERO                TO  SPA-BEDSUIPN
               ELSE
                   MOVE    SYS-1001-BEDSUIPN   TO  SPA-BEDSUIPN
               END-IF
      *        県番号
               MOVE    SYS-1001-PREFNUM    TO  SPA-PREFNUM
      *        有床区分
               MOVE    1                   TO  SPA-BEDKBN
           END-IF
      *
      *    診療日
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-PARA-SRYYMD     TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SRYYMDW
           END-IF
      *    診療日
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    SPA-SRYYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  ERR-SRYYMD
           MOVE    LNK-DAY2-EDTYMD3    TO  PRT-SRYYMD
      *    確認リスト見出し編集
           IF      WRK-PARA-PRTKBN1    =   "1"
               MOVE    CONS-HAED2          TO  PRT-MIDASI
           ELSE
               MOVE    CONS-HAED1          TO  PRT-MIDASI
           END-IF
      *
      *    システム管理検索（チェック機能制御情報）
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                  SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1008-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           MOVE    ZERO                TO  WRK-ZAIJUNKBN-FLG
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1008-REC
      *        剤登録順
               IF      SYS-1008-ZAIJUNKBN-FLG  =   "0"  OR  "1"
                   MOVE    SYS-1008-ZAIJUNKBN-FLG
                                           TO  WRK-ZAIJUNKBN-FLG
               END-IF
           END-IF
      *
           INITIALIZE              SPA-KYOTUKEY
           INITIALIZE              SPA-K02
           INITIALIZE              SPA-SCR-REC
      *
      *****MOVE    ALL "*"             TO  SPA-TERMID
      *v4.8
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
      *
      *    オペレータＩＤ
           MOVE    LNK-PRTKANRI-OPID   TO  SPA-OPID
           INSPECT SPA-OPID            REPLACING  ALL " "  BY  "*"
      *    入外区分
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *    診療日の末日を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYLST-DD
           ELSE
               MOVE    31                  TO  SPA-SRYLST-DD
           END-IF
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為の処理中判定
      *****************************************************************
       120-LOCK-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           MOVE    ZERO                TO  FLG-LOCKERR
           PERFORM UNTIL  (FLG-LOCK        =   1   )  OR
                          (FLG-LOCKERR     =   1   )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   50  )  OR
                                  (FLG-LOCKERR     =   1   )
                   IF     (LOCK-LDNAME (IDX)   =   "ORCA21" ) AND
                          (LOCK-STARTDATE (IDX)    =   SPA-SYSYMD(3:6))
                       MOVE    1               TO  FLG-LOCKERR
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           END-PERFORM
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-LOCKERR         =   1
               MOVE    "オンライン処理中です。処理できません。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       120-LOCK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *
           OPEN    OUTPUT              DATA-FILE
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT   =   1
      *        更新処理中
               IF      WKSRY-MOD-FLG       =   ZERO
                   MOVE    WKSRYACT-REC        TO  DATA-REC
                   WRITE   DATA-REC
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           CLOSE                       DATA-FILE
      *
      *
           OPEN    INPUT               DATA-FILE
           PERFORM 900-DATA-READ-SEC
      *
           PERFORM 2000-INIT-SYORI-SEC
           PERFORM
               UNTIL   FLG-WKSRYACT   =   1
               IF    ( LNK-WKSRYACT-MAX    =   ZERO    )   OR
                     ((WKSRY-PTID          =   LNK-WKSRY-PTID (1))
                 AND  (WKSRY-SRYKA         =   LNK-WKSRY-SRYKA (1))
                 AND  (WKSRY-HKNCOMBI      =   LNK-WKSRY-HKNCOMBI (1))
                 AND  (WKSRY-DOUJITSUKBN   =   LNK-WKSRY-DOUJITSUKBN
                                                                  (1)))
                   CONTINUE
               ELSE
                   MOVE    WKSRYACT-REC        TO  HZN-WKSRYACT-REC
                   IF      FLG-SYORI-ERR       =   ZERO
      *                診療行為作成処理
                       PERFORM 2001-WKSRY-SYORI-SEC
                   ELSE
      *                エラー処理
                       PERFORM 2009-WKSRY-ERRSYORI-SEC
                   END-IF
                   PERFORM 2000-INIT-SYORI-SEC
                   MOVE    HZN-WKSRYACT-REC    TO  WKSRYACT-REC
               END-IF
      *
               ADD     1                   TO  LNK-WKSRYACT-MAX
               MOVE    WKSRYACT-REC        TO  LNK-WKSRYACT-REC
                                                  (LNK-WKSRYACT-MAX)
      *        患者番号チェック
               IF      LNK-WKSRYACT-MAX    =   1
                   PERFORM 2001-PTID-CHKSYORI-SEC
               END-IF
      *        中途終了作成を対象外
               IF      WRK-PARA-SYORIKBN   =   "1"
                   IF      WKSRY-KARTE-FLG     =   1
                       MOVE    1               TO  FLG-SYORI-ERR
                   END-IF
               END-IF
      *        対象データあり
               IF      FLG-SYORI-ERR       =   ZERO
                   ADD     1                   TO  CNT-IN
               END-IF
      *        診療科
               IF      WKSRY-SRYKA         =   ZERO
                                           OR  SPACE
                   MOVE    2               TO  FLG-SYORI-ERR
               END-IF
      *        保険組合せ
               IF      WKSRY-HKNCOMBI      =   ZERO
                   MOVE    3               TO  FLG-SYORI-ERR
               END-IF
      *
               PERFORM 900-DATA-READ-SEC
           END-PERFORM
      *
           CLOSE                       DATA-FILE
      *
           IF      LNK-WKSRYACT-MAX    >   ZERO
               IF      FLG-SYORI-ERR       =   ZERO
      *            診療行為作成処理
                   PERFORM 2001-WKSRY-SYORI-SEC
               ELSE
      *            エラー処理
                   PERFORM 2009-WKSRY-ERRSYORI-SEC
               END-IF
           END-IF
           IF     (CNT-ERR             >   ZERO )
               OR (CNT-OK              >   ZERO )
               PERFORM 2903-END-HENSYU-SEC
           END-IF
      *    エラーリスト処理
           IF      CNT-LINE1           >   ZERO
               PERFORM 2901-HCML55-PRT01-SEC
           END-IF
      *    確認リスト処理
           IF      CNT-LINE2           >   ZERO
               PERFORM 2902-HCML55-PRT02-SEC
           END-IF
      *
           IF      CNT-IN              =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者毎初期処理
      *****************************************************************
       2000-INIT-SYORI-SEC             SECTION.
      *
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    ZERO                TO  SPA-PTID
           MOVE    SPACE               TO  SPA-PTNUM
           MOVE    SPACE               TO  SPA-SRYKA
           MOVE    SPACE               TO  SPA-HKNCOMBINUM
           INITIALIZE                  SPA-K02-AREA
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-SYORI-ERR
           .
       2000-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為エラー処理
      *****************************************************************
       2009-WKSRY-ERRSYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
      *H30.10
           MOVE    SPACE               TO  WRK-ERRMSG2
           EVALUATE     FLG-SYORI-ERR
               WHEN    2
                   MOVE    "診療科が設定されていません"
                                           TO  WRK-ERRMSG
               WHEN    3
                   MOVE    "保険組合せが設定されていません"
                                           TO  WRK-ERRMSG
               WHEN    4
                   EVALUATE    WRK-ERRCD
                       WHEN    "0001"
                           MOVE    "患者ＩＤが登録されていません"
                                               TO  WRK-ERRMSG
                       WHEN    "0002"
                           MOVE    "診療日は入院期間中ではありません。"
                                               TO  WRK-ERRMSG
                   END-EVALUATE
           END-EVALUATE
           IF      WRK-ERRMSG      NOT =   SPACE
               PERFORM 290-ERR-SYORI-SEC
               ADD     1               TO  CNT-ERR
           END-IF
           .
       2009-WKSRY-ERRSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者ＩＤチェック処理
      *****************************************************************
       2001-PTID-CHKSYORI-SEC          SECTION.
      *
           MOVE    LNK-WKSRY-PTID (1)  TO  SPA-PTID
           MOVE    LNK-WKSRY-SRYKA (1) TO  SPA-SRYKA
           MOVE    LNK-WKSRY-HKNCOMBI (1)  TO  SPA-HKNCOMBINUM
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    SPA-PTID            TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0001"              TO  WRK-ERRCD
           ELSE
               MOVE    SPTID-PTNUM         TO  SPA-PTNUM
           END-IF
      *    患者マスター読み込み
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "1003"              TO  WRK-ERRCD
           ELSE
               MOVE    PTINF-NAME          TO  SPA-NAME
               MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-BIRTHDAYW
      *        入金方法
               MOVE    PTINF-NYUKIN-HOHO   TO  SPA-NYUKIN-HOHO
           END-IF
      *
      *    入院期間チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-NYUINRRK-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    4                   TO  FLG-SYORI-ERR
           END-IF
           .
       2001-PTID-CHKSYORI-EXT.
           EXIT.
      *****************************************************************
      *    入院存在チェック処理
      *****************************************************************
       20011-NYUINRRK-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSCNYUHENAREA
           MOVE    "1"                 TO  ORCSCNYUHEN-KBN
           MOVE    SPA-SRYYMD          TO  ORCSCNYUHEN-SRYYMD
      *ver.4.7
      *    同日再入院
           IF      LNK-WKSRY-DOUJITSUKBN(1)    =   "1"
               MOVE   "1"                  TO  ORCSCNYUHEN-KBN2
           END-IF
           CALL    "ORCSCNYUHEN"       USING
                                       SPA-AREA
                                       ORCSCNYUHENAREA
      *    入院中以外はエラー
           IF      ORCSCNYUHEN-NYUIN-KBN   =   ZERO
               MOVE    "0002"              TO  WRK-ERRCD
           ELSE
      *        入院日
               MOVE    ORCSCNYUHEN-NYUINYMD    TO  SPA-K02N-NYUINYMD
      *        退院日
               MOVE    ORCSCNYUHEN-TAIINYMD    TO  SPA-K02N-TAIINYMD
      *        入院期間
               MOVE    ORCSCNYUHEN-NYUIN-DAYG
                                              TO  SPA-K02-NYUIN-AREA
      *ver.4.7
      *        同日再入院有無
               MOVE    ORCSCNYUHEN-DOUNYUIN   TO  SPA-K02N-DOUDAY
      *        診療日付＝同日再入院日
               IF     (SPA-SRYYMD             =   SPA-K02N-NYUINYMD)
                 AND  (ORCSCNYUHEN-DOUNYUIN   =   SPA-SRYYMD(7:2)  )
                   MOVE    "1"                TO  SPA-K02N-DOUKBN
               ELSE
                   MOVE    SPACE              TO  SPA-K02N-DOUKBN
               END-IF
               MOVE    ORCSCNYUHEN-DOUNYUKBN  TO  SPA-K02N-DOUNYUKBN
           END-IF
      *
           .
       410101-NYUINRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為作成処理
      *****************************************************************
       2001-WKSRY-SYORI-SEC                 SECTION.
      *
           INITIALIZE                  TBL-KEIMSG-AREA
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
      *
           IF      LNK-WKSRY-DRCD (1)  NOT =   ZERO
               MOVE    LNK-WKSRY-DRCD (1)  TO  SPA-DRCD
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *
      *    保険組合せ情報編集
           MOVE    LNK-WKSRY-HKNCOMBI (1)  TO  SPA-GMN-HKNCOMBINUM
           PERFORM 20011-HKNCOMBI-HEN-SEC
      *
      *     入院料包括の判定（Ｈ２０．４から）
           INITIALIZE                  SPA-NYUHKT-AREA
           IF     (SPA-HKTNYUIN-FLG    =   1     )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")  AND
                  (SPA-SRYYMD          >=  "20080401" )
      *            治験保険以外
             AND  (SPA-CHIKENKBN   NOT =   "1"   )
               PERFORM 20015-HKTCHK-INIT-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        パラメタ作成
               PERFORM 20012-WKSRYACT-PARA-SEC
      *        中途データ展開・点数計算チェックなど
               PERFORM 20011-INPUTSYORI-SEC
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               PERFORM 3009-ERRSYORI-SEC
           END-IF
      *
           .
       2001-WKSRY-SYORI-EXT.
            EXIT.
      *
      *****************************************************************
      *    保険組合せ情報編集処理
      *****************************************************************
       20011-HKNCOMBI-HEN-SEC                 SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNCOMBI-LISTG
           INITIALIZE                  SPA-HKNCOMI-AREA
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKN-MAX
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )
                           OR (IDX     >   15   )
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDX)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDX)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-NAI-THKNCOMBIMEI (IDX)
      *
               MOVE    ORCSHKN-NAI-HKNID (IDX)
                                       TO  SPA-NAI-HKNID  (IDX)
               MOVE    ORCSHKN-NAI-HKNNUM (IDX)
                                       TO  SPA-NAI-HKNNUM  (IDX)
               MOVE    ORCSHKN-NAI-KOH1HKNNUM (IDX)
                                       TO  SPA-NAI-KOH1HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH2HKNNUM (IDX)
                                       TO  SPA-NAI-KOH2HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH3HKNNUM (IDX)
                                       TO  SPA-NAI-KOH3HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH4HKNNUM (IDX)
                                       TO  SPA-NAI-KOH4HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-TEKSTYMD (IDX)
                                       TO  SPA-NAI-TEKSTYMD (IDX)
               MOVE    ORCSHKN-NAI-TEKEDYMD (IDX)
                                       TO  SPA-NAI-TEKEDYMD (IDX)
      *
               MOVE    ORCSHKN-NAI-FTNRATE (IDX)
                                       TO  SPA-NAI-TFTNRATE (IDX)
               MOVE    ORCSHKN-GMN-FTNRATEMEI (IDX)
                                       TO  SPA-GMN-TFTNRATEMEI (IDX)
      *
      *        四肢区分
               MOVE    ORCSHKN-NAI-RSI-SISIKBN (IDX)
                                           TO  SPA-NAI-RSI-SISIKBN
                                                              (IDX)
      *        傷病年月日
               MOVE    ORCSHKN-NAI-RSI-SHOBYOYMD (IDX)
                                           TO  SPA-NAI-RSI-SHOBYOYMD
                                                               (IDX)
      *        労災区分
               MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX)
                                           TO  SPA-NAI-RSI-HKNKBN
                                                               (IDX)
      *        治験区分
               MOVE    ORCSHKN-NAI-CHIKENKBN  (IDX)
                                           TO  SPA-NAI-CHIKENKBN
                                                               (IDX)
               MOVE    IDX                 TO  SPA-HKN-MAX
           END-PERFORM
           MOVE    SPA-GMN-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
      *
      *    保険組合せ詳細編集
           INITIALIZE                  ORCSC95AREA
           MOVE    "H"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    入院中アフターケア以外はエラーとする
           IF     (SPA-HKN-ERR     NOT =   4  )
               IF     (SPA-HKNNUM          =   "971" ) AND
                      (SPA-RSI-HKNKBN      =   "3" )
                   MOVE    "9029"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    保険組合せエラー
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-RJN-ERR
      *            老人保険編集エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1027"              TO  SPA-ERRCD
      *            老人保険編集エラーがある時、エラーとする
                   WHEN    2
                       MOVE    "1070"              TO  SPA-ERRCD
      *            後期高齢者の保険がない時
                   WHEN    3
                       PERFORM 200111-RJNERR-SEC
      *            退職国保で６５歳以上
                   WHEN    9
                       MOVE    "1077"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *    保険エラーがある時、エラーとする
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-HKN-ERR
                   WHEN    1
                       MOVE    "1028"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "1032"              TO  SPA-ERRCD
      *            保険未選択の場合、エラー
                   WHEN    3
                       MOVE    "1006"              TO  SPA-ERRCD
                   WHEN    4
                       MOVE    "1034"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *    地方公費単独
           MOVE    ZERO                TO  SPA-NAI-KOHFLG-ERR
           MOVE    ZERO                TO  SPA-NAI-KOHFLG
           MOVE    ZERO                TO  SPA-NAI-ZENKIFLG-ERR
           MOVE    ZERO                TO  SPA-NAI-ZENKIFLG
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
           .
       20011-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者の保険エラー処理
      *****************************************************************
       200111-RJNERR-SEC         SECTION.
      *
      *    後期高齢者の保険エラー
           MOVE    "KXXX"              TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  SPA-RJN-ERR
      *    警告エラーとする
           MOVE    2                   TO  FLG-ERR
           STRING  "警告！！"
                   "後期高齢者の年齢です。"
                   "後期高齢者の保険を登録して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG2
           END-STRING
           PERFORM 30010-KEIMSGSYORI-SEC
           .
       200111-RJNERR-EXT.
           EXIT.
      *****************************************************************
      *    入院料包括の判定チェック処理
      *****************************************************************
       20015-HKTCHK-INIT-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *    入院料包括の期間チェック
           INITIALIZE                  ORCSCHKTCHKAREA
           INITIALIZE                  ORCSC00AREA
           MOVE    "5"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02N"              TO  ORCSCHKTCHK-PG
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    ORCSCHKTCHK-NYUHKTCHK   TO  SPA-NYUHKTCHK
           MOVE    ORCSCHKTCHK-NYUHKTCD    TO  SPA-NYUHKTCD
           MOVE    ORCSCHKTCHK-NYUSRYCD    TO  SPA-NYUHKTSRYCD
           MOVE    ORCSCHKTCHK-NYUHKT-G    TO  SPA-NYUHKT-G
      *    入院料算定なし
           IF      ORCSCHKTCHK-NYUERR  =   1
               MOVE    "0217"              TO  SPA-ERRCD
           END-IF
      *    翌月入院料算定なし
           MOVE    ORCSCHKTCHK-NYUERR-2    TO  SPA-NYU-ERR-2
           .
       20015-HKTCHK-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ作成処理
      *****************************************************************
       20012-WKSRYACT-PARA-SEC         SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      ***  OPEN    OUTPUT              PARA-FILE
      *ver.4.8
      *    一時データ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 30011-PARA-DBDELETE-SEC
      *
           MOVE    ZERO                TO  WRK-EDABAN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
      ******   MOVE    SPACE               TO  PARA-REC
      *        MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *        ADD     1                   TO  WRK-EDABAN
      *        MOVE    WRK-EDABAN          TO  PARA-EDABAN
      *        ワーク診療行為マスタからの作成判定とする
      *        MOVE    "WKSRYACT"          TO  LNK-WKSRY-TERMID (IDX)
      *
      *        MOVE    LNK-WKSRYACT-REC(IDX)   TO  PARA-DATA-REC
      ******   WRITE                       PARA-REC
      *
      *        ワーク診療行為マスタからの作成判定とする
               MOVE    "WKSRYACT"          TO  LNK-WKSRY-TERMID (IDX)
               ADD     1                   TO  WRK-EDABAN
      *
               INITIALIZE                      PARA-REC
               MOVE    "WKSRYACT"          TO  PARA-FILEMEI
               MOVE    WRK-EDABAN          TO  PARA-EDANUM
               MOVE    LNK-WKSRYACT-REC(IDX)
                                           TO  PARA-DATA-REC
               PERFORM 30010-PARA-DBINSERT-SEC
           END-PERFORM
      *****CLOSE                       PARA-FILE
           .
       20012-WKSRYACT-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタＤＢ削除処理
      *****************************************************************
       30011-PARA-DBDELETE-SEC          SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       30011-PARA-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタＤＢ出力処理
      *****************************************************************
       30010-PARA-DBINSERT-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
               DISPLAY "ORCABNYUALL PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
           .
       30010-PARA-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面用スパ編集処理
      *****************************************************************
       20011-INPUTSYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  SPA-ERRCD
      *!
           MOVE    SPACE               TO  WRK-ERR-MEI
      *
      *    再表示編集処理
           PERFORM 3103-SAIHEN-HEN-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                    OR     (SPA-ERRCD          NOT =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "."
                       CONTINUE
                   WHEN    "*"
      *                入院期間判定
                       PERFORM 3101-NYUKIKANCHK-SEC
                   WHEN    OTHER
      *H26.1
      *                器材・薬剤の数量＝ゼロはエラー
                       IF     (SPA-COM-INPUTCD (IDX)(1:1)
                                                       =   "7"
                                                       OR  "6" )
                       OR     (SPA-COM-INPUTCD (IDX)(1:3)
                                                       =   "059")
                       OR    ((SPA-COM-INPUTCD(IDX)(1:1)
                                                       =   "1") AND
                              (SPA-COM-KZMCOMPSIKIBETU(IDX)
                                                   NOT =   0  ))
                           IF      SPA-COM-SURYO (IDX)     =   ZERO
      *                        数量＝ゼロはエラー
                               MOVE    "E001"          TO  SPA-ERRCD
                               MOVE    SPA-COM-NAME(IDX)
                                                       TO  WRK-ERR-MEI
                           END-IF
                       END-IF
      *
      *                点数マスタチェック（再表示編集時にエラー）
                       IF     (SPA-COM-CUR (IDX)   NOT =   SPACE )
                        AND   (SPA-ERRCD               =   SPACE )
                           PERFORM 3100-KIHONCHK-SEC
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    診療行為、計計算処理、編集
           PERFORM 510-TBLHEN-SYORI-SEC
      *
      *    入院料包括の判定
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
      *            治験以外
             AND  (SPA-CHIKENKBN   NOT =   "1"   )
               IF     (SPA-HKTSANTEI-FLG   =   1     )  OR
                      (SPA-HKTSANTEI-08    =   1     )  OR
                      (SPA-NYUHKTCHK       =   1     )
                   PERFORM 5109-HKTSANTEI-CHK-SEC
      *H29.1
      *            投薬調剤料の翌月チェック
                   IF     (SPA-KIDCD           =   SPACE )
                      AND (SPA-K02N-TAIINYMD   =   "99999999")
                      AND (SPA-NYU-ERR-2       =   1     )
      *H30.12
                      AND (SPA-ERRCD           =   SPACE )
      *
                       PERFORM 51092-NEXTCHOZAI-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
           AND    (SPA-KIDCD           =   SPACE )
      *        更新前自動算定・更新追加
               PERFORM 530-TBLCHI-SYORI-SEC
      *        更新処理
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 600-KOUSIN-SYORI-SEC
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    2               TO  FLG-ERR
                       PERFORM 30010-KEIMSGSYORI-SEC
                   END-IF
                   IF     (SPA-ERRCD           =   SPACE  )
      *                更新件数
                       ADD     1                   TO  CNT-OK
                       IF     (IDX-KEI             >   ZERO)
                           ADD     1                   TO  CNT-KEI
                       END-IF
      *
      *                処理済患者・警告印刷処理
                       IF     (WRK-PARA-PRTKBN1    =   "0" )
                       OR     (IDX-KEI             >   ZERO)
                           PERFORM 700-PTNUM-PRINT-SEC
                       END-IF
      *                ワーク診療行為削除処理
                       PERFORM 700-WKSRY-DELETE-SEC
      *
                       PERFORM 990-DBCOMMIT-SEC
                   ELSE
                       MOVE    2               TO  FLG-ERR
                       PERFORM 3009-ERRSYORI-SEC
                   END-IF
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
                   PERFORM 3009-ERRSYORI-SEC
      *
                   PERFORM 990-DBCOMMIT-SEC
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               PERFORM 3009-ERRSYORI-SEC
      *
               PERFORM 990-DBCOMMIT-SEC
           END-IF
           .
       310-SPA-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面ＳＰＡ編集処理
      *****************************************************************
       3103-SAIHEN-HEN-SEC                  SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02N"              TO  ORCSCWKSRY-PG
      *    処理区分（１：中途終了展開）
           MOVE    1                   TO  ORCSCWKSRY-SYORIFLG
      *    追加開始位置
           MOVE    IDX2                TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  SPA-ERRCD
           END-IF
      *
           .
       3103-SAIHEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *   点数マスタチェック処理
      *****************************************************************
       3100-KIHONCHK-SEC                  SECTION.
      *
      *    点数マスタ編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    IDX                 TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD(IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)
                                                   TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *    警告は登録対象
           IF      SPA-ERRCD(1:1)      =   "K"
               MOVE    ZERO                TO  FLG-ERR
               PERFORM 30010-KEIMSGSYORI-SEC
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           NOT =   SPACE )
               MOVE    "1"             TO  SPA-COM-CUR (IDX)
           END-IF
           .
       3100-KIHONCHK-EXT.
           EXIT.
      *****************************************************************
      *    入院期間判定処理
      *****************************************************************
       3101-NYUKIKANCHK-SEC         SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN-IDX
           INITIALIZE                  ORCSC97AREA
           MOVE    "K02N"              TO  LNK-SC97-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
      *    入院回数・日数
           INITIALIZE                  SPA-COM-NYUIN-AREA (IDX)
      *    日付チェック
           IF      SPA-K02N-NYUINYMD(1:6)  =   SPA-SRYYMD(1:6)
               MOVE    SPA-K02N-NYUINYMD(7:2)  TO  WRK-STDD
           ELSE
               MOVE    01                      TO  WRK-STDD
           END-IF
           IF      SPA-K02N-TAIINYMD(1:6)  =   SPA-SRYYMD(1:6)
               MOVE    SPA-K02N-TAIINYMD(7:2)  TO  WRK-EDDD
           ELSE
               MOVE    SPA-SRYLST-DD           TO  WRK-EDDD
           END-IF
           MOVE    SPA-SRYYMD (7:2)    TO  WRK-SRYDD
           PERFORM VARYING     IDXN    FROM    1   BY  1
                   UNTIL       IDXN    >   31
              IF      LNK-SC97-NYU-DAY (IDXN)  NOT =   ZERO
                   IF     (IDXN            >=  WRK-STDD )  AND
                          (IDXN            <=  WRK-EDDD )
                       MOVE    1               TO  SPA-COM-NYUINDAY
                                                       (IDX IDXN)
                       ADD     1               TO  SPA-COM-NYU-KAI
                                                       (IDX)
                   ELSE
                       MOVE    "9007"          TO  SPA-ERRCD
                   END-IF
              END-IF
           END-PERFORM
      *
      *    保険組合せ期間との日付チェック
           IF      SPA-COB-TEKSTYMD(1:6)   =   SPA-SRYYMD(1:6)
               MOVE    SPA-COB-TEKSTYMD(7:2)   TO  WRK-STDD
           ELSE
               MOVE    01                      TO  WRK-STDD
           END-IF
           IF      SPA-COB-TEKEDYMD(1:6)   =   SPA-SRYYMD(1:6)
               MOVE    SPA-COB-TEKEDYMD(7:2)   TO  WRK-EDDD
           ELSE
               MOVE    SPA-SRYLST-DD           TO  WRK-EDDD
           END-IF
           MOVE    SPA-SRYYMD (7:2)    TO  WRK-SRYDD
           PERFORM VARYING     IDXN    FROM    1   BY  1
                   UNTIL      (IDXN    >   31  )
                          OR  (SPA-ERRCD   NOT =   SPACE )
              IF      SPA-COM-NYUINDAY (IDX IDXN)   NOT =   ZERO
                   IF     (IDXN            >=  WRK-STDD )  AND
                          (IDXN            <=  WRK-EDDD )
                       CONTINUE
                   ELSE
                       MOVE    "9038"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  SPA-ERRMSG2
                       STRING  WRK-STDD
                               "日〜"
                               WRK-EDDD
                              "日が当月の有効期間です。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG2
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
           .
       3101-NYUKIKANCHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SYORI-SEC            SECTION.
      *
      *    腫瘍マーカー一覧展開しないとする
           MOVE    ZERO                TO  SPA-AKUSEIHYOFLG
      *H30.9
      *    選択式コメント一覧展開しないとする
           MOVE    ZERO                TO  SPA-RECEKISAIFLG
      *    診療行為、計計算処理、編集
           MOVE    ZERO                TO  FLG-END-SYORI
           MOVE    SPACE               TO  SPA-ERRMSG2
           PERFORM
               UNTIL   (FLG-END-SYORI  =   1  )
               OR      (SPA-ERRCD      NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-KIDCD
               MOVE    SPACE               TO  SPA-ERRMSG2
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "1"                 TO  ORCSC100-KBN
               MOVE    SPACE               TO  ORCSC100-TOROKU
               MOVE    ZERO                TO  ORCSC100-INIT
               MOVE    ZERO                TO  ORCSC100-KENSADEL
               MOVE    ZERO                TO  ORCSC100-RIHTEI
               CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *        警告は登録対象
               IF      SPA-ERRCD(1:1)      =   "K"
                   MOVE    ZERO                TO  FLG-ERR
                   PERFORM 30010-KEIMSGSYORI-SEC
                   MOVE    ZERO            TO  FLG-END-SYORI
               ELSE
                   MOVE    1               TO  FLG-END-SYORI
               END-IF
           END-PERFORM
           .
       510-TBLHEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為自動算定・算定チェック処理
      *****************************************************************
       530-TBLCHI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-END-SYORI2
           PERFORM
               UNTIL    FLG-END-SYORI2 =   1
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "2"                 TO  ORCSC100-KBN
               MOVE    "1"                 TO  ORCSC100-TOROKU
               MOVE    ZERO                TO  ORCSC100-INIT
               MOVE    ZERO                TO  ORCSC100-KENSADEL
               MOVE    ZERO                TO  ORCSC100-RIHTEI
               CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *        警告は登録対象
               IF      SPA-ERRCD(1:1)      =   "K"
                   MOVE    ZERO                TO  FLG-ERR
                   PERFORM 30010-KEIMSGSYORI-SEC
                   MOVE    ZERO                TO  FLG-END-SYORI2
               ELSE
                   MOVE    1                   TO  FLG-END-SYORI2
               END-IF
           END-PERFORM
      *
           .
       530-TBLCHI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    包括診療チェック処理
      *****************************************************************
       5109-HKTSANTEI-CHK-SEC              SECTION.
      *
      *H30.12
           MOVE    ZERO                TO  SPA-HKTKEI-FLG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
           MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPA-NYUHKTSRYCD     TO  ORCSCHKTCHK-NYUSRYCD
           MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
           MOVE    "6"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02N"              TO  ORCSCHKTCHK-PG
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      ORCSCHKTCHK-HKTFLG  =   1
               MOVE    1               TO  SPA-HKTSANTEI-CHK
           ELSE
               MOVE    ZERO            TO  SPA-HKTSANTEI-CHK
           END-IF
           IF      ORCSCHKTCHK-HKTERR  =   1
      *        包括をエラーとする削除処理
               PERFORM 51091-HKTSANTEI-DEL-SEC
           END-IF
           .
       5109-HKTSANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療エラー削除処理
      *****************************************************************
       51091-HKTSANTEI-DEL-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
           MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPA-NYUHKTSRYCD     TO  ORCSCHKTCHK-NYUSRYCD
           MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
           MOVE    "3"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02"               TO  ORCSCHKTCHK-PG
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    診療行為、計計算処理、編集
           PERFORM 510-TBLHEN-SYORI-SEC
           .
       51091-HKTSANTEI-DEL-EXT.
           EXIT.
      *
      *H29.1
      *****************************************************************
      *    投薬調剤料の翌月チェック処理
      *****************************************************************
       51092-NEXTCHOZAI-CHK-SEC             SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )
                          OR ((SPA-GMN-INPUTCD(IDX) =   SPACE) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE))
                          OR  (SPA-ERRCD        NOT =   SPACE )
      *
      *        薬評・減点・持参薬対象外
               IF     (SPA-COM-SRYKBN   (IDX)  =   "21"  )
                AND   (SPA-COM-YAKUHYO  (IDX)  =   SPACE  )
                AND   (SPA-COM-GENTEN   (IDX)  =   SPACE  )
                AND   (SPA-COM-JISANYAKU(IDX)  =   SPACE  )
                AND   (SPA-COM-HOUKATU-ZAI(IDX)    =   1  )
                AND   (SPA-COM-SRYSYUKBN (IDX) NOT =   "214" )
                   IF      SPA-COM-ZAIEND (IDX)    =   "1"
                       PERFORM 51921-DAYCHEK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    ZERO                TO  FLG-ERR
               PERFORM 30010-KEIMSGSYORI-SEC
           END-IF
           .
       51092-NEXTCHOZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終行に回数の入力があるかのチェック処理
      *****************************************************************
       51921-DAYCHEK-SEC             SECTION.
      *
           MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *
           MOVE    ZERO                TO  FLG-NEXTTUKI
      *    投薬で入院回数の時、回数集計
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   31  )
                           OR (FLG-NEXTTUKI    =   1 )
               IF      SPA-COM-NYUINDAY (IDX IDN1) >   ZERO
                   MOVE    IDN1                TO  IDN4
                   PERFORM VARYING     IDN3    FROM    1   BY  1
                           UNTIL      (IDN3    >   WRK-KAISU )
                                   OR (FLG-NEXTTUKI    =   1 )
      *                翌月あり
                       IF      IDN4            >   SPA-SRYLST-DD
                           MOVE    1               TO  FLG-NEXTTUKI
                       END-IF
                       ADD     1                   TO  IDN4
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF      FLG-NEXTTUKI        =   1
               MOVE    "K190"              TO  SPA-ERRCD
           END-IF
           .
       51921-DAYCHEK-EXT.
           EXIT.
      *H29.1
      *****************************************************************
      *    更新処理処理
      *****************************************************************
       600-KOUSIN-SYORI-SEC             SECTION.
      *
      *    診療区分順登録の時、並び替えを行う
           IF      WRK-ZAIJUNKBN-FLG   NOT =   "1"
               PERFORM 6001-ZAINUMSORT-SEC
           END-IF
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *    入院更新処理
           INITIALIZE                  ORCSCNYUINAREA
           MOVE    "1"                 TO  ORCSCNYUIN-KBN
      *    退院日
           MOVE    SPA-K02N-TAIINYMD   TO  ORCSCNYUIN-TAIINYMD
           CALL    "ORCSCNYUIN"        USING
                                       SPA-AREA
                                       ORCSCNYUINAREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF     (SPA-ERRCD       NOT =   SPACE  )  AND
                  (SPA-ERRCD(1:1)  NOT =   "K"    )
               PERFORM 990-DBCOMMIT-SEC
           END-IF
           IF      SPA-ERRMSG2     NOT =   SPACE
               MOVE    "K001"              TO  SPA-ERRCD
           END-IF
           .
       600-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為並び替処理
      *****************************************************************
       6001-ZAINUMSORT-SEC             SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-PARA 
      *****END-IF
           INITIALIZE                      SRT-WKSRYACT-AREA
           INITIALIZE                      SRT2-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
                   UNTIL      (FLG-PARA    =    1   )  OR
                              (IDX         >=   600 )
      *        ワーク診療行
      ****     IF      PARA-FILEMEI    =   "WKSRYACT"
                   ADD     1                    TO  IDX
                   MOVE    PARA-DATA-REC        TO  SRT-WKSRYACT-REC
                                                                (IDX)
                   IF      SRT-WKSRY-HKNCOMBI(IDX) =   ZERO
                       MOVE    SPA-HKNCOMBINUM TO
                                               SRT-WKSRY-HKNCOMBI(IDX)
                   END-IF
                   IF      SRT-WKSRY-SRYKA   (IDX) =   SPACE
                       MOVE    SPA-SRYKA       TO
                                               SRT-WKSRY-SRYKA   (IDX)
                   END-IF
      *****    END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  WRK-MAX
      **** CLOSE                       PARA-FILE
      *
      *    ＳＯＲＴ（診療区分別にＳＯＲＴする）
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX4
           MOVE    SPACE               TO  SRT2-WKSRYACT-AREA
           INITIALIZE                  SRT2-WKSRYACT-AREA
           MOVE    ZERO                TO  SRT2-WKSRYACT-MAX
      *
           PERFORM VARYING   IDX       FROM   1   BY  1
                   UNTIL     IDX       >   WRK-MAX
               MOVE    ALL "9"             TO  WRK-ZAINUM-X
               MOVE    ALL "9"             TO  WRK-RENNUM-X
               MOVE    ALL "9"             TO  WRK-SRYKBN
               MOVE    ZERO                TO  IDX2
               MOVE   ZERO                 TO  FLG-CHK
               PERFORM VARYING   IDX1      FROM   1   BY  1
                       UNTIL     IDX1      >   WRK-MAX
                  IF      SRT-WKSRY-SRYKBN (IDX1) NOT =   SPACE
                       MOVE   ZERO                 TO  FLG-CHK
                       IF     SRT-WKSRY-SRYKBN (IDX1)
                                                       <   WRK-SRYKBN
                           MOVE    1                   TO  FLG-CHK
                       END-IF
                           IF      SRT-WKSRY-SRYKBN (IDX1)
                                                   =   WRK-SRYKBN
                               IF      SRT-WKSRY-ZAINUM (IDX1)
                                                   <    WRK-ZAINUM
                                   MOVE    1                TO  FLG-CHK
                               END-IF
                               IF     (SRT-WKSRY-ZAINUM (IDX1)
                                                   =   WRK-ZAINUM) AND
                                      (SRT-WKSRY-RENNUM (IDX1)
                                                   <=   WRK-RENNUM)
                                   MOVE    1                TO  FLG-CHK
                               END-IF
                           END-IF
                           IF      FLG-CHK         =   1
                               MOVE    SRT-WKSRY-SRYKBN (IDX1)
                                                   TO  WRK-SRYKBN
                               MOVE    SRT-WKSRY-ZAINUM (IDX1)
                                                   TO  WRK-ZAINUM
                               MOVE    SRT-WKSRY-RENNUM (IDX1)
                                                   TO  WRK-RENNUM
                               MOVE    IDX1            TO  IDX2
                           END-IF
                   END-IF
               END-PERFORM
               IF     IDX2                 >   ZERO
                   ADD     1              TO  IDX4
                   MOVE    SRT-WKSRYACT-REC(IDX2)
                                          TO  SRT2-WKSRYACT-REC (IDX4)
                   ADD     1              TO  SRT2-WKSRYACT-MAX
                   MOVE    SPACE          TO  SRT-WKSRYACT-REC(IDX2)
                   INITIALIZE                 SRT-WKSRYACT-REC(IDX2)
               END-IF
           END-PERFORM
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    OPEN    OUTPUT              PARA-FILE
      *ver.4.8
      *    一時データ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 30011-PARA-DBDELETE-SEC
      *
           MOVE    ZERO                TO  WRK-EDABAN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SRT2-WKSRYACT-MAX
      ***      MOVE    SPACE               TO  PARA-REC
      *        MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *        ADD     1                   TO  WRK-EDABAN
      *        MOVE    WRK-EDABAN          TO  PARA-EDABAN
      *        MOVE    SRT2-WKSRYACT-REC(IDX)   TO  PARA-DATA-REC
      ****     WRITE                       PARA-REC
      *
               ADD     1                   TO  WRK-EDABAN
               INITIALIZE                      PARA-REC
               MOVE    "WKSRYACT"          TO  PARA-FILEMEI
               MOVE    WRK-EDABAN          TO  PARA-EDANUM
               MOVE    SRT2-WKSRYACT-REC(IDX) 
                                           TO  PARA-DATA-REC
               PERFORM 30010-PARA-DBINSERT-SEC
           END-PERFORM
      *****CLOSE                       PARA-FILE
      *
           .
       6001-ZAINUMSORT-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為削除処理
      *****************************************************************
       700-WKSRY-DELETE-SEC             SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *ver.4.7
           IF      SPA-K02N-DOUKBN     =   "1"
               MOVE    "1"                 TO  WKSRY-DOUJITSUKBN
           END-IF
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       700-WKSRY-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    警告メッセージ処理
      *****************************************************************
       30010-KEIMSGSYORI-SEC          SECTION.
      *
           IF      FLG-ERR             =   2
               MOVE    SPA-ERRMSG2         TO  SPA-ERRMSG
               MOVE    SPACE               TO  SPA-ERRMSG2
           ELSE
               MOVE    "K02"               TO  LNK-SC96-PG
               CALL    "ORCSC96"           USING    SPA-AREA
                                                    ORCSC96AREA
           END-IF
           ADD     1                   TO  IDX-KEI
           IF      IDX-KEI             <=  50
               MOVE    SPA-ERRCD           TO  TBL-KEIERRCD (IDX-KEI)
               IF      SPA-ERRMSG2     NOT =   SPACE
                   MOVE    SPA-ERRMSG2         TO  TBL-KEIMSG (IDX-KEI)
                   ADD     1                   TO  IDX-KEI
               END-IF
               IF      IDX-KEI             <=  50
                   MOVE    SPA-ERRMSG          TO  TBL-KEIMSG (IDX-KEI)
               END-IF
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  SPA-ERRMSG2
           .
       30010-KEIMSGSYORI-EXIT.
           EXIT.
      *****************************************************************
      *    診療行為エラー処理
      *****************************************************************
       3009-ERRSYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG2
           IF     (SPA-ERRCD           =   SPACE )
           AND    (SPA-KIDCD       NOT =   SPACE )
      *        確認メッセージ処理
               PERFORM 30091-KIDCD-SYORI-SEC
           ELSE
               IF     (FLG-ERR             =   2     )
               OR     (SPA-ERRCD(1:1)      =   "E"   )
                   PERFORM 30092-KOUSIN-ERR-SEC
               ELSE
                   MOVE    "K02"               TO  LNK-SC96-PG
                   CALL    "ORCSC96"           USING    SPA-AREA
                                                        ORCSC96AREA
                   MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG2
               END-IF
           END-IF
      *
           PERFORM 290-ERR-SYORI-SEC
           ADD     1                   TO  CNT-ERR
           .
       3009-ERRSYORI-EXT.
           EXIT.
      *****************************************************************
      *    更新エラーメッセージ処理
      *****************************************************************
       30092-KOUSIN-ERR-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0005"
                   STRING  "【ＤＢ更新がエラーになりました。"
                           "ＳＥに連絡して下さい。】"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   STRING  "１日の受診履歴が１３５剤以上になります。"
                           "剤を減らして入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H26.1
               WHEN    "E001"
                   STRING  WRK-ERR-MEI1        DELIMITED  BY  SPACE
                           "　に数量設定がありません"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD         TO  SPA-ERRMSG
           END-EVALUATE
           MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
           MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG2
           .
       30092-KOUSIN-ERR-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ処理
      *****************************************************************
       30091-KIDCD-SYORI-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0180"
                   STRING  "個別療法が１０単位を超えます。"
                           "逓減しますか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KIDCD       TO  WRK-ERRMSG
           END-EVALUATE
           .
       30091-KIDCD-SYORI-EXIT.
           EXIT.
      *****************************************************************
      *    エラー印刷処理
      *****************************************************************
       290-ERR-SYORI-SEC             SECTION.
      *
           IF      WRK-ERRMSG2     NOT =   SPACE
               MOVE    49              TO  LINE-MAX
           ELSE
               MOVE    50              TO  LINE-MAX
           END-IF
      *
           ADD     1                   TO  CNT-LINE1
           IF      CNT-LINE1           >   LINE-MAX
               PERFORM 2901-HCML55-PRT01-SEC
               MOVE    1               TO  CNT-LINE1
           END-IF
           MOVE    SPA-PTNUM           TO  ERR-PTNUM (CNT-LINE1)
           MOVE    SPA-NAME            TO  ERR-NAME  (CNT-LINE1)
           INSPECT ERR-NAME  (CNT-LINE1)
                                       REPLACING  ALL "  "  BY  "　"
           IF    ( SPA-ENCODING    =   2 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"             TO  KANACHK2-SYORI
               MOVE    ERR-NAME  (CNT-LINE1)
                                       TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"  USING
                                       ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT
                                       TO  ERR-NAME  (CNT-LINE1)
           END-IF
           MOVE    SPA-SRYKA           TO  ERR-SRYKA (CNT-LINE1)
           MOVE    SPA-HKNCOMBINUM     TO  ERR-HKNCOMBI (CNT-LINE1)
           IF      WRK-ERRMSG2     NOT =   SPACE
               MOVE    WRK-ERRMSG2     TO  ERR-ERRMSG (CNT-LINE1)
               ADD     1               TO  CNT-LINE1
               INSPECT ERR-NAME  (CNT-LINE1)
                                       REPLACING  ALL "  "  BY  "　"
           END-IF
           MOVE    WRK-ERRMSG          TO  ERR-ERRMSG (CNT-LINE1)
           .
       290-ERR-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    エラーリスト印刷処理
      *****************************************************************
       2901-HCML55-PRT01-SEC          SECTION.
      *
           MOVE    SPACE               TO  HCMSL55
           ADD     1                   TO  CNT-PAGE1
           MOVE    CNT-PAGE1           TO  ERR-PAGE
      *
           MOVE    LST-ERR-HEAD1       TO  HCMSL55-MOJI (1)
           MOVE    LST-ERR-HEAD2       TO  HCMSL55-MOJI (3)
      *
           MOVE    4                   TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   50 )    OR
                              (IDX     >=  55 )
               ADD     1                   TO  IDX
               MOVE    LST-ERR-BODY (IDY)
                                       TO  HCMSL55-MOJI (IDX)
           END-PERFORM
           PERFORM 2901-PRINT01-SEC
      *
           MOVE    ZERO                    TO  CNT-LINE1
      *
           INITIALIZE                      LST-ERR-BODY-G
           .
       2901-HCML55-PRT01-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2901-PRINT01-SEC          SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"            TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                    TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                    TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                    TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                    TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                    TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                    TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"    TO  SPRT-PRTID
           MOVE    "入院診療行為一括エラーリスト"
                                    TO  SPRT-TITLE
           MOVE    HCMSL55          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2901-PRINT01-EXIT.
           EXIT.
      *****************************************************************
      *    登録確認・警告メッセージ処理
      *****************************************************************
       700-PTNUM-PRINT-SEC          SECTION.
      *
           ADD     1                   TO  CNT-LINE2
           IF      CNT-LINE2           >   50
               PERFORM 2902-HCML55-PRT02-SEC
               MOVE    1               TO  CNT-LINE2
           END-IF
           MOVE    SPA-PTNUM           TO  PRT-PTNUM (CNT-LINE2)
           MOVE    SPA-NAME            TO  PRT-NAME  (CNT-LINE2)
           INSPECT PRT-NAME  (CNT-LINE2)
                                       REPLACING  ALL "  "  BY  "　"
           IF    ( SPA-ENCODING    =   2 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"             TO  KANACHK2-SYORI
               MOVE    PRT-NAME  (CNT-LINE2)
                                       TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"  USING
                                       ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT
                                       TO  PRT-NAME  (CNT-LINE2)
           END-IF
           MOVE    SPA-SRYKA           TO  PRT-SRYKA (CNT-LINE2)
           MOVE    SPA-HKNCOMBINUM     TO  PRT-HKNCOMBI (CNT-LINE2)
           PERFORM VARYING IDX-KEI     FROM    1   BY  1
                   UNTIL  (IDX-KEI     >   50   )  OR
                          (TBL-KEIMSG (IDX-KEI) =   SPACE )
               IF      IDX-KEI             >   1
                   ADD     1               TO  CNT-LINE2
                   INSPECT PRT-NAME  (CNT-LINE2)
                                           REPLACING  ALL "  "  BY  "　"
                   IF      CNT-LINE2           >   50
                       PERFORM 2902-HCML55-PRT02-SEC
                       MOVE    1               TO  CNT-LINE2
                   END-IF
               END-IF
               MOVE    TBL-KEIMSG (IDX-KEI)
                                       TO  PRT-MSG (CNT-LINE2)
           END-PERFORM
      *
           .
       700-PTNUM-PRINT-EXIT.
           EXIT.
      *****************************************************************
      *    処理件数編集印刷処理
      *****************************************************************
       2903-END-HENSYU-SEC          SECTION.
      *
      *    エラー件数
           ADD     3                   TO  CNT-LINE1
           IF      CNT-LINE2           >   50
               PERFORM 2901-HCML55-PRT01-SEC
               MOVE    1               TO  CNT-LINE1
           END-IF
           MOVE    CNT-ERR             TO  ERR-CNT
           MOVE    LST-ERR-TAIL1       TO  LST-ERR-BODY (CNT-LINE1)
      *
      *    確認件数
           ADD     3                   TO  CNT-LINE2
           IF      CNT-LINE2           >   50
               PERFORM 2902-HCML55-PRT02-SEC
               MOVE    1               TO  CNT-LINE2
           END-IF
           MOVE    CNT-OK              TO  PRT-OKCNT
           MOVE    CNT-KEI             TO  PRT-KEICNT
           MOVE    LST-PRT-TAIL1       TO  LST-PRT-BODY (CNT-LINE2)
      *
           .
       2903-END-HENSYU-EXIT.
           EXIT.
      *****************************************************************
      *    確認リスト印刷処理
      *****************************************************************
       2902-HCML55-PRT02-SEC          SECTION.
      *
           MOVE    SPACE               TO  HCMSL55
           ADD     1                   TO  CNT-PAGE2
           MOVE    CNT-PAGE2           TO  PRT-PAGE
      *
           MOVE    LST-PRT-HEAD1       TO  HCMSL55-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL55-MOJI (3)
      *
           MOVE    4                  TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   50 )    OR
                              (IDX     >=  55 )
               ADD     1                   TO  IDX
               MOVE    LST-PRT-BODY (IDY)
                                       TO  HCMSL55-MOJI (IDX)
           END-PERFORM
           PERFORM 2902-PRINT02-SEC
      *
           MOVE    ZERO                    TO  CNT-LINE2
      *
           INITIALIZE                      LST-PRT-BODY-G
           .
       2902-HCML55-PRT02-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2902-PRINT02-SEC          SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"            TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                    TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                    TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                    TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                    TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                    TO  SPRT-SHELLID
      *****MOVE    LNK-PRTKANRI-SHORI-RENNUM
           MOVE    2
                                    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                    TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"    TO  SPRT-PRTID
           MOVE    "入院診療行為一括確認リスト"
                                    TO  SPRT-TITLE
           MOVE    HCMSL55          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2902-PRINT02-EXIT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 3001-PARA-DEL-SEC
      *
           DISPLAY "*** ORCBNYUALL OK :"  CNT-OK   " ***"
           DISPLAY "*** ORCBNYUALL ERR:" CNT-ERR  " ***"
           DISPLAY "*** ORCBNYUALL END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-OK          TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       3001-PARA-DEL-SEC               SECTION.
      *
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    FILE-NAME1          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
      *    一時データ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 30011-PARA-DBDELETE-SEC
      *
           .
       3001-PARA-DEL-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
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
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ読込み処理
      *****************************************************************
       940-WKSRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    排他制御読込み処理
      *****************************************************************
       910-LOCK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TBL-LOCK
               MOVE    ZERO            TO  FLG-LOCK
           ELSE
               MOVE    1               TO  FLG-LOCK
           END-IF
      *
           .
       910-LOCK-READ-EXT.
           EXIT.
      *****************************************************************
      *    データ順読み
      *****************************************************************
       900-DATA-READ-SEC         SECTION.
      *
           READ    DATA-FILE 
               AT  END
                   MOVE    1           TO  FLG-WKSRYACT
               NOT AT  END
                   MOVE    ZERO        TO  FLG-WKSRYACT
                   MOVE    DATA-REC    TO  WKSRYACT-REC
           END-READ
      *
           .
       900-DATA-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      *    EXIT.
      *v.4.8
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　更新処理
      *****************************************************************
       990-DBCOMMIT-SEC                SECTION.
      *
      *    ＤＢ　クローズ処理
           PERFORM 900-DBDISCONNECT-SEC
      *    ＤＢ　オープン処理
           PERFORM 100-DBOPEN-SEC
           .
       990-DBCOMMIT-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
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
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
