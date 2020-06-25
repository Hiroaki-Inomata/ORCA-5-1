      ******************************************************************
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
      *****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000TA03.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 未収金一覧表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付       内容
      *   01.00.00    門間    11/03/29   新規作成
      *   01.00.01    門間    13/07/17   保険欄（CSV）の不具合を修正
      *   01.00.02    門間    13/07/24   CSVファイルが20kBを超える場合、
      *                                  正しく出力されない不具合を修正
      *                       13/07/30   CSV出力選択画面での入外印字を修正
      *   01.00.03    門間    13/08/02   未収金欄（CSV）マイナス処理時の修正
      *   01.00.04    門間    14/03/12   テスト患者を対象外とする
      *   02.00.01    門間    14/10/31   ver4.8.0対応（一時ディレクトリ変更）
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    未収金対象患者一時ファイル
           SELECT  TA03-FILE   ASSIGN          TA03P
                                     ORGANIZATION    IS  INDEXED
                                     ACCESS  MODE    IS  DYNAMIC
                                     RECORD  KEY     IS  TA03TMP-KEY
                                     FILE    STATUS  IS  STS-TA03TMP.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    未収金対象患者一時ファイル
       FD  TA03-FILE.
           COPY    "A00000TA03TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //TA03P//.
      *
           COPY    "A00000TA03.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-RECEERR             PIC X(02).
           03  STS-TA03TMP             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PRINT               PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-INVALID             PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-SYUMEI2             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(02).
           03  CNT-GPAGE               PIC 9(02).
           03  CNT-RENNUM              PIC 9(06).
      *
           03  CNT-CSV                 PIC 9(06).
           03  CNT-LEN                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
           03  IDX2                    PIC 9(03).
           03  IDX3                    PIC 9(03).
           03  IDX-MAX                 PIC 9(03).
      *
      *    CSV用
           03  IDX1                    PIC 9(04).
           03  IDX4                    PIC 9(04).
           03  IDX-ST                  PIC 9(03).
           03  IDX-ED                  PIC 9(03).
           03  IDXP                    PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-STYMD          PIC X(08).
           03  WRK-PARA-EDYMD          PIC X(08).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-NYUKINKBN      PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-Z2                  PIC Z9.
           03  WRK-Z6                  PIC ZZZZZ9.
           03  WRK-Z7                  PIC ZZZZZZ9.
           03  WRK-Z9                  PIC Z,ZZZ,ZZZ.
           03  WRK-Z92                 PIC ZZZZZZZZZ.
           03  WRK-Z10                 PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z102                PIC ZZZZZZZZZZ.
           03  WRK-M9                  PIC -,---,--9.
           03  WRK-M92                 PIC --------9.
           03  WRK-M10                 PIC --,---,--9.
           03  WRK-M102                PIC ---------9.
           03  WRK-AMARI               PIC 9(02).
           03  WRK-SYMD                PIC 9(08).
           03  WRK-DATA                PIC 9(04).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-TENTNK              PIC 9(03)V9(02).
           03  WRK-KINGAKU             PIC S9(09).
           03  WRK-DISCOUNT-MONEY      PIC 9(09).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO             PIC X(01).
               05  WRK-HYY             PIC X(02).
               05  FILLER              PIC X(01)   VALUE  ".".
               05  WRK-HMM             PIC X(02).
               05  FILLER              PIC X(01)   VALUE  ".".
               05  WRK-HDD             PIC X(02).
      *
      *    診療科領域
       01  WRK-SRYKA-AREA.
           03  WRK-SRYKA-TBL         OCCURS 30.
             05  WRK-SRYKA             PIC 9(05).
             05  WRK-SRYKA-NAME        PIC X(20).
      *
      *    集計領域
       01  WRK-SYUKEI-AREA.
           03  WRK-GROUP-TBL         OCCURS 3.
             05  WRK-MEISAI-TBL      OCCURS 500.
               07  WRK-RENNUM          PIC 9(06).
               07  WRK-PTNUM           PIC 9(10).
               07  WRK-NAME            PIC X(20).
               07  WRK-HKN             PIC X(04).
               07  WRK-SEIKYUMD        PIC 9(05).
               07  WRK-DENPNUM         PIC 9(07).
               07  WRK-SRYKA-BR        PIC 9(08).
               07  WRK-HKN-TOTAL       PIC 9(09).
               07  WRK-KANJAFTN        PIC 9(09).
               07  WRK-JIHI            PIC 9(09).
               07  WRK-GENMEN          PIC 9(09).
               07  WRK-TAX             PIC 9(09).
               07  WRK-MISYU           PIC S9(09).
               07  WRK-NYUKIN          PIC 9(09).
               07  WRK-NYUKINMD        PIC 9(09).
      *
      *    合計領域
       01  WRK-GOKEI-AREA.
           03  WRK-GKENSU              PIC 9(06).
           03  WRK-GHKN-TOTAL          PIC 9(10).
           03  WRK-GKANJAFTN           PIC 9(10).
           03  WRK-GJIHI               PIC 9(10).
           03  WRK-GGENMEN             PIC 9(10).
           03  WRK-GTAX                PIC 9(10).
           03  WRK-GMISYU              PIC S9(10).
           03  WRK-GNYUKIN             PIC 9(10).
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
               07  CSV-HEAD-RENNUM        PIC X(50) VALUE "連番".
               07  CSV-HEAD-PTNUM         PIC X(50) VALUE "患者番号".
               07  CSV-HEAD-NAME          PIC X(50) VALUE "氏名".
               07  CSV-HEAD-HKNNUM        PIC X(50) VALUE "保険".
               07  CSV-HEAD-SEIKYUYMD     PIC X(50) VALUE "請求年月日".
               07  CSV-HEAD-DENPNUM       PIC X(50) VALUE "伝票Ｎｏ".
               07  CSV-HEAD-BYOTONUM      PIC X(50) VALUE "病棟ー病室".
               07  CSV-HEAD-SRYKA         PIC X(50) VALUE "診療科".
               07  CSV-HEAD-HKNKEI        PIC X(50) VALUE "保険計".
               07  CSV-HEAD-KANJAFTN      PIC X(50) VALUE "患者負担".
               07  CSV-HEAD-JIHI          PIC X(50) VALUE "自費".
               07  CSV-HEAD-MISYUKIN      PIC X(50) VALUE "未収金".
               07  CSV-HEAD-NYUKINGAKU    PIC X(50) VALUE "入金額".
               07  CSV-HEAD-NYUKINYMD     PIC X(50) VALUE "入金月日".
               07  CSV-HEAD-LASTYMD       PIC X(50) VALUE "最終受診日".
               07  CSV-HEAD-TEL           PIC X(50) VALUE "電話番号".
             05  CSV-HEAD-R               REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM          PIC X(50) OCCURS 16.
             05  CSV-HEAD-MAX             PIC 9(05) VALUE 16.
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
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    医師情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納明細マスタ2
       01  SYUMEI2-REC.
           COPY    "CPSYUMEI.INC"    REPLACING  //SMEI//
                                     BY         //SMEI2//.
      *
      *    患者番号マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者労災マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
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
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
      *
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE            SECTION.
       01  COMMAND-PARAM.
           02  FILLER            PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC            SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           IF  FLG-END  NOT =  1
             PERFORM  300-MAIN-SEC
           END-IF
      *
           PERFORM  600-END-SEC
      *
           STOP  RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC            SECTION.
      *
           INITIALIZE                    FLG-AREA
                                         STS-AREA
                                         WRK-AREA
                                         CNT-AREA
                                         SPA-AREA
                                         IDX-AREA
                                         WRK-SYUKEI-AREA
                                         WRK-GOKEI-AREA
           INITIALIZE                    RECEERR
      *
           PERFORM  100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM      DELIMITED  BY  ","
                                         INTO  LNK-PRTKANRI-RENNUM
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
                                               WRK-PARA-STYMD
                                               WRK-PARA-EDYMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-NYUKINKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM          TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"                     TO  SJOBKANRI-MODE
           INITIALIZE                        JOBKANRI-REC
           MOVE    SPA-HOSPNUM               TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID            TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID          TO  JOB-SHELLID
           MOVE    "A00000TA03"              TO  JOB-PGID
           MOVE    "未収金一覧表（月次）"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"                 USING
                                             ORCSJOBKANRIAREA
                                             JOBKANRI-REC
                                             SPA-AREA
      *+
      *    サブプログラムより一時ファイル名編集
           MOVE    "ATA03TP"                TO  TA03P-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  TA03P-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  TA03P-HOSPNUM
      *
           MOVE    TA03P-BASENAME           TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  TA03P-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
           MOVE    SPACE               TO  WRK-ITEM
           INSPECT WRK-ITEM  TALLYING  WRK-ITEM-SIZE   FOR ALL " "
      *
      *    パラメタ編集処理
           PERFORM  200-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       200-PARA-HENSYU-SEC            SECTION.
      *
           MOVE    ZERO                  TO  FLG-END
      *
      *    医療機関ＩＤ編集
           INITIALIZE                        SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE    WRK-PARA-EDYMD        TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-STYMD        TO  SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM   900-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    入外区分チェック
           IF  WRK-PARA-NYUGAIKBN  NOT =  "1"
               AND  WRK-PARA-NYUGAIKBN  NOT =  "2"
             MOVE    "入外区分が間違っています"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    集計名称取得
           MOVE    1           TO  IDX3
      *    診療科
           INITIALIZE                        SYS-1005-REC
           MOVE    "1005"                TO  SYS-1005-KANRICD
           MOVE    "%"                   TO  SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1005-HOSPNUM
           MOVE    WRK-PARA-EDYMD        TO  SYS-1005-EDYUKYMD
           MOVE    WRK-PARA-STYMD        TO  SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC          TO  MCPDATA-REC
           PERFORM   910-SYSKANRI-SELECT-SEC
           MOVE    MCPDATA-REC           TO  SYS-1005-REC
           PERFORM  UNTIL  FLG-SYSKANRI  =  1
             MOVE    SYS-1005-SRYKANAME  TO  WRK-SRYKA-NAME(IDX3)
             MOVE    SYS-1005-KBNCD      TO  WRK-SRYKA(IDX3)
             MOVE    IDX3                TO  IDX-MAX
             ADD   1                     TO  IDX3
             PERFORM   910-SYSKANRI-FETCH-SEC
             MOVE    MCPDATA-REC         TO  SYS-1005-REC
           END-PERFORM
           PERFORM   910-SYSKANRI-CLOSE-SEC
      *
      *    自賠責点数単価取得
           INITIALIZE                        SYS-4001-REC
           MOVE    "4001"                TO  SYS-4001-KANRICD
           MOVE    "*"                   TO  SYS-4001-KBNCD
           MOVE    WRK-PARA-EDYMD        TO  SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-STYMD        TO  SYS-4001-STYUKYMD
           MOVE    SPA-HOSPNUM           TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC          TO  MCPDATA-REC
           PERFORM   910-SYSKANRI-SELECT-SEC
           IF       FLG-SYSKANRI        =   ZERO
             MOVE    MCPDATA-REC         TO  SYS-4001-REC
           ELSE
             INITIALIZE                      SYS-4001-REC
           END-IF
           PERFORM   910-SYSKANRI-CLOSE-SEC
      *
           .
       200-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       300-MAIN-SEC            SECTION.
      *
           OPEN    OUTPUT   TA03-FILE
           CLOSE   TA03-FILE
           OPEN    I-O      TA03-FILE
      *
      *    帳票編集 外来・入院
           PERFORM    400-BODY-HEN-SEC
      *
           CLOSE   TA03-FILE
      *
      *    印刷処理
           IF  WRK-DATA  NOT =  ZERO
      *      印字データ編集処理
             PERFORM  400-PRINT-HEN-SEC
           ELSE
             MOVE    "データがありません"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           .
       300-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>
      *****************************************************************
       400-BODY-HEN-SEC            SECTION.
      *
           PERFORM  900-SYUMEI-READ-SEC
           PERFORM  UNTIL  FLG-SYUMEI  =  1
             IF     SMEI-DENPEDANUM    =  1
               PERFORM  410-BODY-HEN-SEC
             END-IF
             PERFORM  900-SYUMEI-FETCH-SEC
           END-PERFORM
           PERFORM  900-SYUMEI-CLOSE-SEC
      *
           .
       400-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集２<明細行>
      *****************************************************************
       410-BODY-HEN-SEC            SECTION.
      *
           INITIALIZE  WRK-KINGAKU
      *
           PERFORM  900-SYUNOU-READ-SEC
           IF     FLG-SYUNOU          =  0
             AND  SYU-DENPJTIKBN  NOT =  "3"
             AND  SYU-CREATEKBN   NOT =  "3"
      *
             INITIALIZE  TA03TMP-REC
      *
      *      患者番号
             PERFORM  920-PTNUM-READ-SEC
             MOVE    PTNUM-PTNUM         TO  TA03TMP-PTNUM
      *
      *      氏名
             PERFORM  920-PTINF-READ-SEC
             MOVE    PTINF-NAME          TO  TA03TMP-NAME
      *
      *      保険設定
             PERFORM  500-HKNSET-SEC
      *
      *      請求月日
             MOVE    SYU-SRYYMD          TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    WRK-HENYMD          TO  TA03TMP-SEIKYUMD
      *
      *      伝票番号
             MOVE    SYU-DENPNUM         TO  TA03TMP-DENPNUM
      *
      *      病室・診療科
             IF  WRK-PARA-NYUGAIKBN  =  "1"
      *        入院
               MOVE     SYU-ROOMNUM      TO  TA03TMP-SRYKA-BR
               MOVE     "-"              TO  TA03TMP-SRYKA-BR(3:1)
             ELSE
      *        外来
               PERFORM  510-KBNSET-SEC
             END-IF
      *
      *      保険計
      *      点数単価取得
             MOVE     SYU-SYUHKNNUM      TO  WRK-HKNNUM
             PERFORM  920-HKNNUM-SET-SEC
             IF  FLG-HKNNUM  =  1
               MOVE   10                 TO  WRK-TENTNK
             ELSE
               MOVE   HKN-TENTNK         TO  WRK-TENTNK
             END-IF
      *
      *      労災・自賠  点数単価取得
             IF  WRK-HKNNUM  =  "971"  OR  "973"
               IF    SYS-4001-TENKBN  =  "1"
                 MOVE   11.5             TO  WRK-TENTNK
               ELSE
                 MOVE   12               TO  WRK-TENTNK
               END-IF
             END-IF
      *
             COMPUTE  TA03TMP-HKN-TOTAL  =  SYU-TOTAL-HKNTEN
                                            *  WRK-TENTNK
      *
             IF    ((SYU-SYUHKNNUM      =  "971"
               AND   SYU-PTFTNRATE  NOT =  ZERO)
               OR    SYU-SYUHKNNUM      =  "973"
               OR    SYU-SYUHKNNUM      =  "975")
      *        保険組合情報
               INITIALIZE                    HKNCOMBI-REC
               MOVE    SYU-HOSPNUM       TO  COMB-HOSPNUM
               MOVE    SYU-PTID          TO  COMB-PTID
               MOVE    SYU-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
               PERFORM   920-HKNCOMBI-READ-SEC
      *        患者労災情報検索
               INITIALIZE                    PTRSIINF-REC
               MOVE    SYU-HOSPNUM       TO  PTRSI-HOSPNUM
               MOVE    SYU-PTID          TO  PTRSI-PTID
               MOVE    COMB-HKNID        TO  PTRSI-HKNID
               PERFORM   920-PTRSIINF-READ-SEC
      *
               IF  PTRSI-JIBAISEIKBN  NOT  =  2
      *          労災  初診金額
                 ADD  SYU-RSISHOSHIN-MONEY   TO  TA03TMP-HKN-TOTAL
      *          労災  再診金額
                 ADD  SYU-RSISAISHIN-MONEY   TO  TA03TMP-HKN-TOTAL
      *          労災  指導金額
                 ADD  SYU-RSISDO-MONEY       TO  TA03TMP-HKN-TOTAL
      *          労災  その他金額
                 ADD  SYU-RSIETC-MONEY       TO  TA03TMP-HKN-TOTAL
               END-IF
             END-IF
      *
      *      患者負担
             MOVE    SYU-SKYMONEY          TO  TA03TMP-KANJAFTN
      *
      *      自費
             COMPUTE  TA03TMP-JIHI  =  SYU-TOTAL-TGMONEY
                                       +  SYU-TOTAL-TGMONEY-TAX
                                       +  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
      *
      *      未収額
             PERFORM  910-SYUMEI-READ-SEC
             PERFORM  UNTIL  FLG-SYUMEI2  =  1
      *
               IF  WRK-PARA-NYUKINKBN  =  1
                 IF  SMEI2-NYUHEN-YMD  <=  WRK-PARA-EDYMD
                   COMPUTE  WRK-KINGAKU      =  WRK-KINGAKU
                                             +  SMEI2-SKYMONEY
                                             -  SMEI2-NYUHEN-MONEY
                 END-IF
               ELSE
                 COMPUTE  WRK-KINGAKU      =  WRK-KINGAKU
                                           +  SMEI2-SKYMONEY
                                           -  SMEI2-NYUHEN-MONEY
               END-IF
      *
               PERFORM  910-SYUMEI-FETCH-SEC
             END-PERFORM
             PERFORM  910-SYUMEI-CLOSE-SEC
      *
             ADD     WRK-KINGAKU         TO  TA03TMP-MISYU
      *
      *      電話番号ー自宅
             MOVE    PTINF-HOME-TEL1     TO  TA03TMP-TEL
      *
      *      最終受診日
             PERFORM  920-SRYKARRK-READ-SEC
             MOVE    KARRK-LASTYMD       TO  WRK-SYMD
             PERFORM  700-SEIWA-HEN-SEC
             MOVE    WRK-HENYMD          TO  TA03TMP-JYUSHINYMD
      *
             IF  WRK-KINGAKU  NOT =  ZERO
      *
               ADD   1           TO  WRK-DATA
      *
               WRITE   TA03TMP-REC
             END-IF
      *
           END-IF
      *
           .
       410-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険名称編集
      *****************************************************************
       500-HKNSET-SEC                SECTION.
      *
           EVALUATE  SYU-SYUHKNNUM
             WHEN  "980"  THRU  "989"
               MOVE    "自費"        TO  TA03TMP-HKN
             WHEN  "971"
               MOVE    "労災"        TO  TA03TMP-HKN
             WHEN  "973"
               MOVE    "自賠"        TO  TA03TMP-HKN
             WHEN  "039"
             WHEN  "040"
               MOVE    "後高"        TO  TA03TMP-HKN
             WHEN  "060"
             WHEN  "067"
               MOVE    "国保"        TO  TA03TMP-HKN
             WHEN  SPACE
               MOVE    "公費"        TO  TA03TMP-HKN
             WHEN  OTHER
               MOVE    "社保"        TO  TA03TMP-HKN
           END-EVALUATE
      *
           .
       500-HKNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称区分セット(科別)
      *****************************************************************
       510-KBNSET-SEC            SECTION.
      *
      *    名称インデックスセット
           MOVE    ZERO        TO  FLG-LOOPEND
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >  IDX-MAX
                    OR  FLG-LOOPEND  =  1
             IF  SYU-SRYKA  =  WRK-SRYKA(IDX3)(4:2)
               MOVE    1                     TO  FLG-LOOPEND
               MOVE    WRK-SRYKA-NAME(IDX3)  TO  TA03TMP-SRYKA-BR
             END-IF
           END-PERFORM
      *
           .
       510-KBNSET-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       400-PRINT-HEN-SEC            SECTION.
      *
           OPEN    INPUT       TA03-FILE
           MOVE    ZERO        TO  FLG-PRINT
      *
           INITIALIZE          TA03TMP-REC
           INITIALIZE          TA03
      *
           MOVE    ZERO        TO  CNT-RENNUM
           MOVE    ZERO        TO  IDX
      *
           MOVE    SPACE       TO  WRK-CSVDATA
           MOVE    1           TO  IDXP
      *
      *    CSVヘッダ編集処理
           PERFORM  800-CSV-HEAD-SEC
           PERFORM  230-TOUKEICSV-SEC
      *
      *    一時ファイル読込
           PERFORM  710-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
             ADD     1                      TO  WRK-GKENSU
             ADD     1                      TO  IDX
      *
      *      連番
             ADD     1                      TO  CNT-RENNUM
             MOVE    CNT-RENNUM             TO  WRK-Z6
             MOVE    WRK-Z6                 TO  TA03-RENNUM(IDX)
      *      患者番号
             IF  TA03TMP-PTNUM(11:1)  =  SPACE
               MOVE    TA03TMP-PTNUM        TO  TA03-PTNUM(IDX)
             ELSE
               MOVE    TA03TMP-PTNUM(1:10)  TO  TA03-PTNUM2-1(IDX)
               MOVE    TA03TMP-PTNUM(11:10) TO  TA03-PTNUM2-2(IDX)
             END-IF
      *      氏名
             MOVE    TA03TMP-NAME        TO  TA03-NAME(IDX)
      *      保険
             MOVE    TA03TMP-HKN         TO  TA03-HKN(IDX)
      *      請求月日
             MOVE    TA03TMP-SEIKYUMD    TO  TA03-SEIKYUMD(IDX)
      *      伝票番号
             MOVE    TA03TMP-DENPNUM     TO  WRK-Z7
             MOVE    WRK-Z7                 TO  TA03-DENPNUM(IDX)
      *      病室／診療科
             MOVE    TA03TMP-SRYKA-BR    TO  TA03-SRYKA-BR(IDX)
      *      保険計
             MOVE    TA03TMP-HKN-TOTAL   TO  WRK-Z9
             MOVE    WRK-Z9                 TO  TA03-HKN-TOTAL(IDX)
             ADD     TA03TMP-HKN-TOTAL   TO  WRK-GHKN-TOTAL
      *      患者負担
             MOVE    TA03TMP-KANJAFTN    TO  WRK-Z9
             MOVE    WRK-Z9                 TO  TA03-KANJAFTN(IDX)
             ADD     TA03TMP-KANJAFTN    TO  WRK-GKANJAFTN
      *      自費
             MOVE    TA03TMP-JIHI        TO  WRK-Z9
             MOVE    WRK-Z9                 TO  TA03-JIHI(IDX)
             ADD     TA03TMP-JIHI        TO  WRK-GJIHI
      *      未収額
             IF  TA03TMP-MISYU  >=  ZERO
               MOVE    TA03TMP-MISYU       TO  WRK-Z9
               MOVE    WRK-Z9                 TO  TA03-MISYU(IDX)
             ELSE
               MOVE    TA03TMP-MISYU       TO  WRK-M9
               MOVE    WRK-M9                 TO  TA03-MISYU(IDX)
             END-IF
             ADD     TA03TMP-MISYU       TO  WRK-GMISYU
      *      最終受診日
             MOVE    TA03TMP-JYUSHINYMD  TO  TA03-JYUSHINYMD(IDX)
      *      電話番号
             MOVE    TA03TMP-TEL         TO  TA03-TEL(IDX)
      *
             IF  IDX  =  34
               MOVE    ZERO        TO  IDX
      *        印刷処理
               PERFORM  500-PRINT-OUT-SEC
             END-IF
      *
      *      ＣＳＶボディ
             PERFORM  400-CSV-DATA-SEC
             PERFORM  230-TOUKEICSV-SEC
      *
      *      一時ファイル読込
             PERFORM  710-TMP-READ-SEC
           END-PERFORM
      *
      *    合計
      *    総件数
           MOVE    WRK-GKENSU          TO  WRK-Z6
           MOVE    WRK-Z6              TO  TA03-GKENSU
      *    保険計
           MOVE    WRK-GHKN-TOTAL      TO  WRK-Z10
           MOVE    WRK-Z10             TO  TA03-GHKN-TOTAL
      *    患者負担
           MOVE    WRK-GKANJAFTN       TO  WRK-Z10
           MOVE    WRK-Z10             TO  TA03-GKANJAFTN
      *    自費
           MOVE    WRK-GJIHI           TO  WRK-Z10
           MOVE    WRK-Z10             TO  TA03-GJIHI
      *    未収額
           IF  WRK-GMISYU  >=  ZERO
             MOVE    WRK-GMISYU          TO  WRK-Z10
             MOVE    WRK-Z10             TO  TA03-GMISYU
           ELSE
             MOVE    WRK-GMISYU          TO  WRK-M10
             MOVE    WRK-M10             TO  TA03-GMISYU
           END-IF
      *    印刷処理
           PERFORM  500-PRINT-OUT-SEC
      *
      *    ＣＳＶフッター
           PERFORM  400-CSV-GOUKEI-SEC
           PERFORM  230-TOUKEICSV-SEC
      *
           CLOSE   TA03-FILE
      *
           .
       400-PRINT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       400-HEAD-HEN-SEC            SECTION.
      *
      *    対象年月初日取得
           MOVE    WRK-PARA-STYMD         TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  TA03-SRYSTYMD
      *
      *    対象年月最終日取得
           MOVE    WRK-PARA-EDYMD         TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  TA03-SRYEDYMD
      *
      *    作成年月日編集
           MOVE    LNK-PRTKANRI-SKYYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  TA03-CREATEYMD
      *
      *    頁数編集
           ADD     1                      TO  CNT-PAGE
           MOVE    CNT-PAGE               TO  WRK-Z2
           MOVE    WRK-Z2                 TO  TA03-PAGE
      *
      *    合計頁数計算
           MOVE    ZERO                   TO  CNT-GPAGE
           DIVIDE  WRK-DATA   BY  34
                   GIVING  CNT-GPAGE  REMAINDER  WRK-AMARI
           IF  WRK-AMARI  NOT =  ZERO
             COMPUTE  CNT-GPAGE  =  CNT-GPAGE + 1
           END-IF
      *
           MOVE    CNT-GPAGE              TO  WRK-Z2
           MOVE    WRK-Z2                 TO  TA03-GPAGE
      *
      *    入院／外来・病室／診療科
           IF  WRK-PARA-NYUGAIKBN  =  "1"
             MOVE    "入院"               TO  TA03-NYUGAI
             MOVE    "病棟ー病室"         TO  TA03-LABEL
           ELSE
             MOVE    "外来"               TO  TA03-NYUGAI
             MOVE    "  診療科  "         TO  TA03-LABEL
           END-IF
      *
           .
       400-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       500-PRINT-OUT-SEC            SECTION.
      *
      *    ヘッダー編集処理
           PERFORM  400-HEAD-HEN-SEC
      *
      *    印刷処理
           INITIALIZE                         ORCSPRTAREA
           MOVE    "INS"                      TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           MOVE    "A00000TA03.red"           TO  SPRT-PRTID
           MOVE    "未収金一覧表（月次）"     TO  SPRT-TITLE
           MOVE    TA03                       TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                  USING
                                              ORCSPRTAREA
                                              SPA-AREA
           IF  SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"  TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                      TA03
      *
           .
       500-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC              SECTION.
      *
      *    対象年月日取得（開始日）
           MOVE     "開始日"                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           MOVE     WRK-PARA-STYMD           TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE     WRK-HENYMD               TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    対象年月日取得（終了日）
           MOVE     "終了日"                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           MOVE     WRK-PARA-EDYMD           TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE     WRK-HENYMD               TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分
           MOVE     "入外区分"               TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           IF  WRK-PARA-NYUGAIKBN  =  "1"
             MOVE     "入院"                   TO  WRK-ITEM
           ELSE
             MOVE     "外来"                   TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    対象年月日取得（作成日）
           MOVE     "作成日"                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           MOVE     LNK-PRTKANRI-SKYYMD      TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE     WRK-HENYMD               TO  WRK-ITEM
      *
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名称セット
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  CSV-HEAD-MAX
             MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
             IF  IDX4  >=  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               IF  WRK-PARA-NYUGAIKBN  =  1
                 IF  IDX4  NOT =  8
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
               IF  WRK-PARA-NYUGAIKBN  =  2
                 IF  IDX4  NOT =  7
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC              SECTION.
      *
      *    連番
           MOVE     CNT-RENNUM            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           MOVE     TA03TMP-PTNUM         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    氏名
           MOVE     TA03TMP-NAME          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険
           MOVE     TA03TMP-HKN           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    請求月日
           MOVE     TA03TMP-SEIKYUMD      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    伝票番号
           MOVE     TA03TMP-DENPNUM       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    病室／診療科
           MOVE     TA03TMP-SRYKA-BR      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険計
           MOVE     TA03TMP-HKN-TOTAL     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者負担
           MOVE     TA03TMP-KANJAFTN      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    自費
           MOVE     TA03TMP-JIHI          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    未収額
           IF  TA03TMP-MISYU  >=  ZERO
             MOVE    TA03TMP-MISYU          TO  WRK-Z92
             MOVE    WRK-Z92                TO  WRK-ITEM
           ELSE
             MOVE    TA03TMP-MISYU          TO  WRK-M92
             MOVE    WRK-M92                TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入金額
           MOVE     SPACE                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入金月日
           MOVE     SPACE                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    最終受診日
           MOVE     TA03TMP-JYUSHINYMD    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    電話番号
           MOVE     TA03TMP-TEL           TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSV合計編集処理
      *****************************************************************
       400-CSV-GOUKEI-SEC              SECTION.
      *
      *    合計
           MOVE     "総合計"                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    総件数
           MOVE     "総件数"                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険計
           MOVE     "保険計"                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者負担
           MOVE     "患者負担"               TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    自費
           MOVE     "自費"                   TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    未収額
           MOVE     "未収額"                 TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    合計
           INITIALIZE                            WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    総件数
           MOVE     WRK-GKENSU               TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険計
           MOVE     WRK-GHKN-TOTAL           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者負担
           MOVE     WRK-GKANJAFTN            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    自費
           MOVE     WRK-GJIHI                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    未収額
           IF  WRK-GMISYU  >=  ZERO
             MOVE    WRK-GMISYU                TO  WRK-Z102
             MOVE    WRK-Z102                  TO  WRK-ITEM
           ELSE
             MOVE    WRK-GMISYU                TO  WRK-M102
             MOVE    WRK-M102                  TO  WRK-ITEM
           END-IF
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-GOUKEI-EXT.
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
       800-CSVDATA-EDIT-EXT.
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
           INITIALIZE                        ORCSTOUKEICSVAREA
           MOVE    "INS"                     TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY      TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP    TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID      TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM        TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN        TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                      TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"               TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM               DELIMITED  BY  SIZE
                   "misyu_ichiran_"          DELIMITED   BY  SIZE
                   WRK-PARA-STYMD            DELIMITED   BY  SIZE
                   "_"
                   WRK-PARA-EDYMD            DELIMITED   BY  SIZE
                   ".csv"                    DELIMITED   BY  SIZE
           INTO    STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA               TO  STOUKEICSV-CSVDATA
           MOVE    "未収金一覧表（月次）"    TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"           USING
                                             ORCSTOUKEICSVAREA
                                             SPA-AREA
           IF    ( STOUKEICSV-RETURN   =   ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
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
      *    エラー出力処理
      *****************************************************************
       800-ERR-HENSYU-SEC            SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT             RECEERR-FILE
      *
             MOVE    WRK-RECEERR        TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"              TO  SJOBKANRI-MODE
             INITIALIZE                 JOBKANRI-REC
             MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID     TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID   TO  JOB-SHELLID
             MOVE    WRK-RECEERR        TO  JOB-YOBI
             MOVE    "9999"             TO  JOB-ERRCD
             CALL    "ORCSJOB"          USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
           END-IF
      *
           MOVE    1           TO  FLG-END
      *
           .
       800-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       700-SEIWA-HEN-SEC            SECTION.
      *
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY2-AREA
           MOVE    "21"                 TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"            USING
                                        STS-AREA-DAY
                                        LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3     TO  WRK-HENYMDG
           MOVE    LNK-DAY2-EDTYMD1     TO  WRK-HENYMD
           INSPECT WRK-HENYMDG  REPLACING  ALL "  "  BY  "　"
           .
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       700-LASTDAY-GET-SEC          SECTION.
      *
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY7-AREA
           MOVE    "71"                 TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-STYMD(1:6)  TO  LNK-DAY7-YM
           CALL    "ORCSDAY"            USING
                                        STS-AREA-DAY
                                        LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI      TO  WRK-SYMD
           .
       700-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       600-END-SEC            SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"                TO  SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
           MOVE    SPA-HOSPNUM          TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID       TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO  JOB-SHELLID
           MOVE    CNT-PAGE             TO  JOB-UPDCNT
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
      *
           .
       600-END-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    "DBFETCH"        TO  MCP-FUNC
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key10"          TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE    ZERO           TO  FLG-SYSKANRI
             ELSE
               MOVE    1              TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-SELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM  910-SYSKANRI-FETCH-SEC 
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
           .
       910-SYSKANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報
      *****************************************************************
       900-SYUMEI-READ-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
           MOVE    SPA-HOSPNUM         TO   SMEI-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN  TO   SMEI-NYUGAIKBN
           MOVE    WRK-PARA-STYMD      TO   SMEI-STDAILYKEY(1:8)
           MOVE    "0000"              TO   SMEI-STDAILYKEY(9:4)
           MOVE    WRK-PARA-EDYMD      TO   SMEI-EDDAILYKEY(1:8)
           MOVE    "9999"              TO   SMEI-EDDAILYKEY(9:4)
      *
           MOVE    SYUMEI-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key8"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 900-SYUMEI-FETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-SYUMEI
           END-IF
           .
       900-SYUMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key8"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-SYUMEI
             MOVE    MCPDATA-REC       TO   SYUMEI-REC
      *
             PERFORM  900-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
             AND PTINF-TSTPTNUMKBN  =  "1"
                 GO  TO  900-SYUMEI-FETCH-SEC
             END-IF
      *
           ELSE
             MOVE    1                 TO   FLG-SYUMEI
           END-IF  
           .
       900-SYUMEI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細クローズ処理
      *****************************************************************
       900-SYUMEI-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key8"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-SYUMEI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                    PTINF-REC
           MOVE    SMEI-HOSPNUM          TO   PTINF-HOSPNUM
           MOVE    SMEI-PTID             TO   PTINF-PTID
           MOVE    PTINF-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptinf"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTINF-REC
               MOVE    ZERO              TO   FLG-PTINF
             ELSE
               MOVE    1                 TO   FLG-PTINF
               INITIALIZE                PTINF-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-PTINF
             INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索(SELECT)
      *****************************************************************
       900-SYUNOU-READ-SEC             SECTION.
      *
           INITIALIZE                  SYUNOU-REC
      *
           MOVE    SMEI-HOSPNUM        TO  SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYU-PTID
           MOVE    SMEI-DENPNUM        TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_syunou"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYUNOU
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
             ELSE
               MOVE    1                   TO  FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                   TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納明細情報
      *****************************************************************
       910-SYUMEI-READ-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI2-REC
      *
           MOVE    SYU-HOSPNUM         TO   SMEI2-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO   SMEI2-NYUGAIKBN
           MOVE    SYU-PTID            TO   SMEI2-PTID
           MOVE    SYU-DENPNUM         TO   SMEI2-DENPNUM
      *
           MOVE    SYUMEI2-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 910-SYUMEI-FETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-SYUMEI2
           END-IF
           .
       910-SYUMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       910-SYUMEI-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-SYUMEI2
             MOVE    MCPDATA-REC       TO   SYUMEI2-REC
           ELSE
             MOVE    1                 TO   FLG-SYUMEI2
           END-IF  
           .
       910-SYUMEI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細クローズ処理
      *****************************************************************
       910-SYUMEI-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-SYUMEI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換情報読み込み
      *****************************************************************
       920-PTNUM-READ-SEC               SECTION.
      *
           INITIALIZE                   PTNUM-REC
           MOVE    SYU-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    SYU-PTID             TO  PTNUM-PTID
           MOVE    PTNUM-REC            TO  MCPDATA-REC 
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnum"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_ptnum"        TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO  PTNUM-REC
               MOVE    ZERO             TO  FLG-PTNUM
             ELSE
               MOVE    1                TO  FLG-PTNUM
             END-IF
           ELSE
             MOVE    1                  TO  FLG-PTNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptnum"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       920-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                    PTINF-REC
           MOVE    SYU-HOSPNUM           TO   PTINF-HOSPNUM
           MOVE    SYU-PTID              TO   PTINF-PTID
           MOVE    PTINF-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptinf"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTINF-REC
               MOVE    ZERO              TO   FLG-PTINF
             ELSE
               MOVE    1                 TO   FLG-PTINF
               INITIALIZE                PTINF-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-PTINF
             INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴  読込処理
      *****************************************************************
       920-SRYKARRK-READ-SEC         SECTION.
      *
           INITIALIZE               SRYKARRK-REC
           MOVE    SYU-HOSPNUM      TO   KARRK-HOSPNUM
           MOVE    SYU-PTID         TO   KARRK-PTID
           MOVE    SYU-SRYKA        TO   KARRK-SRYKA
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBSELECT"       TO   MCP-FUNC
           MOVE    "tbl_srykarrk"   TO   MCP-TABLE
           MOVE    "key"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_srykarrk"    TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   SRYKARRK-REC
               MOVE    ZERO            TO   FLG-SRYKARRK
             ELSE
               MOVE    1               TO   FLG-SRYKARRK
               INITIALIZE                   SRYKARRK-REC
             END-IF
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
             INITIALIZE                   SRYKARRK-REC
           END-IF
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBCLOSECURSOR"  TO   MCP-FUNC
           MOVE    "tbl_srykarrk"   TO   MCP-TABLE
           MOVE    "key"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       920-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報セット
      *****************************************************************
       920-HKNNUM-SET-SEC  SECTION.
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
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_hknnum"       TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  HKNNUM-REC
               MOVE    ZERO             TO  FLG-HKNNUM
             ELSE
               INITIALIZE               HKNNUM-REC
               MOVE    1                TO  FLG-HKNNUM
             END-IF
           ELSE
             MOVE    1                  TO  FLG-HKNNUM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       920-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
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
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-HKNCOMBI-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者労災マスター読込処理
      *****************************************************************
       920-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC      TO  PTRSIINF-REC
               MOVE    ZERO             TO  FLG-PTRSIINF
             ELSE
               MOVE    1                TO  FLG-PTRSIINF
               INITIALIZE                   PTRSIINF-REC
             END-IF
           ELSE
             MOVE    1                  TO  FLG-PTRSIINF
             INITIALIZE                     PTRSIINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-PTRSIINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(KEY READ)
      *****************************************************************
       700-TMP-READ-SEC              SECTION.
      *
           INITIALIZE                FLG-INVALID
           READ   TA03-FILE
               INVALID    KEY
                 MOVE    1           TO  FLG-INVALID
               NOT  INVALID  KEY
                 MOVE    ZERO        TO  FLG-INVALID
           END-READ
           .
       700-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(NEXT READ)
      *****************************************************************
       710-TMP-READ-SEC              SECTION.
      *
           READ    TA03-FILE  NEXT
               AT  END
                 MOVE    1           TO  FLG-PRINT
               NOT  AT END
                 MOVE    ZERO        TO  FLG-PRINT
           END-READ
           .
       710-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC            SECTION.
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
      *    ＤＢクローズ処理
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
