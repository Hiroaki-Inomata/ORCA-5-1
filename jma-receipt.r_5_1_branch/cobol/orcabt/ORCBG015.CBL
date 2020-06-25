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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBG015.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   一部負担金給与控除一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/02/14    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  03.05.01    NACL-森脇     07/06/06  グループ診療対応
      *  04.08.01    NACL-森脇     14/07/22  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計用ファイル
           SELECT  TOKEI-FILE      ASSIGN  TOKEIPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOKEI-KEY
                                   FILE    STATUS  IS  STS-TOKEI.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-KIGO          PIC X(20).
               05  TOKEI-NUM           PIC X(20).
               05  TOKEI-HONKZKKBN     PIC X(01).
               05  TOKEI-PTNUM         PIC X(20).
               05  TOKEI-NYUGAIKBN     PIC X(01).
               05  TOKEI-DENPNUM       PIC X(07).
           03  TOKEI-NAME              PIC X(26).
           03  TOKEI-HIHKNJA           PIC X(22).
           03  TOKEI-SRYYMD            PIC X(08).
           03  TOKEI-SEIKYU            PIC S9(08).
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    統計ファイル
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER                  PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *    エラーファイル 名称領域 
grpsys*    COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
grpsys*                                BY         //RECEERR//.
grpsys*    03  FILLER                  PIC X(04)   VALUE   ".dat".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCM46.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-WRKSMEI             PIC 9(01).
           03  FLG-BREAK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID         PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM        PIC 9(02).
      *----(04.08.01)--UPD-START---
grpsys*     03  RECEERR                 PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-DENPPRTYM      PIC X(06).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-CD                  PIC 9(03).
           03  CNT-PAGE                PIC 9(03).
           03  CNT-LINE                PIC 9(02).
      *
      *    集計領域  （１）患者計、（２）世帯計、（３）請求合計、（４）患者合計、（５）世帯合計
       01  SUM-AREA.
           03  SUM-GOK-TBL             OCCURS  5.
               05  SUM-GOK-SEIKYU      PIC S9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *    編集用キー
           03  WRK-KEY.
               05  WRK-KIGO            PIC X(20).
               05  WRK-NUM             PIC X(20).
               05  WRK-PTNUM           PIC X(20).
               05  WRK-DENPNUM         PIC X(07).
      *
      *    エラー情報
           03  WRK-RECEERR             PIC X(200).
      *
      *    日付編集用
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-DENPPRTYM           PIC X(18).
           03  WRK-SAKUSEIYMD          PIC X(22).
           03  WRK-SRYYM               PIC X(20).
           03  WRK-SEN                 PIC X(104).
      *
      *    数字編集
           03  WRK-Z3                  PIC ZZ9.
           03  WRK-Z12                 PIC ZZZZ,ZZZ,ZZ9.
           03  WRK-ZM12                PIC ----,---,--9.
           03  WRK-94                  PIC 9(04).
      *
           03  WRK-KBNCD-TBL           OCCURS  99.
               05  WRK-KBNCD           PIC X(02).
      *
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報情報
           COPY    "CPSK1041.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    作業用収納明細
       01  WRKSYUMEI-REC.
           COPY    "CPSYUMEI.INC"  REPLACING  //SMEI//
                                   BY         //WRKSMEI//.
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
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
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *    LIKE使用KEY対応
           COPY    "CPORCSADDSIGN.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
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
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-PARA
grpsys     INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢオープン処理
           PERFORM 100-DBOPEN-SEC
      *
      *    パラメタセット
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
grpsys*DD                                      WRK-PARA-HOSPID
grpsys                                         WRK-PARA-HOSPNUM
grpsys                                         RECEERR
                                               WRK-PARA-DENPPRTYM
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG015"          TO  JOB-PGID
           MOVE    "一部負担金給与控除一覧"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
      *    ファイル設定
grpsys     MOVE    "BG01501"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
grpsys     MOVE    SPA-HOSPNUM         TO  TOKEIPARA-HOSPNUM
      *
grpsys*DD  MOVE    "RECEERR"           TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYSYMD
      *
      *    作成日セット
           MOVE    WRK-SYSYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SAKUSEIYMD
      *
      *    対象年月セット
           MOVE    WRK-PARA-DENPPRTYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-DENPPRTYM(1:16)
           MOVE    "分"                TO  WRK-DENPPRTYM(17:2)
      *
      *    入金区分セット
           MOVE    ZERO                TO  CNT-CD
           PERFORM 900-SYS1041-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1  )
                   OR    ( CNT-CD          >=  99 )
               IF      SYS-1041-NYKN-BUNRUI    =   "04"
                   ADD     1               TO  CNT-CD
                   MOVE    SYS-1041-KBNCD(1:2)
                                           TO  WRK-KBNCD(CNT-CD)
               END-IF
               PERFORM 900-SYS1041-FETCH-SEC
           END-PERFORM
      *
           PERFORM 900-SYS1041-CLOSE-SEC
      *
           MOVE    "--------------------------------------------------"
                                       TO  WRK-SEN(1:50)
           MOVE    "--------------------------------------------------"
                                       TO  WRK-SEN(51:50)
           MOVE    "----"              TO  WRK-SEN(101:4)
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    統計用ファイルをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *    統計用ファイルを開く
           OPEN    I-O                 TOKEI-FILE
      *
      *    統計ファイル集計処理
           PERFORM 210-SYUKEI-HENSYU-SEC
      *
      *    統計用ファイルを閉じる
           CLOSE                       TOKEI-FILE
      *    統計用ファイルを開く
           OPEN    INPUT               TOKEI-FILE
      *
      *    印刷処理
           PERFORM 300-PRINT-HENSYU-SEC
      *
      *    統計用ファイルを閉じる
           CLOSE                       TOKEI-FILE
      *    統計用ファイルをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       210-SYUKEI-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-DENPNUM
      *
      *    収納明細読み込み
           PERFORM 900-SYUMEI-SELECT-SEC
      *
           PERFORM UNTIL   FLG-SYUMEI  =   1
               IF      SMEI-SKYMONEY   NOT =   0
                   MOVE    ZERO            TO  FLG-OK
                   IF      SMEI-DENPNUM    NOT =   WRK-DENPNUM
                       PERFORM 900-WRKSYUMEI-READ-SEC
      *                入金方法チェック
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL ( FLG-WRKSMEI =   1  )
                               OR    ( IDX     >   CNT-CD )
                               OR    ( IDX     >=  99     )
                           IF      WRKSMEI-NYUKIN-HOHO =  WRK-KBNCD(IDX)
                               MOVE    WRKSMEI-DENPNUM TO   WRK-DENPNUM
                               MOVE    1               TO  FLG-OK
                               MOVE    99              TO  IDX
                           END-IF
                       END-PERFORM
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
                   IF      FLG-OK      =   1
                       PERFORM 220-SYUMEI-SYUKEI-SEC
                   END-IF
               END-IF
      *        収納明細読み込み
               PERFORM 900-SYUMEI-FETCH-SEC
           END-PERFORM
      *
      *    収納明細読み込み
           PERFORM 900-SYUMEI-CLOSE-SEC
      *
           .
       210-SYUKEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計集計処理
      *****************************************************************
       220-SYUMEI-SYUKEI-SEC       SECTION.
      *
      *    患者番号読み込み
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM       =   1
               GO  TO  220-SYUMEI-SYUKEI-EXT
           END-IF
      *
      *    患者情報読み込み
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       =   1
               GO  TO  220-SYUMEI-SYUKEI-EXT
           END-IF
      *
      *    収納読み込み
           PERFORM 900-SYUNOU-READ-SEC
           IF      FLG-SYUNOU      =   1
               GO  TO  220-SYUMEI-SYUKEI-EXT
           END-IF
      *
      *    保険組み合わせ情報読み込み
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI       =   1
               GO  TO  220-SYUMEI-SYUKEI-EXT
           END-IF
      *
      *    患者保険情報読み込み
           PERFORM 900-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF       =   1
               GO  TO  220-SYUMEI-SYUKEI-EXT
           END-IF
      *
      *    統計読み込み
           INITIALIZE                  TOKEI-REC
           MOVE    PTHKN-KIGO      TO  TOKEI-KIGO
           MOVE    PTHKN-NUM       TO  TOKEI-NUM
           MOVE    PTHKN-HONKZKKBN TO  TOKEI-HONKZKKBN
           MOVE    PTNUM-PTNUM     TO  TOKEI-PTNUM
           MOVE    SMEI-NYUGAIKBN  TO  TOKEI-NYUGAIKBN
           MOVE    SMEI-DENPNUM    TO  TOKEI-DENPNUM
           PERFORM 900-TOKEI-READ-SEC
      *
           IF      FLG-TOKEI       =   1
               MOVE    PTINF-NAME      TO  TOKEI-NAME
               MOVE    PTHKN-HIHKNJANAME   TO  TOKEI-HIHKNJA
               MOVE    SMEI-SKYMONEY   TO  TOKEI-SEIKYU
               MOVE    SYU-SRYYMD      TO  TOKEI-SRYYMD
               WRITE   TOKEI-REC
           ELSE
               ADD     SMEI-SKYMONEY   TO  TOKEI-SEIKYU
               REWRITE TOKEI-REC
           END-IF
      *
           .
       220-SYUMEI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       300-PRINT-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-KEY
           MOVE    ZERO            TO  SUM-AREA
           MOVE    ZERO            TO  CNT-AREA
      *
      *    統計ファイル読み込み
           INITIALIZE                  TOKEI-REC
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI   =   1 )
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           .
       300-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           ADD     1               TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
           INITIALIZE                  HCM46
      *
           MOVE    WRK-DENPPRTYM   TO  HCM46-DENPPRTYM
           MOVE    WRK-SAKUSEIYMD  TO  HCM46-SAKUSEIYMD
           MOVE    CNT-PAGE        TO  WRK-Z3
           MOVE    WRK-Z3          TO  HCM46-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
           PERFORM UNTIL ( FLG-TOKEI   =   1  )
                   OR    ( CNT-LINE    >=  23 )
               ADD     1               TO  CNT-LINE
               ADD     1               TO  CNT-CD
      *
               MOVE    CNT-CD          TO  WRK-Z3
               MOVE    WRK-Z3          TO  HCM46-CD(CNT-LINE)
      *
               IF      TOKEI-PTNUM(11:)    =   SPACE
                   MOVE    TOKEI-PTNUM TO  HCM46-PTNUM(CNT-LINE)
               ELSE
                   MOVE    TOKEI-PTNUM TO  HCM46-PTNUM2(CNT-LINE)
               END-IF
      *
               MOVE    TOKEI-NAME      TO  HCM46-NAME(CNT-LINE)
               MOVE    TOKEI-DENPNUM   TO  HCM46-DENPNUM(CNT-LINE)
      *
               IF      TOKEI-SEIKYU    <   0
                   MOVE    TOKEI-SEIKYU    TO  WRK-ZM12
                   MOVE    WRK-ZM12    TO  HCM46-SEIKYU(CNT-LINE)
               ELSE
                   MOVE    TOKEI-SEIKYU    TO  WRK-Z12
                   MOVE    WRK-Z12     TO  HCM46-SEIKYU(CNT-LINE)
               END-IF
      *
               IF      TOKEI-NYUGAIKBN =   "1"
                   MOVE    "入院"          TO  HCM46-NYUGAI(CNT-LINE)
               END-IF
      *
               IF      TOKEI-SRYYMD(1:6)   <   WRK-PARA-DENPPRTYM
                   MOVE    SPACE           TO  WRK-SRYYM
                   MOVE    TOKEI-SRYYMD    TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    "（"            TO  WRK-SRYYM(1:2)
                   MOVE    WRK-HENYMDG(1:16)
                                           TO  WRK-SRYYM(3:16)
                   MOVE    "）"            TO  WRK-SRYYM(19:2)
                   MOVE    WRK-SRYYM       TO  HCM46-SRYYM(CNT-LINE)
               END-IF
      *
               IF    ( TOKEI-PTNUM     NOT =   WRK-PTNUM )
                   MOVE    ZERO            TO  SUM-GOK-SEIKYU(1)
                   MOVE    TOKEI-PTNUM     TO  WRK-PTNUM
      *
                   IF    ( TOKEI-NUM           =   SPACE    )  OR
                         ( TOKEI-NUM       NOT =   WRK-NUM  )  OR
                         ( TOKEI-KIGO      NOT =   WRK-KIGO )
                       MOVE    ZERO            TO  SUM-GOK-SEIKYU(2)
                       MOVE    TOKEI-KIGO      TO  WRK-KIGO
                       MOVE    TOKEI-NUM       TO  WRK-NUM
      *
                       MOVE    TOKEI-KIGO      TO  HCM46-KIGO(CNT-LINE)
                       MOVE    TOKEI-NUM       TO  HCM46-NUM(CNT-LINE)
                       MOVE    "被保険者名"    TO  HCM46-LABEL(CNT-LINE)
                       MOVE    TOKEI-HIHKNJA
                                       TO  HCM46-HIHKNJA(CNT-LINE)
                   END-IF
               END-IF
      *
               ADD     TOKEI-SEIKYU    TO  SUM-GOK-SEIKYU(1)
                                           SUM-GOK-SEIKYU(2)
                                           SUM-GOK-SEIKYU(3)
      *        統計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
      *
               IF    ( FLG-TOKEI           =   1         ) OR
                     ( TOKEI-PTNUM     NOT =   WRK-PTNUM )
                   IF      SUM-GOK-SEIKYU(1)   <   0
                       MOVE    SUM-GOK-SEIKYU(1)   TO  WRK-ZM12
                       MOVE    WRK-ZM12    TO
                                           HCM46-SEIKYUPTNUM(CNT-LINE)
                   ELSE
                       MOVE    SUM-GOK-SEIKYU(1)   TO  WRK-Z12
                       MOVE    WRK-Z12     TO
                                           HCM46-SEIKYUPTNUM(CNT-LINE)
                   END-IF
                   IF      CNT-LINE        NOT =   23
                       MOVE    WRK-SEN(1:50)
                                           TO  HCM46-SEN(CNT-LINE)
                   END-IF
                   ADD     SUM-GOK-SEIKYU(1)   TO  SUM-GOK-SEIKYU(4)
      *
                   IF    ( FLG-TOKEI           =   1        )  OR
                         ( TOKEI-NUM           =   SPACE    )  OR
                         ( TOKEI-NUM       NOT =   WRK-NUM  )  OR
                         ( TOKEI-KIGO      NOT =   WRK-KIGO )
                       IF      SUM-GOK-SEIKYU(2)   <   0
                           MOVE    SUM-GOK-SEIKYU(2)   TO  WRK-ZM12
                           MOVE    WRK-ZM12    TO
                                           HCM46-SEIKYUSETAI(CNT-LINE)
                       ELSE
                           MOVE    SUM-GOK-SEIKYU(2)   TO  WRK-Z12
                           MOVE    WRK-Z12     TO
                                           HCM46-SEIKYUSETAI(CNT-LINE)
                       END-IF
                       IF      CNT-LINE        NOT =   23
                           MOVE    WRK-SEN(1:104)
                                               TO  HCM46-SEN2(CNT-LINE)
                       END-IF
                       ADD     SUM-GOK-SEIKYU(2)   TO  SUM-GOK-SEIKYU(5)
                   END-IF
               END-IF
      *
               IF      FLG-TOKEI       =   1
      *            請求合計
                   IF      SUM-GOK-SEIKYU(3)   <   0
                       MOVE    SUM-GOK-SEIKYU(3)   TO  WRK-ZM12
                       MOVE    WRK-ZM12    TO  HCM46-GSEIKYU
                   ELSE
                       MOVE    SUM-GOK-SEIKYU(3)   TO  WRK-Z12
                       MOVE    WRK-Z12     TO  HCM46-GSEIKYU
                   END-IF
      *            患者合計
                   IF      SUM-GOK-SEIKYU(4)   <   0
                       MOVE    SUM-GOK-SEIKYU(4)   TO  WRK-ZM12
                       MOVE    WRK-ZM12    TO  HCM46-GSEIKYUPTNUM
                   ELSE
                       MOVE    SUM-GOK-SEIKYU(4)   TO  WRK-Z12
                       MOVE    WRK-Z12     TO  HCM46-GSEIKYUPTNUM
                   END-IF
      *            世帯合計
                   IF      SUM-GOK-SEIKYU(5)   <   0
                       MOVE    SUM-GOK-SEIKYU(5)   TO  WRK-ZM12
                       MOVE    WRK-ZM12    TO  HCM46-GSEIKYUSETAI
                   ELSE
                       MOVE    SUM-GOK-SEIKYU(5)   TO  WRK-Z12
                       MOVE    WRK-Z12     TO  HCM46-GSEIKYUSETAI
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
      *    印刷処理
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-DENPPRTYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    "HCM46.red"     TO  SPRT-PRTID
           MOVE    "一部負担金給与控除一覧"
                                   TO  SPRT-TITLE
           MOVE    HCM46           TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       31013-GETUMATU-HEN-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"           TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD(1:6)  TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       31013-GETUMATU-HEN-EXT.
            EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
grpsys         MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
                                       FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF          CNT-PAGE    =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG015 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金区分情報読み込み
      *****************************************************************
       900-SYS1041-SELECT-SEC      SECTION.
      *
           INITIALIZE                  SYS-1041-REC
           MOVE    "1041"          TO  SYS-1041-KANRICD
           MOVE    WRK-SYSYMD      TO  SYS-1041-STYUKYMD
                                       SYS-1041-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1041-HOSPNUM
           MOVE    SYS-1041-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF        ( MCP-RC      =   ZERO )
               PERFORM 900-SYS1041-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
            .
       900-SYS1041-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金区分情報読み込み
      *****************************************************************
       900-SYS1041-FETCH-SEC       SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF        ( MCP-RC      =   ZERO )
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYS-1041-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
            .
       900-SYS1041-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金区分情報読み込み
      *****************************************************************
       900-SYS1041-CLOSE-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
            .
       900-SYS1041-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスタ読込
      *****************************************************************
       900-SYUMEI-SELECT-SEC       SECTION.
      *
           INITIALIZE                  SYUMEI-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  SMEI-HOSPID
           MOVE    WRK-PARA-DENPPRTYM
                                   TO  WRK-SYMD(1:6)
           MOVE    "01"            TO  WRK-SYMD(7:2)
           MOVE    WRK-SYMD        TO  SMEI-NYUHEN-YMD
           PERFORM 31013-GETUMATU-HEN-SEC
           MOVE    WRK-SYMD        TO  SMEI-UPNYUHEN-YMD
      *
grpsys     MOVE    SPA-HOSPNUM     TO  SMEI-HOSPNUM
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "st_key4"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-SYUMEI-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスタ読込
      *****************************************************************
       900-SYUMEI-FETCH-SEC        SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "st_key4"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYUMEI
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           .
       900-SYUMEI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスタ読込
      *****************************************************************
       900-SYUMEI-CLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "st_key4"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-SYUMEI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスタ読込
      *****************************************************************
       900-WRKSYUMEI-READ-SEC      SECTION.
      *
           INITIALIZE                  WRKSYUMEI-REC
grpsys*DD  MOVE    SMEI-HOSPID     TO  WRKSMEI-HOSPID
           MOVE    SMEI-NYUGAIKBN  TO  WRKSMEI-NYUGAIKBN
           MOVE    SMEI-PTID       TO  WRKSMEI-PTID
           MOVE    SMEI-DENPNUM    TO  WRKSMEI-DENPNUM
           MOVE    1               TO  WRKSMEI-DENPEDANUM
      *
grpsys     MOVE    SPA-HOSPNUM     TO  WRKSMEI-HOSPNUM
           MOVE    WRKSYUMEI-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syumei"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-WRKSMEI
                   MOVE    MCPDATA-REC     TO  WRKSYUMEI-REC
               ELSE
                   MOVE    1               TO  FLG-WRKSMEI
                   INITIALIZE                  WRKSYUMEI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-WRKSMEI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-WRKSYUMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ読込
      *****************************************************************
       900-SYUNOU-READ-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
grpsys*DD  MOVE    SMEI-HOSPID     TO  SYU-HOSPID
           MOVE    SMEI-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID       TO  SYU-PTID
           MOVE    SMEI-DENPNUM    TO  SYU-DENPNUM
      *
grpsys     MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYUNOU
                   MOVE    MCPDATA-REC     TO  SYUNOU-REC
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
                   INITIALIZE                  SYUNOU-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスタ読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           INITIALIZE                  PTINF-REC
grpsys*DD  MOVE    SMEI-HOSPID     TO  PTINF-HOSPID
           MOVE    SMEI-PTID       TO  PTINF-PTID
grpsys     MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTINF-REC       TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスタ読込
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           INITIALIZE                  PTNUM-REC
grpsys*    MOVE    SMEI-HOSPID     TO  PTNUM-HOSPID
           MOVE    SMEI-PTID       TO  PTNUM-PTID
grpsys     MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    PTNUM-REC       TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTNUM
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
               ELSE
                   MOVE    1               TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       900-PTHKNINF-READ-SEC       SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
grpsys*DD  MOVE    SMEI-HOSPID     TO  PTHKN-HOSPID
           MOVE    SMEI-PTID       TO  PTHKN-PTID
           MOVE    COMB-HKNID      TO  PTHKN-HKNID
grpsys     MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
     *****************************************************************
      *    保険組み合わせマスタ読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC       SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
grpsys*DD   MOVE    SMEI-HOSPID     TO  COMB-HOSPID
           MOVE    SMEI-PTID       TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM TO  WRK-94
           MOVE    WRK-94          TO  COMB-HKNCOMBINUM
grpsys     MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1           TO  FLG-TOKEI
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-BREAK
           MOVE    ZERO                TO  FLG-TOKEI
      *
           PERFORM UNTIL ( FLG-TOKEI   NOT =   ZERO )
                    OR   ( FLG-BREAK   NOT =   ZERO )
      *
               READ    TOKEI-FILE     NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   IF    ( TOKEI-SEIKYU    NOT =   ZERO )
                       MOVE    1       TO  FLG-BREAK
                   END-IF
               END-READ
      *
           END-PERFORM
      *
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
