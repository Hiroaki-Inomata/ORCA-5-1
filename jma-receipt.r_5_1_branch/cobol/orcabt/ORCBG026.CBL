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
       PROGRAM-ID.             ORCBG026.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 後発医薬品数量シェア（置き換え率）（月報）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/02/12    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.08.01    NACL-森脇     16/09/01  持参薬対象外
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計用ファイル
           SELECT  TOKEI-FILE          ASSIGN    TOKEIPARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  TOKEI-KEY
                                       FILE    STATUS  IS  STS-TOKEI.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-YKZKBN        PIC 9(01).
               05  TOKEI-YAKKAKJNCD    PIC X(12).
               05  TOKEI-NAME          PIC X(50).
               05  TOKEI-KOUHATU       PIC 9(01).
               05  TOKEI-SRYCD         PIC X(09).
           03  TOKEI-TANISU            PIC 9(04).
           03  TOKEI-TANINAME          PIC X(24).
           03  TOKEI-TUKI-G            OCCURS 3.
               05  TOKEI-SRYSURYO      PIC 9(10)V9(05).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCMG007.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-GECLASS             PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(03).
           03  CNT-TOKEI               PIC 9(05).
      *
           03  CNT-KETA                PIC 9(02).
           03  CNT-KETA2               PIC 9(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX-T                   PIC 9(01).
           03  IDX-S                   PIC 9(04).
           03  IDX-D                   PIC 9(02).
           03  IDX-W                   PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-SYORIYM        PIC X(06).
           03  WRK-PARA-MEISAIKBN      PIC 9(01).
           03  WRK-PARA-CSVKBN         PIC 9(01).
           03  WRK-PARA-TAISYOKBN      PIC 9(01).
           03  WRK-PARA-NYUGAIKBN      PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-TOUKEIERR           PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-TUKI-G.
               05  WRK-TUKI            PIC X(06)   OCCURS  3.
           03  WRK-KIKAKU-G.
               05  WRK-KIKAKU          PIC 9(07)   OCCURS  4.
           03  WRK-GOUKEI-G.
               05  WRK-KBN-ALL         PIC 9(07)   OCCURS  4.
               05  WRK-KBN-MIX         PIC 9(07)   OCCURS  4.
               05  WRK-KBN-KOU         PIC 9(07)   OCCURS  4.
      *
           03  WRK-PRTYMD              PIC X(22).
           03  WRK-KIKAN               PIC X(36).
           03  WRK-TAISYO              PIC X(16).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-TANISU              PIC 9(03).
           03  WRK-KOUHATU             PIC 9(01).
      *
           03  WRK-WARIAI              PIC 9(07).
           03  WRK-SURYO               PIC 9(10)V9(05).
      *
           03  WRK-ZZ                  PIC ZZZZZZZ.
           03  WRK-Z7                  PIC ZZZZZZ9.
           03  WRK-Z4                  PIC ZZZ9.
           03  WRK-Z3                  PIC ZZ9.
           03  WRK-Z2                  PIC Z9.
      *
      *    ＣＳＶデータ編集用
           03  WRK-REC             PIC X(20000).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(20000).
               05  WRK-NUM         PIC -------------9.99999. 
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-RECSTR      PIC 9(02). 
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-RECEND      PIC 9(01).
      *
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(18)   VALUE
                                                   "kouhatsuhin_ritsu_".
               05  WRK-TO-KIKAN-STRYM  PIC X(06).
               05  FILLER              PIC X(01)   VALUE   "-".
               05  WRK-TO-KIKAN-ENDYM  PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "薬剤区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "診療行為コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "後発区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "薬価基準コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "規格数量".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-TUKI-G                 OCCURS  3.
               05  CSV-HEAD-TUKI   PIC X(02)   VALUE   SPACE.
               05  FILLER          PIC X(03)   VALUE   "月,".
           03  FILLER              PIC X(04)   VALUE   "合計".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    医薬品分類マスタ
       01  GECLASS-REC.
           COPY    "CPGENERICCLASS.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    指定診療行為件数調サブ２
           COPY    "CPORCSS009.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
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
           INITIALIZE                  TTNS-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
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
                                       WRK-PARA-SYORIYM
                                       WRK-PARA-MEISAIKBN
                                       WRK-PARA-CSVKBN
                                       WRK-PARA-TAISYOKBN
                                       WRK-PARA-NYUGAIKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG026"          TO  JOB-PGID
           MOVE    "後発医薬品数量シェア（置換え率）"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "BG02601"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  TOKEIPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES(1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES(2)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(2)
                                       TO  RECEERR
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
           IF      WRK-PARA-SYORIYM    <   "201603"
               MOVE    "診療年月は平成２８年３月以降を指定してください"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-MEISAIKBN  NOT =   0 ) AND
                 ( WRK-PARA-MEISAIKBN  NOT =   1 )
               MOVE    "明細区分が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-CSVKBN     NOT =   0 ) AND
                 ( WRK-PARA-CSVKBN     NOT =   1 )
               MOVE    "ＣＳＶ区分が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-TAISYOKBN  NOT =   0 ) AND
                 ( WRK-PARA-TAISYOKBN  NOT =   1 ) AND
                 ( WRK-PARA-TAISYOKBN  NOT =   2 )
               MOVE    "対象区分が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-NYUGAIKBN  NOT =   0 ) AND
                 ( WRK-PARA-NYUGAIKBN  NOT =   1 ) AND
                 ( WRK-PARA-NYUGAIKBN  NOT =   2 )
               MOVE    "入外区分が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PRTYMD
      *
           MOVE    SPACE               TO  WRK-TUKI-G
      *    対象年月編集（開始年月は２カ月前）
           MOVE    WRK-PARA-SYORIYM    TO  WRK-SYMD(1:6)
           PERFORM VARYING IDX-T   FROM    3   BY  -1
                   UNTIL   IDX-T   =   0
               MOVE    WRK-SYMD(1:6)   TO  WRK-TUKI(IDX-T)
               MOVE    WRK-SMM         TO  WRK-Z2
               MOVE    WRK-Z2          TO  CSV-HEAD-TUKI(IDX-T)
      *
               IF      WRK-SMM     =   1
                   COMPUTE WRK-SYY =   WRK-SYY -   1
                   MOVE    12          TO  WRK-SMM
               ELSE
                   COMPUTE WRK-SMM =   WRK-SMM -   1
               END-IF
           END-PERFORM
      *
           MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
      *
      *    開始日
           MOVE    WRK-TUKI(1)         TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-KIKAN(1:16)
           MOVE    "〜"                TO  WRK-KIKAN(17:2)
      *    終了日
           MOVE    WRK-TUKI(3)         TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-KIKAN(19:16)
           MOVE    "分"                TO  WRK-KIKAN(35:2)
           MOVE    WRK-TUKI(1)         TO  WRK-TO-KIKAN-STRYM
           MOVE    WRK-TUKI(3)         TO  WRK-TO-KIKAN-ENDYM
      *
           EVALUATE    WRK-PARA-TAISYOKBN
               WHEN    0
                   MOVE    "全て"              TO  WRK-TAISYO
               WHEN    1
                   MOVE    "投薬"              TO  WRK-TAISYO
               WHEN    2
                   MOVE    "在・投・注"        TO  WRK-TAISYO
           END-EVALUATE
      *
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    0
                   MOVE    "入外"              TO  WRK-TAISYO(13:4)
               WHEN    1
                   MOVE    "入院"              TO  WRK-TAISYO(13:4)
               WHEN    2
                   MOVE    "外来"              TO  WRK-TAISYO(13:4)
           END-EVALUATE
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
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           OPEN    I-O                 TOKEI-FILE
      *
      *    診療行為集計処理
           PERFORM 220-MAIN-SRYACT-SEC
      *
           CLOSE                       TOKEI-FILE
      *
           IF      CNT-TOKEI       =   ZERO
               GO  TO  200-MAIN-EXT
           END-IF
      *
           OPEN    INPUT               TOKEI-FILE
      *
      *    統計出力処理
           PERFORM 260-TOKEI-OUT-SEC
      *
           CLOSE                       TOKEI-FILE
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為集計処理
      *****************************************************************
       220-MAIN-SRYACT-SEC         SECTION.
      *
      *    診療行為読み込み
           PERFORM 900-SRYACT-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SRYACT  =   1
               IF    (WRK-PARA-NYUGAIKBN   =   0)   OR
                    ((WRK-PARA-NYUGAIKBN   =   1 )  AND
                     (SRY-NYUGAIKBN        =  "1")) OR
                    ((WRK-PARA-NYUGAIKBN   =   2 )  AND
                     (SRY-NYUGAIKBN        =  "2"))
                 EVALUATE    WRK-PARA-TAISYOKBN
      *              全て
                     WHEN    0
      *                  診療会計集計処理
                         PERFORM 230-MAIN-SRYACCT-SEC
      *              投薬
                     WHEN    1
                         IF      SRY-SRYKBN(1:1)     =  "2"
      *                      診療会計集計処理
                             PERFORM 230-MAIN-SRYACCT-SEC
                         END-IF
      *              在宅、投薬、注射
                     WHEN    2
                         IF    ( SRY-SRYKBN(1:1)     =  "2" ) OR
                               ( SRY-SRYKBN(1:1)     =  "3" ) OR
                               ( SRY-SRYKBN          =  "14" )
      *                      診療会計集計処理
                             PERFORM 230-MAIN-SRYACCT-SEC
                         END-IF
                 END-EVALUATE
               END-IF
      *        診療行為読み込み
               PERFORM 900-SRYACT-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
            .
       220-MAIN-SRYACT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計集計処理
      *****************************************************************
       230-MAIN-SRYACCT-SEC           SECTION.
      *
      *    診療会計読み込み
           PERFORM 900-SRYACCT-READ-SEC
           IF      FLG-SRYACCT        =   1
               GO  TO  230-MAIN-SRYACCT-EXT
           END-IF
      *
      *----(04.08.01)--ADD-START---
           IF  ( ACCT-JIHOKBN        = "1" )   AND
               ( ACCT-TEIGENRATE     =  3 OR 4 )
               GO  TO  230-MAIN-SRYACCT-EXT
           END-IF
      *----(04.08.01)--ADD-END-----
      *
           IF      ACCT-HKNCOMBI   NOT =   9999
      *        保険組合せ読み込み
               PERFORM 900-HKNCOMBI-READ-SEC
               IF      FLG-HKNCOMBI       =   1
                   GO  TO  230-MAIN-SRYACCT-EXT
               END-IF
           END-IF
      *
      *    院内チェック
           MOVE   ZERO             TO  FLG-OK
           IF    ( ACCT-ZAIKBN     NOT = 1 )   AND
                 ( ACCT-ZAIKBN     NOT = 2 )
               IF    ( ACCT-ZAITEN         =   ZERO )  AND
                    (( ACCT-SRYKBN         =   "21" )  OR
                     ( ACCT-SRYKBN         =   "22" )  OR
                     ( ACCT-SRYKBN         =   "23" )  OR
                     ( ACCT-SRYKBN         =   "14" ))
                   IF    ( SRY-SRYSYUKBN       =   "213")  OR
                         ( SRY-SRYSYUKBN       =   "223")  OR
                         ( SRY-SRYSYUKBN       =   "233")
                       MOVE    1           TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-OK
               END-IF
      *        院内分のみ対象
               IF    ( FLG-OK          =   1 )
      *            主編集処理
                   PERFORM 240-MAIN-HEN-SEC
               END-IF
           END-IF
      *
           .
       230-MAIN-SRYACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主編集処理
      *****************************************************************
       240-MAIN-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX-S   FROM    1   BY  1
                   UNTIL ( IDX-S   >   5 )
                   OR    ( SRY-SRYCD(IDX-S)    =   SPACE )
      *        薬剤を対象とする
               IF    ( SRY-SRYCD(IDX-S)(1:1)   =   "6" )
                   PERFORM 250-TOKEI-HEN-SEC 
               END-IF
           END-PERFORM
      *
            .
       240-MAIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計編集処理
      *****************************************************************
       250-TOKEI-HEN-SEC           SECTION.
      *
      *    対象外コード
           IF    ( SRY-SRYCD(IDX-S)    =   "620754301" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "620754601" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "613250027" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "610407025" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622071001" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622070901" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622346201" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622307001" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "621490901" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "621491001" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622676401" )
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
           MOVE    SRY-SRYCD(IDX-S)    TO  WRK-SRYCD
      *
      *    点数マスタ検索
           PERFORM 900-TNS-READ-SEC
      *
      *    対象外分類
           IF    ( TNS-YAKKAKJNCD(1:3) =   "510" ) OR
                 ( TNS-YAKKAKJNCD(1:3) =   "520" ) OR
                 ( TNS-YAKKAKJNCD(1:3) =   "590" )
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
      *    点数がない場合は対象外
           IF      TNS-TEN             =   ZERO
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
      *    単位数
           IF      TNS-SSTKIJUNCD(1)   =   ZERO
               MOVE    1                   TO  WRK-TANISU
           ELSE
               MOVE    TNS-SSTKIJUNCD(1)   TO  WRK-TANISU
           END-IF
      *
      *    対象月
           IF      SRY-SRYYM       =   WRK-TUKI(1)
               MOVE    1           TO  IDX-T
           END-IF
           IF      SRY-SRYYM       =   WRK-TUKI(2)
               MOVE    2           TO  IDX-T
           END-IF
           IF      SRY-SRYYM       =   WRK-TUKI(3)
               MOVE    3           TO  IDX-T
           END-IF
      *
           PERFORM VARYING IDX-D FROM  1   BY  1
                   UNTIL   IDX-D >     31
               IF      ACCT-DAY(1 IDX-D)   NOT =   ZERO
      *            医薬品分類マスタ検索
                   PERFORM 900-GECLASS-READ-SEC
                   IF      FLG-GECLASS     =   ZERO
                       MOVE    GECLASS-KOUHATU TO  WRK-KOUHATU
                   ELSE
                       MOVE    ZERO            TO  WRK-KOUHATU
                   END-IF
      *            統計セット処理
                   PERFORM 250-TOKEI-SET-SEC
               END-IF
           END-PERFORM
      *
           .
       250-TOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       250-TOKEI-SET-SEC           SECTION.
      *
           COMPUTE WRK-SURYO       =   SRY-SRYSURYO(IDX-S)
                                   *   ACCT-DAY(1 IDX-D)
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
           MOVE    TNS-YAKKAKJNCD  TO  TOKEI-YAKKAKJNCD
           MOVE    TNS-NAME        TO  TOKEI-NAME
           MOVE    WRK-KOUHATU     TO  TOKEI-KOUHATU
           MOVE    WRK-SRYCD       TO  TOKEI-SRYCD
           PERFORM 900-TOKEI-READ-SEC
      *
           IF      FLG-TOKEI       =   1
               MOVE    TNS-TANINAME    TO  TOKEI-TANINAME
               MOVE    WRK-TANISU      TO  TOKEI-TANISU
               MOVE    ZERO            TO  TOKEI-SRYSURYO(1)
                                           TOKEI-SRYSURYO(2)
                                           TOKEI-SRYSURYO(3)
               MOVE    WRK-SURYO       TO  TOKEI-SRYSURYO(IDX-T)
               WRITE   TOKEI-REC
               COMPUTE CNT-TOKEI       =   CNT-TOKEI   +   1
           ELSE
               COMPUTE TOKEI-SRYSURYO(IDX-T)
                   =   TOKEI-SRYSURYO(IDX-T)   +   WRK-SURYO
               REWRITE TOKEI-REC
           END-IF
      *
           .
       250-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       260-TOKEI-OUT-SEC           SECTION.
      *
      *    薬剤区分毎に印刷する
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
           MOVE    ZERO            TO  WRK-GOUKEI-G
           MOVE    ZERO            TO  WRK-KIKAKU-G
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
      *    見出編集
           PERFORM 310-PRT-HEAD-SET-SEC
           PERFORM 410-CSV-HEAD-SET-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI           =   1  )
      *        統計規格処理
               PERFORM 260-TOKEI-KIKAKU-SEC
      *        明細編集
               PERFORM 320-PRT-BODY-SET-SEC
               PERFORM 420-CSV-BODY-SET-SEC
      *        統計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    割合編集
           PERFORM 320-PRT-WARIAI-SET-SEC
      *
             .
       260-TOKEI-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計規格処理
      *****************************************************************
       260-TOKEI-KIKAKU-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-KIKAKU-G
      *
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   3
      *        単位数量
               COMPUTE WRK-KIKAKU(IDX-T)   =   TOKEI-SRYSURYO(IDX-T)
                                           /   TOKEI-TANISU
               COMPUTE WRK-KIKAKU(4)       =   WRK-KIKAKU(4)
                                           +   WRK-KIKAKU(IDX-T)
      *        全体
               COMPUTE WRK-KBN-ALL(IDX-T)
                   =   WRK-KBN-ALL(IDX-T)  +   WRK-KIKAKU(IDX-T)
               COMPUTE WRK-KBN-ALL(4)
                   =   WRK-KBN-ALL(4)      +   WRK-KIKAKU(IDX-T)
      *        先発品と後発品
               IF    ( TOKEI-KOUHATU   =   2 )   OR
                     ( TOKEI-KOUHATU   =   3 )
                    COMPUTE WRK-KBN-MIX(IDX-T)
                       =   WRK-KBN-MIX(IDX-T)  +   WRK-KIKAKU(IDX-T)
                   COMPUTE WRK-KBN-MIX(4)
                       =   WRK-KBN-MIX(4)      +   WRK-KIKAKU(IDX-T)
               END-IF
      *        後発品のみ
               IF      TOKEI-KOUHATU   =   3
                   COMPUTE WRK-KBN-KOU(IDX-T)
                       =   WRK-KBN-KOU(IDX-T)  +   WRK-KIKAKU(IDX-T)
                   COMPUTE WRK-KBN-KOU(4)
                       =   WRK-KBN-KOU(4)      +   WRK-KIKAKU(IDX-T)
               END-IF
           END-PERFORM
      *
           .
       260-TOKEI-KIKAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理見出し
      *****************************************************************
       310-PRT-HEAD-SET-SEC        SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    SPACE           TO  HCMG007
      *
           MOVE    WRK-PRTYMD      TO  HCMG007-PRTYMD
      *
           MOVE    CNT-PAGE        TO  WRK-Z3
           MOVE    WRK-Z3          TO  HCMG007-PAGE
      *
           MOVE    WRK-KIKAN       TO  HCMG007-KIKAN
           MOVE    WRK-TAISYO      TO  HCMG007-TAISYO
      *
           MOVE    CSV-HEAD-TUKI(1)    TO  HCMG007-LBL(1)
           MOVE    CSV-HEAD-TUKI(2)    TO  HCMG007-LBL(2)
           MOVE    CSV-HEAD-TUKI(3)    TO  HCMG007-LBL(3)
      *
           .
       310-PRT-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理明細
      *****************************************************************
       320-PRT-BODY-SET-SEC        SECTION.
      *
           IF      WRK-PARA-MEISAIKBN  =   0
               GO  TO  320-PRT-BODY-SET-EXT
           END-IF
      *
           IF      CNT-LINE        =   51
      *        印刷処理
               PERFORM 330-PRINT-OUT-SEC
      *        見出編集
               PERFORM 310-PRT-HEAD-SET-SEC
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           EVALUATE    TOKEI-YKZKBN
               WHEN    "1"
                   MOVE    "内"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "2"
                   MOVE    "他"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "4"
                   MOVE    "注"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "6"
                   MOVE    "外"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "8"
                   MOVE    "歯"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "9"
                   MOVE    "特"        TO  HCMG007-YKZKBN(CNT-LINE)
           END-EVALUATE
      *
           MOVE    TOKEI-SRYCD     TO  HCMG007-SRYCD(CNT-LINE)
           MOVE    TOKEI-NAME      TO  HCMG007-NAME(CNT-LINE)
           MOVE    TOKEI-KOUHATU   TO  HCMG007-KOUHATU(CNT-LINE)
           MOVE    TOKEI-YAKKAKJNCD
                                   TO  HCMG007-YAKKAKJNCD(CNT-LINE)
      *
           MOVE    TOKEI-TANISU    TO  WRK-Z4
           MOVE    WRK-Z4          TO  HCMG007-TANISU(CNT-LINE)
      *
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-KIKAKU(IDX-T)   TO  WRK-ZZ
               MOVE    WRK-ZZ      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
           .
       320-PRT-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理割合
      *****************************************************************
       320-PRT-WARIAI-SET-SEC      SECTION.
      *
           IF      WRK-PARA-MEISAIKBN  =   1
      *        印刷処理
               PERFORM 330-PRINT-OUT-SEC
      *        見出編集
               PERFORM 310-PRT-HEAD-SET-SEC
           END-IF
      *
      *    全体
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    "全医薬品の規格単位数量　　　　　　　…（１）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-KBN-ALL(IDX-T)   TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    先発品と後発品
           COMPUTE CNT-LINE    =   CNT-LINE    +   2
           MOVE    "後発医薬品あり先発医薬品"
                                       TO  HCMG007-NAME(CNT-LINE)
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    "及び後発医薬品の規格単位数量　　　　…（２）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-KBN-MIX(IDX-T)  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    後発品
           COMPUTE CNT-LINE    =   CNT-LINE    +   2
           MOVE    "後発医薬品の規格単位数量　　　　　　…（３）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-KBN-KOU(IDX-T)  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    置換え率
           COMPUTE CNT-LINE    =   CNT-LINE    +   2
           MOVE    "カットオフ値の割合（２／１）（％）　…（４）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    ZERO        TO  WRK-WARIAI
               IF    ( WRK-KBN-ALL(IDX-T)  >   ZERO ) AND
                     ( WRK-KBN-MIX(IDX-T)  >   ZERO )
                   COMPUTE WRK-WARIAI
                       = ( WRK-KBN-MIX(IDX-T)  /   WRK-KBN-ALL(IDX-T) )
                       *   100
               END-IF
               MOVE    WRK-WARIAI  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    置換え率
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    "後発医薬品の割合　（３／２）（％）　…（５）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    ZERO        TO  WRK-WARIAI
               IF    ( WRK-KBN-MIX(IDX-T)  >   ZERO ) AND
                     ( WRK-KBN-KOU(IDX-T)  >   ZERO )
                   COMPUTE WRK-WARIAI
                       = ( WRK-KBN-KOU(IDX-T)  /   WRK-KBN-MIX(IDX-T) )
                       *   100
               END-IF
               MOVE    WRK-WARIAI  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    印刷処理
           PERFORM 330-PRINT-OUT-SEC
      *
           .
       320-PRT-WARIAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SYORIYM    TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMG007.red"       TO  SPRT-PRTID
           MOVE    "後発医薬品数量シェア（置換え率）"
                                       TO  SPRT-TITLE
           MOVE    HCMG007             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN     =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       410-CSV-HEAD-SET-SEC        SECTION.
      *
           IF      WRK-PARA-CSVKBN     =   ZERO
               GO  TO  410-CSV-HEAD-SET-EXT
           END-IF
      *
           INITIALIZE                  WRK-REC
      *    改行コード編集
           STRING  CSV-HEAD-01         DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
           PERFORM 430-TOUKEICSV-SEC
      *
           .
       410-CSV-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       420-CSV-BODY-SET-SEC        SECTION.
      *
           IF      WRK-PARA-CSVKBN     =   ZERO
               GO  TO  420-CSV-BODY-SET-EXT
           END-IF
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    薬剤区分
           MOVE    TOKEI-YKZKBN    TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    診療行為コード
           MOVE    TOKEI-SRYCD     TO  WRK-DATA
           MOVE    9               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    薬剤名称
           MOVE    TOKEI-NAME      TO  WRK-DATA
           MOVE    50              TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    後発区分
           MOVE    TOKEI-KOUHATU   TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    薬価基準コード
           MOVE    TOKEI-YAKKAKJNCD    TO  WRK-DATA
           MOVE    12              TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    規格単位
           MOVE    TOKEI-TANISU    TO  WRK-DATA
           MOVE    4               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    規格単位数量
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
      *        単位数量
               MOVE    WRK-KIKAKU(IDX-T)   TO  WRK-DATA
               MOVE    7               TO  WRK-NAGASA
               PERFORM 800-MOJI-HENSYU-SEC
           END-PERFORM
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           PERFORM 430-TOUKEICSV-SEC
      *
           .
       420-CSV-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       430-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                      ORCSTOUKEICSVAREA
           MOVE    "INS"               TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  STOUKEICSV-SHELLID
           MOVE    WRK-PARA-SYORIYM    TO  STOUKEICSV-SRYYM
           MOVE    SPACE               TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC             TO  STOUKEICSV-CSVDATA
           MOVE    "後発医薬品数量シェア（置換え率）ＣＳＶ作成"
                                       TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"         USING
                                           ORCSTOUKEICSVAREA
                                           SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       430-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       800-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
               CONTINUE
           ELSE                   
               PERFORM VARYING IDX-W   FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDX-W   <       1
                   IF      WRK-DATA (IDX-W:1)  NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                   DELIMITED   BY  SIZE
                               WRK-DATA(1:IDX-W)   DELIMITED   BY  SIZE
                               INTO    WRK-REC
                       END-STRING
                       ADD     IDX-W       TO  WRK-REC-LENGTH        
                       MOVE    1           TO  IDX-W
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-RECEND         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           MOVE    ZERO                TO  WRK-NUM    
      *
           .
       800-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF      STS-RECEERR     =   ZERO
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
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG026 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           IF      CNT-TOKEI       =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG026 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG026 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
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
      *
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC          SECTION.
      *
           READ    TOKEI-FILE      NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
      *
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBSELECT-SEC     SECTION.
      *
      *    対象診療年月で検索
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    WRK-TUKI(1)     TO  SRY-SRYYM
           MOVE    WRK-TUKI(3)     TO  SRY-CREYMD
           MOVE    "%"             TO  SRY-NYUGAIKBN
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBFETCH-SEC      SECTION.
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-READ-SEC        SECTION.
      *
      *    該当の診療会計データを検索
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID        TO  ACCT-PTID
           MOVE    SRY-SRYKA       TO  ACCT-SRYKA
           MOVE    SRY-SRYYM       TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN      TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ検索
      *****************************************************************
       900-HKNCOMBI-READ-SEC        SECTION.
      *
      *    該当の診療会計データを検索
           INITIALIZE                  HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    ACCT-PTID       TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-HKNCOMBI
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
      *
               IF    ( COMB-HKNNUM(1:2)    =   "98" ) OR
                     ( COMB-HKNNUM     =   "975" ) OR
                     ( COMB-HKNNUM     =   "973" ) OR
                     ( COMB-HKNNUM     =   "971" )
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索
      *****************************************************************
       900-TNS-READ-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    SRY-SRYYM           TO  ORC-DBYMD(1:6)
           MOVE    "01"                TO  ORC-DBYMD(7:2)
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    ORC-DBYMD           TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF      TTNS-FLG        =   ZERO
               INITIALIZE                  TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD  TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-TENSU
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-TNS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  TTNS-FLG
           INITIALIZE                  TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                   OR    ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD(TTNS-IDX)
                                               =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD(TTNS-IDX)
                                               <=  TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD(TTNS-IDX)
                                               >=  TTNS-CHK-YMD )
                   MOVE    1                   TO  TTNS-FLG
                   MOVE    TTNS-DAT(TTNS-IDX)  TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC           SECTION.
      *
           IF      TTNS-CONST-MAX  >   TTNS-MAX
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD(TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD(TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD(TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT(TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医薬品分類マスタ検索
      *****************************************************************
       900-GECLASS-READ-SEC        SECTION.
      *
           INITIALIZE                  GECLASS-REC
           MOVE    TNS-YAKKAKJNCD  TO  GECLASS-YAKKAKJNCD
           MOVE    SRY-SRYYM       TO  GECLASS-YUKOSTYMD(1:6)
           MOVE    IDX-D           TO  GECLASS-YUKOSTYMD(7:2)
           MOVE    SRY-SRYYM       TO  GECLASS-YUKOEDYMD(1:6)
           MOVE    IDX-D           TO  GECLASS-YUKOEDYMD(7:2)
           MOVE    GECLASS-REC     TO  MCPDATA-REC
           MOVE    "tbl_generic_class"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  GECLASS-REC
               MOVE    ZERO            TO  FLG-GECLASS
           ELSE
               MOVE    1               TO  FLG-GECLASS
           END-IF
      *
           MOVE    "tbl_generic_class"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-GECLASS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-DBFETCH-SEC
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       900-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCLOSECURSOR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
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
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
