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
       PROGRAM-ID.             ORCBG029.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 診療区分別指定点数該当患者数調べ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/08/18    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
               05  TOKEI-NYUGAIKBN     PIC X(01).
               05  TOKEI-SRYKBN        PIC X(02).
               05  TOKEI-KANANAME      PIC X(50).
               05  TOKEI-PTNUM         PIC X(20).
               05  TOKEI-TEN           PIC 9(10).
               05  TOKEI-SRYCD         PIC X(09).
               05  TOKEI-SRYYM         PIC X(06).
           03  TOKEI-FLG               PIC X(01)   OCCURS 31.
           03  TOKEI-NAME              PIC X(20).
           03  TOKEI-NAIYO             PIC X(56).
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
           COPY    "HCMSL80.INC".
           COPY    "HCMSL55.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-LINE                PIC 9(02).
           03  CNT-TOKEI               PIC 9(05).
           03  CNT-NAIYO               PIC 9(05).
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
           03  IDX                     PIC 9(05).
           03  IDX-D                   PIC 9(02).
           03  IDX-K                   PIC 9(02).
           03  IDX-S                   PIC 9(02).
           03  IDX-Z                   PIC 9(02).
           03  IDX-G                   PIC 9(01).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-STRYM          PIC X(06).
           03  WRK-PARA-ENDYM          PIC X(06).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-IJYOU          PIC 9(10).
           03  WRK-PARA-IKA            PIC 9(10).
           03  WRK-PARA-SYORIKBN       PIC X(01).
           03  WRK-PARA-SRYKBN-G       OCCURS 3.
               05  WRK-PARA-SRYKBN     PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-HENYMD              PIC X(09).
      *
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-PTID                PIC 9(10).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-ZAITEN              PIC 9(10).
           03  WRK-SRYYM               PIC X(06).
           03  WRK-SRYCD               PIC X(09).
      *
           03  WRK-NAIYO-G.
               05  WRK-NAIYO-SRYCD     PIC X(09)   OCCURS 3.
               05  WRK-NAIYO-NAME      PIC X(56)   OCCURS 3.
      *
           03  WRK-SRYYM-G.
               05  WRK-SRYYM-TBL                   OCCURS 31.
                   07  FILLER          PIC X(01).
                   07  WRK-SRYYM-D     PIC X(01).
      *
           03  WRK-GOKEI-G.
               05  WRK-GOKEI-NYUGAIKBN PIC X(01)   OCCURS 6.
               05  WRK-GOKEI-SRYKBN    PIC X(02)   OCCURS 6.
               05  WRK-GOKEI-PTCNT     PIC 9(10)   OCCURS 6.
      *
           03  WRK-PTCNT               PIC 9(10).
           03  WRK-PTNUM               PIC X(20).
      *
           03  WRK-STRYM               PIC X(16).
           03  WRK-ENDYM               PIC X(16).
           03  WRK-IJYOU               PIC X(10).
           03  WRK-IKA                 PIC X(10).
           03  WRK-TANI                PIC X(02).
      *
           03  WRK-Z10                 PIC ZZZZZZZZZ9.
           03  WRK-Z03                 PIC ZZ9.
      *
           03  WRK-HEN-TEN-G.
               05  WRK-HEN-TEN-IN      PIC X(10).
               05  WRK-HEN-TEN-OUT     PIC X(10).
      *
       01  WRK-PRT-AREA.
      *
      *    明細頁ページ見出し
           03  WRK-PRT-LINE1.
               05  FILLER              PIC X(60)   VALUE   SPACE.
               05  FILLER              PIC X(30)   VALUE
                   "診療区分別指定点数該当患者一覧".
               05  WRK-PRT-NYUGAIKBN   PIC X(08)   VALUE   SPACE.
           03  WRK-PRT-LINE2.
               05  FILLER              PIC X(10)   VALUE   "診療区分：".
               05  WRK-PRT-SRYKBN      PIC X(10)   VALUE   SPACE.
           03  WRK-PRT-LINE3.
               05  FILLER              PIC X(10)   VALUE   "指定条件：".
               05  WRK-PRT-JYOKEN      PIC X(30)   VALUE   SPACE.
               05  FILLER              PIC X(112)  VALUE   SPACE.
               05  WRK-PRT-PAGE        PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  WRK-PRT-LINE4.
               05  FILLER              PIC X(10)   VALUE   "診療年月：".
               05  WRK-PRT-KIKAN       PIC X(36)   VALUE   SPACE.
               05  FILLER              PIC X(82)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE   "作成日：".
               05  WRK-PRT-PRTYMD      PIC X(22)   VALUE   SPACE.
           03  WRK-PRT-LINE5.
               05  FILLER              PIC X(10)   VALUE   "患者番号".
               05  FILLER              PIC X(20)   VALUE   "患者氏名".
               05  FILLER              PIC X(44)   VALUE   "診療内容".
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE   "剤点数".
               05  FILLER              PIC X(08)   VALUE   "診療月".
               05  FILLER              PIC X(48)   VALUE
                   "01      05        10        15        20        ".
               05  FILLER              PIC X(14)   VALUE
                   "25        30  ".
           03  WRK-PRT-SEN.
               05  FILLER              PIC X(48)   VALUE
                   "------------------------------------------------".
               05  FILLER              PIC X(48)   VALUE
                   "------------------------------------------------".
               05  FILLER              PIC X(48)   VALUE
                   "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+".
               05  FILLER              PIC X(14)   VALUE
                   "-+-+-+-+-+-+-+".
           03  WRK-PRT-MEISAI.
               05  WRK-PRT-PTNUM       PIC X(10)   VALUE   SPACE.
               05  WRK-PRT-NAME        PIC X(20)   VALUE   SPACE.
               05  WRK-PRT-NAIYO       PIC X(44)   VALUE   SPACE.
               05  WRK-PRT-TEN         PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "点".
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  WRK-PRT-SRYYM       PIC X(06)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  WRK-PRT-SRYYM-G     PIC X(62)   VALUE   SPACE.
           03  WRK-PRT-GOKEI.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(10)   VALUE   "該当患者数".
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  WRK-PRT-NINZU       PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "人".
      *    合計ページ見出し
           03  WRK-PRT-G-LINE1.
               05  FILLER              PIC X(38)   VALUE   SPACE.
               05  FILLER              PIC X(32)   VALUE
                   "診療区分別指定点数該当患者数調べ".
           03  WRK-PRT-G-LINE3.
               05  FILLER              PIC X(10)   VALUE   "指定条件：".
               05  WRK-PRT-G-JYOKEN    PIC X(30)   VALUE   SPACE.
               05  FILLER              PIC X(64)   VALUE   SPACE.
               05  WRK-PRT-G-PAGE      PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  WRK-PRT-G-LINE4.
               05  FILLER              PIC X(10)   VALUE   "診療年月：".
               05  WRK-PRT-G-KIKAN     PIC X(36)   VALUE   SPACE.
               05  FILLER              PIC X(34)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE   "作成日：".
               05  WRK-PRT-G-PRTYMD    PIC X(22)   VALUE   SPACE.
           03  WRK-PRT-G-LINE5.
               05  FILLER              PIC X(10)   VALUE   "入外区分".
               05  FILLER              PIC X(10)   VALUE   "診療区分".
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(16)   VALUE
                   "剤点数該当患者数".
           03  WRK-PRT-G-SEN.
               05  FILLER              PIC X(38)   VALUE
                   "--------------------------------------".
           03  WRK-PRT-G-MEISAI.
               05  WRK-PRT-G-NYUGAIKBN PIC X(10)   VALUE   SPACE.
               05  WRK-PRT-G-SRYKBN    PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  WRK-PRT-G-NINZU     PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "人".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
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
                                       WRK-PARA-STRYM
                                       WRK-PARA-ENDYM
                                       WRK-PARA-NYUGAIKBN
                                       WRK-PARA-IJYOU
                                       WRK-PARA-IKA
                                       WRK-PARA-SYORIKBN
                                       WRK-PARA-SRYKBN(1)
                                       WRK-PARA-SRYKBN(2)
                                       WRK-PARA-SRYKBN(3)
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
           MOVE    "ORCBG029"          TO  JOB-PGID
           MOVE    "診療区分別指定点数該当患者数調べ"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "BG02901"           TO  TOKEIPARA-FILE-ID
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
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    期間チェック
           IF      WRK-PARA-ENDYM      =   SPACE
               MOVE    WRK-PARA-STRYM      TO  WRK-PARA-ENDYM
           ELSE
               IF      WRK-PARA-STRYM  >   WRK-PARA-ENDYM
                   MOVE    "日付の範囲が正しくありません"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  110-PARA-HENSYU-EXT
               END-IF
           END-IF
      *
      *    点数チェック
           IF    ( WRK-PARA-IJYOU  =   ZERO )  AND
                 ( WRK-PARA-IKA    =   ZERO )
               MOVE    "点数を入力してください"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    診療区分チェック
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL   IDX >   3
               IF    ( WRK-PARA-SRYKBN(IDX)  NOT =   SPACE ) AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "11" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "12" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "13" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "14" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "20" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "30" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "40" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "50" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "54" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "60" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "70" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "80" )  AND
                     ( WRK-PARA-SRYKBN(IDX)  NOT =   "90" )
                   MOVE    3               TO  IDX
                   MOVE    "正しい診療区分を入力してください"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  110-PARA-HENSYU-EXT
               END-IF
           END-PERFORM
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PRT-PRTYMD
      *
      *    入外区分セット
           IF    ( WRK-PARA-NYUGAIKBN  NOT =   "1" )   AND
                 ( WRK-PARA-NYUGAIKBN  NOT =   "2" )
               MOVE    "%"             TO  WRK-PARA-NYUGAIKBN
           END-IF
      *
      *    印刷区分セット
           IF    ( WRK-PARA-SYORIKBN   NOT =   "1" )
               MOVE    "0"             TO  WRK-PARA-SYORIKBN
           END-IF
      *
      *    下限点数セット
           IF      WRK-PARA-IJYOU  NOT =   ZERO
               MOVE    WRK-PARA-IJYOU  TO  WRK-Z10
               MOVE    WRK-Z10         TO  WRK-HEN-TEN-IN
               PERFORM 800-TENSU-HEN-SEC
               MOVE    WRK-HEN-TEN-OUT TO  WRK-IJYOU
           END-IF
      *
      *    上限点数セット
           IF      WRK-PARA-IKA    NOT =   ZERO
               MOVE    WRK-PARA-IKA    TO  WRK-Z10
               MOVE    WRK-Z10         TO  WRK-HEN-TEN-IN
               PERFORM 800-TENSU-HEN-SEC
               MOVE    WRK-HEN-TEN-OUT TO  WRK-IKA
           END-IF
      *
      *    点数条件セット
           IF    ( WRK-IJYOU       NOT =   SPACE ) AND
                 ( WRK-IKA         NOT =   SPACE )
               STRING  WRK-IJYOU           DELIMITED BY SPACE
                       "点以上〜"          DELIMITED BY SIZE
                       WRK-IKA             DELIMITED BY SPACE
                       "点以下"            DELIMITED BY SIZE
                                           INTO    WRK-PRT-JYOKEN
               END-STRING
           ELSE
               IF    ( WRK-IJYOU       NOT =   SPACE )
                   STRING  WRK-IJYOU           DELIMITED BY SPACE
                           "点以上"            DELIMITED BY SIZE
                                               INTO    WRK-PRT-JYOKEN
                   END-STRING
               END-IF
               IF    ( WRK-IKA         NOT =   SPACE )
                   STRING  WRK-IKA             DELIMITED BY SPACE
                           "点以下"            DELIMITED BY SIZE
                                               INTO    WRK-PRT-JYOKEN
                   END-STRING
               END-IF
           END-IF
      *
      *    開始月セット
           MOVE    WRK-PARA-STRYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-STRYM
      *
      *    終了月セット
           MOVE    WRK-PARA-ENDYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-ENDYM
      *
      *    対象期間セット
           IF    ( WRK-STRYM       NOT =   WRK-ENDYM )
               STRING  WRK-STRYM           DELIMITED BY SPACE
                       "〜"                DELIMITED BY SIZE
                       WRK-ENDYM           DELIMITED BY SPACE
                                           INTO    WRK-PRT-KIKAN
               END-STRING
           ELSE
               MOVE    WRK-STRYM           TO  WRK-PRT-KIKAN
           END-IF
      *
      *    合計ページ項目セット
           IF    ( WRK-PARA-NYUGAIKBN NOT =   "2" )
               MOVE    "1"                TO  WRK-GOKEI-NYUGAIKBN(1)
                                              WRK-GOKEI-NYUGAIKBN(2)
                                              WRK-GOKEI-NYUGAIKBN(3)
               MOVE    WRK-PARA-SRYKBN(1) TO  WRK-GOKEI-SRYKBN(1)
               MOVE    WRK-PARA-SRYKBN(2) TO  WRK-GOKEI-SRYKBN(2)
               MOVE    WRK-PARA-SRYKBN(3) TO  WRK-GOKEI-SRYKBN(3)
           END-IF
           IF    ( WRK-PARA-NYUGAIKBN NOT =   "1" )
               MOVE    "2"                TO  WRK-GOKEI-NYUGAIKBN(4)
                                              WRK-GOKEI-NYUGAIKBN(5)
                                              WRK-GOKEI-NYUGAIKBN(6)
               MOVE    WRK-PARA-SRYKBN(1) TO  WRK-GOKEI-SRYKBN(4)
               MOVE    WRK-PARA-SRYKBN(2) TO  WRK-GOKEI-SRYKBN(5)
               MOVE    WRK-PARA-SRYKBN(3) TO  WRK-GOKEI-SRYKBN(6)
           END-IF
      *
           MOVE    WRK-PRT-KIKAN   TO  WRK-PRT-G-KIKAN
           MOVE    WRK-PRT-JYOKEN  TO  WRK-PRT-G-JYOKEN
           MOVE    WRK-PRT-PRTYMD  TO  WRK-PRT-G-PRTYMD
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
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
      *    診療会計集計処理
           PERFORM 210-MAIN-SRYACCT-SEC
      *
           IF    ( WRK-PARA-NYUGAIKBN  NOT =   "2")
               PERFORM 210-MAIN-NYUINACCT-SEC
           END-IF
      *
           IF      CNT-TOKEI       NOT =   ZERO
      *
               CLOSE                       TOKEI-FILE
               OPEN    INPUT               TOKEI-FILE
      *
      *        統計出力処理
               IF      WRK-PARA-SYORIKBN   =   "1"
                   PERFORM 260-TOKEI-OUT-SEC
               ELSE
                   PERFORM 270-TOKEI-OUT-GOUKEI-SEC
               END-IF
           END-IF
      *
           CLOSE                       TOKEI-FILE
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *   診療会計集計処理
      *****************************************************************
       210-MAIN-SRYACCT-SEC        SECTION.
      *
      *    診療会計読み込み
           PERFORM 900-SRYACCT-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SRYACCT =   1
               IF    ( ACCT-ZAITEN     >   ZERO )  AND
                     ( ACCT-ZAIKAISU   >   ZERO )
                   MOVE    ACCT-ZAITEN     TO  WRK-ZAITEN
                   MOVE    ACCT-PTID       TO  WRK-PTID
                   MOVE    ACCT-SRYKBN     TO  WRK-SRYKBN
                   MOVE    ACCT-NYUGAIKBN  TO  WRK-NYUGAIKBN
      *            チェック処理
                   PERFORM 230-MAIN-CHK-SYORI-SEC
                   IF      FLG-OK          =   1
      *                診療会計集計処理
                       PERFORM 220-MAIN-SRYACCT-CHK-SEC
                   END-IF
               END-IF
      *        診療会計読み込み
               PERFORM 900-SRYACCT-DBFETCH-SEC
           END-PERFORM
      *
      *    診療会計読み込み
           PERFORM 900-SRYACCT-DBCLOSE-SEC
           .
       210-MAIN-SRYACCT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計集計処理
      *****************************************************************
       210-MAIN-NYUINACCT-SEC      SECTION.
      *
      *    入院会計読み込み
           PERFORM 900-NYUINACCT-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-NYUINACCT   =   1
               INITIALIZE              SS009-AREA
               MOVE    "1"         TO  SS009-EXGAIHAKU
               CALL    "ORCSS009"      USING
                                       SS009-AREA
                                       NYUINACCT-REC
                                       SPA-AREA
               IF      SS009-RC        =   ZERO
                   PERFORM VARYING IDX-K   FROM    1   BY  1
                           UNTIL ( IDX-K   >   SS009-HENKYAKU-MAX )
                       IF    ( SS009-ZAITEN(IDX-K)     >   ZERO )  AND
                             ( SS009-ZAIKAISU(IDX-K)   >   ZERO )
                           MOVE    SS009-ZAITEN(IDX-K) TO  WRK-ZAITEN
                           MOVE    NACCT-PTID          TO  WRK-PTID
                           MOVE    SS009-SRYKBN(IDX-K) TO  WRK-SRYKBN
                           MOVE    NACCT-NYUGAIKBN     TO  WRK-NYUGAIKBN
      *                    チェック処理
                           PERFORM 230-MAIN-CHK-SYORI-SEC
                           IF      FLG-OK          =   1
      *                        入院会計集計処理
                               PERFORM 220-MAIN-NYUINACCT-CHK-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *        入院会計読み込み
               PERFORM 900-NYUINACCT-DBFETCH-SEC
           END-PERFORM
      *
      *    入院会計読み込み
           PERFORM 900-NYUINACCT-DBCLOSE-SEC
           .
       210-MAIN-NYUINACCT-EXT.
           EXIT.
      *****************************************************************
      *   診療会計集計処理
      *****************************************************************
       220-MAIN-SRYACCT-CHK-SEC    SECTION.
      *
           IF    ( ACCT-JIHOKBN    =   "1" )   OR
                 ( ACCT-HKNCOMBI   =   9999 )
               GO  TO  220-MAIN-SRYACCT-CHK-EXT
           END-IF
      *
           IF    ( ACCT-JIHOKBN    = "1" )   AND
                 ( ACCT-TEIGENRATE =  3 OR 4 )
               GO  TO  220-MAIN-SRYACCT-CHK-EXT
           END-IF
      *
           IF    ( ACCT-ZAIKBN     =   1 ) OR
                 ( ACCT-ZAIKBN     =   2 )
               GO  TO  220-MAIN-SRYACCT-CHK-EXT
           END-IF
      *
           MOVE    ACCT-SRYYM      TO  WRK-SRYYM
           MOVE    SPACE           TO  WRK-NAIYO-G
           MOVE    ZERO            TO  CNT-NAIYO
      *
           PERFORM 900-SRYACT-DBSELECT-SEC
           PERFORM UNTIL   FLG-SRYACT  =   1
               IF      ACCT-SRYKBN     =   SRY-SRYKBN
                   PERFORM VARYING IDX-S   FROM    1   BY  1
                           UNTIL ( IDX-S   >   5 )
                       IF      SRY-SRYCD(IDX-S)    NOT =   SPACE
                           MOVE    SRY-SRYCD(IDX-S)   TO  WRK-SRYCD
                           PERFORM 230-SRYCD-CHK-SYORI-SEC
                       END-IF
                   END-PERFORM
               END-IF
               PERFORM 900-SRYACT-DBFETCH-SEC
           END-PERFORM
           PERFORM 900-SRYACT-DBCLOSE-SEC
      *
           PERFORM VARYING IDX-K FROM  2   BY  1
                   UNTIL   IDX-K >     10
               PERFORM VARYING IDX-D FROM  1   BY  1
                       UNTIL   IDX-D >     31
                   IF      ACCT-DAY(IDX-K IDX-D)   NOT =   ZERO
      *                集計処理
                       PERFORM 240-TOKEI-SET-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       220-MAIN-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計集計処理
      *****************************************************************
       220-MAIN-NYUINACCT-CHK-SEC  SECTION.
      *
           MOVE    NACCT-SRYYM     TO  WRK-SRYYM
           MOVE    SPACE           TO  WRK-NAIYO-G
           MOVE    ZERO            TO  CNT-NAIYO
      *
           PERFORM VARYING IDX-S   FROM    1   BY  1
                   UNTIL ( IDX-S   >   20 )
               IF      SS009-SRYCD(IDX-K IDX-S)    NOT =   SPACE
                   MOVE    SS009-SRYCD(IDX-K IDX-S)    TO  WRK-SRYCD
                   PERFORM 230-SRYCD-CHK-SYORI-SEC
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-D FROM  1   BY  1
                   UNTIL   IDX-D >     31
               IF      SS009-DAY(IDX-K IDX-D)  NOT =   ZERO
      *            集計処理
                   PERFORM 240-TOKEI-SET-SEC
               END-IF
           END-PERFORM
           .
       220-MAIN-NYUINACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック処理
      *****************************************************************
       230-MAIN-CHK-SYORI-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
      *    テスト患者のときは読み飛ばす
           PERFORM 900-PTINF-READ-SEC
           IF    ( FLG-PTINF           =   ZERO )  AND
                 ( PTINF-TSTPTNUMKBN   =   "1"  )
               GO  TO  230-MAIN-CHK-SYORI-EXT
           END-IF
      *
      *    患者番号読み込み
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM       =   1
               GO  TO  230-MAIN-CHK-SYORI-EXT
           END-IF
      *
      *    診療区分チェック
           EVALUATE    TRUE
               WHEN  ( WRK-SRYKBN  =   "90" OR "92" )
                   MOVE    "90"        TO  WRK-SRYKBN
               WHEN  ( WRK-SRYKBN(1:1)    =  "2" )
                   MOVE    "20"        TO  WRK-SRYKBN
               WHEN  ( WRK-SRYKBN(1:1)    =  "3" )
                   MOVE    "30"        TO  WRK-SRYKBN
               WHEN  ( WRK-SRYKBN  =   "64" )
                   MOVE    "60"        TO  WRK-SRYKBN
               WHEN  ( WRK-SRYKBN  =   "95" OR "96" OR "97")
                   GO  TO  230-MAIN-CHK-SYORI-EXT
           END-EVALUATE
      *
           IF    ( WRK-PARA-SRYKBN(1)  =   WRK-SRYKBN )    OR
                 ( WRK-PARA-SRYKBN(2)  =   WRK-SRYKBN )    OR
                 ( WRK-PARA-SRYKBN(3)  =   WRK-SRYKBN )
               MOVE    1           TO  FLG-OK
           ELSE
               GO  TO  230-MAIN-CHK-SYORI-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
      *    在点数チェック
           IF    ( WRK-PARA-IJYOU  NOT =   ZERO )  AND
                 ( WRK-PARA-IKA    NOT =   ZERO )
               IF    ( WRK-ZAITEN      >=  WRK-PARA-IJYOU ) AND
                     ( WRK-ZAITEN      <=  WRK-PARA-IKA )
                   MOVE    1           TO  FLG-OK
               END-IF
           ELSE
               IF      WRK-PARA-IJYOU  NOT =   ZERO
                   IF      WRK-ZAITEN      >=  WRK-PARA-IJYOU
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-IF
               IF      WRK-PARA-IKA    NOT =   ZERO
                   IF      WRK-ZAITEN      <=  WRK-PARA-IKA
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-IF
           END-IF
           .
       230-MAIN-CHK-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    チェック処理
      *****************************************************************
       230-SRYCD-CHK-SYORI-SEC     SECTION.
      *
           PERFORM 900-TENSU-READ-SEC
           IF      FLG-TENSU       =   1
               GO  TO  230-SRYCD-CHK-SYORI-EXT
           END-IF
      *
           IF      TNS-TEN         <=   0
               GO  TO  230-SRYCD-CHK-SYORI-EXT
           END-IF
      *
           IF      TNS-TENSIKIBETU =   7
               GO  TO  230-SRYCD-CHK-SYORI-EXT
           END-IF
      *
      *    優先順位１
           IF    ( TNS-TENSIKIBETU =   1   OR  3   OR  5 ) AND
                 ( TNS-SRYCD(1:1)  =   "1" )
               IF      WRK-NAIYO-SRYCD(1)  =   SPACE
                   MOVE    TNS-SRYCD   TO  WRK-NAIYO-SRYCD(1)
                   MOVE    TNS-NAME    TO  WRK-NAIYO-NAME(1)
               END-IF
               COMPUTE CNT-NAIYO   =   CNT-NAIYO   +   1
           ELSE
      *        優先順位２
               IF    ( TNS-TENSIKIBETU =   9 )
                   IF      WRK-NAIYO-SRYCD(2)  =   SPACE
                       MOVE    TNS-SRYCD   TO  WRK-NAIYO-SRYCD(2)
                       MOVE    TNS-NAME    TO  WRK-NAIYO-NAME(2)
                   END-IF
                   COMPUTE CNT-NAIYO   =   CNT-NAIYO   +   1
               ELSE
      *            優先順位３
                   IF      WRK-NAIYO-SRYCD(3)  =   SPACE
                       MOVE    TNS-SRYCD   TO  WRK-NAIYO-SRYCD(3)
                       MOVE    TNS-NAME    TO  WRK-NAIYO-NAME(3)
                   END-IF
                   COMPUTE CNT-NAIYO   =   CNT-NAIYO   +   1
               END-IF
           END-IF
           .
       230-SRYCD-CHK-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       240-TOKEI-SET-SEC           SECTION.
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    WRK-NYUGAIKBN   TO  TOKEI-NYUGAIKBN
           MOVE    WRK-SRYKBN      TO  TOKEI-SRYKBN
           MOVE    PTINF-KANANAME  TO  TOKEI-KANANAME
           MOVE    PTNUM-PTNUM     TO  TOKEI-PTNUM
           MOVE    WRK-ZAITEN      TO  TOKEI-TEN
           MOVE    WRK-SRYYM       TO  TOKEI-SRYYM
      *
           IF      WRK-NAIYO-SRYCD(1)  NOT =   SPACE
               MOVE    WRK-NAIYO-SRYCD(1)  TO  TOKEI-SRYCD
               MOVE    WRK-NAIYO-NAME(1)   TO  TOKEI-NAIYO
           ELSE
               IF      WRK-NAIYO-SRYCD(2)  NOT =   SPACE
                   MOVE    WRK-NAIYO-SRYCD(2)  TO  TOKEI-SRYCD
                   MOVE    WRK-NAIYO-NAME(2)   TO  TOKEI-NAIYO
               ELSE
                   IF      WRK-NAIYO-SRYCD(3)  NOT =   SPACE
                       MOVE    WRK-NAIYO-SRYCD(3)  TO  TOKEI-SRYCD
                       MOVE    WRK-NAIYO-NAME(3)   TO  TOKEI-NAIYO
                   END-IF
               END-IF
           END-IF
           IF      CNT-NAIYO   >   1
               STRING  TOKEI-NAIYO     DELIMITED BY SPACE
                       "　他"          DELIMITED BY SIZE
                                       INTO    TOKEI-NAIYO
               END-STRING
           END-IF
           IF      TOKEI-SRYCD     =   SPACE
               GO  TO  240-TOKEI-SET-EXT
           END-IF
      *
           MOVE    PTINF-NAME      TO  TOKEI-NAME
      *
           PERFORM 900-TOKEI-READ-SEC
      *
           IF      FLG-TOKEI       =   1
               COMPUTE CNT-TOKEI   =   CNT-TOKEI   +   1
               MOVE    "1"             TO  TOKEI-FLG(IDX-D)
               WRITE   TOKEI-REC
           ELSE
               MOVE    "1"             TO  TOKEI-FLG(IDX-D)
               REWRITE TOKEI-REC
           END-IF
           .
       240-TOKEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       260-TOKEI-OUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1  )
               MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
               MOVE    TOKEI-SRYKBN    TO  WRK-SRYKBN
               MOVE    ZERO            TO  WRK-PTCNT
               MOVE    SPACE           TO  WRK-PTNUM
      *        診療区分見出し
               PERFORM 310-PRT-HEAD-SEC
               PERFORM UNTIL ( FLG-TOKEI       =   1  )
                       OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
                       OR    ( TOKEI-SRYKBN    NOT =   WRK-SRYKBN )
      *            明細
                   PERFORM 320-PRT-MEISAI-SEC
      *            統計ファイル読み込み
                   PERFORM 900-TOKEI-NEXT-SEC
               END-PERFORM
      *        合計
               PERFORM 330-PRT-GOKEI-SEC
           END-PERFORM
      *    合計ページ印刷
           PERFORM 340-PRT-OUT-GOUKEI-SEC
             .
       260-TOKEI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       270-TOKEI-OUT-GOUKEI-SEC    SECTION.
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1  )
               MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
               MOVE    TOKEI-SRYKBN    TO  WRK-SRYKBN
               MOVE    ZERO            TO  WRK-PTCNT
               MOVE    SPACE           TO  WRK-PTNUM
               PERFORM UNTIL ( FLG-TOKEI       =   1  )
                       OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
                       OR    ( TOKEI-SRYKBN    NOT =   WRK-SRYKBN )
                   IF      TOKEI-PTNUM     NOT =   WRK-PTNUM
                       MOVE    TOKEI-PTNUM     TO  WRK-PTNUM
                       COMPUTE WRK-PTCNT   =   WRK-PTCNT   +   1
                   END-IF
      *            統計ファイル読み込み
                   PERFORM 900-TOKEI-NEXT-SEC
               END-PERFORM
      *
               PERFORM VARYING IDX-G   FROM 1 BY 1
                       UNTIL ( IDX-G   >   6 )
                   IF    ( WRK-GOKEI-NYUGAIKBN(IDX-G) =  WRK-NYUGAIKBN)
                     AND ( WRK-GOKEI-SRYKBN(IDX-G) =  WRK-SRYKBN )
                       MOVE    WRK-PTCNT   TO  WRK-GOKEI-PTCNT(IDX-G)
                       MOVE    6           TO  IDX-G
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    合計ページ印刷
           PERFORM 340-PRT-OUT-GOUKEI-SEC
           .
       270-TOKEI-OUT-GOUKEI-EXT.
           EXIT.
      *****************************************************************
      *    見出し
      *****************************************************************
       310-PRT-HEAD-SEC            SECTION.
      *
           EVALUATE    TOKEI-NYUGAIKBN
               WHEN    "1"
                   MOVE    "（入院）"      TO  WRK-PRT-NYUGAIKBN
               WHEN    "2"
                   MOVE    "（外来）"      TO  WRK-PRT-NYUGAIKBN
           END-EVALUATE
      *
           EVALUATE    WRK-SRYKBN
               WHEN    "11"
                   MOVE    "初　　診"      TO  WRK-PRT-SRYKBN
               WHEN    "12"
                   MOVE    "再　　診"      TO  WRK-PRT-SRYKBN
               WHEN    "13"
                   MOVE    "医学管理"      TO  WRK-PRT-SRYKBN
               WHEN    "14"
                   MOVE    "在　　宅"      TO  WRK-PRT-SRYKBN
               WHEN    "20"
                   MOVE    "投　　薬"      TO  WRK-PRT-SRYKBN
               WHEN    "30"
                   MOVE    "注　　射"      TO  WRK-PRT-SRYKBN
               WHEN    "40"
                   MOVE    "処　　置"      TO  WRK-PRT-SRYKBN
               WHEN    "50"
                   MOVE    "手　　術"      TO  WRK-PRT-SRYKBN
               WHEN    "54"
                   MOVE    "麻　　酔"      TO  WRK-PRT-SRYKBN
               WHEN    "60"
                   MOVE    "検　　査"      TO  WRK-PRT-SRYKBN
               WHEN    "70"
                   MOVE    "画像診断"      TO  WRK-PRT-SRYKBN
               WHEN    "80"
                   MOVE    "そ の 他"      TO  WRK-PRT-SRYKBN
               WHEN    "90"
                   MOVE    "入　　院"      TO  WRK-PRT-SRYKBN
           END-EVALUATE
      *
           MOVE    SPACE           TO  HCMSL55
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    7               TO  CNT-LINE
      *
           MOVE    WRK-PRT-LINE1   TO  HCMSL55-MOJI(1)
           MOVE    WRK-PRT-LINE2   TO  HCMSL55-MOJI(2)
      *
           MOVE    CNT-PAGE        TO  WRK-Z03
           MOVE    WRK-Z03         TO  WRK-PRT-PAGE
           MOVE    WRK-PRT-LINE3   TO  HCMSL55-MOJI(3)
      *
           MOVE    WRK-PRT-LINE4   TO  HCMSL55-MOJI(4)
      *
           MOVE    WRK-PRT-LINE5   TO  HCMSL55-MOJI(6)
           MOVE    WRK-PRT-SEN     TO  HCMSL55-MOJI(7)
           .
       310-PRT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細
      *****************************************************************
       320-PRT-MEISAI-SEC          SECTION.
      *
           IF      CNT-LINE        >=  55
      *        帳票編集処理
               PERFORM 390-PRINT-MEISAI-OUT-SEC
      *        見出し
               PERFORM 310-PRT-HEAD-SEC
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           IF      TOKEI-PTNUM     NOT =   WRK-PTNUM
               MOVE    TOKEI-PTNUM     TO  WRK-PTNUM
               MOVE    TOKEI-PTNUM     TO  WRK-PRT-PTNUM
               MOVE    TOKEI-NAME      TO  WRK-PRT-NAME
               COMPUTE WRK-PTCNT   =   WRK-PTCNT   +   1
           ELSE
               IF      CNT-LINE    =   8
                   MOVE    TOKEI-PTNUM     TO  WRK-PRT-PTNUM
                   MOVE    TOKEI-NAME      TO  WRK-PRT-NAME
               ELSE
                   MOVE    SPACE           TO  WRK-PRT-PTNUM
                   MOVE    SPACE           TO  WRK-PRT-NAME
               END-IF
           END-IF
      *
           MOVE    TOKEI-NAIYO     TO  WRK-PRT-NAIYO
      *
           MOVE    TOKEI-TEN       TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-TEN
      *
           MOVE    TOKEI-SRYYM     TO  WRK-SYMD(1:6)
           MOVE    "01"            TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMD(1:6) TO  WRK-PRT-SRYYM
      *
           MOVE    SPACE           TO  WRK-SRYYM-G
           MOVE    "|"             TO  WRK-SRYYM-D(1)
           MOVE    "|"             TO  WRK-SRYYM-D(5)
           MOVE    "|"             TO  WRK-SRYYM-D(10)
           MOVE    "|"             TO  WRK-SRYYM-D(15)
           MOVE    "|"             TO  WRK-SRYYM-D(20)
           MOVE    "|"             TO  WRK-SRYYM-D(25)
           MOVE    "|"             TO  WRK-SRYYM-D(30)
      *
           PERFORM VARYING IDX-D   FROM    1   BY  1
                   UNTIL   IDX-D   >   31
               IF      TOKEI-FLG(IDX-D)    =   "1"
                   MOVE    "*"         TO  WRK-SRYYM-D(IDX-D)
               END-IF
           END-PERFORM
           MOVE    WRK-SRYYM-G     TO  WRK-PRT-SRYYM-G
      *
           MOVE    WRK-PRT-MEISAI  TO  HCMSL55-MOJI(CNT-LINE)
           .
       320-PRT-MEISAI-EXT.
           EXIT.
      *****************************************************************
      *    合計
      *****************************************************************
       330-PRT-GOKEI-SEC           SECTION.
      *
           IF      CNT-LINE        >=  55
      *        帳票編集処理
               PERFORM 390-PRINT-MEISAI-OUT-SEC
      *        見出し
               PERFORM 310-PRT-HEAD-SEC
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   2
      *
           MOVE    WRK-PTCNT       TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-NINZU
           MOVE    WRK-PRT-GOKEI   TO  HCMSL55-MOJI(CNT-LINE)
      *
      *    印刷処理
           PERFORM 390-PRINT-MEISAI-OUT-SEC
      *
      *    合計集計
           PERFORM VARYING IDX-G   FROM 1 BY 1
                   UNTIL ( IDX-G   >   6 )
               IF    ( WRK-GOKEI-NYUGAIKBN(IDX-G) =  WRK-NYUGAIKBN)
                 AND ( WRK-GOKEI-SRYKBN(IDX-G) =  WRK-SRYKBN )
                   MOVE    WRK-PTCNT   TO  WRK-GOKEI-PTCNT(IDX-G)
                   MOVE    6           TO  IDX-G
               END-IF
           END-PERFORM
           .
       330-PRT-GOKEI-EXT.
           EXIT.
      *****************************************************************
      *    合計ページ印刷
      *****************************************************************
       340-PRT-OUT-GOUKEI-SEC      SECTION.
      *
           MOVE    SPACE           TO  HCMSL80
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    7               TO  CNT-LINE
      *
           MOVE    WRK-PRT-G-LINE1 TO  HCMSL80-MOJI(1)
      *
           MOVE    CNT-PAGE        TO  WRK-Z03
           MOVE    WRK-Z03         TO  WRK-PRT-G-PAGE
           MOVE    WRK-PRT-G-LINE3 TO  HCMSL80-MOJI(3)
      *
           MOVE    WRK-PRT-G-LINE4 TO  HCMSL80-MOJI(4)
      *
           MOVE    WRK-PRT-G-LINE5 TO  HCMSL80-MOJI(6)
           MOVE    WRK-PRT-G-SEN   TO  HCMSL80-MOJI(7)
      *
           PERFORM VARYING IDX-G   FROM 1 BY 1
                   UNTIL ( IDX-G   >   6 )
               IF      WRK-GOKEI-SRYKBN(IDX-G) NOT =   SPACE
      *
                   EVALUATE    WRK-GOKEI-NYUGAIKBN(IDX-G)
                       WHEN    "1"
                           MOVE    "入院"      TO  WRK-PRT-G-NYUGAIKBN
                       WHEN    "2"
                           MOVE    "外来"      TO  WRK-PRT-G-NYUGAIKBN
                   END-EVALUATE
      *
                   EVALUATE    WRK-GOKEI-SRYKBN(IDX-G)
                       WHEN    "11"
                           MOVE    "初　　診"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "12"
                           MOVE    "再　　診"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "13"
                           MOVE    "医学管理"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "14"
                           MOVE    "在　　宅"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "20"
                           MOVE    "投　　薬"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "30"
                           MOVE    "注　　射"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "40"
                           MOVE    "処　　置"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "50"
                           MOVE    "手　　術"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "54"
                           MOVE    "麻　　酔"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "60"
                           MOVE    "検　　査"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "70"
                           MOVE    "画像診断"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "80"
                           MOVE    "そ の 他"      TO  WRK-PRT-G-SRYKBN
                       WHEN    "90"
                           MOVE    "入　　院"      TO  WRK-PRT-G-SRYKBN
                   END-EVALUATE
      *
                   MOVE    WRK-GOKEI-PTCNT(IDX-G)  TO  WRK-Z10
                   MOVE    WRK-Z10                 TO  WRK-PRT-G-NINZU
      *
                   COMPUTE CNT-LINE    =   CNT-LINE    +   1
                   MOVE    WRK-PRT-G-MEISAI  TO  HCMSL80-MOJI(CNT-LINE)
               END-IF
           END-PERFORM
      *
      *    印刷処理
           PERFORM 390-PRINT-GOUKEI-OUT-SEC
           .
       340-PRT-OUT-GOUKEI-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       390-PRINT-MEISAI-OUT-SEC    SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    SPACE               TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"       TO  SPRT-PRTID
           MOVE    "診療区分別指定点数該当患者数調べ"
                                       TO  SPRT-TITLE
           MOVE    HCMSL55             TO  SPRT-PRTDATA
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
           .
       390-PRINT-MEISAI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       390-PRINT-GOUKEI-OUT-SEC    SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    SPACE               TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMSL80.red"       TO  SPRT-PRTID
           MOVE    "診療区分別指定点数該当患者数調べ"
                                       TO  SPRT-TITLE
           MOVE    HCMSL80             TO  SPRT-PRTDATA
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
           .
       390-PRINT-GOUKEI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    前ゼロ編集処理
      *****************************************************************
       800-TENSU-HEN-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-HEN-TEN-OUT
      *
           PERFORM VARYING IDX-Z   FROM    1   BY  1
                   UNTIL   IDX-Z   >   10
               IF      WRK-HEN-TEN-IN(IDX-Z:1) NOT =   SPACE
                   MOVE    WRK-HEN-TEN-IN(IDX-Z:)
                                       TO  WRK-HEN-TEN-OUT
                   MOVE    10          TO  IDX-Z
               END-IF
           END-PERFORM
           .
       800-TENSU-HEN-EXT.
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
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
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
           DISPLAY "*** ORCBG029 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG029 END ***"
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
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-DBSELECT-SEC    SECTION.
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  ACCT-NYUGAIKBN
           MOVE    WRK-PARA-STRYM  TO  ACCT-STSRYYM
           MOVE    WRK-PARA-ENDYM  TO  ACCT-EDSRYYM
           MOVE    "%"             TO  ACCT-SRYKBN
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key59"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-DBFETCH-SEC     SECTION.
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key59"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-DBCLOSE-SEC     SECTION.
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key59"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-SRYACCT-DBCLOSE-EXT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-DBSELECT-SEC  SECTION.
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    WRK-PARA-STRYM  TO  NACCT-SRYYM
           MOVE    WRK-PARA-ENDYM  TO  NACCT-CREYMD
           MOVE    "1"             TO  NACCT-NYUGAIKBN
           MOVE    "5"             TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key42"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
           .
       900-NYUINACCT-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       900-NYUINACCT-DBFETCH-SEC   SECTION.
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key42"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
      *
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
           .
       900-NYUINACCT-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスタ検索
      *****************************************************************
       900-NYUINACCT-DBCLOSE-SEC   SECTION.
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key42"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-NYUINACCT-DBCLOSE-EXT.
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBSELECT-SEC     SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN
                                   TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID       TO  SRY-PTID
           MOVE    ACCT-SRYKA      TO  SRY-SRYKA
           MOVE    ACCT-SRYYM      TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM     TO  SRY-ZAINUM
           MOVE    SRYACT-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SRYACT
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
               INITIALIZE                  SRYACT-REC
           END-IF
           .
       900-SRYACT-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBFETCH-SEC      SECTION.
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
               INITIALIZE                  SRYACT-REC
           END-IF
           .
       900-SRYACT-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBCLOSE-SEC      SECTION.
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-SRYACT-DBCLOSE-EXT.
      *****************************************************************
      *    患者情報マスタ読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    SPACE           TO  PTINF-REC
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    WRK-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスタ読込
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID        TO  PTNUM-PTID
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTNUM
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-SRYYM       TO  TNS-YUKOSTYMD(1:6)
                                       TNS-YUKOEDYMD(1:6)
           MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
                                       TNS-YUKOEDYMD(7:2)
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-TENSU-READ-EXT.
           EXIT.
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
           .
       900-DBSELECT-EXT.
           EXIT.
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
           .
       900-DBFETCH-EXT.
           EXIT.
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
           .
       900-DBCLOSECURSOR-EXT.
           EXIT.
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
