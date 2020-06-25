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
       PROGRAM-ID.             ORCBG030.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 一括再計算該当者チェック表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/01/16    NACL-森脇     新規作成
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
               05  TOKEI-IKTFLG        PIC 9(01).
               05  TOKEI-PTNUM         PIC X(20).
           03  TOKEI-NAME              PIC X(30).
           03  TOKEI-GAITOKBN          PIC 9(02).
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
           COPY    "HCMG008.INC".
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
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(04).
           03  CNT-LINE                PIC 9(02).
           03  CNT-TOKEI               PIC 9(05).
           03  CNT-KENSU               PIC 9(05).
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
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-SRYYM          PIC X(06).
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
           03  WRK-HENTIME.
               05  WRK-HH              PIC X(02).
               05  WRK-FILLER1         PIC X(01).
               05  WRK-MM              PIC X(02).
               05  WRK-FILLER2         PIC X(01).
               05  WRK-SS              PIC X(02).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-IKTFLG              PIC 9(01).
      *
           03  WRK-SRYYM               PIC X(16).
           03  WRK-PRTYMD              PIC X(22).
           03  WRK-PRTTIME             PIC X(10).
      *
           03  WRK-Z03                 PIC ZZ9.
           03  WRK-Z05                 PIC ZZZZ9.
      *
       01  CONST-AREA.
           03  CONST-IKTFLG-0.
               05  FILLER              PIC X(28)   VALUE
                   "【患者請求額の変更がある方】".
           03  CONST-IKTFLG-7.
               05  FILLER              PIC X(28)   VALUE
                   "【患者請求額の変更がない方】".
           03  CONST-IKTFLG-8.
               05  FILLER              PIC X(28)   VALUE
                   "【患者請求額の変更がない方】".
               05  FILLER              PIC X(38)   VALUE
                   "※該当者チェック時に自動で一括再計算済".
           03  CONST-IKTFLG-9.
               05  FILLER              PIC X(36)   VALUE
                   "【該当者チェックでエラーとなった方】".
           03  CONST-GAITOKBN-1.
               05  FILLER              PIC X(18)   VALUE
                   "第三者行為現物給付".
               05  FILLER              PIC X(28)   VALUE
                   "（通常診療分の優先計算該当）".
           03  CONST-GAITOKBN-2.
               05  FILLER              PIC X(18)   VALUE
                   "７０歳未満現物給付".
               05  FILLER              PIC X(28)   VALUE
                   "（公費併用分の優先計算該当）".
           03  CONST-GAITOKBN-3.
               05  FILLER              PIC X(14)   VALUE
                   "高齢者現物給付".
               05  FILLER              PIC X(28)   VALUE
                   "（公費併用分の優先計算該当）".
           03  CONST-GAITOKBN-4.
               05  FILLER              PIC X(14)   VALUE
                   "収納更新未処理".
               05  FILLER              PIC X(40)   VALUE
                   "（会計照会【収納更新】未処理の収納あり）".
           03  CONST-GAITOKBN-5.
               05  FILLER              PIC X(20)   VALUE
                   "該当者チェック抽出分".
               05  FILLER              PIC X(36)   VALUE
                   "（データ項目値が変更となる収納あり）".
           03  CONST-GAITOKBN-6.
               05  FILLER              PIC X(36)   VALUE
                   "診療行為で訂正登録が必要な収納あり。".
               05  FILLER              PIC X(24)   VALUE
                   "訂正登録後に再チェック。".
           03  CONST-GAITOKBN-7.
               05  FILLER              PIC X(18)   VALUE
                   "第三者行為現物給付".
               05  FILLER              PIC X(30)   VALUE
                   "（第三者行為分の優先計算該当）".
           03  CONST-GAITOKBN-8.
               05  FILLER              PIC X(10)   VALUE
                   "マル長計算".
      *
      *    プログラムオプションパラメタ
       01  TPARA-AREA.
           03  TPARA-IKT               PIC X(01).
      *
           03  WRK-TPARA-NAME          PIC X(25).
           03  WRK-TPARA               PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    一括再計算該当者チェックサブ
           COPY    "CPORCSSYUNOUCHK.INC".
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
           INITIALIZE                  IDX-AREA
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
                                       WRK-PARA-SRYYM
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
           PERFORM 900-SYS1001-READ-SEC
           MOVE    SYS-1001-PREFNUM    TO  SPA-PREFNUM
           MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG030"          TO  JOB-PGID
           MOVE    "一括再計算該当者チェック表"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "BG03001"           TO  TOKEIPARA-FILE-ID
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
      *    プログラムオプション取込処理
           PERFORM 100-OPTION-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    診療年月セット
           MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYM
      *
      *    印刷日セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PRTYMD
      *
           ACCEPT  WRK-PRTTIME     FROM    TIME
           MOVE    SPACE               TO  WRK-HENTIME
           MOVE    WRK-PRTTIME(1:2)    TO  WRK-HH
           MOVE    ":"                 TO  WRK-FILLER1
           MOVE    WRK-PRTTIME(3:2)    TO  WRK-MM
           MOVE    ":"                 TO  WRK-FILLER2
           MOVE    WRK-PRTTIME(5:2)    TO  WRK-SS
           MOVE    SPACE               TO  WRK-PRTTIME
           MOVE    "("                 TO  WRK-PRTTIME(1:1)
           MOVE    WRK-HENTIME         TO  WRK-PRTTIME(2:8)
           MOVE    ")"                 TO  WRK-PRTTIME(10:1)
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプション取込処理
      *****************************************************************
       100-OPTION-SELECT-SEC       SECTION.
      *
           INITIALIZE                  TPARA-AREA
      *
      *    一括再計算
           MOVE    "SKYNOT_IKT"    TO  WRK-TPARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE     WRK-TPARA(1:1) TO  TPARA-IKT
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCBG030"      TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-TPARA-NAME  TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING
                                       SPA-AREA
                                       ORCSPRGOPTIONAREA
      *
           MOVE    SPACE               TO  WRK-TPARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-TPARA(1:1)
           END-IF
           .
       1001-OPTION-CALL-EXT.
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
           PERFORM 210-MAIN-TOKEI-SET-SEC
           CLOSE                       TOKEI-FILE
      *
           IF      CNT-TOKEI       NOT =   ZERO
               OPEN    INPUT               TOKEI-FILE
               PERFORM 260-MAIN-TOKEI-OUT-SEC
               CLOSE                       TOKEI-FILE
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       210-MAIN-TOKEI-SET-SEC      SECTION.
      *
           INITIALIZE                  LNK-ORCSSYUNOUCHK-REC
           MOVE    SPA-HOSPNUM     TO  LNK-ORCSSYUNOUCHK-HOSPNUM
           MOVE    "2"             TO  LNK-ORCSSYUNOUCHK-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM  TO  LNK-ORCSSYUNOUCHK-SRYYM
           IF      TPARA-IKT       =   "1"
               MOVE    "1"             TO  LNK-ORCSSYUNOUCHK-OPTION1
           ELSE
               MOVE    "0"             TO  LNK-ORCSSYUNOUCHK-OPTION1
           END-IF
           CALL    "ORCSSYUNOUCHK"     USING
                                       LNK-ORCSSYUNOUCHK-REC
                                       SPA-AREA
           IF      LNK-ORCSSYUNOUCHK-RC    NOT =   ZERO
               GO  TO   210-MAIN-TOKEI-SET-EXT
           END-IF
      *
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL ( LNK-ORCSSYUNOUCHK-PTID(IDX)  =   ZERO )
                   OR    ( IDX >   10000 )
               MOVE    LNK-ORCSSYUNOUCHK-PTID(IDX) TO  WRK-PTID
               PERFORM 900-PTNUM-READ-SEC
               IF      FLG-PTNUM   =    ZERO
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF   =    ZERO
                       INITIALIZE                  TOKEI-REC
                       MOVE    LNK-ORCSSYUNOUCHK-JTIFLG(IDX)
                                               TO  TOKEI-IKTFLG
                       MOVE    PTNUM-PTNUM     TO  TOKEI-PTNUM
      *                統計ファイル読み込み
                       PERFORM 900-TOKEI-READ-SEC
                       IF      FLG-TOKEI   =    1
                           MOVE    LNK-ORCSSYUNOUCHK-JTIFLG(IDX)
                                                   TO  TOKEI-IKTFLG
                           MOVE    PTNUM-PTNUM     TO  TOKEI-PTNUM
                           MOVE    PTINF-NAME      TO  TOKEI-NAME
                           MOVE    LNK-ORCSSYUNOUCHK-GAITOKBN(IDX)
                                                   TO  TOKEI-GAITOKBN
                           WRITE   TOKEI-REC
                           COMPUTE CNT-TOKEI   =   CNT-TOKEI   +   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       210-MAIN-TOKEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       260-MAIN-TOKEI-OUT-SEC      SECTION.
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI   =   1 )
               MOVE    ZERO            TO  CNT-KENSU
               MOVE    TOKEI-IKTFLG    TO  WRK-IKTFLG
      *        見出
               PERFORM 310-PRT-HEAD-SEC
               PERFORM UNTIL ( FLG-TOKEI   =   1 )
                       OR    ( TOKEI-IKTFLG    NOT =   WRK-IKTFLG )
      *            明細
                   PERFORM 320-PRT-MEISAI-SEC
      *            統計ファイル読み込み
                   PERFORM 900-TOKEI-NEXT-SEC
               END-PERFORM
      *        合計
               PERFORM 330-PRT-GOKEI-SEC
           END-PERFORM
             .
       260-MAIN-TOKEI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    見出
      *****************************************************************
       310-PRT-HEAD-SEC            SECTION.
      *
           MOVE    SPACE           TO  HCMG008
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    ZERO            TO  CNT-LINE
      *
           MOVE    CNT-PAGE        TO  WRK-Z03
           MOVE    WRK-Z03         TO  HCMG008-PAGE
           MOVE    WRK-SRYYM       TO  HCMG008-SRYYM
           MOVE    WRK-PRTYMD      TO  HCMG008-PRTYMD
           MOVE    WRK-PRTTIME     TO  HCMG008-PRTTIME
      *
           EVALUATE    TOKEI-IKTFLG
               WHEN    0
                   MOVE    CONST-IKTFLG-0  TO  HCMG008-IKTFLG
               WHEN    7
                   MOVE    CONST-IKTFLG-7  TO  HCMG008-IKTFLG
               WHEN    8
                   MOVE    CONST-IKTFLG-8  TO  HCMG008-IKTFLG
               WHEN    9
                   MOVE    CONST-IKTFLG-9  TO  HCMG008-IKTFLG
           END-EVALUATE
           .
       310-PRT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細
      *****************************************************************
       320-PRT-MEISAI-SEC          SECTION.
      *
           IF      CNT-LINE        >=  50
      *        帳票編集処理
               PERFORM 390-PRINT-OUT-SEC
      *        見出し
               PERFORM 310-PRT-HEAD-SEC
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           COMPUTE CNT-KENSU   =   CNT-KENSU   +   1
      *
           MOVE    TOKEI-PTNUM     TO  HCMG008-PTNUM(CNT-LINE)
           MOVE    TOKEI-NAME      TO  HCMG008-NAME(CNT-LINE)
      *
           EVALUATE    TOKEI-GAITOKBN
               WHEN    1
                   MOVE    CONST-GAITOKBN-1
                                       TO  HCMG008-NAIYO(CNT-LINE)
               WHEN    2
                   MOVE    CONST-GAITOKBN-2
                                       TO  HCMG008-NAIYO(CNT-LINE)
               WHEN    3
                   MOVE    CONST-GAITOKBN-3
                                       TO  HCMG008-NAIYO(CNT-LINE)
               WHEN    4
                   MOVE    CONST-GAITOKBN-4
                                       TO  HCMG008-NAIYO(CNT-LINE)
               WHEN    5
                   MOVE    CONST-GAITOKBN-5
                                       TO  HCMG008-NAIYO(CNT-LINE)
               WHEN    6
                   MOVE    CONST-GAITOKBN-6
                                       TO  HCMG008-NAIYO(CNT-LINE)
               WHEN    7
                   MOVE    CONST-GAITOKBN-7
                                       TO  HCMG008-NAIYO(CNT-LINE)
               WHEN    8
                   MOVE    CONST-GAITOKBN-8
                                       TO  HCMG008-NAIYO(CNT-LINE)
           END-EVALUATE
           .
       320-PRT-MEISAI-EXT.
           EXIT.
      *****************************************************************
      *    合計
      *****************************************************************
       330-PRT-GOKEI-SEC           SECTION.
      *
           MOVE    CNT-KENSU       TO  WRK-Z05
           MOVE    WRK-Z05         TO  HCMG008-GKENSU
           MOVE    "合計件数"      TO  HCMG008-GLBL
           MOVE    "件"            TO  HCMG008-GKEN
      *    印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
           MOVE    ZERO            TO  CNT-KENSU
           MOVE    TOKEI-IKTFLG    TO  WRK-IKTFLG
           .
       330-PRT-GOKEI-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC    SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMG008.red"       TO  SPRT-PRTID
           MOVE    "一括再計算該当者チェック表"
                                       TO  SPRT-TITLE
           MOVE    HCMG008             TO  SPRT-PRTDATA
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
       390-PRINT-OUT-EXT.
           EXIT.
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
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
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
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
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
           DISPLAY "*** ORCBG030 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG030 END ***"
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
           .
       300-END-EXT.
           EXIT.
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
           MOVE    PTNUM-REC       TO  MCPDATA-REC
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
      *    システム管理医療機関情報読込
      *****************************************************************
       900-SYS1001-READ-SEC        SECTION.
      *
           INITIALIZE                  SYS-1001-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"            TO  SYS-1001-STYUKYMD(7:2)
           MOVE    WRK-PARA-SRYYM  TO  SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-1001-EDYUKYMD(7:2)
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-SYS1001-READ-EXT.
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
