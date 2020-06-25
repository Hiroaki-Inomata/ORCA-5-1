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
       PROGRAM-ID.             ORCBG023.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 配置医師による回数のレセコメント作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/04/06    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "HCPOM.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTTOKKI         PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01). 
      *
           03  FLG-TOKKI           PIC 9(01).
           03  FLG-RETURN          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(05).
           03  CNT-OUT             PIC 9(05).
           03  CNT-MOD             PIC 9(05).
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SRYYM      PIC X(06).
           03  WRK-PARA-KAISU      PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-JYURRK-SRYYMD.
               05  WRK-JYURRK-YY   PIC 9(04).
               05  WRK-JYURRK-MM   PIC 9(02).
               05  WRK-JYURRK-DD   PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-ERRMSG          PIC X(300).
      *
           03  WRK-COL             PIC 9(03).
           03  WRK-Z9              PIC Z9.
           03  WRK-LENGTH          PIC 9(04).
      *
           03  WRK-EDYMD.
               05  WRK-EDYMD-FL    PIC X(06).
               05  WRK-EDYMD-DD    PIC 9(02).
      *
           03  WRK-SRYYM           PIC X(16).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-TOKKI1     PIC X(06)   VALUE   "【配】".
           03  WRK-CONS-TOKKI2     PIC X(06)   VALUE   "（配）".
           03  WRK-CONS-TOKKI3     PIC X(06)   VALUE   "「配」".
           03  WRK-CONS-COMMENT.
               05  WRK-CONS-FL1    PIC X(06)   VALUE   "【配】".
               05  WRK-CONS-KAISU  PIC X(04).
               05  WRK-CONS-FL2    PIC X(02)   VALUE   "回".
      *    改行コード
           03  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
      *    レセプトコメント長さ
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    ワークレセプトコメント（１：全科、２：その他の科）
       01  WRK-RECECOM-TABLE.
         02  WRK-RECECOM-REC       OCCURS 2.
           COPY    "CPRECECOM.INC" REPLACING  //RECECOM-//
                                   BY         //WRK-RECECOM-//.
      *
      *    印刷内容
       01  HEAD-01.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  H01-SRYYM               PIC X(16).
           03  FILLER                  PIC X(32)   VALUE
               "分回数コメント更新一覧　設定回数".
           03  H01-KAISU               PIC X(04).
           03  FILLER                  PIC X(06)   VALUE   "回".
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-SYSYMD              PIC X(09).
           03  FILLER                  PIC X(04)   VALUE   "  P.".
           03  H01-PAGE                PIC Z9.
      *
       01  HEAD-02.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(20)   VALUE   "患者番号".
           03  FILLER                  PIC X(20)   VALUE   "氏名".
           03  FILLER                  PIC X(04)   VALUE   SPACE.
           03  FILLER                  PIC X(12)   VALUE   "初回算定日".
           03  FILLER                  PIC X(14)   VALUE   "保険組合せ".
           03  FILLER                  PIC X(04)   VALUE   "警告".
      *
       01  BODY-01.
           03  B01-FL1                 PIC X(02).
           03  B01-PTNUM               PIC X(20).
           03  B01-NAME                PIC X(20).
           03  B01-FL2                 PIC X(04).
           03  B01-FL3                 PIC X(04).
           03  B01-DD                  PIC Z9.
           03  B01-FL4                 PIC X(06).
           03  B01-HKNCOMBI            PIC X(04).
           03  B01-FL5                 PIC X(10).
           03  B01-ERRFLG              PIC X(12).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    患者特記事項情報
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(400).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
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
                                       WRK-PARA-SRYYM
                                       WRK-PARA-KAISU
           END-UNSTRING
      *
           DISPLAY "SRYYM=" WRK-PARA-SRYYM
           DISPLAY "KAISU=" WRK-PARA-KAISU
      *
           MOVE    WRK-PARA-HOSPNUM    TO SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBG023"          TO  JOB-PGID
           MOVE    "配置医師回数作成"  TO  JOB-SHELLMSG
      *
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"            TO  WRK-SRYYMD (7:2)
      *
           MOVE    WRK-SRYYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3(1:16)
                                   TO  H01-SRYYM
                                       WRK-SRYYM
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM  TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  H01-SYSYMD
      *
      *    患者番号構成管理情報取得
           INITIALIZE                  SYS-1009-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    "1009"          TO  SYS-1009-KANRICD
           MOVE    "*"             TO  SYS-1009-KBNCD
           MOVE    WRK-SRYYMD      TO  SYS-1009-STYUKYMD
                                       SYS-1009-EDYUKYMD
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "患者番号構成管理情報が取得できません"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    編集レセコメント作成
           IF      WRK-PARA-KAISU  >   0
               MOVE    WRK-PARA-KAISU  TO  WRK-Z9
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    WRK-Z9          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-MAX     >   ZERO )
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-CONS-KAISU
                                               H01-KAISU
               ELSE
                   MOVE    "入力された設定回数に誤りがあります"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           ELSE
               MOVE    "設定回数を１以上で入力して下さい"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    69                  TO  CNT-LINE
           MOVE    ZERO                TO  CNT-PAGE  
      *
      *    診療月に特記事項０９のある患者の抽出
           INITIALIZE                      PTTOKKI-REC
           MOVE    SPA-HOSPNUM         TO  PTTOKKI-HOSPNUM
           MOVE    PTTOKKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 900-PTTOKKI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTTOKKI
           END-IF
      *
           PERFORM UNTIL   FLG-PTTOKKI     =   1
      *        レセコメント編集処理
               PERFORM 2001-RECECOM-HENSYU-SEC
      *
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 900-PTTOKKI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 200111-PRINT-OUT-SEC
           END-IF
      *
           MOVE    1                    TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *     レセコメント編集処理
      *****************************************************************
       2001-RECECOM-HENSYU-SEC           SECTION.
      *
      *    患者番号取得
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTTOKKI-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
      *
           DISPLAY "PTNUM=" SPTID-PTNUM
      *
      *    受診履歴から保険組合せの取得
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
           MOVE    PTTOKKI-PTID        TO  JYURRK-PTID
           MOVE    WRK-PARA-SRYYM      TO  JYURRK-SRYYMD  (1:6)
                                           JYURRK-UPSRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD  (7:2)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           PERFORM 900-JYURRK-READ-SEC
           IF      FLG-JYURRK       =   1
               DISPLAY "JYURRK NOT FOUND KEY=" JYURRK-KEY "#"
               PERFORM 20011-RECECOM-PRINT-SEC
               GO  TO  2001-RECECOM-HENSYU-EXT
           END-IF
      *
      *    レセコメント（９９）のチェック
           INITIALIZE                      RECECOM-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECECOM-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  RECECOM-SRYYM
           MOVE    PTTOKKI-PTID        TO  RECECOM-PTID
           MOVE    "2"                 TO  RECECOM-NYUGAIKBN
           MOVE    RECECOM-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-N-SEC
           ELSE
               INITIALIZE                      RECECOM-REC
               MOVE    1                   TO  FLG-RECECOM
           END-IF
      *
           MOVE    ZERO                TO  FLG-TOKKI
           INITIALIZE                      WRK-RECECOM-TABLE
           PERFORM             UNTIL   FLG-RECECOM =   1
                               OR      FLG-TOKKI   =   1
      *        【配】の有無チェック
               MOVE    ZERO            TO  WRK-COL
               INSPECT RECECOM-COMMENT TALLYING    WRK-COL
                                       FOR ALL     WRK-CONS-TOKKI1,
                                           ALL     WRK-CONS-TOKKI2,
                                           ALL     WRK-CONS-TOKKI3
               IF      WRK-COL         >   ZERO
                   MOVE    1               TO  FLG-TOKKI
                   DISPLAY "RECECOM found sryka="  RECECOM-SRYKA " " 
                                                   RECECOM-RENNUM
               ELSE
      *            受診履歴と保険組合せでのレセコメント
                   IF      RECECOM-HKNCOMBI    =   JYURRK-HKNCOMBINUM
                       EVALUATE    RECECOM-SRYKA
                           WHEN    "00"
                               MOVE    RECECOM-REC TO
                                                   WRK-RECECOM-REC (1)
                           WHEN    "01"
                               MOVE    RECECOM-REC TO
                                                   WRK-RECECOM-REC (2)
                       END-EVALUATE
                   END-IF
               END-IF
      *
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *   【配】のコメントがある場合は処理しない
           IF      FLG-TOKKI           =   1
               PERFORM 20011-RECECOM-PRINT-SEC
           ELSE
               IF      WRK-RECECOM-HOSPNUM (1) =   ZERO
                   INITIALIZE                      RECECOM-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECECOM-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECECOM-SRYYM
                   MOVE    PTTOKKI-PTID        TO  RECECOM-PTID
                   MOVE    "2"                 TO  RECECOM-NYUGAIKBN
                   MOVE    JYURRK-HKNCOMBINUM  TO  RECECOM-HKNCOMBI
                   MOVE    "00"                TO  RECECOM-SRYKA
                   MOVE    "99"                TO  RECECOM-SJKBN
                   MOVE    1                   TO  RECECOM-RENNUM
                   MOVE    "00"                TO  RECECOM-SRYDD
                   MOVE    WRK-CONS-COMMENT    TO  RECECOM-COMMENT
                   MOVE    SMCNDATE-YMD        TO  RECECOM-CREYMD
      ***          MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
      ***          MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
                   MOVE    "BG001"             TO  RECECOM-TERMID
                   MOVE    RECECOM-REC         TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_rececom"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC              =   ZERO
                       DISPLAY "RECECOM insert rennum="
                                                       RECECOM-RENNUM
                       ADD     1                   TO  CNT-OUT
                       PERFORM 20011-RECECOM-PRINT-SEC
                   ELSE
                       DISPLAY "RECECOM MCP-RC= "  MCP-RC
                       MOVE    "レセコメントに追加できませんでした"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               ELSE
      *            コメントの文字数
                   MOVE    ZERO                TO  WRK-LENGTH
                                                   FLG-RETURN
                   PERFORM VARYING IDX FROM
                                   WRK-CONS-RECECOM-LENGTH-MAX    BY  -1
                           UNTIL   IDX <       1
                       IF      WRK-RECECOM-COMMENT (1)(IDX:1)
                                               NOT =   SPACE
                           MOVE    IDX             TO  WRK-LENGTH
                           IF      WRK-RECECOM-COMMENT (1)(IDX:1)
                                                   =   WRK-CONS-RETURN
                               MOVE   1                TO  FLG-RETURN
                           END-IF
                           MOVE    1           TO  IDX 
                       END-IF
                   END-PERFORM
      *
      *            コメント（改行コードも）を追加できれば更新
                   IF    ( WRK-LENGTH          <   2377 AND
                           FLG-RETURN          =   ZERO    )
                   OR    ( WRK-LENGTH          <   2376 AND
                           FLG-RETURN          =   1       )
                       MOVE    WRK-RECECOM-REC (1) TO  RECECOM-REC
                       IF      FLG-RETURN          =   ZERO
                           COMPUTE IDY     =   WRK-LENGTH   +   1 
                           MOVE    WRK-CONS-RETURN TO  RECECOM-COMMENT
                                                                (IDY:)
                           ADD     1               TO  WRK-LENGTH
                       END-IF 
                       COMPUTE IDY     =   WRK-LENGTH   +   1 
                       MOVE    WRK-CONS-COMMENT     TO  RECECOM-COMMENT
                                                                (IDY:)
                       MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
                       MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
                       MOVE    "BG001"             TO  RECECOM-TERMID
                       MOVE    RECECOM-REC         TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_rececom"       TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC              =   ZERO
                           DISPLAY "RECECOM update rennum="
                                                       RECECOM-RENNUM
                           ADD     1                   TO  CNT-MOD
                           PERFORM 20011-RECECOM-PRINT-SEC
                       ELSE
                           DISPLAY "RECECOM MCP-RC= "  MCP-RC
                           MOVE    "レセコメントに更新できませんでした"
                                                       TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                       END-IF
                   ELSE
                       INITIALIZE                      RECECOM-REC
                       MOVE    WRK-RECECOM-KEY (1) TO  RECECOM-KEY
                       ADD     1                   TO  RECECOM-RENNUM
                       MOVE    WRK-CONS-COMMENT    TO  RECECOM-COMMENT
                       MOVE    SMCNDATE-YMD        TO  RECECOM-CREYMD
      ***              MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
      ***              MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
                       MOVE    "BG001"             TO  RECECOM-TERMID
                       MOVE    RECECOM-REC         TO  MCPDATA-REC
                       MOVE    "DBINSERT"          TO  MCP-FUNC
                       MOVE    "tbl_rececom"       TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC              =   ZERO
                           DISPLAY "RECECOM insert rennum="
                                                       RECECOM-RENNUM
                           ADD     1                   TO  CNT-OUT
                           PERFORM 20011-RECECOM-PRINT-SEC
                       ELSE
                           DISPLAY "RECECOM MCP-RC= "  MCP-RC
                           MOVE    "レセコメントに追加できませんでした"
                                                       TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                       END-IF
                   END-IF    
               END-IF
           END-IF    
           .
       2001-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント更新一覧作成処理
      *****************************************************************
       20011-RECECOM-PRINT-SEC          SECTION.
      *
           IF      CNT-LINE             >=  69
               PERFORM 200111-HEAD-PRINT-SEC
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTTOKKI-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    SPACE               TO  BODY-01
           MOVE    SPTID-PTNUM         TO  B01-PTNUM
           MOVE    PTINF-NAME          TO  B01-NAME
           INSPECT B01-NAME            REPLACING  ALL "  "  BY  "　"
           IF      FLG-JYURRK       =   1
               MOVE    "受診履歴なし"      TO  B01-ERRFLG
           ELSE
               IF      FLG-TOKKI           =   1
                   MOVE    "コメントあり"      TO  B01-ERRFLG
               ELSE
                   MOVE    RECECOM-HKNCOMBI    TO  B01-HKNCOMBI
                   MOVE    JYURRK-SRYYMD       TO  WRK-JYURRK-SRYYMD
                   MOVE    WRK-JYURRK-DD       TO  WRK-Z9
                   MOVE    WRK-Z9              TO  B01-DD
      *
                   COMPUTE IDY     =   WRK-EDYMD-DD    -   WRK-JYURRK-DD
                                                       +   1
                   IF      IDY             <   WRK-PARA-KAISU
                       DISPLAY "JYURRK-DD=" WRK-JYURRK-DD 
                       MOVE    "●"            TO  B01-ERRFLG
                   END-IF
               END-IF
           END-IF
      *
           ADD     1               TO  CNT-LINE
           MOVE    BODY-01         TO  HCPOM-MOJI (CNT-LINE)
           .
       20011-RECECOM-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       200111-HEAD-PRINT-SEC                SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 200111-PRINT-OUT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  H01-PAGE
      *
           MOVE    SPACE               TO  HCPOM
           MOVE    HEAD-01             TO  HCPOM-MOJI (1)
           MOVE    HEAD-02             TO  HCPOM-MOJI (3)
      *
           MOVE    3                   TO  CNT-LINE
           .
       200111-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       200111-PRINT-OUT-SEC                SECTION.
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
           MOVE   "HCPOM.red"          TO  SPRT-PRTID
           MOVE    HCPOM               TO  SPRT-PRTDATA
           STRING  "回数コメント更新一覧（"
                                       DELIMITED  BY  SIZE
                   WRK-SRYYM           DELIMITED  BY  SIZE
                   "分）"              DELIMITED  BY  SIZE
                                       INTO  SPRT-TITLE
           END-STRING
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
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
       200111-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
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
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBG023 "  DELIMITED  BY  SIZE
                   WRK-RECEERR     DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBG023 IN   "  CNT-IN
           DISPLAY "*** ORCBG023 INS  "  CNT-OUT
           DISPLAY "*** ORCBG023 MOD  "  CNT-MOD
           DISPLAY "*** ORCBG023 PAGE "  CNT-PAGE
           DISPLAY "*** ORCBG023 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           ADD     CNT-OUT         TO  JOB-UPDCNT
           ADD     CNT-MOD         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
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
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者特記事項情報読み込み
      *****************************************************************
       900-PTTOKKI-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTTOKKI
               MOVE    MCPDATA-REC         TO  PTTOKKI-REC
               IF    ( PTTOKKI-STYM        <=  WRK-PARA-SRYYM )
               AND   ( PTTOKKI-EDYM        >=  WRK-PARA-SRYYM )
               AND   ( PTTOKKI-CD          =   "09"            )
                   ADD     1                   TO  CNT-IN
               ELSE
                   GO  TO  900-PTTOKKI-READ-N-SEC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTTOKKI
               INITIALIZE                      PTTOKKI-REC
           END-IF
           .
       900-PTTOKKI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読み込み
      *****************************************************************
       900-JYURRK-READ-SEC          SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-JYURRK-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *        
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読み込み
      *****************************************************************
       900-JYURRK-READ-N-SEC           SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
      *
      *        医保分のみ対象とする
               IF    ( JYURRK-HKNCOMBINUM
                                       NOT =   "9999" )
               AND   ( JYURRK-HKNKBN       =   "0"    )
               AND   ( JYURRK-EDANUM       =   1      )
      *
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    JYURRK-PTID         TO  COMB-PTID
                   MOVE    JYURRK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
                   IF    ( FLG-HKNCOMBI         =   1   )
                   OR    ( COMB-HKNNUM (1:1)    =   "9" )
                       GO  TO   900-JYURRK-READ-N-SEC
                   END-IF
               ELSE
                   GO  TO   900-JYURRK-READ-N-SEC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           .
       900-JYURRK-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント読み込み
      *****************************************************************
       900-RECECOM-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECECOM
               MOVE    MCPDATA-REC         TO  RECECOM-REC
           ELSE
               MOVE    1                   TO  FLG-RECECOM
           END-IF
           .
       900-RECECOM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
