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
       PROGRAM-ID.             ORCBG005.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 長期入院対象患者一覧表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/05/21    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03/11/19  患者入院履歴よりデータを取得
      *                                     するよう修正
      *  01.00.02    NACL-太田    03/12/03  通算日数チェックに失敗時
      *                                     処理がループする不具合を修正
      *  01.00.03    NACL-太田    04/07/14  処理区分追加
      *  03.05.01    NACL-森脇    07/06/07  グループ診療対応
      *  04.08.01    NACL-森脇    15/08/24  ＣＳＶ出力対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    処理対象患者ソートファイル(順編成）
           SELECT  BG00501SEQ-FILE ASSIGN  ORCBG00501PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BG00501SEQ-SRT-G
                                   FILE    STATUS  IS  STS-BG00501SEQ.
      *
      *    処理対象患者ファイル
           SELECT  BG00501DYN-FILE ASSIGN  ORCBG00501PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BG00501DYN-SRT-G
                                   FILE    STATUS  IS  STS-BG00501DYN.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    処理対象患者ファイル（順編成）
       FD  BG00501SEQ-FILE.
       01  BG00501SEQ-REC.
           02  BG00501SEQ-SRT-G.
               03  BG00501SEQ-SRT-BTUNUM   PIC X(02).
               03  BG00501SEQ-SRT-BRMNUM   PIC X(08).
               03  BG00501SEQ-SRT-SRYKA    PIC X(02).
               03  BG00501SEQ-SRT-PTID     PIC X(10).
               03  BG00501SEQ-SRT-SEQ      PIC 9(03).
           02  BG00501SEQ-PTNYUINRRK-DAT.
           COPY    "CPPTNYUINRRK.INC"      REPLACING   //PTNYUINRRK-//
                                           BY          //BG00501SEQ-//.
      *
      *    処理対象患者ファイル
       FD  BG00501DYN-FILE.
       01  BG00501DYN-REC.
           02  BG00501DYN-SRT-G.
               03  BG00501DYN-SRT-BTUNUM   PIC X(02).
               03  BG00501DYN-SRT-BRMNUM   PIC X(08).
               03  BG00501DYN-SRT-SRYKA    PIC X(02).
               03  BG00501DYN-SRT-PTID     PIC X(10).
               03  BG00501DYN-SRT-SEQ      PIC 9(03).
           02  BG00501DYN-PTNYUINRRK-DAT.
           COPY    "CPPTNYUINRRK.INC"      REPLACING   //PTNYUINRRK-//
                                           BY          //BG00501DYN-//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    処理対象患者ファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
                                           BY          //ORCBG00501//.
           03  FILLER                      PIC X(04)   VALUE   ".dat".
      *
           COPY    "CPRECEERR.INC".
      *
      *
           COPY    "HCMG005.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
           03  STS-BG00501SEQ                      PIC X(02).
           03  STS-BG00501DYN                      PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-WKPTNYUINRRK                    PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-BG00501                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(03).
           03  CNT-BG005               PIC 9(05).
           03  CNT-BG00501DYN          PIC 9(03).
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
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
      *----(04.08.01)--ADD-START---
           03  IDW2                                PIC 9(04).
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID         PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-KJNYMD         PIC X(08).
           03  WRK-PARA-PROCKBN        PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME            PIC X(100).
           03  WRK-RECEERR             PIC X(200).
           03  WRK-YMD                 PIC X(08).
      *----(04.08.01)--UPD-START---
           03  WRK-TOUKEIERR                       PIC X(200).
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
      *----(04.08.01)--UPD-END-----
           03  WRK-EDTYMD1             PIC X(10).
           03  WRK-EDTYMD3             PIC X(22).
           03  WRK-KJNYMD              PIC X(22).
           03  WRK-SYORIYMD            PIC X(22).
           03  WRK-PAGE                PIC ZZ9.
           03  WRK-HCMG005.
               05  WRK-HCMG005-BTUNAME     PIC X(10).
               05  WRK-HCMG005-BRMNUM      PIC X(6).
               05  WRK-HCMG005-SRYKA       PIC X(8).
               05  WRK-HCMG005-PTNUM       PIC X(20).
               05  WRK-HCMG005-NAME        PIC X(30).
               05  WRK-HCMG005-SEX         PIC X(2).
               05  WRK-HCMG005-BIRTHDAY    PIC X(22).
           03  WRK-STR                 PIC X(200).
           03  WRK-LEN                 PIC 9(05).
           03  WRK-SHO                 PIC 9(05).
           03  WRK-AMARI               PIC 9(05).
      *
      *----(04.08.01)--ADD-START---
      *    ＣＳＶデータ数字編集用
           03  WRK-REC             PIC X(20000).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(20000).
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-RECSTR      PIC 9(02). 
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-RECEND      PIC 9(01).
      *
      *    ＣＳＶデータ日付編集用
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
      *    ＣＳＶデータファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(12)   VALUE
                                       "choki_nyuin_".
               05  WRK-TO-DATA-KJNYMD  PIC X(08).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
       01  WRK-CSV-AREA.
           03  WRK-CSV-G1.
               05  WRK-CSV-BTUNAME             PIC X(10).
               05  WRK-CSV-BRMNUM              PIC X(6).
               05  WRK-CSV-SRYKA               PIC X(02).
               05  WRK-CSV-SRYKAMEI            PIC X(8).
               05  WRK-CSV-PTNUM               PIC X(20).
               05  WRK-CSV-NAME                PIC X(30).
               05  WRK-CSV-SEX                 PIC X(2).
               05  WRK-CSV-BIRTHDAY            PIC X(10).
           03  WRK-CSV-G2.
               05  WRK-CSV-NYUINYMD            PIC X(10).
               05  WRK-CSV-TAIINYMD            PIC X(10).
               05  WRK-CSV-91AST               PIC X(02).
               05  WRK-CSV-91YMD               PIC X(10).
               05  WRK-CSV-180AST              PIC X(02).
               05  WRK-CSV-180YMD              PIC X(10).
      *
      *    ＣＳＶデータヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(06)   VALUE   "基準日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-KJNYMD     PIC X(10)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "作成日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-SKYYMD     PIC X(10)   VALUE   SPACE.
      *
       01  CSV-HEAD-02.
           03  FILLER              PIC X(04)   VALUE   "病棟".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "病室".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "診療科".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "診療科名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "性別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "生年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "入院日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "退院日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "９１日以上".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "１８０日以上".
      *
       01  CSV-HEAD-03.
           03  FILLER              PIC X(30)   VALUE
               "○：基準日から通算日数９１日、".
           03  FILLER              PIC X(36)   VALUE
               "または１８０日が１か月以内であるとき".
           03  FILLER              PIC X(30)   VALUE
               "／●：基準日が通算日数９１日、".
           03  FILLER              PIC X(30)   VALUE
               "または１８０日を超えているとき".
      *----(04.08.01)--ADD-END-----
      *
       01  KEY-AREA.
           03  KEY-BTUNUM              PIC X(02).
           03  KEY-BRMNUM              PIC X(08).
           03  KEY-SRYKA               PIC X(02).
           03  KEY-PTID                PIC 9(10).
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTNUM               PIC X(20).
           03  ERR-ERRMSG              PIC X(80).
           03  ERR-HEN-ERRMSG          PIC X(80).
      *
       01  CONST-AREA.
           03  CONST-LINE-MAX          PIC 9(05)   VALUE 30.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
      *    患者入院履歴ワーク
       01  WKPTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //WKPTNYUINRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
      *    病棟情報
           COPY    "CPSK5001.INC".
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
      *    入退院日取得サブパラメタ
           COPY "CPORCSNTYMD.INC".
      *
      *    病室番号編集サブ
           COPY "CPORCSBRMNUM.INC".
      *
      *    通算日数チェックサブ
           COPY "CPORCSTUSANCHK.INC".
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
      *----(04.08.01)--ADD-START---
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
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
           PERFORM UNTIL ( FLG-END     =   1 )
               PERFORM 200-MAIN-SEC
           END-PERFORM
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
           INITIALIZE                  KEY-AREA
grpsys     INITIALIZE                  SPA-AREA
      *
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
grpsys*DD                              WRK-PARA-HOSPID
grpsys                                 WRK-PARA-HOSPNUM
grpsys                                 RECEERR
                                       WRK-PARA-KJNYMD
                                       WRK-PARA-PROCKBN
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG005"          TO  JOB-PGID
           MOVE    "長期入院対象患者一覧表"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
           MOVE    "BG00501"           TO  ORCBG00501PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  ORCBG00501PARA-TERMID
grpsys     MOVE    SPA-HOSPNUM         TO  ORCBG00501PARA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    ORCBG00501PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ORCBG00501PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           IF        ( FLG-END         NOT =   ZERO )
               GO  TO  100-INIT-EXT
           END-IF
      *
           PERFORM 120-BG00501-HEN-SEC
      *
           OPEN    INPUT               BG00501SEQ-FILE
      *
           MOVE    ZERO                TO  FLG-BG00501
           PERFORM 900-BG00501SEQ-READ-SEC
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    基準日
           MOVE    WRK-PARA-KJNYMD     TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-KJNYMD
      *
      *    作成日
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SYORIYMD
      *
      *----(04.08.01)--ADD-START---
      *    ＣＳＶ出力ファイル名設定
           MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
           MOVE    WRK-PARA-KJNYMD     TO  WRK-TO-DATA-KJNYMD
      *
           MOVE    WRK-PARA-KJNYMD     TO  WRK-SYMD
           MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
           MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
           MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
           MOVE    WRK-HEN-YMD-OT      TO  CSV-HEAD-KJNYMD
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
           MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
           MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
           MOVE    WRK-HEN-YMD-OT      TO  CSV-HEAD-SKYYMD
      *----(04.08.01)--ADD-END-----
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象患者ソートファイル編集処理
      *****************************************************************
       120-BG00501-HEN-SEC             SECTION.
      *
           OPEN    OUTPUT              BG00501SEQ-FILE
           CLOSE                       BG00501SEQ-FILE
      *
           OPEN    OUTPUT              BG00501DYN-FILE
      *
           PERFORM 1201-PTNYUINRRK-SEL-SEC
      *
           MOVE    ZERO                TO  CNT-BG00501DYN
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( PTINF-TSTPTNUMKBN   NOT =   "1" )
                   PERFORM 1201-BG00501-HEN-SEC
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    1           TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE                       BG00501DYN-FILE
      *
           .
       120-BG00501-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴検索処理
      *****************************************************************
       1201-PTNYUINRRK-SEL-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-PATHNAME
      *
      *    患者入院履歴テーブル検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
      *
           IF    ( WRK-PARA-PROCKBN   =    "1" )
grpsys*DD      MOVE    WRK-PARA-HOSPID     TO  PTNYUINRRK-HOSPID
grpsys         MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
               MOVE    WRK-PARA-KJNYMD     TO  PTNYUINRRK-NYUINYMD
               MOVE    WRK-PARA-KJNYMD (1:6)
                                           TO  PTNYUINRRK-TAIINYMD
               MOVE    "01"                TO  PTNYUINRRK-TAIINYMD(7:2)
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "key32"             TO  WRK-PATHNAME
           ELSE
grpsys*DD      MOVE    WRK-PARA-HOSPID     TO  PTNYUINRRK-HOSPID
grpsys         MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
               MOVE    WRK-PARA-KJNYMD     TO  PTNYUINRRK-NYUINYMD
                                               PTNYUINRRK-TAIINYMD
               MOVE    "key32"             TO  WRK-PATHNAME
           END-IF
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           END-IF
      *
           .
       1201-PTNYUINRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル編集処理
      *****************************************************************
       1201-BG00501-HEN-SEC            SECTION.
      *
           COMPUTE CNT-BG00501DYN  =   CNT-BG00501DYN  +   1
      *
           MOVE    ZERO                TO  FLG-WKPTNYUINRRK
      *
      *    入院履歴マスタ検索
           MOVE    PTNYUINRRK-REC      TO  WKPTNYUINRRK-REC
      *
           MOVE    WKPTNYUINRRK-BTUNUM TO  BG00501DYN-SRT-BTUNUM
           MOVE    WKPTNYUINRRK-BRMNUM TO  BG00501DYN-SRT-BRMNUM
           MOVE    WKPTNYUINRRK-NYUINKA
                                       TO  BG00501DYN-SRT-SRYKA
           MOVE    WKPTNYUINRRK-PTID   TO  BG00501DYN-SRT-PTID
      *
           MOVE    CNT-BG00501DYN      TO  BG00501DYN-SRT-SEQ
      *
           MOVE    WKPTNYUINRRK-REC    TO  BG00501DYN-PTNYUINRRK-DAT
      *
           WRITE   BG00501DYN-REC
      *
           .
       1201-BG00501-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  CNT-PAGE
      *
           MOVE    ZERO                TO  CNT-BG005
      *
      *----(04.08.01)--ADD-START---
           IF      FLG-BG00501     =   ZERO
      *        ＣＳＶ出力処理
               PERFORM 270-CSV-HEAD-SET-SEC
               INITIALIZE                  WRK-CSV-AREA
           END-IF
      *----(04.08.01)--ADD-END-----
      *
           PERFORM UNTIL ( FLG-BG00501     =   1 )
                   OR    ( FLG-END     NOT =   ZERO )
      *
               MOVE    SPACE               TO  HCMG005
      *
      *        ヘッダ編集処理
               PERFORM 210-HEAD-EDIT-SEC
      *
               MOVE    ZERO                TO  CNT-LINE
               PERFORM UNTIL ( FLG-BG00501     =   1 )
                       OR    ( CNT-LINE        >=  CONST-LINE-MAX )
                       OR    ( FLG-END     NOT =   ZERO )
      *
      *            明細編集処理
                   PERFORM 210-BODY-EDIT-SEC
                   IF    ( FLG-END     NOT =   ZERO )
                       GO  TO  200-MAIN-EXT
                   END-IF
      *
               END-PERFORM
      *
      *        出力処理
               PERFORM 400-PRINT-OUT-SEC
      *
           END-PERFORM
      *
           CLOSE                       BG00501SEQ-FILE
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       210-HEAD-EDIT-SEC           SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
      *
           MOVE    WRK-KJNYMD      TO  HCMG005-KJNYMD
      *
           MOVE    WRK-SYORIYMD    TO  HCMG005-SYORIYMD
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCMG005-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *----(04.08.01)--UPD-START---
      *****************************************************************
      *    帳票編集<明細部>処理
      *****************************************************************
       210-BODY-EDIT-SEC           SECTION.
      *
           COMPUTE CNT-LINE        =   CNT-LINE    +   1
      *
           MOVE    SPACE           TO  WRK-HCMG005-BTUNAME
                                       WRK-HCMG005-BRMNUM
                                       WRK-HCMG005-SRYKA
                                       WRK-HCMG005-PTNUM
                                       WRK-HCMG005-NAME
                                       WRK-HCMG005-SEX
                                       WRK-HCMG005-BIRTHDAY
      *
           IF        ( CNT-LINE        =   1 )
               OR    ( KEY-BTUNUM  NOT =   PTNYUINRRK-BTUNUM )
               PERFORM 2101-BODY-BTUNAME-EDIT-SEC
           END-IF
      *
           IF        ( CNT-LINE        =   1 )
               OR    ( KEY-BTUNUM  NOT =   PTNYUINRRK-BTUNUM )
               OR    ( KEY-BRMNUM  NOT =   PTNYUINRRK-BRMNUM )
               PERFORM 2101-BODY-BRMNUM-EDIT-SEC
           END-IF
      *
           IF        ( CNT-LINE        =   1 )
               OR    ( KEY-BTUNUM  NOT =   PTNYUINRRK-BTUNUM )
               OR    ( KEY-BRMNUM  NOT =   PTNYUINRRK-BRMNUM )
               OR    ( KEY-SRYKA   NOT =   PTNYUINRRK-NYUINKA )
               PERFORM 2101-BODY-SRYKA-EDIT-SEC
           END-IF
      *
           IF    ( CNT-LINE        =   1 )
            OR   ( KEY-BTUNUM  NOT =   PTNYUINRRK-BTUNUM )
            OR   ( KEY-BRMNUM  NOT =   PTNYUINRRK-BRMNUM )
            OR   ( KEY-SRYKA   NOT =   PTNYUINRRK-NYUINKA )
            OR   ( KEY-PTID    NOT =   PTNYUINRRK-PTID )
               PERFORM 2101-BODY-PTINF-EDIT-SEC
           END-IF
      *
      *    キーを設定
           MOVE    PTNYUINRRK-BTUNUM         TO  KEY-BTUNUM
           MOVE    PTNYUINRRK-BRMNUM         TO  KEY-BRMNUM
           MOVE    PTNYUINRRK-NYUINKA        TO  KEY-SRYKA
           MOVE    PTNYUINRRK-PTID           TO  KEY-PTID
      *
      *    病棟名称
           MOVE    WRK-HCMG005-BTUNAME TO  HCMG005-BTUNAME(CNT-LINE)
      *
      *    病室名称
           MOVE    WRK-HCMG005-BRMNUM  TO  HCMG005-BRMNUM(CNT-LINE)
      *
      *    入院科
           MOVE    WRK-HCMG005-SRYKA   TO  HCMG005-SRYKA(CNT-LINE)
      *
      *    患者番号
           IF        ( WRK-HCMG005-PTNUM(11:1) =   SPACE )
               MOVE    WRK-HCMG005-PTNUM   TO  HCMG005-PTNUM(CNT-LINE)
           ELSE
               MOVE    WRK-HCMG005-PTNUM   TO  HCMG005-PTNUM2(CNT-LINE)
           END-IF
      *
      *    氏名
           MOVE    WRK-HCMG005-NAME    TO  HCMG005-NAME(CNT-LINE)
      *
      *    性別
           MOVE    WRK-HCMG005-SEX     TO  HCMG005-SEX(CNT-LINE)
      *
      *    生年月日
           MOVE    WRK-HCMG005-BIRTHDAY
                                       TO  HCMG005-BIRTHDAY(CNT-LINE)
      *
           INITIALIZE                      WRK-CSV-G2
      *
      *    入院日
           INITIALIZE                          ORCSNTYMDAREA
grpsys*DD  MOVE    PTINF-HOSPID            TO  SNTYMD-HOSPID
grpsys     MOVE    PTINF-HOSPNUM           TO  SNTYMD-HOSPNUM
           MOVE    PTINF-PTID              TO  SNTYMD-PTID
           MOVE    "2"                     TO  SNTYMD-KBN
           CALL    "ORCSNTYMD" USING           ORCSNTYMDAREA
grpsys                                         SPA-AREA
      *
           PERFORM VARYING IDX2   FROM    1   BY  1
                   UNTIL ( IDX2       >   SNTYMD-MAX )
               IF    ( PTNYUINRRK-RRKNUM       = SNTYMD-RRKNUM (IDX2))
      *
                   MOVE    SNTYMD-NYUINYMD (IDX2)
                                           TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD1     TO  HCMG005-NYUINYMD
                                                           (CNT-LINE)
                   MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
                   MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
                   MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
                   MOVE    WRK-HEN-YMD-OT      TO  WRK-CSV-NYUINYMD
      *
                   MOVE    SNTYMD-MAX      TO  IDX2
      *
               END-IF
      *
           END-PERFORM
      *
      *    退院日
           IF    ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
               MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1             TO  HCMG005-TAIINYMD
                                                       (CNT-LINE)
               MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT      TO  WRK-CSV-TAIINYMD
           END-IF
      *
      *    通算日数チェックサブ呼出
           INITIALIZE                      ORCSTUSANCHKAREA
grpsys*DD  MOVE    WRK-PARA-HOSPID     TO  STUSANCHK-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  STUSANCHK-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  STUSANCHK-PTID
           MOVE    PTNYUINRRK-NYUINYMD TO  STUSANCHK-NYUINYMD
           IF    ( PTNYUINRRK-TAIINYMD >=  WRK-PARA-KJNYMD )
               MOVE    WRK-PARA-KJNYMD TO  STUSANCHK-KJNYMD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                       TO  STUSANCHK-KJNYMD
           END-IF
      *
           MOVE    PTNYUINRRK-RRKNUM   TO  STUSANCHK-RRKNUM
      *
           CALL    "ORCSTUSANCHK"          USING
                                           ORCSTUSANCHKAREA
grpsys                                     SPA-AREA
           IF        ( STUSANCHK-RC    NOT =   ZERO )
               MOVE    PTNUM-PTNUM         TO  ERR-PTNUM
               MOVE    "通算日数の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  210-BODY-EDIT-EXT
           END-IF
      *
      *    ９１日以上
           IF    ( STUSANCHK-DAY-91    NOT =   SPACE )
            AND  ( STUSANCHK-DAY-91       <=   PTNYUINRRK-TAIINYMD )
             IF  ( STUSANCHK-DAY-91-STS    =   ZERO OR "1" )
               MOVE    STUSANCHK-DAY-91    TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1         TO  HCMG005-DAY-91(CNT-LINE)
               MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT      TO  WRK-CSV-91YMD
               IF    ( WRK-PARA-KJNYMD     >   STUSANCHK-DAY-91 )
                   MOVE    "●"            TO  HCMG005-AST-91(CNT-LINE)
                                               WRK-CSV-91AST
               ELSE
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY5-AREA
                   MOVE    "51"            TO  LNK-DAY5-IRAI
                   MOVE    WRK-PARA-KJNYMD
                                           TO  LNK-DAY5-START
                   MOVE    STUSANCHK-DAY-91
                                           TO  LNK-DAY5-END
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY5-AREA
                   IF    ( LNK-DAY5-NISSU2  <=  31 )
                       MOVE    "○"        TO  HCMG005-AST-91(CNT-LINE)
                                               WRK-CSV-91AST
                   ELSE
                       MOVE    "　"        TO  HCMG005-AST-91(CNT-LINE)
                                               WRK-CSV-91AST
                   END-IF
               END-IF
             ELSE
               MOVE    "*********"         TO  HCMG005-DAY-91(CNT-LINE)
                                               WRK-CSV-91YMD
             END-IF
           END-IF
      *
      *    １８０日以上
           IF    ( STUSANCHK-DAY-180   NOT =   SPACE )
            AND  ( STUSANCHK-DAY-180      <=   PTNYUINRRK-TAIINYMD )
             IF  ( STUSANCHK-DAY-180-STS   =   ZERO OR "2"  )
               MOVE    STUSANCHK-DAY-180   TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1         TO  HCMG005-DAY-180(CNT-LINE)
               MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT      TO  WRK-CSV-180YMD
               IF    ( WRK-PARA-KJNYMD     >   STUSANCHK-DAY-180 )
                   MOVE    "●"            TO  HCMG005-AST-180(CNT-LINE)
                                               WRK-CSV-180AST
               ELSE
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY5-AREA
                   MOVE    "51"            TO  LNK-DAY5-IRAI
                   MOVE    WRK-PARA-KJNYMD
                                           TO  LNK-DAY5-START
                   MOVE    STUSANCHK-DAY-180
                                           TO  LNK-DAY5-END
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY5-AREA
                   IF    ( LNK-DAY5-NISSU2  <=  31 )
                       MOVE    "○"        TO  HCMG005-AST-180(CNT-LINE)
                                               WRK-CSV-180AST
                   ELSE
                       MOVE    "　"        TO  HCMG005-AST-180(CNT-LINE)
                                               WRK-CSV-180AST
                   END-IF
               END-IF
             ELSE
               MOVE    "*********"         TO  HCMG005-DAY-180(CNT-LINE)
                                               WRK-CSV-180YMD
             END-IF
           END-IF
      *
      *    ＣＳＶ出力処理
           PERFORM 270-CSV-BODY-SET-SEC
      *
           COMPUTE CNT-BG005       =   CNT-BG005   +   1
      *
           PERFORM 900-BG00501SEQ-READ-SEC
      *
           .
       210-BODY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称編集処理
      *****************************************************************
       2101-BODY-BTUNAME-EDIT-SEC  SECTION.
      *
      *    病棟情報
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    WRK-PARA-KJNYMD     TO  SYS-5001-STYUKYMD
                                           SYS-5001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               INITIALIZE                      SYS-5001-REC
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-5001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    SYS-5001-BTU-TANNAME    TO  WRK-HCMG005-BTUNAME
                                               WRK-CSV-BTUNAME
      *
           .
       2101-BODY-BTUNAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室名称編集処理
      *****************************************************************
       2101-BODY-BRMNUM-EDIT-SEC   SECTION.
      *
           INITIALIZE  ORCSBRMNUMAREA
           MOVE   PTNYUINRRK-BRMNUM(3:)
                                       TO  SBRMNUM-IN-BRMNUM
           CALL   "ORCSBRMNUM"         USING
                                       ORCSBRMNUMAREA
grpsys                                 SPA-AREA
      *
           MOVE   SBRMNUM-OT-BRMNUM    TO  WRK-HCMG005-BRMNUM
                                           WRK-CSV-BRMNUM
      *
           .
       2101-BODY-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       2101-BODY-SRYKA-EDIT-SEC    SECTION.
      *
      *    診療科情報
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    PTNYUINRRK-NYUINKA  TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-KJNYMD     TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               INITIALIZE                      SYS-1005-REC
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    SYS-1005-SRYKANAME2 TO  WRK-HCMG005-SRYKA
                                           WRK-CSV-SRYKAMEI
           MOVE    PTNYUINRRK-NYUINKA  TO  WRK-CSV-SRYKA
      *
           .
       2101-BODY-SRYKA-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       2101-BODY-PTINF-EDIT-SEC    SECTION.
      *
      *    患者番号読み込み
           INITIALIZE                  PTNUM-REC
grpsys*DD  MOVE    PTNYUINRRK-HOSPID   TO  PTNUM-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               INITIALIZE                  PTNUM-REC
           ELSE
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    患者番号
           MOVE    PTNUM-PTNUM     TO  WRK-HCMG005-PTNUM
                                       WRK-CSV-PTNUM
      *
      *    氏名
           MOVE    PTINF-NAME      TO  WRK-HCMG005-NAME
                                       WRK-CSV-NAME
      *
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"        TO  WRK-HCMG005-SEX
                                           WRK-CSV-SEX
               WHEN    "2"
                   MOVE    "女"        TO  WRK-HCMG005-SEX
                                           WRK-CSV-SEX
           END-EVALUATE
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1     TO  WRK-HCMG005-BIRTHDAY
           MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
           MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
           MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
           MOVE    WRK-HEN-YMD-OT      TO  WRK-CSV-BIRTHDAY
      *
           .
       2101-BODY-PTINF-EDIT-EXT.
           EXIT.
      *----(04.08.01)--UPD-END-----
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
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
           MOVE   "HCMG005.red"        TO  SPRT-PRTID
           MOVE    "長期入院対象患者一覧表"
                                       TO  SPRT-TITLE
           MOVE    HCMG005             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
grpsys                                     SPA-AREA
           IF          SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *----(04.08.01)--ADD-START---
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       270-CSV-HEAD-SET-SEC        SECTION.
      *
           INITIALIZE                  WRK-REC
           MOVE    1               TO  WRK-REC-LENGTH
      *
           MOVE    CSV-HEAD-01     TO  WRK-DATA
           MOVE    100             TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-KAIGYO          DELIMITED  BY  SIZE
           INTO    WRK-REC
           WITH    POINTER  WRK-REC-LENGTH
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           INITIALIZE                  WRK-REC
           MOVE    1               TO  WRK-REC-LENGTH
      *
           MOVE    CSV-HEAD-03     TO  WRK-DATA
           MOVE    150             TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-KAIGYO          DELIMITED  BY  SIZE
           INTO    WRK-REC
           WITH    POINTER  WRK-REC-LENGTH
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           INITIALIZE                  WRK-REC
           MOVE    1               TO  WRK-REC-LENGTH
      *
           MOVE    CSV-HEAD-02     TO  WRK-DATA
           MOVE    120             TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-KAIGYO          DELIMITED  BY  SIZE
           INTO    WRK-REC
           WITH    POINTER  WRK-REC-LENGTH
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-HEAD-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       270-CSV-BODY-SET-SEC        SECTION.
      *
           INITIALIZE                  WRK-REC
           MOVE    1               TO  WRK-REC-LENGTH
      *
      *    病棟
           MOVE    WRK-CSV-BTUNAME TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    病室
           MOVE    WRK-CSV-BRMNUM  TO  WRK-DATA
           MOVE    6               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療科
           MOVE    WRK-CSV-SRYKA   TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療科名
           MOVE    WRK-CSV-SRYKAMEI
                                   TO  WRK-DATA
           MOVE    8               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    患者番号
           MOVE    WRK-CSV-PTNUM   TO  WRK-DATA
           MOVE    20              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    患者氏名
           MOVE    WRK-CSV-NAME    TO  WRK-DATA
           MOVE    30              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    性別
           MOVE    WRK-CSV-SEX     TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    誕生日
           MOVE    WRK-CSV-BIRTHDAY    TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    入院日
           MOVE    WRK-CSV-NYUINYMD    TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    退院日
           MOVE    WRK-CSV-TAIINYMD    TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ９１日経過
           MOVE    WRK-CSV-91AST   TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ９１日目
           MOVE    WRK-CSV-91YMD   TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １８０日経過
           MOVE    WRK-CSV-180AST  TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １８０日目
           MOVE    WRK-CSV-180YMD  TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-KAIGYO          DELIMITED  BY  SIZE
           INTO    WRK-REC
           WITH    POINTER  WRK-REC-LENGTH
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-BODY-SET-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       500-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "INS"           TO  STOUKEICSV-MODE
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
           MOVE    SPACE           TO  STOUKEICSV-SRYYM
           MOVE    SPACE           TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
      *
           MOVE    "長期入院対象患者一覧表ＣＳＶ作成"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
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
       500-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
               CONTINUE
           ELSE
               STRING  FUNCTION TRIM(WRK-DATA) DELIMITED BY SIZE
               INTO    WRK-REC
               WITH    POINTER  WRK-REC-LENGTH
               END-STRING
           END-IF
      *
           IF      WRK-RECEND         =   ZERO
               STRING  WRK-KUGIRI                DELIMITED  BY  SIZE
               INTO    WRK-REC
               WITH    POINTER  WRK-REC-LENGTH
               END-STRING
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
      *
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG005 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *----(04.08.01)--ADD-END-----
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
           MOVE    ORCBG00501PARA  TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           IF    ( CNT-BG005           =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG005 IN " CNT-BG005  " ***"
           DISPLAY "*** ORCBG005 END ***"
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
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-YMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
                                       WRK-EDTYMD3
      *
           IF        ( WRK-SYMD        =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-ERRMSG-EDIT-SEC         SECTION.
      *
           MOVE    SPACE           TO  ERR-HEN-ERRMSG
      *
           MOVE    1               TO  WRK-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR     WITH    POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN         =   WRK-LEN -   1
      *
           MOVE    ZERO            TO  WRK-SHO
                                       WRK-AMARI
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号："      DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           ELSE
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号:"       DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           END-IF
      *
           .
       800-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理対象患者ファイル読込処理
      *****************************************************************
       900-BG00501SEQ-READ-SEC             SECTION.
      *
           READ    BG00501SEQ-FILE     NEXT
           AT  END
               MOVE    1               TO  FLG-BG00501
           NOT AT  END
               MOVE    BG00501SEQ-PTNYUINRRK-DAT
                                       TO  PTNYUINRRK-REC
           END-READ
      *
           .
       900-BG00501SEQ-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
      *    患者情報読み込み
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               INITIALIZE                  PTINF-REC
           ELSE
               MOVE    MCPDATA-REC     TO  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
