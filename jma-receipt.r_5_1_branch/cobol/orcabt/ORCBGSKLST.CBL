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
       PROGRAM-ID.             ORCBGSKLST.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : システム管理設定内容リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/11/15    NACL-森脇     新規作成
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
               05  TOKEI-KANRICD       PIC X(04).
               05  TOKEI-KBNCD         PIC X(08).
               05  TOKEI-STYUKYMD      PIC X(08).
               05  TOKEI-EDYUKYMD      PIC X(08).
           03  TOKEI-NAME              PIC X(40).
           03  TOKEI-MAX               PIC 9(03).
           03  TOKEI-ITEM              PIC X(100)  OCCURS 500.
           03  TOKEI-NAIYO             PIC X(500)  OCCURS 500.
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
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYSKANRI2           PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-LINE                PIC 9(02).
           03  CNT-TOKEI               PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX-PARA                PIC 9(02).
           03  IDX-MAX                 PIC 9(03).
           03  IDX-STR                 PIC 9(03).
           03  IDX-KET                 PIC 9(03).
           03  IDX-GYO                 PIC 9(01).
           03  IDX-SYO                 PIC 9(01).
           03  IDX-PRT                 PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-KANRICD        PIC X(04)   OCCURS 10.
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
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-KANRICD             PIC X(04).
           03  WRK-KBNCD               PIC 9(04).
           03  WRK-CD                  PIC X(08).
           03  WRK-MEISAI              PIC X(600).
           03  WRK-HENMEISAI-G.
               05  WRK-HENMEISAI       PIC X(110)  OCCURS  7.
      *
           03  WRK-CSVREC              PIC X(20000).
      *
           03  WRK-SUUJI-G.
               05  WRK-SUUJI           PIC X(20).
               05  WRK-SEISU           PIC X(10).
               05  WRK-SYOSU           PIC X(09).
               05  WRK-HENSUUJI        PIC X(20).
      *
           03  WRK-PRTPRO-G.
               05  WRK-PRTPRO          PIC X(20)  OCCURS  20.
      *
           03  WRK-Z03                 PIC ZZ9.
      *
       01  WRK-PRT-AREA.
      *    見出し
           03  WRK-PRT-LINE1.
               05  FILLER              PIC X(42)   VALUE   SPACE.
               05  FILLER              PIC X(26)   VALUE
                   "システム管理設定内容リスト".
           03  WRK-PRT-LINE2.
               05  FILLER              PIC X(12)   VALUE
                                                   "管理コード：".
               05  WRK-PRT-KANRICD     PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  WRK-PRT-NAME        PIC X(40)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE   "作成日：".
               05  WRK-PRT-PRTYMD      PIC X(22)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   SPACE.
               05  WRK-PRT-PAGE        PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   " 頁".
           03  WRK-PRT-LINE3.
               05  FILLER              PIC X(12)   VALUE
                                                   "区分コード：".
               05  WRK-PRT-KBNCD       PIC X(08)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(10)   VALUE   "有効期間：".
               05  WRK-PRT-STYUKYMD    PIC X(08)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "〜".
               05  WRK-PRT-EDYUKYMD    PIC X(08)   VALUE   SPACE.
           03  WRK-PRT-SEN.
               05  FILLER              PIC X(40)   VALUE
                   "----------------------------------------".
               05  FILLER              PIC X(40)   VALUE
                   "----------------------------------------".
               05  FILLER              PIC X(30)   VALUE
                   "------------------------------".
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(14)   VALUE
                                                   "syskanri-list-".
               05  WRK-TO-DATA-PRTYMD  PIC X(08).
               05  FILLER              PIC X(04)   VALUE   ".txt".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC"    REPLACING  //SYS//
                                       BY         //SYS2//.
      *
      *    施設基準コード
           COPY    "CPSK0031.INC".
      *    帳票プログラム選択情報 
           COPY    "CPSK1032.INC".
      *    帳票プログラム選択情報（入院）
           COPY    "CPSK1035.INC".
      *    自費プログラム選択情報
           COPY    "CPSK1013.INC".
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
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    システム管理定義取得サブ
           COPY    "CPORCSSKDEF.INC".
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
           IF      FLG-END         =   ZERO
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
                                       WRK-PARA-KANRICD(1)
                                       WRK-PARA-KANRICD(2)
                                       WRK-PARA-KANRICD(3)
                                       WRK-PARA-KANRICD(4)
                                       WRK-PARA-KANRICD(5)
                                       WRK-PARA-KANRICD(6)
                                       WRK-PARA-KANRICD(7)
                                       WRK-PARA-KANRICD(8)
                                       WRK-PARA-KANRICD(9)
                                       WRK-PARA-KANRICD(10)
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBGSKLST"    TO  JOB-PGID
           MOVE    "システム管理設定内容リスト"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    "BGSKLST"       TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM     TO  TOKEIPARA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME
                                   TO  SGETTEMP-BASENAMES(1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES(2)
           CALL    "ORCSGETTEMP"       USING
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
      *    指定されたシステム管理コードを編集
           PERFORM VARYING IDX-PARA    FROM    1   BY  1
                   UNTIL   IDX-PARA    >   10
               IF      WRK-PARA-KANRICD(IDX-PARA)  NOT =   SPACE
                   MOVE    WRK-PARA-KANRICD(IDX-PARA)
                                           TO  WRK-KANRICD
                   PERFORM VARYING IDX-STR     FROM    1   BY  1
                           UNTIL   IDX-STR     >   4
                       IF      WRK-KANRICD(IDX-STR:1)  =   "*"
                           MOVE    "%"     TO  WRK-KANRICD(IDX-STR:1)
                       END-IF
                   END-PERFORM
                   MOVE    WRK-KANRICD TO  WRK-PARA-KANRICD(IDX-PARA)
               END-IF
           END-PERFORM
      *
           MOVE    WRK-SYSYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-PRT-PRTYMD
      *
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
           MOVE    WRK-SYSYMD      TO  WRK-TO-DATA-PRTYMD
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
      *    指定されたシステム管理コードがなくなるまで
           PERFORM VARYING IDX-PARA    FROM    1   BY  1
                   UNTIL   IDX-PARA    >   10
               IF      WRK-PARA-KANRICD(IDX-PARA)  NOT =   SPACE
      *            システム管理読み込み処理
                   PERFORM 210-MAIN-SYSKANRI-SEC
               END-IF
           END-PERFORM
      *
           CLOSE                       TOKEI-FILE
      *
           IF      CNT-TOKEI       NOT =   ZERO
               OPEN    INPUT               TOKEI-FILE
      *        統計出力処理
               PERFORM 300-TOKEI-OUT-SEC
               CLOSE                       TOKEI-FILE
           END-IF
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    システム管理読み込み処理
      *****************************************************************
       210-MAIN-SYSKANRI-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-KANRICD
      *
      *    システム管理読み込み
           PERFORM 900-SYS-KEY29-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SYSKANRI    =   1
               EVALUATE    SYS-KANRICD
                   WHEN    "0000"  THRU  "1000"
      *                1000以下は対象外
                       CONTINUE
                   WHEN    "1006"
      *                施設基準チェック
                       PERFORM 220-MAIN-SSTKJN-SEC
                   WHEN    "1007"
      *                自動算定・チェック機能制御情報
                       PERFORM 220-MAIN-1007-SEC
                   WHEN    "1008"
      *                チェック機能制御はまとめるので対象外
                       CONTINUE
                   WHEN    "1010"
      *                職員情報
                       PERFORM 220-MAIN-1010-SEC
                   WHEN    "1050"
      *                職員情報はまとめるので対象外
                       CONTINUE
                   WHEN    "1031"
      *                出力先プリンタチェック
                       PERFORM 220-MAIN-PRTPRO-SEC
                   WHEN    "1032"
                   WHEN    "1034"
                   WHEN    "1035"
      *                出力先プリンタはまとめるので対象外
                       CONTINUE
                   WHEN    "1046"
      *                自費チェック
                       PERFORM 220-MAIN-1046-SEC
                   WHEN    "1201"
      *                点数算定基準チェック
                       PERFORM 220-MAIN-1201-SEC
                   WHEN    "1202"
      *                点数算定基準はまとめるので対象外
                       CONTINUE
                   WHEN    "1901"
      *                医療機関編集情報
                       PERFORM 220-MAIN-PRTADRS-SEC
                   WHEN    "1902"
                   WHEN    "1903"
      *                医療機関編集情報はまとめるので対象外
                       CONTINUE
                   WHEN    "2005"
      *                医療機関編集情報
                       IF    ( SYS-KBNCD       =   "*" )
                           PERFORM 220-MAIN-2005-SEC
                       ELSE
                           CONTINUE
                       END-IF
                   WHEN    "2008"
                   WHEN    "2009"
                       CONTINUE
                   WHEN    "5000"
      *                医療機関情報−入院基本
                       PERFORM 220-MAIN-NYUIN-SEC
                   WHEN    "5001"
      *                医療機関情報−病棟情報
                       PERFORM 220-MAIN-BYOTO-SEC
                   WHEN    "5008"
      *                病棟情報はまとめるので対象外
                       CONTINUE
                   WHEN    "5020"
      *                医療機関情報−入院基本はまとめるので対象外
                       CONTINUE
                   WHEN    OTHER
      *                システム管理定義取得サブチェック
                       PERFORM 220-MAIN-SUBCHK-SEC
               END-EVALUATE
      *
      *        システム管理読み込み
               PERFORM 900-SYS-KEY29-DBFETCH-SEC
           END-PERFORM
      *
      *    システム管理読み込み
           PERFORM 900-SYS-KEY29-DBCLOSE-SEC
           .
       210-MAIN-SYSKANRI-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック
      *****************************************************************
       220-MAIN-SSTKJN-SEC         SECTION.
      *
      *    項目をセットする
           PERFORM VARYING IDX-MAX FROM    1   BY  1
                   UNTIL ( IDX-MAX >   500 )
               IF      SYS-KANRITBL(IDX-MAX:1) =   "1"
      *            施設基準セット
                   PERFORM 230-SSTKJN-SET-SEC
               END-IF
           END-PERFORM
           .
       220-MAIN-SSTKJN-EXT.
           EXIT.
      *****************************************************************
      *    施設基準セット
      *****************************************************************
       230-SSTKJN-SET-SEC         SECTION.
      *
           MOVE   ZERO             TO  WRK-KBNCD
           EVALUATE    SYS-KBNCD
               WHEN    "1"
                   MOVE    IDX-MAX     TO  WRK-KBNCD
               WHEN    "2"
                   COMPUTE WRK-KBNCD   =   IDX-MAX +   500
               WHEN    "7"
                   COMPUTE WRK-KBNCD   =   IDX-MAX +   3000
               WHEN    "8"
                   COMPUTE WRK-KBNCD   =   IDX-MAX +   3500
               WHEN    "17"
                   COMPUTE WRK-KBNCD   =   IDX-MAX +   8000
           END-EVALUATE
      *
           INITIALIZE                  TOKEI-REC
           MOVE    SYS-KANRICD     TO  TOKEI-KANRICD
           MOVE    WRK-KBNCD       TO  TOKEI-KBNCD
           MOVE    SYS-STYUKYMD    TO  TOKEI-STYUKYMD
           MOVE    SYS-EDYUKYMD    TO  TOKEI-EDYUKYMD
      *
           PERFORM 900-TOKEI-READ-SEC
      *    データがある場合は抜ける
           IF      FLG-TOKEI       =   ZERO
               GO  TO  230-SSTKJN-SET-EXT
           END-IF
      *
           COMPUTE CNT-TOKEI   =   CNT-TOKEI   +   1
      *
           MOVE    "施設基準情報コード"
                                   TO  TOKEI-NAME
           MOVE    1               TO  TOKEI-MAX
           MOVE    "1"             TO  TOKEI-NAIYO(TOKEI-MAX)
      *
      *    システム管理マスタ検索（施設基準コード）
           INITIALIZE                  SYS-0031-REC
           MOVE    SPA-HOSPNUM     TO  SYS-0031-HOSPNUM
           MOVE    "0031"          TO  SYS-0031-KANRICD
           MOVE    WRK-KBNCD       TO  SYS-0031-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-0031-STYUKYMD
                                       SYS-0031-EDYUKYMD
           MOVE    SYS-0031-REC    TO  MCPDATA-REC
           PERFORM 900-SYS-KEY10-DBREAD-SEC
      *
           IF      FLG-SYSKANRI2   =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-0031-REC
           ELSE
               INITIALIZE                  SYS-0031-REC
           END-IF
      *
           IF      SYS-0031-SSTKIJUN   =   SPACE
               MOVE    "施設基準なし"      TO  SYS-0031-SSTKIJUN
           END-IF
      *
           STRING  WRK-KBNCD           DELIMITED   BY  SPACE
                   "："                DELIMITED   BY  SIZE
                   SYS-0031-SSTKIJUN   DELIMITED   BY  SPACE
                   INTO                TOKEI-ITEM(1)
           END-STRING
      *
           WRITE   TOKEI-REC
           .
       230-SSTKJN-SET-EXT.
           EXIT.
      *****************************************************************
      *    自動算定・チェック機能制御情報チェック
      *****************************************************************
       220-MAIN-1007-SEC        SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
           MOVE     "1008"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           .
       220-MAIN-1007-EXT.
           EXIT.
      *****************************************************************
      *    職員情報チェック
      *****************************************************************
       220-MAIN-1010-SEC        SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
           MOVE     "1050"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           .
       220-MAIN-1010-EXT.
           EXIT.
      *****************************************************************
      *    自費チェック
      *****************************************************************
       220-MAIN-1046-SEC         SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
      *    帳票プログラム選択情報 
           PERFORM 230-PRTPRO-1013-SEC
      *
           .
       220-MAIN-1046-EXT.
           EXIT.
      *****************************************************************
      *    自費プログラム選択情報 
      *****************************************************************
       230-PRTPRO-1013-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-PRTPRO-G
      *
           INITIALIZE                  SYS-1013-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1013-HOSPNUM
           MOVE    "1013"          TO  SYS-1013-KANRICD
           MOVE    SYS-STYUKYMD    TO  SYS-1013-STYUKYMD
           MOVE    SYS-EDYUKYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC    TO  MCPDATA-REC
           PERFORM 900-SYS-KEY2-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SYSKANRI2   =   1
               MOVE    MCPDATA-REC     TO  SYS-1013-REC
               MOVE    SYS-1013-KBNCD  TO  IDX-PRT
               EVALUATE    IDX-PRT
               WHEN    11  THRU  19
                   COMPUTE IDX-PRT = IDX-PRT - 10
               WHEN    21  THRU  29
                   COMPUTE IDX-PRT = IDX-PRT - 10
               END-EVALUATE
               MOVE    SYS-1013-JIHINAME   TO  WRK-PRTPRO(IDX-PRT)
               PERFORM 900-SYS-KEY2-DBFETCH-SEC
           END-PERFORM
      *
           PERFORM 900-SYS-KEY2-DBCLOSE-SEC
      *
           PERFORM VARYING IDX-PRT FROM 1 BY 1
                   UNTIL   IDX-PRT >   20
               EVALUATE    IDX-PRT
               WHEN    1   THRU  10
                   MOVE    IDX-PRT     TO  IDX-MAX
               WHEN    11  THRU  20
                   COMPUTE IDX-MAX = IDX-PRT + 30
               END-EVALUATE
               IF      WRK-PRTPRO(IDX-PRT) NOT =   SPACE
                   MOVE    SPACE           TO  WRK-MEISAI
                   STRING  TOKEI-ITEM(IDX-MAX)
                                               DELIMITED BY SPACE
                           "−"                DELIMITED BY SIZE
                           WRK-PRTPRO(IDX-PRT) DELIMITED BY SPACE
                           INTO                WRK-MEISAI
                   END-STRING
                   MOVE    SPACE           TO  TOKEI-ITEM(IDX-MAX)
                   MOVE    WRK-MEISAI      TO  TOKEI-ITEM(IDX-MAX)
               ELSE
                   MOVE    SPACE           TO  TOKEI-NAIYO(IDX-MAX)
               END-IF
           END-PERFORM
      *
           REWRITE TOKEI-REC
           .
       230-PRTPRO-1013-EXT.
           EXIT.
      *****************************************************************
      *    出力先プリンタチェック
      *****************************************************************
       220-MAIN-PRTPRO-SEC         SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
      *    帳票プログラム選択情報 
           PERFORM 230-PRTPRO-1032-SEC
      *
      *    出力先プリンタ情報（入院）
           MOVE     "1034"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           IF      FLG-SYSKANRI2   =   1
               GO  TO  220-MAIN-PRTPRO-EXT
           END-IF
      *
      *    帳票プログラム選択情報 
           PERFORM 230-PRTPRO-1035-SEC
      *
           .
       220-MAIN-PRTPRO-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム選択情報 
      *****************************************************************
       230-PRTPRO-1032-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-PRTPRO-G
      *
           INITIALIZE                  SYS-1032-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1032-HOSPNUM
           MOVE    "1032"          TO  SYS-1032-KANRICD
           MOVE    SYS-STYUKYMD    TO  SYS-1032-STYUKYMD
           MOVE    SYS-EDYUKYMD    TO  SYS-1032-EDYUKYMD
           MOVE    SYS-1032-REC    TO  MCPDATA-REC
           PERFORM 900-SYS-KEY2-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SYSKANRI2   =   1
               MOVE    MCPDATA-REC     TO  SYS-1032-REC
               IF    ( SYS-1032-KBNCD(1:5) =   "00000" )   OR
                     ( SYS-1032-KBNCD(1:5) =   SYS-KBNCD )
                   MOVE    SYS-1032-KBNCD(6:3) TO  IDX-PRT
                   IF      SYS-1032-ORCAORGCHK =   "T"
                       MOVE    SYS-1032-ORCAORGNM
                                               TO  WRK-PRTPRO(IDX-PRT)
                   END-IF
                   IF      SYS-1032-ORCACSTCHK =   "T"
                       MOVE    SYS-1032-ORCACSTNM
                                               TO  WRK-PRTPRO(IDX-PRT)
                   END-IF
               END-IF
               PERFORM 900-SYS-KEY2-DBFETCH-SEC
           END-PERFORM
      *
           PERFORM 900-SYS-KEY2-DBCLOSE-SEC
      *
           PERFORM VARYING IDX-PRT FROM 1 BY 1
                   UNTIL   IDX-PRT >   12
               IF      WRK-PRTPRO(IDX-PRT) NOT =   SPACE
                   EVALUATE    IDX-PRT
                       WHEN    1
                           MOVE   1            TO  IDX-MAX
                       WHEN    2
                           MOVE   2            TO  IDX-MAX
                       WHEN    3
                           MOVE   3            TO  IDX-MAX
                       WHEN    4
                           MOVE   8            TO  IDX-MAX
                       WHEN    5
                           MOVE   10           TO  IDX-MAX
                       WHEN    6
                           MOVE   9            TO  IDX-MAX
                       WHEN   7
                           MOVE   12           TO  IDX-MAX
                       WHEN    8
                           MOVE   14           TO  IDX-MAX
                       WHEN    9
                           MOVE   16           TO  IDX-MAX
                       WHEN    10
                           MOVE   17           TO  IDX-MAX
                       WHEN    11
                           MOVE   18           TO  IDX-MAX
                       WHEN    12
                           MOVE   19           TO  IDX-MAX
                   END-EVALUATE
                   MOVE    SPACE           TO  WRK-MEISAI
                   STRING  TOKEI-NAIYO(IDX-MAX)
                                               DELIMITED BY SPACE
                           ":"                 DELIMITED BY SIZE
                           WRK-PRTPRO(IDX-PRT) DELIMITED BY SPACE
                           INTO                WRK-MEISAI
                   END-STRING
                   MOVE    SPACE           TO  TOKEI-NAIYO(IDX-MAX)
                   MOVE    WRK-MEISAI      TO  TOKEI-NAIYO(IDX-MAX)
               END-IF
           END-PERFORM
      *
           REWRITE TOKEI-REC
           .
       230-PRTPRO-1032-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム選択情報 
      *****************************************************************
       230-PRTPRO-1035-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-PRTPRO-G
      *
           INITIALIZE                  SYS-1035-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1035-HOSPNUM
           MOVE    "1035"          TO  SYS-1035-KANRICD
           MOVE    SYS-STYUKYMD    TO  SYS-1035-STYUKYMD
           MOVE    SYS-EDYUKYMD    TO  SYS-1035-EDYUKYMD
           MOVE    SYS-1035-REC    TO  MCPDATA-REC
           PERFORM 900-SYS-KEY2-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SYSKANRI2   =   1
               MOVE    MCPDATA-REC     TO  SYS-1035-REC
               IF    ( SYS-1035-KBNCD(1:5) =   "00000" )   OR
                     ( SYS-1035-KBNCD(1:5) =   SYS-KBNCD )
                   MOVE    SYS-1035-KBNCD(6:3) TO  IDX-PRT
                   IF      SYS-1035-ORCAORGCHK =   "T"
                       MOVE    SYS-1035-ORCAORGNM
                                               TO  WRK-PRTPRO(IDX-PRT)
                   END-IF
                   IF      SYS-1035-ORCACSTCHK =   "T"
                       MOVE    SYS-1035-ORCACSTNM
                                               TO  WRK-PRTPRO(IDX-PRT)
                   END-IF
               END-IF
               PERFORM 900-SYS-KEY2-DBFETCH-SEC
           END-PERFORM
      *
           PERFORM 900-SYS-KEY2-DBCLOSE-SEC
      *
           PERFORM VARYING IDX-PRT FROM 1 BY 1
                   UNTIL   IDX-PRT >   10
               IF      WRK-PRTPRO(IDX-PRT) NOT =   SPACE
                   EVALUATE    IDX-PRT
                       WHEN    1
                           MOVE   22           TO  IDX-MAX
                       WHEN    2
                           MOVE   23           TO  IDX-MAX
                       WHEN    3
                           MOVE   24           TO  IDX-MAX
                       WHEN    4
                           MOVE   25           TO  IDX-MAX
                       WHEN    5
                           MOVE   27           TO  IDX-MAX
                       WHEN    6
                           MOVE   28           TO  IDX-MAX
                       WHEN   7
                           MOVE   29           TO  IDX-MAX
                       WHEN    8
                           MOVE   30           TO  IDX-MAX
                       WHEN    9
                           MOVE   31           TO  IDX-MAX
                       WHEN    10
                           MOVE   32           TO  IDX-MAX
                   END-EVALUATE
                   MOVE    SPACE           TO  WRK-MEISAI
                   STRING  TOKEI-NAIYO(IDX-MAX)
                                               DELIMITED BY SPACE
                           ":"                 DELIMITED BY SIZE
                           WRK-PRTPRO(IDX-PRT) DELIMITED BY SPACE
                           INTO                WRK-MEISAI
                   END-STRING
                   MOVE    SPACE           TO  TOKEI-NAIYO(IDX-MAX)
                   MOVE    WRK-MEISAI      TO  TOKEI-NAIYO(IDX-MAX)
               END-IF
           END-PERFORM
      *
           REWRITE TOKEI-REC
           .
       230-PRTPRO-1035-EXT.
           EXIT.
      *****************************************************************
      *    点数算定基準チェック
      *****************************************************************
       220-MAIN-1201-SEC        SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
           MOVE     "1202"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           .
       220-MAIN-1201-EXT.
           EXIT.
      *****************************************************************
      *    医療機関編集情報チェック
      *****************************************************************
       220-MAIN-PRTADRS-SEC        SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
           MOVE     "1902"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           MOVE     "1903"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           .
       220-MAIN-PRTADRS-EXT.
           EXIT.
     *****************************************************************
      *    チェック
      *****************************************************************
       220-MAIN-2005-SEC        SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
           MOVE     "2005"         TO  WRK-KANRICD
           MOVE     "01"           TO  WRK-CD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           MOVE     "2005"         TO  WRK-KANRICD
           MOVE     "05"           TO  WRK-CD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           MOVE     "2008"         TO  WRK-KANRICD
           MOVE     "00000001"     TO  WRK-CD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           MOVE     "2009"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           .
       220-MAIN-2005-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報−入院基本チェック
      *****************************************************************
       220-MAIN-NYUIN-SEC        SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
           MOVE     "5020"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           .
       220-MAIN-NYUIN-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報−病棟情報
      *****************************************************************
       220-MAIN-BYOTO-SEC          SECTION.
      *
      *    システム管理定義取得サブチェック
           PERFORM 220-MAIN-SUBCHK-SEC
      *
           MOVE     "5008"         TO  WRK-KANRICD
           PERFORM 220-MAIN-SUBCHK2-SEC
      *
           .
       220-MAIN-BYOTO-EXT.
           EXIT.
           
      *****************************************************************
      *    帳票プログラム選択情報 
      *****************************************************************
       220-MAIN-SUBCHK2-SEC        SECTION.
      *
           INITIALIZE                  SYS2KANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS2-HOSPNUM
           MOVE    WRK-KANRICD     TO  SYS2-KANRICD
      *
      *    2005のみ区分で構造が異なる
           IF    ( WRK-KANRICD     =   "2005" ) OR
                 ( WRK-KANRICD     =   "2008" )
               MOVE    WRK-CD          TO  SYS2-KBNCD
           ELSE
               MOVE    SYS-KBNCD       TO  SYS2-KBNCD
           END-IF
           MOVE    SYS-STYUKYMD    TO  SYS2-STYUKYMD
           MOVE    SYS-EDYUKYMD    TO  SYS2-STYUKYMD
           MOVE    SYS2KANRI-REC   TO  MCPDATA-REC
           PERFORM 900-SYS-KEY10-DBREAD-SEC
      *
           IF      FLG-SYSKANRI2   =   ZERO
               MOVE    MCPDATA-REC     TO  SYS2KANRI-REC
           ELSE
               INITIALIZE                  SYS2KANRI-REC
               GO  TO  220-MAIN-SUBCHK2-EXT
           END-IF
      *
           INITIALIZE                  ORCSSKDEFAREA
      *    2005のみ区分で構造が異なる
           IF    ( WRK-KANRICD     =   "2005" )
               MOVE    WRK-CD      TO  ORCSSKDEF-KBNCD
           END-IF
           MOVE    WRK-KANRICD     TO  ORCSSKDEF-KANRICD
           CALL    "ORCSSKDEF"         USING
                                       ORCSSKDEFAREA
      *
           MOVE    TOKEI-MAX       TO  IDX-PRT
           MOVE    SYS2-KANRITBL   TO  WRK-MEISAI
      *
      *    項目をセットする
           PERFORM VARYING IDX-MAX FROM    1   BY  1
                   UNTIL ( IDX-MAX >   500 )
                   OR    ( IDX-MAX >   ORCSSKDEF-ITEM-MAX )
               PERFORM 230-TOKEI-SET-SEC
           END-PERFORM
      *
           MOVE    IDX-PRT         TO  TOKEI-MAX
      *
           REWRITE TOKEI-REC
           .
       220-MAIN-SUBCHK2-EXT.
           EXIT.
      *****************************************************************
      *    システム管理定義取得サブチェック
      *****************************************************************
       220-MAIN-SUBCHK-SEC         SECTION.
      *
           INITIALIZE                  TOKEI-REC
           MOVE    SYS-KANRICD     TO  TOKEI-KANRICD
           MOVE    SYS-KBNCD       TO  TOKEI-KBNCD
           MOVE    SYS-STYUKYMD    TO  TOKEI-STYUKYMD
           MOVE    SYS-EDYUKYMD    TO  TOKEI-EDYUKYMD
      *
           PERFORM 900-TOKEI-READ-SEC
      *    データがある場合は抜ける
           IF      FLG-TOKEI       =   ZERO
               GO  TO  220-MAIN-SUBCHK-EXT
           END-IF
      *
           IF      SYS-KANRICD     NOT =   WRK-KANRICD
               MOVE    SYS-KANRICD     TO  ORCSSKDEF-KANRICD
               CALL    "ORCSSKDEF"         USING
                                           ORCSSKDEFAREA
      *        設定内容がない場合はベタ打ち
               IF      ORCSSKDEF-ITEM-MAX  =   ZERO
                   MOVE    "（システム定義未提供）"
                                           TO  ORCSSKDEF-NAME
                   MOVE    1               TO  ORCSSKDEF-ITEM-MAX
                   MOVE    "管理テーブル"  TO  ORCSSKDEF-ITEM-JP(1)
                   MOVE    1               TO  ORCSSKDEF-START(1)
                   MOVE    500             TO  ORCSSKDEF-LENGTH(1)
               END-IF
               MOVE    SYS-KANRICD     TO  WRK-KANRICD
           END-IF
      *
           COMPUTE CNT-TOKEI   =   CNT-TOKEI   +   1
      *
           MOVE    ORCSSKDEF-NAME  TO  TOKEI-NAME
           MOVE    ORCSSKDEF-ITEM-MAX
                                   TO  TOKEI-MAX
           MOVE    SYS-KANRITBL    TO  WRK-MEISAI
           MOVE    ZERO            TO  IDX-PRT
      *
      *    項目をセットする
           PERFORM VARYING IDX-MAX FROM    1   BY  1
                   UNTIL ( IDX-MAX >   500 )
                   OR    ( IDX-MAX >   ORCSSKDEF-ITEM-MAX )
               PERFORM 230-TOKEI-SET-SEC
           END-PERFORM
      *
           WRITE   TOKEI-REC
           .
       220-MAIN-SUBCHK-EXT.
           EXIT.
      *****************************************************************
      *    システム管理定義統計セット
      *****************************************************************
       230-TOKEI-SET-SEC           SECTION.
      *
           COMPUTE IDX-PRT =   IDX-PRT +   1
      *
           MOVE    ORCSSKDEF-START(IDX-MAX)
                                   TO  IDX-STR
           MOVE    ORCSSKDEF-LENGTH(IDX-MAX)
                                   TO  IDX-KET
           MOVE    ORCSSKDEF-ITEM-JP(IDX-MAX)
                                   TO  TOKEI-ITEM(IDX-PRT)
      *    小数点の場合
           IF    ( ORCSSKDEF-TYPE(IDX-MAX) =   "9" )   AND
                 ( ORCSSKDEF-PRECISION(IDX-MAX)    >   0 )
               MOVE    SPACE           TO  WRK-SUUJI-G
               MOVE    WRK-MEISAI(IDX-STR:IDX-KET)
                                       TO  WRK-SUUJI
               MOVE    ORCSSKDEF-PRECISION(IDX-MAX)
                                       TO  IDX-SYO
               COMPUTE IDX-STR =   IDX-KET -   IDX-SYO
               MOVE    WRK-SUUJI(1:IDX-STR)
                                       TO  WRK-SEISU
               COMPUTE IDX-STR =   IDX-STR +   1
               MOVE    WRK-SUUJI(IDX-STR:IDX-SYO)
                                       TO  WRK-SYOSU
               STRING  WRK-SEISU       DELIMITED BY SPACE
                       "."             DELIMITED BY SIZE
                       WRK-SYOSU       DELIMITED BY SPACE
                       INTO            WRK-HENSUUJI
               END-STRING
               MOVE    WRK-HENSUUJI    TO  TOKEI-NAIYO(IDX-PRT)
           ELSE
               MOVE    WRK-MEISAI(IDX-STR:IDX-KET)
                                       TO  TOKEI-NAIYO(IDX-PRT)
           END-IF
           .
       230-TOKEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       300-TOKEI-OUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *    ＣＳＶタイトル見出し
           PERFORM 410-CSV-TITLE-SEC
      *
           PERFORM UNTIL   FLG-TOKEI   =   1
      *        ＣＳＶ管理コード見出し
               PERFORM 410-CSV-HEAD-SEC
      *        管理コード見出し
               PERFORM 310-PRT-HEAD-SEC
               PERFORM UNTIL ( FLG-TOKEI   =   1  )
                       OR    ( TOKEI-KANRICD   NOT =   WRK-KANRICD )
      *            明細
                   PERFORM 320-PRT-MEISAI-SEC
      *            統計ファイル読み込み
                   PERFORM 900-TOKEI-NEXT-SEC
               END-PERFORM
      *        ページ印刷
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
           .
       300-TOKEI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    管理コード見出し
      *****************************************************************
       310-PRT-HEAD-SEC            SECTION.
      *
           MOVE    SPACE           TO  HCMSL80
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE        TO  WRK-Z03
           MOVE    WRK-Z03         TO  WRK-PRT-PAGE
      *
           MOVE    TOKEI-KANRICD   TO  WRK-PRT-KANRICD
           MOVE    TOKEI-NAME      TO  WRK-PRT-NAME
      *
           MOVE    WRK-PRT-LINE1   TO  HCMSL80-MOJI(1)
           MOVE    WRK-PRT-LINE2   TO  HCMSL80-MOJI(3)
           MOVE    WRK-PRT-SEN     TO  HCMSL80-MOJI(4)
      *
           MOVE    4               TO  CNT-LINE
      *
           IF       TOKEI-KANRICD  =   "1006"
      *        ＣＳＶ区分コード見出し
               PERFORM 410-CSV-HEAD2-SEC
      *        区分コード見出し
               PERFORM 310-PRT-HEAD2-SEC
           END-IF
           .
       310-PRT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    区分コード見出し
      *****************************************************************
       310-PRT-HEAD2-SEC           SECTION.
      *
      *    改ページチェック
           PERFORM 380-KAIPAGE-CHK-SEC
      *
           IF      CNT-LINE    >   5
               MOVE    WRK-PRT-SEN     TO  HCMSL80-MOJI(CNT-LINE)
      *        改ページチェック
               PERFORM 380-KAIPAGE-CHK-SEC
           END-IF
      *
           IF       TOKEI-KANRICD  =   "1006"
               MOVE    "*"             TO  WRK-PRT-KBNCD
           ELSE
               MOVE    TOKEI-KBNCD     TO  WRK-PRT-KBNCD
           END-IF
           MOVE    TOKEI-STYUKYMD  TO  WRK-PRT-STYUKYMD
           MOVE    TOKEI-EDYUKYMD  TO  WRK-PRT-EDYUKYMD
      *
           MOVE    WRK-PRT-LINE3   TO  HCMSL80-MOJI(CNT-LINE)
      *
      *    改ページチェック
           PERFORM 380-KAIPAGE-CHK-SEC
           .
       310-PRT-HEAD2-EXT.
           EXIT.
      *****************************************************************
      *    明細
      *****************************************************************
       320-PRT-MEISAI-SEC          SECTION.
      *
           IF       TOKEI-KANRICD  NOT =   "1006"
      *        ＣＳＶ区分コード見出し
               PERFORM 410-CSV-HEAD2-SEC
      *        区分コード見出し
               PERFORM 310-PRT-HEAD2-SEC
           END-IF
      *
      *    項目をセットする
           PERFORM VARYING IDX-MAX     FROM    1   BY  1
                   UNTIL ( IDX-MAX     >   500 )
                   OR    ( IDX-MAX     >   TOKEI-MAX )
               PERFORM 330-MEISAI-HENSYU-SEC
           END-PERFORM
           .
       320-PRT-MEISAI-EXT.
           EXIT.
      *****************************************************************
      *    明細
      *****************************************************************
       330-MEISAI-HENSYU-SEC       SECTION.
      *
           IF     ( TOKEI-KANRICD  =   "1010" )    AND
                  ( TOKEI-ITEM(IDX-MAX)    =   "業務処理権限−未使用" )
               GO  TO  330-MEISAI-HENSYU-EXT
           END-IF
           IF    ( TOKEI-ITEM(IDX-MAX)    =   "未設定" )   OR
                 ( TOKEI-ITEM(IDX-MAX)    =   "予備" )     OR
                 ( TOKEI-NAIYO(IDX-MAX)   =   SPACE )
               GO  TO  330-MEISAI-HENSYU-EXT
           END-IF
      *
           IF      TOKEI-ITEM(IDX-MAX)(17:)
                                   NOT =   SPACE
               MOVE    SPACE           TO  WRK-MEISAI
               STRING  TOKEI-ITEM(IDX-MAX) DELIMITED   BY  SPACE
                       "："                DELIMITED   BY  SIZE
                       TOKEI-NAIYO(IDX-MAX)
                                           DELIMITED   BY  SPACE
                       INTO                WRK-MEISAI
               END-STRING
           ELSE
               MOVE    SPACE           TO  WRK-MEISAI
               MOVE    TOKEI-ITEM(IDX-MAX)
                                       TO  WRK-MEISAI(1:16)
               MOVE    "："            TO  WRK-MEISAI(17:2)
               MOVE    TOKEI-NAIYO(IDX-MAX)
                                       TO  WRK-MEISAI(19:)
           END-IF
      *
      *    ＣＳＶ出力
           PERFORM 420-CSV-MEISAI-SEC
      *    帳票文字編集処理
           PERFORM 340-HENSYU-SYORI-SEC
      *
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL   IDX-STR     >   IDX-GYO
      *        改ページチェック
               PERFORM 380-KAIPAGE-CHK-SEC
               MOVE    WRK-HENMEISAI(IDX-STR)
                                   TO  HCMSL80-MOJI(CNT-LINE)
           END-PERFORM
           .
       330-MEISAI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       340-HENSYU-SYORI-SEC        SECTION.
      *
      *    初期値セット
           MOVE    1               TO  IDX-STR
           MOVE    1               TO  IDX-KET
           MOVE    1               TO  IDX-GYO
           MOVE    SPACE           TO  WRK-HENMEISAI-G
      *
      *    最大行を超えるか編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   600 )
                   OR     (IDX-GYO     >   7 )
                   OR     (WRK-MEISAI(IDX-STR:)    =   SPACE)
      *        全角セット
               IF      WRK-MEISAI(IDX-STR:1)   >=  X"A1"
                   IF      IDX-KET         <=  108
                       MOVE    WRK-MEISAI(IDX-STR:2)
                               TO  WRK-HENMEISAI(IDX-GYO)(IDX-KET:2)
                       ADD     2               TO  IDX-STR
                       ADD     2               TO  IDX-KET
                   ELSE
                       ADD     1               TO  IDX-KET
                   END-IF
      *        半角セット
               ELSE
                   MOVE    WRK-MEISAI(IDX-STR:1)
                               TO  WRK-HENMEISAI(IDX-GYO)(IDX-KET:1)
                   ADD     1               TO  IDX-STR
                   ADD     1               TO  IDX-KET
               END-IF
      *        最大字で改行
               IF      IDX-KET         >=  110
                   ADD     1               TO  IDX-GYO
                   MOVE    19              TO  IDX-KET
               END-IF
           END-PERFORM
           .
       340-HENSYU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    改ページチェック
      *****************************************************************
       380-KAIPAGE-CHK-SEC         SECTION.
      *
           IF      CNT-LINE        >=  80
      *        帳票編集処理
               PERFORM 390-PRINT-OUT-SEC
      *        見出し
               PERFORM 310-PRT-HEAD-SEC
           END-IF
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           .
       380-KAIPAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
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
           MOVE    "システム管理設定内容リスト"
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
       390-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶタイトル見出し編集
      *****************************************************************
       410-CSV-TITLE-SEC           SECTION.
      *
           INITIALIZE                  WRK-CSVREC
           STRING  "システム管理設定内容リスト"
                                       DELIMITED  BY  SIZE
                   "        作成日："  DELIMITED  BY  SIZE
                   WRK-PRT-PRTYMD      DELIMITED  BY  SIZE
                   INTO                WRK-CSVREC
           END-STRING
           PERFORM 490-CSV-OUT-SEC
           .
       410-CSV-TITLE-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ管理コード見出し編集
      *****************************************************************
       410-CSV-HEAD-SEC          SECTION.
      *
           INITIALIZE                  WRK-CSVREC
           PERFORM 490-CSV-OUT-SEC
      *
           INITIALIZE                  WRK-CSVREC
           STRING  "管理コード      "  DELIMITED  BY  SIZE
                   "："                DELIMITED  BY  SIZE
                   TOKEI-KANRICD       DELIMITED  BY  SIZE
                   INTO                WRK-CSVREC
           END-STRING
           PERFORM 490-CSV-OUT-SEC
      *
           INITIALIZE                  WRK-CSVREC
           STRING  "名称            "  DELIMITED  BY  SIZE
                   "："                DELIMITED  BY  SIZE
                   TOKEI-NAME          DELIMITED  BY  SIZE
                   INTO                WRK-CSVREC
           END-STRING
           PERFORM 490-CSV-OUT-SEC
      *
           MOVE    TOKEI-KANRICD   TO  WRK-KANRICD
           .
       410-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ区分コード見出し編集
      *****************************************************************
       410-CSV-HEAD2-SEC           SECTION.
      *
      *    施設基準ならば
           IF      TOKEI-KANRICD   =   "1006"
               INITIALIZE                  WRK-CSVREC
               STRING  "区分コード      "  DELIMITED  BY  SIZE
                       "："                DELIMITED  BY  SIZE
                       "*       "          DELIMITED  BY  SIZE
                       INTO                WRK-CSVREC
               END-STRING
               PERFORM 490-CSV-OUT-SEC
           ELSE
               INITIALIZE                  WRK-CSVREC
               STRING  "区分コード      "  DELIMITED  BY  SIZE
                       "："                DELIMITED  BY  SIZE
                       TOKEI-KBNCD         DELIMITED  BY  SIZE
                       INTO                WRK-CSVREC
               END-STRING
               PERFORM 490-CSV-OUT-SEC
           END-IF
      *
           INITIALIZE                  WRK-CSVREC
           STRING  "開始有効年月日  "  DELIMITED  BY  SIZE
                   "："                DELIMITED  BY  SIZE
                   TOKEI-STYUKYMD      DELIMITED  BY  SIZE
                   INTO                WRK-CSVREC
           END-STRING
           PERFORM 490-CSV-OUT-SEC
      *
           INITIALIZE                  WRK-CSVREC
           STRING  "終了有効年月日  "  DELIMITED  BY  SIZE
                   "："                DELIMITED  BY  SIZE
                   TOKEI-EDYUKYMD      DELIMITED  BY  SIZE
                   INTO                WRK-CSVREC
           END-STRING
           PERFORM 490-CSV-OUT-SEC
           .
       410-CSV-HEAD2-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ明細編集
      *****************************************************************
       420-CSV-MEISAI-SEC          SECTION.
      *
      *    ＣＳＶ出力
           INITIALIZE                  WRK-CSVREC
           MOVE    WRK-MEISAI      TO  WRK-CSVREC
           PERFORM 490-CSV-OUT-SEC
           .
       420-CSV-MEISAI-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       490-CSV-OUT-SEC             SECTION.
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
           MOVE    SPACE               TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVREC          TO  STOUKEICSV-CSVDATA
           MOVE    "システム管理設定内容リストＣＳＶ作成"
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
       490-CSV-OUT-EXT.
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
           DISPLAY "*** ORCBGSKLST IN "      CNT-TOKEI
           DISPLAY "*** ORCBGSKLST END ***"
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
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBGSKLST"        DELIMITED  BY  SIZE
                   WRK-TOUKEIERR       DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                   INTO                WRK-RECEERR
           END-STRING
      *
           CALL    "cobabort"          USING
                                       WRK-RECEERR
           .
       500-COBABORT-EXT.
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
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
      *
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
           .
       800-SEIWA-HEN-EXT.
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
      *    システム管理マスタ検索
      *****************************************************************
       900-SYS-KEY10-DBREAD-SEC    SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI2
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-SYS-KEY10-DBREAD-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索
      *****************************************************************
       900-SYS-KEY2-DBSELECT-SEC   SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI2
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
           .
       900-SYS-KEY2-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索
      *****************************************************************
       900-SYS-KEY2-DBFETCH-SEC    SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI2
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
           .
       900-SYS-KEY2-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索
      *****************************************************************
       900-SYS-KEY2-DBCLOSE-SEC    SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-SYS-KEY2-DBCLOSE-EXT.
      *****************************************************************
      *    システム管理マスタ検索
      *****************************************************************
       900-SYS-KEY29-DBSELECT-SEC  SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    WRK-PARA-KANRICD(IDX-PARA)
                                   TO  SYS-KANRICD
           MOVE    WRK-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key29"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
           .
       900-SYS-KEY29-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索
      *****************************************************************
       900-SYS-KEY29-DBFETCH-SEC   SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key29"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
           .
       900-SYS-KEY29-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索
      *****************************************************************
       900-SYS-KEY29-DBCLOSE-SEC   SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key29"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-SYS-KEY29-DBCLOSE-EXT.
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
