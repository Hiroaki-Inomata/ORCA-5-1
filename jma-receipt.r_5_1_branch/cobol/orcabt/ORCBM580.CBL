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
       PROGRAM-ID.             ORCBM580.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 連携用データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/12/09    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.00    NACL-伊藤    13.12.16  R1レコード編集
      *                                     地域連携IDなし・同意なしは
      *                                     出力しない
      *  00.00.00    NACL-伊藤    14. 6.11  R2,R3レコード編集
      *                                     HO,KOレコード編集
      *  00.00.00    NACL-伊藤    14. 7. 7  C1レコード編集
      *  00.00.00    NACL-伊藤    14. 7. 8  SYレコード編集
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *
      *  00.00.00    NACL-伊藤    15. 2.18  自費保険分を作成
      *                                     地域連携IDが空白の場合は
      *                                     患者番号を記録
      *  04.08.06    NACL-藤原    15/08/07  医療情報連携基盤のクライアン
      *                                     ト保存対応
      *  04.08.07    NACL-伊藤    15.10.01  R1レコード編集
      *                                     地域連携IDなしは対象とし、
      *                                     同意なしは出力しない
      *  05.00.00    NACL-森脇    17.02.09  傷病名称全記録
      *  04.08.08    NACL-藤原    17/03/24  入院のファイル名修正
      *  05.00.01    NACL-森脇    17/06/01  診療行為、医薬品レコード名称登録
      *  05.00.02    NACL-森脇    17/11/09  当日受診のみ出力対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *    レセプト電算ファイル
           SELECT  RECE30-FILE     ASSIGN  RECE30PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-RECE30.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト電算ファイル
       FD  RECE30-FILE.
       01  RECE30-R                PIC X(2500).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
       COPY    "COMMON-SPA".
      *    レセプト電算ファイル 名称領域
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE30PARA//.
      *01  RECE30PARA.
      *****03  FILLER              PIC X(05)   VALUE 
      *****                                    "/tmp/".
       01  RECE30PARA-BASENAME.
           03  RECE30PARA-HOSPNUM  PIC 9(02).
           03  RECE30PARA-FILENM   PIC X(07)   VALUE
                                               "RECEDEN".
           03  RECE30PARA-TEISYUTUSAKI
                                   PIC X(01).
           03  RECE30PARA-NYUGAIKBN
                                   PIC X(01).
           03  RECE30PARA-CREYMD   PIC X(08).
           03  RECE30PARA-CREHMS   PIC X(06).
           03  FILLER              PIC X(04)   VALUE   ".txt".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE30              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTPUBLIC            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-PTCOM               PIC 9(01).
           03  FLG-SRYKBN              PIC 9(01).
      *----(05.00.01)--ADD-START---
           03  FLG-TENSU               PIC 9(01).
      *----(05.00.01)--ADD-END-----
      *----(05.00.02)--ADD-START---
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *----(05.00.02)--ADD-END-----
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(05).
           03  IDX                     PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDY                     PIC 9(05).
           03  IDZ                     PIC 9(05).
           03  IDY1                    PIC 9(05).
           03  IDY2                    PIC 9(05).
           03  IDY3                    PIC 9(05).
           03  IDWW                    PIC 9(05).
           03  IDXX                    PIC 9(05).
           03  IDYY                    PIC 9(05).
           03  IDZZ                    PIC 9(05).
           03  IDF                     PIC 9(05).
           03  IDF1                    PIC 9(05).
      *----(05.00.00)--ADD-START---
           03  IDX-B1                  PIC 9(05).
           03  IDX-B2                  PIC 9(05).
      *----(05.00.00)--ADD-END-----
      *
      *    一時領域
      *    パラメタエリア
       01  WRK-AREA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-TEISYUTUSAKI   PIC X(01).
           05  WRK-PARA-FOLDER         PIC X(150).
           05  WRK-PARA-NYUGAIKBN      PIC X(01).
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
           05  WRK-PARA-DATAKBN        PIC X(01).
           05  WRK-PARA-FIXEDSIZE      PIC 9(07).
           05  WRK-PARA-HOSPNUM        PIC 9(02).
           05  WRK-PARA-PRTKBN         PIC X(01).
           05  WRK-PARA-HKNJANUM       PIC X(08).
      *
      *    マルチボリューム識別情報
           03  WRK-VOLNUM              PIC 9(02).
      *    レセプト番号
           03  WRK-RECENUM             PIC 9(06).
      *    請求年月（和暦）
           03  WRK-SEIYM               PIC 9(05).
      *    診療年月（和暦）
           03  WRK-SRYYM               PIC 9(05).
           03  WRK-TAIYMD              PIC 9(07).
      *    データ長
           03  WRK-DATA-LENGTH         PIC 9(07).
      *
      *     右づめ編集用
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-REC                 PIC X(2500).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-FILE-RENNUM         PIC 9(02).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NUM             PIC ZZZZZZZZZ9.999.
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02).
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
           03  WRK-FTNKBN              PIC X(01).
           03  WRK-FTNKBN-TBL.
               05  WRK-FTNKBN-TBL-OCC  OCCURS  5.
                   07  WRK-FTNKBN-VAL  PIC X(01).
               05  WRK-FTNKBN-TBL-MAX  PIC 9(01).
           03  WRK-SV-PTID             PIC 9(10).
           03  WRK-SV-HKNCOMBI         PIC 9(04).
           03  WRK-HK-TBL.
               05  WRK-HKNJANUM        PIC X(08).
               05  WRK-FTNJANUM-G.
                   07  WRK-FTNJANUM    PIC X(08)
                                       OCCURS  4.
               05  WRK-FTNJANUM-MAX    PIC 9(01).
      *    日付変換用
           03  WRK-SYMD                PIC X(08).
           03  WRK-NYUTAIYMD.
               05  WRK-NYUINYMD        PIC X(07).
               05  WRK-TAIINYMD        PIC X(07).
           03  WRK-RECESYUBETU         PIC X(04).
           03  WRK-PTNUM               PIC X(20).
      *----(05.00.00)--ADD-START---
           03  WRK-BYOMEICD-G.
               05  WRK-BYOMEICD        PIC X(04)   OCCURS  20.
      *----(05.00.00)--ADD-END-----
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    エラーファイル名称領域
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
       01  WRK-SGETTEMP-TEMPDIR        PIC X(1024).
      *
      *    医療機関情報ワーク
           COPY    "CPSK1001.INC"      REPLACING  //SYS-1001//
                                       BY         //WRK-SYS-1001//.
           COPY    "CPSK1002.INC"      REPLACING  //SYS-1002//
                                       BY         //WRK-SYS-1002//.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG             PIC 9(08).
           03  WRK-HENYMDGR            REDEFINES   WRK-HENYMDG.
               05  WRK-HENYMD-FIL      PIC 9(01).
               05  WRK-HENYMD          PIC 9(07).
      *
       01  TABLE-AREA.
           03  TABLE-OCC               OCCURS  10000.
               05  TBL-RECEDATA        PIC X(2500).
               05  TBL-LENGTH          PIC 9(04).
           03  TABLE-MAX               PIC 9(05).
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC".
           COPY    "CPSK9101.INC".
      *
      *    医療機関情報（基本）
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報（住所）
           COPY    "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者地域連携
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *----(05.00.01)--ADD-START---
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *----(05.00.01)--ADD-END-----
      *----(05.00.02)--ADD-START---
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *----(05.00.02)--ADD-END-----
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    CSV分割サブ
           COPY    "CPORCSDIVCSV.INC".
      *
      *    負担区分コード取得サブ
           COPY    "CPORCSGETFTNKBN.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *----(05.00.00)--ADD-START---
      *    病名検索
           COPY    "CPORCSBYOMEI.INC".
      *----(05.00.00)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
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
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              CNT-AREA
                                   WRK-AREA
                                   FLG-AREA
                                   SPA-AREA
      *
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
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-FOLDER
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-FIXEDSIZE
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  RECEERR
           MOVE    SGETTEMP-FULLNAME
                                   TO  RECEERR
      *
           MOVE   SGETTEMP-TEMPDIR TO  WRK-SGETTEMP-TEMPDIR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM580"      TO  JOB-PGID
           EVALUATE    WRK-PARA-TEISYUTUSAKI
                           ALSO    WRK-PARA-NYUGAIKBN
               WHEN    "1" ALSO    "1"
                   MOVE    "連携ファイル作成（社保・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "1" ALSO    "2"
                   MOVE    "連携ファイル作成（社保・入院外）"
                                           TO  JOB-SHELLMSG
               WHEN    "2" ALSO    "1"
                   MOVE    "連携ファイル作成（国保・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "2" ALSO    "2"
                   MOVE    "連携ファイル作成（国保・入院外）"
                                           TO  JOB-SHELLMSG
               WHEN    "3" ALSO    "1"
                   MOVE    "連携ファイル作成（広域・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "3" ALSO    "2"
                   MOVE    "連携ファイル作成（広域・入院外）"
                                           TO  JOB-SHELLMSG
               WHEN    "4" ALSO    "1"
                   MOVE    "連携ファイル作成（国保、広域・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "4" ALSO    "2"
                   MOVE    "連携ファイル作成（国保、広域・入院外）"
                                           TO  JOB-SHELLMSG
           END-EVALUATE
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *
           PERFORM 120-SKYYM-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ取得
      *****************************************************************
       110-SYSKANRI-HENSYU-SEC      SECTION.
      *
      *    医療機関情報編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報（基本）が取得できません"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD   TO  SYS-1002-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               MOVE    "医療機関情報（住所）が取得できません"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SYS-1001-HOSPNAME   TO  WRK-SYS-1001-HOSPNAME
               MOVE    SYS-1002-TEL        TO  WRK-SYS-1002-TEL
      *        医療機関編集情報
               PERFORM 900-1900-READ-SEC
           END-IF
      *
      *    レセプト印刷情報
           MOVE    SPACE               TO  SYS-200501-REC
           INITIALIZE                      SYS-200501-REC
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-200501-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-200501-STYUKYMD (7:2)
           MOVE    SYS-200501-STYUKYMD TO  SYS-200501-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-200501-REC
           ELSE
               INITIALIZE                      SYS-200501-REC
           END-IF
           IF     SYS-200501-RECEDEN-DISASTER
                                       =   SPACE
               MOVE    "1"             TO  SYS-200501-RECEDEN-DISASTER
           END-IF
      *
      *    DISPLAY "RECEDEN-DISASTER=" SYS-200501-RECEDEN-DISASTER
      *
      *    医療情報連携基盤情報
           MOVE    SPACE               TO  SYS-9101-REC
           INITIALIZE                      SYS-9101-REC
           MOVE    "9101"              TO  SYS-9101-KANRICD
           MOVE    "*"                 TO  SYS-9101-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-9101-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-9101-STYUKYMD (7:2)
           MOVE    SYS-9101-STYUKYMD   TO  SYS-9101-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-9101-HOSPNUM
           MOVE    SYS-9101-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9101-REC
           ELSE
               INITIALIZE                      SYS-9101-REC
           END-IF
           .
       110-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求年月編集処理
      *****************************************************************
       120-SKYYM-HENSYU-SEC      SECTION.
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD (1:5)    TO  WRK-SEIYM
           MOVE    WRK-HENYMD          TO  WRK-TAIYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD (1:5)    TO  WRK-SRYYM
           .
       120-SKYYM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-RENKEI-TMP-SELECT-SEC
      *
      *----(05.00.02)--ADD-START---
           MOVE    ZERO            TO  FLG-OK
      *----(05.00.02)--ADD-END-----
      *
           PERFORM         UNTIL   FLG-END     =   1
               PERFORM 2001-HENSYU-SEC
           END-PERFORM
      *
           PERFORM 2002-FILE-WRITE-SEC
      *
           MOVE    "tbl_renkei_tmp"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    国保と広域を一緒に提出する場合の広域分の処理
           IF      WRK-PARA-TEISYUTUSAKI
                                   =   "4"
               MOVE    "3"             TO  WRK-PARA-TEISYUTUSAKI
               PERFORM 900-RENKEI-TMP-SELECT-SEC
      *
      *----(05.00.02)--ADD-START---
               MOVE    ZERO            TO  FLG-OK
      *----(05.00.02)--ADD-END-----
      *
               PERFORM         UNTIL   FLG-END =   1
                   PERFORM 2001-HENSYU-SEC
               END-PERFORM
      *
      *        データ出力処理
               PERFORM 2002-FILE-WRITE-SEC
      *
               MOVE    "tbl_renkei_tmp"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           IF      CNT-OUT             >   ZERO
               CLOSE   RECE30-FILE
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ編集処理
      *****************************************************************
       2001-HENSYU-SEC              SECTION.
      *
      *----(05.00.02)--ADD-START---
           IF      RECEDEN-RECEDATA (1:2)  =   "RE"
      *        当日外来受診分がない場合はスキップ
               IF      WRK-PARA-PRTKBN =   "A"
                   IF      WRK-PARA-NYUGAIKBN  =   "1"
                       MOVE    1               TO  FLG-OK
                   ELSE
                       PERFORM 900-JYURRK-READ-SEC
                       IF    FLG-JYURRK    =   1
                           MOVE    1               TO  FLG-OK
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-OK          =   ZERO
      *----(05.00.02)--ADD-END-----
      *
      *    レセプト共通レコード編集処理
           IF      RECEDEN-RECEDATA (1:2)  =   "RE"
      *        データ出力処理
               PERFORM 2002-FILE-WRITE-SEC
      *        患者IDを退避
               MOVE    RECEDEN-PTID        TO  SPA-PTID
      *        入退院日をレセプト共通レコードから取得
               MOVE    SPACE               TO  WRK-NYUTAIYMD
               MOVE    SPACE               TO  WRK-RECESYUBETU
               MOVE    SPACE               TO  WRK-PTNUM
               INITIALIZE  ORCSDIVCSV-AREA
               MOVE    RECEDEN-RECEDATA    TO  ORCSDIVCSV-REC
      *        21請求情報に退院日がセットされているため
               MOVE    21                  TO  ORCSDIVCSV-ELE-C
               MOVE    ","                 TO  ORCSDIVCSV-DELI
               MOVE    ZERO                TO  ORCSDIVCSV-DQ
               CALL    "ORCSDIVCSV"     USING  ORCSDIVCSV-AREA
               IF      ORCSDIVCSV-RC       =   ZERO
                   IF      ORCSDIVCSV-D (3)
                                           NOT =   SPACE
                       MOVE    ORCSDIVCSV-D (3)
                                               TO  WRK-RECESYUBETU
                   END-IF
                   IF      ORCSDIVCSV-D (14)
                                           NOT =   SPACE
                       MOVE    ORCSDIVCSV-D (14)
                                               TO  WRK-PTNUM
                   END-IF
                   IF      WRK-PARA-NYUGAIKBN  =   "1"
                       IF      ORCSDIVCSV-D (9)
                                               NOT =   SPACE
                           MOVE    ORCSDIVCSV-D (9)    TO  WRK-NYUINYMD
                       END-IF
                       IF     (ORCSDIVCSV-D (21) (1:7)
                                               NOT =   "9999999")
                         AND  (ORCSDIVCSV-D (21)
                                               NOT =   SPACE)
                           MOVE    ORCSDIVCSV-D (21)   TO  WRK-TAIINYMD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           ADD     1                   TO  TABLE-MAX
           MOVE    RECEDEN-RECEDATA    TO  TBL-RECEDATA (TABLE-MAX)
      *
      *----(05.00.02)--ADD-START---
           END-IF
      *----(05.00.02)--ADD-END-----
      *
           PERFORM 900-RENKEI-TMP-READ-SEC
           .
       2001-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       2002-FILE-WRITE-SEC         SECTION.
      *
           IF      TABLE-MAX        >   1
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       TABLE-MAX
                   IF      TBL-RECEDATA (IDX) (1:2)    =   "RE"
                       IF     (SYS-9101-JIHI           NOT =   "T")
                         AND  (WRK-RECESYUBETU             =   "8888")
      *                    自費保険分を作成しない場合はスキップ
                           EXIT  PERFORM
                       END-IF
      *                負担区分クリア
                       MOVE    SPACE               TO  WRK-FTNKBN
                       INITIALIZE  WRK-FTNKBN-TBL
                       INITIALIZE  WRK-HK-TBL
      *                データ提出同意か確認
                       PERFORM 900-PTPUBLIC-READ-SEC
                       EVALUATE    SYS-9101-COOPBASE
                           WHEN    "1"
      *                地域連携IDなし又は同意しない場合はスキップする
      *                入退院データ（処理区分7）の場合、入院中と
      *                指定日より後の退院日はスキップする
                               IF     (PTPUBLIC-AGREEMENT
                                               NOT =   "1")  OR
      *H27.10 地域連携IDは未設定でも対象とする
      **                              (PTPUBLIC-PATIENT-ID1
      **                                           =   SPACE)  OR
                                     ((WRK-PARA-NYUGAIKBN
                                                   =   "1")  AND
                                      (WRK-PARA-PRTKBN
                                                   =   "7")  AND
                                     ((WRK-TAIINYMD
                                                   =   SPACE)  OR
                                      (WRK-TAIINYMD
                                                   >   WRK-TAIYMD)))
                                   EXIT  PERFORM
                               END-IF
                           WHEN    "2"
      *                地域連携IDなし又は同意しない場合でも出力する
      *                入退院データ（処理区分7）の場合、入院中と
      *                指定日より後の退院日はスキップする
                               IF     (WRK-PARA-NYUGAIKBN
                                                   =   "1")  AND
                                      (WRK-PARA-PRTKBN
                                                   =   "7")  AND
                                     ((WRK-TAIINYMD
                                                   =   SPACE)  OR
                                      (WRK-TAIINYMD
                                                   >   WRK-TAIYMD))
                                   EXIT  PERFORM
                               END-IF
                       END-EVALUATE
      *                レセプト番号の編集
                       MOVE    SPACE               TO  WRK-REC
                       MOVE    "RE,"               TO  WRK-REC
                       MOVE    3                   TO  WRK-REC-LENGTH
                       ADD     1                   TO  WRK-RECENUM
                       MOVE    WRK-RECENUM         TO  WRK-NUM
                       MOVE    6                   TO  WRK-NAGASA
                       PERFORM 5005-NUM-HENSYU-SEC
      *
                       STRING  WRK-REC (1:WRK-REC-LENGTH)
                                                   DELIMITED   BY  SIZE
                               TBL-RECEDATA (IDX)  (5:)
                                                   DELIMITED   BY  SIZE
                                                   INTO        WRK-REC
                       END-STRING
                       MOVE    WRK-REC         TO  TBL-RECEDATA (IDX)
                   END-IF
      *
                   IF      CNT-OUT         =   ZERO
      *                医療機関情報レコード編集処理
                       PERFORM     20021-HENSYU-SEC
                   END-IF
      *
      *            レセ電データから保険者・負担者番号を退避
                   PERFORM 20021-HK-SAVE-SEC
      *            レセ電データから負担区分を退避
                   PERFORM 20021-FTNKBN-SAVE-SEC
      *
                   MOVE    TBL-RECEDATA (IDX)  TO  RECE30-R
                   WRITE   RECE30-R
      *
                   IF      STS-RECE30          =   "00"
                       CONTINUE
                   ELSE
                       CALL   "coblog"
                                   USING   "file write err " RECE30PARA
                       MOVE    SPACE               TO  WRK-RECEERR
                       STRING "ファイル 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                               STS-RECE30          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   ADD     1                   TO  CNT-OUT
      *
      *            R1レコードを編集して出力
                   IF      TBL-RECEDATA (IDX) (1:2)    =   "RE"
                       PERFORM 20022-R1-HENSYU-SEC
                       PERFORM 20023-R2-HENSYU-SEC
                       PERFORM 20024-R3-HENSYU-SEC
                   END-IF
      *
      *            C1レコードを編集して出力
      *            レセプト電算データがなくなったら診療データから編集
                   IF      IDX                 =   TABLE-MAX
                       PERFORM 20025-C1-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           INITIALIZE              TABLE-AREA
      *
           .
       2002-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データから保険者・負担者番号を退避処理
      *****************************************************************
       20021-HK-SAVE-SEC           SECTION.
      *
           IF      TBL-RECEDATA (IDX) (1:2)    =   "HO"    OR
                                                   "KO"
               INITIALIZE  ORCSDIVCSV-AREA
               MOVE    TBL-RECEDATA (IDX)  TO  ORCSDIVCSV-REC
               MOVE    3                   TO  ORCSDIVCSV-ELE-C
               MOVE    ","                 TO  ORCSDIVCSV-DELI
               MOVE    ZERO                TO  ORCSDIVCSV-DQ
               CALL    "ORCSDIVCSV"     USING  ORCSDIVCSV-AREA
               IF      ORCSDIVCSV-RC       =   ZERO
                   IF      ORCSDIVCSV-D (2)
                                           NOT =   SPACE
                       IF      ORCSDIVCSV-D (1)    =   "HO"
                           IF      ORCSDIVCSV-D (2)(1:1)
                                                   NOT = SPACE
                               STRING  ORCSDIVCSV-D (2)
                                   DELIMITED   BY  SPACE
                                   INTO    WRK-HKNJANUM
                               END-STRING
                           END-IF
                           IF      ORCSDIVCSV-D (2)(1:2)   =   SPACE
                               STRING  ORCSDIVCSV-D (2)(3:)
                                   DELIMITED   BY  SPACE
                                   INTO    WRK-HKNJANUM
                               END-STRING
                           END-IF
                           IF      ORCSDIVCSV-D (2)(1:4)   =   SPACE
                               STRING  ORCSDIVCSV-D (2)(5:)
                                   DELIMITED   BY  SPACE
                                   INTO    WRK-HKNJANUM
                               END-STRING
                           END-IF
                       ELSE
                           COMPUTE WRK-FTNJANUM-MAX
                                               =   WRK-FTNJANUM-MAX
                                               +   1
                           STRING  ORCSDIVCSV-D (2)
                               DELIMITED   BY  SPACE
                               INTO    WRK-FTNJANUM (WRK-FTNJANUM-MAX)
                           END-STRING
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       20021-HK-SAVE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データから負担区分を退避処理
      *****************************************************************
       20021-FTNKBN-SAVE-SEC       SECTION.
      *
           IF      TBL-RECEDATA (IDX) (1:2)    =   "SI"    OR
                                                   "IY"    OR
                                                   "TO"
               INITIALIZE  ORCSDIVCSV-AREA
               MOVE    TBL-RECEDATA (IDX)  TO  ORCSDIVCSV-REC
               MOVE    5                   TO  ORCSDIVCSV-ELE-C
               MOVE    ","                 TO  ORCSDIVCSV-DELI
               MOVE    ZERO                TO  ORCSDIVCSV-DQ
               CALL    "ORCSDIVCSV"     USING  ORCSDIVCSV-AREA
               IF      ORCSDIVCSV-RC       =   ZERO
                   IF      ORCSDIVCSV-D (3)
                                           NOT =   SPACE
                       PERFORM VARYING IDF FROM    1   BY  1
                               UNTIL   IDF >   WRK-FTNKBN-TBL-MAX
                           IF      ORCSDIVCSV-D (3)
                                           =   WRK-FTNKBN-VAL (IDF)
                               GO  TO    20021-FTNKBN-SAVE-EXT
                           END-IF
                       END-PERFORM
                       IF      IDF             >   5
                           GO  TO    20021-FTNKBN-SAVE-EXT
                       END-IF
                       MOVE    ORCSDIVCSV-D (3)
                                               TO  WRK-FTNKBN-VAL (IDF)
                       MOVE    IDF             TO  WRK-FTNKBN-TBL-MAX
                   END-IF
               END-IF
           END-IF
           .
       20021-FTNKBN-SAVE-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報レコード編集処理
      *****************************************************************
       20021-HENSYU-SEC            SECTION.
      *
           COMPUTE WRK-FILE-RENNUM =   WRK-VOLNUM  +   1
           MOVE    WRK-PARA-HOSPNUM    TO  RECE30PARA-HOSPNUM
           IF      WRK-PARA-TEISYUTUSAKI
                                   =   "1"
               MOVE    "S"             TO  RECE30PARA-TEISYUTUSAKI
           ELSE
               MOVE    "K"             TO  RECE30PARA-TEISYUTUSAKI
           END-IF
           IF      WRK-PARA-NYUGAIKBN
                                   =   "1"
               MOVE    "2"             TO  RECE30PARA-NYUGAIKBN
           ELSE
               MOVE    "1"             TO  RECE30PARA-NYUGAIKBN
           END-IF
           MOVE    LNK-PRTKANRI-TBL-GROUP (1:8)
                                   TO  RECE30PARA-CREYMD
           MOVE    LNK-PRTKANRI-TBL-GROUP (9:6)
                                   TO  RECE30PARA-CREHMS
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECE30PARA-BASENAME
                                       TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECE30PARA
      *
      *??
           DISPLAY "PARA=" RECE30PARA "#"
      *??
      *
           OPEN    OUTPUT              RECE30-FILE
      *
           IF      STS-RECE30          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file open err " RECE30PARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "ファイル オープンエラー STS="
                                               DELIMITED  BY  SIZE
                       STS-RECE30              DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           MOVE    ZERO                TO  WRK-DATA-LENGTH
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "IR,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "1"         TO  WRK-DATA
               WHEN    OTHER
                   MOVE    "2"         TO  WRK-DATA
           END-EVALUATE
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    SYS-1001-PREFNUM    TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    SYS-1001-TENHYOKBN  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    SYS-1001-HOSPCD     TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    WRK-SYS-1001-HOSPNAME
                                       TO  WRK-DATA
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    WRK-SEIYM           TO  WRK-DATA
           MOVE    5                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    WRK-VOLNUM          TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    WRK-SYS-1002-TEL    TO  WRK-DATA
           MOVE    15                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20021-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携用レセプト共通レコード１編集処理
      *****************************************************************
       20022-R1-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-DATA-LENGTH
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "R1,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *
           MOVE    SPACE               TO  WRK-DATA
           IF      PTPUBLIC-PATIENT-ID1
                                   NOT =   SPACE
               MOVE    PTPUBLIC-PATIENT-ID1
                                           TO  WRK-DATA
           ELSE
               IF      SYS-9101-PTNUM      =   "T"
      *            地域連携IDが空白の場合患者番号を記録
                   MOVE    WRK-PTNUM           TO  WRK-DATA
               END-IF
           END-IF
           MOVE    20                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    WRK-NYUINYMD        TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    WRK-TAIINYMD        TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20022-R1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携用レセプト共通レコード２編集処理
      *****************************************************************
       20023-R2-HENSYU-SEC          SECTION.
      *
      *    患者情報取得
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               GO  TO  20023-R2-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-DATA-LENGTH
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "R2,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *
           MOVE    PTINF-KANANAME      TO  WRK-DATA
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    PTINF-HOME-POST     TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           STRING  PTINF-HOME-ADRS
                   PTINF-HOME-BANTI
                       DELIMITED   BY  SPACE
               INTO    WRK-DATA
           END-STRING
           MOVE    800                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    PTINF-HOME-TEL1     TO  WRK-DATA
           MOVE    13                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    PTINF-RENRAKU-POST  TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           STRING  PTINF-RENRAKU-ADRS
                   PTINF-RENRAKU-BANTI
                       DELIMITED   BY  SPACE
               INTO    WRK-DATA
           END-STRING
           MOVE    800                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    PTINF-RENRAKU-TEL1  TO  WRK-DATA
           MOVE    13                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20023-R2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携用レセプト共通レコード３編集処理
      *****************************************************************
       20024-R3-HENSYU-SEC          SECTION.
      *
      *    患者情報取得
      *      連携用レセプト共通レコード２編集処理で取得済み
           IF      FLG-PTINF           =   1
               GO  TO  20024-R3-HENSYU-EXT
           END-IF
      *
      *    アレルギー１
           IF      PTINF-ALLERGY-1 NOT =   SPACE
               MOVE    ZERO                TO  WRK-DATA-LENGTH
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
               MOVE    "R3,1,"             TO  WRK-REC
               MOVE    5                   TO  WRK-REC-LENGTH
      *
               MOVE    PTINF-ALLERGY-1     TO  WRK-DATA
               MOVE    120                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-IF
      *    アレルギー２
           IF      PTINF-ALLERGY-2 NOT =   SPACE
               MOVE    ZERO                TO  WRK-DATA-LENGTH
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
               MOVE    "R3,1,"             TO  WRK-REC
               MOVE    5                   TO  WRK-REC-LENGTH
      *
               MOVE    PTINF-ALLERGY-2     TO  WRK-DATA
               MOVE    120                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-IF
      *    副作用歴１
           IF      PTINF-TABOO-1   NOT =   SPACE
               MOVE    ZERO                TO  WRK-DATA-LENGTH
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
               MOVE    "R3,2,"             TO  WRK-REC
               MOVE    5                   TO  WRK-REC-LENGTH
      *
               MOVE    PTINF-TABOO-1       TO  WRK-DATA
               MOVE    120                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-IF
      *    副作用歴２
           IF      PTINF-TABOO-2   NOT =   SPACE
               MOVE    ZERO                TO  WRK-DATA-LENGTH
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
               MOVE    "R3,2,"             TO  WRK-REC
               MOVE    5                   TO  WRK-REC-LENGTH
      *
               MOVE    PTINF-TABOO-2       TO  WRK-DATA
               MOVE    120                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-IF
           .
       20024-R3-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携用コメントレコード編集処理
      *****************************************************************
       20025-C1-HENSYU-SEC         SECTION.
      *
           INITIALIZE  SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    LNK-PRTKANRI-SRYYM  TO  ACCT-SRYYM
           MOVE    "99"                TO  ACCT-SRYKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SRYACCT
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
               ELSE
                   INITIALIZE  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
           ELSE
               INITIALIZE  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACCT     =   1
               PERFORM 200251-C1-HENSYU-SEC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SRYACCT
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
               ELSE
                   INITIALIZE  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       20025-C1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携用コメントレコード編集処理
      *****************************************************************
       200251-C1-HENSYU-SEC        SECTION.
      *
           INITIALIZE  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SRYACT
                   MOVE    MCPDATA-REC         TO  SRYACT-REC
               ELSE
                   INITIALIZE  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACT    =    1
               PERFORM 200252-C1-HENSYU-SEC
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SRYACT
                   MOVE    MCPDATA-REC         TO  SRYACT-REC
               ELSE
                   INITIALIZE  SRYACT-REC
                   MOVE    1                   TO  FLG-SRYACT
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       200251-C1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携用コメントレコード編集処理
      *****************************************************************
       200252-C1-HENSYU-SEC        SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >   5
                   OR      SRY-SRYCD (IDX2)    =   SPACE
               IF     (SRY-SRYCD (IDX2)    =   "008600000")  AND
                      (SRY-INPUTNUM (IDX2)
                                       NOT =   ZERO)
                   MOVE    SPA-HOSPNUM             TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID                TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM              TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD   (IDX2)      TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)      TO  PTCOM-RENNUM
                   PERFORM 900-PTCOM-READ-SEC
                   PERFORM 200253-C1-HENSYU-SEC
               END-IF
           END-PERFORM
           .
       200252-C1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携用コメントレコード編集処理
      *****************************************************************
       200253-C1-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-DATA-LENGTH
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           MOVE    "C1,01,"            TO  WRK-REC
           MOVE    6                   TO  WRK-REC-LENGTH
      *
           IF     (ACCT-PTID       NOT =   WRK-SV-PTID)        OR
                  (ACCT-HKNCOMBI   NOT =   WRK-SV-HKNCOMBI)
               INITIALIZE  ORCSGETFTNKBNAREA
               MOVE    SPA-HOSPNUM         TO  ORCSGETFK-HOSPNUM
               MOVE    ACCT-PTID           TO  ORCSGETFK-PTID
               MOVE    ACCT-SRYYM          TO  ORCSGETFK-SRYYM
               MOVE    ACCT-HKNCOMBI       TO  ORCSGETFK-HKNCOMBINUM
      *
               CALL    "ORCSGETFTNKBN" USING   ORCSGETFTNKBNAREA
                                               SPA-AREA
               PERFORM 2002531-FIX-FTNKBN-SEC
               MOVE    ACCT-PTID           TO  WRK-SV-PTID
               MOVE    ACCT-HKNCOMBI       TO  WRK-SV-HKNCOMBI
           END-IF
      *
           IF      WRK-FTNKBN          =   SPACE
               GO  TO  200253-C1-HENSYU-EXT
           ELSE
               MOVE    WRK-FTNKBN          TO  WRK-DATA
           END-IF
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    "819990001"         TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    PTCOM-INPUTCOMENT   TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >   31
               IF      IDX3                =   31
                   MOVE    1                   TO  WRK-END
               END-IF
               IF      ACCT-DAY(1 IDX3)    =   ZERO
                   MOVE    SPACE               TO  WRK-DATA
                   MOVE    1                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
               ELSE
                   MOVE    ACCT-DAY(1 IDX3)    TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       200253-C1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携用コメントレコード編集処理
      *****************************************************************
       2002531-FIX-FTNKBN-SEC      SECTION.
      *
      *     DISPLAY ""
      *     DISPLAY "PTID { " ORCSGETFK-PTID " ]"
      *     DISPLAY "PTCOM-INPUTCOMENT     [ " PTCOM-INPUTCOMENT " ]"
      *     DISPLAY "ORCSGETFK-HKNJANUM    [ " ORCSGETFK-HKNJANUM " ]"
      *     DISPLAY "WRK-HKNJANUM          [ " WRK-HKNJANUM " ]"
      *     DISPLAY ""
      *     DISPLAY "ORCSGETFK-FTNJANUM(1) [ " ORCSGETFK-FTNJANUM(1) " ]"
      *     DISPLAY "WRK-FTNJANUM(1)       [ " WRK-FTNJANUM(1) " ]"
      *     DISPLAY "ORCSGETFK-FTNJANUM(2) [ " ORCSGETFK-FTNJANUM(2) " ]"
      *     DISPLAY "WRK-FTNJANUM(2)       [ " WRK-FTNJANUM(2) " ]"
      *     DISPLAY "ORCSGETFK-FTNJANUM(3) [ " ORCSGETFK-FTNJANUM(3) " ]"
      *     DISPLAY "WRK-FTNJANUM(3)       [ " WRK-FTNJANUM(3) " ]"
      *
      *保険者番号が同じ？
           IF     (ORCSGETFK-HKNJANUM  =   SPACE)  AND
                  (WRK-HKNJANUM        =   SPACE)
               CONTINUE
           ELSE
               IF      ORCSGETFK-HKNJANUM  =   WRK-HKNJANUM
                   MOVE    ORCSGETFK-FTNKBN    TO  WRK-FTNKBN
               ELSE
                   MOVE    SPACE               TO  WRK-FTNKBN
                   GO  TO  2002531-FIX-FTNKBN-EXT
               END-IF
           END-IF
      *
           PERFORM VARYING IDF FROM    1   BY  1
                   UNTIL  (ORCSGETFK-FTNJANUM (IDF) =   SPACE) OR
                          (IDF >   4)
      *負担者番号が同じ
               PERFORM VARYING IDF1    FROM    1   BY  1
                       UNTIL   IDF1    >   4
                   IF      ORCSGETFK-FTNJANUM (IDF)
                                           =   WRK-FTNJANUM (IDF1)
                       MOVE    ORCSGETFK-FTNKBN    TO  WRK-FTNKBN
                       EXIT    PERFORM
                   END-IF
                   IF      IDF1                =   4
                       MOVE    SPACE               TO  WRK-FTNKBN
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF      WRK-FTNKBN-TBL-MAX  >   ZERO
               PERFORM VARYING IDF FROM    1   BY  1
                       UNTIL   IDF >   WRK-FTNKBN-TBL-MAX
                   IF      WRK-FTNKBN          =   WRK-FTNKBN-VAL (IDF)
                       EXIT    PERFORM
                   ELSE
                       IF      IDF             =   WRK-FTNKBN-TBL-MAX
                           MOVE    SPACE           TO  WRK-FTNKBN
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       2002531-FIX-FTNKBN-EXT.
           EXIT.
      *----(05.00.00)--ADD-START---
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       2002-BYOMEI-HENSYU-SEC      SECTION.
      *
           INITIALIZE                      ORCSDIVCSV-AREA
           MOVE    RECEDEN-RECEDATA    TO  ORCSDIVCSV-REC
           MOVE    8                   TO  ORCSDIVCSV-ELE-C
           MOVE    ","                 TO  ORCSDIVCSV-DELI
           MOVE    ZERO                TO  ORCSDIVCSV-DQ
           CALL    "ORCSDIVCSV"            USING
                                           ORCSDIVCSV-AREA
      *    未コード化傷病名コードの場合は処理抜ける
           IF      ORCSDIVCSV-RC       =   ZERO
               IF      ORCSDIVCSV-D(2)     =   "0000999"
                   GO  TO   2002-BYOMEI-HENSYU-EXT
               ELSE
                   MOVE    SPACE       TO  WRK-BYOMEICD-G
                   MOVE    ORCSDIVCSV-D(5)
                                       TO  WRK-BYOMEICD-G
      *
                   INITIALIZE              ORCSBYOMEIAREA
                   MOVE    "2"         TO  LNK-BYO-SYORI
      *
                   MOVE    ZERO        TO  IDX-B2
                   PERFORM VARYING IDX-B1  FROM    1   BY  1
                           UNTIL ( IDX-B1  >       20 )
                           OR    ( WRK-BYOMEICD(IDX-B1)    >=  "8000")
                           OR    ( WRK-BYOMEICD(IDX-B1)    =   SPACE )
                       COMPUTE IDX-B2  =   IDX-B2  +   1
                       MOVE    SPACE   TO  LNK-BYO-TBYOMEICD(IDX-B2)
                       STRING  "ZZZ"       DELIMITED BY SIZE
                               WRK-BYOMEICD(IDX-B1)
                                           DELIMITED BY SIZE
                               INTO        LNK-BYO-TBYOMEICD(IDX-B2)
                       END-STRING
                   END-PERFORM
      *
                   COMPUTE IDX-B2  =   IDX-B2  +   1
                   MOVE    ORCSDIVCSV-D(2)
                                       TO  LNK-BYO-TBYOMEICD(IDX-B2)
      *
                   PERFORM VARYING IDX-B1  FROM    IDX-B1  BY  1
                           UNTIL ( IDX-B1  >       20 )
                           OR    ( WRK-BYOMEICD(IDX-B1)    >=  "9000")
                           OR    ( WRK-BYOMEICD(IDX-B1)    =   SPACE )
                       COMPUTE IDX-B2  =   IDX-B2  +   1
                       MOVE    SPACE   TO  LNK-BYO-TBYOMEICD(IDX-B2)
                       STRING  "ZZZ"       DELIMITED BY SIZE
                               WRK-BYOMEICD(IDX-B1)
                                           DELIMITED BY SIZE
                               INTO        LNK-BYO-TBYOMEICD(IDX-B2)
                       END-STRING
                   END-PERFORM
      *
                   CALL    "ORCSBYOMEI"        USING
                                               ORCSBYOMEIAREA
                                               SPA-AREA
      *            病名編集
                   MOVE    LNK-BYO-BYOMEI  TO  ORCSDIVCSV-D(6)
      *
                   MOVE    SPACE           TO  WRK-REC
                   STRING  ORCSDIVCSV-D(1)     DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           ORCSDIVCSV-D(2)     DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           ORCSDIVCSV-D(3)     DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           ORCSDIVCSV-D(4)     DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           ORCSDIVCSV-D(5)     DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           ORCSDIVCSV-D(6)     DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           ORCSDIVCSV-D(7)     DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           ORCSDIVCSV-D(8)     DELIMITED   BY  SPACE
                                                   INTO        WRK-REC
                   END-STRING
                   MOVE    WRK-REC         TO  RECEDEN-RECEDATA
               END-IF
           END-IF
      *
           .
       2002-BYOMEI-HENSYU-EXT.
           EXIT.
      *----(05.00.00)--ADD-END-----
      *----(05.00.01)--ADD-START---
      *****************************************************************
      *    名称編集処理
      *****************************************************************
       2002-NAME-HENSYU-SEC      SECTION.
      *
           INITIALIZE                      ORCSDIVCSV-AREA
           MOVE    RECEDEN-RECEDATA    TO  ORCSDIVCSV-REC
           MOVE    4                   TO  ORCSDIVCSV-ELE-C
           MOVE    ","                 TO  ORCSDIVCSV-DELI
           MOVE    ZERO                TO  ORCSDIVCSV-DQ
           CALL    "ORCSDIVCSV"            USING
                                           ORCSDIVCSV-AREA
           IF      ORCSDIVCSV-RC       NOT =   ZERO
               GO  TO  2002-NAME-HENSYU-EXT
           END-IF
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  TNS-YUKOSTYMD (1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD (7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           MOVE    ORCSDIVCSV-D(4)     TO  TNS-SRYCD
      *
           PERFORM 900-TENSU-READ-SEC
      *     IF      FLG-TENSU       =   ZERO
               PERFORM VARYING WRK-REC-LENGTH  FROM    1  BY  1
                       UNTIL ( WRK-REC-LENGTH  >   2500 )
                       OR    ( RECEDEN-RECEDATA(WRK-REC-LENGTH:1)
                                               =   WRK-KAIGYO )
                   CONTINUE
               END-PERFORM
               COMPUTE WRK-REC-LENGTH  =   WRK-REC-LENGTH - 1
               MOVE    SPACE           TO  WRK-REC
               STRING  RECEDEN-RECEDATA(1:WRK-REC-LENGTH)
                                       DELIMITED   BY  SIZE
                       ","             DELIMITED   BY  SIZE
                       TNS-NAME        DELIMITED   BY  SPACE
                       WRK-KAIGYO      DELIMITED   BY  SIZE
                                       INTO        WRK-REC
               END-STRING
               MOVE    WRK-REC         TO  RECEDEN-RECEDATA
      *     END-IF
      *
           .
       2002-NAME-HENSYU-EXT.
           EXIT.
      *----(05.00.01)--ADD-END-----
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        ファイル保存情報更新
               IF      WRK-PARA-DATAKBN    =   "5"
                   PERFORM 3001-FILE-INFO-INSERT-SEC
               END-IF
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECENUM     TO  JOB-UPDCNT2
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE300  IN   " CNT-IN
           DISPLAY "RECE300  OUT  " CNT-OUT
           DISPLAY "RECE300  RE   " WRK-RECENUM
           DISPLAY "*** ORCBM580 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    "recept4.sh"    TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SFILESV-SHORI-RENNUM
                                                        (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
      *****MOVE    WRK-PARA-FOLDER TO  SFILESV-FOR-FOLDER
      *****                                             (1)
           MOVE    WRK-SGETTEMP-TEMPDIR
                                   TO  SFILESV-FOR-FOLDER
                                                        (1)
           IF      WRK-PARA-TEISYUTUSAKI   =   "1"
               EVALUATE    WRK-PARA-NYUGAIKBN
                 WHEN    "1"
                   IF      WRK-PARA-PRTKBN   =   "7"
                       STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                               "RECEIPTCS2"        DELIMITED  BY  SIZE
                               LNK-PRTKANRI-TBL-GROUP
                                                   DELIMITED  BY  SIZE
                               ".UKE"              DELIMITED  BY  SIZE
                                             INTO  SFILESV-FOR-DATA(1)
                       END-STRING
                       STRING  "RECEIPTCS2"        DELIMITED  BY  SIZE
                               LNK-PRTKANRI-TBL-GROUP
                                                   DELIMITED  BY  SIZE
                               ".UKE"              DELIMITED  BY  SIZE
                                             INTO  SFILESV-TO-DATA (1)
                       END-STRING
                   ELSE
                       STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                               "RECEIPTCS3"        DELIMITED  BY  SIZE
                               LNK-PRTKANRI-TBL-GROUP
                                                   DELIMITED  BY  SIZE
                               ".UKE"              DELIMITED  BY  SIZE
                                             INTO  SFILESV-FOR-DATA(1)
                       END-STRING
                       STRING  "RECEIPTCS3"        DELIMITED  BY  SIZE
                               LNK-PRTKANRI-TBL-GROUP
                                                   DELIMITED  BY  SIZE
                               ".UKE"              DELIMITED  BY  SIZE
                                             INTO  SFILESV-TO-DATA (1)
                       END-STRING
                   END-IF
                 WHEN    "2"
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "RECEIPTCS1"        DELIMITED  BY  SIZE
                           LNK-PRTKANRI-TBL-GROUP
                                               DELIMITED  BY  SIZE
                           ".UKE"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   STRING  "RECEIPTCS1"        DELIMITED  BY  SIZE
                           LNK-PRTKANRI-TBL-GROUP
                                               DELIMITED  BY  SIZE
                           ".UKE"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                   END-STRING
               END-EVALUATE
           ELSE
               EVALUATE    WRK-PARA-NYUGAIKBN
                 WHEN    "1"
                   IF      WRK-PARA-PRTKBN   =   "7"
                       STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                               "RECEIPTCK2"        DELIMITED  BY  SIZE
                               LNK-PRTKANRI-TBL-GROUP
                                                   DELIMITED  BY  SIZE
                               ".UKE"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                       END-STRING
                       STRING  "RECEIPTCK2"        DELIMITED  BY  SIZE
                               LNK-PRTKANRI-TBL-GROUP
                                                   DELIMITED  BY  SIZE
                               ".UKE"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                       END-STRING
                   ELSE
                       STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                               "RECEIPTCK3"        DELIMITED  BY  SIZE
                               LNK-PRTKANRI-TBL-GROUP
                                                   DELIMITED  BY  SIZE
                               ".UKE"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                       END-STRING
                       STRING  "RECEIPTCK3"        DELIMITED  BY  SIZE
                               LNK-PRTKANRI-TBL-GROUP
                                                   DELIMITED  BY  SIZE
                               ".UKE"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                       END-STRING
                   END-IF
                 WHEN    "2"
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "RECEIPTCK1"        DELIMITED  BY  SIZE
                           LNK-PRTKANRI-TBL-GROUP
                                               DELIMITED  BY  SIZE
                           ".UKE"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   STRING  "RECEIPTCK1"        DELIMITED  BY  SIZE
                           LNK-PRTKANRI-TBL-GROUP
                                               DELIMITED  BY  SIZE
                           ".UKE"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                   END-STRING
               END-EVALUATE
           END-IF
      *
           MOVE    CNT-OUT         TO  SFILESV-CNT-ALL  (1)
           IF      WRK-PARA-TEISYUTUSAKI   =   "1"
               EVALUATE    WRK-PARA-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "社保　連携ファイル作成（入院）"
                                           TO  SFILESV-TITLE    (1)
                   WHEN    "2"
                       MOVE    "社保　連携ファイル作成（入院外）"
                                           TO  SFILESV-TITLE    (1)
               END-EVALUATE
           ELSE
               EVALUATE    WRK-PARA-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "国保　連携ファイル作成（入院）"
                                           TO  SFILESV-TITLE    (1)
                   WHEN    "2"
                       MOVE    "国保　連携ファイル作成（入院外）"
                                           TO  SFILESV-TITLE    (1)
               END-EVALUATE
           END-IF
      *
           MOVE    "1"             TO  SFILESV-DATA-TYPE(1)
      *
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
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
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
            .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBM580 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           MOVE    WRK-REC             TO  RECE30-R
           WRITE   RECE30-R
      *
           IF      STS-RECE30          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file write err " RECE30PARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "ファイル 書き込みエラー STS="
                                               DELIMITED  BY  SIZE
                       STS-RECE30              DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    ZERO                TO  WRK-SYMD
           END-IF
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD            TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-YMD        TO  WRK-HENYMDG
           END-IF
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    右づめ編集処理
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           .
       5003-DATA-RIGHT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW <       1
                   IF      WRK-DATA (IDW:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW)           DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF
      *
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ
                       MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療情報連携基盤データ読込
      *****************************************************************
       900-RENKEI-TMP-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-END
           MOVE    SPACE               TO  WRK-SRYKBN
      *
           MOVE    SPACE               TO  RECEDEN-REC
           INITIALIZE                      RECEDEN-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECEDEN-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  RECEDEN-NYUGAIKBN
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    1               TO  RECEDEN-TEISYUTUSAKI
               WHEN    "2"
               WHEN    "4"
                   MOVE    2               TO  RECEDEN-TEISYUTUSAKI
               WHEN    "3"
                   MOVE    6               TO  RECEDEN-TEISYUTUSAKI
           END-EVALUATE
      *
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-RENKEI-TMP-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-RENKEI-TMP-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療情報連携基盤データ読込
      *****************************************************************
       900-RENKEI-TMP-READ-SEC           SECTION.
      *
           MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECEDEN-REC
      *
               EVALUATE    RECEDEN-RECEDATA (1:2)
                   WHEN    "RE"
                   WHEN    "HO"
                   WHEN    "KO"
                   WHEN    "SY"
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    ZERO            TO  FLG-SRYKBN
                   WHEN    "TO"
                   WHEN    "IY"
                   WHEN    "SI"
                   WHEN    "CO"
                       IF      RECEDEN-RECEDATA (4:1)  = ","
                           CONTINUE
                       ELSE
                           MOVE    RECEDEN-RECEDATA (4:2)
                                                   TO  WRK-SRYKBN
                           IF      RECEDEN-RECEDATA (1:2)  =   "TO"
                                                           OR  "CO"
                               MOVE    1               TO  FLG-SRYKBN
                           ELSE
                               MOVE    ZERO            TO  FLG-SRYKBN
                           END-IF
                       END-IF
                   WHEN    OTHER
                       GO  TO  900-RENKEI-TMP-READ-SEC
               END-EVALUATE
      *
               IF     (RECEDEN-RECEDATA (1:2)  =   "SI"  OR  "IY") AND
                      (RECEDEN-RECEDATA (4:1)  =   ",")  AND
                      (FLG-SRYKBN              =   1)
                   MOVE    SPACE               TO  WRK-REC
                   STRING  RECEDEN-RECEDATA (1:3)  DELIMITED   BY  SIZE
                           WRK-SRYKBN              DELIMITED   BY  SIZE
                           RECEDEN-RECEDATA (4:)   DELIMITED   BY  SIZE
                                                   INTO        WRK-REC
                   END-STRING
                   MOVE    WRK-REC         TO  RECEDEN-RECEDATA
                   MOVE    ZERO            TO  FLG-SRYKBN
               END-IF
      *----(05.00.01)--ADD-START---
               IF    ( SYS-9101-NMREC      =   "T" )
                   IF    ( RECEDEN-RECEDATA (1:2)  =   "SI"  OR  "IY" )
                       PERFORM 2002-NAME-HENSYU-SEC
                   END-IF
               END-IF
      *----(05.00.01)--ADD-END-----
               EVALUATE    RECEDEN-RECEDATA (1:2)
                   WHEN    "RE"
                   WHEN    "HO"
                   WHEN    "KO"
                       CONTINUE
                   WHEN    "SY"
                       IF      SYS-9101-SYREC      =   "T"
      *----(05.00.00)--ADD-START---
                           IF      SYS-9101-SYBYO      =   "T"
                               PERFORM 2002-BYOMEI-HENSYU-SEC
                           END-IF
      *                     CONTINUE
      *----(05.00.00)--ADD-END-----
                       ELSE
                           GO  TO  900-RENKEI-TMP-READ-SEC
                       END-IF
                   WHEN    "IY"
                       IF      WRK-SRYKBN   =   "14"    OR  "21"    OR
                                                "22"    OR  "23"    OR
                                                "31"    OR  "32"    OR
                                                "33"
                           CONTINUE
                       ELSE
                           GO  TO  900-RENKEI-TMP-READ-SEC
                       END-IF
                   WHEN    "SI"
                       IF      WRK-SRYKBN   =   "14"    OR  "31"    OR
                                                "32"    OR  "33"    OR
                                                "60"
                           CONTINUE
                       ELSE
                           GO  TO  900-RENKEI-TMP-READ-SEC
                       END-IF
                   WHEN    OTHER
                       GO  TO  900-RENKEI-TMP-READ-SEC
               END-EVALUATE
      *
               ADD     1                   TO  CNT-IN
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-RENKEI-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
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
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1900-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(19)  NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
                END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(19)
                                   TO  SYS-1901-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1901-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-SYS-1001-HOSPNAME
           ELSE
               INITIALIZE                      SYS-1901-REC
           END-IF
           .
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(19)
                                   TO  SYS-1902-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1902-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1902-REC
               IF      SYS-1902-TEL    NOT =   SPACE
                   MOVE    SYS-1902-TEL        TO  WRK-SYS-1002-TEL
               END-IF
           ELSE
               INITIALIZE                      SYS-1902-REC
           END-IF
           .
       900-1902-READ-EXT.
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
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者地域連携読込
      *****************************************************************
       900-PTPUBLIC-READ-SEC       SECTION.
      *
           INITIALIZE  PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    SPA-PTID            TO  PTPUBLIC-PTID
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTPUBLIC
                   MOVE    MCPDATA-REC         TO  PTPUBLIC-REC
               ELSE
                   INITIALIZE  PTPUBLIC-REC
                   MOVE    1               TO  FLG-PTPUBLIC
               END-IF
           ELSE
               INITIALIZE  PTPUBLIC-REC
               MOVE    1                   TO  FLG-PTPUBLIC
           END-IF
      *
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTPUBLIC-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   INITIALIZE  PTINF-REC
                   MOVE    1                   TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE  PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       900-PTCOM-READ-SEC          SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTCOM
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
               ELSE
                   INITIALIZE  PTCOM-REC
                   MOVE    1                   TO  FLG-PTCOM
               END-IF
           ELSE
               INITIALIZE  PTCOM-REC
               MOVE    1                   TO  FLG-PTCOM
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *----(05.00.01)--ADD-START---
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-TENSU
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               ELSE
                   INITIALIZE                      TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-TENSU-READ-EXT.
           EXIT.
      *----(05.00.01)--ADD-END-----
      *----(05.00.02)--ADD-START---
      *****************************************************************
      *    受診履歴読み込み
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    RECEDEN-PTID        TO  JYURRK-PTID
           MOVE    RECEDEN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    LNK-PRTKANRI-SKYYMD TO  JYURRK-SRYYMD
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-JYURRK
                   MOVE    MCPDATA-REC         TO  JYURRK-REC
               ELSE
                   INITIALIZE                      JYURRK-REC
                   MOVE    1                   TO  FLG-JYURRK
               END-IF
           ELSE
               INITIALIZE                      JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *----(05.00.02)--ADD-END-----
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCBM580 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"          USING   WRK-ERRMSG
           END-IF
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
