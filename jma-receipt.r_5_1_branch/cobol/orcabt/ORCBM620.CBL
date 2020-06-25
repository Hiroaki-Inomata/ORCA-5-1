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
       PROGRAM-ID.             ORCBM620.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 様式４
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/10/15    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    15/12/10  テスト患者を対象外とする
      *  04.08.02    NACL-藤原    15/12/22  公費単独の医療保険との組合せ
      *                                     を５にする
      *  04.08.03    NACL-藤原    16/04/28  申出があるときは４にする
      *  04.08.04    NACL-藤原    17/03/30  入院年月日を履歴から取得する
      *  04.08.05    NACL-藤原    17/04/19  特別療養費は対象外とする
      *  04.08.06    NACL-藤原    17/04/21  特別療養費は対象外ではなく
      *                                     ３とする 
      *  04.08.07    NACL-藤原    17/06/21  入院年月日を履歴から取得する
      *                                     ときの条件を追加
      *  04.08.08    NACL-藤原    17/07/20  直近の履歴が入院中の自院歴の
      *                                     場合のみ入院年月日を取得する
      *  05.00.00    NACL-小豆沢  20/02/19  入院中の判定に食事の剤と短期
      *                                     滞在の入院料未算定日を追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *    様式４
           SELECT  STYLE4-FILE     ASSIGN  STYLE4PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-STYLE4.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE  ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    様式４
       FD  STYLE4-FILE.
       01  STYLE4-R                    PIC X(2500).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    様式４ファイル名称 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //STYLE4PARA//.
      *
       01  STYLE4PARA-BASENAME.
           03  STYLE4PARA-HOSPNUM  PIC 9(02).
           03  FILLER              PIC X(04)   VALUE   "DPC_".
           03  STYLE4PARA-FILENM   PIC X(22).
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCMSL80.INC".
      *
       01  STS-AREA.
           03  STS-STYLE4          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-CHK-SRYACCT     PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01). 
           03  FLG-HKNNUM          PIC 9(01). 
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTTOKKI         PIC 9(01).
           03  FLG-PTNYUINRRK    PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-OUT             PIC 9(05).
           03  CNT-CSV             PIC 9(05).
      *
       01  INDEX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-HKNCOMBI  PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-HKNCOMBI  PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-JOBID      PIC 9(07).
           05  WRK-PARA-SHELLID    PIC X(08).
           05  WRK-PARA-HOSPNUM    PIC 9(02).
           05  WRK-PARA-FILENM     PIC X(22).
           05  WRK-PARA-HOSPCD     PIC X(07).
           05  WRK-PARA-PREFNUM    PIC X(02).
           05  WRK-PARA-DATAKBN    PIC X(01).
      *
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-CHK-AREA.
      *        1:医保 2:公費単独 3:公害、労災、自賠責 4:治験 5:自費
               05  WRK-CHK         PIC X(01)   OCCURS  5.
           03  WRK-HKNNUM          PIC 9(01).
      *
           03  WRK-DOUJITU-AREA.
               05  WRK-DOUJITU-NYU PIC X(01).
               05  WRK-DOUJITU-TAI PIC X(01).
           03  WRK-KBN             PIC X(01). 
           03  WRK-PTNUM-X.
               05  WRK-PTNUM       PIC 9(10).
           03  WRK-ST              PIC 9(02).
           03  WRK-ED              PIC 9(02). 
           03  WRK-SRYYM           PIC X(06).
      *
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(04)
                                           OCCURS   31.
      *
      *    ＣＳＶデータ編集用
       01  WRK-CSV-AREA.
           03  WRK-OUT-REC         PIC X(2000).
           03  WRK-CSV-REC         PIC X(2500).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-CSV-HENSYU-AREA.
               05  WRK-DATA        PIC X(2000).
               05  WRK-IN-MAX      PIC 9(04).
               05  WRK-END         PIC 9(01).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR           PIC X(200).
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
       01  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
      *
      *    ワーク入院履歴
       01  WRK-PTNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WRK-PTNRRK-//.
      *
       01  CSV-HEAD-01.
           03  FILLER                  PIC X(10)   VALUE
               "施設コード".
           03  FILLER                  PIC X(01)   VALUE   X"09".
           03  FILLER                  PIC X(14)   VALUE
               "データ識別番号".
           03  FILLER                  PIC X(01)   VALUE   X"09".
           03  FILLER                  PIC X(10)   VALUE
               "入院年月日".
           03  FILLER                  PIC X(01)   VALUE   X"09".
           03  FILLER                  PIC X(10)   VALUE
               "退院年月日".
           03  FILLER                  PIC X(01)   VALUE   X"09".
           03  FILLER                  PIC X(20)   VALUE
               "医療保険外との組合せ".
      *
       01  HEAD-01.
           03  FILLER                  PIC X(04)   VALUE   SPACE.
           03  FILLER                  PIC X(16)   VALUE
               "様式４".
           03  H01-TAIINYM             PIC X(16).
           03  FILLER                  PIC X(06)   VALUE   "退院".
           03  FILLER                  PIC X(36)   VALUE
               "医科保険診療以外のある症例調査票".
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-SYSYMD              PIC X(09).
           03  FILLER                  PIC X(05)   VALUE   "   P.".
           03  H01-PAGE                PIC ZZZ9.
      *
       01  HEAD-02.
           03  FILLER                  PIC X(14)   VALUE   SPACE.
           03  FILLER                  PIC X(12)   VALUE
               "施設コード".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(14)   VALUE
               "データ識別番号".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(10)   VALUE
               "入院年月日".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(10)   VALUE
               "退院年月日".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(20)   VALUE
               "医療保険外との組合せ".
      *
       01  BODY-01.
           03  B01-FIL1                PIC X(14).
           03  B01-PREFNUM             PIC X(02).
           03  B01-HOSPCD              PIC X(07).
           03  B01-FIL2                PIC X(05).
           03  B01-PTNUM               PIC X(10).
           03  B01-FIL3                PIC X(06).
           03  B01-NYUINYMD            PIC X(08).
           03  B01-FIL4                PIC X(04).
           03  B01-TAIINYMD            PIC X(08).
           03  B01-FIL5                PIC X(04).
           03  B01-KBN                 PIC X(01).
      *
       01  WRK-CONST-AREA.
           03  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   80.
      *    タブ
           03  WRK-KUGIRI              PIC X(01)   VALUE   X"09".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM  PIC X(03)   VALUE   "975".
      *
           COPY    "COMMON-SPA".
      *
      *    診療会計
       01  CHK-SRYACCT-REC.
           COPY    "CPSRYACCT.INC" REPLACING   //ACCT-//
                                   BY          //CHK-ACCT-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    ＥＦファイルデータ
           COPY    "CPSK9102.INC".
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者特記事項情報
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
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
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE                   DIVISION
                                       USING   COMMAND-PARAM.
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
                                       WRK-PARA-PREFNUM
                                       WRK-PARA-HOSPCD
                                       WRK-PARA-FILENM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-DATAKBN
                                       WRK-PARA-HOSPNUM
                                       RECEERR
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
      *
           DISPLAY "LNK-PRTKANRI-SRYYM=" LNK-PRTKANRI-SRYYM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM620"      TO  JOB-PGID
           MOVE    "様式４"        TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           MOVE    SPA-HOSPNUM     TO  STYLE4PARA-HOSPNUM
           MOVE    WRK-PARA-FILENM TO  STYLE4PARA-FILENM
           MOVE    STYLE4PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  STYLE4PARA
           DISPLAY "PARA=" STYLE4PARA "#"
      *
           MOVE   SGETTEMP-TEMPDIR TO  WRK-SGETTEMP-TEMPDIR
      *
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2) 
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (1:16)
                                   TO  H01-TAIINYM
      *
      *    作成日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  H01-SYSYMD
      *
      *    ＥＦファイルデータ情報取得
           INITIALIZE                      SYS-9102-REC
           MOVE    SPA-HOSPNUM         TO  SYS-9102-HOSPNUM
           MOVE    "9102"              TO  SYS-9102-KANRICD
           MOVE    "*"                 TO  SYS-9102-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-9102-STYUKYMD  (1:6)
           MOVE    "01"                TO  SYS-9102-STYUKYMD  (7:2)
           MOVE    SYS-9102-STYUKYMD   TO  SYS-9102-EDYUKYMD  
           MOVE    SYS-9102-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9102-REC
           ELSE
               INITIALIZE                  SYS-9102-REC
           END-IF
           IF      SYS-9102-NUMBER     =   SPACE
               MOVE    "0"                 TO  SYS-9102-NUMBER
           END-IF
           IF      SYS-9102-KOH-RECEIPT
                                       =   SPACE
               MOVE    "0"                 TO  SYS-9102-KOH-RECEIPT
           END-IF
           DISPLAY "SYS-9102-NUMBER     =" SYS-9102-NUMBER
           DISPLAY "SYS-9102-KOH-RECEIPT=" SYS-9102-KOH-RECEIPT
      *
           MOVE    WRK-CONS-LINE-MAX   TO  CNT-LINE
      *
      *    ＣＳＶ
           OPEN    OUTPUT              STYLE4-FILE
      *
           IF      STS-STYLE4          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file open err " STYLE4PARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "ファイル オープンエラー STS="
                                               DELIMITED  BY  SIZE
                       STS-STYLE4              DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM    TO  PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM    TO  PTNYUINRRK-NYUINYMD(1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD(7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key66"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    テスト患者は読み飛ばす
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
             DISPLAY "PTID=" PTNYUINRRK-PTID " " PTINF-TSTPTNUMKBN " "
                             PTNYUINRRK-NYUINYMD " " PTNYUINRRK-TAIINYMD
      *
           IF      PTINF-TSTPTNUMKBN   =   "1"
               CONTINUE
           ELSE
             MOVE    PTNYUINRRK-NYUINYMD (1:6)
                                       TO  WRK-SRYYM
      *
             INITIALIZE                      WRK-CHK-AREA
      *
             PERFORM         UNTIL   WRK-SRYYM
                                       >   PTNYUINRRK-TAIINYMD (1:6)
               MOVE    1                   TO  WRK-ST
               MOVE    31                  TO  WRK-ED
               IF      WRK-SRYYM           =   PTNYUINRRK-NYUINYMD (1:6)
                   MOVE    PTNYUINRRK-NYUINYMD (7:2)
                                               TO  WRK-ST
               END-IF 
               IF      WRK-SRYYM           =   PTNYUINRRK-TAIINYMD (1:6)
                   MOVE    PTNYUINRRK-TAIINYMD (7:2)
                                               TO  WRK-ED
               END-IF 
      *
      *        同日再入院情報取得
               PERFORM 2001-ORCSNYUINDAY-CALL-SEC
      *??
      *       DISPLAY "WRK-SRYYM=" WRK-SRYYM "#" WRK-ST "#" WRK-ED 
      *                    "#" WRK-DOUJITU-NYU "#" WRK-DOUJITU-TAI
      *??
      *        入院会計チェック
               PERFORM 2002-NYUINACCT-CHECK-SEC
      *        診療会計チェック
               PERFORM 2003-SRYACCT-CHECK-SEC
      *
               INITIALIZE                     STS-AREA-DAY
               INITIALIZE                     LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SRYYM           TO  LNK-DAY6-KIJUN (1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)
               MOVE  "2"                   TO  LNK-DAY6-ZOGENPTN
               MOVE  1                     TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-SRYYM
             END-PERFORM
      *
      *      明細編集処理
             PERFORM 2004-HENSYU-SEC
           END-IF
      *
           PERFORM 900-PTNYUINRRK-READ-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院情報取得処理
      *****************************************************************
       2001-ORCSNYUINDAY-CALL-SEC   SECTION.
      *
           INITIALIZE                      WRK-DOUJITU-AREA
      *
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-NYUINDAY-HOSPNUM
           MOVE    PTNYUINRRK-NYUINYMD (1:6)
                                       TO  LNK-NYUINDAY-SANTEIYM
           MOVE    PTNYUINRRK-PTID     TO  LNK-NYUINDAY-PTID
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC     >   0
               DISPLAY "LNK-NYUINDAY-RC=" LNK-NYUINDAY-RC
           END-IF
           IF      LNK-NYUINDAY-DOUJITSU-KBN (WRK-ST)
                                       =   "1"
               MOVE    "1"                 TO  WRK-DOUJITU-NYU
           END-IF
      *
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-NYUINDAY-HOSPNUM
           MOVE    PTNYUINRRK-TAIINYMD (1:6)
                                       TO  LNK-NYUINDAY-SANTEIYM
           MOVE    PTNYUINRRK-PTID     TO  LNK-NYUINDAY-PTID
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC     >   0
               DISPLAY "LNK-NYUINDAY-RC=" LNK-NYUINDAY-RC
           END-IF
           IF      LNK-NYUINDAY-DOUJITSU-KBN (WRK-ED)
                                       =   "1"
               MOVE    "1"                 TO  WRK-DOUJITU-TAI
           END-IF
           .
       2001-ORCSNYUINDAY-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計個別編集処理
      *****************************************************************
       2002-NYUINACCT-CHECK-SEC              SECTION.
      *     
           INITIALIZE                      WRK-DAY-AREA
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-SRYYM           TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key67"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key67"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM         UNTIL   FLG-NYUINACCT   =   1 
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                   WHEN    "4"
                       MOVE    1               TO  IDX
                   WHEN    "5"
                       MOVE    2               TO  IDX
               END-EVALUATE
               PERFORM VARYING IDX1    FROM    WRK-ST  BY  1
                       UNTIL   IDX1    >       WRK-ED
                   IF      NACCT-DAY  (IDX1)   >   ZERO
                       MOVE    NACCT-DAY (IDX1)    TO  WRK-DAY(IDX IDX1)
                   END-IF
               END-PERFORM            
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key67"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
      *    短期滞在で入院料未算定日
           PERFORM VARYING IDX1    FROM    WRK-ST  BY  1
                   UNTIL   IDX1    >       WRK-ED
               IF      LNK-NYUINDAY-TANKI2-KBN(IDX1)  NOT =  SPACE
                   MOVE    1    TO  WRK-DAY(1 IDX1)
               END-IF
           END-PERFORM            
      *
      *    入院会計より保険組合せチェック
           PERFORM VARYING IDX     FROM    WRK-ST   BY  1
                   UNTIL   IDX     >       WRK-ED
               IF    ( WRK-DAY (1 IDX)         >   ZERO )
               AND   ( WRK-DAY (2 IDX)         >   ZERO )
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    PTNYUINRRK-HOSPNUM
                                           TO  COMB-HOSPNUM
                   MOVE    PTNYUINRRK-PTID TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX) TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
      *??
      *         DISPLAY "NYUACCT=" IDX "#" WRK-DAY (2 IDX)
      *??
                   PERFORM 20021-HKNCOMBI-CHECK-SEC
                   IF      WRK-HKNNUM      >   ZERO
                       MOVE    "1"             TO  WRK-CHK (WRK-HKNNUM)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key67"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2002-NYUINACCT-CHECK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    保険チェック処理
      *****************************************************************
       20021-HKNCOMBI-CHECK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-HKNNUM
      *
           PERFORM 910-HKNCOMBI-INV-SEC
      *
           IF      FLG-HKNCOMBI        =   ZERO
               EVALUATE    COMB-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                   WHEN    WRK-CONS-KOUGAI-HKNNUM
                       MOVE    3               TO  WRK-HKNNUM
                   WHEN    SPACE
                       IF      SYS-9102-KOH-RECEIPT
                                               =   "0"
                           MOVE    2               TO  WRK-HKNNUM
                       END-IF
                   WHEN    "040"
                   WHEN    "068"
                   WHEN    "069"
                       MOVE    3               TO  WRK-HKNNUM
                   WHEN    OTHER
                       IF      COMB-HKNNUM (1:1)
                                               =   "9"
                           INITIALIZE              HKNNUM-REC
                           MOVE    COMB-HOSPNUM    TO  HKN-HOSPNUM
                           MOVE    COMB-HKNNUM     TO  HKN-HKNNUM
                           MOVE    "00"            TO  HKN-PAYKBN
                           MOVE    WRK-SRYYM       TO  HKN-TEKSTYMD(1:6)
                           MOVE    "01"            TO  HKN-TEKSTYMD(7:2)
                           MOVE    HKN-TEKSTYMD    TO  HKN-TEKEDYMD
                           PERFORM 900-HKNNUM-INV-SEC
                           IF      HKN-HKNKOHSBTKBN    =   "8"
                               MOVE    5               TO  WRK-HKNNUM
                           END-IF
                           IF      HKN-HKNKOHSBTKBN    =   "9"
                               MOVE    4               TO  WRK-HKNNUM
                           END-IF
                       ELSE
                           MOVE    1           TO  WRK-HKNNUM
                       END-IF
               END-EVALUATE
      *??
      *        DISPLAY "WRK-HKNNUM=" COMB-HKNNUM "#" WRK-HKNNUM
      *??
           END-IF
           .
       20021-HKNCOMBI-CHECK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       2003-SRYACCT-CHECK-SEC                SECTION.
      *
           IF    ( WRK-CHK (1)         =   "1" )
           AND   ( WRK-CHK (2)         =   "1" )
           AND   ( WRK-CHK (3)         =   "1" )
           AND   ( WRK-CHK (4)         =   "1" )
           AND   ( WRK-CHK (5)         =   "1" )
               GO  TO  2003-SRYACCT-CHECK-EXT
           END-IF
      *
      *    診療会計チェック
           INITIALIZE                      KEY-AREA
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  ACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  ACCT-PTID
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACCT     =   1
               MOVE    ZERO                TO  FLG-OK
      *??
      *         DISPLAY "ACCT=" ACCT-HKNCOMBI " SRYKA=" ACCT-SRYKA
      *??
      *  
               IF   (( ACCT-SRYYM      =   PTNYUINRRK-NYUINYMD (1:6) )
                 AND ( WRK-DOUJITU-NYU =   "1"                       ) )
               OR   (( ACCT-SRYYM      =   PTNYUINRRK-TAIINYMD (1:6) )
                 AND ( WRK-DOUJITU-TAI =   "1"                       ) )
      *            受診履歴から同日再入院の保険組合せチェック
                   PERFORM 20031-JYURRK-CHECK-SEC
               ELSE  
                   MOVE    1                   TO  FLG-OK
               END-IF
      *
      *??
      *         DISPLAY "ok=" flg-ok
      *??
               IF      FLG-OK              =   1
                   INITIALIZE                      HKNCOMBI-REC
                   MOVE    ACCT-HOSPNUM        TO  COMB-HOSPNUM
                   MOVE    ACCT-PTID           TO  COMB-PTID
                   MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 20021-HKNCOMBI-CHECK-SEC
                   IF      WRK-HKNNUM          =   ZERO
      *                保険がエラーの時同じ保険組合せは対象外
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM         UNTIL   FLG-SRYACCT =   1 
                                       OR      KEY-NEW NOT =   KEY-OLD
                           PERFORM 900-SRYACCT-READ-SEC
                       END-PERFORM
                   ELSE
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM         UNTIL   FLG-SRYACCT =   1 
                                       OR      KEY-NEW NOT =   KEY-OLD
      *                    診療会計の診療区分チェック
                           PERFORM 20032-SRYACCT-JIHI-CHECK-SEC
      *
                           PERFORM 900-SRYACCT-READ-SEC
                       END-PERFORM
                   END-IF
               ELSE
                   PERFORM 900-SRYACCT-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2003-SRYACCT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴から同日再入院の保険組合せチェック
      *****************************************************************
       20031-JYURRK-CHECK-SEC SECTION.
      *
           INITIALIZE                      JYURRK-REC
           MOVE    ACCT-HOSPNUM        TO  JYURRK-HOSPNUM
           MOVE    ACCT-SRYYM          TO  JYURRK-SRYYMD (1:6)
           IF      ACCT-SRYYM           =   PTNYUINRRK-NYUINYMD (1:6)
               MOVE    WRK-ST              TO  JYURRK-SRYYMD (7:)
           ELSE
               MOVE    WRK-ED              TO  JYURRK-SRYYMD (7:)
           END-IF
           MOVE    "1"                 TO  JYURRK-NYUGAIKBN
           MOVE    ACCT-PTID           TO  JYURRK-PTID
           MOVE    ACCT-HKNCOMBI       TO  JYURRK-HKNCOMBINUM
           MOVE    ACCT-SRYKA          TO  JYURRK-SRYKA
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           PERFORM             UNTIL   FLG-JYURRK  =   1
                               OR      FLG-OK      =   1
      *        同日再入院の診療月のとき
               IF      PTNYUINRRK-NYUINYMD =   PTNYUINRRK-TAIINYMD
      *            入院日＝退院日のときは１回目だけ対象
                   IF       JYURRK-RENNUM      =   1
                       MOVE    1                   TO  FLG-OK
                   END-IF
               ELSE
      *            入院日のときは２回目を対象
                   IF       ACCT-SRYYM =   PTNYUINRRK-NYUINYMD (1:6)
                       IF       JYURRK-RENNUM      =   2
                           MOVE    1                   TO  FLG-OK
                       END-IF
                   ELSE
      *                退院日のときは１回目を対象
                       IF       ACCT-SRYYM =   PTNYUINRRK-TAIINYMD (1:6)
                           IF       JYURRK-RENNUM      =   1
                               MOVE    1                   TO  FLG-OK
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *??
      *             DISPLAY "JYURRK-RENNUM=" JYURRK-RENNUM "#" FLG-OK
      *??
      *
               PERFORM 900-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20031-JYURRK-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計の診療区分による入力チェック
      *****************************************************************
       20032-SRYACCT-JIHI-CHECK-SEC               SECTION.
      *
           INITIALIZE                      CHK-SRYACCT-REC
           MOVE    ACCT-HOSPNUM        TO  CHK-ACCT-HOSPNUM
           MOVE    ACCT-PTID           TO  CHK-ACCT-PTID
           MOVE    ACCT-SRYYM          TO  CHK-ACCT-SRYYM
           MOVE    ACCT-NYUGAIKBN      TO  CHK-ACCT-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  CHK-ACCT-SRYKA
           MOVE    ACCT-HKNCOMBI       TO  CHK-ACCT-HKNCOMBI
           MOVE    CHK-SRYACCT-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SRYACCT-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHK-SRYACCT
           END-IF
      *
      *    自費保険のときは診療内容に関係なく自費扱いとする
           PERFORM             UNTIL   FLG-CHK-SRYACCT =   1
               IF    ( CHK-ACCT-SRYKBN =   "95"    OR  "96" )
               AND   ( WRK-HKNNUM          NOT =   5        )
                   INITIALIZE                      SRYACT-REC
                   MOVE    CHK-ACCT-HOSPNUM    TO  SRY-HOSPNUM
                   MOVE    CHK-ACCT-NYUGAIKBN  TO  SRY-NYUGAIKBN
                   MOVE    CHK-ACCT-PTID       TO  SRY-PTID
                   MOVE    CHK-ACCT-SRYKA      TO  SRY-SRYKA
                   MOVE    CHK-ACCT-SRYYM      TO  SRY-SRYYM
                   MOVE    CHK-ACCT-ZAINUM     TO  SRY-ZAINUM
                   MOVE    SRYACT-REC          TO  MCPDATA-REC
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       PERFORM 900-SRYACT-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SRYACT
                   END-IF
                   PERFORM             UNTIL   FLG-SRYACT  =   1
                                       OR      WRK-CHK (5) =   "1"
                       IF    ( SRY-SRYCD (1) (1:1) =
                                                   "1" OR  "6" OR  "7" )
                       OR    ( SRY-SRYCD (2) (1:1) =
                                                   "1" OR  "6" OR  "7" )
                       OR    ( SRY-SRYCD (3) (1:1) =
                                                   "1" OR  "6" OR  "7" )
                       OR    ( SRY-SRYCD (4) (1:1) =
                                                   "1" OR  "6" OR  "7" )
                       OR    ( SRY-SRYCD (5) (1:1) =
                                                   "1" OR  "6" OR  "7" )
                           MOVE    "1"             TO  WRK-CHK (5)
                       END-IF
                       PERFORM 900-SRYACT-READ-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"       TO  MCP-TABLE
                   MOVE    "key2"             TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               ELSE
                   MOVE    "1"             TO  WRK-CHK (WRK-HKNNUM)
               END-IF
      *
               PERFORM 900-SRYACCT-CHK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *??
      *        DISPLAY "JIHI=" COMB-HKNNUM " 1:" 
      *                               WRK-CHK (1) " 2:" WRK-CHK (2) " 3:"
      *                               WRK-CHK (3) " 4:" WRK-CHK (4) " 5:"
      *                               WRK-CHK (5) "#"
      *??
      *
           .
       20032-SRYACCT-JIHI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       2004-HENSYU-SEC             SECTION.
      *
           IF      WRK-CHK-AREA    =   SPACE
               GO  TO  2004-HENSYU-EXT
           END-IF
      *??
      *       DISPLAY "CHK=1:"  WRK-CHK (1) " 2:" WRK-CHK (2) " 3:"
      *                         WRK-CHK (3) " 4:" WRK-CHK (4) " 5:"
      *                         WRK-CHK (5) "#"
      *??
      *
           IF      WRK-CHK (2)     =   "1"
      *        公費単独があるとき
               MOVE    "5"             TO  WRK-KBN
           ELSE
               IF    ( WRK-CHK (1)     =   SPACE )
      *            医保分がないとき
                   IF    ( WRK-CHK (3)     =   "1" )
                   OR    ( WRK-CHK (4)     =   "1" )
                   OR    ( WRK-CHK (5)     =   "1" )
                       MOVE    "3"             TO  WRK-KBN
                   END-IF
               ELSE
      *            医保分があるとき
                   IF    ( WRK-CHK (3)     =   SPACE )
                   AND   ( WRK-CHK (4)     =   SPACE )
                   AND   ( WRK-CHK (5)     =   SPACE )
      *                患者特記事項チェック処理（３７申出）
                       PERFORM 20043-PTTOKKI-CHECK-SEC
                       IF      FLG-PTTOKKI     =   "9"
                           MOVE    "4"             TO  WRK-KBN
                       ELSE
                           MOVE    "1"             TO  WRK-KBN
                       END-IF
                   ELSE
                       IF      WRK-CHK (5)     =   "1"
                           MOVE    "5"             TO  WRK-KBN
                       ELSE
                           IF    ( WRK-CHK (3)     =   "1" )
                           OR    ( WRK-CHK (4)     =   "1" )
                               MOVE    "4"             TO  WRK-KBN
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    DISPLAY "WRK-KBN=" WRK-KBN
      *    明細編集処理
           PERFORM 20041-BODY-PRINT-SEC
           .
       2004-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    患者特記事項チェック処理（３７申出）
      *****************************************************************
       20043-PTTOKKI-CHECK-SEC     SECTION.
      *
           INITIALIZE                      PTTOKKI-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  PTTOKKI-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTTOKKI-PTID
           MOVE    "2"                 TO  PTTOKKI-NYUGAIKBN
           MOVE    PTNYUINRRK-NYUINYMD (1:6)
                                       TO  PTTOKKI-EDYM
           MOVE    PTNYUINRRK-TAIINYMD (1:6)
                                       TO  PTTOKKI-STYM
           MOVE    PTTOKKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-PTTOKKI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTTOKKI
           END-IF
      *
           PERFORM             UNTIL   FLG-PTTOKKI     =   1   OR  9
               IF      PTTOKKI-CD          =   "37"
                   MOVE    9                   TO  FLG-PTTOKKI
               ELSE
                   MOVE    "tbl_pttokki"   TO  MCP-TABLE
                   MOVE    "key4"          TO  MCP-PATHNAME
                   PERFORM 900-PTTOKKI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       20043-PTTOKKI-CHECK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20041-BODY-PRINT-SEC         SECTION.
      *
           IF      CNT-LINE             >=  79
               PERFORM 20042-HEAD-PRINT-SEC
           END-IF
      *
           MOVE    SPACE               TO  BODY-01
      *
           MOVE    WRK-PARA-PREFNUM    TO  B01-PREFNUM
           MOVE    WRK-PARA-HOSPCD     TO  B01-HOSPCD
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
      *
           DISPLAY "OUT PTNUM="  SPTID-PTNUM
      *
           IF      SYS-9102-NUMBER     =   "0"
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPTID-PTNUM         TO  SNUM-INX
               CALL    "ORCSNUM"           USING   ORCSNUMAREA
               IF    ( SNUM-RC             =   ZERO  )
               AND   ( SNUM-SEISUU         <=  10    )
               AND   ( SNUM-SYOSUU         =   0     )
               AND   ( SNUM-MINKBN         =   SPACE ) 
                   MOVE    SNUM-OUTNUM         TO  WRK-PTNUM
               ELSE
                   STRING
                      "患者番号の桁数が１０桁を超えているか数字以外です"
                                               DELIMITED   BY  SIZE
                           "　患者番号＝"      DELIMITED   BY  SIZE
                           SPTID-PTNUM         DELIMITED   BY  SPACE
                                               INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           ELSE
               MOVE    PTNYUINRRK-PTID     TO  WRK-PTNUM
           END-IF
           MOVE    WRK-PTNUM       TO  B01-PTNUM
      *
      *    入院情報の履歴取得処理
           PERFORM 200411-NYUINYMD-HENSYU-SEC
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  B01-NYUINYMD
      *
           MOVE    PTNYUINRRK-TAIINYMD
                                   TO  B01-TAIINYMD
      *
           MOVE    WRK-KBN         TO  B01-KBN
      *
           ADD     1               TO  CNT-LINE
           MOVE    BODY-01         TO  HCMSL80-MOJI (CNT-LINE)
      *
      *    ＣＳＶデータ出力
           PERFORM 500-CSV-SYORI-SEC
      *
           ADD     1               TO  CNT-LINE
           ADD     1               TO  CNT-OUT
           .
       20041-BODY-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院情報の履歴取得処理
      *****************************************************************
       200411-NYUINYMD-HENSYU-SEC                SECTION.
      *??
               DISPLAY "200411 IN=" PTNYUINRRK-NYUINYMD "#"
      *??
      *
           MOVE    PTNYUINRRK-REC  TO  WRK-PTNRRK-REC
      *
      *    初回番号が同じ入院履歴から直近の入院年月日を取得
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    WRK-PTNRRK-HOSPNUM
                                   TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PTNRRK-PTID TO  PTNYUINRRK-PTID
           MOVE    WRK-PTNRRK-SHONUM
                                   TO  PTNYUINRRK-SHONUM
           MOVE    WRK-PTNRRK-NYUINYMD
                                   TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 900-PTNYUINRRK-KEY51-READ-SEC
      *
      *??
               DISPLAY "200411 FLG-PTNYUINRRK=" FLG-PTNYUINRRK "#"
      *??
           IF      FLG-PTNYUINRRK  =   ZERO
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-PTNRRK-NYUINYMD
           END-IF
      *
           MOVE    WRK-PTNRRK-REC TO  PTNYUINRRK-REC
      *??
               DISPLAY "200411 OT =" PTNYUINRRK-NYUINYMD "#"
      *??
           .
       200411-NYUINYMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       20042-HEAD-PRINT-SEC                SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 2005-PRINT-OUT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  H01-PAGE
      *
           MOVE    SPACE               TO  HCMSL80
           MOVE    HEAD-01             TO  HCMSL80-MOJI (1)
           MOVE    HEAD-02             TO  HCMSL80-MOJI (3)
           MOVE    4                   TO  CNT-LINE
           .
       20042-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2005-PRINT-OUT-SEC                SECTION.
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
           MOVE   "HCMSL80.red"        TO  SPRT-PRTID
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           MOVE    "様式４"            TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       2005-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ処理
      *****************************************************************
       500-CSV-SYORI-SEC             SECTION.
      *
           IF      CNT-CSV             =   ZERO
               PERFORM 5001-CSV-HEAD-SEC
           END-IF
      *
           INITIALIZE                      WRK-CSV-AREA
      *    施設コード
           MOVE    WRK-PARA-PREFNUM    TO  WRK-DATA (1:2)
           MOVE    WRK-PARA-HOSPCD     TO  WRK-DATA (3:)
           MOVE    9                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    データ識別番号
           MOVE    WRK-PTNUM-X         TO  WRK-DATA
           MOVE    10                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    入院年月日
           MOVE    PTNYUINRRK-NYUINYMD TO  WRK-DATA
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    退院年月日
           MOVE    PTNYUINRRK-TAIINYMD TO  WRK-DATA
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *
           MOVE    WRK-KBN             TO  WRK-DATA
           MOVE    1                   TO  WRK-IN-MAX
           MOVE    1                   TO  WRK-END
           PERFORM 5002-MOJI-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-OUT-REC
           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                   WRK-KAIGYO              DELIMITED  BY  SIZE
                   INTO        WRK-OUT-REC
           END-STRING
           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
           PERFORM 600-TOUKEICSV-SEC
      *
           .
       500-CSV-SYORI-EXIT.
           EXIT.
      *
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       5001-CSV-HEAD-SEC           SECTION.
      *
           MOVE    CSV-HEAD-01         TO  WRK-OUT-REC
           MOVE    SPACE               TO  WRK-CSV-REC
           STRING  WRK-OUT-REC         DELIMITED  BY  SPACE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                                       INTO  WRK-CSV-REC
           END-STRING
           PERFORM 600-TOUKEICSV-SEC
      *
           .
       5001-CSV-HEAD-EXIT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5002-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA         =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW     FROM    WRK-IN-MAX  BY  -1
                       UNTIL   IDW     <       1
                   IF      WRK-DATA (IDW:1) NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA (1:IDW)
                                                   TO  WRK-CSV-REC
                       ELSE
                           MOVE    SPACE           TO  WRK-OUT-REC
                           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-OUT-REC
                           END-STRING
                           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END             =   ZERO
               MOVE    SPACE               TO  WRK-OUT-REC
               STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                       WRK-KUGIRI              DELIMITED  BY  SIZE
                       INTO        WRK-OUT-REC
               END-STRING
               MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-CSV-HENSYU-AREA
      *
           .
       5002-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       600-TOUKEICSV-SEC           SECTION.
      *
           MOVE    WRK-CSV-REC         TO  STYLE4-R
           WRITE   STYLE4-R
      *
           IF      STS-STYLE4          =   "00"
               CONTINUE
           ELSE
               CALL   "coblog" USING   "file write err " STYLE4PARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "ファイル 書き込みエラー STS="
                                           DELIMITED  BY  SIZE
                       STS-STYLE4          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-CSV
           .
      *
       600-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR   =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR   TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
           PERFORM 500-COBABORT-SEC
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBM620 "  DELIMITED  BY  SIZE
                   WRK-RECEERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key66"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE   STYLE4-FILE
      *
      *    帳票印刷処理
           IF      CNT-PAGE        >   ZERO
               PERFORM 2005-PRINT-OUT-SEC
           END-IF
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
           DISPLAY "*** ORCBM620 OUT  "  CNT-OUT
           DISPLAY "*** ORCBM620 PAGE "  CNT-PAGE
           DISPLAY "*** ORCBM620 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC   SECTION.
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
           MOVE    WRK-SGETTEMP-TEMPDIR
                                   TO  SFILESV-FOR-FOLDER
                                                        (1)
           STRING  WRK-PARA-HOSPNUM    DELIMITED   BY  SIZE
                   STYLE4PARA-FILENM   DELIMITED   BY  SIZE
                                       INTO    SFILESV-FOR-DATA (1)
           END-STRING
           MOVE    STYLE4PARA-FILENM
                                   TO  SFILESV-TO-DATA  (1)
           MOVE    CNT-OUT         TO  SFILESV-CNT-ALL  (1)
           MOVE    "様式４"        TO  SFILESV-TITLE    (1)
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
      *    入院履歴ＲＥＡＤ
      *****************************************************************
       900-PTNYUINRRK-READ-SEC     SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key66"             TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN   =   "5"
                   IF      PTNYUINRRK-TAINRELKBN    =   "1"
                       CONTINUE
                   ELSE
                       GO  TO  900-PTNYUINRRK-READ-SEC
                   END-IF
               END-IF
           ELSE
               MOVE    1                    TO  FLG-END
           END-IF
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       900-SRYACCT-READ-SEC        SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
      *
      *        入院期間中のみ対象
               PERFORM VARYING IDX     FROM    WRK-ST  BY  1
                       UNTIL   IDX     >       WRK-ED
                   IF      ACCT-DAY (1 IDX)    >   ZERO
                       MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
                       GO  TO  900-SRYACCT-READ-EXT
                   END-IF
               END-PERFORM
               GO  TO  900-SRYACCT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計（自費分）読込
      *****************************************************************
       900-SRYACCT-CHK-READ-SEC    SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-CHK-SRYACCT
               MOVE    MCPDATA-REC         TO  CHK-SRYACCT-REC
      *
               IF      CHK-ACCT-ZAIKAISU       >   0
                   CONTINUE
               ELSE
                   GO  TO  900-SRYACCT-CHK-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-CHK-SRYACCT
           END-IF
           .
       900-SRYACCT-CHK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               INITIALIZE                      JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計読込
      *****************************************************************
       900-NYUINACCT-READ-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC        SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-INV-SEC            SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNNUM-REC
                   MOVE    ZERO                TO  FLG-HKNNUM
               ELSE
                   INITIALIZE                      HKNNUM-REC
                   MOVE    1                   TO  FLG-HKNNUM
               END-IF
           ELSE
               INITIALIZE                      HKNNUM-REC
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *  
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者特記事項読込
      *****************************************************************
       900-PTTOKKI-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTTOKKI-REC
               MOVE    ZERO            TO  FLG-PTTOKKI
           ELSE
               INITIALIZE                  PTTOKKI-REC
               MOVE    1               TO  FLG-PTTOKKI
           END-IF
      *
           .
       900-PTTOKKI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（入院中の自院歴のみ対象）
      *****************************************************************
       900-PTNYUINRRK-KEY51-READ-SEC    SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"
                                   TO  MCP-TABLE
           MOVE    "key16"         TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptnyuinrrk"
                                       TO  MCP-TABLE
               MOVE    "key16"         TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC           =   ZERO
                   MOVE    ZERO                 TO  FLG-PTNYUINRRK
                   MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
                   IF    ( PTNYUINRRK-JTIKBN    =   "6" )
                   AND   ( PTNYUINRRK-NYUINCHUKBN
                                                =   "1" )
                       CONTINUE
                   ELSE
                       MOVE    1                    TO  FLG-PTNYUINRRK
                   END-IF
               ELSE
                   MOVE    1                    TO  FLG-PTNYUINRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"
                                   TO  MCP-TABLE
           MOVE    "key16"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTNYUINRRK-KEY51-READ-EXT.
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
               STRING  "ORCBM620 select err TABLE="
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *
