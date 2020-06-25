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
       PROGRAM-ID.             ORHCML13.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ
      *  コンポーネント名  : 一般名一覧表（HCML13）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/05/14    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-森脇    14/07/31  一時ファイルディレクトリ設定
      *  04.08.02    NACL-森脇    16/03/17  金額種別４に対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  TENSUERR-FILE   ASSIGN  TENSUERR
                                   FILE    STATUS  IS  STS-TENSUERR.
      *    ＣＳＶファイル
           SELECT  TENSUCSV-FILE   ASSIGN  TENSUCSV
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-TENSUCSV.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  TENSUERR-FILE.
       01  TENSUERR-REC            PIC X(200).
      *    ＣＳＶファイル
       FD  TENSUCSV-FILE.
       01  TENSUCSV-REC.
           03  CSV-HOSPNUM         PIC 9(2).
           03  CSV-FILLER1         PIC X(1).
           03  CSV-SRYCD           PIC X(9).
           03  CSV-FILLER2         PIC X(1).
           03  CSV-YUKOSTYMD       PIC X(8).
           03  CSV-FILLER3         PIC X(1).
           03  CSV-YUKOEDYMD       PIC X(8).
           03  CSV-FILLER4         PIC X(1).
           03  CSV-NAME            PIC X(200).
           03  CSV-FILLER5         PIC X(1).
      *----(04.08.02)--UPD-START---
           03  CSV-TEN             PIC X(12).
      *----(04.08.02)--UPD-START---
           03  CSV-FILLER6         PIC X(1).
           03  CSV-TANINAME        PIC X(24).
           03  CSV-FILLER7         PIC X(1).
           03  CSV-YKZKBN          PIC X(2).
           03  CSV-FILLER8         PIC X(1).
           03  CSV-YAKKAKJNCD      PIC X(12).
           03  CSV-FILLER9         PIC X(1).
           03  CSV-KISAIKBN        PIC X(2).
           03  CSV-FILLER10        PIC X(1).
           03  CSV-GNAME           PIC X(200).
           03  CSV-FILLER11        PIC X(1).
      *----(04.08.02)--UPD-START---
           03  CSV-SAITEN          PIC X(12).
      *----(04.08.02)--UPD-START---
           03  CSV-FILLER12        PIC X(1).
           03  CSV-KOUHATUKBN      PIC X(1).
           03  CSV-FILLER13        PIC X(1).
           03  CSV-SYUSAISKB       PIC X(1).
           03  CSV-FILLER14        PIC X(1).
           03  CSV-INPUTCD         PIC X(20).
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *----(04.08.01)--ADD-START---
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TENSUERR//.
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCML13.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TENSUERR        PIC X(02).
           03  STS-TENSUCSV        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUP          PIC 9(01).
           03  FLG-GENERIC         PIC 9(01).
           03  FLG-GENERICP        PIC 9(01).
           03  FLG-ICD             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-KENSU           PIC 9(07).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           COPY    "CPHCMLSHELL.INC"
                                   REPLACING  //SHELL-//
                                   BY         //WRK-PARA-//.
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-CSVKBN     PIC X(1).
           03  WRK-PARA-FILE       PIC X(1).
           03  WRK-PARA-CSVDIR     PIC X(100).
           03  WRK-PARA-CSVFILE    PIC X(100).
      *----(04.08.01)--UPD-START---
      *     03  TENSUERR            PIC X(100).
           03  TENSUCSV            PIC X(1024).
      *----(04.08.01)--UPD-END-----
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-TENSUERR        PIC X(200).
      *
           03  WRK-LNKYMD          PIC X(10).
           03  WRK-MSTYMDH         PIC X(10).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-PAGE            PIC ZZ,ZZ9.
           03  WRK-CSYYOURYO       PIC ZZ,ZZ9.
      *----(04.08.02)--ADD-START---
           03  WRK-TEN             PIC ZZZ,ZZZ,ZZ9.999.
           03  WRK-TENSU           PIC 9(09).999.
      *----(04.08.02)--ADD-END-----
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関名
           COPY  "CPSK1001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数添付情報
       01  TENSUP-REC.
           COPY    "CPTENSUPLUS.INC".
      *    一般名マスタ
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *    最低薬価マスタ
       01  GENERICPRICE-REC.
           COPY    "CPGENERICPRICE.INC".
      *    入力コード
       01  ICD-REC.
           COPY    "CPINPUTCD.INC".
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
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(1000).
      ****************************************************************
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
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
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
           INITIALIZE                  SUM-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  IDX-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  TENSUERR
           INITIALIZE                  TENSUCSV
      *----(04.08.01)--ADD-END-----
      *
           PERFORM 100-DBOPEN-SEC
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
                                           WRK-PARA-JOBID
                                           WRK-PARA-SHELLID
                                           WRK-PARA-AREA
                                           WRK-PARA-HOSPNUM
                                           TENSUERR
                                           WRK-PARA-CSVKBN
                                           WRK-PARA-FILE
                                           WRK-PARA-CSVDIR
                                           WRK-PARA-CSVFILE
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORHCML13"          TO  JOB-PGID
           MOVE    "一般名一覧表"      TO  JOB-SHELLMSG
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           MOVE    WRK-LNKYMD          TO  WRK-MSTYMDH
      *
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TENSUERR-BASENAME   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  TENSUERR
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
      *    医療機関名称編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-MSTYMD     TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF
           ELSE
               INITIALIZE                      SYS-1001-REC
               MOVE    "医療機関名称が取得できませんでした。"
                                           TO  WRK-TENSUERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
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
           IF      WRK-PARA-CSVKBN     =   "1"
      *        ＣＳＶ編集処理
               PERFORM 300-CSV-SEC
           ELSE
      *        帳票編集処理
               PERFORM 300-PRINT-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理（ＣＳＶ）
      *****************************************************************
       300-CSV-SEC                 SECTION.
      *
      *----(04.08.01)--UPD-START---
      *     STRING  "/tmp/"             DELIMITED SIZE
           STRING  SGETTEMP-TEMPDIR    DELIMITED SPACE
                   WRK-PARA-CSVFILE    DELIMITED SPACE
                   ".wk"               DELIMITED SIZE
                               INTO    TENSUCSV
           END-STRING
      *----(04.08.01)--UPD-END-----
      *
      *    ＣＳＶファイルを開く
           OPEN    OUTPUT              TENSUCSV-FILE
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      WRK-PARA-IYKCHK(IDX)    >   ZERO
      *            帳票印刷開始処理
                   PERFORM 900-DBSELECT-SEC
      *            帳票編集<CSV>処理
                   PERFORM 330-CSV-HEN-SEC
                           UNTIL   FLG-TENSU   =   1
      *            帳票印刷終了処理
                   PERFORM 900-DBCLOSE-SEC
               END-IF
           END-PERFORM
      *
      *    ＣＳＶファイルを閉じる
           CLOSE                       TENSUCSV-FILE
      *
      *    クライアント保存処理
           IF      WRK-PARA-FILE       =   "1"
      *        帳票ＣＳＶ処理
               PERFORM 380-CSV-OUT-SEC
           END-IF
      *
           .
       300-CSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       300-PRINT-SEC               SECTION.
      *
           PERFORM VARYING IDX FROM     1   BY  1
                   UNTIL   IDX >        6
               IF      WRK-PARA-IYKCHK(IDX)    >   ZERO
                   INITIALIZE                  CNT-AREA
      *            帳票印刷開始処理
                   PERFORM 900-DBSELECT-SEC
                   PERFORM UNTIL   FLG-TENSU   =   1
      *                帳票編集<見出し>処理
                       PERFORM 310-HEAD-HEN-SEC
      *                帳票編集<明細>処理
                       PERFORM 320-BODY-HEN-SEC
                               UNTIL   FLG-TENSU   =   1
                               OR      CNT-LINE    =   15
      *                帳票印刷処理
                       PERFORM 390-PRINT-OUT-SEC
                   END-PERFORM
      *            帳票印刷終了処理
                   PERFORM 900-DBCLOSE-SEC
               END-IF
           END-PERFORM
      *
           .
       300-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
                                           SUM-KENSU
      *
           MOVE    SPACE               TO  HCML13
      *
           MOVE    SYS-1001-HOSPNAME   TO  HCML13-HOSPNAME
           MOVE    WRK-MSTYMDH         TO  HCML13-MSTYMD
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HCML13-PAGE
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
           ADD     1                   TO  CNT-LINE
      *
      *----(04.08.02)--UPD-START---
           IF      TNS-TENSIKIBETU     =   4
               COMPUTE WRK-TENSU   =   TNS-TEN     /   10
               MOVE    WRK-TENSU           TO  WRK-TEN
               MOVE    WRK-TEN(2:14)       TO  HCML13-TEN(CNT-LINE)
           ELSE
               MOVE    TNS-TEN             TO  WRK-TEN
               MOVE    WRK-TEN(1:14)       TO  HCML13-TEN(CNT-LINE)
           END-IF
      *
           MOVE    GENERICPRICE-PRICE  TO  WRK-TEN
           IF      WRK-TEN(15:1)   =   "0"
               MOVE    WRK-TEN(1:14)   TO  HCML13-SAITEN(CNT-LINE)
           ELSE
               MOVE    WRK-TEN(2:14)   TO  HCML13-SAITEN(CNT-LINE)
           END-IF
      *----(04.08.02)--UPD-END-----
      *
           MOVE    TNS-SRYCD           TO  HCML13-SRYCD(CNT-LINE)
           MOVE    TNS-YAKKAKJNCD      TO  HCML13-YAKKAKJNCD(CNT-LINE)
           MOVE    ICD-INPUTCD         TO  HCML13-INPUTCD(CNT-LINE)
      *
           IF      TNS-NAME(91:)       =   SPACE
               MOVE    TNS-NAME            TO  HCML13-NAME(CNT-LINE)
           ELSE
               MOVE    TNS-NAME            TO  HCML13-NAME2(CNT-LINE)
           END-IF
           MOVE    TNS-TANINAME        TO  HCML13-TANINAME(CNT-LINE)
           MOVE    TNS-SYUSAISKB       TO  HCML13-SYUSAISKB(CNT-LINE)
           MOVE    TNS-KOUHATUKBN      TO  HCML13-KOUHATUKBN(CNT-LINE)
      *
      *    薬剤
           EVALUATE    TNS-YKZKBN
               WHEN    "1"
                   MOVE    "内"        TO  HCML13-YKZKBN(CNT-LINE)
               WHEN    "3"
                   MOVE    "他"        TO  HCML13-YKZKBN(CNT-LINE)
               WHEN    "4"
                   MOVE    "注"        TO  HCML13-YKZKBN(CNT-LINE)
               WHEN    "6"
                   MOVE    "外"        TO  HCML13-YKZKBN(CNT-LINE)
               WHEN    "8"
                   MOVE    "歯"        TO  HCML13-YKZKBN(CNT-LINE)
               WHEN    "9"
                   MOVE    "歯"        TO  HCML13-YKZKBN(CNT-LINE)
               WHEN    OTHER
                   MOVE    "＊"        TO  HCML13-YKZKBN(CNT-LINE)
           END-EVALUATE
      *
      *    一般名区分
           EVALUATE  TNSP-IPN-KISAIKBN
      *      0：記載しない
             WHEN  ZERO
               MOVE    "銘"            TO  HCML13-KISAIKBN(CNT-LINE)
               IF      GENERIC-GENERICNAME(139:)       =   SPACE
                   MOVE    GENERIC-GENERICNAME
                                           TO  HCML13-GNAME(CNT-LINE)
               ELSE
                   MOVE    GENERIC-GENERICNAME
                                           TO  HCML13-GNAME2(CNT-LINE)
               END-IF
      *      1：一般名を記載する
             WHEN  1
               MOVE    "般"            TO  HCML13-KISAIKBN(CNT-LINE)
               IF      GENERIC-GENERICNAME(139:)       =   SPACE
                   MOVE    GENERIC-GENERICNAME
                                           TO  HCML13-GNAME(CNT-LINE)
               ELSE
                   MOVE    GENERIC-GENERICNAME
                                           TO  HCML13-GNAME2(CNT-LINE)
               END-IF
      *      2：処方名称を記載する
      *      3：処方名称（一般名）を記載する
             WHEN  2
             WHEN  3
               MOVE    "処"            TO  HCML13-KISAIKBN(CNT-LINE)
               IF      TNSP-SHONAME(139:)       =   SPACE
                   MOVE    TNSP-SHONAME    TO  HCML13-GNAME(CNT-LINE)
               ELSE
                   MOVE    TNSP-SHONAME    TO  HCML13-GNAME2(CNT-LINE)
               END-IF
           END-EVALUATE
      *
           PERFORM 900-DBFETCH-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<CSV>処理
      *****************************************************************
       330-CSV-HEN-SEC             SECTION.
      *
           ADD     1                   TO  SUM-KENSU
      *
           INITIALIZE                      TENSUCSV-REC
           MOVE    TNS-HOSPNUM         TO  CSV-HOSPNUM
           MOVE    TNS-SRYCD           TO  CSV-SRYCD
           MOVE    TNS-YUKOSTYMD       TO  CSV-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  CSV-YUKOEDYMD
           MOVE    TNS-NAME            TO  CSV-NAME
           MOVE    TNS-TANINAME        TO  CSV-TANINAME
      *
      *----(04.08.02)--UPD-START---
           IF      TNS-TENSIKIBETU     =   4
               COMPUTE WRK-TENSU   =   TNS-TEN     /   10
               MOVE    WRK-TENSU(2:12)     TO  CSV-TEN
           ELSE
               MOVE    TNS-TEN             TO  WRK-TENSU
               MOVE    WRK-TENSU(1:12)     TO  CSV-TEN
           END-IF
      *
           MOVE    GENERICPRICE-PRICE  TO  WRK-TENSU
           IF      WRK-TENSU(13:1) =   "0"
               MOVE    WRK-TENSU(1:12) TO  CSV-SAITEN
           ELSE
               MOVE    WRK-TENSU(2:12) TO  CSV-SAITEN
           END-IF
      *----(04.08.02)--UPD-END-----
      *
           MOVE    TNS-SYUSAISKB       TO  CSV-SYUSAISKB
           MOVE    TNS-KOUHATUKBN      TO  CSV-KOUHATUKBN
      *
      *    薬剤 
           EVALUATE    TNS-YKZKBN
               WHEN    "1"
                   MOVE    "内"        TO  CSV-YKZKBN
               WHEN    "3"
                   MOVE    "他"        TO  CSV-YKZKBN
               WHEN    "4"
                   MOVE    "注"        TO  CSV-YKZKBN
               WHEN    "6"
                   MOVE    "外"        TO  CSV-YKZKBN
               WHEN    "8"
                   MOVE    "歯"        TO  CSV-YKZKBN
               WHEN    "9" 
                   MOVE    "歯"        TO  CSV-YKZKBN
               WHEN    OTHER
                   MOVE    "＊"        TO  CSV-YKZKBN
           END-EVALUATE
      *
           MOVE    TNS-YAKKAKJNCD      TO  CSV-YAKKAKJNCD
           MOVE    ICD-INPUTCD         TO  CSV-INPUTCD  
      *
      *    一般名区分
           EVALUATE  TNSP-IPN-KISAIKBN
      *        0：記載しない
               WHEN  ZERO
                   MOVE    "銘"                TO  CSV-KISAIKBN
                   MOVE    GENERIC-GENERICNAME TO  CSV-GNAME
      *        1：一般名を記載する
               WHEN  1
                   MOVE    "般"                TO  CSV-KISAIKBN
                   MOVE    GENERIC-GENERICNAME TO  CSV-GNAME
      *        2：処方名称を記載する
      *        3：処方名称（一般名）を記載する
               WHEN  2
               WHEN  3
                   MOVE    "処"                TO  CSV-KISAIKBN
                   MOVE    TNSP-SHONAME        TO  CSV-GNAME
           END-EVALUATE
      *
           MOVE    ","                 TO  CSV-FILLER1
                                           CSV-FILLER2
                                           CSV-FILLER3
                                           CSV-FILLER4
                                           CSV-FILLER5
                                           CSV-FILLER6
                                           CSV-FILLER7
                                           CSV-FILLER8
                                           CSV-FILLER9
                                           CSV-FILLER10
                                           CSV-FILLER11
                                           CSV-FILLER12
                                           CSV-FILLER13
                                           CSV-FILLER14
           WRITE   TENSUCSV-REC
      *
           PERFORM 900-DBFETCH-SEC
      *
           .
       330-CSV-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＣＳＶ処理
      *****************************************************************
       380-CSV-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSFILESVAREA
           MOVE    "I"                 TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID    TO  SFILESV-TBL-KEY
           MOVE    "tensu.sh"          TO  SFILESV-SHELLID(1)
           MOVE    1                   TO  SFILESV-SHORI-RENNUM(1)
           MOVE    LNK-PRTKANRI-RENNUM TO  SFILESV-RENNUM(1)
      *----(04.08.01)--UPD-START---
      *     MOVE    "/tmp/"             TO  SFILESV-FOR-FOLDER(1)
           MOVE    SGETTEMP-TEMPDIR    TO  SFILESV-FOR-FOLDER(1)
           STRING  WRK-PARA-CSVFILE        DELIMITED  BY  SPACE
                   ".wk"                   DELIMITED  BY  SIZE
                                   INTO    SFILESV-FOR-DATA (1)
           END-STRING
      *----(04.08.01)--UPD-END-----
           MOVE    WRK-PARA-CSVDIR     TO  SFILESV-TO-FOLDER(1)
           MOVE    WRK-PARA-CSVFILE    TO  SFILESV-TO-DATA(1)
           MOVE    SUM-KENSU           TO  SFILESV-CNT-ALL(1)
           MOVE    JOB-SHELLMSG        TO  SFILESV-TITLE(1)
           CALL    "ORCSFILESV"            USING
                                           ORCSFILESVAREA
                                           SPA-AREA
      *
           .
       380-CSV-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
           IF      CNT-LINE        =   ZERO
               GO  TO  390-PRINT-OUT-EXT
           END-IF
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
           MOVE    "HCML13.red"        TO  SPRT-PRTID
           MOVE    "一般名一覧表"      TO  SPRT-TITLE
           MOVE    HCML13              TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-TENSUERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               TENSUERR-FILE
           IF      STS-TENSUERR        =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              TENSUERR-FILE
               MOVE    WRK-TENSUERR    TO  TENSUERR-REC
               WRITE   TENSUERR-REC
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    WRK-TENSUERR        TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               CALL    "ORCSJOB"               USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
           CLOSE   TENSUERR-FILE
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
           DISPLAY "*** ORHCML13 END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SUM-KENSU           TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
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
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC           SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-LNKYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-LNKYMD
      *
           .
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ　検索 
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    "600000000"         TO  TNS-SRYCD
           MOVE    "699999999"         TO  TNSUP-SRYCD
           MOVE    WRK-PARA-MSTYMD     TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    WRK-PARA-IYKCHK(IDX)
                                       TO  TNS-YKZKBN
      *
           EVALUATE    WRK-PARA-CYUKBN
               WHEN    1
                   EVALUATE    WRK-PARA-SYUKBN
                       WHEN    1
                           MOVE    "key11s"    TO  WRK-PATH
                       WHEN    2
                           MOVE    "key11m"    TO  WRK-PATH
                       WHEN    3
                           MOVE    "key11k"    TO  WRK-PATH
                   END-EVALUATE 
               WHEN    2 
               WHEN    3
                   EVALUATE    WRK-PARA-SYUKBN
                       WHEN    1
                           MOVE    "key11s1"   TO  WRK-PATH
                       WHEN    2
                           MOVE    "key11m1"   TO  WRK-PATH
                       WHEN    3
                           MOVE    "key11k1"   TO  WRK-PATH
                       WHEN    4
                           MOVE    "key11i1"   TO  WRK-PATH
                   END-EVALUATE
           END-EVALUATE
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタを検索処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
           PERFORM UNTIL  FLG-OK   =   1
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    WRK-PATH        TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   IF      TNS-YAKKAKJNCD      NOT =   SPACE
                       IF      WRK-PARA-IPN    =   "1"
                           PERFORM 910-TENSUPLUS-READ-SEC
                           IF      TNSP-IPN-KISAIKBN   =   "1" OR  "3"
                               MOVE    1               TO  FLG-OK
                               PERFORM 910-GENERICNAME-READ-SEC
                               PERFORM 910-GENERICPRICE-READ-SEC
                               PERFORM 910-INPUTCD-READ-SEC
                           END-IF
                       ELSE
                           PERFORM 910-TENSUPLUS-READ-SEC
                           PERFORM 910-GENERICNAME-READ-SEC
                           PERFORM 910-GENERICPRICE-READ-SEC
                           PERFORM 910-INPUTCD-READ-SEC
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ読込
      *****************************************************************
       910-INPUTCD-READ-SEC        SECTION.
      *
           INITIALIZE                  ICD-REC
           MOVE    TNS-SRYCD       TO  ICD-SRYCD
           MOVE    WRK-PARA-HOSPNUM    TO  ICD-HOSPNUM
           MOVE    ICD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_inputcd"   TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  ICD-REC
                   MOVE    ZERO            TO  FLG-ICD
               END-IF
           ELSE
               MOVE    1                   TO  FLG-ICD
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-INPUTCD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加情報読込
      *****************************************************************
       910-TENSUPLUS-READ-SEC             SECTION.
      *
           INITIALIZE                   TENSUP-REC
           MOVE    WRK-PARA-HOSPNUM     TO  TNSP-HOSPNUM
           MOVE    TNS-SRYCD            TO  TNSP-SRYCD
           MOVE    TNS-YUKOSTYMD        TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD        TO  TNSP-YUKOEDYMD
      *
           MOVE    TENSUP-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_tensuplus"      TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_tensuplus"      TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
             IF       MCP-RC    =   ZERO
                   MOVE    ZERO                 TO  FLG-TENSUP
                   MOVE    MCPDATA-REC          TO  TENSUP-REC
               ELSE
                   MOVE    1                    TO  FLG-TENSUP
               END-IF
           ELSE
               MOVE    1                  TO  FLG-TENSUP
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_tensuplus"      TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    一般名情報読込
      *****************************************************************
       910-GENERICNAME-READ-SEC             SECTION.
      *
           INITIALIZE                   GENERICNAME-REC
      *     MOVE    TNS-YAKKAKJNCD(1:9)  TO  GENERIC-YAKKAKJNCD
           MOVE    TNS-YAKKAKJNCD       TO  GENERIC-YAKKAKJNCD
      *
           MOVE    GENERICNAME-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_genericname"    TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_genericname"    TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
             IF   MCP-RC    =   ZERO
               MOVE    ZERO                 TO  FLG-GENERIC
               MOVE    MCPDATA-REC          TO  GENERICNAME-REC
             ELSE
               MOVE    1                    TO  FLG-GENERIC
             END-IF
           ELSE
             MOVE    1                  TO  FLG-GENERIC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_genericname"    TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-GENERICNAME-READ-EXT.
           EXIT.
      *****************************************************************
      *    最低薬価情報読込
      *****************************************************************
       910-GENERICPRICE-READ-SEC             SECTION.
      *
           INITIALIZE                   GENERICPRICE-REC
           MOVE    GENERIC-YAKKAKJNCD   TO  GENERICPRICE-YAKKAKJNCD
           MOVE    TNS-YUKOEDYMD        TO  GENERICPRICE-YUKOSTYMD
           MOVE    TNS-YUKOSTYMD        TO  GENERICPRICE-YUKOEDYMD
      *
           MOVE    GENERICPRICE-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_generic_price"  TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_generic_price"  TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
             IF   MCP-RC    =   ZERO
               MOVE    ZERO                 TO  FLG-GENERICP
               MOVE    MCPDATA-REC          TO  GENERICPRICE-REC
             ELSE
               MOVE    1                    TO  FLG-GENERICP
             END-IF
           ELSE
             MOVE    1                  TO  FLG-GENERICP
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_generic_price"  TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-GENERICPRICE-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCLOSE-EXT.
           EXIT.
