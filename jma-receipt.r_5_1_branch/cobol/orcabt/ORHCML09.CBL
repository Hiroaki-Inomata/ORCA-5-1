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
       PROGRAM-ID.             ORHCML09.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 自院病名マスタ
      *  コンポーネント名  : 自院病名一覧（ＨＣＭＬ０９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/24    MCC-森脇      新規作成
      *****************************************************************
      *  00.00.01    NACL-森脇     順番並び替え(廃止)
      *  00.00.02    NACL-森脇     CSV出力対応
      *  04.02.01    NACL-森脇     08/03/04  疾患区分変更
      *  04.08.01    NACL-森脇     14/08/19  一時ファイルディレクトリ設定
      *  04.08.02    NACL-森脇     16/02/26  補足コメント対応
      *  05.00.00    NACL-太田     17/11/29  クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      *----(04.08.01)--UPD-START---
      *     SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *----(04.08.01)--UPD-END-----
      *    エラーファイル
           SELECT  ML09ERR-FILE    ASSIGN  ML09ERR
                                   FILE    STATUS  IS  STS-ML09ERR.
      *    CSV用データ
           SELECT  CSV-FILE        ASSIGN  USERCSV
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "CPPRTALL.INC".
      * 01  PRT-REC.
      *     03  PRT-PRTID               PIC X(30).
      *     03  PRT-PRTNM               PIC X(20).
      *     03  PRT-OFFSET-X            PIC X(05).
      *     03  PRT-OFFSET-Y            PIC X(05).
      *     03  PRT-SITEKBN             PIC X(01).
      *     03  PRT-PRTKBN              PIC X(01).
      *     03  PRT-PSFILE-AREA.
      *         05  PRT-OUT             PIC X(165).
      *         05  PRT-TBL-KEY         PIC X(08).
      *         05  PRT-RENNUM          PIC 9(04).
      *         05  PRT-TBL-GROUP       PIC X(14).
      *         05  PRT-SHORI-RENNUM    PIC 9(04).
      *         05  PRT-PAGE            PIC 9(05).
      *         05  PRT-UUID            PIC X(36).
      *     03  PRT-PRTDATA             PIC X(20000).
      *
      *    エラーファイル
       FD  ML09ERR-FILE.
       01  ML09ERR-REC             PIC X(200).
      *
      *----(04.08.02)--UPD-START---
      *    CSVファイル
       FD  CSV-FILE.
       01  CSV-REC.
           03  CSV-BYOMEISBT       PIC X(01).
           03  CSV-FILLER1         PIC X(01).
           03  CSV-BYOMEIINPUTCD   PIC X(20).
           03  CSV-FILLER2         PIC X(01).
           03  CSV-BUNRUICD        PIC X(03).
           03  CSV-FILLER3         PIC X(01).
           03  CSV-BUNRUIMEI       PIC X(20).
           03  CSV-FILLER4         PIC X(01).
           03  CSV-BYOMEI          PIC X(160).
           03  CSV-FILLER5         PIC X(01).
           03  CSV-HOSOKU-COMMENT  PIC X(80).
           03  CSV-FILLER6         PIC X(01).
           03  CSV-UTAGAIFLG       PIC X(01).
           03  CSV-FILLER7         PIC X(01).
           03  CSV-MANSEIKBN       PIC 9(02).
           03  CSV-FILLER8         PIC X(01).
           03  CSV-DSPSEQ          PIC 9(02).
      *----(04.08.02)--UPD-END-----
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "HCML09.INC".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *----(04.08.01)--ADD-START---
           COPY    "CPASGNPARA.INC".
      *----(04.08.01)--ADD-END-----
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //ML09ERR//.
       01  USERCSV.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  USERCSV-HOSPNUM     PIC 9(02).
           03  USERCSV-FILE-ID     PIC X(08).
           03  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-ML09ERR         PIC X(02).
           03  STS-CSV             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-BUNRUI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-BYOMEI          PIC 9(04).
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA. 
           03  WRK-PARA.
               05  WRK-PARA-TERMID     PIC X(64).
               05  WRK-PARA-SYSYMD     PIC X(8).
               05  WRK-PARA-HOSPNUM    PIC 9(2).
               05  WRK-PARA-SYUKBN     PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-STAFFCD    PIC X(05).
      *
           03  WRK-ML09ERR         PIC X(200).
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
      *
      *----(04.08.02)--ADD-START---
           03  WRK-BYOMEI          PIC X(100).
      *----(04.08.02)--ADD-END-----
           03  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
           03  WRK-ENCODING        PIC X(01).
           03  WRK-ENCODING9       REDEFINES WRK-ENCODING
                                   PIC 9(01).
      * 
       01  CONST-AREA.
           03  CONST-FORMID            PIC X(20)   VALUE  "HCML09.red".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
           COPY    "CPSK1044.INC".
      *
      *    自院病名マスタ
       01  USBYO-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
       01  USBUN-REC.
           COPY    "CPUSERBYOMEI.INC" 
                                   REPLACING  //USBYO//
                                   BY         //USBUN//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *----(04.08.01)--ADD-START---
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ******************************************************************
       PROCEDURE                   DIVISION
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
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
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
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE     SMCNDATE-TIME   TO  SYS-TIME
      *
           PERFORM 120-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                   INTO    WRK-PARA-TERMID
                                           WRK-PARA-SYSYMD
                                           WRK-PARA-HOSPNUM
                                           WRK-PARA-SYUKBN
                                           WRK-PARA-JOBID
                                           WRK-PARA-SHELLID
                                           WRK-PARA-STAFFCD
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    WRK-PARA-STAFFCD    TO  SPA-STAFFCD
           MOVE    WRK-PARA-SYSYMD     TO  SPA-SYSYMD
           MOVE    MCP-USER            TO  SPA-OPID
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           CALL    "ORCSENCODING"          USING
                                           MCPAREA
                                           SPA-AREA
           MOVE    SPA-ENCODING    TO      WRK-ENCODING9
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    "ORHCML09"          TO  JOB-PGID
           MOVE    "自院病名一覧"      TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "ML09ERR"           TO  ML09ERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  ML09ERR-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  ML09ERR-HOSPNUM
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    ML09ERR-BASENAME    TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ML09ERR
      *----(04.08.01)--ADD-END-----

           MOVE    "userbyol"          TO  USERCSV-FILE-ID
           MOVE    SPA-HOSPNUM         TO  USERCSV-HOSPNUM
      *
           INITIALIZE              UIDIO-AREA.
           CALL    "orcuidset"     USING
                                   UIDIO-ST
                                   UIDIO-AREA
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
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    WRK-PARA-SYSYMD     TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"             USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           PERFORM 800-OFFSET-GET-SEC
      *
           .
      *
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      WRK-PARA-SYUKBN =   "1"
               PERFORM 300-CSVSET-SEC
           ELSE
               PERFORM 300-INSATSU-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVセット処理
      *****************************************************************
       300-CSVSET-SEC              SECTION.
      *
      *    CSV用ファイルをクリアする
           OPEN    OUTPUT              CSV-FILE
      *
      *    自院病名セット
           PERFORM 900-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-BYOMEI  =   1
               INITIALIZE                      CSV-REC
               MOVE    USBYO-BYOMEISBT     TO  CSV-BYOMEISBT
               MOVE    USBYO-BYOMEIINPUTCD TO  CSV-BYOMEIINPUTCD
               MOVE    USBYO-BUNRUICD      TO  CSV-BUNRUICD
               MOVE    USBYO-BYOMEI        TO  CSV-BYOMEI
               MOVE    USBYO-UTAGAIFLG     TO  CSV-UTAGAIFLG
               MOVE    USBYO-MANSEIKBN     TO  CSV-MANSEIKBN
               MOVE    USBYO-DSPSEQ        TO  CSV-DSPSEQ
               MOVE    ","                 TO  CSV-FILLER1
               MOVE    ","                 TO  CSV-FILLER2
               MOVE    ","                 TO  CSV-FILLER3
               MOVE    ","                 TO  CSV-FILLER4
               MOVE    ","                 TO  CSV-FILLER5
               MOVE    ","                 TO  CSV-FILLER6
               MOVE    ","                 TO  CSV-FILLER7
      *----(04.08.02)--ADD-START---
               MOVE    ","                 TO  CSV-FILLER8
               MOVE    USBYO-HOSOKU-COMMENT
                                           TO  CSV-HOSOKU-COMMENT
      *----(04.08.02)--ADD-END-----
               IF      USBYO-BUNRUICD  NOT  =  SPACE
                   PERFORM 900-DBREAD-SEC
                   MOVE    USBUN-BYOMEI    TO  CSV-BUNRUIMEI
               END-IF
               WRITE                           CSV-REC
               PERFORM 900-DBFETCH-SEC
           END-PERFORM
      *
           PERFORM 900-DBCLOSE-SEC
      *
      *    CSV用ファイルを閉じる
           CLOSE                       CSV-FILE
           .
       300-CSVSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票セット処理
      *****************************************************************
       300-INSATSU-SEC             SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM TO HC01PARA-HOSPNUM
           MOVE    "HCML"           TO  HC01PARA-FORM-ID
           MOVE    WRK-PARA-TERMID
                                   TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    1               TO  HC01PARA-CNT
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGNPARA
           MOVE    SGETTEMP-ST     TO  ASGNPARA-ST
           MOVE    SGETTEMP-TEMPDIR TO WRK-SGETTEMP-TEMPDIR
      *
           OPEN    OUTPUT              PRT-FILE
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
      *    自院病名セット
           PERFORM 900-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-BYOMEI  =   1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           PERFORM 900-DBCLOSE-SEC
      *
           CLOSE   PRT-FILE
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "HCALL_prv.sh"  TO  SHELLTBL-NAME
           MOVE    ASGNPARA (ASGNPARA-ST:)
                                   TO  SHELLTBL-ARG1
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    WRK-ENCODING    TO  SHELLTBL-ARG3
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
              MOVE    "自院病名一覧"
                                   TO  SHELLTBL-ARG5
              MOVE    SPA-CLIENT-SHOWDIALOG
                                   TO  SHELLTBL-ARG6
           END-IF
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "daily"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       300-INSATSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
      *
           INITIALIZE                  HCML09
           MOVE    SPACE           TO  HCML09
      *
           MOVE    WRK-PARA-SYSYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCML09-SAKUSEIYMD
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCML09-PAGE
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
           MOVE    ZERO            TO  CNT-LINE
      *
           PERFORM UNTIL   FLG-BYOMEI  =   1
                   OR      CNT-LINE    =   28
      *
               ADD     1               TO  CNT-LINE
               MOVE    USBYO-BYOMEIINPUTCD
                               TO  HCML09-BYOMEIINPUTCD(CNT-LINE)
      *----(04.08.02)--UPD-START---
               MOVE    SPACE           TO  WRK-BYOMEI
               IF      USBYO-HOSOKU-COMMENT    =   SPACE
                   MOVE    USBYO-BYOMEI    TO  WRK-BYOMEI
               ELSE
                   STRING  USBYO-BYOMEI            DELIMITED BY SPACE
                           "（"                    DELIMITED BY SIZE
                           USBYO-HOSOKU-COMMENT    DELIMITED BY SPACE
                           "）"                    DELIMITED BY SIZE
                                           INTO    WRK-BYOMEI
                   END-STRING
                   MOVE    "*"             TO  WRK-BYOMEI(100:1)
               END-IF
               MOVE    WRK-BYOMEI      TO  HCML09-BYOMEI(CNT-LINE)
      *----(04.08.02)--UPD-END-----
      *
               EVALUATE    USBYO-MANSEIKBN
                   WHEN    "00"
                       MOVE    SPACE     TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "03"
                       MOVE   "皮膚１"   TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "04"
                       MOVE   "皮膚２"   TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "05"
                       MOVE   "特定疾患" TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "07"
                       MOVE   "てんかん"  TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "08"
                       MOVE   "特疾てん"  TO  HCML09-MANSEIKBN(CNT-LINE)
                   WHEN    "09"
                       MOVE   "難病外来"  TO  HCML09-MANSEIKBN(CNT-LINE)
               END-EVALUATE
      *
               EVALUATE    USBYO-UTAGAIFLG
                   WHEN    "0"
                       MOVE    SPACE     TO  HCML09-UTAGAIFLG(CNT-LINE)
                   WHEN    "1"
                       MOVE   "疑い"     TO  HCML09-UTAGAIFLG(CNT-LINE)
                   WHEN    "2"
                       MOVE   "急性"     TO  HCML09-UTAGAIFLG(CNT-LINE)
                   WHEN    "3"
                       MOVE   "急疑"     TO  HCML09-UTAGAIFLG(CNT-LINE)
               END-EVALUATE
      *
              IF   USBYO-BUNRUICD  NOT  =  SPACE
                   PERFORM 900-DBREAD-SEC
                   MOVE    USBUN-BYOMEI TO  HCML09-BUNRUIMEI(CNT-LINE)
               END-IF
      *
               PERFORM 900-DBFETCH-SEC
      *
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
           IF      CNT-LINE        =   0
               GO  TO  390-PRINT-OUT-EXT
           END-IF
      *
           MOVE    SPACE               TO  PRT-REC 
           MOVE    CONST-FORMID        TO  PRT-PRTID
           MOVE    SYS-1031-PRTNM(6)   TO  PRT-PRTNM
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
           MOVE    "1"                 TO  PRT-SITEKBN
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
               MOVE    "5"             TO  PRT-PRTKBN
           ELSE
               MOVE    "1"             TO  PRT-PRTKBN
           END-IF
           MOVE    WRK-SGETTEMP-TEMPDIR TO PRT-OUT
           MOVE    "HCML09"            TO PRT-TBL-KEY
           MOVE    1                   TO  PRT-RENNUM
           MOVE    SMCNDATE-YMD        TO  PRT-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS        TO  PRT-TBL-GROUP (9:6)
           MOVE    1                   TO  PRT-SHORI-RENNUM
           MOVE    CNT-PAGE            TO  PRT-PAGE  
           MOVE    UIDIO-UID           TO  PRT-UUID
           MOVE    HCML09              TO  PRT-PRTDATA
           WRITE   PRT-REC
      *
           .
      *
       390-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   ML09ERR-FILE
      *     
           IF      STS-ML09ERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              ML09ERR-FILE
      *
               MOVE    WRK-ML09ERR         TO  ML09ERR-REC
               WRITE   ML09ERR-REC
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    "エラー"            TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"               USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           CLOSE  ML09ERR-FILE
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
           DISPLAY "*** ORHCML09 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           IF      WRK-PARA-SYUKBN     =   "1"
               MOVE    1               TO  JOB-UPDCNT
           ELSE
               MOVE    CNT-PAGE        TO  JOB-UPDCNT
           END-IF
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
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
      *    オフセット取得処理
      *****************************************************************
       800-OFFSET-GET-SEC              SECTION.
      *
           INITIALIZE                  SYS-1044-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1044-HOSPNUM
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    CONST-FORMID        TO  SYS-1044-FORMID
           STRING  SYS-1031-PRTNM(6)   DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
           INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               CALL    "ORCDBMAIN"             USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF    ( MCP-RC              =   ZERO )
                   MOVE  MCPDATA-REC       TO  SYS-1044-REC
               ELSE
                   INITIALIZE                  SYS-1044-REC
               END-IF
           ELSE
                   INITIALIZE                  SYS-1044-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF   (  SYS-1044-FORMID  =   CONST-FORMID  )
            AND (  SYS-1044-PRTNM   =   SYS-1031-PRTNM(6) )
               CONTINUE
           ELSE
               INITIALIZE  SYS-1044-REC
           END-IF
      *
           .
       800-OFFSET-GET-EXT.
            EXIT.
      *****************************************************************
      *    自院病名マスタ　検索 
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           INITIALIZE                  USBYO-REC
           MOVE    WRK-PARA-HOSPNUM    TO  USBYO-HOSPNUM
           MOVE    USBYO-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     自院病名マスタを検索処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-BYOMEI
               MOVE    MCPDATA-REC     TO  USBYO-REC
               ADD     1               TO  CNT-BYOMEI
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF  
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ　検索 
      *****************************************************************
       900-DBREAD-SEC              SECTION.
      *
           INITIALIZE                      USBUN-REC
           MOVE    "C"                 TO  USBUN-CDSYU
           MOVE    USBYO-BUNRUICD      TO  USBUN-BYOMEIINPUTCD
      *
           MOVE    WRK-PARA-HOSPNUM    TO  USBUN-HOSPNUM
           MOVE    USBUN-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-BUNRUI
                   MOVE    MCPDATA-REC     TO  USBUN-REC
               ELSE
                   MOVE    1               TO  FLG-BUNRUI
               END-IF  
           ELSE
               MOVE    1                   TO  FLG-BUNRUI
           END-IF        
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBREAD-EXT.
           EXIT.
      *
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
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
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
       120-DBOPEN-SEC              SECTION.
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
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
