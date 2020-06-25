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
       PROGRAM-ID.             ORCBN22.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬情マスタ登録漏れ一覧印刷
      *  コンポーネント名  : 薬情マスタ登録漏れ一覧印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/07/04    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.08.01    NACL-森脇     14/08/21  一時ファイルディレクトリ設定
      *  05.00.00    NACL-森脇     17/02/22  自費薬剤対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    サブプロファイル
           SELECT  TNSCHK-FILE     ASSIGN  TNSCHK
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TNSCHK-KEY
                                   FILE    STATUS  IS  STS-TNSCHK.
      *    並び替え用ファイル
           SELECT  TNSCHK2-FILE    ASSIGN  TNSCHK2
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TNSCHK2-KEY
                                   FILE    STATUS  IS  STS-TNSCHK2.
      *    エラーファイル
           SELECT  YKJMERR-FILE    ASSIGN  YKJMERR
                                   FILE    STATUS  IS  STS-YKJMERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    サブプロファイル
       FD  TNSCHK-FILE.
           COPY    "CPTENSU.INC"       REPLACING  //TNS//
                                       BY         //TNSCHK//.
      *    並び替え用ファイル
       FD  TNSCHK2-FILE.
       01  TNSCHK2-REC.
           03  TNSCHK2-KEY.
               05  TNSCHK2-JYUNBAN    PIC X(209).
           03  TNSCHK2-SRYCD          PIC X(09).
           03  TNSCHK2-NAME           PIC X(200).
      *
      *    エラーファイル
       FD  YKJMERR-FILE.
       01  YKJMERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *
           COPY    "HCM47.INC".
           COPY    "HCMSL80.INC".
      *
      *----(04.08.01)--ADD-START---
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //YKJMERR//.
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //TNSCHK//.
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //TNSCHK2//.
      *----(04.08.01)--ADD-END-----
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-YKJMERR             PIC X(02).
           03  STS-TNSCHK              PIC X(02).
           03  STS-TNSCHK2             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OK                  PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-YAKUJYO             PIC 9(01).
           03  FLG-TNSCHK              PIC 9(01).
           03  FLG-TNSCHK2             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(04).
           03  CNT-NUM                 PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-CHK            PIC X(01).
           03  WRK-PARA-KIKAN          PIC X(01).
           03  WRK-PARA-CYU            PIC X(01).
           03  WRK-PARA-PRT            PIC X(01).
           03  WRK-PARA-SYU            PIC X(01).
      *----(04.08.01)--UPD-START---
      *     03  YKJMERR                 PIC X(100).
      *     03  TNSCHK                  PIC X(200).
      *     03  TNSCHK2                 PIC X(200).
      *----(04.08.01)--UPD-END-----
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YKJMERR             PIC X(200).
           03  WRK-TABLE               PIC X(20).
           03  WRK-PATHNAME            PIC X(20).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-ZZZ9                PIC ZZZ9.
           03  WRK-TITLE               PIC X(28).
           03  WRK-PRTYMD              PIC X(22).
           03  WRK-KIKAN               PIC X(20).
           03  WRK-YKZNAME-G.
               05  WRK-YKZNAME         PIC X(26)   OCCURS  3.
      *
      *    並び替え用キー
           03  WRK-KEY1-G.
               05  WRK-KEY1-SRYCD      PIC X(09).
               05  WRK-KEY1-KANANAME   PIC X(200).
           03  WRK-KEY2-G.
               05  WRK-KEY2-KANANAME   PIC X(200).
               05  WRK-KEY2-SRYCD      PIC X(09).
      *
      *    印刷用レイアウト
       01  WRK-PRT-AREA.
           03  WRK-PRT-LINE1.
               05  WRK-PRT-TITLE       PIC X(28)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "　".
               05  WRK-PRT-KIKAN       PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(18)   VALUE
                                           "　　　　　　　　　".
               05  FILLER              PIC X(08)   VALUE   "発行日：".
               05  WRK-PRT-PRTYMD      PIC X(22)   VALUE   SPACE.
               05  FILLER              PIC X(05)   VALUE   "　　P".
               05  WRK-PRT-PAGE        PIC X(04)   VALUE   SPACE.
           03  WRK-PRT-LINE2.
               05  FILLER              PIC X(18)   VALUE
                                           "診療コード　薬剤名".
           03  WRK-PRT-LINE3.
               05  WRK-PRT-SRYCD       PIC X(09)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  WRK-PRT-NAME        PIC X(96)   VALUE   SPACE.
      *
      *    ファイル名
       01  FILE-NAME                   PIC X(512).
      *    空白の画像
       01  KUHAKU-NAME.
           03  FILLER                  PIC X(05)   VALUE   "data/".
           03  FILLER                  PIC X(15)   VALUE
                                                   "transparent.jpg".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    薬情マスタ
           COPY    "CPYAKUJYO.INC".
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
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *    点数チェックサブプロ
           COPY    "CPORCSS004.INC".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
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
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
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
                                       WRK-PARA-CHK
                                       WRK-PARA-KIKAN
                                       WRK-PARA-CYU
                                       WRK-PARA-PRT
                                       WRK-PARA-SYU
                                       YKJMERR
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBN22"           TO  JOB-PGID
      *
           EVALUATE    WRK-PARA-CHK
           WHEN    "1"
               MOVE    "薬剤情報マスタ登録漏れリスト"
                                       TO  JOB-SHELLMSG
           WHEN    "2"
               MOVE    "薬剤情報マスタ画像なしリスト"
                                       TO  JOB-SHELLMSG
           END-EVALUATE
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    YKJMERR-BASENAME    TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  YKJMERR
      *----(04.08.01)--ADD-END-----
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-PRTYMD
      *
           EVALUATE    WRK-PARA-CHK
           WHEN    "1"
               PERFORM 201-MAIN-TENSU-SEC
           WHEN    "2"
               PERFORM 202-MAIN-YAKUJYO-SEC
           END-EVALUATE
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録なし処理
      *****************************************************************
       201-MAIN-TENSU-SEC          SECTION.
      *
           MOVE    "薬剤情報マスタ登録漏れリスト"
                                   TO  WRK-TITLE
           EVALUATE    WRK-PARA-KIKAN
           WHEN    "0"
               MOVE    "診療年月指定なし　　"  TO  WRK-KIKAN
           WHEN    "2"
               MOVE    "診療年月指定２ヶ月分"  TO  WRK-KIKAN
           WHEN    "6"
               MOVE    "診療年月指定６ヶ月分"  TO  WRK-KIKAN
           WHEN    OTHER
               MOVE    "　　　　　　　　　　"  TO  WRK-KIKAN
           END-EVALUATE
      *
      *    ファイルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    KUHAKU-NAME     TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  FILE-NAME
      *
      *----(05.00.00)--UPD-START---
      *    点数チェックサブプロ
           INITIALIZE                  SS004-AREA
           MOVE    WRK-PARA-KIKAN  TO  SS004-KIKAN
      *     MOVE    "1"             TO  SS004-SRYCDKBN
           MOVE    "0"             TO  SS004-SRYCDKBN
           CALL    "ORCSS004"          USING
                                       SS004-AREA
                                       SPA-AREA
      *----(05.00.00)--UPD-END-----
      *    中間ファイル名セット
           MOVE    SS004-FILENAME  TO  TNSCHK
           STRING  SS004-FILENAME      DELIMITED  BY  SPACE
                   "2"                 DELIMITED  BY  SIZE
                              INTO     TNSCHK2
           END-STRING
           DISPLAY "SS004-FILENAME=" SS004-FILENAME
      *
           OPEN    OUTPUT              TNSCHK2-FILE
           CLOSE                       TNSCHK2-FILE
      *
           OPEN    INPUT               TNSCHK-FILE
           OPEN    I-O                 TNSCHK2-FILE
      *
           PERFORM 900-TNSCHK-READ-SEC
      *
           CLOSE                       TNSCHK-FILE
           CLOSE                       TNSCHK2-FILE
      *
           OPEN    INPUT               TNSCHK2-FILE
           PERFORM 900-TNSCHK2-NEXT-SEC
      *
           PERFORM UNTIL   FLG-TNSCHK2     =   1
               PERFORM 210-PRT-HEN-SEC
               PERFORM 220-PRT-SET-SEC
               PERFORM 230-PRT-PRINT-SEC
           END-PERFORM
      *
           CLOSE                       TNSCHK2-FILE
      *
           OPEN    OUTPUT              TNSCHK-FILE
           CLOSE                       TNSCHK-FILE
           OPEN    OUTPUT              TNSCHK2-FILE
           CLOSE                       TNSCHK2-FILE
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TNSCHK          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TNSCHK2         TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           .
       201-MAIN-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像なし処理
      *****************************************************************
       202-MAIN-YAKUJYO-SEC        SECTION.
      *
           MOVE    "薬剤情報マスタ画像なしリスト"
                                   TO  WRK-TITLE
           MOVE    "　　　　　　　　　　"
                                   TO  WRK-KIKAN
      *
           PERFORM 900-YAKUJYO-SELECT-SEC
      *
           PERFORM UNTIL   FLG-YAKUJYO     =   1
               PERFORM 210-PRT-HEN-SEC
               PERFORM 220-PRT-SET-SEC
               PERFORM 230-PRT-PRINT-SEC
           END-PERFORM
      *
           PERFORM 900-YAKUJYO-CLOSE-SEC
      *
           .
       202-MAIN-YAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票明細処理
      *****************************************************************
       210-PRT-HEN-SEC             SECTION.
      *
           IF      WRK-PARA-PRT    =   "1"
      *        薬剤情報マスタ形式
               INITIALIZE                  HCM47
               MOVE    WRK-TITLE       TO  HCM47-TITLE
               MOVE    WRK-PRTYMD      TO  HCM47-SYORIBI
               MOVE    WRK-KIKAN       TO  HCM47-KIKAN
               COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
               MOVE    CNT-PAGE        TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  HCM47-PAGE
               MOVE    FILE-NAME       TO  HCM47-GAZO(1)
                                           HCM47-GAZO(2)
                                           HCM47-GAZO(3)
                                           HCM47-GAZO(4)
                                           HCM47-GAZO(5)
           ELSE
      *        通常形式
               INITIALIZE                  HCMSL80
               MOVE    WRK-TITLE       TO  WRK-PRT-TITLE
               MOVE    WRK-PRTYMD      TO  WRK-PRT-PRTYMD
               MOVE    WRK-KIKAN       TO  WRK-PRT-KIKAN
               COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
               MOVE    CNT-PAGE        TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  WRK-PRT-PAGE
               MOVE    WRK-PRT-LINE1   TO  HCMSL80-MOJI(1)
               MOVE    WRK-PRT-LINE2   TO  HCMSL80-MOJI(3)
           END-IF
      *
           .
       210-HEN-BODY-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       220-PRT-SET-SEC             SECTION.
      *
           IF      WRK-PARA-PRT    =   "1"
      *        薬剤情報マスタ形式
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   FLG-TNSCHK2     =   1
                       OR      IDX     >       5
      *            番号セット
                   COMPUTE CNT-NUM     =   CNT-NUM     +   1
                   MOVE    CNT-NUM         TO  WRK-ZZZ9
                   MOVE    WRK-ZZZ9        TO  HCM47-NUM(IDX)
      *            診療行為コードセット
                   STRING  "("             DELIMITED  BY  SIZE
                           TNSCHK2-SRYCD   DELIMITED  BY  SPACE
                           ")"             DELIMITED  BY  SIZE
                           INTO     HCM47-SRYCD(IDX)
                   END-STRING
      *            薬剤名セット
                   MOVE    TNSCHK2-NAME    TO  WRK-YKZNAME-G
                   MOVE    WRK-YKZNAME(1)  TO  HCM47-YKZNAME(IDX 1)
                   MOVE    WRK-YKZNAME(2)  TO  HCM47-YKZNAME(IDX 2)
                   MOVE    WRK-YKZNAME(3)  TO  HCM47-YKZNAME(IDX 3)
      *            点数チェック読み込み
                   PERFORM 900-TNSCHK2-NEXT-SEC
               END-PERFORM
           ELSE
      *        通常形式
               EVALUATE    WRK-PARA-CHK
               WHEN    "1"
                   PERFORM VARYING IDX     FROM    4   BY  1
                           UNTIL   FLG-TNSCHK2     =   1
                           OR      IDX     >       80
                       MOVE    SPACE           TO  WRK-PRT-LINE3
                       MOVE    TNSCHK2-SRYCD   TO  WRK-PRT-SRYCD
                       MOVE    TNSCHK2-NAME    TO  WRK-PRT-NAME
                       MOVE    WRK-PRT-LINE3   TO  HCMSL80-MOJI(IDX)
                       PERFORM 900-TNSCHK2-NEXT-SEC
                   END-PERFORM
               WHEN    "2"
                   PERFORM VARYING IDX     FROM    4   BY  1
                           UNTIL   FLG-YAKUJYO     =   1
                           OR      IDX     >       80
                       MOVE    SPACE           TO  WRK-PRT-LINE3
                       MOVE    YAKUJYO-SRYCD   TO  WRK-PRT-SRYCD
                       MOVE    YAKUJYO-NAME    TO  WRK-PRT-NAME
                       MOVE    WRK-PRT-LINE3   TO  HCMSL80-MOJI(IDX)
                       PERFORM 900-YAKUJYO-FETCH-SEC
                   END-PERFORM
               END-EVALUATE
           END-IF
      *
           .
       220-PRT-BODY-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       230-PRT-PRINT-SEC           SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           IF      WRK-PARA-PRT        =   "1"
      *        薬剤情報マスタ形式
               MOVE    "HCM47.red"     TO  SPRT-PRTID
               MOVE    HCM47           TO  SPRT-PRTDATA
           ELSE
      *        通常形式
               MOVE    "HCMSL80.red"   TO  SPRT-PRTID
               MOVE    HCMSL80         TO  SPRT-PRTDATA
           END-IF
      *
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
           MOVE    WRK-TITLE           TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-YKJMERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       230-PRT-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT           YKJMERR-FILE
           IF        ( STS-YKJMERR =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT      YKJMERR-FILE
      *
               MOVE    WRK-YKJMERR     TO  YKJMERR-REC
               WRITE   YKJMERR-REC
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-YKJMERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           CLOSE   YKJMERR-FILE
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
           INSPECT WRK-HENYMDG     REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数チェックファイル順読込
      *****************************************************************
       900-TNSCHK-READ-SEC         SECTION.
      *
           INITIALIZE                  TNSCHK-TENSU-REC
           MOVE    ZERO            TO  FLG-TNSCHK
      *
           PERFORM UNTIL    FLG-TNSCHK =   1
               PERFORM 900-TNSCHK-NEXT-SEC
           END-PERFORM
      *
           .
       900-TNSCHK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数チェックファイル順読込
      *****************************************************************
       900-TNSCHK-NEXT-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM UNTIL    FLG-OK     =   1
               READ    TNSCHK-FILE  NEXT
               AT  END
                   MOVE    1           TO  FLG-TNSCHK
                   MOVE    1           TO  FLG-OK
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TNSCHK
      *----(05.00.00)--ADD-START---
                   EVALUATE    TNSCHK-SRYCD
                   WHEN    "600000000" THRU    "699999999"
                   WHEN    "095210001" THRU    "095279999"
                   WHEN    "096210001" THRU    "096279999"
      *----(05.00.00)--ADD-END-----
      *            薬剤区分をチェック
                   IF        ( TNSCHK-YKZKBN      =   "1" )
                       OR    ( TNSCHK-YKZKBN      =   "6" )
                       OR   (( WRK-PARA-CYU       =   "1" )
                       AND   ( TNSCHK-YKZKBN      =   "4" ))
      *                薬剤情報の存在をチェック
                       PERFORM 900-YAKUJYO-READ-SEC
                       IF      FLG-YAKUJYO     =   1
                           MOVE    1           TO  FLG-OK
                           PERFORM 900-TNSCHK2-READ-SEC
                       ELSE
                           MOVE    ZERO        TO  FLG-OK
                       END-IF
                   ELSE
                       MOVE    ZERO        TO  FLG-OK
                   END-IF
      *----(05.00.00)--ADD-START---
                   WHEN    OTHER
                       MOVE    ZERO        TO  FLG-OK
                   END-EVALUATE
      *----(05.00.00)--ADD-END-----
               END-READ
           END-PERFORM
           .
       900-TENSU-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数チェックファイル順読込
      *****************************************************************
       900-TNSCHK2-READ-SEC        SECTION.
      *
           INITIALIZE                  TNSCHK2-REC
      *
           EVALUATE    WRK-PARA-SYU
           WHEN    "1"
               MOVE    TNSCHK-SRYCD    TO  WRK-KEY1-SRYCD
               MOVE    TNSCHK-KANANAME TO  WRK-KEY1-KANANAME
               MOVE    WRK-KEY1-G      TO  TNSCHK2-JYUNBAN
           WHEN    "2"
               MOVE    TNSCHK-SRYCD    TO  WRK-KEY2-SRYCD
               MOVE    TNSCHK-KANANAME TO  WRK-KEY2-KANANAME
               MOVE    WRK-KEY2-G      TO  TNSCHK2-JYUNBAN
           END-EVALUATE
      *
           READ    TNSCHK2-FILE
               INVALID
                   MOVE    1           TO  FLG-TNSCHK2
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TNSCHK2
           END-READ
      *
           IF      FLG-TNSCHK2     =   ZERO
               GO  TO  900-TNSCHK2-READ-EXT
           END-IF
      *
           INITIALIZE                  TNSCHK2-REC
           MOVE    TNSCHK-SRYCD    TO  TNSCHK2-SRYCD
           MOVE    TNSCHK-NAME     TO  TNSCHK2-NAME
      *
           EVALUATE    WRK-PARA-SYU
           WHEN    "1"
               MOVE    WRK-KEY1-G      TO  TNSCHK2-JYUNBAN
           WHEN    "2"
               MOVE    WRK-KEY2-G      TO  TNSCHK2-JYUNBAN
           END-EVALUATE
      *
           WRITE   TNSCHK2-REC
      *
           .
       900-TNSCHK2-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数チェックファイル順読込
      *****************************************************************
       900-TNSCHK2-NEXT-SEC        SECTION.
      *
           READ    TNSCHK2-FILE  NEXT
               AT  END
                   MOVE    1           TO  FLG-TNSCHK2
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TNSCHK2
           END-READ
      *
           .
       900-TNSCHK2-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報マスタ読み込み
      *****************************************************************
       900-YAKUJYO-READ-SEC        SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
           MOVE    SPA-HOSPNUM     TO  YAKUJYO-HOSPNUM
           MOVE    TNSCHK-SRYCD    TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_yakujyo"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF  MCP-RC  =  ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
               ELSE
                   MOVE    1               TO  FLG-YAKUJYO
                   INITIALIZE                  YAKUJYO-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-YAKUJYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    YAKUJYO-SRYCD   TO  TNS-SRYCD
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF  MCP-RC  =  ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報読み込み処理
      *****************************************************************
       900-YAKUJYO-SELECT-SEC      SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
      *
      *----(05.00.00)--UPD-START---
           MOVE    "tbl_yakujyo"   TO  WRK-TABLE
           EVALUATE    WRK-PARA-SYU
               WHEN    "1"
      *            MOVE    "key5s"     TO  WRK-PATHNAME
                   MOVE    "key5js"    TO  WRK-PATHNAME
               WHEN    "2"
      *            MOVE    "key5k"     TO  WRK-PATHNAME
                   MOVE    "key5jk"    TO  WRK-PATHNAME
           END-EVALUATE
      *----(05.00.00)--UPD-END-----
      *
           MOVE    SPA-HOSPNUM     TO  YAKUJYO-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  YAKUJYO-CREYMD
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-YAKUJYO-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           .
       900-YAKUJYO-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報読み込み処理
      *****************************************************************
       900-YAKUJYO-FETCH-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM UNTIL   FLG-OK  NOT =   ZERO
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
      *            画像のない薬剤を対象とする
                   IF      YAKUJYO-PHOTO   =   SPACE
                       MOVE    1               TO  FLG-OK
                       PERFORM 900-TENSU-READ-SEC
                       MOVE    TNS-NAME        TO  YAKUJYO-NAME
                   ELSE
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-YAKUJYO
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           .
      *
       900-YAKUJYO-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報クロース処理
      *****************************************************************
       900-YAKUJYO-CLOSE-SEC       SECTION.
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-YAKUJYO-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
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
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
