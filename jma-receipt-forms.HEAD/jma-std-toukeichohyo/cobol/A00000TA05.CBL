      ******************************************************************
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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 A00000TA05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 指定病名・診療行為患者一覧
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付        内容
      *   01.00.00    田中    2011/03/07  新規作成
      *   01.00.01    門間    2014/03/12  テスト患者を対象外とする
      *                                   処理名を修正、不要なコメントを削除
      *   02.00.01    門間    2014/10/31   ver4.8.0対応（一時ディレクトリ変更）
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    TMPファイル
           SELECT  TA05-FILE     ASSIGN          TA05P
                                    ORGANIZATION    IS  INDEXED
                                    ACCESS  MODE    IS  DYNAMIC
                                    RECORD  KEY     IS  TA05TMP-KEY
                                    FILE    STATUS  IS  STS-PARA.
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
       01  RECEERR-REC                 PIC X(200).
      *
      *    患者一時ファイル
       FD  TA05-FILE.
           COPY    "A00000TA05TMP.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000TA05.INC".
      *
      *    一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //TA05P//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR              PIC X(02).
           03  STS-PARA                 PIC X(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID           PIC 9(07).
           03  WRK-PARA-SHELLID         PIC X(08).
           03  WRK-PARA-HOSPNUM         PIC 9(02).
           03  WRK-PARA-SRYSTYMD        PIC X(08).
           03  WRK-PARA-SRYEDYMD        PIC X(08).
           03  WRK-PARA-NYUGAIKBN       PIC X(01).
           03  WRK-PARA-TENKIKBN        PIC X(01).
           03  WRK-PARA-SRYCD           PIC X(09)  OCCURS 6.
           03  WRK-PARA-KIKANFLG        PIC X(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYACT               PIC 9(01).
           03  FLG-SRYACCT              PIC 9(01).
           03  FLG-PTBYOMEI             PIC 9(01).
           03  FLG-TA05TMP              PIC 9(01).
           03  FLG-PRINT                PIC 9(01).
           03  FLG-END                  PIC 9(01).
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-CSV-HEADER           PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                 PIC 9(03).
           03  CNT-PT                   PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                     PIC 9(01).
           03  IDX2                     PIC 9(02).
           03  IDX-PARA                 PIC 9(01).
           03  IDX-PARAMAX              PIC 9(01).
           03  IDX-BYO                  PIC 9(01).
           03  IDX-SRY                  PIC 9(01).
           03  IDX-DAY                  PIC S9(02).
           03  IDX-STDAY                PIC 9(02).
           03  IDX-EDDAY                PIC 9(02).
           03  IDX-POINT                PIC 9(05).
           03  IDX-LINE                 PIC 9(02).
           03  IDX-LINE2                PIC 9(02).
           03  IDX-ST                   PIC 9(03).
           03  IDX-ED                   PIC 9(03).
           03  IDX-LEN                  PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR              PIC X(200).
           03  WRK-Z2-9                 PIC ZZ9.
           03  WRK-SRYSTYMD             PIC X(22).
           03  WRK-SRYEDYMD             PIC X(22).
           03  WRK-SYSYMD               PIC X(22).
           03  WRK-SYMD.
             05  WRK-SYY                PIC 9(04).
             05  WRK-SMM                PIC 9(02).
             05  WRK-SDD                PIC 9(02).
           03  WRK-BIRTHDAY.
             05  WRK-BIRTHY             PIC 9(04).
             05  WRK-BIRTHM             PIC 9(02).
             05  WRK-BIRTHD             PIC 9(02).
           03  WRK-EDTYMD1              PIC X(10).
           03  WRK-EDTYMD3              PIC X(22).
           03  WRK-BYOMEICD             PIC X(07).
           03  WRK-SRYCD                PIC X(09).
           03  WRK-NENREI               PIC 9(03).
           03  WRK-NAIYO-TBL            OCCURS 6.
             05  WRK-BYOMEI             PIC X(44).
             05  WRK-BYOSTYMD           PIC X(22).
             05  WRK-TENKI              PIC X(04).
             05  WRK-TENKIYMD           PIC X(22).
             05  WRK-SRYNAME            PIC X(44).
             05  WRK-LASTYMD            PIC X(22).
           03  WRK-AMARI                PIC 9(02).
           03  WRK-GPAGE                PIC 9(03).
           03  WRK-COMP                 PIC 9(03).
           03  WRK-HEAD-SRYNAME         OCCURS 6  PIC X(44).
      *
      *    CSV領域
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE            PIC 9(03)  VALUE 160.
           03  WRK-ITEM                 PIC X(160)  VALUE SPACE.
           03  WRK-CSVDATA              PIC X(20000) VALUE SPACE.
      *
      *    CSVヘッダ領域
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-RENNUM      PIC X(12)  VALUE "連番".
               07  CSV-HEAD-PTNUM       PIC X(12)  VALUE "患者番号".
               07  CSV-HEAD-KANANAME    PIC X(12)  VALUE "カナ氏名".
               07  CSV-HEAD-NAME        PIC X(12)  VALUE "氏名".
               07  CSV-HEAD-SEX         PIC X(12)  VALUE "性別".
               07  CSV-HEAD-AGE         PIC X(12)  VALUE "年齢".
               07  CSV-HEAD-BYOMEI      PIC X(12)  VALUE "病名".
               07  CSV-HEAD-BYOSTYMD    PIC X(12)  VALUE "病名開始日".
               07  CSV-HEAD-TENKI       PIC X(12)  VALUE "転帰".
               07  CSV-HEAD-TENKIYMD    PIC X(12)  VALUE "転帰日".
               07  CSV-HEAD-SRYNAME     PIC X(12)  VALUE "診療行為名称".
               07  CSV-HEAD-LASYYMD     PIC X(12)  VALUE "最終来院日".
             05  CSV-HEAD-R             REDEFINES  CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM        PIC X(12)  OCCURS 12.
             05  CSV-HEAD-MAX           PIC 9(02)  VALUE 12.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
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
      *
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(512).
      *
      ****************************************************************
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY "*** START ***"
           PERFORM  100-INIT-SEC
      *
           DISPLAY "FLG-END  =  "FLG-END
           IF  FLG-END  =  ZERO
             PERFORM  200-MAIN-SEC
           END-IF
      *
           PERFORM  300-END-SEC
           DISPLAY "*** END ***"
      *
           STOP   RUN
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
           PERFORM  100-DBOPEN-SEC
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
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-SRYSTYMD
                                               WRK-PARA-SRYEDYMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-TENKIKBN
                                               WRK-PARA-SRYCD(1)
                                               WRK-PARA-SRYCD(2)
                                               WRK-PARA-SRYCD(3)
                                               WRK-PARA-SRYCD(4)
                                               WRK-PARA-SRYCD(5)
                                               WRK-PARA-SRYCD(6)
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM      TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"                 TO  SJOBKANRI-MODE
           INITIALIZE                    JOBKANRI-REC
           MOVE    SPA-HOSPNUM           TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID        TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID      TO  JOB-SHELLID
           MOVE    "A00000TA05"          TO  JOB-PGID
           MOVE    "指定病名・診療行為患者一覧"
                                         TO  JOB-SHELLMSG
           CALL    "ORCSJOB"             USING
                                         ORCSJOBKANRIAREA
                                         JOBKANRI-REC
                                         SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           MOVE    "ATA05TP"                TO  TA05P-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  TA05P-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  TA05P-HOSPNUM
      *
           MOVE    TA05P-BASENAME           TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  TA05P-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM  110-PARA-HENSYU-SEC
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
      *    集計終了日が空白の場合、集計開始日と同じ値を格納
           IF  WRK-PARA-SRYEDYMD  =  SPACE
             MOVE    WRK-PARA-SRYSTYMD  TO  WRK-PARA-SRYEDYMD
           END-IF
      *
      *    入力パラメタチェック（集計対象範囲）
           IF  WRK-PARA-SRYSTYMD  >  WRK-PARA-SRYEDYMD
             MOVE    "集計対象期間の範囲指定に誤りがあります"
                                        TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
           IF  WRK-PARA-SRYSTYMD(1:6)  NOT =  WRK-PARA-SRYEDYMD(1:6)
             MOVE    "集計対象期間の範囲は同月内で指定をして下さい"
                                        TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
      *    入力パラメタチェック（入外区分）
           IF  WRK-PARA-NYUGAIKBN  NOT =  SPACE
               AND  WRK-PARA-NYUGAIKBN  NOT =  "1"
               AND  WRK-PARA-NYUGAIKBN  NOT =  "2"
             MOVE    "入外区分の値が正しくありません"  TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
      *    入力パラメタチェック（転帰区分）
           IF  WRK-PARA-TENKIKBN  NOT =  SPACE
               AND  WRK-PARA-TENKIKBN  NOT =  "1"
               AND  WRK-PARA-TENKIKBN  NOT =  "2"
               AND  WRK-PARA-TENKIKBN  NOT =  "3"
               AND  WRK-PARA-TENKIKBN  NOT =  "4"
               AND  WRK-PARA-TENKIKBN  NOT =  "5"
             MOVE    "転帰の値が正しくありません"
                                         TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
      *    診療行為CD、病名CD MAXセット
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  6
                    OR  WRK-PARA-SRYCD(IDX1)  =  SPACE
             ADD  1            TO  IDX-PARAMAX
           END-PERFORM
           DISPLAY "入力パラ数 ： " IDX-PARAMAX
      *
      *    集計開始日編集
           MOVE    WRK-PARA-SRYSTYMD    TO  WRK-SYMD
           PERFORM  800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3          TO  WRK-SRYSTYMD
      *    集計開始日取得
           MOVE    WRK-PARA-SRYSTYMD(7:2)    TO  IDX-STDAY
           DISPLAY "開始日 ： " IDX-STDAY
      *
      *    集計終了日編集
           MOVE    WRK-PARA-SRYEDYMD    TO  WRK-SYMD
           PERFORM  800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3          TO  WRK-SRYEDYMD
      *    集計終了日取得
           MOVE    WRK-PARA-SRYEDYMD(7:2)    TO  IDX-EDDAY
           DISPLAY "終了日 ： " IDX-EDDAY
      *
      *    作成日編集
           MOVE    LNK-PRTKANRI-SKYYMD  TO  WRK-SYMD
           PERFORM  800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3          TO  WRK-SYSYMD
      *
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN  OUTPUT  TA05-FILE
           CLOSE  TA05-FILE
           OPEN  I-O  TA05-FILE
      *
      *    診療行為検索
           PERFORM  200-SRYACT-SET-SEC
      *
      *    病名検索
           PERFORM  200-BYOMEI-SET-SEC
      *
           CLOSE  TA05-FILE
      *
      *    合計ページ計算処理
           PERFORM  300-TOTAL-PAGE-SET-SEC
      *
      *    印刷編集処理
           PERFORM  300-PRINT-SET-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索
      *****************************************************************
       200-SRYACT-SET-SEC               SECTION.
      *
      *    集計対象の診療行為を検索
           PERFORM  VARYING  IDX-PARA  FROM  1  BY  1
                    UNTIL  IDX-PARA  >  IDX-PARAMAX
      *
      *    病名CD飛ばす
             IF  WRK-PARA-SRYCD(IDX-PARA)(8:2)  NOT =  SPACE
      *
      *    診療行為SELECT
               INITIALIZE                SRYACT-REC
               MOVE    WRK-PARA-SRYCD(IDX-PARA)   TO  SRY-SRYCD(1)
               MOVE    WRK-PARA-SRYCD(IDX-PARA)   TO  SRY-SRYCD(2)
               PERFORM  900-SRYACT-SELECT-SEC
               PERFORM  UNTIL  FLG-SRYACT  =  1
      *
      *    診療会計READ
                 PERFORM  900-SRYACCT-READ-SEC
      *    集計範囲内の最終来院日を取得したいので集計最終日から検索
                 PERFORM  VARYING  IDX-DAY  FROM  IDX-EDDAY  BY  -1
                          UNTIL  IDX-DAY  <  IDX-STDAY
      *    対象期間内に診療が行われているか判定
                   IF  ACCT-DAY(1 IDX-DAY)  >  ZERO
      *
      *    キー設定
                     INITIALIZE          TA05TMP-REC
                     MOVE    SRY-PTID    TO  TA05TMP-PTID
      *    TMPファイル読み込み(KEY READ)
                     PERFORM  800-SKTTMP-READ-SEC
      *
      *    診療行為FLG１〜６に1を格納
                     MOVE    1           TO  TA05TMP-SRYFLG(IDX-PARA)
      *
      *    点数マスタREAD
                     PERFORM  900-TENSU-READ-SEC
      *
      *    診療行為名称
                     MOVE    TNS-NAME    TO  TA05TMP-SRYNAME(IDX-PARA)
      *
      *    最終来院日
                     IF  TA05TMP-LASTYMD(IDX-PARA)(7:2)  <  IDX-DAY
                         OR  TA05TMP-LASTYMD(IDX-PARA)(7:2)  =  SPACE
                       MOVE    WRK-PARA-SRYSTYMD(1:6)
                                    TO  TA05TMP-LASTYMD(IDX-PARA)(1:6)
                       MOVE    IDX-DAY
                                    TO  TA05TMP-LASTYMD(IDX-PARA)(7:2)
                     END-IF
                     MOVE    ZERO        TO  IDX-DAY
      *
      *    TMPファイル書込み
                     IF  FLG-TA05TMP  =  1
                       WRITE    TA05TMP-REC
                     ELSE
                       REWRITE  TA05TMP-REC
                     END-IF
                   END-IF
                 END-PERFORM
      *    診療行為FETCH
                 PERFORM  900-SRYACT-FETCH-SEC
               END-PERFORM
      *    診療行為CLOSE
               PERFORM  900-SRYACT-CLOSE-SEC
             END-IF
           END-PERFORM
      *
           .
       200-SRYACT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名検索
      *****************************************************************
       200-BYOMEI-SET-SEC               SECTION.
      *
      *    集計対象の診療行為を検索
           PERFORM  VARYING  IDX-PARA  FROM  1  BY  1
                    UNTIL  IDX-PARA  >  IDX-PARAMAX
      *
      *    診療行為CD飛ばす
             IF  WRK-PARA-SRYCD(IDX-PARA)(8:2)  =  SPACE
      *    患者病名SELECT
               PERFORM  900-BYOMEI-SELECT-SEC
               PERFORM  UNTIL  FLG-PTBYOMEI  =  1
      *
      *    対象病名判定(病名CD＋転帰区分＋転帰日) 
                 IF  (WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-1
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-2
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-3
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-4
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-5
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-6
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-7
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-8
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-9
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-10
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-11
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-12
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-13
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-14
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-15
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-16
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-17
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-18
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-19
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-20
                     OR  WRK-PARA-SRYCD(IDX-PARA)  =  PTBYO-BYOMEICD-21)
                     AND  ( ( WRK-PARA-TENKIKBN   =  "1"
                     AND  PTBYO-TENKIKBN  =  SPACE )
                     OR  ( WRK-PARA-TENKIKBN   =  "2"
                     AND  PTBYO-TENKIKBN  =  "1" )
                     OR  ( WRK-PARA-TENKIKBN   =  "3"
                     AND  PTBYO-TENKIKBN  =  "2" )
                     OR  ( WRK-PARA-TENKIKBN   =  "4"
                     AND  PTBYO-TENKIKBN  =  "3" )
                     OR  ( WRK-PARA-TENKIKBN   =  "5"
                     AND  PTBYO-TENKIKBN  =  "8" )
                     OR   WRK-PARA-TENKIKBN  =  SPACE )
                     AND  ( ( PTBYO-TENKIYMD  >=  WRK-PARA-SRYSTYMD
                     AND  PTBYO-TENKIYMD  <=  WRK-PARA-SRYEDYMD
                     AND  WRK-PARA-TENKIKBN  NOT =  SPACE )
                     OR  WRK-PARA-TENKIKBN  =  SPACE
                     OR  WRK-PARA-TENKIKBN  =  "1" )
      *
      *    キー設定
                   INITIALIZE          TA05TMP-REC
                   MOVE    PTBYO-PTID     TO  TA05TMP-PTID
      *
      *    TMPファイル読み込み(KEY READ)
                   PERFORM  800-SKTTMP-READ-SEC
      *
      *    診療行為FLG１〜６に1を格納
                   MOVE    1              TO  TA05TMP-SRYFLG(IDX-PARA)
      *
      *    病名
                   MOVE    PTBYO-BYOMEI   TO  TA05TMP-SRYNAME(IDX-PARA)
      *
      *    病名開始日
                   MOVE    PTBYO-SRYYMD   TO  TA05TMP-BYOSTYMD(IDX-PARA)
      *
      *    転帰日
                   MOVE    PTBYO-TENKIYMD TO  TA05TMP-TENKIYMD(IDX-PARA)
      *
      *    転帰
                   EVALUATE  PTBYO-TENKIKBN
                     WHEN  SPACE
                       MOVE    "継続"     TO  TA05TMP-TENKI(IDX-PARA)
                     WHEN  "1"
                       MOVE    "治癒"     TO  TA05TMP-TENKI(IDX-PARA)
                     WHEN  "2"
                       MOVE    "死亡"     TO  TA05TMP-TENKI(IDX-PARA)
                     WHEN  "3"
                       MOVE    "中止"     TO  TA05TMP-TENKI(IDX-PARA)
                     WHEN  "8"
                       MOVE    "移行"     TO  TA05TMP-TENKI(IDX-PARA)
                   END-EVALUATE
      *
      *    TMPファイル書込み
                   IF  FLG-TA05TMP  =  1
                     WRITE    TA05TMP-REC
                   ELSE
                     REWRITE  TA05TMP-REC
                   END-IF
                 END-IF
      *    患者病名FETCH
                 PERFORM  900-BYOMEI-FETCH-SEC
               END-PERFORM
      *    患者病名CLOSE
             PERFORM  900-BYOMEI-CLOSE-SEC
             END-IF
           END-PERFORM
      *
           .
       200-BYOMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計ページ計算処理
      *****************************************************************
       300-TOTAL-PAGE-SET-SEC               SECTION.
      *
           MOVE    ZERO       TO  CNT-PT
                                  IDX-SRY
                                  IDX-BYO
      *    入力パラの診療行為CDと病名CDの数をカウント
           PERFORM  VARYING  IDX1  FROM   1  BY  1
                    UNTIL  IDX1  >  IDX-PARAMAX
             IF  WRK-PARA-SRYCD(IDX1)(8:2)  =  SPACE
               ADD  1         TO  IDX-BYO
             ELSE
               ADD  1         TO  IDX-SRY
             END-IF
           END-PERFORM
      *
      *    TMPファイルOPEN
           OPEN  INPUT  TA05-FILE
      *
      *    TMPファイル読込み(NEXT READ)
           PERFORM  900-TMP-READ-SEC
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
      *    入力パラが空白の診療行為FLGに1を格納
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL  IDX1  >  6
               IF  IDX1  >  IDX-PARAMAX
                 MOVE    1        TO  TA05TMP-SRYFLG(IDX1)
               END-IF
             END-PERFORM
      *
      *    対象患者チェック
             IF  TA05TMP-SRYFLG(1)  =  1
                 AND  TA05TMP-SRYFLG(2)  =  1
                 AND  TA05TMP-SRYFLG(3)  =  1
                 AND  TA05TMP-SRYFLG(4)  =  1
                 AND  TA05TMP-SRYFLG(5)  =  1
                 AND  TA05TMP-SRYFLG(6)  =  1
      *
      *    対象患者数カウント
               ADD  1             TO  CNT-PT
             END-IF
      *
      *    TMPファイル読込み(NEXT READ)
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
      *    TMPファイルCLOSE
           CLOSE  TA05-FILE
      *
      *    合計ページ計算
           IF  IDX-BYO  >  IDX-SRY
             COMPUTE  WRK-COMP  =  CNT-PT  *  IDX-BYO
           ELSE
             COMPUTE  WRK-COMP  =  CNT-PT  *  IDX-SRY
           END-IF
           DIVIDE  WRK-COMP  BY  22  GIVING  WRK-GPAGE
                                     REMAINDER  WRK-AMARI
           IF   WRK-AMARI  NOT =  ZERO
             ADD  1           TO  WRK-GPAGE
           END-IF
      *
      *
           .
       300-TOTAL-PAGE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       300-PRINT-SET-SEC               SECTION.
      *
      *    TMPファイルOPEN
           OPEN  INPUT  TA05-FILE
      *
           INITIALIZE             TA05
           MOVE    1              TO  IDX-LINE
           MOVE    ZERO           TO  CNT-PT
                                      FLG-PRINT
      *    TMPファイル読込み(NEXT READ)
           PERFORM  900-TMP-READ-SEC
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
      *    入力パラが空白の診療行為FLGに1を格納
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL  IDX1  >  6
               IF  IDX1  >  IDX-PARAMAX
                 MOVE    1        TO  TA05TMP-SRYFLG(IDX1)
               END-IF
             END-PERFORM
      *
      *    対象患者チェック
             IF  TA05TMP-SRYFLG(1)  =  1
                 AND  TA05TMP-SRYFLG(2)  =  1
                 AND  TA05TMP-SRYFLG(3)  =  1
                 AND  TA05TMP-SRYFLG(4)  =  1
                 AND  TA05TMP-SRYFLG(5)  =  1
                 AND  TA05TMP-SRYFLG(6)  =  1
      *
      *    印刷データ編集処理
               PERFORM  300-PRINT-DATA-SEC
      *
               ADD  1             TO  IDX-LINE
               IF  IDX-LINE  >  22
      *    帳票印刷処理
                 PERFORM  300-PRINT-OUT-SEC
      *
                 MOVE    1            TO  IDX-POINT
      *    CSVヘッダ編集処理
                 IF   FLG-CSV-HEADER  =  ZERO
                   PERFORM  400-CSV-HEAD-SEC
                   MOVE    1      TO  FLG-CSV-HEADER
                 END-IF
      *
      *    CSV編集処理
                 PERFORM  400-CSV-DATA-SEC
      *
      *    CSV出力処理
                 PERFORM  230-TOUKEICSV-SEC
               END-IF
             END-IF
      *
      *    TMPファイル読込み(NEXT READ)
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           IF  IDX-LINE  NOT =  1
      *    帳票印刷処理
             PERFORM  300-PRINT-OUT-SEC
      *
             MOVE    1            TO  IDX-POINT
      *    CSVヘッダ編集処理
             IF   FLG-CSV-HEADER  =  ZERO
               PERFORM  400-CSV-HEAD-SEC
               MOVE    1          TO  FLG-CSV-HEADER
             END-IF
      *
      *    CSV編集処理
             PERFORM  400-CSV-DATA-SEC
      *
      *    CSV出力処理
             PERFORM  230-TOUKEICSV-SEC
           END-IF
      *
      *    TMPファイルCLOSE
           CLOSE  TA05-FILE
      *
           .
       300-PRINT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷データ編集処理
      *****************************************************************
       300-PRINT-DATA-SEC               SECTION.
      *
      *    連番
           ADD  1          TO  CNT-PT
           MOVE    CNT-PT            TO  TA05-RENNUM(IDX-LINE)
      *
      *    患者番号マスタREAD
           PERFORM  900-PTNUM-READ-SEC
           IF   PTNUM-PTNUM(11:)  =  SPACE
             MOVE    PTNUM-PTNUM     TO  TA05-PTNUM(IDX-LINE)
           ELSE
             MOVE    PTNUM-PTNUM     TO  TA05-PTNUM2(IDX-LINE)
           END-IF
      *
      *    患者情報マスタREAD
           PERFORM  900-PTINF-READ-SEC
      *
      *    カナ氏名
           MOVE    PTINF-KANANAME    TO  TA05-KANA-NAME(IDX-LINE)
      *
      *    氏名
           MOVE    PTINF-NAME        TO  TA05-NAME(IDX-LINE)
      *
      *    性別
           IF  PTINF-SEX  =  "1"
             MOVE    "男"            TO  TA05-SEX(IDX-LINE)
           ELSE
             MOVE    "女"            TO  TA05-SEX(IDX-LINE)
           END-IF
      *
      *    年齢
           MOVE  LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           MOVE  PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           COMPUTE  WRK-NENREI  =  WRK-SYY  -  WRK-BIRTHY
           IF   WRK-SMM  <  WRK-BIRTHM
             COMPUTE  WRK-NENREI  =  WRK-NENREI  -  1
           END-IF
           MOVE    WRK-NENREI        TO  WRK-Z2-9
           MOVE    WRK-Z2-9          TO  TA05-AGE(IDX-LINE)
      *
      *    病名・診療行為整理(病名と診療行為を別々に分ける)
           MOVE    1                 TO  IDX-SRY
                                         IDX-BYO
           PERFORM  VARYING  IDX1  FROM   1  BY  1
                    UNTIL  IDX1  >  IDX-PARAMAX
             IF  WRK-PARA-SRYCD(IDX1)(8:2)  =  SPACE
               MOVE    TA05TMP-SRYNAME(IDX1)   TO  WRK-BYOMEI(IDX-BYO)
               MOVE    TA05TMP-BYOSTYMD(IDX1)  TO  WRK-BYOSTYMD(IDX-BYO)
               MOVE    TA05TMP-TENKIYMD(IDX1)  TO  WRK-TENKIYMD(IDX-BYO)
               MOVE    TA05TMP-TENKI(IDX1)     TO  WRK-TENKI(IDX-BYO)
               ADD  1         TO  IDX-BYO
             ELSE
               MOVE    TA05TMP-SRYNAME(IDX1)   TO  WRK-SRYNAME(IDX-SRY)
               MOVE    TA05TMP-LASTYMD(IDX1)   TO  WRK-LASTYMD(IDX-SRY)
               ADD  1         TO  IDX-SRY
             END-IF
      *
      *    ヘッダー用に病名・診療行為名称を格納
             MOVE    TA05TMP-SRYNAME(IDX1)   TO  WRK-HEAD-SRYNAME(IDX1)
      *
           END-PERFORM
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  6
                    OR  ( WRK-BYOMEI(IDX1)  =  SPACE
                    AND  WRK-SRYNAME(IDX1)  =  SPACE )
      *
      *    病名
             IF  WRK-BYOMEI(IDX1)(23:)  =  SPACE
               MOVE    WRK-BYOMEI(IDX1)   TO  TA05-BYOMEI(IDX-LINE)
             ELSE
               MOVE    WRK-BYOMEI(IDX1)   TO  TA05-BYOMEI2(IDX-LINE)
             END-IF
      *
      *    病名開始日
             MOVE    WRK-BYOSTYMD(IDX1)   TO  WRK-SYMD
             PERFORM  800-SEIWA-HEN-SEC
             MOVE    WRK-EDTYMD1          TO  TA05-BYOSTYMD(IDX-LINE)
      *
      *    転帰日
             IF  WRK-TENKIYMD(IDX1)  =  SPACE
               MOVE    SPACE              TO  TA05-TENKIYMD(IDX-LINE)
             ELSE
               MOVE    WRK-TENKIYMD(IDX1) TO  WRK-SYMD
               PERFORM  800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1        TO  TA05-TENKIYMD(IDX-LINE)
             END-IF
      *
      *    転帰
             MOVE    WRK-TENKI(IDX1)      TO  TA05-TENKI(IDX-LINE)
      *
      *    診療行為名
             IF  WRK-SRYNAME(IDX1)(23:)  =  SPACE
               MOVE    WRK-SRYNAME(IDX1)  TO  TA05-SRYNAME(IDX-LINE)
             ELSE
               MOVE    WRK-SRYNAME(IDX1)  TO  TA05-SRYNAME2(IDX-LINE)
             END-IF
      *
      *    最終来院日
             MOVE    WRK-LASTYMD(IDX1)    TO  WRK-SYMD
             PERFORM  800-SEIWA-HEN-SEC
             MOVE    WRK-EDTYMD1          TO  TA05-LASTYMD(IDX-LINE)
      *
      *    次のデータが存在するかチェック（IDX1 = 6の場合は最後なので処理しない）
             IF  ( WRK-BYOMEI(IDX1 + 1)  NOT =  SPACE
                 OR  WRK-SRYNAME(IDX1 + 1)  NOT =  SPACE )
                 AND  IDX1  <  6
               ADD  1      TO  IDX-LINE
      *
               IF  IDX-LINE  >  22
      *    帳票印刷処理
                 PERFORM  300-PRINT-OUT-SEC
      *
                 MOVE    1            TO  IDX-POINT
      *    CSVヘッダ編集処理
                 IF   FLG-CSV-HEADER  =  ZERO
                   DISPLAY "HEAD"
                   PERFORM  400-CSV-HEAD-SEC
                   MOVE    1      TO  FLG-CSV-HEADER
                 END-IF
      *
      *    CSV編集処理
                 PERFORM  400-CSV-DATA-SEC
      *
      *    CSV出力処理
                 PERFORM  230-TOUKEICSV-SEC
               END-IF
             END-IF
           END-PERFORM
      *
           .
       300-PRINT-DATA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘッダ印刷処理
      *****************************************************************
       300-PRINT-HEAD-SEC        SECTION.
      *
      *    発行日
           MOVE    WRK-SYSYMD           TO  TA05-PRTYMD
      *
      *    入外区分
           EVALUATE  WRK-PARA-NYUGAIKBN
             WHEN  SPACE
               MOVE    "入院・外来"     TO  TA05-NYUGAIKBN
             WHEN  "1"
               MOVE    "入院"           TO  TA05-NYUGAIKBN
             WHEN  "2"
               MOVE    "外来"           TO  TA05-NYUGAIKBN
           END-EVALUATE
      *
      *    頁数
           ADD   1                      TO  CNT-PAGE
           MOVE    CNT-PAGE             TO  WRK-Z2-9
           MOVE    WRK-Z2-9             TO  TA05-PAGE
      *
      *    合計頁数計算
           MOVE    WRK-GPAGE            TO  WRK-Z2-9
           MOVE    WRK-Z2-9             TO  TA05-GPAGE
      *
      *    診療対象開始日
           MOVE    WRK-SRYSTYMD         TO  TA05-SRYSTYMD
      *
      *    診療対象終了日
           MOVE    WRK-SRYEDYMD(11:12)  TO  TA05-SRYEDMD
      *
      *    検索対象診療行為・病名
           MOVE    1                    TO  IDX-POINT
           STRING  WRK-HEAD-SRYNAME(1)   DELIMITED  BY  SPACE
                                        INTO  TA05-SEARCHNAME
                                        POINTER  IDX-POINT
           END-STRING
           PERFORM  VARYING  IDX2  FROM  2  BY  1
                    UNTIL  IDX2  >  IDX-PARAMAX
                    OR  WRK-HEAD-SRYNAME(IDX2)  =  SPACE
             STRING   " ＋ "                 DELIMITED  BY  SIZE
                      WRK-HEAD-SRYNAME(IDX2)  DELIMITED  BY  SPACE
                                             INTO  TA05-SEARCHNAME
                                             POINTER  IDX-POINT
             END-STRING
           END-PERFORM
      *
           .
      *
       300-PRINT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       300-PRINT-OUT-SEC          SECTION.
      *
      *    ヘッダ印字処理
           PERFORM  300-PRINT-HEAD-SEC
      *
           INITIALIZE                         ORCSPRTAREA
           MOVE    "INS"                      TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM         TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           MOVE    "A00000TA05.red"              TO  SPRT-PRTID
           MOVE    "指定病名・診療行為患者一覧"   TO  SPRT-TITLE
           MOVE    TA05                       TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                  USING
                                              ORCSPRTAREA
                                              SPA-AREA
           IF  ( SPRT-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"  TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
           .
      *
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       400-CSV-HEAD-SEC            SECTION.
      *
           INITIALIZE           WRK-CSVDATA
      *    対象範囲設定
           INITIALIZE                     WRK-ITEM
           STRING    WRK-SRYSTYMD         DELIMITED  BY  SIZE
                     " 〜 "               DELIMITED  BY  SIZE
                     WRK-SRYEDYMD(11:12)  DELIMITED  BY  SIZE
                                          INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分セット
           INITIALIZE                     WRK-ITEM
           MOVE    TA05-NYUGAIKBN      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    システム日付セット 
           INITIALIZE                     WRK-ITEM
           STRING    "発行日："           DELIMITED  BY  SIZE
                     WRK-SYSYMD           DELIMITED  BY  SIZE
                                          INTO  WRK-ITEM
           END-STRING
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    検索対象診療行為・病名
           INITIALIZE                     WRK-ITEM
           MOVE    TA05-SEARCHNAME     TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名セット
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL  IDX2  >  CSV-HEAD-MAX
             MOVE    CSV-HEAD-ITEM(IDX2)  TO  WRK-ITEM
             IF  IDX2  =  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
           .
      *
       400-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
           PERFORM  VARYING  IDX-LINE2  FROM  1  BY  1
                    UNTIL  IDX-LINE2  >  22
      *
      *    連番
             INITIALIZE                             WRK-ITEM
             MOVE    TA05-RENNUM(IDX-LINE2)      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
             INITIALIZE                           WRK-ITEM
             IF   TA05-PTNUM(IDX-LINE2)  =  SPACE
               MOVE    TA05-PTNUM2(IDX-LINE2)    TO  WRK-ITEM
             ELSE
               MOVE    TA05-PTNUM(IDX-LINE2)     TO  WRK-ITEM
             END-IF
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    カナ氏名
             INITIALIZE                           WRK-ITEM
             MOVE    TA05-KANA-NAME(IDX-LINE2)   TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    氏名
             INITIALIZE                             WRK-ITEM
             MOVE    TA05-NAME(IDX-LINE2)        TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    性別
             INITIALIZE                             WRK-ITEM
             MOVE    TA05-SEX(IDX-LINE2)         TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    年齢
             INITIALIZE                             WRK-ITEM
             MOVE    TA05-AGE(IDX-LINE2)         TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    病名
             INITIALIZE                             WRK-ITEM
             IF   TA05-BYOMEI(IDX-LINE2)  =  SPACE
               MOVE    TA05-BYOMEI2(IDX-LINE2)   TO  WRK-ITEM
             ELSE
               MOVE    TA05-BYOMEI(IDX-LINE2)    TO  WRK-ITEM
             END-IF
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    病名開始日
             INITIALIZE                             WRK-ITEM
             MOVE    TA05-BYOSTYMD(IDX-LINE2)    TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    転帰日
             INITIALIZE                             WRK-ITEM
             MOVE    TA05-TENKIYMD(IDX-LINE2)    TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    転帰
             INITIALIZE                             WRK-ITEM
             MOVE    TA05-TENKI(IDX-LINE2)       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療行為名
             INITIALIZE                             WRK-ITEM
             IF   TA05-SRYNAME(IDX-LINE2)  =  SPACE
               MOVE    TA05-SRYNAME2(IDX-LINE2)  TO  WRK-ITEM
             ELSE
               MOVE    TA05-SRYNAME(IDX-LINE2)   TO  WRK-ITEM
             END-IF
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    最終来院日
             INITIALIZE                             WRK-ITEM
             MOVE    TA05-LASTYMD(IDX-LINE2)     TO  WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
      *
           END-PERFORM
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDX-POINT
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","   DELIMITED   BY  SIZE
           INTO      WRK-CSVDATA
           WITH      POINTER   IDX-POINT
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"   DELIMITED BY  SIZE
           INTO      WRK-CSVDATA
           WITH      POINTER   IDX-POINT
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX-LEN  FROM  1  BY  1
                    UNTIL  IDX-LEN  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX-LEN:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX-LEN:1)  NOT =  SPACE
                 MOVE    IDX-LEN      TO  IDX-ST
                 MOVE    1            TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                     ORCSTOUKEICSVAREA
           MOVE    "INS"                  TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
           MOVE    ZERO                   TO  STOUKEICSV-PTID
           MOVE    WRK-PARA-NYUGAIKBN     TO  STOUKEICSV-NYUGAIKBN
           MOVE    "/var/tmp/"            TO  STOUKEICSV-TO-FOLDER
           STRING  WRK-PARA-HOSPNUM       DELIMITED  BY  SIZE
                   "byomeisryactlist_"    DELIMITED  BY  SIZE
                   WRK-PARA-SRYSTYMD      DELIMITED  BY  SIZE
                   "-"                    DELIMITED  BY  SIZE
                   WRK-PARA-SRYEDYMD      DELIMITED  BY  SIZE
                   ".csv"                 DELIMITED  BY  SIZE
                                          INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                    TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA            TO  STOUKEICSV-CSVDATA
           MOVE    "指定病名・診療行為患者一覧"  TO  STOUKEICSV-TITLE
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE           WRK-CSVDATA
                                TA05
           MOVE    1            TO  IDX-LINE
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC            SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT            RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"             TO  SJOBKANRI-MODE
             INITIALIZE                JOBKANRI-REC
             MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID    TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID  TO  JOB-SHELLID
             MOVE    WRK-RECEERR       TO  JOB-YOBI
             MOVE    "9999"            TO  JOB-ERRCD
             CALL    "ORCSJOB"         USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1           TO  FLG-END
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
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    対象患者一時ファイル読込(KEY READ)
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ  TA05-FILE 
             INVALID  KEY
               MOVE    1           TO  FLG-TA05TMP
             NOT  INVALID  KEY
               MOVE    ZERO        TO  FLG-TA05TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   対象患者一時ファイル読込(NEXT READ)
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  TA05-FILE  NEXT
             AT  END
               MOVE    1           TO  FLG-PRINT
           END-READ
      *
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF  ( WRK-SYMD  =  SPACE )
             GO    TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT  WRK-EDTYMD3  REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為SELECT処理
      *****************************************************************
       900-SRYACT-SELECT-SEC         SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    WRK-PARA-SRYSTYMD(1:6)  TO  SRY-SRYYM
           MOVE    WRK-PARA-SRYEDYMD   TO  SRY-CREYMD
           IF  WRK-PARA-NYUGAIKBN  =  SPACE
             MOVE    "%"               TO  SRY-NYUGAIKBN
           ELSE
             MOVE    WRK-PARA-NYUGAIKBN   TO  SRY-NYUGAIKBN
           END-IF
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC  =  ZERO )
             MOVE    ZERO              TO  FLG-SRYACT
             PERFORM  900-SRYACT-FETCH-SEC
           ELSE
             INITIALIZE                SRYACT-REC
             MOVE    1                 TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理
      *****************************************************************
       900-SRYACT-FETCH-SEC        SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    ZERO              TO  FLG-SRYACT
             MOVE    MCPDATA-REC       TO  SRYACT-REC
      *
             INITIALIZE                    PTINF-REC
             MOVE    SRY-HOSPNUM       TO  PTINF-HOSPNUM
             MOVE    SRY-PTID          TO  PTINF-PTID
             PERFORM  910-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
             AND PTINF-TSTPTNUMKBN  =  "1"
                 GO  TO  900-SRYACT-FETCH-SEC
             END-IF
      *
           ELSE
             INITIALIZE                SRYACT-REC
             MOVE    1                 TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為CLOSE処理
      *****************************************************************
       900-SRYACT-CLOSE-SEC        SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           INITIALIZE                  SRYACT-REC
      *
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報READ
      *****************************************************************
       900-SRYACCT-READ-SEC               SECTION.
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    WRK-PARA-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_sryacct"     TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
             ELSE
               INITIALIZE              SRYACCT-REC
             END-IF
      *
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名SELECT処理
      *****************************************************************
       900-BYOMEI-SELECT-SEC         SECTION.
      *
           INITIALIZE                  PTBYOMEI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTBYO-HOSPNUM
           MOVE    WRK-PARA-SRYEDYMD   TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC  =  ZERO )
             MOVE    ZERO              TO  FLG-PTBYOMEI
             PERFORM  900-BYOMEI-FETCH-SEC
           ELSE
             INITIALIZE                PTBYOMEI-REC
             MOVE    1                 TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-BYOMEI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    患者病名FETCH処理
      *****************************************************************
       900-BYOMEI-FETCH-SEC        SECTION.
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    ZERO              TO  FLG-PTBYOMEI
             MOVE    MCPDATA-REC       TO  PTBYOMEI-REC
      *
             INITIALIZE                    PTINF-REC
             MOVE    PTBYO-HOSPNUM     TO  PTINF-HOSPNUM
             MOVE    PTBYO-PTID        TO  PTINF-PTID
             PERFORM  910-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
             AND PTINF-TSTPTNUMKBN  =  "1"
                 GO  TO  900-BYOMEI-FETCH-SEC
             END-IF
      *
           ELSE
             INITIALIZE                PTBYOMEI-REC
             MOVE    1                 TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-BYOMEI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    患者病名CLOSE処理
      *****************************************************************
       900-BYOMEI-CLOSE-SEC        SECTION.
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-BYOMEI-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスタ読込処理
      *****************************************************************
       900-PTNUM-READ-SEC         SECTION.
      *
           INITIALIZE                  PTNUM-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNUM-HOSPNUM
           MOVE    TA05TMP-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptnum"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
             ELSE
               INITIALIZE              PTNUM-REC
             END-IF
           ELSE
             INITIALIZE                PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスタ読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    TA05TMP-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
             ELSE
               INITIALIZE              PTINF-REC
             END-IF
           ELSE
             INITIALIZE                PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスタ読込処理
      *****************************************************************
       910-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO   FLG-PTINF
             ELSE
               MOVE    1               TO   FLG-PTINF
               INITIALIZE              PTINF-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-PTINF
             INITIALIZE                PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SRY-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-PARA-SRYCD(IDX-PARA)  TO  TNS-SRYCD
           MOVE    WRK-PARA-SRYSTYMD(1:6)    TO  TNS-YUKOSTYMD(1:6)
                                                 TNS-YUKOEDYMD(1:6)
           MOVE    IDX-DAY             TO  TNS-YUKOSTYMD(7:2)
                                           TNS-YUKOEDYMD(7:2)
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_tensu"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
             ELSE
               INITIALIZE              TNS-TENSU-REC
             END-IF
           ELSE
             INITIALIZE                TNS-TENSU-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
             ELSE
               MOVE    1               TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
           .
       900-DBCLOSE-EXT.
           EXIT.
