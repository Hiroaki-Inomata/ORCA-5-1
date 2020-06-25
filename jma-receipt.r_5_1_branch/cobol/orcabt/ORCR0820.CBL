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
       PROGRAM-ID.             ORCR0820.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      レセプト種別編集（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  中止処理追加
      *  01.00.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.00.02    NACL-門脇    05/01/08  同月に継続なしで２回入院した
      *                                     場合の入院年月日の記載方法
      *  01.00.03    NACL-門脇    05.09.06  自賠責従来様式対応
      *  01.00.04    NACL-門脇    05.12.01  労災レセプト回数対応
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  04.01.01    NACL-門脇    07/11/01  公務災害対応
      *
      *  04.04.01    NACL-藤原    09/03/10  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *  04.04.02    NACL-門脇    09/06/12  療養期間の編集修正
      *
      *  04.05.01    NACL-門脇    11/10/07  療養期間（終了日）の編集修正
      *  04.05.02    NACL-藤原    11/11/18  他保険での同日再入院時の保険
      *                                     履歴対応
      *
      *  04.07.01    NACL-門脇    11/09/07  労災請求書作成対応
      *  04.07.02    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.03    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.04    NACL-門脇    14/05/08  労災レセ「新継再別」記載修正
      *  04.07.05    NACL-門脇    14/08/07  労災レセ「新継再別」記載修正
      *  04.07.06    NACL-藤原    15/01/24  同日再入院時の入退院履歴の記録
      *                                     を修正
      *                                    （入院日と退院日が同じとき）
      *  04.07.07    NACL-藤原    15/07/07  入退院履歴のからの診療科設定
      *                                     に退院日、転出日を条件に追加
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    14/07/23  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *  04.08.03    NACL-藤原    17/03/31  同日再入院の入退院履歴のチェック
      *
      *  05.00.01    NACL-門脇    19/02/01  療養期間編集修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
           SELECT  RECE88-FILE     ASSIGN  RECE88PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE88-KEY
                                   FILE    STATUS  IS  STS-RECE88.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE88-FILE.
       01  RECE88-REC. 
           COPY    "CPRCF088.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE82//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE88//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82              PIC X(02).
           03  STS-RECE88              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-RECE82              PIC 9(01). 
           03  FLG-RECE88              PIC 9(01). 
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNYUINRRK23        PIC 9(01).
           03  FLG-PTNYUINRRK22        PIC 9(01).
           03  FLG-JYURRKCHECK         PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
      *
           03  FLG-RRKOK               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-IN-CANCEL           PIC 9(06).
           03  CNT-RECE88              PIC 9(06).
           03  CNT-OUT                 PIC 9(06). 
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(03). 
           03  IDX                     PIC 9(03). 
           03  IDX1                    PIC 9(03). 
           03  IDX2                    PIC 9(03). 
           03  IDX3                    PIC 9(03). 
           03  IDX4                    PIC 9(02).
           03  IDX5                    PIC 9(02).
           03  IDX6                    PIC 9(02).
           03  IDX7                    PIC 9(02).
           03  IDX8                    PIC 9(02).
           03  IDX-ROU                 PIC 9(02).
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM      PIC X(06).
               05  WRK-PARA-TERMID     PIC X(16).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-RECEKBN    PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-JIBAIPRTKBN    PIC X(01).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM         PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-RYOSTYMD            PIC X(08).
           03  WRK-RYOEDYMD            PIC X(08).
      *
           03  WRK-STYMD               PIC X(08).
      *
           03  WRK-HKNKBN              PIC X(01).
      *
           03  WRK-EDYMD.
               05  WRK-EDYM            PIC 9(06).
               05  WRK-EDDD            PIC 9(02).
      *
           03  WK-PTNYUINRRK-TENSTUYMD    PIC  X(08).
           03  WK-PTNYUINRRK-TENSTUYMD2   PIC  X(08).
           03  WK-PTNYUINRRK-TENNYUYMD    PIC  X(08).
      *
           03  WRK-TAIHI-RYOSTYMD      PIC X(08).
           03  WRK-TAIHI-RYOEDYMD      PIC X(08).
      *
      *    病棟区分テーブル                                             
       01  WRK-BYOTOKBN-TABLE.                                          
           03  WRK-BYOTOKBN-T            OCCURS  4.                     
               05  WRK-BYOTOKBN          PIC X(02).                     
      *                                                                 
       01  WRK-BYOTOKBN-DATA             PIC X(02).                     
      *    初回番号(最新の入院歴分)
       01  WRK-SHONUM                    PIC 9(03).                     
      *
      *    ワーク入退院履歴
       01  WRK-RECE88-REC. 
           COPY    "CPRCF088.INC"  REPLACING  //RECE88//
                                   BY         //WRK-RECE88//.
      *
      *    ワーク入院履歴
       01  WRK-PTNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WRK-PTNRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    病棟情報                                                     
           COPY    "CPSK5001.INC".                                      
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    労災レセプト回数取得サブ
           COPY    "CPORCSROUKAISU.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
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
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE82PARA
           INITIALIZE                      RECE82PARA
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE88PARA
           INITIALIZE                      RECE88PARA
           MOVE    "RECE880"           TO  RECE88PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE88PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE88PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE82PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE88PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE82PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE88PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0820"          TO  JOB-PGID
               MOVE    "レセプト種別編集（労災・自賠責）"    
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           OPEN    I-O                 RECE82-FILE
      *
           OPEN    OUTPUT              RECE88-FILE
           CLOSE                       RECE88-FILE
           OPEN    I-O                 RECE88-FILE
      *
           PERFORM 900-RECE82-READ-SEC
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 210-HENSYU-SEC
      *
           PERFORM 900-RECE82-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
      *     
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                  SECTION.
      *
      *    受診歴チェック
           MOVE    ZERO   TO   FLG-JYURRKCHECK
           IF      RECE82-HKNKBN    =   "4" OR "5"
              PERFORM 2102-JYURRK-CHECK-SEC
           END-IF
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      RECE82-ERR-ERRKBN   >   ZERO
           OR      FLG-JYURRKCHECK     =    1
               CONTINUE
           ELSE
               EVALUATE    RECE82-HKNKBN   ALSO    RECE82-SAIGAIKBN
                   WHEN    "1"             ALSO    "1"
                       MOVE    7081            TO  RECE82-RECESYUBETU
                   WHEN    "1"             ALSO    "2"
                       MOVE    7082            TO  RECE82-RECESYUBETU
                   WHEN    "2"             ALSO    "1"
                       MOVE    7101            TO  RECE82-RECESYUBETU
                   WHEN    "2"             ALSO    "2"
                       MOVE    7102            TO  RECE82-RECESYUBETU
                   WHEN    "4"             ALSO    ANY
                   WHEN    "6"             ALSO    ANY
                       MOVE    7011            TO  RECE82-RECESYUBETU
                   WHEN    "5"          ALSO    ANY
                       MOVE    7061            TO  RECE82-RECESYUBETU
               END-EVALUATE
           END-IF 
      *
      *    入退院履歴情報編集・病棟番号・種別セット処理
           MOVE   RECE82-RYOSTYMD      TO  WRK-TAIHI-RYOSTYMD
           MOVE   RECE82-RYOEDYMD      TO  WRK-TAIHI-RYOEDYMD
           MOVE   SPACE                TO  RECE82-RYOSTYMD
           MOVE   SPACE                TO  RECE82-RYOEDYMD
           PERFORM 2101-RECE88-HENSYU-SEC
      *
      *    労災レセプト回数取得
           IF    ( RECE82-HKNKBN     =   "1"  OR  "2" )
             INITIALIZE                    LNK-ORCSROUKAISU-REC
             MOVE  RECE82-HOSPNUM      TO  LNK-ORCSROUKAISU-HOSPNUM
             MOVE  RECE82-NYUGAIKBN    TO  LNK-ORCSROUKAISU-NYUGAIKBN
             MOVE  RECE82-PTID         TO  LNK-ORCSROUKAISU-PTID
             MOVE  RECE82-HKNCOMBI     TO  LNK-ORCSROUKAISU-HKNCOMBI
             MOVE  RECE82-SRYYM        TO  LNK-ORCSROUKAISU-SRYYM
             MOVE  RECE82-SHOSHINYMD   TO  LNK-ORCSROUKAISU-RYOSTYMD
             MOVE  RECE82-KIJYUNYM-KSI TO  LNK-ORCSROUKAISU-KIJYUNYM-KSI
             MOVE  RECE82-KAISU-KSI    TO  LNK-ORCSROUKAISU-KAISU-KSI
      *+++
             MOVE  RECE82-SRYYM        TO  WRK-STYMD (1:6)
             MOVE  "01"                TO  WRK-STYMD (7:2)
      *
             IF    RECE82-RYOSTYMD   NOT =  SPACE
               IF      RECE82-RYOSTYMD     <   WRK-STYMD
                   MOVE   WRK-STYMD(7:2) TO  LNK-ORCSROUKAISU-KAISHIBI
               ELSE
                   MOVE   RECE82-RYOSTYMD(7:2)
                                         TO  LNK-ORCSROUKAISU-KAISHIBI
               END-IF
             ELSE
               PERFORM  VARYING  IDX-ROU  FROM   1   BY  1
                         UNTIL   IDX-ROU   >    31
                   IF  (RECE82-DAY(IDX-ROU)  =   RECE82-HKNCOMBI)
                   OR  (RECE82-DOUJITSU-HKNCOMBI(IDX-ROU)
                                             =   RECE82-HKNCOMBI)
                       MOVE    IDX-ROU   TO  LNK-ORCSROUKAISU-KAISHIBI
                       MOVE    31        TO  IDX-ROU
                   END-IF
               END-PERFORM
             END-IF
      *
             CALL    "ORCSROUKAISU"    USING
                                       LNK-ORCSROUKAISU-REC
                                       SPA-AREA
      *
             IF    LNK-ORCSROUKAISU-RC   =   ZERO
              IF   LNK-ORCSROUKAISU-RECEKAISU  =  1
                MOVE   "1"               TO  RECE82-SYOKAIFLG
              END-IF
              MOVE     LNK-ORCSROUKAISU-RECEKAISU
                                         TO  RECE82-RECEKAISU
      *
              IF   (RECE82-SINKEI          =  "3"         )
              AND  (RECE82-SHOSHINYMD(1:6) =  RECE82-SRYYM)
              AND  (RECE82-KIJYUNYM-KSI    =  RECE82-SRYYM)
              AND  (RECE82-KAISU-KSI       >  1           )
                IF  LNK-ORCSROUKAISU-RECEKAISU  >  RECE82-KAISU-KSI
                  MOVE     "5"           TO  RECE82-SINKEI
                END-IF
              ELSE
                IF  LNK-ORCSROUKAISU-RECEKAISU  >  1
                  MOVE     "5"           TO  RECE82-SINKEI
                END-IF
              END-IF
             END-IF
           END-IF
      *
           REWRITE RECE82-REC
           ADD     1     TO    CNT-OUT
           .
       210-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       2101-RECE88-HENSYU-SEC                   SECTION.
      *                                                                
           INITIALIZE                      WRK-BYOTOKBN-TABLE          
                                           WRK-SHONUM
           MOVE         SPACE       TO     WRK-RYOSTYMD
                                           WRK-RYOEDYMD
           MOVE         SPACE       TO     WRK-EDYMD
      *
           MOVE         SPACE       TO     WK-PTNYUINRRK-TENSTUYMD2
           MOVE         SPACE       TO     WK-PTNYUINRRK-TENNYUYMD
      *
      *    月末日編集
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"             TO     LNK-DAY7-IRAI
           MOVE    RECE82-SRYYM     TO     LNK-DAY7-YM
           CALL    "ORCSDAY"        USING  STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI  TO     WRK-EDYMD
      *
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    RECE82-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE82-PTID         TO  PTNYUINRRK-PTID
           MOVE    RECE82-SRYYM        TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    RECE82-SRYYM        TO  PTNYUINRRK-TAIINYMD (1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK23
           END-IF
           PERFORM                 UNTIL   FLG-PTNYUINRRK23    =   1
               IF      PTNYUINRRK-NYUINYMD (1:6)
                                               <   RECE82-SRYYM
                   MOVE    1                   TO  IDX4
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX4
               END-IF 
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                               >   RECE82-SRYYM
                   MOVE    31                  TO  IDX5
                   IF      IDX5   >   WRK-EDDD
                      MOVE    WRK-EDDD         TO  IDX5
                   END-IF
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX5
               END-IF
      *
               MOVE    ZERO      TO   FLG-RRKOK
      *        保険組合せのチェック（会計に対象の組合せがないときは全て）
               IF      RECE82-DAY-AREA     =   ALL "0"
                   MOVE    "1"   TO   RECE82-NYUINRYOFLG
      *
                   MOVE    RECE82-SRYYM   TO   WRK-RYOSTYMD(1:6)
                   MOVE    IDX4           TO   WRK-RYOSTYMD(7:2)
                   MOVE    RECE82-SRYYM   TO   WRK-RYOEDYMD(1:6)
                   MOVE    IDX5           TO   WRK-RYOEDYMD(7:2)
                   IF   RECE82-RYOSTYMD  =   SPACE
                       MOVE    WRK-RYOSTYMD  TO   RECE82-RYOSTYMD
                   ELSE
                       IF  WRK-RYOSTYMD  <  RECE82-RYOSTYMD
                          MOVE    WRK-RYOSTYMD  TO   RECE82-RYOSTYMD
                       END-IF
                   END-IF
                   IF   RECE82-RYOEDYMD  =   SPACE
                       MOVE    WRK-RYOEDYMD  TO   RECE82-RYOEDYMD
                   ELSE
                       IF  WRK-RYOEDYMD  >  RECE82-RYOEDYMD
                          MOVE    WRK-RYOEDYMD  TO   RECE82-RYOEDYMD
                       END-IF
                   END-IF
      *
                   PERFORM 21011-PTNYUINRRK-HENSYU-SEC
               ELSE
                   PERFORM VARYING IDX FROM    IDX4    BY  1
                           UNTIL   IDX >       IDX5
                       IF    ( IDX             =   IDX4 )
                       AND   ( RECE82-DOUJITSU-HKNCOMBI (IDX)
                                               =
                                               PTNYUINRRK-HKNCOMBINUM )
      *                    同日再入院のとき再入院分の保険組合せをチェック
                           IF      RECE82-DOUJITSU-HKNCOMBI (IDX)
                                                   =   RECE82-HKNCOMBI
                               MOVE    1               TO  FLG-RRKOK
                               PERFORM 21011-PTNYUINRRK-HENSYU-SEC
                               MOVE   IDX5             TO  IDX
                           END-IF    
                       ELSE
                           IF      RECE82-DAY (IDX)
                                                   =   RECE82-HKNCOMBI
                               MOVE    1               TO  FLG-RRKOK
                               PERFORM 21011-PTNYUINRRK-HENSYU-SEC
                               MOVE   IDX5             TO  IDX
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   IF  FLG-RRKOK  =  ZERO
                     PERFORM VARYING IDX FROM  IDX4    BY  1
                             UNTIL   IDX >     IDX5
                        IF  RECE82-DAY(IDX)  NOT =  999
                        AND RECE82-DAY(IDX)  NOT =  RECE82-HKNCOMBI
                           MOVE   998    TO   RECE82-DAY(IDX)
                        END-IF
                     END-PERFORM
                   END-IF
               END-IF        
      *                 
               PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    療養期間置き換え
           IF     RECE82-RYOSTYMD   NOT =   SPACE
             IF  (WRK-TAIHI-RYOSTYMD(1:6) = RECE82-RYOSTYMD(1:6))
             AND (WRK-TAIHI-RYOSTYMD(7:2) > RECE82-RYOSTYMD(7:2))
               MOVE   WRK-TAIHI-RYOSTYMD   TO   RECE82-RYOSTYMD
             END-IF
           END-IF
           IF     RECE82-RYOEDYMD   NOT =   SPACE
             IF  (WRK-TAIHI-RYOEDYMD(1:6) = RECE82-RYOEDYMD(1:6))
             AND (WRK-TAIHI-RYOEDYMD(7:2) < RECE82-RYOEDYMD(7:2))
               MOVE   WRK-TAIHI-RYOEDYMD   TO   RECE82-RYOEDYMD
             END-IF
           END-IF
      *
      *    病棟区分並替え
      *
           PERFORM    VARYING    IDX3    FROM    1    BY    1
                       UNTIL    (IDX3                >   4   )
                         OR     (WRK-BYOTOKBN(IDX3)  =  SPACE)
             COMPUTE    IDX4  =  IDX3  +  1
             PERFORM  VARYING    IDX5    FROM   IDX4  BY    1
                       UNTIL    (IDX5                >   4   )
                         OR     (WRK-BYOTOKBN(IDX5)  =  SPACE)
                IF       WRK-BYOTOKBN(IDX3)  >
                                           WRK-BYOTOKBN(IDX5)
                   MOVE  WRK-BYOTOKBN(IDX3) TO  WRK-BYOTOKBN-DATA
                   MOVE  WRK-BYOTOKBN(IDX5) TO  WRK-BYOTOKBN(IDX3)
                   MOVE  WRK-BYOTOKBN-DATA  TO  WRK-BYOTOKBN(IDX5)
                END-IF
             END-PERFORM
           END-PERFORM
      *
           MOVE    ZERO    TO    IDX4
           PERFORM    VARYING    IDX3    FROM    1    BY    1
                       UNTIL     IDX3     >      4
             IF      RECE82-SRYYM     <=  "200609"
                   MOVE    WRK-BYOTOKBN(IDX3)
                                TO  RECE82-BYOTOKBN(IDX3)
             ELSE
               EVALUATE  WRK-BYOTOKBN(IDX3)
                 WHEN     "01"
                 WHEN     "02"
                 WHEN     "07"
                   ADD     1    TO  IDX4
                   MOVE    WRK-BYOTOKBN(IDX3)
                                TO  RECE82-BYOTOKBN(IDX4)
               END-EVALUATE
             END-IF
           END-PERFORM
      *
           .
       2101-RECE88-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       21011-PTNYUINRRK-HENSYU-SEC           SECTION.
      *
      *    病棟区分セット
           PERFORM    210112-BYOTOKBN-SET-SEC
      *
      *    診療科情報のセット
      *    退院日＜診療年月、転出日の前日＜診療年月のときはセットしない
           IF    ( PTNYUINRRK-TENSTUYMD    =  PTNYUINRRK-TAIINYMD )
           AND   ( PTNYUINRRK-TENNYUYMD    =  PTNYUINRRK-NYUINYMD )
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                           >=  RECE82-SRYYM
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       20
                           OR      PTNYUINRRK-NYUINKA
                                               =   RECE82-SRYKA (IDW)
                       IF      RECE82-SRYKA (IDW)  =   SPACE  
                           MOVE    PTNYUINRRK-NYUINKA
                                               TO  RECE82-SRYKA (IDW)
                           MOVE    20          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF    
           ELSE    
               IF      PTNYUINRRK-TENSTUYMD
                                   NOT =   PTNYUINRRK-TAIINYMD
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE   PTNYUINRRK-TENSTUYMD
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    -1              TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   IF      LNK-DAY6-KEISAN (1:6)
                                           >=  RECE82-SRYYM
                       PERFORM VARYING IDW FROM    1   BY  1
                               UNTIL   IDW >       20
                               OR      PTNYUINRRK-NYUINKA
                                               =   RECE82-SRYKA (IDW)
                           IF      RECE82-SRYKA (IDW)  =   SPACE  
                               MOVE    PTNYUINRRK-NYUINKA
                                                   TO  RECE82-SRYKA(IDW)
                               MOVE    20          TO  IDW
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
               IF      PTNYUINRRK-TENNYUYMD
                                   NOT =   PTNYUINRRK-NYUINYMD
                   IF      PTNYUINRRK-TENNYUYMD (1:6)
                                       <=  RECE82-SRYYM
                       PERFORM VARYING IDW FROM    1   BY  1
                               UNTIL   IDW >       20
                               OR      PTNYUINRRK-NYUINKA
                                               =   RECE82-SRYKA (IDW)
                           IF      RECE82-SRYKA (IDW)  =   SPACE  
                               MOVE    PTNYUINRRK-NYUINKA
                                                   TO  RECE82-SRYKA(IDW)
                               MOVE    20          TO  IDW
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
      *
      *    入院年月日
           IF      RECE82-NYUINYMD    =   SPACE
               MOVE    PTNYUINRRK-NYUINYMD TO  RECE82-NYUINYMD
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
           END-IF
      *
      *    入退院履歴情報
           PERFORM 210111-RECE88-WRITE-SEC
      *
      *    診療月内の入退院情報の履歴
           IF      FLG-RECE88          =   1                            
               MOVE    PTNYUINRRK-REC    TO  WRK-PTNRRK-REC
               INITIALIZE                    PTNYUINRRK-REC
               MOVE    RECE82-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
               MOVE    RECE82-PTID       TO  PTNYUINRRK-PTID
               MOVE    WRK-PTNRRK-SHONUM TO  PTNYUINRRK-SHONUM
               MOVE    RECE82-SRYYM      TO  PTNYUINRRK-NYUINYMD (1:6)
               MOVE    "31"              TO  PTNYUINRRK-NYUINYMD (7:2)
               MOVE    PTNYUINRRK-REC    TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"  TO  MCP-TABLE
               MOVE    "key51"           TO  MCP-PATHNAME
               PERFORM    900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               ELSE
                   MOVE    1             TO  FLG-PTNYUINRRK22 
               END-IF
      *
      *        入院年月日
               IF    ( PTNYUINRRK-NYUINYMD <    RECE82-NYUINYMD )
               AND   ( PTNYUINRRK-SHONUM   =    WRK-SHONUM      )
                   MOVE    PTNYUINRRK-NYUINYMD  TO  RECE82-NYUINYMD
      *
                   IF    ( PTNYUINRRK-JTIKBN        =   "5" )
                   AND   ( PTNYUINRRK-TAINRELKBN    =   "1" )
                       MOVE    "1"                  TO  RECE82-TAINFLG
                   END-IF
               END-IF
      *
               PERFORM             UNTIL   FLG-PTNYUINRRK22 =  1
                   PERFORM 210111-RECE88-WRITE-SEC
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               END-PERFORM
      *                 
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key51"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       21011-PTNYUINRRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴出力処理
      *****************************************************************
       210111-RECE88-WRITE-SEC         SECTION.
      *
      *    他院歴の履歴は記載しない
           IF    ( PTNYUINRRK-JTIKBN        =   "5" )
           AND   ( PTNYUINRRK-TAINRELKBN    =   "1" )
               GO  TO  210111-RECE88-WRITE-EXT
           END-IF
      *
      *
           INITIALIZE                      RECE88-REC
           MOVE    RECE82-HOSPNUM      TO  RECE88-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE88-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE88-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE88-PTID
           MOVE    RECE82-RECEKA       TO  RECE88-RECEKA
           MOVE    RECE82-HKNID        TO  RECE88-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE88-TEKSTYMD
           MOVE    PTNYUINRRK-NYUINYMD TO  RECE88-NYUINYMD
           IF      PTNYUINRRK-NYUINYMD =   PTNYUINRRK-TAIINYMD
               MOVE    0                   TO  RECE88-NYUINYMD-SEQNO
           ELSE 
               IF      PTNYUINRRK-TAIINYMD =   "99999999"
                   MOVE    9                   TO  RECE88-NYUINYMD-SEQNO
               ELSE
                   MOVE    1                   TO  RECE88-NYUINYMD-SEQNO
               END-IF
           END-IF
           MOVE    RECE88-REC          TO  WRK-RECE88-REC          
           PERFORM 900-RECE88-READ-SEC
           IF      FLG-RECE88          =   1
               MOVE    WRK-RECE88-REC      TO  RECE88-REC 
               MOVE    PTNYUINRRK-TAIINYMD TO  RECE88-TAIINYMD
               MOVE    PTNYUINRRK-NYUINCHUKBN
                                           TO  RECE88-NYUINCHUKBN
      *        入院年月日と同じ初回番号のときのみセット
               IF      PTNYUINRRK-SHONUM   =    WRK-SHONUM
                   MOVE    WRK-SHONUM           TO  RECE88-SHONUM
               END-IF
               WRITE   RECE88-REC
               ADD     1                   TO  CNT-RECE88
      *
               IF  FLG-RRKOK  =  1
                 IF  PTNYUINRRK-NYUINYMD(1:6)  <  RECE82-SRYYM
                     MOVE     1                      TO  IDX6
                 ELSE
                     MOVE  PTNYUINRRK-NYUINYMD(7:2)  TO  IDX6
                 END-IF 
                 IF  PTNYUINRRK-TAIINYMD(1:6)  >  RECE82-SRYYM
                     MOVE    31                      TO  IDX7
                     IF      IDX7   >   WRK-EDDD
                        MOVE    WRK-EDDD             TO  IDX7
                     END-IF
                 ELSE
                     MOVE  PTNYUINRRK-TAIINYMD(7:2)  TO  IDX7
                 END-IF
                 PERFORM VARYING IDX8 FROM  IDX6    BY  1
                          UNTIL  IDX8   >   IDX7
                   IF  RECE82-DAY(IDX8)  NOT =   RECE82-HKNCOMBI
                     MOVE   999   TO   RECE82-DAY(IDX8)
                   END-IF
                 END-PERFORM
               END-IF
      *
           END-IF    
           .
       210111-RECE88-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟番号・種別セット                                              
      *****************************************************************
       210112-BYOTOKBN-SET-SEC        SECTION. 
      *
      *    異動による転室等の場合は転出日を前日に置き換える
           MOVE    SPACE     TO     WK-PTNYUINRRK-TENSTUYMD
           IF      PTNYUINRRK-TENSTUYMD  =  PTNYUINRRK-TAIINYMD
             MOVE  PTNYUINRRK-TENSTUYMD  TO  WK-PTNYUINRRK-TENSTUYMD
             IF  (WK-PTNYUINRRK-TENSTUYMD2     NOT =  SPACE  )
             AND (WK-PTNYUINRRK-TENSTUYMD(1:6) NOT = "999999")
             AND (WK-PTNYUINRRK-TENSTUYMD  =
                                     WK-PTNYUINRRK-TENSTUYMD2)
               INITIALIZE                       STS-AREA-DAY
               INITIALIZE                       LNK-DAY6-AREA
               MOVE    "61"                 TO  LNK-DAY6-IRAI
               MOVE   PTNYUINRRK-TENSTUYMD  TO  LNK-DAY6-KIJUN
               MOVE    "1"                  TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                   TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"              USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
               MOVE   LNK-DAY6-KEISAN    TO  WK-PTNYUINRRK-TENSTUYMD
             END-IF
           ELSE
             INITIALIZE                         STS-AREA-DAY
             INITIALIZE                         LNK-DAY6-AREA
             MOVE      "61"                 TO  LNK-DAY6-IRAI
             MOVE     PTNYUINRRK-TENSTUYMD  TO  LNK-DAY6-KIJUN
             MOVE      "1"                  TO  LNK-DAY6-ZOGENPTN
             MOVE      -1                   TO  LNK-DAY6-ZOGEN
             CALL    "ORCSDAY"              USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
             MOVE     LNK-DAY6-KEISAN    TO  WK-PTNYUINRRK-TENSTUYMD
           END-IF
      *                       
      *    病棟情報                                           
           MOVE    SPACE                 TO  SYSKANRI-REC     
           INITIALIZE                        SYSKANRI-REC     
           MOVE    "5001"                TO  SYS-KANRICD      
           MOVE    PTNYUINRRK-BTUNUM     TO  SYS-KBNCD        
           MOVE    PTNYUINRRK-TENNYUYMD  TO  SYS-STYUKYMD
           MOVE    SYS-STYUKYMD          TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM           TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-READ-SEC                      
           IF      FLG-SYSKANRI        =   ZERO               
               MOVE  MCPDATA-REC           TO  SYS-5001-REC     
      *
               IF ( WK-PTNYUINRRK-TENSTUYMD(1:6) <   RECE82-SRYYM  )
               OR ( PTNYUINRRK-TENNYUYMD(1:6)    >   RECE82-SRYYM  )
               OR ((PTNYUINRRK-TENNYUYMD = WK-PTNYUINRRK-TENNYUYMD) AND
                   (WK-PTNYUINRRK-TENNYUYMD  NOT =   SPACE        ))
                 CONTINUE
               ELSE
                 IF  SYS-5001-RECEDSPKBN  =  SPACE
                   CONTINUE
                 ELSE
                   PERFORM  VARYING  IDX3  FROM  1  BY  1
                             UNTIL  (IDX3    >       4              )
                               OR   (WRK-BYOTOKBN(IDX3)
                                             =   SYS-5001-RECEDSPKBN)
                     IF     WRK-BYOTOKBN(IDX3)  =  SPACE
                       MOVE    SYS-5001-RECEDSPKBN
                                       TO   WRK-BYOTOKBN(IDX3)
                       MOVE    4       TO   IDX3
                     END-IF
                   END-PERFORM
                 END-IF
                 MOVE   WK-PTNYUINRRK-TENSTUYMD   TO
                                        WK-PTNYUINRRK-TENSTUYMD2
                 MOVE   PTNYUINRRK-TENNYUYMD      TO
                                        WK-PTNYUINRRK-TENNYUYMD
               END-IF
      *
      *        病棟番号・種別
               MOVE    RECE82-SRYYM        TO  WRK-SRYYYMM
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       31
                   IF      RECE82-BYOTONUM(IDX3)   =  SPACE
                       MOVE    IDX3            TO  WRK-SRYDD
                       IF       PTNYUINRRK-TENNYUYMD  <=  WRK-SRYYMD
                       AND      PTNYUINRRK-TENSTUYMD  >=  WRK-SRYYMD
                           MOVE  SYS-5001-BTU-NUM
                                           TO  RECE82-BYOTONUM    (IDX3)
                           MOVE  SYS-5001-BTU-SBT
                                           TO  RECE82-BYOTOSYUBETU(IDX3)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .                                                           
       210112-BYOTOKBN-SET-EXT.                                               
           EXIT.                                                       
      *
      *****************************************************************
      *    受診歴チェック
      *****************************************************************
       2102-JYURRK-CHECK-SEC              SECTION.
      *
      *    労災準拠の場合、受診歴に４、５のデータがない事
      *    健保準拠の場合、受診歴に１、２のデータがない事
           EVALUATE  RECE82-HKNKBN
             WHEN      "4"
               EVALUATE  RECE82-JIBAIKBN
                 WHEN    "1"
                   MOVE   "4"   TO   WRK-HKNKBN
                 WHEN    "2"
                   MOVE   "2"   TO   WRK-HKNKBN
               END-EVALUATE
             WHEN      "5"
               EVALUATE  RECE82-KOMUKBN
                 WHEN    "1"
                   MOVE   "5"   TO   WRK-HKNKBN
                 WHEN    "2"
                   MOVE   "1"   TO   WRK-HKNKBN
               END-EVALUATE
           END-EVALUATE
      *    受診歴の検索
           INITIALIZE                      JYURRK-REC
           MOVE    RECE82-HOSPNUM      TO  JYURRK-HOSPNUM
           MOVE    RECE82-PTID         TO  JYURRK-PTID
           MOVE    RECE82-NYUGAIKBN    TO  JYURRK-NYUGAIKBN
           MOVE    RECE82-SRYYM        TO  JYURRK-SRYYMD(1:6)
           MOVE    "__"                TO  JYURRK-SRYYMD(7:2)
           MOVE    RECE82-HKNCOMBI     TO  JYURRK-HKNCOMBINUM
           MOVE    WRK-HKNKBN          TO  JYURRK-HKNKBN
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"    TO  MCP-TABLE
               MOVE    "key39"         TO  MCP-PATHNAME
               PERFORM   900-JYURRK-READ-SEC
               IF        FLG-JYURRK    =   ZERO
                   MOVE      1         TO   FLG-JYURRKCHECK
               END-IF
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2102-JYURRK-CHECK-EXT.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
                   RECE88-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE82  CNT-IN  :" CNT-IN
           DISPLAY "RECE88  CNT-OUT :" CNT-RECE88
           DISPLAY "RECE82  CNT-OUT :" CNT-OUT
           DISPLAY "*** ORCR0820 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
           END-READ
           .
       900-RECE82-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴読込
      *****************************************************************
       900-RECE88-READ-SEC            SECTION.
      *
           READ    RECE88-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE88
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE88
           END-READ
           .
       900-RECE88-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２３）
      *****************************************************************
       900-PTNYUINRRK-KEY23-READ-SEC    SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO        TO  FLG-PTNYUINRRK23
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK23
           END-IF
      *
           .
       900-PTNYUINRRK-KEY23-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２２）
      *****************************************************************
       900-PTNYUINRRK-KEY22-READ-SEC    SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO        TO  FLG-PTNYUINRRK22
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN    =   "5"
                   IF      PTNYUINRRK-TAINRELKBN    =   "1"
                       CONTINUE
                   ELSE
                       GO  TO  900-PTNYUINRRK-KEY22-READ-SEC
                   END-IF
               END-IF
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK22
           END-IF
      *
           .
       900-PTNYUINRRK-KEY22-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
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
      *    受診履歴読込
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
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
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
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
