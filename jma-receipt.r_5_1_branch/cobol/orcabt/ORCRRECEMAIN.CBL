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
       PROGRAM-ID.             ORCRRECEMAIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 月次業務　明細書主プロ
      *                    : プレビュー用
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    NACL−竹田　　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *  02.08.02    NACL-藤原    06/03/28  山形県のレセプト様式変更対応
      *
      *  03.00.01    NACL-藤原    06/05/29  印刷時のプリンター指定対応
      *  03.00.01    NACL-竹田    06/06/07  算定履歴取り込み変更
      *
      *  03.03.01    NACL-門脇    06/09/27  ２００６年１０月改正対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  労災・自賠責対応
      *  03.05.03    NACL-藤原    07/06/19  自賠責の様式選択対応
      *                                     （平成１９年４月改正様式）
      *
      *  04.01.01    NACL-藤原    07/10/25  公務災害対応
      *  04.01.02    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *
      *  04.03.01    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.02    NACL-藤原    08/08/07  労災の枠印刷選択機能対応
      *  04.03.03    NACL-藤原    08/08/11  コメント記載箇所選択機能対応
      *
      *  04.05.01    NACL-藤原    11/08/02  労災新様式対応
      *  04.05.02    NACL-藤原    11/10/24  「請求しない」設定の請求管理
      *                                     の削除処理の有無対応
      *
      *  04.06.01    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.02    NACL-門脇    12/10/15  コメントのみの剤のレセプト
      *                                     記載対応
      *
      *  04.07.01    NACL-藤原    12/11/20  リアルタイムプレビュー時の
      *                                     一画面対応
      *  04.07.02    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.03    NACL-藤原    13/12/04  自賠責（労災準拠）のレセプト
      *                                     様式追加対応
      *  04.07.04    NACL-藤原    13/12/09  医療情報連携基盤対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    16/07/20  ＥＦファイル作成対応
      *  04.08.03    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *
      *  05.00.01    NACL-藤原    19/04/16  元号改正対応
      *  05.01.01    NACL-太田    19/07/02  同一端末２重起動チェック追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ２重起動チェック用ファイル
           SELECT  RMDMY01-FILE    ASSIGN  RMDMY01PARA
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-RMDMY01.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RMDMY01-FILE.
       01  RMDMY01-REC             PIC X(01).
      *
       WORKING-STORAGE             SECTION.
      *
       01  STS-AREA.
           03  STS-RMDMY01                             PIC X(02).
      *
           COPY    "CPRECEDAT1.INC".
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFL//
                                   BY         //RMDMY01//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    カウント領域
       01  FLG-AREA.
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SYUMEI                              PIC 9(01).
           03  FLG-SYUTTL                              PIC 9(01).
           03  FLG-JYURRK                              PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-SANTEI                              PIC 9(01).
           03  FLG-SRYKARRK                            PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-PRVKANRI                            PIC 9(01).
           03  FLG-END                                 PIC 9(01).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-A1.
                   07  WRK-PARA-A1-1                   PIC  X(06).
                   07  WRK-PARA-A1-2                   PIC  X(16).
                   07  WRK-PARA-A1-3                   PIC  X(08).
               05  WRK-PARA-A2                         PIC  X(01).
               05  WRK-PARA-A3                         PIC  X(01).
               05  WRK-PARA-A4                         PIC  X(100).
               05  WRK-PARA-A5                         PIC  9(07).
               05  WRK-PARA-A6                         PIC  X(08).
               05  WRK-PARA-A7                         PIC  X(15).
               05  WRK-PARA-A8                         PIC  X(06).
               05  WRK-PARA-A9                         PIC  X(08).
               05  WRK-PARA-A10                        PIC  X(08).
               05  WRK-PARA-A11                        PIC  X(02).
      *
               05  WRK-PARA-A12                        PIC  X(01).
               05  WRK-PARA-A13                        PIC  X(01).
               05  WRK-PARA-A14                        PIC  X(01).
      *
               05  WRK-PARA-A15                        PIC  X(16).
               05  WRK-PARA-A16                        PIC  X(16).
               05  WRK-PARA-A17                        PIC  9(04).
               05  WRK-PARA-A18                        PIC  X(08).
               05  WRK-PARA-A19                        PIC  X(14).
               05  WRK-PARA-A20                        PIC  9(04).
               05  WRK-PARA-A21                        PIC  X(06).
               05  WRK-PARA-A22                        PIC  X(08).
               05  WRK-PARA-A23                        PIC  X(20).
               05  WRK-PARA-A24                        PIC  9(04).
               05  WRK-PARA-A25                        PIC  X(64).
               05  WRK-PARA-A26                        PIC  X(16).
               05  WRK-PARA-A27                        PIC  X(16).
               05  WRK-PARA-A28                        PIC  X(34).
               05  WRK-PARA-A29                        PIC  X(34).
               05  WRK-PARA-A35                        PIC  X(01).
               05  WRK-PARA-A38                        PIC  X(01).
               05  WRK-PARA-A39                        PIC  X(01).
      *
               05  WRK-PARA-PTID                       PIC  9(10).
               05  WRK-PARA-NYUGAIKBN                  PIC  X(01).
               05  WRK-PARA-HKNKBN                     PIC  X(01).
               05  WRK-PARA-SHORI-KBN                  PIC  X(01).
               05  WRK-PARA-HOSPID                     PIC  X(24).
               05  WRK-PARA-HOSPNUM                    PIC  9(02).
               05  WRK-PARA-PRTKBN                     PIC  X(01).
               05  WRK-PARA-GYOUMUKBN                  PIC  X(01).
      *
           03  WRK-PARA0.
               05  WRK-PARA0-A1.
                   07  WRK-PARA0-A1-1                  PIC  X(06).
                   07  WRK-PARA0-A1-2                  PIC  X(16).
                   07  WRK-PARA0-A1-3                  PIC  X(08).
               05  WRK-PARA0-A2                        PIC  X(01).
               05  WRK-PARA0-A3                        PIC  X(01).
               05  WRK-PARA0-A5                        PIC  9(07).
               05  WRK-PARA0-A6                        PIC  X(08).
               05  WRK-PARA0-A7                        PIC  9(10).
               05  WRK-PARA0-A8                        PIC  X(06).
      *
           03  WRK-PARA1.
               05  WRK-PARA1-A1.
                   07  WRK-PARA1-A1-1                  PIC  X(06).
                   07  WRK-PARA1-A1-2                  PIC  X(16).
                   07  WRK-PARA1-A1-3                  PIC  X(08).
               05  WRK-PARA1-A3                        PIC  X(01).
               05  WRK-PARA1-A5                        PIC  9(07).
               05  WRK-PARA1-A6                        PIC  X(08).
           03  WRK-PARA2.
               05  WRK-PARA2-A1.
                   07  WRK-PARA2-A1-1                  PIC  X(06).
                   07  WRK-PARA2-A1-2                  PIC  X(16).
                   07  WRK-PARA2-A1-3                  PIC  X(08).
               05  WRK-PARA2-A5                        PIC  9(07).
               05  WRK-PARA2-A6                        PIC  X(08).
      *
           03  WRK-PARA3.
               05  WRK-PARA3-A9                        PIC  X(08).
               05  WRK-PARA3-A10                       PIC  X(08).
               05  WRK-PARA3-A1.
                   07  WRK-PARA3-A1-1                  PIC  X(06).
                   07  WRK-PARA3-A1-2                  PIC  X(16).
                   07  WRK-PARA3-A1-3                  PIC  X(08).
               05  WRK-PARA3-A2                        PIC  X(01).
               05  WRK-PARA3-A3                        PIC  X(01).
               05  WRK-PARA3-A8                        PIC  X(06).
               05  WRK-PARA3-A5                        PIC  9(07).
               05  WRK-PARA3-A6                        PIC  X(08).
      *
           03  WRK-PARA4.
               05  WRK-PARA4-A9                        PIC  X(08).
               05  WRK-PARA4-A10                       PIC  X(08).
               05  WRK-PARA4-A1.
                   07  WRK-PARA4-A1-1                  PIC  X(06).
                   07  WRK-PARA4-A1-2                  PIC  X(16).
                   07  WRK-PARA4-A1-3                  PIC  X(08).
               05  WRK-PARA4-A3                        PIC  X(01).
               05  WRK-PARA4-A8                        PIC  X(06).
               05  WRK-PARA4-A5                        PIC  9(07).
               05  WRK-PARA4-A6                        PIC  X(08).
      *
           03  WRK-ARG                 PIC X(512).
      *
           03  WRK-PTID-X.
               05  WRK-PTID            PIC 9(10).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *  
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-RC                  PIC 9(08).
      *
        01  WRK-CONS-AREA.
      *    帳票出力用パラメタ  
           03  WRK-CONS-ARG.       
               05  WRK-CONS-ARG-PRTID  PIC X(04)   VALUE   "HC06".
               05  WRK-CONS-ARG-SRYYM  PIC X(06)   VALUE   "999999".
               05  WRK-CONS-ARG-CREYMD PIC X(08).
               05  WRK-CONS-ARG-CREHMS PIC X(06).
               05  WRK-CONS-ARG-PREFKBN
                                       PIC X(01)   VALUE   "0".
               05  WRK-CONS-ARG-TEISYUTUSAKI
                                       PIC X(01)   VALUE   "0".
               05  WRK-CONS-ARG-RECESYUBETU
                                       PIC X(04)   VALUE   "0000".
               05  WRK-CONS-ARG-NYUGAIKBN
                                       PIC X(01).
               05  WRK-CONS-ARG-PRTJYUN
                                       PIC X(02)   VALUE   "05".
      *
           03  WRK-CONS-SRYYM-200609   PIC 9(06)   VALUE   200609.      
      *
           COPY    "MCPAREA".
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
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *    医療機関情報
           COPY  "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納合計マスタ
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    受診履歴マスタ
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    会計付加情報マスタ
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    算定マスタ
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    診療科履歴マスタ
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    患者コメントマスタ
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    プレビュー管理マスタ
           COPY    "CPPRVKANRI.INC".
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ZERO            TO  WRK-RC
      *
           PERFORM 100-INIT-SEC
      *
           OPEN    OUTPUT          RMDMY01-FILE
           IF    ( STS-RMDMY01     NOT =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           ELSE
               DISPLAY "RECEMAIN CANCELED[" STS-RMDMY01 "]"
               MOVE    1           TO  WRK-RC
           END-IF
      *
           CLOSE    RMDMY01-FILE
      *
           MOVE    WRK-RC          TO  RETURN-CODE
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
      *    ＤＢオープン処理
           PERFORM 100-DBOPEN-SEC
           MOVE    ALL "Z"         TO  SPA-TERMID
           STRING  FUNCTION TRIM (MCP-TERM)    DELIMITED BY SPACE
           INTO    SPA-TERMID
           END-STRING
      *
           UNSTRING    COMMAND-PARAM   DELIMITED BY ","
                                   INTO  WRK-PARA-A1
                                         WRK-PARA-A2
                                         WRK-PARA-A3
                                         WRK-PARA-A5
                                         WRK-PARA-A6
                                         WRK-PARA-A7
                                         WRK-PARA-A8
                                         WRK-PARA-A9
                                         WRK-PARA-A10
                                         WRK-PARA-A11
                                         WRK-PARA-A12
                                         WRK-PARA-A13
                                         WRK-PARA-A14
                                         WRK-PARA-A15
                                         WRK-PARA-A16
                                         WRK-PARA-A17
                                         WRK-PARA-A18
                                         WRK-PARA-A19
                                         WRK-PARA-A20
                                         WRK-PARA-A21
                                         WRK-PARA-A22
                                         WRK-PARA-A23
                                         WRK-PARA-A24
                                         WRK-PARA-A25
                                         WRK-PARA-A26
                                         WRK-PARA-A27
                                         WRK-PARA-A28
                                         WRK-PARA-A29
                                         WRK-PARA-PTID
                                         WRK-PARA-NYUGAIKBN
                                         WRK-PARA-HKNKBN
                                         WRK-PARA-SHORI-KBN
                                         WRK-PARA-HOSPNUM
                                         WRK-PARA-A35
                                         WRK-PARA-PRTKBN 
                                         WRK-PARA-GYOUMUKBN
                                         WRK-PARA-A38
                                         WRK-PARA-A39
                                         RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA0-A1
           MOVE     WRK-PARA-A2    TO  WRK-PARA0-A2
           MOVE     WRK-PARA-A3    TO  WRK-PARA0-A3
           MOVE     WRK-PARA-A5    TO  WRK-PARA0-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA0-A6
           MOVE     WRK-PARA-PTID  TO  WRK-PARA0-A7
           MOVE     WRK-PARA-A8    TO  WRK-PARA0-A8
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA1-A1
           MOVE     WRK-PARA-A3    TO  WRK-PARA1-A3
           MOVE     WRK-PARA-A5    TO  WRK-PARA1-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA1-A6
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA2-A1
           MOVE     WRK-PARA-A5    TO  WRK-PARA2-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA2-A6
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA3-A1
           MOVE     WRK-PARA-A2    TO  WRK-PARA3-A2
           MOVE     WRK-PARA-A3    TO  WRK-PARA3-A3
           MOVE     WRK-PARA-A5    TO  WRK-PARA3-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA3-A6
           MOVE     WRK-PARA-A8    TO  WRK-PARA3-A8
           MOVE     WRK-PARA-A9    TO  WRK-PARA3-A9
           MOVE     WRK-PARA-A10   TO  WRK-PARA3-A10
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA4-A1
           MOVE     WRK-PARA-A3    TO  WRK-PARA4-A3
           MOVE     WRK-PARA-A5    TO  WRK-PARA4-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA4-A6
           MOVE     WRK-PARA-A8    TO  WRK-PARA4-A8
           MOVE     WRK-PARA-A9    TO  WRK-PARA4-A9
           MOVE     WRK-PARA-A10   TO  WRK-PARA4-A10
      *
      *    作成日・時間の設定
           MOVE    WRK-PARA-A9     TO  WRK-CONS-ARG-CREYMD
           MOVE    WRK-PARA-A10    TO  WRK-CONS-ARG-CREHMS
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  WRK-CONS-ARG-NYUGAIKBN
      *
           EVALUATE    WRK-PARA-HKNKBN
               WHEN    "0"
                   MOVE    "HC06"          TO  WRK-CONS-ARG-PRTID
               WHEN    "1"
                   MOVE    "HC21"          TO  WRK-CONS-ARG-PRTID
               WHEN    "2"
               WHEN    "4"
                   MOVE    "HC23"          TO  WRK-CONS-ARG-PRTID
               WHEN    "3"
                   MOVE    "HC25"          TO  WRK-CONS-ARG-PRTID
           END-EVALUATE
      *
           MOVE    SPA-HOSPNUM             TO  RECEDAT1PARA-HOSPNUM
           INITIALIZE                          RMDMY01PARA
           STRING  RECEDAT1PARA-BASENAME   DELIMITED   BY  SPACE
                   "RECE_RMDMY01"          DELIMITED   BY  SIZE
                   SPA-TERMID              DELIMITED   BY  SPACE
           INTO    RMDMY01PARA
           END-STRING
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RMDMY01PARA     TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RMDMY01PARA
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      WRK-PARA-SHORI-KBN  NOT  =  ZERO
               PERFORM 20011-REAL-DB-MAKE-SEC
           END-IF
      *
           EVALUATE    WRK-PARA-NYUGAIKBN    ALSO    WRK-PARA-HKNKBN
               WHEN    "1"                   ALSO    "0"
                   PERFORM 2002-IHO-NYUIN-SEC
               WHEN    "2"                   ALSO    "0"
                   PERFORM 2001-IHO-GAIRAI-SEC
               WHEN    "1"                   ALSO    "1"
                   PERFORM 2004-ROUSAI-NYUIN-SEC
               WHEN    "2"                   ALSO    "1"
                   PERFORM 2003-ROUSAI-GAIRAI-SEC
               WHEN    "1"                   ALSO    "2"
               WHEN    "1"                   ALSO    "4"
                   PERFORM 2006-JIBAI-NYUIN-SEC
               WHEN    "2"                   ALSO    "2"
               WHEN    "2"                   ALSO    "4"
                   PERFORM 2005-JIBAI-GAIRAI-SEC
               WHEN    "1"                   ALSO    "3"
                   PERFORM 2008-KOUGAI-NYUIN-SEC
               WHEN    "2"                   ALSO    "3"
                   PERFORM 2007-KOUGAI-GAIRAI-SEC
           END-EVALUATE
      *
           IF      WRK-PARA-SHORI-KBN  NOT  =  ZERO
      *    ＤＢオープン処理(もう少し整理)
                   PERFORM 100-DBOPEN-SEC
                   PERFORM 20011-TMPDB-DELETE-SEC
                   PERFORM 900-DBCOMMIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医保（入院外）処理
      *****************************************************************
       2001-IHO-GAIRAI-SEC                SECTION.
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA0      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0010 start = [" WRK-ARG "]"
           CALL    "ORCR0010"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0020 start = [" WRK-ARG "]"
           CALL    "ORCR0020"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0030 start = [" WRK-ARG "]"
           CALL    "ORCR0030"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0035 start = [" WRK-ARG "]"
           CALL    "ORCR0035"          USING
                                       WRK-ARG
           CALL    "ORCR0038"          USING
                                       WRK-ARG
           CALL    "ORCR0040"          USING
                                       WRK-ARG
           CALL    "ORCR0050"          USING
                                       WRK-ARG
           CALL    "ORCR0055"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           CALL    "ORCR0090"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           CALL    "ORCR0091"          USING
                                       WRK-ARG
      *
      ***  IF      WRK-PARA-A7 NOT =   "NOPG"
      ***      CALL    WRK-PARA-A7     USING
      ***                              WRK-PARA2
      ***  END-IF
      *
           PERFORM 20010-PRVDATA-DEL-SEC
           PERFORM 900-DBDISCONNECT-SEC
      *    前回分削除(存在すれば)
           PERFORM 200101-PRVDATA-DEL-SEC
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA3      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           IF    WRK-PARA-A8     <   "200804"
               DISPLAY "ORCR0104 start = [" WRK-ARG "]"
               CALL    "ORCR0104"          USING
                                           WRK-ARG
      *
               IF    ( WRK-PARA-A11    =   "06"     )
               AND   ( WRK-PARA-A8     <   "200601" )
                   CALL    "ORCRCP060"     USING
                                       WRK-ARG
               END-IF
      *
               IF    ( WRK-PARA-A11    =   "40"                  )
               AND   ( WRK-PARA-A8     <=  WRK-CONS-SRYYM-200609 )
                   CALL    "ORCRCP400"     USING
                                           WRK-ARG
               END-IF
           ELSE
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA3      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ",0,"          DELIMITED   BY  SIZE
                        WRK-PARA-GYOUMUKBN
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0105 start = [" WRK-ARG "]"
               CALL    "ORCR0105"          USING
                                           WRK-ARG
           END-IF
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   "1,"        DELIMITED   BY  SIZE
                    "1,"        DELIMITED   BY  SIZE
                    "%%,"       DELIMITED   BY  SIZE
                    WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,05,"     DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A28  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A29  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0101 start = [" WRK-ARG "]"
           CALL    "ORCR0101"          USING
                                       WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
      **************WRK-PARA-A15  DELIMITED BY  SPACE
      **************","         DELIMITED   BY  SIZE
      **************WRK-PARA-A16  DELIMITED BY  SPACE
      **************","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
      **************WRK-PARA-A28  DELIMITED BY  SPACE
      **************","         DELIMITED   BY  SIZE
      **************WRK-PARA-A29  DELIMITED BY  SPACE
      **************","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "orca"      DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0102 start = [" WRK-ARG "]"
           CALL    "ORCR0102"          USING
                                   WRK-ARG
           .
       2001-IHO-GAIRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医保（入院）処理
      *****************************************************************
       2002-IHO-NYUIN-SEC          SECTION.
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA0      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0600 start = [" WRK-ARG "]"
           CALL    "ORCR0600"          USING
                                       WRK-ARG
           CALL    "ORCR0610"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0620 start = [" WRK-ARG "]"
           CALL    "ORCR0620"          USING
                                   WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0630 start = [" WRK-ARG "]"
           CALL    "ORCR0630"          USING
                                   WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0640 start = [" WRK-ARG "]"
           CALL    "ORCR0640"          USING
                                   WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0650 start = [" WRK-ARG "]"
           CALL    "ORCR0650"          USING
                                   WRK-ARG
      *
           CALL    "ORCR0655"          USING
                                   WRK-ARG
      *
           CALL    "ORCR0660"          USING
                                   WRK-ARG
      *
           CALL    "ORCR0670"          USING
                                   WRK-ARG
      *
           CALL    "ORCR0680"          USING
                                   WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0690 start = [" WRK-ARG "]"
           CALL    "ORCR0690"          USING
                                   WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0700 start = [" WRK-ARG "]"
           CALL    "ORCR0700"          USING
                                   WRK-ARG
      *
      ***  IF      WRK-PARA-A7 NOT =   "NOPG"
      ***      CALL    WRK-PARA-A7     USING
      ***                             WRK-ARG
      ***  END-IF
      *
           PERFORM 20010-PRVDATA-DEL-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           PERFORM 200101-PRVDATA-DEL-SEC
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA3      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0710 start = [" WRK-ARG "]"
           IF      WRK-PARA-A8     <=  WRK-CONS-SRYYM-200609     
               CALL    "ORCR0710"          USING
                                           WRK-ARG
      *
               IF      WRK-PARA-A11    =   "40"
                   CALL    "ORCRCP401"     USING
                                           WRK-ARG
               END-IF
           ELSE    
               IF    WRK-PARA-A8     <   "200804"
                   CALL    "ORCR0720"          USING
                                               WRK-ARG
               ELSE
                   MOVE     SPACE       TO WRK-ARG
                   STRING   WRK-PARA3      DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-PRTKBN
                                           DELIMITED   BY  SIZE
                            ",0,"          DELIMITED   BY  SIZE
                            WRK-PARA-GYOUMUKBN
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-HOSPNUM
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                   END-STRING
                   DISPLAY "ORCR0730 start = [" WRK-ARG "]"
                   CALL    "ORCR0730"          USING
                                               WRK-ARG
               END-IF
           END-IF
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   "1,"        DELIMITED   BY  SIZE
                    "1,"        DELIMITED   BY  SIZE
                    "%%,"       DELIMITED   BY  SIZE
                    WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,05,"     DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A28  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A29  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0101 start = [" WRK-ARG "]"
           CALL    "ORCR0101"          USING
                                   WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
      **************WRK-PARA-A15  DELIMITED BY  SPACE
      **************","         DELIMITED   BY  SIZE
      **************WRK-PARA-A16  DELIMITED BY  SPACE
      **************","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
      **************WRK-PARA-A28  DELIMITED BY  SPACE
      **************","         DELIMITED   BY  SIZE
      **************WRK-PARA-A29  DELIMITED BY  SPACE
      **************","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "orca"      DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0102 start = [" WRK-ARG "]"
           CALL    "ORCR0102"          USING
                                   WRK-ARG
      *
           .
       2002-IHO-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災（入院外）処理
      *****************************************************************
       2003-ROUSAI-GAIRAI-SEC                SECTION.
      *???
           DISPLAY "ROUSAI="   WRK-PARA-A13 " " WRK-PARA-A39
      *???
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA0      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0400 start = [" WRK-ARG "]"
           CALL    "ORCR0400"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0410 start = [" WRK-ARG "]"
           CALL    "ORCR0410"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0420 start = [" WRK-ARG "]"
           CALL    "ORCR0420"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0425 start = [" WRK-ARG "]"
           CALL    "ORCR0425"          USING
                                       WRK-ARG
           CALL    "ORCR0428"          USING
                                       WRK-ARG
           CALL    "ORCR0430"          USING
                                       WRK-ARG
           CALL    "ORCR0435"          USING
                                       WRK-ARG
           CALL    "ORCR0440"          USING
                                       WRK-ARG
           CALL    "ORCR0450"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0460 start = [" WRK-ARG "]"
           CALL    "ORCR0460"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0465 start = [" WRK-ARG "]"
           CALL    "ORCR0465"          USING
                                       WRK-ARG
           CALL    "ORCR0466"          USING
                                       WRK-ARG
           CALL    "ORCR0470"          USING
                                       WRK-ARG
           CALL    "ORCR0468"          USING
                                       WRK-ARG
      *
           PERFORM 20010-PRVDATA-DEL-SEC
           PERFORM 900-DBDISCONNECT-SEC
      *    前回分削除(存在すれば)
           PERFORM 200101-PRVDATA-DEL-SEC
      *
           MOVE     SPACE       TO WRK-ARG
           IF       WRK-PARA-A14       =   "0"
               IF       WRK-PARA-A39       =   "0"
                   STRING   WRK-PARA3      DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A35   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A13   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-PRTKBN
                                           DELIMITED   BY  SIZE
                            ",0,"          DELIMITED   BY  SIZE
                            WRK-PARA-HOSPNUM
                                          DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                   END-STRING
                   DISPLAY "ORCR0480 start = [" WRK-ARG "]"
                   CALL    "ORCR0480"          USING
                                               WRK-ARG
                   STRING   WRK-PARA3      DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A35   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A13   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-PRTKBN
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-HOSPNUM
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                   END-STRING
                   CALL    "ORCR0481"          USING
                                               WRK-ARG
               ELSE
                   STRING   WRK-PARA3      DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A35   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A13   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-PRTKBN
                                           DELIMITED   BY  SIZE
                            ",0,"          DELIMITED   BY  SIZE
                            WRK-PARA-HOSPNUM
                                          DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                  END-STRING
                   DISPLAY "ORCR0504 start = [" WRK-ARG "]"
                   CALL    "ORCR0504"          USING
                                               WRK-ARG
                   STRING   WRK-PARA3      DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A35   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A13   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-PRTKBN
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-HOSPNUM
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                   END-STRING
                   CALL    "ORCR0505"          USING
                                               WRK-ARG
               END-IF
               CALL    "ORCR0482"          USING
                                           WRK-ARG
           ELSE
               STRING   WRK-PARA3      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A35   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ",0,"          DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0501 start = [" WRK-ARG "]"
               CALL    "ORCR0501"          USING
                                           WRK-ARG
               STRING   WRK-PARA3      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A35   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               CALL    "ORCR0502"          USING
                                           WRK-ARG
               CALL    "ORCR0503"          USING
                                           WRK-ARG
           END-IF
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA3      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0483 start = [" WRK-ARG "]"
           CALL    "ORCR0483"          USING
                                       WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,05,"     DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0490 start = [" WRK-ARG "]"
           CALL    "ORCR0490"          USING
                                       WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "orca"      DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0500 start = [" WRK-ARG "]"
           CALL    "ORCR0500"          USING
                                   WRK-ARG
           .
       2003-ROUSAI-GAIRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災（入院）処理
      *****************************************************************
       2004-ROUSAI-NYUIN-SEC                SECTION.
      *???
           DISPLAY "ROUSAI="    WRK-PARA-A13 " " WRK-PARA-A39
      *???
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA0      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0800 start = [" WRK-ARG "]"
           CALL    "ORCR0800"          USING
                                       WRK-ARG
           CALL    "ORCR0810"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0820 start = [" WRK-ARG "]"
           CALL    "ORCR0820"          USING
                                       WRK-ARG
           CALL    "ORCR0830"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0840 start = [" WRK-ARG "]"
           CALL    "ORCR0840"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0850 start = [" WRK-ARG "]"
           CALL    "ORCR0850"          USING
                                       WRK-ARG
           CALL    "ORCR0860"          USING
                                       WRK-ARG
           CALL    "ORCR0870"          USING
                                       WRK-ARG
           CALL    "ORCR0880"          USING
                                       WRK-ARG
           CALL    "ORCR0890"          USING
                                       WRK-ARG
           CALL    "ORCR0900"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0910 start = [" WRK-ARG "]"
           CALL    "ORCR0910"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0920 start = [" WRK-ARG "]"
           CALL    "ORCR0920"          USING
                                       WRK-ARG
           CALL    "ORCR0930"          USING
                                       WRK-ARG
           CALL    "ORCR0940"          USING
                                       WRK-ARG
           CALL    "ORCR0936"          USING
                                       WRK-ARG
      *
           PERFORM 20010-PRVDATA-DEL-SEC
           PERFORM 900-DBDISCONNECT-SEC
      *    前回分削除(存在すれば)
           PERFORM 200101-PRVDATA-DEL-SEC
      *
      *
           IF      WRK-PARA-A8     <=  WRK-CONS-SRYYM-200609     
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA3      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0950 start = [" WRK-ARG "]"
               CALL    "ORCR0950"          USING
                                           WRK-ARG
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA4      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0951 start = [" WRK-ARG "]"
               CALL    "ORCR0951"          USING
                                           WRK-ARG
           ELSE
               IF      WRK-PARA-A14    =   "0"
                   MOVE     SPACE       TO WRK-ARG
                   IF      WRK-PARA-A39    =   "0"
                       STRING   WRK-PARA3      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A35   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A13   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ",0,"          DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                       END-STRING
                       DISPLAY "ORCR0960 start = [" WRK-ARG "]"
                       CALL    "ORCR0960"          USING
                                                   WRK-ARG
                       MOVE     SPACE       TO WRK-ARG
                       STRING   WRK-PARA4      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A35   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A13   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                               INTO    WRK-ARG
                       END-STRING
                       DISPLAY "ORCR0961 start = [" WRK-ARG "]"
                       CALL    "ORCR0961"          USING
                                                   WRK-ARG
                   ELSE
                       STRING   WRK-PARA3      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A35   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A13   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ",0,"          DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                       END-STRING
                       DISPLAY "ORCR0968 start = [" WRK-ARG "]"
                       CALL    "ORCR0968"          USING
                                                   WRK-ARG
                       MOVE     SPACE       TO WRK-ARG
                       STRING   WRK-PARA4      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A35   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A13   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                               INTO    WRK-ARG
                       END-STRING
                       DISPLAY "ORCR0969 start = [" WRK-ARG "]"
                       CALL    "ORCR0969"          USING
                                                   WRK-ARG
                   END-IF
               ELSE
                   MOVE     SPACE       TO WRK-ARG
                   STRING   WRK-PARA3      DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A35   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-PRTKBN
                                           DELIMITED   BY  SIZE
                            ",0,"          DELIMITED   BY  SIZE
                            WRK-PARA-HOSPNUM
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
                   END-STRING
                   DISPLAY "ORCR0963 start = [" WRK-ARG "]"
                   CALL    "ORCR0963"          USING
                                               WRK-ARG
                   MOVE     SPACE       TO WRK-ARG
                   STRING   WRK-PARA4      DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-A35   DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-PRTKBN
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-HOSPNUM
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                   END-STRING
                   DISPLAY "ORCR0964 start = [" WRK-ARG "]"
                   CALL    "ORCR0964"          USING
                                               WRK-ARG
               END-IF    
               MOVE     SPACE       TO WRK-ARG
                   STRING   WRK-PARA4      DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-PRTKBN
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            WRK-PARA-HOSPNUM
                                           DELIMITED   BY  SIZE
                            ","            DELIMITED   BY  SIZE
                            RECEERR        DELIMITED   BY  SPACE
                                           INTO    WRK-ARG
                   END-STRING
               DISPLAY "ORCR0962 start = [" WRK-ARG "]"
               CALL    "ORCR0962"          USING
                                           WRK-ARG
           END-IF
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,05,"     DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0490 start = [" WRK-ARG "]"
           CALL    "ORCR0490"          USING
                                       WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "orca"      DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0500 start = [" WRK-ARG "]"
           CALL    "ORCR0500"          USING
                                   WRK-ARG
           .
       2004-ROUSAI-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責（入院外）処理
      *****************************************************************
       2005-JIBAI-GAIRAI-SEC                SECTION.
      *
      *???
           DISPLAY "JIBAI="
                   WRK-PARA-A3 " " WRK-PARA-A13 " " WRK-PARA-A14 " "
                   WRK-PARA-A38
      *???
           IF      WRK-PARA-HKNKBN =   "4"
               CONTINUE
           ELSE
               MOVE    "5"             TO  WRK-PARA0-A3
                                           WRK-PARA1-A3
                                           WRK-PARA3-A3
           END-IF
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA0      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0400 start = [" WRK-ARG "]"
           CALL    "ORCR0400"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0410 start = [" WRK-ARG "]"
           CALL    "ORCR0410"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0420 start = [" WRK-ARG "]"
           CALL    "ORCR0420"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0425 start = [" WRK-ARG "]"
           CALL    "ORCR0425"          USING
                                       WRK-ARG
           CALL    "ORCR0428"          USING
                                       WRK-ARG
           CALL    "ORCR0430"          USING
                                       WRK-ARG
           CALL    "ORCR0435"          USING
                                       WRK-ARG
           CALL    "ORCR0440"          USING
                                       WRK-ARG
           CALL    "ORCR0450"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0460 start = [" WRK-ARG "]"
           CALL    "ORCR0460"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0470 start = [" WRK-ARG "]"
      *
      *    従来様式
           CALL    "ORCR0470"          USING
                                       WRK-ARG
           CALL    "ORCR0465"          USING
                                       WRK-ARG
           CALL    "ORCR0468"          USING
                                       WRK-ARG
      *
           PERFORM 20010-PRVDATA-DEL-SEC
           PERFORM 900-DBDISCONNECT-SEC
      *    前回分削除(存在すれば)
           PERFORM 200101-PRVDATA-DEL-SEC
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA3      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ",0,"          DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           IF      WRK-PARA-A14        =   "0"
               DISPLAY "ORCR0486 start = [" WRK-ARG "]"
               CALL    "ORCR0486"          USING
                                           WRK-ARG
           ELSE
               DISPLAY "ORCR0489 start = [" WRK-ARG "]"
               CALL    "ORCR0489"          USING
                                           WRK-ARG
           END-IF
      *
      *    労災準拠のとき又は平成２９年７月診療分以降のとき（新様式）
           IF    ( WRK-PARA-A3     =   "1"      )
           OR    ( WRK-PARA0-A8    >=  "201707" )
               PERFORM 100-DBOPEN-SEC
               IF      WRK-PARA-SHORI-KBN  NOT  =  ZERO
                   PERFORM 20011-REAL-DB-MAKE-SEC
               END-IF
      *
               MOVE     "4"            TO  WRK-PARA0-A3
                                           WRK-PARA1-A3
                                           WRK-PARA3-A3
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA0      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0400 start = [" WRK-ARG "]"
               CALL    "ORCR0400"          USING
                                           WRK-ARG
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0410 start = [" WRK-ARG "]"
               CALL    "ORCR0410"          USING
                                           WRK-ARG
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0420 start = [" WRK-ARG "]"
               CALL    "ORCR0420"          USING
                                           WRK-ARG
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0425 start = [" WRK-ARG "]"
               CALL    "ORCR0425"          USING
                                           WRK-ARG
               CALL    "ORCR0428"          USING
                                           WRK-ARG
               CALL    "ORCR0430"          USING
                                           WRK-ARG
               CALL    "ORCR0435"          USING
                                           WRK-ARG
               CALL    "ORCR0440"          USING
                                           WRK-ARG
               CALL    "ORCR0450"          USING
                                           WRK-ARG
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0460 start = [" WRK-ARG "]"
               CALL    "ORCR0460"          USING
                                       WRK-ARG
      *
               IF      WRK-PARA-A38    =   "2"
                   DISPLAY "ORCR0461 start = [" WRK-ARG "]"
                   CALL    "ORCR0461"          USING
                                               WRK-ARG
               END-IF
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0465 start = [" WRK-ARG "]"
               CALL    "ORCR0465"          USING
                                           WRK-ARG
               CALL    "ORCR0467"          USING
                                           WRK-ARG
               CALL    "ORCR0470"          USING
                                           WRK-ARG
      *
               IF      WRK-PARA-A38    =   "2"
                   DISPLAY "ORCR0469 start = [" WRK-ARG "]"
                   CALL    "ORCR0469"          USING
                                               WRK-ARG
               END-IF
      *
               PERFORM 20010-PRVDATA-DEL-SEC
               PERFORM 900-DBDISCONNECT-SEC
      *        前回分削除(存在すれば)
               PERFORM 200101-PRVDATA-DEL-SEC
      *
               EVALUATE    WRK-PARA-A38
                   WHEN    "0"
                       MOVE     SPACE       TO WRK-ARG
                       STRING   WRK-PARA3      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ",0,"          DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                               INTO    WRK-ARG
                       END-STRING
                       IF      WRK-PARA-A13        =   "0"
                           DISPLAY "ORCR0485 start = [" WRK-ARG "]"
                           CALL    "ORCR0485"          USING
                                                       WRK-ARG
                       ELSE
                           DISPLAY "ORCR0487 start = [" WRK-ARG "]"
                           CALL    "ORCR0487"          USING
                                                   WRK-ARG
                       END-IF
                   WHEN    "1"
                       MOVE     SPACE       TO WRK-ARG
                       STRING   WRK-PARA3      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A13   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ",0,"          DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                               INTO    WRK-ARG
                       END-STRING
                       DISPLAY "ORCR0488 start = [" WRK-ARG "]"
                       CALL    "ORCR0488"          USING
                                                   WRK-ARG
                   WHEN    "2"
                       MOVE     SPACE       TO WRK-ARG
                       STRING   WRK-PARA3      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A13   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ",0,"          DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                               INTO    WRK-ARG
                       END-STRING
                       DISPLAY "ORCR0510 start = [" WRK-ARG "]"
                       CALL    "ORCR0510"          USING
                                                   WRK-ARG
               END-EVALUATE
           END-IF
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,05,"     DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0490 start = [" WRK-ARG "]"
           CALL    "ORCR0490"          USING
                                       WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    "4"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "orca"      DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0500 start = [" WRK-ARG "]"
           CALL    "ORCR0500"          USING
                                   WRK-ARG
           .
       2005-JIBAI-GAIRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責（入院）処理
      *****************************************************************
       2006-JIBAI-NYUIN-SEC                SECTION.
      *???
           DISPLAY "JIBAI="
                   WRK-PARA-A3 " " WRK-PARA-A13 " " WRK-PARA-A14 " "
                   WRK-PARA-A38
      *???
           IF      WRK-PARA-HKNKBN =   "4"
               CONTINUE
           ELSE
               MOVE    "5"             TO  WRK-PARA0-A3
                                           WRK-PARA1-A3
                                           WRK-PARA3-A3
           END-IF
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA0      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0800 start = [" WRK-ARG "]"
           CALL    "ORCR0800"          USING
                                       WRK-ARG
           CALL    "ORCR0810"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0820 start = [" WRK-ARG "]"
           CALL    "ORCR0820"          USING
                                       WRK-ARG
           CALL    "ORCR0830"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0840 start = [" WRK-ARG "]"
           CALL    "ORCR0840"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0850 start = [" WRK-ARG "]"
           CALL    "ORCR0850"          USING
                                       WRK-ARG
           CALL    "ORCR0860"          USING
                                       WRK-ARG
           CALL    "ORCR0870"          USING
                                       WRK-ARG
           CALL    "ORCR0880"          USING
                                       WRK-ARG
           CALL    "ORCR0890"          USING
                                       WRK-ARG
           CALL    "ORCR0900"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0910 start = [" WRK-ARG "]"
           CALL    "ORCR0910"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA1      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-A14   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0940 start = [" WRK-ARG "]"
      *    従来様式
           CALL    "ORCR0940"          USING
                                       WRK-ARG
           CALL    "ORCR0920"          USING
                                       WRK-ARG
           CALL    "ORCR0936"          USING
                                       WRK-ARG
      *
           PERFORM 20010-PRVDATA-DEL-SEC
           PERFORM 900-DBDISCONNECT-SEC
      *    前回分削除(存在すれば)
           PERFORM 200101-PRVDATA-DEL-SEC
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA3      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ",0,"          DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           IF      WRK-PARA-A14        =   "0"
               DISPLAY "ORCR0956 start = [" WRK-ARG "]"
               CALL    "ORCR0956"          USING
                                           WRK-ARG
           ELSE
               DISPLAY "ORCR0966 start = [" WRK-ARG "]"
               CALL    "ORCR0966"          USING
                                           WRK-ARG
           END-IF
      *
      *    労災準拠のとき又は平成２９年７月診療分以降のとき（新様式）
           IF    ( WRK-PARA-A3     =   "1"      )
           OR    ( WRK-PARA0-A8    >=  "201707" )
               PERFORM 100-DBOPEN-SEC
               IF      WRK-PARA-SHORI-KBN  NOT  =  ZERO
                   PERFORM 20011-REAL-DB-MAKE-SEC
               END-IF
      *
               MOVE     "4"            TO  WRK-PARA0-A3
                                           WRK-PARA1-A3
                                           WRK-PARA3-A3
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA0      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0800 start = [" WRK-ARG "]"
               CALL    "ORCR0800"          USING
                                           WRK-ARG
               CALL    "ORCR0810"          USING
                                           WRK-ARG
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0820 start = [" WRK-ARG "]"
               CALL    "ORCR0820"          USING
                                           WRK-ARG
               CALL    "ORCR0830"          USING
                                           WRK-ARG
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0840 start = [" WRK-ARG "]"
               CALL    "ORCR0840"          USING
                                           WRK-ARG
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0850 start = [" WRK-ARG "]"
               CALL    "ORCR0850"          USING
                                           WRK-ARG
               CALL    "ORCR0860"          USING
                                           WRK-ARG
               CALL    "ORCR0870"          USING
                                           WRK-ARG
               CALL    "ORCR0880"          USING
                                           WRK-ARG
               CALL    "ORCR0890"          USING
                                           WRK-ARG
               CALL    "ORCR0900"          USING
                                           WRK-ARG
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-PRTKBN
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0910 start = [" WRK-ARG "]"
               CALL    "ORCR0910"          USING
                                           WRK-ARG
      *
               IF      WRK-PARA-A38    =   "2"
                   DISPLAY "ORCR0911 start = [" WRK-ARG "]"
                   CALL    "ORCR0911"          USING
                                               WRK-ARG
               END-IF
      *
               MOVE     SPACE       TO WRK-ARG
               STRING   WRK-PARA1      DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-A38   DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        WRK-PARA-HOSPNUM
                                       DELIMITED   BY  SIZE
                        ","            DELIMITED   BY  SIZE
                        RECEERR        DELIMITED   BY  SPACE
                                       INTO    WRK-ARG
               END-STRING
               DISPLAY "ORCR0920 start = [" WRK-ARG "]"
               CALL    "ORCR0920"          USING
                                           WRK-ARG
               CALL    "ORCR0935"          USING
                                           WRK-ARG
               CALL    "ORCR0940"          USING
                                           WRK-ARG
      *
               IF      WRK-PARA-A38    =   "2"
                   DISPLAY "ORCR0937 start = [" WRK-ARG "]"
                   CALL    "ORCR0937"          USING
                                               WRK-ARG
               END-IF
      *
               PERFORM 20010-PRVDATA-DEL-SEC
               PERFORM 900-DBDISCONNECT-SEC
      *        前回分削除(存在すれば)
               PERFORM 200101-PRVDATA-DEL-SEC
      *
      *
               EVALUATE    WRK-PARA-A38
                   WHEN    "0"
                       MOVE     SPACE       TO WRK-ARG
                       STRING   WRK-PARA3      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ",0,"          DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                               INTO    WRK-ARG
                       END-STRING
                       IF      WRK-PARA-A13        =   "0"
                           DISPLAY "ORCR0955 start = [" WRK-ARG "]"
                           CALL    "ORCR0955"          USING
                                                       WRK-ARG
                       ELSE
                           DISPLAY "ORCR0957 start = [" WRK-ARG "]"
                           CALL    "ORCR0957"          USING
                                                       WRK-ARG
                       END-IF
                  WHEN    "1"
                       MOVE     SPACE       TO WRK-ARG
                       STRING   WRK-PARA3      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A13   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ",0,"          DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                               INTO    WRK-ARG
                       END-STRING
                       DISPLAY "ORCR0965 start = [" WRK-ARG "]"
                       CALL    "ORCR0965"          USING
                                                   WRK-ARG
                  WHEN    "2"
                       MOVE     SPACE       TO WRK-ARG
                       STRING   WRK-PARA3      DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-A13   DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                WRK-PARA-PRTKBN
                                               DELIMITED   BY  SIZE
                                ",0,"          DELIMITED   BY  SIZE
                                WRK-PARA-HOSPNUM
                                               DELIMITED   BY  SIZE
                                ","            DELIMITED   BY  SIZE
                                RECEERR        DELIMITED   BY  SPACE
                                               INTO    WRK-ARG
                       END-STRING
                       DISPLAY "ORCR0967 start = [" WRK-ARG "]"
                       CALL    "ORCR0967"          USING
                                                   WRK-ARG
               END-EVALUATE
           END-IF
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,05,"     DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0490 start = [" WRK-ARG "]"
           CALL    "ORCR0490"          USING
                                       WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                     ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    "4"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "orca"      DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0500 start = [" WRK-ARG "]"
           CALL    "ORCR0500"          USING
                                   WRK-ARG
           .
       2006-JIBAI-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公害（入院外）処理
      *****************************************************************
       2007-KOUGAI-GAIRAI-SEC                SECTION.
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA0      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1000 start = [" WRK-ARG "]"
           CALL    "ORCR1000"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1010 start = [" WRK-ARG "]"
           CALL    "ORCR1010"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1020 start = [" WRK-ARG "]"
           CALL    "ORCR1020"          USING
                                       WRK-ARG
           CALL    "ORCR1030"          USING
                                       WRK-ARG
           CALL    "ORCR1040"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1050 start = [" WRK-ARG "]"
           CALL    "ORCR1050"          USING
                                       WRK-ARG
      *
           PERFORM 20010-PRVDATA-DEL-SEC
           PERFORM 900-DBDISCONNECT-SEC
      *    前回分削除(存在すれば)
           PERFORM 200101-PRVDATA-DEL-SEC
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA3      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ",0,"          DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1060 start = [" WRK-ARG "]"
           CALL    "ORCR1060"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,05,"     DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0490 start = [" WRK-ARG "]"
           CALL    "ORCR0490"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                     ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    "4"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "orca"      DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0500 start = [" WRK-ARG "]"
           CALL    "ORCR0500"          USING
                                   WRK-ARG
           .
       2007-KOUGAI-GAIRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公害（入院）処理
      *****************************************************************
       2008-KOUGAI-NYUIN-SEC          SECTION.
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA0      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0600 start = [" WRK-ARG "]"
           CALL    "ORCR0600"          USING
                                       WRK-ARG
      *
           CALL    "ORCR1200"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1210 start = [" WRK-ARG "]"
           CALL    "ORCR1210"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1220 start = [" WRK-ARG "]"
           CALL    "ORCR1220"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1230 start = [" WRK-ARG "]"
           CALL    "ORCR1230"          USING
                                       WRK-ARG
           CALL    "ORCR1240"          USING
                                       WRK-ARG
           CALL    "ORCR1250"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA2      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1260 start = [" WRK-ARG "]"
           CALL    "ORCR1260"          USING
                                       WRK-ARG
      *
           PERFORM 20010-PRVDATA-DEL-SEC
           PERFORM 900-DBDISCONNECT-SEC
      *    前回分削除(存在すれば)
           PERFORM 200101-PRVDATA-DEL-SEC
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA3      DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    WRK-PARA-PRTKBN
                                   DELIMITED   BY  SIZE
                    ",0,"          DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                   DELIMITED   BY  SIZE
                    ","            DELIMITED   BY  SIZE
                    RECEERR        DELIMITED   BY  SPACE
                                   INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR1270 start = [" WRK-ARG "]"
           CALL    "ORCR1270"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,05,"     DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0490 start = [" WRK-ARG "]"
           CALL    "ORCR0490"          USING
                                       WRK-ARG
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                     ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27 DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    "4"         DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-HOSPNUM
                                DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    RECEERR     DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "orca"      DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
           DISPLAY "ORCR0500 start = [" WRK-ARG "]"
           CALL    "ORCR0500"          USING
                                   WRK-ARG
      *
           .
       2008-KOUGAI-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回分削除(存在すれば)
      *****************************************************************
       20010-PRVDATA-DEL-SEC         SECTION.
      *
           INITIALIZE                  PRVKANRI-REC
           MOVE    "RECEPTX"           TO  PRVKANRI-TBL-KEY
           MOVE    WRK-PARA-PTID       TO  WRK-PTID-X
           MOVE    WRK-PTID            TO  PRVKANRI-TBL-GROUP
grpsys     MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC             =   ZERO
               MOVE    "tbl_prvkanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-PRVKANRI-FET-SEC
           ELSE
               MOVE    1               TO  FLG-PRVKANRI
           END-IF
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       20010-PRVDATA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回分削除(存在すれば)
      *****************************************************************
       200101-PRVDATA-DEL-SEC        SECTION.
      *
           IF      FLG-PRVKANRI         =   ZERO
               DISPLAY "RECEMAIN PRV EXIST !!"
               DISPLAY "PAGE = [" PRVKANRI-PAGE "]"
      *        MOVE    PRVKANRI-PAGE   TO  MAX-PAGE-CNT
               PERFORM 100-DBOPEN-SEC
               MOVE    WRK-PARA-A8     TO  WRK-SRYYM
      *
               CALL    "ORCSPRVPSDEL"   USING
grpsys                                  WRK-PARA-HOSPNUM
                                        WRK-PTID
                                        WRK-SRYYM
                                        WRK-PARA-HKNKBN
grpsys                                  SPA-AREA
      *
      *
               PERFORM 900-DBCOMMIT-SEC
           ELSE
               DISPLAY "RECEMAIM PRV NO DATA !!"
           END-IF
      *
           .
       20010-PRVDATA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(収納)
      *****************************************************************
       20011-SYUNOU-DB-MAKE-SEC      SECTION.
           EVALUATE    SYU-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    "tbl_syunou" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    "tbl_syunou" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    "tbl_syunou" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-SYUNOU-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(収納明細)
      *****************************************************************
       20011-SYUMEI-DB-MAKE-SEC      SECTION.
           EVALUATE    SMEI-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    "tbl_syumei" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    "tbl_syumei" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    "tbl_syumei" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-SYUMEI-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(収納トータル)
      *****************************************************************
       20011-SYUTOTAL-DB-MAKE-SEC      SECTION.
           EVALUATE    SYUTTL-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    "tbl_syutotal"
                                        TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    "tbl_syutotal"
                                        TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    "tbl_syutotal"
                                        TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-SYUTOTAL-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(会計)
      *****************************************************************
       20011-SRYACCT-DB-MAKE-SEC      SECTION.
           EVALUATE    ACCT-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    "tbl_sryacct"
                                        TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *    
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
      *---> test   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    "tbl_sryacct"
                                        TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *    
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    "tbl_sryacct"
                                        TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-SRYACCT-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(受診履歴)
      *****************************************************************
       20011-JYURRK-DB-MAKE-SEC      SECTION.
           EVALUATE    JYURRK-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    "tbl_jyurrk" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *    
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    "tbl_jyurrk" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *    
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    "tbl_jyurrk" TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-JYURRK-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(患者コメント)
      *****************************************************************
       20011-PTCOM-DB-MAKE-SEC      SECTION.
           EVALUATE    PTCOM-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    "tbl_ptcom"  TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *    
               WHEN    "999del11"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    "tbl_ptcom"  TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *    
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    "tbl_ptcom"  TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
      *    
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    "tbl_ptcom"  TO      MCP-TABLE
                   MOVE    "key"        TO      MCP-PATHNAME
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-PTCOM-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理
      *****************************************************************
       20011-REAL-DB-DELETE-SEC      SECTION.
      *受診履歴削除
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      JYURRK-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      JYURRK-PTID
           MOVE    WRK-PARA-A8          TO      WRK-SRYYM
           MOVE    "% "                 TO      WRK-SRYDD
           MOVE    WRK-SRYYMD           TO      JYURRK-SRYYMD
           MOVE    JYURRK-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_jyurrk"         TO      MCP-TABLE
           MOVE    "del2"               TO      MCP-PATHNAME
           DISPLAY "take jyurrk  del "
                                WRK-PARA-HOSPNUM WRK-PTID
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *診療会計削除
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      ACCT-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      ACCT-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      ACCT-PTID
           MOVE    WRK-PARA-A8          TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "del3"               TO      MCP-PATHNAME
           DISPLAY "take acct  del "
                                WRK-PARA-HOSPNUM WRK-PTID
grpsys     PERFORM 900-ORCDBMAIN-SEC
           PERFORM 200111-REAL-SRYACY-DB-DELETE2-SEC
      *算定履歴削除
           DISPLAY "real db(santei) del ed  "
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SANTEI-HOSPNUM
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SANTEI-PTID
           MOVE    WRK-PARA-A8          TO      SANTEI-SRYYM
           MOVE    SANTEI-REC           TO      MCPDATA-REC
           MOVE    "tbl_santei"         TO  MCP-TABLE
           MOVE    "del1"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"           TO      MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       20011-REAL-DB-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理
      *****************************************************************
       200111-REAL-SRYACY-DB-DELETE2-SEC   SECTION.
      *
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SRY-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      SRY-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SRY-PTID
           MOVE    WRK-PARA-A8          TO      SRY-SRYYM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           DISPLAY "take real act  select "
                                WRK-PARA-HOSPNUM WRK-PTID SRY-SRYYM
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
           DISPLAY "take act select ok   "
               MOVE    "tbl_sryact"         TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM  900-ACTPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SRYACT         =    1)
           DISPLAY "take act delete loop   "
                   MOVE    SRYACT-REC  TO      MCPDATA-REC
                   MOVE    "DBDELETE"  TO      MCP-FUNC
                   MOVE    "tbl_sryact"         TO  MCP-TABLE
                   MOVE    "key"                TO  MCP-PATHNAME
                   PERFORM  900-REALDB-INS-SEC
      *
                   MOVE    "tbl_sryact"         TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM  900-ACTPRV-READ-SEC
               END-PERFORM
           END-IF 
      *
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-REAL-SRYACY-DB-DELETE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理
      *****************************************************************
       20011-REAL-DB-MAKE-SEC        SECTION.
           PERFORM 20011-REAL-DB-DELETE-SEC
      *収納マスタ関係は、負担金を捨てるということで一時コメントアウト
      *※ 但し、収納金額を見て負担者番号の出し入れをしているのも無視
      *   される。(どうする)
      *収納マスタ
      **   MOVE    WRK-PARA-HOSPNUM      TO      SYU-HOSPNUM
      **   MOVE    WRK-PARA-NYUGAIKBN   TO      SYU-NYUGAIKBN
      **   MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
      **   MOVE    WRK-PTID             TO      SYU-PTID
      **   MOVE    SYUNOU-REC           TO      MCPDATA-REC
      **   MOVE    "DBSELECT"           TO      MCP-FUNC
      **   MOVE    PATH-TBL-SYUNOU-PRV-KEY2
      **                                TO      MCP-PATH
      **   DISPLAY "take syunou read "
      **                        WRK-PARA-HOSPNUM WRK-PTID
      **   CALL    "MCPSUB"             USING
      **                                MCPAREA
      **                                MCPDATA-REC
      **   IF      MCP-RC               =       ZERO
      **       MOVE    PATH-TBL-SYUNOU-PRV-KEY2
      **                                TO      MCP-PATH
      **       PERFORM  900-SYUNOUPRV-READ-SEC
      **       PERFORM  UNTIL
      **                (FLG-SYUNOU             =    1)
      **           MOVE    SYUNOU-REC   TO      MCPDATA-REC
      **           PERFORM 20011-SYUNOU-DB-MAKE-SEC
      **           MOVE    PATH-TBL-SYUNOU-PRV-KEY2
      **                                TO      MCP-PATH
      **           PERFORM  900-SYUNOUPRV-READ-SEC
      **       END-PERFORM
      **   ELSE
      **                DISPLAY "take syunou read err  "
      **                        WRK-PARA-HOSPNUM WRK-PTID
      **   END-IF
      *
      *--------------------------------------------------
      *収納明細マスタ
      **   MOVE    WRK-PARA-HOSPNUM      TO      SMEI-HOSPNUM
      **   MOVE    WRK-PARA-NYUGAIKBN   TO      SMEI-NYUGAIKBN
      **   MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
      **   MOVE    WRK-PTID             TO      SMEI-PTID
      **   MOVE    SYUMEI-REC           TO      MCPDATA-REC
      **   MOVE    "DBSELECT"           TO      MCP-FUNC
      **   MOVE    PATH-TBL-SYUMEI-PRV-KEY2
      **                                TO      MCP-PATH
      **   DISPLAY "take syumei read  "
      **                        WRK-PARA-HOSPNUM WRK-PTID
      **   CALL    "MCPSUB"             USING
      **                                MCPAREA
      **                                MCPDATA-REC
      **   IF      MCP-RC               =       ZERO
      **       MOVE    PATH-TBL-SYUMEI-PRV-KEY2
      **                                TO      MCP-PATH
      **       PERFORM  900-SYUMEIPRV-READ-SEC
      **       PERFORM  UNTIL
      **                (FLG-SYUMEI             =    1)
      **           MOVE    SYUMEI-REC   TO      MCPDATA-REC
      **           PERFORM 20011-SYUMEI-DB-MAKE-SEC
      **           MOVE    PATH-TBL-SYUMEI-PRV-KEY2
      **                                TO      MCP-PATH
      **           PERFORM  900-SYUMEIPRV-READ-SEC
      **       END-PERFORM
      **   ELSE
      **                DISPLAY "take syumei read err  "
      **                        WRK-PARA-HOSPNUM WRK-PTID
      **   END-IF
      *
      *--------------------------------------------------
      *--------------------------------------------------
      *収納トータルマスタ
      **   MOVE    WRK-PARA-HOSPNUM      TO      SYUTTL-HOSPNUM
      **   MOVE    WRK-PARA-NYUGAIKBN   TO      SYUTTL-NYUGAIKBN
      **   MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
      **   MOVE    WRK-PTID             TO      SYUTTL-PTID
      **   MOVE    SYUTTL-REC           TO      MCPDATA-REC
      **   MOVE    "DBSELECT"           TO      MCP-FUNC
      **   MOVE    PATH-TBL-SYUTOTAL-PRV-KEY2
      **                                TO      MCP-PATH
      **   DISPLAY "take syuttl read  "
      **                        WRK-PARA-HOSPNUM WRK-PTID
      **   CALL    "MCPSUB"             USING
      **                                MCPAREA
      **                                MCPDATA-REC
      **   IF      MCP-RC               =       ZERO
      **       MOVE    PATH-TBL-SYUTOTAL-PRV-KEY2
      **                                TO      MCP-PATH
      **       PERFORM  900-SYUTTLPRV-READ-SEC
      **       PERFORM  UNTIL
      **                (FLG-SYUTTL             =    1)
      **           MOVE    SYUTTL-REC   TO      MCPDATA-REC
      **           PERFORM 20011-SYUMEI-DB-MAKE-SEC
      **           MOVE    PATH-TBL-SYUTOTAL-PRV-KEY2
      **                                TO      MCP-PATH
      **           PERFORM  900-SYUTTLPRV-READ-SEC
      **       END-PERFORM
      **   ELSE
      **                DISPLAY "take syuttl read err  "
      **                        WRK-PARA-HOSPNUM WRK-PTID
      **   END-IF
      *
      *--------------------------------------------------
      *会計マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      ACCT-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      ACCT-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      ACCT-PTID
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           DISPLAY "take acct read   "
                                WRK-PARA-HOSPNUM WRK-PTID
           MOVE    "tbl_sryacct_prv"    TO  MCP-TABLE
           MOVE    "key40"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct_prv"    TO  MCP-TABLE
               MOVE    "key40"              TO  MCP-PATHNAME
               PERFORM  900-ACCTPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SRYACCT         =    1)
           DISPLAY "take acct insert   "
                   MOVE    SRYACCT-REC  TO      MCPDATA-REC
                   PERFORM 20011-SRYACCT-DB-MAKE-SEC
      * 
                   MOVE    "tbl_sryacct_prv"    TO  MCP-TABLE
                   MOVE    "key40"              TO  MCP-PATHNAME
                   PERFORM  900-ACCTPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take acct read err  "
                                WRK-PARA-HOSPNUM WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "tbl_sryacct_prv"    TO  MCP-TABLE
           MOVE    "key40"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *--------------------------------------------------
      *診療行為マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SRY-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      SRY-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SRY-PTID
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           DISPLAY "take act read   "
                                WRK-PARA-HOSPNUM WRK-PTID
           MOVE    "tbl_sryact_prv"     TO  MCP-TABLE
           MOVE    "key12"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact_prv"     TO  MCP-TABLE
               MOVE    "key12"              TO  MCP-PATHNAME
               PERFORM  900-ACTPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SRYACT         =    1)
           DISPLAY "take act insert   "
                   MOVE    SRYACT-REC  TO      MCPDATA-REC
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    "tbl_sryact"         TO  MCP-TABLE
                   MOVE    "key"                TO  MCP-PATHNAME
                   PERFORM  900-REALDB-INS-SEC
      *
                   MOVE    "tbl_sryact_prv"     TO  MCP-TABLE
                   MOVE    "key12"              TO  MCP-PATHNAME
                   PERFORM  900-ACTPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take act read err  "
                                WRK-PARA-HOSPNUM WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "tbl_sryact_prv"     TO  MCP-TABLE
           MOVE    "key12"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *--------------------------------------------------
      *算定履歴マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SANTEI-HOSPNUM
      *    MOVE    WRK-PARA-NYUGAIKBN   TO      SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO      SANTEI-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SANTEI-PTID
           MOVE    SANTEI-REC           TO      MCPDATA-REC
           DISPLAY "take santei read   "
                                WRK-PARA-HOSPNUM WRK-PTID
           MOVE    "tbl_santei_prv"     TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_santei_prv"     TO  MCP-TABLE
               MOVE    "key2"               TO  MCP-PATHNAME
               PERFORM  900-SANTEIPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SANTEI         =    1)
                   MOVE    SANTEI-REC   TO      MCPDATA-REC
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    "tbl_santei" TO  MCP-TABLE
                   MOVE    "key"        TO  MCP-PATHNAME
                   PERFORM  900-REALDB-INS-SEC
                   DISPLAY "take santei insert "
      *
                MOVE    "tbl_santei_prv"     TO  MCP-TABLE
                MOVE    "key2"               TO  MCP-PATHNAME
                PERFORM  900-SANTEIPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take santei read err  "
                                WRK-PARA-HOSPNUM WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "tbl_santei_prv"     TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *--------------------------------------------------
      *受診履歴マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      JYURRK-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      JYURRK-PTID
           MOVE    JYURRK-REC           TO      MCPDATA-REC
           DISPLAY "take jyurrk read   "
                                WRK-PARA-HOSPNUM WRK-PTID
           MOVE    "tbl_jyurrk_prv"     TO  MCP-TABLE
           MOVE    "key30"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_jyurrk_prv"     TO  MCP-TABLE
               MOVE    "key30"              TO  MCP-PATHNAME
               PERFORM  900-JYURRKPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-JYURRK         =    1)
           DISPLAY "take jyurrk insert   "
                   MOVE    JYURRK-REC   TO      MCPDATA-REC
                   PERFORM  20011-JYURRK-DB-MAKE-SEC
      *
                   MOVE    "tbl_jyurrk_prv"     TO  MCP-TABLE
                   MOVE    "key30"              TO  MCP-PATHNAME
                   PERFORM  900-JYURRKPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take jyurrk read err  "
                                WRK-PARA-HOSPNUM WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "tbl_jyurrk_prv"     TO  MCP-TABLE
           MOVE    "key30"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *--------------------------------------------------
      *診療科履歴マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      KARRK-HOSPNUM
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      KARRK-PTID
           MOVE    SRYKARRK-REC         TO      MCPDATA-REC
           DISPLAY "take srykarrk read   "
                                WRK-PARA-HOSPNUM WRK-PTID
           MOVE    "tbl_srykarrk_prv"   TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_srykarrk_prv"   TO  MCP-TABLE
               MOVE    "key2"               TO  MCP-PATHNAME
               PERFORM  900-SRYKARRKPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SRYKARRK      =    1)
grpsys*            MOVE SPA-HOSPNUM TO -HOSPNUM
                   MOVE    SRYKARRK-REC TO      MCPDATA-REC
                   IF  KARRK-CREYMD         =       "99999999"
                       MOVE    "DBDELETE"   TO      MCP-FUNC
                   ELSE
                       IF  KARRK-CREYMD     =       "88888888"
                           MOVE    "DBUPDATE"   TO      MCP-FUNC
                       ELSE
                           MOVE    "DBINSERT"   TO      MCP-FUNC
                       END-IF
                   END-IF
                   MOVE    "tbl_srykarrk"       TO  MCP-TABLE
                   MOVE    "key"                TO  MCP-PATHNAME
                   PERFORM  900-REALDB-INS-SEC
      *
                   MOVE    "tbl_srykarrk_prv"   TO  MCP-TABLE
                   MOVE    "key2"               TO  MCP-PATHNAME
                   PERFORM  900-SRYKARRKPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take srykarrk read err  "
                                WRK-PARA-HOSPNUM WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "tbl_srykarrk_prv"   TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *--------------------------------------------------
      *患者コメントマスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      PTCOM-HOSPNUM
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      PTCOM-PTID
           MOVE    PTCOM-REC            TO      MCPDATA-REC
           DISPLAY "take ptcom read   "
                                WRK-PARA-HOSPNUM WRK-PTID
           MOVE    "tbl_ptcom_prv"      TO  MCP-TABLE
           MOVE    "key3"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           MOVE    ZERO                 TO      WRK-ZAINUM
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_ptcom_prv"      TO  MCP-TABLE
               MOVE    "key3"               TO  MCP-PATHNAME
               PERFORM  900-PTCOMPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-PTCOM      =    1)
                   IF  WRK-ZAINUM       NOT =       PTCOM-ZAINUM
                       DISPLAY "take ptcom org del start = "
                                                PTCOM-ZAINUM
                       MOVE    PTCOM-ZAINUM TO      WRK-ZAINUM
                       MOVE    PTCOM-REC    TO      MCPDATA-REC
                       MOVE    "DBDELETE"   TO      MCP-FUNC
                       MOVE    "tbl_ptcom"  TO  MCP-TABLE
                       MOVE    "del11"      TO  MCP-PATHNAME
                       PERFORM  900-REALDB-INS-SEC
                       IF      MCP-RC       NOT  =  ZERO
                           DISPLAY "take ptcom del er " PTCOM-REC(1:100)
                       END-IF
                   END-IF
                   MOVE    PTCOM-REC    TO      MCPDATA-REC
                   IF  PTCOM-CREYMD         =       "99999999"
                       MOVE    "DBDELETE"   TO      MCP-FUNC
                   ELSE
                       IF  PTCOM-CREYMD     =       "88888888"
                           MOVE    "DBUPDATE"   TO      MCP-FUNC
                       ELSE
                           MOVE    "DBINSERT"   TO      MCP-FUNC
                       END-IF
                   END-IF
                   MOVE    "tbl_ptcom"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM  900-REALDB-INS-SEC
                   IF      MCP-RC       NOT  =  ZERO
                       DISPLAY "take ptcom ins er " PTCOM-REC(1:100)
                   END-IF
      *
                   MOVE    "tbl_ptcom_prv"      TO  MCP-TABLE
                   MOVE    "key3"               TO  MCP-PATHNAME
                   PERFORM  900-PTCOMPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take ptcom read err  "
                                WRK-PARA-HOSPNUM WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "tbl_ptcom_prv"      TO  MCP-TABLE
           MOVE    "key3"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *--------------------------------------------------
      *
           .
       20011-REAL-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       900-SYUNOUPRV-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOUPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細読込
      *****************************************************************
       900-SYUMEIPRV-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
               MOVE    ZERO            TO  FLG-SYUMEI
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEIPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納トータル読込
      *****************************************************************
       900-SYUTTLPRV-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUTTL-REC
               MOVE    ZERO            TO  FLG-SYUTTL
           ELSE
               INITIALIZE                  SYUTTL-REC
               MOVE    1               TO  FLG-SYUTTL
           END-IF
      *
           .
       900-SYUTTLPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計読込
      *****************************************************************
       900-ACCTPRV-READ-SEC            SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-ACCTPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-ACTPRV-READ-SEC             SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-ACTPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       900-JYURRKPRV-READ-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRKPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴読込
      *****************************************************************
       900-SANTEIPRV-READ-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
       900-SANTEIPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴読込
      *****************************************************************
       900-SRYKARRKPRV-READ-SEC        SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYKARRK-REC
               MOVE    ZERO            TO  FLG-SRYKARRK
           ELSE
               INITIALIZE                  SRYKARRK-REC
               MOVE    1               TO  FLG-SRYKARRK
           END-IF
      *
           .
       900-SRYKARRKPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント読込
      *****************************************************************
       900-PTCOMPRV-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTCOM-REC
               MOVE    ZERO            TO  FLG-PTCOM 
           ELSE
               INITIALIZE                  PTCOM-REC
               MOVE    1               TO  FLG-PTCOM
           END-IF
      *
           .
       900-PTCOMPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    本体DB更新処理
      *****************************************************************
       900-REALDB-INS-SEC            SECTION.
      *
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC         NOT  =   ZERO
               INITIALIZE                  MCPDATA-REC
               DISPLAY "INS ERR = " MCP-RC
           END-IF
      *
           .
       900-REALDB-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    テンポラリDB削除処理
      *****************************************************************
       20011-TMPDB-DELETE-SEC        SECTION.
           DISPLAY "take tmp db delete st"
      *収納マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SYU-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      SYU-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SYU-PTID
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           DISPLAY "take tmp db delete syunou st = " SYU-PTID "," 
                                           SYU-HOSPNUM ","
                                           SYU-NYUGAIKBN
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_syunou_prv"     TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           PERFORM  900-REALDB-INS-SEC
      *
      *収納明細マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SMEI-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      SMEI-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SMEI-PTID
           MOVE    SYUMEI-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_syumei_prv"     TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           DISPLAY "take tmp db delete syumei st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *収納トータルマスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SYUTTL-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      SYUTTL-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SYUTTL-PTID
           MOVE    SYUTTL-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           DISPLAY "take tmp db delete syutotal st = " SYUTTL-PTID "," 
           MOVE    "tbl_syutotal_prv"   TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           PERFORM  900-REALDB-INS-SEC
      *
      *会計マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      ACCT-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      ACCT-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      ACCT-PTID
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_sryacct_prv"    TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           DISPLAY "take tmp db delete acct st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *診療行為マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SRY-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      SRY-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SRY-PTID
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_sryact_prv"     TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           DISPLAY "take tmp db delete act st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *算定履歴マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      SANTEI-HOSPNUM
      *    MOVE    WRK-PARA-NYUGAIKBN   TO      SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO      SANTEI-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SANTEI-PTID
           MOVE    SANTEI-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_santei_prv"     TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           DISPLAY "take tmp db delete santei st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *受診履歴マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      JYURRK-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      JYURRK-PTID
           MOVE    JYURRK-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_jyurrk_prv"     TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           DISPLAY "take tmp db delete jyurr st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *診療科履歴マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      KARRK-HOSPNUM
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      KARRK-PTID
           MOVE    SRYKARRK-REC         TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_srykarrk_prv"   TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           DISPLAY "take tmp db delete srykarr st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *患者コメントマスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      PTCOM-HOSPNUM
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      PTCOM-PTID
           MOVE    PTCOM-REC            TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_ptcom_prv"      TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           DISPLAY "take tmp db delete ptcom st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *患者マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      PTINF-HOSPNUM
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      PTINF-PTID
           MOVE    PTINF-REC            TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_ptinf_prv"      TO  MCP-TABLE
           MOVE    "delprv"             TO  MCP-PATHNAME
           DISPLAY "take tmp db delete ptinf st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *診療行為付加情報マスタ
grpsys     MOVE    WRK-PARA-HOSPNUM     TO      ACCTP-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO      ACCTP-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      ACCTP-PTID
           MOVE    SRYACCTPLUS-REC      TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    "tbl_sryacctplus_prv"
                                        TO  MCP-TABLE
           MOVE    "del1"               TO  MCP-PATHNAME
           DISPLAY "take tmp db delete ptinf st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
           .
       20011-TMPDB-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-FET-SEC            SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRVKANRI-REC
               MOVE    ZERO                TO  FLG-PRVKANRI
           ELSE
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
           .
       900-PRVKANRI-FET-EXT.
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
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
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
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
      *    MOVE    "ROLLBACK"          TO  MCP-FUNC.
      *    CALL    "MCPSUB"            USING
      *                                MCPAREA
      *                                MCPDATA-REC.
      *    DISPLAY "rollback mcp-rc = " MCP-RC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
