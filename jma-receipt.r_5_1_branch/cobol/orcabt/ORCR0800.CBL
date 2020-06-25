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
       PROGRAM-ID.             ORCR0800.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 仮収納データ作成（労災・自賠責）
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/06/16    NACL−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.12.15  日別収納更新・削除
      *  01.00.02    NACL-門脇    04.01.14  診療行為の包括分は除く
      *                                     SRYACCT8→29
      *                                     SRYACCT9→30
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *                                     診療会計テーブルの読み込みを
      *                                     グループ化に変更
      *                                     SRYACCT-KEY29 -> SRYACCT-KEY36
      *                                     SRYACCT-KEY30 -> SRYACCT-KEY37
      *  01.01.02    NACL-門脇    05/01/28  ダミー収納削除処理の修正
      *  01.01.03    NACL-門脇    05/07/19  ダミー収納削除処理の修正
      *  01.01.04    NACL-門脇    06/01/26  対象データ編集修正
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  04.02.01    NACL-門脇    08/05/29  入院料加算保険組み合わせチェック
      *  04.02.02    NACL-門脇    08/06/15  入院料加算外泊チェック
      *  04.02.03    NACL-門脇    08/10/07  入院基本料有効期間チェック
      *  04.02.04    NACL-門脇    08/10/30  保険組合せ有効期間チェック
      *  04.03.01    NACL-門脇    08/12/04  仮収納作成時のディスプレイ追加
      *
      *  04.04.01    NACL-門脇    09/08/25  出産一時金対応
      *  04.04.02    NACL-門脇    10/05/06  カーソルクローズ修正
      *  04.04.03    NACL-門脇    10/05/07  入院料剤点チェック
      *  04.04.04    NACL-門脇    10/07/20  救急搬送診療料のチェック
      *  04.04.05    NACL-門脇    10/08/09  他医受診のチェック
      *
      *  04.05.01    NACL-門脇    10/01/18  治験対応
      *  04.05.02    NACL-門脇    11/11/09  同日再入院食事対応
      *  04.05.03    NACL-門脇    12/09/18  改正対応（外泊区分８，９の対応）
      *  04.06.01    NACL-門脇    13/03/27  エラー時終了処理対応
      *
      *  04.07.01    NACL-門脇    12/08/28  薬剤料減点対応
      *  04.07.02    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.03    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.04    NACL-門脇    15/01/16  入院点滴：診療種別「．３４０」の
      *                                     入力チェック対応
      *  04.07.05    NACL-門脇    15/06/26  特別食加算の入力チェック対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-門脇    14/07/23  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *  04.08.04    NACL-門脇    15/11/09  該当期間に１０超の保険組合せの
      *                                     使用があるかチェック
      *  04.08.05    NACL-門脇    16/02/29  平成２８年４月改正対応
      *  04.08.06    NACL-門脇    17/04/06  入院期間外の入院診療行為入力
      *                                     チェック
      *  04.08.07    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *  04.08.08    NACL-門脇    17/08/28  ７０歳未満の低１チェックと
      *                                     システム管理５０００−「入院時
      *                                     食事療養」の未設定チェック
      *  04.08.09    NACL-門脇    17/10/18  入院料と異なる保険組み合わせ
      *                                     での食事算定対応
      *  04.08.10    NACL-門脇    18/05/10  入院会計照会の保険組み合わせ
      *                                     （９９９）チェック対応
      *  04.08.11    NACL-門脇    18/05/31  計算金額桁数チェック
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE   ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SYS2002                             PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-PTNYUINRRK                          PIC 9(01).
           03  FLG-PTRSIINF                            PIC 9(01).
      *
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-DATA                                PIC 9(01).
           03  FLG-BUNBEN-SET                          PIC 9(01).
           03  FLG-NG                                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(04). 
           03  IDX1                                    PIC 9(04). 
           03  SYSKAN-IDX                              PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                          PIC 9(10).
               05  KEY-N-HKNCOMBI                      PIC 9(04).
               05  KEY-N-SRYKA                         PIC X(02).
           03  KEY-OLD.
               05  KEY-O-PTID                          PIC 9(10).
               05  KEY-O-HKNCOMBI                      PIC 9(04).
               05  KEY-O-SRYKA                         PIC X(02).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    労災自賠医療機関退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC                         OCCURS  100.
               05  TBL-SYS4001                         PIC X(610).
      *
      *    エラーメッセージ編集領域
       01  ERR-EDIT-AREA.
           03  ERR-PTNUM               PIC X(20).
           03  ERR-ERRMSG              PIC X(80).
           03  ERR-HEN-ERRMSG          PIC X(200).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-SYOKBN                     PIC X(01).
               05  WRK-PARA-RECEKBN                    PIC X(01).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
               05  WRK-PARA-PTID                       PIC X(10).
               05  WRK-PARA-SRYYM-O                    PIC X(06).
           03  WRK-PARA-JIBAIPRTKBN                    PIC X(01).
           03  WRK-PARA-PRTKBN                         PIC  X(01).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-STYMD                               PIC X(08).
           03  WRK-EDYMD                               PIC X(08).
      *
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL                         OCCURS  2.
                   07  WRK-DAY                         PIC 9(04)
                                                       OCCURS   31.
      *
           03  WRK-DAY-AREA2.
               05  WRK-DAY-TBL2                        OCCURS   31.
                   07  WRK-DAY-DOJITU                  PIC 9(04).
                   07  WRK-DAY-KBN                     PIC X(01).
      *
           03  WRK-ROUSAIKBN                           PIC X(01).
           03  WRK-DENPNUM                             PIC 9(07).
           03  WRK-SEIKYUSTYMD                         PIC 9(08).
           03  WRK-SEIKYUEDYMD                         PIC 9(08).
           03  WRK-DATAKBN                             PIC X(01).
           03  WRK-SYOKBN                              PIC X(01).
           03  WRK-HKNCOMBINUM                         PIC 9(04).
      *
           03  WRK-STR                                 PIC X(200).
           03  WRK-LEN                                 PIC 9(05).
           03  WRK-SHO                                 PIC 9(05).
           03  WRK-AMARI                               PIC 9(05).
      *
           03  WRK-CONS-HKNNUM                         PIC X(03).
      *
           03  WRK-JIBAIKBN                            PIC X(01).
      *
           03  WRK-SRYACCT-PATHNAME                    PIC X(64).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-201707-X.
               05  WRK-CONS-SRYYM-201707
                                   PIC 9(06)   VALUE   201707.
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
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
           COPY    "CPSK1001.INC".
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *    患者番号構成情報
           COPY    "CPSK1009.INC".
      *
      *    労災自賠医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    日別収納マスター
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    分娩入院チェックサブ
           COPY    "CPORCSBUNBENCHK.INC".
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    定期請求用収納作成サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
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
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL ( FLG-END =   1 )
                                      OR ( FLG-ERR =   1 )
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
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-HOSPNUM    =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN     =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM      =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN     =" WRK-PARA-PRTKBN
           DISPLAY "WRK-PARA-RECEKBN    =" WRK-PARA-RECEKBN
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           EVALUATE  WRK-PARA-RECEKBN
             WHEN      "3"
                 MOVE    "971"         TO  WRK-CONS-HKNNUM
             WHEN      "4"
             WHEN      "5"
                 MOVE    "973"         TO  WRK-CONS-HKNNUM
             WHEN      "8"
                 MOVE    "970"         TO  WRK-CONS-HKNNUM
           END-EVALUATE
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0800"      TO  JOB-PGID
               MOVE    "仮収納データ作成（労災・自賠責）"
                                   TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        医療機関ＩＤ編集
               INITIALIZE                  SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
               MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
               ELSE
                   MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               IF   WRK-PARA-SRYYM    NOT =  ZERO
                  CONTINUE
               ELSE
                  MOVE     ZERO               TO  SYSKAN-IDX
                  MOVE     SPACE              TO  TBL-SYS1001-T
                  INITIALIZE                      TBL-SYS1001-T
      *
                  INITIALIZE                      SYS-1001-REC
                  MOVE    "1001"              TO  SYS-1001-KANRICD
                  MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys            MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
                  MOVE    SYS-1001-REC        TO  MCPDATA-REC
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
grpsys            PERFORM 900-DBSELECT-SEC
                  IF      MCP-RC              =   ZERO
                      MOVE    "tbl_syskanri"      TO  MCP-TABLE
                      MOVE    "key8"              TO  MCP-PATHNAME
                      PERFORM 900-SYSKANRI-READ-SEC
                      PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                                OR    (SYSKAN-IDX   >=  100)
      *
                          MOVE    MCPDATA-REC   TO  SYS-1001-REC
                          ADD     1             TO  SYSKAN-IDX
                          MOVE    SYS-1001-REC  TO
                                                TBL-SYS1001(SYSKAN-IDX)
      *
                          MOVE    "tbl_syskanri"  TO  MCP-TABLE
                          MOVE    "key8"          TO  MCP-PATHNAME
                          PERFORM 900-SYSKANRI-READ-SEC
                      END-PERFORM
                  ELSE
                      MOVE    "医療機関情報が取得できませんでした。"
                                                TO  WRK-RECEERR
                      PERFORM 500-ERR-HENSYU-SEC
                  END-IF
      *
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM 900-CLOSE-SEC
               END-IF
      *
      *        患者番号構成管理情報編集
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-1009-STYUKYMD(7:2)
               MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-HJNKSIKBN
                                           TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               ELSE
                   MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               IF      WRK-PARA-RECEKBN    =   "8"
                   MOVE    "2"                 TO    WRK-JIBAIKBN
               ELSE
                 IF      WRK-PARA-SRYYM  NOT =   ZERO
      *            システム管理マスタ（４００１）から自賠責区分取得
                   INITIALIZE                       SYS-4001-REC
                   MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
                   MOVE    "4001"              TO  SYS-4001-KANRICD
                   MOVE    "*"                 TO  SYS-4001-KBNCD
                   MOVE    WRK-PARA-SRYYM      TO  SYS-4001-STYUKYMD
                                                                   (1:6)
                   MOVE    "01"                TO  SYS-4001-STYUKYMD
                                                                   (7:2)
                   MOVE    SYS-4001-STYUKYMD   TO  SYS-4001-EDYUKYMD
                   MOVE    SYS-4001-REC        TO  MCPDATA-REC
                   PERFORM 910-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-4001-REC
                       IF     SYS-4001-JIBAIKBN    =   SPACE
                           MOVE    "1"                 TO  WRK-JIBAIKBN
                       ELSE
                           MOVE SYS-4001-JIBAIKBN      TO  WRK-JIBAIKBN
                       END-IF
      *
                       DISPLAY "WRK-JIBAIKBN=" WRK-JIBAIKBN
                   ELSE
                       MOVE
                          "労災自賠医療機関情報が取得できませんでした"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1                   TO  FLG-END
                   END-IF
                 ELSE
                   MOVE     ZERO               TO  SYSKAN-IDX
                   MOVE     SPACE              TO  TBL-SYS4001-T
                   INITIALIZE                      TBL-SYS4001-T
      *
                   INITIALIZE                      SYS-4001-REC
                   MOVE    "4001"              TO  SYS-4001-KANRICD
                   MOVE    "*"                 TO  SYS-4001-KBNCD
                   MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
                   MOVE    SYS-4001-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       PERFORM UNTIL ( FLG-SYSKANRI    =   1   )
                               OR    ( SYSKAN-IDX      >=  100 )
                           MOVE    MCPDATA-REC         TO  SYS-4001-REC
                           ADD     1                   TO  SYSKAN-IDX
                           MOVE    SYS-4001-REC        TO
                                                TBL-SYS4001 (SYSKAN-IDX)
      *
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-READ-SEC
                       END-PERFORM
                   ELSE
                       MOVE
                           "労災自賠医療機関情報が取得できませんでした"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1                   TO  FLG-END
                   END-IF
      *
                   MOVE    "tbl_syskanri"       TO  MCP-TABLE
                   MOVE    "key8"               TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
                 END-IF
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        仮収納の削除処理
               PERFORM  1001-SYUNOU-DELETE-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    仮収納の削除処理
      *****************************************************************
       1001-SYUNOU-DELETE-SEC                SECTION.
      *
      *    ダミー収納削除処理
      *    ダミー日別収納削除処理
      *
           IF    ( WRK-PARA-SYOKBN   =   "1"  )
           AND   ( FLG-ERR           =   ZERO )
             INITIALIZE                           SYUNOU-REC
             MOVE    SPA-HOSPNUM          TO      SYU-HOSPNUM
             MOVE    "1"                  TO      SYU-NYUGAIKBN
             MOVE    WRK-PARA-SRYYM       TO      SYU-SRYYMD(1:6)
             MOVE    "__"                 TO      SYU-SRYYMD(7:2)
             MOVE    SYUNOU-REC           TO      MCPDATA-REC
             MOVE    "tbl_syunou"         TO      MCP-TABLE
             MOVE    "key40"              TO      MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF      MCP-RC               =       ZERO
               MOVE    "tbl_syunou"       TO      MCP-TABLE
               MOVE    "key40"            TO      MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL
                              (FLG-SYUNOU             =    1)
      *
                 EVALUATE    WRK-PARA-RECEKBN
                     WHEN    "3"
                     WHEN    "4"
                     WHEN    "5"
                         IF      SYU-SYUHKNNUM =   WRK-CONS-HKNNUM
                             MOVE    SYUNOU-REC    TO  MCPDATA-REC
                             MOVE    "DBDELETE"    TO  MCP-FUNC
                             MOVE    "tbl_syunou"  TO  MCP-TABLE
                             MOVE    "key"         TO  MCP-PATHNAME
                             PERFORM  900-ORCDBMAIN-SEC
                         END-IF
                     WHEN    "8"
                         IF      SYU-KOH-HKNNUM (1)
                                               =   WRK-CONS-HKNNUM
                             MOVE    SYUNOU-REC    TO  MCPDATA-REC
                             MOVE    "DBDELETE"    TO  MCP-FUNC
                             MOVE    "tbl_syunou"  TO  MCP-TABLE
                             MOVE    "key"         TO  MCP-PATHNAME
                             PERFORM  900-ORCDBMAIN-SEC
                         END-IF
                 END-EVALUATE
     *
                 MOVE    "tbl_syunou"     TO      MCP-TABLE
                 MOVE    "key40"          TO      MCP-PATHNAME
                 PERFORM  900-SYUNOU-READ-SEC
      *
               END-PERFORM
             ELSE
               MOVE  "仮収納データ削除処理に失敗しました"
                               TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
             MOVE    "tbl_syunou"         TO      MCP-TABLE
             MOVE    "key40"              TO      MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       1001-SYUNOU-DELETE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-ERR             =   ZERO
               EVALUATE    WRK-PARA-SYOKBN
                   WHEN    "1"
                       PERFORM 2001-IKKATU-SYORI-SEC
                   WHEN    "2"
                       PERFORM 2002-KOBETU-SYORI-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC               SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-ERR             =   ZERO
      *        診療年月編集
               MOVE    WRK-PARA-SRYYM      TO  WRK-STYMD (1:6)
               MOVE    "01"                TO  WRK-STYMD (7:2)
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *        診療会計読み込み（患者ＩＤ、保険組合せ、診療科順）
      *
               INITIALIZE                  SRYACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
               MOVE    "1"                 TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key60"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key36"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                         OR        FLG-ERR         =   1
                   PERFORM 20011-IKKATU-SRYACCT-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               IF   FLG-ERR   =   ZERO
                 MOVE    "tbl_sryacct"       TO  MCP-TABLE
                 MOVE    WRK-SRYACCT-PATHNAME
                                             TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        入院会計読み込み（患者ＩＤ、診療科、剤識別区分順）
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *
               INITIALIZE                  NYUINACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  NACCT-SRYYM
               MOVE    "1"                 TO  NACCT-NYUGAIKBN
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT   =   1
                       OR          FLG-ERR         =   1
                   PERFORM 20012-IKKATU-NYUINACCT-HEN-SEC
               END-PERFORM
      *        カーソルクロース
               IF   FLG-ERR   =   ZERO
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key13"             TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    診療会計一括処理（患者ＩＤ、保険組合せ、診療科順）
      *****************************************************************
       20011-IKKATU-SRYACCT-HENSYU-SEC              SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
           IF      FLG-ERR             =   1
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
               GO  TO  20011-IKKATU-SRYACCT-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE                   TO  WRK-DATAKBN
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-ERR         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-ERR         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
                   MOVE    SPACE               TO  WRK-ROUSAIKBN
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       EVALUATE    WRK-PARA-RECEKBN
                           WHEN    "3"
                           WHEN    "4"
                           WHEN    "5"
                               IF      COMB-HKNNUM =   WRK-CONS-HKNNUM
                                   MOVE    "1"         TO  WRK-ROUSAIKBN
                              END-IF
                           WHEN    "8"
                               IF      COMB-KOHHKNNUM (1)
                                                   =   WRK-CONS-HKNNUM
                                   MOVE    "1"         TO  WRK-ROUSAIKBN
                              END-IF
                      END-EVALUATE
                  END-IF
      *
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-ERR         =   1
                                   OR      KEY-NEW     NOT =   KEY-OLD
      *
                       MOVE    1     TO   FLG-NG
                       PERFORM   20011-SRYACCT-FILE-HENSYU-SEC 
      *
                       IF      FLG-ERR            =   1
                          CONTINUE
                       ELSE
                         IF      WRK-DATAKBN        =   "1"
      *                    仮収納作成のとき患者ＩＤで読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM UNTIL   FLG-SRYACCT =   1 
                                   OR      FLG-ERR     =   1
                                   OR      KEY-N-PTID  
                                                   NOT =   KEY-O-PTID
                               MOVE    "tbl_sryacct"  TO  MCP-TABLE
                               MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                         ELSE
      *                    同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      FLG-ERR     =   1
                                       OR      KEY-NEW NOT =   KEY-OLD
                               MOVE    "tbl_sryacct"  TO  MCP-TABLE
                               MOVE    WRK-SRYACCT-PATHNAME
                                                      TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                         END-IF    
                       END-IF
                   END-PERFORM    
               END-PERFORM         
           END-IF    
           .
       20011-IKKATU-SRYACCT-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    仮収納作成処理（診療会計）
      *****************************************************************
       20011-SRYACCT-FILE-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-DATAKBN
      *
      *    対象保険以外のとき処理しない
           IF      WRK-ROUSAIKBN       =   "1"
               CONTINUE
           ELSE
               GO  TO  20011-SRYACCT-FILE-HENSYU-EXT
           END-IF
      *
      *    新様式で健保準拠のときは処理しない
           IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201707-X )
           AND   ( WRK-PARA-RECEKBN    =   "4"                     )
               INITIALIZE              PTRSIINF-REC
               MOVE    SPA-HOSPNUM     TO  PTRSI-HOSPNUM
               MOVE    COMB-HKNID      TO  PTRSI-HKNID
               MOVE    COMB-PTID       TO  PTRSI-PTID
               MOVE    PTRSIINF-REC    TO  MCPDATA-REC
               PERFORM 900-PTRSIINF-INV-SEC
      *
               DISPLAY " PTRSI-JIBAIKBN="   PTRSI-JIBAIKBN
      *
               IF      PTRSI-JIBAIKBN  =   SPACE   OR  "0"
                   MOVE    WRK-JIBAIKBN    TO  PTRSI-JIBAIKBN
               END-IF
               IF      PTRSI-JIBAIKBN  =   "2"
                   GO  TO  20011-SRYACCT-FILE-HENSYU-EXT
               END-IF
           END-IF
      *
      *    仮収納作成 
           PERFORM 20011-HENSYU-SEC
      *
           .
       20011-SRYACCT-FILE-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計一括編集処理
      *****************************************************************
       20012-IKKATU-NYUINACCT-HEN-SEC              SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
           IF      FLG-ERR             =   1
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
               GO  TO  20012-IKKATU-NYUINACCT-HEN-EXT
           END-IF
      *
           MOVE    SPACE                   TO  WRK-DATAKBN
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "NYUINACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-ERR         =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
      *
      *      既に仮収納があれば作成しない
             PERFORM 900-SYUNOU-INV-SEC
             IF      FLG-DATA          =   1
                 MOVE    KEY-NEW             TO  KEY-OLD
                 PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                                 OR      FLG-ERR         =   1 
                                 OR      KEY-N-PTID  NOT =   KEY-O-PTID
                     MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                     MOVE    "key13"             TO  MCP-PATHNAME
                     PERFORM 900-NYUINACCT-READ-SEC
                 END-PERFORM
             ELSE
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               INITIALIZE                      WRK-DAY-AREA
               INITIALIZE                      WRK-DAY-AREA2
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-ERR         =   1 
                               OR      KEY-NEW     NOT =   KEY-OLD
      *            剤識別区分別テーブル編集
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1  >        31
                       IF      NACCT-DAY  (IDX1)       >   ZERO
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                       END-IF
                   END-PERFORM            
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
               IF      FLG-ERR         =   ZERO
                   INITIALIZE                   ORCSNYUINDAY-AREA
                   MOVE    SPA-HOSPNUM      TO  LNK-NYUINDAY-HOSPNUM
                   MOVE    KEY-O-PTID       TO  LNK-NYUINDAY-PTID
                   MOVE    SYS-2001-SRYYM   TO  LNK-NYUINDAY-SANTEIYM
                   CALL    "ORCSNYUINDAY"   USING   ORCSNYUINDAY-AREA
                                                    SPA-AREA
                   IF      LNK-NYUINDAY-RC  =   ZERO
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                                  UNTIL    IDX    >    31
                        MOVE  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
                                            TO  WRK-DAY-DOJITU(IDX)
                        MOVE  LNK-NYUINDAY-KBN(IDX)
                                            TO  WRK-DAY-KBN(IDX)
                      END-PERFORM
                   END-IF
               END-IF
      *
               IF      FLG-ERR         =   ZERO 
      *            対象の入院会計があるとき仮収納作成
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       31
                       IF    ((WRK-DAY (1 IDX)     >   ZERO) OR
                              (WRK-DAY-KBN(IDX)    =   "1" ))  AND
                             ( WRK-DAY (2 IDX)     >   ZERO )
                           MOVE    WRK-DAY (2 IDX) TO  WRK-HKNCOMBINUM
                           PERFORM 200121-NYUINACCT-CHK-SEC
                           IF      WRK-SYOKBN      =   SPACE
                               MOVE    2     TO   FLG-NG
                               PERFORM 200122-NYUINACCT-HENSYU-SEC
                               IF    ( WRK-DATAKBN     =   "1" )
                               OR    ( FLG-ERR         =    1 )
                                   MOVE    31              TO  IDX
                               END-IF
                           END-IF
                       END-IF 
                   END-PERFORM        
                   IF    ( WRK-DATAKBN     =   "1" )
                   OR    ( FLG-ERR         =    1  )
                     CONTINUE
                   ELSE
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >       31
                       IF      WRK-DAY-DOJITU(IDX) >   ZERO
                           MOVE    WRK-DAY-DOJITU(IDX)
                                                   TO  WRK-HKNCOMBINUM
                           PERFORM 200121-NYUINACCT-CHK-SEC
                           IF      WRK-SYOKBN      =   SPACE
                               MOVE    2     TO   FLG-NG
                               DISPLAY "DOUJITU"
                               PERFORM 200122-NYUINACCT-HENSYU-SEC
                               IF    ( WRK-DATAKBN     =   "1" ) 
                               OR    ( FLG-ERR         =    1  )
                                   MOVE    31              TO  IDX
                               END-IF
                           END-IF
                       END-IF
                     END-PERFORM
                   END-IF
               END-IF
             END-IF
           END-IF    
           .
       20012-IKKATU-NYUINACCT-HEN-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       200121-NYUINACCT-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOKBN
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    KEY-O-PTID      TO  COMB-PTID
           MOVE    WRK-HKNCOMBINUM TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-INV-SEC
      *
           IF      FLG-HKNCOMBI    =   ZERO
      *        対象保険以外読み飛ばし
               EVALUATE    WRK-PARA-RECEKBN
                   WHEN    "3"
                   WHEN    "4"
                   WHEN    "5"
                       IF      COMB-HKNNUM     =   WRK-CONS-HKNNUM
                           CONTINUE
                       ELSE    
                           MOVE    "1"         TO  WRK-SYOKBN
                       END-IF
                   WHEN    "8"
                       IF      COMB-KOHHKNNUM (1)
                                               =   WRK-CONS-HKNNUM
                           CONTINUE
                       ELSE    
                           MOVE    "1"         TO  WRK-SYOKBN
                       END-IF
               END-EVALUATE 
           END-IF
           .
       200121-NYUINACCT-CHK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    仮収納作成処理（入院会計）
      *****************************************************************
       200122-NYUINACCT-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-DATAKBN
      *
      *    対象保険以外のとき処理しない
           IF      WRK-SYOKBN       =   "1"
               GO  TO  200122-NYUINACCT-HENSYU-EXT
           END-IF
      *
      *    新様式で健保準拠のときは処理しない
           IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201707-X )
           AND   ( WRK-PARA-RECEKBN    =   "4"                     )
               INITIALIZE              PTRSIINF-REC
               MOVE    SPA-HOSPNUM     TO  PTRSI-HOSPNUM
               MOVE    COMB-HKNID      TO  PTRSI-HKNID
               MOVE    COMB-PTID       TO  PTRSI-PTID
               MOVE    PTRSIINF-REC    TO  MCPDATA-REC
               PERFORM 900-PTRSIINF-INV-SEC
      *
               DISPLAY " PTRSI-JIBAIKBN="   PTRSI-JIBAIKBN
      *
               IF      PTRSI-JIBAIKBN  =   SPACE   OR  "0"
                   MOVE    WRK-JIBAIKBN    TO  PTRSI-JIBAIKBN
               END-IF
               IF      PTRSI-JIBAIKBN  =   "2"
                   GO  TO  200122-NYUINACCT-HENSYU-EXT
               END-IF
           END-IF
      *
      *    仮収納作成 
           PERFORM 20011-HENSYU-SEC
      *
           .
        200122-NYUINACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    仮収納作成処理 
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
           MOVE    "1"                 TO  WRK-DATAKBN
      *
      *    ダミー収納削除処理
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    KEY-O-PTID          TO  SYU-PTID
           MOVE    WRK-STYMD (1:6)     TO  SYU-SRYYMD(1:6)
           MOVE    "__"                TO  SYU-SRYYMD(7:2)
           MOVE    "3"                 TO  SYU-CREATEKBN
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "del4"              TO  MCP-PATHNAME
           PERFORM  900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE  "仮収納データ削除処理に失敗しました"
                               TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ダミー日別収納削除処理
      *
           IF   FLG-ERR         =   1
              CONTINUE
           ELSE
              INITIALIZE                      SYUDAY-REC
              MOVE    SPA-HOSPNUM         TO  SDAY-HOSPNUM
              MOVE    "1"                 TO  SDAY-NYUGAIKBN
              MOVE    KEY-O-PTID          TO  SDAY-PTID
              MOVE    WRK-STYMD (1:6)     TO  SDAY-SKYYM
              MOVE    "3"                 TO  SDAY-CREATEKBN
              MOVE    SYUDAY-REC          TO  MCPDATA-REC
              MOVE    "DBDELETE"          TO  MCP-FUNC
              MOVE    "tbl_syuday"        TO  MCP-TABLE
              MOVE    "del3"              TO  MCP-PATHNAME
              PERFORM  900-ORCDBMAIN-SEC
              IF      MCP-RC          NOT =   ZERO
                  MOVE  "仮日別収納データ削除処理に失敗しました"
                                  TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
           END-IF
      *
           IF   FLG-ERR         =   ZERO
      *
      *       伝票番号セット
      *
              INITIALIZE                       SYUNOU-REC
              MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
              MOVE    "1"                  TO  SYU-NYUGAIKBN
              MOVE    KEY-O-PTID           TO  SYU-PTID
              MOVE    "3"                  TO  SYU-CREATEKBN
              MOVE    SYUNOU-REC           TO  MCPDATA-REC
              MOVE    "tbl_syunou"         TO  MCP-TABLE
              MOVE    "key39"              TO  MCP-PATHNAME
              PERFORM    900-DBSELECT-SEC
              IF      MCP-RC               =       ZERO
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key39"          TO  MCP-PATHNAME
                  PERFORM    900-DBFETCH-SEC
                  IF      MCP-RC              =   ZERO
                     MOVE    MCPDATA-REC   TO  SYUNOU-REC
                     MOVE    SYU-DENPNUM   TO  WRK-DENPNUM
                  ELSE
                     MOVE    9100000       TO  WRK-DENPNUM
                  END-IF
              ELSE
                  DISPLAY  "SYUNOU SELECT ERR"
                  MOVE       9100000       TO  WRK-DENPNUM
              END-IF
      *
              MOVE    "tbl_syunou"         TO  MCP-TABLE
              MOVE    "key39"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
      ********
      *
              MOVE    ZERO                TO  WRK-SEIKYUSTYMD
                                              WRK-SEIKYUEDYMD
      *
              MOVE    ZERO                TO  FLG-BUNBEN-SET
      *
      *       診療月内の入退院情報
              INITIALIZE                      PTNYUINRRK-REC
              MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
              MOVE    KEY-O-PTID          TO  PTNYUINRRK-PTID
              MOVE    WRK-STYMD (1:6)     TO  PTNYUINRRK-NYUINYMD (1:6)
              MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
              MOVE    WRK-STYMD (1:6)     TO  PTNYUINRRK-TAIINYMD (1:6)
              MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
              MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
              MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
              MOVE    "key23"             TO  MCP-PATHNAME
              PERFORM    900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                  MOVE    "key23"             TO  MCP-PATHNAME
                  PERFORM 900-PTNYUINRRK-READ-SEC
              ELSE
                  MOVE    1                   TO  FLG-PTNYUINRRK    
              END-IF
              PERFORM     UNTIL   FLG-PTNYUINRRK    =   1
                   IF    ( WRK-SEIKYUSTYMD =   ZERO                )   
                   OR    ( WRK-SEIKYUSTYMD >   PTNYUINRRK-NYUINYMD )
                       IF  WRK-SEIKYUSTYMD =   WRK-STYMD
                          CONTINUE
                       ELSE
                         IF  PTNYUINRRK-NYUINYMD  >  WRK-STYMD
                           MOVE  PTNYUINRRK-NYUINYMD 
                                              TO  WRK-SEIKYUSTYMD
                         ELSE
                           MOVE  WRK-STYMD    TO  WRK-SEIKYUSTYMD
                         END-IF 
                       END-IF
                   END-IF             
                   IF      WRK-SEIKYUEDYMD =   ZERO
                     MOVE PTNYUINRRK-TAIINYMD TO  WRK-SEIKYUEDYMD
                   END-IF             
      *
                   IF      FLG-BUNBEN-SET  =   ZERO
                     INITIALIZE                    SBUNBENCHK-AREA
                     MOVE  KEY-O-PTID          TO  SBUNBENCHK-PTID
                     MOVE  PTNYUINRRK-RRKNUM   TO  SBUNBENCHK-RRKNUM
                     CALL  "ORCSBUNBENCHK" USING
                                           SBUNBENCHK-AREA
                                           SPA-AREA
                     IF    SBUNBENCHK-BUNBEN   =   ZERO
                        CONTINUE
                     ELSE
                        MOVE    1              TO  FLG-BUNBEN-SET
                     END-IF
                   END-IF
      *
                   MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                   MOVE    "key23"            TO  MCP-PATHNAME
                   PERFORM 900-PTNYUINRRK-READ-SEC
              END-PERFORM        
      *                 
              MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
              MOVE    "key23"             TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      WRK-SEIKYUSTYMD     =   ZERO
                  MOVE    WRK-STYMD           TO  WRK-SEIKYUSTYMD
              END-IF 
              IF    ( WRK-SEIKYUEDYMD     =   ZERO    OR   99999999 )
              OR    ( WRK-SEIKYUEDYMD     >   WRK-EDYMD             )
                  MOVE    WRK-EDYMD           TO  WRK-SEIKYUEDYMD
              END-IF 
      *
      *       収納データ取得処理
              MOVE    SPACE           TO  LNK-ORCS02-REC
                                          LNK-SYUNOU-REC-T
                                          LNK-SYUDAY-REC-T
      *
              INITIALIZE                  LNK-ORCS02-REC
                                          LNK-SYUNOU-REC-T
                                          LNK-SYUDAY-REC-T
      *
              MOVE    SPA-HOSPNUM     TO  LNK-S02-HOSPNUM
              MOVE    "1"             TO  LNK-S02-NYUGAIKBN
              MOVE    KEY-O-PTID      TO  LNK-S02-PTID
              MOVE    WRK-SEIKYUSTYMD TO  LNK-S02-SKYSTYMD
              MOVE    WRK-SEIKYUEDYMD TO  LNK-S02-SKYEDYMD
              MOVE    "X"             TO  LNK-S02-TEIKI-KBN
              MOVE    WRK-PARA-SYSYMD TO  LNK-S02-SYSYMD
      *
              IF      FLG-BUNBEN-SET  =   ZERO
                  MOVE    "2"         TO  LNK-S02-SYORI-KBN
              ELSE
                  MOVE    "9"         TO  LNK-S02-SYORI-KBN
              END-IF
      *
              CALL    "ORCSNYUFTN"    USING
                                      SPA-AREA
                                      LNK-ORCS02-REC
                                      LNK-SYUNOU-REC-T
                                      LNK-SYUDAY-REC-T
      *
      *       仮収納出力処理
              IF      LNK-S02-RC      =   ZERO
                  PERFORM VARYING IDX1   FROM    1   BY  1
                          UNTIL ( IDX1                    >     10  )
                            OR  ( LNK-SYU-SKYSTYMD (IDX1) =   SPACE )
                            OR  ( FLG-ERR                 =      1  )
      ****          IF  LNK-SYU-SYUHKNNUM(IDX1)  =
      ****                                       WRK-CONS-HKNNUM
                       PERFORM   20011-KARITEIKI-OUTPUT-SEC
      ****          END-IF
                  END-PERFORM
              ELSE
                  DISPLAY "PTID=" KEY-O-PTID
                  DISPLAY "SKYSTYMD=" WRK-SEIKYUSTYMD
                  DISPLAY "SKYEDYMD=" WRK-SEIKYUEDYMD
      *
                  EVALUATE   LNK-S02-RC
                    WHEN       13
                      MOVE "入院会計が未作成です"
                                          TO  ERR-ERRMSG
                    WHEN       14
                      MOVE "入院料加算の保険組合せ確認"
                                          TO  ERR-ERRMSG
                    WHEN       15
                      MOVE "外泊日に入院料加算有"
                                          TO  ERR-ERRMSG
                    WHEN       16
                      MOVE "入院料未算定日に入院加算有"
                                          TO  ERR-ERRMSG
                    WHEN       17
                      MOVE "入院基本料の有効期間確認"
                                          TO  ERR-ERRMSG
                    WHEN       18
                      MOVE "保険組合せの有効期間確認"
                                          TO  ERR-ERRMSG
                    WHEN       19
                      MOVE "包括点数減点分の点数確認"
                                          TO  ERR-ERRMSG
                    WHEN       20
                      MOVE "入院料算定点数の誤り"
                                          TO  ERR-ERRMSG
                    WHEN       21
                      MOVE "救急搬送診療料算定日の入院料（加算）確認"
                                          TO  ERR-ERRMSG
                    WHEN       22
                      MOVE "他医受診（外泊）の入力誤り"
                                          TO  ERR-ERRMSG
                    WHEN       23
                      MOVE "同日再入院日の実日数算定コード未入力"
                                          TO  ERR-ERRMSG
                    WHEN       24
                      MOVE "同日再入院日の再入院分に入院料加算有"
                                          TO  ERR-ERRMSG
                    WHEN       25
                      MOVE "薬剤料減点数確認"
                                          TO  ERR-ERRMSG
                    WHEN       26
                      MOVE "特定時間退院減算（外泊）の入力誤り"
                                          TO  ERR-ERRMSG
                    WHEN       27
                      MOVE "特定曜日入退院減算（外泊）の入力誤り"
                                          TO  ERR-ERRMSG
                    WHEN       28
                      MOVE "診療種別「．３４０」の診療行為入力確認"
                                          TO  ERR-ERRMSG
                    WHEN       29
                      MOVE "入院時食事療養（２）算定時に特別食加算有"
                                          TO  ERR-ERRMSG
                    WHEN       30
                      MOVE "該当期間に１０超の保険組合せの使用有"
                                          TO  ERR-ERRMSG
                    WHEN       31
                      MOVE "入院時生活療養（２）算定時に流動食有"
                                          TO  ERR-ERRMSG
                    WHEN       32
                      MOVE "入院期間外の入院診療行為入力確認"
                                          TO  ERR-ERRMSG
                    WHEN       33
                      MOVE "入院基本設定「入院時食事療養」が未設定です"
                                          TO  ERR-ERRMSG
                    WHEN       34
                      MOVE "適用できない低所得１の入力有"
                                          TO  ERR-ERRMSG
                    WHEN       35
                      MOVE "食事保険指定コードの診療行為入力重複"
                                          TO  ERR-ERRMSG
                    WHEN       36
                      MOVE "食事保険指定コードの入力日に食事の算定なし"
                                          TO  ERR-ERRMSG
                    WHEN       37
                      MOVE "保険組合せが存在しません"
                                          TO  ERR-ERRMSG
                    WHEN       38
                      MOVE "請求点数の合計が７桁を超えます"
                                          TO  ERR-ERRMSG
                    WHEN       39
                      MOVE "保険外金額の合計が７桁を超えます"
                                          TO  ERR-ERRMSG
                    WHEN       40
                      MOVE "負担割合計算額が７桁を超えます"
                                          TO  ERR-ERRMSG
                    WHEN       41
                      MOVE "今回請求金額が７桁を超えます"
                                          TO  ERR-ERRMSG
                    WHEN       42
                      MOVE "【精神病床】食事負担据え置き対象コードの"
                                          TO  ERR-ERRMSG(1:40)
                      MOVE "診療行為入力確認"
                                          TO  ERR-ERRMSG(41:16)
      *
                    WHEN      101
                      MOVE "包括入院期間に算定できない入院料加算有"
                                          TO  ERR-ERRMSG
      *
                    WHEN      OTHER
                      MOVE "仮収納データの取得に失敗"
                                          TO  ERR-ERRMSG
                  END-EVALUATE
                  PERFORM 600-ERRMSG-EDIT-SEC
      *
                  MOVE  ERR-HEN-ERRMSG    TO  WRK-RECEERR
      *
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
           END-IF
      *
           .
       20011-HENSYU-EXT.
           EXIT. 
      *****************************************************************
      *    仮収納出力処理
      *****************************************************************
       20011-KARITEIKI-OUTPUT-SEC      SECTION.
      *
      ***  MOVE    "1"                 TO  WRK-DATAKBN
      *
           MOVE    LNK-SYUNOU-REC (IDX1)
                                       TO  SYUNOU-REC
           MOVE    LNK-SYUDAY-REC (IDX1)
                                       TO  SYUDAY-REC
      *
      *    伝票番号取得処理
           ADD     1                   TO  WRK-DENPNUM
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
                                           SDAY-DENPNUM
      *
           MOVE    WRK-PARA-TERMID     TO  SYU-TERMID
                                           SDAY-TERMID
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SMCNDATE-YMD        TO  SYU-CREYMD
                                           SDAY-CREYMD
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                                           SDAY-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
                                           SDAY-UPHMS
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM  900-ORCDBMAIN-SEC
      *
           IF      MCP-RC              =   ZERO 
               COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
               DISPLAY "<SYUNOU>PTID=" KEY-O-PTID
               DISPLAY "HKNCOMBINUM="    LNK-SYU-HKNCOMBINUM(IDX1)
               DISPLAY "SKYSTYMD="       LNK-SYU-SKYSTYMD   (IDX1)
               IF  LNK-SYU-SYUHKNNUM(IDX1)  NOT = SPACE
                 DISPLAY "SYUHKNNUM="    LNK-SYU-SYUHKNNUM  (IDX1)
               END-IF
               IF  LNK-SYU-KOH1HKNNUM(IDX1) NOT = SPACE
                 DISPLAY "KOH1HKNNUM="   LNK-SYU-KOH1HKNNUM (IDX1)
               END-IF
               IF  LNK-SYU-KOH2HKNNUM(IDX1) NOT = SPACE
                 DISPLAY "KOH2HKNNUM="   LNK-SYU-KOH2HKNNUM (IDX1)
               END-IF
               IF  LNK-SYU-KOH3HKNNUM(IDX1) NOT = SPACE
                 DISPLAY "KOH3HKNNUM="   LNK-SYU-KOH3HKNNUM (IDX1)
               END-IF
               IF  LNK-SYU-KOH4HKNNUM(IDX1) NOT = SPACE
                 DISPLAY "KOH4HKNNUM="   LNK-SYU-KOH4HKNNUM (IDX1)
               END-IF
           ELSE 
               DISPLAY "PTID=" KEY-O-PTID
               DISPLAY "SKYSTYMD=" WRK-SEIKYUSTYMD
               DISPLAY "SKYEDYMD=" WRK-SEIKYUEDYMD
      *
      *        MOVE    "仮収納データの更新に失敗しました"
      *                                TO  WRK-RECEERR
      *
               MOVE    "仮収納データの更新に失敗"
                                       TO  ERR-ERRMSG
               PERFORM 600-ERRMSG-EDIT-SEC
      *
               MOVE  ERR-HEN-ERRMSG    TO  WRK-RECEERR
      *
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF  FLG-ERR          =   1
               CONTINUE
           ELSE
             IF  SDAY-DAYINFFLG   =   "1"
      *         仮日別収納
                MOVE    SYUDAY-REC          TO  MCPDATA-REC
                MOVE    "tbl_syuday"        TO  MCP-TABLE
                MOVE    "key"               TO  MCP-PATHNAME
                MOVE    "DBINSERT"          TO  MCP-FUNC
                PERFORM    900-ORCDBMAIN-SEC
                IF      MCP-RC              =   ZERO 
                    COMPUTE CNT-OUT2    =   CNT-OUT2    +   1
                ELSE 
                    DISPLAY "PTID=" KEY-O-PTID
                    DISPLAY "SKYYM=" WRK-SEIKYUSTYMD(1:6)
      *
                    MOVE    "仮日別収納データの更新に失敗"
                                            TO  ERR-ERRMSG
                    PERFORM 600-ERRMSG-EDIT-SEC
      *
                    MOVE  ERR-HEN-ERRMSG    TO  WRK-RECEERR
      *
                    PERFORM 500-ERR-HENSYU-SEC
                END-IF
             END-IF
           END-IF
      *
           .
       20011-KARITEIKI-OUTPUT-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
           IF      WRK-PARA-SHELLID        =   "RECEPTX"
                   PERFORM 20021-KOBETU-REAL-SEC
                   MOVE    "1"   TO  PTINF-TSTPTNUMKBN
                   GO  TO    2002-KOBETU-SYORI-EXT
           END-IF
      *
      *    レセプト指示（個別指示）コード読み込み
           INITIALIZE                      SYS-2002-REC
           MOVE    "2002"              TO  SYS-2002-KANRICD
           MOVE    ZERO                TO  SYS-2002-STYUKYMD
                                           SYS-2002-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
           MOVE    SYS-2002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-SYS2002-READ-SEC
           ELSE
               INITIALIZE                  SYS-2002-REC
               MOVE    1                   TO  FLG-SYS2002
           END-IF
      *
           PERFORM 20021-KOBETU-HENSYU-SEC
                              UNTIL       FLG-SYS2002     =   1
                                OR        FLG-ERR         =   1
      *    カーソルクロース
           IF   FLG-ERR   =   ZERO
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key7"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ）
      *****************************************************************
       20029-SYSKANRI-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SET
      *
      *    該当診療年月の医療機関情報
           PERFORM VARYING SYSKAN-IDX  FROM  1  BY  1
                   UNTIL ( SYSKAN-IDX  >   100   )
                   OR    ( TBL-SYS1001 (SYSKAN-IDX)
                                       =   SPACE )
                   OR    ( FLG-SET     =   1     )
               MOVE    TBL-SYS1001 (SYSKAN-IDX)
                                           TO  SYS-1001-REC
               IF    ( SYS-1001-STYUKYMD   <=  WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD      <=  SYS-1001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET         =   ZERO
               STRING
                   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE
                    WRK-SYS-SRYYMD(1:6)    DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               MOVE    WRK-RECEERR     TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    該当診療年月の労災自賠医療機関情報
           IF     WRK-PARA-RECEKBN     =   "8"
               MOVE    "2"                 TO  WRK-JIBAIKBN
           ELSE
               MOVE    ZERO            TO  FLG-SET
      *
               PERFORM VARYING SYSKAN-IDX  FROM  1  BY  1
                       UNTIL ( SYSKAN-IDX  >   100   )
                       OR    ( TBL-SYS4001 (SYSKAN-IDX)
                                           =   SPACE )
                       OR    ( FLG-SET     =   1     )
                   MOVE    TBL-SYS4001 (SYSKAN-IDX)
                                               TO  SYS-4001-REC
                   IF    ( SYS-4001-STYUKYMD   <=  WRK-SYS-SRYYMD    )
                   AND   ( WRK-SYS-SRYYMD      <=  SYS-4001-EDYUKYMD )
                       MOVE     1                  TO  FLG-SET
                       IF      SYS-4001-JIBAIKBN   =   SPACE
                           MOVE    "1"                 TO  WRK-JIBAIKBN
                       ELSE
                           MOVE    SYS-4001-JIBAIKBN   TO  WRK-JIBAIKBN
                       END-IF
                   END-IF
               END-PERFORM
               IF      FLG-SET         =   ZERO
                   STRING
               "労災自賠医療機関情報が取得できませんでした。診療年月＝"
                                              DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD(1:6)    DELIMITED  BY  SIZE
                                              INTO    WRK-RECEERR
                   END-STRING
                   MOVE    WRK-RECEERR     TO  WRK-ERRMSG
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-IF
      *
           DISPLAY "WRK-JIBAIKBN=" WRK-JIBAIKBN
           .
      *
       20029-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-REAL-SEC              SECTION.
      *
           MOVE    WRK-PARA-PTID    TO  SYS-2002-PTID
           MOVE    WRK-PARA-SRYYM-O TO  SYS-2002-SRYYM
           MOVE    "1"              TO  SYS-2002-NYUGAIKBN
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
      *
      *    該当診療年月の医療機関情報取得
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM 20029-SYSKANRI-HENSYU-SEC
      *
      *    患者情報取得
grpsys     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    "1"                 TO  PTINF-TSTPTNUMKBN
      *
      *
           MOVE    SPACE               TO  WRK-DATAKBN
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *    診療会計読み込み
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key61"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key37"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
                     OR        FLG-ERR         =   1
               PERFORM 200211-KOBETU-SRYACCT-HEN-SEC
           END-PERFORM
      *    カーソルクロース
           IF   FLG-ERR   =   ZERO
             MOVE    "tbl_sryacct"       TO  MCP-TABLE
             MOVE    WRK-SRYACCT-PATHNAME
                                         TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    診療会計より仮収納を作成しなかったときのみ処理
           IF    ( WRK-DATAKBN         =   SPACE )
           AND   ( FLG-ERR             =   ZERO  )
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *        入院会計読み込み
               INITIALIZE                  NYUINACCT-REC
               MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
               MOVE    SYS-2002-PTID       TO  NACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM  900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT     =   1
                         OR        FLG-ERR           =   1
                   PERFORM 200212-KOBETU-NYUINACCT-HEN-SEC
               END-PERFORM
      *        カーソルクロース
               IF   FLG-ERR   =   ZERO
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key14"             TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
           END-IF 
      *
           .
       20021-KOBETU-REAL-EXT.
           EXIT.
      *
      *     
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID 
                   " SRYYM="   SYS-2002-SRYYM
      *
      *    医療機関ＩＤ編集
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM 20029-SYSKANRI-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-DATAKBN
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *    診療会計読み込み
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
               MOVE    "key61"             TO  WRK-SRYACCT-PATHNAME
           ELSE
               MOVE    "key37"             TO  WRK-SRYACCT-PATHNAME
           END-IF
           MOVE    WRK-SRYACCT-PATHNAME
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
                     OR        FLG-ERR         =   1
               PERFORM 200211-KOBETU-SRYACCT-HEN-SEC
           END-PERFORM
      *    カーソルクロース
           IF   FLG-ERR   =   ZERO
             MOVE    "tbl_sryacct"       TO  MCP-TABLE
             MOVE    WRK-SRYACCT-PATHNAME
                                         TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    診療会計より仮収納を作成しなかったときのみ処理
           IF    ( WRK-DATAKBN         =   SPACE )
           AND   ( FLG-ERR             =   ZERO  )
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *        入院会計読み込み
               INITIALIZE                  NYUINACCT-REC
               MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
               MOVE    SYS-2002-PTID       TO  NACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM  900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT     =   1
                         OR        FLG-ERR           =   1
                   PERFORM 200212-KOBETU-NYUINACCT-HEN-SEC
               END-PERFORM
      *        カーソルクロース
               IF   FLG-ERR   =   ZERO
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key14"             TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
           END-IF 
      *
           IF   FLG-ERR   =   ZERO
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key7"              TO  MCP-PATHNAME
              PERFORM 900-SYS2002-READ-SEC
           END-IF
      *
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    診療会計個別処理（患者ＩＤ、保険組合せ、診療科順）
      *****************************************************************
       200211-KOBETU-SRYACCT-HEN-SEC    SECTION.
      *
      *    保険組合せ読込み
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    KEY-N-PTID          TO  COMB-PTID
           MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM  910-HKNCOMBI-INV-SEC
      *
           MOVE    SPACE               TO  WRK-ROUSAIKBN
      *
           IF      FLG-HKNCOMBI        =   ZERO
               EVALUATE    WRK-PARA-RECEKBN
                   WHEN    "3"
                   WHEN    "4"
                   WHEN    "5"
                       IF      COMB-HKNNUM     =   WRK-CONS-HKNNUM
                           MOVE    "1"             TO  WRK-ROUSAIKBN
                       END-IF
                   WHEN    "8"
                       IF      COMB-KOHHKNNUM (1)
                                               =   WRK-CONS-HKNNUM
                           MOVE    "1"             TO  WRK-ROUSAIKBN
                       END-IF
               END-EVALUATE
           END-IF    
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM         UNTIL   FLG-SRYACCT     =   1 
                           OR      FLG-ERR         =   1
                           OR      KEY-NEW     NOT =   KEY-OLD
      *
               MOVE    3     TO   FLG-NG
               PERFORM   20011-SRYACCT-FILE-HENSYU-SEC 
      *
               IF        FLG-ERR            =    1
                 CONTINUE
               ELSE
                 IF      WRK-DATAKBN        =   "1"
      *            仮収納作成のとき処理を抜ける 
                   MOVE    1                  TO  FLG-SRYACCT
                 ELSE
      *            同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM     UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-ERR         =   1
                               OR      KEY-NEW     NOT =   KEY-OLD
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-PERFORM
                 END-IF
               END-IF 
           END-PERFORM    
           .
       200211-KOBETU-SRYACCT-HEN-EXT.
           EXIT.
      *            
      *****************************************************************
      *    入院会計個別編集処理
      *****************************************************************
       200212-KOBETU-NYUINACCT-HEN-SEC              SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           INITIALIZE                      WRK-DAY-AREA
           INITIALIZE                      WRK-DAY-AREA2
      *
           PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                           OR      FLG-ERR         =   1 
      *        剤識別区分別テーブル編集
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       MOVE    1               TO  IDX
                   WHEN    "5"
                       MOVE    2               TO  IDX
               END-EVALUATE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1  >        31
                   IF      NACCT-DAY  (IDX1)       >   ZERO
                       MOVE    NACCT-DAY (IDX1)    TO
                                               WRK-DAY (IDX IDX1)
                   END-IF
               END-PERFORM            
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           IF      FLG-ERR             =   ZERO
               INITIALIZE                   ORCSNYUINDAY-AREA
               MOVE    SPA-HOSPNUM      TO  LNK-NYUINDAY-HOSPNUM
               MOVE    SYS-2002-PTID    TO  LNK-NYUINDAY-PTID
               MOVE    SYS-2002-SRYYM   TO  LNK-NYUINDAY-SANTEIYM
               CALL    "ORCSNYUINDAY"   USING   ORCSNYUINDAY-AREA
                                                SPA-AREA
               IF      LNK-NYUINDAY-RC  =   ZERO
                  PERFORM    VARYING   IDX   FROM   1   BY   1
                              UNTIL    IDX    >    31
                    MOVE  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
                                        TO  WRK-DAY-DOJITU(IDX)
                    MOVE  LNK-NYUINDAY-KBN(IDX)
                                        TO  WRK-DAY-KBN(IDX)
                  END-PERFORM
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        対象の入院会計があるとき仮収納作成
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF    ((WRK-DAY (1 IDX)     >   ZERO) OR
                          (WRK-DAY-KBN(IDX)    =   "1" ))  AND
                         ( WRK-DAY (2 IDX)     >   ZERO )
                       MOVE    WRK-DAY (2 IDX) TO  WRK-HKNCOMBINUM
                       PERFORM 200121-NYUINACCT-CHK-SEC
                       IF      WRK-SYOKBN      =   SPACE
                           MOVE    4     TO   FLG-NG
                           PERFORM 200122-NYUINACCT-HENSYU-SEC
                           IF      WRK-DATAKBN     =   "1" 
                           OR      FLG-ERR         =    1
                                   MOVE    31              TO  IDX
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF    ( WRK-DATAKBN     =   "1" )
               OR    ( FLG-ERR         =    1  )
                 CONTINUE
               ELSE
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       31
                   IF      WRK-DAY-DOJITU(IDX) >   ZERO
                       MOVE    WRK-DAY-DOJITU(IDX)
                                               TO  WRK-HKNCOMBINUM
                       PERFORM 200121-NYUINACCT-CHK-SEC
                       IF      WRK-SYOKBN      =   SPACE
                           MOVE    4     TO   FLG-NG
                           DISPLAY "DOUJITU2"
                           PERFORM 200122-NYUINACCT-HENSYU-SEC
                           IF    ( WRK-DATAKBN     =   "1" ) 
                           OR    ( FLG-ERR         =    1  )
                                   MOVE    31              TO  IDX
                           END-IF
                       END-IF
                   END-IF
                 END-PERFORM
               END-IF
           END-IF        
           .
       200212-KOBETU-NYUINACCT-HEN-EXT.
           EXIT. 
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
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCR0800 "             DELIMITED  BY  SIZE
                   WRK-RECEERR             DELIMITED  BY  SIZE
                   LOW-VALUE               DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
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
                                                   FLG-ERR
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       600-ERRMSG-EDIT-SEC             SECTION.                        
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
                                           ERR-PTNUM
      *
      *    患者番号を取得
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    KEY-O-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF    ( SPTID-RC            =   ZERO )
               MOVE    SPTID-PTNUM     TO  ERR-PTNUM
           ELSE
               STRING "?(ID:"          DELIMITED   BY  SIZE
                      KEY-O-PTID       DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
               INTO   ERR-PTNUM
               END-STRING
           END-IF
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
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
           DISPLAY "ERR-ERRMSG=" ERR-ERRMSG
           DISPLAY "ERR-PTNUM=" ERR-PTNUM
      *
           EVALUATE    WRK-PARA-SYOKBN
             WHEN    "1"
               IF   FLG-NG  =  1
                 MOVE    "tbl_sryacct"     TO  MCP-TABLE
                 MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
               IF   FLG-NG  =  2
                 MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
                 MOVE    "key13"           TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
             WHEN    "2"
               IF   FLG-NG  =  3
                 MOVE    "tbl_sryacct"     TO  MCP-TABLE
                 MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
               IF   FLG-NG  =  4
                 MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
                 MOVE    "key14"           TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
               IF   WRK-PARA-SHELLID  =  "RECEPTX"
                 CONTINUE
               ELSE
                 MOVE      "tbl_syskanri"  TO  MCP-TABLE
                 MOVE      "key7"          TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
               END-IF
           END-EVALUATE
      *
           .
       600-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    IF     FLG-ERR     =   ZERO
      *    AND    CNT-OUT1    =   ZERO
      *        MOVE    "処理対象のデータがありませんでした"
      *                                TO  WRK-RECEERR
      *        PERFORM 500-ERR-HENSYU-SEC
      *    END-IF 
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
           DISPLAY "KARITEIKI (ORCR0800) CNT " CNT-OUT1
           DISPLAY "HIBETU    (ORCR0800) CNT " CNT-OUT2
           DISPLAY "*** ORCR0800 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYS2002-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYS2002
               MOVE    MCPDATA-REC     TO  SYS-2002-REC
           ELSE
               MOVE    1               TO  FLG-SYS2002
               INITIALIZE                  SYS-2002-REC
           END-IF
      *
           .
       900-SYS2002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
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
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO        TO  FLG-PTNYUINRRK
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ　ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    NACCT-PTID          TO  KEY-N-PTID 
      *********MOVE    NACCT-HKNCOMBI      TO  KEY-N-HKNCOMBI
      *********MOVE    NACCT-SRYKA         TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1           TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
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
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       900-SYUNOU-INV-SEC         SECTION.
      *
           MOVE       ZERO              TO      FLG-DATA
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO      SYU-HOSPNUM
           MOVE    "1"                  TO      SYU-NYUGAIKBN
           MOVE    KEY-N-PTID           TO      SYU-PTID
           MOVE    WRK-PARA-SRYYM       TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key17"              TO      MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
             MOVE    "tbl_syunou"       TO      MCP-TABLE
             MOVE    "key17"            TO      MCP-PATHNAME
             PERFORM  900-SYUNOU-READ-SEC
             PERFORM  UNTIL
                            (FLG-SYUNOU     =    1)  OR
                            (FLG-DATA       =    1)
      *
               EVALUATE    WRK-PARA-RECEKBN
                   WHEN    "3"
                   WHEN    "4"
                   WHEN    "5"
                       IF      SYU-SYUHKNNUM   =   WRK-CONS-HKNNUM
                           MOVE     1              TO  FLG-DATA
                       END-IF
                   WHEN    "8"
                       IF      SYU-KOH-HKNNUM (1)
                                               =   WRK-CONS-HKNNUM
                           MOVE     1              TO  FLG-DATA
                       END-IF
               END-EVALUATE
      *
               MOVE    "tbl_syunou"     TO      MCP-TABLE
               MOVE    "key17"          TO      MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
      *
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key17"              TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUNOU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスタ読込
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN     =   "1"
                       MOVE    1               TO  FLG-PTRSIINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTRSIINF
                   INITIALIZE                      PTRSIINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTRSIINF
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-INV-EXT.
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
               STRING  "ORCR0800 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
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
