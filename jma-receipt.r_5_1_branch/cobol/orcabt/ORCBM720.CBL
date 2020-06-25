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
       PROGRAM-ID.             ORCBM720.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 光ディスク等送付書（労災）
      *  管理者            :
      *  作成日付    作業者        記述
      *  08/11/21    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-藤原    15/06/18  光ディスク等送付書の媒体種類
      *                                     設定対応
      *  04.07.02    NACL-藤原    15/08/18  医療機関編集情報への開設者名
      *                                     追加対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-藤原    17/04/12  平成２９年３月診療分からの
      *                                     様式変更対応
      *
      *  05.00.01    NACL-藤原    19/01/18  ＦＤ及びＭＯによる請求の終了
      *                                     に伴う様式変更対応
      *                                     Warning 対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      **** COPY    "HCM78.INC".
           COPY    "HCM78V02.INC"  REPLACING  //HCM78V02//
                                   BY         //HCM78//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03).
           03  IDY                 PIC 9(03).
           03  IDZ                 PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-TEISYUTUSAKI
                                   PIC X(01).
           05  WRK-PARA-DATAKBN    PIC X(01).
           05  WRK-PARA-JOBID      PIC 9(07).
           05  WRK-PARA-SHELLID    PIC X(08).
           05  WRK-PARA-HOSPNUM    PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *    エラーメッセージ
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
      *
      *    医療機関情報編集用ワーク
           03  WRK-HOSPNAME        PIC X(60).
           03  WRK-ADRS.
               05  WRK-ADRS1       PIC X(40).
               05  WRK-ADRS2       PIC X(40).
               05  WRK-ADRS3       PIC X(20).
      *
           03  WRK-KAISETUNAME     PIC X(80).
      *
           03  WRK-RSIIRYOCD       PIC X(07).
      *
      *    数字半角全角変換用
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-IDX    PIC 9(02).
               05  WRK-CHAR-FLG    PIC 9(01).
      *
               05  WRK-CHAR-X      PIC X(10).
               05  WRK-CHAR-N      PIC X(20).
               05  WRK-CHAR-CNT    PIC 9(02).
               05  WRK-CHAR-N-CNT  PIC 9(02).
      *
      *    数字変換用
           03  WRK-9                   PIC 9(04).
           03  WRK-9-R                 REDEFINES   WRK-9.
               05  FILLER              PIC X(03).
               05  WRK-9-X             PIC X(01).
      *
      *    日付編集用ワーク
       01  WRK-HEN-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-HENYMDG.
               05  WRK-HENYMD-G    PIC X(04).
               05  WRK-HENYMD-Y    PIC X(04).
               05  WRK-HENYMD-FIL1 PIC X(02).
               05  WRK-HENYMD-M    PIC X(04).
               05  WRK-HENYMD-FIL2 PIC X(02).
               05  WRK-HENYMD-D    PIC X(04).
      *
      *    全角数字
       01  TBL-SUJI-AREA.
           03  FILLER                  PIC X(20)   VALUE
               "１２３４５６７８９０".
       01  TBL-SUJI-R                  REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI                PIC X(02)   OCCURS  10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
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
           COPY    "CPSK1903.INC".
      *
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
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
      *    サブプロ用領域
      *****************************************************************
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    県名取得パラメタ
           COPY    "CPORCSPREF.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *
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
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "*** ORCBM720 START ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
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
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
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
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBM720"          TO  JOB-PGID
           MOVE    "光ディスク等送付書（労災）"
                                       TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    システム管理マスタ取得      
           PERFORM 1001-SYSKANRI-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム管理マスタ取得      
      *****************************************************************
       1001-SYSKANRI-HENSYU-SEC      SECTION.
      *
      *    医療機関情報編集
           INITIALIZE                      SYS-1001-REC
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
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           INITIALIZE                      SYS-1002-REC
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
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-ADRS
      *
      *    医療機関編集情報取得
           PERFORM 900-1900-READ-SEC
      *
      *    労災医療機関情報取得
           INITIALIZE                      SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-4001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-4001-STYUKYMD (7:2)
           MOVE    SYS-4001-STYUKYMD   TO  SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
           ELSE
               MOVE    "労災指定医療機関情報が取得できません"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    労災指定医療機関番号を数字変換
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SYS-4001-RSIIRYOCDN TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-SYUBETU     =   "2"  )
               INITIALIZE                      WRK-CHAR-HENKAN-AREA
               MOVE    SYS-4001-RSIIRYOCDN TO  WRK-CHAR-N
               MOVE    13                  TO  WRK-CHAR-N-CNT
               IF      KANACHK-MAX         <   14
                   COMPUTE WRK-CHAR-N-CNT  =   KANACHK-MAX   -   1
               END-IF
               PERFORM 800-NUM-HANKAKU-HENKAN-SEC
               MOVE    WRK-CHAR-X          TO  WRK-RSIIRYOCD
           END-IF
      *
      *    レセプト・総括編集情報
           INITIALIZE                      SYS-25-REC
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-25-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-25-STYUKYMD (7:2)
           MOVE    SYS-25-STYUKYMD     TO  SYS-25-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-25-HOSPNUM
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-25-REC
           ELSE
               INITIALIZE                      SYS-25-REC
           END-IF
      *    光ディスク等送付書（媒体種類）
           IF      SYS-25-RECEDEN-LST2-MEDIA
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-RECEDEN-LST2-MEDIA
           END-IF
           DISPLAY "SYS-200501-RECEDEN-LST2-MEDIA="
                                       SYS-25-RECEDEN-LST2-MEDIA
           DISPLAY "WRK-PARA-DATAKBN             ="
                                       WRK-PARA-DATAKBN
      *
           .
       1001-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      HCM78
      *
           ADD     1                   TO  CNT-PAGE
      *
      *    年月日
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD-G        TO  HCM78-PRTG
           MOVE    WRK-HENYMD-Y        TO  HCM78-PRTY
           MOVE    WRK-HENYMD-M        TO  HCM78-PRTM
           MOVE    WRK-HENYMD-D        TO  HCM78-PRTD
      *
           INITIALIZE                      SPREF-AREA
           CALL    "ORCSPREF"          USING   SPREF-AREA
                                               SPA-AREA
      *
           MOVE    SYS-1001-PREFNUM    TO  IDX
       *    東京、大阪、京都は「府、都」を消去
           IF      IDX                 =   13  OR  26  OR  27
               MOVE    SPACE               TO  SPREF-PREFNAME2 (IDX)(5:)
           END-IF
           STRING  SPREF-PREFNAME2 (IDX)   DELIMITED    BY  SPACE
                   "労働局　御中"          DELIMITED    BY  SIZE
                                           INTO    HCM78-TEISYUTUSAKI
           END-STRING
      *
      *    住所
           INSPECT WRK-ADRS            REPLACING   ALL "  "    BY  "　"
           MOVE    WRK-ADRS1           TO  HCM78-ADRS-S1
           MOVE    WRK-ADRS2           TO  HCM78-ADRS-S2
           MOVE    WRK-ADRS3           TO  HCM78-ADRS-S3
      *
      *    開設者
           MOVE    WRK-KAISETUNAME     TO  HCM78-KAISETUNAME
      *
      *    医療機関コード
           MOVE    WRK-RSIIRYOCD       TO  HCM78-HOSPCD
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM78-HOSPNAME
           INSPECT HCM78-HOSPNAME      REPLACING   ALL "  "    BY  "　"
      *
      *    点数表区分
           MOVE    "○"                TO  HCM78-TENHYOKBN
      *
      *    診療（調剤）月分
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD-G        TO  HCM78-SRYYMG
           MOVE    WRK-HENYMD-Y        TO  HCM78-SRYYMY
           MOVE    WRK-HENYMD-M        TO  HCM78-SRYYMM
      *
      *    提出年月日
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD-G        TO  HCM78-SKYYMG
           MOVE    WRK-HENYMD-Y        TO  HCM78-SKYYMY
           MOVE    WRK-HENYMD-M        TO  HCM78-SKYYMM
           MOVE    WRK-HENYMD-D        TO  HCM78-SKYYMD
      *
      *    媒体種類
           EVALUATE    SYS-25-RECEDEN-LST2-MEDIA
                                       ALSO    WRK-PARA-DATAKBN
      ****     WHEN    "0"             ALSO    "1"
      ****     WHEN    "0"             ALSO    "3"
      ****     WHEN    "1"             ALSO    ANY
      ****         MOVE    "○"                TO  HCM78-BAITAI (2)
      ****     WHEN    "0"             ALSO    "2"
      ****     WHEN    "0"             ALSO    "4"
      ****     WHEN    "2"             ALSO    ANY
      ****         MOVE    "○"                TO  HCM78-BAITAI (3)
               WHEN    "0"             ALSO    "6"
               WHEN    "3"             ALSO    ANY
                   MOVE    "○"                TO  HCM78-BAITAI (1)
           END-EVALUATE
      *
      *    枚数
      *????MOVE    WRK-CNT             TO  HCM78-CNT
      *
      *    帳票印刷処理
           PERFORM 2001-PRINT-OUT-SEC
      *    
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2001-PRINT-OUT-SEC           SECTION.
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
      **** IF      LNK-PRTKANRI-SRYYM  <   "201702"
      ****     MOVE    "HCM78.red"         TO  SPRT-PRTID
      **** ELSE
      ****     MOVE    "HCM78V01.red"      TO  SPRT-PRTID
      **** END-IF
           MOVE    "HCM78V02.red"      TO  SPRT-PRTID
           MOVE    "光ディスク等送付書（労災）"
                                       TO  SPRT-TITLE
           MOVE    HCM78               TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA 
           IF          SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       2001-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD2
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG     REPLACING   ALL "  "    BY  "　"
           .
       5002-HIZUKE-HEN-EXT.
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
           STRING  "ORCBM720 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM720 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値全角→半角変換処理
      *****************************************************************
       800-NUM-HANKAKU-HENKAN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-CHAR-CNT
                                           WRK-CHAR-IDX
      *
           PERFORM VARYING     IDZ     FROM    1   BY  2
                   UNTIL       IDZ     >       WRK-CHAR-N-CNT
               IF      WRK-CHAR-N (IDZ:2)  =   SPACE
                   MOVE    WRK-CHAR-N-CNT      TO  IDZ
               ELSE
                   MOVE    ZERO                TO  WRK-CHAR-FLG
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY             >   10
                           OR      WRK-CHAR-FLG    =   1
                       IF       WRK-CHAR-N (IDZ:2) =   TBL-SUJI (IDY)
                           ADD     1           TO  WRK-CHAR-IDX
                           IF      IDY         =   10
                               MOVE    "0"     TO
                                           WRK-CHAR-X (WRK-CHAR-IDX:1)
                           ELSE
                               MOVE    IDY         TO  WRK-9
                               MOVE    WRK-9-X     TO
                                           WRK-CHAR-X (WRK-CHAR-IDX:1)
                           END-IF
                           MOVE    1           TO  WRK-CHAR-FLG
                       END-IF
                   END-PERFORM
      *            数字以外のとき変換終了
                   IF      WRK-CHAR-FLG        =   ZERO
                       MOVE    WRK-CHAR-N-CNT      TO  IDZ
                       MOVE    SPACE               TO  WRK-CHAR-X
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       800-NUM-HANKAKU-HENKAN-EXT.
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
      *
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
               IF      SYS-1900-PRTKBN(21)  NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *            医療機関名称編集情報（２）
                   PERFORM 900-1903-READ-SEC
                END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
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
           MOVE    SYS-1900-PRTKBN (21)
                                   TO  SYS-1901-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1901-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
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
           MOVE    SYS-1900-PRTKBN(21)
                                   TO  SYS-1902-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1902-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-ADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-ADRS2
               MOVE    SYS-1902-ADRS3  TO  WRK-ADRS3
           ELSE
               INITIALIZE                      SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報（２）読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1903-REC
           INITIALIZE                  SYS-1903-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1903-HOSPNUM
           MOVE    "1903"          TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(21)
                                   TO  SYS-1903-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1903-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD
                                   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                           TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                      SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
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
