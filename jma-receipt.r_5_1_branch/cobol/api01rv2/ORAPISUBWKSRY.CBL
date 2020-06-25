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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPISUBWKSRY.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 中途終了データ編集サブ
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/09/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDABAN          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM        PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID         PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PARA                PIC 9(01).
      *
           03  FLG-SURYO               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-W               PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *    03  IDY1                PIC 9(04).
      *    03  IDY2                PIC 9(04).
      *
           03  TBL-MAX             PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
           03  IDX-I               PIC 9(04).
           03  IDX-I1              PIC 9(04).
           03  IDX-I2              PIC 9(04).
           03  IDX-IMAX            PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC 9(04).
           03  WRK-IN-HKNCOMBI         PIC 9(04).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-EDABAN              PIC 9(03).
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-RENNUM              PIC 9(01).
      *
      *    診療内容
           03  WRK-KAISU               PIC 9(05).
           03  WRK-KAISU-23            PIC 9(05).
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-ZAITENKEI           PIC 9(08).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SURYO               PIC 9(05)V9(5).
      *    コメント入力値編集
           03  WRK-INPUTCHI-G.
               05  WRK-INPUTCHI        PIC X(40)   OCCURS  5.
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
           03  WRK-TERMID              PIC X(64).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    ワーク診療行為マスタ−テーブル
       01  TBL-WKSRYACT-AREA.
           02  TBL-WKSRYACT-REC      OCCURS   40.
           COPY    "CPWKSRYACT.INC"  REPLACING  //WKSRY//
                                     BY         //TBL-WKSRY//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    保険組合せセット領域
      **   COPY    "CPORCSAPIHKN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
      *    COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "MCPAREA".
      *    スパ領域
           COPY    "COMMON-SPA".
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPISUBWKSRY.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           ORAPISUBWKSRYAREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *****INITIALIZE                  ORAPIWKSRY-HKNCOMBI-AREA
           INITIALIZE                  ORAPIWKSRY-ERRCD
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      ORAPIWKSRY-ERRCD    =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF     (SPA-PTID            =   ZERO )
              OR  (SPA-SRYKA           =   SPACE)
              OR  (SPA-SRYYMD          =   SPACE)
              OR  (ORAPIWKSRY-HKNCOMBI =   SPACE)
      *        未設定項目
               MOVE    "0001"              TO  ORAPIWKSRY-ERRCD
               MOVE    "項目未設定"        TO  ORAPIWKSRY-ERRMSG
               GO      TO      100-INIT-EXT
           END-IF
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
           MOVE    ORAPIWKSRY-HKNCOMBI     TO  WRK-HKNCOMBI
      *
      *    ワーク診療行為データOPEN
      **** MOVE    SPA-TERMID          TO  FILE-TERMID
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM
      *
      **** OPEN    OUTPUT              PARA-FILE
      *
      *    一時データ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      * 
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療行為処理
           PERFORM 2002-WKSRYACT-SYORI-SEC
      *
           IF     (CNT-SRYCNT          =   ZERO )
             AND  (ORAPIWKSRY-ERRCD    =   SPACE)
      *        対象なし
               MOVE    "0003"              TO  ORAPIWKSRY-ERRCD
               MOVE    "対象データなし"    TO  ORAPIWKSRY-ERRMSG
           END-IF
      *
      *****CLOSE                       PARA-FILE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       2002-WKSRYACT-SYORI-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  IDX-W
      *    診療情報設定
           PERFORM VARYING   IDY   FROM    1   BY  1
                   UNTIL    (IDY           >   40)
                       OR   (ORAPIWKSRY-MDC-CLASS (IDY) =   SPACE )
                       OR   (ORAPIWKSRY-ERRCD  NOT =   SPACE )
      *        剤毎の編集
               PERFORM 200211-ZAI-HENSYU-SEC
               IF      TBL-MAX             >   ZERO
      *            中間データ出力
                   PERFORM 200212-WKSRYACT-OUT-SEC
               END-IF
           END-PERFORM
      *
           .
       2002-WKSRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎の編集処理
      *****************************************************************
       200211-ZAI-HENSYU-SEC     SECTION.
      *
           MOVE    ZERO                TO  TBL-MAX
           MOVE    SPACE               TO  TBL-WKSRYACT-AREA
           INITIALIZE                      TBL-WKSRYACT-AREA
      *
      *    診療種別
      *    診療種別検索
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBN
                   WHEN    ORAPIWKSRY-MDC-CLASS (IDY)
                                               =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYSYUKBN (MSYU-IDX)
                                                TO  WRK-SRYSYUKBN
           END-SEARCH
      *
           IF      WRK-SRYSYUKBN       =   SPACE
               MOVE    ORAPIWKSRY-MDC-CLASS (IDY)(1:2) TO  WRK-SRYKBN
           ELSE
      *        入力できない診療種別は削除
               EVALUATE    WRK-SRYSYUKBN
      *            ギブス
                   WHEN    "520"
      *            Ｘ線
                   WHEN    "710"
                   WHEN    "711"
                   WHEN    "712"
                   WHEN    "713"
                   WHEN    "720"
                   WHEN    "721"
                   WHEN    "723"
                   WHEN    "724"
      *                診療種別エラー
                       MOVE    SPACE           TO  WRK-SRYSYUKBN
               END-EVALUATE
           END-IF
      *
      *    剤回数
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  WRK-KAISU-23
           IF     (ORAPIWKSRY-MDC-CLASS-NUMBER (IDY)(1:1)  =   "*")
      *        日付指定はなし（入院なし）
               MOVE    "0002"              TO  ORAPIWKSRY-ERRCD
               MOVE    "回数指定エラー"    TO  ORAPIWKSRY-ERRMSG
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    ORAPIWKSRY-MDC-CLASS-NUMBER (IDY)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   3     )
                   MOVE    1                   TO  WRK-KAISU
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-KAISU
               END-IF
           END-IF
           IF      WRK-KAISU           =   ZERO
               MOVE    1                   TO  WRK-KAISU
           END-IF
      *    外用薬で薬剤の時、回数＝１　編集
           IF      WRK-SRYKBN          =   "23"
               MOVE    WRK-KAISU           TO  WRK-KAISU-23
               MOVE    1                   TO  WRK-KAISU
           END-IF
      *
           MOVE    ZERO                TO  IDX-S
           MOVE    1                   TO  TBL-MAX
      *    剤内容
           PERFORM VARYING   IDZ   FROM    1   BY  1
                   UNTIL    (IDZ           >   40)
               IF     (ORAPIWKSRY-PRSCRPT-CODE (IDY IDZ)
                                           NOT =   SPACE  )
               OR     (ORAPIWKSRY-ERRCD    NOT =   SPACE  )
                   ADD     1               TO  IDX-S
                   IF      IDX-S           >   5
                       ADD     1               TO  TBL-MAX
                       MOVE    1               TO  IDX-S
                   END-IF
                   PERFORM 200212-SRYCD-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           .
       200211-ZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード明細編集処理
      *****************************************************************
       200212-SRYCD-HENSYU-SEC     SECTION.
      *
      *    診療コード
           INITIALIZE                      ORCSNUMAREA
           MOVE    ORAPIWKSRY-PRSCRPT-CODE (IDY IDZ)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    ORAPIWKSRY-PRSCRPT-CODE (IDY IDZ)
                                           TO  TBL-WKSRY-SRYCD
                                                     (TBL-MAX IDX-S)
           ELSE
               MOVE    SNUM-OUTEDT(7:9)    TO  TBL-WKSRY-SRYCD
                                                     (TBL-MAX IDX-S)
           END-IF
      *    点数マスタ検索
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    入力コード
           IF      FLG-TENSU           =   ZERO
               MOVE    TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                       TO  TBL-WKSRY-INPUTCD
                                             (TBL-MAX IDX-S)
      *        時間外区分設定
               IF     (ORAPIWKSRY-TIMEFLG  NOT =   SPACE )
                  AND (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                           =   "1"   )
                  AND (TNS-DATAKBN         =   "1"   )
                  AND (TNS-SRYKBN          =   "11"
                                           OR  "12"
                                           OR  "13"    )
                   PERFORM 2002121-TIMEKBN-HEN-SEC
               END-IF
           ELSE
               MOVE    ORAPIWKSRY-PRSCRPT-CODE (IDY IDZ)
                                       TO  TBL-WKSRY-INPUTCD
                                             (TBL-MAX IDX-S)
           END-IF
           MOVE    ZERO                 TO  FLG-SURYO
      *    コメント値
           IF    ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:2)
                                       =   "84"   )  OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0084"  ) OR
      *H30.4
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:7)
                                       =   "0992081") OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                       =   "001"   ))  AND
                  (ORAPIWKSRY-PRSCRPT-NUMBER(IDY IDZ)
                                   NOT =   SPACE )
               PERFORM 200321-INPUTCHI-HEN-SEC
               MOVE    1               TO  FLG-SURYO
           END-IF
      *    画像診断のフィルム
           IF    ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                       =   "7"   )  OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                       =   "059"  ))  AND
                  (TNS-DATAKBN         =   "3"    )   AND
                  ( WRK-SRYKBN         =   "70"   )   AND
                  (ORAPIWKSRY-PRSCRPT-NUMBER(IDY IDZ)  NOT =   SPACE )
               PERFORM 200322-FIRUMU-HEN70-SEC
               MOVE    1               TO  FLG-SURYO
           END-IF
      *
           IF      FLG-SURYO           =   ZERO
      *        数量
               MOVE    ZERO                TO  WRK-SURYO
               INITIALIZE                      ORCSNUMAREA
               MOVE    ORAPIWKSRY-PRSCRPT-NUMBER(IDY IDZ)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   3     )
                   MOVE    SNUM-OUTNUM         TO  WRK-SURYO
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-SURYO
               END-IF
               IF      WRK-SURYO           =   ZERO
                   MOVE    1               TO  WRK-SURYO
               END-IF
               MOVE    WRK-SURYO           TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
           END-IF
      *    フリーコメントの内容
           IF     ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                       =   "810000001")  OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:2)
                                       =   "83"      )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0083"    )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0085"    )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0086"    ))  AND
                   (ORAPIWKSRY-PRSCRPT-NAME (IDY IDZ)
                                   NOT =   SPACE )
      *        コメント
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    ORAPIWKSRY-PRSCRPT-NAME (IDY IDZ)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT
                                       TO  TBL-WKSRY-INPUTCOMENT
                                                       (TBL-MAX IDX-S)
               END-IF
           END-IF
      *
      *    外用薬で薬剤の時、回数を編集
           IF       WRK-SRYKBN         =   "23"
      *        総量へ変更
               IF      TBL-WKSRY-SRYSURYO (TBL-MAX IDX-S)  >   ZERO
                   COMPUTE WRK-SURYO       =   TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
                                           *   WRK-KAISU-23
                   MOVE    WRK-SURYO       TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
               END-IF
               MOVE    WRK-KAISU-23    TO  TBL-WKSRY-SRYKAISU
                                                       (TBL-MAX IDX-S)
           END-IF
      *
      *H24.4
      *    一般名記載判定
           IF      SPA-SRYYMD          >=  "20120401"
               IF     (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)    =   "6")
                  AND (ORAPIWKSRY-GENERIC-FLG(IDY IDZ)
                                                   =   "yes"
                                                   OR  "no"   )
                  AND (WRK-SRYKBN                  =   "21"
                                                   OR  "22"
                                                   OR  "23"  )
                   PERFORM 200329-GENERICHEN-SEC
               END-IF
           END-IF
      *
           .
       200212-SRYCD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    時間外区分編集処理
      *****************************************************************
       2002121-TIMEKBN-HEN-SEC         SECTION.
      *
      *    時間外加算算定可
           IF     (TNS-TIMEKSNKBN      =   "1"  )
      *       （労災再診料に加算区分の設定がないので判定）
              OR  (TNS-SRYCD           =   "101120010")
               MOVE    SPACE               TO  TBL-WKSRY-INPUTCD
                                                   (TBL-MAX IDX-S)
               STRING  ORAPIWKSRY-TIMEFLG
                       " "
                       TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                           DELIMITED  BY  SIZE
                                        INTO   TBL-WKSRY-INPUTCD
                                                   (TBL-MAX IDX-S)
               END-STRING
           END-IF
           .
       2002121-TIMEKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    コメント値編集処理
      *****************************************************************
       200321-INPUTCHI-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCHI-G
           MOVE    1                   TO  IDX-I1
           MOVE    ZERO                TO  IDX-I2
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL      (IDX-I   >   40  )   OR
                              (IDX-I1  >   5   )
               IF      ORAPIWKSRY-PRSCRPT-NUMBER(IDY IDZ)(IDX-I:1)
                                           =   "-"
                   ADD     1                   TO  IDX-I1
                   MOVE    ZERO                TO  IDX-I2
               ELSE
                   ADD     1                   TO  IDX-I2
                   MOVE    ORAPIWKSRY-PRSCRPT-NUMBER(IDY IDZ)(IDX-I:1)
                                               TO  WRK-INPUTCHI
                                                     (IDX-I1)(IDX-I2:1)
               END-IF
           END-PERFORM
      *    用法コメントは５まで
           IF      TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)    =   "001"
               MOVE    5                   TO  IDX-IMAX
           ELSE
               MOVE    4                   TO  IDX-IMAX
           END-IF
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL       IDX-I   >   IDX-IMAX
               IF      WRK-INPUTCHI(IDX-I) NOT =   SPACE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-INPUTCHI(IDX-I)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
      *            整数のみ対象
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN         =   "1"   )   OR
                          (SNUM-SEISUU         >   8     )   OR
                          (SNUM-SYOSUU         >   5     )
                        CONTINUE
                   ELSE
      *                整数がゼロの時、ゼロ
                       IF      SNUM-SEISUU     =   ZERO
                           MOVE    "0"         TO  TBL-WKSRY-INPUTCHI
                                                   (TBL-MAX IDX-S IDX-I)
                       ELSE
                           COMPUTE IDX-I2      =   15  -   SNUM-SEISUU
                                               +   1
                           MOVE    SNUM-OUTEDT(IDX-I2:SNUM-SEISUU)
                                               TO  TBL-WKSRY-INPUTCHI
                                                  (TBL-MAX IDX-S IDX-I)
                       END-IF
      *                ユーザコメントで小数部がある時、そのまま
                       IF   ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                               =   "0084"   )  OR
                             (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                               =   "001"    )
      *H30.4
                         OR  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:7)
                                               =   "0992081" ))
                        AND  (SNUM-SYOSUU      >   0        )
                           MOVE    WRK-INPUTCHI(IDX-I)
                                               TO  TBL-WKSRY-INPUTCHI
                                                   (TBL-MAX IDX-S IDX-I)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200321-INPUTCHI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画像診断のフィルム数量編集処理
      *****************************************************************
       200322-FIRUMU-HEN70-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCHI-G
           MOVE    1                   TO  IDX-I1
           MOVE    ZERO                TO  IDX-I2
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL      (IDX-I   >   12  )   OR
                              (IDX-I1  >   4   )
               IF      ORAPIWKSRY-PRSCRPT-NUMBER(IDY IDZ)(IDX-I:1)
                                           =   "-"
                   ADD     1                   TO  IDX-I1
                   MOVE    ZERO                TO  IDX-I2
               ELSE
                   ADD     1                   TO  IDX-I2
                   MOVE    ORAPIWKSRY-PRSCRPT-NUMBER(IDY IDZ)(IDX-I:1)
                                               TO  WRK-INPUTCHI
                                                     (IDX-I1)(IDX-I2:1)
               END-IF
           END-PERFORM
      *    数量
           MOVE    ZERO                TO  WRK-SURYO
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-INPUTCHI(1)     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   5     )
               MOVE    ZERO                TO  WRK-SURYO
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-SURYO
           END-IF
           IF      WRK-SURYO           =   ZERO
               MOVE    1               TO  WRK-SURYO
           END-IF
           MOVE    WRK-SURYO           TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
      *    分画数
           IF      WRK-INPUTCHI(2) NOT =   SPACE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-INPUTCHI(2)     TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   5     )
                   CONTINUE
               ELSE
                   MOVE    SNUM-OUTNUM         TO  TBL-WKSRY-SRYKAISU
                                                     (TBL-MAX IDX-S)
               END-IF
           END-IF
      *
           .
       200322-FIRUMU-HEN70-EXT.
           EXIT.
      *****************************************************************
      *    一般名記載編集処理
      *****************************************************************
       200329-GENERICHEN-SEC         SECTION.
      *
            IF     IDX-S               =   5
                ADD     1                  TO  TBL-MAX
                MOVE    ZERO               TO  IDX-S
           END-IF
      *
           ADD     1                   TO  IDX-S
           EVALUATE    ORAPIWKSRY-GENERIC-FLG(IDY IDZ)
               WHEN    "yes"
      *            一般名記載
                   MOVE    "099209908"         TO  TBL-WKSRY-SRYCD
                                                     (TBL-MAX IDX-S)
               WHEN    "no"
      *            銘柄名記載
                   MOVE    "099209907"         TO  TBL-WKSRY-SRYCD
                                                     (TBL-MAX IDX-S)
           END-EVALUATE
           MOVE    TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                       TO  TBL-WKSRY-INPUTCD
                                                      (TBL-MAX IDX-S)
           MOVE    1                   TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
      *    次行は削除
           IF      IDZ                 <   40
               IF      ORAPIWKSRY-PRSCRPT-CODE (IDY IDZ + 1)
                                           =   "099209908"
                                           OR  "099209907"
                   MOVE    SPACE       TO  ORAPIWKSRY-PRSCRPT-CODE
                                                       (IDY IDZ + 1)
               END-IF
           END-IF
           .
       200329-GENERICHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間データ出力処理
      *****************************************************************
       200212-WKSRYACT-OUT-SEC     SECTION.
      *
      *    院内分は剤点数を設定する
           MOVE    ZERO                TO  WRK-ZAITENKEI
           IF     (SPA-SYOHOKBN        =   "0"       )   AND
                  (WRK-SRYKBN          =   "21"  OR  "22"  OR  "23")
               IF      WRK-SRYSYUKBN(3:1)  =   "2"
                   CONTINUE
               ELSE
                   MOVE    1000                TO  WRK-ZAITENKEI
               END-IF
           END-IF
      *
           ADD     1                   TO  WRK-ZAINUM
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX-W   FROM    1   BY  1
                   UNTIL      (IDX-W   >   TBL-MAX)
               MOVE    TBL-WKSRYACT-REC (IDX-W)    TO  WKSRYACT-REC
               PERFORM 2002121-WKSRYACT-HEN-SEC
      *        中間データ出力
      *        MOVE    SPACE               TO  PARA-REC
      *        MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *        ADD     1                   TO  WRK-EDABAN
      *        MOVE    WRK-EDABAN          TO  PARA-EDABAN
      *
      *        ワーク診療行為マスタからの作成判定とする
               MOVE    "WKSRYACT"          TO  WKSRY-TERMID
      *
      ***      MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *****    WRITE                       PARA-REC
      *        一時データ出力
               PERFORM 2002129-PARA-DBINSERT-SEC
      *
               ADD     1                   TO  CNT-SRYCNT
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  TBL-MAX
           MOVE    SPACE               TO  TBL-WKSRYACT-AREA
           INITIALIZE                      TBL-WKSRYACT-AREA
           .
       200212-WKSRYACT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタＤＢ出力処理
      *****************************************************************
       2002129-PARA-DBINSERT-SEC          SECTION.
      *
           ADD     1                   TO  WRK-EDABAN
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    WRK-EDABAN          TO  PARA-EDANUM
           MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
               MOVE    "0009"              TO  ORAPIWKSRY-ERRCD
               MOVE    "一時データ出力エラー"
                                           TO  ORAPIWKSRY-ERRMSG
           END-IF
           .
       2002129-PARA-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為編集処理
      *****************************************************************
       2002121-WKSRYACT-HEN-SEC     SECTION.
      *
           ADD     1                   TO  WRK-RENNUM
      *
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    WRK-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    WRK-ZAINUM          TO  WKSRY-ZAINUM
           MOVE    WRK-RENNUM          TO  WKSRY-RENNUM
           MOVE    WRK-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    WRK-KAISU           TO  WKSRY-ZAIKAIKEI
           MOVE    WRK-ZAITENKEI       TO  WKSRY-ZAITENKEI
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
          .
       2002121-WKSRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
