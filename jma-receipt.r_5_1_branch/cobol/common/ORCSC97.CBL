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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC97.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 画面入力コード編集変換処理
      *                      （Ｋ０２、Ｋ０５、Ｊ０４のみ共通使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/01/09    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−多々納 02/10/18  入院追加
      *  01.00.02    NACL−多々納 03/01/12  半角記号入力可能追加
      *  01.00.03    NACL−多々納 03/04/14  全角判定修正、約束処方修正
      *  01.00.04    NACL-多々納  05/11/22  MONFUNC 対応 他
      *  01.00.05    NACL-多々納  06/01/19  コメント８４対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.05.00    NACL-多々納  09/10/XX  多剤投与・換算値等対応
      *  04.06.00    NACL-多々納  11/02/14  用法コメント対応
      *  04.07.00    NACL-多々納  13/12/20  行挿入対応
      *  05.00.00    NACL-多々納  20/03/XX  コメント842、85対応他
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
      *
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-KAI             PIC 9(01).
           03  FLG-HI              PIC 9(01).
           03  FLG-ALLSUJI         PIC 9(01).
      *
           03  FLG-ZENKAKU         PIC 9(01).
           03  FLG-HANKAKU         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *R02.4
           03  FLG-CHK-ARI         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(02).
           03  IDX-D1              PIC 9(02).
           03  IDX-D2              PIC 9(02).
           03  IDX-Z               PIC 9(02).
      *
           03  MAP-IDX             PIC 9(04).
           03  GMN-IDX             PIC 9(04).
      *
           03  IDK                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-INPUTCD         PIC X(54).
           03  WRK-INPUTCD2        PIC X(54).
           03  WRK-INPUTCD3        PIC X(54).
      *
           03  WRK-CHK-FLG         PIC X(01).
      *
           03  WRK-KOUMOKU         PIC X(40).
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-STR             PIC 9(02).
           03  WRK-END             PIC 9(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
               05  WRK-SRYDD       PIC 9(02).
      *
           03  WRK-ICD-CDSYU       PIC X(01).
      *    パス名
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-CHK-INPUTCD     PIC X(09).
      *
       01  WRK-NYUMOJI-AREA.
           03  WRK-HI              PIC 9(02).
           03  WRK-NISUU           PIC X(54).
           03  WRK-MAE-CD          PIC X(01).
      *
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-AST             PIC 9(02).
           03  FLG-AUTO            PIC 9(02).
      *ver.4.5
           03  FLG-AINPUT          PIC 9(01).
           03  FLG-BINPUT          PIC 9(01).
           03  FLG-CINPUT          PIC 9(01).
           03  FLG-SEND            PIC 9(01).
      *
           03  FLG-JKNKSN          PIC 9(01).
           03  WRK-IDX-FT          PIC 9(02).
           03  WRK-CD-MCNT         PIC 9(02).
           03  WRK-ACCT-MCNT       PIC 9(02).
           03  WRK-SUR-MCNT        PIC 9(02).
           03  WRK-MCNT1           PIC 9(02).
           03  WRK-MCNT2           PIC 9(02).
           03  WRK-MCNT3           PIC 9(02).
           03  WRK-MCNT4           PIC 9(02).
           03  WRK-MCNT5           PIC 9(02).
           03  WRK-CD              PIC X(54).
           03  WRK-KAI             PIC X(54).
           03  WRK-SUR             PIC X(54).
           03  WRK-MOJI1           PIC X(10).
           03  WRK-MOJI2           PIC X(10).
           03  WRK-MOJI3           PIC X(10).
           03  WRK-MOJI4           PIC X(10).
           03  WRK-MOJI5           PIC X(10).
           03  IDM                 PIC 9(04).
           03  IDMS                PIC 9(04).
           03  WRK-HENKAN-MAE      PIC X(54).
           03  WRK-HENKAN-ATO      PIC X(54).
           03  IDX-D               PIC 9(04).
           03  WRK-BUNGSU          PIC 9(08).
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
      *    入院期間
       01  TBL-NYUIN-KAISU-AREA.
           03  TBL-NYUIN-OCC           OCCURS   10.
               05  TBL-HI              PIC 9(02).
               05  TBL-SEL             PIC X(01).
      *
      *    入力コード項目桁数
       01  CONS-INPUT              PIC 9(02)   VALUE   54.
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    全角・半角チェック
           COPY    "CPORCXKANACONV.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    一括日変換パラメタ
           COPY    "CPORCSNDAYCHG.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSC97AREA
           SPA-SCR-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           INITIALIZE                      WRK-MOJI-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      LNK-SC97-OUTAREA
      *R02.4
      *    +,-　842対応の為
           IF     (LNK-SC97-GMN-IDX    NOT =   ZERO )
               MOVE    SPA-COM-INPUTCD(LNK-SC97-GMN-IDX)
                                           TO  WRK-CHK-INPUTCD
           ELSE
               MOVE    SPACE               TO  WRK-CHK-INPUTCD
           END-IF
      *
           PERFORM 100-INPUTCD-SYORI-SEC
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (FLG-END             =   ZERO  )
               IF      LNK-SC97-KBN        =   "1"
      *            半角変換のみの時
                   PERFORM 1001-SYORI-KBN1-SEC
               ELSE
      *            内容変換
                   PERFORM 1002-SYORI-KBN2-SEC
               END-IF
      *
      *R02.4
               IF    (SPA-ERRCD        =   SPACE )
                 AND (FLG-CHK-ARI      =   1     )
      *            +,- がある時コード決定後、再チェック
                   IF      WRK-CHK-INPUTCD(1:3) NOT =   "842"
                       PERFORM 100-INPUTCD-SYORI-SEC
                   END-IF
               END-IF
           END-IF
      *
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    全角、半角変更編集処理
      *****************************************************************
       100-INPUTCD-SYORI-SEC            SECTION.
      *
      *    半角カナの全角変換
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    CONS-INPUT          TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    LNK-SC97-INPUTCD    TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           MOVE    KANACONV-OUTDATA    TO  LNK-SC97-INPUTCD
      *
      *    '_'をなくす
           INSPECT LNK-SC97-INPUTCD
                   TALLYING    LNK-SC97-JODOU  FOR  ALL "_"
                   REPLACING   ALL "_"  BY  " "
      *
      *    １桁目全角、半角変更
           PERFORM 100-MAE-HANKAKU-SEC
           IF      WRK-INPUTCD     NOT =   SPACE
               MOVE    WRK-INPUTCD         TO  LNK-SC97-INPUTCD
               MOVE    1                   TO  FLG-END
           END-IF
      *    保険・診療科
           IF      LNK-SC97-INPUTCD(1:1)   =   "#"
                                           OR  "$"
      *        外来でＨ１６．４月からとする
               IF     (LNK-SC97-PG         =   "K02 " )  AND
                      (SPA-SRYYMD          >=  "20040401")
                   CONTINUE
               ELSE
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "1055"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       100-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角変換のみの時処理
      *****************************************************************
       1001-SYORI-KBN1-SEC            SECTION.
      * 
      *    数量ゼロの判定
           IF      (LNK-SC97-PG        =   "K02N"
                                       OR  "KT01" )  AND
                   (LNK-SC97-INPUTCD(1:1)  =   "*" )
               CONTINUE
           ELSE
               PERFORM 1003-SURYOU-CHK-SEC
           END-IF
      *
           .
       100-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容変換・チェック処理
      *****************************************************************
       1002-SYORI-KBN2-SEC            SECTION.
      *
      *    入院の回数
           IF      (LNK-SC97-PG        =   "K02N"
                                       OR  "KT01" )  AND
                   (LNK-SC97-INPUTCD(1:1)  =   "*"    )
               PERFORM 1002-NYUINKAISU-SEC
           ELSE
      *        入力コードの判定
               PERFORM 200-INPUTCD-CHK-SEC
           END-IF
      *
           .
       1002-SYORI-KBN2-EXT.
           EXIT
           .
      *
      *****************************************************************
      *    全角、半角変更編集処理
      *****************************************************************
       100-MAE-HANKAKU-SEC            SECTION.
      *
           MOVE    SPACE              TO  WRK-INPUTCD2
           MOVE    SPACE              TO  WRK-INPUTCD
      *    漢字変換
           MOVE    1                   TO  IDH1
           MOVE    ZERO                TO  IDH2
           PERFORM
      *****        UNTIL       IDH1    >   22
                   UNTIL       IDH1    >   CONS-INPUT
      *      全角判定
             IF      IDH1             =    CONS-INPUT
                 MOVE    ZERO              TO  FLG-ZENKAKU
             ELSE
                 MOVE    LNK-SC97-INPUTCD(IDH1:2) TO  WRK-KOUMOKU
                 MOVE    2                        TO  WRK-LEN
                 PERFORM 1009-KANACONV-SEC
             END-IF
             IF      FLG-ZENKAKU         =   1
      *****  IF      LNK-SC97-INPUTCD(IDH1:1)
      *****                               >=  X"A1"
               EVALUATE    LNK-SC97-INPUTCD(IDH1:2)
                   WHEN    "−"
                       ADD     1           TO  IDH2
                       MOVE    "-"         TO  WRK-INPUTCD2(IDH2:1)
                   WHEN    "＋"
                       ADD     1           TO  IDH2
                       MOVE    "+"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "！"
                       ADD     1           TO  IDH2
                       MOVE    "!"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "／"
                       ADD     1           TO  IDH2
                       MOVE    "/"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "　"
                       ADD     1           TO  IDH2
                       MOVE    SPACE       TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    OTHER
                       ADD     1           TO  IDH2
                       MOVE    LNK-SC97-INPUTCD(IDH1:2)
                                           TO  WRK-INPUTCD2 (IDH2:2)
                       ADD     1           TO  IDH2
               END-EVALUATE
               ADD     2                   TO  IDH1
             ELSE
               ADD     1                   TO  IDH2
               MOVE    LNK-SC97-INPUTCD(IDH1:1)
                                           TO  WRK-INPUTCD2 (IDH2:1)
               ADD     1                   TO  IDH1
             END-IF
           END-PERFORM
      *
      *    ’＋’、’−’のチェック
           MOVE    SPACE               TO  WRK-CHK-FLG
           MOVE    SPACE               TO  WRK-INPUTCD3
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX    >   CONS-INPUT
               IF      WRK-INPUTCD2(IDX:1)     =   "+"  OR  "-"
                   COMPUTE IDX3                =   IDX  +  1
      *            入院の回数指定はそのまま
                   IF     (WRK-INPUTCD2(1:1)   =   "*"  )  AND
                          (WRK-INPUTCD2(IDX3:) NOT =   SPACE)
                       ADD     1                   TO  IDX2
                       MOVE    WRK-INPUTCD2(IDX:1) TO  WRK-INPUTCD3
                                                             (IDX2:1)
                   ELSE
                       IF    ( WRK-CHK-FLG         =   SPACE )  AND
                             ((IDX2                =   ZERO      ) OR
                              (IDX                 =   CONS-INPUT) OR
                              (WRK-INPUTCD2(IDX3:) =   SPACE     ))
                           MOVE    WRK-INPUTCD2(IDX:1) TO  WRK-CHK-FLG
      *!!!!!!!!!!!!!!!!!!
      *    連続挿入
                           IF     (WRK-INPUTCD2(IDX:2)  =   "++")
                               MOVE    1           TO  LNK-SC97-INSREN
                               ADD     1               TO  IDX
                           END-IF
      *!!!!!!!!!!!!!!!!!!
                       END-IF
      *!!!!!!!!!!!!!!!!!!
      *    連続挿入
                       COMPUTE IDX4                =   IDX  +  2
                       IF    ( WRK-CHK-FLG         =   SPACE )
                           IF     (WRK-INPUTCD2(IDX:2)  =   "++")
                            AND   (WRK-INPUTCD2(IDX4:)  =   SPACE)
                               MOVE    WRK-INPUTCD2(IDX:1)
                                                   TO  WRK-CHK-FLG
                               MOVE    1           TO  LNK-SC97-INSREN
                               ADD     1               TO  IDX
                           END-IF
                       END-IF
                       IF    ( WRK-CHK-FLG         =   SPACE )
                           IF     (WRK-INPUTCD2(IDX3:1)  NUMERIC )
                              AND (WRK-INPUTCD2(IDX4:) =   SPACE )
      *R02.4  +  のみ
                              AND (WRK-INPUTCD2(IDX:1) =   "+" )
                               MOVE    WRK-INPUTCD2(IDX:1)
                                                   TO  WRK-CHK-FLG
                               MOVE    WRK-INPUTCD2(IDX3:1)
                                                   TO  LNK-SC97-INSCNT
                               ADD     2           TO  IDX
                           END-IF
                       END-IF
      *!!!!!!!!!!!!!!!!!!
                   END-IF
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    WRK-INPUTCD2(IDX:1) TO  WRK-INPUTCD3
                                                             (IDX2:1)
      *            １，２桁目以外の '/'は、空白とする
                   IF     (WRK-INPUTCD2(IDX:1)     =   "/" )  AND
                          (IDX                 NOT =   1  AND   2)
                       IF     (WRK-INPUTCD2(1:1)   =   "*" )  OR
                              (WRK-INPUTCD2(1:3)   =   "///")
      *                    区分＋点数検索
                       OR     (WRK-INPUTCD2(1:3)   =   "//*")
                           CONTINUE
                       ELSE
                           MOVE    SPACE       TO  WRK-INPUTCD2(IDX:1)
                           MOVE    SPACE       TO  WRK-INPUTCD3(IDX2:1)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           MOVE    WRK-INPUTCD2        TO  LNK-SC97-INPUTCD
      *
      *    − 入力エラー
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    LNK-SC97-INPUTCD
                                       TO  WRK-INPUTCD
           ELSE
      *
      *        １桁目へ−，＋編集
               IF      WRK-CHK-FLG     NOT =   SPACE
                   MOVE    WRK-CHK-FLG         TO  WRK-INPUTCD(1:1)
                   MOVE    WRK-INPUTCD3        TO  WRK-INPUTCD(2:)
               END-IF
               IF      LNK-SC97-INPUTCD(1:2)   =   "!!"
                   MOVE    LNK-SC97-INPUTCD
                                       TO  WRK-INPUTCD
               END-IF
               IF      LNK-SC97-INPUTCD(1:1)   =   "!"  OR
                                                   " "  OR
                                                   "/"  OR
                                                   "-"  OR
                                                   "+"
                   MOVE    LNK-SC97-INPUTCD
                                           TO  WRK-INPUTCD
               END-IF
           END-IF
      *
           .
       100-MAE-HANKAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角判定処理
      *****************************************************************
       1009-KANACONV-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-ZENKAKU
           MOVE    ZERO                TO  FLG-HANKAKU
           MOVE    ZERO                TO  FLG-ERR
           IF      WRK-KOUMOKU     NOT =   SPACE
      *        半角カナの全角変換
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    WRK-LEN             TO  KANACONV-LEN
               MOVE    1000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    15                  TO  KANACONV-CHAR-TYPE
               MOVE    WRK-KOUMOKU         TO  KANACONV-INDATA
               CALL    "kanaconv"          USING ORCXKANACONVAREA
               IF      KANACONV-RETURN     =   ZERO
      *            全角
                   MOVE    1                   TO  FLG-ZENKAKU
               ELSE
      *            半角のみの判定
                   INITIALIZE                  ORCXKANACONVAREA
                   MOVE    WRK-LEN             TO  KANACONV-LEN
                   MOVE    1000                TO  KANACONV-STLEN
                   MOVE    0                   TO  KANACONV-CONV-FLAG
                   MOVE    112                 TO  KANACONV-CHAR-TYPE
                   MOVE    WRK-KOUMOKU         TO  KANACONV-INDATA
                   CALL    "kanaconv"          USING
                                               ORCXKANACONVAREA
                   IF      KANACONV-RETURN     =   ZERO
                       MOVE    1                   TO  FLG-HANKAKU
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
           .
       1009-KANACONV-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量ゼロの判定処理
      *****************************************************************
       1003-SURYOU-CHK-SEC    SECTION.
      *
           MOVE    LNK-SC97-INPUTCD    TO  WRK-INPUTCD
      *    １桁目の変換
           PERFORM 2001-JIKANGAI-CHK-SEC
      *
           IF     (LNK-SC97-INPUTCD(1:1)   IS  NUMERIC) AND
                  (LNK-SC97-INPUTCD(2:1)       =   SPACE  ) AND
                  (LNK-SC97-INPUTCD(3:1)   NOT =   SPACE  )
               MOVE    3                   TO  WRK-IDX-FT
           ELSE
               MOVE    1                   TO  WRK-IDX-FT
           END-IF
           IF      LNK-SC97-INPUTCD(1:1)   =   " "
      *        削除行
               CONTINUE
           ELSE
      *        数量以下の変換
               MOVE    LNK-SC97-GMN-IDX    TO  GMN-IDX
               PERFORM 300-SURYOU-SYORI-SEC
           END-IF
           IF      LNK-SC97-INPUTCD(1:1)   =   " "
      *        削除行
               CONTINUE
           ELSE
      *        入力コード文字列調査処理
               PERFORM 2020-INPUTCD-CHOSA-SEC
               MOVE    WRK-INPUTCD         TO  LNK-SC97-INPUTCD
               MOVE    WRK-CD              TO  LNK-SC97-INCD
           END-IF
      *
           .
       1003-SURYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の回数の判定処理
      *****************************************************************
       1002-NYUINKAISU-SEC    SECTION.
      *
           MOVE    ZERO                TO  LNK-SC97-NYUIN-KAISU
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
      *    全角半角変換
           MOVE    LNK-SC97-INPUTCD    TO  WRK-HENKAN-MAE
           MOVE    1                   TO  WRK-STR
           MOVE    CONS-INPUT          TO  WRK-END
           PERFORM 2010-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-ATO      TO  LNK-SC97-INPUTCD
      *
           INITIALIZE                      ORCSNDAYCHGAREA
           MOVE    SPA-SRYYMD          TO  ORCSNDAYCHG-SRYYMD
           MOVE    LNK-SC97-INPUTCD    TO  ORCSNDAYCHG-INPUTCD
           MOVE    CONS-INPUT          TO  ORCSNDAYCHG-KETA
           CALL    "ORCSNDAYCHG"       USING   SPA-AREA
                                               ORCSNDAYCHGAREA
           MOVE    ORCSNDAYCHG-ERRCD   TO  SPA-ERRCD
           MOVE    ORCSNDAYCHG-INPUTCD TO  LNK-SC97-INPUTCD
           MOVE    ORCSNDAYCHG-KAISUCNT
                                       TO  LNK-SC97-KAISUCNT
           MOVE    ORCSNDAYCHG-KAISU   TO  LNK-SC97-KAISU
           MOVE    ORCSNDAYCHG-NYUIN-KAISU
                                       TO  LNK-SC97-NYUIN-KAISU
      *
           .
       1002-NYUINKAISU-EXT.
           EXIT.
      *****************************************************************
      *    入力コードの判定処理
      *****************************************************************
       200-INPUTCD-CHK-SEC    SECTION.
      *
           MOVE    LNK-SC97-INPUTCD    TO  WRK-INPUTCD
      *    １桁目の変換
           PERFORM 2001-JIKANGAI-CHK-SEC
      *
           EVALUATE    LNK-SC97-INPUTCD(1:1)
               WHEN    "."
      *        診療種別
                   MOVE    4                   TO  WRK-CD-MCNT
                   MOVE    1                   TO  WRK-IDX-FT
      *            数量以下の変換
                   PERFORM 300-SURYOU-SYORI-SEC
               WHEN    "#"
               WHEN    "$"
      *        保険・診療科
                   MOVE    4                   TO  WRK-CD-MCNT
                   MOVE    1                   TO  WRK-IDX-FT
      *            数量以下の変換
                   PERFORM 300-SURYOU-SYORI-SEC
               WHEN    OTHER
      *            その他
                   PERFORM 2001-INPUTCHK-SYORI-SEC
           END-EVALUATE
           .
       200-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードの判定処理
      *****************************************************************
       2001-INPUTCHK-SYORI-SEC       SECTION.
      *
      *    入力コード文字列調査処理
           PERFORM 2020-INPUTCD-CHOSA-SEC
           MOVE    WRK-INPUTCD         TO  LNK-SC97-INPUTCD
           MOVE    WRK-CD              TO  LNK-SC97-INCD
      *
      *    入力コード変更時、点数マスタの決定
           IF     (LNK-SC97-NYURYOKU   =   1      )  AND
                  (SPA-ERRCD           =   SPACE  )
               PERFORM 2030-INPUTCD-KENSAKU-SEC
           END-IF
           IF     (LNK-SC97-KENSAKU    NOT =   SPACE )  OR
                  (SPA-ERRCD           NOT =   SPACE )
               CONTINUE
           ELSE
      *        数量以下の変換
               PERFORM 300-SURYOU-SYORI-SEC
           END-IF
      *
           .
       200-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    １桁目の変換調査処理
      *****************************************************************
       2001-JIKANGAI-CHK-SEC       SECTION.
      *
      *    １桁が数字で２桁目が空白の判定を全角チェック
      *****IF     (LNK-SC97-INPUTCD(1:1)   >=  X"A1"  )  AND
           MOVE    LNK-SC97-INPUTCD(1:2)   TO  WRK-KOUMOKU
           MOVE    2                       TO  WRK-LEN
           PERFORM 1009-KANACONV-SEC
           IF     (FLG-ZENKAKU         =   1          )  AND
                  (LNK-SC97-INPUTCD(3:1)   =   " "    )  AND
                  (LNK-SC97-INPUTCD(4:)    NOT =   SPACE)
               MOVE    LNK-SC97-INPUTCD    TO  WRK-HENKAN-MAE
               MOVE    1                   TO  WRK-STR
               MOVE    2                   TO  WRK-END
               PERFORM 2010-MOJI-HENKAN-SEC
               IF      FLG-ALLSUJI         =   1
                   MOVE    SPACE               TO  WRK-INPUTCD
                   MOVE    WRK-HENKAN-ATO      TO  WRK-INPUTCD(1:1)
                   MOVE    LNK-SC97-INPUTCD(3:)
                                                TO  WRK-INPUTCD(2:)
                   MOVE    WRK-INPUTCD          TO  LNK-SC97-INPUTCD
               ELSE
                   MOVE    LNK-SC97-INPUTCD     TO  WRK-INPUTCD
               END-IF
           END-IF
      *
      *    １桁が’．’の時、種別入力とする
           IF      LNK-SC97-INPUTCD(1:2)   =   "．" 
               MOVE    LNK-SC97-INPUTCD    TO  WRK-HENKAN-MAE
               MOVE    1                   TO  WRK-STR
               MOVE    CONS-INPUT          TO  WRK-END
               PERFORM 2010-MOJI-HENKAN-SEC
               MOVE    WRK-HENKAN-ATO      TO  LNK-SC97-INPUTCD
           END-IF
      *
           .
       2001-JIKANGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード文字列調査処理
      *****************************************************************
       2020-INPUTCD-CHOSA-SEC       SECTION.
      *
      *--- 文字列調査の開始位置　取得
           IF  (WRK-INPUTCD(1:1)       IS  NUMERIC) AND
               (WRK-INPUTCD(2:1)       =   SPACE  ) AND
               (WRK-INPUTCD(3:1)   NOT =   SPACE  )
      *---     時間加算
               MOVE    WRK-INPUTCD(1:1)
                                       TO  LNK-SC97-JIKAN
      *
               IF      LNK-SC97-JIKAN      =   1
                                           OR  2
                                           OR  3
                                           OR  4
                                           OR  5
                                           OR  6
                                           OR  7
                                           OR  8
                                           OR  0
                   CONTINUE
               ELSE
                   MOVE    "9002"          TO  SPA-ERRCD
                   GO      TO      2020-INPUTCD-CHOSA-EXT
               END-IF
      *---     時間特例チェック
               IF      LNK-SC97-JIKAN      =   4
                   IF      LNK-SC97-JIKANTOKU  =   1
                       CONTINUE
                   ELSE
                       MOVE    "0069"          TO  SPA-ERRCD
                       GO      TO      2020-INPUTCD-CHOSA-EXT
                   END-IF
               END-IF
      *        夜間・早朝加算は診療所でＨ２０．４から
               IF     (LNK-SC97-JIKAN      =   8 )
                   IF     (SPA-SRYYMD          <   "20080401" )
                      OR  (SPA-HOSPSBT         =   1   )
                       MOVE    "9002"          TO  SPA-ERRCD
                       GO      TO      2020-INPUTCD-CHOSA-EXT
                   END-IF
               END-IF
               MOVE    LNK-SC97-JIKAN  TO  FLG-JKNKSN
               MOVE    3               TO  WRK-IDX-FT
           ELSE
               MOVE    ZERO            TO  LNK-SC97-JIKAN
               MOVE    LNK-SC97-JIKAN  TO  FLG-JKNKSN
               MOVE    1               TO  WRK-IDX-FT
           END-IF
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  FLG-KANA
           MOVE    ZERO                TO  FLG-SUJI
           MOVE    ZERO                TO  WRK-CD-MCNT
           MOVE    SPACE               TO  WRK-CD
           MOVE    WRK-IDX-FT          TO  IDX
           MOVE    1                   TO  IDX4
           PERFORM
                   UNTIL  (IDX         >   CONS-INPUT  )
                    OR    (WRK-INPUTCD(IDX:1)
                                       =   SPACE  OR  "*" )
      *      全角の＊を半角の*とする
             IF      WRK-INPUTCD(IDX:2)    =  "＊" OR "　"
                 MOVE    CONS-INPUT           TO  IDX
             ELSE
      *          全角判定
                 MOVE    WRK-INPUTCD(IDX:2)    TO  WRK-KOUMOKU
                 MOVE    2                     TO  WRK-LEN
                 PERFORM 1009-KANACONV-SEC
                 IF      FLG-ZENKAKU         =   1
                     MOVE    WRK-INPUTCD(IDX:2)
                                           TO  WRK-CD (IDX4:2)
                     ADD     2             TO  WRK-CD-MCNT
                     ADD     2             TO  IDX4
                     ADD     2             TO  IDX
                 ELSE
                     MOVE    WRK-INPUTCD(IDX:1)
                                           TO  WRK-CD (IDX4:1)
                     ADD     1             TO  WRK-CD-MCNT
                     ADD     1             TO  IDX4
                     ADD     1             TO  IDX
                 END-IF
             END-IF
      ******** IF      WRK-INPUTCD (IDX:1)
      **********                      >=  X"A1"
      *            全角の時、２桁編集
      *            MOVE    1               TO  FLG-KANJI
      *            ADD     1               TO  IDX
      *            ADD     1               TO  WRK-CD-MCNT
      *            MOVE    WRK-INPUTCD(IDX:1)
      *                                    TO  WRK-CD (WRK-CD-MCNT:1)
      *        ELSE
      *            IF     (WRK-INPUTCD (IDX:1)
      *                                   IS  ALPHABETIC )  OR
      *                   (WRK-INPUTCD (IDX:1)
      *                                   IS  NUMERIC    )  OR
      *                   (WRK-INPUTCD (IDX:1)
      *                                   =   "."  OR  "/" ) OR
      *                   (WRK-INPUTCD (IDX:1)
      *                                   >   SPACE  AND  <= "z")
      *                MOVE    1              TO  FLG-SUJI
      *            ELSE
      *                MOVE    1              TO  FLG-KANA
      *            END-IF
      *        END-IF
      *******END-IF
           END-PERFORM
      *
      *    混在エラー
           MOVE    WRK-CD              TO  WRK-KOUMOKU
           MOVE    WRK-CD-MCNT         TO  WRK-LEN
           PERFORM 1009-KANACONV-SEC
           IF      FLG-ERR             =   1
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
           IF      FLG-ZENKAKU         =   1
               MOVE    1               TO  FLG-KANJI
           END-IF
           IF      FLG-HANKAKU         =   1
               MOVE    1               TO  FLG-SUJI
           END-IF
      *
      *    コード変更の有無
           MOVE    LNK-SC97-GMN-IDX    TO  GMN-IDX
           IF      SPA-COM-ICD-INPUTCD(GMN-IDX)
                                           =   SPACE
      *        入力コードが無い時、点数コードと判定する
               IF      SPA-COM-INPUTCD(GMN-IDX)
                                           =   WRK-CD
                   MOVE    ZERO                TO  LNK-SC97-NYURYOKU
               ELSE
                   MOVE    1                   TO  LNK-SC97-NYURYOKU
               END-IF
           ELSE
      *        入力コードがある時、入力コードと判定する
               IF     (SPA-COM-ICD-INPUTCD(GMN-IDX)
                                           =   WRK-CD  )  OR
                      (SPA-COM-INPUTCD(GMN-IDX)
                                           =   WRK-CD  )
                   MOVE    ZERO                TO  LNK-SC97-NYURYOKU
               ELSE
                   MOVE    1                   TO  LNK-SC97-NYURYOKU
               END-IF
           END-IF
      *
      *    半角カナは入力不可
      **** IF      FLG-KANA            =   1
      *        MOVE    "0005"          TO  SPA-ERRCD
      **** END-IF
      *    半角、全角混在は入力不可
      *    IF     (FLG-KANJI           =   1  )  AND
      *           (FLG-SUJI            =   1  )
      *        MOVE    "0005"          TO  SPA-ERRCD
      *****END-IF
           MOVE    WRK-CD-MCNT         TO  LNK-SC97-INCDCNT
           .
       2020-INPUTCD-CHOSA-EXT.
           EXIT.
      *
      *****************************************************************
      *    漢字変換処理
      *****************************************************************
       2010-MOJI-HENKAN-SEC    SECTION.
      *
      *    漢字変換
           MOVE    SPACE               TO  WRK-HENKAN-ATO
           MOVE    WRK-STR             TO  IDH1
           MOVE    ZERO                TO  IDH2
           MOVE    ZERO                TO  FLG-ALLSUJI
           PERFORM
                   UNTIL       IDH1    >   WRK-END
      *      全角判定
             IF      IDH1              =   WRK-END
                 MOVE    ZERO              TO  FLG-ZENKAKU
             ELSE
                 MOVE    WRK-HENKAN-MAE(IDH1:2)    TO  WRK-KOUMOKU
                 PERFORM 1009-KANACONV-SEC
             END-IF
             IF      FLG-ZENKAKU         =   1
               EVALUATE    WRK-HENKAN-MAE(IDH1:2)
                   WHEN    "０"
                       ADD     1           TO  IDH2
                       MOVE    "0"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "１"
                       ADD     1           TO  IDH2
                       MOVE    "1"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "２"
                       ADD     1           TO  IDH2
                       MOVE    "2"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "３"
                       ADD     1           TO  IDH2
                       MOVE    "3"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "４"
                       ADD     1           TO  IDH2
                       MOVE    "4"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "５"
                       ADD     1           TO  IDH2
                       MOVE    "5"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "６"
                       ADD     1           TO  IDH2
                       MOVE    "6"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "７"
                       ADD     1           TO  IDH2
                       MOVE    "7"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "８"
                       ADD     1           TO  IDH2
                       MOVE    "8"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "９"
                       ADD     1           TO  IDH2
                       MOVE    "9"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "．"
                       ADD     1           TO  IDH2
                       MOVE    "."         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "＊"
                       ADD     1           TO  IDH2
                       MOVE    "*"         TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    "　"
                       ADD     1           TO  IDH2
                       MOVE    SPACE       TO  WRK-HENKAN-ATO (IDH2:1)
                       MOVE    1           TO  FLG-ALLSUJI
                   WHEN    OTHER
                       ADD     1           TO  IDH2
                       MOVE    WRK-HENKAN-MAE(IDH1:2)
                                           TO  WRK-HENKAN-ATO (IDH2:2)
                       ADD     1           TO  IDH2
               END-EVALUATE
               ADD     2                   TO  IDH1
             ELSE
               ADD     1                   TO  IDH2
               MOVE    WRK-HENKAN-MAE(IDH1:1)
                                           TO  WRK-HENKAN-ATO (IDH2:1)
               ADD     1                   TO  IDH1
             END-IF
           END-PERFORM
           .
       2010-MOJI-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       2030-INPUTCD-KENSAKU-SEC        SECTION.
      *
      *    セットの時、時間外がないこと
           IF     (WRK-CD(1:1)         =   "S"  )   AND
                  (FLG-JKNKSN      NOT =   ZERO )
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO                 2030-INPUTCD-KENSAKU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ICD-CDSYU
           IF      FLG-KANJI           =   1
      *        全角のコードチェック
               MOVE    "J"             TO  WRK-ICD-CDSYU
           ELSE
      *        診療コードチェック
               IF      WRK-CD(1:1)         =   "P"  OR  "S"
                   MOVE    "6"             TO  WRK-ICD-CDSYU
               ELSE
                   IF     (WRK-CD-MCNT     >   1   AND  <=  9 )  AND
                          (WRK-CD(1:WRK-CD-MCNT)   NUMERIC    )
                       EVALUATE    WRK-CD-MCNT
                           WHEN    9
      *                    診療行為コード入力
                               MOVE    WRK-CD(1:WRK-CD-MCNT)
                                               TO  LNK-SC97-INCD
      *                    短縮３桁
                           WHEN    3
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *                    ４桁
                           WHEN    4
                               MOVE    "4"         TO  WRK-ICD-CDSYU
      *                    ５桁
                           WHEN    5
                               MOVE    "5"         TO  WRK-ICD-CDSYU
      *                    ６桁
                           WHEN    6
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *
                           WHEN    OTHER
                               MOVE    "A"         TO  WRK-ICD-CDSYU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  WRK-ICD-CDSYU
                   END-IF
               END-IF
           END-IF
      *
      *    入力コード存在チェック
           IF     WRK-ICD-CDSYU    NOT =   SPACE
      *
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    WRK-ICD-CDSYU       TO  ICD-CDSYU
               MOVE    WRK-CD              TO  ICD-INPUTCD
      *
               PERFORM 930-INPUTCD-READ-SEC
      *        入力コードなし
               IF      FLG-INPUTCD     NOT =   ZERO
                   IF      WRK-ICD-CDSYU       =   "4" OR "5" OR "6"
                       MOVE    "0050"          TO  SPA-ERRCD
                       GO  TO                  2030-INPUTCD-KENSAKU-EXT
                   END-IF
               END-IF
      *
      *        全桁対象がある時
               IF     (WRK-CD              =  ICD-INPUTCD )  AND
                      (ICD-SRYCD(1:1)      =   "S"  OR  "P")
      *
      *            セットの時、時間外がないこと
                   IF      FLG-JKNKSN      NOT =   ZERO
                       MOVE    "0010"              TO  SPA-ERRCD
                       GO  TO                  2030-INPUTCD-KENSAKU-EXT
                   END-IF
      *
                   MOVE    LNK-SC97-INPUTCD    TO  WRK-INPUTCD
                   MOVE    ICD-SRYCD(1:6)      TO  LNK-SC97-INPUTCD
                                                               (1:6)
                   COMPUTE IDY                 =   WRK-CD-MCNT   +  1
                   MOVE    WRK-INPUTCD(IDY:)   TO  LNK-SC97-INPUTCD
                                                                (7:)
                   MOVE    ICD-SRYCD           TO  LNK-SC97-INCD
                   MOVE    6                   TO  LNK-SC97-INCDCNT
               ELSE
                   IF     (WRK-CD              =   ICD-INPUTCD )
      *************IF     (WRK-CD              =   ICD-INPUTCD )  AND
      ********************(WRK-ICD-CDSYU   NOT =   "J"         )
      *                点数コード決定
                       MOVE    ICD-SRYCD           TO  LNK-SC97-INCD
                       MOVE    9                   TO  LNK-SC97-INCDCNT
      *                点数コードが同じ時
                       IF      SPA-COM-INPUTCD(GMN-IDX)
                                               =   LNK-SC97-INCD
                           MOVE    ZERO            TO  LNK-SC97-NYURYOKU
      *!!!!
      *H24.5 入力ＣＤが違う時、編集未とする
                           IF     (SPA-COM-ICD-INPUTCD (GMN-IDX)
                                                   NOT =   SPACE )
                           AND    (SPA-COM-ICD-INPUTCD (GMN-IDX)
                                                   NOT =   WRK-CD )
                               MOVE    SPACE       TO  SPA-COM-GMNFLG
                                                           (GMN-IDX)
                           END-IF
      *!!!!
                       END-IF
                   ELSE
      *                一覧表示へ
      **************** MOVE    WRK-ICD-CDSYU       TO  LNK-SC97-CDSYU
                       MOVE    "1"                 TO  LNK-SC97-KENSAKU
                   END-IF
               END-IF
      *
               MOVE    WRK-ICD-CDSYU       TO  LNK-SC97-CDSYU
      *
           END-IF
      *
           .
       2030-INPUTCD-KENSAKU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       300-SURYOU-SYORI-SEC        SECTION.
      *
      *    数量、回数の調査処理
           PERFORM 2030-MOJI-SET-SEC
      *
           .
       300-SURYOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       2030-MOJI-SET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-AST
           MOVE    ZERO                TO  FLG-SP
           MOVE    ZERO                TO  WRK-SUR-MCNT
           MOVE    ZERO                TO  WRK-ACCT-MCNT
           MOVE    ZERO                TO  WRK-MCNT1
           MOVE    ZERO                TO  WRK-MCNT2
           MOVE    ZERO                TO  WRK-MCNT3
           MOVE    ZERO                TO  WRK-MCNT4
           MOVE    ZERO                TO  WRK-MCNT5
           MOVE    SPACE               TO  WRK-CD
           MOVE    SPACE               TO  WRK-SUR
           MOVE    SPACE               TO  WRK-KAI
           MOVE    SPACE               TO  WRK-MOJI1
           MOVE    SPACE               TO  WRK-MOJI2
           MOVE    SPACE               TO  WRK-MOJI3
           MOVE    SPACE               TO  WRK-MOJI4
           MOVE    SPACE               TO  WRK-MOJI5
           MOVE    ZERO                TO  FLG-AUTO
      *    換算値入力
           MOVE    ZERO                TO  FLG-AINPUT
           MOVE    ZERO                TO  FLG-BINPUT
           MOVE    ZERO                TO  FLG-CINPUT
      *
      *    コード以外の全角数字を半角とする
           COMPUTE IDX-D               =   WRK-CD-MCNT
                                       +   WRK-IDX-FT
           MOVE    LNK-SC97-INPUTCD(IDX-D:)
                                       TO  WRK-HENKAN-MAE
           MOVE    1                   TO  WRK-STR
           MOVE    CONS-INPUT          TO  WRK-END
           PERFORM 2010-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-ATO      TO  LNK-SC97-INPUTCD(IDX-D:)
      *
           MOVE    ZERO                TO  WRK-CD-MCNT
           PERFORM VARYING     IDX     FROM    WRK-IDX-FT   BY  1
                   UNTIL       IDX         >   CONS-INPUT
      ****     MOVE    LNK-SC97-INPUTCD(IDX:1)
      *****                                TO  WRK-MOJI
               EVALUATE    LNK-SC97-INPUTCD(IDX:1)
                   WHEN    "*"
                       MOVE    1               TO  FLG-AST
                   WHEN    "@"
      *                数量の直後であること
                       IF     (FLG-SP          =   1    )  AND
                              (FLG-AST         =   ZERO )  AND
                              (WRK-SUR     NOT =   SPACE )
                           MOVE    1               TO  FLG-AUTO
                       END-IF
                   WHEN    SPACE
                       ADD     1               TO  FLG-SP
      *                空白は１つとする
                       IF     (IDX             >   1   )
                           COMPUTE IDX-1       =   IDX     -  1
                           IF      LNK-SC97-INPUTCD(IDX-1:1) =   SPACE
                               COMPUTE FLG-SP      =   FLG-SP    -  1
                           END-IF
                       END-IF
      *
                   WHEN    OTHER
      *---             診療行為コード編集
                       IF     (FLG-SP          =   ZERO )  AND
                              (FLG-AST         =   ZERO )
                           ADD     1               TO  WRK-CD-MCNT
                           MOVE    LNK-SC97-INPUTCD(IDX:1)
                                                   TO  WRK-CD
                                                       (WRK-CD-MCNT:1)
                       END-IF
      *ver.4.5
      *                換算値・多剤入力区分
                       MOVE    ZERO                TO  FLG-SEND
                       IF     (FLG-SP          >=  1    )
                         AND  (FLG-AST         =   ZERO )
      *****              AND  (WRK-SUR-MCNT    >   ZERO )
      *                    多剤入力区分
                           IF       (LNK-SC97-INPUTCD(IDX:1)
                                                   =   "A"
                                                   OR  "a"  )
                               MOVE    1               TO  FLG-AINPUT
                               MOVE    1               TO  FLG-SEND
                           END-IF
      *                    １種類入力区分
                           IF       (LNK-SC97-INPUTCD(IDX:1)
                                                   =   "I"
                                                   OR  "i"  )
                               MOVE    1               TO  FLG-BINPUT
                               MOVE    1               TO  FLG-SEND
                           END-IF
      *                    継続コメント
                           IF       (LNK-SC97-INPUTCD(IDX:1)
                                                   =   "C"
                                                   OR  "c"  )
                               MOVE    1               TO  FLG-CINPUT
                               MOVE    1               TO  FLG-SEND
                           END-IF
                       END-IF
                       IF      FLG-SEND        =   ZERO
      *---             数量以下編集
                           PERFORM 4101121-MOJI-SURYO-SET-SEC
                       END-IF
      *ver.4.5
                   END-EVALUATE
           END-PERFORM
      *
      *    数量
           MOVE    ZERO                TO  WRK-SURYO-R
           MOVE    LNK-SC97-GMN-IDX    TO  GMN-IDX
      *
           IF     (GMN-IDX                     =   ZERO  )
             OR   (SPA-COM-INPUTCD(GMN-IDX)    =   SPACE )
               MOVE    LNK-SC97-INCD       TO  WRK-CHK-INPUTCD
           ELSE
               MOVE    SPA-COM-INPUTCD(GMN-IDX)
                                           TO  WRK-CHK-INPUTCD
           END-IF
      *
      *    数量　数字変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SUR             TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
      *R02.4      (SNUM-SEISUU         >   7     )   OR
      *****       (SNUM-SYOSUU         >   3     )
                  (SNUM-SYOSUU         >   5     )
               MOVE    "0012"              TO  SPA-ERRCD
      *R02.4   8311　対応
               IF     (WRK-CHK-INPUTCD(1:3)
                                           =   "831")
                  OR  (WRK-CHK-INPUTCD(1:3)
                                           =   "842")
                  OR  (WRK-CHK-INPUTCD     =   SPACE )
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           ELSE
      *R02.4   新コメント 7文字以上入力対応
               IF     (SNUM-SEISUU         >   7  )
                   IF     (WRK-CHK-INPUTCD(1:3)
                                           =   "831")
                     OR   (WRK-CHK-INPUTCD(1:2)
                                           =   "85")
                     OR   (WRK-CHK-INPUTCD(1:3)
                                           =   "842")
                     OR   (WRK-CHK-INPUTCD =   SPACE)
                       CONTINUE
                   ELSE
                        MOVE    "0012"              TO  SPA-ERRCD
                   END-IF
               END-IF
               MOVE    SNUM-SEISUU         TO  LNK-SC97-SURYOFLG
               MOVE    WRK-SUR-MCNT        TO  LNK-SC97-SURYOCNT
               MOVE    SNUM-OUTNUM         TO  LNK-SC97-SURYO
      *        器材は小数点以下３桁
               IF     (WRK-CHK-INPUTCD(1:1)
                                           =   "7"  )  AND
                      (SNUM-SYOSUU         >   3    )
                  MOVE    "0312"              TO  SPA-ERRCD
               END-IF
      *
      *        入力値がないもの
               IF     ((SPA-COM-INPUTCD(GMN-IDX)(1:2)   =   "84"  ) OR
                       (SPA-COM-INPUTCD(GMN-IDX)(1:4)   =   "0084") OR
                       (SPA-COM-INPUTCD(GMN-IDX)(1:3)   =   "001"  AND
                        SPA-COM-YCOMKBN(GMN-IDX)        =   1    )
      *R02.4
                   OR  (SPA-COM-INPUTCD(GMN-IDX)(1:2)   =   "85"  )
                   OR  (SPA-COM-INPUTCD(GMN-IDX)(1:3)   =   "831" )
                   OR  (SPA-COM-INPUTCD(GMN-IDX)(1:7)   =   "0992081"))
                  AND ( WRK-MCNT2          >   ZERO )
                   CONTINUE
               ELSE
      *
      *        ゼロ入力不可
               IF      SPA-SURYOZERO-FLG   =   ZERO
      *
      *           数量にゼロ入力した時、行削除とする
                   IF     (WRK-SUR-MCNT        >   ZERO  )  AND
                          (LNK-SC97-SURYO      =   ZERO  )
                       MOVE    "  "            TO  LNK-SC97-INPUTCD
                       MOVE    ZERO            TO  LNK-SC97-SURYOCNT
                       MOVE    ZERO            TO  LNK-SC97-SURYO
                       MOVE    ZERO            TO  LNK-SC97-SURYOFLG
                   END-IF
               ELSE
      *            ’０’以外の時、行削除とする
                   IF     (WRK-SUR-MCNT        >   1     )  AND
                          (LNK-SC97-SURYO      =   ZERO  )
                       MOVE    "  "            TO  LNK-SC97-INPUTCD
                       MOVE    ZERO            TO  LNK-SC97-SURYOCNT
                       MOVE    ZERO            TO  LNK-SC97-SURYO
                       MOVE    ZERO            TO  LNK-SC97-SURYOFLG
                   END-IF
               END-IF
           END-IF
      *
           END-IF
      *
      *    コメント入力値はゼロ以外は数量なしとする
      *    IF    ((SPA-COM-INPUTCD(GMN-IDX)(1:2)   =   "84") OR
      *           (SPA-COM-INPUTCD(GMN-IDX)(1:4)   =   "0084")  OR
      *           (SPA-COM-INPUTCD(GMN-IDX)(1:3)   =   "001"  AND
      *            SPA-COM-YCOMKBN(GMN-IDX)        =   1    ))
      *       AND (SNUM-RC             =   ZERO    )
      *       AND (SNUM-OUTNUM     NOT =   ZERO    )
      *        MOVE    SPACE               TO  SPA-ERRCD
      *        MOVE    ZERO                TO  LNK-SC97-SURYOFLG
      *        MOVE    ZERO                TO  LNK-SC97-SURYOCNT
      *        MOVE    ZERO                TO  LNK-SC97-SURYO
      *    END-IF
      *
           IF      LNK-SC97-SURYO   >   ZERO
               IF      FLG-AUTO         =   1
                   MOVE    "1"          TO  LNK-SC97-AUTOFLG2
               END-IF
           END-IF
      *ver.4.5
      *    換算値入力数量
           IF      LNK-SC97-SURYO   >   ZERO
               IF      FLG-AINPUT       =   1
                   MOVE    "1"          TO  LNK-SC97-INKBNFLG1
               END-IF
           ELSE
               IF      FLG-AINPUT       =   1
                   MOVE    "0205"           TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    数量指示区分
           IF      FLG-BINPUT       =   1
               MOVE    "1"          TO  LNK-SC97-INKBNFLG2
           END-IF
      *    継続コメント指示区分
           IF      FLG-CINPUT       =   1
               MOVE    "1"          TO  LNK-SC97-CONTFLG
           END-IF
      *
      *    回数（＊があるが、回数が入力されてない時、＊削除する）
           IF     (FLG-AST             =   1    )  AND
                  (WRK-ACCT-MCNT       =   ZERO )
               INSPECT LNK-SC97-INPUTCD
                       REPLACING   FIRST "*"  BY  " "
               MOVE    1                   TO  LNK-SC97-KAISUFLG
           ELSE
               MOVE    ZERO                TO  LNK-SC97-KAISUFLG
           END-IF
           MOVE    WRK-ACCT-MCNT       TO  LNK-SC97-KAISUCNT
      *
      *    回数
      *    回数　数字変換
           MOVE    ZERO                TO  LNK-SC97-KAISU
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-KAI             TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   3     )   OR
                  (SNUM-SYOSUU         >   ZERO  )
               MOVE    "0012"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  LNK-SC97-KAISU
           END-IF
      *
      *    フィルムの時、分画数
           MOVE    ZERO                TO  WRK-BUNGSU
           IF     (WRK-MCNT2           >   ZERO  ) AND
                  (SPA-COM-INPUTCD(GMN-IDX)(1:1)   =   "7") 
      *
               MOVE    ZERO                TO  WRK-BUNGSU
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-MOJI2(1:WRK-MCNT2)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    "0012"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  LNK-SC97-BUNGSU
               END-IF
               MOVE    SPACE              TO  WRK-MOJI2
               MOVE    ZERO               TO  WRK-MCNT2
           END-IF
      *
      *    入力値があるものの、行削除（コメント）
      *        入力値がないもの
           IF     ((SPA-COM-INPUTCD(GMN-IDX)(1:2)   =   "84") OR
                   (SPA-COM-INPUTCD(GMN-IDX)(1:4)   =   "0084") OR
      *R02.4
                   (SPA-COM-INPUTCD(GMN-IDX)(1:2)   =   "85"  ) OR
                   (SPA-COM-INPUTCD(GMN-IDX)(1:3)   =   "831" ) OR
      *
      *H30.4
                   (SPA-COM-INPUTCD(GMN-IDX)(1:7)   =   "0992081"))
             AND  ( WRK-MCNT2         >   ZERO )
               IF     ((WRK-MCNT2              >   ZERO )  AND
                       (WRK-MOJI2(1:WRK-MCNT2) NOT =   ZERO ))
                 OR   ((WRK-MCNT3              >   ZERO )  AND
                       (WRK-MOJI3(1:WRK-MCNT3) NOT =   ZERO ))
                 OR   ((WRK-MCNT4              >   ZERO )  AND
                       (WRK-MOJI4(1:WRK-MCNT4) NOT =   ZERO ))
                   IF     (WRK-SUR-MCNT        =   ZERO  )  OR
                          (LNK-SC97-SURYO      =   ZERO  )
                       MOVE    1               TO  LNK-SC97-SURYO
                   END-IF
                   MOVE    ZERO            TO  LNK-SC97-SURYOCNT
                   MOVE    ZERO            TO  LNK-SC97-SURYOFLG
               ELSE
      *            行削除とする
                   IF     (WRK-SUR-MCNT        >   ZERO  )  AND
                          (LNK-SC97-SURYO      =   ZERO  )
                       MOVE    "  "            TO  LNK-SC97-INPUTCD
                       MOVE    ZERO            TO  LNK-SC97-SURYOCNT
                       MOVE    ZERO            TO  LNK-SC97-SURYO
                       MOVE    ZERO            TO  LNK-SC97-SURYOFLG
                   END-IF
               END-IF
           END-IF
      *    入力値があるものの、行削除（用法コメント）
      *        入力値がないもの
           IF     ((SPA-COM-INPUTCD(GMN-IDX)(1:3)   =   "001" )  AND
                   (SPA-COM-YCOMKBN(GMN-IDX)        =   1     ))  AND
                  ( WRK-MCNT2         >   ZERO )
               IF     ((WRK-MCNT2              >   ZERO )  AND
                       (WRK-MOJI2(1:WRK-MCNT2) NOT =   ZERO ))
                 OR   ((WRK-MCNT3              >   ZERO )  AND
                       (WRK-MOJI3(1:WRK-MCNT3) NOT =   ZERO ))
                 OR   ((WRK-MCNT4              >   ZERO )  AND
                       (WRK-MOJI4(1:WRK-MCNT4) NOT =   ZERO ))
                 OR   ((WRK-MCNT5              >   ZERO )  AND
                       (WRK-MOJI5(1:WRK-MCNT5) NOT =   ZERO ))
                   IF     (WRK-SUR-MCNT        =   ZERO  )  OR
                          (LNK-SC97-SURYO      =   ZERO  )
                       MOVE    1               TO  LNK-SC97-SURYO
                   END-IF
                   MOVE    ZERO            TO  LNK-SC97-SURYOCNT
                   MOVE    ZERO            TO  LNK-SC97-SURYOFLG
               ELSE
      *            行削除とする
                   IF     (WRK-SUR-MCNT        >   ZERO  )  AND
                          (LNK-SC97-SURYO      =   ZERO  )
                       MOVE    "  "            TO  LNK-SC97-INPUTCD
                       MOVE    ZERO            TO  LNK-SC97-SURYOCNT
                       MOVE    ZERO            TO  LNK-SC97-SURYO
                       MOVE    ZERO            TO  LNK-SC97-SURYOFLG
                   END-IF
               END-IF
           END-IF
      *
      *    コメント値
           IF      WRK-MCNT1          =   ZERO   AND
                   WRK-MCNT2          >   ZERO
               MOVE    WRK-MOJI2          TO  WRK-MOJI1
               MOVE    SPACE              TO  WRK-MOJI2
               MOVE    ZERO               TO  WRK-MCNT2
           END-IF
           IF      WRK-MCNT2          =   ZERO   AND
                   WRK-MCNT3          >   ZERO
               MOVE    WRK-MOJI3          TO  WRK-MOJI2
               MOVE    SPACE              TO  WRK-MOJI3
               MOVE    ZERO               TO  WRK-MCNT3
           END-IF
           IF      WRK-MCNT3          =   ZERO   AND
                   WRK-MCNT4          >   ZERO
               MOVE    WRK-MOJI4          TO  WRK-MOJI3
               MOVE    SPACE              TO  WRK-MOJI4
               MOVE    ZERO               TO  WRK-MCNT4
           END-IF
           IF      WRK-MCNT4          =   ZERO   AND
                   WRK-MCNT5          >   ZERO
               MOVE    WRK-MOJI5          TO  WRK-MOJI4
               MOVE    SPACE              TO  WRK-MOJI5
               MOVE    ZERO               TO  WRK-MCNT5
           END-IF
      *
           MOVE    WRK-MOJI1           TO  LNK-SC97-MOJI1
           MOVE    WRK-MOJI2           TO  LNK-SC97-MOJI2
           MOVE    WRK-MOJI3           TO  LNK-SC97-MOJI3
           MOVE    WRK-MOJI4           TO  LNK-SC97-MOJI4
           MOVE    WRK-MOJI5           TO  LNK-SC97-MOJI5
           MOVE    WRK-MCNT1           TO  LNK-SC97-MCNT1
           MOVE    WRK-MCNT2           TO  LNK-SC97-MCNT2
           MOVE    WRK-MCNT3           TO  LNK-SC97-MCNT3
           MOVE    WRK-MCNT4           TO  LNK-SC97-MCNT4
           MOVE    WRK-MCNT5           TO  LNK-SC97-MCNT5
           .
       4101121-MOJI-SET-EXT.
           EXIT.
      *
      *ver.4.5
      *****************************************************************
      *    数量以下変換処理
      *****************************************************************
       4101121-MOJI-SURYO-SET-SEC       SECTION.
      *
      *--- 数量編集
           IF     (FLG-SP          =   1    )  AND
                  (FLG-AST         =   ZERO )
                ADD     1              TO  WRK-SUR-MCNT
                MOVE    LNK-SC97-INPUTCD(IDX:1)
                                       TO  WRK-SUR (WRK-SUR-MCNT:1)
           END-IF
      *--- 回数編集
           IF      FLG-AST         =   1
               ADD     1               TO  WRK-ACCT-MCNT
               MOVE    LNK-SC97-INPUTCD (IDX:1)
                                       TO  WRK-KAI  (WRK-ACCT-MCNT:1)
           END-IF
      *--- その他値
           IF     (FLG-SP          =   1    )  AND
                  (FLG-AST         =   0    )
               ADD     1               TO  WRK-MCNT1
               MOVE    LNK-SC97-INPUTCD(IDX:1)
                                       TO  WRK-MOJI1  (WRK-MCNT1:1)
           END-IF
           IF     (FLG-SP          =   2    )  AND
                  (FLG-AST         =   0    )
               ADD     1               TO  WRK-MCNT2
               MOVE    LNK-SC97-INPUTCD(IDX:1)
                                       TO  WRK-MOJI2  (WRK-MCNT2:1)
           END-IF
           IF     (FLG-SP          =   3    )  AND
                  (FLG-AST         =   0    )
               ADD     1               TO  WRK-MCNT3
               MOVE    LNK-SC97-INPUTCD(IDX:1)
                                       TO  WRK-MOJI3  (WRK-MCNT3:1)
           END-IF
           IF     (FLG-SP          =   4    )  AND
                  (FLG-AST         =   0    )
               ADD     1               TO  WRK-MCNT4
               MOVE    LNK-SC97-INPUTCD(IDX:1)
                                       TO  WRK-MOJI4   (WRK-MCNT4:1)
           END-IF
           IF     (FLG-SP          =   5    )  AND
                  (FLG-AST         =   0    )
               ADD     1               TO  WRK-MCNT5
               MOVE    LNK-SC97-INPUTCD(IDX:1)
                                       TO  WRK-MOJI5   (WRK-MCNT5:1)
           END-IF
           .
       4101121-MOJI-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           IF      WRK-CD-MCNT         >=   20
               MOVE    "key2"          TO  WRK-PATH
           ELSE
               MOVE    "key"           TO  WRK-PATH
               COMPUTE IDK             =   WRK-CD-MCNT  +  1
               MOVE    "%"             TO  ICD-INPUTCD(IDK:1)
           END-IF
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   INITIALIZE                  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *
      *
