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
       PROGRAM-ID.             ORCSBYOMEI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 病名コード検索処理
      *  管理者            : 
      *  03/12/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-藤原    04/03/17  開始日と廃止日のよるチェック
      *  00.00.02    NACL-藤原    04/04/13  複数の病名コードが存在する
      *                                     病名のコード編集
      *                                     ２１個のコード検索時に未検索の
      *                                     病名が存在するときの処理
      *  00.00.03    NACL-藤原    04/07/08  病名システム予約コードが存在
      *                                     する場合の疑い・慢性疾患区分の
      *                                     編集を行なう
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.01    NACL-藤原    06/01/25  疑いフラグからの病名の編集方
      *                                     法の変更
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.05.01    NACL-藤原    09/11/25  レセ電コメント拡張対応
      *                                     補足コメントのチェック
      *
      *  04.06.01    NACL-藤原    10/10/20  難病外来指導管理料対応
      *  04.06.02    NACL-藤原    13/05/29  レセ電エラーフラグの判定に
      *                                     主病名フラグを使用しない
      *
      *  04.07.01    NACL-藤原    11/06/10  病名からコード検索時、複数の
      *                                     組み立てを返却可能にする
      *
      *  04.08.01    NACL-藤原    15/12/02  未コード化傷病名のときの廃止、
      *                                     移行の初期化
      ***************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-BYOMEI          PIC 9(01).
      *
           03  FLG-KENSAKU         PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-TBLERR          PIC 9(01).
           03  FLG-HENKAN-ERR      PIC 9(01).
           03  FLG-BYOMEICD        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
       01  FLG-KENSAKU-KBN         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
      *
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDWW                PIC 9(04).
      *
       01  WRK-AREA.
      *
           03  WRK-BYOMEICD-KENSAKU-AREA.
      *        補足コメントまでの病名コード数（修飾語以外）
               05  WRK-BYOMEICDSU      PIC 9(02).
      *        病名コード数（修飾語以外）
               05  WRK-BYOMEICDSU1     PIC 9(02).
      *        検索病名コード数
               05  WRK-BYOMEI-MAX      PIC 9(01).
      *        最後の（の位置
               05  WRK-BYOMEI-YYK-INDEX1
                                       PIC 9(02).
      *        最後の）の位置
               05  WRK-BYOMEI-YYK-INDEX2
                                       PIC 9(02).
      *        最後の（の開始位置
               05  WRK-BYOMEI-YYK-ST   PIC 9(03).
      *        最後の）の開始位置
               05  WRK-BYOMEI-YYK-ED   PIC 9(03).
      *        最初の未コード化病名の位置
               05  WRK-BYOMEI-HENKAN-INDEX
                                       PIC 9(02).
      *        （）を除いた補足コメントの長さ
               05  WRK-BYOMEI-HOSOKU-LEN
                                       PIC 9(03).
      *        補足コメントを除いた病名コード数
               05  WRK-BYO-BYOMEI-MAX  PIC 9(02).
      *
      *        返却する検索病名コード情報の（）フラグ
               05  WRK-LNK-BYOMEI-FLG-AREA.
                   07  WRK-LNK-BYOMEI-FLG  PIC X(01)
                                               OCCURS  21.
      *        検索病名情報
               05  WRK-BYOMEI-FLG-TBL.         
      *            （）フラグ
                   07  WRK-BYOMEI-FLG  PIC X(01)
                                               OCCURS  40.
      *        検索病名情報
               05  WRK-BYOMEI-BYOMEICD-TBL     OCCURS  40.
      *            病名
                   07  WRK-BYOMEI-BYOMEI
                                       PIC X(200).
      *
                   07  WRK-BYOMEI-BYOMEICD-G   OCCURS  2.
      *                病名コード
                       09  WRK-BYOMEI-BYOMEICD 
                                       PIC X(07).    
      *                移行先コード
                       09  WRK-BYOMEI-IKOSAKICD 
                                       PIC X(07).
      *                廃止年月日
                       09  WRK-BYOMEI-HAISIYMD 
                                       PIC X(08).
      *                単独使用禁止区分
                       09  WRK-BYOMEI-TANDOKUKBN 
                                       PIC 9(02).
      *                特定疾患コード
                       09  WRK-BYOMEI-TOKSKNCD
                                       PIC 9(02).
      *                難病外来コード
                       09  WRK-BYOMEI-NANBYOCD
                                       PIC 9(02).
      *                疑いフラグ
                       09  WRK-BYOMEI-UTAGAIFLG
                                       PIC X(01).
           03  WRK-KENSAKU-AREA.
               05  WRK-KEN-BYOMEI  PIC X(80).
               05  WRK-KEN-LENGTH  PIC 9(04).
               05  WRK-KEN-ADDKBN  PIC 9(01).
               05  FLG-BUBUN       PIC 9(03).
           03  WRK-MOJISU          PIC 9(03).
           03  WRK-BYOMEIHENFLG    PIC X(01).
           03  WRK-BUI-MAX         PIC 9(01).
      *
           03  WRK-BYOMEI          PIC X(200).                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-TANDOKUKBN      PIC X(01).
           03  WRK-TANDOKUKBN-BYOMEI
                                   PIC X(200).                             
           03  WRK-KENBYOMEI       PIC X(02).
           03  WRK-KENBYOMEI-ST    PIC 9(03).
           03  WRK-LENGTH          PIC 9(03).
           03  WRK-ST              PIC 9(03).
           03  WRK-RECECD-TBL.
               05  WRK-RECECD      PIC X(01)   OCCURS  3.
      *
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *
      *    固定値
       01  WRK-CONS-AREA.
      * 
      *    病名コードＭＡＸ
           03  WRK-CONS-BYOMEI-MAX PIC 9(02)   VALUE   21.
      *
      *    検索病名情報ＭＡＸ
           03  WRK-CONS-BYOMEICD-TBL-MAX
                                   PIC 9(02)   VALUE   40.
      *    
      *    ワーク病名
       01  WRK-BYOMEI-REC.
           COPY    "CPBYOMEI.INC"  REPLACING  //BYO-//
                                 BY         //WRK-BYO-//.
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSBYOMEI.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *****************************************************************
     *
       PROCEDURE                    DIVISION    USING
                                    ORCSBYOMEIAREA
                                    SPA-AREA.
     *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           MOVE    ZERO            TO  FLG-KENSAKU-KBN
      *
      *    DISPLAY "LNK-BYO-SYORI=" LNK-BYO-SYORI
      *
           PERFORM 100-SYORI-SEC
      *
      *??         
      *     PERFORM VARYING IDX FROM    1   BY  1
      *             UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
      *       DISPLAY "BYOMEICD=" IDX " " LNK-BYO-TBYOMEICD (IDX) "#"
      *                LNK-BYO-TBYOMEI (IDX) 
      *     END-PERFORM
      *   DISPLAY "OT BYOMEIHENFLG="  LNK-BYO-BYOMEIHENFLG
      *   DISPLAY "   HAISIYMD-ERR="  LNK-BYO-HAISIYMD-ERR
      *   DISPLAY "   IKOSAKICD="     LNK-BYO-IKOSAKICD
      *   DISPLAY "   BYO-BYOMEI-MAX="     LNK-BYO-BYOMEI-MAX
      *??
            .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    病名検索処理
      *****************************************************************
       100-SYORI-SEC       SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPACE           TO  LNK-BYO-RC    
      *    処理区分により検索処理
           EVALUATE    LNK-BYO-SYORI
               WHEN    "1"
               WHEN    "3"
                   PERFORM 200-BYOMEI-SYORI-SEC
               WHEN    "2"
               WHEN    "4"
                   PERFORM 200-BYOMEICD-SYORI-SEC
               WHEN    "5"
                   PERFORM 200-HAISI-BYOMEI-SYORI-SEC
                   GO  TO  000-PROC-EXT
               WHEN    OTHER
                   MOVE    "1"         TO  LNK-BYO-RC
      *            DISPLAY "LNK-BYO-RC=" LNK-BYO-RC
                   GO  TO  100-SYORI-EXT
           END-EVALUATE
      *
      *    検索コードより組み立て
           PERFORM 200-TBL-HENSYU-SEC
           IF      LNK-BYO-RC      NOT =   SPACE
      *        DISPLAY "LNK-BYO-RC=" LNK-BYO-RC
               GO  TO  100-SYORI-EXT
           END-IF
      *
      **     DISPLAY "YYK OT=" WRK-BYOMEI-YYK-INDEX1 "#"
      **                      WRK-BYOMEI-YYK-INDEX2 "#" 
      **                      WRK-BYOMEI-YYK-ST "#" 
      **                      WRK-BYOMEI-YYK-ED "#" 
      **                      WRK-BYOMEI-HOSOKU-LEN "#"
      **     DISPLAY "HENKAN=" WRK-BYOMEI-HENKAN-INDEX "#"
      **     DISPLAY "BYOMEICD (1)=" LNK-BYO-TBYOMEICD (1) "#"
      **     DISPLAY "BYOMEIHENFLG=" LNK-BYO-BYOMEIHENFLG "#" 
      **       DISPLAY "FLG-KENSAKU-KBN=" FLG-KENSAKU-KBN "#"
      *?       DISPLAY "BYOMEI-MAX=" LNK-BYO-BYOMEI-MAX "#"
      *?     PERFORM VARYING IDX FROM    1   BY  1
      *?             UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
      *?       DISPLAY "BYOMEICD=" IDX " " LNK-BYO-TBYOMEICD (IDX) "#"
      *?                LNK-BYO-TBYOMEI (IDX) 
      *?     END-PERFORM         
      *?     DISPLAY "BYOMEICD-ERR=" LNK-BYO-BYOMEICD-ERR "#"
      *?     DISPLAY "  HENKAN-ERR=" FLG-HENKAN-ERR "#"
      *?     DISPLAY "  FLG-TBLERR=" FLG-TBLERR "#"
      *
      *    （）が最後にあるときの補足コメントの設定
           IF    ( WRK-BYOMEI-YYK-INDEX1   >   ZERO )
           AND   ( WRK-BYOMEI-YYK-INDEX2   >   ZERO )
           AND   ( WRK-BYOMEI-HOSOKU-LEN   <=  40   )
      *★       編集病名は補足コメントはなし
      **         IF    (     FLG-TBLERR          =   1           )
      **         OR    (   ( LNK-BYO-BYOMEI-MAX  =   ZERO )  AND
      **                   ( LNK-BYO-BYOMEICD-ERR   
      **                                     NOT =   "3")        ) 
      **         OR    (     FLG-HENKAN-ERR      =   1           )
      **         OR    (     LNK-BYO-BYOMEICD-ERR
      **                                 =   "1" OR  "2" OR  "9" )
      **             CONTINUE
      **         ELSE
                   COMPUTE LNK-BYO-HOSOKU-BYOMEICD-MAX =
                               WRK-BYOMEI-YYK-INDEX1   -   1
                   COMPUTE LNK-BYO-BYOMEI-LEN          =
                                WRK-BYOMEI-YYK-ST      -   1
                   MOVE    LNK-BYO-BYOMEI (1:LNK-BYO-BYOMEI-LEN)
                                           TO  LNK-BYO-HOSOKU-BYOMEI-ETC
                   COMPUTE IDXX    =   WRK-BYOMEI-YYK-ST   +   2
                   MOVE    LNK-BYO-BYOMEI (IDXX:WRK-BYOMEI-HOSOKU-LEN)
                                           TO  LNK-BYO-HOSOKU-BYOMEI
                   MOVE    SPACE           TO  LNK-BYO-BYOMEIHENFLG
      *★      END-IF
           END-IF     
      *
      *    病名コードは検索できないが補足コメントが存在するとき
           IF    ( LNK-BYO-SYORI       =   "3" OR  "4"       )
           AND   ( WRK-BYOMEI-YYK-INDEX1
                                       =   ZERO              )
           AND   ( WRK-BYOMEI-YYK-INDEX2
                                       =   ZERO              )
           AND   ( WRK-BYOMEI-YYK-ST   >   2                 )
           AND   ( WRK-BYOMEI-YYK-ED   >   4                 )
           AND   ( WRK-BYOMEI-YYK-ST   <   WRK-BYOMEI-YYK-ED )
           AND   ( LNK-BYO-HOSOKU-BYOMEI
                                       =   SPACE             )
               COMPUTE WRK-BYOMEI-HOSOKU-LEN   =
                                       WRK-BYOMEI-YYK-ED   -
                                       WRK-BYOMEI-YYK-ST   -
                                       2
               IF      WRK-BYOMEI-HOSOKU-LEN   <=  40
                   MOVE    1                   TO
                                           LNK-BYO-HOSOKU-BYOMEICD-MAX
                   COMPUTE LNK-BYO-BYOMEI-LEN          =
                                WRK-BYOMEI-YYK-ST      -   1
                   MOVE    LNK-BYO-BYOMEI (1:LNK-BYO-BYOMEI-LEN)
                                           TO  LNK-BYO-HOSOKU-BYOMEI-ETC
                   COMPUTE IDXX    =   WRK-BYOMEI-YYK-ST   +   2
                   MOVE    LNK-BYO-BYOMEI (IDXX:WRK-BYOMEI-HOSOKU-LEN)
                                           TO  LNK-BYO-HOSOKU-BYOMEI
               END-IF
           END-IF    
      *
      *    最大病名コード数以上検索したとき
      *****IF      WRK-BYOMEI-MAX  >   WRK-CONS-BYOMEI-MAX
           IF      FLG-TBLERR      =   1
               MOVE    1               TO  LNK-BYO-BYOMEICD-MAX-ERR  
               MOVE    "0000999"           TO  LNK-BYO-TBYOMEICD (1)
                                               LNK-BYO-KHNBYOMEICD
               MOVE    LNK-BYO-BYOMEI      TO  LNK-BYO-TBYOMEI   (1)
               MOVE    "1"                 TO  LNK-BYO-BYOMEIHENFLG
               MOVE    1                   TO  LNK-BYO-BYOMEICDSU
                                               LNK-BYO-BYOMEI-MAX
               MOVE    ZERO                TO  LNK-BYO-BYOMEICD-ERR
           END-IF
      *
      *    コードが検索できなかったとき（コード存在エラー以外）
           IF    ( LNK-BYO-BYOMEI-MAX  =   ZERO )
           AND   ( LNK-BYO-BYOMEICD-ERR   
                                   NOT =   "3"  )
               MOVE    "0000999"           TO  LNK-BYO-TBYOMEICD (1)
                                               LNK-BYO-KHNBYOMEICD
               MOVE    LNK-BYO-BYOMEI      TO  LNK-BYO-TBYOMEI   (1)
               MOVE    "1"                 TO  LNK-BYO-BYOMEIHENFLG
               MOVE    1                   TO  LNK-BYO-BYOMEICDSU
                                               LNK-BYO-BYOMEI-MAX
           END-IF
      *
      *    全てコードに変換できなかったとき
           IF      FLG-HENKAN-ERR      =   1
      *
      *        病名から変換時に補足コメント以外に未コード化病名があるか
               IF    ( LNK-BYO-SYORI       =   "3"                     )
               AND   ( LNK-BYO-HOSOKU-BYOMEI
                                       NOT =   SPACE                   )
               AND   ( WRK-BYOMEI-YYK-INDEX1
                                           <   WRK-BYOMEI-HENKAN-INDEX )
                   CONTINUE
               ELSE
                   INITIALIZE                  LNK-BYOMEICD-KENSAKU-AREA
      *
                   MOVE    "0000999"           TO  LNK-BYO-TBYOMEICD (1)
                   MOVE    LNK-BYO-BYOMEI      TO  LNK-BYO-TBYOMEI   (1)
                   MOVE    "1"                 TO  LNK-BYO-BYOMEIHENFLG
                   MOVE    1                   TO  LNK-BYO-BYOMEICDSU
                                                   LNK-BYO-BYOMEI-MAX
               END-IF
           END-IF
      *
           IF      LNK-BYO-TBYOMEICD (1)   =   "0000999"
               MOVE    "0000999"           TO  LNK-BYO-KHNBYOMEICD
               MOVE    SPACE               TO  LNK-BYO-KHNBUICD (1)
                                               LNK-BYO-KHNBUICD (2)
                                               LNK-BYO-KHNBUICD (3)
                                               LNK-BYO-IKOSAKICD
                                               LNK-BYO-IKOSAKICD-BYOMEI
               MOVE    ZERO                TO  LNK-BYO-HAISIYMD-ERR
                                               LNK-BYO-IKOSAKICD-INDEX
           END-IF
      *
           IF      LNK-BYO-BYOMEICD-ERR
                                       =   "1" OR  "2" OR  "9"
               MOVE    "1"                 TO  LNK-BYO-BYOMEIHENFLG
           END-IF
      *
      *    病名から検索時コード変換できた場合
           IF    ( LNK-BYO-SYORI       =   "3"   )
           AND   ( LNK-BYO-HOSOKU-BYOMEI
                                       =   SPACE )
           AND   ( LNK-BYO-BYOMEIHENFLG
                                       =   SPACE )
      *        補足コメントがないときの廃止病名チェック
               IF    ( LNK-BYO-HAISIYMD-ERR    >   ZERO              )
               AND   ( LNK-BYO-HAISIYMD-ERR    <   LNK-BYO-SRYYMD-IN )
                   PERFORM 200-HAISI-BYOMEI-SYORI-SEC
               END-IF
           END-IF 
      *
      *    病名コードから検索のとき病名編集
           IF    ( LNK-BYO-SYORI       =   "2" OR  "4" )
           AND   ( LNK-BYO-BYOMEICD-ERR
                                   NOT =   "3"         )    
               MOVE    ZERO                TO  LNK-BYO-BYOMEIMOJI
               MOVE    SPACE               TO  LNK-BYO-BYOMEI
               MOVE    LNK-BYO-TBYOMEI (1) TO  WRK-BYOMEI
               PERFORM VARYING IDX FROM    2   BY  1
                       UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
                   STRING  WRK-BYOMEI          DELIMITED   BY  SPACE
                           LNK-BYO-TBYOMEI(IDX)
                                               DELIMITED   BY  SPACE
                                               INTO    WRK-BYOMEI
                   END-STRING
               END-PERFORM
      *
      *        半角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   MOVE    KANACHK-OUT-INPUT   TO  LNK-BYO-BYOMEI
      *            病名文字数
                   IF      KANACHK-MAX     >   80
                       MOVE    2               TO  LNK-BYO-KANACHK-ERR
                   END-IF
                   COMPUTE LNK-BYO-BYOMEIMOJI  =   KANACHK-MAX /   2
               ELSE
                   MOVE    1                   TO  LNK-BYO-KANACHK-ERR
               END-IF
           END-IF
      *
      *    レセ電エラーフラグ
           PERFORM 200-RECEDENFLG-HENSYU-SEC
      *
      **   DISPLAY "LNK-BYO-HOSOKU-BYOMEI-ETC="
      **                                    LNK-BYO-HOSOKU-BYOMEI-ETC
      **   DISPLAY "LNK-BYO-HOSOKU-BYOMEI=" LNK-BYO-HOSOKU-BYOMEI
      **   DISPLAY "LNK-BYO-NANBYOCD ="     LNK-BYO-NANBYOCD
      *??
      **     DISPLAY "CHK BYOMEI-MAX=" LNK-BYO-BYOMEI-MAX "#"
      **     PERFORM VARYING IDX FROM    1   BY  1
      **             UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
      **       DISPLAY "    BYOMEICD=" IDX " " LNK-BYO-TBYOMEICD (IDX) "#"
      **                LNK-BYO-TBYOMEI (IDX) 
      **     END-PERFORM         
      *??
      *
           EVALUATE    LNK-BYO-SYORI
               WHEN    "1"
               WHEN    "3"
                   MOVE    ZERO                TO  FLG-CHK
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   LNK-BYOMEI-KENSAKU-MAX
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
                           IF      LNK-BYO-O-TBYOMEICD (IDXX IDX)
                                       NOT =    LNK-BYO-TBYOMEICD (IDX)
                               MOVE    1            TO  FLG-CHK
                               MOVE    LNK-BYO-BYOMEI-MAX
                                                    TO  IDX
                           END-IF
                       END-PERFORM
      *                一致するコードがあるとき
                       IF      FLG-CHK          =   1
                           MOVE    ZERO             TO  FLG-CHK
                       ELSE
                           MOVE    2                TO  FLG-CHK
                           MOVE    LNK-BYOMEI-KENSAKU-MAX
                                                    TO  IDXX 
                       END-IF
                   END-PERFORM
      *??
      **        DISPLAY "FLG-CHK=" FLG-KENSAKU-KBN "#" 
      **                 FLG-CHK " " LNK-BYOMEI-KENSAKU-MAX " "
      *??
                   IF    ( FLG-CHK                 =   0 )
      ***          OR    ( LNK-BYOMEI-KENSAKU-MAX  =   0 ) 
                       ADD     1           TO  LNK-BYOMEI-KENSAKU-MAX
                       MOVE    LNK-BYOMEI-AREA
                                           TO  LNK-BYOMEI-AREA-OCC
                                               (LNK-BYOMEI-KENSAKU-MAX)
      **   DISPLAY "LNK-BYOMEI-KENSAKU-MAX="  LNK-BYOMEI-KENSAKU-MAX
      **   DISPLAY "LNK-BYO-BYOMEIHENFLG="  LNK-BYO-BYOMEIHENFLG
      **   DISPLAY "LNK-BYO-HAISIYMD-ERR="  LNK-BYO-HAISIYMD-ERR
      **   DISPLAY "LNK-BYO-IKOSAKICD="     LNK-BYO-IKOSAKICD
      *??                 GO  TO  100-SYORI-EXT
      *??             END-IF
                   END-IF                              
      *
                   INITIALIZE                   LNK-BYOMEI-AREA
      *
                   IF      FLG-KENSAKU-KBN      <   6
                       GO  TO  100-SYORI-SEC
                   END-IF
      *
      *            検索結果がひとつのとき 
                   IF      LNK-BYOMEI-KENSAKU-MAX   =   1
                       MOVE    LNK-BYOMEI-AREA-OCC (1)  TO
                                                    LNK-BYOMEI-AREA
                       GO  TO  100-SYORI-EXT
                   END-IF
      *            編集病名でなく移行等もないとき
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   LNK-BYOMEI-KENSAKU-MAX
                       IF    ( LNK-BYO-O-BYOMEIHENFLG(IDXX) =   SPACE )
                       AND   ( LNK-BYO-O-IKOSAKICD   (IDXX) =   SPACE )
                       AND   ( LNK-BYO-O-HAISIYMD-ERR(IDXX) =   ZERO  )
                           IF   ( LNK-BYO-BYOMEI-MAX   =   ZERO       )
                           OR   ( LNK-BYO-BYOMEI-MAX   >
                                          LNK-BYO-O-BYOMEI-MAX (IDXX) )
                               MOVE    LNK-BYOMEI-AREA-OCC (IDXX)   TO
                                                        LNK-BYOMEI-AREA
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      LNK-BYO-BYOMEI-MAX  >   ZERO
                       GO  TO  100-SYORI-EXT
                   END-IF
      *            編集病名でないが移行等があるとき
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   LNK-BYOMEI-KENSAKU-MAX
                       IF    ( LNK-BYO-O-BYOMEIHENFLG(IDXX) =   SPACE )
      ****             AND   ( LNK-BYO-O-IKOSAKICD   (IDXX) =   SPACE )
      ****             AND   ( LNK-BYO-O-HAISIYMD-ERR(IDXX) =   ZERO  )
                           IF   ( LNK-BYO-BYOMEI-MAX   =   ZERO       )
                           OR   ( LNK-BYO-BYOMEI-MAX   >
                                          LNK-BYO-O-BYOMEI-MAX (IDXX) )
                               MOVE    LNK-BYOMEI-AREA-OCC (IDXX)   TO
                                                        LNK-BYOMEI-AREA
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      LNK-BYO-BYOMEI-MAX  >   ZERO
                       GO  TO  100-SYORI-EXT
                   END-IF
      *            編集病名のみのとき
                   MOVE    LNK-BYOMEI-AREA-OCC (1)  TO  LNK-BYOMEI-AREA
           END-EVALUATE
           .
       100-SYORI-EXT.
      *
           EXIT
           .
      *     
      *****************************************************************
      *    レセ電エラーフラグ編集処理
      *****************************************************************
       200-RECEDENFLG-HENSYU-SEC       SECTION.
      *
      *   補足コメントがあるときは除いた長さでチェック
           IF      LNK-BYO-BYOMEI-LEN  >   ZERO
               MOVE    LNK-BYO-BYOMEI-LEN
                                       TO  WRK-MOJISU
           ELSE
               COMPUTE WRK-MOJISU      =   LNK-BYO-BYOMEIMOJI
                                           *   2
           END-IF
           MOVE    LNK-BYO-BYOMEIHENFLG
                                   TO  WRK-BYOMEIHENFLG
      *   
      *    疑いフラグが設定されているとき
           IF      LNK-BYO-UTAGAIFLG-IN
                                   NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    LNK-BYO-UTAGAIFLG-IN
                                           TO  SMKBYO-UTAGAIFLG
      *★      補足コメントがあるときは除いた病名でチェック
               IF      LNK-BYO-BYOMEI-LEN  >   ZERO
                   MOVE    LNK-BYO-BYOMEI (1:LNK-BYO-BYOMEI-LEN)
                                               TO  SMKBYO-BYOMEI
                   PERFORM VARYING IDZ FROM    1   BY  1
                           UNTIL   IDZ >   LNK-BYO-HOSOKU-BYOMEICD-MAX
                       MOVE    LNK-BYO-TBYOMEICD (IDX)
                                               TO  SMKBYO-BYOMEICD (IDX)
                   END-PERFORM
               ELSE
                   MOVE    LNK-BYO-BYOMEI      TO  SMKBYO-BYOMEI
                   PERFORM VARYING IDZ FROM    1   BY  1
                           UNTIL   IDZ >       21
                       MOVE    LNK-BYO-TBYOMEICD (IDX)
                                               TO  SMKBYO-BYOMEICD (IDX)
                   END-PERFORM
               END-IF
               MOVE    WRK-BYOMEIHENFLG    TO  SMKBYO-BYOMEIHENFLG
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
               MOVE    SMKBYO-LENGTH       TO  WRK-MOJISU
               MOVE    SMKBYO-BYOMEIHENFLG TO  WRK-BYOMEIHENFLG
           END-IF    
      *
           COMPUTE WRK-MOJISU      =   WRK-MOJISU  /   2
      *  
           IF  (   WRK-BYOMEIHENFLG        =   "1"                    )
           OR  ( ( LNK-BYO-HAISIYMD-ERR    >   ZERO             ) AND
                 ( LNK-BYO-HAISIYMD-ERR    <   LNK-BYO-SRYYMD-IN )    )
               IF      WRK-MOJISU      >   20
                   MOVE    "1"         TO  LNK-BYO-RECEDENFLG
               END-IF
           END-IF    
           .
       200-RECEDENFLG-HENSYU-EXT.
           EXIT
           .
      *     
      *****************************************************************
      *    病名検索処理
      *****************************************************************
       200-BYOMEI-SYORI-SEC       SECTION.
      *
      *?   DISPLAY "200 FLG-KENSAKU-KBN=" FLG-KENSAKU-KBN "#"
      *
           INITIALIZE                  WRK-AREA
      *
      *    病名が空白のとき処理はしない
           IF      LNK-BYO-BYOMEI      =   SPACE
               MOVE    "2"         TO  LNK-BYO-RC
               GO  TO  200-BYOMEI-SYORI-EXT
           END-IF
      *
      *    半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    LNK-BYO-BYOMEI      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-SYUBETU     =   "2"
               MOVE    KANACHK-OUT-INPUT   TO  LNK-BYO-BYOMEI
           ELSE
               MOVE    1                   TO  LNK-BYO-KANACHK-ERR
               MOVE    9                   TO  FLG-KENSAKU-KBN
               GO  TO  200-BYOMEI-SYORI-EXT
           END-IF
      *
      *    文字数チェック
           IF      KANACHK-MAX         >   80
               MOVE    2                   TO  LNK-BYO-KANACHK-ERR
               MOVE    9                   TO  FLG-KENSAKU-KBN
               GO  TO  200-BYOMEI-SYORI-EXT
           END-IF
      *    病名文字数
           COMPUTE LNK-BYO-BYOMEIMOJI  =   KANACHK-MAX     /   2 
      *
           MOVE    KANACHK-MAX         TO  WRK-MOJISU        
      *
           IF      FLG-KENSAKU-KBN     =   ZERO    OR  3
      *
      *?   DISPLAY "200 FLG-KENSAKU-KBN=" FLG-KENSAKU-KBN " syori"
               ADD 1 TO   FLG-KENSAKU-KBN
      *
           MOVE    ZERO                TO  IDY
      *    入力病名で検索（病名のみ検索）
           MOVE    "tbl_byomei"        TO  WRK-MCP-TABLE
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
           INITIALIZE                      BYOMEI-REC
           MOVE    LNK-BYO-BYOMEI      TO  BYO-BYOMEI
           PERFORM 2001-BYOMEICD-KENSAKU-SEC
           IF      FLG-BYOMEI          =   ZERO
               MOVE    1                   TO  FLG-HENSYU
               PERFORM 2002-BYOMEICD-HENSYU-SEC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      IDY                 >   ZERO
               GO  TO  200-BYOMEI-SYORI-EXT
           END-IF
      *
      *    入力病名で検索（病名・修飾語全て検索）
           MOVE    "tbl_byomei"        TO  WRK-MCP-TABLE
           MOVE    "key6"              TO  WRK-MCP-PATHNAME
           INITIALIZE                      BYOMEI-REC
           MOVE    LNK-BYO-BYOMEI      TO  BYO-BYOMEI
           PERFORM 2001-BYOMEICD-KENSAKU-SEC
           IF      FLG-BYOMEI          =   ZERO
               MOVE    1                   TO  FLG-HENSYU
               PERFORM 2002-BYOMEICD-HENSYU-SEC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      IDY                 >   ZERO
               GO  TO  200-BYOMEI-SYORI-EXT
           END-IF
           END-IF
      *
      *
           IF      FLG-KENSAKU-KBN     =   1   OR  4
      *
      *?   DISPLAY "200 FLG-KENSAKU-KBN=" FLG-KENSAKU-KBN " syori"
               ADD 1 TO   FLG-KENSAKU-KBN
      *
      *    入力された病名で検索（前から短縮して検索）
           INITIALIZE                  WRK-BYOMEICD-KENSAKU-AREA
                                       WRK-KENSAKU-AREA
      *****COMPUTE IDY =   WRK-CONS-BYOMEI-MAX +   1
           MOVE    41                  TO  IDY
           MOVE    LNK-BYO-BYOMEI      TO  WRK-KEN-BYOMEI
           MOVE    WRK-MOJISU  TO  WRK-KEN-LENGTH
           MOVE    2                   TO  FLG-HENSYU
      *
           PERFORM             UNTIL   WRK-KEN-LENGTH  =  ZERO
               MOVE    ZERO                TO  WRK-KEN-ADDKBN
               PERFORM VARYING IDZ FROM    1   BY  2
                       UNTIL   IDZ >       WRK-KEN-LENGTH
                       OR      WRK-KEN-ADDKBN  =   1
      *            病名の検索
                   MOVE    "tbl_byomei"        TO  WRK-MCP-TABLE
                   MOVE    "key6"              TO  WRK-MCP-PATHNAME
                   INITIALIZE                  BYOMEI-REC
                   COMPUTE IDX1    =   WRK-KEN-LENGTH  -   IDZ +   1
                 IF    ( IDX1                =   WRK-MOJISU )
                       CONTINUE
                 ELSE
                   MOVE    WRK-KEN-BYOMEI(IDZ:IDX1)  
                                               TO  BYO-BYOMEI
                   PERFORM 2001-BYOMEICD-KENSAKU-SEC
                   IF      FLG-BYOMEI          =   ZERO
                       PERFORM 2002-BYOMEICD-HENSYU-SEC
                       IF      FLG-TBLERR          =   1
                           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                           PERFORM 900-CLOSE-SEC
      *
                           GO  TO  200-BYOMEI-SYORI-EXT
                       END-IF 
                   END-IF      
      *
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      WRK-KEN-ADDKBN          =   1
                       MOVE    WRK-KEN-LENGTH  TO  IDZ
                       COMPUTE WRK-KEN-LENGTH  =   WRK-KEN-LENGTH  -
                                                   IDX1
                   END-IF
                 END-IF
               END-PERFORM
      *
      *        検索できなかったとき予約コードで検索
               IF      WRK-KEN-ADDKBN          =   ZERO
                   MOVE    2                   TO  IDX1
                   COMPUTE IDW =   WRK-KEN-LENGTH  -   1
                   MOVE    WRK-KEN-BYOMEI(IDW:IDX1)  
                                               TO  WRK-KENBYOMEI
                   MOVE    IDW                 TO  WRK-KENBYOMEI-ST
                   PERFORM 2006-KAKKO-KENSAKU-SEC
                   IF      FLG-TBLERR          =   1
                       GO  TO  200-BYOMEI-SYORI-EXT
                   END-IF
      *
      *            検索できたとき
                   IF      WRK-KEN-ADDKBN          =   1
                       COMPUTE WRK-KEN-LENGTH  =   WRK-KEN-LENGTH  -
                                                   IDX1
                   END-IF
      *
      *            検索できなかったときは終了
                   IF      WRK-KEN-ADDKBN          =   ZERO
                       MOVE    ZERO                TO  WRK-KEN-LENGTH
                   END-IF
               END-IF
           END-PERFORM
           END-IF 
      *
      *    入力された病名で検索（後ろから短縮して検索）
      *
           IF      WRK-KEN-ADDKBN          =   ZERO
           IF      FLG-KENSAKU-KBN     =   2   OR  5
      *
      *?   DISPLAY "200 FLG-KENSAKU-KBN=" FLG-KENSAKU-KBN " syori"
               ADD 1 TO   FLG-KENSAKU-KBN
      *
               INITIALIZE                  WRK-BYOMEICD-KENSAKU-AREA
                                           WRK-KENSAKU-AREA
               MOVE    ZERO                TO  IDY 
      *
               MOVE    LNK-BYO-BYOMEI          TO  WRK-KEN-BYOMEI
               MOVE    WRK-MOJISU      TO  WRK-KEN-LENGTH
               MOVE    1                   TO  IDZ
               MOVE    1                   TO  FLG-HENSYU
      *
               PERFORM             UNTIL   IDZ     >=  WRK-MOJISU
      *            病名の検索
                   MOVE    ZERO                TO  WRK-KEN-ADDKBN
                   PERFORM VARYING IDW FROM    WRK-KEN-LENGTH  BY  -2 
                           UNTIL   IDW <       2
                           OR      WRK-KEN-ADDKBN  =   1
                     IF      IDW                   =   WRK-MOJISU
                       CONTINUE
                     ELSE
                       MOVE    IDW             TO  IDX1
                       MOVE    "tbl_byomei"        TO  WRK-MCP-TABLE
                       MOVE    "key6"              TO  WRK-MCP-PATHNAME
                       INITIALIZE                  BYOMEI-REC
                       MOVE    WRK-KEN-BYOMEI(IDZ:IDX1)  
                                               TO  BYO-BYOMEI
                       PERFORM 2001-BYOMEICD-KENSAKU-SEC
                       IF      FLG-BYOMEI      =   ZERO
      *                    未検索の病名があれば編集
                           IF      FLG-BUBUN           >   ZERO
                               PERFORM 2007-BUBUN-HENSYU-SEC
                               IF      FLG-TBLERR          =   1
                                   MOVE    WRK-MCP-TABLE
                                                       TO  MCP-TABLE
                                   MOVE    WRK-MCP-PATHNAME
                                                       TO  MCP-PATHNAME
                                   PERFORM 900-CLOSE-SEC
      *
                                   GO  TO  200-BYOMEI-SYORI-EXT
                               END-IF 
                           END-IF
      *
                           PERFORM 2002-BYOMEICD-HENSYU-SEC
                           IF      FLG-TBLERR  =   1
                               MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
                               MOVE    WRK-MCP-PATHNAME
                                                       TO  MCP-PATHNAME
                               PERFORM 900-CLOSE-SEC
      *
                               GO  TO  200-BYOMEI-SYORI-EXT
                           END-IF
                       END-IF      
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
      *                検索できたとき
                       IF      WRK-KEN-ADDKBN      =   1
                           COMPUTE IDZ     =   IDZ     +   IDX1
                           COMPUTE WRK-KEN-LENGTH
                                           =   WRK-KEN-LENGTH  -
                                               IDX1
                       END-IF
                     END-IF
                   END-PERFORM
      *
      *            検索できなかったとき予約コードで検索
                   IF      WRK-KEN-ADDKBN          =   ZERO
                       MOVE    2                   TO  IDX1
                       MOVE    WRK-KEN-BYOMEI(IDZ:IDX1)  
                                                   TO  WRK-KENBYOMEI
                       MOVE    IDZ                 TO  WRK-KENBYOMEI-ST
                       PERFORM 2006-KAKKO-KENSAKU-SEC
                       IF      FLG-TBLERR          =   1
                           GO  TO  200-BYOMEI-SYORI-EXT
                       END-IF
      *
      *                検索できなかったときの位置を取得
                       IF    ( WRK-KEN-ADDKBN          =   ZERO )
                       AND   ( FLG-BUBUN               =   ZERO )
                            MOVE    IDZ                TO  FLG-BUBUN
                       END-IF
      *
                       COMPUTE IDZ     =   IDZ   +   IDX1
                       COMPUTE WRK-KEN-LENGTH
                                       =   WRK-KEN-LENGTH  -
                                           IDX1
                   END-IF
               END-PERFORM
      *   
      *        未検索の病名があれば編集
               IF      FLG-BUBUN           >   ZERO
                   PERFORM 2007-BUBUN-HENSYU-SEC
                   IF      FLG-TBLERR          =   1
                       GO  TO  200-BYOMEI-SYORI-EXT
                   END-IF 
               END-IF
           END-IF
           END-IF
           .
       200-BYOMEI-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    病名検索処理
      *****************************************************************
       200-BYOMEICD-SYORI-SEC     SECTION.
      *
           MOVE    1                   TO  FLG-HENSYU
      *
      *    病名コードが設定されていないとき
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ  >      21
               IF       LNK-BYO-TBYOMEICD (IDZ)    NOT =   SPACE
                   MOVE    1               TO  IDY
                   MOVE    21              TO  IDX
               END-IF
           END-PERFORM
           IF      IDY                 =   ZERO
               MOVE    "3"                 TO  LNK-BYO-RC
               GO  TO  200-BYOMEICD-SYORI-EXT
           END-IF   
      *
      *    病名コードで検索
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ  >      21
               IF       LNK-BYO-TBYOMEICD (IDZ)    NOT =   SPACE 
                   MOVE    "tbl_byomei"        TO  WRK-MCP-TABLE
                   MOVE    "key"               TO  WRK-MCP-PATHNAME
                   INITIALIZE                  BYOMEI-REC
                   MOVE    LNK-BYO-TBYOMEICD (IDZ)
                                               TO  BYO-BYOMEICD
                   PERFORM 2001-BYOMEICD-KENSAKU-SEC
                   IF      FLG-BYOMEI          =   ZERO
                       PERFORM 2002-BYOMEICD-HENSYU-SEC
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
                       IF      FLG-TBLERR          =   1
                           GO  TO  200-BYOMEICD-SYORI-EXT
                       END-IF 
                   ELSE
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
                       SET     TBL-BYOMEICD-IDX    TO  1
                       SEARCH  TBL-BYOMEICD-OCC
                                       VARYING TBL-BYOMEICD-IDX
                           AT  END
                               MOVE    ZERO        TO  FLG-BYOMEICD-OK
                           WHEN    LNK-BYO-TBYOMEICD (IDZ)
                                       =   TBL-YYK-BYOMEICD 
                                                   (TBL-BYOMEICD-IDX)
                               MOVE    1           TO  FLG-BYOMEICD-OK
                               ADD     1           TO  IDY
                               IF      IDY     >   WRK-CONS-BYOMEI-MAX
                                   MOVE    1           TO  FLG-TBLERR
                                   GO  TO  200-BYOMEICD-SYORI-EXT
                               END-IF
                               MOVE    "1"
                                       TO  WRK-BYOMEI-FLG      (IDY)
                               MOVE
                                   TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                                       TO  WRK-BYOMEI-BYOMEICD (IDY 1) 
                               MOVE
                                   TBL-YYK-BYOMEI   (TBL-BYOMEICD-IDX)
                                       TO  WRK-BYOMEI-BYOMEI   (IDY)
                       END-SEARCH
                       IF      FLG-BYOMEICD-OK =   ZERO            
                           MOVE    3           TO  LNK-BYO-BYOMEICD-ERR
                           MOVE    ZERO        TO  WRK-BYOMEI-MAX
                           GO  TO  200-BYOMEICD-SYORI-EXT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM    
           .
       200-BYOMEICD-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    廃止病名検索処理
      *****************************************************************
       200-HAISI-BYOMEI-SYORI-SEC  SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  2
                   UNTIL   IDX >       200
                   OR      LNK-BYO-BYOMEI (IDX:2)  =   SPACE
               IF      LNK-BYO-BYOMEI (IDX:2)  =   "（"
                   MOVE    IDX             TO  WRK-BYOMEI-YYK-ST
               END-IF
               IF      LNK-BYO-BYOMEI (IDX:2)  =   "）"
                   MOVE    IDX             TO  WRK-BYOMEI-YYK-ED
               END-IF
           END-PERFORM
      *
           IF    ( WRK-BYOMEI-YYK-ST   >   2                 )
           AND   ( WRK-BYOMEI-YYK-ED   >   4                 )
           AND   ( WRK-BYOMEI-YYK-ST   <   WRK-BYOMEI-YYK-ED )
               COMPUTE WRK-BYOMEI-HOSOKU-LEN   =
                                       WRK-BYOMEI-YYK-ED   -
                                       WRK-BYOMEI-YYK-ST   -
                                       2
               IF      WRK-BYOMEI-HOSOKU-LEN   <=  40
                   MOVE    1                   TO
                                           LNK-BYO-HOSOKU-BYOMEICD-MAX
                   COMPUTE LNK-BYO-BYOMEI-LEN          =
                                WRK-BYOMEI-YYK-ST      -   1
                   MOVE    LNK-BYO-BYOMEI (1:LNK-BYO-BYOMEI-LEN)
                                           TO  LNK-BYO-HOSOKU-BYOMEI-ETC
                   COMPUTE IDXX    =   WRK-BYOMEI-YYK-ST   +   2
                   MOVE    LNK-BYO-BYOMEI (IDXX:WRK-BYOMEI-HOSOKU-LEN)
                                           TO  LNK-BYO-HOSOKU-BYOMEI
                   MOVE    "1"             TO  LNK-BYO-BYOMEIHENFLG
                   MOVE    "0000999"       TO  LNK-BYO-TBYOMEICD (1)
                   MOVE    1               TO
                                           LNK-BYO-HOSOKU-BYOMEICD-MAX
               END-IF
           END-IF
      *
           .
       200-HAISI-BYOMEI-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    病名検索処理
      *****************************************************************
       200-TBL-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TBLERR
      *
      *    予約以外の病名コードを検索できなかったとき
           IF      WRK-BYOMEI-MAX      =   ZERO
               GO  TO  200-TBL-HENSYU-EXT
           END-IF 
      *
      *    検索できなかった病名があるとき
           IF      FLG-BUBUN           >   ZERO
               GO  TO  200-TBL-HENSYU-EXT
           END-IF 
      *
      *    検索病名コード情報編集
           MOVE    ZERO                TO  IDY
                                           WRK-BUI-MAX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       40
               IF    ( WRK-BYOMEI-BYOMEICD (IDX 1)
                                               =   SPACE AND
                       WRK-BYOMEI-BYOMEICD (IDX 2)
                                               =   SPACE AND
                       WRK-BYOMEI-FLG  (IDX)  =   SPACE     )
                   CONTINUE
               ELSE
      *****        IF      WRK-BYOMEI-FLG  (IDX)   =   "1"
      *****            CONTINUE
      *****        ELSE
                   IF      LNK-BYO-BYOMEI-MAX
                                   >=  WRK-CONS-BYOMEI-MAX
                       MOVE    1               TO  FLG-TBLERR
                       MOVE    40              TO  IDX
                   ELSE
                       ADD     1               TO  LNK-BYO-BYOMEI-MAX
                       EVALUATE    TRUE
                           WHEN  ( WRK-BYOMEI-BYOMEICD (IDX 1)
                                                       =   SPACE ) AND
                                 ( WRK-BYOMEI-BYOMEICD (IDX 2)
                                                   NOT =   SPACE )
                               MOVE    2               TO  IDX2
                               PERFORM 2003-LNK-HENSYU-SEC
                           WHEN  ( WRK-BYOMEI-BYOMEICD (IDX 1)
                                                   NOT =   SPACE ) AND
                                 ( WRK-BYOMEI-BYOMEICD (IDX 2)
                                                       =   SPACE )
                               MOVE    1               TO  IDX2
                               PERFORM 2003-LNK-HENSYU-SEC
                           WHEN  ( WRK-BYOMEI-BYOMEICD (IDX 1)
                                                   NOT =   SPACE ) AND
                                 ( WRK-BYOMEI-BYOMEICD (IDX 2)
                                                   NOT =   SPACE )
                               IF      IDX             =   40
                                   MOVE    2               TO  IDX2
                                   PERFORM 2003-LNK-HENSYU-SEC
                               ELSE
                                   COMPUTE IDX4    =   IDX     +   1
                                   MOVE    ZERO        TO  FLG-BYOMEICD 
                                   PERFORM VARYING IDX1    FROM    IDX4
                                                           BY      1
                                           UNTIL   IDX1    >       40
                                       IF      WRK-BYOMEI-BYOMEICD
                                                               (IDX1 2)
                                                       NOT =   SPACE
                                           MOVE    1   TO  FLG-BYOMEICD
                                           MOVE    40  TO  IDX1
                                       END-IF 
                                   END-PERFORM     
                                   IF      FLG-BYOMEICD      =   1
                                       MOVE    1           TO  IDX2
                                       PERFORM 2003-LNK-HENSYU-SEC
                                   ELSE
                                       MOVE    2           TO  IDX2
                                       PERFORM 2003-LNK-HENSYU-SEC
                                   END-IF
                               END-IF
                       END-EVALUATE
                   END-IF     
               END-IF
           END-PERFORM
      *
           PERFORM 2004-LNK-BYOMEI-HENSYU-SEC     
           .
       200-TBL-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    病名検索処理
      *****************************************************************
       2001-BYOMEICD-KENSAKU-SEC        SECTION.
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-BYOMEI-READ-N-SEC
           ELSE
               INITIALIZE                  BYOMEI-REC
               MOVE    1               TO  FLG-BYOMEI
           END-IF
           .
       2001-BYOMEICD-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索病名コード編集処理
      *****************************************************************
       2002-BYOMEICD-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TBLERR
      * 
           ADD     1               TO  WRK-BYOMEI-MAX
      *
           EVALUATE    FLG-HENSYU
               WHEN    1
                   ADD     1               TO  IDY
                   IF      IDY             >   WRK-CONS-BYOMEICD-TBL-MAX
                       MOVE    1           TO  FLG-TBLERR
                       GO  TO  2002-BYOMEICD-HENSYU-EXT
                   END-IF
               WHEN    2
                   COMPUTE IDY     =   IDY     -   1
                   IF      IDY         =   ZERO
                       MOVE    1           TO  FLG-TBLERR
                       GO  TO  2002-BYOMEICD-HENSYU-EXT
                   END-IF
           END-EVALUATE
      *    
      *    未コード化傷病名コードのときはコードだけ編集
           IF      BYO-BYOMEICD    =   "0000999"
               MOVE    2               TO  IDX
               MOVE    BYO-BYOMEICD    TO
                                       WRK-BYOMEI-BYOMEICD  (IDY IDX)
               GO  TO  2002-BYOMEICD-HENSYU-EXT
           END-IF
      *
           MOVE    BYO-BYOMEI      TO  WRK-BYOMEI-BYOMEI    (IDY)
      *
           PERFORM             UNTIL   FLG-BYOMEI  =   1
               EVALUATE   BYO-BYOMEICD (1:1)
                   WHEN    "Z"
                       MOVE    1           TO  IDX 
                   WHEN    OTHER
                       MOVE    2           TO  IDX
               END-EVALUATE 
      *  
               MOVE    BYO-BYOMEICD    TO
                                       WRK-BYOMEI-BYOMEICD  (IDY IDX)
               MOVE    BYO-IKOSAKICD   TO
                                       WRK-BYOMEI-IKOSAKICD (IDY IDX)
               MOVE    BYO-HAISIYMD    TO
                                       WRK-BYOMEI-HAISIYMD  (IDY IDX)
               MOVE    BYO-TANDOKUKBN  TO
                                       WRK-BYOMEI-TANDOKUKBN(IDY IDX)
               MOVE    BYO-TOKSKNCD    TO
                                       WRK-BYOMEI-TOKSKNCD  (IDY IDX)
               MOVE    BYO-NANBYOCD    TO
                                       WRK-BYOMEI-NANBYOCD  (IDY IDX)
               MOVE    BYO-UTAGAIFLG   TO
                                       WRK-BYOMEI-UTAGAIFLG (IDY IDX)
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-BYOMEI-READ-N-SEC
           END-PERFORM
      *
           MOVE    1               TO  WRK-KEN-ADDKBN
           .
       2002-BYOMEICD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       2003-LNK-HENSYU-SEC             SECTION.
      *
           MOVE    LNK-BYO-BYOMEI-MAX  TO  IDY
      *
           MOVE    WRK-BYOMEI-BYOMEI     (IDX)
                                       TO  LNK-BYO-TBYOMEI     (IDY)
           MOVE    WRK-BYOMEI-BYOMEICD   (IDX  IDX2)
                                       TO  LNK-BYO-TBYOMEICD   (IDY)
           MOVE    WRK-BYOMEI-IKOSAKICD  (IDX  IDX2)
                                       TO  LNK-BYO-TIKOSAKICD  (IDY)
           MOVE    WRK-BYOMEI-HAISIYMD   (IDX  IDX2)
                                       TO  LNK-BYO-THAISIYMD   (IDY)
           MOVE    WRK-BYOMEI-TANDOKUKBN (IDX  IDX2)
                                       TO  LNK-BYO-TTANDOKUKBN (IDY)
           MOVE    WRK-BYOMEI-TOKSKNCD   (IDX  IDX2)
                                       TO  LNK-BYO-TTOKSKNCD   (IDY)
           MOVE    WRK-BYOMEI-NANBYOCD   (IDX  IDX2)
                                       TO  LNK-BYO-TNANBYOCD   (IDY)
           MOVE    WRK-BYOMEI-UTAGAIFLG  (IDX  IDX2)
                                       TO  LNK-BYO-TUTAGAIFLG  (IDY)
      *
           MOVE    WRK-BYOMEI-FLG        (IDX)
                                       TO  WRK-LNK-BYOMEI-FLG  (IDY)
      *
      *    最後の（）の格納位置を取得
           IF      LNK-BYO-SYORI       =   "3" OR  "4"
               IF      LNK-BYO-TBYOMEICD (IDY)
                                           =   "ZZZ(101"
                   IF      WRK-BYOMEI-YYK-INDEX1   =   ZERO
      *************IF      WRK-BYOMEI-YYK-INDEX1   <   IDY
                       MOVE    IDY          TO  WRK-BYOMEI-YYK-INDEX1
                   END-IF
               END-IF 
               IF      LNK-BYO-TBYOMEICD (IDY)
                                           =   "ZZZ(201"
               IF      WRK-BYOMEI-YYK-INDEX2   =   ZERO
      *************IF      WRK-BYOMEI-YYK-INDEX2   <   IDY
                       MOVE    IDY          TO  WRK-BYOMEI-YYK-INDEX2
                   END-IF
               END-IF
           END-IF 
      *
      *    基本病名数
           EVALUATE    LNK-BYO-TBYOMEICD (IDY) (1:3)
               WHEN    "ZZZ"
                   IF      LNK-BYO-TBYOMEICD (IDY) (4:1)   =   "1"
                       ADD     1           TO  WRK-BUI-MAX
                       IF      WRK-BUI-MAX <   4
                           MOVE    LNK-BYO-TBYOMEICD (IDY)
                                       TO  LNK-BYO-KHNBUICD(WRK-BUI-MAX)
                       END-IF
                   END-IF
               WHEN    "000"
                   MOVE    1           TO  FLG-HENKAN-ERR
      *           最初の未コード化病名の位置を取得
                   IF      WRK-BYOMEI-HENKAN-INDEX =   ZERO
                       MOVE    IDY          TO  WRK-BYOMEI-HENKAN-INDEX
                   END-IF
               WHEN   OTHER
      *            基本病名コード
                   IF      LNK-BYO-KHNBYOMEICD    =   SPACE
                       MOVE    LNK-BYO-TBYOMEICD (IDY)
                                               TO  LNK-BYO-KHNBYOMEICD
                   END-IF
      *
                   ADD     1           TO  WRK-BYOMEICDSU1
      *            補足コメントまでの基本病名数
                   IF    ( WRK-BYOMEI-YYK-INDEX1
                                           >   ZERO )  
                   AND   ( WRK-BYOMEI-YYK-INDEX1
                                           <   IDY  )
                       CONTINUE
                   ELSE   
                       ADD     1           TO  WRK-BYOMEICDSU
                   END-IF
           END-EVALUATE
           .
       2003-LNK-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    病名検索処理
      *****************************************************************
       2004-LNK-BYOMEI-HENSYU-SEC      SECTION.
      *
      *    病名コード数
           MOVE    LNK-BYO-BYOMEI-MAX  TO  LNK-BYO-BYOMEICDSU
      *
      *    疑いフラグ・慢性区分
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
      *
               IF      LNK-BYO-UTAGAIFLG   =   "3"
                   CONTINUE
               ELSE
                   EVALUATE    LNK-BYO-TUTAGAIFLG   (IDX)
                       WHEN    "1"
                           IF      LNK-BYO-UTAGAIFLG   =   "2"
                               MOVE    "3"     TO  LNK-BYO-UTAGAIFLG
                            ELSE
                               MOVE    "1"     TO  LNK-BYO-UTAGAIFLG
                            END-IF    
                       WHEN    "2"
                           IF      LNK-BYO-UTAGAIFLG  =   "1"
                               MOVE    "3"     TO  LNK-BYO-UTAGAIFLG
                            ELSE
                               MOVE    "2"     TO  LNK-BYO-UTAGAIFLG
                           END-IF
                       WHEN    "3"
                           MOVE    "3"     TO  LNK-BYO-UTAGAIFLG
                    END-EVALUATE
               END-IF
      *
               IF      LNK-BYO-TTOKSKNCD (IDX) 
                                       NOT =   ZERO
               AND     LNK-BYO-MANSEIKBN   =   ZERO
                   MOVE    LNK-BYO-TTOKSKNCD (IDX)
                                           TO  LNK-BYO-MANSEIKBN 
               END-IF   
      *
               IF      LNK-BYO-TNANBYOCD (IDX) 
                                       NOT =   ZERO
               AND     LNK-BYO-NANBYOCD    =   ZERO
                   MOVE    LNK-BYO-TNANBYOCD (IDX)
                                           TO  LNK-BYO-NANBYOCD
               END-IF   
           END-PERFORM
      *
      *    病名コードからの処理時
           IF    ( LNK-BYO-SYORI           =   "4" )
               MOVE    LNK-BYO-TBYOMEI (1) TO  WRK-BYOMEI
               PERFORM VARYING IDX FROM    2   BY  1
                       UNTIL   IDX >       LNK-BYO-BYOMEI-MAX
                   STRING  WRK-BYOMEI          DELIMITED   BY  SPACE
                           LNK-BYO-TBYOMEI(IDX)
                                               DELIMITED   BY  SPACE
                                               INTO    WRK-BYOMEI
                   END-STRING
               END-PERFORM
      *
      *        コードから組み立てた病名が実際の病名と違うとき
               IF    ( WRK-BYOMEI      NOT =   LNK-BYO-BYOMEI )
               AND   ( LNK-BYO-BYOMEI  NOT =   SPACE          )
                   MOVE    "9"                 TO  LNK-BYO-RC
                   MOVE    SPACE               TO  LNK-BYO-BYOMEI
                   GO  TO  2004-LNK-BYOMEI-HENSYU-EXT
               END-IF
      *
      *        病名コードからの処理時の補足コメントの位置
               MOVE    SPACE               TO  LNK-BYO-BYOMEI
               IF    ( WRK-BYOMEI-YYK-INDEX1   >   1   )
               AND   ( WRK-BYOMEI-YYK-INDEX2   >   3   )
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       200
                           OR      WRK-BYOMEI (IDX:2)  =   SPACE
                       IF      WRK-BYOMEI (IDX:2)  =   "（"
                           MOVE    IDX             TO  WRK-BYOMEI-YYK-ST
                       END-IF
                       IF      WRK-BYOMEI (IDX:2)  =   "）"
                           MOVE    IDX             TO  WRK-BYOMEI-YYK-ED
                       END-IF
                   END-PERFORM
                   MOVE    WRK-BYOMEI      TO  LNK-BYO-BYOMEI
               END-IF 
           END-IF     
      *    （）が片方のとき、（の前に文字があるか、）の後に文字があるか
      *    チェック
           MOVE    LNK-BYO-BYOMEI-MAX  TO  WRK-BYO-BYOMEI-MAX
      *
           IF    ( WRK-BYOMEI-YYK-INDEX1   >   1    )
           AND   ( WRK-BYOMEI-YYK-INDEX2   >   3    )
               IF      WRK-BYOMEI-YYK-INDEX1   >   WRK-BYOMEI-YYK-INDEX2
               OR      WRK-BYOMEI-YYK-INDEX2   <   LNK-BYO-BYOMEI-MAX
                   MOVE    ZERO            TO  WRK-BYOMEI-YYK-INDEX1
                                               WRK-BYOMEI-YYK-INDEX2
                   MOVE    WRK-BYOMEICDSU1 TO  WRK-BYOMEICDSU
               ELSE
                   COMPUTE WRK-BYOMEI-HOSOKU-LEN   =
                                       WRK-BYOMEI-YYK-ED   -
                                       WRK-BYOMEI-YYK-ST   -
                                       2
      *            補足コメントが存在するとき、それ以前の病名でチェックを行う
      *            補足コメントの（）があっても編集病名扱いとしない
                   IF      WRK-BYOMEI-HOSOKU-LEN   <=  40
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >
                                               WRK-BYOMEI-YYK-INDEX1
                           IF      IDXX    <   WRK-BYOMEI-YYK-INDEX1
                               IF      WRK-LNK-BYOMEI-FLG (IDXX)
                                               =   "1"
                                   MOVE    "1"         TO
                                                   LNK-BYO-BYOMEIHENFLG
                                   MOVE    ZERO        TO
                                                   WRK-BYOMEI-YYK-INDEX1
                                                   WRK-BYOMEI-YYK-INDEX2
                                   MOVE    WRK-BYOMEICDSU1
                                                       TO
                                                   WRK-BYOMEICDSU
                                   GO  TO  2004-LNK-BYOMEI-HENSYU-EXT
                               END-IF
                           END-IF
                       END-PERFORM    
                       MOVE    SPACE           TO  WRK-BYOMEI-FLG-TBL
                       COMPUTE WRK-BYO-BYOMEI-MAX
                                           =   WRK-BYOMEI-YYK-INDEX1
                                               -   1
                   ELSE
                       MOVE    ZERO            TO  WRK-BYOMEI-YYK-INDEX1
                                                   WRK-BYOMEI-YYK-INDEX2
                   MOVE    WRK-BYOMEICDSU1     TO  WRK-BYOMEICDSU
                   END-IF
               END-IF
           ELSE
               MOVE    ZERO            TO  WRK-BYOMEI-YYK-INDEX1
                                           WRK-BYOMEI-YYK-INDEX2
               MOVE    WRK-BYOMEICDSU1 TO  WRK-BYOMEICDSU
           END-IF
      *
      *    病名編集フラグ
           IF      WRK-BYOMEI-FLG-TBL  NOT =   SPACE
               MOVE    "1"             TO  LNK-BYO-BYOMEIHENFLG
               GO  TO  2004-LNK-BYOMEI-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-TANDOKUKBN
                                           WRK-TANDOKUKBN-BYOMEI
                                           WRK-RECECD-TBL
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-BYO-BYOMEI-MAX
      *
      *        単独使用禁止チェック
               IF      LNK-BYO-TTANDOKUKBN (IDX)
                                       =   ZERO
                   MOVE    "1"             TO  WRK-TANDOKUKBN
               ELSE
                   MOVE    LNK-BYO-TBYOMEI (IDX)
                                           TO  WRK-TANDOKUKBN-BYOMEI
               END-IF
      *        廃止病名のとき（最初に見つけたコード）
               IF      LNK-BYO-THAISIYMD (IDX)
                                       =   ZERO    OR  ALL "9"
                   CONTINUE
               ELSE
                   IF      LNK-BYO-HAISIYMD-ERR
                                           =   ZERO
                       MOVE    LNK-BYO-THAISIYMD (IDX)
                                           TO  LNK-BYO-HAISIYMD-ERR
                   END-IF
               END-IF
      *        移行先コードがあるとき（最初に見つけたコード）
               IF    ( LNK-BYO-TIKOSAKICD (IDX)
                                   NOT =   SPACE )
               AND   ( LNK-BYO-IKOSAKICD
                                       =   SPACE )
                   MOVE    LNK-BYO-TIKOSAKICD (IDX)
                                           TO  LNK-BYO-IKOSAKICD
                   MOVE    IDX             TO  LNK-BYO-IKOSAKICD-INDEX 
               END-IF
      *        接尾語・接頭語チェック
               EVALUATE    LNK-BYO-TBYOMEICD(IDX)(1:3)
                               ALSO    LNK-BYO-TBYOMEICD(IDX)(4:1)
                   WHEN    "ZZZ"       ALSO    "1"
                       MOVE    LNK-BYO-TBYOMEICD(IDX)(4:1)
                                           TO  WRK-RECECD (1)
                       IF      WRK-RECECD (2)  NOT =   SPACE   OR
                               WRK-RECECD (3)  NOT =   SPACE 
                           MOVE    2      TO  LNK-BYO-BYOMEICD-ERR
                       END-IF            
                       MOVE    1          TO  LNK-BYO-BYOMEISBT
                   WHEN    "ZZZ"       ALSO    "2" THRU    "7"
                       MOVE    LNK-BYO-TBYOMEICD(IDX)(4:1)
                                           TO  WRK-RECECD (1)
                       IF      WRK-RECECD (2)  NOT =   SPACE   OR
                               WRK-RECECD (3)  NOT =   SPACE 
                            MOVE    2      TO  LNK-BYO-BYOMEICD-ERR
                       END-IF            
                       MOVE    7           TO  LNK-BYO-BYOMEISBT
                    WHEN    "ZZZ"       ALSO    "8"  
                        MOVE    LNK-BYO-TBYOMEICD(IDX)(4:1)
                                           TO  WRK-RECECD (3)
                        IF      WRK-RECECD (2)    =   SPACE 
                            MOVE    2      TO  LNK-BYO-BYOMEICD-ERR
                       END-IF
                       MOVE    8           TO  LNK-BYO-BYOMEISBT
                   WHEN    "ZZZ"       ALSO    ANY
                       CONTINUE
                   WHEN    OTHER                 
                       MOVE    "1"         TO  WRK-RECECD (2)
                       IF      WRK-RECECD (3)    NOT =   SPACE 
                            MOVE    2      TO  LNK-BYO-BYOMEICD-ERR
                       END-IF    
               END-EVALUATE
           END-PERFORM
      *
      *    病名種別
           IF      WRK-RECECD (2)      NOT =   SPACE
               MOVE    ZERO                TO  LNK-BYO-BYOMEISBT
           END-IF
      *
      *    接尾語・接頭語チェック（接頭語のみのとき）
           IF    ( WRK-RECECD (1)    NOT =   SPACE )
           AND   ( WRK-RECECD (2)        =   SPACE )
           AND   ( WRK-RECECD (3)        =   SPACE )
               MOVE    1      TO  LNK-BYO-BYOMEICD-ERR
           END-IF
      *    接尾語・接頭語チェック（接尾語のみのとき）
           IF    ( WRK-RECECD (1)        =   SPACE )
           AND   ( WRK-RECECD (2)        =   SPACE )
           AND   ( WRK-RECECD (3)    NOT =   SPACE )
               MOVE    1      TO  LNK-BYO-BYOMEICD-ERR
           END-IF
      *
      *    単独禁止のみのとき
           IF      WRK-TANDOKUKBN          =   SPACE
               MOVE    1                   TO  LNK-BYO-TANDOKUKBN-ERR
               MOVE    WRK-TANDOKUKBN-BYOMEI
                                           TO  LNK-BYO-TANDOKUKBN-BYOMEI
           END-IF
      *
      *    移行先病名
           IF      LNK-BYO-IKOSAKICD   NOT =   SPACE
               MOVE    "tbl_byomei"        TO  WRK-MCP-TABLE
               MOVE    "key"               TO  WRK-MCP-PATHNAME
               MOVE    LNK-BYO-IKOSAKICD   TO  BYO-BYOMEICD
               PERFORM 900-BYOMEI-READ-SEC
               MOVE    BYO-BYOMEI          TO  LNK-BYO-IKOSAKICD-BYOMEI
           END-IF
      *    
      *    複数基本病名コードが存在するとき
           IF      WRK-BYOMEICDSU      >   1
               MOVE    9                   TO  LNK-BYO-BYOMEICD-ERR
               GO  TO  2004-LNK-BYOMEI-HENSYU-EXT
           END-IF
           .
       2004-LNK-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約コード検索処理
      *****************************************************************
       2006-KAKKO-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-KENSAKU
                                       FLG-TBLERR
      *
           SET     TBL-BYOMEICD-IDX    TO  1
           SEARCH  TBL-BYOMEICD-OCC    VARYING TBL-BYOMEICD-IDX
               AT  END
                   MOVE    ZERO            TO  FLG-BYOMEICD-OK
               WHEN    WRK-KENBYOMEI   =   TBL-YYK-BYOMEI 
                                               (TBL-BYOMEICD-IDX)
                   MOVE    1               TO  FLG-BYOMEICD-OK
                   MOVE    1               TO  FLG-KENSAKU
      *
      *            未検索の病名があれば編集
                   IF      FLG-BUBUN           >   ZERO
                       PERFORM 2007-BUBUN-HENSYU-SEC
                       IF      FLG-TBLERR          =   1
                           GO  TO  2006-KAKKO-KENSAKU-EXT
                       END-IF
                   END-IF
      *
                   EVALUATE    FLG-HENSYU
                       WHEN    "1"
                           ADD     1           TO  IDY
                           IF      IDY         >
                                           WRK-CONS-BYOMEICD-TBL-MAX
                               MOVE    1           TO  FLG-TBLERR
                               GO  TO  2006-KAKKO-KENSAKU-EXT
                           END-IF
                       WHEN    "2"  
                           COMPUTE IDY     =   IDY     -   1
                           IF      IDY         =   ZERO
                               MOVE    1           TO  FLG-TBLERR
                               GO  TO  2006-KAKKO-KENSAKU-EXT
                           END-IF
                   END-EVALUATE
      *
                   MOVE    "1"         TO  WRK-BYOMEI-FLG      (IDY)
                   MOVE    TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                                       TO  WRK-BYOMEI-BYOMEICD (IDY 1) 
                   MOVE    TBL-YYK-BYOMEI   (TBL-BYOMEICD-IDX)
                                       TO  WRK-BYOMEI-BYOMEI   (IDY)
                   MOVE    1           TO  WRK-KEN-ADDKBN
      *
      *            （）の位置を取得
                   IF      LNK-BYO-SYORI   =   "3" OR  "4"
                       IF      TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                                           =   "ZZZ(101"
                           MOVE    WRK-KENBYOMEI-ST
                                                TO  WRK-BYOMEI-YYK-ST
                       END-IF 
                       IF      TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                                           =   "ZZZ(201"
                           MOVE    WRK-KENBYOMEI-ST
                                                TO  WRK-BYOMEI-YYK-ED
                       END-IF
                   END-IF
           END-SEARCH
           .
       2006-KAKKO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名部分検索処理
      *****************************************************************
       2007-BUBUN-HENSYU-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-TBLERR
           EVALUATE    FLG-HENSYU
               WHEN    "1"
                   ADD     1           TO  IDY
                   IF      IDY         >   WRK-CONS-BYOMEICD-TBL-MAX
                       MOVE    1           TO  FLG-TBLERR 
                       GO  TO  2007-BUBUN-HENSYU-EXT
                   END-IF
               WHEN    "2"  
                   COMPUTE IDY     =   IDY     -   1
                   IF      IDY         =   ZERO
                       MOVE    1           TO  FLG-TBLERR 
                       GO  TO  2007-BUBUN-HENSYU-EXT
                   END-IF
           END-EVALUATE
      *
           COMPUTE WRK-LENGTH  =   IDZ     -   FLG-BUBUN
           MOVE    FLG-BUBUN       TO  WRK-ST
      *
           MOVE    WRK-KEN-BYOMEI (WRK-ST:WRK-LENGTH)
                                   TO  WRK-BYOMEI-BYOMEI    (IDY)
           MOVE    "0000999"       TO  WRK-BYOMEI-BYOMEICD  (IDY 1)
      *
           MOVE    1               TO  WRK-KEN-ADDKBN
           MOVE    ZERO            TO  FLG-BUBUN
      * 
           .
       2007-BUBUN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-BYOMEI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-N-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-BYOMEI
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
               EVALUATE    LNK-BYO-SYORI
                   WHEN    "1"
                   WHEN    "3"
                       IF      FLG-KENSAKU-KBN =   1   OR  2  OR  0
                           IF      BYO-HAISIYMD    =   "99999999" 
                           AND     BYO-IKOSAKICD   =   "        "
                               CONTINUE
                           ELSE
                               GO  TO  900-BYOMEI-READ-N-SEC
                           END-IF
                      END-IF
               END-EVALUATE
           ELSE
              MOVE    1               TO  FLG-BYOMEI
              INITIALIZE                  BYOMEI-REC
           END-IF
           .
       900-BYOMEI-READ-N-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
       900-CLOSE-EXT.
           EXIT.
